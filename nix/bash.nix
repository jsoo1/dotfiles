{ config, lib, pkgs, ... }:
let
  username = config.home.username;
  em = "emacsclient -t --socket-name=${username}";
  tm = dir:
    ''
      env __NIX_DARWIN_SET_ENVIRONMENT_DONE="" tmux new-session -A -s $(basename "${dir}" | tr '.' '-') -c "${dir}" ${em} ${dir}'';

  sessionVariables = {
    EDITOR = em;
    ALTERNATE_EDITOR = "nvim";
  } // lib.optionalAttrs pkgs.stdenv.hostPlatform.isDarwin {
    NIX_PATH = "nixpkgs=${pkgs.path}";
  };

  shellAliases =
    lib.optionalAttrs pkgs.stdenv.isDarwin { top = "${pkgs.htop}/bin/htop"; } // {
      inherit em;
      tm = tm "$PWD";
      tml = "tmux list-sessions";
      tma = "tmux attach-session -t";
      gco = "git checkout";
      gst = "git status";
      glg = "git log";
      lsa = "eza -la";
      tree = "eza -Ta";
      vi = "nvim";
      pb = "curl -F c=@- pb";
      psg = "ps -eo pid,user,command | rg";
    };

  skim-cmds = {
    projects = "fd '.git$' ~ -I -t d -t f -d 3 -H -x dirname | sk | tr -d '\n'";
    files = "fd '.*' '.' --hidden -E '.git*' | sk";
    history = "history | sk --tac --no-sort | awk '{$1=\"\"}1'";
    rg = "sk --ansi -i -c 'rg --color=never --line-number \"{}\" .'";
  };
in
{
  imports = [ ./ssh-auth-sock.nix ];

  config.programs.bash = {
    enable = true;
    enableCompletion = pkgs.stdenv.isLinux;
    inherit sessionVariables shellAliases;
    historyControl = [ "erasedups" "ignoredups" "ignorespace" ];
    historyIgnore = [ "ls" "cd" "tmux" ];
    initExtra =
      let
        set-prompt-to = cmd:
          ''
            " \C-b\C-k \C-u`${cmd}`\e\C-e\er\C-a\C-y\C-h\C-e\e \C-y\ey\C-x\C-x\C-f"'';
        bind-key = kbd: val: ''
          bind -m emacs-standard '"\${kbd}": ${val}'
        '';
        bold = text: "\\[\\033[0;1m\\]${text}\\[\\033[0m\\]";
        green = text: "\\[\\033[0;32m\\]${text}\\[\\033[0m\\]";
        red = text: "\\[\\033[0;1;31m\\]${text}\\[\\033[0m\\]";
        yellow = text: "\\[\\033[0;33m\\]${text}\\[\\033[0m\\]";
        cyan = text: "\\[\\033[0;36m\\]${text}\\[\\033[0m\\]";
        purple = text: "\\[\\033[0;35m\\]${text}\\[\\033[0m\\]";
        fmt = styleLinux: styleDarwin: if pkgs.stdenv.isDarwin then styleDarwin else styleLinux;
      in
      ''
        tmux-projects () {
          local proj="$(${skim-cmds.projects})"
          [ "" != "$proj" ] && echo ${tm "$proj"}
        }
        skim-files () {
          echo $(${skim-cmds.files})
        }
        skim-history () {
          echo $(${skim-cmds.history})
        }
        skim-rg () {
          echo $(${skim-cmds.rg})
        }

        if [[ "" != $BASH_VERSION ]]; then # compat testing for bash (moving to osh)
          # Keybindings
          ${bind-key "eo" (set-prompt-to "tmux-projects")}
          ${bind-key "C-o" (set-prompt-to "tmux-projects")}
          ${bind-key "C-t" (set-prompt-to "skim-files")}
          ${bind-key "C-r" (set-prompt-to "skim-history")}
          ${bind-key "e/" (set-prompt-to "skim-rg")}
          bind -r '\ec'
        fi

        # Git stuff
        ${lib.optionalString pkgs.stdenv.hostPlatform.isLinux
          ''[ -n "$SSH_AUTH_SOCK" ] && ln -sf "$SSH_AUTH_SOCK" ${config.ssh-auth-sock}''
        }

        function _cbr {
          (git branch 2>/dev/null | awk '/^\*/ { $1=""; print $0 }') || echo ""
        }

        function _k8sctx {
          if ! (type -p kubectl 2>&1 1>/dev/null); then
            return
          else
            echo "($(kubectl config current-context 2>/dev/null || echo none)) "
          fi
        }

        PS1="${fmt bold bold "\\u@"}${fmt red cyan "\\h"} \\W${
          fmt yellow purple "\\$(_cbr)"
        } ${fmt green green "\\$(_k8sctx)"}$ "

        if [[ "" != $BASH_VERSION ]]; then
          PS0="$(echo -n '\D{%F %T%z}\n')"
          PS1="$(echo -n '\D{%F %T%z}\n')$PS1"
        fi
      '';
  };
}
