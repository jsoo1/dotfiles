{ config
, lib
, ssh-auth-sock
, pkgs
}:
let
  username = config.home.username;
  em = "emacsclient -t --socket-name=${username}";
  tm = dir:
    ''
      env __NIX_DARWIN_SET_ENVIRONMENT_DONE="" tmux new-session -A -s $(basename "${dir}" | tr '.' '-') -c "${dir}" ${em} ${dir}'';

  sessionVariables = {
    EDITOR = em;
    ALTERNATE_EDITOR = "nvim";
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
      lsa = "exa -la";
      tree = "exa -Ta";
      vi = "nvim";
      pb = "curl -F c=@- pb";
      psg = "ps -eo pid,user,command | rg";
    };

  skim-cmds = {
    projects = ''
      fd '.git$' ~ -I -t d -d 3 -H -x dirname | sk | tr -d '\n'
    '';
    files = "fd '.*' '.' --hidden -E '.git*' | sk";
    history = ''
      history | sk --tac --no-sort | awk '{$1=""}1'
    '';
  };
in
{
  enable = true;
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
      red = text: "\\[\\033[0;1;31m\\]${text}\\[\\033[0m\\]";
      yellow = text: "\\[\\033[0;33m\\]${text}\\[\\033[0m\\]";
      cyan = text: "\\[\\033[0;36m\\]${text}\\[\\033[0m\\]";
      purple = text: "\\[\\033[0;35m\\]${text}\\[\\033[0m\\]";
      fmt = styleLinux: styleDarwin: if pkgs.stdenv.isDarwin then styleDarwin else styleLinux;
    in
    ''
      if [[ "" != $BASH_VERSION ]]; then # compat testing for bash (moving to osh)
        # Keybindings
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
        ${bind-key "eo" (set-prompt-to "tmux-projects")}
        ${bind-key "C-o" (set-prompt-to "tmux-projects")}
        ${bind-key "C-t" (set-prompt-to "skim-files")}
        ${bind-key "C-r" (set-prompt-to "skim-history")}
        bind -r '\ec'
      fi

      # Git stuff
      [ -n "$SSH_AUTH_SOCK" ] && ln -sf "$SSH_AUTH_SOCK" ${ssh-auth-sock}

      function _cbr {
        (git branch 2>/dev/null | awk '/^\*/ { $1=""; print $0 }') || echo ""
      }

      PS0=$([[ "" != $BASH_VERSION ]] && echo -n '\D{%F %T%z}\n' || echo $PS0)
      PS1="$([[ "" != $BASH_VERSION ]] && echo -n '\D{%F %T%z}\n' || echo -n "")${fmt bold bold "\\u@"}${fmt red cyan "\\h"} \\W${
        fmt yellow purple "\\$(_cbr)"
      } $ "
    '';
}
