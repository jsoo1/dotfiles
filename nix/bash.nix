{ config, lib, ssh-auth-sock
, isDarwin ? builtins.currentSystem == "x86_64-darwin" }:
let
  username = config.home.username;
  hd = "ssh -q hd 'rm $(gpgconf --list-dirs agent-socket)' && ssh hd";
  em =
    "emacsclient -t ${if !isDarwin then "--socket-name=${username}" else ""}";
  tm = dir:
    let emacs-cmd = if isDarwin then "emacs" else em;
    in ''
      tmux new-session -A -s $(basename "${dir}" | tr '.' '-') -c "${dir}" ${emacs-cmd} ${dir}'';

  sessionVariables = {
    EDITOR = em;
    ALTERNATE_EDITOR = "nvim";
  };
  shellAliases = lib.optionalAttrs isDarwin { inherit hd; } // {
    inherit em;
    tm = tm "$PWD";
    tml = "tmux list-sessions";
    tma = "tmux attach-session -t";
    gco = "git checkout";
    gst = "git status";
    glg = "git log";
    lsa = "ls -lsa";
    vi = "nvim";
    pb = "curl -F c=@- pb";
  };

  skim-cmds = {
    projects = ''
      fd '.git$' ~ -t d -d 3 -H -x dirname | sk | tr -d '\n'
    '';
    files = "fd '.*' '.' --hidden -E '.git*' | sk";
    history = ''
      history | sk | awk '{$1=""}1'
    '';
  };
in lib.optionalAttrs (!isDarwin) { enableAutojump = true; } // {
  enable = true;
  inherit sessionVariables shellAliases;
  historyControl = [ "ignoredups" "ignorespace" ];
  historyIgnore = [ "ls" "cd" "tmux" ];
  initExtra = let
    set-prompt-to = cmd:
      ''
        " \C-b\C-k \C-u`${cmd}`\e\C-e\er\C-a\C-y\C-h\C-e\e \C-y\ey\C-x\C-x\C-f"'';
    bind-key = kbd: val: ''
      bind -m emacs-standard '"\${kbd}": ${val}'
    '';
    bold = text: "\\[\\033[0;1m\\]${text}\\[\\033[0m\\]";
    red = text: "\\[\\033[0;1;31m\\]${text}\\[\\033[0m\\]";
    yellow = text: "\\[\\033[0;33m\\]${text}\\[\\033[0m\\]";
    fmt = style: if !isDarwin then style else lib.id;
  in ''
    ${if isDarwin then ''
      restart-nix-daemon () {
                  sudo launchctl bootout system/org.nixos.nix-daemon \
                      && sudo launchctl load /Library/LaunchDaemons/org.nixos.nix-daemon.plist
      }'' else
      ""}

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

    # Git stuff
    [ -n "$SSH_AUTH_SOCK" ] && ln -sf "$SSH_AUTH_SOCK" ${ssh-auth-sock}

    function _cbr {
      (git branch 2>/dev/null | awk '/^\*/ { $1=""; print $0 }') || echo ""
    }

    PS1="${fmt bold "\\u@"}${fmt red "\\h"} \\w${fmt yellow "\\$(_cbr)"} $ "
  '';
}
