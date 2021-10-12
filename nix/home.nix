{ config, lib, ... }:
let
  pkgs = import ./pin.nix;

  isDarwin = builtins.currentSystem == "x86_64-darwin";

  home = config.home.homeDirectory;
  username = config.home.username;
  dotfiles = "${home}/dotfiles";

  feeds = {
    ".emacs.d/feeds".recursive = true;
    ".emacs.d/feeds".source = "${dotfiles}/rss";
  };

  ssh-auth-sock = "${home}/.ssh/auth_sock";

  hd = "ssh hd 'rm $(gpgconf --list-dirs agent-socket)' && ssh hd";
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
      fd '.git$' ~ -t d -H -x dirname | sk | tr -d '\n'
    '';
    files = "fd '.*' '.' --hidden -E '.git*' | sk";
    history = ''
      history | sk | awk '{$1=""}1'
    '';
  };

  services.gpg-agent = {
    enable = true;
    extraConfig = ''
      allow-emacs-pinentry
      allow-loopback-entry
    '';
    verbose = true;
    pinentryFlavor = "curses";
  };

  systemd.user.services.emacs = {
    Unit.Description = "Emacs Daemon";
    Unit.Documentation = "man:emacs(1)";
    Install.WantedBy = [ "default.target" ];
    Service = {
      Environment = ''SSH_AUTH_SOCK="${ssh-auth-sock}"'';
      ExecStart = "${pkgs.my-emacs}/bin/emacs --fg-daemon=${username}";
      ExecStop = "${pkgs.coreutils}/bin/kill -9 $MAINPID";
    };
  };

  activation.emacs = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    $DRY_RUN_CMD ln -sf $VERBOSE_ARG $HOME/{dotfiles/nix,.emacs.d}/init.el
  '';

in lib.optionalAttrs (!isDarwin) { inherit systemd services; } // {
  home = lib.optionalAttrs isDarwin { inherit activation; } // {
    extraOutputsToInstall = [ "doc" ];

    packages = let
      inherit (pkgs)
        bashCompletion bashInteractive dogdns fd gawk ghcid git
        haskell-language-server iosevka nix-diff nix-prefetch nixfmt rage
        restream ripgrep rnix-lsp watch;
      fonts = [ iosevka ];
      haskell-utilities = [ ghcid haskell-language-server ];
      macos-quirks = [ bashInteractive ];
      nix-utilities = [ nixfmt nix-diff nix-prefetch rnix-lsp ];
      remarkable-utilities = [ restream ];
      shell-utilities =
        [ bashCompletion dogdns fd gawk git rage ripgrep watch ];
    in builtins.concatLists [
      haskell-utilities
      nix-utilities
      shell-utilities
      (lib.optionals isDarwin (fonts ++ macos-quirks ++ remarkable-utilities))
    ];

    file = lib.optionalAttrs isDarwin feeds // {
      ".ghci".source = "${dotfiles}/ghci/.ghci";
      ".haskeline".source = "${dotfiles}/ghci/.haskeline";
      ".psqlrc".source = "${dotfiles}/psql/.psqlrc";
      ".vimrc".source = "${dotfiles}/minimal/.vimrc";
      ".tmux.conf".source = "${dotfiles}/minimal/.tmux.conf";
    };
  };

  programs = {
    bat.enable = true;
    direnv.enable = true;
    direnv.enableBashIntegration = true;
    emacs.enable = true;
    emacs.package = pkgs.my-emacs;
    gpg.enable = true;
    htop.enable = true;
    jq.enable = true;
    neovim.enable = true;
    skim.defaultOptions = [ "-m" "--color=bw" ];
    skim.enable = true;
    tmux.enable = true;

    bash = {
      enable = true;
      inherit sessionVariables shellAliases;
      enableAutojump = true;
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

        function prompt_command {
          if git status > /dev/null 2>&1; then
              export GIT_STATUS=$(git branch | awk '/^\*/ { $1=""; print $0 }')
          else
              export GIT_STATUS=""
          fi
        }
        PROMPT_COMMAND=prompt_command

        PS1="${fmt bold "\\u@"}${fmt red "\\h"} \\w${
          fmt yellow "\\$GIT_STATUS"
        } ${fmt bold "$"} "
      '';
    };
  };
}
