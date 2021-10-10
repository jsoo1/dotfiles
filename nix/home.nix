{ config, lib, ... }:
let
  pkgs = import ./pin.nix;

  isDarwin = builtins.currentSystem == "x86_64-darwin";

  dotfiles = "${config.home.homeDirectory}/dotfiles";

  feeds = {
    ".emacs.d/feeds".recursive = true;
    ".emacs.d/feeds".source = "${dotfiles}/rss";
  };

  ssh-auth-sock = "${config.home.homeDirectory}/.ssh/auth_sock";

  tm = dir:
    ''
      tmux new-session -A -s $(basename "${dir}" | tr '.' '-') -c "${dir}" ${
        if isDarwin then
          "emacs"
        else
          "emacsclient --socket-name=${config.home.username} -t ${dir}"
      }'';

  shellAliases = {
    tm = "${tm "$PWD"}";
    tml = "tmux list-sessions";
    tma = "tmux attach-session -t";
    em = "emacsclient -t ${
        if !isDarwin then "--socket-name=${config.home.username}" else ""
      }";
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
      history | sk | awk '{for (i=2; i<NF; i++) printf $i " "; print $NF}'
    '';
  };

  services.gpg-agent.enable = true;
  services.gpg-agent.extraConfig = "allow-emacs-pinentry";

  systemd.user.services.emacs = {
    Unit.Description = "Emacs Daemon";
    Unit.Documentation = "man:emacs(1)";
    Install.WantedBy = [ "default.target" ];
    Service = {
      Environment = ''SSH_AUTH_SOCK="${ssh-auth-sock}"'';
      ExecStart =
        "${pkgs.my-emacs}/bin/emacs --fg-daemon=${config.home.username}";
      ExecStop = "${pkgs.coreutils}/bin/kill -9 $MAINPID";
    };
  };

  activation.emacs = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    $DRY_RUN_CMD ln -sf $VERBOSE_ARG $HOME/{dotfiles/nix,.emacs.d}/init.el
  '';

in lib.optionalAttrs (!isDarwin) { inherit systemd services; } // {
  home = lib.optionalAttrs isDarwin { inherit activation; } // {
    extraOutputsToInstall = [ "doc" ];

    packages = with pkgs;
      let
        fonts = [ iosevka ];
        haskell-utilities = [ ghcid haskell-language-server ];
        nix-utilities = [ nixfmt nix-diff nix-prefetch rnix-lsp ];
        remarkable-utilities = [ restream ];
        shell-utilities = [ bat dogdns fd gawk git jq mosh rage ripgrep watch ];
      in builtins.concatLists [
        haskell-utilities
        nix-utilities
        shell-utilities
        (lib.optionals isDarwin (fonts ++ remarkable-utilities))
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
    direnv.enable = true;
    gpg.enable = true;
    htop.enable = true;
    neovim.enable = true;
    tmux.enable = true;
    skim.enable = true;
    skim.defaultOptions = [ "-m" "--color=bw" ];
    emacs.enable = true;
    emacs.package = pkgs.my-emacs;

    bash = {
      enable = !isDarwin;
      inherit shellAliases;
      initExtra = ''
        skim-projects () {
          local proj="$(${skim-cmds.projects})"
          echo ${tm "$proj"}
        }
        skim-files () {
          echo $(${skim-cmds.files})
        }
        skim-history () {
          echo $(${skim-cmds.history})
        }
        bind -m emacs-standard '"\C-o": " \C-b\C-k \C-u`skim-projects`\e\C-e\er\C-a\C-y\C-h\C-e\e \C-y\ey\C-x\C-x\C-f"'
        bind -m emacs-standard '"\C-t": " \C-b\C-k \C-u`skim-files`\e\C-e\er\C-a\C-y\C-h\C-e\e \C-y\ey\C-x\C-x\C-f"'
        bind -m emacs-standard '"\C-r": " \C-b\C-k \C-u`skim-history`\e\C-e\er\C-a\C-y\C-h\C-e\e \C-y\ey\C-x\C-x\C-f"'

        [ -n "$SSH_AUTH_SOCK" ] && ln -sf "$SSH_AUTH_SOCK" ${ssh-auth-sock}
      '';
    };

    zsh = {
      enable = isDarwin;
      inherit shellAliases;
      initExtra = let
      in ''
        restart-nix-daemon () {
            sudo launchctl bootout system/org.nixos.nix-daemon \
                && sudo launchctl load /Library/LaunchDaemons/org.nixos.nix-daemon.plist
        }

        # ----- Keybindings -----
        __skim_history () {
          LBUFFER="$LBUFFER$(${skim-cmds.history})"
        }
        zle -N skim_history __skim_history
        __skim_files () {
          LBUFFER="$LBUFFER$(${skim-cmds.files})"
        }
        zle -N skim_files __skim_files
        __tmux_projects () {
          local proj="$(${skim-cmds.projects})"
          [ "" != "$proj" ] && LBUFFER="${tm "$proj"}"
        }
        zle -N tmux_projects __tmux_projects
        bindkey '^r' skim_history
        bindkey '^t' skim_files
        bindkey '^o' tmux_projects
      '';
    };
  };
}
