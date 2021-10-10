{ config, lib, ... }:
let
  isDarwin = builtins.currentSystem == "x86_64-darwin";
  isLinux = builtins.currentSystem == "x86_64-linux";
  dotfiles = "${config.home.homeDirectory}/dotfiles";
  pkgs = import ./pin.nix;
  bq-opml = "bazqux-reader-subscriptions.xml";
  downcast-opml = "Downcast.opml";
  elfeed-feeds = {
    ".emacs.d/${bq-opml}" = { source = "${dotfiles}/${bq-opml}"; };
    ".emacs.d/${downcast-opml}" = { source = "${dotfiles}/${downcast-opml}"; };
  };
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
  ssh-auth-sock = "${config.home.homeDirectory}/.ssh/auth_sock";
  systemd.user.services.emacs = {
    Unit = {
      Description = "Emacs Daemon";
      Documentation = "man:emacs(1)";
    };
    Install = { WantedBy = [ "default.target" ]; };
    Service = {
      Environment = ''SSH_AUTH_SOCK="${ssh-auth-sock}"'';
      ExecStart =
        "${pkgs.my-emacs}/bin/emacs --fg-daemon=${config.home.username}";
      ExecStop = "${pkgs.coreutils}/bin/kill -9 $MAINPID";
    };
  };
in {
  home = {
    activation.emacs = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      $DRY_RUN_CMD ln -sf $VERBOSE_ARG $HOME/{dotfiles/nix,.emacs.d}/init.el
    '';
    extraOutputsToInstall = [ "doc" ];
    packages = with pkgs;
      let
        emacs-utilities = [ pinentry-emacs ];
        fonts = [ iosevka ];
        haskell-utilities = [ ghcid haskell-language-server ];
        nix-utilities = [ nixfmt nix-diff nix-prefetch rnix-lsp ];
        remarkable-utilities = [ restream ];
        shell-utilities = [ bat dogdns fd gawk git jq mosh rage ripgrep ];
      in builtins.concatLists [
        emacs-utilities
        haskell-utilities
        nix-utilities
        shell-utilities
        (lib.optionals isDarwin (fonts ++ remarkable-utilities))
      ];
    file = {
      ".ghci" = { source = "${dotfiles}/ghci/.ghci"; };
      ".haskeline" = { source = "${dotfiles}/ghci/.haskeline"; };
      ".psqlrc" = { source = "${dotfiles}/psql/.psqlrc"; };
      ".vimrc" = { source = "${dotfiles}/minimal/.vimrc"; };
      ".tmux.conf" = { source = "${dotfiles}/minimal/.tmux.conf"; };
    } // lib.optionalAttrs isDarwin elfeed-feeds;
  };
  programs = {
    direnv.enable = true;
    gpg.enable = true;
    home-manager.enable = isDarwin;
    htop.enable = true;
    neovim.enable = true;
    tmux.enable = true;
    skim = {
      enable = true;
      defaultOptions = [ "-m" "--color=bw" ];
    };
    emacs = {
      enable = true;
      package = pkgs.my-emacs;
    };
    bash = {
      enable = isLinux;
      inherit shellAliases;
      initExtra = ''
        [ -n "$SSH_AUTH_SOCK" ] && ln -sf "$SSH_AUTH_SOCK" ${ssh-auth-sock}
      '';
    };
    zsh = {
      enable = isDarwin;
      inherit shellAliases;
      initExtra = let
        skim-files = "fd '.*' '.' --hidden -E '.git*' | sk";
        skim-history = ''
          history | sk | awk '{for (i=2; i<NF; i++) printf $i " "; print $NF}'
        '';
        skim-projects = ''
          fd '.git$' ~/projects -t d -H -x dirname | sk | tr -d '\n'
        '';
      in ''
        restart-nix-daemon () {
            sudo launchctl bootout system/org.nixos.nix-daemon \
                && sudo launchctl load /Library/LaunchDaemons/org.nixos.nix-daemon.plist
        }

        # ----- Keybindings -----
        __skim_history () {
          LBUFFER="$LBUFFER$(${skim-history})"
        }
        zle -N skim_history __skim_history
        __skim_files () {
          LBUFFER="$LBUFFER$(${skim-files})"
        }
        zle -N skim_files __skim_files
        __tmux_projects () {
          local proj="$(${skim-projects})"
          [ "" != "$proj" ] && LBUFFER="${tm "$proj"}"
        }
        zle -N tmux_projects __tmux_projects
        bindkey '^r' skim_history
        bindkey '^t' skim_files
        bindkey '^o' tmux_projects
      '';
    };
  };
} // (lib.optionalAttrs (!isDarwin) { inherit systemd; })
