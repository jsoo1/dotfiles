let
  dotfiles = if builtins.currentSystem == "x86_64-darwin" then
    "/Users/johh.soo/dotfiles"
  else
    "/home/john/dotfiles";
  pkgs = import ../pin.nix;
  bq-opml = "bazqux-reader-subscriptions.xml";
  downcast-opml = "Downcast.opml";
  packages = with pkgs;
    let
      emacs = [ pinentry-emacs ];
      fonts = [ iosevka ];
      haskell-utilities = [ ghcid haskell-language-server ];
      nix-utilities = [ nix-diff nix-prefetch rnix-lsp ];
      remarkable-utilities = [ restream ];
      shell-utilities = [ bat dogdns fd git jq mosh ripgrep ];
    in builtins.concatLists [
      emacs
      haskell-utilities
      nix-utilities
      (if builtins.currentSystem == "x86_64-darwin" then
        fonts ++ remarkable-utilities
      else
        [ ])
      shell-utilities
    ];
  emacs-files = {
    ".emacs.d/${bq-opml}" = {
      source = "${dotfiles}/${bq-opml}";
      target = ".emacs.d/${bq-opml}";
    };
    ".emacs.d/${downcast-opml}" = {
      source = "${dotfiles}/${downcast-opml}";
      target = ".emacs.d/${downcast-opml}";
    };
  };
  file = {
    ".vimrc" = { source = "${dotfiles}/minimal/.vimrc"; };
    ".tmux.conf" = { source = "${dotfiles}/minimal/.tmux.conf"; };
    ".tmuxline.conf" = { source = "${dotfiles}/minimal/.tmuxline.conf"; };
  } // (if builtins.currentSystem == "x86_64-darwin" then emacs-files else { });
  sk = "${pkgs.skim}/bin/sk";
  tmux = "${pkgs.tmux}/bin/tmux";
  emacs = "${pkgs.emacs}/bin/emacs";
  emacsclient = "${pkgs.emacs}/bin/emacslient";
  shellAliases = {
    inherit sk;
    tm = "${tmux} new-session -A -s $(basename $PWD | tr '.' '-') ${
        if builtins.currentSystem == "x86_64-darwin" then emacs else ""
      }";
    tml = "${tmux} list-sessions";
    tma = "${tmux} attach-session -t";
    em = "${emacsclient} -t";
    lsa = "ls -lsa";
    vi = "nvim";
  };
  sessionVariables = { SKIM_DEFAULT_OPTIONS = "-m --color=bw"; };
  programs = {
    direnv = { enable = true; };
    zsh = {
      enable = builtins.currentSystem == "x86_64-darwin";
      inherit shellAliases sessionVariables;
      initExtra = let
        skim-files = "${pkgs.fd}/bin/fd '.*' '.' --hidden -E '.git*' | ${sk}";
        skim-history = ''
          history | ${sk} | ${pkgs.gawk}/bin/awk '{for (i=2; i<NF; i++) printf $i " "; print $NF}'
        '';
      in ''
        restart-nix-daemon () {
            sudo launchctl bootout system/org.nixos.nix-daemon \
                && sudo launchctl load /Library/LaunchDaemons/org.nixos.nix-daemon.plist
        }

        # ----- Keybindings -----
        __skim_history () { LBUFFER="$LBUFFER$(${skim-history})" }
        zle -N skim_history __skim_history
        __skim_files () { LBUFFER="$LBUFFER$(${skim-files})" }
        zle -N skim_files __skim_files
        bindkey '^r' skim_history
        bindkey '^t' skim_files
      '';
    };
    bash = {
      enable = builtins.currentSystem == "x86_64-linux";
      inherit shellAliases sessionVariables;
    };
    emacs = {
      enable = builtins.currentSystem == "x86_64-darwin";
      package = pkgs.my-emacs;
    };
    gpg = { enable = true; };
    home-manager = { enable = builtins.currentSystem == "x86_64-darwin"; };
    htop = { enable = true; };
    neovim = { enable = true; };
    skim = { enable = true; };
    tmux = { enable = true; };
  };
in {
  home = { inherit packages file; };
  inherit programs;
}
