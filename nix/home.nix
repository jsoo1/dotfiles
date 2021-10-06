let
  pkgs = import ../pin.nix;
  bq-opml = "bazqux-reader-subscriptions.xml";
  downcast-opml = "Downcast.opml";
  packages = with pkgs;
    let
      emacs = [ pinentry-emacs ];
      fonts = [ iosevka ];
      haskell-utilities = [ ghcid haskell-language-server ];
      nix-utilities = [ rnix-lsp ];
      remarkable-utilities = [ restream ];
      shell-utilities = [ bat dogdns fd git jq mosh ripgrep ];
    in builtins.concatLists [
      emacs
      fonts
      haskell-utilities
      nix-utilities
      remarkable-utilities
      shell-utilities
    ];
  file = {
    ".vimrc" = {
      source = "/Users/johh.soo/dotfiles/minimal/.vimrc";
      target = ".vimrc";
    };
    ".tmux.conf" = {
      source = "/Users/johh.soo/dotfiles/minimal/.tmux.conf";
      target = ".tmux.conf";
    };
    ".tmuxline.conf" = {
      source = "/Users/johh.soo/dotfiles/minimal/.tmuxline.conf";
    };
    ".emacs.d/${bq-opml}" = {
      source = "/Users/johh.soo/dotfiles/${bq-opml}";
      target = ".emacs.d/${bq-opml}";
    };
    ".emacs.d/${downcast-opml}" = {
      source = "/Users/johh.soo/dotfiles/${downcast-opml}";
      target = ".emacs.d/${downcast-opml}";
    };
  };
  sk = "TERM=xterm-256color command ${pkgs.skim}/bin/sk --color=bw";
  tmux = "${pkgs.tmux}/bin/tmux";
  emacs = "${pkgs.emacs}/bin/emacs";
  emacsclient = "${pkgs.emacs}/bin/emacslient";
  shellAliases = {
    inherit sk;
    tm = "${tmux} new-session -A -s $(basename $PWD | tr '.' '-') ${emacs}";
    tml = "${tmux} list-sessions";
    tma = "${tmux} attach-session -t";
    em = "${emacsclient} -t";
    lsa = "ls -lsa";
    vi = "nvim";
  };
  skim-files = "${pkgs.fd}/bin/fd '.*' '.' --hidden -E '.git*' | ${sk}";
  skim-history = ''
    history | ${sk} | ${pkgs.gawk}/bin/awk '{for (i=2; i<NF; i++) printf $i " "; print $NF}'
  '';
  zsh = {
    enable = true;
    inherit shellAliases;
    initExtra = ''
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
      bindkey '^r' skim_history
      bindkey '^t' skim_files

      # ----- Direnv -----
      eval "$(${pkgs.direnv}/bin/direnv hook zsh)"
    '';
  };
  bash = {
    enable = true;
    inherit shellAliases;
  };
  shell-program = if builtins.currentSystem == "x86_64-darwin" then {
    inherit zsh;
  } else {
    inherit bash;
  };
  programs = shell-program // {
    direnv = { enable = true; };
    emacs = {
      enable = true;
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
