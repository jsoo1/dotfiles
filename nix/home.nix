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
in {
  home = {
    inherit packages;
    file = {
      ".zshrc" = {
        source = "/Users/johh.soo/dotfiles/nix/.zshrc";
        target = ".zshrc";
        executable = true;
      };
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
  };
  programs = {
    direnv = { enable = true; };
    emacs = {
      enable = true;
      package = pkgs.my-emacs;
    };
    gpg = { enable = true; };
    home-manager = { enable = true; };
    htop = { enable = true; };
    neovim = { enable = true; };
    skim = { enable = true; };
    tmux = { enable = true; };
    zsh = { enable = true; };
  };
}
