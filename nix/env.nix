with import ../pin.nix;
let
  emacs = [ my-emacs pinentry-emacs ];
  fonts = [ iosevka ];
  haskell-utilities = [ ghcid haskell-language-server ];
  nix-utilities = [ rnix-lsp ];
  remarkable-utilities = [ restream ];
  shell-utilities = [ direnv fd git gnupg mosh neovim ripgrep skim tmux ];
in builtins.concatLists [
  emacs
  fonts
  haskell-utilities
  nix-utilities
  remarkable-utilities
  shell-utilities
]
