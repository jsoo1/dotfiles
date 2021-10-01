let pkgs = import ../pin.nix;
in with pkgs; [
  direnv
  fd
  ghcid
  gnupg
  iosevka
  mosh
  my-emacs
  neovim
  restream
  ripgrep
  skim
  tmux
]
