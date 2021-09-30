let pkgs = import ../pin.nix;
in with pkgs; [
  direnv
  fd
  ghcid
  gnupg
  iosevka
  my-emacs
  neovim
  qemu
  restream
  ripgrep
  skim
  tmux
]
