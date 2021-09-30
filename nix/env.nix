let pkgs = import ../pin.nix;
in with pkgs; [
  direnv
  fd
  gnupg
  iosevka
  my-emacs
  neovim
  qemu
  restream
  ripgrep
  tmux
]
