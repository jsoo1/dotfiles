let pkgs = import ../pin.nix; in with pkgs; [
  my-emacs
  iosevka
  qemu
  gnupg
]
