let pkgs = import ../pin.nix;
in with pkgs; [ gnupg iosevka my-emacs qemu restream ]
