let pkgs = import ../pin.nix;
in with pkgs; [ fd gnupg iosevka my-emacs qemu restream ripgrep ]
