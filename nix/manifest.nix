let pkgs = import <nixpkgs> { };

in [
  pkgs.checkmake
  pkgs.direnv
  pkgs.fd
  pkgs.fzy
  pkgs.global
  pkgs.htop
  pkgs.idris
  pkgs.jq
  pkgs.ngrok
  pkgs.nixfmt
  pkgs.nodejs-10_x
  pkgs.postgresql_11
  pkgs.qemu
  pkgs.racket-minimal
  pkgs.ripgrep
  pkgs.rustfmt
  pkgs.tmux
  pkgs.watch
]
