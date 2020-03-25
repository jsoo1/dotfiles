let pkgs = import <nixpkgs> { };

in [
  pkgs.bash
  pkgs.bash-completion
  pkgs.checkmake
  pkgs.coq
  pkgs.direnv
  pkgs.exa
  pkgs.fd
  pkgs.fish
  pkgs.fzy
  pkgs.ghcid
  pkgs.global
  pkgs.hlint
  pkgs.hoogle
  pkgs.htop
  pkgs.jq
  pkgs.ngrok
  pkgs.nixfmt
  pkgs.nodejs-10_x
  pkgs.openvpn
  pkgs.ormolu
  pkgs.pijul
  pkgs.postgresql_11
  pkgs.qemu
  pkgs.racket-minimal
  pkgs.ripgrep
  pkgs.rustfmt
  pkgs.stylish-haskell
  pkgs.tmux
  pkgs.watch
]
