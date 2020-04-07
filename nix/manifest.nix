let pkgs = import <nixpkgs> { };

in [
  # broken for now
  # pkgs.haskellPackages.idris
  pkgs.alacritty
  pkgs.bashInteractive
  pkgs.bash-completion
  pkgs.cabal-install
  pkgs.checkmake
  pkgs.coq
  pkgs.direnv
  pkgs.emacsGit-nox
  pkgs.exa
  pkgs.fd
  pkgs.fish
  pkgs.gnumake
  pkgs.iosevka
  pkgs.fzy
  pkgs.ghcid
  pkgs.global
  pkgs.hlint
  pkgs.haskellPackages.hoogle
  pkgs.htop
  pkgs.imagemagick
  pkgs.ngrok
  pkgs.nixfmt
  pkgs.nodejs-10_x
  pkgs.oathToolkit
  pkgs.ocaml
  pkgs.openvpn
  pkgs.ormolu
  pkgs.postgresql_11
  pkgs.purescript
  pkgs.qemu
  pkgs.racket-minimal
  pkgs.rage
  pkgs.readline
  pkgs.reattach-to-user-namespace
  pkgs.ripgrep
  pkgs.rustfmt
  pkgs.spago
  pkgs.openssh
  pkgs.stylish-haskell
  pkgs.tmux
  pkgs.watch
  pkgs.ocamlPackages.num
  # "unsupported" on macos
  # pkgs.libreoffice
  # pkgs.s3fs
]
