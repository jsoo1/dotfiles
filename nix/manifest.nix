let
  pkgs = import <nixpkgs> { };
  panoSpecific = [
    pkgs.bashInteractive
    pkgs.bash-completion
    # Conflicts with hoogle, take the work requirement over hoogle
    (pkgs.lib.setPrio 4 pkgs.elmPackages.elm)
    pkgs.elmPackages.elm-format
    pkgs.elmPackages.elm-language-server
    pkgs.ngrok # Unfree
    pkgs.nodejs-10_x
    pkgs.s3fs
    pkgs.stack
    # "unsupported" on macos
    # pkgs.libreoffice
  ];

in panoSpecific ++ [
  # broken for now (but only in the manifest file?)
  # Also conflicts with hoogle.
  # (pkgs.lib.setPrio 4 pkgs.idris)
  pkgs.alacritty
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
  pkgs.imagemagick
  pkgs.nixfmt
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
  pkgs.texlive
  pkgs.tmux
  pkgs.watch
  pkgs.ocamlPackages.num
]
