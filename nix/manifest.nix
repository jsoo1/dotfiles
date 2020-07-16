let
  pkgs = import "/Users/john/projects/nixpkgs/default.nix" {
    overlays = import "/Users/john/dotfiles/nix/overlays.nix";
  };

  shells = [ pkgs.fish ];

  shell-utils = [
    pkgs.direnv
    pkgs.exa
    pkgs.fd
    pkgs.git
    pkgs.checkmake
    pkgs.gnumake
    pkgs.fzy
    pkgs.global
    pkgs.imagemagick
    pkgs.qemu
    pkgs.rage
    pkgs.readline
    pkgs.ripgrep
    pkgs.openssh
    pkgs.tmux
    pkgs.watch
  ];

  embedded = [ pkgs.avrdude pkgs.dfu-programmer ];

  desktop-utils = [ pkgs.alacritty ];

  editors = [ pkgs.emacsGit-nox ];

  fonts = [ pkgs.iosevka ];

  latex = [ pkgs.texlive.combined.scheme-full ];

  unfree = [ pkgs.ngrok ];

  haskell-utils = [
    pkgs.cabal-install
    pkgs.ghcid
    pkgs.hlint
    pkgs.haskellPackages.hoogle
    pkgs.ormolu
    pkgs.stylish-haskell
  ];

  nix-utils = [ pkgs.nixfmt ];

  purescript-utils = [ pkgs.purescript pkgs.spago ];

  fun-langs = [
    pkgs.coq
    pkgs.dhall
    # conflicts with hoogle.
    (pkgs.lib.setPrio 6 pkgs.idris)
    pkgs.racket-minimal
    pkgs.swiProlog
    pkgs.ocaml
    pkgs.urweb
  ];

  rust-utils = [ pkgs.rustfmt ];

  macos-utils = [ pkgs.reattach-to-user-namespace ];

  kubernetes-utils = [ pkgs.minikube pkgs.kind pkgs.kubectl ];

  pano-specific = [
    # Conflicts with hoogle, take the work requirement over hoogle
    (pkgs.lib.setPrio 4 pkgs.elmPackages.elm)
    pkgs.elmPackages.elm-format
    pkgs.elmPackages.elm-language-server
    pkgs.nodejs-10_x
    pkgs.s3fs
    # stack doesn't work the way I need
    # pkgs.stack
    # libreoffic "unsupported" on macos
    # pkgs.libreoffice-unwrapped
    pkgs.oathToolkit
    pkgs.openvpn
    pkgs.postgresql_11
    pkgs.yarn
  ];

in (shells ++ shell-utils ++ desktop-utils ++ editors ++ embedded ++ fonts
  ++ latex ++ unfree ++ haskell-utils ++ nix-utils ++ purescript-utils
  ++ fun-langs ++ rust-utils ++ macos-utils ++ kubernetes-utils
  ++ pano-specific)
