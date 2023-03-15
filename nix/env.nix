{ pkgs ? import ./pin.nix { } }:
let
  inherit (pkgs)

    bash-completion bashInteractive binutils bottom cachix ccls
    coreutils deadnix dogdns du-dust exa fd findutils gawk gdb
    ghcid git go gopls graphviz-nox gnutar haskell-language-server
    iosevka less libressl man-pages neovim nix-diff nix-prefetch
    nix-top nix-tree nixpkgs-fmt oil peep perl procps pv
    rage recutils restream ripgrep rnix-lsp rr rsync shellcheck
    socat tealdeer terraform-ls unar watch;

  inherit (pkgs.haskellPackages) fourmolu;

  inherit (pkgs.linuxPackages) perf;

in

rec {
  c-utilities = [
    gdb
    man-pages
  ] ++ pkgs.lib.optionals (!pkgs.stdenv.isDarwin) [
    binutils
    ccls
    rr
  ];

  haskell-utilities = [ fourmolu ghcid haskell-language-server ];

  go-utilities = [ go gopls ];

  macos-quirks = [
    bashInteractive
    gnutar
    less
    neovim
    ripgrep
    findutils
    fd
    git
    rage
    rsync
    dogdns
  ];

  nix-utilities = [
    deadnix
    nixpkgs-fmt
    nix-diff
    nix-prefetch
    nix-top
    nix-tree
    rnix-lsp
  ];

  remarkable-utilities = [ restream ];

  socket-utilities = [
    libressl # see "nc" in extraOutputsToInstall
    socat
  ];

  shell-utilities = pkgs.lib.optionals (!pkgs.stdenv.isDarwin) [ perf ] ++ [
    bash-completion
    bottom
    cachix
    coreutils
    dogdns
    du-dust
    exa
    fd
    gawk
    git
    graphviz-nox
    neovim
    oil
    peep
    perl # for skim (???)
    pv
    rage
    recutils
    ripgrep
    shellcheck
    tealdeer
    unar
    watch
  ];

  terraform-utilities = [ terraform-ls ];

  user = builtins.concatLists [
    haskell-utilities
    c-utilities
    go-utilities
    nix-utilities
    socket-utilities
    terraform-utilities
    (pkgs.lib.optionals (!pkgs.stdenv.isDarwin) [ procps ])
    (pkgs.lib.optionals pkgs.stdenv.isDarwin
      (macos-quirks ++ remarkable-utilities))
  ];
}
