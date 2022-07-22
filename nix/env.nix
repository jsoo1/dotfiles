{ pkgs ? import ./pin.nix { } }:
let
  inherit (pkgs)

    bashCompletion bashInteractive binutils bottom cachix ccls
    coreutils dogdns du-dust exa fd gawk gdb ghcid git go gopls
    graphviz-nox gnutar haskell-language-server iosevka less libressl
    neovim nix-diff nix-prefetch nix-top nix-tree nixpkgs-fmt peep
    perl procps rage recutils restream ripgrep rnix-lsp rr rsync
    shellcheck socat tealdeer terraform-lsp watch;

  inherit (pkgs.haskellPackages) fourmolu;

  haskell-utilities = [ fourmolu ghcid haskell-language-server ];

  c-utilities =
    [ gdb ] ++ pkgs.lib.optionals (!pkgs.stdenv.isDarwin) [ binutils ccls rr ];

  go-utilities = [ go gopls ];

  macos-quirks = [
    bashInteractive
    bashCompletion
    gnutar
    less
    neovim
    ripgrep
    fd
    git
    rage
    rsync
    dogdns
  ];

  nix-utilities = [ nixpkgs-fmt nix-diff nix-prefetch nix-top nix-tree rnix-lsp ];

  remarkable-utilities = [ restream ];

  shell-utilities = [
    bashCompletion
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
    peep
    perl # for skim (???)
    rage
    recutils
    ripgrep
    shellcheck
    tealdeer
    watch
  ];

  socket-utilities = [
    libressl # see "nc" in extraOutputsToInstall
    socat
  ];

  terraform-utilities = [ terraform-lsp ];
in
{
  inherit haskell-utilities c-utilities macos-quirks nix-utilities
    remarkable-utilities shell-utilities socket-utilities terraform-utilities;

  user = builtins.concatLists [
    haskell-utilities
    c-utilities
    go-utilities
    nix-utilities
    socket-utilities
    terraform-utilities
    (pkgs.lib.optional (!pkgs.stdenv.isDarwin) procps)
    (pkgs.lib.optionals pkgs.stdenv.isDarwin
      (macos-quirks ++ remarkable-utilities))
  ];
}
