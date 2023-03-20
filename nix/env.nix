{ config, lib, pkgs, ... }:
let
  inherit (pkgs.stdenv.hostPlatform) isDarwin isLinux;

  inherit (pkgs)

    bash-completion bashInteractive binutils bottom cachix ccls
    coreutils deadnix dogdns du-dust exa fd findutils gawk gdb ghcid
    git go gopls graphviz-nox gnutar haskell-language-server iosevka
    less libressl man-pages man-pages-posix neovim nix-diff
    nix-prefetch nix-top nix-tree nixpkgs-fmt oil peep perl procps pv
    rage recutils restream ripgrep rnix-lsp rr rsync shellcheck socat
    tealdeer terraform-ls unar watch;

  inherit (pkgs.haskellPackages) fourmolu;

  inherit (pkgs.linuxPackages) perf;

in

{
  options = {
    c-utilities = lib.mkOption {
      type = lib.types.listOf lib.types.package;
    };

    haskell-utilities = lib.mkOption {
      type = lib.types.listOf lib.types.package;
    };

    go-utilities = lib.mkOption {
      type = lib.types.listOf lib.types.package;
    };

    macos-quirks = lib.mkOption {
      type = lib.types.listOf lib.types.package;
    };

    nix-utilities = lib.mkOption {
      type = lib.types.listOf lib.types.package;
    };

    remarkable-utilities = lib.mkOption {
      type = lib.types.listOf lib.types.package;
    };

    socket-utilities = lib.mkOption {
      type = lib.types.listOf lib.types.package;
    };

    shell-utilities = lib.mkOption {
      type = lib.types.listOf lib.types.package;
    };

    terraform-utilities = lib.mkOption {
      type = lib.types.listOf lib.types.package;
    };
  };

  config = {
    c-utilities = [
      gdb
      man-pages
      man-pages-posix
    ] ++ lib.optionals isLinux [
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

    shell-utilities = lib.optionals isLinux [ perf ] ++ [
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
  };
}
