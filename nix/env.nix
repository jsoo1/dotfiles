{ lib, pkgs, ... }:
let
  inherit (pkgs.stdenv.hostPlatform) isLinux;
in

{
  options = {
    c-utilities = lib.mkOption {
      type = lib.types.listOf lib.types.package;
    };

    haskell-utilities = lib.mkOption {
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
    c-utilities = with pkgs; [
      gdb
      man-pages
      man-pages-posix
    ] ++ lib.optionals isLinux [
      binutils
      ccls
      rr
    ];

    haskell-utilities = with pkgs; [ haskellPackages.fourmolu ghcid haskell-language-server ];

    macos-quirks = with pkgs; [
      bashInteractive
      gnutar
      less
      neovim
      ripgrep
      findutils
      fd
      rage
      rsync
      dogdns
    ];

    nix-utilities = with pkgs; [
      deadnix
      nil
      nixpkgs-fmt
      nix-diff
      nix-prefetch
      nix-top
      nix-tree
      rnix-lsp
    ];

    remarkable-utilities = with pkgs; [ restream ];

    socket-utilities = with pkgs; [
      libressl # see "nc" in extraOutputsToInstall
      socat
    ];

    shell-utilities = with pkgs; lib.optionals isLinux [ iftop linuxPackages.perf ] ++ [
      bash-completion
      bottom
      cachix
      coreutils
      dogdns
      du-dust
      eza
      fd
      gawk
      gitFull
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

    terraform-utilities = with pkgs; [ terraform-ls ];
  };
}
