{ lib, pkgs, ... }:
let
  inherit (pkgs.stdenv.hostPlatform) isLinux isDarwin;
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

    experimental-utilities = lib.mkOption {
      type = lib.types.listOf lib.types.package;
    };
  };

  config = with pkgs; {
    c-utilities = lib.mkDefault (
      [
        binutils
        ccls
        man-pages
        man-pages-posix
      ]
      ++ lib.optionals isLinux [
        gdb
        rr
      ]
    );

    haskell-utilities = lib.mkDefault [
      haskellPackages.fourmolu
      ghciwatch
      haskell-language-server
    ];

    macos-quirks = lib.mkDefault [
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
      self.packages.x86_64-linux.systemd.man
    ];

    nix-utilities = lib.mkDefault [
      deadnix
      nixd
      nixfmt-rfc-style
      nixpkgs-fmt
      nix-diff
      nix-prefetch
      nix-top
      nix-tree
    ];

    remarkable-utilities = lib.mkDefault [ restream ];

    socket-utilities = [
      libressl.nc
      libressl
      socat
      wireshark-cli
    ];

    shell-utilities = lib.mkDefault (
      lib.optionals isLinux [
        iftop
        linuxPackages.perf
      ]
      ++ lib.optionals isDarwin [
        bash-completion
      ]
      ++ [
        bottom
        coreutils
        dogdns
        du-dust
        eza
        fd
        gawk
        git
        (google-cloud-sdk.withExtraComponents [
          google-cloud-sdk.components.gke-gcloud-auth-plugin
        ])
        graphviz-nox
        yq
        mosh
        neovim
        oils-for-unix
        parallel
        peep
        perl # for skim (???)
        pigz
        pstree
        pv
        rage
        ripgrep
        sha2wordlist
        shellcheck
        shfmt
        tealdeer
        unar
        watch
      ]
    );

    terraform-utilities = lib.mkDefault [ terraform-ls ];

    experimental-utilities = lib.mkDefault [ groovy-language-server ];
  };
}
