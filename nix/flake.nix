{
  description = "A home-manager/nix-darwin configuration";
  inputs = {
    emacs = {
      url = "github:jsoo1/emacs-overlay/2023-11-14";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    dotfiles = {
      flake = false;
      url = "git+https://git.sr.ht/~jsoo/dotfiles?ref=release";
    };
    flake-compat = {
      flake = false;
      url = "github:edolstra/flake-compat";
    };
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:jsoo1/nixpkgs/release-2024-02-15";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    darwin = {
      url = "github:jsoo1/nix-darwin/jsoo1/2024-02-18";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    deadnix = {
      url = "github:astro/deadnix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.utils.follows = "flake-utils";
    };
    nil = {
      url = "github:oxalica/nil";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    soclip.url = "git+https://git.sr.ht/~jsoo/soclip?ref=release";
  };

  outputs =
    { self
    , deadnix
    , dotfiles
    , emacs
    , nil
    , nixpkgs
    , flake-utils
    , home-manager
    , darwin
    , soclip
    , ...
    }:
    let
      overlays' = [
        deadnix.overlays.default
        emacs.overlay
        soclip.overlays.default
        nil.overlays.nil
      ] ++ [
        (_: _:
          { emacs-xclip-soclip-support = soclip.patches.emacs-xclip-support; }
        )
      ]
      ++ import ./overlays/my-emacs.nix
      ++ import ./overlays/restream.nix
      ++ import ./overlays/default-shell.nix;
      all-systems = flake-utils.lib.eachSystem [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" ]
        (system: {
          packages = import nixpkgs { inherit system; overlays = overlays'; };
        });
    in
    rec {
      inherit (all-systems) packages;

      overlays.default = pkgsFinal: pkgsPrev:
        pkgsPrev.lib.composeManyExtensions overlays' pkgsFinal pkgsPrev;

      # Single home-manager reconfigure command for flakeless systems.
      # Usage: `nix-shell ~/dotfiles/nix/shell.nix`
      apps.x86_64-linux.default = {
        type = "app";
        program =
          let
            activate = packages.x86_64-linux.writeShellApplication {
              name = "activate-john";
              text = ''
                ${homeConfigurations.john.activationPackage}/activate
              '';
            };
          in
          "${packages.x86_64-linux.lib.getExe activate}";
      };

      darwinConfigurations.johhsoo = darwin.lib.darwinSystem {
        system = "aarch64-darwin";
        modules = [
          ({ pkgs, ... }: {
            nixpkgs.overlays = [ overlays.default ];
          })
          home-manager.darwinModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.users."johh.soo" = import ./home.nix;
            home-manager.extraSpecialArgs = { inherit dotfiles soclip; };
          }
          ./darwin.nix
        ];
      };

      homeConfigurations.john = home-manager.lib.homeManagerConfiguration {
        pkgs = packages.x86_64-linux;
        modules = [
          ./home.nix
          { home = { username = "john"; homeDirectory = "/home/john"; }; }
        ];
        extraSpecialArgs = { inherit dotfiles soclip; };
      };

      nixosConfigurations.vm = packages.aarch64-linux.nixos {
        imports = [
          "${nixpkgs}/nixos/modules/virtualisation/qemu-vm.nix"
          ({ lib, pkgs, config, ... }: {
            networking.hostName = "nixos-testing";

            networking.nameservers = [ "8.8.8.8" ];

            system.stateVersion = "24.05";

            services.openssh.listenAddresses = [{
              port = 22;
              addr = "127.0.0.1";
            }];

            virtualisation = {
              graphics = false;
              diskSize = 24 * 1024;
              memorySize = 4 * 1024;
              forwardPorts = [{
                from = "host";
                guest.port =
                  (lib.head config.services.openssh.listenAddresses).port;
                host.port = 2225;
              }];
              mountHostNixStore = true;
              useHostCerts = true;
            };
          })
          {
            boot.kernelPatches = [{
              name = "bpf-config";
              patch = null;
              extraConfig = ''
                LOCKDEP y
                LOCK_STAT y
              '';
            }];
          }
          ./vbox.nix
          home-manager.nixosModules.home-manager
          {
            home-manager.verbose = true;
            home-manager.useGlobalPkgs = true;
            home-manager.users.john = ./home.nix;
            home-manager.extraSpecialArgs = { inherit dotfiles soclip; };
          }
        ];
      };
    };
}
