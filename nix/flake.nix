{
  description = "A home-manager/nix-darwin configuration";
  inputs = {
    emacs = {
      url = "github:jsoo1/emacs-overlay/2025-06-25";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixpkgs.url = "github:jsoo1/nixpkgs/release-2025-06-25";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    darwin = {
      url = "github:jsoo1/nix-darwin/jsoo1/2025-06-25";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    soclip.url = "git+https://git.sr.ht/~jsoo/soclip?ref=release";
  };
  outputs =
    {
      emacs,
      nixpkgs,
      home-manager,
      darwin,
      soclip,
      self,
    }:
    let
      inherit (nixpkgs) lib;
      toSpecific = systems: f: lib.foldAttrs lib.mergeAttrs { } (lib.map f systems);
      overlays.default =
        pkgsFinal: pkgsPrev: pkgsPrev.lib.composeManyExtensions overlays' pkgsFinal pkgsPrev;
      overlays' =
        [
          emacs.overlay
          soclip.overlays.default
          (_: _: { emacs-xclip-soclip-support = soclip.patches.emacs-xclip-support; })
          (_: _: { inherit self; })
        ]
        ++ import ./overlays/my-emacs.nix
        ++ import ./overlays/restream.nix
        ++ import ./overlays/default-shell.nix
        ++ import ./overlays/groovy-language-server.nix;
      all-systems = toSpecific [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" ] (system: {
        packages.${system} = import nixpkgs {
          inherit system;
          overlays = [ overlays.default ];
        };
      });
    in
    rec {
      inherit (all-systems) packages;

      inherit overlays;

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

      darwinConfigurations.johhsD759KPm = darwin.lib.darwinSystem {
        system = "aarch64-darwin";
        enableNixpkgsReleaseCheck = false;
        modules = [
          {
            nixpkgs.pkgs = packages.aarch64-darwin;
          }
          home-manager.darwinModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.users."johh.soo" = import ./home.nix;
            home-manager.extraSpecialArgs = { inherit soclip; };
          }
          ./darwin.nix
        ];
      };

      homeConfigurations.john = home-manager.lib.homeManagerConfiguration {
        pkgs = packages.x86_64-linux;
        modules = [
          ./home.nix
          {
            home = {
              username = "john";
              homeDirectory = "/home/john";
            };
          }
        ];
        extraSpecialArgs = { inherit soclip; };
      };

      nixosConfigurations.vm = packages.aarch64-linux.nixos {
        imports = [
          "${nixpkgs}/nixos/modules/virtualisation/qemu-vm.nix"
          (
            { lib, config, ... }:
            {
              nixpkgs.pkgs = packages.aarch64-linux;
              networking.hostName = "nixos-testing";

              networking.nameservers = [ "8.8.8.8" ];

              system.stateVersion = "24.05";

              services.openssh.listenAddresses = [
                {
                  port = 22;
                  addr = "127.0.0.1";
                }
              ];

              virtualisation = {
                graphics = false;
                cores = 8;
                diskSize = 24 * 1024;
                memorySize = 4 * 1024;
                forwardPorts = [
                  {
                    from = "host";
                    guest.port = (lib.head config.services.openssh.listenAddresses).port;
                    host.port = 2225;
                  }
                ];
                mountHostNixStore = true;
                useHostCerts = true;
              };
            }
          )
          # {
          #   boot.kernelPatches = [{
          #     name = "bpf-config";
          #     patch = null;
          #     extraConfig = ''
          #       LOCKDEP y
          #       LOCK_STAT y
          #     '';
          #   }];
          # }
          ./vbox.nix
          home-manager.nixosModules.home-manager
          {
            home-manager.verbose = true;
            home-manager.useGlobalPkgs = true;
            home-manager.users.john = ./home.nix;
            home-manager.extraSpecialArgs = { inherit soclip; };
          }
        ];
      };
    };
}
