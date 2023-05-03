{
  description = "A home-manager/nix-darwin configuration";
  inputs = {
    emacs = {
      url = "github:jsoo1/emacs-overlay/2023-01-31";
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
    nixpkgs.url = "github:jsoo1/nixpkgs/release-2023-05-03";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    darwin = {
      url = "github:jsoo1/nix-darwin/2023-05-03";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    deadnix = {
      url = "github:astro/deadnix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.utils.follows = "flake-utils";
    };
    soclip.url = "git+https://git.sr.ht/~jsoo/soclip?ref=release";
  };

  outputs = { self, deadnix, dotfiles, emacs, nixpkgs, flake-utils, home-manager, darwin, soclip, ... }:
    let
      overlays = [
        deadnix.overlays.default
        emacs.overlay
        soclip.overlays.default
      ] ++ [
        (_: _:
          { emacs-xclip-soclip-support = soclip.patches.emacs-xclip-support; }
        )
      ] ++ import ./overlays/my-emacs.nix ++ import ./overlays/restream.nix;
      all-systems = flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ]
        (system: {
          packages = import nixpkgs { inherit system overlays; };
        });
    in
    rec {
      inherit (all-systems) packages;

      overlay = pkgsFinal: pkgsPrev:
        pkgsPrev.lib.composeManyExtensions overlays pkgsFinal pkgsPrev;

      # Single home-manager reconfigure command for flakeless systems.
      # Usage: `nix-shell ~/dotfiles/nix/shell.nix`
      devShell.x86_64-linux = packages.x86_64-linux.mkShell {
        shellHook = "${homeConfigurations.john.activationPackage}/activate; exit $?";
      };

      darwinConfigurations.johhsoo = darwin.lib.darwinSystem {
        system = "x86_64-darwin";
        modules = [
          ({ pkgs, ... }: {
            nixpkgs = { inherit overlays; };
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

      nixosConfigurations.vbox = packages.x86_64-linux.nixos {
        imports = [
          "${nixpkgs}/nixos/modules/virtualisation/virtualbox-image.nix"
          ({ lib, pkgs, ... }: {
            # cribbed from  installer/virtualbox-demo.nix
            # FIXME: UUID detection is currently broken
            boot.loader.grub.fsIdentifier = "provided";

            # Add some more video drivers to give X11 a shot at working in
            # VMware and QEMU.
            services.xserver.videoDrivers = lib.mkOverride 40 [
              "virtualbox"
              "vmware"
              "cirrus"
              "vesa"
              "modesetting"
            ];

            powerManagement.enable = false;

            system.stateVersion = "22.05";

            networking.hostName = "vbox";
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
            home-manager.users.john = import ./home.nix;
            home-manager.extraSpecialArgs = { inherit dotfiles soclip; };
          }
        ];
      };
    };
}
