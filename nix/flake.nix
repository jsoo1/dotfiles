{
  description = "A home-manager/nix-darwin configuration";
  inputs = {
    emacs.url = "github:jsoo1/emacs-overlay/release";
    dotfiles = {
      flake = false;
      url = "git+https://git.sr.ht/~jsoo/dotfiles?ref=release";
    };
    flake-compat = {
      flake = false;
      url = "github:edolstra/flake-compat";
    };
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:jsoo1/nixpkgs/release";
    home-manager = {
      url =
        "github:jsoo1/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    darwin = {
      url = "github:jsoo1/nix-darwin/release";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    deadnix = {
      url = "github:astro/deadnix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, deadnix, dotfiles, emacs, nixpkgs, flake-utils, home-manager, darwin, ... }:
    let
      overlays =
        [ deadnix.overlay emacs.overlay ]
        ++ import ./overlays/my-emacs.nix
        ++ import ./overlays/restream.nix;
      all-systems = flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ]
        (system: {
          packages = import nixpkgs { inherit system overlays; };
        });
    in
    rec {
      inherit (all-systems) packages;

      devShell.x86_64-linux =
        let
          pkgs = import nixpkgs {
            inherit overlays;
            system = "x86_64-linux";
          };
        in
        pkgs.mkShell {
          shellHook = "${homeConfigurations.john.activationPackage}/activate; exit $?";
        };

      darwinConfigurations.johhsoo = darwin.lib.darwinSystem
        {
          system = "x86_64-darwin";
          modules = [
            ({ pkgs, ... }: {
              nixpkgs = { inherit overlays; };
            })
            home-manager.darwinModules.home-manager
            {
              home-manager.useGlobalPkgs = true;
              home-manager.users."johh.soo" = import ./home.nix;
              home-manager.extraSpecialArgs = { inherit dotfiles; };
            }
            ./darwin.nix
          ];
        };

      homeConfigurations.john =
        let
          system = "x86_64-linux";
        in
        home-manager.lib.homeManagerConfiguration {
          pkgs = import nixpkgs { inherit system overlays; };
          modules = [
            ./home.nix
            {
              home = {
                username = "john";
                homeDirectory = "/home/john";
              };
            }
          ];
          extraSpecialArgs = {
            inherit dotfiles;
          };
        };
    };
}
