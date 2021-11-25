{
  description = "A home-manager/nix-darwin configuration";
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url =
      "github:NixOS/nixpkgs/67e5945d357ffa2d9bf7aa40fa59ddfd99513f12";
    home-manager = {
      url =
        "github:nix-community/home-manager/ddcd476603dfd3388b1dc8234fa9d550156a51f5";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    darwin = {
      url = "github:lnl7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs = { self, nixpkgs, flake-utils, home-manager, darwin }:
    let
      overlays = import ./my-emacs.nix ++ import ./restream.nix;
    in
    rec {
      # Useful for the following nix repl scenario to replace `nix repl pin.nix`:
      # $ nix repl
      # > fl = builtins.getFlake "*this $PWD here*"
      # > :b fl.pkgs.x86_64-darwin.ncurses
      packages = flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system: {
        pkgs = import nixpkgs {
          inherit system overlays;
        };
      });

      defaultPackage.x86_64-linux =
        (import nixpkgs { system = "x86_64-linux"; }).writeShellScriptBin
          "home-manager" ''
          ${home-manager.defaultPackage.x86_64-linux}/bin/home-manager switch --flake .#homeConfigurations.john
        '';

      defaultPackage.x86_64-darwin =
        let
          system = "x86_64-darwin";
          pkgs = import nixpkgs { inherit system; };
        in
        pkgs.writeShellScriptBin "darwin-rebuild" ''
          ${darwinConfigurations.johhsoo.system}/sw/bin/darwin-rebuild --impure --flake .# "$@"
        '';

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
            }
            ./darwin.nix
          ];
        };

      homeConfigurations.john =
        let
          system = "x86_64-linux";
          pkgs = import nixpkgs { inherit system overlays; };
        in
        home-manager.lib.homeManagerConfiguration
          {
            username = "john";
            homeDirectory = "/home/john";
            configuration.imports = [ ./home.nix ];
          };
    };
}
