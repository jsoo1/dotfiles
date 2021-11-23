{
  description = "A home-manager/nix-darwin configuration";
  inputs = {
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
  outputs = { self, nixpkgs, home-manager, darwin }: rec {
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
            nixpkgs.overlays = import ./my-emacs.nix ++ import ./restream.nix;
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
        pkgs = import nixpkgs { inherit system; overlays = import ./my-emacs.nix ++ import ./restream.nix; };
      in
      home-manager.lib.homeManagerConfiguration
        {
          username = "john";
          homeDirectory = "/home/john";
          configuration.imports = [ ./home.nix ];
          inputs = { inherit home-manager; };
        };
  };
}
