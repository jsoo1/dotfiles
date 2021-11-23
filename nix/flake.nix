{
  description = "A nix{,-darwin} configuration";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    darwin = {
      url = "github:lnl7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs = { self, nixpkgs, home-manager, darwin }: {
    defaultPackage.x86_64-linux = nixpkgs.writeShellScriptBin "home-manager" ''
      ${home-manager}/bin/home-manager switch -f nix/home.nix
    '';
    defaultPackage.x86_64-darwin =
      nixpkgs.writeShellScriptBin "darwin-rebuild" ''
        ${darwin}/bin/darwin-rebuild switch --flake .#darwinConfigurations.john
      '';

    darwinConfigurations.john = darwin.lib.darwinSystem {
      system = "x86_64-darwin";
      modules = [ ./darwin.nix ];
    };
    homeConfigurations.john = home-manager.lib.homeManagerConfiguration {
      system = "x86_64-linux";
      username = "john";
      homeDirectory = "/home/john";
      configuration.imports = [ ./home.nix ];
    };
  };
}
