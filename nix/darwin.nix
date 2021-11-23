{ config, lib, pkgs, ... }:

let env = import ./env.nix { inherit pkgs; };
in {
  imports = [ <home-manager/nix-darwin> ];
  environment.systemPackages = env.shell-utilities;

  # Use a custom configuration.nix location.
  environment.darwinConfig = "$HOME/dotfiles/nix/darwin.nix";

  services.nix-daemon = {
    enable = true;
    enableSocketListener = true;
  };
  nix = import ./nix-daemon.nix;

  # Create /etc/bashrc that loads the nix-darwin environment.
  programs.bash.enable = true;

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;

  users.users."johh.soo" = {
    name = "John Soo";
    home = "/Users/johh.soo";
  };
  home-manager.useGlobalPkgs = true;
  home-manager.users."johh.soo" = import ./home.nix;
}
