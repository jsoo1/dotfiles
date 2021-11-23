{ config, lib, pkgs, ... }:

let env = import ./env.nix { inherit pkgs; };
in
{
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
    name = "johh.soo";
    home = "/Users/johh.soo";
  };
}
