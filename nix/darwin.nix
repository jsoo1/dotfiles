{ config, lib, pkgs, ... }:

let
  env = import ./env.nix { inherit pkgs; };

  nix-conf = {
    file = pkgs.copyPathToStore ./nix.conf.age;
    path = "/Users/johh.soo/.config/nix/nix.conf";
    owner = "johh.soo";
  };
in
{
  environment.systemPackages = env.shell-utilities;

  environment.variables.TERMINFO = "${pkgs.ncurses}/share/terminfo";

  age = {
    sshKeyPaths = [ "/Users/johh.soo/.ssh/id_rsa" ];
    secrets = {
      inherit nix-conf;
    };
  };

  services.nix-daemon = {
    enable = true;
    enableSocketListener = true;
  };

  nix = {
    maxJobs = 16;
    trustedUsers = [ "johh.soo" ];
    useDaemon = true;
    distributedBuilds = true;
    extraOptions = ''
      experimental-features = nix-command flakes
      system-features = benchmark big-parallel local nixos-test
      builders-use-substitutes = true
    '';
  };

  fonts = {
    fonts = [ pkgs.iosevka ];
    enableFontDir = true;
  };

  # Create /etc/bashrc that loads the nix-darwin environment.
  programs.bash.enable = true;

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;

  users.users."johh.soo" = {
    name = "johh.soo";
    home = "/Users/johh.soo";
  };

  launchd.user.agents.emacs.serviceConfig.ProgramArguments =
    [ "${pkgs.bash}/bin/sh" "-c" "${pkgs.my-emacs}/bin/emacs --fg-daemon=$USER" ];
}
