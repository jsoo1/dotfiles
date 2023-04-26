{ config, lib, pkgs, ... }:

let
  nix-conf = {
    file = pkgs.copyPathToStore ./nix.conf.age;
    path = "/Users/johh.soo/.config/nix/nix.conf";
    owner = "johh.soo";
  };

  ssh-conf = {
    file = pkgs.copyPathToStore ./.sshconfig.age;
    path = "/Users/johh.soo/.ssh/config";
    owner = "johh.soo";
  };

  github = {
    file = pkgs.copyPathToStore ./github.age;
    path = "/Users/johh.soo/.config/nix/github";
    owner = "johh.soo";
  };

  machines = {
    file = pkgs.copyPathToStore ./machines.age;
    path = "/etc/nix/machines";
    owner = "root";
  };

in
{
  imports = [ ./env.nix ];

  networking.hostName = "johhsoo";

  environment.systemPackages = config.shell-utilities;

  environment.shells = [
    pkgs.bashInteractive
    "/run/current-system/sw/bin/osh"
    "/run/current-system/sw/bin/oil"
    "${pkgs.oil}/bin/osh"
    "${pkgs.oil}/bin/oil"
  ];

  environment.etc.shells.knownSha256Hashes = [
    "9d5aa72f807091b481820d12e693093293ba33c73854909ad7b0fb192c2db193"
  ];

  environment.variables.TERMINFO = "${pkgs.ncurses}/share/terminfo";

  age = {
    sshKeyPaths = [ "/Users/johh.soo/.ssh/id_rsa" ];
    secrets = { inherit nix-conf ssh-conf github machines; };
  };

  services.nix-daemon = {
    enable = true;
  };

  nix = {
    settings.max-jobs = 16;
    settings.trusted-users = [ "johh.soo" ];
    distributedBuilds = true;
    extraOptions = ''
      experimental-features = nix-command flakes
      system-features = benchmark big-parallel local nixos-test
      builders-use-substitutes = true
      builders = @/etc/nix/machines
    '';
  };

  fonts = {
    fonts = [ pkgs.iosevka ];
    fontDir.enable = true;
  };

  # Create /etc/bashrc that loads the nix-darwin environment.
  programs.bash.enable = true;
  programs.bash.enableCompletion = false; # breaks with osh

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;

  users.users."johh.soo" = {
    name = "johh.soo";
    home = "/Users/johh.soo";
    shell = "/run/current-system/sw/bin/osh";
  };
}
