{ config, pkgs, ... }:

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

  ssh-known-hosts = {
    file = pkgs.copyPathToStore ./ssh_known_hosts.age;
    path = "/etc/ssh/ssh_known_hosts2";
    owner = "root";
    mode = "0444";
  };

  searches = {
    file = pkgs.copyPathToStore ./searches.age;
    path = "/etc/resolver/searches";
    owner = "root";
    mode = "0644";
  };
in
{
  imports = [ ./env.nix ];

  networking.hostName = "johhsoo";

  environment.systemPackages = config.shell-utilities;

  environment.shells = [
    pkgs.bashInteractive
    pkgs.oil
  ];

  age = {
    sshKeyPaths = [ "/Users/johh.soo/.ssh/id_rsa" ];
    secrets = {
      inherit
        nix-conf
        ssh-conf
        github
        machines
        searches
        ssh-known-hosts
        ;
    };
  };

  services.nix-daemon = {
    enable = true;
    enableSocketListener = true;
  };

  launchd.daemons.nix-daemon.serviceConfig = {
    SoftResourceLimits.NumberOfProcesses = 1048576;
  };

  nix = {
    distributedBuilds = true;
    settings = {
      system = "aarch64-darwin";
      trusted-users = [ "johh.soo" ];
      experimental-features = [ "nix-command" "flakes" "repl-flake" "recursive-nix" ];
      system-features = [ "benchmark" "big-parallel" "local" "nixos-test" ];
      sandbox = "relaxed";
      builders-use-substitutes = true;
      builders = "@${machines.path}";
      fallback = true;
    };
  };

  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

  fonts = {
    fonts = [ pkgs.iosevka ];
    fontDir.enable = true;
  };

  programs.bash.enable = true;
  programs.bash.enableCompletion = true; # breaks with osh

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;

  users.users."johh.soo" = {
    name = "johh.soo";
    home = "/Users/johh.soo";
    shell = pkgs.bashInteractive;
  };
}
