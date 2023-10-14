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
    "/run/current-system/sw/bin/osh"
    "/run/current-system/sw/bin/oil"
    "${pkgs.oil}/bin/osh"
    "${pkgs.oil}/bin/oil"
  ];

  environment.etc.shells.knownSha256Hashes = [
    "9d5aa72f807091b481820d12e693093293ba33c73854909ad7b0fb192c2db193"
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
  };

  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

  nix = {
    distributedBuilds = true;
    settings.system = "aarch64-darwin";
    settings.trusted-users = [ "johh.soo" ];
    settings.experimental-features = [ "nix-command" "flakes" "repl-flake" "recursive-nix" ];
    settings.system-features = [ "benchmark" "big-parallel" "local" "nixos-test" ];
    settings.sandbox = "relaxed";
    settings.builders-use-substitutes = true;
    settings.builders = "@${machines.path}";
    settings.fallback = true;
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
