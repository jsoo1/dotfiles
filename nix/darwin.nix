{ config, pkgs, ... }:

let
  nix-conf = {
    file = pkgs.copyPathToStore ./nix.conf.age;
    path = "/etc/nix/nix-2.conf";
    owner = config.users.users."johh.soo".name;
  };

  ssh-conf = {
    file = pkgs.copyPathToStore ./.sshconfig.age;
    path = "${config.users.users."johh.soo".home}/.ssh/config";
    owner = config.users.users."johh.soo".name;
  };

  github = {
    file = pkgs.copyPathToStore ./github.age;
    path = "/Users/johh.soo/.config/nix/github";
    owner = config.users.users."johh.soo".name;
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
    sshKeyPaths = [ "/Users/johh.soo/.ssh/rage_rsa" ];
    secrets = {
      inherit
        nix-conf
        ssh-conf
        github
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
      trusted-users = [ "root" "@admin" "johh.soo" ];
      experimental-features = [ "nix-command" "flakes" "repl-flake" "recursive-nix" ];
      system-features = [ "benchmark" "big-parallel" "local" "nixos-test" ];
      fallback = true;
      allow-unsafe-native-code-during-evaluation = true;
    };
    extraOptions = ''
      include ${nix-conf.path}
    '';
    linux-builder = {
      enable = true;
      supportedFeatures = [ "kvm" "benchmark" "big-parallel" ];
      maxJobs = 4;
      config = ({ pkgs, ... }: {
        environment.defaultPackages = [ pkgs.neovim ];
        users.users.root.openssh.authorizedKeys.keys = [
          "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILK6/O2/x73878Nz0Jy4nhL8A4lJqH+G43oOZI2yejB4 cardno:18_556_863"
        ];
        virtualisation = {
          cores = 4;
          darwin-builder = {
            diskSize = 128 * 1024;
            memorySize = 6 * 1024;
          };
        };
      });
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
