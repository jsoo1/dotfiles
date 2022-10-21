{ config, lib, pkgs, ... }:

let
  env = import ./env.nix { inherit pkgs; };

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

in
{
  networking.hostName = "johhsoo";

  environment.extraInit = ''
    . ${pkgs.bash-completion}/share/bash-completion/bash_completion
  '';

  environment.systemPackages = env.shell-utilities;

  environment.shells = [
    pkgs.bashInteractive
    "/run/current-system/sw/bin/osh"
    "/run/current-system/sw/bin/oil"
    "${pkgs.oil}/bin/osh"
    "${pkgs.oil}/bin/oil"
  ];

  environment.variables.TERMINFO = "${pkgs.ncurses}/share/terminfo";

  age = {
    sshKeyPaths = [ "/Users/johh.soo/.ssh/id_rsa" ];
    secrets = { inherit nix-conf ssh-conf github; };
  };

  services.nix-daemon = {
    enable = true;
  };

  nix = {
    maxJobs = 16;
    trustedUsers = [ "johh.soo" ];
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
  programs.bash.enableCompletion = false; # breaks with osh

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;

  users.users."johh.soo" = {
    name = "johh.soo";
    home = "/Users/johh.soo";
    shell = "/run/current-system/sw/bin/osh";
  };

  launchd.user.agents.emacs.serviceConfig = {
    KeepAlive = true;
    ProgramArguments = [
      "/bin/sh"
      "-c"
      "/bin/wait4path ${pkgs.my-emacs}/bin/emacs &amp;&amp; exec ${pkgs.my-emacs}/bin/emacs --fg-daemon=${config.users.users."johh.soo".name}"
    ];
  };
}
