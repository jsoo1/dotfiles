# A VM I use sometimes to use a nixos system that has no other tenants
{ lib, pkgs, ... }:

{
  # imports = [
  #   /home/john/projects/remote-build-queue/scratch/module.nix
  #   /home/john/projects/trace-locks/scratch/module.nix
  # ];

  services.avahi.enable = true;

  security.sudo.wheelNeedsPassword = false;

  nix = {
    settings = {
      experimental-features = [ "nix-command" "flakes" "recursive-nix" ];
      trusted-users = [ "@wheel" ];
      allowed-users = [ "*" ];
      build-users-group = "nixbld";
      require-sigs = true;
      sandbox = "relaxed";
      system-features = [ "benchmark" "big-parallel" "nixos-test" "recursive-nix" ];
    };
  };

  programs.git = {
    package = pkgs.gitFull;
    enable = true;
    config.init.defaultBranch = "main";
  };

  time.timeZone = "America/Denver";

  services.openssh = {
    enable = true;
    settings.PasswordAuthentication = true;
    settings.StreamLocalBindUnlink = true;
    openFirewall = true;
  };

  environment = {
    enableDebugInfo = true;
    variables.EDITOR = "${lib.getExe pkgs.neovim}";
    defaultPackages = with pkgs; [
      curl
      direnv
      du-dust
      fd
      gitFull
      htop
      lsof
      man-pages
      man-pages-posix
      neovim
      nixpkgs-fmt
      linuxPackages.perf
      ripgrep
      rr
      strace
      tmux
      watch
    ];
  };

  users.mutableUsers = false;

  users.users.john = {
    password = "john";
    group = "users";
    isNormalUser = true;
    extraGroups = [ "wheel" ];
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILK6/O2/x73878Nz0Jy4nhL8A4lJqH+G43oOZI2yejB4 cardno:18_556_863"
    ];
  };
}
