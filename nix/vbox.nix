# A VM I use sometimes to use a nixos system that has no other tenants
{ config, lib, pkgs, ... }:

{
  # imports = [
  #   /home/john/projects/remote-build-queue/scratch/module.nix
  #   /home/john/projects/trace-locks/scratch/module.nix
  # ];

  services.avahi.enable = true;

  security.sudo.wheelNeedsPassword = false;

  nix = {
    settings.trusted-users = [ "@wheel" ];
    extraOptions = ''
      build-users-group = nixbld

      max-jobs = 16
      cores = 0
      sandbox = false

      substituters = https://cache.nixos.org/
      trusted-substituters =
      trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=
      require-sigs = true
      allowed-users = *
      system-features = benchmark big-parallel local nixos-test recursive-nix
      builders-use-substitutes = true
      require-sigs = false
      experimental-features = nix-command flakes recursive-nix
      sandbox-paths = /etc/nix-serve/nix-serve.sec=/etc/nix-serve/signing-key.sec
    '';
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
    openFirewall = true;
    extraConfig = ''
      StreamLocalBindUnlink yes
    '';
  };

  environment = {
    enableDebugInfo = true;
    variables.EDITOR = "${pkgs.neovim}/bin/nvim";
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
