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
    '';
  };

  programs.git = {
    enable = true;
    config.init.defaultBranch = "main";
  };

  time.timeZone = "America/Los_Angeles";

  services.openssh = {
    enable = true;
    passwordAuthentication = true;
    openFirewall = true;
    extraConfig = ''
      StreamLocalBindUnlink yes
    '';
  };

  environment = {
    enableDebugInfo = true;
    variables.EDITOR = "${pkgs.neovim}/bin/nvim";
    defaultPackages = [
      pkgs.curl
      pkgs.direnv
      pkgs.du-dust
      pkgs.fd
      pkgs.git
      pkgs.htop
      pkgs.lsof
      pkgs.man-pages
      pkgs.man-pages-posix
      pkgs.neovim
      pkgs.nixpkgs-fmt
      pkgs.linuxPackages.perf
      pkgs.ripgrep
      pkgs.rr
      pkgs.strace
      pkgs.tmux
      pkgs.watch
    ];
  };

  users.users.john = {
    password = "john";
    group = "users";
    isNormalUser = true;
    extraGroups = [ "wheel" ];
    openssh.authorizedKeys.keys = [ "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDKY253LDGAf14dHxXlhh2BPj9hvfQMcEMlv2z3Q0zg/HVJGwT1qC0pwbhS5DBckqYs2DzNmoJ0AP9KRmsMZJtbal/Vzffe/MXEwK852u4a2DenxIohFY1La+JLVkX0NGpvOTOkIN1ScZ3UzIpJEkcJniHT+RfNmHtJUJTb/XXS2dULiqby2JpIS5BpqLURTmXeoQQ7fZsGaMDwVtI53PhcUaC6Fz9dAMS5GiK6xlLguIHqqa9tojQYwM4NLIrr1vHGrt3MW2ODtTuHePyRrBvfD98Lnr0nu3syQfgNxFOpkxx0FP7TGY1s6t2wwXDdlevJThxjC3utHTsQBSbcKWajKyAsHnsjQqZmF5802EO+iYwuSH8zhXb4+X/ieKkg9NSPCbeaiI/fTMs3bsJOruPcrjDFjLnzxRmgFW0V+nLFZJLunnXv5JiKvXkepC2ITJEVfwDZsucCUDG81+JMq0yqmPd9PY/hERW1AhKoRnnI6Qih9ddwVBrs9PSf41bcyW4cfO3FGtqDUbod73idVwCvzN58GTttnRhC1hvUajVS3u1xpWxYRw3vz/y7x7UPuNWVCGDuAd2yNh9f08LlDweyKjqh8gMRUA6oy5zQPlE3vc32AAW84SccuUYXu+AqY+ZcL4RytHU1XCru4bP1WUXnRws6MtvM010kq9U9AQwr4w== johh.soo@johhsoo" ];
  };
}
