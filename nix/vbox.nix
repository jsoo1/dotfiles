{ lib, pkgs, config, ... }: {
  imports = [
    "${config.nixpkgs}/nixos/modules/virtualization/virtualbox-image.nix"
    ./configuration.nix
  ];
  
  # environment.defaultPackages = [
  #   (pkgs.writeShellScriptBin "switch-to-configuration" ''
  #     set -e
  #     action="$1"
  #     shift
  #     config=$(nix-build --no-out-link /etc/nixos --attr toplevel "$@")
  #     nix-env --profile /nix/var/nix/profiles/system --set $config
  #     $config/bin/switch-to-configuration "$action"
  #   '')
  # ];
  
  # cribbed from  installer/virtualbox-demo.nix
  # FIXME: UUID detection is currently broken
  boot.loader.grub.fsIdentifier = "provided";
  
  # Add some more video drivers to give X11 a shot at working in
  # VMware and QEMU.
  services.xserver.videoDrivers = lib.mkOverride 40 [ "virtualbox" "vmware" "cirrus" "vesa" "modesetting" ];
  
  powerManagement.enable = false;
  
  system.stateVersion = "22.05";
}
