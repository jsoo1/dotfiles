let pkgs = import ./nix/pin.nix { };
in pkgs.mkShell {
  name = "dotfiles-shell";
  packages = with pkgs; [ home-manager ];
}
