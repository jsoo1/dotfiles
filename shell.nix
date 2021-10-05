{ pkgs ? import ./pin.nix }: pkgs.mkShell {
  name = "dotfiles-shell";
  packages = with pkgs; [ nixfmt home-manager ];
}
