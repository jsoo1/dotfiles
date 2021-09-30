let pkgs = import ./pin.nix;
in pkgs.mkShell {
  name = "dotfiles-shell";
  packages = with pkgs; [ nixfmt ];
}
