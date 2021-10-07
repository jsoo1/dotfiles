{ pkgs ? import ./pin.nix }:
pkgs.mkShell {
  name = "dotfiles-shell";
  packages = with pkgs;
    [ nixfmt ] ++ (if builtins.currentSystem == "x86_64-darwin" then
      [ home-manager ]
    else
      [ ]);
}
