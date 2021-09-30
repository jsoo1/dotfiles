let
  # 2021-08-15
  nix-rev = "3ab8ce12c2db31268f579c11727d9c63cfee2eee";
  nix-src = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${nix-rev}.tar.gz";
    sha256 = "13fx0yb95gxyk0jyvgrxv2yp4fj54g7nzrlvjfc8slx9ccqd2v86";
  };
in import nix-src {
  overlays = import ./nix/restream.nix ++ import ./nix/emacs.nix;
}
