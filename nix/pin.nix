args:
let
  # 2021-11-22
  nix-rev = "67e5945d357ffa2d9bf7aa40fa59ddfd99513f12";
  nix-src = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${nix-rev}.tar.gz";
    sha256 = "03337mz0gjv48kdx0qij9in913m8wqzjjm579iblihanqrij7mxd";
  };
in
import nix-src
  (args // { overlays = import ./restream.nix ++ import ./my-emacs.nix; })
