[
  # checkmake
  (self: super: {
    checkmake =
      super.callPackage (import ../../projects/nix-overlays/checkmake.nix) { };
  })
  # emacs from head and more
  (import ../../projects/emacs-overlay/default.nix)
]
