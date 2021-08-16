(add-to-load-path "/home/john/dotfiles/guix")
(use-modules (gnu packages) (my-packages))

(specifications->manifest
 (append
  languages
  utilities
  browsers
  desktop-tools
  fonts
  c-tools
  haskell-tools
  nix-tools
  ocaml-tools
  rust-tools
  guile-tools
  pdf-tools
  xorg-tools
  emacs-packages))
