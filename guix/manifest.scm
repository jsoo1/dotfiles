(add-to-load-path (dirname (current-filename)))
(use-modules (gnu packages) (guix profiles) (my-packages) (dmenu) (xmonad))

(concatenate-manifests
 `(,(specifications->manifest
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
   ,(packages->manifest
     `(,my-xmonad
       ,my-dmenu))))
