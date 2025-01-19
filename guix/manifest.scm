(add-to-load-path (dirname (current-filename)))

(define-module (manifest)
 #:use-module (gnu packages)
 #:use-module (guix profiles)
 #:use-module (my-packages)
 #:use-module (dmenu)
 #:use-module (xmonad))

(define-public default
  (concatenate-manifests
   `(,(specifications->manifest
       (append
        languages
        utilities
        browsers
        desktop-tools
        fonts
        c-tools
        go-tools
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
         ,my-dmenu)))))

default
