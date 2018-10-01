(define-module (termonad)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages haskell)
  #:use-module (guix download)
  #:use-module (guix modules)
  #:use-module (guix build-system)
  #:use-module (guix build-system haskell)
  #:use-module ((guix licenses) #:prefix license:))

;; Termonad

(define-public termonad
  (package
   (name "ghc-termonad")
   (version "0.2.1.0")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://hackage.haskell.org/package/termonad/termonad-"
           version
           ".tar.gz"))
     (sha256
      (base32
       "1js9sj0gj4igigdnbc5ygbn5l2wfhbrm1k565y3advi99imidsd3"))))
   (build-system haskell-build-system)
   (inputs
    `(("ghc-classy-prelude" ,ghc-classy-prelude)
      ("ghc-colour" ,ghc-colour)
      ("ghc-constraints" ,ghc-constraints)
      ("ghc-data-default" ,ghc-data-default)
      ("ghc-dyre" ,ghc-dyre)
      ("ghc-gi-gdk" ,ghc-gi-gdk)
      ("ghc-gi-gio" ,ghc-gi-gio)
      ("ghc-gi-glib" ,ghc-gi-glib)
      ("ghc-gi-gtk" ,ghc-gi-gtk)
      ("ghc-gi-pango" ,ghc-gi-pango)
      ("ghc-gi-vte" ,ghc-gi-vte)
      ("ghc-haskell-gi-base" ,ghc-haskell-gi-base)
      ("ghc-lens" ,ghc-lens)
      ("ghc-pretty-simple" ,ghc-pretty-simple)
      ("ghc-quickcheck" ,ghc-quickcheck)
      ("ghc-xml-conduit" ,ghc-xml-conduit)
      ("ghc-xml-html-qq" ,ghc-xml-html-qq)))
   (native-inputs
    `(("ghc-doctest" ,ghc-doctest)
      ("ghc-quickcheck" ,ghc-quickcheck)
      ("ghc-hedgehog" ,ghc-hedgehog)
      ("ghc-lens" ,ghc-lens)
      ("ghc-tasty" ,ghc-tasty)
      ("ghc-tasty-hedgehog" ,ghc-tasty-hedgehog)
      ("ghc-cabal-doctest" ,ghc-cabal-doctest)
      ("vte3" ,vte)
      ("gtk+" ,gtk+)
      ("gtk")))
   (home-page
    "https://github.com/cdepillabout/termonad")
   (synopsis
    "Terminal emulator configurable in Haskell")
   (description
    "Please see <https://github.com/cdepillabout/termonad#readme README.md>.")
   (license license:bsd-3)) )

;; Dependencies
