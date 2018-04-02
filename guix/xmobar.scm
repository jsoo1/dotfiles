(define-module (gnu packages my-xmobar)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix licenses)
  #:use-module (guix build-system haskell)
  #:use-module (gnu packages haskell)
  #:use-module (gnu packages haskell-web))

(define-public my-xmobar
  (package
   (name "ghc-xmobar-extras")
   (version "0.25")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://hackage.haskell.org/package/xmobar/xmobar-"
           version
           ".tar.gz"))
     (sha256
      (base32
       "0382r4vzqkz76jlp2069rdbwf4gh1a22r9w4rkphcn5qflw0dlb6"))))
   (build-system haskell-build-system)
   (arguments '(#:configure-flags '("--flags=\"with-xft\""
                                    "--flags=\"with_utf8\""
                                    "--flags=\"with_threaded\""
                                    "--flags=\"with_alsa\""
                                    "--flags=\"with_weather\"")))
   (inputs
    `(("ghc-regex-compat" ,ghc-regex-compat)
      ("ghc-old-locale" ,ghc-old-locale)
      ("ghc-x11" ,ghc-x11)
      ("ghc-mtl" ,ghc-mtl)
      ("ghc-parsec" ,ghc-parsec)
      ("ghc-stm" ,ghc-stm)
      ("ghc-utf8-string" ,ghc-utf8-string)
      ("ghc-http" ,ghc-http)))
   (home-page "http://xmobar.org")
   (synopsis "A Minimalistic Text Based Status Bar")
   (description
    "Xmobar is a minimalistic text based status bar. . Inspired by the Ion3 status bar, it supports similar features, like dynamic color management, output templates, and extensibility through plugins.")
   (license bsd-3)))
