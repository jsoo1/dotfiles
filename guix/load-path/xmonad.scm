(define-module (xmonad)
  #:use-module (ghc-mtl)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages haskell)
  #:use-module (gnu packages haskell-check)
  #:use-module (gnu packages wm)
  #:use-module (guix build-system haskell)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))

;; DEPENDENCIES
(define ghc-semigroups
  (package
   (name "ghc-semigroups")
   (version "0.18.5")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://hackage.haskell.org/package/semigroups/semigroups-" version ".tar.gz"))
     (sha256 (base32 "17g29h62g1k51ghhvzkw72zksjgi6vs6bfipkj81pqw1dsprcamb"))))
   (build-system haskell-build-system)
   (home-page "http://github.com/ekmett/semigroups/")
   (synopsis "Anything that associates")
   (description
    "In mathematics, a semigroup is an algebraic structure consisting of a set together with an associative binary operation. A semigroup generalizes a monoid in that there might not exist an identity element. It also (originally) generalized a group (a monoid with all inverses) to a type where every element did not have to have an inverse, thus the name semigroup.")
   (license license:bsd-3)))

;; PUBLIC
(define-public my-xmonad
  (package
   (name "my-xmonad")
   (version "0.15")
   (synopsis "Tiling window manager")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "mirror://hackage/package/xmonad/" "xmonad-" version ".tar.gz"))
     (sha256 (base32 "0a7rh21k9y6g8fwkggxdxjns2grvvsd5hi2ls4klmqz5xvk4hyaa"))))
   (build-system haskell-build-system)
   (inputs
    `(("ghc-extensible-exceptions" ,ghc-extensible-exceptions)
      ("ghc-mtl"                   ,ghc-mtl)
      ("ghc-quickcheck"            ,ghc-quickcheck)
      ("ghc-setlocale"             ,ghc-setlocale)
      ("ghc-utf8-string"           ,ghc-utf8-string)
      ("ghc-x11"                   ,ghc-x11)))
   (propagated-inputs `(("gcc-toolchain" ,gcc-toolchain)))
   (arguments
    `(#:phases
      (modify-phases %standard-phases
                     (add-after
                      'install 'install-xsession
                      (lambda _
                        (let* ((xsessions (string-append %output "/share/xsessions")))
                          (mkdir-p xsessions)
                          (call-with-output-file
                              (string-append xsessions "/xmonad.desktop")
                            (lambda (port)
                              (format port "~
                    [Desktop Entry]~@
                    Name=~a~@
                    Comment=~a~@
                    Exec=~a/bin/xmonad~@
                    Type=Application~%" ,name ,synopsis %output)))))))))
   (home-page "http://xmonad.org")
   (description
    "My packaging of xmonad to fix deps on gcc tools.")
   (license license:bsd-3)))

(define-public my-ghc-xmonad-contrib
  (package
   (name "my-ghc-xmonad-contrib")
   (version "0.15")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "mirror://hackage/package/xmonad-contrib/"
                         "xmonad-contrib-" version ".tar.gz"))
     (sha256
      (base32
       "0r9yzgy67j4mi3dyxx714f0ssk5qzca5kh4zw0fhiz1pf008cxms"))))
   (build-system haskell-build-system)
   (propagated-inputs
    `(("ghc-mtl" ,ghc-mtl)
      ("ghc-old-time" ,ghc-old-time)
      ("ghc-random" ,ghc-random)
      ("ghc-utf8-string" ,ghc-utf8-string)
      ("ghc-extensible-exceptions" ,ghc-extensible-exceptions)
      ("ghc-semigroups" ,ghc-semigroups)
      ("ghc-x11" ,ghc-x11)
      ("ghc-x11-xft" ,ghc-x11-xft)
      ("xmonad" ,my-xmonad)))
   (home-page "https://xmonad.org")
   (synopsis "Third party extensions for xmonad")
   (description
    "Third party tiling algorithms, configurations, and scripts to Xmonad, a
tiling window manager for X.")
   (license license:bsd-3)))


