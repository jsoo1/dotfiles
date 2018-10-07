(define-module (xmonad)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages haskell)
  #:use-module (gnu packages haskell-check)
  #:use-module (gnu packages wm)
  #:use-module (guix build-system haskell)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))

(define-public my-xmonad
  (package
   (name "my-xmonad")
   (version "0.14")
   (synopsis "Tiling window manager")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://hackage/package/xmonad/"
                                "xmonad-" version ".tar.gz"))
            (sha256
             (base32
              "0lq3k0ap7jxrrswpd954mqa6h8diccbif5srcgbmr39y6y8x0mm4"))))
   (build-system haskell-build-system)
   (inputs
    `(("ghc-extensible-exceptions" ,ghc-extensible-exceptions)
      ("ghc-mtl"                   ,ghc-mtl)
      ("ghc-quickcheck"            ,ghc-quickcheck)
      ("ghc-setlocale"             ,ghc-setlocale)
      ("ghc-utf8-string"           ,ghc-utf8-string)
      ("ghc-x11"                   ,ghc-x11)))
   (propagated-inputs `(("gcc-toolchain"             ,gcc-toolchain)
                        ("ghc-xmonad-contrib"        ,ghc-xmonad-contrib)))
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
