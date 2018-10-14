(define-module (stylish-haskell)
  #:use-module (ghc-mtl)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system haskell)
  #:use-module (gnu packages haskell)
  #:use-module (gnu packages haskell-check)
  #:use-module (gnu packages haskell-web))

(define-public stylish-haskell
  (package
   (name "stylish-haskell")
   (version "0.9.2.0")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://hackage.haskell.org/package/stylish-haskell/stylish-haskell-"
           version
           ".tar.gz"))
     (sha256
      (base32
       "16r2nm1y0s5ybrq2pzsq00nfq7k0i70nyg7ynmhx76lld82i17yv"))))
   (build-system haskell-build-system)
   (inputs
    `(("ghc-aeson" ,ghc-aeson)
      ("ghc-file-embed" ,ghc-file-embed)
      ("ghc-haskell-src-exts" ,ghc-haskell-src-exts)
      ("ghc-mtl" ,ghc-mtl)
      ("ghc-semigroups" ,ghc-semigroups)
      ("ghc-syb" ,ghc-syb)
      ("ghc-yaml" ,ghc-yaml)
      ("ghc-strict" ,ghc-strict)
      ("ghc-optparse-applicative"
       ,ghc-optparse-applicative)
      ("ghc-hunit" ,ghc-hunit)
      ("ghc-test-framework" ,ghc-test-framework)
      ("ghc-test-framework-hunit"
       ,ghc-test-framework-hunit)))
   (home-page
    "https://github.com/jaspervdj/stylish-haskell")
   (synopsis "Haskell code prettifier")
   (description
    "A Haskell code prettifier. For more information, see: . <https://github.com/jaspervdj/stylish-haskell/blob/master/README.markdown>")
   (license license:bsd-3)))

(define ghc-file-embed
  (package
   (name "ghc-file-embed")
   (version "0.0.10.1")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://hackage.haskell.org/package/file-embed/file-embed-"
           version
           ".tar.gz"))
     (sha256
      (base32
       "0lj164cnzqyd487mli91nnr7137a4h4qsasfwsnsh77sx12fpk9k"))))
   (build-system haskell-build-system)
   (home-page
    "https://github.com/snoyberg/file-embed")
   (synopsis
    "Use Template Haskell to embed file contents directly.")
   (description
    "Use Template Haskell to read a file or all the files in a directory, and turn them into (path, bytestring) pairs embedded in your Haskell code.")
   (license license:bsd-3)))


