(define-module (data-fix)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system haskell)
  #:use-module (gnu packages haskell))

(define-public ghc-data-fix
  (package
   (name "ghc-data-fix")
   (version "0.2.0")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://hackage.haskell.org/package/data-fix/data-fix-"
           version
           ".tar.gz"))
     (sha256
      (base32
       "14hk6hq5hdb3l5bhmzhw086jpzlvp9qbw9dzw30wlz5jbh2ihmvy"))))
   (build-system haskell-build-system)
   (home-page "https://github.com/anton-k/data-fix")
   (synopsis "Fixpoint data types")
   (description
    "Fixpoint types and recursion schemes. If you define your AST as fixpoint type, you get fold and unfold operations for free. . Thanks for contribution to: Matej Kollar, Herbert Valerio Riedel")
   (license license:bsd-3)))
