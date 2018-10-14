(define-module (ghc-mtl)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system haskell)
  #:use-module (gnu packages haskell)
  #:use-module (gnu packages haskell-check))

(define-public ghc-mtl
  (package
   (name "ghc-mtl")
   (version "2.2.2")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://hackage.haskell.org/package/mtl/mtl-"
           version
           ".tar.gz"))
     (sha256
      (base32
       "1xmy5741h8cyy0d91ahvqdz2hykkk20l8br7lg1rccnkis5g80w8"))))
   (build-system haskell-build-system)
   (home-page "http://github.com/haskell/mtl")
   (synopsis
    "Monad classes, using functional dependencies")
   (description
    "Monad classes using functional dependencies, with instances for various monad transformers, inspired by the paper /Functional Programming with Overloading and Higher-Order Polymorphism/, by Mark P Jones, in /Advanced School of Functional Programming/, 1995 (<http://web.cecs.pdx.edu/~mpj/pubs/springschool.html>).")
   (license license:bsd-3)))
