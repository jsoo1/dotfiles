(use-modules (guix download)
             ((guix licenses) #:prefix license:)
             (guix packages)
             (guix build-system)
             (guix build-system haskell)
             (gnu packages haskell)
             (gnu packages haskell-check))

(define ghc-boot-th
  (package
   (name "ghc-boot-th")
   (version "8.0.2")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://hackage.haskell.org/package/ghc-boot-th/ghc-boot-th-"
           version
           ".tar.gz"))
     (sha256
      (base32
       "1w7qkgwpbp5h0hm8p2b5bbysyvnjrqbkqkfzd4ngz0yxy9qy402x"))))
   (build-system haskell-build-system)
   (home-page
    "http://hackage.haskell.org/package/ghc-boot-th")
   (synopsis
    "Shared functionality between GHC and the @template-haskell@ library")
   (description
    "This library contains various bits shared between the @ghc@ and @template-haskell@ libraries. . This package exists to ensure that @template-haskell@ has a minimal set of transitive dependencies, since it is intended to be depended upon by user code.")
   (license license:bsd-3)) )

(define ghc-boot
  (package
   (name "ghc-boot")
   (version "8.0.2")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://hackage.haskell.org/package/ghc-boot/ghc-boot-"
           version
           ".tar.gz"))
     (sha256
      (base32
       "0q446bcz38rql96k42yvfyhdg98lycijva1smw2izwv04hx200zp"))))
   (build-system haskell-build-system)
   (inputs `(("ghc-boot-th" ,ghc-boot-th)))
   (home-page
    "http://hackage.haskell.org/package/ghc-boot")
   (synopsis
    "Shared functionality between GHC and its boot libraries")
   (description
    "This library is shared between GHC, ghc-pkg, and other boot libraries. . A note about \"GHC.PackageDb\": it only deals with the subset of the package database that the compiler cares about: modules paths etc and not package metadata like description, authors etc. It is thus not a library interface to ghc-pkg and is __not__ suitable for modifying GHC package databases. . The package database format and this library are constructed in such a way that while ghc-pkg depends on Cabal, the GHC library and program do not have to depend on Cabal.")
   (license license:bsd-3)))

(define ghc-exactprint
  (package
   (name "ghc-exactprint")
   (version "0.5.6.1")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://hackage.haskell.org/package/ghc-exactprint/ghc-exactprint-"
           version
           ".tar.gz"))
     (sha256
      (base32
       "141k6qiys0m0r4br7ikp4i546vs3xcil9cwglzcdfcbnb5nj1z87"))))
   (build-system haskell-build-system)
   (inputs
    `(("ghc-paths" ,ghc-paths)
      ("ghc-mtl" ,ghc-mtl)
      ("ghc-syb" ,ghc-syb)
      ("ghc-free" ,ghc-free)
      ("ghc-boot" ,ghc-boot)
      ("ghc-hunit" ,ghc-hunit)
      ("ghc-diff" ,ghc-diff)
      ("ghc-silently" ,ghc-silently)
      ("ghc-filemanip" ,ghc-filemanip)))
   (home-page
    "http://hackage.haskell.org/package/ghc-exactprint")
   (synopsis "ExactPrint for GHC")
   (description
    "Using the API Annotations available from GHC 7.10.2, this library provides a means to round trip any code that can be compiled by GHC, currently excluding lhs files. . It does this with a phased approach . * Delta - converts GHC API Annotations into relative offsets, indexed by SrcSpan . * Transform - functions to facilitate changes to the AST, adjusting the annotations generated in the Delta phase to suit the changes. . * Print - converts an AST and its annotations to properly formatted source text. . * Pretty - adds annotations to an AST (fragment) so that the output can be parsed back to the same AST. . . Note: requires GHC 7.10.2 or later")
   (license license:bsd-3)))

(package
 (name "ghc-apply-refact")
 (version "0.5.0.0")
 (source
  (origin
   (method url-fetch)
   (uri (string-append
         "https://hackage.haskell.org/package/apply-refact/apply-refact-"
         version
         ".tar.gz"))
   (sha256
    (base32
     "1bvlbchpma3vlxfvjbyd01rmzqc9h5q3my9n7v3wal2p7ysvjpqz"))))
 (build-system haskell-build-system)
 (inputs
  `(("ghc-refact" ,ghc-refact)
    ("ghc-exactprint" ,ghc-exactprint)
    ("ghc-syb" ,ghc-syb)
    ("ghc-mtl" ,ghc-mtl)
    ("ghc-temporary" ,ghc-temporary)
    ("ghc-filemanip" ,ghc-filemanip)
    ("ghc-unix-compat" ,ghc-unix-compat)
    ("ghc-optparse-applicative"
     ,ghc-optparse-applicative)
    ("ghc-tasty" ,ghc-tasty)
    ("ghc-tasty-golden" ,ghc-tasty-golden)
    ("ghc-tasty-expected-failure"
     ,ghc-tasty-expected-failure)
    ("ghc-silently" ,ghc-silently)))
 (home-page
  "http://hackage.haskell.org/package/apply-refact")
 (synopsis
  "Perform refactorings specified by the refact library.")
 (description
  "Perform refactorings specified by the refact library. It is primarily used with HLint's --refactor flag.")
 (license license:bsd-3))
