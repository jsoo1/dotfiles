(define-module (haskell)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system haskell)
  #:use-module (gnu packages haskell)
  #:use-module (gnu packages haskell-check)
  #:use-module (gnu packages haskell-web))

(define-public ghc-boot-th
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
   (license license:bsd-3)))

(define-public ghc-boot
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

(define-public ghc-exactprint
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

(define-public ghc-apply-refact
  (package
   (name "ghc-apply-refact")
   (version "0.3.0.1")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://hackage.haskell.org/package/apply-refact/apply-refact-"
           version
           ".tar.gz"))
     (sha256
      (base32
       "0578ql80fzkbjiw589v4498qd9jd7l2sz626imkdybxr1lqbsm0p"))))
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
   (license license:bsd-3)))

(define-public ghc-js-jquery
  (package
   (name "ghc-js-jquery")
   (version "3.3.1")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://hackage.haskell.org/package/js-jquery/js-jquery-"
           version
           ".tar.gz"))
     (sha256
      (base32
       "16q68jzbs7kp07dnq8cprdcc8fd41rim38039vg0w4x11lgniq70"))))
   (build-system haskell-build-system)
   (inputs `(("ghc-http" ,ghc-http)))
   (home-page
    "https://github.com/ndmitchell/js-jquery#readme")
   (synopsis "Obtain minified jQuery code")
   (description
    "This package bundles the minified <http://jquery.com/ jQuery> code into a Haskell package, so it can be depended upon by Cabal packages. The first three components of the version number match the upstream jQuery version. The package is designed to meet the redistribution requirements of downstream users (e.g. Debian).")
   (license license:expat)))

(define-public ghc-js-flot
  (package
   (name "ghc-js-flot")
   (version "0.8.3")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://hackage.haskell.org/package/js-flot/js-flot-"
           version
           ".tar.gz"))
     (sha256
      (base32
       "0yjyzqh3qzhy5h3nql1fckw0gcfb0f4wj9pm85nafpfqp2kg58hv"))))
   (build-system haskell-build-system)
   (inputs `(("ghc-http" ,ghc-http)))
   (home-page
    "https://github.com/ndmitchell/js-flot#readme")
   (synopsis "Obtain minified flot code")
   (description
    "This package bundles the minified <http://www.flotcharts.org/ Flot> code (a jQuery plotting library) into a Haskell package, so it can be depended upon by Cabal packages. The first three components of the version number match the upstream flot version. The package is designed to meet the redistribution requirements of downstream users (e.g. Debian).")
   (license license:expat)))

(define-public ghc-process-extras
  (package
   (name "ghc-process-extras")
   (version "0.7.4")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://hackage.haskell.org/package/process-extras/process-extras-"
           version
           ".tar.gz"))
     (sha256
      (base32
       "0klqgr37f1z2z6i0a9b0giapmq0p35l5k9kz1p7f0k1597w7agi9"))))
   (build-system haskell-build-system)
   (inputs
    `(("ghc-data-default" ,ghc-data-default)
      ("ghc-listlike" ,ghc-listlike)
      ("ghc-mtl" ,ghc-mtl)
      ("ghc-text" ,ghc-text)
      ("ghc-generic-deriving" ,ghc-generic-deriving)
      ("ghc-hunit" ,ghc-hunit)))
   (home-page
    "https://github.com/seereason/process-extras")
   (synopsis "Process extras")
   (description
    "Extends <http://hackage.haskell.org/package/process>. Read process input and output as ByteStrings or Text, or write your own ProcessOutput instance. Lazy process input and output.  ProcessMaker class for more flexibility in the process creation API.")
   (license license:expat)))

(define-public ghc-storablevector
  (package
   (name "ghc-storablevector")
   (version "0.2.13")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://hackage.haskell.org/package/storablevector/storablevector-"
           version
           ".tar.gz"))
     (sha256
      (base32
       "1zmr738vwnhnyxbikayqnaz31ilv2qlmscp6iqgl7adcfbal4dzq"))))
   (build-system haskell-build-system)
   (inputs
    `(("ghc-non-negative" ,ghc-non-negative)
      ("ghc-utility-ht" ,ghc-utility-ht)
      ("ghc-semigroups" ,ghc-semigroups)
      ("ghc-unsafe" ,ghc-unsafe)
      ("ghc-quickcheck" ,ghc-quickcheck)
      ("ghc-syb" ,ghc-syb)
      ("ghc-random" ,ghc-random)))
   (home-page
    "http://www.haskell.org/haskellwiki/Storable_Vector")
   (synopsis
    "Fast, packed, strict storable arrays with a list interface like ByteString")
   (description
    "Fast, packed, strict storable arrays with a list interface, a chunky lazy list interface with variable chunk size and an interface for write access via the @ST@ monad. This is much like @bytestring@ and @binary@ but can be used for every 'Foreign.Storable.Storable' type. See also package <http://hackage.haskell.org/package/vector> with a similar intention. . We do not provide advanced fusion optimization, since especially for lazy vectors this would either be incorrect or not applicable. However we provide fusion with lazy lists in the package <http://hackage.haskell.org/package/storablevector-streamfusion>.")
   (license license:bsd-3)))

(define-public ghc-non-negative
  (package
   (name "ghc-non-negative")
   (version "0.1.2")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://hackage.haskell.org/package/non-negative/non-negative-"
           version
           ".tar.gz"))
     (sha256
      (base32
       "0f01q916dzkl1i0v15qrw9cviycki5g3fgi6x8gs45iwbzssq52n"))))
   (build-system haskell-build-system)
   (inputs
    `(("ghc-semigroups" ,ghc-semigroups)
      ("ghc-utility-ht" ,ghc-utility-ht)
      ("ghc-quickcheck" ,ghc-quickcheck)))
   (home-page
    "http://code.haskell.org/~thielema/non-negative/")
   (synopsis "Non-negative numbers")
   (description
    "Provides a class for non-negative numbers, a wrapper which can turn any ordered numeric type into a member of that class, and a lazy number type for non-negative numbers (a generalization of Peano numbers). This library is used by the @event-list@ package.")
   (license "'gpl??")))

(define-public ghc-storable-record
  (package
   (name "ghc-storable-record")
   (version "0.0.4")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://hackage.haskell.org/package/storable-record/storable-record-"
           version
           ".tar.gz"))
     (sha256
      (base32
       "0hjs1km0fc9ch0i1rbycxia5w3939hk4p4md73ikgg4aipqb5zyf"))))
   (build-system haskell-build-system)
   (inputs
    `(("ghc-semigroups" ,ghc-semigroups)
      ("ghc-utility-ht" ,ghc-utility-ht)
      ("ghc-storablevector" ,ghc-storablevector)
      ("ghc-timeit" ,ghc-timeit)))
   (home-page
    "http://code.haskell.org/~thielema/storable-record/")
   (synopsis
    "Elegant definition of Storable instances for records")
   (description
    "With this package you can build a Storable instance of a record type from Storable instances of its elements in an elegant way. It does not do any magic, just a bit arithmetic to compute the right offsets, that would be otherwise done manually or by a preprocessor like C2HS. I cannot promise that the generated memory layout is compatible with that of a corresponding C struct. However, the module generates the smallest layout that is possible with respect to the alignment of the record elements. If you encounter, that a record does not have a compatible layout, we should fix that. But also without C compatibility this package is useful e.g. in connection with StorableVector. . We provide Storable instance support for several cases: . * If you wrap a type in a @newtype@, then you can lift its 'Storable' instance to that @newtype@ with the module \"Foreign.Storable.Newtype\". This way you do not need the @GeneralizedNewtypeDeriving@ feature of GHC. . * If you have a type that is an instance of 'Traversable', you can use that feature for implementation of 'Storable' methods. The module \"Foreign.Storable.Traversable\" allows manipulation of the portion of your type, that is accessible by 'Traversable' methods. For instance with the type @data T a = Cons Int [a]@ and an according 'Traversable' implementation, you can load and store the elements of the contained list. This may be part of a 'Storable' implementation of the whole type. . * If you have a record containing elements of various types, then you need module \"Foreign.Storable.Record\". . Note however that the Storable instances defined with this package are quite slow in (up to) GHC-6.12.1. I'm afraid this is due to incomplete inlining, but we have still to investigate the problem. . For examples see packages @storable-tuple@ and @sample-frame@.")
   (license license:bsd-3)))

(define-public ghc-storable-tuple
  (package
   (name "ghc-storable-tuple")
   (version "0.0.3.3")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://hackage.haskell.org/package/storable-tuple/storable-tuple-"
           version
           ".tar.gz"))
     (sha256
      (base32
       "0dfzhxgkn1l6ls7zh6iifhyvhm8l47n40z0ar23c6ibsa94w1ynw"))))
   (build-system haskell-build-system)
   (inputs
    `(("ghc-storable-record" ,ghc-storable-record)
      ("ghc-utility-ht" ,ghc-utility-ht)
      ("ghc-base-orphans" ,ghc-base-orphans)))
   (home-page
    "http://code.haskell.org/~thielema/storable-tuple/")
   (synopsis
    "Storable instance for pairs and triples")
   (description
    "Provides a Storable instance for pair and triple which should be binary compatible with C99 and C++. The only purpose of this package is to provide a standard location for this instance so that other packages needing this instance can play nicely together.")
   (license license:bsd-3)))

(define-public ghc-listlike
  (package
   (name "ghc-listlike")
   (version "4.6")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://hackage.haskell.org/package/ListLike/ListLike-"
           version
           ".tar.gz"))
     (sha256
      (base32
       "16jsj979mzjrgmpa20pls9ganym3wsps49paks1sb1gmlmwyrkf1"))))
   (build-system haskell-build-system)
   (inputs
    `(("ghc-text" ,ghc-text)
      ("ghc-vector" ,ghc-vector)
      ("ghc-dlist" ,ghc-dlist)
      ("ghc-fmlist" ,ghc-fmlist)
      ("ghc-utf8-string" ,ghc-utf8-string)
      ("ghc-semigroups" ,ghc-semigroups)
      ;; TODO: Understand this dependency
      ;; ("ghc-listlike" ,ghc-listlike)
      ("ghc-hunit" ,ghc-hunit)
      ("ghc-quickcheck" ,ghc-quickcheck)
      ("ghc-random" ,ghc-random)))
   (home-page "http://github.com/JohnLato/listlike")
   (synopsis
    "Generic support for list-like structures")
   (description
    "Generic support for list-like structures in Haskell. . The ListLike module provides a common interface to the various Haskell types that are list-like.  Predefined interfaces include standard Haskell lists, Arrays, ByteStrings, and lazy ByteStrings.  Custom types can easily be made ListLike instances as well. . ListLike also provides for String-like types, such as String and ByteString, for types that support input and output, and for types that can handle infinite lists.")
   (license license:bsd-3)))

(define-public ghc-fmlist
  (package
   (name "ghc-fmlist")
   (version "0.9.2")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://hackage.haskell.org/package/fmlist/fmlist-"
           version
           ".tar.gz"))
     (sha256
      (base32
       "02868865hqm189h5wjd916abvqwkhbrx5b0119s1dwp70ifvbi4g"))))
   (build-system haskell-build-system)
   (home-page
    "https://github.com/sjoerdvisscher/fmlist")
   (synopsis "FoldMap lists")
   (description
    "FoldMap lists are lists represented by their foldMap function. FoldMap lists have O(1) cons, snoc and append, just like DLists, but other operations might have favorable performance characteristics as well. These wild claims are still completely unverified though.")
   (license license:bsd-3)))

(define-public hoogle
  (package
   (name "hoogle")
   (version "5.0.17.2")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://hackage.haskell.org/package/hoogle/hoogle-"
           version
           ".tar.gz"))
     (sha256
      (base32
       "009brgwczkszmlk8rq6s5s73rnpi2gw94wr9wwp9rgf3r2cfs1br"))))
   (build-system haskell-build-system)
   (inputs
    `(("ghc-network-uri" ,ghc-network-uri)
      ("ghc-network" ,ghc-network)
      ("ghc-quickcheck" ,ghc-quickcheck)
      ("ghc-aeson" ,ghc-aeson)
      ("ghc-cmdargs" ,ghc-cmdargs)
      ("ghc-conduit" ,ghc-conduit)
      ("ghc-conduit-extra" ,ghc-conduit-extra)
      ("ghc-connection" ,ghc-connection)
      ("ghc-extra" ,ghc-extra)
      ("ghc-old-locale" ,ghc-old-locale)
      ("ghc-haskell-src-exts" ,ghc-haskell-src-exts)
      ("ghc-http-conduit" ,ghc-http-conduit)
      ("ghc-http-types" ,ghc-http-types)
      ("ghc-js-flot" ,ghc-js-flot)
      ("ghc-js-jquery" ,ghc-js-jquery)
      ("ghc-mmap" ,ghc-mmap)
      ("ghc-process-extras" ,ghc-process-extras)
      ("ghc-resourcet" ,ghc-resourcet)
      ("ghc-storable-tuple" ,ghc-storable-tuple)
      ("ghc-tar" ,ghc-tar)
      ("ghc-text" ,ghc-text)
      ("ghc-uniplate" ,ghc-uniplate)
      ("ghc-utf8-string" ,ghc-utf8-string)
      ("ghc-vector" ,ghc-vector)
      ("ghc-wai" ,ghc-wai)
      ("ghc-wai-logger" ,ghc-wai-logger)
      ("ghc-warp" ,ghc-warp)
      ("ghc-warp-tls" ,ghc-warp-tls)
      ("ghc-zlib" ,ghc-zlib)))
   (home-page "http://hoogle.haskell.org/")
   (synopsis "Haskell API Search")
   (description
    "Hoogle is a Haskell API search engine, which allows you to search many standard Haskell libraries by either function name, or by approximate type signature.")
   (license license:bsd-3)))

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

(define-public ghc-file-embed
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

(define-public ghc-timeit
  (package
   (name "ghc-timeit")
   (version "2.0")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://hackage.haskell.org/package/timeit/timeit-"
           version
           ".tar.gz"))
     (sha256
      (base32
       "1sliqpvl501rlcj6s0lhmsf5ym24j4h881wzc1f1wdyvg3jz8kd1"))))
   (build-system haskell-build-system)
   (home-page "https://github.com/merijn/timeit")
   (synopsis
    "Time monadic computations with an IO base.")
   (description
    "A simple wrapper to show the used CPU time of monadic computation with an IO base.")
   (license license:bsd-3)))

(define-public ghc-unsafe
  (package
   (name "ghc-unsafe")
   (version "0.0")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://hackage.haskell.org/package/unsafe/unsafe-"
           version
           ".tar.gz"))
     (sha256
      (base32
       "0hc6xr1i3hkz25gdgfx1jqgpsc9mwa05bkfynp0mcfdlyz6782nz"))))
   (build-system haskell-build-system)
   (home-page
    "http://code.haskell.org/~thielema/unsafe/")
   (synopsis
    "Unified interface to unsafe functions")
   (description
    "SafeHaskell introduced the notion of safe and unsafe modules. In order to make as many as possible modules \\\"safe\\\", the well-known unsafe functions were moved to distinguished modules. This makes it hard to write packages that work with both old and new versions of GHC. This package provides a single module @System.Unsafe@ that exports the unsafe functions from the base package. It provides them in a style ready for qualification, that is, you should import them by . > import qualified System.Unsafe as Unsafe . The package also contains a script called @rename-unsafe.sh@. It replaces all occurrences of the original identifiers with the qualified identifiers from this package. You still have to adapt the import commands. It uses the @darcs-replace-rec@ script from the @darcs-scripts@ package.")
   (license license:bsd-3)))

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
