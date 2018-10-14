(define-module (hoogle)
  #:use-module (ghc-mtl)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system haskell)
  #:use-module (gnu packages haskell)
  #:use-module (gnu packages haskell-check)
  #:use-module (gnu packages haskell-crypto)
  #:use-module (gnu packages haskell-web)
  #:use-module (gnu packages tls))

;; PUBLIC
(define-public hoogle
  (package
   (name "hoogle")
   (version "5.0.17.3")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "https://hackage.haskell.org/package/hoogle/hoogle-" version ".tar.gz"))
     (sha256 (base32 "174gp41v0krzj37m75pnr3aawyhkbk2wq4q6zk2z3zh0avvvmgk6"))))
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
      ("ghc-uniplate" ,ghc-uniplate)
      ("ghc-utf8-string" ,ghc-utf8-string)
      ("ghc-vector" ,ghc-vector)
      ("ghc-wai" ,ghc-wai)
      ("ghc-wai-logger" ,ghc-wai-logger)
      ("ghc-warp" ,ghc-warp)
      ("ghc-warp-tls" ,ghc-warp-tls)
      ("ghc-zlib" ,ghc-zlib)))
   (arguments `(#:tests? #f))
   (home-page "http://hoogle.haskell.org/")
   (synopsis "Haskell API Search")
   (description
    "Hoogle is a Haskell API search engine, which allows you to search many standard Haskell libraries by either function name, or by approximate type signature.")
   (license license:bsd-3)))

;; DEPENDENCIES
(define ghc-unsafe
  (package
   (name "ghc-unsafe")
   (version "0.0")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "https://hackage.haskell.org/package/unsafe/unsafe-" version ".tar.gz"))
     (sha256 (base32 "0hc6xr1i3hkz25gdgfx1jqgpsc9mwa05bkfynp0mcfdlyz6782nz"))))
   (build-system haskell-build-system)
   (arguments `(#:tests? #f))
   (home-page "http://code.haskell.org/~thielema/unsafe/")
   (synopsis "Unified interface to unsafe functions")
   (description
    "SafeHaskell introduced the notion of safe and unsafe modules. In order to make as many as possible modules \\\"safe\\\", the well-known unsafe functions were moved to distinguished modules. This makes it hard to write packages that work with both old and new versions of GHC. This package provides a single module @System.Unsafe@ that exports the unsafe functions from the base package. It provides them in a style ready for qualification, that is, you should import them by . > import qualified System.Unsafe as Unsafe . The package also contains a script called @rename-unsafe.sh@. It replaces all occurrences of the original identifiers with the qualified identifiers from this package. You still have to adapt the import commands. It uses the @darcs-replace-rec@ script from the @darcs-scripts@ package.")
   (license license:bsd-3)))

(define ghc-non-negative
  (package
   (name "ghc-non-negative")
   (version "0.1.2")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://hackage.haskell.org/package/non-negative/non-negative-" version ".tar.gz"))
     (sha256 (base32 "0f01q916dzkl1i0v15qrw9cviycki5g3fgi6x8gs45iwbzssq52n"))))
   (build-system haskell-build-system)
   (inputs
    `(("ghc-semigroups" ,ghc-semigroups)
      ("ghc-utility-ht" ,ghc-utility-ht)
      ("ghc-quickcheck" ,ghc-quickcheck)))
   (arguments `(#:tests? #f))
   (home-page "http://code.haskell.org/~thielema/non-negative/")
   (synopsis "Non-negative numbers")
   (description
    "Provides a class for non-negative numbers, a wrapper which can turn any ordered numeric type into a member of that class, and a lazy number type for non-negative numbers (a generalization of Peano numbers). This library is used by the @event-list@ package.")
   ;; TODO Fix this license to be the right one
   (license license:gpl3+)))

(define ghc-timeit
  (package
   (name "ghc-timeit")
   (version "2.0")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "https://hackage.haskell.org/package/timeit/timeit-" version ".tar.gz"))
     (sha256 (base32 "1sliqpvl501rlcj6s0lhmsf5ym24j4h881wzc1f1wdyvg3jz8kd1"))))
   (build-system haskell-build-system)
   (arguments `(#:tests? #f))
   (home-page "https://github.com/merijn/timeit")
   (synopsis "Time monadic computations with an IO base.")
   (description "A simple wrapper to show the used CPU time of monadic computation with an IO base.")
   (license license:bsd-3)))

(define ghc-storablevector
  (package
   (name "ghc-storablevector")
   (version "0.2.13")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://hackage.haskell.org/package/storablevector/storablevector-" version ".tar.gz"))
     (sha256 (base32 "1zmr738vwnhnyxbikayqnaz31ilv2qlmscp6iqgl7adcfbal4dzq"))))
   (build-system haskell-build-system)
   (inputs
    `(("ghc-non-negative" ,ghc-non-negative)
      ("ghc-utility-ht" ,ghc-utility-ht)
      ("ghc-semigroups" ,ghc-semigroups)
      ("ghc-unsafe" ,ghc-unsafe)
      ("ghc-quickcheck" ,ghc-quickcheck)
      ("ghc-syb" ,ghc-syb)))
   (arguments `(#:tests? #f))
   (home-page "http://www.haskell.org/haskellwiki/Storable_Vector")
   (synopsis "Fast, packed, strict storable arrays with a list interface like ByteString")
   (description
    "Fast, packed, strict storable arrays with a list interface, a chunky lazy list interface with variable chunk size and an interface for write access via the @ST@ monad. This is much like @bytestring@ and @binary@ but can be used for every 'Foreign.Storable.Storable' type. See also package <http://hackage.haskell.org/package/vector> with a similar intention. . We do not provide advanced fusion optimization, since especially for lazy vectors this would either be incorrect or not applicable. However we provide fusion with lazy lists in the package <http://hackage.haskell.org/package/storablevector-streamfusion>.")
   (license license:bsd-3)))

(define ghc-fmlist
  (package
   (name "ghc-fmlist")
   (version "0.9.2")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "https://hackage.haskell.org/package/fmlist/fmlist-" version ".tar.gz"))
     (sha256 (base32 "02868865hqm189h5wjd916abvqwkhbrx5b0119s1dwp70ifvbi4g"))))
   (build-system haskell-build-system)
   (arguments `(#:tests? #f))
   (home-page "https://github.com/sjoerdvisscher/fmlist")
   (synopsis "FoldMap lists")
   (description
    "FoldMap lists are lists represented by their foldMap function. FoldMap lists have O(1) cons, snoc and append, just like DLists, but other operations might have favorable performance characteristics as well. These wild claims are still completely unverified though.")
   (license license:bsd-3)))

(define ghc-storable-record
  (package
   (name "ghc-storable-record")
   (version "0.0.4")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://hackage.haskell.org/package/storable-record/storable-record-" version ".tar.gz"))
     (sha256 (base32 "0hjs1km0fc9ch0i1rbycxia5w3939hk4p4md73ikgg4aipqb5zyf"))))
   (build-system haskell-build-system)
   (inputs
    `(("ghc-semigroups" ,ghc-semigroups)
      ("ghc-utility-ht" ,ghc-utility-ht)
      ("ghc-storablevector" ,ghc-storablevector)
      ("ghc-timeit" ,ghc-timeit)))
   (arguments `(#:tests? #f))
   (home-page "http://code.haskell.org/~thielema/storable-record/")
   (synopsis "Elegant definition of Storable instances for records")
   (description
    "With this package you can build a Storable instance of a record type from Storable instances of its elements in an elegant way. It does not do any magic, just a bit arithmetic to compute the right offsets, that would be otherwise done manually or by a preprocessor like C2HS. I cannot promise that the generated memory layout is compatible with that of a corresponding C struct. However, the module generates the smallest layout that is possible with respect to the alignment of the record elements. If you encounter, that a record does not have a compatible layout, we should fix that. But also without C compatibility this package is useful e.g. in connection with StorableVector. . We provide Storable instance support for several cases: . * If you wrap a type in a @newtype@, then you can lift its 'Storable' instance to that @newtype@ with the module \"Foreign.Storable.Newtype\". This way you do not need the @GeneralizedNewtypeDeriving@ feature of GHC. . * If you have a type that is an instance of 'Traversable', you can use that feature for implementation of 'Storable' methods. The module \"Foreign.Storable.Traversable\" allows manipulation of the portion of your type, that is accessible by 'Traversable' methods. For instance with the type @data T a = Cons Int [a]@ and an according 'Traversable' implementation, you can load and store the elements of the contained list. This may be part of a 'Storable' implementation of the whole type. . * If you have a record containing elements of various types, then you need module \"Foreign.Storable.Record\". . Note however that the Storable instances defined with this package are quite slow in (up to) GHC-6.12.1. I'm afraid this is due to incomplete inlining, but we have still to investigate the problem. . For examples see packages @storable-tuple@ and @sample-frame@.")
   (license license:bsd-3)))

(define ghc-listlike
  (package
   (name "ghc-listlike")
   (version "4.6")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "https://hackage.haskell.org/package/ListLike/ListLike-" version ".tar.gz"))
     (sha256 (base32 "16jsj979mzjrgmpa20pls9ganym3wsps49paks1sb1gmlmwyrkf1"))))
   (build-system haskell-build-system)
   (inputs
    `(("ghc-vector" ,ghc-vector)
      ("ghc-dlist" ,ghc-dlist)
      ("ghc-fmlist" ,ghc-fmlist)
      ("ghc-utf8-string" ,ghc-utf8-string)))
   (arguments `(#:tests? #f))
   (home-page "http://github.com/JohnLato/listlike")
   (synopsis "Generic support for list-like structures")
   (description
    "Generic support for list-like structures in Haskell. . The ListLike module provides a common interface to the various Haskell types that are list-like.  Predefined interfaces include standard Haskell lists, Arrays, ByteStrings, and lazy ByteStrings.  Custom types can easily be made ListLike instances as well. . ListLike also provides for String-like types, such as String and ByteString, for types that support input and output, and for types that can handle infinite lists.")
   (license license:bsd-3)))

(define ghc-mtl
  (package
   (name "ghc-mtl")
   (version "2.2.2")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "https://hackage.haskell.org/package/mtl/mtl-" version ".tar.gz"))
     (sha256 (base32 "1xmy5741h8cyy0d91ahvqdz2hykkk20l8br7lg1rccnkis5g80w8"))))
   (build-system haskell-build-system)
   (arguments `(#:tests? #f))
   (home-page "http://github.com/haskell/mtl")
   (synopsis "Monad classes, using functional dependencies")
   (description
    "Monad classes using functional dependencies, with instances for various monad transformers, inspired by the paper /Functional Programming with Overloading and Higher-Order Polymorphism/, by Mark P Jones, in /Advanced School of Functional Programming/, 1995 (<http://web.cecs.pdx.edu/~mpj/pubs/springschool.html>).")
   (license license:bsd-3)))

(define ghc-storable-tuple
  (package
   (name "ghc-storable-tuple")
   (version "0.0.3.3")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://hackage.haskell.org/package/storable-tuple/storable-tuple-" version ".tar.gz"))
     (sha256 (base32 "0dfzhxgkn1l6ls7zh6iifhyvhm8l47n40z0ar23c6ibsa94w1ynw"))))
   (build-system haskell-build-system)
   (inputs
    `(("ghc-storable-record" ,ghc-storable-record)
      ("ghc-utility-ht" ,ghc-utility-ht)
      ("ghc-base-orphans" ,ghc-base-orphans)))
   (arguments `(#:tests? #f))
   (home-page "http://code.haskell.org/~thielema/storable-tuple/")
   (synopsis "Storable instance for pairs and triples")
   (description
    "Provides a Storable instance for pair and triple which should be binary compatible with C99 and C++. The only purpose of this package is to provide a standard location for this instance so that other packages needing this instance can play nicely together.")
   (license license:bsd-3)))

(define ghc-process-extras
  (package
   (name "ghc-process-extras")
   (version "0.7.4")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://hackage.haskell.org/package/process-extras/process-extras-" version ".tar.gz"))
     (sha256 (base32 "0klqgr37f1z2z6i0a9b0giapmq0p35l5k9kz1p7f0k1597w7agi9"))))
   (build-system haskell-build-system)
   (inputs
    `(("ghc-data-default" ,ghc-data-default)
      ("ghc-listlike" ,ghc-listlike)
      ("ghc-mtl" ,ghc-mtl)
      ("ghc-generic-deriving" ,ghc-generic-deriving)))
   (arguments `(#:tests? #f))
   (home-page "https://github.com/seereason/process-extras")
   (synopsis "Process extras")
   (description
    "Extends <http://hackage.haskell.org/package/process>. Read process input and output as ByteStrings or Text, or write your own ProcessOutput instance. Lazy process input and output.  ProcessMaker class for more flexibility in the process creation API.")
   (license license:expat)))

(define ghc-js-jquery
  (package
   (name "ghc-js-jquery")
   (version "3.3.1")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "https://hackage.haskell.org/package/js-jquery/js-jquery-" version ".tar.gz"))
     (sha256 (base32 "16q68jzbs7kp07dnq8cprdcc8fd41rim38039vg0w4x11lgniq70"))))
   (build-system haskell-build-system)
   (arguments `(#:tests? #f))
   (home-page "https://github.com/ndmitchell/js-jquery#readme")
   (synopsis "Obtain minified jQuery code")
   (description
    "This package bundles the minified <http://jquery.com/ jQuery> code into a Haskell package, so it can be depended upon by Cabal packages. The first three components of the version number match the upstream jQuery version. The package is designed to meet the redistribution requirements of downstream users (e.g. Debian).")
   (license license:expat)))

(define ghc-js-flot
  (package
   (name "ghc-js-flot")
   (version "0.8.3")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "https://hackage.haskell.org/package/js-flot/js-flot-" version ".tar.gz"))
     (sha256 (base32 "0yjyzqh3qzhy5h3nql1fckw0gcfb0f4wj9pm85nafpfqp2kg58hv"))))
   (build-system haskell-build-system)
   (arguments `(#:tests? #f))
   (home-page "https://github.com/ndmitchell/js-flot#readme")
   (synopsis "Obtain minified flot code")
   (description
    "This package bundles the minified <http://www.flotcharts.org/ Flot> code (a jQuery plotting library) into a Haskell package, so it can be depended upon by Cabal packages. The first three components of the version number match the upstream flot version. The package is designed to meet the redistribution requirements of downstream users (e.g. Debian).")
   (license license:expat)))

