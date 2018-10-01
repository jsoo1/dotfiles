(define-module (hoogle)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system haskell)
  #:use-module (gnu packages haskell)
  #:use-module (gnu packages haskell-check)
  #:use-module (gnu packages haskell-crypto)
  #:use-module ((gnu packages haskell-web) #:prefix ghc-web:)
  #:use-module (gnu packages tls))

;;; DEPENDENCIES
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
   (license license:bsd-3)))

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

(define ghc-apply-refact
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
      ("ghc-optparse-applicative" ,ghc-optparse-applicative)
      ("ghc-tasty" ,ghc-tasty)
      ("ghc-tasty-golden" ,ghc-tasty-golden)
      ("ghc-tasty-expected-failure" ,ghc-tasty-expected-failure)
      ("ghc-silently" ,ghc-silently)))
   (home-page
    "http://hackage.haskell.org/package/apply-refact")
   (synopsis
    "Perform refactorings specified by the refact library.")
   (description
    "Perform refactorings specified by the refact library. It is primarily used with HLint's --refactor flag.")
   (license license:bsd-3)))

(define ghc-js-jquery
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
   (arguments
    `(#:tests? #f))
   (inputs `(("ghc-http" ,ghc-web:ghc-http)))
   (home-page
    "https://github.com/ndmitchell/js-jquery#readme")
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
     (uri (string-append
           "https://hackage.haskell.org/package/js-flot/js-flot-"
           version
           ".tar.gz"))
     (sha256
      (base32
       "0yjyzqh3qzhy5h3nql1fckw0gcfb0f4wj9pm85nafpfqp2kg58hv"))))
   (build-system haskell-build-system)
   (inputs `(("ghc-http" ,ghc-web:ghc-http)))
   (home-page
    "https://github.com/ndmitchell/js-flot#readme")
   (synopsis "Obtain minified flot code")
   (description
    "This package bundles the minified <http://www.flotcharts.org/ Flot> code (a jQuery plotting library) into a Haskell package, so it can be depended upon by Cabal packages. The first three components of the version number match the upstream flot version. The package is designed to meet the redistribution requirements of downstream users (e.g. Debian).")
   (license license:expat)))

(define ghc-process-extras
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

(define ghc-storablevector
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

(define ghc-non-negative
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

(define ghc-storable-record
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

(define ghc-storable-tuple
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

(define ghc-listlike
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

(define ghc-fmlist
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

(define ghc-timeit
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

(define ghc-unsafe
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

(define ghc-process
  (package
   (name "ghc-process")
   (version "1.6.4.0")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://hackage.haskell.org/package/process/process-"
           version
           ".tar.gz"))
     (sha256
      (base32
       "1w12ssmwpz5glmm964rxdc3fgi2w5sq9lac17rxs8p626r5p6rkn"))))
   (build-system haskell-build-system)
   (home-page
    "http://hackage.haskell.org/package/process")
   (synopsis "Process libraries")
   (description
    "This package contains libraries for dealing with system processes. . The typed-process package is a more recent take on a process API, which uses this package internally. It features better binary support, easier concurrency, and a more composable API. You can read more about it at <https://haskell-lang.org/library/typed-process>.")
   (license license:bsd-3)))

(define ghc-concurrent-output
  (package
   (name "ghc-concurrent-output")
   (version "1.10.7")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://hackage.haskell.org/package/concurrent-output/concurrent-output-"
           version
           ".tar.gz"))
     (sha256
      (base32
       "0w5x81n9ljs8l2b8ypy2naazvrv16qqlm1lfzvsksnii2nm1al30"))))
   (build-system haskell-build-system)
   (inputs
    `(("ghc-text" ,ghc-text)
      ("ghc-async" ,ghc-async)
      ("ghc-stm" ,ghc-stm)
      ("ghc-exceptions" ,ghc-exceptions)
      ("ghc-ansi-terminal" ,ghc-ansi-terminal)
      ("ghc-process" ,ghc-process)
      ("ghc-terminal-size" ,ghc-terminal-size)))
   (home-page
    "http://hackage.haskell.org/package/concurrent-output")
   (synopsis
    "Ungarble output from several threads or commands")
   (description
    "Lets multiple threads and external processes concurrently output to the console, without it getting all garbled up. . Built on top of that is a way of defining multiple output regions, which are automatically laid out on the screen and can be individually updated by concurrent threads. Can be used for progress displays etc. . <<https://joeyh.name/code/concurrent-output/demo2.gif>>")
   (license license:bsd-2)))

(define ghc-wl-pprint-annotated
  (package
   (name "ghc-wl-pprint-annotated")
   (version "0.1.0.1")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://hackage.haskell.org/package/wl-pprint-annotated/wl-pprint-annotated-"
           version
           ".tar.gz"))
     (sha256
      (base32
       "1br7qyf27iza213inwhf9bm2k6in0zbmfw6w4clqlc9f9cj2nrkb"))))
   (build-system haskell-build-system)
   (inputs `(("ghc-text" ,ghc-text)))
   (native-inputs
    `(("ghc-tasty" ,ghc-tasty)
      ("ghc-tasty-hunit" ,ghc-tasty-hunit)
      ("ghc-text" ,ghc-text)))
   (home-page
    "https://github.com/minad/wl-pprint-annotated#readme")
   (synopsis
    "Pretty printer with annotation support")
   (description
    "Wadler/Leijen pretty printer with support for annotations and modernized API. Annotations are useful for coloring. See wl-pprint-console.")
   (license license:bsd-3)))

(define ghc-hedgehog
  (package
   (name "ghc-hedgehog")
   (version "0.5.3")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://hackage.haskell.org/package/hedgehog/hedgehog-"
           version
           ".tar.gz"))
     (sha256
      (base32
       "1vv8vnkv6d0fvm0vwfm11ajyd9da3hfy2wdkd4p7dhfyscq9cwx4"))))
   (build-system haskell-build-system)
   (inputs
    `(("ghc-ansi-terminal" ,ghc-ansi-terminal)
      ("ghc-async" ,ghc-async)
      ("ghc-concurrent-output" ,ghc-concurrent-output)
      ("ghc-exceptions" ,ghc-exceptions)
      ("ghc-lifted-async" ,ghc-lifted-async)
      ("ghc-mmorph" ,ghc-mmorph)
      ("ghc-monad-control" ,ghc-monad-control)
      ("ghc-mtl" ,ghc-mtl)
      ("ghc-pretty-show" ,ghc-pretty-show)
      ("ghc-primitive" ,ghc-primitive)
      ("ghc-random" ,ghc-random)
      ("ghc-resourcet" ,ghc-resourcet)
      ("ghc-semigroups" ,ghc-semigroups)
      ("ghc-stm" ,ghc-stm)
      ("ghc-text" ,ghc-text)
      ("ghc-th-lift" ,ghc-th-lift)
      ("ghc-transformers-base" ,ghc-transformers-base)
      ("ghc-wl-pprint-annotated" ,ghc-wl-pprint-annotated)))
   (native-inputs
    `(("ghc-pretty-show" ,ghc-pretty-show)
      ("ghc-semigroups" ,ghc-semigroups)
      ("ghc-text" ,ghc-text)))
   (home-page "https://hedgehog.qa")
   (synopsis "Hedgehog will eat all your bugs.")
   (description
    "Hedgehog is a modern property-based testing system, in the spirit of QuickCheck. Hedgehog uses integrated shrinking, so shrinks obey the invariants of generated values by construction. . To get started quickly, see the examples: <https://github.com/hedgehogqa/haskell-hedgehog/tree/master/hedgehog-example>")
   (license license:bsd-3)))

(define ghc-tasty-hedgehog
  (package
   (name "ghc-tasty-hedgehog")
   (version "0.2.0.0")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://hackage.haskell.org/package/tasty-hedgehog/tasty-hedgehog-"
           version
           ".tar.gz"))
     (sha256
      (base32
       "10m1akbiblnjq9ljk469725k30b254d36d267rk51z2f171py42s"))))
   (build-system haskell-build-system)
   (inputs
    `(("ghc-tagged" ,ghc-tagged)
      ("ghc-tasty" ,ghc-tasty)
      ("ghc-hedgehog" ,ghc-hedgehog)))
   (native-inputs
    `(("ghc-tasty" ,ghc-tasty)
      ("ghc-tasty-expected-failure" ,ghc-tasty-expected-failure)
      ("ghc-hedgehog" ,ghc-hedgehog)))
   (home-page
    "https://github.com/qfpl/tasty-hedgehog")
   (synopsis "Integration for tasty and hedgehog.")
   (description
    "Integrates the hedgehog testing library with the tasty testing framework.")
   (license license:bsd-3)))

(define ghc-bsb-http-chunked
  (package
   (name "ghc-bsb-http-chunked")
   (version "0.0.0.4")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://hackage.haskell.org/package/bsb-http-chunked/bsb-http-chunked-"
           version
           ".tar.gz"))
     (sha256
      (base32
       "0z0f18yc6zlwh29c6175ivfcin325lvi4irpvv0n3cmq7vi0k0ql"))))
   (build-system haskell-build-system)
   (native-inputs
    `(("ghc-attoparsec" ,ghc-attoparsec)
      ("ghc-blaze-builder" ,ghc-blaze-builder)
      ("ghc-hedgehog" ,ghc-hedgehog)
      ("ghc-tasty" ,ghc-tasty)
      ("ghc-tasty-hedgehog" ,ghc-tasty-hedgehog)
      ("ghc-tasty-hunit" ,ghc-tasty-hunit)
      ("ghc-doctest" ,ghc-doctest)))
   (home-page
    "http://github.com/sjakobi/bsb-http-chunked")
   (synopsis
    "Chunked HTTP transfer encoding for bytestring builders")
   (description
    "This library contains functions for encoding [bytestring builders](http://hackage.haskell.org/package/bytestring/docs/Data-ByteString-Builder.html#t:Builder) for [chunked HTTP\\/1.1 transfer](https://en.wikipedia.org/wiki/Chunked_transfer_encoding). . This functionality was extracted from the [blaze-builder](http://hackage.haskell.org/package/blaze-builder) package.")
   (license license:bsd-3)))

(define ghc-streaming-commons-0.1.19
  (package
   (name "ghc-streaming-commons-0.1.19")
   (version "0.1.19")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "https://hackage.haskell.org/package/"
                         "streaming-commons/streaming-commons-"
                         version ".tar.gz"))
     (sha256
      (base32
       "19qp8bnnfs31jk08991lmj3dywbjxh9iydriifbdjj2mvy8axz23"))))
   (build-system haskell-build-system)
   (inputs
    `(("ghc-async" ,ghc-async)
      ("ghc-blaze-builder" ,ghc-blaze-builder)
      ("ghc-network" ,ghc-network)
      ("ghc-random" ,ghc-random)
      ("ghc-stm" ,ghc-stm)
      ("ghc-text" ,ghc-text)
      ("ghc-zlib" ,ghc-zlib)))
   (native-inputs
    `(("ghc-quickcheck" ,ghc-quickcheck)
      ("ghc-hspec" ,ghc-hspec)
      ("hspec-discover" ,hspec-discover)))
   (home-page "https://hackage.haskell.org/package/streaming-commons")
   (synopsis "Conduit and pipes needed by some streaming data libraries")
   (description "This package provides low-dependency functionality commonly
needed by various Haskell streaming data libraries, such as @code{conduit} and
@code{pipe}s.")
   (license license:expat)))

(define ghc-warp-3.2.25
  (package
   (name "ghc-warp-3.2.25")
   (version "3.2.25")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "https://hackage.haskell.org/package/"
                         "warp-" version "/" "warp-" version
                         ".tar.gz"))
     (sha256
      (base32
       "0rl59bs99c3wwwyc1ibq0v11mkc7pxpy28r9hdlmjsqmdwn8y2vy"))))
   (build-system haskell-build-system)
   (arguments
    `(#:tests? #f)) ; FIXME: Test-Suite `spec` fails.
   (inputs
    `(("ghc-async" ,ghc-async)
      ("ghc-auto-update" ,ghc-auto-update)
      ("ghc-blaze-builder" ,ghc-blaze-builder)
      ("ghc-bsb-http-chunked" ,ghc-bsb-http-chunked)
      ("ghc-bytestring-builder" ,ghc-bytestring-builder)
      ("ghc-case-insensitive" ,ghc-case-insensitive)
      ("ghc-hashable" ,ghc-hashable)
      ("ghc-http-types" ,ghc-web:ghc-http-types)
      ("ghc-iproute" ,ghc-iproute)
      ("ghc-network" ,ghc-network)
      ("ghc-stm" ,ghc-stm)
      ("ghc-streaming-commons" ,ghc-streaming-commons-0.1.19)
      ("ghc-text" ,ghc-text)
      ("ghc-unix-compat" ,ghc-unix-compat)
      ("ghc-vault" ,ghc-vault)
      ("ghc-wai" ,ghc-web:ghc-wai)
      ("ghc-word8" ,ghc-word8)
      ("ghc-lifted-base" ,ghc-lifted-base)
      ("ghc-http-date" ,ghc-web:ghc-http-date)
      ("ghc-simple-sendfile" ,ghc-simple-sendfile)
      ("ghc-http2" ,ghc-web:ghc-http2)))
   (native-inputs
    `(("ghc-silently" ,ghc-silently)
      ("ghc-hspec" ,ghc-hspec)
      ("ghc-auto-update" ,ghc-auto-update)
      ("ghc-doctest" ,ghc-doctest)
      ("ghc-quickcheck" ,ghc-quickcheck)
      ("ghc-hunit" ,ghc-hunit)
      ("ghc-http" ,ghc-web:ghc-http)
      ("hspec-discover" ,hspec-discover)))
   (home-page "http://github.com/yesodweb/wai")
   (synopsis "HTTP server library for Haskell's WAI")
   (description "Warp is a server library for HTTP/1.x and HTTP/2
based WAI (Web Application Interface in Haskell).")
   (license license:expat)))

(define-public ghc-warp-tls-3.2.3
  (package
   (name "ghc-warp-tls-3.2.3")
   (version "3.2.3")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "https://hackage.haskell.org/package/"
                         "warp-tls-" version "/"
                         "warp-tls-" version ".tar.gz"))
     (sha256
      (base32
       "14m2bzk5ivz9gdpxlcj6qnh46f2lycm1ybdjnfkj2876zrqwii7m"))))
   (build-system haskell-build-system)
   (inputs
    `(("ghc-cryptonite" ,ghc-cryptonite)
      ("ghc-data-default-class" ,ghc-data-default-class)
      ("ghc-network" ,ghc-network)
      ("ghc-streaming-commons" ,ghc-streaming-commons-0.1.19)
      ("ghc-tls" ,ghc-tls)
      ("ghc-wai" ,ghc-web:ghc-wai)
      ("ghc-warp" ,ghc-warp-3.2.25)))
   (home-page "http://github.com/yesodweb/wai")
   (synopsis "SSL/TLS support for Warp")
   (description "This package provides SSL/TLS support for Warp,
a WAI handler, via the native Haskell TLS implementation.")
   (license license:expat)))

(define-public ghc-http-conduit-2.3.2
  (package
    (name  "ghc-http-conduit-2.3.2")
    (version "2.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "http-conduit-" version "/" "http-conduit-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1iay4hr0mj8brkxvgkv1liqa8irl9axfc3qhn8qsvcyq4n1l95km"))))
    (build-system haskell-build-system)
    ;; FIXME: `httpLbs TLS` in test-suite `test` fails with
    ;; ConnectionFailure getProtocolByName: does not exist (no such protocol
    ;; name: tcp)
    (arguments `(#:tests? #f))
    (inputs
     `(("ghc-aeson" ,ghc-web:ghc-aeson)
       ("ghc-resourcet" ,ghc-resourcet)
       ("ghc-conduit" ,ghc-conduit)
       ("ghc-conduit-extra" ,ghc-conduit-extra)
       ("ghc-http-types" ,ghc-web:ghc-http-types)
       ("ghc-lifted-base" ,ghc-lifted-base)
       ("ghc-http-client" ,ghc-web:ghc-http-client)
       ("ghc-http-client-tls" ,ghc-web:ghc-http-client-tls)
       ("ghc-monad-control" ,ghc-monad-control)
       ("ghc-mtl" ,ghc-mtl)
       ("ghc-exceptions" ,ghc-exceptions)
       ("ghc-unliftio" ,ghc-unliftio)))
    (native-inputs
     `(("ghc-hunit" ,ghc-hunit)
       ("ghc-hspec" ,ghc-hspec)
       ("ghc-data-default-class" ,ghc-data-default-class)
       ("ghc-connection" ,ghc-connection)
       ("ghc-warp-tls" ,ghc-warp-tls-3.2.3)
       ("ghc-blaze-builder" ,ghc-blaze-builder)
       ("ghc-text" ,ghc-text)
       ("ghc-conduit" ,ghc-conduit)
       ("ghc-utf8-string" ,ghc-utf8-string)
       ("ghc-case-insensitive" ,ghc-case-insensitive)
       ("ghc-lifted-base" ,ghc-lifted-base)
       ("ghc-network" ,ghc-network)
       ("ghc-wai" ,ghc-web:ghc-wai)
       ("ghc-warp" ,ghc-warp-3.2.25)
       ("ghc-wai-conduit" ,ghc-web:ghc-wai-conduit)
       ("ghc-http-types" ,ghc-web:ghc-http-types)
       ("ghc-http-client" ,ghc-web:ghc-http-client)
       ("ghc-cookie" ,ghc-web:ghc-cookie)
       ("ghc-conduit-extra" ,ghc-conduit-extra)
       ("ghc-streaming-commons" ,ghc-streaming-commons-0.1.19)
       ("ghc-aeson" ,ghc-web:ghc-aeson)
       ("ghc-temporary" ,ghc-temporary)
       ("ghc-resourcet" ,ghc-resourcet)))
    (home-page "https://hackage.haskell.org/package/http-conduit")
    (synopsis "HTTP/HTTPS client with conduit interface")
    (description "This library uses attoparsec for parsing the actual
contents of the HTTP connection.  It also provides higher-level functions
which allow you to avoid direct usage of conduits.")
    (license license:bsd-3)))

;;; PUBLIC
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
      ("ghc-aeson" ,ghc-web:ghc-aeson)
      ("ghc-cmdargs" ,ghc-cmdargs)
      ("ghc-conduit" ,ghc-conduit)
      ("ghc-conduit-extra" ,ghc-conduit-extra)
      ("ghc-connection" ,ghc-connection)
      ("ghc-extra" ,ghc-extra)
      ("ghc-old-locale" ,ghc-old-locale)
      ("ghc-haskell-src-exts" ,ghc-haskell-src-exts)
      ("ghc-http-conduit" ,ghc-http-conduit-2.3.2)
      ("ghc-http-types" ,ghc-web:ghc-http-types)
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
      ("ghc-wai" ,ghc-web:ghc-wai)
      ("ghc-wai-logger" ,ghc-web:ghc-wai-logger)
      ("ghc-warp" ,ghc-warp-3.2.25)
      ("ghc-warp-tls" ,ghc-warp-tls-3.2.3)
      ("ghc-zlib" ,ghc-zlib)))
   (home-page "http://hoogle.haskell.org/")
   (synopsis "Haskell API Search")
   (description
    "Hoogle is a Haskell API search engine, which allows you to search many standard Haskell libraries by either function name, or by approximate type signature.")
   (license license:bsd-3)))
