(define-module (gnu packages hoogle)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix licenses)
  #:use-module (guix build-system haskell)
  #:use-module (gnu packages haskell)
  #:use-module (gnu packages haskell-check)
  #:use-module (gnu packages haskell-web))

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
   (inputs `(("ghc-http" ,ghc-http)))
   (home-page
    "https://github.com/ndmitchell/js-jquery#readme")
   (synopsis "Obtain minified jQuery code")
   (description
    "This package bundles the minified <http://jquery.com/ jQuery> code into a Haskell package, so it can be depended upon by Cabal packages. The first three components of the version number match the upstream jQuery version. The package is designed to meet the redistribution requirements of downstream users (e.g. Debian).")
   (license expat)))

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
   (inputs `(("ghc-http" ,ghc-http)))
   (home-page
    "https://github.com/ndmitchell/js-flot#readme")
   (synopsis "Obtain minified flot code")
   (description
    "This package bundles the minified <http://www.flotcharts.org/ Flot> code (a jQuery plotting library) into a Haskell package, so it can be depended upon by Cabal packages. The first three components of the version number match the upstream flot version. The package is designed to meet the redistribution requirements of downstream users (e.g. Debian).")
   (license expat)))

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
   (license expat)))

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
   (license bsd-3)))

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
      ("ghc-listlike" ,ghc-listlike)
      ("ghc-hunit" ,ghc-hunit)
      ("ghc-quickcheck" ,ghc-quickcheck)
      ("ghc-random" ,ghc-random)))
   (home-page "http://github.com/JohnLato/listlike")
   (synopsis
    "Generic support for list-like structures")
   (description
    "Generic support for list-like structures in Haskell. . The ListLike module provides a common interface to the various Haskell types that are list-like.  Predefined interfaces include standard Haskell lists, Arrays, ByteStrings, and lazy ByteStrings.  Custom types can easily be made ListLike instances as well. . ListLike also provides for String-like types, such as String and ByteString, for types that support input and output, and for types that can handle infinite lists.")
   (license bsd-3)))

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
   (license bsd-3)))

(define-public hoogle
  (package
   (name "ghc-hoogle")
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
   (license bsd-3)))
