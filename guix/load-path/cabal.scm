(define-module (cabal)
  #:use-module (gnu packages haskell)
  #:use-module (gnu packages haskell-check)
  #:use-module (gnu packages haskell-crypto)
  #:use-module (gnu packages haskell-web)
  #:use-module (gnu packages python)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system haskell)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))

;;; DEPENDENCIES
(define ghc-cryptohash-sha256
  ;; FIXME: Do a better guix import from hackage. Account for changed metadata
  (package
   (name "ghc-cryptohash-sha256")
   (version "0.11.101.0")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "https://hackage.haskell.org/package/cryptohash-sha256/"
                         "cryptohash-sha256-" version ".tar.gz"))
     (sha256
      (base32
       "1p85vajcgw9hmq8zsz9krzx0vxh7aggwbg5w9ws8w97avcsn8xaj"))))
   (build-system haskell-build-system)
   (inputs
    `(("ghc-bytestring" ,ghc-bytestring)))
   (arguments
    ;; TODO fix the hackage import to include test dependencies
    `(#:tests? #f))
   (home-page "https://hackage.haskell.org/package/cryptohash-sha256")
   (synopsis "A practical incremental and one-pass, pure API to the SHA-256 cryptographic hash algorithm according to FIPS 180-4 with performance close to the fastest implementations available in other languages.")
   (description "")
   (license license:bsd-3)))

(define ghc-echo
  (package
   (name "ghc-echo")
   (version "0.1.3")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://hackage.haskell.org/package/echo/echo-"
           version
           ".tar.gz"))
     (sha256
      (base32
       "1vw5ykpwhr39wc0hhcgq3r8dh59zq6ib4zxbz1qd2wl21wqhfkvh"))))
   (build-system haskell-build-system)
   (home-page "https://github.com/RyanGlScott/echo")
   (synopsis
    "A cross-platform, cross-console way to handle echoing terminal input")
   (description
    "The @base@ library exposes the @hGetEcho@ and @hSetEcho@ functions for querying and setting echo status, but unfortunately, neither function works with MinTTY consoles on Windows. This is a serious issue, since @hGetEcho@ and @hSetEcho@ are often used to disable input echoing when a program prompts for a password, so many programs will reveal your password as you type it on MinTTY! . This library provides an alternative interface which works with both MinTTY and other consoles. An example is included which demonstrates how one might prompt for a password using this library. To build it, make sure to configure with the @-fexample@ flag.")
   (license license:bsd-3)))

(define-public ghc-bytestring
  (package
   (name "ghc-bytestring")
   (version "0.10.8.2")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://hackage.haskell.org/package/bytestring/bytestring-"
           version
           ".tar.gz"))
     (sha256
      (base32
       "0fjc5ybxx67l0kh27l6vq4saf88hp1wnssj5ka90ii588y76cvys"))))
   (build-system haskell-build-system)
   (native-inputs
    `(("ghc-byteorder" ,ghc-byteorder)
      ("ghc-dlist" ,ghc-dlist)
      ("ghc-mtl" ,ghc-mtl)))
   (arguments
    ;; FIXME Seems like tests have orphan instances
    `(#:tests? #f))
   (home-page
    "https://github.com/haskell/bytestring")
   (synopsis
    "Fast, compact, strict and lazy byte strings with a list interface")
   (description
    "An efficient compact, immutable byte string type (both strict and lazy) suitable for binary or 8-bit character data. . The 'ByteString' type represents sequences of bytes or 8-bit characters. It is suitable for high performance use, both in terms of large data quantities, or high speed requirements. The 'ByteString' functions follow the same style as Haskell\\'s ordinary lists, so it is easy to convert code from using 'String' to 'ByteString'. . Two 'ByteString' variants are provided: . * Strict 'ByteString's keep the string as a single large array. This makes them convenient for passing data between C and Haskell. . * Lazy 'ByteString's use a lazy list of strict chunks which makes it suitable for I\\/O streaming tasks. . The @Char8@ modules provide a character-based view of the same underlying 'ByteString' types. This makes it convenient to handle mixed binary and 8-bit character content (which is common in many file formats and network protocols). . The 'Builder' module provides an efficient way to build up 'ByteString's in an ad-hoc way by repeated concatenation. This is ideal for fast serialisation or pretty printing. . There is also a 'ShortByteString' type which has a lower memory overhead and can can be converted to or from a 'ByteString', but supports very few other operations. It is suitable for keeping many short strings in memory. . 'ByteString's are not designed for Unicode. For Unicode strings you should use the 'Text' type from the @text@ package. . These modules are intended to be imported qualified, to avoid name clashes with \"Prelude\" functions, e.g. . > import qualified Data.ByteString as BS")
   (license license:bsd-3)))

(define-public ghc-prim-0.5.2.0
  (package
   (name "ghc-prim-0.5.2.0")
   (version "0.5.2.0")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://hackage.haskell.org/package/ghc-prim/ghc-prim-"
           version
           ".tar.gz"))
     (sha256
      (base32
       "1ccvzkw3v4xlj7g126wwlc5rvd480hbv1pcq2rfb85k77rzi6bjr"))))
   (build-system haskell-build-system)
   (arguments `(#:haskell ,ghc-8))
   (home-page
    "http://hackage.haskell.org/package/ghc-prim")
   (synopsis "GHC primitives")
   (description
    "This package contains the primitive types and operations supplied by GHC.")
   (license #f)))

(define ghc-ed25519
  (package
   (name "ghc-ed25519")
   (version "0.0.5.0")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://hackage.haskell.org/package/ed25519/ed25519-"
           version
           ".tar.gz"))
     (sha256
      (base32
       "0v8msqvgzimhs7p5ri25hrb1ni2wvisl5rmdxy89fc59py79b9fq"))))
   (build-system haskell-build-system)
   (arguments `(#:tests? #f))
   (inputs `(("ghc-prim" ,ghc-prim)))
   (home-page
    "http://thoughtpolice.github.com/hs-ed25519")
   (synopsis "Ed25519 cryptographic signatures")
   (description
    "This package provides a simple, fast, self-contained copy of the Ed25519 public-key signature system with a clean interface. It also includes support for detached signatures, and thorough documentation on the design and implementation, including usage guidelines.")
   (license license:expat)))

(define ghc-binary
  (package
   (name "ghc-binary")
   (version "0.8.6.0")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://hackage.haskell.org/package/binary/binary-"
           version
           ".tar.gz"))
     (sha256
      (base32
       "0pz1va9bmj4daw8gi8r6d1rrnqsd4bislky6d8pjwwbyrgrw9s8y"))))
   (build-system haskell-build-system)
   (native-inputs
    `(("ghc-random" ,ghc-random)
      ("ghc-test-framework" ,ghc-test-framework)
      ("ghc-test-framework-quickcheck2" ,ghc-test-framework-quickcheck2)
      ("ghc-quickcheck" ,ghc-quickcheck)
      ("ghc-hunit" ,ghc-hunit)))
   (home-page "https://github.com/kolmodin/binary")
   (synopsis
    "Binary serialisation for Haskell values using lazy ByteStrings")
   (description
    "Efficient, pure binary serialisation using lazy ByteStrings. Haskell values may be encoded to and from binary formats, written to disk as binary, or sent over the network. The format used can be automatically generated, or you can choose to implement a custom format if needed. Serialisation speeds of over 1 G\\/sec have been observed, so this library should be suitable for high performance scenarios.")
   (license license:bsd-3)))

(define ghc-containers
  (package
   (name "ghc-containers")
   (version "0.6.0.1")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://hackage.haskell.org/package/containers/containers-"
           version
           ".tar.gz"))
     (sha256
      (base32
       "0smc1g58l968jxcjxhxcd4qpfm4zk7zr6r4q6wf6ay75av9rf4d7"))))
   (build-system haskell-build-system)
   (arguments
    `(#:tests? #f
      #:phases (modify-phases %standard-phases (delete 'haddock))))
   (home-page
    "http://hackage.haskell.org/package/containers")
   (synopsis "Assorted concrete container types")
   (description
    ". This package contains efficient general-purpose implementations of various immutable container types including sets, maps, sequences, trees, and graphs. . For a walkthrough of what this package provides with examples of common operations see the [containers introduction](https://haskell-containers.readthedocs.io). . The declared cost of each operation is either worst-case or amortized, but remains valid even if structures are shared.")
   (license license:bsd-3)))

(define ghc-bytestring-0.9.2.1
  (package
   (name "ghc-bytestring-0.9.2.1")
   (version "0.9.2.1")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://hackage.haskell.org/package/bytestring/bytestring-"
           version
           ".tar.gz"))
     (sha256
      (base32
       "09z6xnw9rjmkg0g9ifn529v864qi6rl6wxnplz3adjqflcv0n42r"))))
   (build-system haskell-build-system)
   (arguments
    `(#:tests? #f))
   (home-page
    "https://github.com/haskell/bytestring")
   (synopsis
    "Fast, compact, strict and lazy byte strings with a list interface")
   (description
    "An efficient compact, immutable byte string type (both strict and lazy) suitable for binary or 8-bit character data. . The 'ByteString' type represents sequences of bytes or 8-bit characters. It is suitable for high performance use, both in terms of large data quantities, or high speed requirements. The 'ByteString' functions follow the same style as Haskell\\'s ordinary lists, so it is easy to convert code from using 'String' to 'ByteString'. . Two 'ByteString' variants are provided: . * Strict 'ByteString's keep the string as a single large array. This makes them convenient for passing data between C and Haskell. . * Lazy 'ByteString's use a lazy list of strict chunks which makes it suitable for I\\/O streaming tasks. . The @Char8@ modules provide a character-based view of the same underlying 'ByteString' types. This makes it convenient to handle mixed binary and 8-bit character content (which is common in many file formats and network protocols). . The 'Builder' module provides an efficient way to build up 'ByteString's in an ad-hoc way by repeated concatenation. This is ideal for fast serialisation or pretty printing. . There is also a 'ShortByteString' type which has a lower memory overhead and can can be converted to or from a 'ByteString', but supports very few other operations. It is suitable for keeping many short strings in memory. . 'ByteString's are not designed for Unicode. For Unicode strings you should use the 'Text' type from the @text@ package. . These modules are intended to be imported qualified, to avoid name clashes with \"Prelude\" functions, e.g. . > import qualified Data.ByteString as BS")
   (license license:bsd-3)))

;; TODO Fix with better guix import data
(define ghc-resolv
  (package
   (name "ghc-resolv")
   (version "0.1.1.1")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://hackage.haskell.org/package/resolv/resolv-"
           version
           ".tar.gz"))
     (sha256
      (base32
       "0wh7wj56l3f2bylz563g5g04a4nydj8acv60hpwa7k3mn792xca9"))))
   (build-system haskell-build-system)
   (inputs
    `(("ghc-base16-bytestring" ,ghc-base16-bytestring)
      ("ghc-binary" ,ghc-binary)
      ("ghc-bytestring" ,ghc-bytestring-0.9.2.1)
      ("ghc-containers" ,ghc-containers)))
   (arguments `(#:tests? #f))
   (home-page "https://hackage.haskell.org/package/resolv")
   (synopsis "")
   (description "")
   (license license:bsd-3)))

(define ghc-hackage-security
  (package
   (name "ghc-hackage-security")
   (version "0.5.3.0")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://hackage.haskell.org/package/hackage-security/hackage-security-"
           version
           ".tar.gz"))
     (sha256
      (base32
       "08bwawc7ramgdh54vcly2m9pvfchp0ahhs8117jajni6x4bnx66v"))))
   (build-system haskell-build-system)
   (inputs
    `(("ghc-base16-bytestring" ,ghc-base16-bytestring)
      ("ghc-base64-bytestring" ,ghc-base64-bytestring)
      ("ghc-ed25519" ,ghc-ed25519)
      ("ghc-mtl" ,ghc-mtl)
      ("ghc-parsec" ,ghc-parsec)
      ("ghc-cryptohash-sha256" ,ghc-cryptohash-sha256)
      ("ghc-tar" ,ghc-tar)
      ("ghc-zlib" ,ghc-zlib)
      ("ghc-network-uri" ,ghc-network-uri)
      ("ghc-network" ,ghc-network)))
   (native-inputs
    `(("ghc-network-uri" ,ghc-network-uri)
      ("ghc-tar" ,ghc-tar)
      ("ghc-zlib" ,ghc-zlib)
      ("ghc-tasty" ,ghc-tasty)
      ("ghc-tasty-hunit" ,ghc-tasty-hunit)
      ("ghc-tasty-quickcheck" ,ghc-tasty-quickcheck)
      ("ghc-quickcheck" ,ghc-quickcheck)
      ("ghc-temporary" ,ghc-temporary)))
   (home-page
    "https://github.com/haskell/hackage-security")
   (synopsis "Hackage security library")
   (description
    "The hackage security library provides both server and client utilities for securing the Hackage package server (<http://hackage.haskell.org/>).  It is based on The Update Framework (<http://theupdateframework.com/>), a set of recommendations developed by security researchers at various universities in the US as well as developers on the Tor project (<https://www.torproject.org/>). . The current implementation supports only index signing, thereby enabling untrusted mirrors. It does not yet provide facilities for author package signing. . The library has two main entry points: \"Hackage.Security.Client\" is the main entry point for clients (the typical example being @cabal@), and \"Hackage.Security.Server\" is the main entry point for servers (the typical example being @hackage-server@).")
   (license license:bsd-3)))

(define ghc-cabal
  (package
   (name "ghc-cabal")
   (version "2.4.0.1")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://hackage.haskell.org/package/Cabal/Cabal-"
           version
           ".tar.gz"))
     (sha256
      (base32
       "161l9lgayzpb3wrp9bcp8k0a3rq5dpyiyrxjb87dhximi2mc16rv"))))
   (build-system haskell-build-system)
   (inputs
    `(("ghc-mtl" ,ghc-mtl)
      ("ghc-text" ,ghc-text)
      ("ghc-parsec" ,ghc-parsec)))
   (native-inputs
    `(("ghc-integer-logarithms" ,ghc-integer-logarithms)
      ("ghc-tasty" ,ghc-tasty)
      ("ghc-tasty-hunit" ,ghc-tasty-hunit)
      ("ghc-tasty-quickcheck" ,ghc-tasty-quickcheck)
      ("ghc-tagged" ,ghc-tagged)
      ("ghc-temporary" ,ghc-temporary)
      ("ghc-text" ,ghc-text)
      ("ghc-quickcheck" ,ghc-quickcheck)
      ("ghc-base-compat" ,ghc-base-compat)
      ("ghc-tasty" ,ghc-tasty)
      ("ghc-tasty-hunit" ,ghc-tasty-hunit)
      ("ghc-tasty-quickcheck" ,ghc-tasty-quickcheck)
      ("ghc-tasty-golden" ,ghc-tasty-golden)
      ("ghc-diff" ,ghc-diff)
      ("ghc-tree-diff" ,ghc-tree-diff)
      ("ghc-tasty" ,ghc-tasty)
      ("ghc-tasty-golden" ,ghc-tasty-golden)
      ("ghc-diff" ,ghc-diff)
      ("ghc-base-compat" ,ghc-base-compat)
      ("ghc-base-orphans" ,ghc-base-orphans)
      ("ghc-optparse-applicative" ,ghc-optparse-applicative)
      ("ghc-tar" ,ghc-tar)
      ("ghc-tree-diff" ,ghc-tree-diff)))
   (home-page "http://www.haskell.org/cabal/")
   (synopsis
    "A framework for packaging Haskell software")
   (description
    "The Haskell Common Architecture for Building Applications and Libraries: a framework defining a common interface for authors to more easily build their Haskell applications in a portable way. . The Haskell Cabal is part of a larger infrastructure for distributing, organizing, and cataloging Haskell libraries and tools.")
   (license license:bsd-3)))

;;; ACTUAL PACKAGE
(define-public cabal-install-2.4.0.0
  (package
   (name "cabal-install-2.4.0.0")
   (version "2.4.0.0")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://hackage.haskell.org/package/cabal-install/cabal-install-"
           version
           ".tar.gz"))
     (sha256
      (base32
       "1xmyl0x8wqfrnray6ky5wy0g0samv4264fbdlzxhqsvk9dbfja8k"))))
   (build-system haskell-build-system)
   (inputs
    `(("ghc-async" ,ghc-async)
      ("ghc-base16-bytestring" ,ghc-base16-bytestring)
      ("ghc-cryptohash-sha256" ,ghc-cryptohash-sha256)
      ("ghc-echo" ,ghc-echo)
      ("ghc-edit-distance" ,ghc-edit-distance)
      ("ghc-hashable" ,ghc-hashable)
      ("ghc-http" ,ghc-http)
      ("ghc-mtl" ,ghc-mtl)
      ("ghc-network-uri" ,ghc-network-uri)
      ("ghc-network" ,ghc-network)
      ("ghc-random" ,ghc-random)
      ("ghc-stm" ,ghc-stm)
      ("ghc-tar" ,ghc-tar)
      ("ghc-zlib" ,ghc-zlib)
      ("ghc-hackage-security" ,ghc-hackage-security)
      ("ghc-text" ,ghc-text)
      ("ghc-zip-archive" ,ghc-zip-archive)
      ("ghc-parsec" ,ghc-parsec)
      ("ghc-resolv" ,ghc-resolv)))
   (home-page "http://www.haskell.org/cabal/")
   (synopsis
    "The command-line interface for Cabal and Hackage.")
   (description
    "The \\'cabal\\' command-line program simplifies the process of managing Haskell software by automating the fetching, configuration, compilation and installation of Haskell libraries and programs.")
   (license license:bsd-3)))
