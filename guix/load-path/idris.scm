(define-module (idris)
  #:use-module (gnu packages haskell)
  #:use-module (gnu packages haskell-check)
  #:use-module (gnu packages haskell-web)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pkg-config)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system haskell)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))

(define ghc-libffi
  (package
   (name "ghc-libffi")
   (version "0.1")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://hackage.haskell.org/package/libffi/libffi-"
           version
           ".tar.gz"))
     (sha256
      (base32
       "0g7jnhng3j7z5517aaqga0144aamibsbpgm3yynwyfzkq1kp0f28"))))
   (build-system haskell-build-system)
   (native-inputs `(("pkg-config" ,pkg-config)
                    ("libffi" ,libffi)))
   (home-page
    "http://hackage.haskell.org/package/libffi")
   (synopsis "A binding to libffi")
   (description
    "A binding to libffi, allowing C functions of types only known at runtime to be called from Haskell.")
   (license license:bsd-3)))

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

(define ghc-fingertree
  (package
   (name "ghc-fingertree")
   (version "0.1.4.1")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://hackage.haskell.org/package/fingertree/fingertree-"
           version
           ".tar.gz"))
     (sha256
      (base32
       "192fyzv0pn1437wdpqg1l80rswkk4rw3w61r4bq7dhv354bdqy4p"))))
   (build-system haskell-build-system)
   (native-inputs
    `(("ghc-hunit" ,ghc-hunit)
      ("ghc-quickcheck" ,ghc-quickcheck)
      ("ghc-test-framework" ,ghc-test-framework)
      ("ghc-test-framework-hunit" ,ghc-test-framework-hunit)
      ("ghc-test-framework-quickcheck2" ,ghc-test-framework-quickcheck2)))
   (home-page
    "http://hackage.haskell.org/package/fingertree")
   (synopsis
    "Generic finger-tree structure, with example instances")
   (description
    "A general sequence representation with arbitrary annotations, for use as a base for implementations of various collection types, with examples, as described in section 4 of . * Ralf Hinze and Ross Paterson, \\\"Finger trees: a simple general-purpose data structure\\\", /Journal of Functional Programming/ 16:2 (2006) pp 197-217. <http://staff.city.ac.uk/~ross/papers/FingerTree.html> . For a tuned sequence type, see @Data.Sequence@ in the @containers@ package, which is a specialization of this structure.")
   (license license:bsd-3)))

(define-public idris-1.3.0
  (package
   (name "idris-1.3.0")
   (version "1.3.0")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://hackage.haskell.org/package/idris/idris-"
           version
           ".tar.gz"))
     (sha256
      (base32
       "1w5i2z88li4niykwc6yrgxgfp25ll6ih95cip0ri7d8i7ik03c48"))))
   (build-system gnu-build-system)
   (inputs
    `(("cabal-install" ,cabal-install)
      ("ghc-aeson" ,ghc-aeson)
      ("ghc-annotated-wl-pprint" ,ghc-annotated-wl-pprint)
      ("ghc-ansi-terminal" ,ghc-ansi-terminal)
      ("ghc-ansi-wl-pprint" ,ghc-ansi-wl-pprint)
      ("ghc-base64-bytestring" ,ghc-base64-bytestring)
      ("ghc-binary" ,ghc-binary)
      ("ghc-blaze-html" ,ghc-blaze-html)
      ("ghc-blaze-markup" ,ghc-blaze-markup)
      ("ghc-cheapskate" ,ghc-cheapskate)
      ("ghc-code-page" ,ghc-code-page)
      ("ghc-fingertree" ,ghc-fingertree)
      ("ghc-ieee754" ,ghc-ieee754)
      ("ghc-megaparsec" ,ghc-megaparsec)
      ("ghc-mtl" ,ghc-mtl)
      ("ghc-network" ,ghc-network)
      ("ghc-optparse-applicative" ,ghc-optparse-applicative)
      ("ghc-regex-tdfa" ,ghc-regex-tdfa)
      ("ghc-safe" ,ghc-safe)
      ("ghc-split" ,ghc-split)
      ("ghc-terminal-size" ,ghc-terminal-size)
      ("ghc-text" ,ghc-text)
      ("ghc-uniplate" ,ghc-uniplate)
      ("ghc-unordered-containers" ,ghc-unordered-containers)
      ("ghc-utf8-string" ,ghc-utf8-string)
      ("ghc-vector" ,ghc-vector)
      ("ghc-vector-binary-instances" ,ghc-vector-binary-instances)
      ("ghc-zip-archive" ,ghc-zip-archive)
      ("ghc-fsnotify" ,ghc-fsnotify)
      ("ghc-async" ,ghc-async)
      ("ghc-libffi" ,ghc-libffi)))
   (native-inputs
    `(("ghc-optparse-applicative" ,ghc-optparse-applicative)
      ("ghc-tagged" ,ghc-tagged)
      ("ghc-tasty" ,ghc-tasty)
      ("ghc-tasty-golden" ,ghc-tasty-golden)
      ("ghc-tasty-rerun" ,ghc-tasty-rerun)))
   (arguments
    `(#:configure-flags
      (list
       (string-append "--datasubdir=" (assoc-ref %outputs "out") "/lib/idris"))
      #:phases
      (modify-phases %standard-phases
                     (add-before 'configure 'set-cc-command
                                 (lambda _ (setenv "CC" "gcc") #t))
                     (add-after 'install 'fix-libs-install-location
                                (lambda* (#:key outputs #:allow-other-keys)
                                  (let* ((out (assoc-ref outputs "out"))
                                         (lib (string-append lib "/lib/idris"))
                                         (modules (string-append lib "/libs")))
                                    (for-each (lambda (module)
                                                (symlink (string-append modules "/" module)
                                                         (string-append lib "/" module)))
                                              '("prelude" "base" "contrib" "effects" "pruviloj"))))))))
   (native-search-paths
    (list (search-path-specification
           (variable "IDRIS_LIBRARY_PATH")
           (files '("lib/idris")))))
   (home-page "http://www.idris-lang.org/")
   (synopsis
    "Functional Programming Language with Dependent Types")
   (description
    "Idris is a general purpose language with full dependent types. It is compiled, with eager evaluation. Dependent types allow types to be predicated on values, meaning that some aspects of a program's behaviour can be specified precisely in the type. The language is closely related to Epigram and Agda. There is a tutorial at <http://www.idris-lang.org/documentation>. Features include: . * Full, first class, dependent types with dependent pattern matching . * where clauses, with rule, case expressions, pattern matching let and lambda bindings . * Interfaces (similar to type classes), monad comprehensions . * do notation, idiom brackets, syntactic conveniences for lists, tuples, dependent pairs . * Totality checking . * Coinductive types . * Indentation significant syntax, extensible syntax . * Cumulative universes . * Simple foreign function interface (to C) . * Hugs style interactive environment")
   (license license:bsd-3)))
