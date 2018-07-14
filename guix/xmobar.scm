(use-modules (gnu packages base)
             (gnu packages haskell)
             (gnu packages haskell-check)
             (gnu packages haskell-web)
             (gnu packages linux)
             (gnu packages pkg-config)
             (gnu packages xorg)
             (guix build-system)
             (guix build-system haskell)
             (guix download)
             ((guix licenses) #:prefix license:)
             (guix packages))


;; ---------- Missing Haskell Libraries


(define ghc-alsa-core
  (package
   (name "ghc-alsa-core")
   (version "0.5.0.1")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://hackage.haskell.org/package/alsa-core/alsa-core-"
           version
           ".tar.gz"))
     (sha256
      (base32
       "1avh4a419h9d2zsslg6j8hm87ppgsgqafz8ll037rk2yy1g4jl7b"))))
   (build-system haskell-build-system)
   (inputs
    `(("alsa-lib" ,alsa-lib)
      ("ghc-extensible-exceptions" ,ghc-extensible-exceptions)
      ("pkg-config" ,pkg-config)))
   (home-page
    "http://www.haskell.org/haskellwiki/ALSA")
   (synopsis
    "Binding to the ALSA Library API (Exceptions).")
   (description
    "This package provides access to ALSA infrastructure, that is needed by both alsa-seq and alsa-pcm.")
   (license license:bsd-3)))

(define ghc-alsa-mixer
  (package
   (name "ghc-alsa-mixer")
   (version "0.2.0.3")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://hackage.haskell.org/package/alsa-mixer/alsa-mixer-"
           version
           ".tar.gz"))
     (sha256
      (base32
       "13fgd78msqsyzm92cbasm8m3s1rww6r1g83qbrv4mkm2h50fnvgp"))))
   (build-system haskell-build-system)
   (inputs `(("ghc-alsa-core" ,ghc-alsa-core)
             ("ghc-c2hs"      ,ghc-c2hs)))
   (home-page
    "https://github.com/ttuegel/alsa-mixer")
   (synopsis
    "Bindings to the ALSA simple mixer API.")
   (description
    "This package provides bindings to the ALSA simple mixer API.")
   (license license:bsd-3)))

(define ghc-dbus
  (package
   (name "ghc-dbus")
   (version "1.0.1")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://hackage.haskell.org/package/dbus/dbus-"
           version
           ".tar.gz"))
     (sha256
      (base32
       "1xg8wzs7xnh3455v3bbw9nd8inzr06n5939pzlq3nd4ajp3ba9d3"))))
   (build-system haskell-build-system)
   (inputs
    `(("ghc-cereal" ,ghc-cereal)
      ("ghc-lens" ,ghc-lens)
      ("ghc-libxml-sax" ,ghc-libxml-sax)
      ("ghc-network" ,ghc-network)
      ("ghc-parsec" ,ghc-parsec)
      ("ghc-random" ,ghc-random)
      ("ghc-split" ,ghc-split)
      ("ghc-text" ,ghc-text)
      ("ghc-th-lift" ,ghc-th-lift)
      ("ghc-vector" ,ghc-vector)
      ("ghc-xml-types" ,ghc-xml-types)
      ("ghc-extra" ,ghc-extra)
      ("ghc-quickcheck" ,ghc-quickcheck)
      ("ghc-resourcet" ,ghc-resourcet)
      ("ghc-tasty" ,ghc-tasty)
      ("ghc-tasty-hunit" ,ghc-tasty-hunit)
      ("ghc-tasty-quickcheck" ,ghc-tasty-quickcheck)))
   (home-page
    "https://github.com/rblaze/haskell-dbus#readme")
   (synopsis
    "A client library for the D-Bus IPC system.")
   (description
    "D-Bus is a simple, message-based protocol for inter-process communication, which allows applications to interact with other parts of the machine and the user's session using remote procedure calls. . D-Bus is a essential part of the modern Linux desktop, where it replaces earlier protocols such as CORBA and DCOP. . This library is an implementation of the D-Bus protocol in Haskell. It can be used to add D-Bus support to Haskell applications, without the awkward interfaces common to foreign bindings. . Example: connect to the session bus, and get a list of active names. . @ &#x7b;-\\# LANGUAGE OverloadedStrings \\#-&#x7d; . import Data.List (sort) import DBus import DBus.Client . main = do &#x20;   client <- connectSession &#x20;   // &#x20;   \\-- Request a list of connected clients from the bus &#x20;   reply <- call_ client (methodCall \\\"\\/org\\/freedesktop\\/DBus\\\" \\\"org.freedesktop.DBus\\\" \\\"ListNames\\\") &#x20;       &#x7b; methodCallDestination = Just \\\"org.freedesktop.DBus\\\" &#x20;       &#x7d; &#x20;   // &#x20;   \\-- org.freedesktop.DBus.ListNames() returns a single value, which is &#x20;   \\-- a list of names (here represented as [String]) &#x20;   let Just names = fromVariant (methodReturnBody reply !! 0) &#x20;   // &#x20;   \\-- Print each name on a line, sorted so reserved names are below &#x20;   \\-- temporary names. &#x20;   mapM_ putStrLn (sort names) @ . >$ ghc --make list-names.hs >$ ./list-names >:1.0 >:1.1 >:1.10 >:1.106 >:1.109 >:1.110 >ca.desrt.dconf >org.freedesktop.DBus >org.freedesktop.Notifications >org.freedesktop.secrets >org.gnome.ScreenSaver")
   (license license:asl2.0)))

(define ghc-libxml-sax
  (package
   (name "ghc-libxml-sax")
   (version "0.7.5")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://hackage.haskell.org/package/libxml-sax/libxml-sax-"
           version
           ".tar.gz"))
     (sha256
      (base32
       "0lbdq6lmiyrnzk6gkx09vvp928wj8qnqnqfzy14mfv0drj21f54r"))))
   (build-system haskell-build-system)
   (inputs
    `(("ghc-text" ,ghc-text)
      ("ghc-xml-types" ,ghc-xml-types)))
   (home-page
    "https://john-millikin.com/software/haskell-libxml/")
   (synopsis
    "Bindings for the libXML2 SAX interface")
   (description "")
   (license license:expat)))

(define ghc-libpmd
  (package
   (name "ghc-libmpd")
   (version "0.9.0.8")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://hackage.haskell.org/package/libmpd/libmpd-"
           version
           ".tar.gz"))
     (sha256
      (base32
       "0kpdj4ciwrfd6vmr60y7c276h5z2r40avs26a0x8s51rbr00lasq"))))
   (build-system haskell-build-system)
   (inputs
    `(("ghc-attoparsec" ,ghc-attoparsec)
      ("ghc-mtl" ,ghc-mtl)
      ("ghc-old-locale" ,ghc-old-locale)
      ("ghc-text" ,ghc-text)
      ("ghc-data-default-class" ,ghc-data-default-class)
      ("ghc-network" ,ghc-network)
      ("ghc-utf8-string" ,ghc-utf8-string)
      ("ghc-quickcheck" ,ghc-quickcheck)
      ("ghc-hspec" ,ghc-hspec)))
   (home-page
    "http://github.com/vimus/libmpd-haskell#readme")
   (synopsis "An MPD client library.")
   (description
    "A client library for MPD, the Music Player Daemon.")
   (license license:expat)))

(define ghc-timezone-olson
  (package
   (name "ghc-timezone-olson")
   (version "0.1.9")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://hackage.haskell.org/package/timezone-olson/timezone-olson-"
           version
           ".tar.gz"))
     (sha256
      (base32
       "05abywx1nrcaz0nqzfy4zw62bc5qd7pdfnjvv4drxkwv084ha8rj"))))
   (build-system haskell-build-system)
   (inputs
    `(("ghc-timezone-series" ,ghc-timezone-series)
      ("ghc-extensible-exceptions" ,ghc-extensible-exceptions)))
   (home-page
    "http://projects.haskell.org/time-ng/")
   (synopsis
    "A pure Haskell parser and renderer for binary Olson timezone files")
   (description
    "A parser and renderer for binary Olson timezone files whose format is specified by the tzfile(5) man page on Unix-like systems. For more information about this format, see <http://www.iana.org/time-zones/repository/tz-link.html>. Functions are provided for converting the parsed data into 'TimeZoneSeries' objects from the timezone-series package. On many platforms, binary Olson timezone files suitable for use with this package are available in the directory /usr/share/zoneinfo and its subdirectories on your computer. For a way to read binary Olson timezone files at compile time, see the timezone-olson-th package (<http://hackage.haskell.org/package/timezone-olson-th>).")
   (license license:bsd-3)))

(define ghc-timezone-series
  (package
   (name "ghc-timezone-series")
   (version "0.1.9")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://hackage.haskell.org/package/timezone-series/timezone-series-"
           version
           ".tar.gz"))
     (sha256
      (base32
       "1blwgnyzqn917rgqkl4dncv9whv3xmk0lav040qq0214vksmvlz5"))))
   (build-system haskell-build-system)
   (home-page
    "http://projects.haskell.org/time-ng/")
   (synopsis
    "Enhanced timezone handling for Data.Time")
   (description
    "This package endows Data.Time, from the time package, with several data types and functions for enhanced processing of timezones. For one way to create timezone series, see the timezone-olson (<http://hackage.haskell.org/package/timezone-olson>) and timezone-olson-th (<http://hackage.haskell.org/package/timezone-olson-th>) packages.")
   (license license:bsd-3)))

(define ghc-c2hs
  (package
   (name "ghc-c2hs")
   (version "0.28.5")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://hackage.haskell.org/package/c2hs/c2hs-"
           version
           ".tar.gz"))
     (sha256
      (base32
       "1xid997cc38rym6hsgv8xz5dg8jcsh8hs5rrwaxkij7mc09an45x"))))
   (build-system haskell-build-system)
   (arguments `(#:phases (modify-phases %standard-phases (delete 'check))))
   (inputs
    `(("ghc-dlist" ,ghc-dlist)
      ("ghc-test-framework" ,ghc-test-framework)
      ("ghc-test-framework-hunit" ,ghc-test-framework-hunit)
      ("ghc-hunit" ,ghc-hunit)
      ("ghc-language-c" ,ghc-language-c-0.8)
      ("ghc-shelly" ,ghc-shelly)
      ("ghc-text" ,ghc-text)))
   (home-page "https://github.com/haskell/c2hs")
   (synopsis
    "C->Haskell FFI tool that gives some cross-language type safety")
   (description
    "C->Haskell assists in the development of Haskell bindings to C libraries. It extracts interface information from C header files and generates Haskell code with foreign imports and marshaling. Unlike writing foreign imports by hand (or using hsc2hs), this ensures that C functions are imported with the correct Haskell types.")
   (license license:gpl2)))

(define ghc-language-c-0.8
  (package
   (name "ghc-language-c")
   (version "0.8")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "https://hackage.haskell.org/package/"
                         "language-c/language-c-" version ".tar.gz"))
     (sha256
      (base32
       "0ms0hfg65d7phfr2dq6183vcbmq9ddpvmlqbs8nwbqhqvcdpfl4w"))))
   (build-system haskell-build-system)
   (inputs `(("ghc-syb" ,ghc-syb)))
   (native-inputs
    `(("ghc-happy" ,ghc-happy)
      ("ghc-alex" ,ghc-alex)))
   (home-page "https://visq.github.io/language-c/")
   (synopsis "Analysis and generation of C code")
   (description
    "Language C is a Haskell library for the analysis and generation of C code.
It features a complete, well-tested parser and pretty printer for all of C99
and a large set of GNU extensions.")
   (license license:bsd-3)))

;; ------- Actual Package

(package
 (name "my-xmobar")
 (version "0.26")
 (source (origin
          (method url-fetch)
          (uri (string-append "mirror://hackage/package/xmobar/"
                              "xmobar-" version ".tar.gz"))
          (sha256
           (base32
            "19g40vqj3cs94i27f66194k7d5cazrv1lx54bz9kc0qy2npxjzgz"))))
 (build-system haskell-build-system)
 (native-inputs
  `(("ghc-hspec" ,ghc-hspec)
    ("hspec-discover" ,hspec-discover)))
 (inputs
  `(("ghc-alsa-core"       ,ghc-alsa-core)
    ("ghc-alsa-mixer"      ,ghc-alsa-mixer)
    ("ghc-dbus"            ,ghc-dbus)
    ("ghc-hinotify"        ,ghc-hinotify)
    ("ghc-http"            ,ghc-http)
    ("ghc-iwlib"           ,ghc-iwlib)
    ("ghc-libpmd"          ,ghc-libpmd)
    ("ghc-parsec"          ,ghc-parsec)
    ("ghc-regex-compat"    ,ghc-regex-compat)
    ("ghc-stm"             ,ghc-stm)
    ("ghc-timezone-olson"  ,ghc-timezone-olson)
    ("ghc-timezone-series" ,ghc-timezone-series)
    ("ghc-x11-xft"         ,ghc-x11-xft)
    ("libxpm"              ,libxpm)
    ("pkg-config"          ,pkg-config)))
 (arguments
  `(#:configure-flags
    (list (string-append "--flags="
                         (string-join (list "with_alsa"
                                            "with_inotify"
                                            "with_iwlib"
                                            "with_threaded"
                                            "with_utf8"
                                            "with_weather"
                                            "with_xft"
                                            "with_xpm")
                                      " ")))))
 (home-page "http://xmobar.org")
 (synopsis "Minimalistic text based status bar")
 (description
  "My version of xmobar with alsa")
 (license license:bsd-3))
