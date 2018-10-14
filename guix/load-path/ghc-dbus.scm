(define-module (ghc-dbus)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system haskell)
  #:use-module (gnu packages haskell)
  #:use-module (gnu packages haskell-check)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xml))

;; PUBLIC
(define-public ghc-dbus
  (package
   (name "ghc-dbus")
   (version "1.0.1")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "https://hackage.haskell.org/package/dbus/dbus-" version ".tar.gz"))
     (sha256 (base32 "1xg8wzs7xnh3455v3bbw9nd8inzr06n5939pzlq3nd4ajp3ba9d3"))))
   (build-system haskell-build-system)
   (inputs
    `(("ghc-cereal" ,ghc-cereal)
      ("ghc-lens" ,ghc-lens)
      ("ghc-libxml-sax" ,ghc-libxml-sax)
      ("ghc-network" ,ghc-network)
      ("ghc-parsec" ,ghc-parsec)
      ("ghc-random" ,ghc-random)
      ("ghc-split" ,ghc-split)
      ("ghc-th-lift" ,ghc-th-lift)
      ("ghc-vector" ,ghc-vector)
      ("ghc-xml-types" ,ghc-xml-types)))
   (arguments `(#:tests? #f))
   (home-page "https://github.com/rblaze/haskell-dbus#readme")
   (synopsis "A client library for the D-Bus IPC system.")
   (description
    "D-Bus is a simple, message-based protocol for inter-process communication, which allows applications to interact with other parts of the machine and the user's session using remote procedure calls. . D-Bus is a essential part of the modern Linux desktop, where it replaces earlier protocols such as CORBA and DCOP. . This library is an implementation of the D-Bus protocol in Haskell. It can be used to add D-Bus support to Haskell applications, without the awkward interfaces common to foreign bindings. . Example: connect to the session bus, and get a list of active names. . @ &#x7b;-\\# LANGUAGE OverloadedStrings \\#-&#x7d; . import Data.List (sort) import DBus import DBus.Client . main = do &#x20;   client <- connectSession &#x20;   // &#x20;   \\-- Request a list of connected clients from the bus &#x20;   reply <- call_ client (methodCall \\\"\\/org\\/freedesktop\\/DBus\\\" \\\"org.freedesktop.DBus\\\" \\\"ListNames\\\") &#x20;       &#x7b; methodCallDestination = Just \\\"org.freedesktop.DBus\\\" &#x20;       &#x7d; &#x20;   // &#x20;   \\-- org.freedesktop.DBus.ListNames() returns a single value, which is &#x20;   \\-- a list of names (here represented as [String]) &#x20;   let Just names = fromVariant (methodReturnBody reply !! 0) &#x20;   // &#x20;   \\-- Print each name on a line, sorted so reserved names are below &#x20;   \\-- temporary names. &#x20;   mapM_ putStrLn (sort names) @ . >$ ghc --make list-names.hs >$ ./list-names >:1.0 >:1.1 >:1.10 >:1.106 >:1.109 >:1.110 >ca.desrt.dconf >org.freedesktop.DBus >org.freedesktop.Notifications >org.freedesktop.secrets >org.gnome.ScreenSaver")
   (license license:asl2.0)))

;; DEPENDENCIES
(define-public ghc-libxml-sax
  (package
   (name "ghc-libxml-sax")
   (version "0.7.5")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "https://hackage.haskell.org/package/libxml-sax/libxml-sax-" version ".tar.gz"))
     (sha256 (base32 "0lbdq6lmiyrnzk6gkx09vvp928wj8qnqnqfzy14mfv0drj21f54r"))))
   (build-system haskell-build-system)
   (inputs
    `(("ghc-xml-types" ,ghc-xml-types)))
   (native-inputs
    `(("pkg-config" ,pkg-config)
      ("libxml2" ,libxml2)))
   (home-page "https://john-millikin.com/software/haskell-libxml/")
   (synopsis "Bindings for the libXML2 SAX interface")
   (description "")
   (license license:expat)))

