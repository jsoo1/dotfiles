(define-module (xmonad)
 #:use-module (gnu packages wm)
 #:use-module (gnu packages haskell-xyz)
 #:use-module (gnu packages xorg)
 #:use-module (guix gexp)
 #:use-module (guix git-download)
 #:use-module (guix packages)
 #:use-module (guix utils)
 #:use-module (ice-9 popen)
 #:use-module (ice-9 rdelim)
 #:use-module (ice-9 regex))

(define %name "my-xmonad")
(define %commit (read-string (open-pipe "git rev-parse HEAD" OPEN_READ)))
(define %version (git-version "0.1" "HEAD" %commit))

(define %local
  (local-file (dirname (current-filename))
              #:recursive? #t
              #:select?
              (lambda (f _)
                (not
                 (or (string-match "\\.ghc\\.environment" f)
		     (string-match "cabal.project" f)
                     (string-match "dist" f)
                     (string-match "dist-newstyle" f))))))

(define-public my-xmobar
  (let ((commit "release"))
    (package
      (inherit ghc-xmobar)
      (name "my-ghc-xmobar")
      (version "0.40")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/jsoo1/xmobar")
               (commit commit)))
         (sha256
          (base32 "1npfwxlxywfl69nw2na3hi7il51pac8rch02z0bn3nx2khz92h1n"))
         (file-name (git-file-name name version))))
      (inputs `(("ghc-uuid" ,ghc-uuid)
                ,@(package-inputs ghc-xmobar)))
      (arguments `(#:tests? #f ,@(package-arguments ghc-xmobar))))))

(define-public my-xmonad
  (package
    (inherit xmonad)
    (name %name)
    (version %version)
    (source %local)
    (inputs
     `(("libxpm" ,libxpm)
       ("xmobar" ,my-xmobar)
       ("xmonad" ,xmonad)
       ("ghc-random" ,ghc-random)
       ("ghc-uuid" ,ghc-uuid)
       ("ghc-xmonad-contrib" ,ghc-xmonad-contrib)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'install-license-files))))))

my-xmonad
