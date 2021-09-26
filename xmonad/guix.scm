(use-modules (gnu packages wm)
             (gnu packages xorg)
             (guix gexp)
             (guix git-download)
             (guix packages)
             (guix utils)
             (ice-9 popen)
             (ice-9 rdelim)
             (ice-9 regex))

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
                     (string-match "dist" f)
                     (string-match "dist-newstyle" f))))))

(package
 (inherit xmonad)
 (name %name)
 (version %version)
 (source %local)
 (inputs
  `(("libxpm" ,libxpm)
    ("xmobar" ,xmobar)
    ("xmonad" ,xmonad)
    ("ghc-xmonad-contrib" ,ghc-xmonad-contrib)))
 (arguments
  `(#:phases
    (modify-phases %standard-phases
      (add-after 'install 'make-static
        (lambda* (#:key outputs #:allow-other-keys)
          (mkdir-p (assoc-ref outputs "static"))))
      (delete 'install-license-files)))))
