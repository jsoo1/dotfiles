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

(define my-xmobar
  (let ((commit "ipc-improvements"))
    (package
      (inherit xmobar)
      (name "xmobar")
      (version "0.40")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/jsoo1/xmobar")
               (commit commit)))
         (sha256
          (base32 "18c81gqyi80ps30hzb7dlvfs51af07vfnaik8sxjvmlvhkwz3683"))
         (file-name (git-file-name name version)))))))

(package
 (inherit xmonad)
 (name %name)
 (version %version)
 (source %local)
 (inputs
  `(("libxpm" ,libxpm)
    ("xmobar" ,my-xmobar)
    ("xmonad" ,xmonad)
    ("ghc-xmonad-contrib" ,ghc-xmonad-contrib)))
 (arguments
  `(#:phases
    (modify-phases %standard-phases
      (add-after 'install 'make-static
        (lambda* (#:key outputs #:allow-other-keys)
          (mkdir-p (assoc-ref outputs "static"))))
      (delete 'install-license-files)))))
