(define-module (dmenu)
 #:use-module (gnu packages suckless)
 #:use-module (guix packages)
 #:use-module (guix git-download))

(define-public my-dmenu
  (let ((revision "1")
        (commit "8bbe57738d5e6ab19db48c0620330c4c1dd7876d"))
    (package
      (inherit dmenu)
      (name "my-dmenu")
      (version (git-version (package-version dmenu) revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/jsoo1/dmenu")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "16m84q78022vna04q0ar6gs2km5k93aqm78wwhpdlvanjzfw55h2")))))))

my-dmenu
