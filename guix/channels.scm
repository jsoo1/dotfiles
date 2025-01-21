(define-module (channels)
  #:use-module (guix channels))

(define-public default
  (list
   (channel
    (name 'guix)
    (url "file:///home/john/projects/guix/.git")
    (branch "john")
    (introduction
     (make-channel-introduction
      "ddbe3c96788b96f609612987582fd079446403ae"
      (openpgp-fingerprint
       "5A15 8FAF 406A 748A 81A9  DC4E 4F43 7A76 B448 A23B"))))
   (channel
    (name 'nonguix)
    (url "file:///home/john/projects/nonguix/.git")
    (branch "master")
    (introduction
     (make-channel-introduction
      "9b23baa4ba1f04fc0a7cb68553aba46ab4efa46b"
      (openpgp-fingerprint
       "5A15 8FAF 406A 748A 81A9  DC4E 4F43 7A76 B448 A23B"))))
   (channel
    (name 'private)
    (url "file:///home/john/projects/guix-channel/.git")
    (branch "release")
    (introduction
     (make-channel-introduction
      "3c659b0e3bafe5698e1ecc6a11e72e600bcdfd60"
      (openpgp-fingerprint
       "5A15 8FAF 406A 748A 81A9  DC4E 4F43 7A76 B448 A23B"))))))

default
