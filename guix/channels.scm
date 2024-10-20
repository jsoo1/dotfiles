(list
 (channel
  (name 'guix)
  (url "file:///home/john/projects/guix/.git")
  (branch "john")
  (introduction
   (make-channel-introduction
    "ebc6c866f9c3845a2aff09937e93bb5454ab3cd5"
    (openpgp-fingerprint
     "5A15 8FAF 406A 748A 81A9  DC4E 4F43 7A76 B448 A23B"))))
 (channel
  (name 'nonguix)
  (url "file:///home/john/projects/nonguix/.git")
  (commit "cc6667726df28b18f07c18b824695ad271757b55")
  (introduction
   (make-channel-introduction
    "50efe4e85f29a457bdcc20b3028ab4c660d3cdd0"
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
     "5A15 8FAF 406A 748A 81A9  DC4E 4F43 7A76 B448 A23B")))))
