(list
 (channel
  (name 'guix)
  (url "file:///home/john/projects/guix/.git")
  (branch "john"))
 (channel
  (name 'private)
  (url "file:///home/john/projects/guix-channel/.git")
  (branch "release")
  (introduction
   (make-channel-introduction
    "cf22e20eceb112c940d02c3b57e16c7cee1f4a7f"
    (openpgp-fingerprint
     "5A15 8FAF 406A 748A 81A9  DC4E 4F43 7A76 B448 A23B")))))
