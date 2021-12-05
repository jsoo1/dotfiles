(list
 (channel
  (name 'guix)
  (url "file:///home/john/projects/guix/.git")
  (branch "john")
  (introduction
   (make-channel-introduction
    "40ee6d749f3263fe61252699f673e194cb373cdc"
    (openpgp-fingerprint
     "5A15 8FAF 406A 748A 81A9  DC4E 4F43 7A76 B448 A23B"))))
 (channel
  (name 'nonguix)
  (url "https://gitlab.com/nonguix/nonguix")
  (commit "3ec1e7f16590c3b681d6cb3b110b0d9c115586ea")
  (introduction
   (make-channel-introduction
    "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
    (openpgp-fingerprint
     "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
 (channel
  (name 'private)
  (url "file:///home/john/projects/guix-channel/.git")
  (branch "release")
  (introduction
   (make-channel-introduction
    "cf22e20eceb112c940d02c3b57e16c7cee1f4a7f"
    (openpgp-fingerprint
     "5A15 8FAF 406A 748A 81A9  DC4E 4F43 7A76 B448 A23B")))))
