(list
 (channel
  (name 'guix)
  (url "file:///home/john/projects/guix/.git")
  (branch "john")
  (introduction
   (make-channel-introduction
    "b2a9169db5a61facf54b73b5301391d589675541"
    (openpgp-fingerprint
     "5A15 8FAF 406A 748A 81A9  DC4E 4F43 7A76 B448 A23B"))))
 (channel
  (name 'nonguix)
  (url "https://gitlab.com/nonguix/nonguix")
  (commit "d3a550dbf2cd099a1e20ed5407c443e396b5d44b")
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
    "c8e2830a1418362e3f0981df89a3947b72852683"
    (openpgp-fingerprint
     "5A15 8FAF 406A 748A 81A9  DC4E 4F43 7A76 B448 A23B")))))
