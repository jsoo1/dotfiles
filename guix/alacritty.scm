(use-modules (guix packages)
             (guix download)
             (guix git-download)
             (guix licenses)
             (guix build-system cargo)
             (gnu packages cmake)
             (gnu packages fontutils)
             (gnu packages pkg-config)
             (gnu packages xdisorg)
             (gnu packages python)
             )
(package
  (name "alacritty")
  (version "0.1.0")
  (home-page "https://github.com/jwilm/alacritty")
  (synopsis "A cross-platform, GPU-accelerated terminal emulator")
  (description "Alacritty is the fastest terminal emulator in existence. Using the GPU for rendering enables optimizations that simply aren't possible in other emulators.")
  (license asl2.0)
  (build-system cargo-build-system)
  (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/jwilm/alacritty")
                  (commit "3d75c49")))
            (sha256
             (base32
              "1bpkn4hx88cl25ymc0bhvs0pvcxyxqqbvwmgrdxaxzqhq6kqzkp4"))))
  (native-inputs `(("xclip" ,xclip)
                   ("cmake" ,cmake)
                   ("fontutils" ,fontconfig)
                   ("freetype" ,freetype)
                   ("python3" ,python)))
  (arguments `(#:phases (modify-phases %standard-phases
                          (add-after 'configure 'set-cargo-home
                              (lambda _
                                (setenv "CARGO_HOME" %output)))))))
