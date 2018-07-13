(use-modules (gnu)
             (gnu system nss)
             (guix modules)
             (guix packages)
             (guix download)
             (guix build-system haskell)
             ((guix licenses) #:prefix license:))
(use-service-modules base desktop ssh)
(use-package-modules admin
                     bootloaders
                     certs
                     commencement
                     compton
                     emacs
                     fonts
                     haskell
                     haskell-check
                     haskell-web
                     gnupg
                     image-viewers
                     lsof
                     ncurses
                     shells
                     ssh
                     suckless
                     terminals
                     tmux
                     version-control
                     vim
                     web-browsers
                     wm
                     xdisorg
                     xorg)

(operating-system
 (host-name "ecenter")
 (timezone "America/Los_Angeles")
 (locale "en_US.utf8")

 ;; TODO: find out if "ahcpi" can be removed
 (initrd-modules (append '(;; "ahcpi"
                           "shpchp")
                         %base-initrd-modules))

 (bootloader (bootloader-configuration
              (bootloader grub-efi-bootloader)
              (target "/boot/efi")))

 (file-systems (cons* (file-system
                       (device "guix-root")
                       ;; TODO: Find out why `title` is deprecated
                       (title 'label)
                       (mount-point "/")
                       (type "ext4"))
                      (file-system
                       (device (uuid "60E8-6B6F" 'fat))
                       ;; TODO: Find out why `title` is deprecated
                       (title 'uuid)
                       (mount-point "/boot/efi")
                       (type "vfat"))
                      %base-file-systems))

 (users (cons (user-account
               (name "john")
               (comment "idiot man")
               (group "users")
               (supplementary-groups '("wheel"
                                       "netdev"
                                       "audio"
                                       "video"
                                       "lp"))
               (home-directory "/home/john")
               ;; TODO: Figure out fish environmanet issues.
               ;; (shell #~(string-append #$fish "/bin/fish"))
               )
              %base-user-accounts))

 (packages (cons*
            ;; window manager related
            ;; TODO Use these when fixed
            ;; ghc-xmonad-contrib
            ;; xmonad
            ;; take care of missing deps
            (package
             (name "my-xmonad")
             (version "0.13")
             (synopsis "Tiling window manager")
             (source (origin
                      (method url-fetch)
                      (uri (string-append "mirror://hackage/package/xmonad/"
                                          "xmonad-" version ".tar.gz"))
                      (sha256
                       (base32
                        "1jh3lcs20qpna36fa5a0r174xqrsxhj10x1rm5vwf64zariipy7r"))))
             (build-system haskell-build-system)
             (inputs
              `(("ghc-extensible-exceptions" ,ghc-extensible-exceptions)
                ("ghc-mtl"                   ,ghc-mtl)
                ("ghc-quickcheck"            ,ghc-quickcheck)
                ("ghc-setlocale"             ,ghc-setlocale)
                ("ghc-utf8-string"           ,ghc-utf8-string)
                ("ghc-x11"                   ,ghc-x11)))
             (propagated-inputs `(("gcc-toolchain"             ,gcc-toolchain)
                                  ("ghc-xmonad-contrib"        ,ghc-xmonad-contrib)))
             (arguments
              `(#:phases
                (modify-phases %standard-phases
                               (add-after
                                'install 'install-xsession
                                (lambda _
                                  (let* ((xsessions (string-append %output "/share/xsessions")))
                                    (mkdir-p xsessions)
                                    (call-with-output-file
                                        (string-append xsessions "/my-xmonad.desktop")
                                      (lambda (port)
                                        (format port "~
                    [Desktop Entry]~@
                    Name=~a~@
                    Comment=~a~@
                    Exec=~a/bin/xmonad~@
                    Type=Application~%" ,name ,synopsis %output)))))))))
             (home-page "http://xmonad.org")
             (description
              "My packaging of xmonad to fix deps on gcc tools.")
             (license license:bsd-3))
            ;; TODO: Use when supports alsa
            ;; xmobar
            (package
             (name "my-xmobar")
             (version "0.26")
             (source (origin
                      (method url-fetch)
                      (uri (string-append "mirror://hackage/package/xmobar/"
                                          "xmobar-" version ".tar.gz"))
                      (sha256
                       (base32
                        "19g40vqj3cs94i27f66194k7d5cazrv1lx54bz9kc0qy2npxjzgz"))))
             (build-system haskell-build-system)
             (native-inputs
              `(("ghc-hspec" ,ghc-hspec)
                ("hspec-discover" ,hspec-discover)))
             (inputs
              `(("ghc-hinotify" ,ghc-hinotify)
                ("ghc-http" ,ghc-http)
                ("ghc-iwlib" ,ghc-iwlib)
                ("ghc-parsec" ,ghc-parsec)
                ("ghc-regex-compat" ,ghc-regex-compat)
                ("ghc-stm" ,ghc-stm)
                ("ghc-x11-xft" ,ghc-x11-xft)
                ("libxpm" ,libxpm)))
             (arguments
              `(#:configure-flags
                (list (string-append "--flags="
                                     (string-join (list "with_alsa"
                                                        "with_inotify"
                                                        "with_iwlib"
                                                        "with_threaded"
                                                        "with_utf8"
                                                        "with_weather"
                                                        "with_xft"
                                                        "with_xpm")
                                                  " ")))))
             (home-page "http://xmobar.org")
             (synopsis "Minimalistic text based status bar")
             (description
              "My version of xmobar with alsa")
             (license license:bsd-3))
            rofi

            ;;for HTTPS access
            nss-certs

            ;; X related
            ;; setxkbmap
            ;; xrandr
            ;; xfontsel
            ;; mkfontdir
            ;; mkfontscale
            ;; compton
            ;; xcape
            ;; xclip
            ;; feh

            ;; essentials
            lsof
            inetutils
            git
            fish
            openssh
            gnupg
            htop
            ncurses
            tmux

            ;; text editors
            ;; vim
            ;; emacs

            ;; TODO: Create alacritty
            ;; alacritty
            ;; termite

            ;; web browser
            ;; qutebrowser

            ;; fonts
            ;; font-adobe-source-code-pro
            ;; font-fantasque-sans

            %base-packages))

 (services (cons*
            ;; TODO: buy a libre bluetooth device
            ;; (bluetooth-service #:auto-enable? #t)
            (service openssh-service-type)
            (console-keymap-service "/home/john/dotfiles/minimal/Caps2Ctrl.map")
            ;; TODO: Build the terminus psf font from powerline
            ;; (console-fonts "")
            %desktop-services))

 ;; Allow resolution of '.local' host names with mDNS.
 (name-service-switch %mdns-host-lookup-nss))
