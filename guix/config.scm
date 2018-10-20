(use-modules (gnu)
             (guix modules)
             (guix packages)
             (guix download)
             (guix build-system)
             (guix build-system haskell)
             ((guix licenses) #:prefix license:)
             (xmonad))
(use-service-modules base
                     desktop
                     ssh)
(use-package-modules admin
                     bootloaders
                     certs
                     commencement
                     compton
                     emacs
                     fonts
                     gnupg
                     haskell
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
                     wm
                     xdisorg
                     xorg)


(operating-system
 (host-name "ecenter")
 (timezone "America/Los_Angeles")
 (locale "en_US.utf8")

 (initrd-modules %base-initrd-modules)

 (bootloader (bootloader-configuration
              (bootloader grub-efi-bootloader)
              (target "/boot/efi")))

 (file-systems (cons* (file-system
                       (device (uuid "462563db-3f82-44d2-829c-eb2bce9fd0e0" 'ext4))
                       (mount-point "/")
                       (type "ext4"))
                      (file-system
                       (device (uuid "60E8-6B6F" 'fat))
                       (mount-point "/boot/efi")
                       (type "vfat"))
                      %base-file-systems))

 (swap-devices '("/dev/sda7"))

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
               ;; TODO: Figure out fish environmnet issues.
               ;; (shell #~(string-append #$fish "/bin/fish"))
               )
              %base-user-accounts))

 (packages (cons*
            ;; window manager related
            ;; TODO: Use when supports alsa
            my-xmonad
            my-ghc-xmonad-contrib
            rofi

            ;;for HTTPS access
            nss-certs

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
            vim
            emacs

            %base-packages))

 (services (cons*
            ;; TODO: Add service for modprobe.d modules
            ;; TODO: Build the terminus psf font from powerline
            ;; (console-fonts "")
            (console-keymap-service "/home/john/dotfiles/minimal/Caps2Ctrl.map")
            (bluetooth-service #:auto-enable? #t)
            (service openssh-service-type)
            %desktop-services))

 ;; Allow resolution of '.local' host names with mDNS.
 (name-service-switch %mdns-host-lookup-nss))

