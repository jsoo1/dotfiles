(use-modules (gnu)
             (gnu system nss)
             (guix modules))
(use-service-modules desktop ssh)
(use-package-modules admin
                     bootloaders
                     certs
                     compton
                     emacs
                     fonts
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

 (initrd-modules (append '(;; "ahcpi"
                           "shpchp")
                         %base-initrd-modules))

 (bootloader (bootloader-configuration
              (bootloader grub-efi-bootloader)
              (target "/boot/efi")))

 (file-systems (cons* (file-system
                       (device "guix-root")
                       (title 'label)
                       (mount-point "/")
                       (type "ext4"))
                      (file-system
                       (device (uuid "60E8-6B6F" 'fat))
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
               (shell #~(string-append #$fish "/bin/fish")))
              %base-user-accounts))

 (packages (cons*
            ;; window manager related
            xmonad
            xmobar
            rofi
            dmenu

            ;;for HTTPS access
            nss-certs

            ;; X related
            setxkbmap
            xrandr
            xfontsel
            mkfontdir
            mkfontscale
            compton
            xcape
            xclip
            feh

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

            ;; no alacritty yet :(
            termite

            ;; web browser
            qutebrowser

            font-adobe-source-code-pro
            font-fantasque-sans
            %base-packages))

 (services (cons*
            ;; TODO: buy a libre bluetooth device
            ;; (bluetooth-service #:auto-enable? #t)
            (service openssh-service-type)
            %desktop-services))

 ;; Allow resolution of '.local' host names with mDNS.
 (name-service-switch %mdns-host-lookup-nss))
