(use-modules (gnu)
             ((gnu packages admin) #:select (htop inetutils tree))
             ((gnu packages base) #:select (glibc-utf8-locales))
             ((gnu packages certs) #:select (nss-certs))
             ((gnu packages curl) #:select (curl))
             ((gnu packages docker) #:select (docker-cli))
             ((gnu packages emacs) #:select (emacs-next))
             ((gnu packages fonts)
              #:select (font-adobe-source-code-pro
                        font-fantasque-sans
                        font-iosevka
                        font-tamzen))
             ((gnu packages fontutils) #:select (fontconfig))
             ((gnu packages gnupg) #:select (gnupg))
             ((gnu packages linux) #:select (bluez light))
             ((gnu packages lsof) #:select (lsof))
             ((gnu packages ncurses) #:select (ncurses))
             ((gnu packages shells) #:select (fish))
             ((gnu packages shellutils) #:select (fzy))
             ((gnu packages ssh) #:select (openssh))
             ((gnu packages tmux) #:select (tmux))
             ((gnu packages version-control) #:select (git))
             ((gnu packages vim) #:select (vim))
             ((gnu packages web-browsers) #:select (lynx))
             ((gnu packages xdisorg) #:select (xcape))
             ((gnu packages wm) #:select (xmonad ghc-xmonad-contrib xmobar))
             ((gnu services base)
              #:select (gpm-service-type
                        gpm-configuration))
             ((gnu services desktop)
              #:select (bluetooth-service
                        %desktop-services))
             ((gnu services dns)
              #:select (dnsmasq-service-type
                        dnsmasq-configuration))
             (gnu services docker)
             ((gnu services networking)
              #:select (network-manager-service-type
                        network-manager-configuration))
             ((gnu services pm)
              #:select (thermald-configuration
                        thermald-service-type
                        tlp-configuration
                        tlp-service-type))
             ((gnu services shepherd)
              #:select (shepherd-service
                        shepherd-service-type))
             ((gnu services ssh)
              #:select (openssh-service-type
                        openssh-configuration))
             ;; ((gnu services xdisorg)
             ;;  #:select (xcape-configuration
             ;;            xcape-service-type))
             ((gnu services virtualization)
              #:select (qemu-binfmt-service-type
                        qemu-binfmt-configuration
                        lookup-qemu-platforms))
             ((gnu services xorg)
              #:select (gdm-service-type
                        gdm-configuration
                        xorg-configuration))
             (guix gexp)
             ((dmenu) #:select (my-dmenu))
             ((yaft) #:select (yaft)))

(define cst-trackball
  "Section \"InputClass\"
    Identifier \"CST Trackball\"
    MatchProduct \"CST CST USB UNITRAC\"
    Driver \"libinput\"
    Option \"AccelSpeed\" \"2.0\"
EndSection\n")

(define touchscreen-disable
  "Section \"InputClass\"
    Identifier \"Touchscreen Disable\"
    MatchIsTouchscreen \"on\"
    MatchProduct \"ELAN Touchscreen\"
    Option \"ignore\" \"on\"
EndSection\n")

(define ctrl-nocaps (keyboard-layout "us" #:options '("ctrl:nocaps")))

(define tamzen-psf-font
  (file-append
   font-tamzen "/share/kbd/consolefonts/TamzenForPowerline10x20.psf"))

(operating-system
 (host-name "ecenter")
 (timezone "America/Los_Angeles")
 (locale "en_US.utf8")
 (locale-libcs (list glibc-2.28 (canonical-package glibc)))
 (keyboard-layout ctrl-nocaps)
 (initrd-modules %base-initrd-modules)
 (bootloader
  (bootloader-configuration
   (bootloader grub-efi-bootloader)
   (target "/boot/efi")
   (keyboard-layout ctrl-nocaps)))
 (file-systems
  (cons* (file-system
          (device
           (uuid "462563db-3f82-44d2-829c-eb2bce9fd0e0" 'ext4))
          (mount-point "/")
          (type "ext4"))
         (file-system
          (device (uuid "60E8-6B6F" 'fat))
          (mount-point "/boot/efi")
          (type "vfat"))
         ;; (file-system
         ;;  (device
         ;;   (uuid "42533fd0-59d6-448c-9a7d-cf10d13647ff" 'btrfs))
         ;;  (mount-point "/mnt/sdcard")
         ;;  (type "btrfs"))
         %base-file-systems))
 (swap-devices '("/dev/sda7"))
 (users
  (cons (user-account
         (name "john")
         (comment "idiot man")
         (group "users")
         (supplementary-groups
          '("wheel" "netdev" "audio" "video" "lp"))
         (home-directory "/home/john")
         (shell #~(string-append #$fish "/bin/fish")))
        %base-user-accounts))
 (packages
  (cons*
   ;; nice tty font
   font-tamzen
   ;; kmscon fonts
   fontconfig font-fantasque-sans font-iosevka
   ;; window manager related
   xmonad ghc-xmonad-contrib xmobar my-dmenu
   ;; backlight config
   light
   ;;for HTTPS access
   curl nss-certs
   ;; essentials
   lsof inetutils git fish openssh gnupg htop ncurses tmux fzy lynx
   tree yaft glibc-utf8-locales
   ;; text editors
   vim emacs-next
   ;; for keyboards
   bluez
   %base-packages))
 (setuid-programs
  (append
   `(,#~(string-append #$docker-cli "/bin/docker")
     ,#~(string-append #$light "/bin/light"))
   %setuid-programs))
 (services
  (cons*
   ;; TODO: Add service for modprobe.d modules?
   (bluetooth-service #:auto-enable? #t)
   (service dnsmasq-service-type
            (dnsmasq-configuration
             (servers '("1.1.1.1"))))
   ;; (service xcape-service-type
   ;;          (xcape-configuration
   ;;           "john"
   ;;           '(("Control_L" . "Escape"))))
   (service docker-service-type)
   (service kmscon-service-type
            (kmscon-configuration
             (virtual-terminal "tty8")
             ;; (scrollback "100000")
             ;; (font-name "'Fantasque Sans Mono'")
             ;; (font-size "15")
             ;; (xkb-layout "us")
             ;; (xkb-variant "")
             ;; (xkb-options "ctrl:nocaps")
             ))
   (service openssh-service-type
            (openssh-configuration
             (challenge-response-authentication? #f)
             (password-authentication? #f)))
   (service thermald-service-type
            (thermald-configuration))
   (service tlp-service-type
            (tlp-configuration
             (tlp-default-mode "BAT")
             (usb-autosuspend? #f)))
   (service gpm-service-type (gpm-configuration))
   (service qemu-binfmt-service-type
            (qemu-binfmt-configuration
             (platforms (lookup-qemu-platforms "arm" "aarch64" "mips64el"))
             (guix-support? #t)))
   (modify-services
    %desktop-services
    (console-font-service-type
     s =>
     `(("tty1" . "LatGrkCyr-8x16")
       ("tty2" . ,tamzen-psf-font)
       ("tty3" . ,tamzen-psf-font)
       ("tty4" . ,tamzen-psf-font)
       ("tty5" . ,tamzen-psf-font)
       ("tty6" . ,tamzen-psf-font)))
    (gdm-service-type
     c =>
     (gdm-configuration
      (inherit c)
      (xorg-configuration
       (xorg-configuration
        (keyboard-layout ctrl-nocaps)
        (extra-config `(,cst-trackball ,touchscreen-disable)))))))))
 ;; Allow resolution of '.local' host names with mDNS.
 (name-service-switch %mdns-host-lookup-nss))
