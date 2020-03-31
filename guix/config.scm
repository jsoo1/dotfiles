(use-modules (gnu)
             ((gnu packages admin) #:select (htop inetutils tree))
             ((gnu packages base) #:select (glibc-utf8-locales))
             ((gnu packages certs) #:select (nss-certs))
             ((gnu packages curl) #:select (curl))
             ((gnu packages docker) #:select (docker-cli))
             ((gnu packages emacs) #:select (emacs-next-no-x))
             ((gnu packages fonts)
              #:select (font-adobe-source-code-pro
                        font-iosevka
                        font-tamzen))
             ((gnu packages fontutils) #:select (fontconfig))
             ((gnu packages gnupg) #:select (gnupg))
             ((gnu packages linux) #:select (bluez light))
             ((gnu packages ncurses) #:select (ncurses))
             ((gnu packages shells) #:select (fish))
             ((gnu packages shellutils) #:select (fzy))
             ((gnu packages ssh) #:select (openssh))
             ((gnu packages tmux) #:select (tmux))
             ((gnu packages version-control) #:select (git))
             ((gnu packages vim) #:select (vim))
             ((gnu packages web-browsers) #:select (lynx))
             ((gnu packages xdisorg) #:select (xcape))
             ((gnu packages xorg) #:select (xinit))
             ((gnu services)
              #:select (special-files-service-type))
             ((gnu services base)
              #:select (gpm-service-type
                        gpm-configuration))
             ((gnu services dbus)
              #:select (dbus-service))
             ((gnu services desktop)
              #:select (bluetooth-service
                        %desktop-services
                        fontconfig-file-system-service
                        elogind-service-type
                        polkit-wheel-service
                        udisks-service
                        x11-socket-directory-service))
             ((gnu services dns)
              #:select (dnsmasq-service-type
                        dnsmasq-configuration))
             (gnu services docker)
             ((gnu services networking)
              #:select (network-manager-service-type
                        network-manager-configuration
                        ntp-service-type
                        usb-modeswitch-service-type
                        wpa-supplicant-service-type))
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
             ((gnu services sound)
              #:select (alsa-service-type))
             ((gnu services virtualization)
              #:select (qemu-binfmt-service-type
                        qemu-binfmt-configuration
                        lookup-qemu-platforms))
             ((gnu services xorg)
              #:select (gdm-service-type
                        gdm-configuration
                        xorg-configuration
                        xorg-start-command))
             (guix gexp)
             (ice-9 match)
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

;; Workaround for https://github.com/alols/xcape/issues/62
;; I would prefer nocaps
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
          %base-file-systems))
  (swap-devices '("/dev/sda7"))
  (users
   (cons (user-account
          (name "john")
          (comment "idiot man")
          (group "users")
          (supplementary-groups
           '("wheel"
             "netdev"
             "audio"
             "video"
             "tty"
             "lp"))
          (home-directory "/home/john")
          (shell #~(string-append #$fish "/bin/fish")))
         %base-user-accounts))
  (packages
   (cons*
    ;; nice tty font
    font-tamzen
    ;; kmscon fonts
    fontconfig font-iosevka
    ;; backlight config
    light
    ;;for HTTPS access
    curl nss-certs
    ;; essentials
    inetutils git fish openssh gnupg htop ncurses tmux fzy lynx
    tree yaft glibc-utf8-locales
    ;; text editors
    vim emacs-next-no-x
    ;; for keyboards
    bluez
    %base-packages))
  (setuid-programs
   (cons*
    #~(string-append #$docker-cli "/bin/docker")
    "/home/john/.xserverrc"
    %setuid-programs))
  (services
   (cons*
    (simple-service 'light-udev-rules
                    udev-service-type `(,light))
    ;; TODO: Add service for modprobe.d modules?
    (bluetooth-service #:auto-enable? #t)
    (service alsa-service-type)
    (service dnsmasq-service-type
             (dnsmasq-configuration
              (servers '("1.1.1.1"))))
    (service docker-service-type)
    (dbus-service)
    (service elogind-service-type)
    fontconfig-file-system-service
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
    (service mingetty-service-type (mingetty-configuration
                                    (tty "tty7")))
    (service network-manager-service-type)
    (service ntp-service-type)
    (service openssh-service-type
             (openssh-configuration
              (challenge-response-authentication? #f)
              (password-authentication? #f)))
    polkit-wheel-service
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
    (udisks-service)
    (service usb-modeswitch-service-type)
    (service wpa-supplicant-service-type)
    x11-socket-directory-service
    (modify-services %base-services
      (special-files-service-type
       files =>
       `(("/home/john/.xserverrc"
          ,(xorg-start-command
            (xorg-configuration
             (keyboard-layout ctrl-nocaps)
             (extra-config `(,cst-trackball ,touchscreen-disable)))))
         ,@files))
      (console-font-service-type
       s =>
       (cons
        `("tty7" . ,tamzen-psf-font)
        (map
         (match-lambda
           ((tty . font) `(,tty . ,tamzen-psf-font)))
         s))))))
  ;; Allow resolution of '.local' host names with mDNS.
  (name-service-switch %mdns-host-lookup-nss))
