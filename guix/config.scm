(use-modules
 (gnu)
 ((gnu packages admin) #:select (htop inetutils tree))
 ((gnu packages base) #:select (glibc-utf8-locales))
 ((gnu packages certs) #:select (nss-certs))
 ((gnu packages cups) #:select (cups-filters hplip-minimal))
 ((gnu packages curl) #:select (curl))
 ((gnu packages docker) #:select (docker-cli))
 ((gnu packages emacs) #:select (emacs-no-x))
 ((gnu packages fonts)
  #:select (font-adobe-source-code-pro
            font-iosevka
            font-terminus))
 ((gnu packages fontutils) #:select (fontconfig))
 ((gnu packages gl) #:select (mesa))
 ((gnu packages gnupg) #:select (gnupg))
 ((gnu packages linux)
  #:select (bluez
            iproute
            light
            linux-libre-with-bpf))
 ((gnu packages ncurses) #:select (ncurses))
 ((gnu packages package-management) #:select (nix nix-unstable))
 ((gnu packages shells) #:select (fish))
 ((gnu packages shellutils) #:select (fzy))
 ((gnu packages ssh) #:select (openssh))
 ((gnu packages suckless) #:select (slock))
 ((gnu packages tmux) #:select (tmux))
 ((gnu packages version-control) #:select (git))
 ((gnu packages vim) #:select (neovim))
 ((gnu packages vpn) #:select (wireguard-tools))
 ((gnu packages web-browsers) #:select (lynx))
 ((gnu packages xdisorg) #:select (xlockmore))
 ((gnu packages xorg)
  #:select (xkbcomp
            xinit
            xorg-server
            xkeyboard-config))
 (gnu services)
 ((gnu services audio)
   #:select (mpd-service-type mpd-configuration))
 ((gnu services base)
  #:select (gpm-service-type
            gpm-configuration))
 ((gnu services cups)
  #:select (cups-service-type
            cups-configuration))
 ((gnu services dbus)
  #:select (dbus-service))
 ((gnu services desktop)
  #:select (bluetooth-service
            %desktop-services
            fontconfig-file-system-service
            elogind-service-type
            polkit-wheel-service
            cups-pk-helper-service-type
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
 ((gnu services nix)
  #:select (nix-service-type nix-configuration))
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
 ((gnu services sound)
  #:select (alsa-service-type))
 ((gnu services virtualization)
  #:select (qemu-binfmt-service-type
            qemu-binfmt-configuration
            lookup-qemu-platforms))
 (gnu services xorg)
 (gnu system setuid)
 (guix gexp)
 (ice-9 match))

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

(define xorg-conf
  (xorg-configuration
   (keyboard-layout ctrl-nocaps)
   (extra-config `(,cst-trackball))
   (server-arguments
    `("-keeptty" ,@%default-xorg-server-arguments))))

(define startx
  (program-file
   "startx"
   #~(begin
       (setenv
        "XORG_DRI_DRIVER_PATH" (string-append #$mesa "/lib/dri"))
       (setenv
        "XKB_BINDIR" (string-append #$xkbcomp "/bin"))

       ;; X doesn't accept absolute paths when run with suid
       (apply
        execl
        (string-append #$xorg-server "/bin/X")
        (string-append #$xorg-server "/bin/X")
        "-config" #$(xorg-configuration->file xorg-conf)
        "-configdir" #$(xorg-configuration-directory
                        (xorg-configuration-modules xorg-conf))
        "-logverbose" "-verbose" "-terminate"
        (append '#$(xorg-configuration-server-arguments xorg-conf)
                (cdr (command-line)))))))

(define terminus-psf-font
  (file-append
   font-terminus "/share/consolefonts/ter-v32n.psf.gz"))

(define my-services
  (cons*
    ;; TODO: Add service for modprobe.d modules?
    (bluetooth-service #:auto-enable? #t)
    (service alsa-service-type)
    (service cups-pk-helper-service-type)
    (service cups-service-type
             (cups-configuration
              (web-interface? #t)
              (extensions
               `(,cups-filters ,hplip-minimal))
              (browsing? #t)))
    (service dnsmasq-service-type
             (dnsmasq-configuration
              (servers '("1.1.1.1"))))
    (service docker-service-type)
    (dbus-service)
    (service elogind-service-type)
    fontconfig-file-system-service
    (service nix-service-type
             (nix-configuration
              (package nix)
              (extra-config '("keep-derivations = true"
                              "keep-outputs = true"))))
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
    (service mpd-service-type (mpd-configuration
                               (user "john")))
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
              (platforms
               (lookup-qemu-platforms "arm" "aarch64" "mips64el"))))
    (udisks-service)
    (service usb-modeswitch-service-type)
    (service wpa-supplicant-service-type)

    (screen-locker-service slock)
    (screen-locker-service xlockmore "xlock")

    ;; The following two are for xorg without display manager
    x11-socket-directory-service
    (modify-services %base-services
      (udev-service-type
       c =>
       (udev-configuration
        (inherit c)
        (rules
         `(,light ; Use light without sudo
           ,(udev-rule ; For xorg sans display manager (gentoo wiki)
             "99-dev-input-group.rules"
             "SUBSYSTEM==\"input\", ACTION==\"add\", GROUP=\"input\"")
           ,@(udev-configuration-rules c)))))
      (console-font-service-type
       s =>
       (map
        (match-lambda ((tty . font) `(,tty . ,terminus-psf-font)))
        s)))))

(operating-system
  ;; (host-name "moon1")
  (host-name "ecenter")
  (timezone "America/Los_Angeles")
  (locale "en_US.utf8")
  (keyboard-layout ctrl-nocaps)
  (initrd-modules %base-initrd-modules)
  (bootloader
   (bootloader-configuration
    (bootloader grub-efi-bootloader)
    (targets '("/boot/efi"))
    (keyboard-layout ctrl-nocaps)))
  (kernel linux-libre-with-bpf)
  (file-systems
    (cons* (file-system
             (mount-point "/boot/efi")
             (device (uuid "F0B3-65A1" 'fat32))
             (type "vfat"))
           (file-system
             (mount-point "/")
             (device
               (uuid "b5fc5dff-5d24-4292-8ea7-933c2a533607"
                     'ext4))
             (type "ext4"))
           %base-file-systems))
  (swap-devices (list (uuid "7bcddb1d-889b-4cd4-8335-dc7c4a1a358d")))
  (users
   `(,(user-account
       (name "john")
       (comment "idiot man")
       (group "users")
       (supplementary-groups
        '("wheel" "netdev" "audio" "video" "lp"))
       (home-directory "/home/john")
       (shell (file-append fish "/bin/fish")))
     ,@%base-user-accounts))
  (packages
   `(;; for HTTPS access
     ,curl ,nss-certs
     ;; essentials
     ,iproute ,git ,openssh ,gnupg ,ncurses
     ;; ???
     ,glibc-utf8-locales
     ;; text editors
     ,neovim ,emacs-no-x
     ;; for keyboards
     ,bluez
     ;; backlight config
     ,light
     ,@%base-packages))
  (setuid-programs
   `(,(setuid-program
       (program (file-append docker-cli "/bin/docker")))
     ;; Stuff for xorg without display manager.
     ;; startx and X need to be in setuid-programs.
     ;; They also need extra tweaks in the chown-file service below.
     ,(setuid-program
       (program (file-append xorg-server "/bin/X"))
       (user "john")
       (group "input")
       (setuid? #f)
       (setgid? #t))
     ,(setuid-program
       (program startx)
       (user "john")
       (group "input")
       (setuid? #f)
       (setgid? #t))
     ,@%setuid-programs))
  (services my-services)
  ;; Allow resolution of '.local' host names with mDNS.
  (name-service-switch %mdns-host-lookup-nss))
