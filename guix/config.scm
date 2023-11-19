(use-modules (gnu) (gnu system setuid) (guix gexp) (ice-9 match))
(use-package-modules admin base certs cups curl docker emacs
                     fonts fontutils gl gnupg linux ncurses
                     package-management security-token
                     shells shellutils ssh
                     suckless tmux version-control vim vpn
                     web-browsers xdisorg xorg)
(use-service-modules audio base cups dbus desktop dns docker
                     networking nix pm shepherd ssh sound
                     virtualization xorg)

(define username "john")

(define cst-trackball
  "Section \"InputClass\"
    Identifier \"CST Trackball\"
    MatchProduct \"CST CST USB UNITRAC\"
    Driver \"libinput\"
    Option \"AccelSpeed\" \"1\"
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
   font-terminus "/share/consolefonts/ter-v22n.psf.gz"))

(define my-services
  (cons*
   (bluetooth-service #:auto-enable? #t)
   (service alsa-service-type)
   (service cups-pk-helper-service-type)
   (service cups-service-type (cups-configuration (web-interface? #t)))
   (service dnsmasq-service-type (dnsmasq-configuration
                                  (servers '("1.1.1.1"))))
   (service docker-service-type)
   (dbus-service)
   (service elogind-service-type)
   fontconfig-file-system-service
   (service nix-service-type (nix-configuration
                              (package nix)
                              (extra-config '("keep-derivations = true"
                                              "keep-outputs = true"
                                              "sandbox-paths = /bin/sh=/nix/store/iyqah3h6ywjdxl6xmsdxqv26x71i091v-busybox-static-x86_64-unknown-linux-musl-1.32.1"))))
   (service kmscon-service-type (kmscon-configuration
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
                              (user username)))
   (service network-manager-service-type)
   (service ntp-service-type)
   (service openssh-service-type (openssh-configuration
                                  (challenge-response-authentication? #f)
                                  (password-authentication? #f)))
   (pam-limits-service
    (list
     (pam-limits-entry "john" 'both 'nofile 100000)))
   polkit-wheel-service
   (service tlp-service-type (tlp-configuration
                              (tlp-default-mode "BAT")
                              (usb-autosuspend? #f)))
   (service gpm-service-type (gpm-configuration))
   (service qemu-binfmt-service-type (qemu-binfmt-configuration
                                      (platforms
                                       (lookup-qemu-platforms "arm" "aarch64" "mips64el"))))
   (udisks-service)
   (service usb-modeswitch-service-type)
   (service wpa-supplicant-service-type)

   (screen-locker-service slock)
   (screen-locker-service xlockmore "xlock")

    (udev-rules-service 'fido2 libfido2 #:groups '("plugdev"))

    ;; The following is for xorg without display manager
    x11-socket-directory-service
    (udev-rules-service 'light light)
    ; For xorg sans display manager (gentoo wiki)
    (udev-rules-service 'xorg-rootless (udev-rule
                                        "99-dev-input-group.rules"
                                        "SUBSYSTEM==\"input\", ACTION==\"add\", GROUP=\"input\""))
    (modify-services %base-services
      (console-font-service-type s =>
                                 (map
                                  (match-lambda ((tty . font) `(,tty . ,terminus-psf-font)))
                                  s)))))

(operating-system
  (host-name "ecenter")
  (timezone "America/Denver")
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
       (name username)
       (comment "idiot man")
       (group "users")
       (supplementary-groups
        '("wheel" "netdev" "audio" "video" "lp"))
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
       (user username)
       (group "input")
       (setuid? #f)
       (setgid? #t))
     ,(setuid-program
       (program startx)
       (user username)
       (group "input")
       (setuid? #f)
       (setgid? #t))
     ,@%setuid-programs))
  (services my-services)
  ;; Allow resolution of '.local' host names with mDNS.
  (name-service-switch %mdns-host-lookup-nss))
