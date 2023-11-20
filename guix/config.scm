(define-module (config)
  #:use-module (ice-9 match)
  #:use-module (guix gexp)

  #:use-module (gnu)
  #:use-module (gnu system setuid)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages docker)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages security-token)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages shellutils)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages suckless)
  #:use-module (gnu packages tmux)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages vpn)
  #:use-module (gnu packages web-browsers)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)

  #:use-module (gnu services audio)
  #:use-module (gnu services base)
  #:use-module (gnu services cups)
  #:use-module (gnu services dbus)
  #:use-module (gnu services desktop)
  #:use-module (gnu services dns)
  #:use-module (gnu services docker)
  #:use-module (gnu services networking)
  #:use-module (gnu services nix)
  #:use-module (gnu services pm)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services ssh)
  #:use-module (gnu services sound)
  #:use-module (gnu services virtualization)
  #:use-module (gnu services xorg))

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
   (service bluetooth-service-type (bluetooth-configuration
                                    (auto-enable? #t)))
   (service alsa-service-type)
   (service cups-pk-helper-service-type)
   (service cups-service-type (cups-configuration (web-interface? #t)))
   (service dnsmasq-service-type (dnsmasq-configuration
                                  (servers '("1.1.1.1"))))
   (service docker-service-type)
   (service dbus-root-service-type)
   (service elogind-service-type)
   fontconfig-file-system-service
   (service nix-service-type (nix-configuration
                              (package nix)
                              (extra-config '("keep-derivations = true\n"
                                              "keep-outputs = true\n"))))
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
   (service pam-limits-service-type (list
                                     (pam-limits-entry "john" 'both 'nofile 100000)))
   polkit-wheel-service
   (service tlp-service-type (tlp-configuration
                              (tlp-default-mode "BAT")
                              (usb-autosuspend? #f)))
   (service gpm-service-type (gpm-configuration))
   (service qemu-binfmt-service-type (qemu-binfmt-configuration
                                      (platforms
                                       (lookup-qemu-platforms "arm" "aarch64" "mips64el"))))
   (service udisks-service-type)
   (service usb-modeswitch-service-type)
   (service wpa-supplicant-service-type)

   (service screen-locker-service-type (screen-locker-configuration
                                        (name "xlock")
                                        (program (file-append xlockmore "/bin/xlock"))))

    (udev-rules-service 'fido2 libfido2 #:groups '("plugdev"))

    ;; The following is for xorg without display manager
    (service x11-socket-directory-service-type)
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

(define ecenter
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
    (swap-devices `(,(swap-space
                      (target (uuid "7bcddb1d-889b-4cd4-8335-dc7c4a1a358d")))))
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
    (name-service-switch %mdns-host-lookup-nss)))

ecenter
