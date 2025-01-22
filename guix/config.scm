(define-module (config)
  #:use-module (gnu)
  #:use-module (guix gexp)
  #:use-module (ice-9 match)

  #:use-module ((gnu packages curl) #:prefix curl:)
  #:use-module ((gnu packages emacs) #:prefix emacs:)
  #:use-module ((gnu packages fonts) #:prefix fonts:)
  #:use-module ((gnu packages gnupg) #:prefix gnupg:)
  #:use-module ((gnu packages linux) #:prefix linux:)
  #:use-module ((gnu packages ncurses) #:prefix ncurses:)
  #:use-module ((gnu packages security-token) #:prefix security-token:)
  #:use-module ((gnu packages shells) #:prefix shells:)
  #:use-module ((gnu packages ssh) #:prefix ssh:)
  #:use-module ((gnu packages tmux) #:prefix tmux:)
  #:use-module ((gnu packages version-control) #:prefix version-control:)
  #:use-module ((gnu packages vim) #:prefix vim:)
  #:use-module ((gnu packages xdisorg) #:prefix xdisorg:)
  ;; FIXME: prefixing breaks g-exps
  #:use-module (gnu packages gl)
  #:use-module (gnu packages xorg)

  #:use-module ((gnu services audio) #:prefix audio:)
  #:use-module ((gnu services base) #:prefix base:)
  #:use-module ((gnu services cups) #:prefix cups:)
  #:use-module ((gnu services dbus) #:prefix dbus:)
  #:use-module ((gnu services desktop) #:prefix desktop:)
  #:use-module ((gnu services dns) #:prefix dns:)
  #:use-module ((gnu services networking) #:prefix networking:)
  #:use-module ((gnu services nix) #:prefix nix:)
  #:use-module ((gnu services pm) #:prefix pm:)
  #:use-module ((gnu services security-token) #:prefix security-token:)
  #:use-module ((gnu services shepherd) #:prefix shepherd:)
  #:use-module ((gnu services ssh) #:prefix ssh:)
  #:use-module ((gnu services sound) #:prefix sound:)
  #:use-module ((gnu services virtualization) #:prefix virtualization:)
  ;; FIXME: prefixing breaks g-exps
  #:use-module (gnu services xorg)

  #:use-module ((gnu system setuid) #:prefix setuid:)

  #:use-module ((nongnu packages linux) #:prefix nongnu:)
  #:use-module ((nongnu system linux-initrd) #:prefix nongnu:))

(define username "john")

(define john
  (user-account
   (name username)
   (comment "idiot man")
   (group "users")
   (supplementary-groups
    '("wheel" "netdev" "audio" "video" "lp" "kvm"))
   (shell (file-append shells:fish "/bin/fish"))))

(define-public ctrl-nocaps
  (keyboard-layout "us" #:options '("ctrl:nocaps")))

(define terminus-psf-font
  (file-append
   fonts:font-terminus "/share/consolefonts/ter-v22n.psf.gz"))

(define my-services
  (cons*
   (service desktop:bluetooth-service-type (desktop:bluetooth-configuration
                                            (auto-enable? #t)))
   (service sound:alsa-service-type)
   (service desktop:cups-pk-helper-service-type)
   (service cups:cups-service-type (cups:cups-configuration (web-interface? #t)))
   (service dns:dnsmasq-service-type (dns:dnsmasq-configuration
                                      (servers '("1.1.1.1"))))
   (service dbus:dbus-root-service-type)
   (service desktop:elogind-service-type)
   desktop:fontconfig-file-system-service
   (service nix:nix-service-type (nix:nix-configuration
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
   (service audio:mpd-service-type (audio:mpd-configuration
                                    (user john)))
   (service networking:network-manager-service-type)
   (service networking:ntp-service-type)
   (service ssh:openssh-service-type (ssh:openssh-configuration
                                      (challenge-response-authentication? #f)
                                      (password-authentication? #f)))
   (service pam-limits-service-type (list
                                     (pam-limits-entry username 'both 'nofile 100000)))
   desktop:polkit-wheel-service
   (service pm:tlp-service-type (pm:tlp-configuration
                                 (tlp-default-mode "BAT")
                                 (usb-autosuspend? #f)))
   (service security-token:pcscd-service-type)
   (service gpm-service-type (gpm-configuration))
   (service virtualization:qemu-binfmt-service-type (virtualization:qemu-binfmt-configuration
                                                     (platforms
                                                      (virtualization:lookup-qemu-platforms "arm" "aarch64" "mips64el"))))
   (service desktop:udisks-service-type)
   (service networking:usb-modeswitch-service-type)
   (service networking:wpa-supplicant-service-type)

   (service screen-locker-service-type (screen-locker-configuration
                                        (name "xlock")
                                        (program (file-append xdisorg:xlockmore "/bin/xlock"))))

    (udev-rules-service 'fido2 security-token:libfido2 #:groups '("plugdev"))

    ;; The following is for xorg without display manager
    (service desktop:x11-socket-directory-service-type)
    (udev-rules-service 'light linux:light)
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
    (bootloader
     (bootloader-configuration
      (bootloader grub-efi-bootloader)
      (targets '("/boot/efi"))
      (keyboard-layout ctrl-nocaps)))
    (kernel nongnu:linux)
    (initrd nongnu:microcode-initrd)
    (firmware `(,nongnu:linux-firmware))
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
     `(,john
       ,@%base-user-accounts))
    (packages
     `(;; for HTTPS access
       ,curl:curl
       ;; essentials
       ,linux:iproute ,version-control:git ,ssh:openssh ,gnupg:gnupg ,ncurses:ncurses ,tmux:tmux
       ;; ???
       ,glibc-utf8-locales
       ;; text editors
       ,vim:neovim ,emacs:emacs-no-x
       ;; for keyboards
       ,linux:bluez
       ;; backlight config
       ,linux:light
       ,@%base-packages))
    (services my-services)
    ;; Allow resolution of '.local' host names with mDNS.
    (name-service-switch %mdns-host-lookup-nss)))

ecenter
