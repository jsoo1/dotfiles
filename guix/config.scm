(use-modules (gnu) (gnu system nss))
(use-service-modules desktop)
(use-package-modules bootloaders suckless xdisorg certs wm)

(operating-system
  (host-name "ecenter")
  (timezone "America/Los_Angeles")
  (locale "en_US.utf8")

  (bootloader (bootloader-configuration
                (bootloader grub-efi-bootloader)
                (target "/boot/efi")))

  (file-systems
    (cons*
      (file-system
        (device "guix-large")
        (title 'label)
        (mount-point "/")
        (type "ext4"))
      (file-system
        (device (uuid "60E8-6B6F" 'fat))
        (title 'uuid)
        (type "vfat")
        (mount-point "/boot/efi"))
      %base-file-systems))

  (swap-devices '("/dev/sda7"))

  (users (cons (user-account
                (name "john")
                (comment "idiot man")
                (group "users")
                (supplementary-groups '("wheel" "netdev"
                                        "audio" "video"))
                (home-directory "/home/john"))
               %base-user-accounts))

  (packages (cons* xmonad
                   dmenu
                   rofi
                   nss-certs         ;for HTTPS access
                   %base-packages))

  (services %desktop-services)

  ;; Allow resolution of '.local' host names with mDNS.
  (name-service-switch %mdns-host-lookup-nss))
