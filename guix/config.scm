;; This is an operating system configuration template
;; for a "desktop" setup with GNOME and Xfce where the
;; root partition is encrypted with LUKS.

(use-modules (gnu) (gnu system nss))
(use-service-modules desktop)
;; (use-package-modules certs gnome)
(use-package-modules bootloaders suckless xdisorg certs wm)

(operating-system
  (host-name "ecenter")
  (timezone "America/Los_Angeles")
  (locale "en_US.utf8")

  ;; Assuming /dev/sdX is the target hard disk, and "my-root"
  ;; is the label of the target root file system.
  (bootloader (bootloader-configuration
                (bootloader grub-efi-bootloader)
                (target "/boot/efi")))

  (file-systems
    (cons*
      (file-system
        (device "guix-root")
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

  ;; This is where we specify system-wide packages.
  (packages (cons* xmonad
                   dmenu
                   rofi
                   nss-certs         ;for HTTPS access
                   %base-packages))

  ;; Add GNOME and/or Xfce---we can choose at the log-in
  ;; screen with F1.  Use the "desktop" services, which
  ;; include the X11 log-in service, networking with Wicd,
  ;; and more.
  (services %desktop-services)

  ;; Allow resolution of '.local' host names with mDNS.
  (name-service-switch %mdns-host-lookup-nss))
