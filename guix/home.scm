(define-module (home)
  #:use-module (guix gexp)
  #:use-module (guix channels)
  #:use-module (guix packages)
  #:use-module (guix profiles)
  #:use-module ((guix records) #:select (match-record))

  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services dotfiles)
  #:use-module (gnu home services desktop)
  #:use-module (gnu home services guix)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu home services sound)

  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu services xorg)

  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compton)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages gawk)
  #:use-module ((gnu packages gl) #:prefix gl:)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages vim)
  #:use-module ((gnu packages xorg) #:prefix xorg:)
  #:use-module ((gnu packages xdisorg) #:prefix xdisorg:)

  #:use-module (clipmenud)
  #:use-module (dmenu)
  #:use-module (dunst)
  #:use-module (emacs)
  #:use-module (xmonad)

  #:use-module ((config) #:prefix config:)
  #:use-module ((channels) #:prefix channels:)
  #:use-module ((env) #:prefix env:))

;; Emacs
(define emacs-socket-name "term")
(define emacs-service
  (home-emacs-configuration
   (package emacs-next)
   (socket-name emacs-socket-name)))

;; Env Vars
(define-public chromium-flags
  (mixed-text-file "chromium-flags.conf" "\
--enable-features=WebUIDarkMode,CSSColorSchemeUARendering
--force-dark-mode
"))

(define-public editor
  (string-append "emacsclient -t --socket-name=" emacs-socket-name))

(define-public env-vars
  `(("EDITOR" . ,editor)
    ("ALTERNATE_EDITOR" . "nvim")
    ("PATH" . "${HOME}/.local/bin${PATH:+:$PATH}")
    ("CHROMIUM_FLAGS" . ,chromium-flags)
    ("BAT_THEME" . "Solarized (dark)")
    ("SKIM_DEFAULT_OPTIONS" . "-m --color=bw --reverse")))

;; Fish
(define-public config.fish
  (local-file "../fish/config.fish"))

(define-public fish_prompt.fish
  (local-file "../fish/fish_prompt.fish"))

(define-public keybindings.fish
  (local-file "../fish/keybindings.fish"))

(define-public aliases.fish
  (local-file "../fish/aliases.fish"))

(define cst-trackball
  "Section \"InputClass\"
    Identifier \"CST Trackball\"
    MatchProduct \"CST CST USB UNITRAC\"
    Driver \"libinput\"
    Option \"AccelSpeed\" \"1\"
EndSection\n")

(define xorg-conf
  (xorg-configuration
   (keyboard-layout config:ctrl-nocaps)
   (extra-config `(,cst-trackball))
   (server-arguments
    `("-keeptty" ,@%default-xorg-server-arguments))))

(define-public xsession
  (program-file
   "xsession"
   #~(begin
       (system* #$(file-append xorg:xsetroot "/bin/xsetroot")
                "-cursor_name" "left_ptr")

       (spawn #$(file-append xdisorg:clipmenu "/bin/clipmenud")
              '(#$(file-append xdisorg:clipmenu "/bin/clipmenud")))

       (execl #$(file-append my-xmonad "/bin/my-xmonad")
              #$(file-append my-xmonad "/bin/my-xmonad")))))

(define-public startx
  (program-file
   "startx"
   #~(begin
       (setenv
        "XORG_DRI_DRIVER_PATH" (string-append #$(identity gl:mesa) "/lib/dri"))
       (setenv
        "XKB_BINDIR" (string-append #$(identity xorg:xkbcomp) "/bin"))

       (apply
        execl
        (string-append #$(identity xorg:xorg-server) "/bin/X")
        (string-append #$(identity xorg:xorg-server) "/bin/X")
        "-config" #$(xorg-configuration->file xorg-conf)
        "-configdir" #$(xorg-configuration-directory
                        (xorg-configuration-modules xorg-conf))
        "-logverbose" "-verbose" "-terminate"
        (append '#$(xorg-configuration-server-arguments xorg-conf)
                (cdr (command-line)))))))

(define-public startx.scm
  #~(when (string= "/dev/tty1" (readlink (readlink "/dev/stdin")))
      (execl #$(file-append xorg:xinit "/bin/xinit")
             #$(file-append xorg:xinit "/bin/xinit")
             #$xsession "--" #$startx "vt1")))

(define fish-config
  (home-fish-extension
   (config `(,config.fish ,aliases.fish ,fish_prompt.fish ,keybindings.fish))
   (aliases `(("vim" . "nvim")))
   (abbreviations
    `(("gst" . "git status")
      ("gco" . "git checkout")
      ("ls" . "eza")
      ("ll" . "eza -l")
      ("lsa" . "eza -la")
      ("lsah" . "eza -la")
      ("tree" . "eza -Ta")
      ("tma" . "tmux attach -t")
      ("tml" . "tmux list-sessions")
      ("tmux" . "tmux new-session -A -s (basename (pwd) | tr '.' '-') -n emacs")))))

;; Sound
(define pipewire-config
  (home-pipewire-configuration
   (enable-pulseaudio? #t)))

;; Base Env
;; Provides what would normally be in a guix operating-system,
;; but sometimes this is run in a container.
(define-public base-env
  (simple-service 'base-packages-service-type
                  home-profile-service-type
                  `(,coreutils ,bash ,neovim ,procps ,grep ,gawk ,sed)))

;; Dotfiles
(define-public xdg-config-files
  `(("procps/toprc" ,(local-file "../top/toprc"))))

;; Home Environment
(define-public default
  (home-environment
   (packages env:default)
   (services
    `(;; Sound
      ,(service home-pipewire-service-type pipewire-config)
      ;; Desktop
      ,(service home-dbus-service-type)
      ;; X
      ,(simple-service 'startx home-run-on-first-login-service-type startx.scm)
      ,(service home-x11-service-type)
      ;; Shepherd
      ,(service home-emacs-service-type emacs-service)
      ; ,(service home-dunst-service-type)
      ; ,(service home-clipmenud-service-type)
      ,(service home-shepherd-service-type)
      ;; Shell
      ,(simple-service 'fish-extra-init
                       home-fish-service-type fish-config)
      ,(service home-fish-service-type)
      ;; Channels
      ,(service home-channels-service-type channels:default)
      ;; Basics
      ,(service home-xdg-configuration-files-service-type xdg-config-files)
      ,(simple-service 'home-env-vars-service
                       home-environment-variables-service-type env-vars)
      ;; Needed in container
      ,base-env
      ,@%base-home-services))))

default
