(define-module (home)
  #:use-module (guix gexp)
  #:use-module (guix channels)
  #:use-module (guix packages)
  #:use-module (guix profiles)
  #:use-module ((guix records) #:select (match-record))

  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services desktop)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services shepherd)

  #:use-module (gnu services)
  #:use-module (gnu services configuration)

  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages vim)
  #:use-module ((gnu packages xorg) #:prefix xorg:)

  #:use-module (clipmenud)
  #:use-module (dmenu)
  #:use-module (dunst)
  #:use-module (emacs)
  #:use-module (xmonad)

  #:use-module ((env) #:prefix env:))

;; Emacs
(define emacs-service
  (home-emacs-configuration
   (package emacs-next)
   (socket-name "term")))

;; Env Vars
(define-public chromium-flags
  (mixed-text-file "chromium-flags.conf" "\
--enable-features=WebUIDarkMode,CSSColorSchemeUARendering
--force-dark-mode
"))

(define-public env-vars
  `(("EDITOR" . "emacsclient -t --socket-name=term")
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

(define-public xsession
  (program-file "xsession"
    #~(begin
       (system* #$(file-append xorg:xsetroot "/bin/xsetroot")
		"-cursor_name" "left_ptr")
       (execl #$(file-append my-xmonad "/bin/my-xmonad")))))

(define-public startx.fish
  (mixed-text-file "startx.fish" "\
if test (tty) = /dev/tty1 && status is-login
    xinit " xsession " -- /run/setuid-programs/startx vt1
    loginctl terminate-session (loginctl list-sessions | awk '/tty1/ { print $1 }')
end
"))

(define fish-config
  (home-fish-extension
   (config `(,config.fish ,aliases.fish ,fish_prompt.fish ,keybindings.fish ,startx.fish))
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

;; Base Env
;; Provides what would normally be in a guix operating-system,
;; but sometimes this is run in a container.
(define-public base-env
  (simple-service 'base-packages-service-type
                  home-profile-service-type
                  `(,coreutils ,bash ,neovim ,procps ,grep ,gawk ,sed)))

;; Home Environment
(define-public default
  (home-environment
   (packages env:default)
   (services
    `(;; Shepherd
      ;; Desktop
      ,(service home-dbus-service-type)
      ;; Shepherd
      ,(service home-emacs-service-type emacs-service)
      ,(service home-dunst-service-type)
      ,(service home-clipmenud-service-type)
      ,(service home-shepherd-service-type)
      ;; Shell
      ,(simple-service 'fish-extra-init
                       home-fish-service-type fish-config)
      ,(service home-fish-service-type)
      ;; Basics
      ,(simple-service 'home-env-vars-service
                       home-environment-variables-service-type env-vars)
      ;; Needed in container
      ,base-env
      ,@%base-home-services))))

default
