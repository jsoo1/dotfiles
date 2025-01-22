(define-module (env)
  #:use-module (guix gexp)
  #:use-module (guix packages)

  #:use-module (gnu packages agda)
  #:use-module (gnu packages coq)
  #:use-module (gnu packages ocaml)
  #:use-module (gnu packages purescript)
  #:use-module (gnu packages racket)
  #:use-module (gnu packages twelf)

  #:use-module (gnu packages admin)
  #:use-module (gnu packages aspell)
  #:use-module (gnu packages base)
  #:use-module (gnu packages code)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages dns)
  #:use-module (gnu packages gdb)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages groff)
  #:use-module (gnu packages haskell-apps)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages shellutils)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tmux)
  #:use-module (gnu packages video)
  #:use-module (gnu packages virtualization)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xdisorg)

  #:use-module (gnu packages chromium)
  #:use-module (gnu packages web-browsers)
  #:use-module (nongnu packages mozilla)

  #:use-module (gnu packages compton)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages libreoffice)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mpd)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages xfce)

  #:use-module (gnu packages fonts)
  #:use-module (gnu packages xorg)

  #:use-module (gnu packages cpp)

  #:use-module (gnu packages haskell)

  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile-xyz)

  #:use-module (gnu packages tree-sitter)

  #:use-module (gnu packages texlive)
  #:use-module (gnu packages pdf)

  #:use-module (gnu packages freedesktop)

  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages cmake)

  #:use-module (dmenu)
  #:use-module (xmonad))

(define-public languages
  (list
   agda
   coq
   ;; FIXME: Broken upstream
   ;; idris
   ocaml
   purescript
   racket
   twelf))

(define-public utilities
  (list
   aspell
   aspell-dict-en
   bat
   ;; FIXME: broken
   ;; bpftrace
   cups
   direnv
   du-dust
   eza
   fd
   fish
   fish-foreign-env ; Busted for now. Let's fix it.
   gdb
   global
   groff
   ;; FIXME: Broken
   ;; lastpass-cli
   jq
   gnu-make
   mosh
   mpv
   pinentry
   qemu
   recutils
   ripgrep
   rlwrap
   shellcheck
   skim
   time
   tmux
   unzip))

(define-public browsers
  (list
   firefox
   lynx
   ungoogled-chromium))

(define-public desktop-tools
  (list
   alacritty
   compton
   my-dmenu
   garcon
   gnuplot
   libnotify
   libreoffice
   mpd-mpc
   pamixer
   pulsemixer
   scrot
   wireless-tools))

(define-public fonts
  (list
   font-dejavu
   font-google-noto
   font-google-noto-sans-cjk
   font-google-noto-serif-cjk
   font-iosevka))

(define-public c-tools
  (list
   ccls
   tree-sitter-c
   tree-sitter-cpp))

(define-public go-tools
  (list
   tree-sitter-go
   tree-sitter-gomod))

(define-public haskell-tools
  (list
   cabal-install
   ghc
   hlint
   hoogle
   stylish-haskell
   tree-sitter-haskell
   threadscope))

(define-public nix-tools
  (list
   nixfmt
   tree-sitter-org))

(define-public ocaml-tools
  (list
   dune
   opam
   tree-sitter-ocaml))

(define-public rust-tools
  (list
   tree-sitter-rust))

(define-public guile-tools
  (list
   guile-colorized
   guile-readline
   guile-syntax-highlight))

(define-public pdf-tools
  (list
   texlive
   zathura
   zathura-ps
   zathura-pdf-mupdf))

(define-public xorg-tools
  (list
   xdg-utils
   xdotool
   xev
   xfontsel
   xinput
   xlockmore
   xmessage
   xrandr
   xsel
   xwallpaper))

(define-public emacs-packages
  (list
   emacs-aio
   emacs-anzu
   emacs-base16-theme
   emacs-clojure-mode
   emacs-cmake-mode
   emacs-company
   emacs-company-coq
   emacs-company-math
   emacs-consult
   emacs-csv-mode
   emacs-cql-mode
   emacs-dash
   emacs-debbugs
   emacs-dhall-mode
   emacs-dired-git-info
   emacs-diredfl
   emacs-dockerfile-mode
   emacs-ediprolog
   emacs-editorconfig
   emacs-eglot
   emacs-elfeed
   emacs-elf-mode
   emacs-elpher
   emacs-embark
   emacs-emmet-mode
   emacs-emms
   emacs-envrc
   emacs-eredis
   emacs-eshell-syntax-highlighting
   emacs-evil
   emacs-evil-anzu
   emacs-evil-collection
   emacs-evil-commentary
   emacs-evil-escape
   emacs-evil-leader
   emacs-evil-org
   emacs-evil-replace-with-register
   emacs-evil-surround
   emacs-evil-tmux-navigator
   emacs-f
   emacs-fill-column-indicator
   emacs-fish-completion
   emacs-fish-mode
   emacs-flycheck
   emacs-forge
   emacs-geiser
   emacs-goto-chg
   emacs-graphql-mode
   emacs-graphviz-dot-mode
   emacs-guix
   emacs-haskell-mode
   emacs-haskell-snippets
   emacs-helpful
   emacs-highlight-indent-guides
   emacs-hydra
   emacs-idris-mode
   emacs-imenu-list
   emacs-let-alist
   emacs-magit
   emacs-markdown-mode
   ;; Busted?
   ;; emacs-merlin
   emacs-multi-term
   emacs-nix-mode
   emacs-nodejs-repl
   emacs-ob-restclient
   emacs-orderless
   emacs-org-mime
   emacs-origami-el
   emacs-popper
   emacs-prescient
   emacs-psc-ide
   emacs-pulseaudio-control
   emacs-racket-mode
   emacs-rec-mode
   emacs-redis
   emacs-reformatter
   emacs-restclient
   emacs-rust-mode
   emacs-s
   emacs-shackle
   emacs-slime
   emacs-slime-company
   emacs-sml-mode
   emacs-systemd-mode
   emacs-terraform-mode
   emacs-tuareg
   emacs-uuidgen-el
   emacs-vertico
   emacs-vimrc-mode
   emacs-web-mode
   emacs-wgrep
   emacs-which-key
   emacs-xclip
   emacs-xterm-color
   emacs-yaml-mode
   emacs-yasnippet
   proof-general))

(define-public default
  (append
   languages
   utilities
   browsers
   desktop-tools
   fonts
   c-tools
   go-tools
   haskell-tools
   nix-tools
   ocaml-tools
   rust-tools
   guile-tools
   pdf-tools
   xorg-tools
   emacs-packages))

default
