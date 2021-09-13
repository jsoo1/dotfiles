(define-module (my-packages))

(define-public languages
  '("agda"
    "agda-ial"
    "cedille"
    "coq"
    "idris"
    "ocaml"
    "purescript"
    "racket"))

(define-public utilities
  '("aspell"
    "aspell-dict-en"
    "bat"
    "bpftrace"
    "cups"
    "diffoscope"
    "direnv"
    "docker-cli"
    "dog"
    "emacs"
    "exa"
    "fd"
    "fish"
    "fish-foreign-env" ; Busted for now. Let's fix it.
    "fzy"
    "gdb"
    "global"
    "groff"
    "lastpass-cli"
    "make"
    "mosh"
    "pijul"
    "pinentry"
    "qemu"
    "recutils"
    "ripgrep"
    "rlwrap"
    "shellcheck"
    "skim"
    "time"
    "tokei"
    "tmux"
    "unzip"))

(define-public browsers
  '("icecat"
    "lynx"
    "ungoogled-chromium"))

(define-public desktop-tools
  '("alacritty"
    "alsa-utils"
    "clipmenu"
    "compton"
    "dbus"
    "dunst"
    ;; Broken by python 3.8
    ;; Waiting for https://forum.freecadweb.org/viewtopic.php?t=38982
    ;; "freecad"
    "garcon"
    "gnuplot"
    ;; "ibm-capsense-usb-util"
    "libnotify"
    "libreoffice"
    "my-dmenu"
    "mpd-mpc"
    "pulseaudio"
    "scrot"))

(define-public fonts
  '("mkfontdir"
    "mkfontscale"
    "font-dejavu"
    "font-iosevka"
    "font-iosevka-term-slab"))

(define-public c-tools
  '("ccls"))

(define-public haskell-tools
  '(; "cabal-install" Broken right now
    "ghc@8.6"
    "hlint"
    "hoogle"))

(define-public nix-tools
  '("nixfmt"))

(define-public ocaml-tools
  '("dune"
    "opam"))

(define-public rust-tools
  '("rust"
    "rust:cargo"
    "rust:rls"
    "rust:rust-analyzer"
    "rust:rustfmt"
    "rust:src"))

(define-public guile-tools
  '("guile"
    "guile-colorized"
    "guile-readline"
    "guile-syntax-highlight"))

(define-public pdf-tools
  '("texlive"
    "zathura"
    "zathura-ps"
    "zathura-pdf-mupdf"))

(define-public xorg-tools
  '("gcc-toolchain" ;; needed by xmonad
    "ghc-dbus"
    "ghc-xmonad-contrib"
    "pavucontrol"
    "setxkbmap"
    "xdg-utils"
    "xdotool"
    "xev"
    "xfontsel"
    "xinit"
    "xinput"
    "xlockmore"
    "xmessage"
    "xmobar"
    "xmonad"
    "xrandr"
    "xsel"
    "xsetroot"
    "xwallpaper"))

(define-public emacs-packages
  '("emacs-aio"
    "emacs-anzu"
    "emacs-clojure-mode"
    "emacs-cmake-mode"
    "emacs-company"
    "emacs-company-coq"
    "emacs-company-math"
    "emacs-counsel-projectile"
    "emacs-csv-mode"
    "emacs-cql-mode"
    "emacs-dash"
    "emacs-debbugs"
    "emacs-dhall-mode"
    "emacs-dired-git-info"
    "emacs-diredfl"
    "emacs-docker"
    "emacs-dockerfile-mode"
    "emacs-ediprolog"
    "emacs-editorconfig"
    "emacs-eglot"
    "emacs-elfeed"
    "emacs-elf-mode"
    "emacs-elpher"
    "emacs-emmet-mode"
    "emacs-emms"
    "emacs-envrc"
    "emacs-eredis"
    "emacs-eshell-syntax-highlighting"
    "emacs-evil"
    "emacs-evil-anzu"
    "emacs-evil-collection"
    "emacs-evil-commentary"
    "emacs-evil-escape"
    "emacs-evil-leader"
    "emacs-evil-org"
    "emacs-evil-replace-with-register"
    "emacs-evil-surround"
    "emacs-evil-tmux-navigator"
    "emacs-f"
    "emacs-fill-column-indicator"
    "emacs-fish-completion"
    "emacs-fish-mode"
    "emacs-flycheck"
    "emacs-forge"
    "emacs-geiser"
    "emacs-goto-chg"
    "emacs-graphql-mode"
    "emacs-graphviz-dot-mode"
    "emacs-guix"
    "emacs-haskell-mode"
    "emacs-haskell-snippets"
    "emacs-helpful"
    "emacs-ibuffer-projectile"
    "emacs-idris-mode"
    "emacs-imenu-list"
    "emacs-ivy"
    "emacs-let-alist"
    "emacs-magit"
    "emacs-markdown-mode"
    "emacs-merlin"
    "emacs-multi-term"
    "emacs-nix-mode"
    "emacs-nodejs-repl"
    "emacs-ob-restclient"
    "emacs-org-mime"
    "emacs-origami-el"
    "emacs-popper"
    "emacs-prescient"
    "emacs-projectile"
    "emacs-psc-ide"
    "emacs-racket-mode"
    "emacs-recutils"
    "emacs-redis"
    "emacs-reformatter"
    "emacs-restclient"
    "emacs-rust-mode"
    "emacs-s"
    "emacs-shackle"
    "emacs-slime"
    "emacs-slime-company"
    "emacs-sml-mode"
    "emacs-solarized-theme"
    "emacs-systemd-mode"
    "emacs-terraform-mode"
    "emacs-tuareg"
    ;; Remove undo-tree and use undo-redo for evil-undo-system with
    ;; emacs 28
    "emacs-undo-tree"
    "emacs-uuidgen-el"
    "emacs-vimrc-mode"
    "emacs-web-mode"
    "emacs-wgrep"
    "emacs-which-key"
    "emacs-xclip"
    "emacs-xterm-color"
    "emacs-yaml-mode"
    "emacs-yasnippet"
    "proof-general"))
