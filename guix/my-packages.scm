(define-module (my-packages))

(define-public languages
  '("agda"
    "coq"
    ;; FIXME: Broken upstream
    ;; "idris"
    "ocaml"
    "purescript"
    "racket"))

(define-public utilities
  '("aspell"
    "aspell-dict-en"
    "bat"
    "bpftrace"
    "cups"
    "direnv"
    "docker-cli"
    "dog"
    "emacs"
    "exa"
    "fd"
    "fish"
    "fish-foreign-env" ; Busted for now. Let's fix it.
    "gdb"
    "global"
    "groff"
    ;; FIXME: Broken
    ;; "lastpass-cli"
    "make"
    "mosh"
    "mpv"
    "pijul"
    "pinentry"
    "qemu"
    "recutils"
    "ripgrep"
    "rlwrap"
    "shellcheck"
    "skim"
    "time"
    "tmux"
    "unzip"))

(define-public browsers
  '("firefox"
    "icecat"
    "lynx"
    ;; FIXME: Borken
    ;; "ungoogled-chromium"
    ))

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
    "mpd-mpc"
    "pamixer"
    "pulsemixer"
    "pulseaudio"
    "scrot"
    "wireless-tools"))

(define-public fonts
  '("mkfontdir"
    "mkfontscale"
    "font-dejavu"
    "font-iosevka"
    "font-iosevka-term-slab"))

(define-public c-tools
  '("ccls"))

(define-public haskell-tools
  '("cabal-install"
    "ghc"
    "hspec-discover"
    "hlint"
    "hoogle"
    "stylish-haskell"
    "threadscope"))

(define-public nix-tools
  '("nixfmt"))

(define-public ocaml-tools
  '("dune"
    "opam"))

(define-public rust-tools
  '("rust"
    "rust:cargo"
    ;; FIXME: Rebase the patches upstream
    ;; "rust:rls"
    ;; "rust:rust-analyzer"
    ;; "rust:rustfmt"
    ;; "rust:src"
    ))

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
  '("xdg-utils"
    "xdotool"
    "xev"
    "xfontsel"
    "xinit"
    "xinput"
    "xlockmore"
    "xmessage"
    "xrandr"
    "xsel"
    "xsetroot"
    "xwallpaper"))

(define-public emacs-packages
  '("emacs-aio"
    "emacs-anzu"
    "emacs-base16-theme"
    "emacs-clojure-mode"
    "emacs-cmake-mode"
    "emacs-company"
    "emacs-company-coq"
    "emacs-company-math"
    "emacs-consult"
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
    "emacs-highlight-indent-guides"
    "emacs-ibuffer-projectile"
    "emacs-idris-mode"
    "emacs-imenu-list"
    "emacs-ivy"
    "emacs-let-alist"
    "emacs-magit"
    "emacs-markdown-mode"
    ;; Busted?
    ;; "emacs-merlin"
    "emacs-multi-term"
    "emacs-nix-mode"
    "emacs-nodejs-repl"
    "emacs-ob-restclient"
    "emacs-orderless"
    "emacs-org-mime"
    "emacs-origami-el"
    "emacs-popper"
    "emacs-prescient"
    "emacs-projectile"
    "emacs-psc-ide"
    "emacs-pulseaudio-control"
    "emacs-racket-mode"
    "emacs-rec-mode"
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
    "emacs-uuidgen-el"
    "emacs-vertico"
    "emacs-vimrc-mode"
    "emacs-web-mode"
    "emacs-wgrep"
    "emacs-which-key"
    "emacs-xclip"
    "emacs-xterm-color"
    "emacs-yaml-mode"
    "emacs-yasnippet"
    "proof-general"))
