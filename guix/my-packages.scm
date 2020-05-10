(define-module (my-packages))

(define-public languages
  '("agda"
    "cedille"
    "coq"
    "idris"
    "purescript"))

(define-public utilities
  '("aspell"
    "aspell-dict-en"
    "cups"
    "direnv"
    "docker-cli"
    "emacs-no-x"
    "exa"
    "fd"
    "fish"
    "fish-foreign-env" ; Busted for now. Let's fix it.
    "fzy"
    "gdb"
    "global"
    "make"
    "pijul"
    "pinentry"
    "ripgrep"
    "rlwrap"
    "shellcheck"
    "strace"
    "time"
    "tlsdate"
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
    "libnotify"
    "my-dmenu"
    "pulseaudio"))

(define-public fonts
  '("mkfontdir"
    "mkfontscale"
    "font-dejavu"
    "font-iosevka"
    "font-iosevka-term"
    "font-iosevka-term-slab"))

(define-public haskell-tools
  '("cabal-install"
    "ghc"
    "ghcid"
    "hlint"
    "hoogle"
    "ormolu"
    "stylish-haskell"))

(define-public rust-tools
  '("racer"
    "rust"
    "rust:cargo"
    "rustfmt"))

(define-public guile-tools
  '("guile"
    "guile-colorized"
    "guile-readline"
    "guile-syntax-highlight"))

(define-public pdf-tools
  '("texlive"
    "zathura"
    "zathura-pdf-mupdf"))

(define-public xorg-tools
  '("gcc-toolchain" ;; needed by xmonad
    "ghc-dbus"
    "ghc-xmonad-contrib"
    "setxkbmap"
    "xcape"
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
  '("emacs-anzu"
    "emacs-avy"
    "emacs-cmake-mode"
    "emacs-company"
    "emacs-company-coq"
    "emacs-company-math"
    "emacs-counsel-projectile"
    "emacs-csv-mode"
    "emacs-dash"
    "emacs-debbugs"
    "emacs-dhall-mode"
    "emacs-dired-git-info"
    "emacs-diredfl"
    "emacs-docker"
    "emacs-dockerfile-mode"
    "emacs-ediprolog"
    "emacs-elf-mode"
    "emacs-elm-mode"
    "emacs-emmet-mode"
    "emacs-evil"
    "emacs-evil-anzu"
    "emacs-evil-commentary"
    "emacs-evil-escape"
    "emacs-evil-leader"
    "emacs-evil-magit"
    "emacs-evil-org"
    "emacs-evil-surround"
    "emacs-evil-tmux-navigator"
    "emacs-f"
    "emacs-fill-column-indicator"
    "emacs-fish-mode"
    "emacs-flycheck"
    "emacs-flycheck-elm"
    "emacs-flycheck-rust"
    "emacs-geiser"
    "emacs-goto-chg"
    "emacs-graphviz-dot-mode"
    "emacs-guix"
    "emacs-haskell-mode"
    "emacs-haskell-snippets"
    "emacs-ibuffer-projectile"
    "emacs-idris-mode"
    "emacs-imenu-list"
    "emacs-ivy"
    "emacs-let-alist"
    "emacs-magit"
    "emacs-markdown-mode"
    "emacs-multi-term"
    ;; mmm-mode broken
    ;; "emacs-nix-mode"
    "emacs-nodejs-repl"
    "emacs-projectile"
    "emacs-psc-ide"
    "emacs-racer"
    "emacs-reformatter"
    "emacs-restclient"
    "emacs-rust-mode"
    "emacs-s"
    "emacs-slime"
    "emacs-slime-company"
    "emacs-smartparens"
    "emacs-solarized-theme"
    "emacs-tuareg"
    "emacs-vimrc-mode"
    "emacs-web-mode"
    "emacs-wgrep"
    "emacs-which-key"
    "emacs-xclip"
    "emacs-xterm-color"
    "emacs-yaml-mode"
    "emacs-yasnippet"
    "ocaml4.07-merlin"
    "proof-general"))
