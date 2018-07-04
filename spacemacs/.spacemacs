;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs

   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused

   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t

   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     ;; (
     ;; agda
     ;; :variables
     ;; agda-mode-path "~/.local/bin/agda-mode"
     ;; )
     (auto-completion
      (haskell :variables haskell-completion-backend 'dante))
     c-c++
     clojure
     coq
     csv
     dash
     docker
     (elm
      :variables
      elm-format-command "elm-format")
     (emacs-lisp :variables emacs-lisp-hide-namespace-prefix nil)
     emoji
     (erc :variables
          erc-server-list '(("irc.freenode.net"
                             :port "6697"
                             :ssl t
                             :nick "jsoo"))
          erc-auto-join-channels-alist '(("#idris" "#coq" "#agda")))
     erlang
     evil-snipe
     fsharp
     (git :variables git-magit-status-fullscreen t)
     graphviz
     (gtags :variables gtags-enable-by-default t)
     (haskell :variables
              ;; haskell-process-type 'stack-ghci
              haskell-enable-hindent-style "fundamental")
     html
     (ibuffer
      :variables ibuffer-group-buffers-by 'projects)
     idris
     ivy
     (java :variables java-backend 'ensime)
     (javascript :variables tern-command '("node" "/usr/local/bin/tern"))
     (latex :variables
            latex-build-command "LaTeX"
            latex-enable-auto-fill t
            latex-enable-folding t)
     lsp
     (markdown :variables markdown-live-preview-engine 'vmd)
     nginx
     nixos
     (org :variables
          org-enable-reveal-js-support t
          org-enable-github-support t
          org-projectile-file "TODOs.org")
     osx
     (python :variables
             python-backend 'lsp
             python-enable-yapf-format-on-save t)
     racket
     restclient
     rust
     scheme
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom)
     shell-scripts
     (spell-checking :variables
                     spell-checking-enable-by-default nil)
     spotify
     (sql :variables
          sql-auto-indent nil)
     swift
     syntax-checking
     systemd
     tmux
     treemacs
     twitter
     version-control
     vimscript
     vinegar
     windows-scripts
     yaml
     )

   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   ;; To use a local version of a package, use the `:location' property:
   ;; '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages
   '(
     ;; ------ `Applescript' ------
     applescript-mode
     ;; ------ `Swift' ------
     company-sourcekit
     ;; ------ `Clojure' ------
     clojars
     clojure-cheatsheet
     ;; ------ `Gradle' ------
     gradle-mode
     groovy-mode
     ;; ------ `Guix' ------
     guix
     ;; ------ `Nand2Tetris' ------
     nand2tetris
     ;; ------ `Shen\ Elisp' ------
     (shen-elisp
      :location (recipe
                 :repo "deech/shen-elisp"
                 :fetcher github
                 :files ("shen*.el"))
      :upgrade 't)
     ;; ------ `Haskell' `Prettify' -----
     ;; Thanks to:
     ;; https://github.com/cpitclaudel/.emacs.d/blob/master/lisp/prettify-alists/haskell-prettify.el
     ;; No thanks to:
     ;; https://github.com/haskell/haskell-mode/issues/823
     ;; (haskell-prettify :location "~/.emacs.d/private/")
     ;; ------ `Themes' ------
     doom-themes
     )

   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '(nameless)

   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-only)
  )

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil then enable support for the portable dumper. You'll need
   ;; to compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;; (default nil)
   dotspacemacs-enable-emacs-pdumper nil

   ;; File path pointing to emacs 27.1 executable compiled with support
   ;; for the portable dumper (this is currently the branch pdumper).
   ;; (default "emacs-27.0.50")
   dotspacemacs-emacs-pdumper-executable-file "emacs-27.0.50"

   ;; Name of the Spacemacs dump file. This is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; To load it when starting Emacs add the parameter `--dump-file'
   ;; when invoking Emacs 27.1 executable on the command line, for instance:
   ;;   ./emacs --dump-file=~/.emacs.d/.cache/dumps/spacemacs.pdmp
   ;; (default spacemacs.pdmp)
   dotspacemacs-emacs-dumper-dump-file "spacemacs.pdmp"

   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t

   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 5

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(100000000 0.1)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default nil)
   dotspacemacs-verify-spacelpa-archives nil

   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil

   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory 'emacs-version

   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'hybrid

   ;; If non-nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil

   ;; If non-nil then Spacemacs will import your PATH and environment variables
   ;; from your default shell on startup. This is enabled by default for macOS
   ;; users and X11 users.
   dotspacemacs-import-env-vars-from-shell (and (display-graphic-p)
                                                (or (eq system-type 'darwin)
                                                    (eq system-type 'gnu/linux)
                                                    (eq window-system 'x)))

   ;; If nil then use the default shell is used to fetch the environment
   ;; variables. Set this variable to a different shell executable path to
   ;; import the environment variables from this shell. Note that
   ;; `file-shell-name' is preserved and always points to the default shell. For
   ;; instance to use your fish shell environment variables set this variable to
   ;; `/usr/local/bin/fish'.
   ;; (default nil)
   dotspacemacs-import-env-vars-shell-file-name nil

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'random

   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '(
                                (projects . 7)
                                (recents . 16)
                                )

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'org-mode

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(
                         doom-spacegrey
                         spacemacs-dark
                         doom-one
                         )

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `vim-powerline' and `vanilla'. The first three
   ;; are spaceline themes. `vanilla' is default Emacs mode-line. `custom' is a
   ;; user defined themes, refer to the DOCUMENTATION.org for more info on how
   ;; to create your own spaceline theme. Value can be a symbol or list with\
   ;; additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme (if (or (display-graphic-p) (string-equal 'frame (daemonp)))
                                    '(
                                      all-the-icons
                                      :separator arrow
                                      :separator-scale 1.4
                                      )
                                  '(
                                    spacemacs
                                    :separator utf-8
                                    :separator-scale 1.5
                                    )
                                  )

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '(
                               (
                                "FantasqueSansMono Nerd Font Mono"
                                :size 14
                                :weight normal
                                :width wide
                                :powerline-scale 1.4
                                )
                               (
                                "Source Code Pro for Powerline"
                                :size 14
                                :weight normal
                                :width normal
                                :powerline-scale 1.1
                                )
                               )

   ;; The leader key (default "SPC")
   dotspacemacs-leader-key "SPC"

   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"

   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"

   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"

   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","

   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil

   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"

   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil

   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil

   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil

   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1

   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache

   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5

   ;; If non-nil, the paste transient-state is enabled. While enabled, pressing
   ;; `p' several times cycles through the elements in the `kill-ring'.
   ;; (default nil)
   dotspacemacs-enable-paste-transient-state t

   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.1

   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar nil

   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup (pcase system-type
                                        ('darwin t)
                                        (_ nil))

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil

   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 85

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 70

   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t

   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t

   ;; If non-nil unicode symbols are displayed in the mode line.
   ;; If you use Emacs as a daemon and wants unicode characters only in GUI set
   ;; the value to quoted `display-graphic-p'. (default t)
   dotspacemacs-mode-line-unicode-symbols t

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling nil

   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers '(
                               :relative t
                               :disabled-for-modes dired-mode org-mode text-mode doc-view-mode
                               :size-limit-kb 1500
                               )

   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis t

   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all

   ;; If non-nil, start an Emacs server if one is not already running.
   ;; (default nil)
   dotspacemacs-enable-server nil

   ;; Set the emacs server socket location.
   ;; If nil, uses whatever the Emacs default is, otherwise a directory path
   ;; like \"~/.emacs.d/server\". It has no effect if
   ;; `dotspacemacs-enable-server' is nil.
   ;; (default nil)
   dotspacemacs-server-socket-dir nil

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server t

   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")

   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   dotspacemacs-frame-title-format "%F  %t  %b"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'changed

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs t
   ))

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (spacemacs/load-spacemacs-env))

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."

  ;; nand2tetris needs this prior to user-config
  (setq nand2tetris-core-base-dir "~/projects/nand2tetris")

  ;; :) no annoying changes at the bottom of .spacemacs
  (setq custom-file "~/.customize.el")

  ;; ------ Make sure we get agda-mode ------
  (add-to-list 'exec-path "~/.local/bin")
  (add-to-list 'exec-path "/usr/local/bin")

  ;; Apple spaceline is messed up without this for now
  (if (string-equal 'darwin system-type)
      (if (string-equal "frame" (daemonp))
          (add-hook 'before-make-frame-hook
                    #'(lambda ()
                        (setq powerline-image-apple-rgb t)))
        (setq powerline-image-apple-rgb t)))

  ;; ------ Default Shell ------
  ;; Fish outputs a bunch of junk from "call-process"
  ;; and clogs up ivy/counsel buffers :(
  ;; It also messes up the agda-mode loading which is why it's in init:(
  (setq shell-file-name (pcase system-type
                          ('darwin "/bin/bash")
                          (_ "/bin/sh")))
  )

(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included
in the dump."
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."


  ;; `~~~~~~' `EMACS-WIDE' `~~~~~~~'

  ;; ------ `Global\ Key\ Bindings' ------
  ;; Woman under help
  (define-key evil-normal-state-map (kbd "SPC h m") 'woman)
  ;; Face description
  (spacemacs/set-leader-keys "hdF" 'describe-face)
  ;; yas insert in hybrid mode
  (define-key evil-hybrid-state-map (kbd "C-c C-i") 'yas-insert-snippet)
  ;; Run a project
  (spacemacs/set-leader-keys "pu" 'projectile-run-project)
  ;; Open the compilation buffer
  (define-key evil-normal-state-map (kbd "SPC b c") #'open-compilation-window)
  (define-key evil-normal-state-map (kbd "SPC c b") #'open-compilation-window)

  ;; Pixel scroll is sick
  (pixel-scroll-mode 1)

  (setq
   ;; df == fd
   evil-escape-unordered-key-sequence t
   ;; pixel scrolling
   scroll-step 1
   scroll-conservatively 10000)


  ;; ------ `Load\ Path' ------
  (add-to-list 'exec-path "~/.nix-profile/bin/")
  (add-to-list 'exec-path "~/.local/bin")
  (add-to-list 'exec-path "~/.guix-profile/bin")

  ;; ------ `Mouse\ Support' ------
  ;; scrolling in terminal
  (unless window-system
    (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
    (global-set-key (kbd "<mouse-5>") 'scroll-up-line))

  ;; horizontal scrolling
  (pcase system-type
    ('darwin
     (progn
       (global-set-key [wheel-right] 'scroll-left)
       (global-set-key [wheel-left] 'scroll-right)))

    ('gnu/linux
     (progn
       (global-set-key (kbd "S-<mouse-4>") 'scroll-right)
       (global-set-key (kbd "S-<mouse-5>") 'scroll-left))))


  ;; ------ `DirEd' ------
  ;; all-the-icons in dired
  (add-hook
   'dired-mode-hook
   (lambda ()
     (progn
       (when (not (fboundp 'all-the-icons-dired-mode))
         (load-file "~/.emacs.d/private/all-the-icons-dired/all-the-icons-dired.el")))
     (all-the-icons-dired-mode)))

  ;; ------ `Paradox' ------
  ;; key for paradox
  (setq
   paradox-github-token 'paradox)


  ;; ------ `Projectile' ------
  (setq
   ;; Speed it up
   projectile-enable-caching t)


  ;; ------ `Compilation'
  (defun open-compilation-window ()
    "Open the window containing the '*compilation*' buffer."
    (interactive)
    (when compilation-last-buffer
      (let ((curwin (selected-window)))
        (pop-to-buffer compilation-last-buffer)
        (select-window curwin))))


  ;; ------ `Shell' -----
  (setq
   ;; Defaults: multi-term, fish
   multi-term-program "/usr/local/bin/fish"
   shell-default-shell 'multi-term)


  ;; ------ `Eshell' ------
  ;; eshell has an annoying banner
  (setq eshell-banner-message "")


  ;; ------ `Spelling' ------
  ;; ispell setup for spell checking (use ispell)
  (setq ispell-program-name "/usr/local/bin/ispell")


  ;; ------ `Symlinks' ------
  ;; don't prompt to follow symlinks
  (setq vc-follow-symlinks t)


  ;; ------ `Truncate\ Lines' ------
  ;; always truncate :)
  (set-default 'truncate-lines t)


  ;; ------ `ERC' ------
  ;; Ignore annoying crap
  (setq erc-hide-list '("JOIN" "PART" "QUIT"))


  ;; ------ `XClip' ------
  (use-package xclip-integration
    :defer t
    :load-path "private"
    :config (evil-leader/set-key "o y" #'copy-to-clipboard)
    (evil-leader/set-key "o p" #'paste-from-clipboard))

  ;; Workaround for https://github.com/syl20bnr/spacemacs/issues/10896
  (when (equalp system-type 'darwin)
    (defun aj/copy-from-osx ()
      (shell-command-to-string "pbpaste"))

    (defun aj/paste-to-osx (text &optional push)
      (let ((process-connection-type nil))
        (let ((proc (start-process "pbcopy" nil "pbcopy")))
          (process-send-string proc text)
          (process-send-eof proc))))

    (defun aj/select-text (text)
      (if (display-graphic-p)
          (gui-select-text text)
        (aj/paste-to-osx text)))

    (defun aj/selection-value ()
      (if (display-graphic-p)
          (gui-selection-value)
        (aj/copy-from-osx)))

    (setq interprogram-cut-function 'aj/select-text
          interprogram-paste-function 'aj/selection-value))


  ;; ------ `Transparency' ------
  (when (display-graphic-p) (spacemacs/toggle-transparency))


  ;; ------ `Fullscreen\ client\ frames'
  (add-to-list
   'after-make-frame-functions #'(lambda (frame)
                                   (if (string-equal "frame" (daemonp))
                                       (spacemacs/toggle-fullscreen-frame-on))))

  ;; ------ `Org-Mode' ------
  (with-eval-after-load 'org

    ;; ;; Agenda notifications
    ;; (pcase system-type
    ;;   ('darwin (load-file "~/.emacs.d/private/agenda-notify.el")))

    (setq
     ;; Agenda files
     org-agenda-files (pcase system-type
                        ('gnu/linux
                         '("~/Dropbox/org/"
                           "~/development"))
                        ('darwin
                         '("~/Desktop/org/"
                           "~/projects/client-browser")))

     ;; Org HTML presentations with reveal.js
     org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/"
     org-reveal-title-slide nil

     ;; Remove pesky validate link
     org-html-validation-link nil

     ;; Don't show scheduled items in global todo list
     org-agenda-todo-ignore-with-date t

     ;; Org Reveal
     org-reveal-title-slide 'auto
     org-reveal-progress nil
     org-reveal-history t
     org-reveal-rolling-links t
     org-reveal-keyboard t
     org-reveal-mathjax t
     org-reveal-overview t
     org-reveal-slide-number nil

     ;; Prettify exports for email
     org-export-with-author nil
     org-export-with-creator nil
     org-export-with-toc nil
     org-export-with-email nil
     org-export-time-stamp-file nil
     org-export-with-section-numbers nil
     org-export-with-todo-keywords nil
     org-html-validation-link nil

     ;; Org Capture Templates
     org-capture-templates (pcase system-type
                             ('gnu/linux
                              '(("p" "RevealJS Presentation"
                                 plain (function (lambda() (buffer-file-name)))
                                 "%[~/Dropbox/org/templates/presentation.org]")))))

    (add-hook 'org-mode-hook 'emojify-mode)

    ;; Org-Babel
    (require 'ob-python)
    (require 'ob-clojure)
    (require 'ob-shell)
    (require 'ob-haskell)
    (require 'ob-js)
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((clojure . t)
       (python . t)
       (shell . t)
       (haskell . t)
       (js . t)))

    ;; Keybindings
    ;; Month view
    (spacemacs/set-leader-keys-for-major-mode 'org-agenda-mode "m" 'org-agenda-month-view)
    )


  ;; ------ `Spaceline' ------
  ;; no time in mode line by default
  (spacemacs/toggle-display-time-off)

  ;; unbreak powerline symbols in terminal
  (unless (or (string-equal "frame" (daemonp)) (display-graphic-p))
    (setq powerline-default-separator 'utf8 ))

  (defun my-fix-faces ()
    "Fix the spaceline faces for the spacemacs spaceline faces."
    (when (or (not (display-graphic-p)) (string-equal "term" (daemonp)))
      (progn
        (set-face-attribute 'spacemacs-normal-face nil :foreground "#262626")
        (set-face-attribute 'spacemacs-hybrid-face nil :foreground "#262626")
        (set-face-attribute 'spacemacs-emacs-face nil :foreground "#262626")
        (set-face-attribute 'spacemacs-evilified-face nil :foreground "#262626")
        (set-face-attribute 'spacemacs-visual-face nil :foreground "#262626")
        (set-face-attribute 'spacemacs-replace-face nil :foreground "#262626")
        (set-face-attribute 'spacemacs-iedit-face nil :foreground "#262626")
        (set-face-attribute 'spacemacs-lisp-face nil :foreground "#262626")
        (set-face-attribute 'spacemacs-evilified-face nil :foreground "#262626")
        (set-face-attribute 'mode-line nil :background "black" :foreground "white")
        (set-face-attribute 'mode-line-inactive nil :foreground "#65737E"))) )

  ;; Fix space line faces when theme changes (particularly for doom themes).
  ;; https://www.reddit.com/r/emacs/comments/4v7tcj/does_emacs_have_a_hook_for_when_the_theme_changes/
  (defvar after-load-theme-hook nil
    "Hook run after a color theme is loaded using `load-theme'.")
  (defadvice load-theme (after run-after-load-theme-hook activate)
    "Run `after-load-theme-hook'."
    (run-hooks 'after-load-theme-hook))
  (add-hook 'after-load-theme-hook #'my-fix-faces)
  (my-fix-faces)

  ;; unbreak powerline symbols in terminal
  (unless (or (string-equal "frame" (daemonp)) (display-graphic-p))
    (progn
      (setq powerline-default-separator 'utf-8)
      (spacemacs/toggle-mode-line-minor-modes-off)))


  ;; ------ `Diminish' ------
  (if (or (string-equal "frame" (daemonp)) (display-graphic-p))

      ;; Graphical
      (progn
        (add-to-list 'spacemacs--diminished-minor-modes '(server-buffer-clients " ⒮" " $"))
        (add-to-list 'spacemacs--diminished-minor-modes '(interactive-haskell-mode " ⒤" nil))
        (add-to-list 'spacemacs--diminished-minor-modes '(idris-simple-indent-mode nil nil))
        (add-to-list 'spacemacs--diminished-minor-modes '(dired-omit-mode nil nil))
        (add-to-list 'spacemacs--diminished-minor-modes '(emoji-cheat-sheet-plus-display-mode " 🤔" nil)))

    ;; Terminal
    (progn
      (add-to-list 'spacemacs--diminished-minor-modes '(emoji-cheat-sheet-plus-display-mode " ⒠" nil))
      (add-to-list 'spacemacs--diminished-minor-modes '(server-buffer-clients " ⒮" " $"))
      (add-to-list 'spacemacs--diminished-minor-modes '(elm-indent-mode nil nil))
      (add-to-list 'spacemacs--diminished-minor-modes '(interactive-haskell-mode " ⒤" nil))
      (add-to-list 'spacemacs--diminished-minor-modes '(meghanada-mode " M" " M"))))


  ;; ------ `Yasnippet' ------
  (setq yas-indent-line 'fixed
        yas-after-exit-snippet-hook (lambda () (progn (elm-mode-format-buffer) (evil-insert 0))))


  ;; `~~~~~~' `LANGUAGE-SUPPORT' `~~~~~~'

  ;; ------ `Applescript' ------
  ;; file types
  (add-to-list 'auto-mode-alist '("\\.scpt\\'" . applescript-mode))


  ;; ------ `ATS' ------
  (use-package ats-mode
    :defer t
    :load-path "~/.emacs.d/private"
    :mode "\\.\\(s\\|d\\|h\\)ats\\'")

  (use-package ats2-flycheck
    :defer t
    :after (:all ats-mode)
    :load-path "~/.emacs.d/private"
    :commands #'flycheck-ats2-setup
    :config (progn
              (flycheck-ats2-setup)
              (add-hook 'ats-mode-hook #'flycheck-mode)))


  ;; ------ `Cider\ and\ Clojure' ------
  (setq
   ;; fancy symbols
   clojure-enable-fancify-symbols t

   ;; no annoying banner in repl
   cider-repl-display-help-banner nil

   ;; only clojure related error messages
   cider-stacktrace-default-filters '(tooling dup java))

  ;; Keybindings
  ;; cider-restart
  (spacemacs/set-leader-keys-for-major-mode 'clojure-mode "s X" 'cider-restart)


  ;; ------ `FSharp' ------
  ;; file types
  (add-to-list 'auto-mode-alist '("\\.fsproj\\'" . xml-mode))


  ;; ------ `Elm' ------
  (setq elm-format-on-save 't)
  (spacemacs/set-leader-keys-for-major-mode
    'elm-mode
    "i"
    (lambda ()
      (interactive)
      (progn
        (evil-append 0)
        (yas-insert-snippet))))


  ;; ------ `Gradle' ------
  ;; file types
  (add-to-list 'auto-mode-alist '("\\.gradle\\'" . groovy-mode))


  ;; ------ `Haskell' ------
  ;; Nice little popup
  (add-hook 'haskell-mode-hook #'company-quickhelp-mode)

  ;; Pretty symbols
  (add-hook 'haskell-mode-hook #'prettify-symbols-mode)

  ;; Haskell Prettify Symbols
  ;; Thanks to:
  ;; https://github.com/cpitclaudel/.emacs.d/blob/master/lisp/prettify-alists/haskell-prettify.el
  ;; No thanks to:
  ;; https://github.com/haskell/haskell-mode/issues/823
  (use-package haskell-prettify-enable
    :if (file-exists-p "~/.emacs.d/private/haskell-prettify-enable.el")
    :load-path "~/.emacs.d/private/"
    :defer t
    :hook #'haskell-mode)

  ;; Keybindings
  ;; Haskell interactive include hoogle
  (spacemacs/set-leader-keys-for-major-mode 'haskell-interactive-mode "h h" 'hoogle)
  (spacemacs/set-leader-keys-for-major-mode 'haskell-interactive-mode "h i" 'haskell-process-do-info)
  (spacemacs/set-leader-keys-for-major-mode 'haskell-interactive-mode "h t" 'haskell-process-do-type)

  ;; Haskell mode restart GHCI
  (spacemacs/set-leader-keys-for-major-mode 'haskell-interactive-mode "s x" 'haskell-process-restart)
  (spacemacs/set-leader-keys-for-major-mode 'haskell-mode "s x" 'haskell-process-restart)
  (spacemacs/set-leader-keys-for-major-mode 'haskell-mode "s k" 'haskell-interactive-mode-clear)


  ;; ------ `Idris' ------
  ;; Idris clear repl
  (spacemacs/set-leader-keys-for-major-mode 'idris-mode "s c" 'idris-repl-clear-buffer)
  (spacemacs/set-leader-keys-for-major-mode 'idris-repl-mode "s c" 'idris-repl-clear-buffer)
  (with-eval-after-load 'idris-mode
    (define-key idris-repl-mode-map (kbd "C-c C-k") 'idris-repl-clear-buffer))


  ;; ------ `Java' ------
  ;; ENSIME
  (setq ensime-startup-notification 'nil)

  ;; Use Gradle in Java files
  (add-hook 'java-mode-hook gradle-mode)

  ;; Recognize .tag files as jsp
  (setq web-mode-engines-alist '(("jsp" . "\\.tag\\'")))

  ;; Ignore AA project directories while searching
  (eval-after-load "grep"
    '(progn
       (add-to-list 'grep-find-ignored-directories "build")
       (add-to-list 'grep-find-ignored-directories ".sass-cache")
       (add-to-list 'grep-find-ignored-directories "node_modules")
       (add-to-list 'grep-find-ignored-directories ".gradle")
       (add-to-list 'grep-find-ignored-directories ".build")
       (add-to-list 'grep-find-ignored-directories "bin")
       (add-to-list 'grep-find-ignored-directories ".accurev")
       (add-to-list 'grep-find-ignored-directories ".git")))


  ;; ------ `JavaScript' ------
  (setq-default
   ;; Sensible indentation
   js2-basic-offset 4
   js-indent-level 4)

  (setq
   ;; Use Flycheck for linting instead of js2-mode
   ;; Disable annoying linting errors in minibuffer
   js2-strict-missing-semi-warning nil
   js2-mode-show-strict-warnings nil
   js2-mode-show-parse-errors nil

   ;; No port collision between skewer and tomcat for skewer
   httpd-port 9090)

  ;; json indent 2 spaces per panoramic
  (add-hook 'json-mode-hook
            (lambda ()
              (make-local-variable 'js-indent-level)
              (setq js-indent-level 2)))

  ;; ------ `LaTex' ------
  ;; LaTeX auto load on save
  (add-hook 'doc-view-mode-hook 'auto-revert-mode)


  ;; ------ `Nand2Tetris' ------
  ;; file types
  (add-to-list 'auto-mode-alist '("\\.asm\\'" . nand2tetris-mode))

  ;; Indent with 4 spaces
  (add-hook 'nand2tetris-mode (lambda () (setq tab-width 4)))


  ;; ------ `Scheme/Guix' ------
  (add-hook 'scheme-mode 'guix-devel-mode)

  ;; ------ `SCSS/SASS' ------
  ;; SCSS indented 4 spaces
  (add-hook 'scss-mode-hook (lambda () (setq css-indent-offset 4)))


  ;; ----- `Swift' ------
  ;; Autocompletion like
  ;; https://gist.github.com/fiveNinePlusR/611afb16f1cafdf45044f66f0d16e4cc
  (with-eval-after-load 'company-sourcekit
    (add-to-list 'company-backends 'company-sourcekit))


  ;; ------ `Web-Mode' ------
  ;; file types
  (add-to-list 'auto-mode-alist '("\\.tag\\'" . web-mode))

  ;; Keybindings
  ;; Emmet under web-mode
  (spacemacs/set-leader-keys-for-major-mode 'web-mode "E" 'emmet-expand-line)


  ;; ------ `XML' ------
  ;; file types
  (add-to-list 'auto-mode-alist '("\\.xml\\'" . web-mode))
  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
