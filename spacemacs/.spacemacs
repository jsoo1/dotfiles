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
   '(agda
     (auto-completion
      (haskell :variables haskell-completion-backend 'dante))
     c-c++
     clojure
     common-lisp
     coq
     csv
     dash
     docker
     (elm :variables
          elm-format-command "elm-format-0.17"
          elm-sort-imports-on-save t)
     (erc :variables
          erc-server-list
          '(("irc.freenode.net"
             :port "6697"
             :ssl t
             :nick "jsoo")))
     emacs-lisp
     erlang
     evil-snipe
     fsharp
     (git :variables git-magit-status-fullscreen t)
     graphviz
     gtags
     (haskell :variables haskell-process-type 'stack-ghci)
     html
     (ibuffer :variables ibuffer-group-buffers-by 'projects)
     idris
     ivy
     javascript
     (latex :variables
            latex-build-command "LaTeX"
            latex-enable-auto-fill t
            latex-enable-folding t)
     major-modes
     markdown
     (mu4e :variables
           mu4e-installation-path "/usr/share/emacs/site-lisp"
           mu4e-maildir "~/.mail"
           mu4e-update-interval nil
           mu4e-compose-signature-auto-include nil
           mu4e-view-show-images t
           mu4e-view-show-addresses t)
     nginx
     nixos
     (org :variables org-enable-reveal-js-support t)
     purescript
     python
     ranger
     react
     restclient
     rust
     (shell :variables
            shell-default-height 50
            shell-defaul-position 'bottom)
     shell-scripts
     slack
     (spell-checking :variables spell-checking-enable-by-default nil)
     sql
     syntax-checking
     systemd
     themes-megapack
     treemacs
     tmux
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
   dotspacemacs-additional-packages '(doom-themes
                                      nand2tetris
                                      js-comint
                                      (shen-elisp
                                       :location (recipe :repo "deech/shen-elisp"
                                                         :fetcher github
                                                         :files ("shen*.el"))
                                       :upgrade 't))

   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()

   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
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

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the lastest
   ;; version of packages from MELPA. (default nil)
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
                                ;; (agenda . 7)
                                ;; (todos . 7)
                                (projects . 3)
                                (recents . 3))

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
   dotspacemacs-themes '(doom-one
                         spacemacs-dark
                         spacemacs-light)

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `vim-powerline' and `vanilla'. The first three
   ;; are spaceline themes. `vanilla' is default Emacs mode-line. `custom' is a
   ;; user defined themes, refer to the DOCUMENTATION.org for more info on how
   ;; to create your own spaceline theme. Value can be a symbol or list with\
   ;; additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme (if (display-graphic-p)
                                    '(spacemacs :separator arrow :separator-scale 1.5)
                                  '(vim-powerline :separator arrow :separator-scale 1.5))

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '(("Fantasque Sans Mono"
                                :size 16
                                :weight normal
                                :width wide
                                :powerline-scale 1.4)
                               ("Source Code Pro for Powerline"
                                :size 13
                                :weight normal
                                :width normal
                                :powerline-scale 1.1))

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

   ;; If non-nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ nil

   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t

   ;; If non-nil, `J' and `K' move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text t

   ;; If non-nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil

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

   ;; If non-nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil

   ;; if non-nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil

   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom

   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy 'always

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
   dotspacemacs-fullscreen-at-startup nil

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

   ;; If non-nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t

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
   dotspacemacs-line-numbers '(:relative nil :disabled-for-modes dired-mode org-mode text-mode doc-view-mode :size-limit-kb 1500)

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

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server t

   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")

   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil

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
   dotspacemacs-frame-title-format "%t %a"

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
   dotspacemacs-pretty-docs t))

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."

  ;; ------ Customize ------
  (setq custom-file "~/.customize.el")

  ;; ------ Default Shell ------
  ;; Fish outputs a bunch of junk from "call-process"
  ;; and clogs up ivy/counsel buffers :(
  ;; It also messes up the agda-mode loading which is why it's in init:(
  (setq shell-file-name "/bin/sh"))

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."

  ;; ------ Mode Hooks ------
  (add-to-list 'auto-mode-alist '("\\.tag\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.gradle\\'" . groovy-mode))
  (add-to-list 'auto-mode-alist '("\\.xml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.asm\\'" . nand2tetris-mode))

  ;; ------ Key Bindings ------
  ;; Face description
  (spacemacs/set-leader-keys "hdF" 'describe-face)
  ;; Clojure
  (spacemacs/set-leader-keys-for-major-mode 'clojure-mode "s X" 'cider-restart)
  ;; Haskell
  (spacemacs/set-leader-keys-for-major-mode 'haskell-interactive-mode "s X" 'haskell-process-restart)
  (spacemacs/set-leader-keys-for-major-mode 'haskell-interactive-mode "s c" 'haskell-interactive-mode-clear)
  (spacemacs/set-leader-keys-for-major-mode 'haskell-mode "s X" 'haskell-process-restart)
  ;; Woman in ivy/counsel
  (evil-leader/set-key "h m" 'woman)
  ;; Idris clear REPL
  (spacemacs/set-leader-keys-for-major-mode 'idris-mode "s c" 'idris-repl-clear-buffer)
  (spacemacs/set-leader-keys-for-major-mode 'idris-repl-mode "s c" 'idris-repl-clear-buffer)
  (with-eval-after-load 'idris-mode
    (define-key idris-repl-mode-map (kbd "C-c C-k") 'idris-repl-clear-buffer))
  ;; df == fd
  (setq evil-escape-unordered-key-sequence t)

  ;; ------ Fish Shell ------
  (add-hook 'term-mode-hook 'toggle-truncate-lines)

  ;; ------ Paradox ------
  (setq paradox-github-token 'paradox)

  ;; ------ JavaScript ------
  (setq inferior-js-program-command "/usr/local/bin/node"

        ;; Use Flycheck for linting instead of js2-mode
        js2-strict-missing-semi-warning nil
        js2-mode-show-strict-warnings nil
        js2-mode-show-parse-errors nil

        ;; No port collision between skewer and tomcat for skewer
        httpd-port 9090)

  ;; ------ Slack ------
  (use-package slack-config
    :defer t
    :load-path "private"
    :hook (slack-start . slack-register))

  ;; ------ Erc ------
  (setq erc-hide-list '("JOIN" "PART" "QUIT"))

  ;; ------ Copying to clipboard in terminal ------
  (use-package xclip-integration
    :defer t
    :load-path "private"
    :config
      (evil-leader/set-key "o y" #'copy-to-clipboard)
      (evil-leader/set-key "o p" #'paste-from-clipboard))

  ;; ------ CIDER ------
  (add-hook 'clojure #'evil-cleverparens-mode)
  (add-hook 'clojure #'smartparens-strict-mode)
  (setq clojure-enable-fancify-symbols t
        cider-repl-display-help-banner nil
        cider-stacktrace-default-filters '(tooling dup java))
  ;; Boot is in Nix
  (add-to-list 'exec-path "~/.nix-profile/bin/")

  ;; ------ Org Mode ------
  ;; Babel
  (with-eval-after-load 'org
    (require 'ob-python)
    (require 'ob-clojure)
    (require 'ob-shell)
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((clojure . t)
       (python . t)
       (shell . t)))

    (setq
     ;; Agenda files
     org-agenda-files (list "~/Dropbox/org/"
                            "~/Dropbox/org/pi-slice"
                            "~/Dropbox/org/haskell-beginner"
                            "~/Dropbox/org/topology"
                            "~/Dropbox/org/build-lisp" )
     ;; Org Reveal
     org-reveal-title-slide 'auto
     org-reveal-progress nil
     org-reveal-history t
     org-reveal-rolling-links t
     org-reveal-keyboard t
     org-reveal-mathjax t
     org-reveal-overview t
     org-reveal-slide-number nil

     ;; Org Export less crappy
     org-export-with-author nil
     org-export-with-creator nil
     org-export-with-toc nil
     org-export-with-email nil
     org-export-time-stamp-file nil
     org-export-with-section-numbers nil
     org-export-with-todo-keywords nil
     org-html-validation-link nil

     ;; Org Capture Templates
     org-capture-templates '(("p" "RevealJS Presentation"
                              plain (function (lambda() (buffer-file-name)))
                              "%[~/Dropbox/org/templates/presentation.org]")))
    )

  ;; ------ Email ------
  ;; sendmail
  (setq send-mail-function    'smtpmail-send-it
        smtpmail-smtp-server  "smtp.gmail.com"
        smtpmail-stream-type  'ssl
        smtpmail-smtp-service 587
        message-send-mail-function 'smtpmail-send-it)
  ;; mu4e html
  (use-package mu4e-contrib
    :init (setq mu4e-html2text-command 'mu4e-shr2text)
    :defer t)

  ;; ------ LaTeX ------
  (add-hook 'doc-view-mode-hook 'auto-revert-mode)

  ;; Diminish symbols
  (when (not (display-graphic-p))
    (setq spacemacs--diminished-minor-modes
          (seq-map
           (lambda (mode)
             (pcase (car mode)
               ('hybrid-mode                          '(hybrid-mode "Ⓔ h" " Eh"))
               ('holy-mode                            '(holy-mode "Ⓔ e" " Eh"))
               ('which-key-mode                       '(which-key-mode "Ⓚ  " " K"))
               (_                                     mode)))
           spacemacs--diminished-minor-modes))
    (add-to-list 'spacemacs--diminished-minor-modes '(server-buffer-clients " ⒮" "$"))
    (add-to-list 'spacemacs--diminished-minor-modes '(emoji-cheat-sheet-plus-display-mode " ⒠" ""))
    (add-to-list 'spacemacs--diminished-minor-modes '(interactive-haskell-mode " ⒤" ""))
    (add-to-list 'spacemacs--diminished-minor-modes '(meghanada-mode "M" "M")))

  (add-to-list 'spacemacs--diminished-minor-modes '(dired-omit-mode nil nil))
  (add-to-list 'spacemacs--diminished-minor-modes '(all-the-icons-dired-mode nil nil))

  ;; ------ DirEd ------
  ;; Icons
  (use-package all-the-icons-dired
    :defer t
    :load-path "private/all-the-icons-dired/"
    :hook (dired-mode . all-the-icons-dired-mode))

  ;; ------ Surround ------
  ;; don't put spaces between my shit!
  (evil-add-to-alist
   'evil-surround-pairs-alist
   ?\s '(" " . " ")
   ?\( '("(" . ")")
   ?\[ '("[" . "]")
   ?\{ '("{" . "}")
   ?\) '("( " . " )")
   ?\] '("[ " . " ]")
   ?\} '("{ " . " }"))

  ;; ------ Haskell ------
  ;; Indent sanely per:
  ;; http://spacemacs.org/layers/+lang/haskell/README.html#indentation-doesnt-reset-when-pressing-return-after-an-empty-line
  (defun haskell-indentation-advice ()
    (when (and (< 1 (line-number-at-pos))
               (save-excursion
                 (forward-line -1)
                 (string= "" (s-trim (buffer-substring (line-beginning-position) (line-end-position))))))
      (delete-region (line-beginning-position) (point))))

  (advice-add 'haskell-indentation-newline-and-indent
              :after 'haskell-indentation-advice)
  ;; Key bindings
  (spacemacs/set-leader-keys-for-major-mode 'haskell-interactive-mode "s X" 'haskell-process-restart)
  (spacemacs/set-leader-keys-for-major-mode 'haskell-interactive-mode "s c" 'haskell-interactive-mode-clear)
  (spacemacs/set-leader-keys-for-major-mode 'haskell-mode "s X" 'haskell-process-restart)
  ;; Nice little popup
  (add-hook 'haskell-mode-hook 'company-quickhelp-mode)
  ;; Prettify symbols
  (use-package haskell-prettify
    :defer t
    :config (spacemacs/set-leader-keys-for-major-mode
              'haskell-mode "T s" #'prettify-symbols-mode)
    :load-path "private"
    :hook (haskell-mode . haskell-prettify-enable))

  ;; ------ Truncate Long Lines Always ------
  (set-default 'truncate-lines t)

  ;; ------ Symlinks ------
  (setq vc-follow-symlinks t)

  ;; ------ Transparency ------
  (spacemacs/toggle-transparency)
  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
