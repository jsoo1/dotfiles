;;; package --- Summary -*- lexical-binding:t -*-
;;; My not minimal-ish anymore init.el
;;; Commentary:
;;; use like any ol init.el
;;; Code:

(defun make-standard-paragraph-rules ()
  "Return `paragraph-separate' and `paragraph-start' to normal."
  (setq-local paragraph-separate "[ \t\f]*$")
  (setq-local paragraph-start "\f\\|[ \t]*$"))

(require 'seq)
(defmacro define-prefix-keymap (name docstring &rest bindings)
  "Define a keymap named `NAME' and docstring `DOCSTRING' with many `BINDINGS' at once using `define-key'."
  `(,#'progn
     (defvar ,name (make-sparse-keymap) ,docstring)
     (define-prefix-command (quote ,name))
     ,@(seq-map
        (lambda (key-fn)
          `(define-key (quote ,name) ,(car key-fn)
             (function
              ,(pcase (cadr key-fn)
                 ((pred symbolp) (cadr key-fn))
                 ((pred (lambda (fn) (symbolp (eval fn)))) (eval (cadr key-fn)))
                 (_ (cadr key-fn))))))
        (seq-partition bindings 2))
     (quote ,name)))

;; Built in GUI elements
(setq ring-bell-function 'ignore
      initial-scratch-message ""
      focus-follows-mouse t
      vc-follow-symlinks 't)
(setq-default truncate-lines 't)
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(font . "Iosevka 12"))

(defalias 'yes-or-no-p 'y-or-n-p)

(toggle-frame-fullscreen)
(menu-bar-mode -1)
(when (fboundp #'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp #'toggle-scroll-bar)
  (toggle-scroll-bar -1))
(defun disable-scroll-bars (frame)
  "Disable scroll bars in `FRAME'."
  (modify-frame-parameters frame
                           '((vertical-scroll-bars . nil)
                             (horizontal-scroll-bars . nil))))
(add-hook 'after-make-frame-functions #'disable-scroll-bars)

;; No tabs
(setq-default indent-tabs-mode nil)

;; Cursor
(setq cursor-type 'box)
(blink-cursor-mode 0)

;; Large files
(setq large-file-warning-threshold (* 1024 1024))

;; Mouse
(xterm-mouse-mode 1)
(unless window-system
  (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
  (global-set-key (kbd "<mouse-5>") 'scroll-up-line))

;; Font
(set-face-attribute 'default t :font "Iosevka 12")

;; Custom
(setq custom-file "/dev/null")

;; Tab width
(setq tab-width 4)

;; Trailing whitespace
(add-hook 'before-save-hook #'delete-trailing-whitespace)

;; GC Threshold
(setq gc-cons-threshold (* 2 1000 1000 10))

;; Path
(setq exec-path '("~/.local/.bin"
                  "/run/setuid-programs"
                  "~/.config/guix/current/bin"
                  "~/.guix-profile/bin"
                  "~/.guix-profile/sbin"
                  "/run/current-system/profile/bin"
                  "/run/current-system/profile/sbin"
                  "~/dotfiles/emacs/"))

;; Pinentry
(setf epa-pinentry-mode 'loopback)

;; Gnus
;; set-face-attribute does not work here, why?
;; even with with-eval-after-load 'mm-uu
;; or in a hook
(defface mm-uu-extract
  '((t . (:foreground "#268bd2" :background "unspecified")))
  "Face for extracted buffers."
  :group 'gnus-article-mime)

;; Erc
(setq erc-autojoin-channels-alist nil
      erc-hide-list '("JOIN" "PART" "QUIT"))

(defun my-erc-freenode ()
  "Open erc with my configuration for freenode."
  (interactive)
  (let ((erc-prompt-for-password nil))
    (erc-tls
     :server "irc.refl.club"
     :port 5555
     :nick "jsoo")))

(defun my-erc-libera ()
  "Open erc with my configuration for freenode."
  (interactive)
  (let ((erc-prompt-for-password nil))
    (erc-tls
     :server "irc.libera.chat"
     :port 6697
     :nick "jsoo_")))

(defun my-erc-oftc ()
  "Open erc with my configuration for oftc."
  (interactive)
  (let ((erc-prompt-for-password nil))
    (erc-tls
     :server "irc.oftc.net"
     :port 6697
     :nick "jsoo")))

(add-hook 'erc-mode-hook
          (defun toggle-truncate-lines-on ()
            (toggle-truncate-lines 1)))

;; Timers
(put 'list-timers 'disabled nil)

;; Electric pairs
(electric-pair-mode 1)

;; Eldoc
(setq eldoc-echo-area-use-multiline-p 'truncate-sym-name-if-fit)

;; Elfeed
(with-eval-after-load 'elfeed
  (progn
    (define-key elfeed-search-mode-map (kbd "C-l") elfeed-load-map)
    (define-key elfeed-search-mode-map "l" elfeed-load-map)))
(add-hook
 'elfeed-search-mode-hook
 (defun make-elfeed-search-ctrl-c-map ()
   (local-set-key (kbd "C-c") elfeed-search-mode-map)))
(add-hook
 'elfeed-show-mode-hook
 (defun make-elfeed-show-ctrl-c-map ()
   (local-set-key (kbd "C-c") elfeed-show-mode-map)))

;; Shell
(setq shell-file-name "bash")

;; EShell
(add-hook 'emacs-startup-hook
          (defun eshell-in-current-directory ()
            (when (not (display-graphic-p)) (cd default-directory))
            (eshell)))

(defun my-eshell-clear-scrollback ()
  "My version of `eshell/clear-scrollback' that is interactive."
  (interactive)
  (eshell/clear-scrollback)
  (eshell-send-input nil nil t))

(add-hook 'eshell-mode-hook
            (defun my-eshell-set-keybindings ()
              (interactive)
              (define-key eshell-mode-map (kbd "C-l") #'my-eshell-clear-scrollback)))

(when (and (executable-find "fish")
           (require 'fish-completion nil t))
  (global-fish-completion-mode))

(setq initial-buffer-choice (lambda () (get-buffer-create "*eshell*"))
      eshell-highlight-prompt nil
      eshell-prompt-regexp "^[^λ]* [λ] "
      eshell-banner-message ""
      eshell-prompt-function
      (defun make-my-eshell-prompt ()
        (concat
         (propertize (eshell/whoami) 'face `(:foreground "#93a1a1"))
         " "
         (propertize (replace-regexp-in-string (concat "^" (getenv "HOME")) "~" (eshell/pwd))
                     'face `(:foreground "#268bd2"))
         " "
         (propertize (condition-case nil
                         (let ((curr-branch (magit-get-current-branch)))
                           (if curr-branch curr-branch
                             (substring (magit-rev-parse "HEAD") 0 7)))
                       (error ""))
                     'face `(:foreground "#859900"))
         " "
         (propertize "λ" 'face `(:foreground "#b58900" :weight normal))
         " ")))

(defun my-side-eshell (props)
  "Pop Eshell in a buffer using window `PROPS'."
  (interactive)
  (with-current-buffer (get-buffer-create eshell-buffer-name)
    (display-buffer-in-side-window (current-buffer) props)
    (eshell-mode))
  (pop-to-buffer eshell-buffer-name))

;; Window management
;; Split windows vertically by default, see:
;; https://stackoverflow.com/questions/20167246/emacs-open-buffer-in-vertical-split-by-default
(setq
 split-height-threshold nil
 split-width-threshold 160)

;; Dired
(setq dired-listing-switches "-al --group-directories-first")
(add-hook 'dired-mode-hook
          (defun my-dired-hook ()
            (turn-on-gnus-dired-mode)
            (auto-revert-mode)
            (dired-hide-details-mode)))

(with-eval-after-load 'dired
  (define-key dired-mode-map ")" #'dired-git-info-mode))

(diredfl-global-mode 1)
(set-face-attribute
 diredfl-dir-heading nil
 :foreground "#268bd2"
 :background "unspecified")
(set-face-attribute
 diredfl-number nil
 :foreground "#859900"
 :background "unspecified")
(set-face-attribute
 diredfl-date-time nil
 :foreground "#b58900"
 :background "unspecified")
(set-face-attribute
 diredfl-file-name nil
 :foreground "#839496"
 :background "unspecified")
(set-face-attribute
 diredfl-file-suffix nil
 :foreground "#859900"
 :background "unspecified")
(set-face-attribute
 diredfl-dir-name nil
 :foreground "#268bd2"
 :background "unspecified")
(set-face-attribute
 diredfl-symlink nil
 :foreground "#2aa198"
 :background "unspecified")
(set-face-attribute
 diredfl-no-priv nil
 :foreground "#839496"
 :background "unspecified")
(set-face-attribute
 diredfl-dir-priv nil
 :foreground "#268bd2"
 :background "unspecified")
(set-face-attribute
 diredfl-read-priv nil
 :foreground "#839496"
 :background "unspecified")
(set-face-attribute
 diredfl-write-priv nil
 :foreground "#2aa198"
 :background "unspecified")
(set-face-attribute
 diredfl-exec-priv nil
 :foreground  "#d33682"
 :background "unspecified")
(set-face-attribute
 diredfl-rare-priv nil
 :foreground "#d33682"
 :background "unspecified")
(set-face-attribute
 diredfl-other-priv nil
 :foreground "#cb4b16"
 :background "unspecified")
(set-face-attribute
 diredfl-deletion nil
 :foreground "#dc322f"
 :background "unspecified")
(set-face-attribute
 diredfl-deletion-file-name nil
 :foreground "#dc322f"
 :background "unspecified")
(set-face-attribute
 diredfl-flag-mark nil
 :foreground "#6c71c4"
 :background "unspecified")
(set-face-attribute
 diredfl-flag-mark-line nil
 :foreground "#6c71c4"
 :background "unspecified")
(set-face-attribute
 diredfl-ignored-file-name nil
 :foreground "#586e75"
 :background "unspecified")

;; World times to display
(setq
 display-time-world-list '(("America/Los_Angeles" "California")
                           ("America/Phoenix" "Phoenix")
                           ("America/Denver" "Colorado")
                           ("America/New_York" "New York")
                           ("Europe/Paris" "Central Europe"))
 display-time-world-time-format "%a, %b %d %I:%M%p %Z")

;; Backups, lockfiles, auto-saves, local variables
(setq
 backup-directory-alist `((".*" . ,(expand-file-name "backups" user-emacs-directory)))
 delete-old-versions nil
 create-lockfiles nil
 auto-save-file-name-transforms `((".*" ,(expand-file-name "auto-saves" user-emacs-directory) t))
 enable-local-eval t
 safe-local-variable-values
 '(;; Haskell-specific
   (haskell-stylish-on-save . nil)
   (haskell-process-type . stack-ghci)
   (haskell-process-type . cabal-repl)
   (haskell-mode-stylish-haskell-path . "ormolu")
   (haskell-mode-stylish-haskell-args . ("--ghc-opt" "TypeApplications"))
   (haskell-stylish-on-save . t)
   (haskell-stylish-on-save . nil)
   (projectile-compilation-command . "cabal new-build")
   (projectile-compilation-command . "guix environment guix --ad-hoc git -- make && ./pre-inst-env guix ")
   (haskell-process-wrapper-function
    . (lambda (argv)
        (append (list "env" "NO_COLOR=true") argv)))
   ;; Ocaml-specific
   (smie-indent-basic . 2)
   ;; Rust-specific
   (projectile-run-command . "cargo run")
   (projectile-compilation-command . "cargo build")
   (projectile-test-command . "cargo test")
   ;; Guix projects
   (projectile-compilation-command . "guix build -f guix.scm")
   ;; Eglot-specific
   (eglot-connect-timeout . nil)
   ;; Javascript-specific
   (js-indent-level . 2)
   ;; Builtins
   (tab-width . 4)))

;; Info
(with-eval-after-load 'Info-mode
  (define-key Info-mode-map (kbd "C-c") Info-mode-map))

;; Imenu List
(setq imenu-list-size 0.2)

;; Winner
(winner-mode t)

;; Eshell syntax highlighting
(eshell-syntax-highlighting-global-mode 1)

;; Undo-Tree
(global-undo-tree-mode)

;; Evil
(global-set-key (kbd "<escape>") #'keyboard-escape-quit)
;; somehow needs to happen before any mention of evil mode
(defvar evil-want-C-u-scroll t)
(defvar evil-want-minibuffer t)
(defvar evil-disable-insert-state-bindings t)
(defvar evil-want-abbrev-expand-on-insert-exit nil)
;; For evil-collection, which is only used for evil-magit, which
;; is only required because evil-magit is not maintained...
(defvar evil-want-keybinding nil)
(require 'evil)
(require 'evil-surround)
(require 'evil-commentary)
(require 'evil-leader)
(require 'evil-escape)
(require 'evil-replace-with-register)
(require 'navigate)
(require 'evil-collection)
(evil-mode 1)
(global-evil-surround-mode 1)
(evil-commentary-mode)
(evil-escape-mode)
(setq-default evil-replace-with-register-key (kbd "gr"))
(evil-replace-with-register-install)
(setq-default evil-escape-key-sequence "jk")
(setq-default evil-escape-unordered-key-sequence 't)
(with-eval-after-load 'magit (evil-collection-magit-setup))
(with-eval-after-load 'dired (evil-collection-dired-setup))
(global-evil-leader-mode)

(evil-set-initial-state 'compilation-mode 'normal)
(evil-set-initial-state 'ibuffer-mode 'normal)
(evil-set-initial-state 'package-menu-mode 'normal)
(evil-set-initial-state 'debugger-mode 'emacs)
(evil-set-initial-state 'proced 'normal)
(evil-set-initial-state 'ert-results-mode 'normal)
(evil-set-initial-state 'Info-mode 'normal)
(evil-set-initial-state 'comint-mode 'normal)
(evil-set-initial-state 'org-agenda-mode 'normal)
(evil-set-initial-state 'erc-mode 'normal)
(evil-set-initial-state 'Man-mode 'normal)
(evil-set-initial-state 'eshell-mode 'normal)
(evil-set-initial-state 'debbugs-gnu-mode 'normal)
(evil-set-initial-state 'tab-switcher-mode 'emacs)

(evil-declare-not-repeat #'flycheck-next-error)
(evil-declare-not-repeat #'flycheck-previous-error)

;; Evil tab motions
(add-hook 'dired-mode-hook
          (defun fix-dired-tab-motions ()
            (interactive)
            (evil-local-set-key 'normal (kbd "g t") #'tab-bar-switch-to-next-tab)
            (evil-local-set-key 'normal (kbd "g T") #'tab-bar-switch-to-prev-tab)))

(add-hook 'compilation-mode-hook #'fix-dired-tab-motions)

;; Vinegar and friends
(define-key evil-normal-state-map "-" #'dired-jump)
(define-key dired-mode-map "-" #'dired-up-directory)
(evil-define-key 'normal dired-mode-map "l" #'dired-find-file)
(evil-define-key 'normal dired-mode-map "h" #'dired-up-directory)

;; Xref
(evil-define-key 'normal xref--xref-buffer-mode-map (kbd "C-c") xref--xref-buffer-mode-map)
(setq xref-show-definitions-function #'xref--show-defs-minibuffer)
(setq xref-show-xrefs-function #'xref--show-defs-minibuffer)

;; Magit
(setq magit-display-buffer-function #'magit-display-buffer-fullcolumn-most-v1)

;; Projectile
(projectile-mode +1)
(setq projectile-completion-system 'ivy
      projectile-indexing-method 'hybrid
      projectile-enable-caching 't
      projectile-project-search-path "~/projects/"
      projectile-project-root-files-functions (list #'projectile-root-local
                                                    #'projectile-root-top-down-recurring
                                                    #'projectile-root-top-down
                                                    #'projectile-root-bottom-up)
      projectile-ignored-projects '("~" "~/projects/work"))


;; IBuffer
(defun my-set-ibuffer-filter-groups ()
  "Create my ibuffer filter groupings."
  (ibuffer-projectile-set-filter-groups)
  (setq
   ibuffer-filter-groups
   (append
    (ibuffer-projectile-generate-filter-groups)
    '(("ERC" (mode . erc-mode))
      ("Coq" (or (mode . coq-shell-mode)
                 (mode . coq-response-mode)
                 (mode .  coq-goals-mode))))))
  (unless (eq ibuffer-sorting-mode 'alphabetic)
    (ibuffer-do-sort-by-alphabetic))
  (ibuffer-update nil t))

(add-hook 'ibuffer-hook #'my-set-ibuffer-filter-groups)
(setq ibuffer-show-empty-filter-groups nil)

;; Don't always ask me to reload the tags table
(setq tags-revert-without-query 1)

(defun my-projectile-compile-buffer-name (project kind)
  "Get the name for `PROJECT's command `KIND' (`RUN' | `TEST' | `COMPILE')."
  (concat "*" project "-" kind "*"))

(defun my-projectile-command (kind)
  "Do command `KIND' (`RUN' | `TEST' | `COMPILE') the projectile project in a compilation buffer named *`PROJECTILE-PROJECT-NAME'-`KIND'*."
  (interactive)
  (let* ((old-compile-buffer (get-buffer "*compilation*"))
         (buffer-name (my-projectile-compile-buffer-name (projectile-project-name) kind))
         (old-cmd-buffer (get-buffer buffer-name)))
    (when old-compile-buffer (kill-buffer old-compile-buffer))
    (funcall (intern (concat "projectile-" kind "-project")) nil)
    (with-current-buffer (get-buffer "*compilation*")
      (when old-cmd-buffer (kill-buffer old-cmd-buffer))
      (rename-buffer buffer-name))
    (balance-windows)))

(defun my-switch-to-compile-buffer (kind)
  "Switch to compile buffer named *`PROJECTILE-PROJECT-NAME'-`KIND'."
  (switch-to-buffer-other-window (get-buffer-create (concat "*" (projectile-project-name) "-" kind "*"))))

;; Org
(require 'org-tempo)
(require 'evil-org)
(require 'evil-org-agenda)
(add-hook 'org-mode-hook #'evil-org-mode)
(add-hook 'org-agenda-mode-hook #'evil-org-mode)
(evil-org-set-key-theme '(textobjects insert navigation additional shift todo heading))
(evil-org-agenda-set-keys)
(org-babel-do-load-languages 'org-babel-load-languages
                             '((js . t)
                               (haskell . t)
                               (emacs-lisp . t)
                               (rec . t)
                               (restclient . t)
                               (scheme . t)
                               (shell . t)
                               (sql . t)))
(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "|" "DONE" "CANCELLED"))
      counsel-projectile-org-capture-templates
      '(("t" "[${name}] Todo" entry
         (file+headline "${root}/TODOs.org" "Todos")
         "* TODO %U %?
  %a")
        ("bt" "[${name}] Note" entry
         (file+headline "${root}/TODOs.org" "Notes")
         "* %U %?
  %a")))

(setq org-directory "~")
(with-eval-after-load 'org-agenda-mode
  (progn
    (define-key org-agenda-mode-map (kbd "C-c") org-agenda-mode-map)
    (define-key org-agenda-mode-map (kbd "C-m") #'org-agenda-month-view)
    (define-key org-agenda-mode-map "m" #'org-agenda-month-view)))

(defun str-to-org-dirs (repo-dir string)
  "Take newline delimited `STRING' and return list of all directories with org files in `REPO-DIR'."
  (seq-map
   (lambda (x) (concat repo-dir "/" (or (file-name-directory x) "")))
   (seq-filter
    (lambda (file) (string-match "\\.org$" file))
    (split-string string "[\n\r]+"))))

;; Set org-agenda-files
;; Right now, very not thread safe.
(defvar home-org-dirs '())
(defvar on-my-org-callback nil)
(defvar on-my-org-repo-dir nil)
(defun on-my-org-repo (repo-dir ref cb)
  "Perform `CB' on the org directories of `REPO-DIR' at git `REF'."
  ;; (assert (stringp repo-dir))
  ;; (assert (stringp ref))
  ;; (assert (functionp cb))
  (setq on-my-org-callback cb)
  (setq on-my-org-repo-dir repo-dir)
  (let ((default-directory repo-dir))
    (make-process
     :name "list-tracked-org-files"
     :command `("git" "ls-tree" "--name-only" "-r" ,ref)
     :buffer (current-buffer)
     :filter
     (lambda (proc string)
       (funcall on-my-org-callback (str-to-org-dirs on-my-org-repo-dir string)))
     :sentinel (lambda (proc event) nil))))

(on-my-org-repo
 "~" "master"
 (lambda (home-dirs)
   (setq home-org-dirs home-dirs)
   (on-my-org-repo
    "~/projects/work" "consumable"
    (lambda (work-dirs)
      (setq org-agenda-files (append home-org-dirs work-dirs))))))

(let ((default-directory "~"))
  (make-process
   :name "list-tracked-org-files"
   :command `("git" "ls-tree" "--name-only" "-r" "master")
   :buffer (current-buffer)
   :filter
   (lambda (proc string)
     (let ((default-directory "~/projects/work"))
       (setq home-org-dirs (str-to-org-dirs "~/" string))
       (make-process
        :name "list-tracked-org-files"
        :command `("git" "ls-tree" "--name-only" "-r" "consumable")
        :buffer (current-buffer)
        :filter
        (lambda (proc string)
          (setq work-org-dirs (str-to-org-dirs "~/projects/work" string))
          (setq org-agenda-files (append home-org-dirs work-org-dirs)))
        :sentinel (lambda (proc event) nil))))
   :sentinel (lambda (proc event) nil)))

(set-face-attribute
 'variable-pitch nil
 :family "Monospace")

;; export
(setq
 org-export-with-author nil
 org-export-with-toc nil
 org-export-with-title nil
 org-export-with-creator nil
 org-export-time-stamp-file nil
 org-html-validation-link nil)

;; Ediff
(setq
 ediff-split-window-function #'split-window-horizontally
 ediff-make-buffers-readonly-at-startup t)

;; LaTex
(add-hook 'latex-mode-hook #'make-standard-paragraph-rules)

;; Anzu
(global-anzu-mode)
(setq anzu-cons-mode-line-p nil)
(with-eval-after-load 'evil (require 'evil-anzu))

;; Ivy
(ivy-mode 1)
(counsel-mode 1)
(setq ivy-use-virtual-buffers t
      ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
(setcdr (assoc 'counsel-M-x ivy-initial-inputs-alist) "")
(ivy-prescient-mode)

;; Line numbers
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(setq-default display-line-numbers-type nil)
(global-hl-line-mode +1)
(defun toggle-global-hl-line ()
  "Toggle function `global-hl-line-mode'."
  (interactive)
  (global-hl-line-mode (if global-hl-line-mode -1 1)))

(defun next-line-number (curr)
  "Get the next line number after `CURR'."
  (pcase curr
    ('absolute 'relative)
    ('relative nil)
    (_ 'absolute)))

;; Which key
(require 'which-key)
(which-key-mode)
(setq which-key-idle-delay 0.1)

;; Clipboard
(xclip-mode 1)
(setq
 xclip-method 'xsel
 xclip-program "xsel")
(defun toggle-xclip-mode ()
  "Toggle `xclip-mode'."
  (interactive)
  (xclip-mode (if xclip-mode -1 1)))

;; GNUTLS issues
;; Skip v1.3 per https://debbugs.gnu.org/cgi/bugreport.cgi?bug=34341#19
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; Compilation
(define-key compilation-mode-map (kbd "C-c C-l") #'recompile)
(add-hook 'compilation-mode-hook
          (defun toggle-truncate-lines-off ()
            (toggle-truncate-lines -1)))

;; Comint
(define-key comint-mode-map (kbd "C-c C-k" ) #'comint-clear-buffer)
(define-key comint-mode-map (kbd "C-d") nil)

;; Swiper
(define-key evil-normal-state-map (kbd "C-s") #'swiper)
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "M-y") 'counsel-yank-pop)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)

;; Compilation and shell ansi colors
(require 'xterm-color)
(setq compilation-environment '("TERM=xterm-256color"))
(add-hook 'compilation-start-hook
          (defun do-xterm-color-filter (proc)
            ;; We need to differentiate between compilation-mode buffers
            ;; and running as part of comint (which at this point we assume
            ;; has been configured separately for xterm-color)
            (when (eq (process-filter proc) 'compilation-filter)
              ;; This is a process associated with a compilation-mode buffer.
              ;; We may call `xterm-color-filter' before its own filter function.
              (set-process-filter
               proc
               (lambda (proc string)
                 (funcall 'compilation-filter proc
                          (xterm-color-filter string)))))))

;; Flycheck
(require 'flycheck)
(global-flycheck-mode)
(add-hook 'flycheck-error-list-mode #'auto-revert-mode)

;; ispell
(setq ispell-program-name "aspell"
      ispell-list-command "--list")

;; Company
(add-hook 'after-init-hook 'global-company-mode)
(with-eval-after-load 'company
  (progn
    (global-company-mode)
    (define-key company-active-map (kbd "C-n") 'company-select-next)
    (define-key company-active-map (kbd "C-p") 'company-select-previous)
    (define-key company-search-map (kbd "C-n") 'company-select-next)
    (define-key company-search-map (kbd "C-p") 'company-select-previous)))

;; Eglot
(require 'eglot)
(add-hook 'eglot-managed-mode-hook #'eldoc-mode)

;; Indentation
;; Per http://emacsredux.com/blog/2013/03/27/indent-region-or-buffer/
(defun indent-buffer ()
  "Indent the currently visited buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (indent-region (region-beginning) (region-end))
          (message "Indented selected region."))
      (progn
        (indent-buffer)
        (message "Indented buffer.")))))

;; Debbugs
(setq debbugs-gnu-all-packages '("emacs" "guix" "guix-patches"))
(setq debbugs-gnu-default-packages '("guix" "guix-patches"))
(add-hook
 'debbugs-gnu-mode-hook
 (defun make-debbugs-gnu-ctrl-c-map ()
   (evil-local-set-key 'normal (kbd ",") debbugs-gnu-mode-map)
   (local-set-key (kbd "C-c") debbugs-gnu-mode-map)))

;; Elpher
(require 'elpher)
(add-hook 'elpher-mode-hook
          (defun make-elpher-ctrl-c-map ()
            (evil-local-set-key 'motion (kbd ",") elpher-mode-map)
            (local-set-key (kbd "C-c") elpher-mode-map)))

;; Restclient
(add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode))

;; Proof General
(load-file "~/.guix-profile/share/emacs/site-lisp/site-start.d/pg-init.el")

;; Idris mode
(require 'idris-mode)
(require 'inferior-idris)
(require 'idris-ipkg-mode)
(setq idris-interpreter-path "/home/john/.guix-profile/bin/idris")

(dolist (f '((idris-active-term-face        "#657b83")
             (idris-semantic-type-face      "#b58900")
             (idris-semantic-data-face      "#dc322f")
             (idris-semantic-function-face  "unspecified")
             (idris-semantic-bound-face     "#6c71c4")
             (idris-semantic-module-face    "#b58900")
             (idris-identifier-face         "#586e75")))
  (set-face-foreground (car f) (cadr f)))

(define-key idris-repl-mode-map (kbd "C-c C-k" ) #'idris-repl-clear-buffer)
(define-key idris-mode-map (kbd "C-c C-k") #'idris-repl-clear-buffer)

;; Emacs Lisp Mode
(with-eval-after-load 'company
  (add-hook 'emacs-lisp-mode-hook #'company-mode 't))
(define-key emacs-lisp-mode-map (kbd "C-c C-e") #'edebug-defun)
(define-key emacs-lisp-mode-map (kbd "C-c C-b") #'eval-buffer)
(add-hook 'emacs-lisp-mode-hook
          (defun setup-elisp-imenu ()
            (setq-local
             imenu-generic-expression
             (cons '("Keymap" "^(define-prefix-keymap\\s-+\\([a-z-]+\\)" 1)
                   imenu-generic-expression))))

;; JavaScript
(require 'nodejs-repl)
(add-hook
 'js-mode-hook
 (defun make-js-mode-keys nil
   (progn
     (define-key js-mode-map (kbd "C-c C-s") 'nodejs-repl)
     (define-key js-mode-map (kbd "C-c C-c") 'nodejs-repl-send-last-expression)
     (define-key js-mode-map (kbd "C-c C-j") 'nodejs-repl-send-line)
     (define-key js-mode-map (kbd "C-c C-r") 'nodejs-repl-send-region)
     (define-key js-mode-map (kbd "C-c C-l") 'nodejs-repl-load-file)
     (define-key js-mode-map (kbd "C-c C-k") (defun clear-nodejs-buffer () (interactive) (with-current-buffer "*nodejs*" (comint-clear-buffer))))
     (define-key js-mode-map (kbd "C-c C-z") 'nodejs-repl-switch-to-repl))))
(setq js-indent-level 4)

;; Coq
(setq proof-three-window-mode-policy 'hybrid
      proof-script-fly-past-comments t
      proof-splash-seen t
      company-coq-disabled-features '(hello))

(with-eval-after-load 'coq
  (progn
    (set-face-attribute
     'proof-locked-face nil
     :underline nil
     :background "#073642")
    (add-hook 'coq-mode-hook #'company-coq-mode)
    (define-key coq-mode-map (kbd "C-c RET") #'proof-goto-point)))

;; Haskell mode
(require 'haskell-interactive-mode)
(require 'haskell-process)
(require 'haskell-snippets)
;; See https://github.com/haskell/haskell-mode/issues/1553#issuecomment-358373643
(setq haskell-process-type 'auto
      haskell-process-log 't
      haskell-interactive-popup-errors nil
      flycheck-haskell-hpack-preference 'prefer-cabal)

(add-hook 'haskell-mode-hook
          (defun setup-haskell-flycheck ()
            (flycheck-mode)
            (flycheck-disable-checker 'haskell-ghc)))
(add-hook 'haskell-mode-hook #'yas-minor-mode-on)
;; (add-hook 'haskell-mode-hook #'interactive-haskell-mode)
(define-key haskell-mode-map (kbd "C-c C-f") 'haskell-mode-stylish-buffer)
(add-hook 'haskell-mode-hook #'make-standard-paragraph-rules)

;; Agda mode
(load-library (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda-mode locate")))
(with-eval-after-load 'agda2-mode
  (progn
    (define-key agda2-mode-map (kbd "C-c C-SPC") #'agda2-give)
    (define-key agda2-mode-map (kbd "C-c C-u") #'counsel-unicode-char)
    (define-key agda2-mode-map (kbd "C-c ,") #'agda2-goal-and-context)
    (define-key agda2-mode-map (kbd "C-c .") #'agda2-goal-and-context-and-inferred)
    (define-key agda2-mode-map (kbd "C-c ;") #'agda2-goal-and-context-and-checked)
    (set-face-attribute
     'agda2-highlight-function-face nil
     :inherit 'default
     :foreground 'unspecified)
    (set-face-attribute
     'agda2-highlight-datatype-face nil
     :inherit 'font-lock-type-face
     :foreground 'unspecified)
    (set-face-attribute
     'agda2-highlight-primitive-face nil
     :inherit 'font-lock-variable-name-face
     :foreground 'unspecified)
    (set-face-attribute
     'agda2-highlight-primitive-type-face nil
     :inherit 'font-lock-type-face
     :foreground 'unspecified)
    (set-face-attribute
     'agda2-highlight-inductive-constructor-face nil
     :inherit 'font-lock-type-face
     :foreground 'unspecified)
    (set-face-attribute
     'agda2-highlight-postulate-face nil
     :inherit 'font-lock-type-face
     :foreground 'unspecified)
    (set-face-attribute
     'agda2-highlight-keyword-face nil
     :inherit 'font-lock-keyword-face
     :foreground 'unspecified)
    (set-face-attribute
     'agda2-highlight-module-face nil
     :inherit 'font-lock-type-face
     :foreground 'unspecified)
    (set-face-attribute
     'agda2-highlight-symbol-face nil
     :inherit 'font-lock-variable-name-face
     :foreground 'unspecified)
    (set-face-attribute
     'agda2-highlight-primitive-face nil
     :inherit 'default
     :foreground 'unspecified)
    (set-face-attribute
     'agda2-highlight-number-face nil
     :inherit 'default
     :foreground 'unspecified)
    (set-face-attribute
     'agda2-highlight-string-face nil
     :inherit 'font-lock-string-face
     :foreground 'unspecified)
    (set-face-attribute
     'agda2-highlight-catchall-clause-face nil
     :background 'unspecified)
    (set-face-attribute
     'agda2-highlight-error-face nil
     :inherit 'default
     :background 'unspecified)
    (set-face-attribute
     'agda2-highlight-unsolved-meta-face nil
     :inherit 'default
     :background 'unspecified
     :foreground "red")))

;; Ocaml
(add-hook 'tuareg-mode-hook #'merlin-mode)
(add-to-list 'auto-mode-alist '("\\.ml\\'" . tuareg-mode))
(add-to-list 'auto-mode-alist '("\\.mli\\'" . tuareg-mode))
(add-hook 'tuareg-mode-hook
          (defun my-setup-ocaml-imenu ()
            "Add generic imenu indexing to tuareg's."
            (interactive)
            (setq-local
             imenu-create-index-function
             (lambda ()
               (append (tuareg-imenu-create-index)
                       (imenu--generic-function imenu-generic-expression))))))
(add-hook 'tuareg-mode-hook
          (defun my-setup-ocaml-imenu-expressions ()
            "Add module definitions to imenu expressions."
            (interactive)
            (setq-local
             imenu-generic-expression
             `(("Module" "^\\s-*module\\s-+\\(type\\s-+\\)?\\([a-zA-Z0-9_]+\\)" 2)
               ,@imenu-generic-expression))))

;; TODO: Remove when these are properly packaged in guix
(load-file "~/.guix-profile/share/emacs/site-lisp/dune.el")
(load-file "~/.guix-profile/share/emacs/site-lisp/dune-flymake.el")

;; Purescript
(add-to-list 'load-path "~/.emacs.d/private/purescript-mode")
(require 'purescript-mode-autoloads)
(add-to-list 'Info-default-directory-list "~/.emacs.d/private/purescript-mode/")
(add-to-list 'auto-mode-alist '("\\.purs\\'" . purescript-mode))
(require 'psc-ide)
(add-hook 'purescript-mode-hook
          (defun my-purescript-hook ()
            (psc-ide-mode)
            (company-mode)
            (flycheck-mode)
            (turn-on-purescript-indentation)))
(define-key purescript-mode-map (kbd "C-c C-s") 'psc-ide-server-start)
(define-key purescript-mode-map (kbd "C-c C-q") 'psc-ide-server-quit)

;; Guix
(add-to-list 'auto-mode-alist '("\\.scm\\'" . scheme-mode))
(add-hook 'scheme-mode-hook #'geiser-mode)
(with-eval-after-load 'geiser-guile
  (add-to-list 'geiser-guile-load-path "~/projects/guix"))
(with-eval-after-load 'yasnippet
  (add-to-list 'yas-snippet-dirs "~/projects/guix/etc/snippets"))
(require 'scheme)
(defvar guile-imenu-generic-expression
  (append '(("Public" "^(define-public\\s-+(?\\(\\sw+\\)" 1)
            ("Function*" "^(define\\*\\s-+(?\\(\\sw+\\)" 1)
            ("Syntax Rule" "^(define-syntax-rule\\s-+(?\\(\\sw+\\)" 1)
            ("Record" "^(define-record-type\\*?\\s-+<\\(\\sw+\\)>" 1))
          scheme-imenu-generic-expression)
  "Imenu generic expression for Guile modes.  See `imenu-generic-expression'.")
(add-hook
 'scheme-mode-hook
 (defun set-better-guile-imenu ()
   (setq-local imenu-generic-expression guile-imenu-generic-expression)))

;; Nix
(add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode))
(with-eval-after-load 'nix-mode
  (define-key nix-mode-map (kbd "C-c C-f") 'nix-format-buffer))
(defvar nix-format-on-save t
  "Format the nix buffer with nixfmt before saving.")
(add-hook 'before-save-hook
          (defun my-nix-format-buffer ()
            (when (and nix-format-on-save (eq major-mode 'nix-mode))
              (nix-format-buffer))))

;; Common Lisp
(with-eval-after-load 'geiser-guile
  (add-to-list 'geiser-guile-load-path "~/projects/guix"))

;; Rust
(require 'rust-mode)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
(setf
 (alist-get 'rust-mode eglot-server-programs)
 '("/home/john/.guix-profile/bin/rust-analyzer"))
(evil-define-key 'normal rust-mode-map (kbd ",") 'my-eglot-mode-map)
(add-hook 'rust-mode-hook #'eglot-ensure)
(add-hook 'rust-mode-hook #'eldoc-mode)
(add-hook 'rust-mode-hook #'company-mode)
(add-hook 'rust-mode-hook
          (defun disable-rust-flycheck ()
            (flycheck-mode -1)))
(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
(setq
 rust-format-on-save t
 rust-imenu-generic-expression
 (cons
  '("Async Fn" "^[[:space:]]*\\(?:\\<pub\\>[[:space:]]+\\)?\\(?:\\<default\\>[[:space:]]+\\)?\\(?:\\<unsafe\\>[[:space:]]+\\)?\\(?:\\<extern\\>[[:space:]]+\\(?:\"[^\"]+\"[[:space:]]+\\)?\\)?\\<async\\>[[:space:]]+\\<fn\\>[[:space:]]+\\([[:word:][:multibyte:]_][[:word:][:multibyte:]_[:digit:]]*\\)" 1)
  rust-imenu-generic-expression))

;; SQL
;; Inspired by:
;; https://github.com/alezost/emacs-config/blob/master/utils/al-sql.el#L47
(defun my-sql-password-from-auth-source (_ _ user server _ _)
  "Return sql password from auth-sources for SERVER and USER.
Return nil if credentials not found."
  (let ((auth (car (auth-source-search :host server :user user))))
    (require 'auth-source)
    (when auth
      (let* ((secret (plist-get auth :secret))
             (password (if (functionp secret)
                           (funcall secret)
                         secret)))
        password))))

(setq
 sql-password-wallet auth-sources
 sql-password-search-wallet-function #'my-sql-password-from-auth-source
 sql-postgres-login-params
 '((user :default "john")
   (server :default "localhost")
   (port :default 5432))
 sql-postgres-options
 '("-P" "pager=off" "--pset=columns=100"))

(with-eval-after-load 'sql
  (progn
    (sql-set-product-feature
     'postgres :prompt-regexp "^.* λ ")
    (define-key sql-mode-map (kbd "C-c C-i") #'sql-connect)
    (define-key sql-mode-map (kbd "C-c C-k")
      (defun clear-sql-buffer ()
        (interactive)
        (with-current-buffer sql-buffer (comint-clear-buffer))))))

;; Cassandra/CQL
(add-to-list 'auto-mode-alist '("\\.schema\\'" . cql-mode))

;; Cedille
(require 'cedille-mode)
(define-key cedille-mode-map (kbd "C-c C-l") #'cedille-start-navigation)
(evil-define-key 'normal cedille-mode-map (kbd "C-c") (se-navi-get-keymap 'cedille-mode))
(evil-define-key 'insert cedille-mode-map (kbd "C-c") (se-navi-get-keymap 'cedille-mode))

(set-face-attribute
 'cedille-type-face-df nil
 :foreground "#268bd2")
(set-face-attribute
 'cedille-constructor-face-df nil
 :foreground "unspecified")
(set-face-attribute
 'cedille-kind-face-df nil
 :foreground "red"
 :weight 'bold)
(set-face-attribute
 'cedille-datatype-face-df nil
 :foreground "#268bd2")
(set-face-attribute
 'cedille-keyword-face-df nil
 :foreground "#b58900")

;; Xml
(add-hook 'nxml-mode-hook #'origami-mode)

;; YAML
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

;; Plist
(add-to-list 'auto-mode-alist '("\\.plist\\'" . xml-mode))

;; Dhall
(add-to-list 'auto-mode-alist '("\\.dhall\\'" . dhall-mode))

;; Markdown
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(autoload 'gfm-mode "markdown-mode"
  "Major mode for editing GitHub Flavored Markdown files" t)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

;; Systemd
(add-to-list 'auto-mode-alist '("\\.timer\\'" . systemd-mode))
(add-to-list 'auto-mode-alist '("\\.service\\'" . systemd-mode))

;; Docker
;; dockerfile
(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

;; Shellcheck
(add-hook 'sh-mode-hook #'flycheck-mode)

;; Vimrc
(require 'vimrc-mode)
(add-to-list 'auto-mode-alist '("\\.vim\\(rc\\)?\\'" . vimrc-mode))

;; CSV
(require 'csv-mode)

;; CMake
(require 'cmake-mode)

;; ELF
(add-to-list 'auto-mode-alist '("\\.\\(?:a\\|so\\)\\'" . elf-mode))

;; Web mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.xml\\'" . web-mode))

;; Emmet
(require 'emmet-mode)
(setq emmet-move-cursor-between-quotes t)
(add-hook 'css-mode-hook  'emmet-mode)
(add-hook 'web-mode-hook 'emmet-mode)

;; Prolog
(require 'ediprolog)
(add-to-list 'auto-mode-alist '("\\.pro\\'" . prolog-mode))

;; C
(evil-define-key 'normal c-mode-map (kbd ",") 'my-eglot-mode-map)
(add-hook 'c-mode-hook #'eglot-ensure)
(add-hook 'c-mode-hook #'eldoc-mode)
(add-hook 'c-mode-hook #'company-mode)
(add-hook 'c-mode-hook
          (defun disable-c-flycheck ()
            (flycheck-mode -1)))

;; Theme
(require 'solarized)
(require 'solarized-dark-theme)
(load-theme 'solarized-dark t)

;; Transparency in gui
(set-frame-parameter (selected-frame) 'alpha '(80 . 50))
(add-to-list 'default-frame-alist '(alpha . (80 . 50)))

;; Transparency in terminal
(defun my-make-frame-transparent (frame)
  "Make `FRAME' transparent'."
  (if (or (not (display-graphic-p frame))
          (string= "base" (daemonp))
          (string= "term" (daemonp)))
      (progn (set-face-background 'default "unspecified-bg" frame)
             (set-face-background 'line-number "#073642" frame))))

(defun my-make-this-frame-transparent ()
  "Make `selected-frame' transparent."
  (interactive)
  (my-make-frame-transparent (selected-frame)))

(my-make-frame-transparent (selected-frame))
(add-hook 'after-make-frame-functions #'my-make-frame-transparent)

;; See https://stackoverflow.com/questions/19054228/emacs-disable-theme-background-color-in-terminal#
(defun on-after-init ()
  "Setup transparency in terminal."
  (unless (display-graphic-p (selected-frame))
    (progn (set-face-background 'default "unspecified-bg" (selected-frame))
           (set-face-background 'line-number "#073642" (selected-frame)))))

(add-hook 'window-setup-hook #'on-after-init)

(when (or (string= "base" (daemonp))
          (string= "term" (daemonp))
          (not (display-graphic-p (selected-frame))))
    (progn (set-face-background 'default "unspecified-bg" (selected-frame))
           (set-face-background 'line-number "#073642" (selected-frame))))

;; Tab bar
(setq
 tab-bar-show nil
 tab-bar-tab-name-function
 (defun my-window-project-name ()
   "Projectile project name of current window"
   (with-current-buffer (window-buffer (minibuffer-selected-window))
     (let ((project-name (projectile-project-name)))
       (if (string-equal "-" project-name)
           (buffer-name (current-buffer))
         project-name)))))

(set-face-attribute
 'tab-bar nil
 :foreground "#586e75"
 :background "unspecified")
(set-face-attribute
 'tab-bar-tab nil
 :foreground "#839496"
 :background "unspecified")
(set-face-attribute
 'tab-bar-tab-inactive nil
 :foreground "#586e75"
 :background "unspecified")

(defun my-tab-bar-name (tab)
  "Get `NAME' and `BUFFER' from `TAB'."
  (pcase tab
    (`(,tag . ,fields) (alist-get 'name fields))))

(defun counsel-switch-tab ()
  "Select a tab to switch to with ivy."
  (interactive)
  (ivy-read "tab: "
            (seq-map #'my-tab-bar-name (funcall tab-bar-tabs-function))
            :initial-input ""
            :action #'tab-bar-select-tab-by-name
            :require-match t))

;; Mode Line
(set-face-attribute
 'mode-line nil
 :underline nil
 :overline nil
 :foreground "#839496"
 :background "#073642"
 :box '(:line-width 1 :color "#073642" :style 'unspecified))

(set-face-attribute
 'mode-line-inactive nil
 :overline nil
 :underline nil
 :foreground "#586e75"
 :background "#002b36"
 :box '(:line-width 1 :color "#002b36" :style 'unspecified))

(defun evil-state-foreground (state)
  "The mode line color for evil-state `STATE'."
  (pcase state
    ('normal  "#859900")
    ('insert  "#b58900")
    ('emacs   "#2aa198")
    ('replace "#dc322f")
    ('visual  "#268bd2")
    ('motion  "#2aa198")))

(defun my-flycheck-error-str (n fg)
  "Properties string for a number of errors `N' with foreground color `FG'."
  (propertize (format "%s" n) 'face `(:foreground ,fg)))

(defun my-flycheck-error-format (errors)
  "Format `ERRORS', if there are any of type warning or error."
  (let-alist errors
    `(,(if .error (my-flycheck-error-str .error "#dc322f")
         "")
      " "
      ,(if .warning (my-flycheck-error-str .warning  "#b58900")
         ""))))

(defun my-flycheck-mode-line-status-text ()
  "Get text for the current flycheck state."
  (pcase flycheck-last-status-change
    (`not-checked "")
    (`no-checker "-")
    (`running "*")
    (`errored "!")
    (`finished (my-flycheck-error-format
                (flycheck-count-errors flycheck-current-errors)))
    (`interrupted ".")
    (`suspicious "?")))

(defvar my-mode-line-format
  `(" "
    (:eval (propertize
            (buffer-name)
            'face `(:foreground ,(evil-state-foreground evil-state) :weight bold)))
    "   "
    (:eval vc-mode)
    "  "
    (:eval (if (and (featurep 'flycheck) flycheck-mode)
               (my-flycheck-mode-line-status-text)
             ""))
    (:eval (if flymake-mode flymake--mode-line-format ""))
    " "
    (:eval anzu--mode-line-format)))

(setq-default mode-line-format nil)

(defun toggle-mode-line ()
  "Toggle mode-line."
  (interactive)
  (let ((ml (if mode-line-format 'nil my-mode-line-format)))
    (setq mode-line-format ml)
    (setq-default mode-line-format ml)
    (force-mode-line-update t)))

;; Window dividers
(defun my-change-window-divider ()
  "Configure window dividers to be pretty."
  (let ((display-table (or buffer-display-table standard-display-table)))
    (set-display-table-slot display-table 5 ?│)
    (set-window-display-table (selected-window) display-table)))

(add-hook 'window-configuration-change-hook 'my-change-window-divider)

;; ISO 8601
(defun iso-8601-string (&optional time zone)
  "Make an ISO 8601 formatted date string for `TIME' and `ZONE'."
  (let ((time* (or time (current-time)))
        (zone* (or zone (current-time-zone))))
    (concat
     (format-time-string "%Y-%m-%dT%T" time* zone*)
     ((lambda (x) (concat (substring x 0 3) ":" (substring x 3 5)))
      (format-time-string "%z" time* zone*)))))

;; Keybindings
(evil-leader/set-leader "<SPC>")

(evil-leader/set-key
  "<SPC>" 'counsel-M-x
  "TAB" 'evil-switch-to-windows-last-buffer
  "a" 'my-process-map
  "b" 'my-buffer-map
  "c" 'my-compile-map
  "C" 'my-counsel-map
  "d" 'my-directory-map
  "e" 'my-flycheck-map
  "f" 'my-file-map
  "g" 'my-git-map
  "h" help-map
  "i" 'my-insert-map
  "j" 'my-jump-map
  "o" 'my-org-map
  "p" 'my-projectile-map
  "q" 'my-quit-map
  "s" 'my-search-map
  "t" 'my-toggle-map
  "w" 'my-window-map
  "x" 'my-text-map
  "y" 'my-yank-map
  "z" 'my-zoom-map
  "'" 'eshell
  "/" 'counsel-projectile-rg)

(define-key help-map (kbd "w") #'woman)
(define-key help-map (kbd "W") #'man)
(define-key help-map (kbd "i") #'counsel-info-lookup-symbol)
(define-key help-map (kbd "I") #'info-apropos)

(define-prefix-keymap my-directory-map
  "my directory commands"
  "d" dired
  "/" dired-jump-other-window)

(define-prefix-keymap elfeed-load-map
  "Various ways of loading feeds"
  "o" elfeed-load-opml
  (kbd "C-o") elfeed-load-opml)

(define-prefix-keymap my-eglot-find-map
  "Find things with eglot"
  "d" xref-find-definitions
  "D" xref-find-definitions-other-window
  "r" xref-find-references
  "t" eglot-find-typeDefinition)

(define-prefix-keymap my-eglot-buffer-map
  "Goto eglot buffers"
  "e" eglot-events-buffer
  "d" eglot-stderr-buffer)

(define-prefix-keymap my-eglot-connection-map
  "Manage eglot connections."
  "e" eglot
  "r" eglot-reconnect
  "x" eglot-shutdown)

(define-prefix-keymap my-eglot-mode-map
  "My eglot bindings"
  "a" eglot-code-actions
  "b" my-eglot-buffer-map
  "C" my-eglot-connection-map
  "e" my-flymake-map
  "g" my-eglot-find-map
  "f" eglot-format
  "h" eldoc-doc-buffer
  "r" eglot-rename
  "X" eglot-signal-didChangeConfiguration)

(define-prefix-keymap my-process-map
  "my process keybindings"
  "b" my-debbugs-modes-map
  "d" docker
  "e" gnus
  "f" elfeed
  "g" guix
  "G" elpher-go
  "i" my-erc-map
  "l" list-processes
  "o" org-agenda
  "p" proced
  "t" display-time-world
  "T" list-timers)

(define-prefix-keymap my-debbugs-modes-map
  "my debbugs modes."
  "o" debbugs-org
  "b" debbugs-gnu)

(define-prefix-keymap my-erc-map
  "my erc keybindings"
  "f" my-erc-freenode
  "l" my-erc-libera
  "o" my-erc-oftc)

(define-prefix-keymap my-buffer-map
  "my buffer keybindings"
  "b" ivy-switch-buffer
  "c" (defun switch-to-compile-buffer ()
        (interactive) (my-switch-to-compile-buffer "compile"))
  "d" kill-current-buffer
  "i" ibuffer
  "k" kill-buffer
  "m" (defun switch-to-messages-buffer ()
        (interactive)
        (switch-to-buffer (get-buffer-create "*Messages*")))
  "n" normal-mode
  "r" (defun switch-to-run-buffer ()
        (interactive)
        (my-switch-to-compile-buffer "run"))
  "R" revert-buffer
  "s" (defun switch-to-scratch-buffer ()
        (interactive)
        (switch-to-buffer (get-buffer-create "*Scratch*")))
  "t" (defun switch-to-test-buffer ()
        (interactive)
        (my-switch-to-compile-buffer "test")))

(define-prefix-keymap my-compile-map
  "my keybindings for compiling"
  "b" (defun pop-to-compilation-buffer ()
        (interactive) (pop-to-buffer (get-buffer-create "*compilation*")))
  "c" counsel-compile)

(define-prefix-keymap my-counsel-map
  "my keybindings to counsel"
  "b" counsel-switch-buffer
  "c" counsel-colors-emacs
  "d" counsel-dired
  "g" counsel-git
  "h" counsel-command-history
  "i" counsel-ibuffer
  "I" counsel-info-lookup-symbol
  "m" counsel-minor
  "M" counsel-major
  "p" counsel-projectile
  "v" counsel-set-variable
  "w" counsel-colors-web)

(define-prefix-keymap my-describe-map
  "my describe keybindings"
  "a" counsel-apropos
  "b" describe-bindings
  "c" describe-char
  "f" helpful-function
  "F" counsel-describe-face
  "k" helpful-key
  "m" describe-mode
  "s" describe-symbol
  "t" describe-theme
  "v" helpful-variable)

(load-file "~/dotfiles/emacs/counsel-info-apropos.el")
(require 'counsel-info-apropos)

(define-key help-map (kbd "D") my-describe-map)
(define-key help-map (kbd "i") #'counsel-info-manual-apropos)
(define-key help-map (kbd "I") #'counsel-info-apropos)
(define-key help-map (kbd "f") #'helpful-symbol)
(define-key help-map (kbd "v") #'helpful-variable)
(define-key help-map (kbd "k") #'helpful-key)

(define-prefix-keymap my-flycheck-map
  "my flycheck keybindings"
  "b" flycheck-buffer
  "d" flycheck-describe-checker
  "e" flycheck-mode
  "n" flycheck-next-error
  "l" flycheck-list-errors
  "p" flycheck-previous-error
  "s" flycheck-select-checker)

(define-prefix-keymap my-file-map
  "my file keybindings"
  "f" counsel-find-file
  "l" find-file-literally
  "r" counsel-buffer-or-recentf
  "s" save-buffer
  "t" find-file-other-tab
  "y" (defun kill-file-name
          () (interactive) (kill-new (buffer-file-name (current-buffer)))))

(define-prefix-keymap my-flymake-map
  "My bindings for flymake"
  "l" flymake-show-diagnostics-buffer
  "n" flymake-goto-next-error
  "p" flymake-goto-prev-error)

(define-prefix-keymap my-git-map
  "my git keybindings"
  "b" magit-blame
  "c" counsel-git-checkout
  "g" magit-file-dispatch
  "r" magit-refresh-all
  "s" magit-status
  "l" magit-log-buffer-file)

(define-prefix-keymap my-insert-map
  "my insertion keybindings"
  "c" insert-char
  "i" (defun insert-uuid-v4 ()
        (interactive) (uuidgen nil))
  "t" (defun insert-time-now-as-iso-8601 ()
        (interactive) (insert (iso-8601-string)))
  "u" counsel-unicode-char)

(define-prefix-keymap my-jump-map
  "my jump keybindings"
  "i" counsel-imenu
  "o" counsel-org-goto-all
  "t" counsel-switch-tab
  "u" undo-tree-visualize
  "]" evil-jump-to-tag
  "'" counsel-mark-ring
  "\"" counsel-evil-marks
  "=" indent-region-or-buffer)

(define-prefix-keymap my-org-map
  "my org bindings"
  "a" counsel-projectile-org-agenda
  "c" counsel-projectile-org-capture
  "g" counsel-org-goto
  "i" counsel-org-entity
  "l" org-store-link
  "t" counsel-org-tag)

(defun get-tab-by-name-create (name)
  "Get or create tab for `NAME'."
  (let* ((tabs (funcall tab-bar-tabs-function))
         (tab-names (seq-map #'my-tab-bar-name tabs)))
    (if (seq-contains-p tab-names name #'string-equal)
        (tab-bar-select-tab-by-name name)
      (tab-new))))

(defun find-file-in-project-tab (project-root)
  "Find file in `PROJECT-ROOT' in a new or existing tab."
  (get-tab-by-name-create (file-name-base (directory-file-name project-root)))
  (counsel-projectile-switch-project-action-find-file project-root))

(defun switch-project-workspace ()
  "Switch to a known projectile project in a new workspace."
  (interactive)
  (let* ((counsel-projectile-switch-project-action #'find-file-in-project-tab))
    (counsel-projectile-switch-project)))

(define-prefix-keymap my-projectile-map
  "my projectile keybindings"
  "a" counsel-projectile-org-agenda
  "b" counsel-projectile-switch-to-buffer
  "c" (defun projectile-compile ()
        (interactive)
        (my-projectile-command "compile"))
  "C" counsel-projectile-org-capture
  "d" counsel-projectile-find-dir
  "D" (defun switch-to-projectile-project-root ()
        (interactive)
        (dired (projectile-project-root)))
  "e" projectile-edit-dir-locals
  "f" counsel-projectile-find-file
  "I" projectile-invalidate-cache
  "o" (defun switch-to-projectile-todos ()
        (interactive)
        (find-file (format "%sTODOs.org" (projectile-project-root))))
  "p" switch-project-workspace
  "r" (defun projectile-run ()
        (interactive) (my-projectile-command "run"))
  "t" (defun projectile-test ()
        (interactive) (my-projectile-command "test"))
  "'" (defun projectle-run-eshell-other-window ()
        (interactive)
        (switch-to-buffer-other-window (current-buffer))
        (projectile-run-eshell nil))
  "]" projectile-find-tag)

(define-prefix-keymap my-quit-map
  "my quit keybindings"
  "q" save-buffers-kill-terminal)

(define-prefix-keymap my-search-map
  "my searching keybindings"
  "s" swiper
  "p" counsel-projectile-rg)

(define-prefix-keymap my-text-map
  "my text keybindings"
  "d" delete-trailing-whitespace
  "p" my-print-map)

(define-prefix-keymap my-print-map
  "printing buffers"
  "p" print-buffer
  "P" lpr-buffer
  "r" print-region
  "R" lpr-region)

(define-prefix-keymap my-toggle-map
  "my toggles"
  "c" display-fill-column-indicator-mode
  "d" toggle-debug-on-error
  "D" toggle-debug-on-quit
  "f" toggle-frame-fullscreen
  "h" toggle-global-hl-line
  "i" imenu-list-smart-toggle
  "l" toggle-truncate-lines
  "m" toggle-mode-line
  "n" (defun cycle-line-numbers ()
        (interactive)
        (setq display-line-numbers (next-line-number display-line-numbers)))
  "o" my-org-toggle-map
  "t" tab-bar-mode
  "T" counsel-load-theme
  "w" whitespace-mode
  "x" toggle-xclip-mode)

(define-prefix-keymap my-org-toggle-map
  "org specific toggles"
  "l" org-toggle-link-display)

(define-prefix-keymap my-window-map
  "my window keybindings"
  "/" (defun my-vsplit ()
        (interactive)
        (progn (split-window-horizontally) (balance-windows)))
  "-" (defun my-split ()
        (interactive)
        (progn (split-window-vertically) (balance-windows)))
  "'" (defun pop-to-eshell ()
        (interactive)
        (my-side-eshell '((side . right) (slot . 1))) (balance-windows))
  "c" make-frame
  "d" (defun my-delete-window ()
        (interactive) (progn (delete-window) (balance-windows)))
  "D" delete-frame
  "h" (defun tmux-left () (interactive) (tmux-navigate "left"))
  "j" (defun tmux-down () (interactive) (tmux-navigate "down"))
  "k" (defun tmux-up () (interactive) (tmux-navigate "up"))
  "l" (defun tmux-right () (interactive) (tmux-navigate "right"))
  "H" evil-window-move-far-left
  "J" evil-window-move-very-bottom
  "K" evil-window-move-very-top
  "L" evil-window-move-far-right
  "m" delete-other-windows
  "r" winner-redo
  "u" winner-undo
  "=" balance-windows)

(define-prefix-keymap my-yank-map
  "my yanking keybindings"
  "y" counsel-yank-pop)

(define-prefix-keymap my-zoom-map
  "my zoom/text scaling keybindings"
  "+" text-scale-increase
  "=" text-scale-increase
  "-" text-scale-decrease)

;; Reset these to have all the configuration we just did
(with-current-buffer (get-buffer "*Messages*") (normal-mode))

(setq gc-cons-threshold (* 2 1000 1000))

;; Envrc and Direnv
;; Should be setup as late as possible.
(envrc-global-mode)
(add-hook 'eshell-directory-change-hook
          (defun envrc-reload-or-clear ()
            (interactive)
            (require 'envrc)
            (envrc--clear (current-buffer))
            (condition-case nil
                (envrc--with-required-current-env env-dir
                  (when (string= (envrc--find-env-dir) env-dir)
                    (envrc--update)
                    (message "Refreshed %s in env %s" (buffer-name) env-dir)))
              (user-error
               (message "Unloaded env for %s" (buffer-name))))))

;;; init.el ends here
