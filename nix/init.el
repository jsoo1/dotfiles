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
  "Declaratively create prefix keymaps.

Define a keymap named `NAME' and docstring `DOCSTRING' with many
`BINDINGS' at once using `define-key'."
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
      focus-follows-mouse t)
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

;;; Enable narrow-to-region
(put 'narrow-to-region 'disabled nil)

;; Cursor
(setq cursor-type 'box)
(blink-cursor-mode 0)

;; Large files
(setq large-file-warning-threshold (* 1024 1024))
(global-so-long-mode 1)

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
(defvar delete-trailing-whitespace-on-save t
  "Whether to delete trailing whitespace on save.")

(defun delete-trailing-whitespace-hook ()
  "Delete trailing whitespace on save."
  (when delete-trailing-whitespace-on-save (delete-trailing-whitespace)))

(add-hook 'before-save-hook #'delete-trailing-whitespace-hook)

;; GC Threshold
(setq gc-cons-threshold (* 2 1000 1000 10))

;; Paths
(defun package-manager-user-profile ()
  "Setup env for nix/guix."
  (let* ((guix-profile (getenv "GUIX_PROFILE"))
         (nix-profile (getenv "NIX_PROFILE"))
         (nix-profiles* (getenv "NIX_PROFILES"))
         (nix-profiles (when nix-profiles*
                         (split-string nix-profiles* "\\s-+"))))
    (or guix-profile
        nix-profile
        (seq-find (lambda (p)
                    (string-match-p (rx bol (eval (getenv "HOME"))) p))
                  nix-profiles)))) ;

(setq exec-path '("~/.local/.bin"
                  "/run/current-system/profile/bin"
                  "/run/current-system/profile/sbin"
                  "~/dotfiles/emacs/"))
(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)

(load-file (expand-file-name "local.el" user-emacs-directory))

;; Tramp
(with-eval-after-load 'tramp
  (setq tramp-remote-path `(tramp-own-remote-path
                            "/home/john/.nix-profile/bin"
                            ,@tramp-remote-path)
        enable-remote-dir-locals t))

;; Pinentry
(setf epa-pinentry-mode 'loopback)

;; Color setup
(defvar my-base03  "#002b36" "Theme base03.")
(defvar my-base02  "#073642" "Theme base02.")
(defvar my-base01  "#586e75" "Theme base01.")
(defvar my-base00  "#657b83" "Theme base00.")
(defvar my-base3   "#fdf6e3" "Theme base3.")
(defvar my-base2   "#eee8d5" "Theme base2.")
(defvar my-base1   "#93a1a1" "Theme cyan.")
(defvar my-base0   "#839496" "Theme base0.")
(defvar my-yellow  "#b58900" "Theme yellow.")
(defvar my-red     "#dc322f" "Theme red.")
(defvar my-green   "#859900" "Theme green.")
(defvar my-blue    "#286bd2" "Theme blue.")
(defvar my-cyan    "#2aa198" "Theme cyan.")
(defvar my-magenta "#d33682" "Theme magenta.")
(defvar my-orange  "#cb4b16" "Theme orange.")
(defvar my-violet  "#6c71c4" "Theme violet.")

;; Recentf
(recentf-mode 1)

;; Grep
(with-eval-after-load 'grep
  (grep-apply-setting 'grep-find-command '("rg --no-heading -nIH ''" . 23)))

;; Gnus
;; set-face-attribute does not work here, why?
;; even with with-eval-after-load 'mm-uu
;; or in a hook
(defface mm-uu-extract
  `((,t . (:foreground ,my-blue :background unspecified)))
  "Face for extracted buffers."
  :group 'gnus-article-mime)

;; Erc
(setq erc-autojoin-channels-alist nil
      erc-prompt-for-password nil
      erc-rename-buffers t
      erc-ignore-list '("{\\^-\\^}")
      erc-hide-list '("JOIN" "PART" "QUIT"))

(add-hook 'erc-mode-hook
          (defun toggle-truncate-lines-on ()
            (toggle-truncate-lines 1)))

;; Timers
(put 'list-timers 'disabled nil)

;; Electric pairs
(electric-pair-mode 1)

;; Eldoc
(setq eldoc-echo-area-use-multiline-p nil)

;; VC
(setq vc-follow-symlinks 't)
(with-eval-after-load 'vc
  (defadvice vc-mode-line (after strip-backend () activate)
    (when (stringp vc-mode)
      (let ((noback (replace-regexp-in-string
                     (format "^ %s." (vc-backend buffer-file-name))
                     " " vc-mode)))
        (setq vc-mode noback)))))

;; Elfeed
(when (eq 'darwin system-type)
  (with-eval-after-load 'elfeed
    (setq elfeed-curl-max-connections 8)
    (setq-default elfeed-search-filter "@6-months-ago +unread")
    (seq-each #'elfeed-load-opml
              (directory-files
               (expand-file-name "feeds" user-emacs-directory)
               t "\\(\\.xml\\|\\.opml\\)$" t))
    (run-with-timer 0 (* 60 60) 'elfeed-update))
  (defun my-elfeed-podcast-tagger (entry)
    (when (elfeed-entry-enclosures entry)
      (elfeed-tag entry 'podcast)))
  (add-hook 'elfeed-new-entry-hook #'my-elfeed-podcast-tagger))

;; EMMS
(emms-all)
(emms-default-players)
(setq emms-source-file-directory (expand-file-name "~/Music")
      emms-player-mpd-server-name "localhost"
      emms-player-mpd-server-port "6600"
      emms-player-mpd-music-directory "~/Music")
(add-to-list 'emms-info-functions 'emms-info-mpd)
(add-to-list 'emms-player-list 'emms-player-mpd)
;; (emms-player-mpd-connect)

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
            (evil-define-key 'insert eshell-mode-map (kbd "M-C-l") #'my-eshell-clear-scrollback)
            (evil-define-key 'normal eshell-mode-map (kbd "M-C-l") #'my-eshell-clear-scrollback)
            (evil-define-key 'insert eshell-mode-map (kbd "M-C-d") #'kill-buffer-and-window)
            (evil-define-key 'normal eshell-mode-map (kbd "M-C-d") #'kill-buffer-and-window)))

(add-hook 'eshell-mode-hook #'toggle-truncate-lines-off)

(setq initial-buffer-choice (lambda () (get-buffer-create "*eshell*"))
      eshell-highlight-prompt nil
      eshell-prompt-regexp "^[^λ]* [λ] "
      eshell-banner-message ""
      eshell-prompt-function
      (defun make-my-eshell-prompt ()
        (concat
         (propertize (eshell/whoami) 'face `(:foreground ,my-base1))
         " "
         (propertize (eshell/basename (eshell/pwd))
                     'face `(:foreground ,my-blue))
         " "
         (propertize (condition-case nil
                         (let ((curr-branch (magit-get-current-branch)))
                           (if curr-branch curr-branch
                             (substring (magit-rev-parse "HEAD") 0 7)))
                       (error ""))
                     'face `(:foreground ,my-green))
         " "
         (propertize "λ" 'face `(:foreground ,my-yellow :weight normal))
         " ")))

(defun my-side-eshell (props)
  "Pop Eshell in a buffer using window `PROPS'."
  (interactive)
  (with-current-buffer (get-buffer-create eshell-buffer-name)
    (display-buffer-in-side-window (current-buffer) props)
    (eshell-mode))
  (pop-to-buffer eshell-buffer-name))

(with-eval-after-load 'term
  (set-face-attribute 'term nil
                      :background "unspecified-bg")
  (set-face-attribute 'term-color-black nil
                      :background "unspecified-bg"))

;; Window management
;; Split windows vertically by default, see:
;; https://stackoverflow.com/questions/20167246/emacs-open-buffer-in-vertical-split-by-default
(setq
 split-height-threshold nil
 split-width-threshold 160)

;; Dired
(setq dired-listing-switches (if (not (eq 'gnu/linux system-type))
                                 "-al"
                               "-al --group-directories-first")
      dired-use-ls-dired nil)
(add-hook 'dired-mode-hook
          (defun my-dired-hook ()
            (turn-on-gnus-dired-mode)
            (auto-revert-mode)
            (dired-hide-details-mode)))

(with-eval-after-load 'evil
  (evil-define-key 'normal dired-mode-map ")" #'dired-git-info-mode))

(require 'diredfl)
(diredfl-global-mode 1)
(set-face-attribute
 diredfl-dir-heading nil
 :foreground my-blue
 :background "unspecified")
(set-face-attribute
 diredfl-number nil
 :foreground my-green
 :background "unspecified")
(set-face-attribute
 diredfl-date-time nil
 :foreground my-yellow
 :background "unspecified")
(set-face-attribute
 diredfl-file-name nil
 :foreground my-base0
 :background "unspecified")
(set-face-attribute
 diredfl-file-suffix nil
 :foreground my-green
 :background "unspecified")
(set-face-attribute
 diredfl-dir-name nil
 :foreground my-blue
 :background "unspecified")
(set-face-attribute
 diredfl-symlink nil
 :foreground my-cyan
 :background "unspecified")
(set-face-attribute
 diredfl-no-priv nil
 :foreground my-base0
 :background "unspecified")
(set-face-attribute
 diredfl-dir-priv nil
 :foreground my-blue
 :background "unspecified")
(set-face-attribute
 diredfl-read-priv nil
 :foreground my-base0
 :background "unspecified")
(set-face-attribute
 diredfl-write-priv nil
 :foreground my-cyan
 :background "unspecified")
(set-face-attribute
 diredfl-exec-priv nil
 :foreground my-magenta
 :background "unspecified")
(set-face-attribute
 diredfl-rare-priv nil
 :foreground my-magenta
 :background "unspecified")
(set-face-attribute
 diredfl-other-priv nil
 :foreground my-orange
 :background "unspecified")
(set-face-attribute
 diredfl-deletion nil
 :foreground my-red
 :background "unspecified")
(set-face-attribute
 diredfl-deletion-file-name nil
 :foreground my-red
 :background "unspecified")
(set-face-attribute
 diredfl-flag-mark nil
 :foreground my-violet
 :background "unspecified")
(set-face-attribute
 diredfl-flag-mark-line nil
 :foreground my-violet
 :background "unspecified")
(set-face-attribute
 diredfl-ignored-file-name nil
 :foreground my-base01
 :background "unspecified")

;; World times to display
(setq
 display-time-world-list '(("America/Los_Angeles" "California")
                           ("America/Phoenix" "Phoenix")
                           ("America/Denver" "Colorado")
                           ("America/New_York" "East Coast")
                           ("America/Chicago" "Chicago")
                           ("Europe/Paris" "Central Europe")
                           ("Africa/Douala" "Camaroon")
                           ("Asia/Calcutta" "Bangalore"))
 display-time-world-time-format "%a, %b %d %I:%M%p %Z")

;; Backups, lockfiles, auto-saves, local variables
(setq
 backup-directory-alist `((".*" . ,(expand-file-name "backups" user-emacs-directory)))
 delete-old-versions nil
 create-lockfiles nil
 auto-save-file-name-transforms `((".*" ,(expand-file-name "auto-saves" user-emacs-directory) t))
 enable-local-eval t
 safe-local-variable-values
 `(;; Haskell-specific
   (before-save-hook . nil)
   (haskell-stylish-on-save . nil)
   (nix-format-on-save . nil)
   (haskell-process-type . stack-ghci)
   (haskell-process-type . cabal-repl)
   (haskell-mode-stylish-haskell-path . "ormolu")
   (haskell-mode-stylish-haskell-path . "fourmolu")
   (haskell-mode-stylish-haskell-args . '("-m" "inplace"))
   (haskell-mode-stylish-haskell-args . ("--ghc-opt" "TypeApplications"))
   (haskell-stylish-on-save . t)
   (haskell-stylish-on-save . nil)
   (haskell-process-wrapper-function
    . (lambda (argv)
        (append (list "env" "NO_COLOR=true") argv)))
   ;; Ocaml-specific
   (smie-indent-basic . 2)
   ;; C-specific
   (c-block-comment-prefix . "  ")
   ;; Eglot-specific
   (eglot-connect-timeout . nil)
   ;; Javascript-specific
   (js-indent-level . 2)
   ;; Builtins
   (indicate-empty-lines . t)
   (tab-width . 4)))

;; Compilation
(setq compilation-scroll-output t)

;; Info
(with-eval-after-load 'Info-mode
  (define-key Info-mode-map (kbd "C-c") Info-mode-map))

;; Imenu List
(setq imenu-list-size 0.2)

;; Winner
(setq winner-dont-bind-my-keys t)
(winner-mode t)

;; Eshell syntax highlighting
(require 'eshell-syntax-highlighting)
(eshell-syntax-highlighting-global-mode 1)

;; Smerge
(add-hook 'smerge-mode-hook
          (defun my-smerge-faces-hook ()
            (set-face-attribute 'smerge-upper nil
                                :background "unspecified-bg")
            (set-face-attribute 'smerge-lower nil
                                :background "unspecified-bg")
            (set-face-attribute 'smerge-markers nil
                                :background "unspecified-bg")
            (set-face-attribute 'smerge-refined-added nil
                                :foreground my-green
                                :background "unspecified-bg"
                                :weight 'bold)
            (set-face-attribute 'smerge-refined-removed nil
                                :foreground my-red
                                :background "unspecified-bg"
                                :weight 'bold)))

;; Indentation guides
(setq highlight-indent-guides-method 'character
      highlight-indent-guides-auto-enabled nil)
(with-eval-after-load 'highlight-indent-guides
  (set-face-foreground 'highlight-indent-guides-character-face my-base01))

;; Helpful
(require 'helpful)

;; Remove upcase-word (I never use it and it is always pestering me)
(define-key esc-map (kbd "u") nil)
(define-key global-map (kbd "M-u") nil)

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
(setq evil-replace-with-register-key (kbd "gr"))
(evil-replace-with-register-install)
(require 'evil-collection)
(evil-set-undo-system 'undo-redo)
(setq-default evil-undo-system 'undo-redo)
(evil-mode 1)
(global-evil-surround-mode 1)
(evil-commentary-mode)
(evil-escape-mode)
(setq-default evil-escape-key-sequence "jk")
(setq-default evil-escape-unordered-key-sequence 't)
(with-eval-after-load 'compilation-mode (evil-collection-compile-setup))
(with-eval-after-load 'magit (evil-collection-magit-setup))
(with-eval-after-load 'dired (evil-collection-dired-setup))
(with-eval-after-load 'ibuffer (evil-collection-ibuffer-setup))
(with-eval-after-load 'man (evil-collection-man-setup))
(with-eval-after-load 'debbugs (evil-collection-debbugs-setup))
(with-eval-after-load 'info (evil-collection-info-setup))
(with-eval-after-load 'elfeed (evil-collection-elfeed-setup))
(with-eval-after-load 'timer-list (evil-collection-timer-list-setup))
(with-eval-after-load 'emms (evil-collection-emms-setup))
(with-eval-after-load 'proced (evil-collection-proced-setup))
(with-eval-after-load 'process-list (evil-collection-process-menu-setup))
(with-eval-after-load 'tar-mode (evil-collection-tar-mode-setup))
(global-evil-leader-mode)

(evil-set-initial-state 'compilation-mode 'normal)
(evil-set-initial-state 'ibuffer-mode 'normal)
(evil-set-initial-state 'package-menu-mode 'normal)
(evil-set-initial-state 'debugger-mode 'emacs)
(evil-set-initial-state 'proced 'normal)
(evil-set-initial-state 'ert-results-mode 'normal)
(evil-set-initial-state 'Info-mode 'normal)
(evil-set-initial-state 'comint-mode 'normal)
(evil-set-initial-state 'org-agenda-mode 'motion)
(evil-set-initial-state 'erc-mode 'normal)
(evil-set-initial-state 'eshell-mode 'insert)
(evil-set-initial-state 'term-mode 'normal)
(evil-set-initial-state 'tab-switcher-mode 'emacs)
(evil-set-initial-state 'reb-mode 'normal)
(evil-set-initial-state 'tar-mode 'motion)

(evil-declare-not-repeat #'flycheck-next-error)
(evil-declare-not-repeat #'flycheck-previous-error)

(evil-declare-not-repeat #'flymake-goto-next-error)
(evil-declare-not-repeat #'flymake-goto-prev-error)

;; Evil tab motions
(add-hook 'dired-mode-hook
          (defun fix-dired-tab-motions ()
            (interactive)
            (evil-local-set-key 'normal (kbd "g t") #'tab-bar-switch-to-next-tab)
            (evil-local-set-key 'normal (kbd "g T") #'tab-bar-switch-to-prev-tab)))

(add-hook 'compilation-mode-hook #'fix-dired-tab-motions)

;; Tmux
(require 'tmux-pane)
(tmux-pane-mode)
(when (eq 'gnu/linux system-type)
  (require 'xdg)
  (setenv "TMUX_TMPDIR" (xdg-runtime-dir)))

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
(with-eval-after-load 'magit
  (define-key git-commit-mode-map (kbd "C-c M-c") #'git-commit-co-authored))

;; Project.el
(setq project-vc-merge-submodules nil
      project-compilation-buffer-name-function #'project-prefixed-buffer-name)

;; IBuffer
(defvar my-ibuffer-filter-groups
  `(,@(mapcar (lambda (r)
                (let ((expanded-root (expand-file-name r)))
                  `(,r (or (filename . ,expanded-root)
                           (directory . ,expanded-root)
                           ;; FIXME: I think this should work...
                           ;; (predicate . (string-match-p ,r (ibuffer-buffer-file-name)))
                           ))))
              (project-known-project-roots))
    ("Nix Store" (filename . "/nix/store"))
    ("Nix Build" (filename . "/tmp/nix-build"))
    ("Help" (or (mode . helpful-mode) (mode . help-mode)))
    ("ERC" (mode . erc-mode))
    ("Coq" (or (mode . coq-shell-mode)
               (mode . coq-response-mode)
               (mode .  coq-goals-mode))))
  "Filter groups for iBuffer.")

(setq ibuffer-saved-filter-groups `(("main" . ,my-ibuffer-filter-groups)))

(defun my-set-ibuffer-filter-groups ()
  "Set my ibuffer filter groups."
  (interactive)
  (setq ibuffer-filter-groups my-ibuffer-filter-groups)
  (ibuffer-do-sort-by-alphabetic)
  (ibuffer-update nil nil))

(add-hook 'ibuffer-mode-hook #'my-set-ibuffer-filter-groups)
(setq ibuffer-show-empty-filter-groups nil)

;; Don't always ask me to reload the tags table
(setq tags-revert-without-query 1)

(defun my-project-recompile ()
  "Recompile project."
  (declare (interactive-only compile))
  (interactive)
  (let* ((default-directory (project-root (project-current t)))
         (buf (get-buffer (funcall project-compilation-buffer-name-function "compilation"))))
    (if buf (with-current-buffer buf (recompile))
      (call-interactively #'project-compile))))

(defun my-switch-to-compile-buffer ()
  "Switch to project compilation buffer."
  (declare (interactive-only compile))
  (interactive)
  (let* ((default-directory (project-root (project-current t)))
         (buf (get-buffer (funcall project-compilation-buffer-name-function "compilation"))))
    (if buf (switch-to-buffer-other-window buf)
      (call-interactively #'project-compile))))

;; Org
(require 'org-tempo)
(require 'evil-org)
(require 'evil-org-agenda)
(add-hook 'org-mode-hook #'evil-org-mode)
(add-hook 'org-agenda-mode-hook #'evil-org-mode)
(evil-org-set-key-theme '(textobjects insert navigation additional shift todo heading))
(evil-org-agenda-set-keys)
(with-eval-after-load 'org-agenda
  (define-key org-agenda-mode-map (kbd "C-c RET") #'org-agenda-switch-to)
  (setq org-agenda-window-setup 'only-window))
(org-babel-do-load-languages 'org-babel-load-languages
                             '((js . t)
                               (haskell . t)
                               (emacs-lisp . t)
                               ;; (rec . t)
                               (restclient . t)
                               (scheme . t)
                               (shell . t)
                               (sql . t)))
(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "|" "DONE" "CANCELLED"))
      org-capture-templates
      '(("t" "Todo" entry (file+headline "" "Todos")
         "* TODO %U %?
  %a")
        ("p" "Plain Todo" entry (file+headline "" "Todos")
         "* TODO %?")
        ("n" "Note" entry (file+headline "" "Notes")
         "* %U %?
  %a")))

(setq org-directory "~")

(defun my-project-org-capture ()
  "Org-capture in project TODOs.org."
  (interactive)
  (let ((org-default-notes-file (format "%sTODOs.org" (project-root (project-current t)))))
    (org-capture)))

;; todos
(setq org-enforce-todo-dependencies t)

(defun str-to-org-dirs (repo-dir string)
  "Find org directories.
Take newline delimited `STRING' and return list of all
 directories with org files in `REPO-DIR'."
  (seq-map
   (lambda (x) (concat repo-dir "/" (or (file-name-directory x) "")))
   (seq-filter
    (lambda (file) (string-match "\\.org$" file))
    (split-string string "[\n\r]+"))))

;; Set org-agenda-files
(make-thread
 (lambda () (setq org-agenda-files
                  (append
                   (directory-files-recursively "~/dotfiles" "TODOs\\.org$" nil t)
                   (directory-files-recursively "~/projects" "TODOs\\.org$" nil t))))
 "get-org-files")

(set-face-attribute
 'variable-pitch nil
 :family "Monospace")

;; export
(setq
 org-export-with-author nil
 org-export-with-toc nil
 org-export-with-section-numbers nil
 org-export-with-title nil
 org-export-with-creator nil
 org-export-time-stamp-file nil
 org-html-validation-link nil)

;; refile
(setq org-refile-targets '((nil . (:maxlevel . 10))))

;; Mail composition
(setq message-fill-column nil)

;; Ediff
(setq
 ediff-split-window-function #'split-window-horizontally
 ediff-make-buffers-readonly-at-startup t)

;; LaTex
(add-hook 'latex-mode-hook #'make-standard-paragraph-rules)

;; Anzu
(require 'anzu)
(global-anzu-mode)
(setq anzu-cons-mode-line-p nil)
(with-eval-after-load 'evil (require 'evil-anzu))

;; Vertico
(vertico-mode)
(vertico-prescient-mode)
(setq enable-recursive-minibuffers t)

;; Orderless
(setq completion-styles '(orderless basic)
      completion-category-overrides '((file (styles basic partial-completion))))

;; Consult
(global-set-key (kbd "M-y") 'consult-yank-pop)
(global-set-key (kbd "C-s") 'consult-line)

;; Embark
(require 'embark-consult)
(define-key vertico-map (kbd "C-c C-c") #'embark-act)

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
(require 'xclip)
(xclip-mode 1)
(defun toggle-xclip-mode ()
  "Toggle `xclip-mode'."
  (interactive)
  (xclip-mode (if xclip-mode -1 1)))

;; GNUTLS issues
;; Skip v1.3 per https://debbugs.gnu.org/cgi/bugreport.cgi?bug=34341#19
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; Compilation
(with-eval-after-load 'compile
  (define-key compilation-mode-map (kbd "C-c C-l") #'recompile))
(add-hook 'compilation-mode-hook
          (defun toggle-truncate-lines-off ()
            (toggle-truncate-lines -1)))

;; Comint
(define-key comint-mode-map (kbd "C-c C-k" ) #'comint-clear-buffer)
(define-key comint-mode-map (kbd "C-d") nil)

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

;; Editorconfig
(require 'editorconfig)
(editorconfig-mode 1)

;; Idris mode
(require 'idris-mode)
(require 'inferior-idris)
(require 'idris-ipkg-mode)
(when (package-manager-user-profile)
  (setq idris-interpreter-path
        (expand-file-name "bin/idris" (package-manager-user-profile))))


(dolist (f `((idris-active-term-face        ,my-base00)
             (idris-semantic-type-face      ,my-yellow)
             (idris-semantic-data-face      ,my-red)
             (idris-semantic-function-face  "unspecified")
             (idris-semantic-bound-face     ,my-violet)
             (idris-semantic-module-face    ,my-yellow)
             (idris-identifier-face         ,my-base01)))
  (set-face-foreground (car f) (cadr f)))

(define-key idris-repl-mode-map (kbd "C-c C-k" ) #'idris-repl-clear-buffer)
(define-key idris-mode-map (kbd "C-c C-k") #'idris-repl-clear-buffer)

;; Emacs Lisp Mode
(with-eval-after-load 'company
  (add-hook 'emacs-lisp-mode-hook #'company-mode 't))
(define-key emacs-lisp-mode-map (kbd "C-c C-e") #'edebug-defun)
(define-key emacs-lisp-mode-map (kbd "C-c C-b") #'eval-buffer)
(define-key emacs-lisp-mode-map (kbd "C-C C-r") #'eval-region)
(add-hook 'emacs-lisp-mode-hook
          (defun setup-elisp-imenu ()
            (setq-local
             imenu-generic-expression
             `(("Keymap" "^(define-prefix-keymap\\s-+\\([a-z-]+\\)" 1)
               ("Hydra" "^(defhydra\\+?\\s-+\\([a-z-]+\\)" 1)
               ,@imenu-generic-expression))))

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
     :background "unspecified")
    (add-hook 'coq-mode-hook #'company-coq-mode)
    (define-key coq-mode-map (kbd "C-c RET") #'proof-goto-point)))

;; Haskell mode
(require 'haskell-interactive-mode)
(require 'haskell-process)
(require 'haskell-snippets)
(evil-define-key 'normal haskell-mode-map (kbd ",") 'my-eglot-mode-map)

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
(add-hook 'haskell-mode-hook #'highlight-indent-guides-mode)
;; (add-hook 'haskell-mode-hook #'eglot-ensure)
(defvar eww-hoogle-url "https://hoogle.haskell.org")
(defun eww-hoogle (query)
  "Hoogle `QUERY' in eww."
  (interactive "sQuery: ")
  (eww (format "%s/?hoogle=%s" eww-hoogle-url (url-encode-url query))))
(define-key haskell-mode-map (kbd "C-c C-h") #'eww-hoogle)

;; ;; Agda mode
;; (load-library (let ((coding-system-for-read 'utf-8))
;;                 (shell-command-to-string "agda-mode locate")))
;; (with-eval-after-load 'agda2-mode
;;   (progn
;;     (define-key agda2-mode-map (kbd "C-c C-SPC") #'agda2-give)
;;     (define-key agda2-mode-map (kbd "C-c C-u") #'insert-char)
;;     (define-key agda2-mode-map (kbd "C-c ,") #'agda2-goal-and-context)
;;     (define-key agda2-mode-map (kbd "C-c .") #'agda2-goal-and-context-and-inferred)
;;     (define-key agda2-mode-map (kbd "C-c ;") #'agda2-goal-and-context-and-checked)
;;     (set-face-attribute
;;      'agda2-highlight-function-face nil
;;      :inherit 'default
;;      :foreground 'unspecified)
;;     (set-face-attribute
;;      'agda2-highlight-datatype-face nil
;;      :inherit 'font-lock-type-face
;;      :foreground 'unspecified)
;;     (set-face-attribute
;;      'agda2-highlight-primitive-face nil
;;      :inherit 'font-lock-variable-name-face
;;      :foreground 'unspecified)
;;     (set-face-attribute
;;      'agda2-highlight-primitive-type-face nil
;;      :inherit 'font-lock-type-face
;;      :foreground 'unspecified)
;;     (set-face-attribute
;;      'agda2-highlight-inductive-constructor-face nil
;;      :inherit 'font-lock-type-face
;;      :foreground 'unspecified)
;;     (set-face-attribute
;;      'agda2-highlight-postulate-face nil
;;      :inherit 'font-lock-type-face
;;      :foreground 'unspecified)
;;     (set-face-attribute
;;      'agda2-highlight-keyword-face nil
;;      :inherit 'font-lock-keyword-face
;;      :foreground 'unspecified)
;;     (set-face-attribute
;;      'agda2-highlight-module-face nil
;;      :inherit 'font-lock-type-face
;;      :foreground 'unspecified)
;;     (set-face-attribute
;;      'agda2-highlight-symbol-face nil
;;      :inherit 'font-lock-variable-name-face
;;      :foreground 'unspecified)
;;     (set-face-attribute
;;      'agda2-highlight-primitive-face nil
;;      :inherit 'default
;;      :foreground 'unspecified)
;;     (set-face-attribute
;;      'agda2-highlight-number-face nil
;;      :inherit 'default
;;      :foreground 'unspecified)
;;     (set-face-attribute
;;      'agda2-highlight-string-face nil
;;      :inherit 'font-lock-string-face
;;      :foreground 'unspecified)
;;     (set-face-attribute
;;      'agda2-highlight-catchall-clause-face nil
;;      :background 'unspecified)
;;     (set-face-attribute
;;      'agda2-highlight-error-face nil
;;      :inherit 'default
;;      :background 'unspecified)
;;     (set-face-attribute
;;      'agda2-highlight-unsolved-meta-face nil
;;      :inherit 'default
;;      :background 'unspecified
;;      :foreground "red")))

;; Ocaml
(require 'tuareg)
(require 'merlin)
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
             `(("Module" "^\\s-*\\(module\\|and\\)\\s-+\\(type|rec\\s-+\\)?\\([a-zA-Z0-9_]+\\)" 3)
               ,@imenu-generic-expression))))
(define-key tuareg-mode-map (kbd "C-c C-o") #'merlin-occurrences)
(define-key tuareg-mode-map (kbd "C-c C-c") #'merlin-error-next)

;; ;; Purescript
;; (add-to-list 'load-path "~/.emacs.d/private/purescript-mode")
;; (require 'purescript-mode-autoloads)
;; (add-to-list 'Info-default-directory-list "~/.emacs.d/private/purescript-mode/")
;; (add-to-list 'auto-mode-alist '("\\.purs\\'" . purescript-mode))
;; (require 'psc-ide)
;; (add-hook 'purescript-mode-hook
;;           (defun my-purescript-hook ()
;;             (psc-ide-mode)
;;             (company-mode)
;;             (flycheck-mode)
;;             (turn-on-purescript-indentation)))
;; (define-key purescript-mode-map (kbd "C-c C-s") 'psc-ide-server-start)
;; (define-key purescript-mode-map (kbd "C-c C-q") 'psc-ide-server-quit)

;; Guix
(add-to-list 'auto-mode-alist '("\\.scm\\'" . scheme-mode))
(add-hook 'scheme-mode-hook #'geiser-mode)
;; (with-eval-after-load 'geiser-guile
;;   (add-to-list 'geiser-guile-load-path "~/projects/guix"))
;; (with-eval-after-load 'yasnippet
;;   (add-to-list 'yas-snippet-dirs "~/projects/guix/etc/snippets"))
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
(require 'nix-mode)

(defvar my-nix-format-cmd "nixpkgs-fmt")

;; Cribbed from haskell-mode/haskell-commands.el::haskell-mode-buffer-apply-command
(defun my-nix-format-buffer ()
  (interactive)
  (set-buffer-modified-p t)
  (let* ((out-file (make-temp-file "nixpkgs-fmt-output"))
         (err-file (make-temp-file "nixpkgs-fmt-error"))
         (coding-system-for-read 'utf-8)
         (coding-system-for-write 'utf-8)
         (cmd (executable-find my-nix-format-cmd)))
    (if (not cmd)
        (message "%s not found, doing nothing" my-nix-format-cmd)
      (unwind-protect
          (let* ((_errcode
                  (apply 'call-process-region (point-min) (point-max) cmd nil
                         `((:file ,out-file) ,err-file)
                         nil nil))
                 (err-file-empty-p
                  (equal 0 (nth 7 (file-attributes err-file))))
                 (out-file-empty-p
                  (equal 0 (nth 7 (file-attributes out-file)))))
            (if err-file-empty-p
                (if out-file-empty-p
                    (message "Error: %s produced no output and no error information, leaving buffer alone" cmd)
                  (insert-file-contents out-file nil nil nil t))
              (progn
                (with-current-buffer (get-buffer-create "*my-nix-mode*")
                  (insert-file-contents err-file)
                  (buffer-string))
                (message "Error: %s ended with errors, leaving buffer alone, see *my-nix-mode* buffer for stderr" cmd)
                (with-temp-buffer
                  (insert-file-contents err-file)
                  (display-warning cmd
                                   (buffer-substring-no-properties (point-min) (point-max))
                                   :debug)))))
        (ignore-errors (delete-file err-file))
        (ignore-errors (delete-file out-file))))))

(add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode))
(setf
 nix-nixfmt-bin "nixpkgs-fmt"
 (alist-get 'nix-mode eglot-server-programs)
 '("nil"))
(defvar nix-format-on-save t
  "Format the nix buffer with nixfmt before saving.")
(add-hook 'nix-mode-hook #'eglot-ensure)
(add-hook 'nix-mode-hook #'highlight-indent-guides-mode)
(add-hook 'before-save-hook #'my-nix-format-buffer)
(evil-define-key 'normal nix-mode-map (kbd ",") 'my-eglot-mode-map)
(evil-define-key 'normal nix-mode-map (kbd "C-c C-f") #'my-nix-format-buffer)

;; Common Lisp
(with-eval-after-load 'geiser-guile
  (add-to-list 'geiser-guile-load-path "~/projects/guix"))

;; Rust
(require 'rust-mode)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
(setf
 (alist-get 'rust-mode eglot-server-programs)
 `(,(expand-file-name "bin/rust-analyzer" (package-manager-user-profile))))
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
 rust-format-show-buffer nil
 rust-imenu-generic-expression
 (cons
  '("Async Fn" "^[[:space:]]*\\(?:\\<pub\\>[[:space:]]+\\)?\\(?:\\<default\\>[[:space:]]+\\)?\\(?:\\<unsafe\\>[[:space:]]+\\)?\\(?:\\<extern\\>[[:space:]]+\\(?:\"[^\"]+\"[[:space:]]+\\)?\\)?\\<async\\>[[:space:]]+\\<fn\\>[[:space:]]+\\([[:word:][:multibyte:]_][[:word:][:multibyte:]_[:digit:]]*\\)" 1)
  rust-imenu-generic-expression))

;; SQL
(require 'origami)

(add-hook 'sql-mode-hook #'origami-mode)

(setq sql-sqlite-program "sqlite3")
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

;; Redis
(add-to-list 'auto-mode-alist '("\\.redis\\'" . redis-mode))
(defun redis-send-paragraph ()
  "Send the current paragraph to the Redis process."
  (interactive)
  (let ((start (save-excursion
		 (backward-paragraph)
		 (point)))
	(end (save-excursion
	       (forward-paragraph)
	       (point))))
    (redis-send-region-content start end)))

(defun redis-read-args (&optional pipe)
  "Read the login params to redis-cli.

If PIPE is non nil add redis --pipe to the args list, only used
when send commands with redis protocol."
  (if-let (url (redis-cli-get-login 'url)) `("--tls" "-u" ,url)
    (let ((host (redis-cli-get-login 'host (and current-prefix-arg "Hostname: ")))
          (port (redis-cli-get-login 'port (and current-prefix-arg "Port: ")))
          (db (redis-cli-get-login 'db (and current-prefix-arg "Database number: ")))
          (password (and current-prefix-arg (read-passwd "Password: "))))
      (append (and host (list "-h" host))
              (and port (list "-p" (number-to-string port)))
              (and db (list "-n" (number-to-string db)))
              (and (not (or (null password) (string= password "")))
                   (list "-a" password))
              (and pipe (list "--pipe"))))))

(with-eval-after-load 'redis
  (define-key redis-mode-map (kbd "C-c C-i") #'redis-cli)
  (define-key redis-mode-map (kbd "C-c C-c") #'redis-send-paragraph)
  (define-key redis-mode-map (kbd "C-c C-b") #'redis-send-buffer-content)
  (define-key redis-mode-map (kbd "C-c C-k")
    (defun clear-redis-buffer ()
      (interactive)
      (with-current-buffer (get-buffer "*redis*") (comint-clear-buffer)))))

;; jq
(require 'jq-mode)
(add-to-list 'auto-mode-alist '("\\.jq\\'" . jq-mode))

;; Xml
(add-hook 'nxml-mode-hook #'origami-mode)

;; YAML
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

;; Plist
(add-to-list 'auto-mode-alist '("\\.plist\\'" . xml-mode))

;; Dhall
(with-eval-after-load 'dhall-mode
  (define-key dhall-mode-map (kbd "C-c C-i") #'insert-char))
(add-to-list 'auto-mode-alist '("\\.dhall\\'" . dhall-mode))
(defvar dhall-imenu-generic-expression
  '((nil "^\\s-*let\\s-+\\([a-zA-Z]+\\)\\(\\s-+=.*\\)?" 1)))
(add-hook 'dhall-mode-hook (defun setup-dhall-imenu ()
                             (setq-local imenu-generic-expression dhall-imenu-generic-expression)))

;; Markdown
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(autoload 'gfm-mode "markdown-mode"
  "Major mode for editing GitHub Flavored Markdown files" t)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
(add-hook 'markdown-mode-hook #'make-standard-paragraph-rules)

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

;; Terraform
(require 'terraform-mode)
(setf (alist-get 'terraform-mode eglot-server-programs)
      '("terraform-lsp"))
(add-hook 'terraform-mode-hook #'eglot-ensure)
(with-eval-after-load 'terraform-mode
  (define-key terraform-mode-map (kbd "C-c C-f") #'terraform-format-buffer))
(evil-define-key 'normal terraform-mode-map (kbd ",") 'my-eglot-mode-map)

;; C
(evil-define-key 'normal c-mode-map (kbd ",") 'my-eglot-mode-map)
(add-hook 'c-mode-hook #'eglot-ensure)
(add-hook 'c-mode-hook #'eldoc-mode)
(add-hook 'c-mode-hook #'company-mode)
(add-hook 'c-mode-hook
          (defun disable-c-flycheck ()
            (flycheck-mode -1)))

;; Meson
(require 'meson-mode)

;; Go
(require 'go-mode)
(evil-define-key 'normal go-mode-map (kbd ",") 'my-eglot-mode-map)
(add-hook 'go-mode-hook #'eglot-ensure)
(add-hook 'go-mode-hook #'eldoc-mode)
(add-hook 'go-mode-hook #'eldoc-mode #'company-mode)
(add-hook 'go-mode-hook
          (defun disable-go-flycheck ()
            (flycheck-mode -1)))
(setf (alist-get 'go-mode eglot-server-programs) '("gopls"))

;; C++
(evil-define-key 'normal c++-mode-map (kbd ",") 'my-eglot-mode-map)
(add-hook 'c++-mode-hook #'eglot-ensure)
(add-hook 'c++-mode-hook #'eldoc-mode)
(add-hook 'c++-mode-hook #'company-mode)
(add-hook 'c++-mode-hook
          (defun disable-c-flycheck ()
            (flycheck-mode -1)))

;; Theme
(require 'base16-solarized-dark-theme)
(setq base16-theme-256-color-source "colors")
(load-theme 'base16-solarized-dark t)

;; Missing from base theme
(set-face-attribute
 'help-key-binding nil
 :background 'unspecified
 :foreground my-orange)

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
             (set-face-background 'line-number my-base02 frame))))

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
           (set-face-background 'line-number my-base02 (selected-frame)))))

(add-hook 'window-setup-hook #'on-after-init)

(when (or (string= "base" (daemonp))
          (string= "term" (daemonp))
          (not (display-graphic-p (selected-frame))))
  (progn (set-face-background 'default "unspecified-bg" (selected-frame))
         (set-face-background 'line-number my-base02 (selected-frame))))

;; Shackle
(setq shackle-rules '((compilation-mode :noselect t :align right :other t)
                      (Man-mode :select t :popup t :align right :size 0.5 :other t)
                      (woman-mode :select t :popup t :align right :size 0.5 :other t)
                      (helpful-mode :align right)
                      (org-agenda-mode :select 1 :size 1.0)))
(shackle-mode)

;; Popper
(setq popper-display-control 'user
      popper-group-function #'popper-group-by-project
      popper-reference-buffers
      '("\\*Async Shell Command\\*"
        "*Warnings*"
        "\\*Async-native-compile-log\\*"
        "*\\*.*compile\\*$" compilation-mode
        display-time-world-mode
        "^\\*eldoc\\*$"
        "^\\*eshell.*\\*$" eshell-mode
        elfeed-search-mode
        flycheck-error-list-mode
        flymake-diagnostics-buffer-mode
        grep-mode
        help-mode
        helpful-mode
        "^\\*ivy-occur.*\\*$" ivy-occur-mode
        org-agenda-mode
        "^\\*Proced\\*$" proced-mode
        "^\\*Process List\\*$" process-menu-mode
        magit-diff-mode
        magit-process-mode
        magit-log-mode
        Man-mode
        "\\*Messages\\*"
        "Output\\*$"
        reb-mode
        term-mode
        woman-mode
        world-clock-mode))
(popper-mode 1)

;; Tab bar
(set-face-attribute
 'tab-bar nil
 :foreground my-base01
 :background "unspecified")
(set-face-attribute
 'tab-bar-tab nil
 :foreground my-base0
 :background "unspecified")
(set-face-attribute
 'tab-bar-tab-inactive nil
 :foreground my-base01
 :background "unspecified")

;; Mode Line
(set-face-attribute
 'mode-line nil
 :underline nil
 :overline nil
 :foreground my-base0
 :background my-base02
 :box `(:line-width 1 :color ,my-base02 :style unspecified))

(set-face-attribute
 'mode-line-inactive nil
 :overline nil
 :underline nil
 :foreground my-base01
 :background "unspecified"
 :box `(:line-width 1 :color ,my-base03 :style unspecified))

(defun evil-state-foreground (state)
  "The mode line color for evil-state `STATE'."
  (pcase state
    ('normal  my-green)
    ('insert  my-yellow)
    ('emacs   my-cyan)
    ('replace my-red)
    ('visual  my-blue)
    ('motion  my-cyan)))

(defun my-flycheck-error-str (n fg)
  "Properties string for a number of errors `N' with foreground color `FG'."
  (propertize (format "%s" n) 'face `(:foreground ,fg)))

(defun my-flycheck-error-format (errors)
  "Format `ERRORS', if there are any of type warning or error."
  (let-alist errors
    `(,(if .error (my-flycheck-error-str .error my-red)
         "")
      " "
      ,(if .warning (my-flycheck-error-str .warning my-yellow)
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
    (:eval (if flymake-mode flymake-mode-line-format ""))
    " "
    (:eval anzu--mode-line-format)))

(setq-default mode-line-format my-mode-line-format)

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
  "Make short ISO date string.
Make a short ISO 8601 formatted date string for `TIME' and
`ZONE' - defaulting to `CURRENT-TIME' and `CURRENT-TIME-ZONE',
respectively."
  (let ((time* (or time (current-time)))
        (zone* (or zone (current-time-zone))))
    (format-time-string "%Y-%m-%d" time* zone*)))

(defun iso-8601-string-full (&optional time zone)
  "Make longer ISO date string.
Make full ISO 8601 formatted date string for `TIME' and `ZONE.'
- defaulting to `CURRENT-TIME' and `CURRENT-TIME-ZONE',
respectively."
  (let ((time* (or time (current-time)))
        (zone* (or zone (current-time-zone))))
    (concat
     (format-time-string "%Y-%m-%dT%T" time* zone*)
     ((lambda (x) (concat (substring x 0 3) ":" (substring x 3 5)))
      (format-time-string "%z" time* zone*)))))

;; Zoom
(defhydra zoom (:hint nil)
  "zoom"
  ("+" text-scale-increase "+")
  ("=" text-scale-increase "+")
  ("-" text-scale-decrease "-")
  ("_" text-scale-decrease "-"))

;; Keybindings
(evil-leader/set-leader "<SPC>")

(evil-leader/set-key
  "<SPC>" 'execute-extended-command
  "TAB" 'evil-switch-to-windows-last-buffer
  "a" 'my-process-map
  "b" 'my-buffer-map
  "c" 'my-consult-map
  "d" 'my-directory-map
  "e" 'my-flycheck-map
  "f" 'my-file-map
  "g" 'magit-dispatch
  "G" 'magit-status
  "h" help-map
  "i" 'my-insert-map
  "j" 'my-jump-map
  "o" 'my-org-map
  "p" 'my-project-map
  "q" 'my-quit-map
  "s" 'my-search-map
  "t" 'my-toggle-map
  "w" 'evil-window-map
  "x" 'my-text-map
  "y" 'my-yank-map
  "z" 'zoom/body
  "'" 'eshell
  "/" 'consult-ripgrep)

(define-key help-map (kbd "w") #'woman)
(define-key help-map (kbd "W") #'man)
(define-key help-map (kbd "i") #'info-lookup-symbol)
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
  "h" eldoc
  "r" eglot-rename
  "X" eglot-signal-didChangeConfiguration)

(define-prefix-keymap my-emms-map
  "my emms bindings"
  "m" emms
  "p" emms-pause)

(define-prefix-keymap my-process-map
  "my process keybindings"
  "&" async-shell-command
  "a" pulseaudio-control-hydra/body
  "b" my-debbugs-modes-map
  "d" docker
  "e" gnus
  "f" elfeed
  "g" guix
  "G" elpher-go
  "i" my-erc-map
  "l" list-processes
  "m" my-emms-map
  "o" org-agenda
  "p" proced
  "r" re-builder
  "t" display-time-world
  "T" list-timers
  "w" eww)

(define-prefix-keymap my-debbugs-modes-map
  "my debbugs modes."
  "o" debbugs-org
  "b" debbugs-gnu)

(defun my-erc (port)
  "Open my erc configuration using znc on `PORT'."
  (interactive "nport: ")
  (erc-tls
   :server "irc.refl.club"
   :port port
   :nick "jsoo"))

(define-prefix-keymap my-erc-map
  "my erc keybindings"
  "f" (defun my-erc-freenode ()
        "Open erc with my configuration for freenode."
        (interactive)
        (my-erc 5555))
  "l" (defun my-erc-libera ()
        "Open erc with my configuration for libera."
        (interactive)
        (my-erc 5556))
  "o" (defun my-erc-oftc ()
        "Open erc with my configuration for oftc."
        (interactive)
        (my-erc 5557)))

(define-prefix-keymap my-buffer-map
  "my buffer keybindings"
  "b" switch-to-buffer
  "B" switch-to-buffer-other-window
  "c" my-switch-to-compile-buffer
  "d" kill-buffer-and-window
  "i" ibuffer
  "k" kill-buffer
  "m" (defun switch-to-messages-buffer ()
        (interactive)
        (switch-to-buffer (get-buffer-create "*Messages*")))
  "n" normal-mode
  "R" revert-buffer
  "s" (defun switch-to-scratch-buffer ()
        (interactive)
        (switch-to-buffer (get-buffer-create "*scratch*"))))

(define-prefix-keymap my-consult-map
  "my keybindings to consult"
  "g" project-find-file
  "m" consult-minor-mode-menu)

(define-prefix-keymap my-describe-map
  "my describe keybindings"
  "a" consult-apropos
  "b" describe-bindings
  "f" helpful-function
  "F" describe-face
  "k" helpful-key
  "m" describe-mode
  "s" describe-symbol
  "t" describe-theme
  "v" helpful-variable)

(define-key help-map (kbd "D") my-describe-map)
(define-key help-map (kbd "f") #'helpful-symbol)
(define-key help-map (kbd "v") #'helpful-variable)
(define-key help-map (kbd "k") #'helpful-key)
(define-key help-map (kbd "c") #'describe-char)

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
  "f" find-file
  "g" magit-find-file
  "l" find-file-literally
  "r" consult-recent-file
  "s" save-buffer
  "t" find-file-other-tab
  "y" (defun kill-file-name
          () (interactive) (kill-new (buffer-file-name (current-buffer)))))

(define-prefix-keymap my-flymake-map
  "My bindings for flymake"
  "l" flymake-show-buffer-diagnostics
  "n" flymake-goto-next-error
  "p" flymake-goto-prev-error)

(define-prefix-keymap my-git-map
  "my git keybindings"
  "A" magit-cherry-pick
  "b" magit-branch
  "c" magit-checkout
  "d" magit-diff
  "f" magit-fetch
  "g" magit-file-dispatch
  "G" magit-dispatch
  "O" magit-reset
  "p" magit-push
  "r" magit-rebase
  "s" magit-status
  "l" magit-log
  "z" magit-stash)

(define-prefix-keymap my-insert-map
  "my insertion keybindings"
  "c" insert-char
  "i" (defun insert-uuid-v4 ()
        (interactive) (uuidgen nil))
  "t" (defun insert-time-now-as-iso-8601 ()
        (interactive) (insert (iso-8601-string)))
  "T" (defun insert-time-now-as-iso-8601-full ()
        (interactive) (insert (iso-8601-string-full))))

(define-prefix-keymap my-jump-map
  "my jump keybindings"
  "i" consult-imenu
  "o" consult-org-agenda
  "p" switch-to-buffer-other-window
  "t" tab-switch
  "]" evil-jump-to-tag
  "'" consult-mark
  "=" indent-region-or-buffer)

(define-prefix-keymap my-org-mime-map
  "my org-mime keybindings"
  "m" org-mime-htmlize
  "s" org-mime-org-subtree-htmlize
  "b" org-mime-org-buffer-htmlize)

(define-prefix-keymap my-org-map
  "my org bindings"
  "a" org-agenda
  "c" my-project-org-capture
  "d" org-babel-detangle
  "g" consult-org-heading
  "l" org-store-link
  "m" my-org-mime-map)

(define-prefix-keymap my-project-compile-map
  "my project compilation keybindings"
  "c" my-project-recompile
  "C" project-compile)

(define-prefix-keymap my-project-map
  "my project keybindings"
  "&" project-async-shell-command
  "b" consult-project-buffer
  "c" my-project-compile-map
  "C" my-project-org-capture
  "d" project-find-dir
  "D" project-dired
  "e" (defun switch-to-project-dir-locals ()
        (interactive)
        (find-file (format "%s.dir-locals.el" (project-root (project-current t)))))
  "f" project-find-file
  "o" (defun switch-to-project-todos ()
        (interactive)
        (find-file (format "%sTODOs.org" (project-root (project-current t)))))
  "p" project-switch-project
  "'" (defun project-eshell-other-window ()
        (interactive)
        (switch-to-buffer-other-window (current-buffer))
        (project-eshell)))

(define-prefix-keymap my-quit-map
  "my quit keybindings"
  "q" save-buffers-kill-terminal)

(define-prefix-keymap my-search-map
  "my searching keybindings"
  "g" grep-find
  "s" consult-line
  "p" consult-ripgrep)

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
  "I" highlight-indent-guides-mode
  "l" toggle-truncate-lines
  "m" toggle-mode-line
  "n" (defun cycle-line-numbers ()
        (interactive)
        (setq display-line-numbers (next-line-number display-line-numbers)))
  "o" my-org-toggle-map
  "p" popper-cycle
  "P" popper-toggle-type
  "t" tab-bar-mode
  "T" consult-theme
  "w" whitespace-mode
  "x" toggle-xclip-mode)

(define-prefix-keymap my-org-toggle-map
  "org specific toggles"
  "l" org-toggle-link-display)

(pcase-dolist
    (`(,key . ,fn)
     `(("/" . ,(defun my-vsplit ()
                 (interactive)
                 (progn (split-window-horizontally) (balance-windows))))
       ("-" . ,(defun my-split ()
                 (interactive)
                 (progn (split-window-vertically) (balance-windows))))
       ("'" . ,(defun pop-to-eshell ()
                 (interactive)
                 (if (project-current)
                     (project-eshell-other-window)
                   (my-side-eshell '((side . right) (slot . 1))) (balance-windows))))
       ("c" . make-frame)
       ("d" . ,(defun my-delete-window ()
                 (interactive) (progn (delete-window) (balance-windows))))
       ("D" . delete-frame)
       ("h" . tmux-pane-omni-window-left)
       ("j" . tmux-pane-omni-window-down)
       ("k" . tmux-pane-omni-window-up)
       ("l" . tmux-pane-omni-window-right)
       ("H" . evil-window-move-far-left)
       ("J" . evil-window-move-very-bottom)
       ("K" . evil-window-move-very-top)
       ("L" . evil-window-move-far-right)
       ("m" . delete-other-windows)
       ("p" . ,(defun my-popper-toggle-latest ()
                 (interactive) (popper-toggle-latest) (delete-window)))
       ("r" . winner-redo)
       ("u" . winner-undo)
       ("=" . balance-windows)))
  (define-key evil-window-map (kbd key) fn))

(define-prefix-keymap my-yank-map
  "my yanking keybindings"
  "y" consult-yank-pop)

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
