;;; package --- Summary
;;; My not minimal-ish anymore init.el
;;; Commentary:
;;; use like any ol init.el
;;; Code:

(defun make-standard-paragraph-rules ()
  "Return `paragraph-separate' and `paragraph-start' to normal."
  (setq-local paragraph-separate "[ \t\f]*$")
  (setq-local paragraph-start "\f\\|[ \t]*$"))

;; Built in GUI elements
(setq ring-bell-function 'ignore
      initial-scratch-message ""
      focus-follows-mouse t
      vc-follow-symlinks 't)
(setq-default truncate-lines 't)
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(font . "Iosevka 18"))

(defalias 'yes-or-no-p 'y-or-n-p)

(toggle-frame-fullscreen)
(menu-bar-mode -1)

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
(set-face-attribute 'default t :font "Iosevka 18")

;; Custom
(setq custom-file "/dev/null")

;; Tab width
(setq tab-width 4)

;; Trailing whitespace
(add-hook 'before-save-hook #'delete-trailing-whitespace)

;; GC Threshold
(setq gc-cons-threshold 200000000)

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
(add-hook 'erc-mode-hook #'erc-notifications-mode)

; Elfeed
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

(setq initial-buffer-choice (lambda () (get-buffer-create "*eshell*"))
      eshell-highlight-prompt nil
      eshell-prompt-regexp "^[^λ]* [λ] "
      eshell-banner-message ""
      eshell-prompt-function
      (lambda ()
        (concat
         (propertize (eshell/whoami) 'face `(:foreground "#93a1a1"))
         " "
         (propertize (replace-regexp-in-string (concat "^" (getenv "HOME")) "~" (eshell/pwd))
                     'face `(:foreground "#268bd2"))
         " "
         (propertize (condition-case nil (magit-get-current-branch) (error ""))
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

;; Dired
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
 :foreground "#2aa198"
 :background "unspecified")
(set-face-attribute
 diredfl-write-priv nil
 :foreground "#d33682"
 :background "unspecified")
(set-face-attribute
 diredfl-exec-priv nil
 :foreground  "#dc322f"
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

;; Byte compile
(require 'bytecomp)
(setq byte-compile-warnings t)
(setq byte-compile-error-on-warn nil)

;; Backups, lockfiles, auto-saves, local variables
(setq
 backup-directory-alist `((".*" . "~/.emacs.d/private/backups/"))
 delete-old-versions nil
 create-lockfiles nil
 auto-save-file-name-transforms `((".*" "~/.emacs.d/private/auto-saves/" t))
 enable-local-eval t
 safe-local-variable-values
 (append
  '((haskell-stylish-on-save . nil)
    (haskell-process-type . stack-ghci)
    (haskell-process-type . cabal-repl)
    (haskell-mode-stylish-haskell-path . "ormolu")
    (haskell-mode-stylish-haskell-args . ("--ghc-opt TypeApplications"))
    (projectile-compilation-command . "cargo build")
    (projectile-test-command . "cargo test")
    (projectile-compilation-command . "./.bin/cargo build")
    (projectile-test-command . "./.bin/cargo test")
    (projectile-compilation-command . "../.bin/wasm-pack build --target web --out-name wasm --out-dir ../frontend-server/static")
    (projectile-compilation-dir . ".")
    (projectile-compilation-command . "cabal new-build")
    (projectile-run-command . "cargo run")
    (projectile-run-command . "cabal new-run exe:refl-club -- .static")
    (haskell-stylish-on-save . t)
    (haskell-stylish-on-save . nil)
    (projectile-project-root . "/home/john/projects/xml-types-xmlbf/")
    (projectile-project-root . "/home/john/projects/work/")
    (projectile-project-root . "/home/john/projects/feed-xmlbf/")
    (projectile-project-root . "/home/john/projects/guix/")
    (projectile-project-root . "/home/john/projects/work/projects/vast/")
    (projectile-project-root . "/home/john/projects/work/projects/bid-server/")
    (projectile-project-root . "/home/john/projects/psc-ide-emacs/")
    (projectile-project-root . "/home/john/projects/haskell-mode/")
    (haskell-mode-stylish-haskell-args . '("--ghc-opt TypeApplications"))
    (projectile-compilation-command . "guix environment --ad-hoc openssl libuv mysql cassandra-cpp postgresql pkg-config gcc-toolchain -- env CC=gcc cargo build")
    (projectile-test-command . "guix environment --ad-hoc openssl libuv mysql cassandra-cpp postgresql pkg-config gcc-toolchain -- env CC=gcc cargo test")
    (tab-width . 4)
    (js-indent-level . 2)
    (haskell-process-wrapper-function
     . (lambda (argv)
         (append (list "env" "NO_COLOR=true") argv)))
    (projectile-compilation-command . "guix environment guix --ad-hoc git -- make && ./pre-inst-env guix "))
  safe-local-variable-values))

;; Direnv
(direnv-mode)

;; Imenu List
(setq imenu-list-size 0.2)

;; Winner
(winner-mode t)

;; Evil
(global-set-key (kbd "<escape>") #'keyboard-escape-quit)
(setq evil-want-C-u-scroll t
      evil-want-minibuffer t
      evil-disable-insert-state-bindings t
      ;; somehow needs to happen before any mention of evil mode
      evil-want-abbrev-expand-on-insert-exit nil)
(require 'evil)
(require 'evil-surround)
(require 'evil-commentary)
(require 'evil-leader)
(require 'evil-escape)
(require 'smartparens-config)
(require 'navigate)

(evil-mode 1)
(global-evil-surround-mode 1)
(evil-commentary-mode)
(evil-escape-mode)
(setq-default evil-escape-key-sequence "df")
(setq-default evil-escape-unordered-key-sequence 't)
(global-evil-leader-mode)
(smartparens-global-mode 1)

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

(evil-declare-not-repeat #'flycheck-next-error)
(evil-declare-not-repeat #'flycheck-previous-error)

;; Magit
(require 'evil-magit)
(setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)

;; Projectile
(projectile-mode +1)
(setq projectile-completion-system 'ivy
      projectile-indexing-method 'hybrid
      projectile-enable-caching 't
      projectile-project-search-path "~/projects/"
      projectile-project-root-files-functions '(projectile-root-top-down-recurring
                                                projectile-root-top-down
                                                projectile-root-bottom-up
                                                projectile-root-local))

;; IBuffer
(defun my-set-ibuffer-filter-groups ()
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

;; (add-hook 'ibuffer-hook #'ibuffer-auto-mode)
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
      (rename-buffer buffer-name))))

(defun my-switch-to-compile-buffer (kind)
  "Switch to compile buffer named *`PROJECTILE-PROJECT-NAME'-`KIND'."
  (switch-to-buffer-other-window (get-buffer-create (concat "*" (projectile-project-name) "-" kind "*"))))

;; Dir Locals -- see https://emacs.stackexchange.com/questions/13080/reloading-directory-local-variables
(defun my-projectile-reload-dir-locals ()
  "Reload each buffer with the same `default-directory` as the current buffer's."
  (interactive)
  (dolist (buffer (projectile-project-buffers))
    (with-current-buffer buffer
      (hack-dir-local-variables-non-file-buffer))))

;; Org
(require 'evil-org)
(add-hook 'org-mode-hook #'evil-org-mode)
(evil-org-set-key-theme '(textobjects insert navigation additional shift todo heading))
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
         "* TODO %?
  %u
  %a")
        ("bt" "[${name}] Note" entry
         (file+headline "${root}/TODOs.org" "Notes")
         "* %?
  %t")))

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
  ; (assert (stringp repo-dir))
  ; (assert (stringp ref))
  ; (assert (functionp cb))
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
(set-face-attribute
 'diff-context nil
 :foreground "#586e75")

;; LaTex
(add-hook 'latex-mode-hook #'make-standard-paragraph-rules)

;; Anzu
(global-anzu-mode)
(setq anzu-cons-mode-line-p 'nil)
(with-eval-after-load 'evil (require 'evil-anzu))

;; Ivy
(ivy-mode 1)
(counsel-mode 1)
(setq ivy-use-virtual-buffers t
      ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
(setcdr (assoc 'counsel-M-x ivy-initial-inputs-alist) "")

;; Line numbers
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(setq-default display-line-numbers-type 'relative)
(global-hl-line-mode +1)
(defun toggle-global-hl-line ()
  "Toggle function `global-hl-line-mode'."
  (interactive)
  (global-hl-line-mode (if global-hl-line-mode -1 1)))

(defun next-line-number (curr)
  "Get the next line number after `CURR'."
  (pcase curr
    ('absolute 'relative)
    ('relative 'nil)
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

;; Keybindings
(define-key comint-mode-map (kbd "C-c C-k" ) #'comint-clear-buffer)
(define-key comint-mode-map (kbd "C-d") nil)

;; Vinegar
(define-key evil-normal-state-map "-" (defun dired-dot () (interactive) (dired ".")))
(define-key dired-mode-map "-" #'dired-up-directory)

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
;; Slightly broken, but hey
(add-hook
 'debbugs-gnu-mode-hook
 (defun make-debbugs-gnu-ctrl-c-map ()
   (local-set-key (kbd "C-c") debbugs-gnu-mode-map)))

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
(add-hook 'emacs-lisp-mode-hook
          (defun setup-elisp-imenu ()
            (setq-local
             imenu-generic-expression
             (cons '("Keymap" "^(define-prefix-keymap\\s-+\\([a-z-]+\\)" 1)
              imenu-generic-expression))))

;; Elm mode
(require 'flycheck-elm)
(require 'elm-mode)
(setq elm-format-on-save 't
      elm-format-elm-version "0.18"
      elm-package-catalog-root "http://package.elm-lang.org/")
(add-hook 'flycheck-mode-hook #'flycheck-elm-setup)
(with-eval-after-load 'company-mode (add-to-list 'company-backends 'company-elm))
;; Can't get only elm 18 packages without this hack
(defun elm-package-refresh-contents ()
  "Refresh the package list."
  (interactive)
  (elm--assert-dependency-file)
  (let* ((all-packages (elm-package--build-uri "all-packages?elm-package-version=0.18")))
    (with-current-buffer (url-retrieve-synchronously all-packages)
      (goto-char (point-min))
      (re-search-forward "^ *$")
      (setq elm-package--marked-contents nil)
      (setq elm-package--contents (append (json-read) nil)))))

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

;; Mercury
(add-to-list 'load-path "~/.emacs.d/private/metal-mercury-mode/")
(require 'metal-mercury-mode)

;; Ocaml
(let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share")))))
  (when (and opam-share (file-directory-p opam-share))
    ;; Register Merlin
    (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
    (autoload 'merlin-mode "merlin" nil t nil)
    ;; Automatically start it in OCaml buffers
    (add-hook 'tuareg-mode-hook 'merlin-mode t)
    (add-hook 'caml-mode-hook 'merlin-mode t)
    ;; Use opam switch to lookup ocamlmerlin binary
    (setq merlin-command 'opam)))

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
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)
(require 'rust-mode)
(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
(with-eval-after-load 'rust-mode
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))
(setq
 rust-format-on-save t
 rust-imenu-generic-expression
 (cons
  '("Async Fn" "^[[:space:]]*\\(?:\\<pub\\>[[:space:]]+\\)?\\(?:\\<default\\>[[:space:]]+\\)?\\(?:\\<unsafe\\>[[:space:]]+\\)?\\(?:\\<extern\\>[[:space:]]+\\(?:\"[^\"]+\"[[:space:]]+\\)?\\)?\\<async\\>[[:space:]]+\\<fn\\>[[:space:]]+\\([[:word:][:multibyte:]_][[:word:][:multibyte:]_[:digit:]]*\\)" 1)
  rust-imenu-generic-expression))

;; SQL
(setq
 sql-product 'postgres
 sql-connection-alist
 '((scratch (sql-product 'postgres)
           (sql-port 5432)
           (sql-server "localhost")
           (sql-database "impression")
           (sql-user "john")))
 sql-postgres-login-params
 '((user :default "john")
   (server :default "localhost")
   (port :default 5432))
 sql-postgres-options
 '("-P" "pager=off" "--tuples-only" "--no-align"))

(with-eval-after-load 'sql
  (progn
    (sql-set-product-feature
     'postgres :prompt-regexp "^.* λ ")
    (define-key sql-mode-map (kbd "C-c C-i") #'sql-connect)
    (define-key sql-mode-map (kbd "C-c C-k")
      (defun clear-sql-buffer ()
        (interactive)
        (with-current-buffer sql-buffer (comint-clear-buffer))))))


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

;; Theme
(require 'solarized)
(require 'solarized-dark-theme)
(load-theme 'solarized-dark t)

;; Transparency in terminal
(defun my-make-frame-transparent (frame)
  "Make `FRAME' transparent'."
  (if (or (not (display-graphic-p frame))
	  (string= 'base (daemonp))
          (string= 'term (daemonp)))
      (progn (set-face-background 'default "unspecified-bg" frame)
             (set-face-background 'line-number "#073642" frame))))

(defun my-make-this-frame-transparent ()
  "Make `selected-frame' transparent."
  (interactive)
  (my-make-frame-transparent (selected-frame)))

(my-make-frame-transparent (selected-frame))
(add-hook 'after-make-frame-functions #'my-make-frame-transparent)

(defun on-after-init ()
  "From https://stackoverflow.com/questions/19054228/emacs-disable-theme-background-color-in-terminal# ."
  (unless (display-graphic-p (selected-frame))
    (progn (set-face-background 'default "unspecified-bg" (selected-frame))
           (set-face-background 'line-number "#073642" (selected-frame)))))

(add-hook 'window-setup-hook #'on-after-init)

(if (or (string= 'base (daemonp))
        (string= 'term (daemonp))
        (not (display-graphic-p (selected-frame))))

    (progn (set-face-background 'default "unspecified-bg" (selected-frame))
           (set-face-background 'line-number "#073642" (selected-frame))))

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
            (if (string-equal "-" (projectile-project-name))
                (format "%s" evil-state)
              (projectile-project-name))
            'face `(:foreground ,(evil-state-foreground evil-state) :weight bold)))
    "  %b "
    (:eval vc-mode)
    "  "
    (:eval (if (and (featurep 'flycheck) flycheck-mode)
               (my-flycheck-mode-line-status-text)
             ""))
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
(require 'seq)
(defmacro define-prefix-keymap (name &optional docstring &rest bindings)
  "Define a keymap named `NAME' and docstring `DOCSTRING' with many `BINDINGS' at once using `define-key'."
  `(,#'progn
     (defvar ,name ,docstring (make-sparse-keymap))
     (define-prefix-command (quote ,name))
     ,@(seq-reduce
        (lambda (bindings key-fn)
          `((define-key (quote ,name) ,(car key-fn)
              (function
               ,(pcase (cadr key-fn)
                  ((pred symbolp) (cadr key-fn))
                  ((pred (lambda (fn) (symbolp (eval fn)))) (eval (cadr key-fn)))
                  (_ (cadr key-fn)))))
            ,@bindings))
        (seq-partition bindings 2)
        `(,name))))

(evil-leader/set-leader "<SPC>")

(evil-leader/set-key
  "<SPC>" 'counsel-M-x
  "TAB"'evil-switch-to-windows-last-buffer
  "a" 'my-process-map
  "b" 'my-buffer-map
  "c" 'my-compile-map
  "C" 'my-counsel-map
  "d" 'my-directory-map
  "e" 'my-error-map
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

(define-key help-map (kbd "D") my-describe-map)
(define-key help-map (kbd "w") #'woman)
(define-key help-map (kbd "W") #'man)
(define-key help-map (kbd "i") #'counsel-info-lookup-symbol)
(define-key help-map (kbd "I") #'info-apropos)

(define-prefix-keymap my-directory-map
  "my directory commands"
  "d" dired
  "/" (defun dired-dot-other-window ()
        (interactive) (dired-other-window ".")))

(define-prefix-keymap elfeed-load-map
  "Various ways of loading feeds"
  "o" elfeed-load-opml
  (kbd "C-o") elfeed-load-opml)

(define-prefix-keymap my-process-map
  "my process keybindings"
  "b" my-debbugs-modes-map
  "d" docker
  "e" gnus
  "f" elfeed
  "g" guix
  "i" my-erc-map
  "l" list-processes
  "o" org-agenda
  "p" proced)

(define-prefix-keymap my-debbugs-modes-map
  "my debbugs modes."
  "o" debbugs-org
  "b" debbugs-gnu)

(define-prefix-keymap my-erc-map
  "my erc keybindings"
  "f" my-erc-freenode
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
        (switch-to-buffer-other-window (get-buffer-create "*Messages*")))
  "r" (defun switch-to-run-buffer ()
        (interactive)
        (my-switch-to-compile-buffer "run"))
  "R" revert-buffer
  "s" (defun switch-to-scratch-buffer ()
        (interactive)
        (switch-to-buffer-other-window (get-buffer-create "*Scratch*")))
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
  "f" describe-function
  "F" counsel-describe-face
  "k" describe-key
  "m" describe-mode
  "s" describe-symbol
  "t" describe-theme
  "v" describe-variable)

(define-prefix-keymap my-error-map
  "my flycheck keybindings"
  "b" flycheck-buffer
  "e" flycheck-mode
  "n" flycheck-next-error
  "l" flycheck-list-errors
  "p" flycheck-previous-error)

(define-prefix-keymap my-file-map
  "my file keybindings"
  "f" counsel-find-file
  "l" find-file-literally
  "r" counsel-buffer-or-recentf
  "s" save-buffer
  "y" (defun kill-file-name
          () (interactive) (kill-new (buffer-file-name (current-buffer)))))

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
  "t" (defun insert-iso-8601-string ()
        (interactive) (insert (iso-8601-string)))
  "u" counsel-unicode-char)

(define-prefix-keymap my-jump-map
  "my jump keybindings"
  "c" avy-goto-char
  "i" counsel-imenu
  "j" avy-goto-char-2
  "l" avy-goto-line
  "o" counsel-org-goto-all
  "t" evil-jump-to-tag
  "=" indent-region-or-buffer)

(define-prefix-keymap my-org-map
  "my org bindings"
  "a" counsel-projectile-org-agenda
  "c" counsel-projectile-org-capture
  "g" counsel-org-goto
  "i" counsel-org-entity
  "t" counsel-org-tag)

(defun switch-project-workspace ()
  "Switch to a known projectile project in a new workspace."
  (interactive)
  (let ((projectile-switch-project-action #'projectile-find-file))
    (projectile-switch-project)))

(define-prefix-keymap my-projectile-map
  "my projectile keybindings"
  "a" counsel-projectile-org-agenda
  "b" counsel-projectile-switch-to-buffer
  "c" (defun projectile-compile ()
        (interactive) (my-projectile-command "compile"))
  "C" counsel-projectile-org-capture
  "d" counsel-projectile-find-dir
  "D" (defun switch-to-projectile-project-root
          () (interactive) (dired (projectile-project-root)))
  "e" projectile-edit-dir-locals
  "f" counsel-projectile-find-file
  "I" projectile-invalidate-cache
  "l" switch-project-workspace
  "o" (defun switch-to-projectile-todos ()
        (interactive)
        (find-file (format "%sTODOs.org" (projectile-project-root))))
  "p" counsel-projectile-switch-project
  "r" (defun projectile-run ()
        (interactive) (my-projectile-command "run"))
  "R" my-projectile-reload-dir-locals
  "t" (defun projectile-test ()
        (interactive) (my-projectile-command "test"))
  "'" projectile-run-eshell
  "]" projectile-find-tag)

(define-prefix-keymap my-quit-map
  "my quit keybindings"
  "q" save-buffers-kill-terminal)

(define-prefix-keymap my-search-map
  "my searching keybindings"
  "s" swiper)

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
  "c" (defun toggle-fill-column ()
        (interactive) (fci-mode (if (bound-and-true-p fci-mode) -1 1)))
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
  "t" counsel-load-theme
  "w" whitespace-mode
  "x" toggle-xclip-mode)

(define-prefix-keymap my-org-toggle-map
  "org specific toggles"
  "l" org-toggle-link-display)

(define-prefix-keymap my-window-map
  "my window keybindings"
  "/" (defun my-vsplit ()
        (interactive)
        (progn (split-window-horizontally) (balance-windows-area)))
  "-" (defun my-split ()
        (interactive)
        (progn (split-window-vertically) (balance-windows-area)))
  "'" (defun pop-to-eshell ()
        (interactive)
        (my-side-eshell '((side . right) (slot . 1))) (balance-windows-area))
  "c" make-frame
  "d" (defun my-delete-window ()
        (interactive) (progn (delete-window) (balance-windows-area)))
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
  "=" balance-windows-area)

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

;;; init.el ends here
