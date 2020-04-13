;;; package --- init.el
;;; Commentary:
;;; Code:

;; Helpers
(require 'seq)
(defmacro add-to-listq (&rest xs)
  "Add `XS' to `LIST'."
  (cons #'progn
        (seq-reduce (lambda (expr list-val-pair)
                      (cons `(add-to-list (quote ,(car list-val-pair)) ,(cadr list-val-pair)) expr))
                    (seq-partition xs 2)
                    nil)))

(defmacro ->> (&rest body)
  "Thrush combinator for `BODY'."
  (let ((result (pop body)))
    (dolist (form body result)
      (setq result (append form (list result))))))

(defmacro define-prefix-keymap (name &optional docstring &rest bindings)
  "Define a keymap named `NAME' and docstring `DOCSTRING' with many `BINDINGS' at once using `define-key'."
  (cons #'progn
        (cons (if docstring `(defvar ,name ,docstring (make-sparse-keymap))
                `(defvar ,name (make-sparse-keymap)))
              (cons `(define-prefix-command (quote ,name))
                    (seq-reduce (lambda (bindings key-fn)
                                  (cons `(define-key (quote ,name) ,(car key-fn) (function ,(cadr key-fn)))
                                        bindings))
                                (seq-partition bindings 2)
                                `(,name))))))

(defun my-package-install (package)
  "Install `PACKAGE' unless already installed."
  (unless (package-installed-p package)
    (package-install package)))

;; Built in GUI elements
(setq ring-bell-function 'ignore
      initial-scratch-message ""
      vc-follow-symlinks 't)
(setq-default truncate-lines 't)
(add-to-listq
 default-frame-alist '(ns-transparent-titlebar . t)
 default-frame-alist '(font . "Iosevka 18"))
(set-fontset-font "fontset-default" 'unicode "DejaVu Math Tex Gyre")

(defalias 'yes-or-no-p 'y-or-n-p)

(toggle-frame-fullscreen)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)

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

;; Package
;; (add-to-list 'load-path "~/.emacs.d/private/evil-tmux-navigator")
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

;; GC Threshold
(setq gc-cons-threshold 200000000)

;; Pinentry
(setf epa-pinentry-mode 'loopback)

;; GNUTLS issues
;; Skip v1.3 per https://debbugs.gnu.org/cgi/bugreport.cgi?bug=34341#19
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; Email
(setq user-mail-address "jsoo1@asu.edu"
      user-full-name "John Soo")

;; Erc
(setq erc-autojoin-channels-alist nil)
(setq erc-hide-list '("JOIN" "PART" "QUIT"))
(defun my-erc ()
  (interactive)
  (let ((erc-prompt-for-password nil))
    (erc-tls
     :server "irc.refl.club"
     :port 5555
     :nick "jsoo")))

;; Shell
(setq shell-file-name "bash")

;; EShell
(add-hook 'emacs-startup-hook
          (lambda ()
            (when (not (display-graphic-p)) (cd default-directory))
            (eshell)))
(setq initial-buffer-choice (lambda () (get-buffer-create "*eshell*"))
      eshell-highlight-prompt nil
      eshell-prompt-function
      (lambda ()
        (concat
         (propertize (eshell/whoami) 'face `(:foreground "#93a1a1"))
         " "
         (propertize (eshell/pwd) 'face `(:foreground "#268bd2"))
         " "
         (propertize (or (magit-get-current-branch) "") 'face `(:foreground "#859900"))
         " "
         (propertize "λ" 'face `(:foreground "#b58900" :weight normal))
         " "))
      eshell-prompt-regexp "^[^λ]* [λ] "
      eshell-banner-message "")

(defun my-side-eshell (props)
  "Pop Eshell in a buffer using window `PROPS'."
  (interactive)
  (with-current-buffer (get-buffer-create eshell-buffer-name)
    (display-buffer-in-side-window (current-buffer) props)
    (eshell-mode))
  (pop-to-buffer eshell-buffer-name))

;; Dired
(add-hook 'dired-mode-hook (lambda ()
                             (auto-revert-mode)
                             (dired-hide-details-mode)))

;; Backups, lockfiles, auto-saves, local variables
(setq
 backup-directory-alist `((".*" . "~/.emacs.d/private/backups/"))
 delete-old-versions nil
 create-lockfiles nil
 auto-save-file-name-transforms `((".*" "~/.emacs.d/private/auto-saves/" t))
 enable-local-eval t)

;; not packaged yet
;; (my-package-install 'imenu-list)
;; (require 'imenu-list)

;; Evil
(setq evil-want-C-u-scroll t
      evil-disable-insert-state-bindings t
      evil-want-abbrev-expand-on-insert-exit nil) ; somehow needs to happen before any mention of evil mode

;; not packaged yet
;; (my-package-install 'evil-commentary)
;; (require 'evil-commentary)
;; (my-package-install 'evil-leader)
;; (require 'evil-leader)
;; (my-package-install 'evil-escape)
;; (require 'evil-escape)

(evil-mode 1)
(global-evil-surround-mode 1)
(evil-commentary-mode)

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

;; Smartparens
(smartparens-global-mode 1)

;; not packaged yet
;; (require 'navigate)

;; Magit
(setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)

;; Projectile
(projectile-mode +1)
(setq projectile-completion-system 'ivy
      projectile-indexing-method 'hybrid
      projectile-enable-caching 't
      projectile-project-search-path "~/projects/")

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
  (switch-to-buffer (get-buffer-create (concat "*" (projectile-project-name) "-" kind "*"))))

;; Dir Locals -- see https://emacs.stackexchange.com/questions/13080/reloading-directory-local-variables
(defun my-projectile-reload-dir-locals ()
  "Reload each buffer with the same `default-directory` as the current buffer's."
  (interactive)
  (dolist (buffer (projectile-project-buffers))
    (with-current-buffer buffer
      (hack-dir-local-variables-non-file-buffer))))

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

(add-hook 'ibuffer-hook #'ibuffer-auto-mode)
(add-hook 'ibuffer-hook #'my-set-ibuffer-filter-groups)
(setq ibuffer-show-empty-filter-groups nil)

;; Org
(require 'org-tempo)
(org-babel-do-load-languages 'org-babel-load-languages
                             '((js . t)
                               (haskell . t)
                               (emacs-lisp . t)
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

(with-eval-after-load 'org-agenda-mode
  (progn
    (define-key org-agenda-mode-map (kbd "C-c") org-agenda-mode-map)
    (define-key org-agenda-mode-map (kbd "C-m") #'org-agenda-month-view)
    (define-key org-agenda-mode-map "m" #'org-agenda-month-view)))

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

;; LaTex
(add-hook
 'latex-mode-hook (lambda ()
                    (setq-local paragraph-separate "[ \t\f]*$"
                                paragraph-start "\f\\|[ \t]*$")))

;; Anzu
(global-anzu-mode)
(setq anzu-cons-mode-line-p 'nil)
(with-eval-after-load 'evil (require 'evil-anzu))

;; Ivy
(require 'ivy)
(require 'counsel)
(require 'swiper)
(require 'counsel-projectile)
(require 'wgrep)
(ivy-mode 1)
(counsel-mode 1)
(setq ivy-use-virtual-buffers t
      ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
(setcdr (assoc 'counsel-M-x ivy-initial-inputs-alist) "")

;; Line numbers
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(setq-default display-line-numbers-type 'relative)
(global-hl-line-mode +1)

(defun next-line-number (curr)
  "Get the next line number after `CURR'."
  (pcase curr
    ('absolute 'relative)
    ('relative 'nil)
    (_ 'absolute)))

;; Which key
(which-key-mode)
(setq which-key-idle-delay 0.1)

;; Clipboard
(pcase system-type
  ('darwin (progn (my-package-install 'osx-clipboard)
                  (osx-clipboard-mode +1))))

;; Compilation
(define-key compilation-mode-map (kbd "C-c C-l") #'recompile)

;; Comint
(define-key comint-mode-map (kbd "C-c C-k" ) #'comint-clear-buffer)
(define-key comint-mode-map (kbd "C-d") nil)

;; Vinegar
(define-key evil-normal-state-map "-" #'(lambda () (interactive) (dired ".")))
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

;; not packaged
;; (my-package-install 'osx-clipboard)
;; (require 'osx-clipboard)

;; Compilation and shell ansi colors
(setq compilation-environment '("TERM=xterm-256color"))
(add-hook 'compilation-start-hook
          (lambda (proc)
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
(setq debbugs-gnu-mode-map (make-sparse-keymap))
(define-key debbugs-gnu-mode-map (kbd "C-c") debbugs-gnu-mode-map)

;; Restclient
(add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode))

;; Emacs Lisp Mode
(with-eval-after-load 'company (add-hook 'emacs-lisp-mode-hook #'company-mode 't))

;; JavaScript
(add-hook
 'js-mode-hook
 (lambda nil
   (progn
     (define-key js-mode-map (kbd "C-c C-s") 'nodejs-repl)
     (define-key js-mode-map (kbd "C-c C-c") 'nodejs-repl-send-last-expression)
     (define-key js-mode-map (kbd "C-c C-j") 'nodejs-repl-send-line)
     (define-key js-mode-map (kbd "C-c C-r") 'nodejs-repl-send-region)
     (define-key js-mode-map (kbd "C-c C-l") 'nodejs-repl-load-file)
     (define-key js-mode-map (kbd "C-c C-k") (lambda () (interactive) (with-current-buffer "*nodejs*" (comint-clear-buffer))))
     (define-key js-mode-map (kbd "C-c C-z") 'nodejs-repl-switch-to-repl))))
(setq js-indent-level 4)

;; not packaged yet
;; (my-package-install 'flycheck-elm)
;; (require 'flycheck-elm)
;; (add-to-list 'load-path "~/.emacs.d/private/elm-mode")
;; (require 'elm-mode)
;; (my-package-install 'proof-general)
;; (require 'proof-general)
;; (my-package-install 'company-coq)
;; (require 'company-coq)

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
            ("Functions*" "^(define\\*\\s-+(?\\(\\sw+\\)" 1))
        scheme-imenu-generic-expression)
  "Imenu generic expression for Guile modes.  See `imenu-generic-expression'.")
(add-hook
 'scheme-mode-hook
 (lambda ()
   (setq-local imenu-generic-expression guile-imenu-generic-expression)))
(with-eval-after-load 'geiser-guile
       (add-to-list 'geiser-guile-load-path "~/projects/guix"))

;; Cedille
(require 'cedille-mode)
(define-key cedille-mode-map (kbd "C-c C-l") #'cedille-start-navigation)
(evil-define-key 'normal cedille-mode-map (kbd "C-c") (se-navi-get-keymap 'cedille-mode))
(add-hook
 'cedille-mode-hook
 (lambda ()
   (setq-local company-backends
              (append '((company-math-symbols-latex company-latex-commands))
                      company-backends))))

;; not packaged yet
;; (add-to-list 'load-path "~/.emacs.d/private/idris-mode")
;; (byte-compile-file "~/.emacs.d/private/idris-mode/idris-mode.el")
;; (byte-compile-file "~/.emacs.d/private/idris-mode/idris-ipkg-mode.el")
;; (byte-compile-file "~/.emacs.d/private/idris-mode/inferior-idris.el")
;; (require 'idris-mode)
;; (require 'inferior-idris)
;; (require 'idris-ipkg-mode)

;; Haskell mode
;; not packaged yet
;; (my-package-install 'haskell-snippets)
;; (require 'haskell-snippets)

;; See https://github.com/haskell/haskell-mode/issues/1553#issuecomment-358373643
(require 'haskell-interactive-mode)
(require 'haskell-process)
(setq haskell-process-args-ghci '("-ferror-spans")
      haskell-process-args-cabal-repl '("--ghc-options=-ferror-spans")
      haskell-process-args-stack-ghci
      '("--ghci-options=-ferror-spans" "--no-build" "--no-load")
      haskell-process-args-cabal-new-repl '("--ghc-options=-ferror-spans")
      haskell-process-type 'auto
      haskell-process-log 't
      haskell-interactive-popup-errors nil
      flycheck-haskell-hpack-preference 'prefer-cabal
      safe-local-variable-values
      (append
       '((haskell-stylish-on-save . t)
         (haskell-mode-stylish-haskell-path . "ormolu")
         (haskell-mode-stylish-haskell-args . ("--ghc-opt TypeApplications"))
         (haskell-process-type . cabal-new-repl))
       safe-local-variable-values)
      my-old-haskell-mode-hook haskell-mode-hook)

(add-hook 'haskell-mode-hook
          (lambda ()
            (setq-local paragraph-separate "[ \t\f]*$"
                        paragraph-start "\f\\|[ \t]*$")
            (interactive-haskell-mode)
            (yas-minor-mode-on)
            (flycheck-mode)
            (flycheck-disable-checker 'haskell-ghc)))

(define-key haskell-mode-map (kbd "C-c C-f") 'haskell-mode-stylish-buffer)

;; not packaged yet
;; (add-to-list 'load-path "/home/john/.emacs.d/private/metal-mercury-mode/")
;; (require 'metal-mercury-mode)
;; (add-to-list 'load-path "~/.emacs.d/private/company-math")
;; (require 'company-math)

;; not packaged yet
;; (my-package-install 'merlin)
;; (require 'merlin)
;; (let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share")))))
;;   (when (and opam-share (file-directory-p opam-share))
;;     ;; Register Merlin
;;     (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
;;     (autoload 'merlin-mode "merlin" nil t nil)
;;     ;; Automatically start it in OCaml buffers
;;     (add-hook 'tuareg-mode-hook 'merlin-mode t)
;;     (add-hook 'caml-mode-hook 'merlin-mode t)
;;     ;; Use opam switch to lookup ocamlmerlin binary
;;     (setq merlin-command 'opam)))
;; (add-to-list 'load-path "~/.emacs.d/private/purescript-mode")
;; (require 'purescript-mode-autoloads)
;; (add-to-list 'Info-default-directory-list "~/.emacs.d/private/purescript-mode/")
;; (my-package-install 'psc-ide)
;; (require 'psc-ide)

;; not packaged yet
;; (my-package-install 'racer)
;; (require 'racer)
;; (my-package-install 'flycheck-rust)
;; (require 'flycheck-rust)
;; (my-package-install 'sql)
;; (require 'sql)

;; Plist
(add-to-list 'auto-mode-alist '("\\.plist\\'" . xml-mode))

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
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

;; Agda
(load-library (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda-mode locate")))

;; Shellcheck
(add-hook 'sh-mode-hook #'flycheck-mode)

;; Vimrc
(add-to-list 'auto-mode-alist '("\\.vim\\(rc\\)?\\'" . vimrc-mode))

;; not packaged yet
;; (my-package-install 'dhall-mode)
;; (require 'dhall-mode)
;; (my-package-install 'elf-mode)
;; (require 'elf-mode)

;; Web mode
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
(setq emmet-move-cursor-between-quotes t)
(add-hook 'css-mode-hook  'emmet-mode)
(add-hook 'web-mode-hook 'emmet-mode)

;; Prolog
(add-to-list 'auto-mode-alist '("\\.pro\\'" . prolog-mode))

(require 'solarized)
(require 'solarized-dark-theme)

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
  (unless (or (display-graphic-p (selected-frame))
              (not (string= 'base (daemonp)))
              (not (string= 'term (daemonp))))
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

;; Keybindings
(evil-leader/set-leader "<SPC>")

(evil-leader/set-key
  "<SPC>" 'counsel-M-x
  "TAB"'evil-switch-to-windows-last-buffer
  "a" 'my-process-map
  "b" 'my-buffer-map
  "c" 'my-compile-map
  "C" 'my-counsel-map
  "d" 'dired
  "e" 'my-error-map
  "f" 'my-file-map
  "g" 'my-git-map
  "h" 'my-describe-map
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

(define-prefix-keymap my-process-map
  "my process keybindings"
  "d" docker
  "e" gnus
  "g" guix
  "i" my-erc
  "l" list-processes
  "o" org-agenda
  "p" proced)

(define-prefix-keymap my-buffer-map
  "my buffer keybindings"
  "b" ivy-switch-buffer
  "c" (lambda () (interactive) (my-switch-to-compile-buffer "compile"))
  "d" (lambda () (interactive) (kill-buffer (current-buffer)))
  "i" ibuffer
  "m" (lambda () (interactive) (switch-to-buffer (get-buffer-create "*Messages*")))
  "r" (lambda () (interactive) (my-switch-to-compile-buffer "run"))
  "R" revert-buffer
  "s" (lambda () (interactive) (switch-to-buffer (get-buffer-create "*Scratch*")))
  "t" (lambda () (interactive) (my-switch-to-compile-buffer "test")))

(define-prefix-keymap my-compile-map
  "my keybindings for compiling"
  "b" (lambda () (interactive) (pop-to-buffer (get-buffer-create "*compilation*")))
  "c" counsel-compile)

(define-prefix-keymap my-counsel-map
  "my keybindings to counsel"
  "b" counsel-switch-buffer
  "c" counsel-colors-emacs
  "d" counsel-dired
  "g" counsel-git
  "h" counsel-command-history
  "i" counsel-ibuffer
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
  "i" counsel-info-lookup-symbol
  "I" info-apropos
  "k" describe-key
  "m" describe-mode
  "s" describe-symbol
  "t" describe-theme
  "w" woman
  "v" describe-variable)

(define-prefix-keymap my-error-map
  "my flycheck keybindings"
  "b" flycheck-buffer
  "n" flycheck-next-error
  "l" flycheck-list-errors
  "p" flycheck-previous-error)

(define-prefix-keymap my-file-map
  "my file keybindings"
  "f" counsel-find-file
  "l" find-file-literally
  "r" counsel-buffer-or-recentf
  "s" save-buffer
  "y" (lambda () (interactive) (kill-new (buffer-file-name (current-buffer)))))

(define-prefix-keymap my-git-map
  "my git keybindings"
  "b" magit-blame
  "c" counsel-git-checkout
  "r" magit-refresh-all
  "s" magit-status
  "l" magit-log-buffer-file)

(define-prefix-keymap my-insert-map
  "my insertion keybindings"
  "c" insert-char
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
  "b" counsel-projectile
  "c" (lambda () (interactive) (my-projectile-command "compile"))
  "C" counsel-projectile-org-capture
  "d" counsel-projectile-find-dir
  "D" (lambda () (interactive) (dired (projectile-project-root)))
  "e" projectile-edit-dir-locals
  "f" counsel-projectile-find-file
  "I" projectile-invalidate-cache
  "l" switch-project-workspace
  "o" (lambda () (interactive) (find-file (format "%sTODOs.org" (projectile-project-root))))
  "p" counsel-projectile-switch-project
  "r" (lambda () (interactive) (my-projectile-command "run"))
  "R" my-projectile-reload-dir-locals
  "t" (lambda () (interactive) (my-projectile-command "test"))
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
  "d" delete-trailing-whitespace)

(define-prefix-keymap my-toggle-map
  "my toggles"
  "c" (lambda nil () (interactive) (fci-mode (if (bound-and-true-p fci-mode) -1 1)))
  "d" toggle-debug-on-error
  "D" toggle-debug-on-quit
  "f" toggle-frame-fullscreen
  "i" imenu-list-smart-toggle
  "l" toggle-truncate-lines
  "m" toggle-mode-line
  "n" (lambda nil () (interactive) (setq display-line-numbers (next-line-number display-line-numbers)))
  "t" counsel-load-theme
  "w" whitespace-mode)

(define-prefix-keymap my-window-map
  "my window keybindings"
  "/" (lambda nil () (interactive) (progn (split-window-horizontally) (balance-windows-area)))
  "-" (lambda nil () (interactive) (progn (split-window-vertically) (balance-windows-area)))
  "'" (lambda nil () (interactive) (my-side-eshell '((side . right) (slot . 1))) (balance-windows-area))
  "c" make-frame
  "d" (lambda nil () (interactive) (progn (delete-window) (balance-windows-area)))
  "D" delete-frame
  "h" (lambda nil () (interactive) (tmux-navigate "left"))
  "j" (lambda nil () (interactive) (tmux-navigate "down"))
  "k" (lambda nil () (interactive) (tmux-navigate "up"))
  "l" (lambda nil () (interactive) (tmux-navigate "right"))
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

;;; predump.el ends here