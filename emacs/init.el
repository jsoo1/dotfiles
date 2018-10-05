;;; package --- Summary
;;; My minimal-ish init.el
;;; Commentary:
;;; use like any ol init.el
;;; Code:

(require 'seq)
(defmacro add-to-listq (&rest xs)
  "Add `XS' to `LIST'."
  (cons #'progn
	(seq-reduce (lambda (expr list-val-pair) (cons `(add-to-list (quote ,(car list-val-pair)) ,(cadr list-val-pair)) expr))
		    (seq-partition xs 2)
		    nil)))

(defmacro ->> (&rest body)
  "Thrush combinator for `BODY'."
  (let ((result (pop body)))
    (dolist (form body result)
      (setq result (append form (list result))))))

;; Built in GUI elements
(toggle-frame-fullscreen)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)
(setq ring-bell-function 'ignore)
(setq truncate-lines 't)
(setq initial-scratch-message "")
(add-to-listq
 default-frame-alist '(ns-transparent-titlebar . t)
 default-frame-alist '(font . "FantasqueSansMono Nerd Font Mono 16"))
(setq vc-follow-symlinks 't)
(defalias 'yes-or-no-p 'y-or-n-p)

;; Cursor
(setq cursor-type 'box)
(blink-cursor-mode 0)

;; Font
(set-face-attribute 'default t :font "FantasqueSansMono Nerd Font Mono 16")

;; Custom
(setq custom-file "/dev/null"
      initial-buffer-choice "~/.emacs.d/private/idris-testing-setup.el")

;; Package
(require 'package)
(add-to-list 'load-path "~/.emacs.d/private/evil-tmux-navigator")
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
(package-refresh-contents)

;; Path
(setq exec-path '("/Users/john/.fzf/bin"
		  "/usr/local/bin"
		  "/usr/bin"
		  "/bin"
		  "/usr/sbin"
		  "/sbin"
		  "/usr/local/anaconda3/bin"
		  "/usr/local/sbin"
		  "/Users/john/.cargo/bin"
		  "/Users/john/.local/bin"
		  "/Users/john/Library/Python/3.6/bin"
		  "/usr/local/bin"
		  "/usr/bin"
		  "/bin"
		  "/usr/sbin"
		  "/sbin"
		  "/usr/local/anaconda3/bin"
		  "/usr/local/sbin"
		  "/Users/john/.cargo/bin"
		  "/Users/john/.local/bin"
		  "/Users/john/Library/Python/3.6/bin"
		  "/usr/local/bin"
		  "/usr/bin"
		  "/bin"
		  "/usr/sbin"
		  "/sbin"
		  "/opt/X11/bin"
		  "/Library/TeX/texbin"
		  "/usr/local/anaconda3/bin"
		  "/usr/local/sbin"
		  "/usr/local/bin"
		  "/usr/sbin"
		  "/usr/bin"
		  "/sbin"
		  "/bin"
		  "/Users/john/.cargo/bin"
		  "/Users/john/.local/bin"
		  "/Users/john/Library/Python/3.6/bin"))

(package-install 'exec-path-from-shell)
(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)

;; Shell
(package-install 'multi-term)
(setq shell-file-name "/bin/bash")

;; Package
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(require 'bytecomp)
(setq byte-compile-warnings t)
(setq byte-compile-error-on-warn nil)

;; Backups
(setq backup-directory-alist `(("~/.emacs.d/private/backups")))

;; Evil
(setq evil-want-C-u-scroll t) ; somehow needs to happen before any mention of evil mode
(package-install 'evil)
(require 'evil)
(package-install 'evil-surround)
(require 'evil-surround)
(package-install 'evil-commentary)
(require 'evil-commentary)
(package-install 'evil-leader)
(require 'evil-leader)
(package-install 'evil-escape)
(require 'evil-escape)
(package-install 'smartparens)
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

;; Magit
(package-install 'magit)
(package-install 'evil-magit)
(require 'evil-magit)
(setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)

;; Projectile
(package-install 'projectile)
(package-install 'ibuffer-projectile)
(projectile-mode +1)
(setq projectile-completion-system 'ivy)
(add-hook 'ibuffer-hook
	  (lambda ()
	    (ibuffer-projectile-set-filter-groups)
	    (unless (eq ibuffer-sorting-mode 'alphabetic)
              (ibuffer-do-sort-by-alphabetic))))

;; Ivy
(package-install 'ivy)
(package-install 'counsel)
(package-install 'swiper)
(package-install 'counsel-projectile)
(package-install 'wgrep)
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)

;; Line numbers
(global-display-line-numbers-mode 1)
(setq-default display-line-numbers-type 'relative)
(global-hl-line-mode +1)

;; Which key
(require 'which-key)
(which-key-mode)
(setq which-key-idle-delay 0.1)

;; Keybindings
(defmacro define-prefix-keymap (name &optional docstring &rest bindings)
  "Define a keymap named `NAME' and docstring `DOCSTRING' with many `BINDINGS' at once using `define-key'."
  (cons #'progn
	(cons (if docstring
		  `(defvar ,name ,docstring (make-sparse-keymap))
		`(defvar ,name (make-sparse-keymap)))
	      (cons `(define-prefix-command (quote ,name))
		    (seq-reduce (lambda (bindings key-fn)
				  (cons `(define-key (quote ,name) ,(car key-fn) (function ,(cadr key-fn)))
					bindings))
				(seq-partition bindings 2)
				`(,name))))))

(evil-leader/set-leader "<SPC>")

(evil-leader/set-key
  "<SPC>" 'counsel-M-x
  "<tab>" 'evil-switch-to-windows-last-buffer
  "b" 'my-buffer-map
  "c" 'my-compile-map
  "d" 'dired
  "e" 'my-flycheck-map
  "f" 'my-file-map
  "g" 'my-git-map
  "h" 'my-describe-map
  "p" 'my-projectile-map
  "q" 'my-quit-map
  "s" 'my-search-map
  "t" 'my-toggle-map
  "w" 'my-window-map
  "y" 'my-yank-map
  "z" 'my-zoom-map
  "'" 'multi-term
  "/" 'counsel-projectile-rg)

(define-prefix-keymap my-buffer-map
  "my buffer keybindings"
  "b" ivy-switch-buffer
  "c" (lambda () (interactive) (pop-to-buffer (get-buffer-create "*compilation*")))
  "d" (lambda () (interactive) (kill-buffer (current-buffer)))
  "i" ibuffer
  "m" (lambda () (interactive) (switch-to-buffer (get-buffer-create "*Messages*")))
  "s" (lambda () (interactive) (switch-to-buffer (get-buffer-create "*Scratch*"))))

(define-prefix-keymap my-compile-map
  "my keybindings for compiling"
  "b" (lambda () (interactive) (pop-to-buffer (get-buffer-create "*compilation*"))))

(define-prefix-keymap my-describe-map
  "my describe keybindings"
  "b" describe-bindings
  "f" describe-function
  "k" describe-key
  "m" describe-mode
  "v" describe-variable)

(define-prefix-keymap my-flycheck-map
  "my flycheck keybindings"
  "n" flycheck-next-error
  "l" flycheck-list-errors
  "p" flycheck-previous-error)

(define-prefix-keymap my-file-map
  "my file keybindings"
  "f" counsel-find-file
  "r" counsel-recentf
  "s" save-buffer)

(define-prefix-keymap my-git-map
  "my git keybindings"
  "b" magit-blame
  "s" magit-status
  "l" magit-log-buffer-file)

(defun switch-project-workspace ()
  "Switch to a known projectile project in a new workspace."
  (interactive)
  (let ((eyebrowse-new-workspace
	 #'(lambda ()
	     (->> (projectile-project-name)
		  (eyebrowse-rename-window-config (eyebrowse--get 'current-slot)))))
	(projectile-switch-project-action
	 #'(lambda ()
	     (eyebrowse-create-window-config)
	     (projectile-find-file))))
    (projectile-switch-project)))

(define-prefix-keymap my-projectile-map
  "my projectile keybindings"
  "b" counsel-projectile-switch-to-buffer
  "c" projectile-compile-project
  "d" counsel-projectile-find-dir
  "D" (lambda () (interactive) (dired (projectile-project-root)))
  "f" counsel-projectile-find-file
  "l" switch-project-workspace
  "o" (lambda () (interactive) (find-file (format "%sTODOs.org" (projectile-project-root))))
  "p" counsel-projectile-switch-project
  "r" projectile-run-project
  "t" projectile-test-project
  "'" (lambda () (interactive) (projectile-with-default-dir (projectile-project-root) (multi-term))))

(define-prefix-keymap my-quit-map
  "my quit keybindings"
  "q" save-buffers-kill-terminal)

(define-prefix-keymap my-search-map
  "my searching keybindings"
  "s" swiper)

(define-prefix-keymap my-toggle-map
  "my toggles"
  "d" toggle-debug-on-error
  "D" toggle-degub-on-quit
  "l" toggle-truncate-lines
  "t" counsel-load-theme)

(define-prefix-keymap my-window-map
  "my window keybindings"
  (kbd "<tab>") eyebrowse-last-window-config
  "/" split-window-horizontally
  "-" split-window-vertically
  "d" delete-window
  "m" delete-other-windows
  "w" eyebrowse-switch-to-window-config
  "=" balance-windows-area)

(define-prefix-keymap my-yank-map
  "my yanking keybindings"
  "y" counsel-yank-pop)

(define-prefix-keymap my-zoom-map
  "my zoom/text scaling keybindings"
  "+" text-scale-increase
  "-" text-scale-decrease)

;; Compilation and shell ansi colors
(package-install 'xterm-color)
(require 'xterm-color)
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


;; Theme
(package-install 'solarized-theme)
(require 'solarized-theme)
(package-install 'which-key)

(setq
 custom-safe-themes
 '("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879"
   "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4"
   default))
(setq solarized-high-contrast-mode-line t)
(setq x-underline-at-descent-line t)
(load-theme 'solarized-light)

;; All the icons
(package-install 'all-the-icons)
(require 'all-the-icons)
(all-the-icons-install-fonts)

;; Spaceline
(package-install 'spaceline)
(package-install 'spaceline-all-the-icons)
(require 'spaceline-all-the-icons)
(require 'spaceline-config)

(spaceline-all-the-icons-theme)
(setq powerline-image-apple-rgb t)
(spaceline-toggle-minor-modes-off)
(spaceline-toggle-projectile-root-on)
(setq powerline-text-scale-factor 1.1)

;; Theme
(package-install 'solarized-theme)
(require 'solarized-theme)
(package-install 'which-key)

(setq
 custom-safe-themes
 '("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879"
   "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4"
   default))
(setq solarized-high-contrast-mode-line t)
(setq x-underline-at-descent-line t)
(load-theme 'solarized-light)

;; Eyebrowse
(package-install 'eyebrowse)
(eyebrowse-mode 1)

;; Flycheck
(package-install 'flycheck)
(require 'flycheck)
(global-flycheck-mode)

;; Company
(package-install 'company)
(add-hook 'after-init-hook 'global-company-mode)

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

;; Idris mode
(add-to-listq load-path "~/.emacs.d/layers/+lang/idris/local/idris-mode")

(byte-compile-file "~/.emacs.d/layers/+lang/idris/local/idris-mode/idris-mode.el")
(byte-compile-file "~/.emacs.d/layers/+lang/idris/local/idris-mode/idris-ipkg-mode.el")
(byte-compile-file "~/.emacs.d/layers/+lang/idris/local/idris-mode/inferior-idris.el")

(require 'idris-mode)
(require 'inferior-idris)
(require 'idris-ipkg-mode)
(setq idris-interpreter-path "/usr/local/bin/idris")

;; Elm mode
(package-install 'flycheck-elm)
(require 'flycheck-elm)
(add-to-list 'load-path "~/.emacs.d/layers/+lang/elm/local/elm-mode")
(require 'elm-mode)
(setq elm-format-on-save 't)
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-elm-setup))
(with-eval-after-load 'company-mode (add-to-list 'company-backends 'company-elm))

;; JavaScript
(setq js-indent-level 2)

;; Proof General
(package-install 'proof-general)

;; Haskell mode
(package-install 'haskell-mode)
(package-install 'intero)
(intero-global-mode 1)

;; Agda mode
(load-library (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda-mode locate")))

;; SQL
(package-install 'sql)
(setq sql-postgres-login-params
      '((user :default "postgres")
        (database :default "vetpro")
        (server :default "localhost")
        (port :default 5432)))

;; YAML
(package-install 'yaml-mode)
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

;;; init.el ends here
