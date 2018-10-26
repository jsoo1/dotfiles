;;; package --- Summary
;;; My minimal-ish init.el
;;; Commentary:
;;; use like any ol init.el
;;; Code:

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
;; Built in GUI elements
(setq ring-bell-function 'ignore
      truncate-lines 't
      initial-scratch-message ""
      vc-follow-symlinks 't)
(add-to-listq
 default-frame-alist '(ns-transparent-titlebar . t)
 default-frame-alist '(font . "FantasqueSansMono Nerd Font Mono 16"))

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

;; Mouse
(xterm-mouse-mode 1)
(unless window-system
  (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
  (global-set-key (kbd "<mouse-5>") 'scroll-up-line))

;; Font
(set-face-attribute 'default t :font "FantasqueSansMono Nerd Font Mono 16")

;; Custom
(setq custom-file "/dev/null"
      initial-buffer-choice "~/dotfiles/emacs/init.el")

;; Package
(require 'package)
(add-to-list 'load-path "~/.emacs.d/private/evil-tmux-navigator")
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
(package-refresh-contents)

;; Path
(setq exec-path '("~/.local/.bin"
                  "/run/setuid-programs"
                  "~/.config/guix/current/bin"
                  "~/.guix-profile/bin"
                  "~/.guix-profile/sbin"
                  "/run/current-system/profile/bin"
                  "/run/current-system/profile/sbin"))

(package-install 'exec-path-from-shell)
(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)

;; Shell
(package-install 'multi-term)
(setq shell-file-name "bash")

;; Byte compile
(require 'bytecomp)
(setq byte-compile-warnings t)
(setq byte-compile-error-on-warn nil)

;; Backups, Lockfiles, etc.
(setq backup-directory-alist         `((".*" . "~/.emacs.d/private/backups"))
      auto-save-file-name-transforms `((".*" "~/.emacs.d/private/auto-saves" 't))
      create-lockfiles               nil)

;; Evil
(setq evil-want-C-u-scroll t
      evil-disable-insert-state-bindings t) ; somehow needs to happen before any mention of evil mode
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
(package-install 'which-key)
(require 'which-key)
(which-key-mode)
(setq which-key-idle-delay 0.1)

;; Clipboard
(pcase system-type
  ('gnu/linux (progn (package-install 'xclip)
                     (xclip-mode 1)))
  ('darwin (progn (package-install 'osx-clipboard)
                  (osx-clipboard-mode +1))))

;; Avy
(package-install 'avy)

;; OSX Clipboard
(package-install 'osx-clipboard)
(osx-clipboard-mode +1)

;; Keybindings
(define-key comint-mode-map "C-c C-k" #'comint-clear-buffer)
(define-key evil-normal-state-map "-" #'(lambda () (interactive) (dired ".")))
(define-key dired-mode-map "-" #'dired-up-directory)

(evil-leader/set-leader "<SPC>")

(evil-leader/set-key
  "<SPC>" 'counsel-M-x
  "TAB"'evil-switch-to-windows-last-buffer
  "b" 'my-buffer-map
  "c" 'my-compile-map
  "d" 'dired
  "e" 'my-error-map
  "f" 'my-file-map
  "g" 'my-git-map
  "h" 'my-describe-map
  "j" 'my-jump-map
  "p" 'my-projectile-map
  "q" 'my-quit-map
  "s" 'my-search-map
  "t" 'my-toggle-map
  "w" 'my-window-map
  "x" 'my-text-map
  "y" 'my-yank-map
  "z" 'my-zoom-map
  "'" 'multi-term
  "/" 'counsel-projectile-grep)

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

(define-prefix-keymap my-error-map
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

(define-prefix-keymap my-jump-map
  "my jump keybindings"
  "j" avy-goto-char
  "l" avy-goto-line
  "=" indent-region-or-buffer)

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

(define-prefix-keymap my-text-map
  "my text keybindings"
  "d" delete-trailing-whitespace)

(define-prefix-keymap my-toggle-map
  "my toggles"
  "d" toggle-debug-on-error
  "D" toggle-degub-on-quit
  "f" toggle-frame-fullscreen
  "l" toggle-truncate-lines
  "t" counsel-load-theme
  "w" whitespace-mode)

(define-prefix-keymap my-window-map
  "my window keybindings"
  (kbd "TAB") eyebrowse-last-window-config
  "/" split-window-horizontally
  "-" split-window-vertically
  "d" delete-window
  "h" (lambda nil () (interactive) (tmux-navigate "left"))
  "j" (lambda nil () (interactive) (tmux-navigate "down"))
  "k" (lambda nil () (interactive) (tmux-navigate "up"))
  "l" (lambda nil () (interactive) (tmux-navigate "right"))
  "m" delete-other-windows
  "r" eyebrowse-rename-window-config
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


;; All the icons
(package-install 'all-the-icons)
(require 'all-the-icons)
(let ((window-system 'mac)) (all-the-icons-install-fonts 't))

;; Spaceline
(package-install 'spaceline)
(package-install 'spaceline-all-the-icons)
(require 'spaceline-all-the-icons)
(require 'spaceline-config)
(if (or (string= 'term (daemonp))
        (not (display-graphic-p (selected-frame))))
    (progn (setq powerline-default-separator 'utf-8)
           (spaceline-spacemacs-theme))
  (progn (setq powerline-default-separator 'arrow)
         (spaceline-all-the-icons-theme)))

(dolist (s '((solarized-evil-normal "#859900" "Evil normal state face.")
             (solarized-evil-insert "#b58900" "Evil insert state face.")
             (solarized-evil-emacs "#2aa198" "Evil emacs state face.")
             (solarized-evil-replace "#dc322f" "Evil replace state face.")
             (solarized-evil-visual "#268bd2" "Evil visual state face.")
             (solarized-evil-motion "#586e75" "Evil motion state face.")
             (solarized-unmodified "#586e75" "Unmodified buffer face.")
             (solarized-modified "#2aa198" "Modified buffer face.")
             (solarized-read-only "#586e75" "Read-only buffer face.")))
  (eval `(defface ,(nth 0 s) `((t (:background ,(nth 1 s) :foreground "#002b36" :inherit 'mode-line))) ,(nth 2 s) :group 'spaceline)))

(defvar solarized-evil-state-faces
  '((normal . solarized-evil-normal)
    (insert . solarized-evil-insert)
    (emacs . solarized-evil-emacs)
    (replace . solarized-evil-replace)
    (visual . solarized-evil-visual)
    (motion . solarized-evil-motion))
  "Association list mapping evil states to their corresponding highlight faces.
Is used by `solarized-highlight-face-func'.")

(defun solarized-highlight-face ()
  "Set the highlight face depending on the evil state.
Set `spaceline-highlight-face-func' to
`solarized-highlight-face' to use this."
  (if (bound-and-true-p evil-local-mode)
      (let* ((state (if (eq 'operator evil-state) evil-previous-state evil-state))
             (face (assq state solarized-evil-state-faces)))
        (if face (cdr face) (spaceline-highlight-face-default)))

    (spaceline-highlight-face-default)))

(setq powerline-image-apple-rgb t
      powerline-text-scale-factor 1.1
      spaceline-highlight-face-func #'solarized-highlight-face)

(spaceline-toggle-minor-modes-off)
(spaceline-toggle-projectile-root-on)

;; Theme
(package-install 'solarized-theme)
(require 'solarized-theme)

(setq
 custom-safe-themes
 '("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879"
   "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4"
   default))
(setq solarized-high-contrast-mode-line t)
(setq x-underline-at-descent-line t)
(if (daemonp)
    (load-theme 'solarized-dark)
  (load-theme 'solarized-light))

;; Transparency in terminal
(defun on-frame-open (frame)
  "Make `FRAME' transparent'."
  (if (or (not (display-graphic-p frame))
	  (string= 'base (daemonp))
          (string= 'term (daemonp)))
      (progn (set-face-background 'default "unspecified-bg" frame)
             (set-face-background 'line-number "#073642" frame))))
(on-frame-open (selected-frame))
(add-hook 'after-make-frame-functions 'on-frame-open)
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
           (set-face-background 'line-number "#-73642" (selected-frame))))

;; Eyebrowse
(package-install 'eyebrowse)
(setq eyebrowse-keymap-prefix "")
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

(dolist (f '((idris-active-term-face        "#657b83")
             (idris-semantic-type-face      "#b58900")
             (idris-semantic-data-face      "#dc322f")
             (idris-semantic-function-face  "#859900")
             (idris-semantic-bound-face     "#6c71c4")))
  (set-face-foreground (car f) (cadr f)))

(define-key idris-repl-mode-map (kbd "C-c C-k" ) #'idris-repl-clear-buffer)
(define-key idris-mode-map (kbd "C-c C-k") #'idris-repl-clear-buffer)

;; Emacs Lisp Mode
(with-eval-after-load 'company #'(lambda () (add-hook 'emacs-lisp-mode-hook #'company-mode 't)))

;; Elm mode
(package-install 'flycheck-elm)
(require 'flycheck-elm)
(add-to-list 'load-path "~/.emacs.d/layers/+lang/elm/local/elm-mode")
(package-install 'f)
(package-install 'dash)
(package-install 's)
(package-install 'let-alist)
(require 'elm-mode)
(setq elm-format-on-save 't)
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-elm-setup))
(with-eval-after-load 'company-mode (add-to-list 'company-backends 'company-elm))

;; Fish mode
(package-install 'fish-mode)

;; JavaScript
(setq js-indent-level 2)

;; Proof General
(package-install 'proof-general)

;; Haskell mode
(package-install 'haskell-mode)
(require 'haskell-interactive-mode)
;; See https://github.com/haskell/haskell-mode/issues/1553#issuecomment-358373643
(setq haskell-process-args-ghci
      '("-ferror-spans" "-fshow-loaded-modules"))
(setq haskell-process-args-cabal-repl
      '("--ghc-options=-ferror-spans -fshow-loaded-modules"))
(setq haskell-process-args-stack-ghci
      '("--ghci-options=-ferror-spans -fshow-loaded-modules"
        "--no-build" "--no-load"))
(setq haskell-process-args-cabal-new-repl
      '("--ghc-options=-ferror-spans --ghc-options=-fshow-loaded-modules"))
(require 'haskell-process)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(setq haskell-process-type 'auto)
(package-install 'dante)
(add-hook 'haskell-mode-hook #'flycheck-mode)
(add-hook 'dante-mode-hook
          #'(lambda () (flycheck-add-next-checker 'haskell-dante) '(warning . haskell-hlint)))

;; Agda mode
(load-library (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda-mode locate")))

;; Guix
(package-install 'geiser)
(add-hook 'scheme-mode-hook #'geiser-mode)

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

;; Shellcheck
(add-hook 'sh-mode-hook #'flycheck-mode)

;;; init.el ends here
