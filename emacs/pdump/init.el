;;; package --- Summary
;;; My not minimal-ish anymore init.el
;;; Commentary:
;;; use like any ol init.el
;;; Code:

;; Package
(require 'package)
(package-refresh-contents t)
(package-initialize)

;; Path
(setq exec-path '("~/.local/.bin"
                  "/run/setuid-programs"
                  "~/.config/guix/current/bin"
                  "~/.guix-profile/bin"
                  "~/.guix-profile/sbin"
                  "/run/current-system/profile/bin"
                  "/run/current-system/profile/sbin"
                  "~/dotfiles/emacs/"))

(exec-path-from-shell-initialize)

;; Byte compile
(require 'bytecomp)
(setq byte-compile-warnings t)
(setq byte-compile-error-on-warn nil)

;; Imenu List
(my-package-install 'imenu-list)
(require 'imenu-list)
(setq imenu-list-size 0.2)

;; Winner
(winner-mode t)

;; Evil
(my-package-install 'evil-leader)
(require 'evil-leader)
(my-package-install 'evil-escape)
(require 'evil-escape)
(add-to-list 'load-path "~/.emacs.d/private/evil-tmux-navigator")
(require 'navigate)

(evil-escape-mode)
(setq-default evil-escape-key-sequence "df")
(setq-default evil-escape-unordered-key-sequence 't)
(global-evil-leader-mode)

;; Idris mode
(add-to-listq load-path "~/.emacs.d/private/idris-mode")

(byte-compile-file "~/.emacs.d/private/idris-mode/idris-mode.el")
(byte-compile-file "~/.emacs.d/private/idris-mode/idris-ipkg-mode.el")
(byte-compile-file "~/.emacs.d/private/idris-mode/inferior-idris.el")

(require 'idris-mode)
(require 'inferior-idris)
(require 'idris-ipkg-mode)
(setq idris-interpreter-path "/home/john/.guix-profile/bin/idris")

(dolist (f '((idris-active-term-face        "#657b83")
             (idris-semantic-type-face      "#b58900")
             (idris-semantic-data-face      "#dc322f")
             (idris-semantic-function-face  "#859900")
             (idris-semantic-bound-face     "#6c71c4")))
  (set-face-foreground (car f) (cadr f)))

(define-key idris-repl-mode-map (kbd "C-c C-k" ) #'idris-repl-clear-buffer)
(define-key idris-mode-map (kbd "C-c C-k") #'idris-repl-clear-buffer)

;; Elm mode
(my-package-install 'flycheck-elm)
(require 'flycheck-elm)
(add-to-list 'load-path "~/.emacs.d/private/elm-mode")
(require 'elm-mode)
(setq elm-format-on-save 't
      elm-format-elm-version "0.18"
      elm-package-catalog-root "http://package.elm-lang.org/")
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-elm-setup))
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

;; Proof General
(my-package-install 'proof-general)

;; Coq
(add-hook
 'coq-mode-hook
 (lambda ()
   (set-face-attribute
    'proof-locked-face nil
    :underline nil
    :background "#073642")))

(my-package-install 'company-coq)
(add-hook 'coq-mode-hook #'company-coq-mode)
(setq proof-three-window-mode-policy 'hybrid
      proof-script-fly-past-comments t
      proof-splash-seen t
      company-coq-disabled-features '(hello))

;; Haskell mode
(my-package-install 'haskell-snippets)
(require 'haskell-snippets)

;; Mercury
(add-to-list 'load-path "~/.emacs.d/private/metal-mercury-mode/")
(require 'metal-mercury-mode)

;; Ocaml
(my-package-install 'merlin)
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
(my-package-install 'psc-ide)
(require 'psc-ide)
(add-hook 'purescript-mode-hook
          (lambda ()
            (psc-ide-mode)
            (company-mode)
            (flycheck-mode)
            (turn-on-purescript-indentation)))
(define-key purescript-mode-map (kbd "C-c C-s") 'psc-ide-server-start)
(define-key purescript-mode-map (kbd "C-c C-q") 'psc-ide-server-quit)
(add-hook
 'purescript-mode-hook
 (lambda ()
   (setq-local company-backends
              (append '((company-math-symbols-latex company-latex-commands))
                      company-backends))))

;; Rust
(my-package-install 'racer)
(my-package-install 'flycheck-rust)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)
(require 'rust-mode)
(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
(setq
 racer-rust-src-path "~/.guix-profile/lib/rustlib/src/rust/src"
 rust-format-on-save t)
(with-eval-after-load 'rust-mode
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;; SQL
(my-package-install 'sql)
(setq
 sql-product 'postgres
 sql-connection-alist
 '((vetpro (sql-product 'postgres)
           (sql-port 5432)
           (sql-server "localhost")
           (sql-user "postgres")
           (sql-database "vetpro"))
   (logs (sql-product 'postgres)
            (sql-port 5432)
            (sql-server "localhost")
            (sql-user "postgres")
            (sql-database "countySchemaMigrator")))
 sql-postgres-login-params
 '((user :default "postgres")
   (database :default "vetpro")
   (server :default "localhost")
   (port :default 5432))
 sql-postgres-options
 '("-P" "pager=off" "--tuples-only" "--no-align"))

(with-eval-after-load 'sql
  (progn
    (sql-set-product-feature
     'postgres :prompt-regexp "^.* λ ")
    (define-key sql-mode-map (kbd "C-c C-i") #'sql-connect)
    (define-key sql-mode-map (kbd "C-c C-k") #'(lambda () (interactive)
                                                 (with-current-buffer sql-buffer (comint-clear-buffer))))))

;; Math/TeX
(add-to-list 'load-path "~/.emacs.d/private/company-math")
(require 'company-math)

;; Dhall
(my-package-install 'dhall-mode)
(add-to-list 'auto-mode-alist '("\\.dhall\\'" . dhall-mode))

;; ELF
(my-package-install 'elf-mode)
(add-to-list 'auto-mode-alist '("\\.\\(?:a\\|so\\)\\'" . elf-mode))
;;; init.el ends here
(put 'proof-frob-locked-end 'disabled nil)
