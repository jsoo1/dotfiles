(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
  (custom-set-variables
   ;; custom-set-variables was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(evil-want-Y-yank-to-eol nil)
   '(helm-dash-docsets-path "~/.local/Zeal/docsets")
   '(package-selected-packages
     (quote
      (helm-themes helm-swoop helm-purpose helm-projectile helm-mode-manager helm-flx helm-descbinds helm-ag ace-jump-helm-line yapfify yaml-mode xterm-color ws-butler wolfram-mode winum which-key wgrep web-mode web-beautify volatile-highlights vimrc-mode vi-tilde-fringe vala-snippets vala-mode uuidgen use-package toml-mode toc-org thrift tagedit systemd symon string-inflection stan-mode sql-indent spaceline smex smeargle slime-company slim-mode slack shell-pop scss-mode scad-mode sayid sass-mode restclient-helm restart-emacs ranger rainbow-delimiters racer qml-mode pyvenv pytest pyenv-mode py-isort pug-mode psci psc-ide popwin pkgbuild-mode pip-requirements persp-mode pcre2el password-generator paradox ox-reveal orgit org-projectile org-present org-pomodoro org-download org-bullets org-brain open-junk-file ob-restclient ob-http nix-mode nginx-mode neotree multi-term mu4e-maildirs-extension mu4e-alert move-text mmm-mode matlab-mode markdown-toc magit-gitflow lorem-ipsum logcat livid-mode live-py-mode linum-relative link-hint less-css-mode kivy-mode julia-mode js2-refactor js-doc ivy-purpose ivy-hydra intero insert-shebang info+ indent-guide impatient-mode idris-mode ibuffer-projectile hy-mode hungry-delete hoon-mode hlint-refactor hl-todo hindent highlight-parentheses highlight-numbers highlight-indentation hide-comnt help-fns+ helm-pydoc helm-nixos-options helm-make helm-hoogle helm-gtags helm-gitignore helm-css-scss helm-company helm-c-yasnippet haskell-snippets google-translate golden-ratio gnuplot gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ gh-md ggtags fuzzy fsharp-mode flyspell-correct-ivy flycheck-rust flycheck-pos-tip flycheck-haskell flycheck-elm flycheck-bashate flx-ido fish-mode fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-snipe evil-search-highlight-persist evil-org evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-lion evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-cleverparens evil-args evil-anzu eshell-z eshell-prompt-extras esh-help erlang erc-yt erc-view-log erc-social-graph erc-image erc-hl-nicks emoji-cheat-sheet-plus emmet-mode elm-mode elisp-slime-nav editorconfig ebuild-mode dumb-jump dockerfile-mode docker diff-hl define-word dante dactyl-mode cython-mode csv-mode counsel-projectile company-web company-tern company-statistics company-shell company-restclient company-nixos-options company-ghci company-ghc company-emoji company-cabal company-auctex company-anaconda common-lisp-snippets column-enforce-mode coffee-mode cmm-mode clojure-snippets clj-refactor clean-aindent-mode cider-eval-sexp-fu cargo browse-at-remote auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile arduino-mode aggressive-indent adaptive-wrap ace-window ace-link ac-ispell)))
   '(psc-ide-add-import-on-completion t t)
   '(psc-ide-rebuild-on-save nil t)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil))))
 '(spaceline-highlight-face ((t (:inherit (quote mode-line) :foreground "#3E3D31" :background "DarkGoldenrod2"))))
 '(spaceline-modified ((t (:inherit (quote mode-line) :foreground "#3E3D31" :background "SkyBlue2"))))
 '(spaceline-read-only ((t (:inherit (quote mode-line) :foreground "#3E3D31" :background "plum3"))))
 '(spacemacs-emacs-face ((t (:inherit (quote mode-line) :foreground "#3E3D31" :background "SkyBlue2"))))
 '(spacemacs-insert-face ((t (:inherit (quote mode-line) :foreground "#3E3D31" :background "chartreuse3"))))
 '(spacemacs-motion-face ((t (:inherit (quote mode-line) :foreground "#3E3D31" :background "plum3"))))
 '(spacemacs-normal-face ((t (:inherit (quote mode-line) :foreground "#3E3D31" :background "DarkGoldenrod2"))))
 '(spacemacs-replace-face ((t (:inherit (quote mode-line) :foreground "#3E3D31" :background "chocolate"))))
 '(spacemacs-visual-face ((t (:inherit (quote mode-line) :foreground "#3E3D31" :background "gray")))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-want-Y-yank-to-eol nil)
 '(helm-dash-docsets-path "/home/john/.local/share/Zeal/Zeal/docsets/")
 '(package-selected-packages
   (quote
    (helm-themes helm-swoop helm-purpose helm-projectile helm-mode-manager helm-flx helm-descbinds helm-ag ace-jump-helm-line yapfify yaml-mode xterm-color ws-butler wolfram-mode winum which-key wgrep web-mode web-beautify volatile-highlights vimrc-mode vi-tilde-fringe vala-snippets vala-mode uuidgen use-package toml-mode toc-org thrift tagedit systemd symon string-inflection stan-mode sql-indent spaceline smex smeargle slime-company slim-mode slack shell-pop scss-mode scad-mode sayid sass-mode restclient-helm restart-emacs ranger rainbow-delimiters racer qml-mode pyvenv pytest pyenv-mode py-isort pug-mode psci psc-ide popwin pkgbuild-mode pip-requirements persp-mode pcre2el password-generator paradox ox-reveal orgit org-projectile org-present org-pomodoro org-download org-bullets org-brain open-junk-file ob-restclient ob-http nix-mode nginx-mode neotree multi-term mu4e-maildirs-extension mu4e-alert move-text mmm-mode matlab-mode markdown-toc magit-gitflow lorem-ipsum logcat livid-mode live-py-mode linum-relative link-hint less-css-mode kivy-mode julia-mode js2-refactor js-doc ivy-purpose ivy-hydra intero insert-shebang info+ indent-guide impatient-mode idris-mode ibuffer-projectile hy-mode hungry-delete hoon-mode hlint-refactor hl-todo hindent highlight-parentheses highlight-numbers highlight-indentation hide-comnt help-fns+ helm-pydoc helm-nixos-options helm-make helm-hoogle helm-gtags helm-gitignore helm-css-scss helm-company helm-c-yasnippet haskell-snippets google-translate golden-ratio gnuplot gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ gh-md ggtags fuzzy fsharp-mode flyspell-correct-ivy flycheck-rust flycheck-pos-tip flycheck-haskell flycheck-elm flycheck-bashate flx-ido fish-mode fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-snipe evil-search-highlight-persist evil-org evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-lion evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-cleverparens evil-args evil-anzu eshell-z eshell-prompt-extras esh-help erlang erc-yt erc-view-log erc-social-graph erc-image erc-hl-nicks emoji-cheat-sheet-plus emmet-mode elm-mode elisp-slime-nav editorconfig ebuild-mode dumb-jump dockerfile-mode docker diff-hl define-word dante dactyl-mode cython-mode csv-mode counsel-projectile company-web company-tern company-statistics company-shell company-restclient company-nixos-options company-ghci company-ghc company-emoji company-cabal company-auctex company-anaconda common-lisp-snippets column-enforce-mode coffee-mode cmm-mode clojure-snippets clj-refactor clean-aindent-mode cider-eval-sexp-fu cargo browse-at-remote auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile arduino-mode aggressive-indent adaptive-wrap ace-window ace-link ac-ispell)))
 '(psc-ide-add-import-on-completion t t)
 '(psc-ide-rebuild-on-save nil t))
