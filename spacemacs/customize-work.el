(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-want-Y-yank-to-eol nil)
 '(safe-local-variable-values
   (quote
    ((eval progn
           (require
            (quote projectile))
           (puthash
            (projectile-project-root)
            "elm-make App.elm --output app.js" projectile-compilation-cmd-map)
           (setq projectile-project-run-cmd "node main.js"))
     (eval progn
           (require
            (quote projectile))
           (puthash
            (projectile-project-root)
            "npm run watch" projectile-compilation-cmd-map)
           (setq projectile-project-run-cmd "npm start"))
     (javascript-backend . tern)
     (javascript-backend . lsp)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
