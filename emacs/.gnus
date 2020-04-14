;;; package --- Summary
;;; Commentary:
;;; gnus configuration
;;; Code:

(setq user-mail-address "jsoo1@asu.edu"
      user-full-name "John Soo")

(with-eval-after-load 'gnus
  (progn
   (setq
    gnus-select-method
    '(nnimap "gmail"
             ;; it could also be imap.googlemail.com if that's your server.
             (nnimap-address "imap.gmail.com")
             (nnimap-server-port "imaps")
             (nnimap-stream ssl))
    mml-secure-openpgp-sign-with-sender t
    smtpmail-smtp-server "smtp.gmail.com"
    smtpmail-smtp-service 587
    gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")

   (add-to-list
   'gnus-secondary-select-methods
   '(nnimap "gmail"
            ;; it could also be imap.googlemail.com if that's your server.
            (nnimap-address "imap.gmail.com")
            (nnimap-server-port "imaps")
            (nnimap-stream ssl)
            ;; Move expired messages to Gmail's trash.
            (nnmail-expiry-target "nnimap+gmail:[Gmail]/Trash")
            ;; Mails marked as expired can be processed immediately.
            (nnmail-expiry-wait immediate)))))

;;; .gnus ends here
