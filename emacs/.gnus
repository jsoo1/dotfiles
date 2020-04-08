;;; package --- Summary
;;; Commentary:
;;; gnus configuration
;;; Code:

(setq user-mail-address "jsoo1@asu.edu"
      user-full-name "John Soo")

(with-eval-after-load 'gnus
  (setq gnus-select-method
        '(nnimap "gmail"
                 (nnimap-address "imap.gmail.com")  ; it could also be imap.googlemail.com if that's your server.
                 (nnimap-server-port "imaps")
                 (nnimap-stream ssl))))

(with-eval-after-load 'gnus
  (add-to-list
   'gnus-secondary-select-methods
   '(nnimap "gmail"
            (nnimap-address "imap.gmail.com")  ; it could also be imap.googlemail.com if that's your server.
            (nnimap-server-port "imaps")
            (nnimap-stream ssl)
            (nnmail-expiry-target "nnimap+gmail:[Gmail]/Trash")  ; Move expired messages to Gmail's trash.
            (nnmail-expiry-wait immediate)))) ; Mails marked as expired can be processed immediately.

(with-eval-after-load 'gnus
  (setq smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 587
        gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]"))

;;; .gnus ends here
