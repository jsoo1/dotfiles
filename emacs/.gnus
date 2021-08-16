;;; package --- Summary
;;; Commentary:
;;; gnus configuration
;;; Code:

(setq user-mail-address "jsoo1@asu.edu"
      user-full-name "John Soo"
      mml-secure-openpgp-sign-with-sender t)

(with-eval-after-load 'gnus
  (setq
   gnus-select-method '(nnnil nil)
   mml-secure-openpgp-sign-with-sender t
   message-send-mail-function #'message-smtpmail-send-it
   send-mail-function #'smtpmail-send-it
   smtpmail-smtp-server "smtp.gmail.com"
   smtpmail-stream-type  nil
   smtpmail-smtp-service 587
   gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]"
   gnus-secondary-select-methods
   '((nnimap "asu"
             ;; it could also be imap.googlemail.com if that's your server.
             (nnimap-address "imap.gmail.com")
             (nnimap-server-port "imaps")
             (nnimap-stream ssl)
             (nnimap-search-engime imap)
             ;; Move expired messages to Gmail's trash.
             (nnmail-expiry-target "nnimap+gmail:[Gmail]/Trash")
             ;; Mails marked as expired can be processed immediately.
             (nnmail-expiry-wait immediate))
     (nnimap "consumable"
             (nnimap-address "imap.gmail.com")
             (nnimap-port "imaps")
             (nnimap-stream ssl)
             (nnimap-search-engime imap)
             (nnimap-expiry-target "nnimap+gmail:[Gmail]/Trash")
             (nnimap-expiry-wait immediate)))
   gnus-posting-styles
   '((".*" ; Matches all groups of messages
      (address "John Soo <jsoo1@asu.edu>"))
     ("consumable" ; Matches Gnus group called "work"
      (address "John Soo <john@consumable.com>")
      (signature "
Software Engineer
Consumable")
      (organization "Consumable")
      ("X-Message-SMTP-Method" "smtp smtp.gmail.com 587 john@consumable.com")))))

;;; .gnus ends here
