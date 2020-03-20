;; init.scm -- default shepherd configuration file.

;; Services known to shepherd:
;; Add new services (defined using 'make <service>') to shepherd here by
;; providing them as arguments to 'register-services'.
(define emacs-term
  (make <service>
    #:provides '(emacs-term)
    #:docstring "Emacs terminal deamon."
    #:respawn? #f
    #:start (make-forkexec-constructor
             '("/home/john/.guix-profile/bin/emacs" "--fg-daemon=term")
             #:user "john"
             #:log-file "/home/john/var/log/emacs-term.log")
    #:stop (make-kill-destructor)
    #:actions (make-actions)))

(register-services emacs-term)

(define dunst
  (make <service>
    #:provides '(dunst)
    #:docstring "Dunst notification service"
    #:respawn #t
    #:start (make-forkexec-constructor
             `("/home/john/.guix-profile/bin/dunst")
             #:user "john"
             #:log-file "/home/john/var/log/dunst.log")
    #:stop (make-kill-destructor)
    #:actions (make-actions)))

(register-services dunst)

;; Send shepherd into the background
(action 'shepherd 'daemonize)

;; Services to start when shepherd starts:
;; Add the name of each service that should be started to the list
;; below passed to 'for-each'.
(for-each start '(dunst emacs-term))
