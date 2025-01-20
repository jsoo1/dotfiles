(define-module (emacs)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module ((guix records) #:select (match-record))

  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu packages emacs)
  #:use-module (gnu services configuration)
  #:export (home-emacs-configuration
            home-emacs-shepherd-services
            home-emacs-service-type))

(define-configuration/no-serialization home-emacs-configuration
  (package (package emacs) "the emacs package to use")
  (socket-name (string "sock") "the name of the daemon socket")
  (debug-init? (boolean #f) "debug init.el"))

(define (home-emacs-shepherd-services config)
  (match-record config <home-emacs-configuration>
    (package socket-name debug-init?)
    `(,(shepherd-service
        (provision '(emacs))
        (modules '((shepherd support)))
        (documentation "Emacs daemon")
        (start #~(make-forkexec-constructor
                  '(#$(file-append package "/bin/emacs")
                    #$(string-append "--fg-daemon=" socket-name)
                    #$@(if debug-init? '("--debug-init") '()))
                  #:log-file (string-append %user-log-dir "/emacs.log")))
        (stop #~(make-kill-destructor))))))

(define home-emacs-service-type
  (service-type
   (name 'emacs)
   (extensions `(,(service-extension home-shepherd-service-type
                                     home-emacs-shepherd-services)
                 ,(service-extension home-profile-service-type
                                     (lambda (c) `(,(home-emacs-configuration-package c))))))
   (default-value (home-emacs-configuration))
   (description "emacs - the elisp environment and image")))
