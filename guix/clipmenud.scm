(define-module (clipmenud)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module ((guix records) #:select (match-record))

  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu services configuration)
  #:export (home-clipmenud-configuration
            home-clipmenud-shepherd-services
            home-clipmenud-service-type))

(define-configuration/no-serialization home-clipmenud-configuration
  (package (package clipmenu) "the clipmenud package to use"))

(define-public (home-clipmenud-shepherd-services config)
  (match-record config <home-clipmenud-configuration>
    (package)
    `(,(shepherd-service
        (provision '(clipmenud))
        (modules '((shepherd support)))
        (documentation "Clipboard manager daemon")
        (respawn? #f)
        (start #~(make-forkexec-constructor
                  '(#$(file-append package "/bin/clipmenud"))
                  #:log-file (string-append %user-log-dir "/clipmenud.log")))
        (stop #~(make-kill-destructor))))))

(define-public home-clipmenud-service-type
  (service-type
   (name 'clipmenud)
   (extensions `(,(service-extension home-shepherd-service-type
                                     home-clipmenud-shepherd-services)
                 ,(service-extension home-profile-service-type
                                     (lambda (c)
                                       `(,(home-clipmenud-configuration-package c))))))
   (default-value (home-clipmenud-configuration))
   (description "clipmenud - a clipboard history daemon")))
