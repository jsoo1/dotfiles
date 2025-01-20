(define-module (dunst)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module ((guix records) #:select (match-record))

  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu packages wm)
  #:use-module (gnu services configuration)
  #:export (home-dunst-configuration
            home-dunst-shepherd-services
            home-dunst-service-type))

(define-configuration/no-serialization home-dunst-configuration
  (package (package dunst) "the dunst package to use"))

(define-public (home-dunst-shepherd-services config)
  (match-record config <home-dunst-configuration>
    (package)
    `(,(shepherd-service
        (provision '(dunst))
        (modules '((shepherd support)))
        (documentation "xorg notifications dispatcher")
        (respawn? #t)
        (start #~(make-forkexec-constructor
                  '(#$(file-append package "/bin/dunst"))
                  #:log-file (string-append %user-log-dir "/dunst.log")))
        (stop #~(make-kill-destructor))))))

(define-public home-dunst-service-type
  (service-type
   (name 'dunst)
   (extensions `(,(service-extension home-shepherd-service-type
                                     home-dunst-shepherd-services)
                 ,(service-extension home-profile-service-type
                                     (lambda (c) `(,(home-dunst-configuration-package c))))))
   (default-value (home-dunst-configuration))
   (description "dunst - an xorg notifications daemon")))
