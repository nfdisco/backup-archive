(define-module (backup-archive program)
  #:use-module (backup-archive message)
  #:use-module (backup-archive common)
  #:export     (program-name backup-archive-version config-file))

(define program-name (make-parameter "backup-archive"))

(define backup-archive-version "0.3")

(define config-file
  (make-parameter
   (path-join (or (getenv "XDG_CONFIG_HOME")
                  (path-join (or (getenv "HOME")
                                 (error "variable HOME not set"))
                             ".config"))
              (program-name)
              "config.scm")))
