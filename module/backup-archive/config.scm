(define-module (backup-archive config)
  #:use-module (backup-archive program)
  #:use-module (backup-archive common)
  #:use-module (backup-archive profile)
  #:export     (config-get-profile))


(define config-imports
  (map resolve-module '((backup-archive profile))))

(define (config-get-config)
  "Read configuration file.  Return a module object."
  (let ((config (make-module)))
    (for-each (lambda (module) (module-use! config module))
              config-imports)
    (save-module-excursion
     (lambda ()
       (set-current-module config)
       (primitive-load (config-file))))
    config))

(define (config-get-profile profile)
  "Return profile."
  (let* ((config (config-get-config))
         (value (false-if-exception (module-ref config profile))))
    (if (profile? value)
        value
        (error "no such profile:" profile))))
