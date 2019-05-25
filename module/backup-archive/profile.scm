(define-module (backup-archive profile)
  #:use-module (backup-archive common)
  #:export     (read-profiles
                profile-id
                profile-dest
                profile-orig
                profile-pred
                profile-fmt))


(define (profile-error msg . args)
  "Signal a profile-error."
  (scm-error 'profile-error #f msg args #f))

(define (profile-id prof)
  (list-ref prof 0))

(define (profile-dest prof)
  (list-ref prof 1))

(define (profile-orig prof)
  (list-ref prof 2))

(define (profile-pred prof)
  (list-ref prof 3))

(define (profile-fmt prof)
  (list-ref prof 4))

(define (make-profile id dest orig pred fmt)
  "Return a profile object."
  (cond
   ((not (symbol? id))
    (profile-error "not a symbol: ~s" id))
   ((not (string? dest))
    (profile-error "not a string: ~s" dest))
   ((or (not (list? orig))
        (null? orig)
        (not (all? string? orig)))
    (profile-error "not a list of strings: ~s" orig))
   ((not (symbol? fmt))
    (profile-error "not a symbol: ~s" fmt))
   (else
    (list id dest orig pred fmt))))

(define (read-profiles filename)
  "Read profiles from FILENAME."
  (catch #t
         (lambda ()
           (with-input-from-file filename
             (lambda ()
               (define (iter prof-lst)
                 (let ((expr (read)))
                   (cond
                    ((eof-object? expr)
                     prof-lst)
                    ((not (list? expr))
                     (profile-error "not a list: ~s" expr))
                    ((< (length expr) 5)
                     (profile-error "invalid profile: too few elements"))
                    ((> (length expr) 5)
                     (profile-error "invalid profile: too many elements"))
                    (else
                     (iter (cons (apply make-profile expr) prof-lst))))))
               (iter '()))))
         (lambda (key . rest)
           (case key
             ((read-error)
              (profile-error (cadr rest)))
             ((system-error)
              (profile-error "~s: ~a"
                             filename
                             (strerror (car (cadddr rest)))))
             (else
              (apply throw key rest))))))

