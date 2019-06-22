(define-module (backup-archive find)
  #:use-module (backup-archive shell)
  #:use-module (backup-archive common)
  #:export     (find-files))


(define find-invalid-primary-operands
  '(print print0 printf fprint fprint0 fprintf ls fls exec ok newer))

(define (find-primary-operands pred)
  "List of primary operands in PRED."
  (cond ((null? pred)
         pred)
        ((not (pair? pred))
         (list pred))
        ((eq? (car pred) ':not)
         (find-primary-operands (cadr pred)))
        ((or (eq? (car pred) ':and)
             (eq? (car pred) ':or))
         (apply append (map find-primary-operands (cdr pred))))
        (else
         (find-primary-operands (car pred)))))

(define (find-test-args user-pred prev)
  "Return a list of command-line arguments."
  (define (iter pred)
    (cond
     ((null? pred)
      pred)
     ((not (pair? pred))
      (list (format #f "-~a" pred)))
     ((eq? (car pred) ':not)
      `("!" "(" ,@(iter (cadr pred)) ")"))
     ((or (eq? (car pred) ':or)
          (eq? (car pred) ':and))
      (apply append
             (add-infix (if (eq? (car pred) ':or)
                            '("-o")
                            '("-a"))
                        (map (lambda (x) `("(" ,@(iter x) ")"))
                             (cdr pred)))))
     (else
      (cons (car (iter (car pred)))
            (map (lambda (x) (format #f "~a" x))
                 (cdr pred))))))
  (let ((invalid-primaries (filter (lambda (x)
                                     (memq x find-invalid-primary-operands))
                                   (find-primary-operands user-pred))))
    (when (not (null? invalid-primaries))
          (error "invalid primary operand: ~a" (car invalid-primaries))))
  (iter (filter identity (list ':and
                               (and (not (null? user-pred)) user-pred)
                               (and prev (list 'newer prev))
                               'print0))))

(define (find-files path pred prev)
  "Return a string of NULL-separated filenames and the exit status of find."
  (apply shell-command* "find" (append path (find-test-args pred prev))))

