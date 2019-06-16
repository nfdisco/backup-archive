(define-module (backup-archive common)
  #:use-module (rnrs io ports)
  #:export     (all? path-join add-infix ask-yes-no-question?))

(define (all? pred lst)
  (cond
   ((null? lst)
    #t)
   ((pred (car lst))
    (all? pred (cdr lst)))
   (else
    #f)))

(define (path-join arg . rest)
  (if (null? rest)
      arg
      (string-join (map (lambda (s)
                          (string-trim-right s file-name-separator?))
                        (cons arg rest))
                   file-name-separator-string)))

(define (add-infix infix lst)
  (if (or (null? lst)
          (null? (cdr lst)))
      lst
      (append (list (car lst) infix)
              (add-infix infix (cdr lst)))))

(define (ask-yes-no-question? prompt)
  (define (iter)
    (display prompt)
    (let ((answer (string-downcase (get-line (current-input-port)))))
      (cond ((string=? answer "yes")
             #t)
            ((string=? answer "no")
             #f)
            (else
             (display "please, answer \"yes\" or \"no\"")
             (newline)
             (iter)))))
  (iter))
