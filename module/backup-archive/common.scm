(define-module (backup-archive common)
  #:export     (all?))

(define (all? pred lst)
  (cond
   ((null? lst)
    #t)
   ((pred (car lst))
    (all? pred (cdr lst)))
   (else
    #f)))
