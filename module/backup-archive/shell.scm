(define-module (backup-archive shell)
  #:use-module (ice-9 popen)
  #:use-module (rnrs io ports)
  #:use-module (srfi srfi-1)
  #:export     (shell-command shell-command* shell-pipe))

(define (shell-command prog . args)
  "Run a shell command and return its exit status."
  (apply system* prog args))

(define (shell-command* prog . args)
  "Run a shell command and return a list containing the command's output and
its exit status"
  (let* ((pipe (apply open-pipe* OPEN_READ prog args)))
    (list (get-string-all pipe) (close-pipe pipe))))

(define (make-pipe lst)
  "Return a procedure of one argument which is either a procedure that
writes to the standard output port or #f."
  (lambda (thunk)
    (if thunk
        (let ((pipe (apply open-pipe* OPEN_WRITE lst)))
          (with-output-to-port pipe thunk)
          (close-pipe pipe))
        (apply shell-command lst))))

(define (cons-pipe head tail)
  "Combine a series of pipes."
  (fold (lambda (elt prev)
          (lambda (thunk)
            (elt (lambda () (prev thunk)))))
        head
        tail))

(define (shell-pipe command . rest)
  "Return a pipe."
  (cons-pipe (make-pipe command)
             (map make-pipe rest)))


