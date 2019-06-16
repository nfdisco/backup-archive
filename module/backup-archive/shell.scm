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

(define (shell-pipe command . rest)
  "Run shell commands in a pipe.  Return the exit status of the last
command."
  ((fold (lambda (cmd thunk)
          (lambda ()
            (let ((pipe (apply open-pipe* OPEN_WRITE cmd)))
              (with-output-to-port pipe thunk)
              (close-pipe pipe))))
        (lambda () (apply shell-command command))
        rest)))



