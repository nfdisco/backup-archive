(define-module (backup-archive cli)
  #:use-module (backup-archive program)
  #:use-module (backup-archive message)
  #:use-module (backup-archive backup)
  #:use-module (ice-9 getopt-long)
  #:export     (main))

(define (display-help)
  "Print help message."
  (format #t "\
Usage: ~a [OPTIONS] [BACKUP-ID]
Backup data using find and tar.

Options:
     -h, --help     show this help and exit
         --version  show version and exit
" (program-name)))

(define (display-version)
  "Print version string."
  (format #t "~a version ~a\n" (program-name) backup-archive-version))

(define option-spec
  '((help    (single-char #\h) (value #f))
    (version                   (value #f))))

(define (%main args)
  "Process command-line arguments."
  (let* ((options      (getopt-long args option-spec))
         (non-opt-args (option-ref options '() '())))
    (when (option-ref options 'help #f)
          (display-help)
          (exit 0))
    (when (option-ref options 'version #f)
          (display-version)
          (exit 0))
    (when (null? non-opt-args)
          (msg-error "non-option argument required")
          (exit 1))
    (when (not (null? (cdr non-opt-args)))
          (msg-error "too many non-option arguments")
          (exit 1))
    (make-backup (car non-opt-args))))

(define (main args)
  (parameterize ((program-name (basename (car args))))
                (%main args)))

