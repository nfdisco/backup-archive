(define-module (backup-archive cli)
  #:use-module (backup-archive program)
  #:use-module (backup-archive message)
  #:use-module (backup-archive profile)
  #:use-module (ice-9 getopt-long)
  #:export     (main))

(define (display-help)
  "Print help message."
  (format #t "\
Usage: ~a [OPTIONS]
Backup data with find and tar.

Options:
     -h, --help     show this help and exit
         --version  show version and exit
" (program-name)))

(define (display-version)
  "Print version string."
  (format #t "~a version ~a\n" (program-name) archive-backup-version))

(define option-spec
  '((help    (single-char #\h) (value #f))
    (version                   (value #f))))

(define (%main args)
  "Process command-line arguments."
  (let ((options (getopt-long args option-spec)))
    (cond ((option-ref options 'help #f)
           (display-help))
          ((option-ref options 'version #f)
           (display-version))
          (else
           (format #t "~s\n"
                   (read-profiles (car (option-ref options '() #f))))))))

(define (main args)
  (parameterize ((program-name (basename (car args))))
                (%main args)))

