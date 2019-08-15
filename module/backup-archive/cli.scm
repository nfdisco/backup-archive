(define-module (backup-archive cli)
  #:use-module (backup-archive program)
  #:use-module (backup-archive message)
  #:use-module (backup-archive find)
  #:use-module (backup-archive config)
  #:use-module (backup-archive archive)
  #:use-module (backup-archive profile)
  #:use-module (backup-archive common)
  #:use-module (ice-9 getopt-long)
  #:export     (main))

(define (display-help)
  "Print help message."
  (format #t "\
Usage: ~a [OPTIONS] [BACKUP-ID]
Incremental backups with find and tar.

Options:
     -c, --config FILE  read configuration from FILE
     -l, --list         produce a list of files
     -h, --help         show this help and exit
         --version      show version and exit
" (program-name)))

(define (display-version)
  "Print version string."
  (format #t "~a version ~a\n" (program-name) backup-archive-version))

(define option-spec
  '((help    (single-char #\h) (value #f))
    (version                   (value #f))
    (config  (single-char #\c) (value #t))
    (list    (single-char #\l) (value #f))))

(define (%main args)
  "Process command-line arguments."
  (let* ((options      (getopt-long args option-spec))
         (config       (option-ref options 'config #f))
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
    (when config
          (config-file config))
    (let* ((profile (config-get-profile (string->symbol (car non-opt-args))))
           (label   (archive-label profile))
           (dir     (archive-dir profile)))
      (chdir dir)
      (let* ((archive-last (archive-find-most-recent label dir))
             (file-list    (backup-find-files (backup-dirs profile)
                                              (backup-predicate profile)
                                              archive-last))
             (file-count   (string-count file-list #\null)))
        (when (option-ref options 'list #f)
              (for-each (lambda (filename)
                          (display filename)
                          (newline))
                        (string-split file-list #\null))
              (exit 0))
        (format #t "file count: ~s\n" file-count)
        (when (and (not (zero? file-count))
                   (ask-yes-no-question? "continue? "))
              (archive-write (archive-prefix-current label archive-last)
                             file-list
                             (archive-format profile)))))))

(define (main args)
  (parameterize ((program-name (basename (car args))))
                (%main args)))

