(define-module (backup-archive archive)
  #:use-module (backup-archive common)
  #:use-module (backup-archive shell)
  #:use-module (ice-9 regex)
  #:export     (archive-format-supported?
                archive-format-suffix
                archive-format-filters
                archive-find
                archive-last
                archive-current
                archive-write))

(define archive-format-alist
  '((tar     . (".tar"     ()))
    (tar-gz  . (".tar.gz"  (("gzip"))))
    (tar-xz  . (".tar.xz"  (("xz"))))
    (tar-bz2 . (".tar.bz2" (("bz2"))))
    (tar-gpg . (".tar.gpg" (("gpg" "--symmetric"))))))

(define (archive-format-supported? key)
  "True if KEY is a supported archive format."
  (assq key archive-format-alist))

(define (archive-format-suffix key)
  "Return the file name suffix for KEY, or false if KEY is an unsupported
archive format."
  (let ((value (assq-ref archive-format-alist key)))
    (if (not value)
        #f
        (car value))))

(define (archive-format-filters key)
  "Return a list of filters for KEY, or false if KEY is an unsupported
archive format."
  (let ((value (assq-ref archive-format-alist key)))
    (if (not value)
        #f
        (cadr value))))

(define (archive-filename-pattern name)
  "Return regular expression that matches NAME archive filenames."
  (make-regexp (string-append "^" (regexp-quote name) "-([0-9]+)([.].+)?$")))

(define (archive-filename name n fmt)
  "Return archive filename."
  (string-append name "-" (number->string n) (archive-format-suffix fmt)))

(define (archive-find name dir fmt)
  "Return the pathnames of last and current archive."
  (let ((stream (opendir dir))
        (pattern (archive-filename-pattern name)))
    (define (iter last)
      (let ((filename (readdir stream)))
        (if (eof-object? filename)
            last
            (let* ((match (regexp-exec pattern filename))
                   (n (and (regexp-match? match)
                           (string->number (match:substring match 1)))))
              (if (and n
                       (> n (cdr last)))
                  (iter (cons filename n))
                  (iter last))))))
    (let ((last (iter '(#f . -1))))
      (closedir stream)
      (cons (if (car last)
                (path-join dir (car last))
                #f)
            (path-join dir (archive-filename name (1+ (cdr last)) fmt))))))

(define archive-last car)

(define archive-current cdr)

(define (archive-write output-file backup-files filters)
  "Write archive file."
  (with-output-to-file output-file
    (lambda ()
      ((apply shell-pipe
              (cons '("tar"
                      "--no-recursion"
                      "--null"
                      "--files-from" "-"
                      "--preserve-permissions"
                      "--create"
                      "--totals")
                    filters))
       (lambda () (display backup-files))))))

