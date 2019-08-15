(define-module (backup-archive archive)
  #:use-module (backup-archive common)
  #:use-module (backup-archive shell)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 format)
  #:export     (archive-format-supported?
                archive-find-most-recent
                archive-prefix-current
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

(define (archive-filename-pattern prefix)
  "Return filename pattern."
  (string-append "^" (regexp-quote prefix) "-([0-9]+)-([a-z]+)([.].+)?$"))

(define (archive-filename-volume match)
  "Return volume component."
  (and match
       (string->number (match:substring match 1))))

(define (archive-filename-part match)
  "Return part component."
  (and match
       (match:substring match 2)))

(define (archive-filename match)
  "Return whole filename."
  (and match
       (match:string match)))

(define (archive-filename-parser prefix)
  "Return filename-parsing procedure."
  (let ((pattern (make-regexp (archive-filename-pattern prefix))))
    (lambda (filename)
      (regexp-exec pattern filename))))

(define (archive-filename-prefix name volume)
  "Return archive filename prefix."
  (format #f "~a-~3,'0d-" name volume))

(define (archive-prefix-current name last)
  "Return filename prefix for current archive."
  (if last
      (let ((match ((archive-filename-parser name) last)))
        (archive-filename-prefix name
                                 (1+ (archive-filename-volume match))))
      (archive-filename-prefix name 0)))

(define (archive-find-most-recent name dir)
  "Return filename of most recent archive."
  (let ((stream (opendir dir))
        (f (archive-filename-parser name)))
    (define (iter match)
      (let ((filename (readdir stream)))
        (cond ((eof-object? filename)
               (closedir stream)
               (and match
                    (archive-filename match)))
              (else
               (let ((match-current (f filename)))
                 (cond ((not match-current)
                        (iter match))
                       ((not match)
                        (iter match-current))
                       (else
                        (if (> (archive-filename-volume match-current)
                               (archive-filename-volume match))
                            (iter match-current)
                            (iter match)))))))))
    (iter #f)))

(define (archive-write output-prefix backup-files archive-format)
  "Write archive file."
  ((apply shell-pipe
          `(("tar"
             "--no-recursion"
             "--null"
             "--files-from"
             "-"
             "--preserve-permissions"
             "--create"
             "--totals")
            ,@(archive-format-filters archive-format)
            ("split"
             "--additional-suffix"
             ,(archive-format-suffix archive-format)
             "-b"
             "1G"
             "-d"
             "-"
             ,output-prefix)))
   (lambda () (display backup-files))))
