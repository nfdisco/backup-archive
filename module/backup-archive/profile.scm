(define-module (backup-archive profile)
  #:use-module (srfi srfi-9)
  #:export     (make-profile
                profile?
                archive-label
                archive-dir
                backup-dirs
                backup-predicate
                archive-format))


(define-record-type profile
  (make-profile label dir dirs predicate format)
  profile?
  (label     archive-label)
  (dir       archive-dir)
  (dirs      backup-dirs)
  (predicate backup-predicate)
  (format    archive-format))

