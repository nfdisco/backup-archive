(define-module (backup-archive profile)
  #:use-module (srfi srfi-9)
  #:export     (make-profile
                profile?
                archive-name
                archive-dir
                backup-dirs
                backup-predicate
                archive-format))


(define-record-type profile
  (make-profile name dir dirs predicate format)
  profile?
  (name      archive-name)
  (dir       archive-dir)
  (dirs      backup-dirs)
  (predicate backup-predicate)
  (format    archive-format))

