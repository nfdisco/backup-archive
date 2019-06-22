(define-module (backup-archive profile)
  #:use-module (srfi srfi-9)
  #:export     (make-profile
                profile?
                profile-id
                profile-dest
                profile-orig
                profile-pred
                profile-fmt))


(define-record-type <profile>
  (make-profile id dest orig pred fmt)
  profile?
  (id   profile-id)
  (dest profile-dest)
  (orig profile-orig)
  (pred profile-pred)
  (fmt  profile-fmt))

