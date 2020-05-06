#lang typed/racket/base

(require typed/racket/class)

(provide get-user-process-utmp-entries
         (struct-out whoentry))

;; access to getutmp, which uses libc to get info on logged-in users
(require/typed "../../libc/utmp.rkt"
               [get-utmp (-> (Instance Getutmp%))])

(require "../../typedef/getutmp.rkt")

;; libc's identifier for login processes.
(define LOGIN_PROCESS 6)
;; libc's identifier for user processes.
(define USER_PROCESS 7)

(define (get-user-process-utmp-entries)
  (get-utmp-entries-by-type USER_PROCESS))

;; Make a list of whoentry for output by querying libc's utmp
(: get-utmp-entries-by-type (-> Integer (Listof whoentry)))
(define (get-utmp-entries-by-type type-no)
  (let ([utmp (get-utmp)])
    (send utmp start-utmp)
    (define out (make-utmp-entry-list-for-type (list) utmp type-no))
    (send utmp end-utmp)
    out))

;; Get all current users from libc's utmp and filter them by type-no
(: make-utmp-entry-list-for-type (-> (Listof whoentry) (Instance Getutmp%) Integer (Listof whoentry)))
(define (make-utmp-entry-list-for-type entries utmp type-no)      
  (if (send utmp next-utmp)                    
      (if (eq? (send utmp get-type) type-no)
          (make-utmp-entry-list-for-type (append entries (list (make-whoentry utmp))) utmp type-no)
          (make-utmp-entry-list-for-type entries utmp type-no))
      entries))

;; Query an instance of Getutmp% and make an instance
;; of whoentry, which contains what we need to display
(: make-whoentry (-> (Instance Getutmp%) whoentry))
(define (make-whoentry utmp)
  (whoentry
   (send utmp get-type)
   (send utmp get-user)
   (send utmp get-line)
   (send utmp get-host)
   (send utmp get-time)))

(struct whoentry ([type : Integer]
                  [user : String]
                  [line : String]
                  [host : String]
                  [time : Integer]))
