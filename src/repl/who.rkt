#lang typed/racket/base

;; Copyright 2020 David Wilson
;; See COPYING for details

(provide who%)

;; includes help-method
(require "util/util.rkt"
         "../typedef/getutmp.rkt")

(require typed/racket/class
         typed/racket/date
         racket/format
         racket/string)

;; access to getutmp, which uses libc to get info on logged-in users
(require/typed "../libc/utmp.rkt"
               [get-utmp (-> (Instance Getutmp%))])

;; who% -- emulate the functionality of the coreutils "who" command.
;; type "man who" at a shell prompt for documentation on the original
;; program.
(define who%
  (class object%
    (super-new)

    (help-function
     (list "Print information about users that are currently logged in."
           ""
           "Methods:"
           "(help) -- display this help message"
           "(execute) -- print information about users that are currently logged in."))

    ;; peform the "who" program execution
    (define/public (execute)
      (let ([entries (build-who-list)])
        (for ([e entries])
          (let* ([output-fields (list (whoentry-user e)
                                      (whoentry-line e)
                                      (date->string (seconds->date (whoentry-time e)) #t)
                                      (whoentry-host e))]
                 [output (string-join output-fields " ")])
            (displayln output)))))

    ;; Make a list of whoentry for output by querying libc's utmp
    (: build-who-list (-> (Listof whoentry)))
    (define/private (build-who-list)
      (let ([utmp (get-utmp)])
        (send utmp start-utmp)
        (define out (make-entry-list (list) utmp))
        (send utmp end-utmp)
        out))

    ;; libc's identifier for login processes.
    (define LOGIN_PROCESS 6)
    ;; libc's identifier for user processes.
    (define USER_PROCESS 7)

    (date-display-format 'iso-8601)
    
    ;; Get all current users from libc's utmp and filter them by type for USER_PROCESS 
    (: make-entry-list (-> (Listof whoentry) (Instance Getutmp%) (Listof whoentry)))
    (define (make-entry-list entries utmp)      
      (if (send utmp next-utmp)                    
          (if (eq? (send utmp get-type) USER_PROCESS)
              (make-entry-list (append entries (list (make-whoentry utmp))) utmp)
              (make-entry-list entries utmp))
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
       (send utmp get-time)))))

(struct whoentry ([type : Integer]
                  [user : String]
                  [line : String]
                  [host : String]
                  [time : Integer]))
