#lang typed/racket/base

;; Copyright 2020 David Wilson
;; See COPYING for details

(provide users%)

(require typed/racket/class
         racket/string
         racket/struct)

(require "util/util.rkt" ; contains definition of help-function
         "../typedef/getutmp.rkt")

(require/typed "../libc/utmp.rkt"
               [get-utmp (-> (Instance Getutmp%))])

;; users% -- emulate the functionality of the coreutils "users" command
;; type "man users" at a shell prompt for documentation on the original
;; version.
(define users%
  (class object%
    (super-new)

    (help-function (list "Print who is currently logged in"
                         ""
                         "Methods:"
                         "(help) -- display this help message"
                         "(execute) -- display user information"))

    ;; Perform the "users" program operation
    (define/public (execute)
      (let* ([utmp (get-utmp)]
             [the-users (get-usernames)]
             [output (string-join the-users " ")])
        (displayln output)))

    ;; Get a list of users according to libc's utmp functions
    (: get-usernames (-> (Listof String)))
    (define/public (get-usernames)
      (let ([utmp (get-utmp)])
        (send utmp start-utmp)
        (define out (build-user-list utmp (list)))
        (send utmp end-utmp)
        out))    

    ;; libc's identifier for user processes.
    (define USER_PROCESS 7)

    ;; Get all current users from libc and filter them by type for USER_PROCESS 
    (: build-user-list (-> (Instance Getutmp%) (Listof String) (Listof String)))
    (define (build-user-list utmp us)
        (if (send utmp next-utmp)
            (if (eq? (send utmp get-type) USER_PROCESS)
                (build-user-list utmp (append us (list (send utmp get-user))))
                (build-user-list utmp us))
            us))))
      