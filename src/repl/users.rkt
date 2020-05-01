#lang typed/racket/base

; Copyright 2020 David Wilson
; See COPYING for details

(provide users%)

(require typed/racket/class
         racket/string
         racket/struct)

(require "util/util.rkt")

(define-type Getutmp%
  (Class
   [start-utmp (-> Void)]
   [end-utmp (-> Void)]
   [next-utmp (-> Boolean)]
   [get-type (-> Integer)]
   [get-user (-> String)]))

(require/typed "../libc/utmp.rkt"
               [get-utmp (-> (Instance Getutmp%))])

(define users%
  (class object%
    (super-new)

    (help-function (list "Print who is currently logged in"
                         ""
                         "Methods:"
                         "(help) -- display this help message"
                         "(execute) -- display user information"))

    (define/public (execute)
      (let* ([utmp (get-utmp)]
             [the-users (get-usernames)]
             [output (string-join the-users " ")])
        (displayln output)))

    (: get-usernames (-> (Listof String)))
    (define/public (get-usernames)
      (let ([utmp (get-utmp)])
        (send utmp start-utmp)
        (define out (build-user-list utmp (list)))
        (send utmp end-utmp)
        out))    

    (define USER_PROCESS 7)
    
    (: build-user-list (-> (Instance Getutmp%) (Listof String) (Listof String)))
    (define (build-user-list utmp us)
        (if (send utmp next-utmp)
            (if (eq? (send utmp get-type) USER_PROCESS)
                (build-user-list utmp (append us (list (send utmp get-user))))
                (build-user-list utmp us))
            us))))
      