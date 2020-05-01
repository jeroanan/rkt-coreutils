#lang typed/racket/base

; Copyright 2020 David Wilson
; See COPYING for details

(provide users%)

(require typed/racket/class
         racket/string)

(require "util/util.rkt")

(require/typed "../libc/utmp.rkt"
               [get-utmp-users (-> (Listof String))])

(define users%
  (class object%
    (super-new)

    (help-function (list "Print who is currently logged in"
                         ""
                         "Methods:"
                         "(help) -- display this help message"
                         "(execute) -- display user information"))

    (define/public (execute)
      (let* ([the-users (get-utmp-users)]
             [output (string-join the-users " ")])
        (displayln output)))))