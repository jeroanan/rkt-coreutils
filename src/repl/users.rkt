#lang typed/racket/base

;; Copyright 2020 David Wilson
;; See COPYING for details

(provide users%)

(require typed/racket/class
         racket/string
         racket/struct)

(require "util/getutmp.rkt"
         "util/util.rkt")

;; users% -- emulate the functionality of the coreutils "users" command
;; type "man users" at a shell prompt for documentation on the original
;; program.
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
      (let* ([utmp (get-user-process-utmp-entries)]
             [the-users (map (λ (x) (whoentry-user x)) utmp)]
             [output (string-join the-users " ")])
        (displayln output)))))
