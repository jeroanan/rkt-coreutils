#lang s-exp "util/program/repl-program.rkt"

;; Copyright 2020 David Wilson
;; See COPYING for details

(provide users)

(require racket/string
         racket/struct)

(require "util/getutmp.rkt")

(define users
  (λ ()
    (define utmp (get-user-process-utmp-entries))
    (define the-users (map (λ (x) (whoentry-user x)) utmp))
    (displayln (string-join the-users " "))))
