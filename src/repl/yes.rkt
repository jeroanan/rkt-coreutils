#lang s-exp "util/program/repl-program.rkt"

; Copyright 2020 David Wilson
; See COPYING for details

(provide yes)
   #; (help-function "Print the given string until stopped"
                   (list "(execute STRING) -- Print STRING until stopped")
                   (list "repeated-string (string) -- The string to print"))

(define yes
  (λ (#:repeated-string [rs "y"])
    (displayln rs)
    (yes #:repeated-string rs)))

