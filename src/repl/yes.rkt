#lang s-exp "util/program/repl-program.rkt"

; Copyright 2020 David Wilson
; See COPYING for details

(provide yes%)

(define yes%
  (class object%
    (super-new)

    (help-function "Print the given string until stopped"
                   (list "(execute STRING) -- Print STRING until stopped")
                   (list "repeated-string (string) -- The string to print"))

    (public-string-attribute repeated-string "y")

    (on-execute-with-void
      (do-yes repeated-string))))

(: do-yes (-> String Void))
(define (do-yes s)
  (displayln s)
  (do-yes s))
