#lang s-exp "util/program/repl-program.rkt"

; Copyright 2020 David Wilson
; See COPYING for details

(provide yes%)

(define yes%
  (class object%
    (super-new)

    (help-function "Print the given string until stopped"
                   (list "(execute STRING) -- Print STRING until stopped"))

    (on-execute-with-string s
      (do-yes s))))

(: do-yes (-> String Void))
(define (do-yes s)
  (displayln s)
  (do-yes s))






