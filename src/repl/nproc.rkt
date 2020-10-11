#lang s-exp "util/program/repl-program.rkt"

; Copyright 2020 David Wilson
; See COPYING for details

(provide nproc)

(require nproc)

    #;(help-function "Print number of procesing units"
                   (list "(execute) - Print number of processing units"))
(define nproc
  (λ ()
    (displayln (get-number-of-processors))))
;; nproc - Print number of processing units
#;(define nproc%
  (class object%
    (super-new)

    (help-function "Print number of procesing units"
                   (list "(execute) - Print number of processing units"))

    (on-execute-with-void 
      (displayln (get-number-of-processors)))))
