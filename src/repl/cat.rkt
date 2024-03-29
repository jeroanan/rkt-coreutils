#lang s-exp "util/program/line-by-line-processor-program.rkt"

; Copyright 2020 David Wilson
; See COPYING for details

(provide cat)

(define help-text (list "Concatenate files and print on standard output."
                        ""
                        "Methods:"
                        "(help) -- display this help message"
                        "(execute FILES) -- concatenate and print FILES"))

(line-by-line-processor cat process-line null)

(define (process-line x)
  (displayln x))
