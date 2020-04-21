#lang typed/racket

; Copyright 2020 David Wilson
; See COPYING for details

(provide nl%)

(require "util/program/line-by-line-processor-program.rkt")

(define help-text (list "numbers lines of files and print on standard output."
                        ""
                        "Methods:"
                        "(help) -- display this help message"
                        "(execute FILES) -- concatenate and print FILES"))

(define counter 0)

(define (number-line [x : String])
  (if (string=? x "")
      x
      (begin
        (set! counter (add1 counter))
        (format "~a ~a" counter x))))

(line-by-line-processor-program nl% help-text number-line)
