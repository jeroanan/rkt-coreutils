#lang typed/racket/base

; Copyright 2020 David Wilson
; See COPYING for details

(provide nl%)

(require racket/format)

(require "util/program/line-by-line-processor-program.rkt")

(define help-text (list "numbers lines of files and print on standard output."
                        ""
                        "Methods:"
                        "(help) -- display this help message"
                        "(execute FILES) -- number lines and print FILES"))

(define counter 0)

(define (number-line [x : String])
  (if (string=? x "")
      x
      (begin
        (set! counter (add1 counter))
        (let ([line-number (~a counter #:width 6  #:align 'right)])
          (format "~a\t~a" line-number x)))))

(line-by-line-processor-program nl% help-text number-line)
