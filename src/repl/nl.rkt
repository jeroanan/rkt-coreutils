#lang s-exp "util/program/line-by-line-processor-program.rkt"

; Copyright 2020 David Wilson
; See COPYING for details

(require racket/format)

(define help-text (list "numbers lines of files and print on standard output."
                        ""
                        "Methods:"
                        "(help) -- display this help message"
                        "(execute FILES) -- number lines and print FILES"))

(line-by-line-processor nl process-line null)

(define counter 0)

(define (process-line x)
  (if (string=? x "")
      (displayln x)
      (begin
        (set! counter (add1 counter))
        (let ([line-number (~a counter #:width 6  #:align 'right)])
          (displayln (format "~a\t~a" line-number x))))))

