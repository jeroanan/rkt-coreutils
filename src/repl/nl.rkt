#lang s-exp "util/program/line-by-line-processor-program.rkt"

; Copyright 2020 David Wilson
; See COPYING for details

(provide nl%)

(require racket/format)

(define help-text (list "numbers lines of files and print on standard output."
                        ""
                        "Methods:"
                        "(help) -- display this help message"
                        "(execute FILES) -- number lines and print FILES"))

(line-by-line-processor-program nl%
                                help-text
                                (λ (x)
                                  (if (string=? x "")
                                      (displayln x)
                                      (begin
                                        (set! counter (add1 counter))
                                        (let ([line-number (~a counter #:width 6  #:align 'right)])
                                          (displayln (format "~a\t~a" line-number x))))))
                                (attribute private integer counter 0))
