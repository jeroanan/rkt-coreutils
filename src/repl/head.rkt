#lang s-exp "util/program/line-by-line-processor-program.rkt"

; Copyright 2020 David Wilson
; See COPYING for licence details

;; Head: Print the first lines of the given files
(provide head)

(define help-text (list "Print the first lines of each provided file."
                        ""
                        "Methods"
                        "(execute FILES) -- display the first lines of FILES"
                        "(help) -- Display this help message"
                        ""
                        "Attributes"
                        "number-of-lines (integer)"))

(line-by-line-processor-program head%
                                help-text
                                (λ (x)
                                  (begin
                                    (when (< counter number-of-lines) (displayln x))
                                    (set! counter (add1 counter))))
                                (λ ()
                                  (set! counter 0))
                                (attribute public integer number-of-lines 10)
                                (attribute private integer counter 0))

(define h (new head%))

(define head
  (λ (f . fs)    
    (send h execute (cons f fs))))