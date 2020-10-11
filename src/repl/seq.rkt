#lang s-exp "util/program/repl-program.rkt"

; Copyright 2020 David Wilson
; See COPYING for details

(provide seq)

(define seq
  (λ (x y #:interval [i 1])
    (do-seq x y i)))
    
    #;(help-function "Prints out a sequence of numbers"
                   (list "(exec) (int, int) -- Print out sequence between two numbers")
                   (list "interval (int) -- Set the interval between numbers in the sequence"))    
;(require typed/racket/class)

(define (do-seq x y interval)
  (when (<= x y)
    (begin
      (displayln (number->string x))
      (do-seq (+ x interval) y interval))))

