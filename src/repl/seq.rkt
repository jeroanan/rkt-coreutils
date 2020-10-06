#lang s-exp "util/program/repl-program.rkt"

; Copyright 2020 David Wilson
; See COPYING for details

(provide seq%)

(require typed/racket/class)

(define seq%
  (class object%
    (super-new)

    (help-function "Prints out a sequence of numbers"
                   (list "(exec) (int, int) -- Print out sequence between two numbers")
                   (list "interval (int) -- Set the interval between numbers in the sequence"))

    (public-integer-attribute interval 1)

    (define/public (execute x y)
      (do-seq x y interval))))

(define (do-seq x y interval)
  (when (<= x y)
    (begin
      (displayln (number->string x))
      (do-seq (+ x interval) y interval))))

