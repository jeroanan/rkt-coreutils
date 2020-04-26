#lang typed/racket

(provide right-aligned-string
         string<>?)

(define-syntax-rule (right-aligned-string val width)
  (~a val #:width width #:align 'right))

(define-syntax-rule (string<>? s1 s2)
  (not (string=? s1 s2)))
