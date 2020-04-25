#lang typed/racket

(provide right-aligned-string)

(define-syntax-rule (right-aligned-string val width)
  (~a val #:width width #:align 'right))

