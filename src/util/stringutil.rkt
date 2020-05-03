#lang typed/racket/base

;; Copyright 2020 David Wilson
;; See COPYING for details

(require racket/format)

(provide right-aligned-string
         left-aligned-string
         string<>?
         anything->string)

(define-syntax-rule (right-aligned-string val width)
  (~a val #:width width #:align 'right))

(define-syntax-rule (left-aligned-string val width)
  (~a val #:width width #:align 'left))

(define-syntax-rule (string<>? s1 s2)
  (not (string=? s1 s2)))

(define (anything->string a)
  (~a a))