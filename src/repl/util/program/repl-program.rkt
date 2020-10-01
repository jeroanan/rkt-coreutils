#lang racket/base

; Copyright 2020 David Wilson
; See COPYING for details
(require racket/class)

(provide (all-from-out racket/base)
         (all-from-out racket/class)
         (all-from-out "../help.rkt")
         (all-from-out "../member.rkt")
         on-execute-with-string
         on-execute-with-strings
         on-execute-with-void)

(require "../help.rkt"
         "../member.rkt")

(define-syntax-rule (on-execute-with-string param-name s)
  (on-execute-with-type String param-name s))

(define-syntax-rule (on-execute-with-strings param-name s)
  (on-execute-with-type (Listof String) param-name s))

(define-syntax-rule (on-execute-with-type type param-name s)
  (begin
    (define/public (execute param-name)
      s)))

(define-syntax-rule (on-execute-with-void s)
  (begin
    (define/public (execute)
      s)))


