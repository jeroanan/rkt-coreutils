#lang typed/racket/base

; Copyright 2020 David Wilson
; See COPYING for details

(provide (all-from-out typed/racket/base)
         (except-out (all-from-out typed/racket/class) define/public)
         (all-from-out "../help.rkt")
         (all-from-out "../member.rkt")
         on-execute-with-string
         on-execute-with-strings
         on-execute-with-void)

(require typed/racket/class)
(require "../help.rkt"
         "../member.rkt")

(define-syntax-rule (on-execute-with-string param-name s)
  (on-execute-with-type String param-name s))

(define-syntax-rule (on-execute-with-strings param-name s)
  (on-execute-with-type (Listof String) param-name s))

(define-syntax-rule (on-execute-with-type type param-name s)
  (begin
    (: execute (-> type Void))
    (define/public (execute param-name)
      s)))

(define-syntax-rule (on-execute-with-void s)
  (begin
    (define/public (execute)
      s)))


