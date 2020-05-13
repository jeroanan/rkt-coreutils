#lang typed/racket/base

;; Copyright 2020 David Wilson
;; see COPYING for details

(provide boolean-parameter
         string-parameter)

(define-syntax-rule (boolean-parameter name initial-value)
  (make-typed-parameter Boolean name initial-value))

(define-syntax-rule (string-parameter name initial-value)
  (make-typed-parameter String name initial-value)) 

(define-syntax-rule (make-typed-parameter type name initial-value)
  (begin
    (: name (Parameterof type))
    (define name (make-parameter initial-value))))
           