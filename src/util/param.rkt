#lang typed/racket/base

;; Copyright 2020 David Wilson
;; see COPYING for details

(provide boolean-parameter)

(define-syntax-rule (boolean-parameter name initial-value)
  (begin
    (: name (Parameterof Boolean))
    (define name (make-parameter initial-value))))