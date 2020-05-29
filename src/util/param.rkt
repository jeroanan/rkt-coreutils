#lang typed/racket/base

;; Copyright 2020 David Wilson
;; see COPYING for details

(provide boolean-parameter
         string-parameter
         string-list-parameter)

(require (for-syntax racket/base))

(define-syntax-rule (boolean-parameter name initial-value)
  (make-typed-parameter Boolean name initial-value))

(define-syntax-rule (string-parameter name initial-value)
  (make-typed-parameter String name initial-value)) 

(define-syntax (string-list-parameter stx)
  (syntax-case stx ()
    [(_ name)
     (with-syntax ([setter-name
                    (datum->syntax #'name
                                   (string->symbol (format "set-~a"
                                                           (syntax->datum #'name))))])
       #'(begin
           (make-typed-parameter (Listof String) name (list ""))
      
           (define (setter-name [s : (Pairof Any (Listof Any))])
             (let ([strings (map (λ (x) (format "~a" x)) s)])
               (name strings)))))]))

(define-syntax-rule (make-typed-parameter type name initial-value)
  (begin
    (: name (Parameterof type))
    (define name (make-parameter initial-value))))
           