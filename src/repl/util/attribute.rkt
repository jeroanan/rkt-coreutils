#lang racket/base

(provide attribute)

(require (for-syntax racket/base))

(define-syntax (attribute stx)
  (syntax-case stx ()
    [(_ access data-type name default)     
     (with-syntax ([declaration
                    (datum->syntax stx
                                   (string->symbol (format "~a-~a-attribute"
                                                           (syntax->datum #'access)
                                                           (syntax->datum #'data-type))))])
       #'(declaration name default))]))