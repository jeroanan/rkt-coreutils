#lang typed/racket/base

; Copyright 2020 David Wilson
; See COPYING for licence

(provide public-boolean-attribute
         private-boolean-attribute
         public-integer-attribute
         private-integer-attribute
         public-string-attribute
         private-string-attribute
         string-list-attribute)

(require typed/racket/class)

(require (for-syntax racket/base))

(define-syntax-rule (public-boolean-attribute field-name default-value)
  (public-attribute field-name Boolean default-value))

(define-syntax-rule (private-boolean-attribute field-name default-value)
  (private-attribute field-name Boolean default-value))

(define-syntax-rule (public-integer-attribute field-name default-value)
  (public-attribute field-name Integer default-value))

(define-syntax-rule (private-integer-attribute field-name default-value)
  (private-attribute field-name Integer default-value))

(define-syntax-rule (public-string-attribute field-name default-value)
  (public-attribute field-name String default-value))

(define-syntax-rule (private-string-attribute field-name default-value)
  (private-attribute field-name String default-value))

(define-syntax-rule (string-list-attribute field-name default-value)
  (make-attribute (Listof String) field-name default-value))

(define-syntax-rule (string-attribute field-name default-value)
  (make-attribute String field-name default-value))

(define-syntax (public-attribute stx)
  (syntax-case stx ()
    [(_ field-name type default-value)
     (with-syntax ([getter-name
                    (datum->syntax #'field-name
                                   (string->symbol (format "get-~a"
                                                           (syntax->datum #'field-name))))]
                   [setter-name
                    (datum->syntax #'field-name
                                   (string->symbol (format "set-~a"
                                                           (syntax->datum #'field-name))))])
       #'(begin
           (: field-name type)
           (field [field-name default-value])
           
           (: getter-name (-> type))
           (define/public (getter-name) field-name)

           (define/public (setter-name [x : type]) (set! field-name x))))]))

(define-syntax-rule (private-attribute field-name type default-value)
  (begin
    (: field-name type)
    (field [field-name default-value])))
                 
                 
(define-syntax make-attribute
  (syntax-rules ()
    [(make-public-attribute type field-name default-value getter-name setter-name)
     (begin
       (: field-name type)
       (field [field-name default-value])

       (define/public (setter-name [x : type]) (set! field-name x))

       (: getter-name (-> type))
       (define/public (getter-name) field-name))]
    [(make-private-attribute type field-name default-value)
     (begin
       (: field-name type)
       (field [field-name default-value]))]))