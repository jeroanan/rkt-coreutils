#lang typed/racket

; Copyright 2020 David Wilson
; See COPYING for licence

(provide boolean-attribute
         integer-attribute
         string-list-attribute)
  
(define-syntax-rule (boolean-attribute field-name getter-name setter-name default-value)
  (make-attribute Boolean field-name getter-name setter-name default-value))

(define-syntax-rule (integer-attribute field-name getter-name setter-name default-value)
  (make-attribute Integer field-name getter-name setter-name default-value))

(define-syntax-rule (string-list-attribute field-name default-value)
  (make-attribute (Listof String) field-name default-value))

(define-syntax make-attribute
  (syntax-rules ()
    [(make-public-attribute type field-name getter-name setter-name default-value)
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