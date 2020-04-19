#lang typed/racket

(provide boolean-attribute
         integer-attribute)

(define-syntax-rule (make-attribute type field-name getter-name setter-name default-value)
  (begin
    (: field-name type)
    (field [field-name default-value])

    (define/public (setter-name [x : type]) (set! field-name x))

    (: getter-name (-> type))
    (define/public (getter-name) field-name)))
  
(define-syntax-rule (boolean-attribute field-name getter-name setter-name default-value)
  (make-attribute Boolean field-name getter-name setter-name default-value))

(define-syntax-rule (integer-attribute field-name getter-name setter-name default-value)
  (make-attribute Integer field-name getter-name setter-name default-value))