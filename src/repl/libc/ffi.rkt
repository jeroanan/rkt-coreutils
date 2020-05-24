#lang racket/base

; Copyright 2020 David Wilson
; See COPYING for details

(provide (all-from-out racket/base)         
         c-function)

(require ffi/unsafe
         (for-syntax racket/base))

(define-syntax (c-function stx)
  (syntax-case stx ()
    [(_ racket-name clib return-type c-name param-type ...)
     #'(define racket-name (get-ffi-obj c-name clib (_fun param-type ... -> return-type)))]
    [(_ racket-name clib return-type c-name)
     #'(define racket-name (get-ffi-obj c-name clib (_fun -> return-type)))]))