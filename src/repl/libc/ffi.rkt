#lang racket/base

; Copyright 2020 David Wilson
; See COPYING for details

(provide (all-from-out racket/base)         
         c-function
         _bytes/len
         _size_t
         bytes->string)

(require ffi/unsafe
         (for-syntax racket/base)
         racket/list)

(define-syntax (c-function stx)
  (syntax-case stx ()
    [(_ racket-name clib return-type c-name param-type ...)
     #'(define racket-name (get-ffi-obj c-name clib (_fun param-type ... -> return-type)))]
    [(_ racket-name clib return-type c-name)
     #'(define racket-name (get-ffi-obj c-name clib (_fun -> return-type)))]))

(define (_bytes/len n)
  (make-ctype (make-array-type _byte n)
              ;; see https://github.com/dyoo/ffi-tutorial

              ;; ->c
              (lambda (v)
                (unless (and (bytes? v) (= (bytes-length v) n))
                  (raise-argument-error '_chars/bytes 
                                        (format "bytes of length ~a" n)
                                        v))
                v)

              ;; ->racket
              (lambda (v)
                (make-sized-byte-string v n))))

(define (bytes->string bs)
  (define bytes-list (bytes->list bs))  
  (define first-null (index-of bytes-list 0))
  (define no-nulls (take bytes-list first-null))
  (define output (list->bytes no-nulls))
  (bytes->string/utf-8 output))

(define (strip-null-bytes bs)
      (filter (λ (x) (not (eq? x 0))) bs))

(define _size_t _long)