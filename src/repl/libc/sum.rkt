#lang s-exp "ffi.rkt"

; Copyright 2020 David Wilson
; See COPYING for details.

(provide get-bsd-sum
         get-sysv-sum)

(require ffi/unsafe
         racket/list
         racket/runtime-path
         racket/string
         (for-syntax racket/base))

(define-runtime-path lib-path (build-path ".." ".." ".." "lib" "sum"))
(define clib (ffi-lib lib-path))

(define _sum-len 99)
(define _sum (_bytes/len _sum-len))

(define _bsd-sum-file
  (get-ffi-obj "bsd_sum_file" clib
               (_fun _string (o : (_ptr o _sum)) 
                     -> (r : _int)
                     -> o)))
(define _sysv-sum-file
  (get-ffi-obj "sysv_sum_file" clib
               (_fun _string (o : (_ptr o _sum)) 
                     -> (r : _int)
                     -> o)))

(define (get-bsd-sum filename)
  (define raw-sum (_bsd-sum-file filename))
  (define string-output (bytes->string raw-sum))
  (define split (string-split string-output " "))
  (list (first split) (string->number (last split))))

(define (get-sysv-sum filename)
  (define raw-sum (_sysv-sum-file filename))
  (define string-output (bytes->string raw-sum))
  (define split (string-split string-output " "))
  (list (first split) (string->number (last split))))

