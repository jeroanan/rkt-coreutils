#lang s-exp "ffi.rkt"

; Copyright 2020 David Wilson
; See COPYING for details

(provide get-number-of-processors)

(require ffi/unsafe
         racket/class
         racket/runtime-path
         (for-syntax racket/base))

(define-runtime-path lib-path (build-path ".." ".." ".." "lib" "gnulib"))
(define clib (ffi-lib lib-path))


(c-function num-processors clib _long "num_processors" _int)

(define (get-number-of-processors)
  (num-processors 0))
