#lang racket

; Copyright 2020 David Wilson
; See COPYING for details.

(provide get-euid get-uid)

(require ffi/unsafe)

(define clib (ffi-lib #f))

(define-syntax-rule (ffi-fun->int func-name)
  (get-ffi-obj func-name clib (_fun -> _int)))

(define _geteuid (ffi-fun->int "geteuid"))
(define _getuid (ffi-fun->int "getuid"))

(define (get-euid) (_geteuid))
(define (get-uid) (_getuid))