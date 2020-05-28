#lang s-exp "ffi.rkt"

; Copyright 2020 David Wilson
; See COPYING for details.

(provide get-euid get-uid)

(require ffi/unsafe)

(define clib (ffi-lib #f))

(c-function _geteuid clib _int "geteuid")
(c-function _getuid clib _int "getuid")

(define (get-euid) (_geteuid))
(define (get-uid) (_getuid))
