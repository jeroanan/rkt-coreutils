#lang s-exp "ffi.rkt"

; Copyright 2020 David Wilson
; See COPYING for details.

(provide uname
         (struct-out utsname))

(require ffi/unsafe)

(define clib (ffi-lib #f))

(define _utsname-field (_bytes/len 65))

(define-cstruct _utsname ([sysname  _utsname-field]
                          [nodename _utsname-field]
                          [release  _utsname-field]
                          [version  _utsname-field]
                          [machine  _utsname-field]
                          [domainname _utsname-field]))

(define _uname
  (get-ffi-obj "uname" clib
               (_fun (o : (_ptr o _utsname)) 
                     -> (r : _int)
                     -> o)))
(define (uname)
  (_uname))
