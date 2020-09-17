#lang s-exp "ffi.rkt"

; Copyright 2020 David Wilson
; See COPYING for details.

(provide get-euid
         get-uid
         get-hostid
         get-hostname
         get-ttyname)

(require ffi/unsafe)
(require racket/list)

(define clib (ffi-lib #f))

(define _hostname-len 99)

(c-function _geteuid clib _int "geteuid")
(c-function _getuid clib _int "getuid")
(c-function _gethostid clib _long "gethostid")
(c-function _ttyname clib _string "ttyname" _int)

(define (get-euid) (_geteuid))
(define (get-uid) (_getuid))
(define (get-hostid) 
  (define raw-hostid (_gethostid))
  (define unsigned (bitwise-and raw-hostid #xffffffff))
  unsigned)

(define _hostname (_bytes/len _hostname-len))

(define _gethostname
  (get-ffi-obj "gethostname" clib
               (_fun (o : (_ptr o _hostname)) _int
                     -> (r : _int)
                     -> o)))

(define (get-hostname)
  (define raw-hostname (_gethostname _hostname-len))
  (bytes->string raw-hostname))

(define (get-ttyname)
  (_ttyname 0))
  
