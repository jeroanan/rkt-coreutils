#lang s-exp "ffi.rkt"

; Copyright 2020 David Wilson
; See COPYING for details

(provide get-statvfs
         get-blocks
         get-free
         get-available
         get-blocksize
         get-fragmentsize
         get-files)

(require ffi/unsafe
         racket/runtime-path
         (for-syntax racket/base))

(define _fsblkcnt_t _int)
(define _fsfilcnt_t _ulong)

(define-runtime-path lib-path (build-path "src" "lib" "getstatvfs"))
(define clib (ffi-lib lib-path))

(c-function get-statvfs clib _int "getstatvfs" _string)
(c-function get-blocks clib _fsblkcnt_t "getblocks")
(c-function get-free clib _fsblkcnt_t "getfree")
(c-function get-available clib _fsblkcnt_t "getavailable")
(c-function get-blocksize clib _ulong "getblocksize")
(c-function get-fragmentsize clib _ulong "getfragmentsize")
(c-function get-files clib _fsfilcnt_t "getfiles")
