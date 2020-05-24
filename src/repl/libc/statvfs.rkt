#lang racket/base

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

(define-syntax (c-function stx)
  (syntax-case stx ()
    [(_ racket-name return-type c-name param-type)
     #'(define racket-name (get-ffi-obj c-name clib (_fun param-type -> return-type)))]
    [(_ racket-name return-type c-name)
     #'(define racket-name (get-ffi-obj c-name clib (_fun -> return-type)))]))
     
;(define-syntax-rule (c-function racket-name return-type c-name param-type)
;  (define racket-name (get-ffi-obj c-name clib (_fun param-type -> return-type))))

(define _fsblkcnt_t _int)
(define _fsfilcnt_t _ulong)

(define-runtime-path lib-path (build-path "src" "getgrouplist"))
(define clib (ffi-lib lib-path))

(c-function get-statvfs _int "getstatvfs" _string)
(c-function get-blocks _fsblkcnt_t "getblocks")
(c-function get-free _fsblkcnt_t "getfree")
(c-function get-available _fsblkcnt_t "getavailable")
(c-function get-blocksize _ulong "getblocksize")
(c-function get-fragmentsize  _ulong "getfragmentsize")
(c-function get-files _fsfilcnt_t "getfiles")
#|
(define-inline-ffi ggl #:compiler "clang"
  "#include <sys/statvfs.h>\n"
  
  "struct statvfs stat;\n"

  "unsigned long getblocksize(void) {\n"
  "  return stat.f_bsize;\n"
  "}\n"

  "unsigned long getfragmentsize(void) {\n"
  "  return stat.f_frsize;\n"
  "}\n"
  
  "fsblkcnt_t getblocks(void) {\n"
  "  return stat.f_blocks;\n"
  "}\n"

  "fsblkcnt_t getfree(void) {\n"
  "  return stat.f_bfree;\n"
  "}\n"

  "fsblkcnt_t getavailable(void) {\n"
  "  return stat.f_bavail;\n"
  "}\n"

  "fsfilcnt_t getfiles(void) {\n"
  "  return stat.f_files;\n"
  "}\n"
  
  "int getstatvfs(const char* path) {\n"
  "  return statvfs(path, &stat);\n"
  "}\n")

(define (get-statvfs path)
  (ggl 'getstatvfs path))

(define (get-blocksize)
  (ggl 'getblocksize))

(define (get-fragmentsize)
  (ggl 'getfragmentsize))

(define (get-blocks)
  (ggl 'getblocks))

(define (get-free)
  (ggl 'getfree))

(define (get-available)
  (ggl 'getavailable))

(define (get-files)
  (ggl 'getfiles))
|#
