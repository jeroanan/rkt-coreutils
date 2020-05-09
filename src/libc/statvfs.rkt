#lang racket/base

; Copyright 2020 David Wilson
; See COPYING for details

(require ffi/unsafe
         dynamic-ffi/unsafe)

(define _fsblkcnt_t _ulong);_uint64)
(define _fsfilcnt_t _ulong);_uint32)

(define-cstruct _statvfsstruct([f_bsize   _ulong] ; block size
                               [f_frsize  _ulong] ; fragment size
                               [f_blocks  _fsblkcnt_t]
                               [f_bfree   _fsblkcnt_t]
                               [f_bavail  _fsblkcnt_t]
                               [f_files   _fsfilcnt_t]
                               [f_ffree   _fsfilcnt_t]
                               [f_favail  _fsfilcnt_t]
                               [f_fsid    _ulong]
                               [f_flag    _ulong]
                               [f_namemax _ulong]))

(define clib (ffi-lib #f))
(define statvfs (get-ffi-obj "statvfs" clib (_fun _string (i : (_ptr o _statvfsstruct))
                                                  -> (r : _int)
                                                  -> (values r i))))




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

  "fsfilcnt_t getfiles(void) {\n"
  "  return stat.f_files;\n"
  "}\n"
  
  "int getstatvfs(const char* path) {\n"
  "  return statvfs(path, &stat);\n"
  "}\n")

(provide get-statvfs get-blocks get-blocksize get-fragmentsize get-files)

(define (get-statvfs path)
  (ggl 'getstatvfs path))

(define (get-blocksize)
  (ggl 'getblocksize))

(define (get-fragmentsize)
  (ggl 'getfragmentsize))

(define (get-blocks)
  (ggl 'getblocks))

(define (get-files)
  (ggl 'getfiles))