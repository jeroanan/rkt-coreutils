#lang racket/base

; Copyright 2020 David Wilson
; See COPYING for details

(require dynamic-ffi/unsafe)

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

(provide get-statvfs
         get-blocks
         get-free
         get-available
         get-blocksize
         get-fragmentsize
         get-files)

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