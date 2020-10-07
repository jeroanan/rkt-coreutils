#lang racket/base

; Copyright 2020 David Wilson
; See COPYING for details

(provide simple-program
         (all-from-out "simple-file-handler-program.rkt")
         (all-from-out "param.rkt")
         (all-from-out "version.rkt")
         (all-from-out racket/base)
         (rename-out [require backend-code]))

(require racket/cmdline
         racket/class)

(require "simple-program.rkt"
         "simple-file-handler-program.rkt"
         "param.rkt"
         "version.rkt")
