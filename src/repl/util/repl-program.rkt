#lang typed/racket/base

; Copyright 2020 David Wilson
; See COPYING for details

(provide (all-from-out typed/racket/base)
         (all-from-out typed/racket/class)
         (all-from-out "help.rkt")
         (all-from-out "member.rkt"))

(require typed/racket/class)
(require "help.rkt"
         "member.rkt")


#|
(provide simple-program
         simple-file-handler-program
         (all-from-out "param.rkt")
         (all-from-out "version.rkt")
         (all-from-out typed/racket/base)
         (rename-out [require backend-code]))

(require racket/cmdline
         racket/class)

(require "simple-program.rkt"
         "simple-file-handler-program.rkt"
         "param.rkt"
         "version.rkt")
|#
