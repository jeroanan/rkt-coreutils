#lang typed/racket/base

; Copyright 2020 David Wilson
; See COPYING for details

(provide simple-program
         simple-file-handler-program
         (all-from-out typed/racket/base)
         (rename-out [require backend-code]))

(require racket/cmdline
         racket/class)

(require "simple-program.rkt"
         "simple-file-handler-program.rkt")
