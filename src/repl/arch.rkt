#lang s-exp "util/program/repl-program.rkt"

; Copyright 2020 David Wilson
; See COPYING for details

(provide arch)

(require "libc/utsname.rkt")

(define arch
  (λ ()
    (displayln (uname-machine))))
