#lang s-exp "util/program/repl-program.rkt"

; Copyright 2020 David Wilson
; See COPYING for details

(provide tty)

(require "libc/unistd.rkt")

(define tty
  (λ ()
    (displayln (get-ttyname))))

