#lang s-exp "util/program/repl-program.rkt"

; Copyright 2020 David Wilson
; See COPYING for details

(provide tty%)

(require "libc/unistd.rkt")

(define tty%
  (class object%
    (super-new)

    (help-function "Print current tty name"
                   (list "(execute) -- Print current tty name"))

    (on-execute-with-void
      (displayln (get-ttyname)))))



