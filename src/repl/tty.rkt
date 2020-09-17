#lang s-exp "util/program/repl-program.rkt"

; Copyright 2020 David Wilson
; See COPYING for details

(provide tty%)

(require/typed "libc/unistd.rkt"
               [get-ttyname (-> String)])
(define tty%
  (class object%
    (super-new)

    (help-function "Print current tty name"
                   (list "(execute) -- Print current tty name"))

    (on-execute-with-void
      (displayln (get-ttyname)))))



