#lang s-exp "util/program/repl-program.rkt"

(provide hostname%)

(require "libc/unistd.rkt")

(define hostname%
  (class object%
    (super-new)

    (help-function "Get current hostname"
                   (list "(execute) -- display the current hostname"))

    (on-execute-with-void
      (displayln (get-hostname)))))
