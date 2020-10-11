#lang s-exp "util/program/repl-program.rkt"

(provide hostname)

(require "libc/unistd.rkt")
    #;(help-function "Get current hostname"
                   (list "(execute) -- display the current hostname"))

(define hostname
  (λ ()
    (displayln (get-hostname))))

