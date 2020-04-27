#lang typed/racket/base

(provide true-false-program)

(require racket/cmdline)

(require "version.rkt")

(define-syntax-rule (true-false-program return-value)
  (begin
    (define args (make-parameter (list)))

    (command-line
     #:argv (current-command-line-arguments)
     #:once-each
     [("-v" "--version") "display version information and exit" (print-version-text-and-exit)]
     #:args a (args a))

    (exit return-value)))