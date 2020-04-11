#lang racket

(require "util/version.rkt")

(define args (make-parameter ""))

(command-line
  #:argv (current-command-line-arguments)
  #:once-each
  [("-v" "--version") "display version information and exit" (print-version-text-and-exit)]
  #:args a (args a))

(exit 1)