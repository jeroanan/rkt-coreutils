#lang typed/racket/base

(require racket/cmdline
         typed/racket/class)

(require "repl/id.rkt"
         "util/version.rkt")

(command-line
  #:argv (current-command-line-arguments)
  #:once-each
  [("-v" "--version") "display version information and exit" (print-version-text-and-exit)])

(send (new id%) execute)
