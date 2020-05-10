#lang typed/racket/base

; Copyright 2020 David Wilson
; See COPYING for details

(require racket/cmdline
         typed/racket/class)

(require "repl/df.rkt"
         "util/version.rkt")

(command-line
  #:argv (current-command-line-arguments)
  #:once-each
  [("-v" "--version") "display version information and exit" (print-version-text-and-exit)])

(let ([d (new df%)])
  (send d execute))