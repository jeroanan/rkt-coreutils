#lang s-exp "util/frontend-program.rkt"

; Copyright 2020 David Wilson
; See COPYING for details

(require racket/cmdline
         typed/racket/class)

(require "repl/who.rkt")

(boolean-parameter show-heading #f)

(command-line
  #:argv (current-command-line-arguments)
  #:once-each
  [("-H" "--heading") "print line of column headings" (show-heading #t)]
  [("-v" "--version") "display version information and exit" (print-version-text-and-exit)])

(who #:show-header (show-heading))
