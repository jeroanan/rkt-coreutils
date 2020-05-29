#lang s-exp "util/frontend-program.rkt"

; Copyright 2020 David Wilson
; See COPYING for details.

(require racket/cmdline
         racket/list
         racket/string)

(string-list-parameter the-string)

(command-line
  #:argv (current-command-line-arguments)
  #:once-each
  [("-v" "--version") "display version information and exit" (print-version-text-and-exit)]
  #:args strings (unless (empty? strings) (set-the-string strings)))

(displayln (string-join (the-string) " "))
