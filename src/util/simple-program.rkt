#lang typed/racket

; Copyright 2020 David Wilson
; See COPYING for details

(provide simple-program)

(require "version.rkt")

(define-syntax-rule (simple-program backend-loc type)
  (begin
    (require backend-loc)
    (define args (make-parameter (list)))

    (command-line
     #:argv (current-command-line-arguments)
     #:once-each
     [("-v" "--version") "display version information and exit" (print-version-text-and-exit)]
     #:args a (args a))

    (let ([repl-obj (new type)])
      (send repl-obj execute))))
