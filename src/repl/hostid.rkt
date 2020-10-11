#lang s-exp "util/program/repl-program.rkt"

(provide hostid)

(require "libc/unistd.rkt")

    #;(help-function "Get current host id"
                   (list "(execute) -- display the current host id"))
(define hostid
  (λ ()
    (displayln (number->string (get-hostid) 16))))
   
