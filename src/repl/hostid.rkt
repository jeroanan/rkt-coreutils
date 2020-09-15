#lang s-exp "util/program/repl-program.rkt"

(provide hostid%)

(require/typed "libc/unistd.rkt"
               [get-hostid (-> Integer)])

(define hostid%
  (class object%
    (super-new)

    (help-function "Get current host id"
                   (list "(execute) -- display the current host id"))

    (on-execute-with-void
      (displayln (number->string (get-hostid) 16)))))
