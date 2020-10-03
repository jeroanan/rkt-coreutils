#lang s-exp "util/program/repl-program.rkt"

; Copyright 2020 David Wilson
; See COPYING for details

(provide uname%)

(require "libc/utsname.rkt")

(define uname%
  (class object%
    (super-new)

    (help-function "Print system information"
                  (list "(execute) -- Print system information"))

    (on-execute-with-void
     (displayln (uname-sysname)))))
