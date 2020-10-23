#lang s-exp "repl-program.rkt"

; Copyright 2020 David Wilson
; See COPYING for details

(provide (all-from-out "repl-program.rkt"))
(provide get-user-name-program2)

(require "../../typedef/getpwuid.rkt"
         "../../libc/pwd.rkt")

(define-syntax-rule (get-user-name-program2 name get-uid-func)
  (begin
    (provide name)
    (define (name)
      (define uid (get-uid-func))
      (define getpwuid (get-pwuid uid))
      (displayln (send getpwuid get-username)))))
