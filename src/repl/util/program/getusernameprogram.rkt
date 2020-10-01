#lang s-exp "repl-program.rkt"

; Copyright 2020 David Wilson
; See COPYING for details

(provide (all-from-out "repl-program.rkt"))
(provide get-user-name-program)

(require "../../typedef/getpwuid.rkt"
         "../../libc/pwd.rkt")

;; Define a program the calls get-uid-func and retrieves the username with the result
(define-syntax-rule (get-user-name-program type-name help-strings get-uid-func)
  (define type-name
  (class object%
    (super-new)

    (help-function help-strings)
    
    (on-execute-with-void 
      (let* ([uid (get-uid-func)]
             [getpwuid (get-pwuid uid)])
        (displayln (send getpwuid get-username)))))))
