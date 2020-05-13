#lang typed/racket

; Copyright 2020 David Wilson
; See COPYING for details

(provide get-user-name-program)

(require "../../typedef/getpwuid.rkt"
         "../util.rkt")

(require/typed "../../libc/pwd.rkt"
               [get-pwuid (-> Number (Instance Getpwuid%))])

;; Define a program the calls get-uid-func and retrieves the username with the result
(define-syntax-rule (get-user-name-program type-name help-strings get-uid-func)
  (define type-name
  (class object%
    (super-new)

    (help-function help-strings)
    
    (define/public (execute)
      (let* ([uid (get-uid-func)]
             [getpwuid (get-pwuid uid)])
        (displayln (send getpwuid get-username)))))))