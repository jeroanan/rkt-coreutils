#lang typed/racket/base

; Copyright 2020 David Wilson
; See COPYING for details

(require typed/racket/class)

(require "../typedef/stat.rkt")
         
(define-syntax-rule (get-permission-oct name r w x)
  (define (name [stat : (Instance Stat%)])
    (let ([has-r (λ () (if (send stat r) 4 0))]
          [has-w (λ () (if (send stat w) 2 0))]
          [has-x (λ () (if (send stat x) 1 0))])
    (number->string (+ (has-r) (has-w) (has-x))))))

(get-permission-oct get-owner-permission-oct get-owner-has-r? get-owner-has-w? get-owner-has-x?)
(get-permission-oct get-group-permission-oct get-group-has-r? get-group-has-w? get-group-has-x?)
(get-permission-oct get-other-permission-oct get-other-has-r? get-other-has-w? get-other-has-x?)

(define (get-mode-oct-str [stat : (Instance Stat%)])
  (format "0~a~a~a"
          (get-owner-permission-oct stat)
          (get-group-permission-oct stat)
          (get-other-permission-oct stat)))

(provide get-mode-oct-str)
          

    