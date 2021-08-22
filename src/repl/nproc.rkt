#lang racket/base 

; Copyright 2020, 2021 David Wilson
; See COPYING for details

(provide nproc)

(require nproc)

(define nproc
  (λ ()
    (get-number-of-processors)))
