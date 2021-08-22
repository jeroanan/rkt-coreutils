#lang racket/base ;s-exp "util/program/repl-program.rkt"

; Copyright 2020, 2021 David Wilson
; See COPYING for details

(provide nproc)

(require nproc)

(define nproc
  (λ ()
    (displayln (get-number-of-processors))))
