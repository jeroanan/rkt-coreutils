#lang s-exp "util/program/line-by-line-processor-program.rkt"

; Copyright 2020 David Wilson
; See COPYING for licence details

;; Head: Print the first lines of the given files
(provide head)

(define counter 0)
(define nl 10)

(define head
  (λ (#:n [number-of-lines 10] f . fs)
    (begin
      (set! nl number-of-lines)
      (define files (cons f fs))
      (process-line-by-line files process-line end-of-file))))

(define (process-line line)
  (when (< counter nl) (displayln line))
  (set! counter (add1 counter)))

(define (end-of-file)
  (set! counter 0))
