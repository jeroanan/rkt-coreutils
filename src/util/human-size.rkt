#lang typed/racket/base

; Copyright 2020 David Wilson
; See COPYING for details

(define (get-human-readable-size [byte-size : Integer]
                                 [unit-size : Integer]
                                 [unit-symbol : String])
  (let* ([whole-things (floor (/ byte-size unit-size))]
         [remainder (modulo byte-size unit-size)]
         [remainder-decimal
          (inexact->exact
           (ceiling
            (exact->inexact
             (* 10 (/ remainder unit-size)))))]
         [is-kilobytes? (eq? unit-size 1024)]
         [omit-decimal? (or is-kilobytes? (eq? remainder-decimal 0))]
         [whole-thing-string (number->string
                              (if
                               (and is-kilobytes? (> remainder-decimal 0))
                               (+ whole-things 1)
                               whole-things))]
         [decimal-string (string-append "." (number->string remainder-decimal))]
         [output (string-append
                  whole-thing-string
                  (if omit-decimal? "" decimal-string)
                  unit-symbol)])
    output))

(define (human-readable-byte-size [byte-size : Integer])
  (let* ([kilobyte 1024]
         [megabyte (* kilobyte 1024)]
         [gigabyte (* megabyte 1024)])
    (cond
      [(< byte-size kilobyte) (number->string byte-size)]
      [(< byte-size megabyte) (get-human-readable-size byte-size kilobyte "K")]
      [(and (>= byte-size megabyte) (< byte-size gigabyte))
       (get-human-readable-size byte-size megabyte "M")] 
      [else (get-human-readable-size byte-size gigabyte "G")])))

(provide human-readable-byte-size)