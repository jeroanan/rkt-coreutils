#lang typed/racket/base

; Copyright 2020 David Wilson
; See COPYING for details

(provide human-readable-byte-size)

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
         [omit-decimal? (and is-kilobytes? (> whole-things 9))]
         [whole-number-to-display (if omit-decimal? (add1 whole-things) whole-things)]
         [decimal-string (if omit-decimal? "" (format ".~a" remainder-decimal))]
         [output (format
                  "~a~a~a"
                  whole-number-to-display
                  decimal-string
                  unit-symbol)])
    output))

(define kilobyte-bytes 1024)
(define megabyte-bytes (* kilobyte-bytes 1024))
(define gigabyte-bytes (* megabyte-bytes 1024))
          
(define (human-readable-byte-size [number-of-bytes : Integer])
  (define under-a-kilobyte? (< number-of-bytes kilobyte-bytes))
  (define under-a-megabyte? (< number-of-bytes megabyte-bytes))
  (define under-a-gigabyte? (< number-of-bytes gigabyte-bytes))
  
  (cond
    [under-a-kilobyte? (number->string number-of-bytes)]
    [under-a-megabyte? (get-human-readable-size number-of-bytes kilobyte-bytes "K")]
    [under-a-gigabyte? (get-human-readable-size number-of-bytes megabyte-bytes "M")] 
    [else (get-human-readable-size number-of-bytes gigabyte-bytes "G")]))

