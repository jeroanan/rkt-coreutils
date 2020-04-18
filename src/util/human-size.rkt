#lang typed/racket

; Copyright 2020 David Wilson

;This program is free software: you can redistribute it and/or modify
;it under the terms of the GNU General Public License as published by
;the Free Software Foundation, either version 3 of the License, or
;(at your option) any later version.
;
;This program is distributed in the hope that it will be useful,
;but WITHOUT ANY WARRANTY; without even the implied warranty of
;MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;GNU General Public License for more details.
;
;You should have received a copy of the GNU General Public License
;along with this program.  If not, see <https://www.gnu.org/licenses/>.

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