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
          

    