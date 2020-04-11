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

(define-type stat%
  (Class 
   [get-owner-has-rwx? (-> Boolean)]
   [get-owner-has-r? (-> Boolean)]
   [get-owner-has-w? (-> Boolean)]
   [get-owner-has-x? (-> Boolean)]
   [get-group-has-rwx? (-> Boolean)]
   [get-group-has-r? (-> Boolean)]
   [get-group-has-w? (-> Boolean)]
   [get-group-has-x? (-> Boolean)]
   [get-other-has-rwx? (-> Boolean)]
   [get-other-has-r? (-> Boolean)]
   [get-other-has-w? (-> Boolean)]
   [get-other-has-x? (-> Boolean)]
   [get-is-directory? (-> Boolean)]))

(define (get-file-type-char [stat : (Instance stat%)])
  (cond
    [(send stat get-is-directory?) "d"]
    [else "-"]))

(define-syntax-rule (get-permissions-mode name rwx r w x)
  (define (name [stat : (Instance stat%)])
    (if (send stat rwx)
        "rwx"
        (let ([r-flag (if (send stat r) "r" "-")]
              [w-flag (if (send stat w) "w" "-")]
              [x-flag (if (send stat x) "x" "-")])
          (string-append r-flag w-flag x-flag)))))
  
(get-permissions-mode get-owner-mode get-owner-has-rwx? get-owner-has-r? get-owner-has-w? get-owner-has-x?)
(get-permissions-mode get-group-mode get-group-has-rwx? get-group-has-r? get-group-has-w? get-group-has-x?)
(get-permissions-mode get-other-mode get-other-has-rwx? get-other-has-r? get-other-has-w? get-other-has-x?)

(define (get-mode-str [stat : (Instance stat%)])
  (let ([file-type (get-file-type-char stat)]
        [owner-mode (get-owner-mode stat)]
        [group-mode (get-group-mode stat)]
        [other-mode (get-other-mode stat)])
  (string-append file-type owner-mode group-mode other-mode)))

(provide get-mode-str)