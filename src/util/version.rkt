#lang typed/racket/base

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

(define project-name "Rkt Coreutils")
(define copyright-year "2020")
(define version-number "0.1")
(define author "David Wilson")

(define (get-program-name)
  (let-values ([(p f _) (split-path (find-system-path 'run-file))])
    f))

(define (get-version-text)
  (let* ([program-name (get-program-name)]
         [line-1 (format "~a (~a) ~a" program-name project-name version-number)]
         [line-2 (format "Copyright (C) ~a ~a" copyright-year author)]
         [line-3 "License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>."]
         [line-4 "This is free software: you are free to change and redistribute it."]
         [line-5 "There is NO WARRANTY, to the extent permitted by law."])
    (format "~a\n~a\n~a\n~a\n~a\n" line-1 line-2 line-3 line-4 line-5)))

(define (print-version-text)
  (display (get-version-text)))

(define (print-version-text-and-exit)
  (begin
    (print-version-text)
    (exit 0)))

(provide project-name
         copyright-year
         version-number
         author
         get-version-text
         print-version-text
         print-version-text-and-exit)