#lang racket

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

(require "util/version.rkt")

(define file-name (make-parameter ""))
(define number-of-lines (make-parameter 10))

(define (exit-with-error error-msg)
  (begin
    (displayln error-msg)
    (exit 1)))

(define (set-number-of-lines nl)
  (let ([i (string->number nl)])
    (if (false? i)
        (exit-with-error (format "invalid number of lines: '~a'" nl))
        (number-of-lines i))))      
                             
(command-line
  #:argv (current-command-line-arguments)
  #:once-each
  [("-n" "--lines") nl "print the first NUM lines instead of the first 10" (set-number-of-lines nl)]
  [("-v" "--version") "display version information and exit" (print-version-text-and-exit)]
  #:args filename (unless (empty? filename) (file-name (first filename))))

(let ([f (open-input-file (file-name) #:mode 'text )])
  (for ([i (number-of-lines)])
        (displayln (read-line f))))
