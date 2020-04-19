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

(require "repl/md5sum.rkt"
         "util/version.rkt")

(define the-files (make-parameter (list "")))

(define (set-the-files [s : (Pairof Any (Listof Any))])
  (let ([#{strings : (Listof String)} (map (λ (x) (format "~a" x)) s)])
    (the-files strings)))

(define (get-the-files)
  (map (λ ([x : String]) (format "~a" x)) (the-files)))

(command-line
  #:argv (current-command-line-arguments)
  #:once-each
  [("-v" "--version") "display version information and exit" (print-version-text-and-exit)]
  #:args filename (unless (empty? filename) (set-the-files filename)))

(let ([md5sum (new md5sum%)])
      (send md5sum execute (get-the-files)))

