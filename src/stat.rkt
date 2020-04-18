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

(require)

(require "util/version.rkt"
         "repl/stat.rkt")

(require/typed racket/date
               [date-display-format ( -> Symbol Void)]
               [date->string (-> date Boolean String)])

(define the-files (make-parameter (list "")))

(define (get-path [filename : String])
  (path->complete-path (string->path filename)))

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

(send (new stat%) execute (get-the-files))
