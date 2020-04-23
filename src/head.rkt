#lang typed/racket

; Copyright 2020 David Wilson
; See COPYING for details

(require "util/version.rkt"
         "repl/head.rkt")

(: the-files (Parameterof (Listof String)))
(define the-files (make-parameter (list)))

(: number-of-lines (Parameterof Integer))
(define number-of-lines (make-parameter 10))

(: get-number-of-lines (-> Integer))
(define (get-number-of-lines)
  (number-of-lines))

(define (exit-with-error error-msg)
  (begin
    (displayln error-msg)
    (exit 1)))

(define (set-number-of-lines [nl : String])
  (let ([i (assert (string->number nl) exact-integer?)])
    (if (false? i)
        (exit-with-error (format "invalid number of lines: '~a'" nl))
        (number-of-lines i))))      

(define (set-the-files [s : (Pairof Any (Listof Any))])
  (let ([#{strings : (Listof String)} (map (λ (x) (format "~a" x)) s)])
    (the-files strings)))

(define (get-the-files)
  (map (λ ([x : String]) (format "~a" x)) (the-files)))

(command-line
  #:argv (current-command-line-arguments)
  #:once-each
  [("-n" "--lines") nl "print the first NUM lines instead of the first 10" (set-number-of-lines (format "~a" nl))]
  [("-v" "--version") "display version information and exit" (print-version-text-and-exit)]
  #:args filename (unless (empty? filename) (set-the-files filename)))

(let ([head (new head%)])
  (send head set-number-of-lines (number-of-lines))
  (send head execute (get-the-files)))