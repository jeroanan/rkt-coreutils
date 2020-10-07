#lang racket/base

; Copyright 2020 David Wilson
; See COPYING for details

(require racket/bool
         racket/cmdline
         racket/list
         typed/racket/class)

(require "util/version.rkt"
         "repl/head.rkt")

(define the-files (make-parameter (list)))

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
  [("-n" "--lines") nl "print the first NUM lines instead of the first 10"
                    (set-number-of-lines (format "~a" nl))]
  [("-v" "--version") "display version information and exit" (print-version-text-and-exit)]
  #:args filename (unless (empty? filename) (the-files filename)))

(define (do-head x)
  (head #:n (number-of-lines) x))

(apply do-head (the-files))
