#lang racket/base

; Copyright 2020 David Wilson
; See COPYING for details

(require racket/cmdline
         racket/list
         typed/racket/class
         racket/date)

(require "util/version.rkt"
         "repl/stat.rkt")

(define the-files (make-parameter (list "")))

(define (get-path filename)
  (path->complete-path (string->path filename)))

(define (set-the-files s)
  (let ([strings (map (λ (x) (format "~a" x)) s)])
    (the-files strings)))

(define (get-the-files)
  (map (λ (x) (format "~a" x)) (the-files)))

(command-line
  #:argv (current-command-line-arguments)
  #:once-each
  [("-v" "--version") "display version information and exit" (print-version-text-and-exit)]
  #:args filename (unless (empty? filename) (set-the-files filename)))

;;(send (new stat%) execute (get-the-files))
(stat (get-the-files))
