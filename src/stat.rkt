#lang typed/racket/base

; Copyright 2020 David Wilson
; See COPYING for details

(require racket/cmdline
         racket/list
         typed/racket/class)

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
