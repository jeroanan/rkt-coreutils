#lang typed/racket/base

; Copyright 2020 David Wilson
; See COPYING for details.

(require racket/cmdline
         racket/list
         racket/string)

(require "util/version.rkt")

(define the-string (make-parameter (list "")))

(define (set-the-string [s : (Pairof Any (Listof Any))])
  (let ([#{strings : (Listof String)} (map (λ (x) (format "~a" x)) s)])
    (the-string strings)))

(command-line
  #:argv (current-command-line-arguments)
  #:once-each
  [("-v" "--version") "display version information and exit" (print-version-text-and-exit)]
  #:args strings (unless (empty? strings) (set-the-string strings)))

(displayln (string-join (the-string) " "))
