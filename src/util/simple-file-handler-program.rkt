#lang racket/base

(provide simple-file-handler-program)

; Copyright 2020 David Wilson
; See COPYING for details

(require racket/cmdline
         racket/list
         racket/class)

(define-syntax-rule (simple-file-handler-program backend-loc dispatch-type)
  (begin
    (require "util/version.rkt")
    (require backend-loc)
    
    (define the-files (make-parameter (list)))

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

    (let ([dispatch (new dispatch-type)])
      (send dispatch execute (get-the-files)))))
