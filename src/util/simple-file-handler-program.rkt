#lang racket/base

(provide simple-file-handler-program
         simple-file-handler-program2)

; Copyright 2020 David Wilson
; See COPYING for details

(require racket/cmdline
         racket/list
         racket/class)

;; TODO: Remove this when no longer needed
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

(define-syntax-rule (simple-file-handler-program2 backend-loc backend-func)
  (begin
    (require "util/version.rkt")
    (require backend-loc)
    
    (define the-files (make-parameter (list)))

    (command-line
     #:argv (current-command-line-arguments)
     #:once-each
     [("-v" "--version") "display version information and exit" (print-version-text-and-exit)]
     #:args filename (unless (empty? filename) (the-files filename)))

    (define (do-it file)
      (backend-func file))
    
    (apply backend-func (the-files))))