#lang typed/racket/base

(provide simple-file-handler-program)

(require racket/cmdline
         racket/list
         racket/class)

(define-syntax-rule (simple-file-handler-program dispatch-type)
  (begin
    (require "util/version.rkt")

    (: the-files (Parameter (Listof String)))
    (define the-files (make-parameter (list)))

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

    (let ([dispatch (new dispatch-type)])
      (send dispatch execute (get-the-files)))))
