#lang s-exp "repl-program.rkt"

; Copyright 2020 David Wilson
; See COPYING for details

(provide (all-from-out "repl-program.rkt") 
         line-by-line-processor)

(require (for-syntax  racket/list)
         (for-syntax racket/base)
         racket/format
         racket/list)

(define-syntax-rule (line-by-line-processor name line-processor eof-processor)
  (begin
    (provide name)
    (define name
      (λ (f . fs)
        (define files (cons f fs))
        (process-line-by-line files line-processor eof-processor)))))
    
(define (process-line-by-line files line-processor-function end-of-file-function)
         (if (empty? files)
             (process-stdin line-processor-function end-of-file-function)
             (process-files files line-processor-function end-of-file-function)))

(define (process-files files lp-function eof-function)
  (for ([file-name files])
    (let* ([f (open-input-file file-name #:mode 'text )])
      (for ([l (in-lines f)])
        (lp-function l))
      (unless (null? eof-function) (eof-function)))))

(define (process-stdin lp-function eof-function)
  (let* ([r (read-line)]
         [rs (~a r)])
    (when (not (eof-object? r))
      (lp-function rs)
      (process-stdin))
    (eof-function)))