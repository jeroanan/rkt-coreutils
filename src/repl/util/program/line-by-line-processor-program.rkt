#lang s-exp "repl-program.rkt"

; Copyright 2020 David Wilson
; See COPYING for details

(provide (all-from-out "repl-program.rkt") 
         line-by-line-processor-program
         line-by-line-processor
         process-line-by-line
         attribute)

(require (for-syntax  racket/list)
         (for-syntax racket/base)
         racket/format
         racket/list)

(require "../attribute.rkt")

(define-syntax (line-by-line-processor-program stx)    
  (syntax-case stx ()
    [(_ type-name help-text line-handler-body finished-body extras ...)    
   
     #'(begin
         (provide type-name)
         (require "util/help.rkt")
         (define type-name
           (class object%
             (super-new)

             (help-function help-text)

             extras ...
              
             (define (output-function x)
               (line-handler-body x))

             (define (finished)
               (unless (null? finished-body)
                 (finished-body)))

             (on-execute-with-strings files
                                      (begin
                                        (if (empty? files)
                                            (process-stdin)
                                            (process-files files))))

             (define/private (process-files files)
               (for ([file-name files])
                 (let* ([f (open-input-file file-name #:mode 'text )])
                   (for ([l (in-lines f)])
                     (output-function l))
                   (finished))))

             (define/private (process-stdin)
               (let* ([r (read-line)]
                      [rs (~a r)])
                 (when (not (eof-object? r))
                   (output-function rs)
                   (process-stdin)))))))]))

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