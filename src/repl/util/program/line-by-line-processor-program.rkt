#lang s-exp "repl-program.rkt"

; Copyright 2020 David Wilson
; See COPYING for details

(provide (all-from-out "repl-program.rkt") 
         line-by-line-processor-program
         attribute)

(require (for-syntax  racket/list)
         (for-syntax racket/base)
         racket/format
         racket/list)

(require "../attribute.rkt")

(define-syntax (line-by-line-processor-program stx)    
  (syntax-case stx ()
    [(_ type-name help-text line-handler-body extras ...)    
   
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

             (on-execute-with-strings files
                                      (begin
                                        (if (empty? files)
                                            (process-stdin)
                                            (process-files files))))

             (define/private (process-files files)
               (for ([file-name files])
                 (let* ([f (open-input-file file-name #:mode 'text )])
                   (for ([l (in-lines f)])
                     (output-function l)))))

             (define/private (process-stdin)
               (let* ([r (read-line)]
                      [rs (~a r)])
                 (when (not (eof-object? r))
                   (output-function rs)
                   (process-stdin)))))))]))
