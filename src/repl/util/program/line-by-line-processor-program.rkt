#lang s-exp "repl-program.rkt"

; Copyright 2020 David Wilson
; See COPYING for details

(provide (all-from-out "repl-program.rkt") 
         line-by-line-processor-program
         attribute
         lh)

(require (for-syntax  racket/list)
         (for-syntax racket/base))

(require typed/racket/class)

(define-syntax (attribute stx)
  (syntax-case stx ()
    [(_ access data-type name default)     
     (with-syntax ([declaration
                    (datum->syntax stx
                                   (string->symbol (format "~a-~a-attribute"
                                                           (syntax->datum #'access)
                                                           (syntax->datum #'data-type))))])
       #'(declaration name default))]))

(define-syntax (lh stx)
  (syntax-case stx ()
    [(_ body ...)
     #'(begin
         (: line-handler (-> String Void))
         (define (line-handler line)
           body ...))]))

(define-syntax (line-by-line-processor-program stx)    
  (syntax-case stx ()
    [(_ type-name help-text line-handler-body extras ...)
     
     
         #'(begin
             (require "util/help.rkt"
                      "util/line-by-line-processor.rkt")
             (define type-name
               (class object%
                 (super-new)

                 (help-function help-text)

                 extras ...
              
                 (: output-function (-> String Void))
                 (define (output-function x)
                   (line-handler-body x))
              
                 (line-by-line-processor output-function))))]))
  