#lang typed/racket

; Copyright 2020 David Wilson
; See COPYING for details

(provide line-by-line-processor-program)

(define-syntax-rule (line-by-line-processor-program type-name help-text line-function)  
  (begin
    (require "util/util.rkt"
             "util/line-by-line-processor.rkt")
    
    (define type-name
      (class object%
        (super-new)

        (help-function help-text)

        (: output-function (-> String Void))
        (define/private (output-function x)
          (displayln (line-function x)))
        
        (line-by-line-processor output-function)))))