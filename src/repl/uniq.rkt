#lang typed/racket

; Copyright 2020 David Wilson
; See COPYING for licence

(provide uniq%)

(require "util/util.rkt"
         "util/line-by-line-processor.rkt"
         "../util/member.rkt"
         "../util/stringutil.rkt")

(define uniq%
  (class object%
    (super-new)

    (string-attribute previous-line "")
    (boolean-attribute first-line? #t)
    
    (define help-text (list "Display uniq lines in FILE"
                            ""
                            "Methods:"
                            "(execute FILE) -- Display uniq lines in FILE"))
    (help-function help-text)

    (line-by-line-processor process-line)
    
    (: process-line (-> String Void))
    (define (process-line line)
      (when (or first-line? (string<>? line previous-line))
        (begin
          (set! first-line? #f)
          (set! previous-line line)
          (displayln line))))))