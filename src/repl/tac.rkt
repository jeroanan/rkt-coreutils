#lang typed/racket

; Copyright 2020 David Wilson
; See COPYING for licence details

(provide tac%)

(require "util/util.rkt"
         "util/file-by-file-processor.rkt")

(define tac%
  (class object%
    (super-new)

    (help-function (list "Concatenate files and print on standard output in reverse."
                            ""
                            "Methods:"
                            "(help) -- display this help message"
                            "(execute FILES) -- concatenate and print FILES in reverse"))
    
    (: file-handler (-> (Listof String) Void))
    (define/private (file-handler file-contents)
      (let ([reversed (reverse file-contents)])
        (for ([l reversed])
          (display l))))

    (file-by-file-processor file-handler)))
