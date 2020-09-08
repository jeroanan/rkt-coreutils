#lang typed/racket/base

; Copyright 2020 David Wilson
; See COPYING for licence details

(provide tac%)

(require typed/racket/class)

(require "util/help.rkt"
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
          (displayln l))))

    (file-by-file-processor file-handler)))
