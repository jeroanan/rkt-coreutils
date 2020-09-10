#lang s-exp "util/repl-program.rkt"

; Copyright 2020 David Wilson
; See COPYING for licence details

(provide tac%)

(require "util/file-by-file-processor.rkt")

(define tac%
  (class object%
    (super-new)

    (help-function 
      "Concatenate files and print on standard output in reverse."
      (list "(execute FILES) -- Concatenate and print FILES in reverse"))
    
    (: file-handler (-> (Listof String) Void))
    (define/private (file-handler file-contents)
      (let ([reversed (reverse file-contents)])
        (for ([l reversed])
          (displayln l))))

    (file-by-file-processor file-handler)))
