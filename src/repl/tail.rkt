#lang typed/racket/base

; Copyright 2020 David Wilson
; See COPYING for licence details

(provide tail%)

(require typed/racket/class
         racket/list)

(require "util/help.rkt"
         "util/file-by-file-processor.rkt")

(define tail%
  (class object%
    (super-new)

    (help-function 
      "Output the end of files."
      (list "(execute FILES) -- Display the last lines of FILES"))

    (: file-handler (-> (Listof String) Void))
    (define/private (file-handler file-contents)
      (let* ([reversed (reverse file-contents)]
             [take-elements (min 10 (length file-contents))]
             [top-x (take reversed take-elements)]
             [output (reverse top-x)])
        (for ([l output])
          (displayln l))))

    (file-by-file-processor file-handler)))

