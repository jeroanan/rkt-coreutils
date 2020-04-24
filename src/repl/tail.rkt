#lang typed/racket

; Copyright 2020 David Wilson
; See COPYING for licence details

(provide tail%)

(require "util/util.rkt"
         "util/file-by-file-processor.rkt")

(define tail%
  (class object%
    (super-new)

    (help-function (list "Output the end of files."
                         ""
                         "Methods:"
                         "(execute FILES) -- display the last lines of FILES"))

    (: file-handler (-> (Listof String) Void))
    (define/private (file-handler file-contents)
      (let* ([reversed (reverse file-contents)]
             [take-elements (min 10 (length file-contents))]
             [top-x (take reversed take-elements)]
             [output (reverse top-x)])
        (for ([l output])
          (displayln l))))

    (file-by-file-processor file-handler)))

