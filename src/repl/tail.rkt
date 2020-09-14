#lang s-exp "util/program/file-by-file-processor-program.rkt"

; Copyright 2020 David Wilson
; See COPYING for licence details

(require racket/list
         racket/port)

(define help-text (list "Output the end of files."
                        "(execute FILES) -- Display the last lines of FILES"))

(file-by-file-processor-program tail%
                                help-text
                                #t
                                file-handler
                                null)

(: file-handler (-> String Input-Port Void))
(define (file-handler filename stream)
  (let* ([file-contents (port->lines stream)]
         [reversed (reverse file-contents)]
         [take-elements (min 10 (length file-contents))]
         [top-x (take reversed take-elements)]
         [output (reverse top-x)])
    (for ([l output])
      (displayln l))))
