#lang s-exp "util/program/file-by-file-processor-program.rkt"

; Copyright 2020 David Wilson
; See COPYING for licence details

(require racket/port)

(define help-text (list "Concatenate files and print on standard output in reverse."
                        "(execute FILES) -- Concatenate and print FILES in reverse"))

(file-by-file-processor-program tac%
                                help-text
                                #t
                                file-handler
                                null)

(define (file-handler filename stream)
  (let* ([file-contents (port->lines stream)]
        [reversed (reverse file-contents)])
    (for ([l reversed])
      (displayln l))))