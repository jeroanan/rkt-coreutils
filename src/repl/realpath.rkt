#lang s-exp "util/program/file-by-file-processor-program.rkt"

; Copyright 2020 David Wilson
; See COPYING for details

(require racket/path)

(define help-text (list "Display path for given FILES"
                        "(exec FILES) -- Display path for given FILES"))
  
(file-by-file-processor-program realpath%
                                help-text
                                #f
                                (λ (filename ip)
                                   (define path (string->path filename))
                                   (displayln (normalize-path path)))
                                null)

