#lang s-exp "util/program/file-by-file-processor-program.rkt"

; Copyright 2020 David Wilson
; See COPYING for licence details

(require racket/path)

(define help-text (list "Strip path from the given file."
                        "(execute FILES) -- Strip path from the given file"))

(file-by-file-processor-program basename%
                                help-text
                                #f
                                (λ (filename ip)
                                  (when (or (not already-processed-one?) process-multiple?)
                                    (begin
                                      (displayln (file-name-from-path (string->path filename)))
                                      (set! already-processed-one? #t))))
                                (λ ()
                                  (set! already-processed-one? #f))
                                (attribute public boolean process-multiple? #f))

(define already-processed-one? #f)