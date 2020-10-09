#lang s-exp "util/program/file-by-file-processor-program.rkt"

; Copyright 2020 David Wilson
; See COPYING for licence details

(require racket/path)

(provide basename)

(define help-text (list "Strip path from the given file."
                        "(execute FILES) -- Strip path from the given file"))

(define already-processed-one? #f)
(define process-multiple? #f)

(define basename
      (λ (#:process-multiple? [pm #f] f . fs)
        (define files (cons f fs))
        (set! process-multiple? pm)
        (process-file-by-file files file-processor finished-processing #f)))

(define (file-processor filename _)
  (when (or (not already-processed-one?) process-multiple?)
    (begin
      (displayln (file-name-from-path (string->path filename)))
      (set! already-processed-one? #t))))

(define (finished-processing)
        (set! already-processed-one? #f))

