#lang s-exp "util/program/file-by-file-processor-program.rkt"

; Copyright 2020 David Wilson
; See COPYING for details

(provide cksum)

(require cksum)

(define cksum
  (λ (f . fs)
    (define files (cons f fs))
    (process-file-by-file files file-processor null #f)))

(define (file-processor filename ip)
  (displayln (get-cksum filename)))


  
    
