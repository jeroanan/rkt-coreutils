#lang s-exp "util/program/file-by-file-processor-program.rkt"

; Copyright 2020 David Wilson
; See COPYING for details

(provide sum)

(require bsd-sysv-checksum)

(require racket/class)

(define help-text (list
                    "Print sum for FILE"
                    "(execute FILE (string)) -- Print sum for FILE"
                    "(set-bsd-mode) -- Set use of BSD sum algorithm (default)"
                    "(set-sysv-mode) -- Set use of System V sum algorithm"))

(define sum
  (λ (#:sum-mode [m bsd-mode] f . fs)
    (when (not (or (eq? m bsd-mode) (eq? m sysv-mode))) error("sum-mode myst be 'BSD or 'SYSV"))
    (define files (cons f fs))
    (process-file-by-file files file-processor null #f)))

(define (file-processor filename ip)  
  (displayln (get-sum filename)))

(define bsd-mode 'BSD)
(define sysv-mode 'SYSV)

(define current-mode bsd-mode)

(define (get-sum file)  
  (if (eq? current-mode bsd-mode)
      (get-bsd-sum file)
      (get-sysv-sum file)))