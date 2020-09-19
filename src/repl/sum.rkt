#lang s-exp "util/program/file-by-file-processor-program.rkt"

; Copyright 2020 David Wilson
; See COPYING for details

(require/typed "libc/sum.rkt"
               [get-bsd-sum (-> String (Listof (U String Integer)))]
               [get-sysv-sum (-> String (Listof(U String Integer)))])

(require "libc/sum.rkt")

(require typed/racket/class)

(define help-text (list
                    "Print sum for FILE"
                    "(execute FILE (string)) -- Print sum for FILE"
                    "(set-bsd-mode) -- Set use of BSD sum algorithm (default)"
                    "(set-sysv-mode) -- Set use of System V sum algorithm"))

(file-by-file-processor-program sum%
                                help-text
                                #f
                                (λ (filename ip)
                                   (displayln (get-sum filename)))
                                null
                                (define/public (set-bsd-mode)
                                  (set! current-mode bsd-mode))

                                (define/public (set-sysv-mode)
                                  (set! current-mode sysv-mode)))

(: bsd-mode Symbol)
(define bsd-mode 'BSD)

(: sysv-mode Symbol)
(define sysv-mode 'SYSV)

(: current-mode Symbol)
(define current-mode bsd-mode)

(: get-sum (-> String (Listof (U Integer String))))
(define (get-sum file)
  (if (eq? current-mode bsd-mode)
      (get-bsd-sum file)
      (get-sysv-sum file)))

