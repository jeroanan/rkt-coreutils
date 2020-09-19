#lang s-exp "util/program/repl-program.rkt"

; Copyright 2020 David Wilson
; See COPYING for details

(provide sum%)

(require typed/racket/class)

(require/typed "libc/sum.rkt"
               [get-bsd-sum (-> String (Listof (U String Integer)))]
               [get-sysv-sum (-> String (Listof(U String Integer)))])

(require "libc/sum.rkt")

(define sum%
  (class object%
    (super-new)

    (help-function "Print sum for FILE"
                   (list "(execute FILE (string)) -- Print sum for FILE"
                         "(set-bsd-mode) -- Set use of BSD sum algorithm (default)"
                         "(set-sysv-mode) -- Set use of System V sum algorithm"))

    (define/public (set-bsd-mode)
      (set! current-mode bsd-mode))

    (define/public (set-sysv-mode)
      (set! current-mode sysv-mode))

    (on-execute-with-string file
                            (displayln (get-sum file)))))

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

                   


