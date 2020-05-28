#lang s-exp "ffi.rkt"

; Copyright 2020 David Wilson
; See COPYING for details.

(provide get-load-avgs)

(require ffi/unsafe)

(define clib (ffi-lib #f))

(define _avgs (_array _double 3))

(c-function getloadavg clib _int "getloadavg" _avgs _int)

;; Get load averages for last 1, 5 and 15 minutes
(define (get-load-avgs)
  (let* ([avgs (malloc _avgs)]
         [r (getloadavg (ptr-ref avgs _avgs) 3)]
         [aout (ptr-ref avgs _avgs)])
    (list (array-ref aout 0)
          (array-ref aout 1)
          (array-ref aout 2))))
