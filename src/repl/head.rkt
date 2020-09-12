#lang s-exp "util/program/line-by-line-processor-program.rkt"

; Copyright 2020 David Wilson
; See COPYING for licence details

;; Head: Print the first lines of the given files
;; TODO: It seems that currently only the first file will be dealt with and then we exit. Need to be
;;       able to handle multiple files.


(provide head%)

(define help-text (list "Print the first lines of each provided file."
                        ""
                        "Methods"
                        "(execute FILES) -- display the first lines of FILES"
                        "(help) -- Display this help message"
                        ""
                        "Attributes"
                        "number-of-lines (integer)"))

(line-by-line-processor-program head%
                                help-text
                                (λ (x)
                                  (begin
                                    (displayln x)
                                    (set! counter (add1 counter))
                                    (when (eq? counter number-of-lines) (exit 0))))
                                (attribute public integer number-of-lines 10)
                                (attribute private integer counter 0))
