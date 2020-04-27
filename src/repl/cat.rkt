#lang typed/racket/base

; Copyright 2020 David Wilson
; See COPYING for details

(provide cat%)

(require "util/program/line-by-line-processor-program.rkt")

(define help-text (list "Concatenate files and print on standard output."
                        ""
                        "Methods:"
                        "(help) -- display this help message"
                        "(execute FILES) -- concatenate and print FILES"))

(line-by-line-processor-program cat% help-text (λ (x) x))
