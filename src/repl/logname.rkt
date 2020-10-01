#lang s-exp "util/program/getusernameprogram.rkt"

; Copyright 2020 David Wilson
; See COPYING for details

(provide logname%)

(require "util/program/getusernameprogram.rkt"
         "libc/unistd.rkt")

(define help-strings (list "Print logged-in userid"
                               ""
                               "Methods:"
                               "(help) -- display this help message"
                               "(execute) -- display logged-in userid"))

(get-user-name-program logname% help-strings get-uid)
