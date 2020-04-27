#lang typed/racket/base

; Copyright 2020 David Wilson
; See COPYING for details

(provide logname%)

(require typed/racket/class)

(require "util/program/getusernameprogram.rkt")

(require/typed "../libc/unistd.rkt"
               [get-uid (-> Integer)])

(define help-strings (list "Print logged-in userid"
                               ""
                               "Methods:"
                               "(help) -- display this help message"
                               "(execute) -- display logged-in userid"))

(get-user-name-program logname% help-strings get-uid)
