#lang s-exp "util/program/getusernameprogram.rkt"

; Copyright 2020 David Wilson
; See COPYING for details

(provide whoami)

(require "libc/unistd.rkt")

(define help-strings (list "Print effective userid"
                               ""
                               "Methods:"
                               "(help) -- display this help message"
                               "(execute) -- display the effective userid"))

(get-user-name-program2 whoami get-euid)
