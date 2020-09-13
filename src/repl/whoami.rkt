#lang s-exp "util/program/getusernameprogram.rkt"

; Copyright 2020 David Wilson
; See COPYING for details

(provide whoami%)

(require/typed "libc/unistd.rkt"
               [get-euid (-> Integer)])

(define help-strings (list "Print effective userid"
                               ""
                               "Methods:"
                               "(help) -- display this help message"
                               "(execute) -- display the effective userid"))

(get-user-name-program whoami% help-strings get-euid)
