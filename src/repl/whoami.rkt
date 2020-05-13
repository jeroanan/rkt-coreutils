#lang typed/racket/base

; Copyright 2020 David Wilson
; See COPYING for details

(provide whoami%)

;; Include class so instances of whoami% can be made from the repl more easily
(require typed/racket/class)

(require "util/program/getusernameprogram.rkt")

(require/typed "libc/unistd.rkt"
               [get-euid (-> Integer)])

(define help-strings (list "Print effective userid"
                               ""
                               "Methods:"
                               "(help) -- display this help message"
                               "(execute) -- display the effective userid"))

(get-user-name-program whoami% help-strings get-euid)
