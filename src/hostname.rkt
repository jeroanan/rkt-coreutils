#lang s-exp "util/frontend-program.rkt"

;; Copyright 2020 David Wilson
;; see COPYING for details

;; hostname.rkt: Command-line front-end for repl/hostname.rkt
(require "repl/hostname.rkt")
(hostname)
#;(simple-program "repl/hostname.rkt" hostname%)


