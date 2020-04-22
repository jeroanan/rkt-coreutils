#lang typed/racket

; Copyright 2020 David Wilson
; See COPYING for details

(require "repl/tac.rkt"
         "util/simple-file-handler-program.rkt")

(simple-file-handler-program tac%)
