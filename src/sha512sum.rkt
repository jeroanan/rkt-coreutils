#lang typed/racket

; Copyright 2020 David Wilson
; See COPYING for licence

(require "repl/sha512sum.rkt"
         "util/simple-file-handler-program.rkt")

(simple-file-handler-program sha512sum%)
