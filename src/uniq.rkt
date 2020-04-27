#lang typed/racket/base

; Copyright 2020 David Wilson
; See COPYING for licence

(require "repl/uniq.rkt"
         "util/simple-file-handler-program.rkt")

(simple-file-handler-program uniq%)
