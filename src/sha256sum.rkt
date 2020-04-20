#lang typed/racket

; Copyright 2020 David Wilson
; See COPYING for licence

(require "repl/sha256sum.rkt"
         "util/programs.rkt")

(simple-file-handler-program sha256sum%)
