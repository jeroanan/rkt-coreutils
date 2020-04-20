#lang typed/racket

; Copyright 2020 David Wilson
; See COPYING for licence

(require "repl/sha1sum.rkt"
         "util/programs.rkt")

(simple-file-handler-program sha1sum%)
