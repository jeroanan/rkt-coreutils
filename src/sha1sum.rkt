#lang typed/racket/base

; Copyright 2020 David Wilson
; See COPYING for licence

(require "repl/sha1sum.rkt"
         "util/simple-file-handler-program.rkt")

(simple-file-handler-program sha1sum%)
