#lang typed/racket/base

; Copyright 2020 David Wilson
; see COPYING for licence

(require "repl/md5sum.rkt"
         "util/simple-file-handler-program.rkt")

(simple-file-handler-program md5sum%)
