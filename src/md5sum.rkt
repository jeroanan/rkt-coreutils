#lang typed/racket

; Copyright 2020 David Wilson
; see COPYING for licence

(require "repl/md5sum.rkt"
         "util/programs.rkt")

(simple-file-handler-program md5sum%)
