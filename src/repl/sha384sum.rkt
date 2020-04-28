#lang typed/racket/base

; Copyright 2020 David Wilson
; See COPYING for licence

(provide sha384sum%)

(require typed/racket/class)

(require/typed sha
                   [sha384  (-> Bytes Bytes)])

(require "util/program/shaprogram.rkt")

(sha-program "SHA384" sha384sum% sha384)
