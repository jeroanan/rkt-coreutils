#lang typed/racket/base

; Copyright 2020 David Wilson
; See COPYING for licence

(provide sha224sum%)

(require/typed sha
                   [sha224  (-> Bytes Bytes)])

(require "util/program/shaprogram.rkt")

(sha-program "SHA224" sha224sum% sha224)
