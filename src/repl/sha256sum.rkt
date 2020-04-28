#lang typed/racket/base

; Copyright 2020 David Wilson
; See COPYING for licence

(provide sha256sum%)

(require typed/racket/class)

(require/typed sha
                   [sha256  (-> Bytes Bytes)])

(require "util/program/shaprogram.rkt")

(sha-program "SHA256" sha256sum% sha256)
