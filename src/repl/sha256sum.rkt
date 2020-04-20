#lang typed/racket

(provide sha256sum%)

(require/typed sha
                   [sha256  (-> Bytes Bytes)])

(require "util/program/shaprogram.rkt")

(sha-program "SHA256" sha256sum% sha256)
