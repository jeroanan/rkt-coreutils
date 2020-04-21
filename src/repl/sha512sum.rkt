#lang typed/racket

(provide sha512sum%)

(require/typed sha
                   [sha512  (-> Bytes Bytes)])

(require "util/program/shaprogram.rkt")

(sha-program "SHA512" sha512sum% sha512)