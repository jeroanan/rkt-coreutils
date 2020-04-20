#lang typed/racket

(provide sha1sum%)

(require/typed sha
                   [sha1  (-> Bytes Bytes)])

(require "util/program/shaprogram.rkt")

(sha-program "SHA1" sha1sum% sha1)
