#lang typed/racket/base

(provide sha1sum%)

(require typed/racket/class)

(require/typed sha
                   [sha1  (-> Bytes Bytes)])

(require "util/program/shaprogram.rkt")

(sha-program "SHA1" sha1sum% sha1)
