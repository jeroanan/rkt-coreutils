#lang typed/racket

(provide sha384sum%)

(require/typed sha
                   [sha384  (-> Bytes Bytes)])

(require "util/program/shaprogram.rkt")

(sha-program "SHA384" sha384sum% sha384)
