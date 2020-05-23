#lang typed/racket/base

(provide Getpwnam%)

(define-type Getpwnam%
  (Class
   [get-gid (-> Integer)]))