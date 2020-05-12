#lang typed/racket/base

(provide Getpwuid%)

(define-type Getpwuid%
  (Class
   [get-username (-> String)]
   [get-gid (-> Integer)]))