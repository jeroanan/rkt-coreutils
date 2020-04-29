#lang typed/racket/base

(provide Getpwuid%)

(define-type Getpwuid%
  (Class
   [get-username (-> String)]
   [get-groups (-> (Listof Integer))]
   [reset (-> Void)]))