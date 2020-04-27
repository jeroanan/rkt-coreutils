#lang typed/racket/base

(provide Getgrgid%)

(define-type Getgrgid%
  (Class
   [get-name (-> String)]))
