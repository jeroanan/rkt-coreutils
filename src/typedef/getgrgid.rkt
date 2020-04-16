#lang typed/racket

(provide Getgrgid%)

(define-type Getgrgid%
  (Class
   [get-name (-> String)]))
