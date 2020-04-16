#lang typed/racket

(provide Getpwuid%)

(define-type Getpwuid%
  (Class
   [get-username (-> String)]))