#lang typed/racket/base

; Copyright 2020 David Wilson
; See COPYING for licence details

(provide Getgrouplist%)

(define-type Getgrouplist%
  (Class
   [get-next-group-id (-> Integer)]
   [get-number-of-groups (-> Integer)]
   [get-groups (-> (Listof Integer))]))
