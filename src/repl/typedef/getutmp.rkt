#lang typed/racket/base

;; Copyright 2020 David Wilson
;; See COPYING for details

(provide Getutmp%)

;; Provide type definition for Getutmp as defined in libc.
(define-type Getutmp%
  (Class
   [start-utmp (-> Void)]
   [end-utmp (-> Void)]
   [next-utmp (-> Boolean)]
   [get-type (-> Integer)]
   [get-user (-> String)]
   [get-host (-> String)]
   [get-line (-> String)]
   [get-time (-> Integer)]))
