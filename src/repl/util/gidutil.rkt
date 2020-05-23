#lang typed/racket/base

; Copyright 2020 David Wilson
; See COPYING for details

(provide gid->group-name
         get-user-groups)

(require typed/racket/class)

(require/typed "../libc/grp.rkt"
               [get-getgrgid (-> Number (Instance Getgrgid%))]
               [get-group-list (-> String Number (Listof Integer))])

(require/typed "../libc/pwd.rkt"
               [get-pwnam (-> String (Instance Getpwnam%))])

(require "../typedef/getgrgid.rkt"
         "../typedef/getpwnam.rkt")

;; Take a group id and return its name
(: gid->group-name (-> Integer String))
(define (gid->group-name gid)
  (let ([grgid  (get-getgrgid gid)])
    (send grgid get-name)))

;; Get all groups that the given user-name is a member of
(: get-user-groups (-> String (Listof Integer)))
(define (get-user-groups user-name)
  (let* ([pwnam (get-pwnam user-name)]
         [primary-gid (send pwnam get-gid)]
         [the-groups (get-group-list user-name primary-gid)])
    the-groups))
  
