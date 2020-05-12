#lang typed/racket/base

; Copyright 2020 David Wilson
; See COPYING for details

(provide gid->group-name
         get-user-groups)

(require typed/racket/class)

(require/typed "../../libc/grp.rkt"
               [get-getgrgid (-> Number (Instance Getgrgid%))]
               [get-getgrouplist (-> String Integer (Instance Getgrouplist%))])

(require "../typedef/getgrgid.rkt"
         "../typedef/getgrouplist.rkt")

;; Take a group id and return its name
(: gid->group-name (-> Integer String))
(define (gid->group-name gid)
  (let ([grgid  (get-getgrgid gid)])
    (send grgid get-name)))

;; Get all groups that the given user-name is a member of
(: get-user-groups (-> String (Listof Integer)))
(define (get-user-groups user-name)
  (let* ([group-counter (get-getgrouplist user-name 0)]
         [group-count (send group-counter get-number-of-groups)]
         [group-getter (get-getgrouplist user-name group-count)]
         [all-groups (send group-getter get-groups)])
    all-groups))
  
