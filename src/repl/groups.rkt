#lang typed/racket/base

; Copyright 2020 David Wilson
; See COPYING for details

(provide groups%)

(require "util/util.rkt"
         "../typedef/getgrouplist.rkt"
         "../typedef/getgrgid.rkt"
         "../util/member.rkt")

(require typed/racket/class
         racket/string)

(require/typed "../libc/grp.rkt"
               [get-getgrouplist (-> String Number (Instance Getgrouplist%))]
               [get-getgrgid (-> Integer (Instance Getgrgid%))])

(define groups%
  (class object%
    (super-new)

    (help-function (list "Print the names of the given user's groups"
                         ""
                         "Methods:"                         
                         "(execute user-name) -- Print the user-name's groups" 
                         "(help) -- display this help message"))

    (: gid->group-name (-> Integer String))
    (define/private (gid->group-name gid)
      (let ([getgrgid (get-getgrgid gid)])
        (send getgrgid get-name)))
    
    (: execute (-> String Void))
    (define/public (execute user-name)
      (let* ([group-counter (get-getgrouplist user-name 0)]
             [number-of-groups (send group-counter get-number-of-groups)]
             [group-number-getter (get-getgrouplist user-name number-of-groups)]
             [the-groups (send group-number-getter get-groups)]
             [group-names (map (λ ([x : Integer]) (gid->group-name x)) the-groups)])
      (displayln (string-join group-names))))))
