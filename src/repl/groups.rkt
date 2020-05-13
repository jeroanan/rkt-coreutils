#lang typed/racket/base

;; Copyright 2020 David Wilson
;; See COPYING for details

(provide groups%)

(require "util/util.rkt"
         "typedef/getgrouplist.rkt"
         "typedef/getgrgid.rkt"
         "typedef/getpwuid.rkt")

(require typed/racket/class
         racket/string)

(require/typed "libc/grp.rkt"
               [get-getgrouplist (-> String Number (Instance Getgrouplist%))]
               [get-getgrgid (-> Integer (Instance Getgrgid%))])

(require/typed "libc/unistd.rkt"
               [get-euid (-> Integer)])

(require/typed "libc/pwd.rkt"
               [get-pwuid (-> Number (Instance Getpwuid%))])

;; Groups: Print the given user's groups
(define groups%
  (class object%
    (super-new)

    (help-function (list "Print the names of the given user's groups"
                         ""
                         "Methods:"                         
                         "(execute user-name) -- Print the user-name's groups" 
                         "(help) -- display this help message"))

    ;; Takes a group id and returns its name
    (: gid->group-name (-> Integer String))
    (define/private (gid->group-name gid)
      (let ([getgrgid (get-getgrgid gid)])
        (send getgrgid get-name)))

    ;; Main program execution
    (: execute (-> String Void))
    (define/public (execute user-name)
      (let* ([un (get-username user-name)]
             [group-counter (get-getgrouplist un 0)]
             [number-of-groups (send group-counter get-number-of-groups)]
             [group-number-getter (get-getgrouplist un number-of-groups)]
             [the-groups (send group-number-getter get-groups)]
             [group-names (map (λ ([x : Integer]) (gid->group-name x)) the-groups)])        
        (displayln (string-join group-names))))

    ;; Return either the given username or, if it's blank, the current user's username
    (: get-username (-> String String))
    (define/private (get-username user-name)
      (if (string=? "" user-name)
          (let* ([uid (get-euid)]
                 [pwuid (get-pwuid uid)])
            (send pwuid get-username))
          user-name))))
