#lang typed/racket/base

; Copyright 2020 David Wilson
; See COPYING for details

(provide id%)

(require "../typedef/getpwuid.rkt"
         "util/gidutil.rkt"
         "util/util.rkt")

(require typed/racket/class
         racket/string)

(require/typed "../libc/unistd.rkt"
               [get-euid (-> Integer)])

(require/typed "../libc/pwd.rkt"
               [get-pwuid (-> Integer (Instance Getpwuid%))])

(define id%
  (class object%
    (super-new)
    
    (help-function (list "Print user information"
                               ""
                               "Methods:"
                               "(help) -- display this help message"
                               "(execute) -- display user information"))

    (define/public (execute)
      (let* ([uid (get-euid)]
             [pwd (get-pwuid uid)]
             [user-name (send pwd get-username)]
             [main-gid (send pwd get-gid)]             
             [all-gids (get-user-groups user-name)]
             [formatted-main-gid (format-group main-gid)]
             [formatted-gids (format-groups all-gids)]
             [output (format "uid=~a(~a) gid=~a groups=~a" uid user-name formatted-main-gid formatted-gids)])        
        (displayln output)))

    (: format-groups (-> (Listof Integer) String))
    (define/private (format-groups gids)
      (let ([formatted-entries (map format-group gids)])
        (string-join formatted-entries ",")))

    (: format-group (-> Integer String))
    (define (format-group gid)
      (let ([group-name (gid->group-name gid)])
        (format "~a(~a)" gid group-name)))))
      
