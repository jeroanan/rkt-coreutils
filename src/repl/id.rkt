#lang s-exp "util/program/repl-program.rkt"

; Copyright 2020 David Wilson
; See COPYING for details

(provide id%)

(require "typedef/getpwuid.rkt"
         "util/gidutil.rkt")

(require racket/string)

(require/typed "libc/unistd.rkt"
               [get-euid (-> Integer)])

(require/typed "libc/pwd.rkt"
               [get-pwuid (-> Integer (Instance Getpwuid%))])

;; id - Print user information
(define id%
  (class object%
    (super-new)
    
    (help-function 
      "Print user information"
      (list "(execute) -- display user information"))

    ;; Main program execution
    (on-execute-with-void
      (let* ([uid (get-euid)]
             [pwd (get-pwuid uid)]
             [user-name (send pwd get-username)]
             [main-gid (send pwd get-gid)]             
             [all-gids (get-user-groups user-name)]
             [formatted-main-gid (format-group main-gid)]
             [formatted-gids (format-groups all-gids)]
             [output (format "uid=~a(~a) gid=~a groups=~a"
                             uid
                             user-name
                             formatted-main-gid
                             formatted-gids)])        
        (displayln output)))

    ;;; Joins list of group ids into a comma-separated list of formatted groups
    (: format-groups (-> (Listof Integer) String))
    (define/private (format-groups gids)
      (let ([formatted-entries (map format-group gids)])
        (string-join formatted-entries ",")))

    ;; Take a groupid and format it with its name.
    (: format-group (-> Integer String))
    (define (format-group gid)
      (let ([group-name (gid->group-name gid)])
        (format "~a(~a)" gid group-name)))))
      
