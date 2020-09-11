#lang s-exp "util/program/repl-program.rkt"

;; Copyright 2020 David Wilson
;; See COPYING for details

(provide groups%)

(require "typedef/getpwuid.rkt"
         "util/gidutil.rkt")

(require racket/string)

(require/typed "libc/unistd.rkt"
               [get-euid (-> Integer)])

(require/typed "libc/pwd.rkt"
               [get-pwuid (-> Number (Instance Getpwuid%))])

;; Groups: Print the given user's groups
(define groups%
  (class object%
    (super-new)

    (help-function "Print the names of the given user's groups"
                   (list "(execute user-name) -- Print the user-name's groups"))

    ;; Main program execution
    (on-execute-with-string user-name
      (let* ([un (get-username user-name)]             
             [the-groups (get-user-groups un)]             
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
