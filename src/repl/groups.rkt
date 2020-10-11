#lang s-exp "util/program/repl-program.rkt"

;; Copyright 2020 David Wilson
;; See COPYING for details

(provide groups)

(require "typedef/getpwuid.rkt"
         "util/gidutil.rkt"
         "libc/unistd.rkt"
         "libc/pwd.rkt")

(require racket/string)

#;(help-function "Print the names of the given user's groups"
           (list "(execute user-name) -- Print the user-name's groups"))

(define groups
  (λ (user-name)
    (define un (get-username user-name))
    (define the-groups (get-user-groups un))
    (define group-names (map (λ (x) (gid->group-name x)) the-groups))
    (displayln (string-join group-names))))

(define (get-username user-name)
  (if (string=? "" user-name)
      (let* ([uid (get-euid)]
             [pwuid (get-pwuid uid)])
        (send pwuid get-username))
      user-name))
