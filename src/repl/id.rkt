#lang s-exp "util/program/repl-program.rkt"

; Copyright 2020 David Wilson
; See COPYING for details

(provide id)

(require "typedef/getpwuid.rkt"
         "util/gidutil.rkt")

(require racket/string)

(require "libc/unistd.rkt"
         "libc/pwd.rkt")

(define id
  (λ ()
    (define uid (get-euid))
    (define pwd (get-pwuid uid))
    (define user-name (send pwd get-username))
    (define main-gid (send pwd get-gid))             
    (define all-gids (get-user-groups user-name))
    (define formatted-main-gid (format-group main-gid))
    (define formatted-gids (format-groups all-gids))
    (displayln (format "uid=~a(~a) gid=~a groups=~a"
                           uid
                           user-name
                           formatted-main-gid
                           formatted-gids))))

;;; Joins list of group ids into a comma-separated list of formatted groups
(define (format-groups gids)
  (let ([formatted-entries (map format-group gids)])
    (string-join formatted-entries ",")))

;; Take a groupid and format it with its name.
(define (format-group gid)
  (let ([group-name (gid->group-name gid)])
    (format "~a(~a)" gid group-name)))
