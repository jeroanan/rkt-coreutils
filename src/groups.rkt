#lang s-exp "util/frontend-program.rkt"

; Copyright 2020 David Wilson
; See COPYING for details

(require racket/cmdline
         racket/list
         racket/class)

(require "util/stringutil.rkt"
         "repl/groups.rkt")

(string-parameter the-username "")

(command-line
  #:argv (current-command-line-arguments)
  #:once-each
  #:args username (unless (empty? username) (the-username (anything->string (first username)))))

(groups the-username)
