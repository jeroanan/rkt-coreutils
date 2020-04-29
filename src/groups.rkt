#lang typed/racket/base

; Copyright 2020 David Wilson
; See COPYING for details

(require racket/cmdline
         racket/list
         racket/class)

(require "util/version.rkt"
         "util/stringutil.rkt"
         "repl/groups.rkt")

(: the-username (Parameterof String))
(define the-username (make-parameter ""))

(command-line
  #:argv (current-command-line-arguments)
  #:once-each
  #:args username (unless (empty? username) (the-username (anything->string (first username)))))

(let ([g (new groups%)])
  (send g execute (anything->string (the-username))))
