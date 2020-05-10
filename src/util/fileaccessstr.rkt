#lang typed/racket/base

; Copyright 2020 David Wilson
; See COPYING for details

(provide get-mode-str)

(require "../repl/typedef/stat.rkt")

(require typed/racket/class)

(define (get-file-type-char [stat : (Instance Stat%)])
  (cond
    [(send stat get-is-directory?) "d"]
    [else "-"]))

(define-syntax-rule (get-permissions-mode name rwx r w x)
  (define (name [stat : (Instance Stat%)])
    (if (send stat rwx)
        "rwx"
        (let ([r-flag (if (send stat r) "r" "-")]
              [w-flag (if (send stat w) "w" "-")]
              [x-flag (if (send stat x) "x" "-")])
          (string-append r-flag w-flag x-flag)))))
  
(get-permissions-mode get-owner-mode
                      get-owner-has-rwx?
                      get-owner-has-r?
                      get-owner-has-w?
                      get-owner-has-x?)

(get-permissions-mode get-group-mode
                      get-group-has-rwx?
                      get-group-has-r?
                      get-group-has-w?
                      get-group-has-x?)

(get-permissions-mode get-other-mode
                      get-other-has-rwx?
                      get-other-has-r?
                      get-other-has-w?
                      get-other-has-x?)

(define (get-mode-str [stat : (Instance Stat%)])
  (let ([file-type (get-file-type-char stat)]
        [owner-mode (get-owner-mode stat)]
        [group-mode (get-group-mode stat)]
        [other-mode (get-other-mode stat)])
  (string-append file-type owner-mode group-mode other-mode)))

