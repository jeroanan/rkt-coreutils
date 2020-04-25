#lang typed/racket

; Copyright 2020 David Wilson
; see COPYING for details

(require "repl/ls.rkt"
         "util/version.rkt")

(define-syntax-rule (boolean-parameter name initial-value)
  (begin
    (: name (Parameterof Boolean))
    (define name (make-parameter initial-value))))

(boolean-parameter show-hidden #f)
(boolean-parameter hide-implied #t)
(boolean-parameter long-mode #f)
(boolean-parameter print-inodes #f)
(boolean-parameter show-colors #f)

(: pwds (Parameterof (Listof Path)))
(define pwds (make-parameter (list (current-directory))))

(define (set-almost-all)
  (begin
    (when (false? show-hidden) (hide-implied #t))
    (show-hidden #t)))

(define (set-show-hidden)
  (begin
    (show-hidden #t)
    (hide-implied #f)))

(define (set-the-pwds [ps : (Pairof Any (Listof Any))])
  (let* ([#{strings : (Listof String)} (map (λ (x) (format "~a" x)) ps)]
         [the-dirs (map (λ (x) (string->path x)) strings)])
    (pwds the-dirs)))

(command-line
  #:argv (current-command-line-arguments)
  #:once-each
  [("-a" "--all") "do not ignore entries starting with ." (set-show-hidden)]
  [("-A" "--almost-all") "do not list implied . and .." (set-almost-all)]
  [("-c" "--color") "colorise output" (show-colors #t)]
  [("-i" "--inode") "print the index number of each file" (print-inodes #t)]
  [("-l") "use a long listing format" (long-mode #t)]
  [("-v" "--version") "display version information and exit" (print-version-text-and-exit)]
  #:args dir (unless (empty? dir) (set-the-pwds dir)))

(let ([ls (new ls%)])
  (send ls set-show-hidden (show-hidden))
  (send ls set-hide-implied (hide-implied))
  (send ls set-print-inodes (print-inodes))
  (send ls set-long-mode (long-mode))
  (send ls set-show-colors (show-colors))
  (send ls execute
        (map (λ (x) (path->string x)) (pwds))))