#lang typed/racket

; Copyright 2020 David Wilson
; see COPYING for details

(require "repl/ls.rkt"
         "util/version.rkt")

(: show-hidden (Parameterof Boolean))
(define show-hidden (make-parameter #f))

(define hide-implied (make-parameter #t))

(: long-mode (Parameterof Boolean))
(define long-mode (make-parameter #f))

(: print-inodes (Parameterof Boolean))
(define print-inodes (make-parameter #f))

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
  [("-i" "--inode") "print the index number of each file" (print-inodes #t)]
  [("-l") "use a long listing format" (long-mode #t)]
  [("-v" "--version") "display version information and exit" (print-version-text-and-exit)]
  #:args dir (unless (empty? dir) (set-the-pwds dir)))

(let ([ls (new ls%)])
  (send ls set-show-hidden (show-hidden))
  (send ls set-hide-implied (hide-implied))
  (send ls set-print-inodes (print-inodes))
  (send ls set-long-mode (long-mode))
  (send ls execute
        (map (λ (x) (path->string x)) (pwds))))