#lang s-exp "util/frontend-program.rkt"

;; Copyright 2020 David Wilson
;; see COPYING for details

;; ls.rkt: Command-line front-end for repl/ls.rkt

(require racket/bool
         racket/cmdline
         racket/list
         typed/racket/class)

(require "repl/ls.rkt"
         "util/stringutil.rkt")

;; Should hidden entries be shown?
(boolean-parameter show-hidden #f)

;; Should implied entries ("." and "..") be shown?
(boolean-parameter hide-implied #t)

;; Should we display in long mode?
(boolean-parameter long-mode #f)

;; Should inode numbers be displayed?
(boolean-parameter print-inodes #f)

;; Should output be in color?
(boolean-parameter show-colors #f)

;; Sets show hidden but ensures we don't display implied entries as part of output.
(define (set-almost-all)
  (begin
    (when (false? show-hidden) (hide-implied #t))
    (show-hidden #t)))

;; Show everything, including implied entries
(define (set-show-hidden)
  (begin
    (show-hidden #t)
    (hide-implied #f)))

;; The directories to list
(string-list-parameter pwds (list (anything->string (current-directory))))

(command-line
  #:argv (current-command-line-arguments)
  #:once-each
  [("-a" "--all") "do not ignore entries starting with ." (set-show-hidden)]
  [("-A" "--almost-all") "do not list implied . and .." (set-almost-all)]
  [("-c" "--color") "colorise output" (show-colors #t)]
  [("-i" "--inode") "print the index number of each file" (print-inodes #t)]
  [("-l") "use a long listing format" (long-mode #t)]
  [("-v" "--version") "display version information and exit" (print-version-text-and-exit)]
  #:args dir (unless (empty? dir) (set-pwds dir)))

(let ([ls (new ls%)])
  (send ls set-show-hidden (show-hidden))
  (send ls set-hide-implied (hide-implied))
  (send ls set-print-inodes (print-inodes))
  (send ls set-long-mode (long-mode))
  (send ls set-show-colors (show-colors))
  (send ls execute (pwds)))