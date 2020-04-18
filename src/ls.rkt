#lang typed/racket

; Copyright 2020 David Wilson

;This program is free software: you can redistribute it and/or modify
;it under the terms of the GNU General Public License as published by
;the Free Software Foundation, either version 3 of the License, or
;(at your option) any later version.
;
;This program is distributed in the hope that it will be useful,
;but WITHOUT ANY WARRANTY; without even the implied warranty of
;MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;GNU General Public License for more details.
;
;You should have received a copy of the GNU General Public License
;along with this program.  If not, see <https://www.gnu.org/licenses/>.

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