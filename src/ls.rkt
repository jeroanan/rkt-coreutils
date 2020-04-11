#lang racket

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

(require "libc/stat.rkt"
         "libc/pwd.rkt"
         "libc/grp.rkt"
         "util/human-size.rkt"
         "util/human-date.rkt"
         "util/fileaccessstr.rkt")

(define show-hidden (make-parameter #f))
(define hide-implied (make-parameter #t))
(define long-mode (make-parameter #f))
(define print-inodes (make-parameter #f))
(define pwd (make-parameter (current-directory)))

(define (set-almost-all)
  (begin
    (when (false? show-hidden) (hide-implied #t))
    (show-hidden #t)))

(define (set-show-hidden)
  (begin
    (show-hidden #t)
    (hide-implied #f)))

(command-line
  #:argv (current-command-line-arguments)
  #:once-each
  [("-a" "--all") "do not ignore entries starting with ." (set-show-hidden)]
  [("-A" "--almost-all") "do not list implied . and .." (set-almost-all)]
  [("-i" "--inode") "print the index number of each file" (print-inodes #t)]
  [("-l") "use a long listing format" (long-mode #t)]
  #:args dir (unless (empty? dir) (pwd (first dir))))

(define (add-implied es)
  (if (not (hide-implied))
    (append (list "." "..") es)
    es))

(define (filter-hidden es)
  (define (weed-hidden f)
    (not (string-prefix? (path->string f) ".")))  
  (filter (lambda (x) (weed-hidden x)) es))

(define (get-inode-for-print inode)
  (if (print-inodes)
      (number->string (send inode get-inode))
      #f))

(define (process-entry-list es)
  (add-implied
   (filter-hidden
    es)))

(define (when-long-mode x) (if (long-mode) (x) #f))

(define (format-entry path filename)
  (let* ([filename-string (path->string filename)]
         [full-path (build-path path filename)] 
         [f-str (path->string full-path)]
         [stat (new stat% [path path] [file-name (path->string filename)])]
         [user (new getpwuid% [uid (send stat get-uid)])]
         [group (new getgrgid% [gid (send stat get-gid)])]
         
         [inode (get-inode-for-print stat)]

         [mode-str (when-long-mode (λ () (get-mode-str stat)))]
         [owner-user (when-long-mode (λ () (send user get-username)))]
         [owner-group (when-long-mode (λ () (send group get-name)))]
         [number-of-hardlinks (when-long-mode (λ () (number->string (send stat get-number-of-hardlinks))))]
         [size (when-long-mode (λ () (human-readable-byte-size (send stat get-size))))]
         [mtime (when-long-mode (λ () (unix-seconds->human-date (send stat get-modified-time))))]
         
         [outp-list (list inode mode-str number-of-hardlinks owner-user owner-group size mtime filename-string)]
         [outp-filtered (filter (λ (x) (not (false? x))) outp-list)]
         [outp-string (string-join outp-filtered " ")])
  outp-string))

(define (is-implied-path? p)
  (or
   (eq? p ".")
   (eq? p "..")))
  
(let* ([dlist (process-entry-list (directory-list (pwd)))])
  (for ([f dlist])
    (let* ([full-path (build-path (pwd) f)])           
      (displayln
       (if (is-implied-path? f) f (format-entry (pwd) f))))))
