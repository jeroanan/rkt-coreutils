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

(require "libc/grp.rkt"
         "typedef/stat.rkt"
         "typedef/getpwuid.rkt"
         "typedef/getgrgid.rkt"
         "util/human-size.rkt"
         "util/human-date.rkt"
         "util/fileaccessstr.rkt"
         "util/version.rkt")

(require/typed "libc/pwd.rkt"
               [get-pwuid (-> Number (Instance Getpwuid%))])

(require/typed "libc/grp.rkt"
               [get-getgrgid (-> Number (Instance Getgrgid%))])

(require/typed "libc/stat.rkt"
               [get-stat (-> String String (Instance Stat%))])

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

(define (add-implied [es : (Listof Path)])
  (if (not (hide-implied))
    (append (list "." "..") es)
    es))

(define (filter-hidden [es : (Listof Path)])  
  (define (weed-hidden [f : Path])
    (not (string-prefix? (path->string f) ".")))  
  (filter (lambda ([x : Path]) (weed-hidden x)) es))

(define (get-inode-for-print [inode : (Instance Stat%)])
  (if (print-inodes)
      (number->string (send inode get-inode))
      #f))

(define (process-entry-list [es : (Listof Path)])
  (add-implied
   (filter-hidden
    es)))

(define (when-long-mode [x : (-> String) ])
  (if (long-mode) (x) #f))

(define (format-entry [path : Path] [filename : Path])
  (let* ([filename-string (path->string filename)]
         [full-path (build-path path filename)] 
         [f-str (path->string full-path)]
         [stat (get-stat (path->string path) (path->string filename))]
         [user (get-pwuid (send stat get-uid))]
         [group (get-getgrgid (send stat get-gid))]
         
         [inode (get-inode-for-print stat)]

         [mode-str (when-long-mode (λ () (get-mode-str stat)))]
         [owner-user (when-long-mode (λ () (send user get-username)))]
         [owner-group (when-long-mode (λ () (send group get-name)))]
         [number-of-hardlinks (when-long-mode (λ () (number->string (send stat get-number-of-hardlinks))))]
         [size (when-long-mode (λ () (human-readable-byte-size (assert (send stat get-size) exact-integer?))))]
         [mtime (when-long-mode (λ () (unix-seconds->human-date (assert (send stat get-modified-time) exact-integer?))))]
         
         [outp-list (list inode mode-str number-of-hardlinks owner-user owner-group size mtime filename-string)]
         [outp-filtered (map (λ (p) (format "~a" p)) (filter (λ (x) (not (false? x))) outp-list))]
         [outp-string (string-join outp-filtered " ")])
  outp-string))

(define (is-implied-path? p)
  (or
   (eq? p ".")
   (eq? p "..")))
  
(for ([p (pwds)])
  (let* ([dlist (process-entry-list (directory-list p))])
    (for ([f dlist])
      (let* ([full-path (build-path p f)])           
        (displayln
         (if (is-implied-path? f)
             f
             (format-entry p (string->path (format "~a" f)))))))))
  