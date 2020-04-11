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

(require racket/date)

(require "libc/stat.rkt"
         "libc/pwd.rkt"
         "libc/grp.rkt"
         "util/fileaccessoct.rkt"
         "util/fileaccessstr.rkt"
         "util/version.rkt")

(define the-file (make-parameter ""))

(define (get-path filename)
  (path->complete-path (string->path filename)))

(command-line
  #:argv (current-command-line-arguments)
  #:once-each
  [("-v" "--version") "display version information and exit" (print-version-text-and-exit)]
  #:args filename (unless (empty? filename) (the-file (get-path (first filename)))))

(define (get-path-parts)
  (let-values ([(p f _) (split-path (the-file))])
    (list p f)))

(define (get-file-type-string stat)
  (cond
    [(send stat get-is-regular-file?) "regular file"]
    [(send stat get-is-directory?) "directory"]
    [(send stat get-is-character-device?) "character special file"]
    [else ""]))

(define (get-device-string stat)
  (let* ([dev-no (send stat get-dev)]
         [dev-no-decimal (number->string dev-no)]
         [dev-no-hex (number->string dev-no 16)])
    (format "~ah/~ad" dev-no-hex dev-no-decimal)))

(define (get-access-string stat)
  (let ([access-oct (get-mode-oct-str stat)]
        [access-str (get-mode-str stat)])
    (format "(~a/~a)" access-oct access-str)))

(define (get-uid-string stat)
  (let* ([uid (send stat get-uid)]
         [p (new getpwuid% [uid uid])]
         [username (send p get-username)])
    (format "(~a/~a)" uid username)))

(define (get-gid-string stat)
  (let* ([gid (send stat get-gid)]
         [g (new getgrgid% [gid gid])]
         [groupname (send g get-name)])
    (format "(~a/~a)" gid groupname)))

(define-syntax-rule (file-time-getter name time-prop)
  (define (name stat)
    (let ([the-time (send stat time-prop)])
      (date-display-format 'iso-8601)
      (date->string (seconds->date the-time) #t))))

(file-time-getter get-access-time get-accessed-time)
(file-time-getter get-modified-time get-modified-time)
(file-time-getter get-created-time get-created-time)

(define (display-output the-path the-file)
  (let* ([s (new stat% [path the-path] [file-name the-file])]
         [output-elements (list (list ;Line 1
                                 (list "File" the-file))
                                (list ; Line 2
                                 (list "Size" (number->string (send s get-size)))
                                 (list "Blocks" (number->string (send s get-blocks)))
                                 (list "IO Block" (number->string (send s get-block-size)))
                                 (list "" (get-file-type-string s)))
                                (list ; Line 3
                                 (list "Device" (get-device-string s))
                                 (list "Inode" (number->string (send s get-inode)))
                                 (list "Links" (number->string (send s get-number-of-hardlinks))))
                                (list ; Line 4
                                 (list "Access" (get-access-string s))
                                 (list "Uid" (get-uid-string s))
                                 (list "Gid" (get-gid-string s)))
                                (list ; Line 5
                                 (list "Access" (get-access-time s)))
                                (list ; Line 6
                                 (list "Modify" (get-modified-time s)))
                                (list ; Line 7
                                 (list "Change" (get-created-time s)))
                                (list ; Line 8
                                 (list "Birth" "-")))])
    (for ([line output-elements])
      (for ([element line])
        (let* ([key (first element)]
               [value (second element)]
               [seperator (if (eq? key "") "" ": ")])
          (display (format "~a~a~a\t" key seperator value))))
      (displayln ""))))
          
(let* ([path-parts (get-path-parts)]
       [the-path (first path-parts)]
       [the-file (path->string (second path-parts))])
  (display-output the-path the-file))