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

(provide stat%)

(require "../util/fileaccessoct.rkt"
         "../util/fileaccessstr.rkt"
         "../typedef/stat.rkt"
         "../typedef/getpwuid.rkt"
         "../typedef/getgrgid.rkt")

(require/typed "../libc/stat.rkt"
               [get-stat (-> String String (Instance Stat%))])

(require/typed "../libc/pwd.rkt"
               [get-pwuid (-> Number (Instance Getpwuid%))])

(require/typed "../libc/grp.rkt"
               [get-getgrgid (-> Number (Instance Getgrgid%))])

(require/typed racket/date
               [date-display-format ( -> Symbol Void)]
               [date->string (-> date Boolean String)])

(define (get-file-type-string [stat : (Instance Stat%)])
  (cond
    [(send stat get-is-regular-file?) "regular file"]
    [(send stat get-is-directory?) "directory"]
    [(send stat get-is-character-device?) "character special file"]
    [else ""]))

(define (get-device-string [stat : (Instance Stat%)])
  (let* ([dev-no (send stat get-dev)]
         [dev-no-decimal (number->string dev-no)]
         [dev-no-hex (number->string dev-no 16)])
    (format "~ah/~ad" dev-no-hex dev-no-decimal)))

(define (get-access-string [stat : (Instance Stat%)])
  (let ([access-oct (get-mode-oct-str stat)]
        [access-str (get-mode-str stat)])
    (format "(~a/~a)" access-oct access-str)))

(define (get-uid-string [stat : (Instance Stat%)])
  (let* ([uid (send stat get-uid)]
         [p (get-pwuid uid)]
         [username (send p get-username)])
    (format "(~a/~a)" uid username)))

(define (get-gid-string [stat : (Instance Stat%)])
  (let* ([gid (send stat get-gid)]
         [g (get-getgrgid gid)]
         [groupname (send g get-name)])
    (format "(~a/~a)" gid groupname)))

(define-syntax-rule (file-time-getter name time-prop)
  (define (name [stat : (Instance Stat%)])
    (let ([the-time (send stat time-prop)])
      (date-display-format 'iso-8601)
      (date->string (seconds->date the-time) #t))))

(file-time-getter get-access-time get-accessed-time)
(file-time-getter get-modified-time get-modified-time)
(file-time-getter get-created-time get-created-time)

(define stat% 
  (class object%
    (super-new)
    
    (define (display-output [the-path : String] [the-file : String])
      (let* ([s (get-stat the-path the-file)]
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

    (define (get-path-parts [the-file : String])
      (let-values ([(p f _) (split-path the-file)])        
        (let ([p-string (if (eq? p 'relative)
                            (path->string (current-directory))
                            (format "~a" p))]
              [f-string (format "~a" f)])
          (list p-string f-string))))

    (define/public (execute [files : (Listof String)])
      (for ([f files])
        (let* ([path-parts (get-path-parts f)]
               [the-path (first path-parts)]
               [file-name (second path-parts)])
          (display-output the-path file-name))))

    (define/public (help)
      (let ([help-strings (list "Print status of each provided file."
                                ""
                                "Methods:"
                                "(execute FILES) -- display the status of FILES")])
        (for ([hs help-strings])
          (displayln hs))))))
