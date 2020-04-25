#lang typed/racket

; Copyright 2020 David Wilson
; See COPYING for details

(provide ls%)

(require "../typedef/stat.rkt"
         "../typedef/getpwuid.rkt"
         "../typedef/getgrgid.rkt"
         "../util/fileaccessstr.rkt"
         "../util/human-date.rkt"
         "../util/human-size.rkt"
         "../util/member.rkt")

(require/typed "../libc/stat.rkt"
               [get-stat (-> String String (Instance Stat%))])

(require/typed "../libc/pwd.rkt"
               [get-pwuid (-> Number (Instance Getpwuid%))])

(require/typed "../libc/grp.rkt"
               [get-getgrgid (-> Number (Instance Getgrgid%))])

(define ls%
  (class object%
    (super-new)

    (boolean-attribute print-inodes get-print-inodes set-print-inodes #f)
    (boolean-attribute long-mode get-long-mode set-long-mode #f)
    (boolean-attribute hide-implied get-hide-implied set-hide-implied #f)
    (boolean-attribute show-hidden get-show-hidden set-show-hidden #f)
    
    (: when-long-mode (-> (-> String) (U String Boolean)))
    (define/private (when-long-mode x)
      (if long-mode (x) #f))

    (: get-inode-for-print (-> (Instance Stat%) (U String Boolean)))
    (define/private (get-inode-for-print [inode : (Instance Stat%)])
      (if print-inodes
          (number->string (send inode get-inode))
          #f))

    (: format-entry (-> Path Path String))
    (define/private (format-entry path filename)
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

    (: add-implied (-> (Listof String) (Listof String)))
    (define/private (add-implied es)
      (if (not hide-implied)
          (append (list "." "..") es)
          es))

    (: filter-hidden (-> (Listof String) (Listof String)))
    (define (filter-hidden es)  
      (define (weed-hidden [f : String])
        (not (string-prefix? f ".")))
      (if show-hidden
          es
          (filter (lambda ([x : String]) (weed-hidden x)) es)))

    (: process-entry-list (-> (Listof Path) (Listof String)))
    (define/private (process-entry-list es)
      (let* ([path-strings (map (λ (x) (format "~a" x)) es)]
             [sorted (sort path-strings string-ci<?)])
        (add-implied
         (filter-hidden
          sorted))))

    (define (is-implied-path? p)
      (or
       (eq? p ".")
       (eq? p "..")))
    
    (define/public (execute [dirs : (Listof String)])
      (for ([p dirs])
        (let* ([dlist (process-entry-list (directory-list p))])
          (for ([f dlist])
            (let* ([full-path (build-path p f)])           
              (displayln
               (if (is-implied-path? f)
                   f
                   (format-entry (string->path p) (string->path (format "~a" f))))))))))))
