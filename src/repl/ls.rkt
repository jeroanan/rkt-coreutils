#lang typed/racket/base

; Copyright 2020 David Wilson
; See COPYING for details

(provide ls%)

(require typed/racket/class
         racket/bool
         racket/string
         racket/list
         racket/format)

(require "../typedef/stat.rkt"
         "../typedef/getpwuid.rkt"
         "../util/fileaccessstr.rkt"         
         "../util/human-date.rkt"
         "../util/human-size.rkt"
         "../util/member.rkt"
         "../util/stringutil.rkt"
         "util/gidutil.rkt")

(require/typed "../libc/stat.rkt"
               [get-stat (-> String String (Instance Stat%))])

(require/typed "../libc/pwd.rkt"
               [get-pwuid (-> Number (Instance Getpwuid%))])

(define ls%
  (class object%
    (super-new)

    (boolean-attribute print-inodes #f get-print-inodes set-print-inodes)
    (boolean-attribute long-mode #f get-long-mode set-long-mode)
    (boolean-attribute hide-implied #f get-hide-implied set-hide-implied)
    (boolean-attribute show-colors #f get-show-colors set-show-colors)
    (boolean-attribute show-hidden #f get-show-hidden set-show-hidden)
    
    (: when-long-mode (-> (-> String) (U String Boolean)))
    (define/private (when-long-mode x)
      (if long-mode (x) #f))

    (: get-inode-for-print (-> (Instance Stat%) (U String Boolean)))
    (define/private (get-inode-for-print [inode : (Instance Stat%)])
      (if print-inodes
          (number->string (send inode get-inode))
          #f))

    (: get-ls-colors (-> (HashTable String String)))
    (define/private (get-ls-colors)
      (let* ([env-val (environment-variables-ref (current-environment-variables) (string->bytes/utf-8 "LS_COLORS"))]
             [#{colors-hash : (HashTable String String)} (make-hash)])
        (if (false? env-val)
            colors-hash
            (let* ([env-string (bytes->string/utf-8 env-val)]
                   [color-entries (string-split env-string ":")])
              (for ([color-entry color-entries])
                (let* ([kvp (string-split color-entry "=")]
                       [key (first kvp)]
                       [val (second kvp)])
                  (hash-set! colors-hash key val)))
              colors-hash))))

    (: ls-colors (HashTable String String))
    (define ls-colors (get-ls-colors))
    
    (define-syntax-rule (make-colorizer name hr)
      (begin
        (: name (-> String String))
        (define/private (name entry-name)
          (let ([color-code (hash-ref ls-colors  hr)]
                [default-color (hash-ref ls-colors "rs")])
            (~a "\033[" color-code "m" entry-name "\033[" default-color "m")))))

    (make-colorizer colorize-directory "di")
    (make-colorizer colorize-executable "ex")
    (make-colorizer colorize-character-device "cd")
    (make-colorizer colorize-symbolic-link "ln")
    (make-colorizer colorize-pipe "pi")

    (: is-executable? (-> (Instance Stat%) Boolean))
    (define/private (is-executable? stat)
      (or (send stat get-owner-has-x?)
          (send stat get-group-has-x?)
          (send stat get-other-has-x?)))                
      
    (: colorize-filename (-> String (Instance Stat%) String))
    (define/private (colorize-filename filename stat)      
      (cond
        [(not show-colors) filename]
        [(send stat get-is-symbolic-link?) (colorize-symbolic-link filename)] ; This doesn't work -- don't know why!
        [(send stat get-is-directory?) (colorize-directory filename)]
        [(is-executable? stat) (colorize-executable filename)]
        [(send stat get-is-fifo?) (colorize-pipe filename)]
        [(send stat get-is-character-device?) (colorize-character-device filename)]
        [else filename]))

    (define-syntax-rule (anything->integer val)
      (assert val exact-integer?))

    (define-syntax-rule (any-list->string-list the-list)
      (map (λ (p) (~a p)) the-list))
    
    (: get-size-string (-> (Instance Stat%) String))
    (define/private (get-size-string stat)
      (let ([raw-size (anything->integer (send stat get-size))])
        (right-aligned-string (human-readable-byte-size raw-size) 4)))
    
    (: format-entry (-> Path Path String))
    (define/private (format-entry path filename)
      (let* ([filename-string (path->string filename)]
             [full-path (build-path path filename)] 
             [f-str (path->string full-path)]
             [stat (get-stat (path->string path) (path->string filename))]
             [user (get-pwuid (send stat get-uid))]            
         
             [inode (get-inode-for-print stat)]

             [mode-str (when-long-mode (λ () (get-mode-str stat)))]
             [owner-user (when-long-mode (λ () (send user get-username)))]
             [owner-group (when-long-mode (λ () (gid->group-name (send stat get-gid))))]
             [number-of-hardlinks (when-long-mode (λ () (number->string (send stat get-number-of-hardlinks))))]
             [size (when-long-mode (λ () (get-size-string stat)))]
             [mtime (when-long-mode (λ () (unix-seconds->human-date (anything->integer (send stat get-modified-time)))))]
             [outp-filename (colorize-filename filename-string stat)]
         
             [outp-list (list inode mode-str number-of-hardlinks owner-user owner-group size mtime outp-filename)]
             [outp-filtered (any-list->string-list (filter (λ (x) (not (false? x))) outp-list))]
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
      (let* ([path-strings (any-list->string-list es)]
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
