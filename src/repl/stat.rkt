#lang typed/racket/base

; Copyright 2020 David Wilson
; See COPYING for details

(provide stat%)

(require typed/racket/class
         typed/racket/date
         racket/list)

(require "util/fileaccessoct.rkt"
         "util/fileaccessstr.rkt"
         "typedef/stat.rkt"
         "typedef/getpwuid.rkt"
         "util/gidutil.rkt"
         "../util/stringutil.rkt"
         "util/help.rkt")

(require/typed "libc/stat.rkt"
               [get-stat (-> String String (Instance Stat%))])

(require/typed "libc/pwd.rkt"
               [get-pwuid (-> Number (Instance Getpwuid%))])

;; Take an instance of Stat% and return a string representing the inode type
(define (get-file-type-string [stat : (Instance Stat%)])
  (cond
    [(send stat get-is-regular-file?) "regular file"]
    [(send stat get-is-directory?) "directory"]
    [(send stat get-is-character-device?) "character special file"]
    [else ""]))

;; Take an instance of Stat% and return a hex and dec representation of the device no.
(define (get-device-string [stat : (Instance Stat%)])
  (let* ([dev-no (send stat get-dev)]
         [dev-no-decimal (number->string dev-no)]
         [dev-no-hex (number->string dev-no 16)])
    (format "~ah/~ad" dev-no-hex dev-no-decimal)))

;; Take an instance of Stat% and return octal and string representatinos of its access mode
(define (get-access-string [stat : (Instance Stat%)])
  (let ([access-oct (get-mode-oct-str stat)]
        [access-str (get-mode-str stat)])
    (format "(~a/~a)" access-oct access-str)))

;; Take an instance of Stat% and return the uid and associated username
(define (get-uid-string [stat : (Instance Stat%)])
  (let* ([uid (send stat get-uid)]
         [p (get-pwuid uid)]
         [username (send p get-username)])
    (format "(~a/~a)" uid username)))

;; Take an instance of Stat% and return the gid and associated group name
(define (get-gid-string [stat : (Instance Stat%)])
  (let* ([gid (send stat get-gid)]
         [groupname (gid->group-name gid)])
    (format "(~a/~a)" gid groupname)))

;; Define a function that gets the given time property from an instance of Stat%
(define-syntax-rule (file-time-getter name time-prop)
  (define (name [stat : (Instance Stat%)])
    (let ([the-time (send stat time-prop)])
      (date-display-format 'iso-8601)
      (date->string (seconds->date the-time) #t))))

(file-time-getter get-access-time get-accessed-time)
(file-time-getter get-modified-time get-modified-time)
(file-time-getter get-created-time get-created-time)

;; Print status of each provided file.
(define stat% 
  (class object%
    (super-new)

    ;; Display formatted output
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

    ;; Takes a string representation of a filename with path and returns the path and filename parts.
    (define (get-path-parts [the-file : String])
      (let-values ([(p f _) (split-path the-file)])        
        (let ([p-string (if (eq? p 'relative)
                            (path->string (current-directory))
                            (anything->string p))]
              [f-string (anything->string f)])
          (list p-string f-string))))

    ;; Main program execution
    (define/public (execute [files : (Listof String)])
      (for ([f files])
        (let* ([path-parts (get-path-parts f)]
               [the-path (first path-parts)]
               [file-name (second path-parts)])
          (display-output the-path file-name))))

    (help-function 
      "Print status of each provided file."
      (list "(execute FILES) -- display the status of FILES"
            "(help) -- display this help message")
      (list))))
