#lang racket 

(require "libc/stat.rkt"
         "libc/pwd.rkt"
         "libc/grp.rkt")

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

(define (when-long-mode x) (if (long-mode) x #f))

(define (format-entry path filename)
  (let* ([full-path (build-path path filename)] 
         [f-str (path->string full-path)]
         [stat (new stat% [path path] [file-name filename])]
         [user (new getpwuid% [uid (send stat get-uid)])]
         [group (new getgrgid% [gid (send stat get-gid)])]
         
         [inode (get-inode-for-print stat)]
         [owner-user (when-long-mode (send user get-username))]
         [owner-group (when-long-mode (send group get-name))]
         
         [outp-list (list owner-user owner-group inode f-str)]
         [outp-filtered (filter (λ (x) (not (false? x))) outp-list)]
         [outp-string (string-join outp-filtered " ")])
  outp-string))
  
(let* ([dlist (process-entry-list (directory-list (pwd)))])
  (for ([f dlist])
    (let* ([full-path (build-path (pwd) f)])           
      (displayln (format-entry (pwd) f)))))
