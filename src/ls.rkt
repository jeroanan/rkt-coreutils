#lang racket 

(require "libc/stat.rkt")

(define show-hidden (make-parameter #f))
(define hide-implied (make-parameter #t))
(define pwd (make-parameter (current-directory)))
(define print-inodes (make-parameter #f))

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

(define (format-entry path filename)
  (let* ([full-path (build-path path filename)] 
         [f-str (path->string full-path)]
         [stat (new stat% [path path] [file-name filename])]
         [inode (get-inode-for-print stat)]
         [outp-list (list inode f-str)]
         [outp-filtered (filter (λ (x) (not (false? x))) outp-list)]
         [outp-string (string-join outp-filtered " ")])
  outp-string))
  
(let* ([dlist (process-entry-list (directory-list (pwd)))])
  (for ([f dlist])
    (let* ([full-path (build-path (pwd) f)])           
      (displayln (format-entry (pwd) f)))))
