#lang s-exp "repl-program.rkt"

(provide sha-program
         sha-program2)

(provide (all-from-out "repl-program.rkt")
         (all-from-out "file-by-file-processor-program.rkt")
         (all-from-out sha))
         
(require racket/class
         racket/port
         racket/list
         sha)

(require "file-by-file-processor-program.rkt")

(require (for-syntax racket/base))

(define-syntax-rule (sha-program2 name sha-method)
  (begin
    (provide name)
    
    (require sha)
    (file-by-file-processor name file-processor null #t)

    (define (port->hash-string ip)
      (let ([the-hash (sha-method (port->bytes ip))])
        (bytes->hex-string the-hash)))

    (define (display-result hash-str file-name)
      (displayln (format "~a  ~a" hash-str file-name)))
    
    (define (file-processor f fp)
      (define the-hash (port->hash-string fp))
      (display-result the-hash f))))

    
  
(define-syntax (sha-program stx)
  (syntax-case stx ()
  [(_  description type-name sha-method)
   #'(begin     
    
      (provide type-name)
      (require sha)
      
      (define type-name
        (class object%       
        
          (define help-strings
            (list (format "Compute and print ~a message digest" description)
                  ""
                  "Methods:"
                  "(help) -- display this help message"
                  (format
                   "(execute files) -- compute and print ~a message digest for files"
                   description)))

          (help-function help-strings)
        
          (super-new)
        
          (on-execute-with-strings files
                                   (if (empty? files)
                                       (process-stdin)
                                       (process-files files)))

          (define/private (process-stdin)
            (let ([the-hash (port->hash-string (current-input-port))])
              (display-result the-hash "-")))

          (define/private (process-files files)
            (for ([file files])
              (let* ([f (open-input-file file #:mode 'text )]
                     [the-hash (port->hash-string f)])
                (display-result the-hash file))))

          (define/private (port->hash-string ip)
            (let ([the-hash (sha-method (port->bytes ip))])
              (bytes->hex-string the-hash)))

          (define/private (display-result hash-str file-name)
            (displayln (format "~a  ~a" hash-str file-name))))))]))
