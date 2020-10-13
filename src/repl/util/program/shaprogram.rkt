#lang s-exp "repl-program.rkt"

(provide sha-program2)

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
 
