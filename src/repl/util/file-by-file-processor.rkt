#lang typed/racket/base

; Copyright 2020 David Wilson
; See COPYING for licence details

(provide file-by-file-processor)

(require racket/list
         typed/racket/class
         racket/port)

(define-syntax file-by-file-processor
  (syntax-rules ()
    [(file-by-file-processor file-function)
     (begin
       (: execute (-> (Listof String) Void))
       (define/public (execute files)
         (if (empty? files)
             (_process-stdin file-function)
             (_process-files file-function files))))]
    [(file-by-file-processor file-function finish-function)
     (begin
       (: execute (-> (Listof String) Void))
       (define/public (execute files)
         (if (empty? files)
             (_process-stdin file-function finish-function)
             (_process-files file-function finish-function files))))]))

(define-syntax _process-stdin
  (syntax-rules ()
    [(_process-stdin file-function)  
     (let ([lines (port->lines (current-input-port))])
       (file-function lines))]
    [(_process-stdin file-function finish-function)
     (let ([lines (port->lines (current-input-port))])
       (file-function lines)
       (finish-function))]))

(define-syntax _process-files
  (syntax-rules ()
    [(_process-files file-function files)
     (for ([file-name files])
       (let* ([f (open-input-file file-name #:mode 'text)]
              [lines (port->lines f)])
         (file-function lines)))]
    [(_process-files file-function finished-function files)
     (begin
       (for ([file-name files])
         (let* ([f (open-input-file file-name #:mode 'text)]
                [lines (port->lines f)])
           (file-function lines)))
       (finished-function))]))
  