#lang typed/racket/base

; Copyright 2020 David Wilson
; See COPYING for details

(provide line-by-line-processor)

(require typed/racket/class
         racket/format
         racket/list)

(define-syntax (line-by-line-processor line-function)
  (syntax-rules ()
    [(line-by-line-processor line-function)
     (begin 
       (define/public (execute [files : (Listof String)])
         (begin
           (if (empty? files)
               (process-stdin)
               (process-files files))))

       (_process-files)

       (_process-stdin))]
    [(line-by-line-processor line-function on-before-processing-function)
     (begin
       (define/public (execute [files : (Listof String)])
         (begin
           (on-before-processing-function)
           (if (empty? files)
               (process-stdin)
               (process-files files))))

       (_process-files)

       (_process-stdin))]))
    

(define-syntax-rule (_process-stdin)
  (begin
    (: process-stdin (-> Void))
    (define/private (process-stdin)
      (let* ([r (read-line)]
             [rs (~a r)])
        (when (not (eof-object? r))
          (line-function rs)
          (process-stdin))))))

(define-syntax-rule (_process-files)
  (begin
    (: process-files (-> (Listof String) Void))
    (define/private (process-files files)
      (for ([file-name files])
        (let* ([f (open-input-file file-name #:mode 'text )])
          (for ([l (in-lines f)])
            (line-function l)))))))