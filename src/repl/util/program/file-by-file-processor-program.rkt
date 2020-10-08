#lang s-exp "repl-program.rkt"

(provide (all-from-out "repl-program.rkt")
         file-by-file-processor-program
         file-by-file-processor
         process-file-by-file
         attribute)

(require (for-syntax racket/base))

(require racket/list)

(require "../attribute.rkt")

(define-syntax-rule (file-by-file-processor name file-processor finished-handler read-files?)
  (begin
    (provide name)
    (define name
      (λ (f . fs)
        (define files (cons f fs))
        (process-file-by-file files file-processor finished-handler read-files?)))))

(define (process-file-by-file files file-processor finished-handler read-files?)
  (if (empty? files)
      (process-stdin file-processor finished-handler)      
      (process-files files file-processor finished-handler read-files?)))

(define (process-stdin fp fh)
  (fp "-" (current-input-port))
  (fh))

(define (process-files files fp fh read-files?)
  (for ([f files])
    (if read-files?
        (read-and-process-file f fp)
        (process-file f fp))
    (unless (null? fh) (fh))))

(define (read-and-process-file filename fp)
  (let* ([ip (open-input-file filename #:mode 'text)])
    (fp filename ip)
    (close-input-port ip)))

(define (process-file filename fp)
  (let ([ip (current-input-port)])
    (fp filename ip)))

(define-syntax (file-by-file-processor-program stx)    
  (syntax-case stx ()
    [(_ type-name help-text read-files? file-handler-body finish-handler-body extras ...)     
     #'(begin
         (require racket/list)
         (provide type-name)
         
         (define type-name
           (class object%
             (super-new)

             (help-function help-text)

             extras ...

             (on-execute-with-strings files
                                      (if (empty? files)
                                          (process-stdin)
                                          (process-files files)))
             
             (define/private (process-files files)
               (for ([f files])
                 (if read-files?
                     (read-and-process-file f)
                     (process-file f))
                 (unless (null? finish-handler-body) (finish-handler-body))))

             (define (read-and-process-file filename)
               (let* ([ip (open-input-file filename #:mode 'text)])
                 (file-handler-body filename ip)
                 (close-input-port ip)))

             (define (process-file filename)
               (let ([ip (current-input-port)])
                 (file-handler-body filename ip)))

             (define/private (process-stdin)             
               (file-handler-body "-" (current-input-port))))))]))
