#lang s-exp "repl-program.rkt"

(provide (all-from-out "repl-program.rkt")
         file-by-file-processor-program)

(require (for-syntax racket/base))

(require "../attribute.rkt")

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
             
             (: process-files (-> (Listof String) Void))
             (define/private (process-files files)
               (for ([f files])
                 (if read-files?
                     (read-and-process-file f)
                     (file-handler-body f null))
                 (unless (null? finish-handler-body) (finish-handler-body))))

             (: read-and-process-file (-> String Void))
             (define (read-and-process-file filename)
               (let* ([ip (open-input-file filename #:mode 'text)])
                   (file-handler-body filename ip)
                   (close-input-port ip)))               

             (: process-stdin (-> Void))
             (define/private (process-stdin)             
               (file-handler-body "-" (current-input-port))))))]))
