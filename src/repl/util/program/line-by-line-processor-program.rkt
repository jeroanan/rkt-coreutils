#lang typed/racket

(provide line-by-line-processor-program)

(define-syntax-rule (line-by-line-processor-program type-name help-text line-function)
  (begin
    (require "util/util.rkt")
    (define type-name
      (class object%
        (super-new)

        (help-function help-text)

        (define/public (execute [files : (Listof String)])
          (begin
            (if (empty? files)
                (process-stdin)
                (process-files files))))

        (define (process-files [files : (Listof String)])
          (for ([file-name files])
            (let* ([f (open-input-file file-name #:mode 'text )])
              (for ([l (in-lines f)])
                (displayln (line-function l))))))

        (: process-stdin (-> Void))
        (define (process-stdin)
          (let* ([r (read-line)]
                 [rs (~a r)])
            (when (not (eof-object? r))
              (displayln (line-function rs))
              (process-stdin))))))))