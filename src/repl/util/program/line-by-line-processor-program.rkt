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
          (for ([file-name files])
            (let* ([f (open-input-file file-name #:mode 'text )]
                   [lines (port->lines f)])
              (for ([l lines])
                (displayln (line-function l))))))))))