#lang typed/racket

(provide tac%)

(require "util/util.rkt")

(define tac%
  (class object%
    (super-new)

    (help-function (list "Concatenate files and print on standard output in reverse."
                            ""
                            "Methods:"
                            "(help) -- display this help message"
                            "(execute FILES) -- concatenate and print FILES in reverse"))    
    
    (define/public (execute [files : (Listof String)])
      (for ([file-name files])
        (let* ([f (open-input-file file-name #:mode 'text)]
               [lines (port->lines f)]
               [reversed (reverse lines)])
          (for ([l reversed])
            (displayln l)))))))
