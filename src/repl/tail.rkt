#lang typed/racket

(provide tail%)

(require "util/util.rkt")

(define tail%
  (class object%
    (super-new)

    (help-function (list "Output the end of files."
                         ""
                         "Methods:"
                         "(execute FILES) -- display the last lines of FILES"))

    (define/public (execute [files : (Listof String)])
      (for ([file-name files])                     
        (let* ([f (open-input-file file-name #:mode 'text)]
               [lines (port->lines f)]
               [reversed (reverse lines)]
               [top-x (take reversed 10)]
               [output (reverse top-x)])
          (for ([l output])
            (displayln l)))))))
