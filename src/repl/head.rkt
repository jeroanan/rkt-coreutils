#lang typed/racket

; Copyright 2020 David Wilson
; See COPYING for licence details

(provide head%)

(require "../util/member.rkt"
         "util/util.rkt")

(define head%
  (class object%  
    (super-new)

    (integer-attribute number-of-lines get-number-of-lines set-number-of-lines 10)

    (help-function (list "Print the first lines of each provided file."
                         ""
                         "Methods:"
                         "(set-number-of-lines NUM) -- set the number of lines to be printed to NUM"
                         "(get-number-of-lines) -- get the number of lines to be printed"
                         "(execute FILES) -- display the first lines of FILES"))
        
    (define/public (execute [files : (Listof String)])
      (if (empty? files) (handle-stdin) (handle-files files)))     

    (: handle-files (-> (Listof String) Void))
    (define/private (handle-files files)
      (for ([file-name files])
        (when (> (length files) 1) (displayln (format "==> ~a <==" file-name)))
        (let ([f (open-input-file file-name #:mode 'text )])
          (for ([i number-of-lines])
            (displayln (read-line f))))))

    (: handle-stdin (-> Void))
    (define/private (handle-stdin)
      (for ([i number-of-lines])
        (let ([l (read-line)])
          (if (eof-object? l)
              (exit 0)
              (displayln l)))))))