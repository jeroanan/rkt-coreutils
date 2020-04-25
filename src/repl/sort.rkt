#lang typed/racket

; Copyright 2020 David Wilson
; See COPYING for licence

(provide sort%)

(require "util/util.rkt")

(define sort%
  (class object%
    (super-new)

    (define help-text (list "Display the sorted contents of provided FILES."
                            ""
                            "Methods:"
                            "(execute FILES) -- Display the sorted contents of provided FILES"))
    (help-function help-text)

    (define/public (execute [files : (Listof String)])
      (if (empty? files)
          (process-stdin)
          (process-files files)))

    (: process-stdin (-> Void))
    (define/private (process-stdin)
      (let* ([the-lines (port->lines (current-input-port))]
             [sorted (sort the-lines sort-func)])
       (for ([l sorted])
         (displayln l))))
        
    (: process-files (-> (Listof String) Void))
    (define/private (process-files files)
      (let ([the-lines (list "")])
        (for ([f files])
          (set! the-lines (append the-lines (port->lines (open-input-file f)))))
        (for ([l (sort the-lines sort-func)])
          (displayln l))))
    
    (: sort-func (-> String String Boolean))
    (define (sort-func str-a str-b)
      (let* ([r #rx"[^A-Za-z0-9]*"]
             [str-a-trim (string-trim str-a r #:right? #f)]
             [str-b-trim (string-trim str-b r #:right? #f)])
        (string-ci<? str-a-trim str-b-trim)))))