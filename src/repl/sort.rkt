#lang typed/racket

; Copyright 2020 David Wilson
; See COPYING for licence

(provide sort%)

(require "util/util.rkt"
         "util/file-by-file-processor.rkt"
         "../util/member.rkt")

(define sort%
  (class object%
    (super-new)

    (define help-text (list "Display the sorted contents of provided FILES."
                            ""
                            "Methods:"
                            "(execute FILES) -- Display the sorted contents of provided FILES"))
    (help-function help-text)

    (string-list-attribute contents (list))

    (file-by-file-processor on-process-file on-finished-processing-files)
    
    (: on-process-file (-> (Listof String) Void))
    (define (on-process-file lines)
      (set! contents (append contents lines)))

    (: on-finished-processing-files (-> Void))
    (define (on-finished-processing-files)
      (let ([sorted-contents (sort contents sort-func)])
        (for ([l sorted-contents])
          (displayln l))))

    (: sort-func (-> String String Boolean))
    (define (sort-func str-a str-b)
      (let* ([r #rx"[^A-Za-z0-9]*"]
             [str-a-trim (string-trim str-a r #:right? #f)]
             [str-b-trim (string-trim str-b r #:right? #f)])
        (string-ci<? str-a-trim str-b-trim)))))