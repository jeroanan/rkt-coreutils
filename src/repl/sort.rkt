#lang s-exp "util/program/file-by-file-processor-program.rkt"

;; Copyright 2020 David Wilson
;; See COPYING for licence

(require racket/string
         racket/port)

(define help-text (list "Display the sorted contents of provided FILES."
                        "(execute FILES) -- Display the sorted contents of provided FILES"))

(define contents (list))

(file-by-file-processor-program sort%
                                help-text
                                #t
                                on-process-file
                                on-finished-processing-files)

;; Process a file. Add its contents to the contents field.
(: on-process-file (-> String Input-Port Void))
(define (on-process-file filename stream)
  (let ([lines (port->lines stream)])
    (set! contents (append contents lines))))

;; Finished reading all files. Sort their accumulated contents and display the result.
(: on-finished-processing-files (-> Void))
(define (on-finished-processing-files)
  (let ([sorted-contents (sort contents sort-func)])
    (for ([l sorted-contents])
      (displayln l))
    (set! contents (list))))

;; The custom sorting function. Trim each string of everything other than alphanumeric characters
;; and do a case-insensitive sort on the result.
(: sort-func (-> String String Boolean))
(define (sort-func str-a str-b)
  (let* ([r #rx"[^A-Za-z0-9]*"]
         [str-a-trim (string-trim str-a r #:right? #f)]
         [str-b-trim (string-trim str-b r #:right? #f)])
    (string-ci<? str-a-trim str-b-trim)))