#lang s-exp "util/program/repl-program.rkt"

;; Copyright 2020 David Wilson
;; See COPYING for licence

(provide sort%)

(require racket/string)

(require "util/file-by-file-processor.rkt")

;; Sort -- display the sorted contents of provided files.
(define sort%
  (class object%
    (super-new)

    (help-function 
      "Display the sorted contents of provided FILES."
      (list "(execute FILES) -- Display the sorted contents of provided FILES"))

    ;; The contents of the files
    (private-string-list-attribute contents (list))

    ;; Process each file provided and for each file call on-process-file. When finished processing
    ;; all files, call on-finished-processing-files
    (file-by-file-processor on-process-file on-finished-processing-files)

    ;; Process a file. Add its contents to the contents field.
    (: on-process-file (-> (Listof String) Void))
    (define (on-process-file lines)
      (set! contents (append contents lines)))

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
        (string-ci<? str-a-trim str-b-trim)))))
