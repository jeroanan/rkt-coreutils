#lang typed/racket/base

;; Copyright 2020 David Wilson
;; See COPYING for licence

(provide uniq%)

(require typed/racket/class)

(require "util/util.rkt"
         "util/line-by-line-processor.rkt"
         "util/member.rkt"
         "../util/stringutil.rkt")

;; Uniq -- display unique consecutive lines in the provided file
(define uniq%
  (class object%
    (super-new)

    ;; The last line to have been processed
    (string-attribute previous-line "")

    ;; Is this the first line in the file?
    (private-boolean-attribute first-line? #t)
    
    (help-function (list "Display uniq lines in FILE"
                         ""
                         "Methods:"
                         "(execute FILE) -- Display uniq lines in FILE"))

    ;; Read the file and call process-line with each line.
    (line-by-line-processor process-line)

    ;; Output the line unless it's the same as the previous one.
    (: process-line (-> String Void))
    (define (process-line line)
      (when (or first-line? (string<>? line previous-line))
        (begin
          (set! first-line? #f)
          (set! previous-line line)
          (displayln line))))))