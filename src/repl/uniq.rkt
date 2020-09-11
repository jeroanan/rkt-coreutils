#lang s-exp "util/program/repl-program.rkt"

;; Copyright 2020 David Wilson
;; See COPYING for licence

(provide uniq%)

(require "util/line-by-line-processor.rkt"
         "../util/stringutil.rkt")

;; Uniq -- display unique consecutive lines in the provided file
(define uniq%
  (class object%
    (super-new)

    ;; The last line to have been processed
    (private-string-attribute previous-line "")

    ;; Is this the first line in the file?
    (private-boolean-attribute first-line? #t)
    
    (help-function 
      "Display uniq lines in FILE"
      (list "(execute FILE) -- Display uniq lines in FILE"))

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
