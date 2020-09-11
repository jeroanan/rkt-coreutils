#lang s-exp "util/program/repl-program.rkt"

;; Copyright 2020 David Wilson
;; See COPYING for licence

(provide uniq%)

(require "util/program/line-by-line-processor-program.rkt"
         "../util/stringutil.rkt")

;; Uniq -- display unique consecutive lines in the provided file
(define help-text 
  (list"Display uniq lines in FILE"
    "(execute FILE) -- Display uniq lines in FILE"))

(line-by-line-processor-program uniq% help-text process-line)

;; Output the line unless it's the same as the previous one.
(: process-line (-> String Void))
(define (process-line line)
  (when (or first-line? (string<>? line previous-line))
    (begin
      (set! first-line? #f)
      (set! previous-line line)
      (displayln line))))

;; The last line to have been processed
(define previous-line "")

;; Is this the first line in the file?
(define first-line? #t)

