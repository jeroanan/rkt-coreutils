#lang s-exp "util/program/line-by-line-processor-program.rkt"

;; Copyright 2020 David Wilson
;; See COPYING for licence

(require "../util/stringutil.rkt")

;; Uniq -- display unique consecutive lines in the provided file
(define help-text 
  (list"Display uniq lines in FILE"
    "(execute FILE) -- Display uniq lines in FILE"))

(line-by-line-processor uniq process-line null)

(define first-line? #t)
(define previous-line "")

(define (process-line x)
  (when (or first-line? (string<>? x previous-line))
    (begin
      (set! first-line? #f)
      (set! previous-line x)
      (displayln x))))
