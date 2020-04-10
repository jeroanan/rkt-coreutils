#lang racket

(define the-string (make-parameter ""))

(command-line
  #:argv (current-command-line-arguments)
  #:once-each
  #:args string (unless (empty? string) (the-string (first string))))

(displayln (the-string))
