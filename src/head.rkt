#lang racket

(define file-name (make-parameter ""))
(define number-of-lines (make-parameter 10))

(command-line
  #:argv (current-command-line-arguments)
  #:once-each
  #:args filename (unless (empty? filename) (file-name (first filename))))

(let ([f (open-input-file (file-name) #:mode 'text )])
  (for ([i (- (number-of-lines) 1)])
        (displayln (read-line f)))
  (displayln ""))
