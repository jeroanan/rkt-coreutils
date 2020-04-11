#lang racket

(define file-name (make-parameter ""))
(define number-of-lines (make-parameter 10))

(command-line
  #:argv (current-command-line-arguments)
  #:once-each
  [("-n" "--lines") nl "print the first NUM lines instead of the first 10" (number-of-lines (string->number nl))]
  #:args filename (unless (empty? filename) (file-name (first filename))))

(let ([f (open-input-file (file-name) #:mode 'text )])
  (for ([i (number-of-lines)])
        (displayln (read-line f))))
