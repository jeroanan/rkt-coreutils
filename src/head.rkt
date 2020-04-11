#lang racket

(define file-name (make-parameter ""))
(define number-of-lines (make-parameter 10))

(define (exit-with-error error-msg)
  (begin
    (displayln error-msg)
    (exit 1)))

(define (set-number-of-lines nl)
  (let ([i (string->number nl)])
    (if (false? i)
        (exit-with-error (format "invalid number of lines: '~a'" nl))
        (number-of-lines i))))      
                             
(command-line
  #:argv (current-command-line-arguments)
  #:once-each
  [("-n" "--lines") nl "print the first NUM lines instead of the first 10" (set-number-of-lines nl)]
  #:args filename (unless (empty? filename) (file-name (first filename))))

(let ([f (open-input-file (file-name) #:mode 'text )])
  (for ([i (number-of-lines)])
        (displayln (read-line f))))
