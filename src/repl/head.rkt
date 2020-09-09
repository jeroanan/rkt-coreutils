#lang typed/racket/base

; Copyright 2020 David Wilson
; See COPYING for licence details

(provide head%)

(require typed/racket/class)

(require "util/member.rkt"
         "util/help.rkt"
         "util/line-by-line-processor.rkt")

;; Head: Print the first lines of the given files
;; TODO: It seems that currently only the first file will be dealt with and then we exit. Need to be
;;       able to handle multiple files.
(define head%
  (class object%  
    (super-new)

    ;; The number of lines from each file to print
    (public-integer-attribute number-of-lines 10)

    (help-function 
      "Print the first lines of each provided file."
      (list "(execute FILES) -- display the first lines of FILES"
            "(help) -- Display this help message")
      (list "number-of-lines (integer) -- the number of lines to be printed"))

    ;; This program takes each file provided and calls line-handler within each line therein.
    (line-by-line-processor line-handler)

    ;; How many lines have been printed so far.
    (private-integer-attribute counter 0)

    ;; Print each line as it is sent in. If we have gotten to the number of lines spcified, stop the
    ;; program.
    (: line-handler (-> String Void))
    (define/private (line-handler line)
      (begin
        (displayln line)
        (set! counter (add1 counter))
        (when (eq? counter number-of-lines) (exit 0))))))
   
