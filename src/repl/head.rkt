#lang typed/racket/base

; Copyright 2020 David Wilson
; See COPYING for licence details

(provide head%)

(require typed/racket/class)

(require "../util/member.rkt"
         "util/util.rkt"
         "util/line-by-line-processor.rkt")

(define head%
  (class object%  
    (super-new)

    (integer-attribute number-of-lines 10 get-number-of-lines set-number-of-lines)

    (help-function (list "Print the first lines of each provided file."
                         ""
                         "Methods:"
                         "(set-number-of-lines NUM) -- set the number of lines to be printed to NUM"
                         "(get-number-of-lines) -- get the number of lines to be printed"
                         "(execute FILES) -- display the first lines of FILES"))

    (line-by-line-processor line-handler)

    (integer-attribute counter 0)
    
    (: line-handler (-> String Void))
    (define/private (line-handler line)
      (begin
        (displayln line)
        (set! counter (add1 counter))
        (when (eq? counter number-of-lines) (exit 0))))))
   