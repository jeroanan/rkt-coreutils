#lang s-exp "util/program/file-by-file-processor-program.rkt"

; Copyright 2020 David Wilson
; See COPYING for details

(provide md5sum%)

(require racket/list
         file/md5)

(define help-text (list "Compute and print MD5 message digest"
                        "(execute files) -- compute and print MD5 message digest for files"))

(file-by-file-processor-program md5sum%
                                help-text
                                #t
                                file-handler
                                null)

(define (file-handler filename stream)  
  (let ([the-sum (md5 stream)])
    (displayln (format "~a ~a" the-sum filename))))
