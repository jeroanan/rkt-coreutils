#lang typed/racket/base

; Copyright 2020 David Wilson
; See COPYING for details

(provide md5sum%)

(require typed/racket/class
         racket/list)

(require/typed file/md5
               [md5 (-> Input-Port Bytes)])

(define md5sum%
  (class object%
    (super-new)

    (define (help)
      (let ([help-strings (list "Compute and print MD5 message digest"
                                ""
                                "Methods:"
                                "(help) -- display this help message"
                                "(execute files) -- compute and print MD5 message digest for files")])
        (for ([hs help-strings])
          (displayln hs))))
    
    (define/public (execute [files : (Listof String)])
      (if (empty? files)
          (process-stdin)
          (process-files files)))      

    (: process-files (-> (Listof String) Void))
    (define/private (process-files files)
      (for ([f files])
        (let* ([ip (open-input-file f #:mode 'text)]
               [the-sum (md5 ip)])
          (displayln
           (format "~a ~a" the-sum f)))))
    
    (: process-stdin (-> Void))
    (define/private (process-stdin)
      (let ([the-sum (md5 (current-input-port))])
        (displayln (format "~a -" the-sum))))))
