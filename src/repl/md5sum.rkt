#lang s-exp "util/repl-program.rkt"

; Copyright 2020 David Wilson
; See COPYING for details

(provide md5sum%)

(require racket/list)

(require/typed file/md5
               [md5 (-> Input-Port Bytes)])

(define md5sum%
  (class object%
    (super-new)

    (help-function "Compute and print MD5 message digest"
                   (list "(execute files) -- compute and print MD5 message digest for files"))
    
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
