#lang typed/racket/base

(provide base64%)

(require typed/racket/class
         racket/list)

(require/typed net/base64
               [base64-encode-stream (-> Input-Port Output-Port Void)])

(require "util/help.rkt")

(define base64%
  (class object%
    (super-new)

    (help-function 
      "Output base64-encoded representation of FILES."
      (list "(execute FILES) -- Display the base64-encoded representation of FILES"
            "(help) -- Display this help message")
      (list))

    (: execute (-> (Listof String) Void))
    (define/public (execute files)
      (if (empty? files)
          (process-stdin)
          (process-files files)))

    (: process-files (-> (Listof String) Void))
    (define/private (process-files files)
      (for ([file-name files])
        (let* ([f (open-input-file file-name #:mode 'text)])
          (base64-encode-stream f (current-output-port)))))

    (: process-stdin (-> Void))
    (define/private (process-stdin)
      (base64-encode-stream (current-input-port) (current-output-port)))))
