#lang typed/racket

(provide base64%)

(require/typed net/base64
               [base64-encode-stream (-> Input-Port Output-Port Void)])
(require "util/util.rkt"
         "util/file-by-file-processor.rkt")

(define base64%
  (class object%
    (super-new)

    (help-function (list "Output base64-encoded representation of FILES."
                         ""
                         "Methods:"
                         "(execute FILES) -- display the base64-encoded representation of FILES"
                         "(help) -- display this help message"))

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