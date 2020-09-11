#lang s-exp "util/program/repl-program.rkt"

(provide base64%)

(require racket/list)

(require/typed net/base64
               [base64-encode-stream (-> Input-Port Output-Port Void)])

(define base64%
  (class object%
    (super-new)

    (help-function 
      "Output base64-encoded representation of FILES."
      (list "(execute FILES) -- Display the base64-encoded representation of FILES"))

    (on-execute-with-strings files
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
