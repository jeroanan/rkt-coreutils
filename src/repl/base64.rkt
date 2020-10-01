#lang s-exp "util/program/repl-program.rkt"

(provide base64%)

(require racket/list
         net/base64)

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

    (define/private (process-files files)
      (for ([file-name files])
        (let* ([f (open-input-file file-name #:mode 'text)])
          (base64-encode-stream f (current-output-port)))))

    (define/private (process-stdin)
      (base64-encode-stream (current-input-port) (current-output-port)))))
