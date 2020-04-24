#lang typed/racket

; Copyright 2020 David Wilson
; See COPYING for licence details

(provide file-by-file-processor)

(define-syntax-rule (file-by-file-processor file-function)
  (begin
    (: execute (-> (Listof String) Void))
    (define/public (execute files)
      (if (empty? files)
          (process-stdin)
          (process-files files)))

    (: process-files (-> (Listof String) Void))
    (define/private (process-files files)
      (for ([file-name files])
        (let* ([f (open-input-file file-name #:mode 'text)]
               [lines (port->lines f)])
          (file-function lines))))

    (: process-stdin (-> Void))
    (define/private (process-stdin)
      (let ([lines (port->lines (current-input-port))])
        (file-function lines)))))