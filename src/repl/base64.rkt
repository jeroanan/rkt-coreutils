#lang s-exp "util/program/file-by-file-processor-program.rkt"

(require racket/list
         net/base64)

(file-by-file-processor base64 process-file null #t)

(define (process-file filename ip)
  (base64-encode-stream ip (current-output-port)))
