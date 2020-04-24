#lang typed/racket

; Copyright 2020 David Wilson
; See COPYING for details


(provide line-by-line-processor)

(define-syntax-rule (line-by-line-processor line-function)
  (begin 
    (define/public (execute [files : (Listof String)])
      (begin
        (if (empty? files)
            (process-stdin)
            (process-files files))))

    (: process-files (-> (Listof String) Void))
    (define/private (process-files files)
      (for ([file-name files])
        (let* ([f (open-input-file file-name #:mode 'text )])
          (for ([l (in-lines f)])
            (line-function l)))))

    (: process-stdin (-> Void))
    (define/private (process-stdin)
      (let* ([r (read-line)]
             [rs (~a r)])
        (when (not (eof-object? r))
          (line-function rs)
          (process-stdin))))))
