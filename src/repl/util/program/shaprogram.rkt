#lang typed/racket

(provide sha-program)

(define-syntax-rule (sha-program description type-name sha-method)
  (begin
    (require/typed sha
                   [sha1  (-> Bytes Bytes)]
                   [bytes->hex-string (-> Bytes String)])
    (define type-name
      (class object%

        (super-new)
        (define/public (help)
          (let ([help-strings (list (format "Compute and print ~a message digest" description)
                                    ""
                                    "Methods:"
                                    "(help) -- display this help message"
                                    (format "(execute files) -- compute and print ~a message digest for files" description))])
            (for ([hs help-strings])
              (displayln hs))))

        (define/public (execute [files : (Listof String)])
          (for ([file files])
            (let* ([f (open-input-file file #:mode 'text )]
                   [the-hash (sha-method (port->bytes f))]
                   [hash-string (bytes->hex-string the-hash)])
              (displayln (format "~a ~a" hash-string file)))))))))
