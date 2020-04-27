#lang typed/racket/base

(provide sha-program)

(require typed/racket/class
         racket/port
         racket/list)

(define-syntax-rule (sha-program description type-name sha-method)
  (begin
    (require/typed sha
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
          (if (empty? files)
              (process-stdin)
              (process-files files)))

        (: process-stdin (-> Void))
        (define/private (process-stdin)
          (let ([the-hash (port->hash-string (current-input-port))])
            (display-result the-hash "-")))

        (: process-files (-> (Listof String) Void))
        (define/private (process-files files)
          (for ([file files])
            (let* ([f (open-input-file file #:mode 'text )]
                   [the-hash (port->hash-string f)])
              (display-result the-hash file))))

        (: port->hash-string (-> Input-Port String))
        (define/private (port->hash-string ip)
          (let ([the-hash (sha-method (port->bytes ip))])
            (bytes->hex-string the-hash)))

        (: display-result (-> String String Void))
        (define/private (display-result hash-str file-name)
          (displayln (format "~a  ~a" hash-str file-name)))))))
