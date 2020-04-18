#lang typed/racket

(provide head%)

(define head%
  (class object%  
    (super-new)

    (: number-of-lines Integer)
    (field [number-of-lines 10])
    
    (define/public (set-number-of-lines [n : Integer]) (set! number-of-lines n))
    (define/public (get-number-of-lines) number-of-lines)

    (define/public (help)
      (let ([help-strings (list "Print the first lines of each provided file."
                                ""
                                "Methods:"
                                "(set-number-of-lines NUM) -- set the number of lines to be printed to NUM"
                                "(get-number-of-lines) -- get the number of lines to be printed"
                                "(execute FILES) -- display the first lines of FILES")])
        (for ([hs help-strings])
          (displayln hs))))
    
    (define/public (execute [files : (Listof String)])
      (for ([file-name files])
        (when (> (length files) 1) (displayln (format "==> ~a <==" file-name)))
        (let ([f (open-input-file file-name #:mode 'text )])
          (for ([i number-of-lines])
            (displayln (read-line f))))))))