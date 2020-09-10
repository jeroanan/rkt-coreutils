#lang typed/racket

(provide help-function)

(: print-methods (-> (Listof String) Void))
(define (print-methods methods)
  (displayln "Methods:")
  (for ([m methods])
       (displayln m))
  (displayln "(help) -- Display this help message."))

(define-syntax (help-function stx)
  (syntax-case stx ()
    [(_ help-text)
     #'(define/public (help)      
       (for ([hs help-text])
            (displayln hs)))]
    [(_ intro-text methods)
     #'(define/public (help)
         (displayln intro-text)
         (displayln "")
         (print-methods methods))]
    [(_ intro-text methods properties)
     #'(define/public (help)
         (displayln intro-text)
         (displayln "")
         (print-methods methods)
         (displayln "")
         (displayln "Properties:")
         (displayln "(all properties prefixed with get- or set-)" )
         (for ([ps properties])
                (displayln ps)))]))
     
