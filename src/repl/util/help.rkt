#lang racket

(provide help-function)

(define-syntax (help-function stx)
  (syntax-case stx ()
    [(_ help-text)
     #'(define/public (help)      
       (for ([hs help-text])
            (displayln hs)))]
    [(_ intro-text methods properties)
     #'(define/public (help)
       (displayln intro-text)
       (displayln "")
       (displayln "Methods:")
       (for ([ms methods])
            (displayln ms))
       (displayln "")
       (displayln "Properties:")
       (displayln "(all properties prefixed with get- or set-)" )
       (for ([ps properties])
            (displayln ps)))]))
     
