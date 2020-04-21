#lang typed/racket

(provide help-function)

(define-syntax-rule (help-function help-text)
  (define/public (help)      
    (for ([hs help-text])
      (displayln hs))))