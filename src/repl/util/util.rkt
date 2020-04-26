#lang typed/racket/base

(provide help-function)

(require racket/class)

(define-syntax-rule (help-function help-text)
  (define/public (help)      
    (for ([hs help-text])
      (displayln hs))))