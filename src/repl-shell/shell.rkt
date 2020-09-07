#lang racket

(require "../repl/ls.rkt")

(provide ls)

(define-syntax (shell-command stx)
  (syntax-case stx ()
    [(_ name class)
     (with-syntax ([help-func 
                     (datum->syntax #'name
                                    (string->symbol (format "~a-help"
                                                            (syntax->datum #'name))))]
                   [name-obj
                     (datum->syntax #'name
                                    (string->symbol (format "~a-obj"
                                                            (syntax->datum #'name))))])
        #'(begin
            (provide help-func name-obj)
            (define name-obj (new class))

            
            (define (help-func)
              (send name help))))]))


(shell-command ls ls%)
(define (ls) (send ls-obj execute (list (path->string (current-directory)))))

