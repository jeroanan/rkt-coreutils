#lang racket

(define-syntax (shell-command stx)
  (syntax-case stx ()
    ;; Add a shell command. Parameters:
    ;;   * mod-path -- the path to the moduel containing the command
    ;;   * name -- The name of the default function
    ;;   * class -- The class to instantiate
    ;;   * default-func the body of the default function
    ;;
    ;; Executing this macro:
    ;;   * requires mod-path
    ;;   * provides:
    ;;   *   <name>-help -- Executes the help method of the command's class
    ;;   *   <name>-obj -- The instance of the command object
    ;;   *   <name> -- The "default function" - running this executes code
    ;;         passed as default-func.
    [(_ mod-path name class default-func)
     (with-syntax ([help-func 
                     (datum->syntax #'name
                                    (string->symbol 
                                      (format "~a-help"
                                              (syntax->datum #'name))))]
                   [name-obj
                     (datum->syntax #'name
                                    (string->symbol 
                                      (format "~a-obj"
                                              (syntax->datum #'name))))])
        #'(begin
            (require mod-path)
            (provide name help-func name-obj)

            (define name-obj (new class))
            
            (define (name) 
              default-func)

            (define (help-func)
              (send name-obj help))))]
    [(_ mod-path name class)
     (with-syntax ([help-func 
                     (datum->syntax #'name
                                    (string->symbol 
                                      (format "~a-help"
                                              (syntax->datum #'name))))]
                   [name-obj
                     (datum->syntax #'name
                                    (string->symbol 
                                      (format "~a-obj"
                                              (syntax->datum #'name))))])
        #'(begin
            (require mod-path)
            (provide help-func name-obj)

            (define name-obj (new class))
            
            (define (help-func)
              (send name-obj help))))]))


(define-syntax (execute-command stx)
  (syntax-case stx ()
    [(_ name)
     #'(send (name-obj name) execute)]
    [(_ name param)
     #'(send (name-obj name) execute param)]))

(define-syntax (name-obj stx)
  (syntax-case stx ()
   [(_ name)
    (with-syntax ([name-obj
                    (datum->syntax #'name
                                   (string->symbol
                                     (format "~a-obj"
                                             (syntax->datum #'name))))])
                 #'name-obj)]))

(shell-command "../repl/df.rkt"
               df
               df%
               (execute-command df))

(shell-command "../repl/groups.rkt"
               groups
               groups%)
(provide groups)
(define (groups user-id)
  (send groups-obj execute user-id))

(shell-command "../repl/id.rkt"
               id
               id%
               (execute-command id))

(shell-command "../repl/ls.rkt"
               ls 
               ls% 
               (execute-command ls (list (path->string (current-directory)))))

(shell-command "../repl/logname.rkt"
               logname
               logname%
               (execute-command logname))

(shell-command "../repl/users.rkt"
               users
               users%
               (execute-command users))

(shell-command "../repl/uptime.rkt"
               uptime
               uptime%
               (execute-command uptime))

(shell-command "../repl/who.rkt"
               who 
               who% 
               (execute-command who))

(shell-command "../repl/whoami.rkt"
               whoami
               whoami%
               (execute-command whoami))

