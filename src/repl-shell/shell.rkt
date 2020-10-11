#lang racket

(define-syntax (shell-command2 stx)
  (syntax-case stx ()
    [(_ mod-path name)
     (with-syntax ([internal-name
                    (datum->syntax #'name
                                   (string->symbol
                                    (format "_~a"
                                            (syntax->datum #'name))))])
       #'(begin
           (require (rename-in mod-path (name internal-name)))
           (provide name)

           (define name
             (λ (f . fs)
               (let* ([files (cons f fs)])
                 (apply internal-name files))))))]))
                 
     
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
     ;; Same as above but without default-func Instead, a function is 
     ;; provided that takes a single argument and passes that arguent 
     ;; to the object's execute function.
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

            (provide name)
            (define (name param)
              (send name-obj execute param))
            
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

(shell-command2 "../repl/base64.rkt" base64)
(shell-command2 "../repl/basename.rkt" basename)
(shell-command2 "../repl/cat.rkt" cat)
(shell-command2 "../repl/df.rkt" df)
(shell-command2 "../repl/groups.rkt" groups)
(shell-command2 "../repl/head.rkt" head)
(shell-command2 "../repl/hostid.rkt" hostid)
(shell-command2 "../repl/hostname.rkt" hostname)
(shell-command2 "../repl/id.rkt" id)

(shell-command "../repl/logname.rkt"
               logname
               logname%
               (execute-command logname))

(shell-command "../repl/ls.rkt"
               ls 
               ls% 
               (execute-command ls (list (path->string (current-directory)))))

(shell-command2 "../repl/md5sum.rkt" md5sum)
(shell-command2 "../repl/nl.rkt" nl)
(shell-command2 "../repl/nproc.rkt" nproc)
(shell-command2 "../repl/realpath.rkt" realpath)
               
(shell-command "../repl/sha1sum.rkt"
               sha1sum
               sha1sum%)
               
(shell-command "../repl/sha224sum.rkt"
               sha224sum
               sha224sum%)
               
(shell-command "../repl/sha256sum.rkt"
               sha256sum
               sha256sum%)
               
(shell-command "../repl/sha384sum.rkt"
               sha384sum
               sha384sum%)
               
(shell-command "../repl/sha512sum.rkt"
               sha512sum
               sha512sum%)
               
(shell-command2 "../repl/sort.rkt" sort)

(shell-command "../repl/stat.rkt"
               stat
               stat%)

(shell-command2 "../repl/sum.rkt" sum)
(shell-command2 "../repl/tac.rkt" tac)
(shell-command2 "../repl/tty.rkt" tty)
(shell-command2 "../repl/uniq.rkt" uniq)
(shell-command2 "../repl/uptime.rkt" uptime)
(shell-command2 "../repl/users.rkt" users)

(shell-command "../repl/who.rkt"
               who 
               who% 
               (execute-command who))

(shell-command "../repl/whoami.rkt"
               whoami
               whoami%
               (execute-command whoami))

(shell-command "../repl/yes.rkt"
               yes
               yes%
               (execute-command yes))

