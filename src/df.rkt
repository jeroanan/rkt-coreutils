#lang s-exp "util/frontend-program.rkt"

(backend-code "repl/df.rkt")

(require racket/cmdline
         typed/racket/class)

(boolean-parameter human-readable #f)

(command-line
  #:argv (current-command-line-arguments)
  #:once-each
  [("-H" "--human-readable") "print sizes as human-readable numbers" (human-readable #t)]
  [("-v" "--version") "display version information and exit" (print-version-text-and-exit)])

(let ([d (new df%)])
  (send d set-human-readable (human-readable))
  (send d execute))
      
;;(simple-program "repl/df.rkt" df%)
