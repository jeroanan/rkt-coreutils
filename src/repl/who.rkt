#lang s-exp "util/program/repl-program.rkt"

;; Copyright 2020 David Wilson
;; See COPYING for details

(provide who%)

(require "typedef/getutmp.rkt"
         "../util/stringutil.rkt"
         "util/getutmp.rkt")

(require typed/racket/date
         racket/format
         racket/string
         racket/list)

;; who% -- emulate the functionality of the coreutils "who" command.
;; type "man who" at a shell prompt for documentation on the original
;; program.
(define who%
  (class object%
    (super-new)
   
    (help-function
      "Print information about users that are currently logged in."
      (list 
           "(help) -- Display this help message"
           "(execute) -- Print information about users that are currently logged in.")
      (list "show-header (bool) -- Whether or not to print header line"))

    ;; Whether to show headings above the listing
    (public-boolean-attribute show-header #t)    
    
    ;; How wide various column widths in output should be
    (: user-column-width Exact-Nonnegative-Integer)
    (define user-column-width 8)

    (: line-column-width Exact-Nonnegative-Integer)
    (define line-column-width 12)

    (: time-column-width Exact-Nonnegative-Integer)
    (define time-column-width 16)
    
    ;; peform the "who" program execution
    (on-execute-with-void
      (let ([entries (get-user-process-utmp-entries)])
        (when show-header (displayln (get-header)))
        (for ([e entries])
          (let* ([output-fields (list (left-aligned-string (whoentry-user e) user-column-width)
                                      (left-aligned-string (whoentry-line e) line-column-width)
                                      (make-time-string (whoentry-time e))
                                      (~a "(" (whoentry-host e) ")"))]
                 [output (string-join output-fields " ")])
            (displayln output)))))     
    
    (date-display-format 'iso-8601)

    ;; Build the string that will be used for the header of the listing.
    (: get-header (-> String))
    (define/private (get-header)
      (let ([name-header (left-aligned-string "NAME" user-column-width)]
            [line-header (left-aligned-string "LINE" line-column-width)]
            [time-header (left-aligned-string "TIME" time-column-width)]
            [comment-header "COMMENT"])
        (~a name-header " " line-header " " time-header " " comment-header)))

    ;; Take an iso-8601and output an yyyy-mm-dd HH:MM timestring.
    (: make-time-string (-> Integer String))
    (define (make-time-string seconds)
      (let* ([the-date (seconds->date seconds)]
             [date-str (date->string the-date #t)]
             [date-time (string-split date-str "T")]             
             [time-split (string-split (second date-time) ":")]
             [date-out (first date-time)]
             [time-out (~a (first time-split) ":" (second time-split))])        
             
      (~a date-out " " time-out)))))
