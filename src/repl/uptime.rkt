#lang typed/racket/base

;; Copyright 2020 David Wilson
;; See COPYING for licence

(provide uptime%)

(require typed/racket/class
         typed/racket/date
         racket/format
         racket/list
         racket/port
         racket/string)

(require "util/util.rkt"
         "util/getutmp.rkt")

(require/typed "../libc/stdlib.rkt"
               [get-load-avgs  (-> (Listof Real))])

;; The file in GNU/Linux that holds the uptime number.
(define uptime-file "/proc/uptime")

(define seconds-per-day 86400)
(define seconds-per-hour 3600)
(define seconds-per-minute 60)

;; Uptime: Get system uptime
(define uptime%
  (class object%
    (super-new)

    (help-function
     (list
      "Display system uptime"
      ""
      "Methods:"
      "(help) -- Display this help message"
      "(execute) -- Display system uptime"
      "(get-data) -- Get the data that would be displayed by execute and return as a string"))
    
    ;; Display data obtained from get-data
    (define/public (execute)
      (displayln (get-data)))

    ;; Main program execution
    (: get-data (-> String))
    (define/public (get-data)
      (let* ([f (open-input-file uptime-file #:mode 'text)]
             [c (first (port->lines f))]
             [secs (assert (string->number (first (string-split c " "))) number?)]
             [uptime-days (floor (assert (/ secs seconds-per-day) real?))]
             [uptime-days-secs (* uptime-days seconds-per-day)]
             [secs-less-days (- secs uptime-days-secs)]
             [uptime-hours  (floor (assert (/ secs-less-days seconds-per-hour) real?))]
             [uptime-hours-secs (* uptime-hours 3600)]
             [secs-less-days-and-hours (- secs uptime-days-secs uptime-hours-secs)]
             [uptime-minutes (floor (assert (/ secs-less-days-and-hours seconds-per-minute) real?))]
             [load-avgs (get-load-avgs)]
             [no-of-users (length (get-user-process-utmp-entries))])
        (format " ~a up ~a days, ~a:~a,  ~a users,  load average: ~a, ~a, ~a"
                (current-time)
                (real->integer-string uptime-days)
                (real->integer-string uptime-hours)
                (real->integer-string uptime-minutes)
                no-of-users
                (first load-avgs)
                (second load-avgs)
                (third load-avgs))))

    ;; Take a real number (e.g. 39.0) and return the integer portion (e.g. 39) as a string
    (: real->integer-string (-> Real String))
    (define/private (real->integer-string real-in)
      (let* ([s (number->string real-in)]
             [sp (string-split s ".")])
        (if (< real-in 10)
            (format "0~a" (first sp))
            (first sp))))

    ;; Get the current time part of the current date.
    (: current-time (-> String))
    (define/private (current-time)
      (let* ([d (current-date)]
             [h (date-hour d)]
             [m (date-minute d)]
             [s (date-second d)])
        (format "~a:~a:~a" (pad-zero h) (pad-zero m) (pad-zero s))))

    ;; If the given number is single-digit prefix it with a zero
    (: pad-zero (-> Integer String))
    (define/private (pad-zero num-in)
      (let ([s (number->string num-in)])
        (if (< num-in 10) 
            (format "0~a" s)
            s)))))
