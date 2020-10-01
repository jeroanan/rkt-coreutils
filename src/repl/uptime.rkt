#lang s-exp "util/program/repl-program.rkt"

;; Copyright 2020 David Wilson
;; See COPYING for licence

(provide uptime%)

(require racket/date
         racket/format
         racket/list
         racket/port
         racket/string)

(require "util/getutmp.rkt"
         "libc/stdlib.rkt")

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
      "Display system uptime"
      (list "(execute) -- Display system uptime"))

    ;; Main program execution
    (on-execute-with-void
      (let* ([f (open-input-file uptime-file #:mode 'text)]
             [c (first (port->lines f))]
             [secs (string->number (first (string-split c " ")))]
             [uptime-days (floor (/ secs seconds-per-day))]
             [uptime-days-secs (* uptime-days seconds-per-day)]
             [secs-less-days (- secs uptime-days-secs)]
             [uptime-hours  (floor (/ secs-less-days seconds-per-hour))]
             [uptime-hours-secs (* uptime-hours 3600)]
             [secs-less-days-and-hours (- secs uptime-days-secs uptime-hours-secs)]
             [uptime-minutes (floor (/ secs-less-days-and-hours seconds-per-minute))]
             [load-avgs (get-load-avgs)]
             [no-of-users (length (get-user-process-utmp-entries))])
        (displayln (format " ~a up ~a days, ~a:~a,  ~a users,  load average: ~a, ~a, ~a"
                           (current-time)
                           (real->integer-string uptime-days)
                           (real->integer-string uptime-hours)
                           (real->integer-string uptime-minutes)
                           no-of-users
                           (first load-avgs) 
                           (second load-avgs)
                           (third load-avgs)))))

    ;; Take a real number (e.g. 39.0) and return the integer portion (e.g. 39) as a string
    (define/private (real->integer-string real-in)
      (let* ([s (number->string real-in)]
             [sp (string-split s ".")])
        (if (< real-in 10)
            (format "0~a" (first sp))
            (first sp))))

    ;; Get the current time part of the current date.
    (define/private (current-time)
      (let* ([d (current-date)]
             [h (date-hour d)]
             [m (date-minute d)]
             [s (date-second d)])
        (format "~a:~a:~a" (pad-zero h) (pad-zero m) (pad-zero s))))

    ;; If the given number is single-digit prefix it with a zero
    (define/private (pad-zero num-in)
      (let ([s (number->string num-in)])
        (if (< num-in 10) 
            (format "0~a" s)
            s)))))
