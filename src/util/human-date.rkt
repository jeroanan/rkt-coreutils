#lang typed/racket

; Copyright 2020 David Wilson
; See COPYING for details

(require "stringutil.rkt")

(define month-names
  (list ""
        "Jan"
        "Feb"
        "Mar"
        "Apr"
        "May"
        "Jun"
        "Jul"
        "Aug"
        "Sep"
        "Oct"
        "Nov"
        "Dev"))

(define (get-year-or-time
         [year : Integer]
         [time : String])
  (let ([current-date (seconds->date (current-seconds))])
    (if (> (date-year current-date) year)
        (right-aligned-string (number->string year) 5)
        time)))

(define (pad-time-element [te : String])
  (if (eq? (string-length te) 1)
      (~a "0" te)
      te))
  
(define (unix-seconds->human-date [unix-seconds : Integer])
  (let* ([the-date (seconds->date unix-seconds)]
         [year (date-year the-date)]
         [month (list-ref month-names (date-month the-date))]
         [day (number->string (date-day the-date))]
         [hour (pad-time-element (number->string (date-hour the-date)))]
         [minute (pad-time-element (number->string (date-minute the-date)))]
         [time (string-append hour ":" minute)]
         [year-or-time : String (get-year-or-time year time)]
         [output-list (list month day year-or-time)])
    (string-join output-list " ")))

(provide unix-seconds->human-date)
  