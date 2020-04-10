#lang racket

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

(define (get-year-or-time year time)
  (let ([current-date (seconds->date (current-seconds))])
    (if (> (date-year current-date) year)
        year
        time)))

(define (stringify-list the-list)
  (map (λ (x) (if (string? x)
                  x
                  (number->string x)))
       the-list))
                        
(define (unix-seconds->human-date unix-seconds)
  (let* ([the-date (seconds->date unix-seconds)]
         [year (date-year the-date)]
         [month (list-ref month-names (date-month the-date))]
         [day (number->string (date-day the-date))]
         [hour (number->string (date-hour the-date))]
         [minute (number->string (date-minute the-date))]
         [time (string-append hour ":" minute)]
         [year-or-time (get-year-or-time year time)]
         [output-list (stringify-list (list month day year-or-time))])
    (string-join output-list " ")))

(provide unix-seconds->human-date)
  