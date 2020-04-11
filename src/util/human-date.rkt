#lang racket

; Copyright 2020 David Wilson

;This program is free software: you can redistribute it and/or modify
;it under the terms of the GNU General Public License as published by
;the Free Software Foundation, either version 3 of the License, or
;(at your option) any later version.
;
;This program is distributed in the hope that it will be useful,
;but WITHOUT ANY WARRANTY; without even the implied warranty of
;MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;GNU General Public License for more details.
;
;You should have received a copy of the GNU General Public License
;along with this program.  If not, see <https://www.gnu.org/licenses/>.

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
  