#lang typed/racket

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

(provide whoami%)

(require "../typedef/getpwuid.rkt")

(require/typed "../libc/pwd.rkt"
               [get-pwuid (-> Number (Instance Getpwuid%))])

(require/typed "../libc/unistd.rkt"
               [get-euid (-> Integer)])

(define whoami%
  (class object%
    (super-new)

    (define/public (help)
      (let ([help-strings (list "Print effective userid"
                                ""
                                "Methods:"
                                "(help) -- display this help message"
                                "(execute) -- display the effective userid")])
        (for ([hs help-strings])
          (displayln hs))))
    
    (define/public (execute)
      (let* ([uid (get-euid)]
             [getpwuid (get-pwuid uid)])
        (displayln (send getpwuid get-username))))))