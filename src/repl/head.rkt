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

(provide head%)

(require "../util/member.rkt")

(define head%
  (class object%  
    (super-new)

    (integer-attribute number-of-lines get-number-of-lines set-number-of-lines 10)

    (define/public (help)
      (let ([help-strings (list "Print the first lines of each provided file."
                                ""
                                "Methods:"
                                "(set-number-of-lines NUM) -- set the number of lines to be printed to NUM"
                                "(get-number-of-lines) -- get the number of lines to be printed"
                                "(execute FILES) -- display the first lines of FILES")])
        (for ([hs help-strings])
          (displayln hs))))
    
    (define/public (execute [files : (Listof String)])
      (for ([file-name files])
        (when (> (length files) 1) (displayln (format "==> ~a <==" file-name)))
        (let ([f (open-input-file file-name #:mode 'text )])
          (for ([i number-of-lines])
            (displayln (read-line f))))))))