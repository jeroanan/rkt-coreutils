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

(provide cat%)

(define cat%
  (class object%
    (super-new)

    (define/public (help)
      (let ([help-strings (list "Concatenate files and print on standard output."
                                ""
                                "Methods:"
                                "(help) -- display this help message"
                                "(execute FILES) -- concatenate and print FILES")])
        (for ([hs help-strings])
          (displayln hs))))

    (define/public (execute [files : (Listof String)])

      (: print-file-contents (-> Input-Port Void))
      (define (print-file-contents file-port)
        (let ([#{the-line : (U EOF String)} (read-line file-port)])
          (unless (eof-object? the-line)
            (begin
              (displayln the-line)
              (print-file-contents file-port)))))
             
             
      (for ([file-name files])
        (let ([f (open-input-file file-name #:mode 'text )])
          (print-file-contents f))))))
