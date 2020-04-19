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

(provide md5sum%)

(require/typed file/md5
               [md5 (-> Input-Port Bytes)])

(define md5sum%
  (class object%
    (super-new)

    (define (help)
      (let ([help-strings (list "Compute and print MD5 message digest"
                                ""
                                "Methods:"
                                "(help) -- display this help message"
                                "(execute files) -- compute and print MD5 message digest for files")])
        (for ([hs help-strings])
          (displayln hs))))
    
    (define/public (execute [files : (Listof String)])
      (for ([f files])
        (let* ([ip (open-input-file f #:mode 'text)]
               [the-sum (md5 ip)])
          (displayln
           (format "~a ~a" the-sum f)))))))
