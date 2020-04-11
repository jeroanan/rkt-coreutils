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

(require ffi/unsafe)

(define getgrgid%
  (class object%
    (super-new)

    (init gid)

    (define igid gid)

    (define/public (get-name) (grpstruct-name result))
    (define/public (get-password) (grpstruct-passwd result))
    (define/public (get-gid) (grpstruct-gid result))
    (define/public (get-members) (grpstruct-members result))

    (define clib (ffi-lib #f))
    
    (define-cstruct _grpstruct ([name _string]
                                [passwd _string]
                                [gid _uint]
                                [members _string]))
    
    (define getgrgid (get-ffi-obj
                      "getgrgid" clib
                      (_fun #:save-errno 'posix
                            _int -> _grpstruct-pointer)) )

    (define result (getgrgid igid))))
    
(provide getgrgid%)