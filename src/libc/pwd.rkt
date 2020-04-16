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

(define getpwuid%
  (class object%
    (super-new)

    (init uid)

    (define iuid uid)

    (define/public (get-username) (passwdstruct-name result))
    (define/public (get-password) (passwdstruct-passwd result))
    (define/public (get-uid) (passwdstruct-uid result))
    (define/public (get-gid) (passwdstruct-gid result))
    (define/public (get-gecos) (passwdstruct-gecos result))
    (define/public (get-home-dir) (passwdstruct-dir result))
    (define/public (get-shell) (passwdstruct-shell result))

    (define clib (ffi-lib #f))

    (define-cstruct _passwdstruct([name _string]
                                  [passwd _string]
                                  [uid _uint]
                                  [gid _uint]
                                  [gecos _string]
                                  [dir _string]
                                  [shell _string]))

    (define getpwuid (get-ffi-obj
                  "getpwuid" clib
                  (_fun #:save-errno 'posix
                         _int -> _passwdstruct-pointer)) )

    (define result (getpwuid iuid))))

(define (get-pwuid uid)
  (new getpwuid% [uid uid]))  

(provide getpwuid% get-pwuid)