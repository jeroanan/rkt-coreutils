#lang racket
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

(provide getpwuid%)