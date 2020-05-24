#lang s-exp "ffi.rkt"

;; Copyright 2020 David Wilson
;; See COPYING for details.

(provide getpwuid%
         get-pwuid
         getpwnam%
         get-pwnam)

(require ffi/unsafe
         racket/class)

(define-cstruct _passwdstruct([name _string]
                                  [passwd _string]
                                  [uid _uint]
                                  [gid _uint]
                                  [gecos _string]
                                  [dir _string]
                                  [shell _string]))

(define clib (ffi-lib #f))

(define getpwd%
  (class object%
    (super-new)

    (field [result null])
    
    (define/public (get-username) (passwdstruct-name result))
    (define/public (get-password) (passwdstruct-passwd result))
    (define/public (get-uid) (passwdstruct-uid result))
    (define/public (get-gid) (passwdstruct-gid result))
    (define/public (get-gecos) (passwdstruct-gecos result))
    (define/public (get-home-dir) (passwdstruct-dir result))
    (define/public (get-shell) (passwdstruct-shell result))
    
    (define/public (set-result res) (set! result res))))

(define getpwuid%
  (class getpwd%
    (super-new)

    (init uid)

    (define iuid uid)    
    
    (c-function getpwuid clib _passwdstruct-pointer "getpwuid" _int)
    (send this set-result (getpwuid iuid))))

(define (get-pwuid uid)
  (new getpwuid% [uid uid]))

(define getpwnam%
  (class getpwd%
    (super-new)

    (init user-name)
    
    (define iuser-name user-name)

    (c-function getpwnam clib _passwdstruct-pointer "getpwnam" _string)

    (send this set-result (getpwnam iuser-name))))

(define (get-pwnam user-name)
  (new getpwnam% [user-name user-name]))

