#lang racket
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