#lang racket/base

; Copyright 2020 David Wilson
; See COPYING for details

(provide getgrgid%
         get-getgrgid
         get-group-list)

(require ffi/unsafe
         racket/class
         racket/runtime-path
         racket/list
         (for-syntax racket/base))

;; wrapper for C getgrgid function. Take a gid and
;; store an instance of Grpstruct in a member
(define getgrgid%
  (class object%
    (super-new)

    (init gid)

    (define-cstruct _grpstruct ([name _string]
                                [passwd _string]
                                [gid _int]
                                [members _string]))
    
    (define igid gid)

    (define/public (get-name) (grpstruct-name result))
    (define/public (get-password) (grpstruct-passwd result))
    (define/public (get-gid) (grpstruct-gid result))
    (define/public (get-members) (grpstruct-members result))

    (define clib (ffi-lib #f))
    
    (define getgrgid (get-ffi-obj
                      "getgrgid" clib
                      (_fun #:save-errno 'posix
                            _int -> _grpstruct-pointer)) )

    (define result (getgrgid igid))))

(define (get-getgrgid gid)
  (new getgrgid% [gid gid]))

(define-runtime-path lib-path (build-path "src" "getgrouplist"))
(define clib (ffi-lib lib-path))

(define get-groups (get-ffi-obj "getgroups" clib (_fun _string _int _int -> (r : _int))))
(define get-number-of-groups (get-ffi-obj "get_number_of_groups" clib (_fun -> _int)))
(define get-next-group (get-ffi-obj "get_next_group_id" clib (_fun -> _int)))

(define (get-group-list user-name primary-gid)
  (define group-count (get-group-count user-name primary-gid))  
  (define r (get-groups user-name primary-gid group-count))
  (flatten (collate-group-ids (list))))

(define (get-group-count user-name primary-gid)
  (begin
    (get-groups user-name primary-gid 0)    
    (get-number-of-groups)))

(define (collate-group-ids ids)
  (define next-group-id (get-next-group))
  (if (eq? -1 next-group-id)
      ids
      (collate-group-ids (cons ids next-group-id))))