#lang racket/base

; Copyright 2020 David Wilson
; See COPYING for details

(provide get-utmp
         get-utmp-users)

(require ffi/unsafe
         racket/bool)

(define (_bytes/len n)
  (make-ctype (make-array-type _byte n)
              ;; see https://github.com/dyoo/ffi-tutorial

              ;; ->c
              (lambda (v)
                (unless (and (bytes? v) (= (bytes-length v) n))
                  (raise-argument-error '_chars/bytes 
                                        (format "bytes of length ~a" n)
                                        v))
                v)

              ;; ->racket
              (lambda (v)
                (make-sized-byte-string v n))))


(define _pid_t _int32)
(define _int32_t _int32)
(define _ut_user_t (_bytes/len 32))
(define _ut_line_t (_bytes/len 32))
(define _ut_id_t (_bytes/len 4))
(define _ut_host_t (_bytes/len 256))
(define _ut_unused_t (_bytes/len 20))

(define LOGIN_PROCESS 6)
(define USER_PROCESS 7)

(define-cstruct _exitstatus([e_termination _short]
                            [e_exit        _short]))

(define-cstruct _timeval([time_t      _long]
                         [suseconds_t _long]))


(define-cstruct _utmpstruct([ut_type    _short]
                            [ut_pid     _pid_t]
                            [ut_line    _ut_line_t]
                            [ut_id      _ut_id_t]
                            [ut_user    _ut_user_t]
                            [ut_host    _ut_host_t]
                            [ut_exit    _exitstatus]
                            [ut_session _long]
                            [ut_tv      _timeval]
                            [ut_addr_v6 _int32_t]
                            [unused     _ut_unused_t]))

(define clib (ffi-lib #f))
(define setutent (get-ffi-obj "setutxent" clib (_fun -> _void)))
(define endutent (get-ffi-obj "endutxent" clib (_fun -> _void)))
(define getutent (get-ffi-obj "getutxent" clib (_fun #:save-errno 'posix -> _utmpstruct-pointer/null)))

(define (get-utmp)
  (define (get-entries entries)    
    (let ([u (getutent)])      
      (if (false? u)
          entries
          (get-entries (append entries (list (struct-copy utmpstruct u)))))))
  
    (setutent)
    (define entries (get-entries (list)))
    (endutent)
    entries)

(define (get-utmp-users)
  (let* ([utmp (filter-utmps-by-process-type (get-utmp) USER_PROCESS)]
         [users (map (λ (x) (utmpstruct-ut_user x)) utmp)]
         [no-nulls-users (map strip-null-bytes users)]
         [users-strings (map (λ (x) (bytes->string/utf-8 (list->bytes x))) no-nulls-users)])    
    users-strings))

(define (filter-utmps-by-process-type utmps type)
  (filter (λ (x) (eq? (utmpstruct-ut_type x) type)) utmps))

(define (strip-null-bytes bs)
  (filter (λ (x) (not (eq? x 0))) (bytes->list bs)))
