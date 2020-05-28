#lang s-exp "ffi.rkt"

; Copyright 2020 David Wilson
; See COPYING for details

(provide get-utmp%
         get-utmp
         (struct-out utmpstruct))

(require ffi/unsafe
         racket/bool
         racket/class)

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

(define-cstruct _exitstatus([e_termination _short]
                            [e_exit        _short]))

(define-cstruct _timeval([time_t      _int32]
                         [suseconds_t _int32]))


(define-cstruct _utmpstruct([ut_type    _short]
                            [ut_pid     _pid_t]
                            [ut_line    _ut_line_t]
                            [ut_id      _ut_id_t]
                            [ut_user    _ut_user_t]
                            [ut_host    _ut_host_t]
                            [ut_exit    _exitstatus]
                            [ut_session _int32]
                            [ut_tv      _timeval]
                            [ut_addr_v6 _int32_t]
                            [unused     _ut_unused_t]))

(define clib (ffi-lib #f))
(c-function setutent clib _void "setutxent")
(c-function endutent clib _void "endutxent")
(c-function getutent clib _utmpstruct-pointer/null "getutxent")

(define get-utmp%
  (class object%
    (super-new)

    (define/public (start-utmp)
      (setutent))

    (define/public (end-utmp)
      (endutent))

    (define current-utmp null)

    (define/public (next-utmp)
      (let ([u (getutent)])
        (unless (false? u) (set! current-utmp u))
        (not (false? u))))

    (define-syntax-rule (string-member-from-bytes name value)
      (define/public (name) (bytes->string value)))
    
    (define/public (get-type) (utmpstruct-ut_type current-utmp))
    (define/public (get-pid) (utmpstruct-ut_pid current-utmp))
    (string-member-from-bytes get-line (utmpstruct-ut_line current-utmp))
    (define/public (get-id) (utmpstruct-ut_id current-utmp))
    (string-member-from-bytes get-user (utmpstruct-ut_user current-utmp))
    (string-member-from-bytes get-host (utmpstruct-ut_host current-utmp))
    (define/public (get-exit) (utmpstruct-ut_exit current-utmp))
    (define/public (get-session) (utmpstruct-ut_session current-utmp))

    (define/public (get-time)
      (let ([tv (utmpstruct-ut_tv current-utmp)])
        (timeval-time_t tv)))
    
    (define (bytes->string bs)
      (let* ([bytes-list (bytes->list bs)]
             [no-nulls (strip-null-bytes bytes-list)]
             [bytes-out (list->bytes no-nulls)]
             [out-str (bytes->string/utf-8 bytes-out)])
        out-str))

    (define (strip-null-bytes bs)
      (filter (λ (x) (not (eq? x 0))) bs))))

(define (get-utmp) (new get-utmp%))
  
