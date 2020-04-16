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

(define stat%
  (class object%
    (super-new)

    (init path file-name)

    (define ipath path)
    (define ifile-name file-name)
    
    (define full-path
      (if (string=? file-name "")
          path
          (build-path path file-name)))
 
    (define/public (get-dev) (statstruct-dev stat-buf))
    (define/public (get-inode) (statstruct-ino stat-buf))
    (define/public (get-mode) (statstruct-mode stat-buf))
    (define/public (get-number-of-hardlinks) (statstruct-nlink stat-buf))
    (define/public (get-uid) (statstruct-uid stat-buf))
    (define/public (get-gid) (statstruct-gid stat-buf))
    (define/public (get-rdev) (statstruct-rdev stat-buf))
    (define/public (get-size) (statstruct-size stat-buf))
    (define/public (get-block-size) (statstruct-blksize stat-buf))
    (define/public (get-blocks) (statstruct-blocks stat-buf))
    (define/public (get-accessed-time) (timespec-sec (statstruct-atim stat-buf)))
    (define/public (get-modified-time) (timespec-sec (statstruct-mtim stat-buf)))
    (define/public (get-created-time) (timespec-sec (statstruct-ctim stat-buf)))

    (define (has-file-type-flag? file-type-mask) (eq? (bitwise-and (get-mode) s-ifmt) file-type-mask))
    
    (define/public (get-is-regular-file?) (has-file-type-flag? s-ifreg))
    (define/public (get-is-directory?) (has-file-type-flag? s-ifdir))
    (define/public (get-is-character-device?) (has-file-type-flag? s-ifchr))
    (define/public (get-is-block-device?) (has-file-type-flag? s-ifblk))
    (define/public (get-is-fifo?) (has-file-type-flag? s-ififo))
    (define/public (get-is-symbolic-link?) (has-file-type-flag? s-iflnk))
    (define/public (get-is-socket?) (has-file-type-flag? s-ifsock))

    (define (has-mode-flag? flag-mask) (eq? (bitwise-and (get-mode) flag-mask) flag-mask))
    
    (define/public (get-has-set-user-id-bit?) (has-mode-flag? s-isuid))
    (define/public (get-has-set-group-id-bit?) (has-mode-flag? s-isgid))
    (define/public (get-has-sticky-bit?) (has-mode-flag? s-isvtx))         
    (define/public (get-owner-has-rwx?) (has-mode-flag? s-irwxu)) 
    (define/public (get-owner-has-r?) (has-mode-flag? s-irusr))
    (define/public (get-owner-has-w?) (has-mode-flag? s-iwusr))
    (define/public (get-owner-has-x?) (has-mode-flag? s-ixusr))
    (define/public (get-group-has-rwx?) (has-mode-flag? s-irwxg))
    (define/public (get-group-has-r?) (has-mode-flag? s-irgrp))
    (define/public (get-group-has-w?) (has-mode-flag? s-iwgrp))
    (define/public (get-group-has-x?) (has-mode-flag? s-ixgrp))
    (define/public (get-other-has-rwx?) (has-mode-flag? s-irwxo))
    (define/public (get-other-has-r?) (has-mode-flag? s-iroth))
    (define/public (get-other-has-w?) (has-mode-flag? s-iwoth))
    (define/public (get-other-has-x?) (has-mode-flag? s-ixoth))

    (define clib (ffi-lib #f))
    (define-cstruct _timespec ([sec _long]
                               [nsec _long]))

    (define-cstruct _statstruct([dev _long]
                                [ino _long]
                                [nlink _long]
                                [mode _uint]
                                [uid _uint]
                                [gid _uint]
                                [__pad0 _int]
                                [rdev _ulong]
                                [size _long]
                                [blksize _long]
                                [blocks _long]
                                [atim _timespec]
                                [mtim _timespec]
                                [ctim _timespec]
                                [__glibc_reserved (_array _long 3)]))

    (define stat (get-ffi-obj
                  "__xstat" clib
                  (_fun #:save-errno 'posix
                        _int _string _statstruct-pointer -> _int)) )
    
    (define stat-buf (cast
                      (malloc _statstruct)
                      _pointer
                      _statstruct-pointer))
    
    (define stat-result (stat 1 full-path stat-buf))

    ;; file type mask definitions
    (define s-ifmt #o00170000)
    (define s-ifsock #o0140000)
    (define s-iflnk #o0120000)
    (define s-ifreg #o0100000)
    (define s-ifblk #o0060000)
    (define s-ifdir #o0040000)
    (define s-ifchr #o0020000)
    (define s-ififo #o0010000)
        
    ;; mode mask definitions    
    (define s-isuid #o04000)
    (define s-isgid #o02000)
    (define s-isvtx #o01000)
    
    (define s-irwxu #o00700)
    (define s-irusr #o00400)
    (define s-iwusr #o00200)
    (define s-ixusr #o00100)

    (define s-irwxg #o00070)
    (define s-irgrp #o00040)
    (define s-iwgrp #o00020)
    (define s-ixgrp #o00010)

    (define s-irwxo #o00007)
    (define s-iroth #o00004)
    (define s-iwoth #o00002)
    (define s-ixoth #o00001)))

(define (stat? x) #t)

(define (get-stat path file-name)
  (new stat% [path path] [file-name file-name]))

(provide stat% stat? get-stat)
