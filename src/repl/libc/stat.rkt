#lang s-exp "ffi.rkt"

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

(provide stat% stat? get-stat)

(require ffi/unsafe
         racket/class
         racket/runtime-path
         (for-syntax racket/base))

(define-runtime-path lib-path (build-path ".." ".." ".." "lib" "stat"))
(define clib (ffi-lib lib-path))

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

    (define _dev_t _long)
    (define _ino_t _long)
    (define _mode_t _uint)
    (define _nlink_t _long)
    (define _uid_t _uint)
    (define _gid_t _uint)
    (define _off_t _long)
    (define _blksize_t _long)
    (define _blkcnt_t _long)
    
    (c-function stat clib _int "getstat" _string)
    (c-function _get-dev clib _dev_t "get_dev")
    (c-function _get-ino clib _ino_t "get_ino")
    (c-function _get-mode clib _mode_t "get_mode")
    (c-function _get-nlink clib _nlink_t "get_nlink")
    (c-function _get-uid clib _uid_t "get_uid")
    (c-function _get-gid clib _gid_t "get_gid")
    (c-function _get-rdev clib _dev_t "get_rdev")
    (c-function _get-size clib _off_t "get_size")
    (c-function _get-blksize clib _blksize_t "get_blksize")
    (c-function _get-blocks clib _blkcnt_t "get_blocks")
    (c-function _get-atime clib _long "get_atime")
    (c-function _get-mtime clib _long "get_mtime")
    (c-function _get-ctime clib _long "get_ctime")
    
    (define/public (get-dev) (_get-dev))
    (define/public (get-inode) (_get-ino))
    (define/public (get-mode) (_get-mode))
    (define/public (get-number-of-hardlinks) (_get-nlink))
    (define/public (get-uid) (_get-uid))
    (define/public (get-gid) (_get-gid))
    (define/public (get-rdev) (_get-dev))
    (define/public (get-size) (_get-size))
    (define/public (get-block-size) (_get-blksize))
    (define/public (get-blocks) (_get-blocks))
    (define/public (get-accessed-time) (_get-atime))
    (define/public (get-modified-time) (_get-mtime))
    (define/public (get-created-time) (_get-ctime))

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

    (stat full-path)
    
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


