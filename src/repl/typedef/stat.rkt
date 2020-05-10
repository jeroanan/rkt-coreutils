#lang typed/racket/base

(provide Stat%)

(define-type Stat%
  (Class 
   [get-owner-has-rwx? (-> Boolean)]
   [get-owner-has-r? (-> Boolean)]
   [get-owner-has-w? (-> Boolean)]
   [get-owner-has-x? (-> Boolean)]
   [get-group-has-rwx? (-> Boolean)]
   [get-group-has-r? (-> Boolean)]
   [get-group-has-w? (-> Boolean)]
   [get-group-has-x? (-> Boolean)]
   [get-other-has-rwx? (-> Boolean)]
   [get-other-has-r? (-> Boolean)]
   [get-other-has-w? (-> Boolean)]
   [get-other-has-x? (-> Boolean)]
   [get-is-directory? (-> Boolean)]
   [get-is-regular-file? (-> Boolean)]
   [get-is-character-device? (-> Boolean)]
   [get-is-symbolic-link? (-> Boolean)]
   [get-is-fifo? (-> Boolean)]
   [get-dev (-> Number)]
   [get-uid (-> Number)]
   [get-gid (-> Integer)]
   [get-accessed-time (-> Integer)]
   [get-modified-time (-> Integer)]
   [get-created-time (-> Integer)]
   [get-size (-> Number)]
   [get-blocks (-> Number)]
   [get-block-size (-> Number)]
   [get-inode (-> Number)]
   [get-number-of-hardlinks (-> Number)]))