#lang typed/racket/base

;; Copyright 2020 David Wilson
;; See COPYING for licence

(require typed/racket/class
         racket/list
         racket/port
         racket/string)

(define df%
  (class object%
    (super-new)

    (define/public (execute)
      (let ([mountinfo-entries (get-mountinfo-entries)])
        (for ([m mountinfo-entries])
          (displayln (mountinfo-entry-id m)))))
    
    (define mountinfo-file "/proc/self/mountinfo")

    (: get-mountinfo-entries (-> (Listof mountinfo-entry)))
    (define/private (get-mountinfo-entries)
      (let* ([f (open-input-file mountinfo-file #:mode 'text)]
             [mount-info (port->lines f)])
        (map mountinfo-line->mountinfo-entry mount-info)))

    (: mountinfo-line->mountinfo-entry (-> String mountinfo-entry))
    (define (mountinfo-line->mountinfo-entry mountinfo-line)
      (let* ([line-split (string-split mountinfo-line " ")]
             [type-onwards (string-split (second (string-split mountinfo-line "-")) " ")]
             [id (first line-split)]
             [parent (second line-split)]
             [dev-id (third line-split)]
             [mnt-root (fourth line-split)]
             [target (fifth line-split)]
             [optional-fields (get-optional-fields line-split)]
             [type (first type-onwards)]
             [source (second type-onwards)]
             [rw (third type-onwards)])             
        (mountinfo-entry id
                         parent
                         dev-id
                         mnt-root
                         target
                         optional-fields
                         type
                         source
                         rw)))

    (: get-optional-fields (-> (Listof String) String))
    (define (get-optional-fields line-split)
      (let* ([optionals-onward (drop line-split 5)]
             [rejoined (string-join optionals-onward " ")]
             [resplit (string-split rejoined "-")])
        (first resplit)))

    (define/private (get-distinct-device-ids mountinfo-entries)
      #f)))

(struct mountinfo-entry
  [(id : String)
   (parent : String)
   (dev-id : String)
   (mnt-root : String)
   (target : String)
   (optional-fields : String)
   (type : String)
   (source : String)
   (rw : String)])