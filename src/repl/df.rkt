#lang typed/racket/base

;; Copyright 2020 David Wilson
;; See COPYING for licence

(require typed/racket/class
         racket/list
         racket/port
         racket/string)

(require (for-syntax racket/list))

;; df - display file system diskusage
(define df%
  (class object%
    (super-new)

    ;; Main program execution
    (define/public (execute)
      (let* ([mountinfo-entries (get-mountinfo-entries)]
             [distinct-device-ids (get-distinct-device-ids mountinfo-entries)]
             [de-duped-entries (get-entries-for-device-ids mountinfo-entries distinct-device-ids)])
        (displayln (length mountinfo-entries))
        (displayln (length distinct-device-ids))))
    
    (define mountinfo-file "/proc/self/mountinfo")

    ;; Given a list of device ids get one entry from mountinfo-entries for each.
    (: get-entries-for-device-ids
       (-> (Listof mountinfo-entry) (Listof String) (Listof mountinfo-entry)))
    (define/private (get-entries-for-device-ids mountinfo-entries device-ids)

      (: get-entries-for-dev-id (-> String (Listof mountinfo-entry)))
      (define (get-entries-for-dev-id device-id)
        (let ([the-entries
               (filter (λ ([x : mountinfo-entry]) (eq? (get-device-id x) device-id))
                       mountinfo-entries)])
          the-entries))

      (: iterator (-> (Listof String) (Listof mountinfo-entry) (Listof mountinfo-entry)))
      (define (iterator device-ids out-list)
        (if (empty? device-ids)
            out-list
            (let* ([current-dev-id (first device-ids)]
                   [entries (get-entries-for-dev-id current-dev-id)])              
              (iterator (rest device-ids) (append out-list (list (first entries)))))))
      (iterator device-ids (list)))
    
    ;; open file specified in mountinfo-file and read lines, returning a list of mountinfo-entry
    (: get-mountinfo-entries (-> (Listof mountinfo-entry)))
    (define/private (get-mountinfo-entries)
      (let* ([f (open-input-file mountinfo-file #:mode 'text)]
             [mount-info (port->lines f)])
        (map mountinfo-line->mountinfo-entry mount-info)))

    ;; Make a line from mountinfo into an instance of mountinfo-entry
    (: mountinfo-line->mountinfo-entry (-> String mountinfo-entry))
    (define (mountinfo-line->mountinfo-entry mountinfo-line)
      (let* ([line-split (string-split mountinfo-line " ")]
             [type-onwards (get-type-onwards mountinfo-line)]
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

    ;; Take a line from the mountinfo file and return a list of fields from its type onwards
    (: get-type-onwards (-> String (Listof String)))
    (define/private (get-type-onwards mountinfo-line)
      (let ([result (string-split
                     (string-join
                      (append
                       (rest
                        (string-split mountinfo-line "-")))) " ")])
        result))
      
    ;; The optional fields part of the line is everything after the fifth word and before the "-".
    (: get-optional-fields (-> (Listof String) String))
    (define (get-optional-fields line-split)
      (let* ([optionals-onward (drop line-split 5)]
             [rejoined (string-join optionals-onward " ")]
             [resplit (string-split rejoined "-")])
        (first resplit)))

    ;; Given the list of mountinfo-entries, get the unique values of the dev-id field
    (: get-distinct-device-ids (-> (Listof mountinfo-entry) (Listof String)))
    (define/private (get-distinct-device-ids mountinfo-entries)
      (let ([device-ids (map get-device-id mountinfo-entries)])
        (remove-duplicates device-ids)))

    ;; Given a mountinfo-entry, work out if it's remote. It's remote if:
    ;; - Its name contains a colon
    ;; - It is a cifs, smb3 or smbfs filesystem and its name starts with "//"
    ;; - Its type is "afs" or "auristorfs"
    ;; - Its name is "-hosts"
    (: is-remote-filesystem? (-> mountinfo-entry Boolean))
    (define (is-remote-filesystem? mi-entry)
      (let* ([fs-name (mountinfo-entry-dev-id mi-entry)]
             [fs-type (mountinfo-entry-type mi-entry)]
             [contains-colon? (string-contains? fs-name ":")]
             [starts-with-double-slash? (string-prefix? fs-name "//")]
             [smbfs-type? (string=? fs-type "smbfs")]
             [smb3-type? (string=? fs-type "smb3")]
             [cifs-type? (string=? fs-type "cifs")]
             [afs-type? (string=? fs-type "afs")]
             [auristorfs-type? (string=? fs-type "auristorfs")]
             [hosts-name? (string=? fs-name "-hosts")]
             [smb-or-cifs? (or smbfs-type? smb3-type? cifs-type?)]
             [is-share? (and smb-or-cifs? starts-with-double-slash?)])
        (or contains-colon? is-share? afs-type? auristorfs-type? hosts-name?)))
    
    ;; Given a mountinfo-entry return the value of its dev-id field
    (: get-device-id (-> mountinfo-entry String))
    (define (get-device-id x)
      (mountinfo-entry-dev-id x))))

(define-syntax-rule (make-de-duper de-duper-name list-type output-type de-dupe-function)
  (begin
    (: de-duper-name (-> (Listof list-type) (Listof output-type) (Listof output-type)))
    (define (de-duper-name to-de-dupe output-lst)
      (if (empty? to-de-dupe)
          output-lst
          (let ([de-dupe-value (de-dupe-function (first to-de-dupe))])
                (if (memv de-dupe-value output-lst)                    
                    (de-duper-name (rest to-de-dupe) output-lst)                    
                    (de-duper-name (rest to-de-dupe) (append output-lst (list de-dupe-value)))))))))

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