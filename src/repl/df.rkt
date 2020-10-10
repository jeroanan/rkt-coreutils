#lang s-exp "util/program/repl-program.rkt"

;; Copyright 2020 David Wilson
;; See COPYING for licence

(provide df)

(require racket/bool
         racket/list
         racket/port
         racket/string)

(require "../util/stringutil.rkt"
         "util/human-size.rkt"
         "libc/statvfs.rkt")

#;(help-function
      "Print disk usage information"      
      (list 
            "(help) -- Display this help message")
      (list
            "human-readable (bool) -- Whether to display sizes in human-readable format"))
(define mountinfo-file "/proc/self/mountinfo")

(define df
  (λ (#:human-readable [hr #f])
    (define mountinfo-entries (get-mountinfo-entries))
    (define distinct-device-ids (get-distinct-device-ids mountinfo-entries))
    (define de-duped-entries (get-entries-for-device-ids mountinfo-entries distinct-device-ids))
    (define without-remotes (filter is-local-filesystem? de-duped-entries))
    (define without-dummies (filter is-non-dummy-filesystem? without-remotes))
    (define without-zero-blocksize (filter has-non-zero-blocksize? without-dummies))
    (define output (map mountinfo-entry->df-output without-zero-blocksize))
    (set-column-widths output)
    (displayln (get-header))
    (for ([o output])
      (displayln (get-row o hr)))))
    
;; open file specified in mountinfo-file and read lines, returning a list of mountinfo-entry
(define (get-mountinfo-entries)
  (let* ([f (open-input-file mountinfo-file #:mode 'text)]
         [mount-info (port->lines f)])
    (map mountinfo-line->mountinfo-entry mount-info)))

;; Make a line from mountinfo into an instance of mountinfo-entry
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
(define (get-type-onwards mountinfo-line)
  (let ([result (string-split
                 (string-join
                  (append
                   (rest
                    (string-split mountinfo-line " - ")))) " ")])
    result))
      
;; The optional fields part of the line is everything after the fifth word and before the "-".
(define (get-optional-fields line-split)
  (let* ([optionals-onward (drop line-split 5)]
         [rejoined (string-join optionals-onward " ")]
         [resplit (string-split rejoined " - ")])
    (first resplit)))

;; Given the list of mountinfo-entries, get the unique values of the dev-id field
(define (get-distinct-device-ids mountinfo-entries)
  (let ([device-ids (map get-device-id mountinfo-entries)])
    (remove-duplicates device-ids)))

(define (get-entries-for-device-ids mountinfo-entries device-ids)
  (define (get-entries-for-dev-id device-id)
    (let ([the-entries
           (filter (λ (x) (eq? (get-device-id x) device-id))
                   mountinfo-entries)])
      the-entries))

  (define (iterator device-ids out-list)
    (if (empty? device-ids)
        out-list
        (let* ([current-dev-id (first device-ids)]
               [entries (get-entries-for-dev-id current-dev-id)])
          (iterator (rest device-ids) (append out-list (list (first entries)))))))
  (iterator device-ids (list)))

;; Given a mountinfo-entry return the value of its dev-id field
(define (get-device-id x)
  (mountinfo-entry-dev-id x))

;; Is the filesystem non-remote?
(define (is-local-filesystem? f)
  (not (is-remote-filesystem? f)))

;; Given a mountinfo-entry, work out if it's remote. It's remote if:
;; - Its name contains a colon
;; - It is a cifs, smb3 or smbfs filesystem and its name starts with "//"
;; - Its type is "afs" or "auristorfs"
;; - Its name is "-hosts"
(define (is-remote-filesystem? mi-entry)
  (let* ([fs-name (mountinfo-entry-source mi-entry)]
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

(define (is-non-dummy-filesystem? f)
  (not (is-dummy-filesystem? f)))

;; Given a mountinfo-entry, check if it is of a dummy filesystem type.
(define (is-dummy-filesystem? mi-entry)
  (let ([dummy-fs-types (list "autofs"
                              "proc"
                              "subfs"
                              "debugfs"
                              "devpts"
                              "fusectl"
                              "mqueue"
                              "rpc_pipefs"
                              "sysfs"
                              "devfs"
                              "kernfs"
                              "ignore")])
    (list? (member (mountinfo-entry-type mi-entry) dummy-fs-types))))

(define (has-non-zero-blocksize? f)
  (not (is-zero-block-size? f)))

;; Given a mountinfo-entry, determine whether it has zero block size.
(define (is-zero-block-size? mi-entry)
  (begin
    (get-statvfs (mountinfo-entry-target mi-entry))
    (= (get-blocks) 0)))

(define (mountinfo-entry->df-output mi-entry)
  (let* ([stat (get-statvfs (mountinfo-entry-target mi-entry))]
         [block-multiplier (/ (get-fragmentsize) 1024)]
         [1k-blocks (* (get-blocks) block-multiplier)]
         [1k-blocks-available (* (get-available) block-multiplier)]
         [1k-blocks-used (- 1k-blocks 1k-blocks-available)]
         [percent-used (if (eq? 1k-blocks-available 0)
                           100
                           (ceiling (* (/ 1k-blocks-used 1k-blocks) 100)))])
    (df-output (mountinfo-entry-source mi-entry)
               1k-blocks
               1k-blocks-used
               1k-blocks-available
               percent-used
               (mountinfo-entry-target mi-entry))))
    
(define (set-column-widths df-outputs)
  (set! filesystem-width (get-col-width df-output-filesystem filesystem-width df-outputs))
  (set! 1k-blocks-width (get-col-width df-output-1k-blocks 1k-blocks-width df-outputs))
  (set! 1k-blocks-used-width
        (get-col-width df-output-1k-blocks-used 1k-blocks-used-width df-outputs))
  (set! 1k-blocks-available-width
        (get-col-width df-output-1k-blocks-available 1k-blocks-available-width df-outputs))
  (set! percent-used-width (get-col-width df-output-percent-used percent-used-width df-outputs)))
                    
(define (get-header)
  (let ([headers
         (list
          (left-aligned-string "Filesystem" filesystem-width)
          (right-aligned-string "1K-blocks" 1k-blocks-width)
          (right-aligned-string "Used" 1k-blocks-used-width)
          (right-aligned-string "Available" 1k-blocks-available-width)
          (right-aligned-string "Use%" percent-used-width)
          "Mounted on")])
    (string-join headers " ")))

(define (get-row df-out human-readable?)
  (let ([fields
         (list
          (left-aligned-string (df-output-filesystem df-out) filesystem-width)
          (right-aligned-string (get-size-string (df-output-1k-blocks df-out) human-readable?)
                                1k-blocks-width)
          (right-aligned-string (get-size-string
                                 (df-output-1k-blocks-used df-out) human-readable?)
                                1k-blocks-used-width)
          (right-aligned-string (get-size-string
                                 (df-output-1k-blocks-available df-out) human-readable?)
                                1k-blocks-available-width)
          (right-aligned-string
           (format "~a%" (df-output-percent-used df-out)) percent-used-width)
          (df-output-mounted-on df-out))])
    (string-join fields " ")))

(define-syntax-rule (make-col-width-field name min-width)
  (begin
    (define name min-width)))

(define-syntax-rule (get-col-width col-member min-width lst)
  (max min-width (get-max-list-member
                  lst
                  (λ (x)
                    (string-length (anything->string (col-member x)))))))

(define (get-size-string 1k-blocks human-readable?)
  (if human-readable?
      (human-readable-byte-size (* 1k-blocks 1024))
      (number->string 1k-blocks)))

(define (get-max-list-member lst number-extractor)
  (define (iterator lst current-max)
    (if (empty? lst)
        current-max
        (if (>(number-extractor (first lst)) current-max)
            (iterator (rest lst) (number-extractor (first lst)))
            (iterator (rest lst) current-max))))
  (iterator lst 0))

(make-col-width-field filesystem-width 14)
(make-col-width-field 1k-blocks-width 9)
(make-col-width-field 1k-blocks-used-width 5)
(make-col-width-field 1k-blocks-available-width 9)
(make-col-width-field percent-used-width 4)

(struct mountinfo-entry
  [id
   parent
   dev-id
   mnt-root
   target
   optional-fields
   type
   source
   rw])

(struct df-output
  [filesystem
   1k-blocks
   1k-blocks-used
   1k-blocks-available
   percent-used
   mounted-on])
