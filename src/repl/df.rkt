#lang typed/racket/base

;; Copyright 2020 David Wilson
;; See COPYING for licence

(provide df%)

(require typed/racket/class
         racket/bool
         racket/list
         racket/port
         racket/string)

(require "../util/stringutil.rkt"
         "util/human-size.rkt"
         "util/member.rkt"
         "util/util.rkt")

(require/typed "libc/statvfs.rkt"
               [get-statvfs  (-> String Integer)]
               [get-blocks (-> Integer)]
               [get-available (-> Integer)]
               [get-fragmentsize (-> Integer)])

;; df - display file system diskusage
(define df%
  (class object%
    (super-new)

    (help-function
      "Print disk usage information"      
      (list 
            "(help) -- Display this help message"
            "(execute) -- Display disk usage information")
      (list
            "human-readable (bool) -- Whether to display sizes in human-readable format"))

    ;; Whether to display sizes in human-readable format
    (public-boolean-attribute human-readable #f)
    
    ;; Main program execution
    (define/public (execute)
      (let* ([mountinfo-entries (get-mountinfo-entries)]
             [distinct-device-ids (get-distinct-device-ids mountinfo-entries)]
             [de-duped-entries (get-entries-for-device-ids mountinfo-entries distinct-device-ids)]
             [filter-remotes (filter (λ ([x : mountinfo-entry])
                                       (not (is-remote-filesystem? x))) de-duped-entries)]
             [filter-dummies (filter (λ ([x : mountinfo-entry])
                                       (not (is-dummy-filesystem? x))) filter-remotes)]
             [filter-zero-blocksize (filter (λ ([x : mountinfo-entry])
                                              (not (is-zero-block-size? x))) filter-dummies)]
             [output (map mountinfo-entry->df-output filter-zero-blocksize)])
        (set-column-widths output)
        (displayln (get-header))
        (for ([o output])
          (displayln (get-row o)))))
    
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
                        (string-split mountinfo-line " - ")))) " ")])
        result))
      
    ;; The optional fields part of the line is everything after the fifth word and before the "-".
    (: get-optional-fields (-> (Listof String) String))
    (define (get-optional-fields line-split)
      (let* ([optionals-onward (drop line-split 5)]
             [rejoined (string-join optionals-onward " ")]
             [resplit (string-split rejoined " - ")])
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

    ;; Given a mountinfo-entry, check if it is of a dummy filesystem type.
    (: is-dummy-filesystem? (-> mountinfo-entry Boolean))
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

    ;; Given a mountinfo-entry, determine whether it has zero block size.
    (: is-zero-block-size? (-> mountinfo-entry Boolean))
    (define (is-zero-block-size? mi-entry)
      (begin
        (get-statvfs (mountinfo-entry-target mi-entry))        
        (= (get-blocks) 0)))

    ;; Given a mountinfo-entry return the value of its dev-id field
    (: get-device-id (-> mountinfo-entry String))
    (define (get-device-id x)
      (mountinfo-entry-dev-id x))

    (: mountinfo-entry->df-output (-> mountinfo-entry df-output))
    (define (mountinfo-entry->df-output mi-entry)
      (let* ([stat (get-statvfs (mountinfo-entry-target mi-entry))]
             [block-multiplier (/ (get-fragmentsize) 1024)]
             [1k-blocks (assert (* (get-blocks) block-multiplier) integer?)]
             [1k-blocks-available (assert (* (get-available) block-multiplier) integer?)]
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
    
    (define-syntax-rule (get-col-width col-member min-width lst)
      (max min-width (get-max-list-member 
                       lst
                       (λ ([x : df-output])
                         (string-length (anything->string (col-member x)))))))

    (define-syntax-rule (make-col-width-field name min-width)
      (begin
        (: name Exact-Nonnegative-Integer)
        (define name min-width)))
    
    (make-col-width-field filesystem-width 14)
    (make-col-width-field 1k-blocks-width 9)
    (make-col-width-field 1k-blocks-used-width 5)
    (make-col-width-field 1k-blocks-available-width 9)
    (make-col-width-field percent-used-width 4)

    (: set-column-widths (-> (Listof df-output) Void))
    (define (set-column-widths df-outputs)
      (set! filesystem-width (get-col-width df-output-filesystem filesystem-width df-outputs))
      (set! 1k-blocks-width (get-col-width df-output-1k-blocks 1k-blocks-width df-outputs))
      (set! 1k-blocks-used-width
            (get-col-width df-output-1k-blocks-used 1k-blocks-used-width df-outputs))
      (set! 1k-blocks-available-width
            (get-col-width df-output-1k-blocks-available 1k-blocks-available-width df-outputs))
      (set! percent-used-width (get-col-width df-output-percent-used percent-used-width df-outputs)))
                    
    (: get-header (-> String))
    (define/private (get-header)
      (let ([headers
             (list
              (left-aligned-string "Filesystem" filesystem-width)
              (right-aligned-string "1K-blocks" 1k-blocks-width)
              (right-aligned-string "Used" 1k-blocks-used-width)
              (right-aligned-string "Available" 1k-blocks-available-width)
              (right-aligned-string "Use%" percent-used-width)
              "Mounted on")])
        (string-join headers " ")))

    (: get-row (-> df-output String))
    (define/private (get-row df-out)
      (let ([fields
             (list
              (left-aligned-string (df-output-filesystem df-out) filesystem-width)
              (right-aligned-string (get-size-string (df-output-1k-blocks df-out)) 1k-blocks-width)
              (right-aligned-string (get-size-string
                                     (df-output-1k-blocks-used df-out))
                                      1k-blocks-used-width)
              (right-aligned-string (get-size-string
                                     (df-output-1k-blocks-available df-out))
                                     1k-blocks-available-width)
              (right-aligned-string
               (format "~a%" (df-output-percent-used df-out)) percent-used-width)
              (df-output-mounted-on df-out))])
        (string-join fields " ")))

    (: get-size-string (-> Integer String))
    (define/private (get-size-string 1k-blocks)
      (if human-readable
          (human-readable-byte-size (* 1k-blocks 1024))
          (number->string 1k-blocks)))

    (: get-max-list-member (-> (Listof df-output) (-> df-output Index) Integer))
    (define (get-max-list-member lst number-extractor)

      (: iterator (-> (Listof df-output) Integer Integer))
      (define (iterator lst current-max)        
        (if (empty? lst)
            current-max
            (if (>(number-extractor (first lst)) current-max)
                (iterator (rest lst) (number-extractor (first lst)))
                (iterator (rest lst) current-max))))
      (iterator lst 0))))

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

(struct df-output
  [(filesystem : String)   
   (1k-blocks : Integer)
   (1k-blocks-used : Integer)
   (1k-blocks-available : Integer)
   (percent-used : Integer)
   (mounted-on : String)])
