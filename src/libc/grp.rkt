#lang racket

; Copyright 2020 David Wilson
; See COPYING for details

(provide getgrgid%
         get-getgrgid
         getgrouplist%)

(require ffi/unsafe
         dynamic-ffi/unsafe
         racket/class
         racket/list
         racket/format)

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

(define getgrouplist%
  (class object%
    (super-new)

    (init user-name number-to-retrieve)

    (define/public (get-next-group-id)
      (ggl 'getnextgroupid))

    (define/public (get-number-of-groups)
      (ggl 'getnumberofgroups))
    
    (define clib (ffi-lib #f))
        
    (define-inline-ffi ggl #:compiler "clang"
      "#include <stdlib.h>\n"
      "#include <grp.h>\n"
      "#include <stdio.h>\n"
    
      "gid_t* groups;\n"
      "int ngroups;\n"
      "int i = 0;\n"

      "int getnextgroupid(void) {\n"
      "  if (i<=ngroups) {\n"
      "    gid_t j = groups[i];\n"
      "    i++;\n"
      "    return j;\n"
      "  }\n"
      "  return -1;"
      "}\n"

      "int getnumberofgroups(void) {\n"
      "  return ngroups;\n"
      "}\n"
    
      "int getgroups(char* username, int number_of_groups) {\n"
      "  int r;"
      "  groups = malloc(ngroups * sizeof(gid_t));\n"    
      "  r = getgrouplist(username, 1000, groups, &ngroups);\n"
      "  return r;\n"
      "}\n")

  
    (ggl 'getgroups user-name number-to-retrieve)))
  
  