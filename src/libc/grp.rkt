#lang racket
(require dynamic-ffi/unsafe)

(define getgrgid%
  (class object%
    (super-new)

    (init gid)

    (define igid gid)

    (define/public (get-name) name)
    (define/public (get-password) passwd)
    (define/public (get-gid) the-gid)
    (define/public (get-members) members)
    
    (define-inline-ffi c-grp #:compiler "clang"
      "#include <sys/types.h>\n"
      "#include <grp.h>\n"

      "struct group g;\n"

      "void get_group_for_group_id(int gid) {\n"
      "  g = *getgrgid(gid);\n"
      "}\n"

      "char* get_gr_name(void) {\n"
      "  return g.gr_name;\n"
      "}\n"

      "char* get_gr_passwd(void) {\n"
      "  return g.gr_passwd;\n"
      "}\n"

      "unsigned int get_gid(void) {\n"
      "  return g.gr_gid;\n"
      "}\n"

      "char** get_members(void) {\n"
      "  return g.gr_mem;\n"
      "}\n")

    (c-grp 'get_group_for_group_id igid)
    (define name (c-grp 'get_gr_name))
    (define passwd (c-grp 'get_gr_passwd))
    (define the-gid (c-grp 'get_gid))
    (define members (c-grp 'get_members))))

(provide getgrgid%)