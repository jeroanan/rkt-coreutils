#lang racket
(require dynamic-ffi/unsafe)

(define getpwuid%
  (class object%
    (super-new)

    (init uid)

    (define iuid uid)

    (define/public (get-username) name)
    (define/public (get-password) passwd)
    (define/public (get-uid) u)
    (define/public (get-gid) gid)
    (define/public (get-gecos) gecos)
    (define/public (get-home-dir) dir)
    (define/public (get-shell) shell)
        
    (define-inline-ffi c-pwd #:compiler "clang"
      "#include <sys/types.h>\n"
      "#include <pwd.h>\n"

      "struct passwd p;\n"

      "void getpwuid_for_uid(int uid) {\n"
      "  p = *getpwuid(uid);\n"
      "}\n"

      "char* get_name(void) {\n"
      "  return p.pw_name;\n"
      "}\n"

      "char* get_passwd(void) {\n"
      "  return p.pw_passwd;\n"
      "}\n"

      "unsigned int get_uid(void) {\n"
      "  return p.pw_uid;\n"
      "}\n"

      "unsigned int get_gid(void) {\n"
      " return p.pw_gid;\n"
      "}\n"

      "char* get_gecos(void) {\n"
      "  return p.pw_gecos;\n"
      "}\n"

      "char* get_dir(void) {\n"
      "  return p.pw_dir;\n"
      "}\n"

      "char* get_shell(void) {\n"
      "  return p.pw_shell;\n"
      "}\n")

    (c-pwd 'getpwuid_for_uid iuid)
    
    (define name (c-pwd 'get_name))
    (define passwd (c-pwd 'get_passwd))
    (define u (c-pwd 'get_uid))
    (define gid (c-pwd 'get_gid))
    (define gecos (c-pwd 'get_gecos))
    (define dir (c-pwd 'get_dir))
    (define shell (c-pwd 'get_shell))))