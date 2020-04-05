#lang racket
(require dynamic-ffi/unsafe)

(define stat%
  (class object%
    (super-new)

    (init path file-name)

    (define ipath path)
    (define ifile-name file-name)
    (define full-path (build-path path file-name))

    (define/public (get-inode)
      (c-stat 'get_inode))
    
    (define-inline-ffi c-stat #:compiler "clang"
      "#include <sys/stat.h>\n"
      "#include <sys/types.h>\n"

      "struct stat s;\n"

      "int stat_for_file(char* file_name) {\n"
      "  return stat(file_name, &s);\n"
      "}\n"
  
      "unsigned long get_inode(void) {\n"      
      "  return s.st_ino;\n"
      "}\n")

    (c-stat 'stat_for_file full-path)))

(provide stat%)