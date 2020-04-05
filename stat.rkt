#lang racket
(require dynamic-ffi/unsafe)

(define stat%
  (class object%
    (super-new)

    (init path file-name)

    (define ipath path)
    (define ifile-name file-name)
    (define full-path (build-path path file-name))
 
    (define/public (get-stat)
      (c-stat 'get_stat))

    (define/public (get-dev) dev)
    (define/public (get-inode) inode)
    (define/public (get-mode) mode)
    (define/public (get-number-of-hardlinks) nlink)
    (define/public (get-uid) uid)
    (define/public (get-gid) gid)
    (define/public (get-rdev) rdev)
    (define/public (get-size) size)
    (define/public (get-block-size) blksize)
    (define/public (get-blocks) blocks)
    (define/public (get-accessed-time) atime)
    (define/public (get-modified-time) mtime)
    (define/public (get-created-time) ctime)

    (define-inline-ffi c-stat #:compiler "clang"
      "#include <sys/stat.h>\n"
      "#include <sys/types.h>\n"

      "struct stat s;\n"

      "struct stat stat_for_file(char* file_name) {\n"
      "  stat(file_name, &s);\n"
      "  return s;\n"
      "}\n"

      "struct stat get_stat(void) {\n"
      "  return s;\n"
      "}\n"

      "unsigned long get_dev(void) {\n"
      "  return s.st_dev;\n"
      "}\n"
      
      "unsigned long get_inode(void) {\n"      
      "  return s.st_ino;\n"
      "}\n"

      "unsigned int get_mode(void) {\n"
      "  return s.st_mode;\n"
      "}\n"

      "unsigned int get_nlink(void) {\n"
      "  return s.st_nlink;\n"
      "}\n"

      "unsigned int get_uid(void) {\n"
      "  return s.st_uid;\n"
      "}\n"

      "unsigned int get_gid(void) {\n"
      "  return s.st_gid;\n"
      "}\n"

      "unsigned long get_rdev(void) {\n"
      "  return s.st_rdev;\n"
      "}\n"

      "unsigned long get_size(void) {\n"
      "  return s.st_size;\n"
      "}\n"

      "unsigned int get_blksize(void) {\n"
      "  return s.st_blksize;\n"
      "}\n"

      "unsigned long get_blocks(void) {\n"
      "  return s.st_blocks;\n"
      "}\n"

      "long get_atime(void) {\n"
      "  return s.st_atime;\n"
      "}\n"

      "long get_mtime(void) {\n"
      "  return s.st_mtime;\n"
      "}\n"

      "long get_ctime(void) {\n"
      "  return s.st_ctime;\n"
      "}\n")

    (c-stat 'stat_for_file full-path)

    (define dev (c-stat 'get_dev))
    (define inode (c-stat 'get_inode))
    (define mode (c-stat 'get_mode))
    (define nlink (c-stat 'get_nlink))
    (define uid (c-stat 'get_uid))
    (define gid (c-stat 'get_gid))
    (define rdev (c-stat 'get_rdev))
    (define size (c-stat 'get_size))
    (define blksize (c-stat 'get_blksize))
    (define blocks (c-stat 'get_blocks))
    (define atime (c-stat 'get_atime))
    (define mtime (c-stat 'get_mtime))
    (define ctime (c-stat 'get_ctime))))

(provide stat%)