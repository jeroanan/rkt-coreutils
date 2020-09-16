#include <sys/stat.h>
#include <sys/types.h>
#include <stdio.h>

struct stat sb;

int getstat(char* filename) {
  int result = stat(filename, &sb);
  return result;
}

dev_t get_dev() {
  return sb.st_dev;
}

ino_t get_ino() {
  return sb.st_ino;
}

mode_t get_mode() {
  return sb.st_mode;
}

nlink_t get_nlink() {
  return sb.st_nlink;
}

uid_t get_uid() {
  return sb.st_uid;
}

gid_t get_gid() {
  return sb.st_gid;
}

dev_t get_rdev() {
  return sb.st_rdev;
}

off_t get_size() {
  return sb.st_size;
}

blksize_t get_blksize() {
  return sb.st_blksize;
}

blkcnt_t get_blocks() {
  return sb.st_blocks;
}

long get_atime() {
  return sb.st_atime;
}

long get_mtime() {
  return sb.st_mtime;
}

long get_ctime() {
  return sb.st_ctime;
}
