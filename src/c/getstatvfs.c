#include <sys/statvfs.h>
  
struct statvfs stat;

unsigned long getblocksize(void) {
  return stat.f_bsize;
}

unsigned long getfragmentsize(void) {
  return stat.f_frsize;
}

fsblkcnt_t getblocks(void) {
  return stat.f_blocks;
}

fsblkcnt_t getfree(void) {
  return stat.f_bfree;
}

fsblkcnt_t getavailable(void) {
  return stat.f_bavail;
}

fsfilcnt_t getfiles(void) {
  return stat.f_files;
}

int getstatvfs(const char* path) {
  return statvfs(path, &stat);
}
