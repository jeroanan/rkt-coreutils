#include "gnulib/config.h"

#include <errno.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <verify.h>

#include "gnulib/gllib/error.h"
#include "gnulib/gllib/fcntl.h"
#include "gnulib/gllib/human.h"
#include "gnulib/gllib/quotearg.h"
#include "gnulib/gllib/safe-read.h"
#include "gnulib/gllib/xbinary-io.h"

#include "fadvise/fadvise.h"

static bool have_read_stdin;

#define STREQ(a, b) (strcmp (a, b) == 0)

/* Use these to shell quote only when necessary,
 *  when the quoted item is already delimited with colons.  */
#define quotef(arg) \
  quotearg_n_style_colon (0, shell_escape_quoting_style, arg)
     
bool
bsd_sum_file (const char *file, char *sum)
{
  FILE *fp;
  int checksum = 0;	/* The checksum mod 2^16. */
  uintmax_t total_bytes = 0;	/* The number of bytes. */
  int ch;		/* Each character read. */
  char hbuf[LONGEST_HUMAN_READABLE + 1];
  bool is_stdin = STREQ (file, "-");

  if (is_stdin)
    {
      fp = stdin;
      have_read_stdin = true;
      xset_binary_mode (STDIN_FILENO, O_BINARY);
    }
  else
    {
      fp = fopen (file, (O_BINARY ? "rb" : "r"));
      if (fp == NULL)
        {
          error (0, errno, "%s", quotef (file));
          return false;
        }
    }

  fadvise (fp, FADVISE_SEQUENTIAL);

  while ((ch = getc (fp)) != EOF)
    {
      total_bytes++;
      checksum = (checksum >> 1) + ((checksum & 1) << 15);
      checksum += ch;
      checksum &= 0xffff;	/* Keep it within bounds. */
    }

  if (ferror (fp))
    {
      error (0, errno, "%s", quotef (file));
      if (!is_stdin)
        fclose (fp);
      return false;
    }

  if (!is_stdin && fclose (fp) != 0)
    {
      error (0, errno, "%s", quotef (file));
      return false;
    }

  sprintf (sum, "%05d %5s", checksum,
          human_readable (total_bytes, hbuf, human_ceiling, 1, 1024));

  return true;
}

/* Calculate and print the checksum and the size in 512-byte blocks
   of file FILE, or of the standard input if FILE is "-".
   If PRINT_NAME is >0, print FILE next to the checksum and size.
   Return true if successful.  */

bool
sysv_sum_file (const char *file, char *sum)
{
  int fd;
  unsigned char buf[8192];
  uintmax_t total_bytes = 0;
  char hbuf[LONGEST_HUMAN_READABLE + 1];
  int r;
  int checksum;

  /* The sum of all the input bytes, modulo (UINT_MAX + 1).  */
  unsigned int s = 0;

  bool is_stdin = STREQ (file, "-");

  if (is_stdin)
    {
      fd = STDIN_FILENO;
      have_read_stdin = true;
      xset_binary_mode (STDIN_FILENO, O_BINARY);
    }
  else
    {
      fd = open (file, O_RDONLY | O_BINARY);
      if (fd == -1)
        {
          error (0, errno, "%s", quotef (file));
          return false;
        }
    }

  while (1)
    {
      size_t bytes_read = safe_read (fd, buf, sizeof buf);

      if (bytes_read == 0)
        break;

      if (bytes_read == SAFE_READ_ERROR)
        {
          error (0, errno, "%s", quotef (file));
          if (!is_stdin)
            close (fd);
          return false;
        }

      for (size_t i = 0; i < bytes_read; i++)
        s += buf[i];
      total_bytes += bytes_read;
    }

  if (!is_stdin && close (fd) != 0)
    {
      error (0, errno, "%s", quotef (file));
      return false;
    }

  r = (s & 0xffff) + ((s & 0xffffffff) >> 16);
  checksum = (r & 0xffff) + (r >> 16);

  sprintf (sum, "%d %s", checksum,
          human_readable (total_bytes, hbuf, human_ceiling, 1, 512));

  return true;
}

