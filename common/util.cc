#include "util.h"
#include <ctype.h>
#include <sys/time.h>
#include <time.h>

int (*zprint)(const char *, ...) = printf;

void setdmp(int (*zp)(const char *, ...)) {
  zprint = zp;
}

/* Lookup key/value pair by key index */
const char *kvlookup(struct kv *kv, int k, const char *def)
{
  while (kv->key != -1) {
    if (kv->key == k)
      return kv->val;
    kv++;
  }
  return def;
}

void dump(const void  *ptr, int len, int w) {
  int i, j;
  const uint8_t *pb = (const uint8_t *)ptr;

  for (i = 0; i < len; i+=w) {
    zprint("%.6x: ", i);
    for (j = 0; j < w; j++) {
      zprint("%.2x", pb[i+j]);
    }
    zprint("\n");
  }
}

static const char dch(int c)
{
  return isprint(c) ? c : '.';
}

void hexdump(const void  *ptr, int len, int step, const char *spc) {
  const uint8_t *pb = (const uint8_t *)ptr;
  int i, j;

  for (i = 0; i < len; i+=step) {
    zprint("%.6x: ", i);
    for (j = 0; j < step; j++) {
      zprint("%.2x%s", pb[i+j], spc);
    }
    zprint("  ");
    for (j = 0; j < step; j++) {
      uint8_t c = pb[i+j];
      zprint("%c", dch(c));
    }
    zprint("\n");
  }
}

static uint32_t nxtPow2(const uint32_t v) {
  for (uint32_t size = 1; size; size <<= 1) {
    if (size > v)
      return size;
  }
  return 0;
}

/* Load ROM file into buffer */
uint8_t *loadrom(const char *file, size_t& len, bool pow2)
{
  uint8_t *buf;
  size_t   len2;
  int fd;
  
  if ((fd = open(file, O_RDONLY)) < 0) {
    printf("Error can't open file: %s\n", file);
    exit(-1);
  }
  len = lseek(fd, 0, SEEK_END);
  buf = (uint8_t*)malloc(len);
  lseek(fd, 0, SEEK_SET);
  read(fd, buf, len);
  close(fd);
  len2 = nxtPow2(len);
  if (pow2 && len != len2) {
    buf = (uint8_t *)realloc(buf, len2);
    len = len2;
  }
  return buf;
}

uint64_t gt()
{
  struct timespec ts;
  clock_gettime(CLOCK_REALTIME, &ts);
  return (ts.tv_sec * 1000000000) + ts.tv_nsec;
}


/* Check if source string contains needle, replace with new format and advance pointer */
bool replace(const char **src, const char *match, char **dest, const char *fmt, ...)
{
  if (!src || !*src || !match || !fmt) {
    return false;
  }
  int len = strlen(match);
  if (strncmp(*src, match, len) != 0)
    return false;
  *src += len;
  if (dest) {
    va_list ap;
    char buffer[1024];

    /* Replace old string with new string */
    va_start(ap, fmt);
    int n = vsnprintf(buffer, sizeof(buffer), fmt, ap);
    if (n >= 0) {
      memcpy(*dest, buffer, n);
      *dest += n;
    }
  }
  return true;
}

// binary bits
const char *bb(int n)
{
  static char bits[32];

  memset(bits, 0, sizeof(bits));
  for (int i = 0; i < 8; i++) {
    bits[i] = (n & 0x80) ? '1' : '0';
    n <<= 1;
  }
  return bits;
}

const char *sbits(const char *mm, uint16_t bits) {
  static char sm[32] = { };

  int mask = 0x8000;
  for (int i = 0; i < 16; i++) {
    sm[i] = (bits & mask) ? mm[i] : '-';
    mask >>= 1;
  }
  return sm;
}

const char *sbits8(const char *mm, uint8_t bits) {
  static char sm[32] = { };

  int mask = 0x80;
  for (int i = 0; i < 8; i++) {
    sm[i] = (bits & mask) ? mm[i] : '-';
    mask >>= 1;
  }
  return sm;
}
