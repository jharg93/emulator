#ifndef __n64_crc_h__
#define __n64_crc_h__

static unsigned int crc_table[256];

static void gen_table() {
  unsigned int crc, poly;
  int	i, j;

  poly = 0xEDB88320;
  for (i = 0; i < 256; i++) {
    crc = i;
    for (j = 8; j > 0; j--) {
      if (crc & 1) crc = (crc >> 1) ^ poly;
      else crc >>= 1;
    }
    crc_table[i] = crc;
  }
}

static unsigned int crc32(unsigned char *data, int len) {
  static bool init = false;
  unsigned int crc = ~0;
  int i;

  if (!init) {
    gen_table();
    init = true;
  }
  for (i = 0; i < len; i++) {
    crc = (crc >> 8) ^ crc_table[(crc ^ data[i]) & 0xFF];
  }

  return ~crc;
}

#endif
