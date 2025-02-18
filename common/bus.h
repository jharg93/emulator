#ifndef __bus_h__
#define __bus_h__
#include <deque>
#include <assert.h>

#ifdef GBA
typedef uint32_t iodata_t;
#elif (defined(M68K) || defined(N64))
typedef uint32_t iodata_t;
#else
typedef uint8_t  iodata_t;
#endif

typedef int (*iofn_t)(void *arg, uint32_t addr, const int mode, iodata_t& val);

struct handler_t {
  uint32_t    start;
  uint32_t    end;
  uint32_t    mask;
  iofn_t      fn;
  void       *arg;
  int         flag;
  const char *lbl;
};

#define _RD  0x01
#define _WR  0x02
#define _RW  0x03
#define _DBG 0x04
#define _OVR 0x08 // explicitly override any previous

#define _SZ16 0x200
#define _SZ32 0x400

#define _WR8  'w' /* for compatability */
#define _RD8  'r'
#define _WR16 _SZ16+'w'
#define _WR32 _SZ32+'w'
#define _RD16 _SZ16+'r'
#define _RD32 _SZ32+'r'

class bus_t {
  uint32_t busmask;
  std::deque<handler_t *> handlers;

  handler_t *find(uint32_t, const int);
public:
  bus_t(uint32_t mask = 0);
  void init(uint32_t mask);
  int write(uint32_t, iodata_t, int sz=0);
  int read(uint32_t, iodata_t&, int sz=0);
  int register_handler(uint32_t, uint32_t, uint32_t, iofn_t fn, void *, int, const char *);
  void dump(int start=0, int end=0);

  static uint8_t bus_data;
};

int bememio(void *arg, uint32_t addr, int mode, iodata_t&data);
int memio(void *arg, uint32_t addr, const int mode, iodata_t&data);

struct bank_t {
  const char *name;
  uint8_t    *base;       // memory base
  int         nbank;      // total number of banks
  int         banksz;     // size of bank
  int         bank;       // current bank number
  int         sel;        // bank select value
};

void initbank(bank_t *, uint8_t *, int, int, int, const char *name="");
void setbank(bank_t *, int, const char *lbl=NULL);
int  bankio(void *, uint32_t, int, iodata_t&);
int  bankset(void *, uint32_t, int, iodata_t&);
int  banksel(void *, uint32_t, int, iodata_t&);
int  banksyio(void *, uint32_t, int, iodata_t&);

struct banksy {
  int         bank    = -2;
  int         nbank   = -2;
  uint8_t   **banks   = NULL;
  uint8_t    *current = NULL;
  const char *name    = NULL;

  int init(int n, const char *lbl);
  int init(uint8_t *buf, int len, int sz, int cb, const char *lbl = NULL);
  int setbank(int nb, const char *lbl = NULL);
};

void setbank(banksy *, int, const char *lbl = NULL);
void initbank(banksy *b, uint8_t *mem, int memsz, int bsz, int sb, const char *name);

extern int (*bus_hook)(uint32_t addr, int mode, iodata_t data);
extern void flogger(int, const char *, ...);

/* Native memory read/write */
static constexpr uint8_t get8(const void *buf) {
  return *(const uint8_t *)buf;
}
static constexpr uint16_t get16(const void *buf) {
  return *(const uint16_t *)buf;
}
static constexpr uint32_t get32(const void *buf) {
  return *(const uint32_t *)buf;
}
static constexpr uint64_t get64(const void *buf) {
  return *(const uint64_t *)buf;
}
static constexpr void put8(void *buf, const uint8_t v) {
  *(uint8_t *)buf = v;
}
static constexpr void put16(void *buf, const uint16_t v) {
  *(uint16_t *)buf = v;
}
static constexpr void put32(void *buf, const uint32_t v) {
  *(uint32_t *)buf = v;
}
static constexpr void put64(void *buf, const uint64_t v) {
  *(uint64_t *)buf = v;
}

/* Big-endian read/write */
static constexpr uint16_t get16be(const void *buf) {
#if 1
  return __builtin_bswap16(*(uint16_t *)buf);
#else
  const uint8_t *pb = (const uint8_t *)buf;
  return (pb[0] << 8) + pb[1];
#endif
}
static constexpr uint32_t get32be(const void *buf) {
#if 1
  return __builtin_bswap32(*(uint32_t *)buf);
#else
  const uint8_t *pb = (const uint8_t *)buf;
  return (pb[0] << 24) + (pb[1] << 16) + (pb[2] << 8) + pb[3];
#endif
}
static constexpr uint64_t get64be(const void *buf) {
#if 1
  return __builtin_bswap64(*(uint64_t *)buf);
#else
  uint64_t lo, hi;
  lo = get32be((uint8_t *)buf + 4);
  hi = get32be((uint8_t *)buf + 0);
  return (hi << 32LL) + lo;
#endif
};

static constexpr void put16be(void *buf, const uint16_t v) {
#if 1
  *(uint16_t *)buf = __builtin_bswap16(v);
#else
  ((uint8_t *)buf)[0] = v >> 8;
  ((uint8_t *)buf)[1] = v;
#endif
}
static constexpr void put32be(void *buf, const uint32_t v) {
#if 1
  *(uint32_t *)buf = __builtin_bswap32(v);
#else
  ((uint8_t *)buf)[0] = v >> 24;
  ((uint8_t *)buf)[1] = v >> 16;
  ((uint8_t *)buf)[2] = v >> 8;
  ((uint8_t *)buf)[3] = v;
#endif
}
static constexpr void put64be(void *buf, const uint64_t v) {
#if 1
  *(uint64_t *)buf = __builtin_bswap64(v);
#else
  ((uint8_t *)buf)[0] = v >> 56;
  ((uint8_t *)buf)[1] = v >> 48;
  ((uint8_t *)buf)[2] = v >> 40;
  ((uint8_t *)buf)[3] = v >> 32;
  ((uint8_t *)buf)[4] = v >> 24;
  ((uint8_t *)buf)[5] = v >> 16;
  ((uint8_t *)buf)[6] = v >> 8;
  ((uint8_t *)buf)[7] = v;
#endif
}

void putn(uint8_t *dst, uint32_t v, int n);
uint32_t getn(const uint8_t *src, int n);

/* Generic device base class
 * Register static member function io will call back object
 */
struct gendev_t {
  static int io(void *arg, uint32_t addr, int mode, iodata_t& val) {
    gendev_t *g = (gendev_t *)arg;
    return g->io(addr, mode, val);
  };
  virtual int io(uint32_t addr, int mode, iodata_t& val) {
    return 0;
  };
};

#endif
