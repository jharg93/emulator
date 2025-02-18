#ifndef __util_h__
#define __util_h__

#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdarg.h>
#include <inttypes.h>
#include <string.h>

#define xsizeof(x) (sizeof(x) / sizeof(x[0]))

extern int (*zprint)(const char *, ...);

bool replace(const char **src, const char *match, char **dest = NULL, const char *fmt = "", ...);

enum {
  D0 = (1L << 0),
  D1 = (1L << 1),
  D2 = (1L << 2),
  D3 = (1L << 3),
  D4 = (1L << 4),
  D5 = (1L << 5),
  D6 = (1L << 6),
  D7 = (1L << 7),
  D8 = (1L << 8),
  D9 = (1L << 9),
  D10 = (1L << 10),
  D11 = (1L << 11),
  D12 = (1L << 12),
  D13 = (1L << 13),
  D14 = (1L << 14),
  D15 = (1L << 15),
  D16 = (1L << 16),
  D17 = (1L << 17),
  D18 = (1L << 18),
  D19 = (1L << 19),
  D20 = (1L << 20),
  D21 = (1L << 21),
  D22 = (1L << 22),
  D23 = (1L << 23),
  D24 = (1L << 24),
  D25 = (1L << 25),
  D26 = (1L << 26),
  D27 = (1L << 27),
  D28 = (1L << 28),
  D29 = (1L << 29),
  D30 = (1L << 30),
  D31 = (1L << 31),

  D63 = (1LL << 63),
};

enum {
 _256k = 256*1024,
 _128k = 128*1024,
 _96k = 96*1024,
 _32k = 32*1024,
 _16k = 16*1024,
 _8k = 8*1024,
 _4k = 4*1024,
 _2k = 2*1024,
 _1k = 1*1024,
};

struct kv {
  int         key;
  const char *val;
};

const char *kvlookup(struct kv *, int k, const char *def=NULL);

void hexdump(const void *, int, int w=16, const char *spc=" ");
void dump(const void *, int, int w=16);
uint8_t *loadrom(const char *, size_t&, bool p2 = false);

constexpr uint8_t *zmalloc(size_t s) {
  return new uint8_t[s]{0};
};

/* Set Lo/Hi byte */
static void setlo(auto& v, uint8_t nv) {
  v = (v & 0xFF00) + nv;
};
static void sethi(auto& v, uint8_t nv) {
  v = (v & 0xFF00) + (nv << 8);
};

/* Read-modify-write value */
constexpr void rmw(auto& v, uint32_t nv, uint32_t mask, const char *lbl = "") {
  v = (v & ~mask) | (nv & mask);
};

/* Set or clear a mask */
constexpr void setclr(auto& v, auto mask, const bool state) {
  v = state ? (v | mask) : (v & ~mask);
}

/* Check if value is a range: L <= v < L+D */
constexpr bool inrange(const int v, const int l, const int delta) {
  return (v >= l && v < l+delta);
};

/* Extracts masked bits from input value
 *   EG: v    = 0x12345678
 *       mask = 0x0000FF00
 *     result = 0x67
 */
static inline uint32_t extract(uint32_t v, uint32_t mask) {
  const uint32_t x = (mask & ~(mask-1));
  return x ? ((v & mask) / x) : 0;
}

/* Sign-extend N-bits */
static inline uint32_t signex(uint32_t v, int bit) {
  const uint32_t m = (1U << (bit-1));

  /* clear out upper bits */
  v &= ((1U << bit) - 1);
  return (v ^ m) - m;
}

// 24-bit integer (for 65c816)
struct uint24_t {
  uint8_t *p;
  inline uint24_t(uint8_t *ref) {
    p = ref;
  };
  inline operator uint32_t const() {
    return (p[0] + (p[1] << 8) + (p[2] << 16));
  };
  inline uint24_t &operator=(const uint32_t v) {
    p[0] = v;
    p[1] = v >> 8;
    p[2] = v >> 16;
    return *this;
  };
};

/* Timer class:
 *  if count == 0 : expired this tick
 *  if count <  0 : expired
 *  if count >  0 : not expired
 */
struct Timer {
  int  latch = 0;
  int  count = 0;
  int  reset = -1;
  bool enabled = false;
  Timer(const char *slug = "") {
  };
  void settimer(int ctr, bool reload, bool en, const char *_name = "??") {
    zprint("Set Timer[%s]: %d, reload=%d, enabled=%d\n",
	   _name, ctr, reload, en);
    enabled = en;
    count = ctr;
    reset = reload ? ctr : -1;
  };

  /* Tick a timer.  returns false if disabled or running. Returns true if timer expired */
  bool tick() {
    if (!enabled) {
      return false;
    }
    /* Decrement counter if not zero */
    if (count > 0) {
      count--;
    }
    /* Reset timer, return expired */
    if (count == 0) {
      count = reset;
      return true;
    }
    return false;
  };
};

void setdmp(int (*zp)(const char *, ...));

/* Return number of set bits */
constexpr uint32_t set_bits(uint32_t v) {
  uint32_t n = 0;
  while (v) {
    if (v & 1)
      n++;
    v >>= 1;
  }
  return n;
}

constexpr bool testbit(int val, int bit) {
  return (val >> bit) & 1;
}

struct irq_t {
  const char *name = "";
  uint32_t en;  // ier.irq enabled
  uint32_t sts; // icr.irq requested

  /* Enable/disable IRQ line */
  void enable(const uint32_t mask, const bool ben) {
    if (ben) {
      enable(mask);
    }
    else {
      disable(mask);
    }
  };
  void enable(const uint32_t mask) {
    if (~en & mask) {
      printf("enable  int:%.8x %.8x [%s]\n", mask, en, name);
      en |= mask;
    }
  };
  void disable(const uint32_t mask) {
    if (en & mask) {
      printf("disable int:%.8x %.8x [%s]\n", mask, en, name);
      en &= ~mask;
    }
  };

  /* Signal/ACK IRQ */
  void set(const uint32_t mask, bool bset) {
    if (bset) {
      set(mask);
    }
    else {
      clear(mask);
    }
  };
  uint32_t set(const uint32_t mask) {
    if (~sts & mask) {
      printf("set int:%.8x %.8x [%s]\n", mask, sts, name);
      sts |= mask;
    }
    return pending(mask);
  };
  void clear(const uint32_t mask) {
    if (sts & mask) {
      printf("clr int:%.8x %.8x [%s]\n", mask, sts, name);
      sts &= ~mask;
    }
  };

  /* Check if interrupts are pending */
  uint32_t pending(const uint32_t mask = -1) const {
    return (sts & en & mask);
  };
};

const char *bb(int n);

const char *sbits(const char *mm, uint16_t bits);
const char *sbits8(const char *mm, uint8_t bits);

constexpr void minmax(auto& d, int lt, int gt) {
  if (d < lt)
    d = lt;
  else if (d > gt)
    d = gt;
}
#endif
