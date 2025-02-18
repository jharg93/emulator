#ifndef __dstk_h__
#define __dstk_h__

#include <stdio.h>
#include <stdarg.h>
#include <vector>

static inline int dlogger(const char *fmt, ...) {
  va_list ap;

  va_start(ap, fmt);
  vfprintf(stderr, fmt, ap);
  return 0;
};

struct dstk {
  uint32_t len;
  uint32_t base = 0;
  std::vector<uint8_t> _stk;
  int (*logfn)(const char *, ...);
public:
  enum {
    VISITED = 0x01,
    PENDING = 0x02,
    JUMPTGT = 0x04,
    MEM     = 0x13,
    CODE    = 0x23,
    STACK   = 0x43,
    DMA     = 0x73,
  };
  void setbase(uint32_t b) {
    base = b;
  }
  dstk(uint32_t _len, int (*logger)(const char *fmt, ...) = dlogger) {
    len = _len;
    _stk.resize(len);
    logfn = logger;
  };
  ~dstk() {
  };
  int memc(int c) {
    switch (c) {
    case MEM :     return 'm';
    case MEM|CODE: return 'x';
    case CODE :    return '@';
    case STACK:    return 'k';
    }
    return '_';
  };
  void showstk(int n = 16) {
    /* Dump stack usage */
    logfn("----:  ");
    for (uint32_t i = 0; i < n; i+= 8) {
      logfn("%-3x     ", i);
    }
    for (uint32_t i = 0; i < len; i++) {
      int c = _stk[i];
      if ((i % n) == 0)
	logfn("\n%.8x:  ", i);
      logfn("%c", memc(c));
    }
    logfn("\n");
  };

  int peek(uint32_t addr) {
    addr -= base;
    if (addr >= 0 && addr <= len)
      return _stk[addr];
    return 0;
  };
  
  /* Add pending address to the stack */
  void push(uint32_t off, int n = 1, int flag = PENDING, const char *what="") {
    off -= base;
    for (uint32_t i = off; i < off+n; i++) {
      if (i >= 0 && i <= len) {
	_stk[i] |= flag | PENDING;
      }
    };
  };
  
  /* Pop next unvisited address */
  int pop() {
    for (uint32_t i = 0; i < len; i++) {
      if ((_stk[i] & (PENDING|VISITED)) == PENDING) {
	_stk[i] |= VISITED;
	return i + base;
      }
    };
    return -1;
  };
};
#endif
