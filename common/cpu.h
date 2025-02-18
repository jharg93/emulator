#ifndef __cpu_h__
#define __cpu_h__
#include "dstk.h"

void        cpu_setpc(const uint32_t);

/* CPU Read/Write */
uint8_t     cpu_read8(const uint32_t, int type=dstk::MEM);
void        cpu_write8(const uint32_t, uint8_t, int type=dstk::MEM);

uint16_t    cpu_read16(const uint32_t, int type = dstk::MEM);
void        cpu_write16(const uint32_t, uint16_t, int type = dstk::MEM);

uint16_t    cpu_read16be(const uint32_t, int type = dstk::MEM);
void        cpu_write16be(const uint32_t, uint16_t, int type = dstk::MEM);

uint32_t    cpu_read32(const uint32_t, int type = dstk::MEM);
void        cpu_write32(const uint32_t, const uint32_t, int type = dstk::MEM);

uint64_t    cpu_read64(const uint32_t, int type = dstk::MEM);
void        cpu_write64(const uint32_t, const uint64_t, int type = dstk::MEM);

uint32_t    cpu_read32be(const uint32_t, int type = dstk::MEM);
void        cpu_write32be(const uint32_t, const uint32_t, int type = dstk::MEM);

/* Generic size read/write */
void        cpu_write(uint32_t addr, uint32_t val, int size);
uint32_t    cpu_read(uint32_t addr, int size);
uint32_t    cpu_fetch(const int size, const char *lbl = "");

/* Stack push/pop */
void        cpu_push8(const uint8_t v);
uint8_t     cpu_pop8();

void        cpu_push16(const uint16_t v);
uint16_t    cpu_pop16();

void        cpu_push32(const uint32_t v);
uint32_t    cpu_pop32();

/* Other helpers */
void        cpu_showregs(void);
void        cpu_shutdown(void);
void        cpu_reset(const uint32_t addr=0);
int         cpu_step();
bool        cpu_irq(int);
void        cpu_nmi();

const char *cpu_flagstr();
const char *cpu_getstate();
bool        cpu_irqenabled();

constexpr uint32_t cpu_fetch8(auto& pc) {
  uint32_t addr = pc;
  pc += 1;
  return cpu_read16(addr, dstk::CODE);
}

constexpr uint32_t cpu_fetch16(auto& pc) {
  uint32_t addr = pc;
  pc += 2;
  return cpu_read16(addr, dstk::CODE);
}

constexpr uint32_t cpu_fetch32(auto& pc) {
  uint32_t addr = pc;
  pc += 4;
  return cpu_read32(addr, dstk::CODE);
}

#if 0
class cpu_t {
public:
  /* Memory handlers */
  virtual uint8_t     cpu_read8(const uint32_t& addr, int mode = dstk::MEM);
  virtual uint16_t    cpu_read16(const uint32_t& addr, int mode = dstk::MEM);
  virtual uint32_t    cpu_read32(const uint32_t& addr, int mode = dstk::MEM);
  virtual void        cpu_write8(const uint32_t& addr, uint8_t val, int mode = dstk::MEM);
  virtual void        cpu_write16(const uint32_t& addr, uint16_t val, int mode = dstk::MEM);
  virtual void        cpu_write32(const uint32_t& addr, uint32_t val, int mode = dstk::MEM);

  /* Stack handlers */
  virtual uint8_t     cpu_pop8();
  virtual uint16_t    cpu_pop16();
  virtual uint32_t    cpu_pop32();
  virtual void        cpu_push8(const uint8_t v);
  virtual void        cpu_push16(const uint16_t v);
  virtual void        cpu_push32(const uint32_t v);
  
  /* Common handlers */
  virtual void        cpu_reset(uint32_t addr) = 0;
  virtual bool        cpu_irq(int n) = 0;
  virtual int         cpu_step() = 0;

  virtual const char *cpu_getflags() = 0;
  virtual void        cpu_showregs() = 0;
};
#endif

/* Overflow and Carry flags */
#define VFLAG(a, b, r, m) ((((a ^ r) & (b ^ r)) & m) != 0)
#define CFLAG(a, b, r, m) ((((a & b) | (a & ~r) | (b & ~r)) & m) != 0)
#define ZFLAG(a, m)       ((a & m) == 0)
#define NFLAG(a, m)       ((a & m) != 0)

#define VFLAG_SUB(a, b, r, m) VFLAG(r, b, a, m)
#define CFLAG_SUB(a, b, r, m) CFLAG(r, b, a, m)

#define VFLAG32(a, b, r) VFLAG(a, b, r, 0x80000000)
#define CFLAG32(a, b, r) CFLAG(a, b, r, 0x80000000)

void dumpmem();

/* List of irq events:
 *  mask = (sts & en & mask) : is irq active
 * level = cpu_irq(level) if mask is valid
 *  name = name of interrupt
 */
struct irqevent {
  uint32_t mask;
  uint32_t level;
  const char *name;
};

bool check_irq(uint32_t sts, uint32_t en, int n, irqevent *map);

struct cpustate_t {
  uint32_t regs[32];
};

uint32_t cpu_getstate(cpustate_t *);

auto predec(auto& v, int n) {
  v -= n;
  return v;
}

auto postinc(auto& v, int n) {
  auto oldv = v;
  v += n;
  return oldv;
}

int run_dma(uint32_t dst, int dstIncr, uint32_t src, int srcIncr, int size);

constexpr int hibyte(uint16_t v) {
  return v >> 8;
};
constexpr int lobyte(uint16_t v) {
  return v & 0xff;
}
constexpr uint16_t ror16(uint16_t m, uint16_t n) {
  return (m >> n) | (m << (16-n));
};
#endif
