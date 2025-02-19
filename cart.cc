#include <vector>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <inttypes.h>
#include <fcntl.h>
#include <time.h>
#include <stdarg.h>
#include <unistd.h>
#include "gr.h"
#include "cart.h"
#include "nes.h"
#include "dstk.h"
#include <math.h>

const char *comment(int n) {
  return "";
}

void flogger(int lvl, const char *fmt, ...);

int _elapsed;

void apu_init();
void apu_tick();
void apu_write(int addr, uint8_t v);
uint8_t apu_read(int addr);
void apu_run_frame();

int  ppuGetMirror();
void ppuSetMirror(int);

#ifndef _WSDL
uint8_t apu_read(int addr) { return 0; };
void apu_run_frame() { };
void apu_init() { };
void apu_tick() { };
void apu_write(int a, uint8_t v) { }
#endif

#define SCANLINE_VBLANK 241
#define SCANLINE_PRE    261

int dma_addr = 0;
int dma_count = 0;
int dma_ticks = 1;

extern uint32_t clockticks6502, clockgoal6502;
extern uint16_t OPC;
extern uint8_t a,x,y,status;

extern bool cpu_irq(int);
extern void cpu_nmi(void);
extern void cpu_reset(uint32_t);
extern int  cpu_tick(int);
extern int rdy;

int curbank, sreg=0;

/* mapping of nametables */
#define MIRROR_4SCR  0x0123
#define MIRROR_HORZ  0x0022
#define MIRROR_VERT  0x0101
#define MIRROR_LEFT  0x0000
#define MIRROR_RIGHT 0x1111

int vismask;

void flogger(int lvl, const char *fmt, ...) {
  va_list ap;

  va_start(ap,fmt);
  vprintf(fmt, ap);
}

/* Mapper code: read/write nbanks */
uint8_t *mapper_t::setbank(int pg) {
  if (pg < 0)
    pg += nBank;
  if (curpg != pg) {
    flogger(0,"======= setbank(%x) %x\n", pg, pg * bankSz);
    curpg = pg;
    page = cartmem + (pg * bankSz);
  }
  return page;
};

bool mapper_t::write(int addr, uint8_t v) {
  int mapped = addr & 0x1FFF;
  
  if (mapped >= bankStart && mapped <= bankEnd) {
    setbank(mapped - bankStart);
  }
  if (addr >= 0x1000 && addr <= 0xFFFF) {
    flogger(2,"Write to ROM\n");
    page[addr % bankSz] = v;
    return true;
  }
  return false;
}

bool mapper_t::read(int addr, uint8_t& v) {
  int mapped = addr & 0x1FFF;

  if (mapped >= bankStart && mapped <= bankEnd) {
    setbank(mapped - bankStart);
  }
  if (addr >= 0x1000 && addr <= 0xFFFF) {
    v = page[addr % bankSz];
    return true;
  }
  return false;
}

/* Mappers
 *  000: PRG=16/32 RAM=2/4  CHR=8
 *  001: PRG=[2]256/512 RAM=32 CHR=[4/4 OR 8]128    001,105,155
 *  002: PRG=[16/16]256/4M  RAM=0  CHR=8K           002,094,180
 *  003: PRG=16/32 RAM=0 CHR=[8]32/2M               003
 *  004: PRG=[8/8/16]512  RAM=8K  CHR=[2/2/1/1/1/1]
 */
/* Return CHR and PRG addresses based on bank 
 * A16|A15 A14 A13 A12|A11 A10 A09 A08|A07 A06 A05 A04|A03 A02 A01 A00 */

/* Mapper 0/Generic : NROM */
struct nesMapper {
  int prgMask, chrMask;
  int prgSz, nPrgSlice;
  int chrSz, nChrSlice;
  uint8_t *prg;
  uint8_t *chr;
  bank_t  *pb;
  bank_t  *cb;

  /* RAM: 0x0000 - 0x1FFF (mirrored) */
  nesMapper(int csl, int csz, uint8_t *c, int psl, int psz, uint8_t *p) :
    prgSz(psz), nPrgSlice(psl),
    chrSz(csz), nChrSlice(csl),
    prg(p), chr(c)
  {
  };

  virtual void scanline() {
  };

#if 1
  /* Set bank to new value */
  void setb(int& br, int nb, const char *name) {
    if (nb != br) {
      flogger(1, "[%s]: setbank %.2x\n", name, nb);
      br = nb;
    }
  };
  uint8_t &cv(int b, int bsz, int addr) {
    return chr[(b * bsz) + (addr % bsz)];
  };
  uint8_t &pv(int b, int bsz, int maxb, int addr) {
    if (b < 0) {
      /* If negative bank, add to total */
      b += maxb;
    }
    addr = (b * bsz) + (addr % bsz);
    printf("BANKIO: %.4x %.4x %.2x\n", (b * bsz), (addr % bsz), prg[addr]);
    if (addr >= prgSz) {
      fprintf(stderr, "toobig %x/%x\n", b * bsz, prgSz);
      exit(0);
    }
    return prg[addr];
  };
  /* Overridables */
  virtual int write(int addr, uint8_t data) {
    assert(0);
    return 0;
  };
  virtual uint8_t &prgaddr(int addr) {
    assert(0);
    return prg[addr % prgSz];
  };
  virtual uint8_t &chraddr(int addr) {
    assert(0);
    return chr[addr];
  };
#endif
};

/* Mapper 1: MMC1
 * 2 16k PRG banks
 * 2 4k  CHR banks
 */
struct nesMapper001 : public nesMapper {
  nesMapper001(int csz, uint8_t *c, int psz, uint8_t *p) :
    nesMapper(2, csz, c, 2, psz, p) {
    flogger(0, "Mapper1: %d PRG, %d CHR\n", psz / 16384, csz / 4096);
    CR0 = 0;
    CR1 = 1;
    update();
  };
  enum {
    MIRROR = 0x03,
    PMODE  = 0x0C,
    CMODE  = 0x10,

    PRG00  = 0x00,
    PRG01  = 0x04,
    PRG02  = 0x08,
    PRG03  = 0x0C,
  };
  int PB[2] = { 0xdead, 0xdead };
  int CB[2] = { 0xdead, 0xdead };
  int CR0, CR1, PR;
  int shifty = 0x10;
  uint8_t ctrl = 0x0c;

  void update() {
    /* Set PRG bank */
    if ((ctrl & PMODE) <= PRG01) {
      setb(PB[0], PR & 0xFE, "001.8000.32k");
      setb(PB[1], PR | 0x01, "001.8000.32k");
    }
    else if ((ctrl & PMODE) == PRG02) {
      setb(PB[0], 0x0, "001.8000.fixed");
      setb(PB[1], PR,  "001.C000.switch");
    }
    else {
      setb(PB[0], PR, "001.8000.switch");
      setb(PB[1], -1, "001.C000.fixed");
    }
    /* Set CHR banks */
    if (ctrl & CMODE) {
      setb(CB[0], CR0, "001.0000.4k");
      setb(CB[1], CR1, "001.1000.4k");
    }
    else {
      setb(CB[0], CR0 & 0xFE, "001.0000.8k");
      setb(CB[1], CR0 | 0x01, "001.0000.8k");
    }
    /* Set new mirror state */
    if ((ctrl & MIRROR) == 0)
      ppuSetMirror(MIRROR_LEFT);	
    else if ((ctrl & MIRROR) == 1)
      ppuSetMirror(MIRROR_RIGHT);
    else if ((ctrl & MIRROR) == 2)
      ppuSetMirror(MIRROR_VERT);
    else if ((ctrl & MIRROR) == 3)
      ppuSetMirror(MIRROR_HORZ);
  }
  int write(int addr, uint8_t data) {
    if (addr >= 0x8000 && addr <= 0xffff) {
      if (data & 0x80) {
	shifty = 0x10;
      }
      else {
	int complete = shifty & 1;
	shifty = (shifty >> 1) | ((data & 1) << 4);
	if (complete) {
	  data = shifty;
	  shifty = 0x10;

	  flogger(0, "Set complete: %.4x %.2x\n", addr, shifty);
	  switch (addr) {
	  case 0x8000 ... 0x9fff:
	    // CPPMM
	    // C=0 {8k mode}
	    // C=1 {4k mode}
	    ctrl = data;
	    break;
	  case 0xa000 ... 0xbfff:
	    CR0 = data;
	    break;
	  case 0xc000 ... 0xdfff:
	    CR1 = data;
	    break;
	  case 0xe000 ... 0xffff:
	    PR = data;
	    break;
	  }
	  update();
	}
      }
    }
    return 0;
  };
  uint8_t &prgaddr(int addr) {
    // assume 16k PRG bank
    int pbank = PB[(addr / 16384) % nPrgSlice];
    return pv(pbank, 16384, prgSz / 16384, addr);
  };
  uint8_t &chraddr(int addr) {
    // ---x.----.----.----
    int cbank = CB[(addr / 4096) % nChrSlice];
    return chr[cbank*4096 + (addr % 4096)];
  };
};

/* Mapper 2: UxROM
 * Compatible: 002/094/180 
 * 2 16k PRG banks [256k]
 *   0x8000 remappable   sel=8000..ffff
 *   0xc000 fixed        -1
 * 1  8k CHR bank
 */
int b2sel(void *arg, uint32_t offset, int mode, uint8_t& data)
{
  bank_t *b = (bank_t *)arg;
  printf("setbank: %x\n", data);
  setbank(b, data);
  return 0;
}

void map002_init(bus_t &mb, bus_t &ppu, uint8_t *prg, int psz, uint8_t *chr, int csz)
{
  static bank_t b[2];

  initbank(&b[0], prg, psz, 16384, 0,  "002.prg0");
  initbank(&b[1], prg, psz, 16384, -1, "002.prg1");
  mb.register_handler(0x8000, 0xBFFF, 0x3FFF, bankio, &b[0], _RD, "002.prg0");
  mb.register_handler(0xC000, 0xFFFF, 0x3FFF, bankio, &b[1], _RD, "002.prg1");
  mb.register_handler(0x8000, 0xFFFF, 0xFFFF, b2sel,  &b[0], _WR, "002.prg0.sel");
}

struct nesMapper002 : public nesMapper {
  nesMapper002(int csz, uint8_t *c, int psz, uint8_t *p) : nesMapper(1, csz, c, 2, psz, p) {
    flogger(0, "Mapper2: %d PRG, %d CHR\n", psz / 16384, 1);
  };
  int PB[2] = { 0, -1 };
  int write(int addr, uint8_t data) {
    if (addr >= 0x8000 && addr <= 0xffff) {
      setb(PB[0], data, "002.prg0");
      setb(PB[1], -1,   "002.prg1");
    };
    return 0;
  };
  uint8_t &prgaddr(int addr) {
    int pbank = PB[(addr / 16384) % nPrgSlice];
    return pv(pbank, 16384, prgSz / 16384, addr);
  };
};

/* Mapper 4: MMC8
 * 8 1k CHR banks [256k]
 * 4 8k PRG banks [512k]
 */
struct nesMapper004 : public nesMapper {
  nesMapper004(int csz, uint8_t *c, int psz, uint8_t *p) : nesMapper(8, csz, c, 4, psz, p) {
    flogger(0, "Mapper4 %.2x PRG, %.2x CHR\n", psz / 8192, csz / 1024);
    chrMask = (csz / 1024) - 1;
    prgMask = (psz / 8192) - 1;
  };
  enum {
    PMODE = 0x40,
    CMODE = 0x80
  };
  uint8_t R[8],reg8000;
  int     PB[4] = { 0,0,0,-1 };
  int     CB[8];
  int     irqCounter = 0, irqReload = 0;
  bool    irqEnabled = false;
  
  int write(int addr, uint8_t data) {
    int x;

    switch (addr & 0xE001) {
    case 0x8000:
      // reg number
      reg8000 = data;
      break;
    case 0x8001:
      // reg value
      flogger(1, "004:R[%x] = %x\n", reg8000 & 7, data);
      R[reg8000 & 7] = data;
      break;
    case 0xA000:
      // mirror mode
      flogger(1, "004:Mirroring: %x\n", data);
      if (ppuGetMirror() != MIRROR_4SCR)
	ppuSetMirror(data & 1 ? MIRROR_HORZ : MIRROR_VERT);
      break;
    case 0xA001:
      flogger(1, "004:prg ram: %x\n", data);
      break;
    case 0xc000:
      flogger(1, "004:irq latch: %x\n", data);
      irqReload = data;
      break;
    case 0xc001:
      flogger(1, "004:irq reload: %x\n", data);
      irqCounter = 0;
      break;
    case 0xe000:
      flogger(1, "000:set irqEnabled false\n");
      irqEnabled = false;
      break;
    case 0xe001:
      flogger(1, "004:set irqEnabled true\n");
      irqEnabled = true;
      break;
    }
    /* Set PRG map */
    x = (reg8000 & PMODE) ? 0x2 : 0x0;
    setb(PB[x],   R[6] & prgMask, "004.prg0");
    setb(PB[1],   R[7] & prgMask, "004.prg1");
    setb(PB[x^2], -2,   "004.prg2");
    setb(PB[3],   -1,   "004.prg3");

    /* Set CHR Map */
    x = (reg8000 & CMODE) ? 0x4 : 0x0;
    setb(CB[x]  , (R[0] & 0xFE) & chrMask, "004.chr0");
    setb(CB[x^1], (R[0] | 0x01) & chrMask, "004.chr1");
    setb(CB[x^2], (R[1] & 0xFE) & chrMask, "004.chr2");
    setb(CB[x^3], (R[1] | 0x01) & chrMask, "004.chr3");
    setb(CB[x^4], R[2] & chrMask, "004.chr4");
    setb(CB[x^5], R[3] & chrMask, "004.chr5");
    setb(CB[x^6], R[4] & chrMask, "004.chr6");
    setb(CB[x^7], R[5] & chrMask, "004.chr7");

    return 0;
  };
  uint8_t &prgaddr(int addr) {
    int pbank = PB[(addr / 8192) % nPrgSlice];
    return pv(pbank, 8192, prgSz / 8192, addr);
  };
  uint8_t &chraddr(int addr) {
    int cbank = CB[(addr / 1024) % nChrSlice];
    return cv(cbank, 1024, addr);
  };
  void scanline() {
    if (irqCounter-- == 0)
      irqCounter = irqReload;
    if (irqEnabled && !irqCounter) {
      flogger(0, "004:scanline: irq\n", irqCounter);
      cpu_irq(0);
    }
  };
};

struct b4_t {
  bank_t  pb[4];
  bank_t  cb[8];
  uint8_t reg;
  uint8_t R[8];
};

int b4sel(void *arg, uint32_t offset, int mode, uint8_t& data)
{
  b4_t *b = (b4_t *)arg;
}

void map004_init(bus_t &mb, bus_t &ppu, uint8_t *prg, int psz, uint8_t *chr, int csz)
{
  static b4_t b;

  for (int i = 0x8000; i < 0xFFFF; i += 0x4000) {
    initbank(&b.pb[i], prg, psz, 0x4000, 0, "004.prg");
    mb.register_handler(i, i+0x3FFF, 0x3FFF, bankio, &b.pb[i], _RD, "004.PRG");
  }
  for (int i = 0x0000; i < 0x1FFF; i += 0x0800) {
    initbank(&b.cb[i], chr, csz, 0x0800, 0, "004.chr");
    mb.register_handler(i, i+0x07FF, 0x07FF, bankio, &b.cb[i], _RD, "004.CHR");
  }
  mb.register_handler(0x8000, 0xFFFF, 0xE001, b4sel, &b, _WR, "004.PRGsel");
}

struct nesMapper69 : public nesMapper {
  nesMapper69(int csz, uint8_t *c, int psz, uint8_t *p) : nesMapper(8, csz, c, 4, psz, p) {
    flogger(0, "Mapper69 %.2x PRG, %.2x CHR\n", psz / 32768, csz / 8192);
    prgMask = (psz / 32768) - 1;
    chrMask = (csz / 8192) - 1;
  };
  uint8_t cmd;
  int C[8] = { 0, 1, 2, 3, 4, 5, 6, 7, }, P[4] = { 0, 1, 2, -1 };
  int write(int addr, uint8_t data) {
    int mirrtype[] = { MIRROR_VERT, MIRROR_HORZ, 0x0000, 0x1111 };
    
    flogger(0, "Mapper69 Write: %.4x %.2x\n", addr, data);
    switch (addr & 0xE000) {
    case 0x8000:
      cmd = data;
      break;
    case 0xA000:
      switch (cmd) {
      case 0 ... 7:
	flogger(0, "set chrbank %d %d\n", cmd, data);
	setb(C[cmd], (data & chrMask), "CHR");
	break;
      case 0x9 ... 0xB:
	flogger(0, "set prgbank %d %d\n", cmd, data);
	setb(P[cmd - 9], (data & prgMask), "PRG");
	break;
      case 0x0c:
	ppuSetMirror(mirrtype[data & 3]);
	break;
      default:
	assert(0);
      };
    };
    return 0;
  };
  uint8_t &prgaddr(int addr) {
    int pbank = P[(addr / 8192) % nPrgSlice];
    return pv(pbank, 8192, prgSz / 8192, addr);
  };
  uint8_t &chraddr(int addr) {
    int cbank = C[(addr / 1024) % nChrSlice];
    return cv(cbank, 1024, addr);
  };
};

struct b7_t {
  bank_t b;
  int    reg;
} b7;

/* Write only */
int b7sel(void *arg, uint32_t offset, int mode, uint8_t& data)
{
  b7_t *b = (b7_t *)arg;

  flogger(0, "B7SEL!!!!! %.2x\n", data);
  setbank(&b->b, (data & 7));
  ppuSetMirror(data & 0x10 ? MIRROR_LEFT : 0x1111);
  return 0;
}

/* 32k PRG 8000...8FFF, sel = 8000..800f
 *  8k CHR 0000...1FFF */
void map007_init(bus_t &mb, bus_t &ppu, uint8_t *prg, int psz, uint8_t *chr, int csz)
{
  static b7_t b;
  
  initbank(&b.b, prg, psz, 32768, -1, "007");
  mb.register_handler(0x8000,  0xFFFF, 0x7FFF, bankio, &b.b,_RD, "007.prg");
  mb.register_handler(0x8000,  0xFFFF, 0x0000, b7sel,  &b,  _WR, "007.sel"); 
}

// PRG: 256k Capacity, 32k window, sel.wr.8000.000f
// CHR: 8k
struct nesMapper007 : public nesMapper {
  nesMapper007(int csz, uint8_t *c, int psz, uint8_t *p) : nesMapper(8, csz, c, 4, psz, p) {
    flogger(0, "Mapper7 %.2x PRG, %.2x CHR\n", psz / 32768, csz / 8192);
    prgMask = (psz / 32768) - 1;
    chrMask = (csz / 8192) - 1;
  };
};

nesMapper *mapper;

/* NES memory map:
 *   0x0000 - 0x07FF : 2k internal ram
 *   0x0800 - 0x0FFF : mirrors 0000..07FF
 *   0x1000 - 0x17FF : mirrors 0000..07FF
 *   0x1800 - 0x1FFF : mirrors 0000..07FF
 *
 *   0x2000 - 0x2007 : ppu registers
 *   0x2008 - 0x3FFF : mirrors 2000..2007
 *
 *   0x4000 - 0x4017 : APU & I/O registers
 *   0x4018 - 0x401F : disabled
 * 
 *   0x4020 - 0xFFFF : Cartridge space
 */

/* PPU memory map:
 * 0000-0FFF : pattern table 0 [chr rom] (4k = 256 tiles * 16 bytes per tile)
 * 1000-1FFF : pattern table 1 [chr rom] (4k = 256 tiles * 16 bytes per tile)
 * 2000-23FF : name table 1
 * 2400-27FF : name table 2
 * 2800-2Bff : name table 3
 * 2C00-2FFF : name table 4
 * 3000-33FF : nt1 mirror
 * 3400-37FF : nt2 mirror
 * 3800-3BFF : nt3 mirror
 * 3C00-3EFF : nt4 mirror
 * 3F00-3F1F : palette
 *             0123 4567 89ab cdef
 *             -123 -567 -9ab -def  : '-' is transparent
 * 3F20-3FFF : palette mirror
 */
struct sprite_t {
  uint8_t y;
  uint8_t index; // tttttttn
  uint8_t attr;  // vhp---PP
  uint8_t x;
};
#define ATTR_V 0x80
#define ATTR_H 0x40
#define ATTR_P 0x20

enum {
  UL,
  UR,
  LL,
  LR
};

// BG:  bgclr = bmk(attrbyte,    bgH, bgL,    15 - finex)
// SPR: bgclr = bmk(oam[i].attr, patH, patL,   7 - x)
int bmk(int attr, uint16_t l, uint16_t h, int m)
{
  l = (l >> m) & 1;
  h = (h >> m) & 1;
  return (attr & 3)*4 + (l * 2) + h;
}

// Sprite Attr bits
// v-------
// -h------
// --p-----
// ------PP
struct nesgraphic {
  uint16_t patLsb;
  uint16_t patMsb;
  uint16_t atLsb;
  uint16_t atMsb;
  int      finex;
  int      delay;
  int      palid;
  uint8_t  enabled;
  uint8_t  attr;

  /* Set graphic bits */
  void clear() {
    patLsb = patMsb = atLsb = atMsb = 0;
  };
  int setBits(int fx, uint8_t l, uint8_t m, uint8_t at) {
    finex = fx;
    patLsb = (patLsb & 0xFF00) | l;
    patMsb = (patMsb & 0xFF00) | m;
    atLsb  = (atLsb  & 0xFF00) | ((at & 1) ? 0xff : 0x00);
    atMsb  = (atMsb  & 0xFF00) | ((at & 2) ? 0xff : 0x00);
    return 0;
  }
  int getpixel() {
    int p;
#if 0
    fprintf(stderr, "scanline:%4d hPos:%4d delay:%d pat:%.2x %.2x palid:%x\n",
	    scanline, hPos, delay, patLsb, patMsb, palid);
#endif
    if (!enabled)
      return 0;
    /* Wait for delay counter */
    if (delay > 0) {
      delay--;
      return 0;
    }
    if (finex <= 7) {
      int p0, p1;

      p0 = (patLsb >> (15-finex)) & 1;
      p1 = (patMsb >> (14-finex)) & 2;
      p = p0 + p1;

      p0 = (atLsb >> (15-finex)) & 1;
      p1 = (atMsb >> (14-finex)) & 2;
      palid = 0x3f00 + (p0 + p1)*4;

      patMsb <<= 1;
      patLsb <<= 1;
      atMsb <<= 1;
      atLsb <<= 1;
      if (!p)
	return 0;
      return palid + p;
    }
    if (attr & ATTR_H) {
      p = ((patMsb << 1) & 2) | (patLsb & 1);
      patMsb >>= 1;
      patLsb >>= 1;
    } else {
      p = ((patMsb >> 14) & 2) | ((patLsb >> 15) & 1);
      patMsb <<= 1;
      patLsb <<= 1;
    }
    if (!p)
      return 0;
    return palid + p;
  };
};

struct ppu : public bus_t {
  ppu() : bus_t(16384) { };
  enum {
    /* Registers */
    PPUCTRL   = 0x2000, // writeonly
    PPUMASK   = 0x2001, // writeonly
    PPUSTATUS = 0x2002, // read
    OAMADDR   = 0x2003, // writeonly
    OAMDATA   = 0x2004, // read/write
    PPUSCROLL = 0x2005, // writeonly
    PPUADDR   = 0x2006, // writeonly
    PPUDATA   = 0x2007, // read/write

    OAMDMA    = 0x4014, // write
    CTRLR1    = 0x4016,
    CTRLR2    = 0x4017,

    PPUCTRL_NMI        = D7,
    PPUCTRL_MASTER     = D6,
    PPUCTRL_SPRITESZ   = D5,
    PPUCTRL_BGTBL      = D4,
    PPUCTRL_SPRITETBL  = D3,
    PPUCTRL_INCR       = D2,
    PPUCTRL_NTMASK     = (D1|D0),
    PPUCTRL_VTAB       = D1,
    PPUCTRL_HTAB       = D0,

    PPUMASK_BLUE       = D7,
    PPUMASK_GREEN      = D6,
    PPUMASK_RED        = D5,
    PPUMASK_SHOWSPRITE = D4,
    PPUMASK_SHOWBG     = D3,
    PPUMASK_LEFTSPRITE = D2,
    PPUMASK_LEFTBG     = D1,
    PPUMASK_GRAYSCALE  = D0,

    PPUSTATUS_V        = 0x80, // vblank
    PPUSTATUS_S        = 0x40, // sprite 0 hit
    PPUSTATUS_O        = 0x20, // sprite overflow

    HTAB   = 0x400,
    VTAB   = 0x800,
    VHTAB  = 0xc00,
    
    CXMASK  = (0x1F << 0),
    CYMASK  = (0x1F << 5),
    ACXMASK = (0x1C << 0),
    ACYMASK = (0x1C << 5),
    
    VHMASK  = (0x03 << 10),
    FYMASK  = (0x07 << 12),
    
    CX31   = 0x1F,
    CY29   = (29 << 5),
    CY31   = (31 << 5),

    MAX_SPRITE = 64,
    SPRITE_OVERFLOW = 8,
  };

  /* Background/Sprite chr table */
  int       chwr;
  uint8_t   ram[0x4000];
  uint8_t   oam[256]{240};

  /* Nametable pointers */
  uint8_t  *nametable = &ram[0x2000];
  uint8_t  *nt[4] = {
    /* Default nametables. Reset with setmirror */
    &nametable[0x000],
    &nametable[0x400],
    &nametable[0x800],
    &nametable[0xC00]
  };

  /* Palette pointer 
   *  0x3f00 : BG Color
   *  0x3f01 : BG Palette 0
   *  0x3f05 : BG Palette 1
   *  0x3f09 : BG Palette 2
   *  0x3f0d : BG Palette 3
   *  0x3f10 : 0x3f00 mirror
   *  0x3f11 : Sprite Palette 0
   *  0x3f14 : 0x3f04 mirror
   *  0x3f15 : Sprite Palette 1
   *  0x3f18 : 0x3f08 mirror
   *  0x3f19 : Sprite Palette 2
   *  0x3f1c : 0x3f0c mirror
   *  0x3f1d : Sprite Palette 3
   *
   *  BPPpp : pp = pixel data, PP = pallete #, B = Background/Sprite
   */
  uint8_t   *palette = &ram[0x3f00];

  /* Sprite Data */
  sprite_t *o_sprites = (sprite_t *)&oam[0];
  sprite_t  s_sprites[SPRITE_OVERFLOW];
  
  /* PPU Registers */
  uint8_t   last;
  uint8_t   ppuctrl;
  uint8_t   ppumask;
  uint8_t   ppustatus;
  uint8_t   oamaddr;
  uint8_t   ppudata_cpy;
  int       latch;

  int       mirror = -1;

  /* Addresses for vram */
  int       vramIncr;
  int       nameTbl;
  int       bgTbl;
  int       spriteTbl;
  int       spriteSz;
  
  // Video Addr (PPUADDR)
  //  Nametables = 0x2000 .. 0x3FFF
  // Coarse X   ----.----|---X.XXXX   0..31
  // Coarse Y   ----.--YY|YYY-.----   0..31
  // Horiz NT   ----.-N--|----.----
  // Vert NT    ----.V---|----.----
  // Fine Y     -yyy.----|----.----   0..7
  // Fine X
  uint16_t  finex = 0;
  union {
    uint16_t vramaddr = 0;
    struct {
      uint16_t cx : 5;
      uint16_t cy : 5;
      uint16_t h  : 1;
      uint16_t v  : 1;
      uint16_t fy : 3;
    };
  };
  uint16_t  tramaddr = 0;

  int rendering() {
    return ppumask & (PPUMASK_SHOWBG|PPUMASK_SHOWSPRITE);
  }

  void copyhorz() {
    // h_update
    // ----.-h--.---X.XXXX
    if (rendering()) {
      vramaddr = (vramaddr & ~0x41F) | (tramaddr & 0x41F);
    }
  }
  void copyvert() {
    // v_update
    // -yyy.v-yy.yyy-.----
    if (rendering()) {
      vramaddr = (vramaddr & ~0x7BE0) | (tramaddr & 0x7BE0);
    }
  }
  void inc_hori() {
    /* h_scroll */
    if (!rendering())
      return;
    if ((vramaddr & CXMASK) == 31) {
      vramaddr ^= (HTAB|CXMASK);
    }
    else {
      vramaddr++;
    }
  };
  void inc_vert() {
    /* v_scroll */
    if (!rendering())
      return;
    if (fy != 7) {
      fy++;
    }
    else {
      fy = 0;
      if (cy == 29) {
	cy = 0;
	v ^= 1;
      }
      else if (cy == 31) {
	cy = 0;
      }
      else {
	cy++;
      }
    }
  };
  uint8_t read(int addr);
 void    write(int addr, uint8_t data);

  uint8_t readreg(int addr);
  void    writereg(int addr, uint8_t data);

  /* Set mirroring mode. each nybble maps to different nameid */
  void setmirror(int mode) {
    if (mirror != mode) {
      flogger(0, "Set Mirror: %.4x\n", mode); 
      mirror = mode;
      nt[UL] = &nametable[((mode >> 12) & 0xF) * 0x400];
      nt[UR] = &nametable[((mode >> 8)  & 0xF) * 0x400];
      nt[LL] = &nametable[((mode >> 4)  & 0xF) * 0x400];
      nt[LR] = &nametable[((mode >> 0)  & 0xF) * 0x400];
    }
  };

  /* Load any sprites on this scanline */
  int loadsprites(int y) {
    uint8_t l,m;
    int count = 0;
    int i, dy, h = spriteSz;
    int tile, base;
    
    flogger(1, "------------------------ LoadSprites\n");
    for (int i = 0; i < MAX_SPRITE; i++) {
      /* Ignore off-screen sprites */
      if (o_sprites[i].y >= 239 || o_sprites[i].x == 255) {
	continue;
      }
      dy = (y - o_sprites[i].y);
      if (dy >= 0 && dy < h) {
	flogger(1, "LoadSprite %d: (%d,%d) 8x%d %c%c pri:%d pal:%x tile:%.2x\n",
		i, o_sprites[i].x, o_sprites[i].y,spriteSz,
		o_sprites[i].attr & ATTR_V ? 'v' : '-',
		o_sprites[i].attr & ATTR_H ? 'h' : '-',
		!!(o_sprites[i].attr & ATTR_P),
		(o_sprites[i].attr & 3),
		o_sprites[i].index);
	if (count >= SPRITE_OVERFLOW) {
	  flogger(0, "sprite overflow\n");
	  ppustatus |= PPUSTATUS_O;
	  return SPRITE_OVERFLOW;
	}
	/* Vert mirror */
	if (spriteSz == 16) {
	  base = (o_sprites[i].index & 1) << 12;
	  tile = o_sprites[i].index & 0xFE;
	  if (o_sprites[i].attr & ATTR_V) {
	    dy ^= 0xF;
	  }
	  if (dy >= 8) {
	    tile++;
	    dy &= 7;
	  }
	}
	else {
	  base = spriteTbl;
	  tile = o_sprites[i].index;
	  if (o_sprites[i].attr & ATTR_V)
	    dy ^= 7;
	}
	/* Load sprite tiles data */
	l = read(base + tile * 16 + dy);
	m = read(base + tile * 16 + dy + 8);
	spr[count].setBits(8, l, m, 0);
	spr[count].attr    = o_sprites[i].attr;
	spr[count].palid   = 0x3f10 + 4*(o_sprites[i].attr & 3);
	spr[count].enabled = ppumask & PPUMASK_SHOWSPRITE;
	spr[count].delay   = o_sprites[i].x - 2;
	if ((o_sprites[i].attr & ATTR_H) == 0)
	  spr[count].delay -= 8;
	s_sprites[count++] = o_sprites[i];
      }
    }
    for (i = count; i < 8; i++) {
      spr[i].enabled = 0;
    }
    return count;
  };

  /*=======================================
   * Rendering fetch 
   *=======================================*/
  uint16_t    ntbyte, atbyte, patLsb, patMsb;
  nesgraphic  bk;
  nesgraphic  spr[8];
  void fetchsprite(int clk) {
    if ((ppumask & (PPUMASK_SHOWBG|PPUMASK_SHOWSPRITE)) == 0)
      return;
    switch (clk % 8) {
    case 1: // dummy nt read
      break;
    case 3: // dummy at read
      break;
    case 5:
      break;
    case 7:
      break;
    }
  };
  void fetch(int clk) {
    int fineY, addr, qb;

    if ((ppumask & (PPUMASK_SHOWBG|PPUMASK_SHOWSPRITE)) == 0)
      return;

    // vramaddr:
    //  -yyy.VHab.cdeF.GHIJ = vramaddr
    //  0010.VHab.cdeF.GHIJ = ntbyte
    //  0010.VH11.11ab.cFGH = atbyte
    //  0000.0000.0000.0dI0 = attr quadrant
    fineY = (vramaddr & FYMASK) >> 12;
    switch(clk % 8) {
    case 0x1: // ntbyte
      addr = 0x2000 + (vramaddr & 0x0FFF);
      ntbyte = read(addr) * 16;
      break;
    case 0x3: // atbyte
      addr = 0x23C0 + (vramaddr & VHTAB) + ((vramaddr & ACYMASK) >> 4) + ((vramaddr & ACXMASK) >> 2);
      qb = ((vramaddr >> 4) & 0x4) + (vramaddr & 0x2);
      atbyte = (read(addr) >> qb) & 3;
      break;
    case 0x5: // patlsb
      addr = bgTbl + ntbyte + fineY;
      patLsb = read(addr);
      break;
    case 0x7: // patmsb
      addr = bgTbl + ntbyte + fineY + 8;
      patMsb = read(addr);
      flogger(1, "fx:%x %.2x %.2x %x vram=%.4x\n", finex, patLsb, patMsb, atbyte, vramaddr);
      bk.enabled = ppumask & PPUMASK_SHOWBG;
      bk.setBits(finex, patLsb, patMsb, atbyte);
      // Increment CX
      inc_hori();
      break;
    }
  };
};

int paladdr (int addr) {
  addr &= 0x1F;
  if (addr == 0x10 || addr == 0x14 || addr == 0x18 || addr == 0x1c)
    addr -= 0x10;
  return addr;
}

const char *ppureg(int n) {
  switch (n) {
  case ppu::PPUCTRL:   return "PPUCTRL";
  case ppu::PPUMASK:   return "PPUMASK";
  case ppu::PPUSTATUS: return "PPUSTATUS";
  case ppu::OAMDATA:   return "OAMDATA";
  case ppu::OAMADDR:   return "OAMADDR";
  case ppu::PPUSCROLL: return "PPUSCROLL";
  case ppu::PPUADDR:   return "PPUADDR";
  case ppu::PPUDATA:   return "PPUDATA";
  }
  return "";
}

/* Read from ppu memory */
uint8_t ppu::read(int addr) {
  uint8_t data;
  
  if (bus_t::read(addr, data) == 0)
    return data;
  flogger(0, "Unknown ppu read address: %x\n", addr);
  return 0xff;
};

/* Write to ppu memory */
void ppu::write(int addr, uint8_t data) {
  last = data & 0x1F;

  if (bus_t::write(addr, data) == 0)
    return;
  flogger(0, "Unknown ppu write address: %x\n", addr);
};

/* Write PPU register */
uint8_t ppu::readreg(int addr) {
  uint8_t data = 0xFF;
  
  flogger(1, " ppu.readreg(%x:%s)\n", addr, ppureg(addr));
  switch (addr) {
  case PPUCTRL:
    data = ppuctrl;
    break;
  case PPUSTATUS:
    data = ppustatus | (last & 0x1F);
    ppustatus &= ~PPUSTATUS_V;
    latch = 0;
    break;
  case OAMDATA:
    data = oam[oamaddr];
    break;
  case PPUADDR:
    break;
  case PPUDATA:
    /* Palette data is not buffered */
    if ((vramaddr & 0x3FFF) < 0x3F00) {
      data = ppudata_cpy;
      ppudata_cpy = read(vramaddr);
    }
    else {
      data = read(vramaddr);
    }
    vramaddr += vramIncr;
    break;
  default:
    flogger(0, "Unknown ppu read register:%x\n", addr);
    break;
  }
  return data;
};

/* Read PPU Register */
#define yn(x) (data & (x)) ? "yes" : "no"

#define chg(a,b) !((data ^ (a)) & (b))

void ppu::writereg(int addr, uint8_t data) {
  flogger(1, " ppu.writereg(%.4x:%s,%.2x)\n", addr, ppureg(addr), data);
  switch (addr) {
  case PPUCTRL:
    bgTbl     = (data & PPUCTRL_BGTBL) ? 0x1000 : 0x0000;
    spriteTbl = (data & PPUCTRL_SPRITETBL) ? 0x1000 : 0x0000;
    spriteSz  = (data & PPUCTRL_SPRITESZ)  ? 16 : 8;
    nameTbl   = 0x2000 + ((data & PPUCTRL_NTMASK) * 0x400);
    vramIncr  = (data & PPUCTRL_INCR) ? 32 : 1;

    flogger(1, " PPUCTRL:NMI         %s\n",   yn(PPUCTRL_NMI));
    flogger(1, " PPUCTRL:MASTER      %s\n",   yn(PPUCTRL_MASTER));
    flogger(1, " PPUCTRL::NameTable  %.4x\n", nameTbl);
    flogger(1, " PPUCTRL::BGTBL      %.4x\n", bgTbl);
    flogger(1, " PPUCTRL::SPRITETBL  %.4x\n", spriteTbl);
    flogger(1, " PPUCTRL::SPRITESIZE 8x%d\n", spriteSz);
    flogger(1, " PPUCTRL::VRAM Incr  %d\n",   vramIncr);
    ppuctrl = data;
    // data:               ----.--VH
    // tramaddr: ----.VH--.----.----
    tramaddr = (tramaddr & 0xF3FF) | ((data & 3) << 10);
    flogger(1, "setctrl: tram=%.4x\n", tramaddr);
    break;
  case PPUMASK:
    flogger(1," PPUMASK::LeftBG     : %s\n", yn(PPUMASK_LEFTBG));
    flogger(1," PPUMASK::LeftSprite : %s\n", yn(PPUMASK_LEFTSPRITE));
    flogger(1," PPUMASK::ShowBG     : %s\n", yn(PPUMASK_SHOWBG));
    flogger(1," PPUMASK::ShowSprite : %s\n", yn(PPUMASK_SHOWSPRITE));
    vismask = data & (PPUMASK_SHOWBG|PPUMASK_SHOWSPRITE);
    ppumask = data;
    break;
  case PPUSTATUS:
    break;
  case OAMADDR:
    oamaddr = data;
    break;
  case OAMDATA:
    oam[oamaddr++] = data;
    break;
  case PPUSCROLL:
    if (!latch) {
      // write lower half of scroll register (X)
      // data    :           ABCD.Efgh
      // finex   :           ----.-fgh
      // tramaddr: ----.----.---A.BCDE
      finex    = (data & 7);
      tramaddr = (tramaddr & 0x7FE0) | (data >> 3);
      flogger(1, " P:setscroll: cx:%d fx:%d\n", (data >> 3), data & 7);
    }
    else {
      // write upper half of scroll register (Y)
      // data:               ABCD.Efgh
      // tramaddr: -fgh.--AB.CDE-.----
      tramaddr = (tramaddr & ~0x73E0) | ((data << 12) & FYMASK) | ((data << 2) & CYMASK);
      flogger(1, " P:setscroll: cy:%d fy:%d\n", (data >> 3), data & 7);
      flogger(sreg, " P:tramaddr: %.4x NT:%d  Y=%d.%d  X=%d.%d\n", tramaddr,
	      (tramaddr >> 10) & 0x3,
	      (tramaddr >>  5) & 0x1F,
	      (tramaddr >> 12) & 0x7,
	      (tramaddr >>  0) & 0x1F,
	      finex);
    }
    latch ^= 1;
    break;
  case PPUADDR:
    if (!latch) {
      // write UPPER part of register first
      // data            -yyy.vhYY
      // tram: -yyy.vhYY.----.----
      tramaddr = (tramaddr & 0x00FF) | ((data & 0x3F) << 8);
    }
    else {
      // write LOWER part of register second
      // data            YYYX.XXXX
      // tram: ----.----.YYYX.XXXX
      tramaddr = (tramaddr & 0xFF00) | data;
      vramaddr = tramaddr;
      flogger(sreg, " P:vramaddr: %.4x NT:%d  Y=%d.%d  X=%d.%d\n", vramaddr,
	      (vramaddr >> 10) & 0x3,
	      (vramaddr >>  5) & 0x1F,
	      (vramaddr >> 12) & 0x7,
	      (vramaddr >>  0) & 0x1F,
	      finex);
    }
    latch ^= 1;
    break;
  case PPUDATA:
    /* Write data & increment address */
    flogger(1, " P:vramwrite: %.4x = %.2x,%d\n", vramaddr, data, vramIncr);
    write(vramaddr, data);
    vramaddr += vramIncr;
    break;
  default:
    flogger(0, "Unknown ppu register:%x\n", addr);
    break;
  }
};

struct ppu ppu;

int  ppuGetMirror() {
  return ppu.mirror;
}

void ppuSetMirror(int m) {
  ppu.setmirror(m);
}

void dumpcfg(uint8_t *buf, int sz);

/*=====================================================================
 *=====================================================================
 *                    IO functions for NES
 *=====================================================================
 *=====================================================================*/
int ppuio(void *arg, uint32_t addr, int mode, uint8_t& data)
{
  if (mode == 'w')
    ppu.writereg(addr, data);
  else
    data = ppu.readreg(addr);
  return 0;
}

int apuio(void *arg, uint32_t addr, int mode, uint8_t& data)
{
  if (mode == 'w')
    apu_write(addr, data);
  else
    data = apu_read(addr);
  return 0;
}

int ctrlio(void *arg, uint32_t addr, int mode, uint8_t& data)
{
  nescart *nc = (nescart *)arg;
  if (mode == 'r') {
    /* Read out serial data */
    data = !!(nc->controller_state[addr] & 0x1);
    nc->controller_state[addr] >>= 1;
  }
  else if (!(data & 1)) {
    /* Save current keystate */
    nc->controller_state[addr] = nc->controller[addr];
  }
  return 0;
}

int prgio(void *arg, uint32_t addr, int mode, uint8_t& data)
{
  nesMapper *m = (nesMapper *)arg;
  
  if (mode == 'w')
    m->write(addr, data);
  else {
    data = m->prgaddr(addr);
  }
  return 0;
}

int chrio(void *arg, uint32_t addr, int mode, uint8_t& data)
{
  nesMapper *m = (nesMapper *)arg;
  
  if (mode == 'r')
    data = m->chraddr(addr);
  else if (ppu.chwr)
    m->chraddr(addr) = data;
  return 0;
}

int dmaio(void *arg, uint32_t addr, int mode, uint8_t& data)
{
  if (mode != 'w')
    return 0;
  if (mode == 'w') {
    dma_addr = data * 256;
    dma_count = 256;
    clockticks6502 += 512;
  }
  return 0;
}

int ntio(void *arg, uint32_t addr, int mode, uint8_t& data)
{
  if (mode == 'r')
    data = ppu.nt[(addr >> 10) & 3][addr & 0x3FF];
  else
    ppu.nt[(addr >> 10) & 3][addr & 0x3FF] = data;
  return 0;
}

int palio(void *arg, uint32_t addr, int mode, uint8_t& data)
{
  return memio(arg, addr, mode, data);
}

nescart::nescart(const char *file) : cart(file), mb(65536) {
  int offset = sizeof(*hdr);
  int mappertype, i;
  int chram = _RD;    /* readonly */

  mb.init(0xffff);
  hdr = (nes_header *)data;
  if (hdr->mapper1 & D2) {
    // skip trainer
    flogger(0, "skip trainer\n");
    offset += 512;
  }

  mapper = NULL;
  prgRomSz = hdr->prgRomSz * 16384;
  chrRomSz = hdr->chrRomSz * 8192;
  prgRamSz = hdr->prgRamSz * 8192;

  /* Setup offsets to memory */
  prgRom = &data[offset];
  if (!chrRomSz) {
    /* No CHR rom */
    flogger(0, "Allocating CHR-rom\n");
    chrRomSz = 8192;
    chrRom = new uint8_t[chrRomSz]{0xAA};
    ppu.chwr = 1;
    chram = _RW;
  }
  else {
    ppu.chwr = 0;
    chrRom = &data[offset + prgRomSz];
  }
  //dumpcfg(prgRom, prgRomSz);

  mappertype = (hdr->mapper1 >> 4) | (hdr->mapper2 & 0xF0);
  flogger(0, "NES Header\n");
  flogger(0, " PRG RAM size: %5d\n", prgRamSz);
  flogger(0, " PRG ROM size: %5d @ %x\n", prgRomSz, offset);
  flogger(0, " CHR ROM size: %5d @ %x\n", chrRomSz, offset + prgRomSz);
  flogger(0, " mapper: %.3d [%.2x %.2x]\n", mappertype, hdr->mapper1, hdr->mapper2);

  printf("size is: %x\n", prgRomSz);
  romMask = 0x7fff;
  if (prgRomSz < 0x8000)
    romMask = 0x3fff;
#if 0
  switch (mappertype) {
  case 69:
    mapper = new nesMapper69(chrRomSz, chrRom, prgRomSz, prgRom);
    break;
  case 004:
    mapper = new nesMapper004(chrRomSz, chrRom, prgRomSz, prgRom);
    map004_init(mb, ppu, prgRom, prgRomSz, chrRom, chrRomSz);
    break;
  case 007:
    mapper = new nesMapper007(chrRomSz, chrRom, prgRomSz, prgRom);
    map007_init(mb, ppu, prgRom, prgRomSz, chrRom, chrRomSz);
    break;
  case 002:
    mapper = new nesMapper002(chrRomSz, chrRom, prgRomSz, prgRom);
    map002_init(mb, ppu, prgRom, prgRomSz, chrRom, chrRomSz);
    break;
  case 001:
    mapper = new nesMapper001(chrRomSz, chrRom, prgRomSz, prgRom);
    break;
  case 0000:
    mapper = new nesMapper(1, chrRomSz, chrRom, 1, prgRomSz, prgRom);
    break;
  default:
    exit(0);
    break;
  }
#endif
  
  /* Add in CPU memory space */
  mb.register_handler(0x0000, 0x1FFF, 0x07FF, memio, nesram, _RW, "RAM"); /* RAM Area */
  mb.register_handler(0x2000, 0x3FFF, 0x2007, ppuio, &ppu,   _RW, "PPU");      /* PPU Registers */
  mb.register_handler(0x4000, 0x4013, 0xFFFF, apuio, NULL,   _RW, "APU");      /* Sound HW */
  mb.register_handler(0x4015, 0x4015, 0xFFFF, apuio, NULL,   _RW, "APU");      /* Sound HW */
  mb.register_handler(0x4014, 0x4014, 0xFFFF, dmaio, this,   _RW, "DMA");      /* DMA */
  mb.register_handler(0x4016, 0x4017, 0x0001, ctrlio,this,   _RW, "CTRL");     /* Input controller */
  mb.register_handler(0x6000, 0x7FFF, 0x1FFF, memio, prgram, _RW, "PRGRAM");   /* PRG-RAM */
  ppu.register_handler(0x2000, 0x3EFF, 0xFFFF, ntio,  NULL,  _RW, "NT");        /* Nametables */

  mb.register_handler(0x8000,  0xFFFF, 0x7FFF, memio, prgRom, _RD, "PRG");      /* PRG ROM */
  ppu.register_handler(0x0000, 0x1FFF, 0xFFFF, memio, chrRom, chram, "CHR");     /* CHR ROM or RAM */

  for (i = 0x3F00; i <= 0x3FFF; i++) {
    int pa = paladdr(i);
    ppu.register_handler(i, i, 0x0000, palio, &ppu.palette[pa], 0, "palette");
  }
  mb.dump();
  ppu.dump();
 
  if (hdr->mapper1 & D3) {
    // 4-screen
    flogger(0, "4-screen\n");
    ppu.setmirror(MIRROR_4SCR);
  }
  else if (hdr->mapper1 & D0) {
    // Vertical mirror (2000=2800, 2400=2C00) */
    flogger(0, "Vertical\n");
    ppu.setmirror(MIRROR_VERT);
  }
  else {
    // Horizontal mirror (2000=2400, 2800=2C00) */
    flogger(0, "Horizontal\n");
    ppu.setmirror(MIRROR_HORZ);
  }

  /* Setup our colors */
  scr = new Screen(256, 240, 0, 10, 64, nespalette);
  scr->init();
};


/* Name Table to Attribute Table mapping
 *   32 tiles per row x 30 rows = 960 tiles
 *    8 attr x 7.5 attr 
 *    8 attr  per row = 16 quadrants per row 
 *       2 horiz tile per quadrant
 *
 * 00          01          02          03          04          05         06           07            ax
 * 00    02    04    06    08    0a    0c    0e    10    12    14    16   18     1a    1c    1e          ay
 * @--+--%--+--@--+--%--+--@--+--%--+--@--+--%--+--@--+--%--+--@--+--%--+--@--+--%--+--@--+--%--+--@ 00  00
 * |A0|A0|A2|A2|B0|B0|B2|B2|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
 * +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+ 01
 * |A0|A0|A2|A2|B0|B0|B2|B2|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
 * %--+--%--+--%--+--%--+--%--+--%--+--%--+--%--+--%--+--%--+--%--+--%--+--%--+--%--+--%--+--%--+--% 02
 * |A4|A4|A6|A6|B4|B4|B6|B6|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
 * +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+ 03
 * |A4|A4|A6|A6|B4|B4|B6|B6|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
 * @--+--%--+--@--+--%--+--@--+--%--+--@--+--%--+--@--+--%--+--@--+--%--+--@--+--%--+--@--+--%--+--@ 04  08
 * |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
 * +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+ 05
 * |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
 * %--+--%--+--%--+--%--+--%--+--%--+--%--+--%--+--%--+--%--+--%--+--%--+--%--+--%--+--%--+--%--+--% 06
 * |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
 * +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+ 07
 * |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
 * @--+--%--+--@--+--%--+--@--+--%--+--@--+--%--+--@--+--%--+--@--+--%--+--@--+--%--+--@--+--%--+--@ 08  10
 * |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
 * +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+ 09
 * |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
 * %--+--%--+--%--+--%--+--%--+--%--+--%--+--%--+--%--+--%--+--%--+--%--+--%--+--%--+--%--+--%--+--% 0a
 * |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
 * +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+ 0b
 * |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
 * @--+--%--+--@--+--%--+--@--+--%--+--@--+--%--+--@--+--%--+--@--+--%--+--@--+--%--+--@--+--%--+--@ 0c  18
 * |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
 * +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+ 0d
 * |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
 * %--+--%--+--%--+--%--+--%--+--%--+--%--+--%--+--%--+--%--+--%--+--%--+--%--+--%--+--%--+--%--+--% 0e
 * |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
 * +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+ 0f
 * |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
 * @--+--%--+--@--+--%--+--@--+--%--+--@--+--%--+--@--+--%--+--@--+--%--+--@--+--%--+--@--+--%--+--@ 10  20
 * |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
 * +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+ 11
 * |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
 * %--+--%--+--%--+--%--+--%--+--%--+--%--+--%--+--%--+--%--+--%--+--%--+--%--+--%--+--%--+--%--+--% 12
 * |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
 * +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+ 13
 * |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
 * @--+--%--+--@--+--%--+--@--+--%--+--@--+--%--+--@--+--%--+--@--+--%--+--@--+--%--+--@--+--%--+--@ 14  28
 * |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
 * +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+ 15
 * |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
 * %--+--%--+--%--+--%--+--%--+--%--+--%--+--%--+--%--+--%--+--%--+--%--+--%--+--%--+--%--+--%--+--% 16
 * |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
 * +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+ 17
 * |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
 * @--+--%--+--@--+--%--+--@--+--%--+--@--+--%--+--@--+--%--+--@--+--%--+--@--+--%--+--@--+--%--+--@ 18  30
 * |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
 * +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+ 19
 * |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
 * %--+--%--+--%--+--%--+--%--+--%--+--%--+--%--+--%--+--%--+--%--+--%--+--%--+--%--+--%--+--%--+--% 1a
 * |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
 * +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+ 1b
 * |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
 * @--+--%--+--@--+--%--+--@--+--%--+--@--+--%--+--@--+--%--+--@--+--%--+--@--+--%--+--@--+--%--+--@ 1c  38
 * |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
 * +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+ 1d
 * |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
 * @--+--%--+--@--+--%--+--@--+--%--+--@--+--%--+--@--+--%--+--@--+--%--+--@--+--%--+--@--+--%--+--@ 1e
 */
void nescart::drawnt(int id, int base, int tx, int ty)
{
  int i, cx, cy, av, qb, pp, at;

  /* Nametable is 32x30 */
  cx = cy = 0;
#if 0
  flogger(-1, "ATTR TABLE\n");
  for (i = 0; i < 64; i++) {
    flogger(-1, "%.2x", ppu.nt[id][0x3c0 + i]);
    if ((i & 7) == 7)
      flogger(-1,"\n");
  }
  /* ----.--ab.cdef.ghij : Tile Address
   * ----.----.---f.ghij : Coarse X   (0-31)
   * ----.--ab.cde-.---- : Coarse Y   (0-31)
   * ----.----.----.-di- : Quadrant   (0-3)
   * ----.--11.11ab.cfgh : Attribute address
   */
  flogger(-1, "\n");
#endif
  for (i = 0; i < 32*30; i++) {
    // Attribute Table offset, Quadrant Bit, Attribute Value
    at = ((i >> 4) & 0x38) + ((i >> 2) & 0x07) + (32*30);
    qb = ((i >> 4) & 0x04) + (i & 0x02);
    av = ppu.nt[id][at];

    flogger(1, "%.2x.%x ", ppu.nt[id][i], (av >> qb) & 3);

    // pp = palette index
    pp = (av >> qb) & 0x3;
    drawpat(tx + cx*8, ty + cy*8, base + ppu.nt[id][i] * 16, pp);
    if (++cx >= 32) {
      cx = 0;
      cy++;
      flogger(1, "\n");
    }
  }
}

void nescart::dumpnt(int id)
{
  int i;

  flogger(0,"Name Table %d\n", id);
  for (i = 0; i < 32*30; i++) {
    fprintf(stderr, "%.2x", ppu.nt[id][i]);
    if ((i & 0x1F) == 0x1F) {
      fprintf(stderr,"\n");
    }
  }
  fprintf(stderr,"Attribute Table\n");
  for (i = 0; i < 64; i++) {
    fprintf(stderr,"%.2x ", ppu.nt[id][32*30 + i]);
  }
  fprintf(stderr,"\n");

  fprintf(stderr,"Palette Table\n");
  for (i = 0; i < 16; i++) {
    //printpix(ppu.palette[i], 0, scr->defpal);
    fprintf(stderr,"%.2x ", ppu.palette[i]);
  }
  fprintf(stderr,"\n");
}

/* Draw tile pattern */
void nescart::drawpat(int x, int y, int base, int pid, int hm, int vm, int sprid, int xm) {
  uint16_t patlsb, patmsb, p;
  int i, j, clr[4], pclr, tx, ty;

  clr[0] = ppu.read(0x3f00);
  clr[1] = ppu.read(0x3f01 + pid*4);
  clr[2] = ppu.read(0x3f02 + pid*4);
  clr[3] = ppu.read(0x3f03 + pid*4);
  if (dwpat >= 0) {
    base += dwpat * 4096;
  }
  for (i = 0; i < 8; i++) {
    if (dwpat > 0) {
      patlsb = chrRom[base + i];
      patmsb = chrRom[base + i + 8];
    }
    else {
      patlsb = ppu.read(base + i) ^ xm;
      patmsb = (ppu.read(base + i + 8) ^ xm) << 1;
    }
    for (j = 7; j >= 0; j--) {
      tx = x + (j ^ hm);
      ty = y + (i ^ vm);
      p = (patlsb & 0x1) | (patmsb & 0x2);
      pclr = clr[p];
#if 0
      if (sprid && chkbg(tx,ty))
	ppu.ppustatus |= ppu::PPUSTATUS_S;
#endif
      if (p || pid < 4)
	scr->setpixel(tx, ty, xm ? 0x31 : pclr);
      patlsb >>= 1;
      patmsb >>= 1;
    }
  }
};

#if 0
/* APU:                                    silence
 *   4000: pulse1.control     DDLC.VVVV    30
 *      VVVV=0000 silence, VVVV=1111=maximum
 *      DD=00 0100.0000 12.5%
 *      DD=01 0110.0000 25%
 *      DD=10 0111.1000 50%
 *      DD=11 1001.1111 75% (-25%)
 *      L=length counter halt
 *      C=constant volume
 *   4001: pulse1.sweep       EPPP.NSSS    08
 *      E=enabled
 *      PPP=divider period half-frames
 *      N=negate (0=add to period, 1=subtract from period)
 *      SSS=shift count
 *   4002: pulse1.timerlo     TTTT.TTTT    00
 *   4003: pulse1.timerhi     LLLL.LTTT    00
 *      11-bit counter (TTT.TTTTTTTT), tick every *APU* cycle
 *      raw period = 11860.9/freq = 1
 *
 *   4004: pulse2.control     DDLC.VVVV    30
 *   4005: pulse2.sweep       EPPP.NSSS    08
 *   4006: pulse2.timerlo     TTTT.TTTT    00
 *   4007: pulse2.timerhi     LLLL.LTTT    00
 *
 *   4008: triangle.control  CRRR.RRRR     80
 *   4009:                                 00
 *   400a: triangle.timerlo  TTTT.TTTT     00
 *   400b: triangle.timerhi  LLLL.LTTT     00
 *      11-bit counter (TTT.TTTTTTTT + 1), tick every *CPU* cycle
 *      C=control (length counter halt)
 *      RRRRRRR=reload value
 *      raw period = 55930.4/freq - 1
 *   f = fcpu/(32*(tval=1))
 *   'silence' = tval <= 2 (Ultrasonic)
 *
 *   400c: noise.control     --LC.VVVV     30
 *   400d:                                 00
 *   400e:                   L---.PPPP     00
 *   400f: noise.length      LLLL.L---     00
 *
 *   4010: digital           IL--.RRRR     00
 *   4011: digital           -DDD.DDDD     00
 *   4012: digital.addr      AAAA.AAAA     00
 *   4013: digital.length    LLLL.LLLL     00
 *     7-bit PCM
 *
 *   4015 status.enable.wr   ---D.NT21     0f
 *   4015 status.rd          IF-D.NT21
 *   4017 frame counter      MI-------         (ticks 4 times per frame), irq on last step
 */

/* period = 1/frequency
 * wavelength = speed/frequency
 * bytesPerPeriod = sampleFreq/freq;
 * for (i = 0; i < bytesPerPeriod; i++) {
 *   buf[i] = A * sin(phase/bytesPerPeriod);
 *   phase = (phase + 1) % bytesPerPeriod;
 * }
 */

double apuRate = 1789773.0;
double sampleRate = 44100;            // 44.1 kHz
double frameRate  = 600.0;             // 60Hz
double samplePerFrame = sampleRate / frameRate;
double samplePerTick  = sampleRate / apuRate;
double silent = 0.001;

int soundPlaying = 0;

// tonehz
// tonevolume
// waveperiod       = samplespersecond/tonehz
// sampleindex      = phase
// samplespersecond =
// bytespersample
struct noisy {
  int vol     = 3000;
  int counter = 0;
  int reload  = 100;
  int index   = 0;
  uint8_t pattern = 0b11111111;
  uint8_t output;

  double freq, period;
  double phase, phaseinc;
  
  void f2t(double f) {
    freq = f;
    phaseinc = freq * Tau / sampleRate;

    reload = (1789773 / (16 * f)) - 1;
    flogger(0, "Setting frequency: %f period:%f-> %d\n", f, period, reload);
  }
  int16_t sample(double n) {
    if ((phase += phaseinc) >= Tau)
      phase -= Tau;
    return n * vol;
  }
  int16_t sample_square() {
    double ds = sin(phase);
    int bit = pattern & 1;
    pattern = (pattern >> 1) | (pattern << 7);
    return sample(bit && ds >= 0 ? ds : 0);
  };
  int16_t sample_sin() {
    return sample(sin(phase));
  };
  int16_t sample_triangle() {
    return sample(asin(sin(phase)) * Tau);
  }
  int16_t sample_noise() {
    return (vol * (double)rand() / (double)RAND_MAX);
  };
  int16_t getnoize(int type) {
    type %= 4;
    if (type == 0)
      return sample_square();
    if (type == 1)
      return sample_sin();
    if (type == 2)
      return sample_triangle()/2;
    if (type == 3)
      return sample_noise()*2;
  }
};

int nozetype;

struct nesaudio {
  uint8_t  reg[4];
  const char *name;
  
  uint8_t  pattern;
  struct {
    int counter;
    int reload;
  } timer;
  struct {
    int enabled;
    int counter;
    int reload;
  } length;
  struct {
    int counter;
    int reload;
  } linear;

  int      volume  = 0;
  int      enabled = 0;
  int      output  = 0;

  double freq, period;
  double phase, phaseinc;

  nesaudio() {
    freq = 440.0;
    phaseinc = freq * Tau / sampleRate;
  };
  virtual int tick() {
    return 0;
  };
  virtual void write(int addr, uint8_t nv) {
    reg[addr & 3] = nv;
  };
};

SDL_AudioDeviceID dev;

const int min_samples = 8192;
std::vector<int16_t> apu_queue;

uint8_t LengthTable[] = {
  10,254,20, 2 40, 4,80, 6,160, 8 60,10,14,12,26,14,
  12, 16,24,18,48,20,96,22,192,24,72,26,16,28,32,30
};

struct Pulse : public nesaudio {
  uint8_t duty_cycle;
  Pulse(const char *n) {
    name = n;
  };
  void write(int addr, uint8_t nv) {
    switch (addr & 3) {
    case 0x00:
      // 4000/4004: pulse1.control     DDLC.VVVV
      switch (nv & 0xC0) {
      case 0x00: duty_cycle = 0b01000000; break;
      case 0x40: duty_cycle = 0b01100000; break;
      case 0x80: duty_cycle = 0b01111000; break;
      case 0xC0: duty_cycle = 0b10011111; break;
      }
      length.halt = nv & D5;
      volume = reg[0] & 0x0F;
      fprintf(stderr, "%s: reloaded volume/duty %d/%.2x\n", name, volume, duty);
      break;
    case 0x01:
      // 4001: pulse1.sweep       EPPPNSSS
      // 4005: pulse2.sweep       EPPPNSSS
      break;
    case 0x02:
      // 4002: pulse1.timerlo     TTTT.TTTT
      // 4006: pulse2.timerlo     TTTT.TTTT
      timer.reload = (reload & 0xFF00) | nv;
      break;
    case 0x03:
      // 4003: pulse1.timerhi     LLLL.LTTT
      // 4007: pulse2.timerhi     LLLL.LTTT
      timer.reload = (reload & 0x00FF) | ((nv & 7) << 8);
      timer.counter = timer.reload;

      length_counter = LengthTable[nv >> 5];
      fprintf(stderr, "%s: reloaded timer %d\n", name, reload);
      break;
    }
  };
  void tick() {
    if (!enabled)
      return 0;
    /* if counter expires, set new output bit */
    if (timer.counter-- == 0x0) {
      timer.counter = timer.reload+1;
      output  = (duty & 1);
      duty_cycle = ((duty_cycle >> 1) | (duty_cycle << 7));
    }
    return output;
  };
};

struct Triangle : public nesaudio
{
  Triangle(const char *n) {
    name = n;
  };
  void write(int addr, uint8_t nv) {
    switch (addr & 3) {
    case 0x00:
      // 4008:
      length.halt = (nv & D7);
      linear.reload = (nv & 0x7F);
      linear.counter = linear.reload;
      break;
    case 0x02:
      // 400a: pulse1.timerlo     TTTT.TTTT
      timer.reload = (reload & 0xFF00) | nv;
      break;
    case 0x03:
      // 400b: pulse1.timerhi     LLLL.LTTT    00
      timer.reload = (reload & 0x00FF) | ((nv & 7) << 8);
      timer.counter = timer.reload;
      length.counter = LengthTable[nv >> 5];
      fprintf(stderr, "%s: reloaded timer %d\n", name, reload);
      break;
    };
  }
  virtual int tick() {
    if (!enabled)
      return 0;
    if (timer.counter-- == 0x0) {
      timer.counter = timer.reload+1;
      output  = (pattern & 1);
      pattern = ((pattern >> 1) | (pattern << 7));
    }
    return output;
  };
};

Pulse pulse1("pulse1");
Pulse pulse2("pulse2");
Triangle triangle("triangle");

nesaudio noise, pcm;

/* APU Clocks: every other CPU cycle (CPU/2), (PPU/6) */
void apu_write(int addr, uint8_t v) {
  return;
  switch (addr) {
  case 0x4000 ... 0x4003:
    fprintf(stderr, "pulse1: %.4x %.2x\n", addr, v);
    pulse1.write(addr, v);
    break;
  case 0x4004 ... 0x4007:
    fprintf(stderr, "pulse2: %.4x %.2x\n", addr, v);
    pulse1.write(addr, v);
    break;
  case 0x4008 ... 0x400b:
    fprintf(stderr, "triangle: %.4x %.2x\n", addr, v);
    triangle.write(addr, v);
    break;
  case 0x400c ... 0x400f:
    fprintf(stderr, "noise: %.4x %.2x\n", addr, v);
    noise.write(addr, v);
    break;
  case 0x4010 ... 0x4013:
    fprintf(stderr, "pcm: %.4x %.2x\n", addr, v);
    pcm.write(addr, v);
    break;
  case 0x4015:
    fprintf(stderr, "enable: %.2x[%d] [%c%c%c%c%c]\n",
	    v, soundPlaying,
	    v & D0 ? '1' : '-',
	    v & D1 ? '2' : '-',
	    v & D2 ? 'T' : '-',
	    v & D3 ? 'N' : '-',
	    v & D4 ? 'D' : '-');
    if (!v != soundPlaying) {
      soundPlaying = !v;
      SDL_PauseAudio(soundPlaying);
      fprintf(stderr, "SDL_PauseAudio: %x\n", soundPlaying);
    }
    pulse1.enabled = (v & D0);
    pulse2.enabled = (v & D1);
    triangle.enabled = (v & D2);
    noise.enabled = (v & D3);
    pcm.enabled = (v & D4);
    break;
  };
};

void apu_tick()
{
  static int ctr;
  float fp = 0;
  int p;
  int8_t s, amp=100, mod=128;

  return;
  if ((ctr++ % 2) == 0) {
    // APU rate
    pulse1.tick();
    pulse2.tick();

    /* Setup output */
    p = pulse1.output+pulse2.output;
    if (pulse1.enabled || pulse2.enabled) {
      apu_queue.push_back(sin(pulse1.phase));
      pulse1.phase += pulse1.phaseinc;
      flogger(0,"here...: %.2f %.2f\n", pulse1.phase, pulse1.phaseinc);
    }
  }
  // CPU rate 
  triangle.tick();

  if (apu_queue.size() >= 100) {
    SDL_QueueAudio(dev, apu_queue.data(), apu_queue.size() * sizeof(apu_queue[0]));
    apu_queue.clear();
  }
}

void apu_callback(void *userdata, uint8_t *stream, int len)
{
  int16_t *snd = reinterpret_cast<int16_t*>(stream);
  len /= sizeof(*snd);
  
  flogger(0, "callback: %d %d\n", len, apu_queue.size());
#if 0
  for (int i = 0; i < len; i++) {
    if ((pulse1.enabled && pulse1.output) || (pulse2.enabled && pulse2.output))
      snd[i] = noize.sample_sin();
    else
      snd[i] = 0;
  }
#endif
  if (len > apu_queue.size())
    len = apu_queue.size();
  SDL_memcpy(stream, apu_queue.data(), apu_queue.size() * sizeof(apu_queue[0]));
}

void apu_init()
{
  SDL_AudioSpec want, have;
  int rc;

  return;
  flogger(0, "APU init\n");
  SDL_zero(want);
  want.freq = sampleRate;
  want.format = AUDIO_S16LSB;
  want.channels = 1;
  want.samples = 512;
  //want.callback = apu_callback;
  dev=SDL_OpenAudioDevice(NULL, 0, &want, &have, SDL_AUDIO_ALLOW_FREQUENCY_CHANGE);
  flogger(0, "SDL_OpenAudio: %d format=%.4x/%.4x\n", rc, want.format, have.format);

  /* Pause until we turn on */
  SDL_PauseAudio(1);
}
#endif

/* Read from a cartridge */
uint8_t nescart::read(int addr)
{
#if 1
  uint8_t data;
  
  switch (addr) {
  case 0x0000 ... 0x1fff:
    return nesram[addr & 0x7ff];
  case 0x2000 ... 0x3fff:
    return ppu.readreg(addr & 0x2007);
  case 0x4000 ... 0x4013:
  case 0x4015:
    return apu_read(addr);
  case 0x4016 ... 0x4017:
    ctrlio(this, addr, 'r', data);
    return data;
  case 0x6000 ... 0x7fff:
    return prgram[addr & 0x1fff];
  case 0x8000 ... 0xffff:
    return prgRom[addr & romMask];
  }
  return 0xff;
#else
  uint8_t data = 0xFF;
  
  /* Check mapper read */
  if (mb.read(addr, data) == 0)
    return data;
  flogger(0, "unknown addr: %.4x\n", addr);
  return 0xFF;
#endif
};

/* Write to a cartridge */
void nescart::write(int addr, uint8_t data) {
#if 1
  switch (addr) {
  case 0x0000 ... 0x1fff:
    nesram[addr & 0x7ff] = data;
    break;
  case 0x2000 ... 0x3fff:
    ppu.writereg(addr & 0x2007, data);
    break;
  case 0x6000 ... 0x7fff:
    prgram[addr & 0x1fff] = data;
    break;
  }
#else
  if (mb.write(addr, data) == 0)
    return;
  flogger(0,"nescart write unknown addr: %x\n", addr);
#endif
};

void nescart::setvblank()
{
  // Set Vblank
  flogger(0,"=== Set VBlank (%x)\n", ppu.ppuctrl & ppu::PPUCTRL_NMI);
  ppu.ppustatus |= ppu::PPUSTATUS_V;
  if (ppu.ppuctrl & ppu::PPUCTRL_NMI) {
    flogger(0, "===================NMI!\n");
    cpu_nmi();
    _elapsed += 7;
  }
}

void nescart::clrvblank()
{
  flogger(0,"==== Clear VBLank\n");
  ppu.ppustatus &= ~(ppu::PPUSTATUS_V|ppu::PPUSTATUS_O|ppu::PPUSTATUS_S);
}

void setmask(Screen *s, int key, uint8_t &k, uint8_t m)
{
  if (s->KeyState[key]) {
    k |= m;
  }
  else {
    k &= ~m;
  }
}

static time_t fpstime=time(NULL);

void nescart::drawframe()
{
  int dodraw = 00;

  dodraw = 1;
  if (ppu.ppumask & ppu::PPUMASK_SHOWBG) {
    if (dwpat >= 0) {
      /* Draw all patterns */
      int cx = 2, cy = 2;
      scr->clear();
      for (int i = 0; i < 512; i++) {
	drawpat(cx, cy, i * 16, ppid);
	cx += 9;
	if (cx+9 >= scr->width || ((i + 1) % 256) == 0) {
	  cx = 2;
	  cy += 9;
	}
      }
      scr->scrtext(2, cy + 5, 0x31, "V:%d/%d PP:%d", dwpat, (chrRomSz / 4096), ppid);
      for (int i = 0; i < 4; i++) {
	scr->scrrect(100 + i*9, cy + 5, 8, 8, ppu.read(0x3f00 + ppid * 4 + i));
      }
    }
    else if (nnid) {
      // draw our nametable
#if 1
      drawnt(0, ppu.bgTbl, 0, 0);
      drawnt(1, ppu.bgTbl, 256, 0);
      drawnt(2, ppu.bgTbl, 0, 240);
      drawnt(3, ppu.bgTbl, 256, 240);
      scr->scrtext(10, 2, 0x31, "nnid: %d", nnid);
#endif
      dumpnt(0);
      dumpnt(1);
      dumpnt(2);
      dumpnt(3);
    }
    dodraw = 1;
  }
  if (ppu.ppumask & ppu::PPUMASK_SHOWSPRITE) {
#if 0
    int i;
    static int sps;
    
    // index: tttttttn
    //   t = tile # 0..127
    //   n = bank (0x0000 or 0x1000)
    // attr : vhp---PP
    //   v = flip vert
    //   h = flip horz
    //   p = priority
    //  PP = Sprite Palette [4-7]
    for (i=0; i<64; i++) {
      int sbase, spid, tile, hm, vm;
      if (ppu.o_sprites[i].y >= 240) {
	continue;
      }
      flogger(0, "Sprite %d: (%d,%d) %.2x %.2x\n",
	      i, ppu.o_sprites[i].x,ppu.o_sprites[i].y,
	      ppu.o_sprites[i].index,
	      ppu.o_sprites[i].attr);
      
      /* Draw Sprite */
      spid  = 4 + (ppu.o_sprites[i].attr & 0x3);
      tile = ppu.o_sprites[i].index;
      
      /* Get horizontal/Vertial flip */
      hm = ppu.o_sprites[i].attr & ATTR_H ? 7 : 0;
      vm = ppu.o_sprites[i].attr & ATTR_V ? 7 : 0;
      
      /* 8x8 use PPUCTRL, 8x16 use bit 1 */
      if (ppu.spriteSz == 16) {
	sbase = (tile & 1) ? 0x1000 : 0x0000;
	if (ppu.o_sprites[i].attr & ATTR_V) {
	  drawpat(ppu.o_sprites[i].x, ppu.o_sprites[i].y,   sbase + (tile | 0x01) * 16, spid, hm, vm, i, sps);
	  drawpat(ppu.o_sprites[i].x, ppu.o_sprites[i].y+8, sbase + (tile & 0xFE) * 16, spid, hm, vm, i, sps);
	}
	else {
	  drawpat(ppu.o_sprites[i].x, ppu.o_sprites[i].y,   sbase + (tile & 0xFE) * 16, spid, hm, vm, i, sps);
	  drawpat(ppu.o_sprites[i].x, ppu.o_sprites[i].y+8, sbase + (tile | 0x01) * 16, spid, hm, vm, i, sps);
	}
      }
      else {
	sbase = ppu.spriteTbl;
	drawpat(ppu.o_sprites[i].x, ppu.o_sprites[i].y, sbase + tile * 16, spid, hm, vm, i, sps);
      }
    }
#endif
    dodraw = 1;
  }
  if (dodraw) {
    time_t now = time(NULL);
    float fps = (float)frame / (now - fpstime);
    scr->scrtext(0, scr->height+1, 0x31, "frame:%d fps:%.2f", frame, fps);
    scr->scrtext(0, scr->height+10, 0x31, "v:%.4x b:%x s:%x", ppu.vramaddr, !!(ppu.ppuctrl & ppu::PPUCTRL_BGTBL), !!(ppu.ppuctrl & ppu::PPUCTRL_SPRITETBL));
    scr->draw();
  }
  // draw CHR patterns
  if (scr->KeyState['v']) {
    dwpat = (dwpat + 2);
    if (dwpat >= (chrRomSz / 4096))
      dwpat=-2;
  }
  // Increment palette id (in chr pattern)
  if (scr->KeyState['b']) {
    ppid = (ppid+1) % 8;
  }
  // Increment nametable displayed
  if (scr->KeyState['n']) {
    nnid = (nnid+1) % 3;
  }
  // dump stack
  if (scr->KeyState['p']) {
    //stk->showstk(128);
  }
  if (scr->KeyState['y']) {
    sreg ^= 1;
  }

  controller[0] = 0;
  setmask(scr, 'x', controller[0], D0);                   //A
  setmask(scr, 'z', controller[0], D1);                   //B
  setmask(scr, 'a', controller[0], D2);                   //SELECT
  setmask(scr, 's', controller[0], D3);                   //START
  setmask(scr, Key::K_UP, controller[0], D4);             //U
  setmask(scr, Key::K_DOWN, controller[0], D5);           //D
  setmask(scr, Key::K_LEFT, controller[0], D6);           //L
  setmask(scr, Key::K_RIGHT, controller[0], D7);          //R
}

int bg_enabled(int x)
{
  if (!(ppu.ppumask & ppu::PPUMASK_SHOWBG))
    return 0;
  return (x >= 8 || (ppu.ppumask & ppu::PPUMASK_LEFTBG));
}

int sprite_enabled(int x)
{
  if (!(ppu.ppumask & ppu::PPUMASK_SHOWSPRITE))
    return 0;
  if (x >= 8)
    return 1;
  if (ppu.ppumask & ppu::PPUMASK_LEFTSPRITE)
    return 1;
  return 0;
}

void nescart::drawpixel()
{
  int bgclr, spclr, pclr, pri;

  if (hPos == 321) {
    ppu.bk.clear();
  }
  if ((vPos < 340) && ((hPos >= 2 && hPos <= 255) || (hPos >= 321 && hPos <= 337))) {
    /* Default to background color */
    pclr = ppu.read(0x3F00);
    spclr = pri = 0;

    ppu.bk.enabled = (ppu.ppumask & ppu::PPUMASK_SHOWBG);
    bgclr = ppu.bk.getpixel();
    /* Check if we're not showing left pixels or being displayed */
    if (!bg_enabled(hPos-2))
      bgclr = 0;
    if (hPos >= 2 && hPos <= 257) {
      // loop through all sprites, claim first
      for (int i = 0; i < nSprite; i++) {
	pri = ppu.spr[i].attr & ATTR_P;
	ppu.spr[i].enabled = 1;
	flogger(1, "en:%d del:%d l:%.4x m:%.4x\n", ppu.spr[i].enabled, ppu.spr[i].delay, ppu.spr[i].patLsb, ppu.spr[i].patMsb); 
	if (spclr == 0) {
	  spclr = ppu.spr[i].getpixel();
	  if (!sprite_enabled(hPos-2))
	    spclr = 0;
	  if (i == 0 && spclr && bgclr) {
	    flogger(1, "sprite 0 hit\n");
	    ppu.ppustatus |= ppu::PPUSTATUS_S;
	  }
	  if (spclr)
	    spclr = spclr; //(0x3f10 + (rand() % 15));
	} else {
	  // still get ppixel to advance decrement regs
	  ppu.spr[i].getpixel();
	}
      }
    }
    /* Colors:  PR=sprite priority
     *   BG  SP  PR
     *    0   0   X  Default BG    [3f00]
     *    0 1-3   X  Sprite
     *  1-3 1-3   0  Sprite
     *  1-3   0   X  BG
     *  1-3 1-3   1  BG
     */
    if ((ppu.ppumask & ppu::PPUMASK_SHOWBG) == 0)
      ;
    else if ((!bgclr || !pri) && spclr)
      pclr = ppu.read(spclr);
    else if (bgclr)
      pclr = ppu.read(bgclr);
    if (vPos <= 239 && hPos <= 256) {
      printf("setpixel: %d %d %d\n", hPos, vPos, pclr);
      scr->setpixel(hPos-2, vPos, pclr);
    }
  }
}

/* Evaluated for 0..239 and 261 */
void nescart::evalbg()
{
  if ((ppu.ppumask & ppu::PPUMASK_SHOWBG) == 0)
    return;
  printf("evalbg: %d %d\n", vPos, hPos);
  if ((hPos >= 1 && hPos <= 256) || (hPos >= 321 && hPos <= 336)) { 
    drawpixel();
    ppu.fetch(hPos);
  }
  if (hPos == 256)
    ppu.inc_vert();
  else if (hPos == 257) {
    ppu.copyhorz();
  }
  else if (vPos == SCANLINE_PRE && (hPos >= 280 && hPos <= 304)) 
    ppu.copyvert();
}

/* Evaluated for lines 0..239 and 261 */
void nescart::evalsprite()
{
  if ((ppu.ppumask & ppu::PPUMASK_SHOWSPRITE) == 0)
    return;
  if (vPos <= 239) {
    if (hPos >= 1 && hPos <= 64) {
      // secondary oam clear
    }
    else if (hPos >= 65 && hPos <= 256) {
      // evaluate sprites for next line
    }
  }
  if (hPos==257) {
    nSprite = ppu.loadsprites(vPos);
    if (nSprite > 0) {
      flogger(1, "line: %d sprites = %d\n", vPos, nSprite);
    }
  };
}

enum {
      NTBYTE = 0x01,
      ATBYTE = 0x03,
      PATLSB = 0x05,
      PATMSB = 0x07,
      SPRAT  = 0x13,
      SPRLSB = 0x15,
      SPRMSB = 0x17,
};

// 0.. 239      : VISIBLE
//              : 0: Skip or IDLE
//              : 1..255 [nt,at,lsb,msb,inch]
//              : 256    [msb,incv]
//              : 257    [copyh]
//              : 258...320 idle
//              : 257 .. 320 SPRITE
//              : 321 .. 338 [nt,at,lsb,msb,inch]
//              : 339 .. 338 [nt]
// 240          : n/a
// 241          : Set VBlank
// 242 .. 260   : n/a
// 261
struct fetch {
  int     addr;
  uint8_t nt;
  uint8_t at;
  uint8_t lsb;
  uint8_t msb;
};

void dofetch(int clk, fetch& f)
{
  int addr, v, fy, qb, what;

  what = (clk % 8);
  v = ppu.vramaddr;
  fy = (v & ppu::FYMASK) >> 12;
  if (clk >= 257 && clk < 321) {
    what += 0x10;
    qb = (clk - 257) / 8;
  }
  switch (what) {
  case NTBYTE:
    addr = 0x2000 + (v & 0xFFF);
    f.nt = ppu.read(addr) * 16;
    break;
  case ATBYTE:
    addr = 0x23C0 + (v & ppu::VHTAB) + ((v & ppu::ACYMASK) >> 4) + ((v & ppu::ACYMASK) >> 2);
    qb   = ((v >> 4) & 0x4) + (v & 0x02);
    f.at = (ppu.read(addr) >> qb) & 3;
    break;
  case PATLSB:
    addr = ppu.bgTbl + f.nt + fy;
    f.lsb = ppu.read(addr);
    break;
  case PATMSB:
    addr = ppu.bgTbl + f.nt + fy + 8;
    f.msb = ppu.read(addr);
    break;
  case SPRLSB:
    break;
  case SPRMSB:
    break;
  }
}
  
void nescart::gr_tick()
{
  switch (vPos) {
  case 0 ... 239:
    /* Eval sprites and background */
    evalsprite();
    evalbg();
    if (hPos == 260 && ppu.rendering() && mapper)
      mapper->scanline();
    break;
  case SCANLINE_VBLANK: /* 241 */
    // Set VBlank/NMI
    if (hPos == 1) {
      setvblank();
    }
    break;
  case SCANLINE_PRE:    /* 261 */
    // Clear Vblank/Sprite0/Sprite Overflow
    if (hPos == 1)
      clrvblank();
    evalsprite();
    evalbg();
    break;
  }
  if (hPos++ == 340) {
    hPos = 0;
    if (vPos++ == SCANLINE_PRE) {
      drawframe();
      apu_run_frame();
      _elapsed = 0;
      vPos = 0;
      if ((++frame & 1) && ppu.rendering()) {
	hPos++;
      }
    }
  }
}

/* 341 cycles x 262 scanlines = ppu.ticks 89342 (89341.5)
 * 341/3 = cpu.ticks 113.66667 per scanline
 * 89341.5/3 = cpu.ticks 29780.5 per frame
 * hblank = [256+85 pixels=341] cpu.ticks 28.3333
 * nmi to start of render: cpu ticks 2273.3333
 * oam dma: cpu.ticks 513 (+1 on odd-numbered cycle) [must be within cpu.ticks 2131 after nmi]
 */
void nescart::run()
{
  rdy = 1;
  cpu_reset(0);
  apu_init();
  for(;;) {
    /* Frame: odd frames are 1 clock shorter [skip] 
     * BGfetch
     *      mod8 1 : fetch NT byte [scanline=N,X+8..15] tile ID
     *      mod8 3 : fetch AT byte [scanline=N.X+8..15] tile Attib/Quadrant
     *      mod8 5 : fetch BG LSB  [scanline=N,X+8..15]
     *      mod8 7 : fetch BG MSB  [scanline=N,X+8..15]
     *      mod8 0 : inchorz(v)
     * SpriteFetch
     *      mod8 1 : garbage NT fetch
     *      mod8 3 : garbage NT fetch
     *      mod8 5 : fetch Sprite LSB [scanline=N+1]
     *      mod8 7 : fetch Sprite MSB [scanline=N+1]
     * Scanlines 0..239:
     *  1   .. 64  : Secondary OAM clear
     *  65 .. 256  : Sprite Eval  [scanline=N+1]
     *  257 .. 320 : Sprite Fetch [scanline=N+1]
     *  1   .. 256 : BGfetch      [scanline=N,X+8..15] render && fetch
     *  256        : incvert(v)
     *  257        : horiz(v) = horiz(t)
     *  258 .. 320 : idle
     *  321 .. 336 : BGfetch      [scanline=N+1,X=0..15] render && fetch
     *  337, 339   : dummy NT read  
     * Scanline 241
     *   1         :  Set VBlank
     * Scanline 261
     *  1          : Clear VBlank
     *  257 .. 320 : Sprite Fetch [scanline=0]
     *  1 .. 256   : BGfetch [unused] render && fetch
     *  256        : incvert(v)
     *  257        : horiz(v) = horiz(t)
     *  280 .. 305 : vert(v) = vert(to)
     *  321 .. 336 : BGfetch [scanline=0,X=0..15]  render && fetch
     *    337, 339 : dummy NT read  
     * 
     * Sprite 0 hit where h=2
     * First pixel output at h=4
    */
    if (dma_count > 0) {
      ppu.oam[ppu.oamaddr++] = read(dma_addr++);
      dma_count--;
    }
    uint32_t cc = clockticks6502;
    cpu_tick(1);
    _elapsed += (clockticks6502 - cc);
    gr_tick();
    gr_tick();
    gr_tick();
  }
}

void getextra(char *s, int n, int addr)
{
  int l = strlen(s);

  switch (addr) {
  case ppu::PPUDATA:
    snprintf(s+l, n-l, " : ppuaddr=%.4x", ppu.vramaddr);
    break;
  }
}
  

