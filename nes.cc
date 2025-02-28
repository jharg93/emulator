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

int _elapsed;
int vk = 9;
int ncycles;

extern int cpu_step();
extern int clks, scanline, frame;
extern int trace;
extern void cpu_nmi(void);
extern void cpu_reset(uint32_t addr=0);
extern int  cpu_tick(int);

uint32_t clockticks6502;

int rdy = 1;
uint8_t cartRam[0x2000];

Screen *s;
static cart *cart;

void apu_init();
void apu_tick();
void apu_write(int addr, uint8_t v);
uint8_t apu_read(int addr) {
  return 0;
};
void apu_run_frame() {
};
const char *comment(int addr) {
  return "";
}

#define SCANLINE_VBLANK 241
#define SCANLINE_PRE    261

int dma_addr = 0;
int dma_count = 0;

extern uint16_t OPC;
extern uint8_t a,x,y,status;

extern int rdy;

int frame, scanline, clks, curbank;

/* mapping of nametables */
#define MIRROR_4SCR  0x0123
#define MIRROR_HORZ  0x0022
#define MIRROR_VERT  0x0101
#define MIRROR_LEFT  0x0000
#define MIRROR_RIGHT 0x1111

int vismask;

void flogger(int lvl, const char *fmt, ...) {
  va_list ap;

  return;
  if (lvl > 0)
    return;
  if (lvl == -3) {
    return;
    fprintf(stderr, "[%s] frame:%4d scanline:%4d clks:%4d ", vismask ? "vis" : "---", frame, scanline, clks);
  }
  va_start(ap, fmt);
  if (lvl == 0) {
    fprintf(stderr,"%.2x.%.4X: [%7d] A=%.2X X=%.2X Y=%.2X [%c%c%c%c%c%c%c%c] ", curbank,OPC, _elapsed, a,x,y,
	    status & 0x80 ? 'N' : ' ',
	    status & 0x40 ? 'V' : ' ',
	    status & 0x20 ? 'K' : ' ',
	    status & 0x10 ? 'B' : ' ',
	    status & 0x08 ? 'D' : ' ',
	    status & 0x04 ? 'I' : ' ',
	    status & 0x02 ? 'C' : ' ',
	    status & 0x01 ? 'Z' : ' ');
    fprintf(stderr, "[%s] frame:%4d scanline:%4d clks:%4d ", vismask ? "vis" : "---", frame, scanline, clks);
  }
  vfprintf(stderr, fmt, ap);
  va_end(ap);
}

/* Mapper code: read/write nbanks */
#if 0
mapper_t::mapper_t(uint8_t *mem, int nbank, int sz, int start, int end, const char *name) {
  flogger(0,"Setting up %d %dk banks [%s] \n", nbank, sz / 1024, name);
  nBank     = nbank;
  bankStart = start;
  bankEnd   = end;
  bankSz    = sz;
  cartmem   = mem;

  /* Always set to last bank */
  setbank(nbank-1);
};
#endif

uint8_t *mapper_t::setbank(int pg) {
  if (pg < 0)
    pg += nBank;
  if (curpg != pg) {
    flogger(0,"======= setbank(%d) %x\n", pg, pg * bankSz);
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

void ppuSetMirror(int);

struct slice {
  int      nSlice;
  int      sliceSz;
  int      maxBank;
};

/* Return CHR and PRG addresses based on bank 
 * A16|A15 A14 A13 A12|A11 A10 A09 A08|A07 A06 A05 A04|A03 A02 A01 A00 */

/* Mapper 0/Generic : NROM */
struct nesMapper {
  int prgSz, nPrgSlice;
  int chrSz, nChrSlice;
  uint8_t *prg;
  uint8_t *chr;

  nesMapper(int csl, int csz, uint8_t *c, int psl, int psz, uint8_t *p) :
    prgSz(psz), nPrgSlice(psl),
    chrSz(csz), nChrSlice(csl),
    prg(p), chr(c)
  {
  };
  
  /* Set bank to new value */
  void setb(int& br, int nb, const char *name) {
    if (nb != br) {
      flogger(0, "[%s]: setbank %d\n", name, nb);
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
    if (addr >= prgSz) {
      fprintf(stderr, "toobig\n");
      exit(0);
    }
    return prg[addr];
  };
  /* Overridables */
  virtual int write(int addr, uint8_t data) {
    return 0;
  };
  virtual uint8_t &prgaddr(int addr) {
    return prg[addr % prgSz];
  };
  virtual uint8_t &chraddr(int addr) {
    return chr[addr];
  };
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
    flogger(0, "Mapper4 %d PRG, %d CHR\n", psz / 8192, csz / 1024);
  };
  enum {
    PMODE = 0x40,
    CMODE = 0x80
  };
  uint8_t R[8],reg8000;
  int PB[4] = { 0,0,0,-1 };
  int CB[8];
  
  int write(int addr, uint8_t data) {
    int x;

    switch (addr & 0xF001) {
    case 0x8000:
      reg8000 = data;
      break;
    case 0x8001:
      R[reg8000 & 7] = data;
      break;
    case 0xA000:
      flogger(0, "004:Mirroring: %x\n", data);
      ppuSetMirror(data & 1 ? MIRROR_HORZ : MIRROR_VERT);
      break;
    case 0xA001:
      flogger(0, "004:prg ram: %x\n", data);
      break;
    case 0xc000:
      flogger(0, "004:irq latch: %x\n", data);
      break;
    case 0xc001:
      flogger(0, "004:irq reload: %x\n", data);
      break;
    }
    /* Set PRG map */
    x = (reg8000 & PMODE) ? 0x2 : 0x0;
    setb(PB[x],   R[6] & 0x1f, "004.prg0");
    setb(PB[1],   R[7] & 0x1f, "004.prg1");
    setb(PB[x^2], -2,   "004.prg2");
    setb(PB[3],   -1,   "004.prg3");

    /* Set CHR Map */
    x = (reg8000 & CMODE) ? 0x4 : 0x0;
    setb(CB[x]  , R[0] & 0xFE, "004.chr0");
    setb(CB[x^1], R[0] | 0x01, "004.chr1");
    setb(CB[x^2], R[1] & 0xFE, "004.chr2");
    setb(CB[x^3], R[1] | 0x01, "004.chr3");
    setb(CB[x^4], R[2], "004.chr4");
    setb(CB[x^5], R[3], "004.chr5");
    setb(CB[x^6], R[4], "004.chr6");
    setb(CB[x^7], R[5], "004.chr7");
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
};

nesMapper *nmap;

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
    fprintf(stderr, "scanline:%4d clks:%4d delay:%d pat:%.2x %.2x palid:%x\n",
	    scanline, clks, delay, patLsb, patMsb, palid);
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

struct ppu {
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
    
    CXMASK = (0x1F << 0),
    CYMASK = (0x1F << 5),
    VHMASK = (0x03 << 10),
    FYMASK = (0x07 << 12),
    
    CX31   = 0x1F,
    CY29   = (29 << 5),
    CY31   = (31 << 5),

    MAX_SPRITE = 64,
    SPRITE_OVERFLOW = 8,
  };
  mapper_t *mapper;

  /* Background/Sprite chr table */
  int       chwr;
  uint8_t  *chrRom;
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

  void copyhorz() {
    // ----.-h--.---X.XXXX
    if (ppumask & (PPUMASK_SHOWBG|PPUMASK_SHOWSPRITE)) {
      vramaddr = (vramaddr & ~0x41F) | (tramaddr & 0x41F);
    }
  }
  void copyvert() {
    // -yyy.v-yy.yyy-.----
    if (ppumask & (PPUMASK_SHOWBG|PPUMASK_SHOWSPRITE)) {
      vramaddr = (vramaddr & ~0x7BE0) | (tramaddr & 0x7BE0);
    }
  }
  void inc_hori() {
    if ((vramaddr & CXMASK) == 31) {
      vramaddr ^= (HTAB|CXMASK);
    }
    else {
      vramaddr++;
    }
  };
  void inc_vert() {
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
    if (mirror != MIRROR_4SCR) {
      mirror = mode;
      nt[UL] = &nametable[((mode >> 12) & 0xF) * 0x400];
      nt[UR] = &nametable[((mode >> 8)  & 0xF) * 0x400];
      nt[LL] = &nametable[((mode >> 4)  & 0xF) * 0x400];
      nt[LR] = &nametable[((mode >> 0)  & 0xF) * 0x400];
    };
  };

  /* Load any sprites on this scanline */
  int loadsprites(int y) {
    uint8_t l,m;
    int count = 0;
    int dy, h = spriteSz;
    int tile, base;
    
    flogger(1, "------------------------ LoadSprites\n");
    for (int i = 0; i < MAX_SPRITE; i++) {
      /* Ignore off-screen sprites */
      if (o_sprites[i].y >= 240)
	continue;
      dy = (y + 1) - o_sprites[i].y;
      if (dy >= 0 && dy < h) {
	flogger(1, "LoadSprite %d: (%d,%d) 8x%d %c%c pri:%d pal:%x\n",
		i, o_sprites[i].x, o_sprites[i].y,spriteSz,
		o_sprites[i].attr & ATTR_V ? 'v' : '-',
		o_sprites[i].attr & ATTR_H ? 'h' : '-',
		(o_sprites[i].attr & ATTR_P) > 0,
		o_sprites[i].attr & 3);
	if (count >= SPRITE_OVERFLOW) {
	  flogger(1, "sprite overflow\n");
	  ppustatus |= PPUSTATUS_O;
	  return SPRITE_OVERFLOW;
	}
	/* Vert mirror */
	if (o_sprites[i].attr & ATTR_V)
	  dy ^= 7;
	if (spriteSz == 16) {
	  base = (o_sprites[i].index & 1) << 12; 
	  tile = o_sprites[i].index & ~1;
	}
	else {
	  base = spriteTbl;
	  tile = o_sprites[i].index;
	}
	/* Load sprite tiles data */
	l = read(base + tile * 16 + dy);
	m = read(base + tile * 16 + dy + 8);
	spr[count].setBits(8, l, m, 0);
	spr[count].attr    = o_sprites[i].attr;
	spr[count].palid   = 0x3f10 + 4*(o_sprites[i].attr & 3);
	spr[count].enabled = ppumask & PPUMASK_SHOWSPRITE;
	spr[count].delay   = o_sprites[i].x;
	if ((o_sprites[i].attr & ATTR_H) == 0)
	  spr[count].delay -= 8;
	s_sprites[count++] = o_sprites[i];
      }
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
      ntbyte = read(addr);
      break;
    case 0x3: // atbyte
      addr = 0x23C0 + (vramaddr & 0x0C00) + ((vramaddr >> 4) & 0x38) + ((vramaddr >> 2) & 0x07);
      qb = ((vramaddr >> 4) & 0x4) + (vramaddr & 0x2);
      atbyte = (read(addr) >> qb) & 3;
      break;
    case 0x5: // patlsb
      addr = bgTbl + ntbyte*16 + fineY;
      patLsb = read(addr);
      break;
    case 0x7: // patmsb
      addr = bgTbl + ntbyte*16 + fineY + 8;
      patMsb = read(addr);
      break;
    case 0x0:
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
  if (addr == 0x10 || addr == 0x14 || addr == 0x18 || addr == 0x0c)
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
  flogger(2, "read ppu: %.4x\n", addr);
  switch (addr) {
  case 0x0000 ... 0x1FFF:
    // read tile rom
    return nmap->chraddr(addr);
  case 0x2000 ... 0x3EFF:
    // read nametable
    return nt[(addr >> 10) & 3][addr & 0x3FF];
  case 0x3F00 ... 0x3FFF:
    return palette[paladdr(addr)];
  default:
    flogger(0, "Unknown ppu address: %x\n", addr);
    break;
  }
  return 0xff;
};

/* Write to ppu memory */
void ppu::write(int addr, uint8_t data) {
  last = data & 0x1F;

  flogger(2, "write ppu: %.4x = %.2x\n", addr, data);
  switch (addr) {
  case 0x0000 ... 0x1FFF:
    // chr-rom
    if (chwr) {
      chrRom[addr & 0x1FFF] = data;
    }
    break;
  case 0x2000 ... 0x3EFF:
    // write to nametables ----.vh--.----.----
    nt[(addr >> 10) & 3][addr & 0x3FF] = data;
    break;
  case 0x3F00 ... 0x3FFF:
    // write palette
    palette[paladdr(addr)] = data;
    break;
  default:
    flogger(0, "Unknown ppu address: %x\n", addr);
    break;
  }
};

/* Write PPU register */
uint8_t ppu::readreg(int addr) {
  uint8_t data = 0xFF;
  
  flogger(1, " ppu.readreg(%x:%s)\n", addr, ppureg(addr));
  switch (addr) {
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
    flogger(0, "Unknown ppu register:%x\n", addr);
    break;
  }
  return data;
};

/* Read PPU Register */
#define yn(x) (data & (x)) ? "yes" : "no"

void ppu::writereg(int addr, uint8_t data) {
  flogger(1, " ppu.writereg(%.4x:%s,%.2x)\n", addr, ppureg(addr),data);
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
    flogger(0, " PPUCTRL::BGTBL      %.4x\n", bgTbl);
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
    flogger(1," PPUMASK:ShowBG      : %s\n", yn(PPUMASK_SHOWBG));
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
      flogger(0, "setscroll: cx:%d fx:%d\n", (data >> 3), data & 7);
    }
    else {
      // write upper half of scroll register (Y)
      // data:               ABCD.Efgh
      // tramaddr: -fgh.--AB.CDE-.----
      tramaddr = (tramaddr & ~0x73E0) | ((data << 12) & FYMASK) | ((data << 2) & CYMASK);
      flogger(0, "setscroll: cy:%d fy:%d\n", (data >> 3), data & 7);
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
    }
    flogger(0, "setaddr: %.4x\n", vramaddr);
    latch ^= 1;
    break;
  case PPUDATA:
    /* Write data & increment address */
    write(vramaddr, data);
    vramaddr += vramIncr;
    break;
  default:
    flogger(0, "Unknown ppu register:%x\n", addr);
    break;
  }
};

struct ppu ppu;

void ppuSetMirror(int m) {
  ppu.setmirror(m);
}

void dumpcfg(uint8_t *buf, int sz);

nescart::nescart(const char *file) : cart(file) {
  int offset = sizeof(*hdr);
  int mappertype;
  
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
  }
  else {
    ppu.chwr = 0;
    chrRom = &data[offset + prgRomSz];
  }
  //dumpcfg(prgRom, prgRomSz);

  mappertype = (hdr->mapper1 >> 4) | (hdr->mapper2 & 0xF0);
  flogger(0, "NES Header\n");
  flogger(0, " PRG RAM size: %d\n", prgRamSz);
  flogger(0, " PRG ROM size: %d @ %x\n", prgRomSz, offset);
  flogger(0, " CHR ROM size: %d @ %x\n", chrRomSz, offset + prgRomSz);
  flogger(0, " mapper: %.2x [%.2x %.2x]\n", mappertype, hdr->mapper1, hdr->mapper2);

  switch (mappertype) {
  case 004:
    nmap = new nesMapper004(chrRomSz, chrRom, prgRomSz, prgRom);
    break;
  case 002:
    nmap = new nesMapper002(chrRomSz, chrRom, prgRomSz, prgRom);
    break;
  case 001:
    nmap = new nesMapper001(chrRomSz, chrRom, prgRomSz, prgRom);
    break;
  default:
    nmap = new nesMapper(1, chrRomSz, chrRom, 1, prgRomSz, prgRom);
    break;
  }
  nmap->chr = chrRom;
  nmap->prg = prgRom;
  
  ppu.chrRom = chrRom;
  ppu.mapper = mapper;

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
  scr = new Screen(256, 240, 10, 30, 64, nespalette);
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
  for (i = 0; i < 8; i++) {
    if (dwpat >= 0) {
      patlsb = chrRom[(dwpat-1)*4096 + base + i];
      patmsb = chrRom[(dwpat-1)*4096 + base + i + 8];
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

int c_index;
int c_strobe;
int c_button;

/* Read from a cartridge */
uint8_t nescart::read(int addr)
{
  uint8_t data = 0xFF;
  
  if (mapper && mapper->read(addr, data)) {
    return data;
  }
  switch (addr) {
  case 0x0000 ... 0x1FFF:
    // 2k RAM, mirrored
    return nesram[addr & 0x7FF];
  case 0x2000 ... 0x3FFF:
    // PPU register
    return ppu.readreg(addr & 0x2007);
  case 0x6000 ... 0x7fff:
  case 0x8000 ... 0xFFFF:
    // Cartridge ROM
    return nmap->prgaddr(addr);
  case ppu::CTRLR1:
  case ppu::CTRLR2:
    flogger(1, "Get input %x %x %x %x\n", addr, strobe[addr %1], controller_state[addr % 1], controller[addr % 1]);
    strobe[addr %1]++;
    data = !!(controller_state[addr % 1] & 0x1);
    controller_state[addr % 1] >>= 1;
    return data | 0x40;
  case 0x4000 ... 0x4013: case 0x4015:
    return apu_read(addr);
  default:
    flogger(0,"Unknown cart addr: %x\n",addr);
    return 0xFF;
  }
  return 0xFF;
};

#if 0
/* APU:                                    silence

 *   4000: pulse1.control     DDLC.VVVV    30
 *   4001: pulse1.sweep       EPPP.NSSS    08
 *   4002: pulse1.timerlo     TTTT.TTTT    00
 *   4003: pulse1.timerhi     LLLL.LTTT    00
 *      11-bit counter (TTT.TTTTTTTT), tick every *APU* cycle
 *      L=length counter halt (Reload?)
 *      C=constant volume
 *      VVVV=0000 silence, VVVV=1111=maximum
 *      DD=00 0100.0000 12.5%
 *      DD=01 0110.0000 25%
 *      DD=10 0111.1000 50%
 *      DD=11 1001.1111 75% (-25%)
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

noisy noize;
int nozetype;

struct nesaudio {
  uint8_t  reg[4];
  const char *name;
  
  uint8_t  pattern;
  int      counter;
  int      reload  = -1;

  int      volume  = 0;
  int      enabled = 0;
  int      output  = 0;

  double freq, period;
  double phase, phaseinc;

  nesaudio() {
    freq = 440.0;
    phaseinc = freq * Tau / sampleRate;
  };
  virtual void tick() {
  };
  virtual void write(int addr, uint8_t nv) {
    reg[addr & 3] = nv;
  };
};

SDL_AudioDeviceID dev;

const int min_samples = 8192;
std::vector<int16_t> apu_queue;

struct Triangle : public nesaudio
{
  Triangle(const char *n) {
    name = n;
  };
  void write(int addr, uint8_t nv) {
    switch (addr & 3) {
    case 0x02:
      // 400a: pulse1.timerlo     TTTT.TTTT
      reload = (reload & 0xFF00) | nv;
      break;
    case 0x03:
      // 400b: pulse1.timerhi     LLLL.LTTT    00
      reload = (reload & 0x00FF) | ((nv & 7) << 8);
      counter = reload;
      fprintf(stderr, "%s: reloaded timer %d\n", name, reload);
      break;
    };
  }
  virtual void tick() {
    if (enabled && counter-- == 0x0) {
      counter = reload+1;
      output  = (pattern & 1) * volume;
      pattern = ((pattern >> 1) | (pattern << 7));
    }
  };
};

struct Pulse : public nesaudio {
  Pulse(const char *n) {
    name = n;
  };
  void write(int addr, uint8_t nv) {
    switch (addr & 3) {
    case 0x00:
      // 4000/4004: pulse1.control     DDLC.VVVV
      switch (nv & 0xC0) {
      case 0x00: pattern = 0b01000000; break;
      case 0x40: pattern = 0b01100000; break;
      case 0x80: pattern = 0b01111000; break;
      case 0xC0: pattern = 0b10011111; break;
      }
      volume = reg[0] & 0x0F;
      fprintf(stderr, "%s: reloaded volume/pattern %d/%.2x\n", name, volume, pattern);
      break;
    case 0x02:
      // 4002: pulse1.timerlo     TTTT.TTTT
      // 4006: pulse2.timerlo     TTTT.TTTT
      reload = (reload & 0xFF00) | nv;
      break;
    case 0x03:
      // 4003: pulse1.timerhi     LLLL.LTTT    00
      // 4007: pulse2.timerhi     LLLL.LTTT    00
      reload = (reload & 0x00FF) | ((nv & 7) << 8);
      counter = reload;
      fprintf(stderr, "%s: reloaded timer %d\n", name, reload);
      break;
    }
  };
  void tick() {
    if (enabled && counter-- == 0x0) {
      counter = reload+1;
      output  = (pattern & 1) * volume;
      pattern = ((pattern >> 1) | (pattern << 7));
    }
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
  noize.vol = 2000;
  noize.f2t(440.0);
  
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
void apu_init() {
}
void apu_write(int addr, uint8_t nv) {
}

/* Write to a cartridge */
void nescart::write(int addr, uint8_t data) {
  if (mapper && mapper->write(addr, data))
    return;
  switch (addr) {
  case 0x0000 ... 0x1FFF:
    // 2k RAM, mirrored
    flogger(1, "write %x = %x\n", addr, data);
    nesram[addr & 0x7FF] = data;
    break;
  case 0x2000 ... 0x3FFF:
    // PPU register
    ppu.writereg(addr & 0x2007, data);
    break;
  case ppu::OAMDMA:
    // PPU DMA
    flogger(1, "DMA from %.4x/%x\n", data * 256, ppu.oamaddr);
#if 0
    dma_addr = data * 256;
    dma_count = 256;
#else
    for (int i = 0; i < 256; i++)
      ppu.oam[ppu.oamaddr++] = read(data * 256 + i);
    for(int i = 0; i < 64; i++) {
      flogger(1, "DMASprite: %d (%d,%d)\n", i, ppu.o_sprites[i].x, ppu.o_sprites[i].y);
    }
#endif
    clockticks6502 += (clockticks6502 & 1) ? 514 : 513;
    break;
  case ppu::CTRLR1:
  case ppu::CTRLR2:
    if (!(data & 1)) {
      controller_state[addr % 1] = controller[addr % 1];
      controller[addr % 1] = 0;
      strobe[addr % 1] = 0;
    }
    break;
  case 0x4000 ... 0x4013: case 0x4015:
    apu_write(addr, data);
    break;
  case 0x8000 ... 0xFFFF:
    nmap->write(addr, data);
    break;
  default:
    flogger(0,"nescart write unknown addr: %x\n", addr);
    break;
  }
};

void nescart::gr_tick()
{
  /* Extra tick on odd frame start */
  if ((frame & 1) && !clks && !scanline)
    clks++;

  switch (scanline) {
  case 0 ... 239:
    /* Eval sprites and background */
    evalsprite();
    evalbg();
    break;
  case SCANLINE_VBLANK:
    // Set VBlank/NMI
    if (clks == 1)
      set_vblank();
    break;
  case SCANLINE_PRE:
    // Clear Vblank/Sprite0/Sprite Overflow
    if (clks == 1)
      clr_vblank();
    evalsprite();
    evalbg();
    break;
  }
  if (clks++ == 340) {
    clks = 0;
    if (scanline++ == SCANLINE_PRE) {
      drawframe();
      apu_run_frame();
      _elapsed = 0;
      scanline = 0;
      frame++;
    }
  }
}

void nescart::set_vblank()
{
  // Set Vblank
  flogger(0,"=== Set VBlank (%x)\n", ppu.ppuctrl & ppu::PPUCTRL_NMI);
  ppu.ppustatus |= ppu::PPUSTATUS_V;
  if (ppu.ppuctrl & ppu::PPUCTRL_NMI) {
    flogger(1, "===================NMI!\n");
    clockticks6502 += 9;
    cpu_nmi();
  }
}

void nescart::clr_vblank()
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

time_t fpstime=time(NULL);

void nescart::drawframe()
{
  int dodraw = 00;
  
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
      scr->scrtext(2, cy + 5, 0x31, "V:%d PP:%d", dwpat, ppid);
      for (int i = 0; i < 4; i++) {
	scr->scrrect(100 + i*9, cy + 5, 8, 8, ppu.read(0x3f00 + ppid * 4 + i));
      }
    }
    else if (nnid) {
      // draw our nametable
#if 0
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
	tile &= ~1;
	drawpat(ppu.o_sprites[i].x, ppu.o_sprites[i].y,   sbase + (tile * 16),   spid, hm, vm, i, sps);
	drawpat(ppu.o_sprites[i].x, ppu.o_sprites[i].y+8, sbase + (tile+1) * 16, spid, hm, vm, i, sps);
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
    scr->scrtext(0, scr->height+15, 0x31, "frame:%d fps:%.2f", frame, fps);
    scr->draw();
  }
  // draw CHR patterns
  if (scr->key('v', true)) {
    dwpat = (dwpat + 1);
    if (dwpat == (chrRomSz / 4096))
      dwpat=-1;
  }
  // Increment palette id (in chr pattern)
  if (scr->key('b', true)) {
    ppid = (ppid+1) % 8;
  }
  // Increment nametable displayed
  if (scr->key('n', true)) {
    nnid ^= 1;
  }
  // show full sprite
  if (scr->key('f', true)) {
    vk = (vk+1) % 16;
    flogger(0, "set vk: %d\n", vk);
  }
  setmask(scr, 'x', controller[0], D0);
  setmask(scr, 'z', controller[0], D1);
  setmask(scr, 'a', controller[0], D2);
  setmask(scr, 's', controller[0], D3);
  setmask(scr, Key::K_UP, controller[0], D4);
  setmask(scr, Key::K_DOWN, controller[0], D5);
  setmask(scr, Key::K_LEFT, controller[0], D6);
  setmask(scr, Key::K_RIGHT, controller[0], D7);

#if 0
  if (scr->KeyState['0']) {
    noize.f2t(noize.freq - 10);
  }
  if (scr->KeyState['1']) {
    noize.f2t(noize.freq + 10);
  }
  if (scr->KeyState['2']) {
    noize.vol = 0;
  }
  if (scr->KeyState['3']) {
    noize.vol = 8000;
  }
  if (scr->KeyState['4']) {
    nozetype = 0;
  }
  if (scr->KeyState['5']) {
    nozetype = 1;
  }
  if (scr->KeyState['6']) {
    nozetype = 2;
  }
  if (scr->KeyState['7']) {
    nozetype = 3;
  }
#endif
}

void nescart::drawpixel()
{
  int bgclr, spclr, pclr, pri;
  
  if ((scanline < 340) && ((clks >= 1 && clks <= 257)) || (clks >= 321 && clks <= 337))  {
    /* Default to background color */
    pclr = ppu.read(0x3f00);
    bgclr = ppu.bk.getpixel();
    spclr = pri = 0;

    if (clks >=2 && clks <= 257) {
      // loop through all sprites, claim first
      for (int i = 0; i < nSprite; i++) {
	pri = ppu.spr[i].attr & ATTR_P;
	if (spclr == 0) {
	  spclr = ppu.spr[i].getpixel();
	  if (i == 0 && spclr && bgclr) {
	    flogger(1, "sprite 0 hit\n");
	    ppu.ppustatus |= ppu::PPUSTATUS_S;
	  }
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
    if ((!bgclr || !pri) && spclr)
      pclr = ppu.read(spclr);
    else if (bgclr)
      pclr = ppu.read(bgclr);
    if (scanline <= 239  && (clks <= 256))
      scr->setpixel(clks-1, scanline, pclr);
  }
}

/* Evaluated for 0..239 and 261 */
void nescart::evalbg()
{
  if ((ppu.ppumask & ppu::PPUMASK_SHOWBG) == 0)
    return;
  if ((clks >= 1 && clks <= 256) || (clks >= 321 && clks <= 336)) {
    ppu.fetch(clks);
    drawpixel();
  }
  if (clks == 256)
    ppu.inc_vert();
  else if (clks == 257)
    ppu.copyhorz();
  else if (scanline == SCANLINE_PRE && (clks >= 280 && clks <= 304)) 
    ppu.copyvert();
}

/* Evaluated for lines 0..239 and 261 */
void nescart::evalsprite()
{
  if ((ppu.ppumask & ppu::PPUMASK_SHOWSPRITE) == 0)
    return;
  if (scanline <= 239) {
    if (clks >= 1 && clks <= 64) {
      // secondary oam clear
    }
    else if (clks >= 65 && clks <= 256) {
      // evaluate sprites for next line
    }
  }
  if (clks==257) {
    nSprite = ppu.loadsprites(scanline);
    if (nSprite > 0) {
      flogger(1, "line: %d sprites = %d\n", scanline, nSprite);
    }
  };
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
  int ctr=0;

  rdy = 1;
  cpu_reset();
  apu_init();
  ppu.ppustatus |= ppu::PPUSTATUS_V;
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
    if (((ctr++) % 3) == 0) {
      if (dma_count > 0) {
	ppu.oam[ppu.oamaddr++] = read(dma_addr++);
	dma_count--;
      }
      else {
	_elapsed += cpu_tick(1);
      }
      //apu_tick();
    }
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

uint8_t cpu_read8(uint32_t addr, int mode)
{
  uint8_t v = cart->read(addr);
  return v;
}

void cpu_write8(uint32_t addr, uint8_t v, int mode)
{
  cart->write(addr, v);
}

int cpu_tick(int n)
{
  trace=3;
  if (ncycles > 0) {
    ncycles--;
  }
  if (ncycles == 0) {
    ncycles = cpu_step();
  }
  return 0;
}

int main(int argc, char *argv[])
{
  setbuf(stdout, NULL);

  cart = new nescart(argv[1]);
  cart->run();

  return 0;
}
