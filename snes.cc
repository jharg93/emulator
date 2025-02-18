#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <inttypes.h>
#include <fcntl.h>
#include <unistd.h>
#include <assert.h>
#include <time.h>

#include "dstk.h"
#include "cpu.h"
#include "util.h"
#include "gr.h"
#include "bus.h"

#include "snes.h"
#include "snesreg.h"
#include "cpu/cpu_65816.h"

#include <deque>
#include <vector>

int scanline;

Screen scr(256, 224, 10, 60);
bus_t mmu(0xFFFFFF);

void genframe();

const int OBJ_VFLIP = 0x80;
const int OBJ_HFLIP = 0x40;
const int NT_VFLIP = 0x8000;
const int NT_HFLIP = 0x4000;
const int HVB_VBLANK = 0x80;
const int HVB_HBLANK = 0x40;

void flogger(int n, const char *fmt, ...)
{
  va_list ap;
  va_start(ap, fmt);
  vfprintf(stdout, fmt, ap);
}

/* Common OAM structure */
struct oam_t {
  uint16_t x;
  uint16_t y;
  uint16_t tid;
  uint16_t attr; // vhoopppc

  int XPOS() const {
    return x;
  };
  int YPOS() const {
    return y;
  };
  int HEIGHT() const {
    return 0;
  };
  int WIDTH() const {
    return 0;
  };
  int PAL() const {
    return (16*8) + ((attr >> 1) & 7);
  };
};

struct gsu_t {
  enum {
    GSU_R15   = 0x301E,
    GSU_PBR   = 0x3034,
    GSU_ROMBR = 0x3036,
    GSU_CFGR  = 0x3037,
    GSU_SCBR  = 0x3038,
    GSU_CSLR  = 0x3039,
    GSU_SCMR  = 0x303A,
    GSU_RAMBR = 0x303C,
  };
  static int io(void *arg, uint32_t addr, int mode, iodata_t& v) {
    gsu_t *g = (gsu_t *)arg;
    return g->gsuio(addr, mode, v);
  };
  int gsuio(uint32_t addr, int mode, iodata_t& v);
};

struct apu_t {
  enum {
    init, xfer, running
  };
  bool isreset;
  int state, index;
  uint8_t regs[4], cmd;
  uint16_t addr;
  
  apu_t() {
    reset();
  }
  void reset() {
    isreset = true;
    index   = 0;
    state   = init;
    regs[0] = 0xAA;
    regs[1] = 0xBB;
  };
  uint8_t access(uint32_t addr, uint8_t nv, int wr) {
    uint8_t &r = regs[addr - APUI00];

    if (wr) {
      if (addr == APUI02 || addr == APUI03)
	r = nv;
      else if (addr == APUI01 && nv != 0) {
	isreset = false;
      }
      else if (addr == APUI00 && !isreset) {
	r = nv;
      }
    }
    printf("APU:%.4x:%c %.2x [%.2x,%.2x,%.2x,%.2x]\n", addr, wr ? 'w' : 'r', r, regs[0], regs[1], regs[2], regs[3]);
    return r;
  }
};

struct snes {
  /* 128k of Work RAM (WRAM) at 7e:0000 - 7f:ffff
   *  mapped at                 xx:0000 - xx:1fff (xx = 00..3F and 80..BF)
   */
  uint8_t wram[128 * 1024];

  /* VRAM: 64k, 16 bit access
   *  32x32x2 = 2k   yy.yyyx.xxxx
   *    +--+
   *    |  |
   *    +--+
   *  32x64x2 = 4k
   *    +--+--+
   *    |  |  |
   *    +--+--+
   *  64x32x2 = 4k
   *    +--+
   *    |  |
   *    +--+
   *    |  |
   *    +--+
   *  64x64x2 = 8k
   *    +--+--+
   *    |  |  |
   *    +--+--+
   *    |  |  |
   *    +--+--+
   *
   * Tile Map (NameTable)
   * vhopppcc.cccccccc
   *   v = vert flip
   *   h = horiz flip
   *   o = priority
   *   p = palette# (0-7)
   *   c = tile# (0-1023)
   */
  uint8_t vram[64 * 1024];

  /* Color RAM (palettes)
   * 256 colors, BGR format.  0bbbbbgggggrrrrr
   */
  uint8_t cgram[512];

  /* OAM is 128x4 = 512 bytes + (128x2/8) 32 bytes = 544 bytes
   * Sprites are 16-colors
   *   uint8_t x;
   *   uint8_t y;
   *   uint8_t tid;
   *   uint8_t attr;  vhoopppc
   *
   *     c = upper tile# bit
   *   ppp = palette
   *    oo = priority
   *    vh = vert/horiz flip
   * 
   *   uint8_t highoam   sxsxsxsx
   *      x = upper x bit
   *      s = size (0=small, 1=large) + OBSEL[5:7]
   */
  uint8_t oam[544];

  /* Stored OAM ID */
  uint16_t oamid[128];

  int romtype;

  void showdma(int enabled, bool isHdma = false);
  void init(uint8_t *rom, uint32_t sz);
  int snesregio(void *arg, uint32_t addr, int mode, iodata_t& val);

  apu_t apu;
};

#define oamid thegame.oamid
#define oam   thegame.oam
#define vram  thegame.vram
#define wram  thegame.wram
#define cgram thegame.cgram

/*===========================================================*
 * DMA code
 *===========================================================*/
struct dmaxfer_t
{
  uint32_t src;
  uint32_t dst;
  uint32_t rdst;
  uint16_t count;
  int8_t   sincr;
  int8_t   dincr;
  uint8_t  mode; // DMA 0x00..0x07, HMDA=0x10..0x17
  uint8_t  dir;
  const char *lbl;
};

dmaxfer_t hdma[8];
int nxtval[8];

void rundma(dmaxfer_t *m);

void rundma(dmaxfer_t *m) {
  int nbxfer[] = { 1, 2, 1, 4, 4, 4, 2, 4  };
  
  printf("@DMA: src:%.6x[+/-]%d -> dst:%.6x len:%.6x [%d] ioaddr:%.6x %s\n",
	 m->src, m->sincr, m->dst, m->count,
	 nbxfer[m->mode],
	 m->rdst, m->lbl);
  do {
    switch (m->mode) {
    case 0x0:
      // write one byte, auto-advance src, dst
      cpu_write8(m->dst, cpu_read8(m->src));
      m->src += m->sincr;
      m->dst += m->dincr;
      m->count -= 1;
      break;
    case 0x1:
      // write two bytes, auto advance src, dst
      cpu_write16(m->dst, cpu_read16(m->src));
      m->src += m->sincr * 2;
      m->dst += m->dincr * 2;
      m->count -= 2;
      break;
    default:
      m->count--;
      break;
    }
  } while (m->count != 0);
}

/*============================================================*
 * Common CPU Read/Write routines
 *============================================================*/
uint8_t cpu_read8(uint32_t addr, int type) {
  iodata_t val;
  
  mmu.read(addr, val);
  return val;
}

void cpu_write8(uint32_t addr, uint8_t val, int type) {
  mmu.write(addr, (val & 0xFF));
}

uint32_t cpu_read24(uint32_t addr) {
  uint32_t ll,mm,hh;

  ll = cpu_read8(addr+0);
  mm = cpu_read8(addr+1);
  hh = cpu_read8(addr+2);
  return ((hh << 16) | (mm << 8) | ll);
}

/* LoRom: 2MB, banks 00-3f, mirror 80-bf : 512 'pages' xxxx.xxxx.xOOO.OOOO.OOOO.OOOO
 *  base0 = 0x808000;
 *  for (i = 0; i < romsz; i+= 0x8000) {
 *    mmu.register_handler(base0, base0 + 0x7FFF, 0xFFFFFF, memio, rom + i, bus_t::RD, "ROM 00-7D");
 *    base0 += 0x10000;
 *  |
 *  808000 ... 80FFFF = 00000..07FFF
 *  818000 ... 81FFFF = 08000..0FFFF
 *  ....
 *  Fx0000 ... Fx7FFF = SRAM (32k)
 *
 * HiRom: 4MB, banks 40-7d, mirror c0-ff
 */

/* SNES memory map
 * 00-3f 0000:1fff Fast  mirror     WRAM 7e:0000-7e:1fff
 *       2000:20ff Fast  ABA
 *       2100:21ff FAst  ABB        I/O ports
 *       2200:3fff Fast  ABA        Unused
 *       4000:41ff XSlow internal   cpu registers
 *       4200:43ff Fast  internal   cpu registers
 *       4400:5fff Fast  ABA
 *       6000:7fff Slow  ABA        expansion
 *       8000:ffff Slow  ABA /CART  64x32k = cartridge rom
 * 40-7d 0000:ffff Slow  ABA /CART  62x64k = cartridge rom
 * 7e-7f 0000:ffff Slow  ABA /WRAM   2x64k = Working RAM
 * 80-bf LoRom 64x32k
 * c0-ff HiRom 64x64k
 *
 * SNES Clock: 1.89e9/88 Hz = 
 *   CPU Cycle: 6 master
 *   Fast mem : 6 master
 *   Slow mem : 8 master
 *   XSlow    : 12 master
 */

struct snes thegame;

uint16_t &oamaddr = *(uint16_t *)&ioregs[OAMADDL & 0xFF];
uint16_t &vmaddr  = *(uint16_t *)&ioregs[VMADDL & 0xFF];
uint16_t &rdvram  = *(uint16_t *)&ioregs[RDVRAML & 0xFF];

uint8_t  &oamdata = ioregs[OAMDATA & 0xFF];

/* DMA registers
 *   DMAPx: DHICEEE
 *     000 Write 1 byte B0->21xx
 *     001 Write 2 byte B0->21xx B1->21xx+1
 *     010 [dma] Write 1 byte B0->21xx
 *     010[hdna] Write 2 bytes B0->21xx, B1->21xx
 *     011 Write 4 bytes, B0->21xx, B1->21xx, B2->21xx+1, B3->21xx+1
 *     100 Write 4 bytes, B0->21xx, B1->21xx+1, B2->21xx+2, B3->21xx+3
 *     101
 *     110
 *     111
 *  BBADx: nnnnnnnn (PPU register)
 *
 *  43x2 : hdma cpu ll
 *  43x3 : hdma cpu mm
 *  43x4 : hdma cpu hh
 *  43x5 : hdna xfer ll
 *  43x6 : hdma xfer hh
 *  43x7 : ----              xx
 *
 * HDMA:
 *   DMAP0: two bytes per scanline
 *   BBAD0: offset of register (BG1HOFS)
 *   A10T0L:hdma data
 *   A1B0:  hdma data bank
 */
void snes::showdma(int enabled, bool isHdma)
{
  int i, base, dmp;
  dmaxfer_t xfer;
  const char *incr[] = { "inc", "fix", "dec", "fix" };
  const char *dmaxfer[] =
    {
      "xx",                   // WRAM, 1 byte     1-byte to 1-register
      "xx/xx+1",              // VRAM, 2 bytes    2-bytes to 2-registers
      "xx/xx",                // OAM/CGRAM        2-bytes to 1-register (write twice)
      "xx/xx/xx+1/xx+1",      // BGnxOFS,M7x      4-bytes to 2-registers (write twice)
      "xx/xx+1/xx+2/xx+3",    // BGnSC, Window,   4-bytes to 4-registers
      "xx/xx+1/xx/xx+1",      // ??
      "xx/xx",                // same as 2
      "xx/xx/xx+1/xx+1",      // same as 3
    };
  int incrv[] = { 1, 0, -1, 0 };

  for (i = 0; i < 8; i++) {
    if ((enabled & (1L << i)) == 0)
      continue;

    base = 0x4300 + (i * 0x10);
    dmp = sr8(base);
    printf("==== DMA %d %.4x\n", i, base);
    printf("  DMAPx: %.2x %s %c %s type:%s\n",  dmp,
	   dmp & 0x80 ? "io->cpu" : "cpu->io",
	   dmp & 0x40 ? 'i' : 'd',
	   incr[(dmp >> 3) & 0x3],
	   dmaxfer[dmp & 7]);
    printf("  DEST:  %.6x ",  0x2100 + sr8(base + 1));
    switch (0x2100 + sr8(base+1)) {
    case OAMDATA:
      xfer.lbl = "OAM";
      xfer.rdst = oamaddr;
      break;
    case VMDATAL:
      xfer.lbl = "VRAM";
      xfer.rdst = sr16(VMADDL);
      break;
    case CGDATA:
      xfer.lbl = "CGRAM";
      xfer.rdst = R_CGADD;
      break;
    case WMDATA:
      xfer.lbl = "WRAM";
      xfer.rdst = sr24(WMADDL);
      break;
    default:
      xfer.lbl = "???";
      xfer.rdst = 0;
      printf("\n");
      break;
    }
    printf("  SRC :  %.6lx\n", sr24(base + 2));
    printf("  LENGTH:%.6lx\n", sr24(base + 5));
    if ((dmp & 0x80) == 0 && !isHdma) {
      /* CPU->IO */
      xfer.src   = sr24(base + 2);             // A1Tx
      xfer.dst   = 0x2100 + sr8(base + 1);     // BBADx
      xfer.count = sr16(base + 5);             // IDADDR
      xfer.mode  = dmp & 7;
      xfer.sincr = incrv[(dmp >> 3) & 3];
      xfer.dincr = 0;
      if (xfer.count)
	rundma(&xfer);
    }
  }
}

const uint16_t bgsz[] = { _32x32, _64x32, _32x64, _64x64 };

struct bg_t {
  bool      bgEn;      // background enabled
  bool      bgSubEn;
    
  uint16_t  scrSz;     // 32x32, 64x32, 32x64, 64x64 (NameTable)
  uint16_t  scrBase;   // nametable base
    
  uint16_t  tileSz;    // 8x8, 16x16 (CHR)
  uint16_t  tileBase;  // tile base

  uint16_t  hPos;      // scroll base
  uint16_t  vPos;

  int       mosaic;
    
  uint16_t *nt[4];
};

struct ppu_t {
  uint8_t  bgMode;
  uint8_t  bgPrio;
  uint8_t  inidisp;
  bg_t     bg[4];

  /* Sprites enabled */
  bool     objEn;
  bool     objSubEn;

  uint8_t  vPrev1;
  uint8_t  vPrev2;
  uint8_t  vPrev7;

  uint32_t objBase0;
  uint32_t objBase1;
  uint8_t  objSize;

  /* Mode 7 matrix */
  uint16_t m7a;
  uint16_t m7b;
  uint16_t m7c;
  uint16_t m7d;
  uint16_t m7x;
  uint16_t m7y;

  int m7px[256];
  int m7py[256];
  void m7gen(int y);
  int m7conv(int x, int y);

  void loadr(uint16_t& r, uint8_t v, int mode);

  void runhdma(int y);

  void renderline(int y);
  void renderbg(int, int, int *);
  
  void set_bgsc(int id, uint8_t val);
  void set_bghofs(int id, uint8_t val);
  void set_bgvofs(int id, uint8_t val);

  void drawbg(int id, int pri);
};

/* Nametable mirrors */
enum { UL, UR, LL, LR };

ppu_t ppu;

/* Set mirroring mode for nametable */
int mirrormode[] =
  {
    0x0000,
    0x0101,
    0x0011,
    0x0123
  };

void setmirror(int n, int mask)
{
#if 0
  uint8_t *base = vram + ppu.bg[n].scrBase;

  ppu.bg[n].nt[UL] = base + ((mask >> 12) & 0xF) * 0x800;
  ppu.bg[n].nt[UR] = base + ((mask >> 8)  & 0xF) * 0x800;
  ppu.bg[n].nt[LL] = base + ((mask >> 4)  & 0xF) * 0x800;
  ppu.bg[n].nt[LR] = base + ((mask >> 0)  & 0xF) * 0x800;
#endif
}

/* AAAAAA.SS : A = BG Map Address (Nametable), SS=32x32, 64x32, 32x64, 64x64 */
void ppu_t::set_bgsc(int id, uint8_t val) {
  bg[id].scrSz = bgsz[val & 0x3];
  bg[id].scrBase = (val >> 2) * 2048;
  setmirror(id, mirrormode[val & 0x3]);
  printf("@BGxSC:  ScreenBase[%d]: %.6x ScreenSz: %.4x %.2x\n",
	 id, bg[id].scrBase, bg[id].scrSz, val);
};

void ppu_t::set_bghofs(int id, uint8_t val) {
  bg[id].hPos = (val << 8) | (vPrev1 & 0xF8) | (vPrev2 & 0x07);
  printf("@BGxHOFS: Screen%d: hPos: %.4x\n", id, bg[id].hPos);
  vPrev1 = val;
  vPrev2 = val;
}

void ppu_t::set_bgvofs(int id, uint8_t val) {
  bg[id].vPos = (val << 8) | vPrev1;
  printf("@BGxVOFS: Screen%d: vPos: %.4x\n", id, bg[id].vPos);
  vPrev1 = val;
}

/*==============================*
 * LoRom:
 *  Bank 00-3F
 *   8000-FFFF: 000000..007FFF, 008000..00FFFF 
 *  Bank 80-BF
 * HiRom:
 *  Bank 00-3F
 *   8000-FFFF: 008000..00FFFF, 018000..01FFFF, 028000..02FFFF
 *  Bank 80-BF
 *==============================*/

/* Remap VRAM address for DMA */
uint16_t vramRemap() {
  uint16_t addr = R_VMADDL;
  int mm;

  mm = R_VMAIN & 0x0c;
  if (mm == 0x04) {
    /* 8-bit aaaaaaaa.YYYxxxxx -> aaaaaaaa.xxxxxYYY */
    return (addr & 0xFF00) | ((addr & 0x00E0) >> 5) | ((addr & 0x001F) << 3);
  } else if (mm == 0x08) {
    /* 9-bit aaaaaaaY.YYxxxxxP -> aaaaaaax.xxxxPYYY */
    return (addr & 0xFE00) | ((addr & 0x01C0) >> 6) | ((addr & 0x003F) << 3);
  }
  else if (mm == 0x0c) {
    /* 10-bit aaaaaaYY.YxxxxxPP -> aaaaaaxx.xxxPYYY */
    return (addr & 0xFC00) | ((addr & 0x0380) >> 7) | ((addr & 0x007F) << 3);
  }
  return addr;
}

uint16_t signex13(uint16_t iv)
{
  if (iv & 0x2000) {
    iv |= 0xE000;
  }
  return iv;
}

/* Load matrix calculation */
void loadr(uint16_t& r, uint8_t v, int mode, const char *lbl)
{
  if (mode != 'w')
    return;
  r = signex13((v << 8) | ppu.vPrev7);
  printf("load[%s] = %.4x\n", lbl, r);
  ppu.vPrev7 = v;
}

// 10.xxxx.xxxX.XXXX
int snes::snesregio(void *arg, uint32_t addr, int mode, iodata_t& val) {
  uint8_t  incr[] = { 1,32,128,128 };
  uint32_t tmp, vm;
  static int cgaddr = 0;
  struct snes *c = &thegame;

  switch (addr) {
  case OBSEL:
    if (mode == 'w') {
      /* sss1100 : sss = size+oam hiattr, 11 = objBase1, 00 = objBase0 */
      ppu.objSize = (val >> 5) & 0x7;
      ppu.objBase0 = (val & 0x3) * 16384;
      ppu.objBase1 = ppu.objBase0 + 16384 + (((val >> 2) & 0x3) * 8192);
#if 0
      printf("@Write OBSEL: objBase0:%.4x objBase1:%.4x sz:%x %.2x\n",
	     ppu.objBase0, ppu.objBase1, ppu.objSize, val);
#endif
    }
    break;
#if 0
  case OAMADDL:
    val = setlo(oamaddr, val, mode);
    break;
  case OAMADDH:
    val = sethi(oamaddr, val, mode);
    break;
#endif
  case OAMDATA:
    if (mode == 'w') {
      tmp = oamaddr & 0x1FF;
      printf("Write:OAMDATA : %.4x %.2x\n", tmp, val);
      if (tmp >= sizeof(oam)) {
	printf("AAACK OVER OAM\n");
	break;
      }
      oam[tmp] = val;
      if (tmp < 512 && ((tmp & 0x3) == 2)) {
	oamid[tmp / 4] = (oamid[tmp / 4] & 0xFF00) | val;
      }
      else if (tmp >= 512 && tmp < 544) {
	/* Store out upper bits */
	vm = (tmp - 512) / 4;
	oamid[vm+0] = (oamid[vm+0] & 0xFF) | ((val & 0x03) << 8);
	oamid[vm+1] = (oamid[vm+1] & 0xFF) | ((val & 0x0C) << 6);
	oamid[vm+2] = (oamid[vm+2] & 0xFF) | ((val & 0x30) << 4);
	oamid[vm+3] = (oamid[vm+3] & 0xFF) | ((val & 0xC0) << 2);
      }
    }
    break;
  case VMAIN:
    /* IxxxTTss : I = Low/High ; TT=remap type ; ss = step */
    if (mode == 'w') {
      //printf("@VMAIN: %.2x\n", val);
    }
    break;
  case VMDATAL:
  case VMDATAH:
    if (mode == 'w') {
      tmp = vramRemap();
      //printf("Write:VRAMDATA%c : %.4x VMAIN:%.2x %x\n", (addr == VMDATAL) ? 'L' : 'H', tmp, vm, val);
      if (addr == VMDATAH) {
	vram[(tmp*2+1) & 0xFFFF] = val;
	if ((R_VMAIN & 0x80) != 0) {
	  R_VMADDL += incr[R_VMAIN & 3];
	}
      }
      else {
	vram[(tmp*2) & 0xFFFF] = val;
	if ((R_VMAIN & 0x80) == 0) {
	  R_VMADDL += incr[R_VMAIN & 3];
	}
      }
    }
    break;
  case WMDATA:
    if (mode == 'w') {
      tmp = sr24(WMADDL) & 0x1FFFF;
      //printf("Write WRAM: %.6x %.2x\n", tmp, val);
      assert(tmp < sizeof(wram));
      
      wram[tmp++] = val;
      sr8(WMADDL) = tmp;
      sr8(WMADDM) = (tmp >> 8);
      sr8(WMADDH) = (tmp >> 16);
    }
    break;
  case CGADD:
    if (mode == 'w') {
      printf("Reset CGADD: %.2x\n", val);
      cgaddr = val;
    }
    break;
  case CGDATA:
    if (mode == 'w') {
      //printf("Write CGRAM: %.2x = %.2x\n", cgaddr, val);
      cgram[cgaddr++] = val;
      R_CGADD = (cgaddr >> 1);
    }
    break;
  case MDMAEN:
    if (mode == 'w') {
      printf("DMA enable: %x\n", val);
      c->showdma(val, false);
    }
    break;
  case HDMAEN:
    printf("HDMA enable: %x:%x\n", val, R_HDMAEN);
    break;
  case RDNMI:
    R_RDNMI |= 0x72;
    if (mode == 'r') {
      val = R_RDNMI;
      R_RDNMI &= ~0x80;
      return 0;
    }
    break;
  case BGMODE:
    /* 4321.pmmm : p=priority, mmm=mode */
    if (mode == 'w') {
      ppu.bgMode = (val & 0x7);
      if (ppu.bgMode == 5 || ppu.bgMode == 6) {
	val |= 0xf0;
      }
      ppu.bg[0].tileSz = (val & D4) ? _16x16 : _8x8;
      ppu.bg[1].tileSz = (val & D5) ? _16x16 : _8x8;
      ppu.bg[2].tileSz = (val & D6) ? _16x16 : _8x8;
      ppu.bg[3].tileSz = (val & D7) ? _16x16 : _8x8;
      ppu.bgPrio = !!(val & D3);
      printf("@BGMODE: Screen BG0: %.4x BG1: %.4x BG2: %.4x BG3: %.4x Mode:%d Prio:%d\n",
	     ppu.bg[0].tileSz,
	     ppu.bg[1].tileSz,
	     ppu.bg[2].tileSz,
	     ppu.bg[3].tileSz,
	     ppu.bgMode,
	     ppu.bgPrio);
    }
    break;
  case BG1SC: ppu.set_bgsc(0, val); break;
  case BG2SC: ppu.set_bgsc(1, val); break;
  case BG3SC: ppu.set_bgsc(2, val); break;
  case BG4SC: ppu.set_bgsc(3, val); break;
  case BG12NBA:
    /* BBBB.AAAA : A = BG Tile Address (CHR) */
    if (mode == 'w') {
      // tilebase ok
      ppu.bg[0].tileBase = (val & 0xF) * 0x2000;
      ppu.bg[1].tileBase = (val >> 4)  * 0x2000;
      printf("@BG12NBA: ScreenTile: %.6x %.6x %.2x\n",
	     ppu.bg[0].tileBase, ppu.bg[1].tileBase, val);
    }
    break;
  case BG34NBA:
    if (mode == 'w') {
      // tilebase ok
      ppu.bg[2].tileBase = (val & 0xF) * 0x2000;
      ppu.bg[3].tileBase = (val >> 4)  * 0x2000;
      printf("@BG34NBA: ScreenTile: %.6x %.6x %.2x\n", ppu.bg[2].tileBase, ppu.bg[3].tileBase, val);
    }
    break;
  case BG1HOFS: ppu.set_bghofs(0, val); break;
  case BG2HOFS: ppu.set_bghofs(1, val); break;
  case BG3HOFS: ppu.set_bghofs(2, val); break;
  case BG4HOFS: ppu.set_bghofs(3, val); break;
  case BG1VOFS: ppu.set_bgvofs(0, val); break;
  case BG2VOFS: ppu.set_bgvofs(1, val); break;
  case BG3VOFS: ppu.set_bgvofs(2, val); break;
  case BG4VOFS: ppu.set_bgvofs(3, val); break;
  case TM: /* Main Screen designation */
    if (mode == 'w') {
      ppu.bg[0].bgEn = !!(val & D0);
      ppu.bg[1].bgEn = !!(val & D1);
      ppu.bg[2].bgEn = !!(val & D2);
      ppu.bg[3].bgEn = !!(val & D3);
      printf("@TM: ScreenEnabled: %d %d %d %d %.2x\n",
	     ppu.bg[0].bgEn, ppu.bg[1].bgEn,
	     ppu.bg[2].bgEn, ppu.bg[3].bgEn,
	     val);
    }
    break;
  case TS: /* Sub Screen designation */
    if (mode == 'w') {
      ppu.bg[0].bgSubEn = !!(val & D0);
      ppu.bg[1].bgSubEn = !!(val & D1);
      ppu.bg[2].bgSubEn = !!(val & D2);
      ppu.bg[3].bgSubEn = !!(val & D3);
      printf("@TS: SubScreenEnabled: %d %d %d %d %.2x\n",
	     ppu.bg[0].bgSubEn, ppu.bg[1].bgSubEn,
	     ppu.bg[2].bgSubEn, ppu.bg[3].bgSubEn,
	     val);
    }
    break;
  case M7SEL:
    break;
  case M7A: loadr(ppu.m7a, val, mode, "m7a"); break;
  case M7B: loadr(ppu.m7b, val, mode, "m7b"); break;
  case M7C: loadr(ppu.m7c, val, mode, "m7c"); break;
  case M7D: loadr(ppu.m7d, val, mode, "m7d"); break;
  case M7X: loadr(ppu.m7x, val, mode, "m7x"); break;
  case M7Y: loadr(ppu.m7y, val, mode, "m7y"); break;
  case INIDISP:
    if (mode == 'w') {
      ppu.inidisp = val;
      printf("Screen: IniDisp: %.2x\n", val);
    }
    break;
  case MOSAIC:
    if (mode == 'w') {
      // vvvv.3210
      tmp = (val >> 4) + 1;
      ppu.bg[0].mosaic = (val & 0x1) ? tmp : 0;
      ppu.bg[1].mosaic = (val & 0x2) ? tmp : 0;
      ppu.bg[2].mosaic = (val & 0x4) ? tmp : 0;
      ppu.bg[3].mosaic = (val & 0x8) ? tmp : 0;
      printf("MOSAIC : %d.%d.%d.%d\n",
	     ppu.bg[0].mosaic,
	     ppu.bg[1].mosaic,
	     ppu.bg[2].mosaic,
	     ppu.bg[3].mosaic);
    }
    break;
  case JOY1H:
  case JOY1L:
    printf("ReadJoy: %x %x\n", addr, sr8(addr));
    break;
  case APUI00:
  case APUI01:
  case APUI02:
  case APUI03:
    val = apu.access(addr, val, mode == 'w');
    break;
  default:
    //printf("unk:%c %.4x %x\n", mode, addr, val);
    break;
  }
  //printf("snesreg: %.8x[%s] %c %.2x\n", addr, iorname(addr), mode, val);
  return memio(arg, addr, mode, val);
}

int snesio(void *arg, uint32_t addr, int mode, iodata_t& val) {
  uint8_t *cartrom = (uint8_t *)arg;
  uint16_t offset = addr;
  uint8_t  bank = (addr >> 16) & 0x7F;
  struct snes *c = &thegame;
  
  if (offset <= 0x1FFF) {
    /* WRAM 8k mirror */
    return memio(wram, offset, mode, val);
  }
  if (offset >= 0x2100 && offset < 0x437F)
    return c->snesregio(ioregs, offset, mode, val);
  if (offset >= 0x8000) {
    if (c->romtype == 0) {
      addr = (bank * 0x8000) + (addr & 0x7FFF);
    }
    else {
      addr = (bank * 0x8000) + (addr & 0xFFFF);
    }
    return memio(cartrom, addr, mode, val);
  }
  printf("No addr: %x\n", addr);
  return -1;
}

enum {
  LOROM = 0x01,
  HIROM = 0x02,
  ExLOROM = 0x11,
  ExHIROM = 0x12,
  FastLOROM = 0x21,
  FastHIROM = 0x22,
};

int getrt(int rt) {
  switch (rt) {
  case 0x20: return LOROM;
  case 0x21: return HIROM;
  case 0x30: return FastLOROM;
  case 0x31: return FastHIROM;
  case 0x32: return ExLOROM;
  case 0x35: return ExHIROM;
  }
  return 0;
}

/* Memory maps:
 * LoRom (max 4Mb)
 *  80.8000-80.FFFF : 000000-007FFF of rom
 *  81.0000-81.FFFF : 008000-00FFFF of rom
 *  82.0000-82.FFFF : 010000-01FFFF of rom
 *  F0.0000-F0.7FFF : sram
 *  F1.0000-F1.7FFF : sram
 * HiRom (max 4mb)
 *  C0.0000-C0.FFFF : 000000-00FFFF of rom
 *  C1.0000-C1.FFFF : 010000-01FFFF of rom
 * 256 banks of 64k
 * 00-3F
 *  XX.0000-XX.1FFF mirror wram
 *  XX.2100-XX.21FF ppu
 *  XX.4000-XX.40FF joypad
 *  XX.4200-XX.43XX cpu
 * 40-7F
 *  7E.0000-7F.FFFF work ram
 * 80-BF
 *  XX.0000-XX.1FFF mirror wram
 *  XX.2100-XX.21FF ppu
 *  XX.4000-XX.40FF joypad
 *  XX.4200-XX.43XX cpu
 * C0-FF
 */
void snes::init(uint8_t *rom, uint32_t sz) {
  const snes_header *lo = (snes_header *)(rom + 0x7fc0);
  const snes_header *hi = (snes_header *)(rom + 0xffc0); 
  uint16_t rtlo = -1, rthi = -1;
  uint16_t cksum, base;
  
  if (sz & (sz - 1)) {
    printf("Not a power of 2....\n");
    exit(0);
  }
  if (sz < 32768)
    return;
  
  cksum = 0;
  for (int i = 0; i < sz; i++) {
    cksum += rom[i];
  }
  lo = (snes_header *)(rom + 0x7fc0);
  hi = lo;
  if (sz >= 65536) {
    hi = (snes_header *)(rom + 0xffc0);
  }
  printf("Cksum: %.4x %.4x %.4x | %.4x %.4x\n", cksum, lo->cmc_check, lo->cksum, hi->cmc_check, hi->cksum);
  
  /* Map upper 32k of each rom bank */
  rtlo = (sz > 0x7FFF) ? get16(&rom[0x7FD5]) : -1;
  rthi = (sz > 0xFFFF) ? get16(&rom[0xFFD5]) : -1;
  printf("Lo: %.4x Hi: %.4x\n", rtlo, rthi);

  base = 0;
  romtype = 0;
  if ((getrt(rtlo) & LOROM) && !(getrt(rthi) & LOROM)) {
    /* Map bank 00-3F and 80-BF */
    printf("LoRom\n");
    hexdump(rom + 0x7fb0, 0x50);
    base = get16(&rom[0x7ffc]);
    romtype = 0;
  }
  else if ((getrt(rthi) & HIROM) && !(getrt(rtlo) & HIROM)) {
    printf("HiRom\n");
    hexdump(rom + 0xffb0, 0x50);
    romtype = 1;
  }
  else {
    printf("Unknown ROM\n");
  }
  /* Internal bank ranges:
   *  xx:0000-1FFF: WRAM 8k
   *  xx:2100-437F: snes registers
   *  xx:8000-FFFF
   *     type0: &ROM[(xx * 0x8000) + (addr & 0x7fff)]
   */
  mmu.register_handler(0x000000, 0x3FFFFF, 0xFFFFFF, snesio, rom, _RW, "SNES 00-3F");
  mmu.register_handler(0x800000, 0xBFFFFF, 0xFFFFFF, snesio, rom, _RW, "SNES 80-BF");
  mmu.register_handler(0x400000, 0x7DFFFF, 0x03FFFF, memio,  rom, _RD, "ROM 40-7D");
  mmu.register_handler(0xC00000, 0xFFFFFF, 0x03FFFF, memio,  rom, _RD, "ROM C0-FF");
  mmu.register_handler(0x7E0000, 0x7FFFFF, 0x01FFFF, memio, wram, _RW, "WRAM");

  mmu.dump();
  mkop();

  cpu_reset(0);
  hexdump(&rom[PC], 32);
  printf("data = %x\n", PC);
  //dumpcfg(0x8003, 0, sz);
}

/* 256x224 screen ; 16 bytes per tile, 2bpp */
/*  0x00       0x08
 *  row0.bit0  row4.bit0
 *  row0.bit1  row4.bit1
 *  row1.bit0  row5.bit0
 *  row1.bit1  row5.bit1
 *  row2.bit0  row6.bit0
 *  row2.bit1  row6.bit1
 *  row3.bit0  row7.bit0
 *  row3.bit1  row7.bit1
 */
void drawtile2(int x, int y, int w, int h, uint16_t *addr, int pbase, int hm = 0, int vm = 0, int mosaic=1) {
  uint16_t *palette = (uint16_t *)cgram;
  int yp, xp, l, m, sy;
  uint8_t a[2];

  for (yp = 0; yp < 8; yp+=mosaic) {
    sy = yp ^ vm;
    a[0] = addr[sy+0] & 0xff;
    a[1] = addr[sy+0] >> 8;
    for (xp = 0; xp < 8; xp+=mosaic) {
      m = 0;
      for (l = 0; l < 2; l++) {
	m |= ((a[l] >> (xp ^ hm)) & 1) << l;
      }
      if (m && inrange(y+yp, 0, 224)) {
	scr.scrrect(x+xp, y+yp, mosaic, mosaic, BGRRGB(palette[pbase+m]));
      }
    }
  }
}

/* 16-colors, 32 bytes per tile, 4bpp */
/*  0x00       0x08       0x10       0x18
 *  row0.bit0  row4.bit0  row0.bit2  row4.bit2
 *  row0.bit1  row4.bit1  row0.bit3  row4.bit3
 *  row1.bit0  row5.bit0  row1.bit2  row5.bit2
 *  row1.bit1  row5.bit1  row1.bit3  row5.bit3
 *  row2.bit0  row6.bit0  row2.bit2  row6.bit2
 *  row2.bit1  row6.bit1  row2.bit3  row6.bit3
 *  row3.bit0  row7.bit0  row3.bit2  row7.bit2
 *  row3.bit1  row7.bit1  row3.bit3  row7.bit3
 */
void drawtile4(int x, int y, int w, int h, uint16_t *addr, int pbase, int hm = 0, int vm = 0, int mosaic=1) {
  uint16_t *palette = (uint16_t *)cgram;
  int yp, xp, l, m, sy;
  uint8_t a[4];

  for (yp = 0; yp < 8; yp+=mosaic) {
    sy = yp ^ vm;
    a[0] = addr[sy+0] & 0xff;
    a[1] = addr[sy+0] >> 8;
    a[2] = addr[sy+8] & 0xff;
    a[3] = addr[sy+8] >> 8;
    for (xp = 0; xp < 8; xp+=mosaic) {
      m = 0;
      for (l = 0; l < 4; l++) {
	m |= ((a[l] >> (xp ^ hm)) & 1) << l;
      }
      if (m && inrange(y+yp, 0, 224)) {
	scr.scrrect(x+xp, y+yp, mosaic, mosaic, BGRRGB(palette[pbase+m]));
      }
    }
  }
}

/* Draw 8bpp pixels, 64 bytes per tile */
/*  0x00       0x08       0x10       0x18       0x20       0x28       0x30       0x38
 *  row0.bit0  row4.bit0  row0.bit2  row4.bit2  row0.bit4  row4.bit4  row0.bit6  row4.bit6
 *  row0.bit1  row4.bit1  row0.bit3  row4.bit3  row0.bit5  row4.bit5  row0.bit7  row4.bit7
 *  row1.bit0  row5.bit0  row1.bit2  row5.bit2  row1.bit4  row5.bit4  row1.bit6  row5.bit6
 *  row1.bit1  row5.bit1  row1.bit3  row5.bit3  row1.bit5  row5.bit5  row1.bit7  row5.bit7
 *  row2.bit0  row6.bit0  row2.bit2  row6.bit2  row2.bit4  row6.bit4  row2.bit6  row6.bit6
 *  row2.bit1  row6.bit1  row2.bit3  row6.bit3  row2.bit5  row6.bit5  row2.bit7  row6.bit7
 *  row3.bit0  row7.bit0  row3.bit2  row7.bit2  row3.bit4  row7.bit4  row3.bit6  row7.bit6
 *  row3.bit1  row7.bit1  row3.bit3  row7.bit3  row3.bit5  row7.bit5  row3.bit7  row7.bit7
 */

/* 256 colors : */
void drawtile8(int x, int y, int w, int h, uint16_t *addr, int pbase, int hm = 0, int vm = 0, int mosaic=1) {
  uint16_t *palette = (uint16_t *)cgram;
  int yp, xp, l, m, sy;
  uint8_t a[8];

  for (yp = 0; yp < 8; yp+=mosaic) {
    sy = yp ^ vm;
    a[0] = addr[sy+0] & 0xff;
    a[1] = addr[sy+0] >> 8;
    a[2] = addr[sy+8] & 0xff;
    a[3] = addr[sy+8] >> 8;
    a[4] = addr[sy+16] & 0xff;
    a[5] = addr[sy+16] >> 8;
    a[6] = addr[sy+24] & 0xff;
    a[7] = addr[sy+24] >> 8;
    for (xp = 0; xp < 8; xp+=mosaic) {
      m = 0;
      for (l = 0; l < 8; l++) {
	m |= ((a[l] >> (xp ^ hm)) & 1) << l;
      }
      if (m) {
	scr.scrrect(x+xp, y+yp, mosaic, mosaic, BGRRGB(palette[pbase+m]));
      }
    }
  }
}

/* 64 bytes per tile, no bitplanes */
void drawtilem7(int x, int y, int w, int h, uint8_t *addr, int pbase, int hm = 0, int vm = 0, int mosaic=1) {
  uint16_t *palette = (uint16_t *)cgram;
  int yp, xp, pxl;

  for (yp = 0; yp < 8; yp++) {
    for (xp = 0; xp < 8; xp++) {
      pxl = *addr;
      addr += 2;
      scr.scrrect(x+xp, y+yp, mosaic, mosaic, BGRRGB(palette[pxl]));
    }
  }
}

int setmask(int key, uint8_t & k, uint8_t m) {
  if (scr.key(key, true)) {
    k |= m;
    printf("@readkey: '%c' %.2x\n", key >= ' ' && key <= 'z' ? key : '.', k);
  }
  else
    k &= ~m;
  return 0;
}

void joyinput() {
  setmask(Key::K_UP,    R_JOY1H, 0b1000);
  setmask(Key::K_DOWN,  R_JOY1H, 0b0100);
  setmask(Key::K_LEFT,  R_JOY1H, 0b0010);
  setmask(Key::K_RIGHT, R_JOY1H, 0b0001);

  setmask('u', R_JOY1H, 0b1000); // up
  setmask('n', R_JOY1H, 0b0100); // down
  setmask('h', R_JOY1H, 0b0010); // left
  setmask('j', R_JOY1H, 0b0001); // right

  setmask('b', R_JOY1H, 0b10000000); // B
  setmask('y', R_JOY1H, 0b01000000); // Y
  setmask('w', R_JOY1H, 0b00100000); // SELECT
  setmask('s', R_JOY1H, 0b00010000); // START

  setmask('a', R_JOY1L, 0b10000000); // A
  setmask('x', R_JOY1L, 0b01000000); // X
  setmask('q', R_JOY1L, 0b00100000); // L
  setmask('w', R_JOY1L, 0b00010000); // R
}

static inline int getbit(const uint8_t *base, int off)
{
  return (base[off >> 3] >> (off & 7)) & 0x1;
}

/* Decode common sprite format */
int getsprite(oam_t *spr, int n) {
  if (n >= 128)
    return -1;
  spr->tid  = oam[n*4+2];
  spr->y    = oam[n*4+1];
  spr->x    = oam[n*4+0];
  spr->attr = oam[n*4+3];
  if (getbit(oam, 512 * (n*2)+0))
    spr->x += 256;
  if (getbit(oam, 512 * (n*2)+0))
    spr->attr += 256;

  if (spr->y == 0)
    return -1;
  return 0;
}

uint16_t *tileset(int base, int tile, int tilestep)
{
  return (uint16_t *)&vram[base + (tile * tilestep)];
}

void drawsprite(int pri)
{
  int i, id, hm, vm, pp, base;
  oam_t obj;

  //printf("OAM\n");
  //dump(oam, sizeof(oam), 64);
  for (i=0; i<128; i++) {
    if (getsprite(&obj, i) < 0)
      continue;
#if 0
    printf("OBJ%.2x: x:%3d y:%3d id:%3d attr:%.2x\n",
	   i, obj.x, obj.y, obj.tid, obj.attr);
#endif
    id = obj.tid;
    /* get hflip, vflip, palette base */
    hm = (obj.attr & OBJ_HFLIP) ? 0 : 7;
    vm = (obj.attr & OBJ_VFLIP) ? 7 : 0;
    pp = obj.PAL();
    base = ppu.objBase0;
    if (id >= 0x100) {
      base = ppu.objBase1;
      id -= 0x100;
    }
    drawtile4(obj.x, obj.y, 8, 8, tileset(base, id, 32), pp, hm, vm, 1);
  }
}

void dumpnt(uint16_t *nt, int scrSz) {
  return;
  
  if ((ppu.bgMode * 0x10) == MODE7_BG0) {
    /* Mode 7 nametable */
    for (int i = 0; i < 128*128; i+=128) {
      for (int j=0; j<128; j++) {
	if (nt[i+j] & 0xFF) {
	  printf("%.2x", nt[i+j] & 0xFF);
	}
	else {
	  printf("__");
	}
      }
      printf("\n");
    }
    return;
  }
  // nametable:
  //   vhopppcc.cccccccc
  //
  // nt addr:
  // vhaa.aaaaaaaa
  int mm = 32*32;
  int step = 32;
  
  if (scrSz == _32x64)
    mm = 32*64;
  for (int i = 0; i < mm; i+=step) {
    for (int j=0; j<step; j++) {
      int tid = nt[i+j];

      if (nt[i+j] & 0x3FF) {
	printf("%.3x ", nt[i+j] & 0x3FF);
      }
      else {
	printf("    ");
      }
      //printf("%c%c%d ", tid & NT_VFLIP ? 'v' : '_', tid & NT_HFLIP ? 'h' : '_', (tid >> 10) & 7);
    }
    printf("|");
    for (int j=0; j<step; j++) {
      int c = nt[i+j];
      if (c < ' ' || c > 'z')
	c = '.';
      printf("%c", c);
    }
    printf("\n");
  }
}

/* Mode 7: each word in vram:
 * 8x8 pixels per screen = 64 bytes 0000 - 003F
 * 256 characters at once = 256x64=16Kb
 * tilemap = 1024x1024 or 128x128 characters = 16Kb
 * ggtt ggtt ggtt ggtt ggtt ggtt ggtt ggtt ggtt ggtt ggtt ggtt ggtt ggtt ggtt ggtt
 * ggtt ggtt ggtt ggtt ggtt ggtt ggtt ggtt ggtt ggtt ggtt ggtt ggtt ggtt ggtt ggtt
 * ggtt ggtt ggtt ggtt ggtt ggtt ggtt ggtt ggtt ggtt ggtt ggtt ggtt ggtt ggtt ggtt
 * ggtt ggtt ggtt ggtt ggtt ggtt ggtt ggtt ggtt ggtt ggtt ggtt ggtt ggtt ggtt ggtt
 */
void ppu_t::renderbg(int b, int pri, int *line)
{
}

void ppu_t::renderline(int y)
{
  if (bgMode * 0x10 != MODE7_BG0) {
    return;
  }
  m7gen(y);
  for (int x = 0; x < 256; x++) {
    scr.setpixel(x, y, m7conv(x, y));
  }
}

void ppu_t::drawbg(int n, int pri)
{
  static int hm, vm, pb, cm;
  int x, y, tile, id, hp, vp, tid, bgmode, w, h, tp;
  uint16_t *nt;
  uint16_t *cg = (uint16_t *)cgram;
  bg_t *b = &bg[n];

  if (b->bgEn == false)
    return;
  printf("draw: %d %d\n", n, pri);

  // Get Backplane horizontal/vertial position
  hp = b->hPos;
  vp = b->vPos;
#if 0
  printf("DrawBG%d: MapSz:%.4x MapBase:%.6x TileSz:%.4x TileBase:%.6x mode:%d hpos:%.4x vpos:%.4x\n",
	 n, b->scrSz, b->scrBase, b->tileSz, b->tileBase,
	 bgMode, b->hPos, b->vPos);
#endif
  
  nt = (uint16_t *)&vram[b->scrBase];
  /* Format of nametable:
   *   vhopppcc cccccccc 
   *   v = vflip
   *   h = hflip
   *   o = pri;
   *   p = palette (0-7)
   *   c = tile id (0-1023)
   */
  dumpnt(nt, b->scrSz);
  
  x = y = 0;
  bgmode = (bgMode * 0x10) + n;
  if (bgmode >= MODE7_BG0) {
    return;
  }
  w = b->tileSz >> 8;
  h = b->tileSz & 0xff;
  int sw = b->scrSz >> 8;
  int sh = b->scrSz & 0xff;
  for (tile = 0; tile < 32*sh; tile++) {
    tid = nt[tile];

    /* Pallete number, horz/vert flip */
    vm = (tid & NT_VFLIP) ? 7 : 0;
    hm = (tid & NT_HFLIP) ? 0 : 7;
    pb = ((tid >> 10) & 7);
    tp = ((tid >> 13) & 1);
    id = tid & 0x3FF;

    if (cm) {
      /* Direct color mode */
      if (tile < 256)
	scr.scrrect(x, y, 7, 7, BGRRGB(cg[tile]));
    }
    else if (tp == pri) {
      /* Key off mode and BG: mode.BGn */
      switch (bgmode) {
      case MODE0_BG0: // Mode 0.0 2bpp, pb=ppp*4 + bg*32
      case MODE0_BG1: // Mode 0.1 2bpp, pb=ppp*4 + bg*32
      case MODE0_BG2: // Mode 0.2 2bpp, pb=ppp*4 + bg*32
      case MODE0_BG3: // Mode 0.3 2bpp, pb=ppp*4 + bg*32
      case MODE1_BG2: // Mode 1.2 2ppp, pb=ppp*ncolors
      case MODE4_BG1: // Mode 4.1 2bpp, pb=ppp*4
      case MODE5_BG1: // Mode 5.1 2bpp, pb=ppp*ncolors
	pb = (pb * 4) + (n * 16);
	drawtile2(x-hp, y-vp, w, h, tileset(b->tileBase, id, 16), pb, hm, vm, b->mosaic+1);
	break;
      case MODE1_BG0: // Mode 1.0 4bpp, pb=ppp*ncolors
      case MODE1_BG1: // Mode 1.1 4bpp, pb=ppp*ncolors
      case MODE2_BG0: // Mode 2.0 4bpp, pb=ppp*16
      case MODE2_BG1: // Mode 2.1 4bpp, pb=ppp*16
      case MODE3_BG1: // Mode 3.1 4bpp, pb=ppp*16 
      case MODE5_BG0: // Mode 5.0 4bpp, pb=ppp*ncolors
      case MODE6_BG0: // Mode 6.0 4bpp, pb=ppp*ncolors
	pb = (pb * 16);
	if (w == 16) {
	  id &= ~1;
	  drawtile4(x-hp,   y-vp, w, h, tileset(b->tileBase, id+0, 32), pb, hm, vm, b->mosaic+1);
	  drawtile4(x-hp+8, y-vp, w, h, tileset(b->tileBase, id+1, 32), pb, hm, vm, b->mosaic+1);
	}
	else {
	  drawtile4(x-hp, y-vp, w, h, tileset(b->tileBase, id, 32), pb, hm, vm, b->mosaic+1);
	}
	break;
      case MODE3_BG0: // Mode 3.0 8bpp, pb=0
      case MODE4_BG0: // Mode 4.0 8bpp, pb=0
	pb = 0;
	drawtile8(x-hp, y-vp, w, h, tileset(b->tileBase, id, 64), pb, hm, vm, b->mosaic+1);
	break;
      default:
	printf("Unknown mode...\n");
	break;
      }
    }
    x += w;
    if (x+w > 32*w) {
      //printf("\n");
      x = 0;
      y += 8;
    }
  }
}

/* Mode priority:
 *  0:        SP:3,  BG1:1, BG2:1, SP:2, BG1:0, BG2:0, SP:1, BG3:1, BG4:1, SP:0, BG3:0, BG4:0
 *  1: BG3:1*,SP:3   BG1:1, BG2:1, SP:2, BG1:0, BG2:0, SP:1, BG3:1*,       SP:0, BG3:0
 *  2:        SP:3,  BG1:1,        SP:2, BG2:1,        SP:1, BG1:0,        SP:0, BG2:0
 *  3: 
 */
int frame;

#define _PRBG    0x10
#define _PROBJ   0x20
#define PRBG(x)  _PRBG+x
#define PROBJ(x) _PROBJ+x

int szof(int sz) {
  switch (sz) {
  case _8x8:   return 0x88;
  case _8x16:  return 0x81;
  case _16x8:  return 0x18;
  case _16x16: return 0x11;
  case _32x32: return 0x33;
  case _64x32: return 0x63;
  case _32x64: return 0x36;
  case _64x64: return 0x66;
  }
  return 0;
}

#define P0   0x000
#define P1   0x100
#define P2   0x200
#define P3   0x300
#define BG1  0x000
#define BG2  0x001
#define BG3  0x002
#define BG4  0x003
#define OBJ  0x010

int pri0[] = {
  BG4+P0, BG3+P0, OBJ+P0, BG4+P1, BG3+P1, OBJ+P1, BG2+P0, BG1+P0, OBJ+P2, BG2+P1, BG1+P1, OBJ+P3, -1
};
int pri1[] = {
  BG4+P0, BG3+P0, OBJ+P0, BG4+P1, OBJ+P1, BG2+P0, BG1+P0, BG2+P1, OBJ+P2, BG1+P1, OBJ+P3, BG3+P1, -1
};

void genframe()
{
  static bool init = false;
  static time_t fpstime = time(NULL);
  time_t now = time(NULL);
  float fps = (float)frame / (now - fpstime);

  if (ppu.inidisp == 0)
    return;
  if (!init) {
    scr.xs = 2;
    scr.ys = 2;
    scr.init(1);
    init = true;
  }
  frame++;
  printf("--- frame: %d\n", frame);

  int *ptab = pri0;
  if (ppu.bgPrio == 1) {
    ptab = pri1;
  }
  while (*ptab != -1) {
    if (*ptab & OBJ) {
      drawsprite(*ptab >> 8);
    }
    else {
      ppu.drawbg(*ptab & 0xF, *ptab >> 8);
    }
    ptab++;
  }
  scr.scrtext(0, scr.height+4, MKRGB(0x7f, 0x7f, 0x00),
	      "frm:%d fps:%.2f",
	      frame, fps);
  scr.scrtext(0, scr.height+12, MKRGB(0x7f, 0x7f, 0x00),
	      "md:%d-%d%d%d%d-%d%d%d%d",
	      ppu.bgMode,
	      ppu.bg[0].bgEn, 
	      ppu.bg[1].bgEn, 
	      ppu.bg[2].bgEn, 
	      ppu.bg[3].bgEn,
	      ppu.bg[0].bgSubEn, 
	      ppu.bg[1].bgSubEn, 
	      ppu.bg[2].bgSubEn, 
	      ppu.bg[3].bgSubEn);
  scr.scrtext(0, scr.height+20, MKRGB(0x7f, 0x7f, 0x00),
	      "%.4x %.4x %.4x %.4x %.4x %.4x",
	      ppu.m7a, ppu.m7b, ppu.m7c, ppu.m7d, ppu.m7x, ppu.m7y);
  scr.scrtext(0, scr.height+28, MKRGB(0x7f, 0x7f, 0x00), "%.2x:%.2x %.2x:%.2x %.2x:%.2x %.2x:%.2x",
	      szof(ppu.bg[0].scrSz),szof(ppu.bg[0].tileSz),
	      szof(ppu.bg[1].scrSz),szof(ppu.bg[1].tileSz),
	      szof(ppu.bg[2].scrSz),szof(ppu.bg[2].tileSz),
	      szof(ppu.bg[3].scrSz),szof(ppu.bg[3].tileSz));
  scr.draw();
  scr.scrrect(0, 0, scr.width, scr.height, BGRRGB(*(uint16_t *)cgram));

  joyinput();
}

int trace = 0;

#include "json/thjson.cc"

#define CLIP(x) ((x) & 0x2000 ? ((x) | ~0x3ff) : ((x) & 0x3FF))
#define mm(x) ((x) & ~0x3f)

/* Perform math calculations for each line:

 * [vx] = [ab][sx + m7hofs - m7x] + [m7x]
 * [vy] = [cd][sy + m7vofs - m7y] + [m7y]
 *
 * vx = a*xm + b*ym + m7x
 * vy = c*xm + d*ym + m7y
 *
 * a(sx + h - m7x) + b(sy + v - m7y) + m7x
 * -> a(h - m7x) + b(sy + v - m7y) + asx + m7x
 */

/* Default screen:
 *   m7a = 1, m7b = 0, m7x = 0
 *   m7c = 0, m7d = 1, m7y = 0
 */
void ppu_t::m7gen(int y)
{
  int h = CLIP(bg[0].hPos - m7x);
  int v = CLIP(bg[0].vPos - m7y);

  /* Get first pixels */
  m7px[0] = mm(h * m7a) + mm(y * m7b) + mm(v * m7b) + (m7x << 8);
  m7py[0] = mm(h * m7c) + mm(y * m7d) + mm(v * m7d) + (m7y << 8);

  /* linear add factor to each pixel */
  for (int i = 1; i < 256; i++) {
    m7px[i] = m7px[i-1] + m7a;
    m7py[i] = m7py[i-1] + m7c;
  }
}

/* Return scaled pixel color at x, y */
int ppu_t::m7conv(int x, int y)
{
  uint16_t *nt = (uint16_t *)&vram[bg[0].scrBase];
  int xp = m7px[x] >> 8;
  int yp = m7py[x] >> 8;
  int tx, ty;

  if (xp < 0 || yp < 0 || xp >= 1024 || yp >= 1024)
    return 0;
  
  /* Nametable/Tilemap is 128x128 */
  tx = (xp & 0x3f8) >> 3;
  ty = (yp & 0x3f8) >> 3;
  int tid = nt[(ty * 128) + tx] & 0xFF;

  /* get row of pixel, data in upper byte */
  uint16_t *ts = &nt[tid * 64] + ((yp & 7) * 8) + (xp & 7);
  uint16_t *palette = (uint16_t *)cgram;

  return BGRRGB(palette[*ts >> 8]);
}

/* Vert:  Start at 1,  vblank 224, max 261
 * Horiz: Start at 22, hblank 277, max 339
 */
/* SNES Clock:
 *   NTSC: 21.477MHz
 *    PAL: 21.28137MHz
 * 1364 master clocks per scanline = 341 dots
 * CPU pauses for 40 clocks at middle of each scanline, 1324 effective dots
 * Scanline 0 is blank
 * Scanline 1-224/239 render image
 * Scanline 261/311 is last line
 * If Interlace is on, 1 extra scanline every even frame
 */
struct snes_crtc : public crtc_t {
  ppu_t *ppu;
  bool   _hblank = false;
  bool   _vblank = false;
  snes_crtc(ppu_t *p) {
    hBlank = 256; // 256x224 SNES resolution
    hEnd   = 384;
    vBlank = 224;
    vEnd   = 262; // NTSC 262 scanlines

    ppu    = p;
  };
  bool tick() {
    bool rc = crtc_t::tick();
#if 0
    if (hPos == hBlank && vPos < vBlank) {
      ppu->runhdma(vPos);
      ppu->renderline(vPos);
    }
#else
    if (hPos == 0) {
      sethblank(false);
    }
    else if (hPos == hBlank) {
      if (vPos < vBlank) {
	ppu->runhdma(vPos);
	ppu->renderline(vPos);
      }
      sethblank(true);
    }
    if (vPos == 0 && !hPos) {
      setvblank(false);
    }
    else if (vPos == vBlank && !hPos)
      setvblank(true);
#endif
    scanline = vPos;
    return rc;
  };
  void setvblank(bool state) {
    _vblank = state;
    if (state) {
      printf("nmi vblank\n");
      R_RDNMI |= 0x80;
    }
    else {
      R_RDNMI &= ~0x80;
    }
    R_HVBJOY = state ? (R_HVBJOY | HVB_VBLANK) : (R_HVBJOY & ~HVB_VBLANK);
  };
  void sethblank(bool state) {
    _hblank = state;
    R_HVBJOY = state ? (R_HVBJOY | HVB_HBLANK) : (R_HVBJOY & ~HVB_HBLANK);
  };
};

void ppu_t::runhdma(int y)
{
  dmaxfer_t *h;

  /* Run HDMA on line y */
  for (int i = 0; i < 8; i++) {
    int base = 0x4300 + (i * 0x10);
    if (~R_HDMAEN & (1L << i)) {
      continue;
    }
    h = &hdma[i];
    if (y == 0) {
      /* Reset line */
      h->src = sr24(base+2);
      h->dst = 0x2100 + sr8(base+1);
      h->mode = sr8(base);
      h->count = 1;
    }
    if (--h->count == 0) {
      h->count = cpu_read8(h->src++);
      switch(h->mode) {
      case 0x00: // write 1 byte
	nxtval[i] = cpu_read8(h->src);
	h->src += 1;
	break;
      case 0x02: // write 2 bytes
	nxtval[i] = cpu_read16(h->src);
	h->src += 2;
	break;
      }
      printf("line: %d, count:$d, val:%x\n", scanline, h->count, nxtval[i]);
    }
    switch(h->mode) {
    case 0x00:
      // write 1 byte
      cpu_write8(h->dst, nxtval[i]);
      break;
    case 0x02:
      // write 2 bytes
      cpu_write16(h->dst, nxtval[i]);
      //cpu_write8(h->dst, nxtval[i]);
      //cpu_write8(h->dst, nxtval[i] >> 8);
      break;
    }
    printf("line:%3d hdma:%d mode:%.2x ppu:%.4x[%s] cpu:%.6x ctr:%.4x\n",
	   scanline, i, h->mode, h->dst, iorname(h->dst), h->src, h->count);
  }
}

snes_crtc sc(&ppu);

int main(int argc, char *argv[])
{
  size_t sz = 0;
  uint8_t *cart = NULL;
  uint32_t offset = 0;
  const char *sramsz[] = { "off", "16k", "64k", "256k" };

  if (argc > 4) {
    read_json(argv[1]);
    exit(0);
  }
  cart = loadrom(argv[1], sz);
  printf("Got cart: %x\n", sz);
  if ((sz % 1024) != 0) {
    printf("Rom Header! %.2x %.2x %.2x %.2x %.2x\n",
	   cart[0], cart[1], cart[2], cart[0x8], cart[0x9]);
    if (cart[8] == 0xaa && cart[9] == 0xbb) {
      printf("SRAM Mode%d, DRAM Mode%d, SRAM:%5s RunMode:%d\n",
	     cart[2] & 0x20 ? 2 : 1,
	     cart[2] & 0x10 ? 21 : 20,
	     sramsz[(cart[2] >> 2) & 3],
	     cart[2] & 0x02 ? 3 : 2);
    }
    if (sz >= 0x3E20F) {
      printf("Register Backup:\n");
      hexdump(cart+0x3E200, 0xF);
    }
    offset = 0x200;
  }
  thegame.init(cart + offset, sz - offset);

  // scanline = 1364 cycles, 262 scanlines
  for(;;) {
    cpu_step();
    if (sc.tick())
      genframe();
  }
  return 0;
}
