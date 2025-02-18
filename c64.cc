/*
  c64.cc : Commodore 64 emulator

  Copyright (c) 2021 <Alan_Smithee@nowhere.org>

  Permission is hereby granted, free of charge, to any person obtaining
  a copy of this software and associated documentation files (the
  "Software"), to deal in the Software without restriction, including
  without limitation the rights to use, copy, modify, merge, publish,
  distribute, sublicense, and/or sell copies of the Software, and to
  permit persons to whom the Software is furnished to do so, subject to
  the following conditions:
  
  The above copyright notice and this permission notice shall be
  included in all copies or substantial portions of the Software.
  
  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
  LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
  OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
  WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <inttypes.h>
#include <fcntl.h>
#include <unistd.h>
#include <assert.h>
#include <time.h>
#include "cpu.h"
#include "util.h"
#include "gr.h"
#include "bus.h"
#include "audio.h"
#include "c64.h"

extern int SPC;
extern int trace;
extern bool hlt;

enum {
  CIA1_IRQ_TMRA = 0x1,
  CIA1_IRQ_TMRB = 0x2,
  CIA1_IRQ_TOD  = 0x4,
  CIA1_IRQ_SERIAL = 0x8,
  CIA1_IRQ_DATASETTE = 0x10,
  CIA1_IRQ_PENDING = 0x80,
  
  CIA2_IRQ_TMRA = 0x1,
  CIA2_IRQ_TMRB = 0x2,
  CIA2_IRQ_TOD  = 0x4,
  CIA2_IRQ_SERIAL = 0x8,
  CIA2_IRQ_DATASETTE = 0x10,
  CIA2_IRQ_PENDING = 0x80,
  
  VIC_IRQ_RASTER = 0x1,
  VIC_IRQ_SPRITE_BG = 0x2,
  VIC_IRQ_SPRITE_SPRITE = 0x4,
};
/* .CRT Format
 * 0000 'C64 CARTRIDGE'
 * 0020 Game Name
 * 0040 'CHIP'
 * 004C Load Address (8000)
 * 004E Length
 * 0050 Start address
 */

/* Memory map
 * 0001-0001 Memory Map config
 * 0400-07FF Screen Memory 1000 bytes   (40x25)
 * D800-DBE7 Color RAM     1000 nybbles (40x25) ----cccc
 *
 * 8000-9FFF RAM CARTLO
 * A000-BFFF RAM CARTHI   BASIC
 * C000-CFFF RAM
 * D000-DFFF RAM CHARROM  IO
 * E000-FFFF RAM CARTHI   KERNAL
 *
 * -------- VIC registers
 * D000 SPRITE_X
 * D001 SPRITE_Y
 * ....
 * D010 SPRITE_X8             |M7X8|M6X8|M6X8|M6X8|M6X8|M6X8|M6X8|M6X8|
 * D011 CTRL1                 |RST8|ECM |BMM |DEN |RSEL|YSCROLL       |
 * D012 RASTER                |RST7|RST6|RST5|RST4|RST3|RST2|RST1|RST0|
 * D013 LIGHTPEN_X
 * D014 LIGHTPEN_Y
 * D015 SPRITE_EN             |M7E |M7E |M7E |M7E |M7E |M7E |M7E |M7E |
 * D016 CTRL2                 |    |    |RES |MCM |CSEL|XSCROLL       |
 * D017 SPRITE_Y2
 * D018 MEMPTR                |VM13|VM12|VM11|VM10|CB13|CB12|CB11|    |
 * D019 INTSTS                |EVT |              |LP  |SSCL|SBCL|RSTR|
 * D01A INTEN
 * D01B SPRITE_PRI
 * D01C SPRITE_MC
 * D01D SPRITE_X2
 * D01E SPRITE_SPR_COLL      Sprite-sprite collision
 * D01F SPRITE_BG_COLL       Sprite-background collision
 * D020 BORDER_CLR           ----cccc
 * D021 BG0_CLR              ----cccc
 * D022 BG1_CLR              ----cccc
 * D023 BG2_CLR              ----cccc
 * D024 BG3_CLR              ----cccc
 * D025 SPRITE_MM0           ----cccc
 * D026 SPRITE_MM1           ----cccc
 * D027 SPRITE_CLR 0
 * D028 SPRITE_CLR 1
 * D029 SPRITE_CLR 2
 * D02A SPRITE_CLR 3
 * D02B SPRITE_CLR 4
 * D02C SPRITE_CLR 5
 * D02D SPRITE_CLR 6
 * D02E SPRITE_CLR 7
 * D02F ... unusable
 *
 * D040-D3FF mirror
 *
 * SID
 * ---- Voice 1
 * D400 Freq Lo               ffffffff
 * D401 Freq Hi               ffffffff
 * D402 Duty Lo               dddddddd
 * D403 Duty Hi               ----dddd
 * D404 Control               npstTRSG
 * D405 Attack/decay          aaaadddd
 * D406 Sustain/release       ssssrrrr
 * ---- Voice 2
 * D407 Freq Lo               ffffffff
 * D408 Freq Hi               ffffffff
 * D409 Duty Lo               dddddddd
 * D40a Duty Hi               ----dddd
 * D40b Control               npstTRSG
 * D40c Attack/decay          aaaadddd
 * D40d Sustain/release       ssssrrrr
 * ---- Voice 3
 * D40e Freq Lo               ffffffff
 * D40f Freq Hi               ffffffff
 * D410 Duty Lo               dddddddd
 * D411 Duty Hi               ----dddd
 * D412 Control               npstTRSG
 * D413 Attack/decay          aaaadddd
 * D414 Sustain/release       ssssrrrr
 * D500..D7FF mirrored
 */

/* VIC memory map. Select=CIA2.0x0A[0:1]
 * ---- Bank 0
 * 0000-0FFF RAM
 * 1000-1FFF Char ROM
 * 2000-2FFF RAM
 * 3000-3FFF RAM
 * ---- Bank 1
 * 4000-4FFF RAM
 * 5000-5FFF RAM
 * 6000-6FFF RAM
 * 7000-7FFF RAM
 * ---- Bank 2
 * 8000-8FFF RAM
 * 9000-9FFF Char ROM
 * A000-AFFF RAM
 * B000-BFFF RAM
 * ---- Bank 3
 * C000-CFFF RAM
 * D000-DFFF RAM
 * E000-EFFF RAM
 * F000-FFFF RAM
 */

#define EX 40
int EY=52;
int SPRY=0;

#define SCREEN_XRES (320 + 2*EX) //504
#define SCREEN_YRES (200 + 2*EY) //312

enum {
  SPRITE_X   = 0xD000, // 53248 xxxxxxxx  Sprite X
  SPRITE_Y   = 0xD001, // 53249 yyyyyyyy  Sprite Y

  SPRITE_SPR = 0xD01E,
  BG0_CLR    = 0xD021, 
  
  SPRITE_PTR = 2040,   // 2040-2047 or screen_ram+0x3f8

  COLORRAM_BASE = 0xD800,

  // Screen color modes
  ECM = 0x40,
  BMM = 0x20,
  MCM = 0x10,
};
/* Sprites 24x21
 *  Mem Pointer @ 0800..0807
 *  Sprite Data:  SPRITE_PTR[n] * 64
 */

static int frame, scanline;

extern uint64_t totcyc;

#define RASTER scanline

int sli[512];

volatile uint16_t& ioreg16(int n);
volatile uint8_t&  ioreg(int n);

#define IOREG(x) ioregs[(x) & 0xFFF]
#define IOPTR8(name, addr)  volatile uint8_t *name = &ioregs[(addr) & 0xFFF]
#define IOREG8(name, addr)  volatile uint8_t& name = ioregs[(addr) & 0xFFF]
#define IOREG16(name, addr) volatile uint16_t& name = *(uint16_t *)&ioregs[(addr) & 0xFFF]

void flogger(int lvl, const char *fmt, ...)
{
  va_list ap;
  FILE *fp;

  if (lvl == 134 || trace) {
    va_start(ap, fmt);
    fp = stdout;
    fprintf(fp, "%.4x frame:%8d/%3d %10lld: ", SPC, frame, scanline, totcyc);
    vfprintf(fp,fmt, ap);
    va_end(ap);
  }
}

enum {
  NONE, RAM, ROMLO, ROMHI, BASIC, KERNAL, IOR, CHAR
};

const char *mapregvals[] = {
  "none",
  "ram",
  "rlo",
  "rhi",
  "bsc",
  "knl",
  "ior",
  "chr"
};

const char *comment(int addr) {
  if (addr == -1)
    return "";
  if (addr >= 0xD000 && addr <= 0xD3FF) {
    addr &= 0xFF3F;
  }
  if (addr >= 0xDC00 && addr <= 0xDDFF) {
    addr &= 0xFF0F;
  }
  switch (addr) {
#define o(addr, name, x...) case addr: return #name;
    CHIPREG(o)
#undef o
      };
  return ">>";
}

/* memory bankswitch
 * 8000-9FFF ram,romlo
 * A000-BFFF ram,romhi,basic,none
 * D000-DFFF ram,char,ior
 * E000-FFFF ram,kernal,romhi
 *
 * bits are EXROM,GAME,CHAREN,HIRAM,LORAM
 * EXROM/GAME, from CRT header, default = 11
 * CHAREN,HIRAM,LOROM = ram[0x01] bits 2-0, default = 111
 */
struct bmap_t {
  uint8_t bank8, bankA, bankD, bankE;
} bankmap[] = {
  // EXROM=0, GAME=0 (usual games)
  { RAM,   RAM,   RAM,   RAM },
  { RAM,   RAM,   RAM,   RAM },
  { RAM,   ROMHI, CHAR,  KERNAL },
  { ROMLO, ROMHI, CHAR,  KERNAL },
  { RAM,   RAM,   RAM,   RAM },
  { RAM,   RAM,   IOR,   RAM },
  { RAM,   ROMHI, IOR,   KERNAL },
  { ROMLO, ROMHI, IOR,   KERNAL },
  // EXROM=0, GAME=1 (some games)
  { RAM,   RAM,   RAM,   RAM },
  { RAM,   RAM,   CHAR,  RAM },
  { RAM,   RAM,   CHAR,  KERNAL },
  { ROMLO, BASIC, CHAR,  KERNAL },
  { RAM,   RAM,   RAM,   RAM },
  { RAM,   RAM,   IOR,   RAM },
  { RAM,   RAM,   IOR,   KERNAL },
  { ROMLO, BASIC, IOR,   KERNAL },
  // EXROM=1, GAME=0
  { ROMLO, NONE,  IOR,   ROMHI },
  { ROMLO, NONE,  IOR,   ROMHI },
  { ROMLO, NONE,  IOR,   ROMHI },
  { ROMLO, NONE,  IOR,   ROMHI },
  { ROMLO, NONE,  IOR,   ROMHI },
  { ROMLO, NONE,  IOR,   ROMHI },
  { ROMLO, NONE,  IOR,   ROMHI },
  { ROMLO, NONE,  IOR,   ROMHI },
  // EXROM=1, GAME=1 (no cartridge/default)
  { RAM,   RAM,   RAM,   RAM },
  { RAM,   RAM,   CHAR,  RAM },
  { RAM,   RAM,   CHAR,  KERNAL },
  { RAM,   BASIC, CHAR,  KERNAL },
  { RAM,   RAM,   RAM,   RAM },
  { RAM,   RAM,   IOR,   RAM },
  { RAM,   RAM,   IOR,   KERNAL },
  { RAM,   BASIC, IOR,   KERNAL },   // default
};

struct c64;

/* DC0D cia1 irq
 *  bit 0: timer a underflow
 *  bit 1: timer b underflow
 *  bit 2: tod == alarm
 *  bit 3: serial
 *  bit 4: datasette
 *  bit 7: interrupt pending
 *
 * DD0D: cia2 irq
 *  bit 0: timer a underflow
 *  bit 1: timer b underflow
 *  bit 2: tod == alarm
 *  bit 3: serial
 *  bit 4: datasette
 *  bit 7: nmi pending
 */
uint8_t getkp(c64 *c);
void setvicbase(c64 *c, int v);
void serialio(uint8_t& val, bool read);

#include "cia.h"
struct cia64_t : public cia_t {
  int id;

  void Setreg(c64 *, int n, uint8_t v);
  uint8_t Getreg(c64 *, int n);
  void init(int _id, uint8_t *r, const char *name) {
    id = _id;
    regs = r;
    cia_irq.name = name;
  };
};

void cia64_t::Setreg(c64 *c, int n, uint8_t val) {
  assert(n <= 0xF);
  cia_t::setreg(n, val);
  if (n == PRA && id == 2) {
      setvicbase(c, val);
      serialio(val, false);
  };
}

uint8_t cia64_t::Getreg(c64 *c, int n) {
  assert(n <= 0xF);
  uint8_t val = cia_t::getreg(n);
  if (n == PRA && id == 2) {
    serialio(val, true);
  }
  if (n == PRB && id == 1) {
    val = getkp(c);
  }
  return val;
};

struct sprite_t {
  int      state = 0;
  int      sx;
  int      sy;
  int      ex;
  int      ey;
  int      pri;
  int      mc;
  int      sp;
  int      clr[4];

  bool     load(c64 *c, int n, int y);
};

struct c64 : public bus_t {
  uint8_t *basic;
  uint8_t *charset;
  uint8_t *kernal;

  // Cartridge ROM
  int      cart_type;
  uint8_t *cartrom;
  size_t   romsz;

  // Bankio regions
  banksy   b8;
  banksy   ba;
  banksy   be;
  
  uint8_t *prg;
  uint32_t prgsize;
  uint16_t prgaddr;

  // Bankswitch setting
  bmap_t   bmap;
  uint8_t  ram[65536];

  // IO Regs D000-DFFF
  uint8_t  ioregs[4096];

  // Get some pointers to common area
  uint16_t  chrbase;
  uint8_t  *screen_ram = &ram[0x0400];
  uint8_t  *color_ram  = &IOREG(COLORRAM_BASE);
  uint8_t  *sprite_ptr;//  = &ram[SPRITE_PTR];
  int cols = 40;
  int rows = 25;

  cia64_t cia1;
  cia64_t cia2;
  
  // Keyboard press
  uint8_t  key_row[8];
  uint8_t  joystate[2];

  uint16_t vicbase;
  uint8_t  smode;

  /* D019/DO1A
   * bit 0: raster
   * bit 1: sprite-bg collision
   * bit 2: sprite-sprite collision
   * bit 3: light-pen
   */
  irq_t    vic_irq;

  /* Line to generate Raster IRQ */
  int      raster_irq;

  uint8_t  exgame = 0x3 << 3;
  uint8_t  mapreg;

  sprite_t xspr[8];

  /* VIC */
  IOREG8(sprite_en,  0xD015); // 53270 76543210  Sprite enable
  IOREG8(sprite_x8,  0xD010); // 53264 76543210  Sprite X8 bit
  IOREG8(sprite_ey,  0xD017); // 53271 76543210  Sprite X2
  IOREG8(sprite_ex,  0xD01D); // 53277 76543210  Sprite Y2
  IOREG8(sprite_pri, 0xD01B); // 53275 76543210  Sprite Priority
  IOREG8(sprite_mc,  0xD01C); // 53276 76543210  Sprite Multicolor
  IOREG8(sprite_mm0, 0xD025); // 53285 ----cccc  Sprite Multicolor 0
  IOREG8(sprite_mm1, 0xD026); // 53286 ----cccc  Sprite Multicolor 1
  IOPTR8(sprite_clr, 0xD027); // 53287 ----cccc  Sprite Color 0-7
  
  IOREG8(border_clr, 0xD020); // 53280 ----cccc  Border color
  IOPTR8(bg_clr,     0xD021); // 53281 ----cccc  Background 0-3 color
  
  IOREG8(ctrl1,      0xD011);
  IOREG8(ctrl2,      0xD016);

  IOREG8(memptr,     0xD018);
  
  /* CIA1 */
  IOREG8(pra,        0xDC00);
  IOREG8(prb,        0xDC01);
  IOREG8(ddra,       0xDC02);
  IOREG8(ddrb,       0xDC03);

  IOREG8(rtc1_ds,    0xDC08);
  IOREG8(rtc1_s,     0xDC09);
  IOREG8(rtc1_m,     0xDC0A);
  IOREG8(rtc1_h,     0xDC0B);

  /* CIA2 */
  IOREG8(rtc2_ds,    0xDD08);
  IOREG8(rtc2_s,     0xDD09);
  IOREG8(rtc2_m,     0xDD0A);
  IOREG8(rtc2_h,     0xDD0B);

  void load(const char *romfile);
  void run(const char *prgfile);
  void ppu_tick();
  void drawline(int y);
  void drawsprite(int n, int y);
  void drawtile(int tx, int ty, int id, int bpp, int *clr, int line = -1);
  
  void drawframe();
  int  scrmode();
  
  int YSCROLL() {
    return ctrl1 & 7;
  }
  int XSCROLL() {
    return ctrl2 & 7;
  }
  bool is_badline(int n) {
    int ys = 0;
    return (n >= 0x30 && n <= 0xf7 && ((n & 7) == ys));
  };
  c64();
  uint8_t *charram(int offset);
  
  int cia1io(uint32_t offset, const int mode, iodata_t& val);
  int cia2io(uint32_t offset, const int mode, iodata_t& val);
  int vicio(uint32_t offset, const int mode, iodata_t& val);

  apu_t *apu;
  
  int sidio(uint32_t offset, const int mode, iodata_t& val);
  void sid_init();
  void sid_tick();
};

const char *scrmodes[] = {
  "none",
  "char",
  "mcchar",
  "bitmap",
  "mcbitmap",
  "extbg",
};

enum {
  mode_none,
  mode_char,
  mode_mcchar,
  mode_bitmap,
  mode_mcbitmap,
  mode_extbg,
};


/*  k  Axx Dxx Exx
 * 000 ram,ram,ram
 * 001 ram,chr,ram
 * 010 ram,chr,knl
 * 011 bsc,chr,knl
 * 100 ram,ram,ram
 * 101 ram,ior,ram
 * 110 ram,ior,knl
 * 111 bsc,ior,knl
 */

/* charset:
 * 000-07f A
 * 080-0ff reverse A
 * 100-17f a
 * 180-1ff reverse a
 * 1c1-
 */

bool is2(uint8_t v) {
  return v && !(v & (v - 1));
}

void setvicbase(c64 *c, int val) {
  c->vicbase = (~val & 3) << 14;
}

/* Get keypress result */
uint8_t getkp(c64 *c) {
  uint8_t ret, tst;

  ret = ~c->ddrb;
  tst = ~(c->pra | ~c->ddra);
  for (int i = 0; i < 8; i++) {
    if (tst & (1L << i))
      ret &= ~c->key_row[i];
  }
  return ret & ~c->joystate[0];
}

#ifndef _SDL
const char petscii[] =
  "@ABCDEFGHIJKLMNO"
  "PQRSTUVWXYZ[_]__"
  " !\"#$%&'()*+,-./"
  "0123456789:;<=>?";
#endif

void setmapreg(c64 *c, uint8_t val)
{
  int bm = c->exgame + (val & 7);

  if (c->mapreg != val) {
    c->mapreg = val;
    c->bmap   = bankmap[bm];
    printf("%.4x Set Mapreg: %.2x [%s,%s,%s,%s]\n",
	   SPC, bm,
	   mapregvals[c->bmap.bank8],
	   mapregvals[c->bmap.bankA],
	   mapregvals[c->bmap.bankD],
	   mapregvals[c->bmap.bankE]);
  }
}

int zpgio(void *arg, uint32_t offset, const int mode, iodata_t& val) {
  c64 *c = (c64 *)arg;

  memio(c->ram, offset, mode, val);
  if (offset == 0x01 && mode == 'w') {
    setmapreg(c, val);
    
  }
  if (offset == 0x02) {
    if (c->prg && mode == 'w' && val == 3) {
      trace = 133;
      memcpy(&c->ram[c->prgaddr], c->prg+2, c->prgsize-2);
      if (c->prgaddr == 0x0801) {
	cpu_reset(0x0816);
      }
    }
  }
  return 0;
}

const char *regname(int offset) {
  switch (offset) {
#define o(addr, name, x...) case addr: return #name;
    CHIPREG(o)
#undef o
      }
  return "";
}

/* D000-D3FF VIC-II */
#define VIC_READ(x)  (x)
#define VIC_WRITE(x) ((x)+0x10000)

int vf[64] = { 0 };

int c64::vicio(uint32_t offset, const int mode, iodata_t& val) {
  memio(ioregs, offset & 0xF3F, mode, val);
  if (mode == 'w') {
    offset |= 0x10000;
  }
  vf[0x12] = 1;
  switch (offset) {
  case VIC_WRITE(0xD001): xspr[0].state = 1; break;
  case VIC_WRITE(0xD003): xspr[1].state = 1; break;
  case VIC_WRITE(0xD005): xspr[2].state = 1; break;
  case VIC_WRITE(0xD007): xspr[3].state = 1; break;
  case VIC_WRITE(0xD009): xspr[4].state = 1; break;
  case VIC_WRITE(0xD00b): xspr[5].state = 1; break;
  case VIC_WRITE(0xD00d): xspr[6].state = 1; break;
  case VIC_WRITE(0xD00f): xspr[7].state = 1; break;
    break;
  case VIC_READ(0xD011):
    /* Read CTRL1 */
    val = (val & 0x7F) | ((scanline  & 0x100) >> 1);
    break;
  case VIC_READ(0xD012):
    /* Read RASTER */
    val = scanline;
    break;
  case VIC_WRITE(0xD012):
    /* Write Raster IRQ */
    raster_irq = (raster_irq & 0xFF00) | val;
    sli[raster_irq] = vic_irq.en & VIC_IRQ_RASTER;
    break;
  case VIC_WRITE(0xD011):
    /* Write CTRL1 */
    smode = scrmode();
    fprintf(stdout, "set ctrl1: %.2x[%s] rst8:%x ecm:%x bmm:%x den:%x rsel:%x yscroll:%x\n",
	    val, scrmodes[smode],
	    !!(val & 0x80), !!(val & 0x40), !!(val & 0x20), !!(val & 0x10),
	    !!(val & 0x08), (val & 0x07));
    raster_irq = (raster_irq & 0x00FF) | ((val & 0x80) << 1);
    sli[raster_irq] = vic_irq.en & VIC_IRQ_RASTER;
    rows = (val & 0x08) ? 25 : 24;
    EY   = (val & 0x08) ? 51 : 55;
    break;
  case VIC_WRITE(0xD016):
    /* Write CTRL2 */
    smode = scrmode();
    fprintf(stdout, "set ctrl2: %.2x[%s] res:%x mcm:%x csel:%x xscroll:%x\n",
	    val, scrmodes[smode], !!(val &  0x20), !!(val & 0x10), !!(val & 0x08),
	    val & 7);
    cols = (val & 0x08) ? 40 : 38;
    break;
  case VIC_WRITE(0xD018):
    fprintf(stdout,"%4d Set Pointer: sb:%.4x cb:%.4x\n", scanline, ((val & 0xF0) << 6), ((val & 0x0E) << 10));
    break;
  case VIC_WRITE(0xD019):
    /* ACK interrupt */
    vic_irq.clear(val & 0xF);
    flogger(134,"Set VIC IRQSTS: %.2x\n", val);
    break;
  case VIC_READ(0xD019):
    val = (vic_irq.sts & 0xF);
    if (val) {
      val |= 0x80;
    }
    val |= 0x70;
    flogger(134,"Get VIC IRQSTS: %.2x\n", val);
    break;
  case VIC_WRITE(0xD01A):
    vic_irq.en = val;
    flogger(134,"Set VIC IRQEN: %.2x\n", val);
    break;
  case VIC_READ(0xD01A):
    val = 0xF0 | vic_irq.en;
    flogger(134,"Get VIC IRQEN: %.2x\n", val);
    break;
  default:
    if ((offset & 0xFFFF) >= 0xD02F) {
      val = 0xFF;
    }
    break;
  }
  if (((mode == 'r') && (vf[offset & 0x3F] & 1) == 0) ||
      ((mode == 'w') && (vf[offset & 0x3F] & 2) == 0)) {
    printf("%.4x %-4d %-4d !!%-6s:%.4x %.2x(%4d) %s\n",
	   SPC, scanline, raster_irq, offset > 0x10000 ? "write" : "read",
	   offset & 0xFFFF, val, val, regname(offset & 0xffff));
  }
  return 0;
}

/* DC00-DCFF CIA1 */
#define CIA_READ(x)  (x)
#define CIA_WRITE(x) ((x)+0x10000)

int rdfreq(int port)
{
  int freq = ioreg16(port);

  if (freq == 0)
    return 65536;
  return freq;
}

/* DC00: r  pra
 * DC01: r  prb
 * DC02: r  ddra     gpio direction (0=ro, 1=rw)
 * DC03: r  ddrb     gpio direction (0=ro, 1=rw)
 * DC04: rw talo     
 * DC05: rw tahi
 * DC06: rw tblo
 * DC07: rw tbhi
 * DC08: rw tod10ths rd: 0000xxxx  wr: ___xxxx : CRB.7=0: x=time tod10ths, CRB.7=1: x=alarm tod10ths
 * DC09: rw todsec   rw: 0xxxxxxx
 * DC0A: rw todmin   rw: 0xxxxxxx
 * DC0B: rw todhr    rw: Zxxxxxxx Z=0: AM, Z=1: PM
 * DC0C: rw sdr
 * DC0D: rw icr
 * DC0E: rw cra
 * DC0F: rw crb
 */
int c64::cia1io(uint32_t offset, const int mode, iodata_t& val) {
  printf("%5s:%.4x %.2x[%4d] %s\n", mode == 'r' ? "read" : "write",offset, val, val, regname(offset));
  if (mode == 'w') {
    cia1.Setreg(this, offset & 0xF, val);
  }
  else {
    val = cia1.Getreg(this, offset & 0xF);
  }
  return 0;
}

enum {
  sIDLE,
  sATN,
  sTX,
  sRX,
};

#define RX    0x80
#define RXCLK 0x40
#define TX    0x20
#define TXCLK 0x10
#define ATN   0x08
#define DATA  0x04

/* 0:0v = true/down
 * 1:5v = false/released
 * data valid on rising edge of clock
 *
 * 3:atn out
 * 4:clk out
 * 5:data out
 * 6:clk in
 * 7:data in
 */
void serialio(uint8_t& val, bool read)
{
  static int state = sIDLE;
  static int oldval;
  
  switch (state) {
  case sIDLE:
    printf("IDLE state\n");
    if ((val & ATN) && !(oldval & ATN)) {
      oldval = val & ~3;
      state = sATN;
      printf("Attention bit set!!!\n");
    }
    break;
  case sATN:
    printf("ATN state: %x %d, %d, %d\n", val, !!(val & TX), !!(val & RX), !!(val & DATA));
    if (read)
      val ^= 0x80;
    break;
  };
}

/* DD00: r  pra
 * DD01: r  prb
 * DD02: r  ddra     gpio direction (0=ro, 1=rw)
 * DD03: r  ddrb     gpio direction (0=ro, 1=rw)
 * DD04: rw talo     
 * DD05: rw tahi
 * DD06: rw tblo
 * DD07: rw tbhi
 * DD08: rw tod10ths rd: 0000xxxx  wr: ___xxxx : CRB.7=0: x=time tod10ths, CRB.7=1: x=alarm tod10ths
 * DD09: rw todsec   rw: 0xxxxxxx
 * DD0A: rw todmin   rw: 0xxxxxxx
 * DD0B: rw todhr    rw: Zxxxxxxx Z=0: AM, Z=1: PM
 * DD0C: rw sdr
 * DD0D: rw icr
 * DD0E: rw cra
 * DD0F: rw crb
 */
int c64::cia2io(uint32_t offset, const int mode, iodata_t& val) {
  printf("%5s:%.4x %.2x[%4d] %s\n", mode == 'r' ? "read" : "write",offset, val, val, regname(offset));
  if (mode == 'w') {
    cia2.Setreg(this, offset & 0xF, val);
  }
  else {
    val = cia2.Getreg(this, offset & 0xF);
  }
  return 0;
}

/* Format of each voice:
 *  00 freq lo
 *  01 freq hi
 *  02 duty lo
 *  03 duty hi
 *  04 control
 *  05 attack|decay
 *  06 sustain|release
 */
#define SID_READ(x)  (x)
#define SID_WRITE(x) ((x)+0x10000)

#define SNDHZ  985248.0
#define MSHZ   1000.0
#define FR(n)  (int)((n) * SNDHZ / MSHZ)

#define SID_FREQLO   0x0
#define SID_FREQHI   0x1
#define SID_DUTYLO   0x2
#define SID_DUTYHI   0x3
#define SID_CTRL     0x4
#define SID_AAAADDDD 0x5
#define SID_SSSSRRRR 0x6

#define SFREQ(r) (int)((r)*(SNDHZ/44100))

struct sid_voice {
  bool     enabled;
  int      offset;
  int      id;
  int      state;
  int      ctrl;
  uint32_t level;
  uint32_t count;
} sid_voices[4];

enum {
  OFF,
  ATTACK,
  DECAY,
  SUSTAIN,
  RELEASE
};

int sid_attack(c64 *c, int base) {
  return ioreg(base + SID_AAAADDDD) >> 4;
}
int sid_decay(c64 *c, int base) {
  return ioreg(base + SID_AAAADDDD) & 0xF;
}
int sid_sustain(c64 *c, int base) {
  return ioreg(base + SID_SSSSRRRR) >> 4;
}
int sid_release(c64 *c, int base) {
  return ioreg(base + SID_SSSSRRRR) & 0xF;
}
uint32_t sid_freq(c64 *c, int base) {
  return ioreg16(base + SID_FREQLO);
}
int sid_duty(c64 *c, int base) {
  return ioreg16(base + SID_DUTYLO) & 0xFFF;
}
int sid_control(c64 *c, int base) {
  return ioreg16(base + SID_CTRL);
}

/*
 *  A  D   S    R
 *----+-+-----+---
 *    /\
 *   /  \______
 *  /          \
 * /   	        \
 *================
 */
const uint32_t rates[16] = {
  /* ms per attack.  decay/release = 3X */
  /* sustain is on/off via gate */
  2, 8, 16, 24, 38, 56, 68, 80, 100, 250, 500, 800, 1000, 3000, 5000, 8000
};

void setvoice(sid_voice *v, int ctrl)
{
  if ((v->ctrl & 1) == 0 && (ctrl & 1) != 0) {
    printf("  SID%d VOICE ON\n", v->id);
    v->state = ATTACK;
    v->ctrl = 1;
  }
  else if ((v->ctrl & 1) != 0 && (ctrl & 1) == 0) {
    printf("  SID%d VOICE OFF\n", v->id);
    v->state = RELEASE;
    v->ctrl = 0;
  }
}

void tickvoice(sid_voice *v)
{
  int r;

  return;
  if (!v->enabled)
    return;
  if (v->count > 0)
    v->count--;
  if (v->count != 0)
    return;
  printf("%10lld sid%d:state:%d expired\n", totcyc, v->id, v->state);
  switch (v->state) {
  case ATTACK:
    r = sid_decay(NULL, v->offset);
    v->count = 3 * FR(rates[r]);
    v->state = DECAY;
    break;
  case DECAY:
    v->state = SUSTAIN;
    break;
  case SUSTAIN:
    r = sid_control(NULL, v->offset);
    printf("sid%d:tick:sustain = %.2x\n", v->id, r);
    setvoice(v, RELEASE);
    break;
  case RELEASE:
    printf("sid%d:turn off sound\n", v->id);
    v->enabled = false;
    break;
  }
}

struct sidsound : public soundgen_t {
  int tick() {
    return 0;
  };
  int sample(int ch) {
    return 0;
  };
};

/* CPU Clock rate is 1.022727 MHz
 * SID assumes 1MHz
 *
 * SID rate is 985248Hz
 */
int c64::sidio(uint32_t offset, const int mode, iodata_t& val) {
  sid_voice *pv = NULL;
  int r;
  
  memio(ioregs, offset & 0xFFF, mode, val);
  if (mode == 'r') {
    val = 0xFF;
    if (offset >= 0xD41B && offset <= 0xD41C)
      val = rand();
    return 0;
  }
  printf("write:%.4x %.2x[%4d] %s\n", offset, val, val, regname(offset));
  if (offset <= 0xD406)
    pv = &sid_voices[0];
  else if (offset <= 0xD40D)
    pv = &sid_voices[1];
  else if (offset <= 0xD414)
    pv = &sid_voices[2];
  switch (offset) {
  case 0xD400: case 0xD401:
  case 0xD407: case 0xD408:
  case 0xD40e: case 0xD40f:
    r = sid_freq(this, pv->offset);
    fprintf(stdout, "sid%d: freq:%6d %6d\n", pv->id, r, SFREQ(r));
    return 0;
  case 0xD402: case 0xd403:
  case 0xd409: case 0xd40a:
  case 0xd410: case 0xd411:
    fprintf(stdout, "sid%d: duty:%d\n", pv->id, sid_duty(this, pv->offset));
    return 0;
  case 0xd405:
  case 0xd40c:
  case 0xd413:
    fprintf(stdout, "sid%d: attack:%d decay:%d\n", pv->id, (val >> 4), val & 0xF);
    return 0;
  case 0xd406:
  case 0xd40d:
  case 0xd414:
    fprintf(stdout, "sid%d: sustain:%d release:%d\n", pv->id, (val >> 4), val & 0xF);
    return 0;
  }
  if (pv == NULL)
    return 0;
  fprintf(stdout,"sid voice:%d [%c%c%c%c] freq:%6d duty:%6d a:%2d d:%2d s:%2d r:%2d gate:%.2x %d\n",
	  pv->id,
	  val & 0x80 ? 'n' : ' ',
	  val & 0x40 ? 'p' : ' ',
	  val & 0x20 ? 's' : ' ',
	  val & 0x10 ? 't' : ' ',
	  sid_freq(this, pv->offset),
	  sid_duty(this, pv->offset),
	  sid_attack(this, pv->offset),
	  sid_decay(this, pv->offset),
	  sid_sustain(this, pv->offset),
	  sid_release(this, pv->offset),
	  val, pv->state);
  setvoice(pv, val);
  return 0;
}

void c64::sid_tick()
{
  if (apu) {
    apu->tick();
  }
}

void c64::sid_init()
{
  for (int i = 0; i < 3; i++) {
    sid_voices[i].id = i;
    sid_voices[i].offset = 0xD400 + (i * 7);
    sid_voices[i].enabled = false;
  }
  for (int i = 0; i < 16; i++) {
    printf("rates: ");
    if (rates[i]) {
      printf(" %d", (uint32_t)(SNDHZ / 1000.0) * rates[i]);
    }
    printf("\n");
  }
  //apu = new apu_t(sndHz / sampHz, new sidsound);
}

/* Ram expansion unit:
 * DF00 |irq|eob|err|sz |     version   |  7-5 cleared on read
 * DF01 |trg|   |in0|now|   |   |  dir  |
 * DF02 C64 RAM address lo
 * DF03 C64 RAM address hi
 * DF04 REU address lo
 * DF05 REU address hi
 * DF06 REU address bank
 * DF07 length lo
 * DF08 length hi
 * DF0A |c64|reu|   |   |   |   |   |   | fix address
 */
int reu_io(c64 *c, uint32_t offset, const int mode, iodata_t& val) {
  return 0;
}

/* 8000-9FFF: RAM or ROMLO */
int bank8x(void *arg, uint32_t offset, const int mode, iodata_t& val) {
  c64 *c = (c64 *)arg;
  uint8_t *ptr;

  if (c->cart_type == 18) {
    setbank(&c->ba, (offset >= 0x1000), "Zaxxon.A");
    offset &= 0xFFF;
  }

  /* RAM pointer */
  ptr = &c->ram[0x8000];
  if (mode == 'r' && (c->bmap.bank8 == ROMLO)) {
    ptr = c->b8.current;
  }
  return memio(ptr, offset, mode, val);
}

/* A000-BFFF: RAM, ROMHI or BASIC */
int bankAx(void *arg, uint32_t offset, const int mode, iodata_t& val) {
  c64 *c = (c64 *)arg;
  uint8_t *ptr;

  /* All writes go to RAM */
  ptr = &c->ram[0xA000];

  /* Basic/romhi are readonly */
  if (mode == 'r' && (c->bmap.bankA == BASIC)) {
    ptr = c->basic;
  }
  else if (mode == 'r' && (c->bmap.bankA == ROMHI)) {
    ptr = c->ba.current;
  }
  return memio(ptr, offset, mode, val);
}

/* E000-FFFF: RAM, ROMHI or KERNEL */
int bankEx(void *arg, uint32_t offset, const int mode, iodata_t& val) {
  c64 *c = (c64 *)arg;
  uint8_t *ptr;

  /* All writes go to RAM */
  ptr = &c->ram[0xE000];

  /* kernal/romhi are readonly */
  if (mode == 'r' && (c->bmap.bankE == KERNAL)) {
    ptr = c->kernal;
  }
  else if (mode == 'r' && (c->bmap.bankE == ROMHI)) {
    ptr = c->be.current;
  }
  return memio(ptr, offset, mode, val);
}

/* D000-DFFF: RAM or IO or CHARSET */
int bankDx(void *arg, uint32_t offset, const int mode, iodata_t& val) {
  c64 *c = (c64 *)arg;
  uint8_t *ptr;

  /* Default to RAM */
  ptr = c->ram;
  if (c->bmap.bankD == IOR) {
    /* D000 - D3FF : vic
       D400 - D7FF : sid
       D800 - DBFF : colorram
       DC00 - DCFF : cia1
       DD00 - DDFF : cia2
    */
    if (offset >= 0xD000 && offset <= 0xD3FF)
      return c->vicio(offset & 0xFF3F, mode, val);
    if (offset >= 0xD400 && offset <= 0xD7FF)
      return c->sidio(offset & 0xFF3F, mode, val);
    if (offset >= 0xDC00 && offset <= 0xDCFF)
      return c->cia1io(offset & 0xFF0F, mode, val);
    if (offset >= 0xDD00 && offset <= 0xDDFF)
      return c->cia2io(offset & 0xFF0F, mode, val);
    ptr = c->ioregs;
    offset &= 0xFFF;
  }
  else if (c->bmap.bankD == CHAR && mode == 'r') {
    // CHARSET is readonly
    ptr = c->charset;
    offset &= 0xFFF;
  }
  return memio(ptr, offset, mode, val);
}

c64::c64() : bus_t(0xFFFF)
{
}

int set19(void *d, uint32_t addr, int mode, iodata_t& val)
{
  banksy *b = (banksy *)d;

  printf("%.4x: BANKSY %x\n", SPC, val);
  if (val <= 7) {
    setbank(b, val, "8.19");
  }
  return 0;
}

/* Bamkswtich methods 
 * https://codebase64.org/doku.php?id=base:crt_file_format
 *
 * Type 5: (16, 32 or 64 banks)
 *   8000-9FFF: Bank 00-15
 *   A000-BFFF: Bank 16-31
 *   Bank set:  DE00
 * Type 18: Zaxxon
 *   8000-8FFF 4k ROM
 *   9000-9FFF mirrored 4k ROM
 *   A000-Bfff 8k banked
 *   READ 8000-8FFF : bank 0
 *   READ 9000-9FFF : bank 1
 * Type 19: Magic Desk, 4-16 8k banks
 *   8000-9FFF (banks 00-15), write to DE00
 */
void c64::load(const char *romfile)
{
  size_t sz;

  /* A000-BFFF: 8k */
  basic = loadrom("c64_rom/basic.bin", sz);

  /* D000-DFFF: 4k */
  charset = loadrom("c64_rom/charset.bin", sz);
  
  /* E000-FFFF: 8k */
  kernal = loadrom("c64_rom/kernal.bin", sz);

  exgame = 0x3 << 3;

  /* Setup memory handlers */
  register_handler(0x0000, 0x0001, 0xFFFF, zpgio,  this, _RW, "REGIO");
  register_handler(0x0002, 0x7FFF, 0xFFFF, memio,  ram,  _RW, "RAM");
  register_handler(0x8000, 0x9FFF, 0x1FFF, bank8x, this, _RW, "RAM:ROMLO");
  register_handler(0xA000, 0xBFFF, 0x1FFF, bankAx, this, _RW, "RAM:ROMHI:BASIC");
  register_handler(0xC000, 0xCFFF, 0xFFFF, memio,  ram,  _RW, "RAM");
  register_handler(0xD000, 0xDFFF, 0xFFFF, bankDx, this, _RW, "RAM:CHARSET:IO");
  register_handler(0xE000, 0xFFFF, 0x1FFF, bankEx, this, _RW, "RAM:ROMHI:KERNAL");

  /* Load ROM cartridge */
  romsz = 0;
  if (romfile != NULL) {
    cartrom = loadrom(romfile, romsz);

    if (cartrom != NULL) {
      cart_type = get16be(cartrom + 0x16);
      printf(" name  : %s\n", cartrom+0x20);
      printf(" ver   : %d.%d\n", cartrom[0x14], cartrom[0x15]);
      printf(" type  : %d\n",  cart_type);
      printf(" exrom : %d\n",  cartrom[0x18]);
      printf(" game  : %d\n",  cartrom[0x19]);
      exgame = ((cartrom[0x18] & 1) << 4) | ((cartrom[0x19] & 1) << 3);

      b8.init(256, "8000");
      ba.init(256, "A000");
      be.init(256, "E000");
      if (cart_type == 19) {
	register_handler(0xDE00, 0xDE01, 0x01, set19, &b8, _WR, "type19.sel");
      }
      for (uint32_t off = get32be(cartrom + 0x10); off < romsz; ) {
	uint16_t addr = get16be(cartrom + off + 0x0c);
	uint16_t size = get16be(cartrom + off + 0x0e);
	uint16_t bank = get16be(cartrom + off + 0x0a);
	uint8_t *cptr;
	
	printf("======= %.8x %p\n", off, cartrom + off + 0x10);
	hexdump(cartrom + off, 16);
	printf(" Length: %.8x\n",  get32be(cartrom + off + 0x4));
	printf(" Type  : %x\n",    get16be(cartrom + off + 0x8));
	printf(" bank  : %x\n",    bank);
	printf(" addr  : %.4x\n",  addr);
	printf(" size  : %.4x\n",  size);

	cptr = cartrom + off + 0x10;
	if (addr == 0x8000 && size == 0x2000) {
	  printf(":set8 %d\n", bank);
	  b8.banks[bank] = cptr;
	}
	else if (addr == 0x8000 && size == 0x1000) {
	  printf(":set8.1000");
	  b8.banks[0] = cptr;
	  b8.banks[1] = cptr;
	}
	else if (addr == 0xA000 && size == 0x2000) {
	  printf(":seta %d\n", bank);
	  ba.banks[bank] = cptr;
	}
	else if (addr == 0x8000 && size == 0x4000) {
	  printf(":set8a %d\n", bank);
	  b8.banks[bank] = cptr;
	  ba.banks[bank] = cptr + 0x2000;
	}
	else {
	  printf("UNKNOWN ROM\n");
	  assert(0);
	}
	off += get32be(cartrom + off + 0x4);
      }
      setbank(&b8, 0, "b8.initial");
      setbank(&ba, 0, "ba.initial");
      setbank(&be, 0, "be.initial");
    }
  }
  setmapreg(this, 0xE7);
  ctrl1 = 0x1B;
  vic_irq.en = VIC_IRQ_RASTER;

  vic_irq.name = "vic";

  cia1.init(1, &ioregs[0xc00], "cia1");
  cia2.init(2, &ioregs[0xd00], "cia2");
  sid_init();
  //ioregs[0x11] = 0x9b;
  //ioregs[0x18] = 21;
}

/*
 * 320x200, Char 2 colors, 8 pixels per byte
 *   Screen:  0400-07FF (40x25)
 *   Charset  D000-DFFF (256*8)
 *   CharBase:D018:---ccc- 
 *   abcdefgh
 *      0 : Background 0  :D021
 *      1 : Color         :D800-DBFF
 *
 * 320x200, Bitmap 2 colors, 8 pixels per byte
 *   Screen Ram is bg/fg color
 *   Charset 0000/2000 is 1bpp
 * 
 * 160x200 MChar, 4 colors, 4 pixels per byte
 *   Screen: 0400-07FF (40x25)
 *   Charset D000-DFFF (256*8)
 *   CharBase:D018:---ccc- 
 *   aabbccdd
 *      00 : Background 0 :D021
 *      01 : Background 1 :D022
 *      10 : Background 2 :D023
 *      11 : Color        :D800-DBFF
 *
 * I/O
 * D016 Multicolor   ---m.----
 * D018 Charset base ----.bbb- 0000/0800/1000/1800/2000/2800/3000/3800
 *
 * D000 Charset Low
 * D800 Charset High
 */

#define R(x) (((x) >> 16) & 0xFF)
#define G(x) (((x) >> 8) & 0xFF)
#define B(x) (((x) >> 0) & 0xFF)

/* Commodore 64 Palette */
static uint32_t c64pal[] = {
  0x000000, // black
  0xFFFFFF, // white
  0x880000, // red
  0xAAFFEE, // cyan
  0xCC44CC, // violet/purple
  0x00CC55, // green
  0x0000AA, // blue
  0xEEEE77, // yellow
  0xDD8855, // orange
  0x664400, // brown
  0xFF7777, // light red
  0x333333, // dark gray
  0x777777, // gray 2
  0xAAFF66, // light green
  0x0088FF, // light blue
  0xBBBBBB, // light gray
};

#define  pr(x) (c64pal[x] >> 16) & 0xff
#define  pg(x) (c64pal[x] >> 8) & 0xff
#define  pb(x) (c64pal[x] >> 0) & 0xff

int pc[] = { 30, 30, 37, 88, 14, 13, 40, 4, 11, 214, 94, 204, 239, 248, 155, 33, 254 };
const char *pclr(int fg, int bg)
{
  return "";
    
  //static char cbuf[64];
  //snprintf(cbuf, sizeof(cbuf), "\033[48;5;%dm", pc[(fg & 0xF) + 1]);
  //snprintf(cbuf, sizeof(cbuf), "\033[38;2;%d;%d;%d;48;2;%d;%d;%dm",
  //pr(fg), pg(fg), pb(fg), pr(0), pg(0), pb(0));
  //return cbuf;
}

/* RealScreen = 320x200 */
Screen *screen;

// +-_-_-_-_+-_-_-_-_+-_-_-_-_+-_-_-_-_+
// +--__--__+--__--__+--__--__+--__--__+
struct bmp {
  int      w;
  int      h;
  int      sx, sy;
  int      stride;
  int      bpp;
  int      shift, mask;
  uint8_t *bits;
  bmp(int _w, int _h, int _bpp, uint8_t *_bits) {
    w   = _w;
    h   = _h;
    bpp = _bpp;
    bits = _bits;
    stride = _w / 8;

    mask = (1L << bpp) - 1;
    if (bpp == 1) {
      /* +-+-+-+-+-+-+-+-+
       * | | | | | | | | | 8 pixel per byte (2 colors)
       * +-+-+-+-+-+-+-+-+*/
      shift = 0x7;
    }
    else if (bpp == 2) {
      /* +--+--+--+--+
       * |  |  |  |  | 4 pixel per byte (4 colors)
       * +--+--+--+--+*/
      shift = 0x6;
    }
    else if (bpp == 4) {
      /* +----+----+
       * |    |    | 2 pixel per byte (16 colors)
       * +----+----+*/
      shift = 0x4;
    };
  };
  int getpixel(int x, int y) {
    if (x < 0 || y < 0 || x >= w || y >= h) {
      return 0;
    }
    /* Swap low-order bits */
    const uint8_t bb = bits[(y * stride) + (x / 8)];
    return (bb >> (~x & shift)) & mask;
  };
};

bool collide = false;

uint8_t *c64::charram(int offset)
{
  offset += chrbase;
  
  switch(offset) {
  case 0x1000 ... 0x1FFF:
  case 0x9000 ... 0x9FFF:
    return &charset[offset & 0xFFF];
  }
  return &ram[offset];
}

/* Tiles: 8x8
 * Sprites: 24x21
 *
 * tx, ty: screen coords
 * w, h  : width/height
 * sx, sy: scale
 * bpp   : bits per pixel
 * clr   : colors
 * bits  : bitmap data
 */
void drawbmp(int tx, int ty, int w, int h, uint8_t *bits, int sx, int sy, int bpp, const int *clr, int line = -1)
{
  bmp tbmp(w, h, bpp, bits);

  if (line != -1) {
    printf("Drawbmp: %d, %d\n", ty, line);
  }
  for (int y=0; y<h*sy; y++) {
    if (ty + y < EY || ty+y >= EY+200)
      continue;
    int n = (ty + y - line);
    if (line != -1 && (n < 0 || n > 7))
      continue;
    for (int x=0; x<w*sx; x++) {
      if (tx + x < EX || tx+x >= EX+320)
	continue;
      int nc = tbmp.getpixel(x/sx, y/sy);
      if (clr[nc] != -1) {
	int oc = screen->pixel(tx + x, ty + y);
	if (oc) {
	  collide = true;
	}
	screen->setpixel(tx+x, ty+y, clr[nc]);
      }
    }
  }
}

void c64::drawtile(int tx, int ty, int id, int bpp, int *clr, int line)
{
  uint8_t *bits = charram(id * 8);
  
  drawbmp(EX+tx, EY+ty, 8, 8, bits, 1, 1, bpp, clr, line);
}

/* Convert to BCD for RTC */
int bcd(int n)
{
  return ((n / 10) * 16) + (n % 10);
}

#define LSHIFT_ROW 1
#define LSHIFT_COL 7
#define CBM_ROW 7
#define CBM_COL 5

/* Keyboard mapping : 64 entries
 * DC00 : keyboard matrix col
 * DC01 : keyboard matrix row
 *
 *      |------|------|------|------|------|------|------|------|
 * PA7  | STOP |  Q   |  C=  |SPACE |  2   | CTRL | LEFT |  1   |
 * PA6  |  /   |  ^   |  =   |RSHiFT| HOME | ;    |  *   |GBP   |
 * PA5  |  ,   |  @   |  ;   |  .   |  -   |  L   |  P   |  +   |
 * PA4  |  N   |  O   |  K   |  M   |  0   |  J   |  I   |  9   |FIRE
 * PA3  |  V   |  U   |  H   |  B   |  8   |  G   |  Y   |  7   |RIGHT
 * PA2  |  X   |  T   |  F   |  C   |  6   |  D   |  R   |  5   |LEFT
 * PA1  |LSHiFT|  E   |  S   |  Z   |  4   |  A   |  W   |  3   |DOWN
 * PA0  | DOWN | F5   | F3   | F1   | F7   |  2   |RETURN|DELETE|UP
 *      |------|------|------|------|------|------|------|------|
 *                             FIRE   RIGHT  LEFT   DOWN    UP
 */

#define JOY_UP    0x01
#define JOY_DOWN  0x02
#define JOY_LEFT  0x04
#define JOY_RIGHT 0x08
#define JOY_FIRE  0x10

#define JOY_THRESH 24000

/* Joystick:
 *    DOWNLEFT: 0x06 0110
 *    DOWN:     0x02 0010
 *    DOWNRIGHT:0x0A 1010
 *    LEFT:     0x04 0100
 *    RIGHT:    0x08 1000
 *    UPLEFT:   0x05 0101
 *    UP:       0x01 0001
 *    UPRIGHT:  0x09 1001
 *    FIRE:     0x10 1.0000
 */
struct keymap {
  int ch;
  int row;
  int col;
  int shift;
} kmap[] = {
  { Key::K_DOWN,  0, 7, Key::K_UP },
  { Key::K_RIGHT, 0, 2, Key::K_LEFT },
  { Key::K_ENTER, 0, 1 },
  { Key::K_DEL,   0, 0 },
  { Key::K_F1,    0, 4 },
  { Key::K_F3,    0, 5 },
  { Key::K_F5,    0, 6 },
  { Key::K_F7,    0, 3 },

  { Key::K_WINDOWS,7, 5 },
  { Key::K_LCTRL,  7, 2 },
  { Key::K_LSHIFT, 1, 7 },
  { Key::K_RSHIFT, 6, 4 },

  { '0',   4, 3 },
  { '1',   7, 0, '!'  },
  { '2',   7, 3, '\"' },
  { '3',   1, 0, '#'  },
  { '4',   1, 3, '$'  },
  { '5',   2, 0, '%'  },
  { '6',   2, 3, '&'  },
  { '7',   3, 0, '\'' },
  { '8',   3, 3, '('  },
  { '9',   4, 0, ')'  },

  { ' ',   7, 4 },
  { '@',   5, 6 },
  { '+',   5, 0 },
  { '-',   5, 3 },
  { '*',   6, 1 },
  { ';',   6, 2, ']' },
  { ':',   5, 5, '[' },
  { ',',   5, 7, '<' },
  { '.',   5, 4, '>' },
  { '/',   6, 7, '?' },
  { '^',   6, 6 },
  { '=',   6, 5 },

  /* these shift to lowercase */
  { 'A',   1, 2 },
  { 'B',   3, 4 },
  { 'C',   2, 4 },
  { 'D',   2, 2 },
  { 'E',   1, 6 },
  { 'F',   2, 5 },
  { 'G',   3, 2 },
  { 'H',   3, 5 },
  { 'I',   4, 1 },
  { 'J',   4, 2 },
  { 'K',   4, 5 },
  { 'L',   5, 2 },
  { 'M',   4, 4 },
  { 'N',   4, 7 },
  { 'O',   4, 6 },
  { 'P',   5, 1 },
  { 'Q',   7, 6 },
  { 'R',   2, 1 },
  { 'S',   1, 5 },
  { 'T',   2, 6 },
  { 'U',   3, 6 },
  { 'V',   3, 7 },
  { 'W',   1, 1 },
  { 'X',   2, 7 },
  { 'Y',   3, 1 },
  { 'Z',   1, 4 },

  { 0 },
};

void getkey(c64 *c)
{
  /* Clear out row data */
  for (int i = 0; i < 8; i++)
    c->key_row[i] = 0;

  c->joystate[0] = 0;
#if 1
  if (screen->key('a', true)) {
    c->joystate[0] |= JOY_LEFT;
  }
  if (screen->key('w', true)) {
    c->joystate[0] |= JOY_UP;
  }
  if (screen->key('d', true)) {
    c->joystate[0] |= JOY_RIGHT;
  }
  if (screen->key('x', true)) {
    c->joystate[0] |= JOY_DOWN;
  }
  if (screen->key('s', true)) {
    c->joystate[0] |= JOY_FIRE;
  }
#endif
#if 0
  if (screen->key('a', true)) {
    EY--;
  }
  if (screen->key('s', true)) {
    EY++;
  }
  if (screen->key('q', true)) {
    SPRY--;
  }
  if (screen->key('w', true)) {
    SPRY++;
  }
#endif
  for (int i = 0; kmap[i].ch != 0; i++) {
    int ch  = tolower(kmap[i].ch);
    int row = kmap[i].row;
    int col = kmap[i].col;
    int sft = kmap[i].shift;

    /* is this key pressed? */
    if (screen->key(ch, true)) {
      c->key_row[row] |= (1L << col);
      printf("RCKey: %d,%d\n", row, col);
    }
    
    /* or is the shifted key pressed? */
    if (sft && screen->key(sft, true)) {
      c->key_row[row] |= (1L << col);
      c->key_row[LSHIFT_ROW] |= (1L << LSHIFT_COL);
      printf("RCKey: %d,%d SHIFT\n", row, col);
    }
  }
  if (screen->joy) {
    if (screen->axes[0] < -JOY_THRESH) c->joystate[0] |= JOY_LEFT;
    if (screen->axes[0] >  JOY_THRESH) c->joystate[0] |= JOY_RIGHT;
    if (screen->axes[1] < -JOY_THRESH) c->joystate[0] |= JOY_UP;
    if (screen->axes[1] >  JOY_THRESH) c->joystate[0] |= JOY_DOWN;
    if (screen->buttons[9]) c->joystate[0] |= JOY_FIRE;
  }
}

void pbits(uint8_t *m, int *clr, int w, int h, int row, int mc)
{
  char *str, *p;
  char chz[] = { '_', '1', '2', '3' };
  int ix;

  str = (char *)alloca(w * 32 + 1);
  for (int y = 0; y < h; y++) {
    p = str;
    memset(str, 0, w*8 + 1);
    for (int x = 0; x < w; x += 8) {
      uint8_t ch = *m++;
      for (int i = 0; i < 8; i++) {
	if (mc == 2) {
	  ix = (ch >> (6 - i)) & 3;
	  p += snprintf(p, w * 32, "%s%c%c", pclr(clr[ix], 0), chz[ix], chz[ix]);
	  i++;
	}
	else {
	  ix = (ch >> (7 - i)) & 1;
	  p += snprintf(p, w * 32, "%s%c", pclr(clr[ix], 0), chz[ix]);
	}
      }
    }
    const char *m0 = ""; //"\033[0m";
    printf("|%s|%s %2d %s\n", str, m0, row + y, (row + y) >= 0 && (row + y) <= 7 ? "YES" : "");
  }
}

/* Get screen mode */
int c64::scrmode()
{
  const bool ecm = (ctrl1 & ECM) > 0;
  const bool bmm = (ctrl1 & BMM) > 0;
  const bool mcm = (ctrl2 & MCM) > 0;

  if (!ecm && !bmm && !mcm)
    return mode_char;
  else if (!ecm && !bmm && mcm)
    return mode_mcchar;
  else if (!ecm && bmm && !mcm)
    return mode_bitmap;
  else if (!ecm && bmm && mcm)
    return mode_mcbitmap;
  else if (ecm && !bmm && !mcm)
    return mode_extbg;
  return mode_none;
}

/* sprite bitmap 3x21 = 63 bytes
 *  0  1  2
 *  3  4  5
 *  .  .  .
 *  60 61 62
 */

/* Holds data of rurent rendering sprite */
/* Load a sprite from VIC registers */
bool sprite_t::load(c64 *c, int n, int y)
{
  /* Sprite upper left coords = (24,50) */
  const int sbit = (1L << n);

  //printf("LoadSprite: %d, %d, %d\n", n, y, state);
  
  /* Check if sprite enabled */
  if ((c->sprite_en & sbit) == 0)
    return false;
  
  /* Get Sprite coordinate */
  sy = ioreg(SPRITE_Y + (n * 2)) - 50;
  sx = ioreg(SPRITE_X + (n * 2)) - 24;
  if (c->sprite_x8 & sbit)
    sx += 256;

  /* Get extra attributes */
  ex  = !!(c->sprite_ex  & sbit);
  ey  = !!(c->sprite_ey  & sbit);
  pri = !!(c->sprite_pri & sbit);
  mc  = !!(c->sprite_mc  & sbit) ? 2 : 1;
  
  /* Get sprite bits position in RAM */
  sp = c->sprite_ptr[n] * 64 + c->vicbase;

  /* Get sprite colors */
  if (mc == 2) {
    /* Multicolor sprite */
    clr[0] = -1;
    clr[1] = c->sprite_mm0 & 0xF;
    clr[2] = c->sprite_clr[n] & 0xF;
    clr[3] = c->sprite_mm1 & 0xF;
  }
  else {
    clr[0] = -1;
    clr[1] = c->sprite_clr[n] & 0xF;
  }
  return true;
}

void c64::drawsprite(int n, int y) {
  sprite_t *s = &xspr[n];

  if (!s->load(this, n, y))
    return;

#if 1
  printf(" drawsprite%d: (%3d,%3d) 2X(%d,%d) pri:%d mc:%d y:%d sp:%.4x\n",
	 n, s->sx, s->sy, s->ex, s->ey, s->pri, s->mc, y, s->sp);
  for (int i = 0; i < s->mc*2; i++) {
    printf(" clr:%d ", s->clr[i]);
  }
  printf("\n");
#endif

  collide = false;
  pbits(&ram[s->sp], s->clr, 24, 21, s->sy - y, s->mc);
  drawbmp(EX+s->sx, EY+s->sy, 24, 21, &ram[s->sp], s->ex+1, s->ey+1, s->mc, s->clr, y + EY);
  if (collide) {
    ioreg(SPRITE_SPR) |= (1L << n);
  }
}

/* Draw row of 8 raster line */
void c64::drawline(int y)
{
  int clr[4], ch, smode;
  int sx, sy, cb, sb, bpp;

  smode = scrmode();
  
  /* Get Memory pointers, screen, chrbase */
  sb = (memptr & 0xF0) << 6;
  cb = (memptr & 0x0E) << 10;
  if (smode == mode_bitmap) {
    // xxxx.Mxxx -> xxMx.xxxx.xxxx.xxxx, RAM @ 0000 or 2000
    cb = (memptr & 0x08) << 10;
    printf("bitmap CB is %.8x\n", cb);
  }

  screen_ram = &ram[vicbase + sb];
  sprite_ptr = screen_ram + 0x3f8;
  chrbase = vicbase + cb;

  /* Get XSCROLL/YSCROLL */
  sx = 0;
  sy = 0;
  for (int x = 0; x < 40; x++) {
    const int addr = (y * 40) + x;

    bpp = 1;
    ch = screen_ram[addr];
    if (smode == mode_char) {
      /* Char mode
       * 2 colors from BG0.lo, Color RAM.lo 
       * bitmap = &char_rom[ch * 8 + (y % 8)] */
      clr[0] = bg_clr[0] & 0xF;
      clr[1] = color_ram[addr] & 0xF;
    }
    else if (smode == mode_mcchar) {
      /* Multi-color Char mode
       * 4 Colors from BG0.lo, BG1.lo, BG2.lo, Color RAM.lo 
       * Values 8-15 = multicolor, values 0-7 = standard
       * bitmap = &char_rom[ch * 8 + (y % 8)]
       */
      if ((color_ram[addr] & 0x8) == 0x8) {
	clr[0] = bg_clr[0] & 0xF;
	clr[1] = bg_clr[1] & 0xF;
	clr[2] = bg_clr[2] & 0xF;
	clr[3] = color_ram[addr] & 0x7;
	bpp = 2;
      }
      else {
	clr[0] = bg_clr[0] & 0xF;
	clr[1] = color_ram[addr] & 0xF;
      }
    }
    else if (smode == mode_extbg) {
      /* Extended BG mode
       * 2 colors, but background has 4 options
       * upper bits of ch select which color, 
       * bg0, bg1, bg2, bg3 but limits to only certain chars
       * bitmap = &char_rom[(ch & 0x3F) * 8 + (y % 8)]
       */
      clr[0] = bg_clr[ch >> 6] & 0xF;
      clr[1] = color_ram[addr] & 0xF;
      ch = ch & 0x3f;
    }
    else if (smode == mode_bitmap) {
      /* Bitmap mode
       * 2 Colors from Screen RAM[hi,lo]: 2000-3FFF */
      clr[0] = (ch >> 0) & 0xF;
      clr[1] = (ch >> 4) & 0xF;
      ch = addr;
    }
    else if (smode == mode_mcbitmap) {
      /* Multi-color bitmap mode
       * 4 Colors from BG0, Screen RAM[hi,lo], Color RAM */
      clr[0] = bg_clr[0] & 0xF;
      clr[1] = (ch >> 4) & 0xF;
      clr[2] = (ch >> 0) & 0xF;
      clr[3] = color_ram[addr] & 0xF;
      ch = addr;
      bpp = 2;
    }
    if (bpp >= 1) {
      drawtile(x*8+sx, y*8+sy, ch, bpp, clr);
    }
  }
}

void c64::drawframe()
{
  int bdr;
  static time_t fpstime = time(NULL);
  time_t now = time(NULL);
  float fps = (float)frame / (now - fpstime);
  struct tm *rtc;
  int cb, sb, smode;

  /* Get Video mode */
  smode = scrmode();

  /* Get Memory pointers */
  sb = (memptr & 0xF0) << 6;
  cb = (memptr & 0x0E) << 10;
  if (smode == mode_bitmap) {
    cb = (memptr & 0x08) << 10;
  }
  printf("============================\nframe: %6d scr:%.4x chr:%.4x %s\n",
	 frame, vicbase+sb, chrbase,
	 scrmodes[smode]);
  printf("cia1:%.2x %.2x %.2x  cia2:%.2x %.2x %.2x  vic:%.2x %.2x %.2x\n",
	 cia1.cia_irq.en, cia1.cia_irq.sts, ioreg(0xDC0D),
	 cia2.cia_irq.en, cia2.cia_irq.sts, ioreg(0xDD0D),
	 vic_irq.en,  vic_irq.sts,  ioreg(0xD01A));
  
  /* Poke CIA1/CIA2 RTC */
  rtc = localtime(&now);
  rtc1_s = bcd(rtc->tm_sec);
  rtc1_m = bcd(rtc->tm_min);
  rtc1_h = bcd(rtc->tm_hour);
  rtc2_s = bcd(rtc->tm_sec);
  rtc2_m = bcd(rtc->tm_min);
  rtc2_h = bcd(rtc->tm_hour);
  
  frame++;
  screen->scrtext(0, screen->height+5, 1, "frm:%d fps:%.2f %s %dx%d", frame, fps, scrmodes[smode],
		  cols, rows);
  screen->scrtext(0, screen->height+13, 1, 
		 "v:%.4x s:%.4x c:%.4x EY:%d",
		  vicbase, sb, cb, EY);

  /* Draw Raster line */
  for (int i = 0; i < screen->height; i++) {
    if (sli[i]) {
      screen->scrline(0, i, 320+2*EY, 3);
    }
  }
  memset(sli, 0, sizeof(sli));
  screen->draw();

  bdr = border_clr & 0xF;
  screen->scrrect(0, 0, screen->width, screen->height, bdr);
}

static c64 thegame;

volatile uint8_t& ioreg(int n) {
  return thegame.ioregs[n & 0xFFF];
}

volatile uint16_t& ioreg16(int n) {
  return *(uint16_t *)&thegame.ioregs[n & 0xFFF];
}

uint8_t cpu_read8(uint32_t offset, int type)
{
  iodata_t v;

  /* 0000 - 7FFF: RAM
   * 8000 - 9FFF: RAM CARTLO
   * A000 - BFFF: RAM CARTHI BASIC
   * C000 - CFFF: RAM
   * D000 - DFFF: RAM CHAR   IO
   * E000 - FFFF: RAM CARTHI KERNAL
   */
  thegame.read(offset, v);
  if (offset >= 0xFFF0) {
    printf("nmi read: %.4x = %.2x\n", offset, v);
  }
  return v;
}

void  cpu_write8(uint32_t offset, uint8_t v, int type)
{
  if (v == 0x55) {
    printf("%.4x = 0x55\n", offset);
  }
  //printf("write:%.8x = %.2x\n", offset, v);
  thegame.write(offset, v);
}

/* HBlank = (504-403)
 * VBlank = (312-284)
 * 320 (40 cycles) x 200 screen
 * 403 (xx cycles) x 284 border
 * 504 (63 cycles) x 312 total
 * BadLines: (raster >= 48:0x30) && (raster <= 247:0xf7) && ((raster & 7) == yscroll)
 *
 * Scanlines:
 * PAL  = 504x312
 *   FirstVisible = 14
 *   FirstGR      = 56
 *   LastGr       = 256
 *   LastVisible  = 298
 * NTSC = 262
 *
 * Cycles per frame
 * PAL  = 63x312 = 19656 @ 50.125 FPS = 985248
 * NTSC = 65x263 = 17095 @ 59.826 FPS = 1022727
 *
 * CIA1 timer
 * PAL  = 16421 cycles * 60 / 985248
 * NTSC = 17045 cycles * 60 / 1022730
 */

#define CYCLES_PER_LINE    63
#define CYCLES_PER_BADLINE 23
#define LINES              312

void c64::ppu_tick()
{
  static uint32_t dot, nextline = CYCLES_PER_LINE;
  int smode = scrmode();
  int row = scanline - EY;

  if (++dot < nextline) {
    return;
  }
  /* Row is Displayed video line */
  if (row >= 0 && row < 200) {
    if ((row % 8) == 0) {
      smode = scrmode();
      fprintf(stdout, "--- line %3d: %3d-%3d [%d,%d] raster:%d %s\n",
	      scanline, row, row + 7, XSCROLL(), YSCROLL(), raster_irq, 
	      scrmodes[smode]);
      /* Draw line and sprite */
      drawline(row / 8);
      for (int i = 7; i >= 0; i--) {
	drawsprite(i, row);
      }
      //draw line every 8-rows
      //screen->scrline(0, EY+row, 320+2*EY, 8);
    }
  }
  /* Increase scanline, get next cycles */
  dot -= nextline;
  scanline++;
  if (is_badline(scanline)) {
    nextline = CYCLES_PER_BADLINE;
  }
  else {
    nextline = CYCLES_PER_LINE;
  }
  /* End of screen - display frame */
  if (scanline >= LINES) {
    getkey(this);
    drawframe();
    scanline=0;
  }
  if (raster_irq == scanline) {
    printf("%-12s RASTER IRQ @ %4d %.2x\n",
	   scrmodes[smode],
	   scanline, vic_irq.en);
    if (vic_irq.en & VIC_IRQ_RASTER) {
      vic_irq.set(VIC_IRQ_RASTER);
    }
  }
}

void c64::run(const char *prgfile)
{
  palclr cpal[16];
  int cycs = 0;
  int n, iPC = 0;

  // setup palette
  for (int i = 0; i < 16; i++) {
    cpal[i].r = R(c64pal[i]);
    cpal[i].g = G(c64pal[i]);
    cpal[i].b = B(c64pal[i]);
  }
  screen = new Screen(SCREEN_XRES, SCREEN_YRES, 0, 30, 16, cpal);
  screen->xs = 2;
  screen->ys = 2;
  screen->init(0);

  // load .prg file
  if (prgfile != NULL) {
    size_t sz;
    prg = loadrom(prgfile, sz);
    prgaddr = get16(prg);
    prgsize = sz - 2;
    printf("prg: %p %.4x %x\n", prg, prgaddr, prgsize);
  }
  cpu_reset(iPC);
  for(;;) {
    if (SPC == 0x8a46) {
      trace = 1;
    }
    if (hlt) {
      hexdump(ram, 65536, 32);
      exit(0);
    }
    cycs = cpu_step();
    for (n = 0; n < cycs; n++) {
      // tick timers
      if (cia1.tmra.tick()) {
	fprintf(stdout,"---- tick A1 [%d] sts:%.2x en:%.2x\n", cia1.tmra.reset, cia1.cia_irq.sts, cia1.cia_irq.en);
	cia1.cia_irq.set(CIA1_IRQ_TMRA);
      }
      if (cia1.tmrb.tick()) {
	fprintf(stdout,"---- tick B1 [%d] sts:%.2x en:%.2x\n", cia1.tmrb.reset, cia1.cia_irq.sts, cia1.cia_irq.en);
	cia1.cia_irq.set(CIA1_IRQ_TMRB);
      }  
      if (cia2.tmra.tick()) {
	fprintf(stdout,"---- tick A2 [%d] sts:%.2x en:%.2x\n", cia2.tmra.reset, cia2.cia_irq.sts, cia2.cia_irq.en);
	cia2.cia_irq.set(CIA2_IRQ_TMRA);
      }
      if (cia2.tmrb.tick()) {
	fprintf(stdout,"---- tick B2 [%d] sts:%.2x en:%.2x\n", cia2.tmrb.reset, cia2.cia_irq.sts, cia2.cia_irq.en);
	cia2.cia_irq.set(CIA2_IRQ_TMRB);
      }
      // tick ppu,sid
      ppu_tick();
      sid_tick();
    }
    // check interrupts
    int pendcia1 = cia1.cia_irq.pending();
    int pendcia2 = cia2.cia_irq.pending();
    int pendvic = vic_irq.pending();
    if (pendcia2) {
      //cpu_nmi();
      cia2.cia_irq.clear(pendcia2);
      pendcia2 = 0;
    }
    if (pendcia1 || pendcia2 || pendvic) {
      bool rc = cpu_irq(0);
#if 0
      printf("%10d Pending IRQ: ciaen::%.2x/sts:%.2x vicen:%.2x/vicsts:%.2x runirq:%d\n",
	     totcyc, cia1.cia_irq.en, cia1.cia_irq.sts,
	     vic_irq.en, vic_irq.sts, rc);
#endif
      if (rc && pendcia1) {
	cia1.cia_irq.clear(pendcia1);
      }
      if (rc && pendvic) {
	cia1.cia_irq.clear(pendvic);
      }
    }
  }
}

int main(int argc, char *argv[])
{
  char *romfile = NULL;
  char *prgfile = NULL;
  int n = 1;

  extern int nokbd;
  nokbd=0;
  setbuf(stdout, NULL);
  if (argc > 1 && !strcmp(argv[1], "-t")) {
    trace = 1;
    n = 2;
  }
  if (argc > n) {
    const char *a=argv[n];
    if (strstr(a,".CRT") || strstr(a,".crt")) {
      romfile = argv[n];
    }
    if (strstr(a,".PRG") || strstr(a,".prg")) {
      prgfile = argv[n];
    }
  }
  thegame.load(romfile);
  thegame.run(prgfile);
}
