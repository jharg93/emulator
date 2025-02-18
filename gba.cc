#include <time.h>
#include <stdarg.h>
#include "util.h"

#define HAVE_CPU16
#define HAVE_CPU32
#include "bus.h"
#include "cpu.h"
#include "gr.h"
#include "gba.h"

#include "armcpu.h"

extern int iztest;
extern int trace;

/* Memory map
 *   0x00000000 - 0x00003FFF 16k   bios
 *   0x02000000 - 0x0203FFFF 256k  WRAM on-board
 *   0x03000000 - 0x03007FFF 32k   WRAM on-chip
 *   0x04000000 - 0x040003FF       i/o registers
 *   0x05000000 - 0x000001ff 512   BG Palette      [256x2]
 *   0x05000200 - 0x500003ff 512   Sprite Palette  [256x2]
 *   0x06000000 - 0x0600FFFF 64k   VRAM  | BG Tile, BG Map
 *   0x06010000 - 0x06017FFF 32k   VRAM  | Sprite Tile Data
 *   0x07000000 - 0x070003FF 1k    OAM
 *   0x08000000 - 0x09FFFFFF 32Mb  ROM
 *   0x0A000000 - 0x0BFFFFFF 32Mb  ROM
 *   0x0C000000 - 0x0DFFFFFF 32Mb  ROM
 */

/* Screen:
 * 240x160 @ 59.73 Hz
 * 4 cycles per pixel
 * scanline = 240+68(HBLANK) => 1232 cycles
 * VDraw    = 160x1232 = 197120 cycles
 * VBlank   = 160+68   = 83776 cycles
 * Frame    = 280896 cycles
 */
static uint8_t *bios;
static uint8_t *palram;
static uint8_t *vram;
static uint8_t *bram;
static uint8_t *cram;
static uint8_t  ioreg[0x400];
static uint8_t  oam[0x400];

struct ppu_t {
  uint8_t   bgMode;
  uint8_t   bgPage;
  
  struct {
    bool      bgEn;
    uint32_t  cbb;
    uint32_t  sbb;
    uint8_t   cm;
    uint8_t   sz;
    uint8_t   pri;
  } bg[4];
  struct {
    bool     objEn;
    uint8_t  obj1d;
  } obj;
} ppu;

// 4-bit tiles = 8x4
// 8-bit tiles = 8x8
/* Map Entry = 10 bits (1024 tiles)
 * Each tile is 0x20 for 4bb (512), 0x40 for 8bpp (256)
 * 4-bpp use up to two CBB
 * 8-bpp use all 4 CBB
 *
 * CBB_SIZE = 1024 * 8                 [CHR]
 * SBB_SIZE = 32*32*sizeof(uint16_t)   [NT]
 */
/* Palette RAM
 *  05000000 512 = (256x2) BGR BG color entries
 *  05000200 512 = (256x2) BGR Sprite color entries
 *
 * Video RAM
 *  06000000 CBB0, SBB=0..7
 *  06004000 CBB1, SBB=8..15
 *  06008000 CBB2, SBB=16..23
 *  0600C000 CBB3, SBB=24..31
 *  06010000 CBB4, Sprite 0-511
 *  06014000 CBB5, Sprite 512-1023
 *
 *  06010000 BG (mode 3-5)  Sprite Index 0-511     (not avail in modes 3-5)
 *  06014000                Sprite Index 512-1023
 *    cbb: 00-03 0x4000 [CHR] = 512x32  tiledata [4bpp]
 *    sbb: 00-31 0x0800 [NT]  = 32x32x2 tilemap
 */
enum {
      /* DISPCNT: Display Control Register */
      DISPCNT      = 0x04000000,
      DCNT_MODE     = D0|D1|D2, // rw 0/1/2 = tiled, 3/4/5 = bitmap
      DCNT_GB       = D3,  // ro GBC game
      DCNT_PAGE     = D4,  // Displayed page (mode=4/5) @ 0600A000
      DCNT_OAM_HBL  = D5,  // OAM during HBlank
      DCNT_OBJ_1D   = D6,  // 0=2D, 1=1D
      DCNT_BLANK    = D7,  // force screen blank
      DCNT_BG0      = D8,  // Enable rendering of objects
      DCNT_BG1      = D9,
      DCNT_BG2      = D10,
      DCNT_BG3      = D11,
      DCNT_OBJ      = D12,
      DCNT_WIN0     = D13,  // Enable Window/Objects
      DCNT_WIN1     = D14,
      DCNT_WINOBJ   = D15,

      /* DISPSTAT: Display Status Register */
      DISPSTAT     = 0x04000004,
      DSTAT_IN_VBL  = D0,        // in VBlank  (160 <= scanline < 228)
      DSTAT_IN_HBL  = D1,        // in HBblank (240 <= clks < 308) 
      DSTAT_IN_VCT  = D2,        // VCT == VCOUNT
      DSTAT_VBL_IRQ = D3,        // requested IRQs
      DSTAT_HBL_IRQ = D4,
      DSTAT_VCT_IRQ = D5,
      DSTAT_VCT     = 0xFF00,

      VCOUNT       = 0x04000006,

      /* BGxCNT: Background Control Registers */
      BG0CNT       = 0x04000008,
      BG1CNT       = 0x0400000A,
      BG2CNT       = 0x0400000C,
      BG3CNT       = 0x0400000E,
      BG_PRIO       = D0|D1,              // Priority
      BG_CBB        = D2|D3,              // Character Base Block [0-3] (CHR), 16kb steps    (1024*8*2 = 4000h)
      BG_MOSAIC     = D6,
      BG_CM         = D7,                 // 0=4bpp, 1=8bpp
      BG_SBB        = D8|D9|D10|D11|D12,  // Screen Base Block [0-31] (Nametable), 2Kb steps (32x32x2 = 0800h)
      BG_WRAP       = D13,                // Affine wrapping
      BG_SIZE       = D14|D15,            // Size: 32x32, 64x32, 32x64, 64x64

      /* BGxxOFS: Background Scroll Registers */
      BG0HOFS      = 0x04000010,
      BG0VOFS      = 0x04000012,
      BG1HOFS      = 0x04000014,
      BG1VOFS      = 0x04000016,
      BG2HOFS      = 0x04000018,
      BG2VOFS      = 0x0400001A,
      BG3HOFS      = 0x0400001C,
      BG3VOFS      = 0x0400001E,

      BG2PA        = 0x04000020,
      BG2PB        = 0x04000022,
      BG2PC        = 0x04000024,
      BG2PD        = 0x04000026,
      BG2X         = 0x04000028,
      BG2Y         = 0x0400002C,

      BG3PA        = 0x04000030,
      BG3PB        = 0x04000032,
      BG3PC        = 0x04000034,
      BG3PD        = 0x04000036,
      BG3X         = 0x04000038,
      BG3Y         = 0x0400003C,

      /* Window */
      WIN0H        = 0x04000040,
      WIN1H        = 0x04000042,
      WIN0V        = 0x04000044,
      WIN1V        = 0x04000046,
      WININ        = 0x04000048,
      WINOUT       = 0x0400004A,

      /* Sound */
      SOUND1CNT_L  = 0x04000060, // sweep, NR10
      SOUND1CNT_H  = 0x04000062, // duty length, envelope NR11/12
      SOUND1CNT_X  = 0x04000064, // frequency/control  NR13/14
      SOUND2CNT_L  = 0x04000068, // duty/length/envelope NR21/22
      SOUND2CNT_H  = 0x0400006A,
      SOUND2CNT_X  = 0x0400006C, // frequency/control, NR23/24
      SOUND3CNT_L  = 0x04000070, // stop/wave ram select NR30
      SOUND3CNT_H  = 0x04000072, // length/volume NR31/32
      SOUND3CNT_X  = 0x04000074, // frequency/control NR33/34
      SOUND4CNT_L  = 0x04000078, // length/envelope NR41/42
      SOUND4CNT_H  = 0x0400007A,
      SOUND4CNT_X  = 0x0400007C, // frequency/conrol NR43/44
      SOUNDCNT_L   = 0x04000080, // control/stereo NR50/51
      SOUNDCNT_H   = 0x04000082, // control/mixing dma control
      SOUNDCNT_X   = 0x04000084, // control sound on/off (NR52)
      SOUNDBIAS    = 0x04000088,
      WAVE_RAM     = 0x04000090,
      FIFO_A       = 0x040000A0,
      FIFO_B       = 0x040000A4,

      /* 4 DMA Channels */
      DMA0SAD      = 0x040000B0,   // source address
      DMA0DAD      = 0x040000B4,   // dest address
      DMA0CNT_L    = 0x040000B8,   // length
      DMA0CNT_H    = 0x040000BA,

      DMA1SAD      = 0x040000BC,
      DMA1DAD      = 0x040000C0,
      DMA1CNT_L    = 0x040000C4,
      DMA1CNT_H    = 0x040000C6,

      DMA2SAD      = 0x040000C8,
      DMA2DAD      = 0x040000CC,
      DMA2CNT_L    = 0x040000D0,
      DMA2CNT_H    = 0x040000D2,

      DMA3SAD      = 0x040000D4,
      DMA3DAD      = 0x040000D8,
      DMA3CNT_L    = 0x040000DC,
      DMA3CNT_H    = 0x040000DE,

      /* Timer */
      TM0CNT_L     = 0x04000100,
      TM0CNT_H     = 0x04000102,
      TM1CNT_L     = 0x04000104,
      TM1CNT_H     = 0x04000106,
      TM2CNT_L     = 0x04000108,
      TM2CNT_H     = 0x0400010A,
      TM3CNT_L     = 0x0400010C,
      TM3CNT_H     = 0x0400010E,
      
      /* IE: Interrupt Enable */
      IE           = 0x04000200, // interrupt enable
      IRQ_VBLANK     = D0,
      IRQ_HBLANK     = D1,
      IRQ_VCOUNT     = D2,
      IRQ_TIMER      = 0x0078,
      IRQ_COM        = D7,
      IRQ_DMA        = 0x0F00,
      IRQ_KEYPAD     = D12,
      IRQ_GAMEPAK    = D13,

      IF           = 0x04000202, // interrupt request
      WAITCNT      = 0x04000204,
      IME          = 0x04000208, // interrupt master enable

      /* KEYPAD: Key input */
      KEYPAD       = 0x04000130,
      KEYINPUT_A     = D0,
      KEYINPUT_B     = D1,
      KEYINPUT_SEL   = D2,
      KEYINPUT_START = D3,
      KEYINPUT_RIGHT = D4,
      KEYINPUT_LEFT  = D5,
      KEYINPUT_UP    = D6,
      KEYINPUT_DOWN  = D7,
      KEYINPUT_R     = D8,
      KEYINPUT_L     = D9,

      KEYCNT       = 0x04000132,
      
      /* TILE */
      TID        = 0x1FF,
      HF         = D10,
      VF         = D11,
      PB         = 0xF000,

      /* Sprite Attr */
      ATTR0_Y     = 0xFF,
      ATTR0_OM    = (D8|D9),
      ATTR0_GM    = (D10|D11),
      ATTR0_MOS   = D12,
      ATTR0_CM    = D13,
      ATTR0_SHAPE = (D14|D15),

      ATTR1_X     = 0x1FF,
      ATTR1_HF    = D12,
      ATTR1_VF    = D13,
      ATTR1_SIZE  = (D14|D15),

      ATTR2_TID   = 0x3FF,
      ATTR2_PRIO  = D12,
      ATTR2_PB    = (D13|D14|D15),
};

uint16_t o_dispcnt;

#define GBAREG16(name, rw) volatile uint16_t& R_##name = *(uint16_t *)&ioreg[(name & 0x3FF)]
#define GBAREG32(name, rw) volatile uint32_t& R_##name = *(uint32_t *)&ioreg[(name & 0x3FF)]

/* Display Registers */
struct gba {
  GBAREG16(DISPCNT,  RW);
  GBAREG16(DISPSTAT, RW);
  GBAREG16(VCOUNT,   RO);
  GBAREG16(BG0CNT,   RW);
  GBAREG16(BG1CNT,   RW);
  GBAREG16(BG2CNT,   RW);
  GBAREG16(BG3CNT,   RW);
  
/* Scrolling registers */
  GBAREG16(BG0HOFS,  RO);
  GBAREG16(BG0VOFS,  RO);
  GBAREG16(BG1HOFS,  WO);
  GBAREG16(BG1VOFS,  WO);
  GBAREG16(BG2HOFS,  RO);
  GBAREG16(BG2VOFS,  WO);
  GBAREG16(BG3HOFS,  WO);
  GBAREG16(BG3VOFS,  WO);
  
/* BG Rotation/Scaling */
  GBAREG16(BG2PA,    WO);
  GBAREG16(BG2PB,    WO);
  GBAREG16(BG2PC,    WO);
  GBAREG16(BG2PD,    WO);
  GBAREG32(BG2X,     WO);
  GBAREG32(BG2Y,     WO);
  GBAREG16(BG3PA,    WO);
  GBAREG16(BG3PB,    WO);
  GBAREG16(BG3PC,    WO);
  GBAREG16(BG3PD,    WO);
  GBAREG32(BG3X,     WO);
  GBAREG32(BG3Y,     WO);
  
  /* Window */
  GBAREG16(WIN0H,    WO);
  GBAREG16(WIN1H,    WO);
  GBAREG16(WIN0V,    WO);
  GBAREG16(WIN1V,    WO);
  GBAREG16(WININ,    RW);
  GBAREG16(WINOUT,   RW);
  
  /* Sound */
  GBAREG16(SOUND1CNT_L,RW);
  GBAREG16(SOUND1CNT_H,RW);
  GBAREG16(SOUND1CNT_X,RW);
  GBAREG16(SOUND2CNT_L,RW);
  GBAREG16(SOUND2CNT_H,RW);
  GBAREG16(SOUND2CNT_X,RW);
  GBAREG16(SOUND3CNT_L,RW);
  GBAREG16(SOUND3CNT_H,RW);
  GBAREG16(SOUND3CNT_X,RW);
  GBAREG16(SOUND4CNT_L,RW);
  GBAREG16(SOUND4CNT_H,RW);
  GBAREG16(SOUND4CNT_X,RW);
  
  /* Timer */
  GBAREG16(TM0CNT_L,RW);
  GBAREG16(TM0CNT_H,RW);
  GBAREG16(TM1CNT_L,RW);
  GBAREG16(TM1CNT_H,RW);
  GBAREG16(TM2CNT_L,RW);
  GBAREG16(TM2CNT_H,RW);
  GBAREG16(TM3CNT_L,RW);
  GBAREG16(TM3CNT_H,RW);
  
  /* Interrupt */
  GBAREG16(IE,RW);
  GBAREG16(IF,RW);
  GBAREG16(WAITCNT,RW);
  GBAREG16(IME,RW);
  
  GBAREG16(KEYPAD,RO);
  
  GBAREG32(DMA0SAD,RW);
  GBAREG32(DMA0DAD,RW);
  GBAREG16(DMA0CNT_L,RW);
  GBAREG16(DMA0CNT_H,RW);
  
  GBAREG32(DMA1SAD,RW);
  GBAREG32(DMA1DAD,RW);
  GBAREG16(DMA1CNT_L,RW);
  GBAREG16(DMA1CNT_H,RW);
  
  GBAREG32(DMA2SAD,RW);
  GBAREG32(DMA2DAD,RW);
  GBAREG16(DMA2CNT_L,RW);
  GBAREG16(DMA2CNT_H,RW);
  
  GBAREG32(DMA3SAD,RW);
  GBAREG32(DMA3DAD,RW);
  GBAREG16(DMA3CNT_L,RW);
  GBAREG16(DMA3CNT_H,RW);

  bool inVBlank();
  bool inHBlank();
  void setHblank(bool);
  void setVblank(bool);
  void setLYC(bool);
  void setkeymask(int key, int mask);
  
  void ppu_tick();
  void drawnt(int);
  void genframe();
  void drawsprites();
  
  int regio(uint32_t, int, iodata_t&);
  static int regio(void *arg, uint32_t offset, int mode, iodata_t& data) {
    gba *g = (gba *)arg;
    return g->regio(offset, mode, data);
  };

  void init(uint8_t *, size_t);

  void run();
};

gba thegame;
volatile uint16_t& scanline = thegame.R_VCOUNT;

/* 240x160 Screen.  
 *   (+68 HBlank) = 308
 *   (+68 VBlank) = 228
 */
#define TX 245
Screen s(240 + 260, 160, 10, 60);

uint32_t rdreg16(uint32_t off) {
  return get16(ioreg + (off & 0x3FF));
}

int bgsel, palsel, tracer;

void flogger(int n, const char *fmt, ...)
{
  va_list ap;

  if (n >= 0) {
    va_start(ap, fmt);
    vprintf(fmt, ap);
    va_end(ap);
  }
}

/*==================================*
 * Common Frame/Scanline/Clock tick
 *==================================*/
uint32_t  frame;
uint16_t  clk;

/* (240+68) x (160+68)
 * 1 pixel  = 4 cycles
 * HDraw    = 4 x 240 = 960 cycles
 * HBlank   = 4 x 68  = 272 cycles
 * scanline = 960 + 272 = 1232 cycles
 * VDraw    = 160 x 1232 = 197120 cycles
 * VBlank   = 68  x 1232 = 83776 cycles
 * refresh  = 197120 + 83776 = 280896
 *
 * 280896 cycles / clock speed = 59.73 Hz
 */
#define HBLANK_START 240
#define VBLANK_START 160
#define MAX_LX      (HBLANK_START + 68)
#define MAX_LY      (VBLANK_START + 68)

#define VBLANK_IRQ 1
#define HBLANK_IRQ 2
#define LYC_IRQ    3

bool gba::inVBlank() {
  return R_DISPSTAT & DSTAT_IN_VBL;
}
bool gba::inHBlank() {
  return R_DISPSTAT & DSTAT_IN_HBL;
}

/* Set when entering/exiting HBLANK */
void gba::setHblank(bool state)
{
  if (state && !inHBlank()) {
    R_DISPSTAT = R_DISPSTAT | DSTAT_IN_HBL;
    if (R_DISPSTAT & DSTAT_HBL_IRQ)
      cpu_irq(HBLANK_IRQ);
  }
  else if (!state && inHBlank()) {
    R_DISPSTAT = R_DISPSTAT & ~DSTAT_IN_HBL;
  }
}

/* Set when entering/exiting VBLANK */
void gba::setVblank(bool state)
{
  if (state && !inVBlank()) {
    R_DISPSTAT = R_DISPSTAT | DSTAT_IN_VBL;
    if (R_DISPSTAT & DSTAT_VBL_IRQ)
      cpu_irq(VBLANK_IRQ);
  }
  else if (!state && inVBlank()) {
    R_DISPSTAT = R_DISPSTAT & ~DSTAT_IN_VBL;
  }
}

void gba::setLYC(bool state)
{
  if (state && !(R_DISPSTAT & DSTAT_IN_VCT)) {
    printf("set LYC:%d\n", state);
    R_DISPSTAT = R_DISPSTAT | DSTAT_IN_VCT;
    if (R_DISPSTAT & DSTAT_VCT_IRQ)
      cpu_irq(LYC_IRQ);
  }
  else if (!state && (R_DISPSTAT & DSTAT_IN_VCT)) {
    R_DISPSTAT = R_DISPSTAT & ~DSTAT_IN_VBL;
  };
}

/* Video modes
 *  0: 240x160 : tiles 4xBG, 0 affine [reg,reg,reg,reg]
 *  1: 240x160 : tiles 3xBG, 1 affine [reg,reg,aff,---]
 *  2: 240x160 : tiles 2xBG, 2 affine [---,---,aff,aff]
 * 
 *  3: 240x160x16 BGR, 1 page
 *     06000000..06012BFF (wraps into Sprite block 0-511)
 *  4: 240x160x8  PAL, 2 page
 *     06000000..06009600
 *     0600A000..06013600 (wraps into Sprite block 0-511)
 *  5: 160x128x16 BGR, 2 page
 *     06000000..06009FFF
 *     0600A000..06013FFF (wraps into Sprite Block 0-511)
 */
void genbmp(uint32_t vpage, int w, int h, int bpp)
{
  int y, x, clr;
  uint16_t *vp = (uint16_t *)&vram[vpage];
  uint16_t *cp = (uint16_t *)palram;
  uint32_t p;

  for (y = 0; y < h; y++) {
    for (x = 0; x < w; x++) {
      if (bpp == 16) {
	/* VRAM is directly BGR, 16 bits */
	p = vp[(y * w) + x];
      }
      else {
	/* VRAM is palette index, 8 bits */
	clr = get8(vram + vpage + ((y * w) + x));
	p   = cp[clr];
      }
      s.setpixel(x, y, BGRRGB(p));
    }
  }
}

/* Mirror mode:
 *    0.....x-1
 * 0
 * .
 * y-1
 *
 * xmirror: map(dx,dy)
 */

/* hm=0: sx=1,  tx=spr.x 
 * hm=1: sx=-1, tx=spr.x+spr.w-1
 * vm=0: sy=1,  ty=spr.y
 * vm=1: sy=-1, ty=spr.y+spr.h-1;
 *
 * want x = (xd * sx) + tx;
 *      y = (yd * sy) + ty;
 */
//#include "vec.h"

//mtx2d affine;

void ppx(const int x, const int y, int clr) {
  //vec2d n0 = affine * vec2d(x,y,1);
  s.setpixel(x, y, clr); 
}

/* base/tid point to chr data
 * +----+----+ +----+----+ +----+----+ +----+----+
 * | p1 | p0 | | p3 | p2 | | p5   p4 | | p7 | p6 | 
 * +----+----+ +----+----+ +----+----+ +----+----+ 
 * 4bpp: 32 bytes per tile, 8 dwords
 * 8bpp: 64 bytes,          8 qwords
 */
void drawtile(int x, int y, uint32_t base, int tid, int bpp, int ppid)
{
  int i, j, clr;
  uint64_t p, px, m;

  s.clrmode = 1;
  for (i = 0; i < 8; i++) {
    if (bpp == 4) {
      m = 0x10;
      p = get32(vram + base + (tid * 32) + (i * 4));
    }
    else {
      m = 0x100;
      p = get64(vram + base + (tid * 64) + (i * 8));
    }
    for (j = 0; j < 8; j++) {
      /* Get color bits out of P */
      clr = (p % m);
      p /= m;

      /* Get Palette color, draw if not transparent */
      px = get16(palram + ppid + (clr * 2));
      if (clr) {
	ppx(x+j, y+i, BGRRGB(px));
      }
    }
  }
}

/* Blocks = 32x32x2 = 0x800 
 * 32x32:   yy.yyyx.xxxx
 * 64x32
 */
int getmpos(int x, int y, int sz)
{
  int mpos = (((y % 32) * 32) + (x % 32)) * 2;

  if (sz == BG_REG_32x32) {
    assert(x < 32 && y < 32);
  }
  if (sz == BG_REG_64x32) {
    assert (y < 32);
    mpos += (x >= 32) ? 0x800 : 0x000;
  }
  if (sz == BG_REG_32x64) {
    assert(x < 32);
    mpos += (x >= 32) ? 0x800 : 0x000;
  }
  if (sz == BG_REG_64x64) {
    if (y >= 32)
      mpos += 0x1000;
    if (x >= 32)
      mpos += 0x0800;
  }
  return mpos;
}

const char *bgsz[] = {
  "32x32", // 2k
  "64x32", // 4k
  "32x64", // 4k
  "64x64"  // 8k
};

/* size flag [sbb = 0x800]
 *  00: 32x32 (32x32x2 = 2k) 1 sbb
 *  01: 64x32 (64x32x2 = 4k) 2 sbb
 *  10: 32x64 (32x64x2 = 4k) 2 sbb
 *  11: 64x64 (64x64x2 = 8k) 4 sbb
 */
void gba::drawnt(int nt)
{
  int sbb, cbb, w, h, qx, qy, bpp, mpos;
  uint16_t cnt, tile, tid, sz;
  int16_t  sx, sy, pb;

  if (!ppu.bg[nt].bgEn)
    return;
  
  /* Read Control + Scroll registers */
  cnt = rdreg16(BG0CNT  + (nt * 2));
  sx  = rdreg16(BG0HOFS + (nt * 4));
  sy  = rdreg16(BG0VOFS + (nt * 4));

  /* SBB is nametable, CBB is CHR */
  sbb = ppu.bg[nt].sbb * 0x0800; // 2k  nametable
  cbb = ppu.bg[nt].cbb * 0x4000; // 16k CHR
  sz  = ppu.bg[nt].sz;
  bpp = ppu.bg[nt].cm;

  /* Calculate size of map area
   * Map    is  W x H
   * Screen is  30 x 20
   *
   * Scr position p
   * Map position q
   *   q = p - dx
   */
  w  = (sz & 0x1) ? 64 : 32;
  h  = (sz & 0x2) ? 64 : 32;

  printf("CNT=%.4x w=%d h=%d\n", cnt, w, h);
  
  for (qy = 0; qy < h; qy++) {
    for (qx = 0; qx < w; qx++) {
      mpos = getmpos(qx, qy, sz);
      tile = get16(vram + sbb + mpos);
      tid  = (tile & 0x1FF);
      pb   = (tile >> 0xC) & 0xF;
      drawtile(qx*8 - sx, qy*8 - sy, cbb, tid, bpp, pb * 32);
    }
  }
  // Sprites: cbb = 0x06010000, pal = 0x05000200
  flogger(0,"BG%d: %.4x sbb:%x cbb:%.4x sz:%2d,%2d scroll:%3d,%3d bpp:%d\n",
	  nt, cnt, sbb, cbb, w, h, sx, sy, bpp);
}

/* sh.sz 
 * 00.00 8x8    1,1    square
 * 00.01 16x16  2,2
 * 00.10 32x32  4,4
 * 00.11 64x64  8,8
 * 01.00 16x8   2,1    wide
 * 01.01 32x8   4,1
 * 01.10 32x16  4,2
 * 01.11 64x32  8,4
 * 10.00 8x16   1,2    tall
 * 10.01 8x32   1,4
 * 10.10 16x32  2,4
 * 10.11 32x64  4,8
 */
void _dts(int px, int py, int n, int pxi, int txi, int base, int id, int pal, int bpp)
{
  px = 0;
  printf("y:%d {", py);
  while (n--) {
    printf(" %d[%.3x]", px, id);
    drawtile(px, py, base, id, bpp, pal);
    px += pxi;
    id += txi;
  }
  printf("} \n");
}

struct soam_t {
  uint16_t x;
  uint16_t y;
  uint16_t w;
  uint16_t h;
  uint16_t tid;
  bool     vm;
  bool     hm;
  uint16_t attr0;
  uint16_t attr1;
  uint16_t attr2;
};

/* 128 sprites x 8 bytes = 1024 */
int getsprite(soam_t *spr, int n)
{
  oam_t *odata = (oam_t *)oam;
  int shp;
  int wmap[] = { 8,  16, 32, 64,
		 16, 32, 32, 64,
		 8,  8,  16, 32,
		 0,  0,  0,  0 };
  int hmap[] = { 8,  16, 32, 64,
		 8,  8,  16, 32,
		 16, 32, 32, 64,
		 0,  0,  0,  0 };

  /*        +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
   * attr0: |shp    |cm |mos|gm     |om     |                Y              |
   *        +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
   * attr1: |size   |vf |hf |           |                    X              |
   * attr1: |size   |aid                |                    X              |
   *        +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
   * attr2: |palette        |pri    |                       TID             |
   *        +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
   */
  if (n >= 128)
    return -1;
  spr->x     = odata[n].attr1 & ATTR1_X;
  spr->y     = odata[n].attr0 & ATTR0_Y;
  spr->tid   = odata[n].attr2 & ATTR2_TID;
  spr->attr0 = odata[n].attr0;
  spr->attr1 = odata[n].attr1;
  spr->attr2 = odata[n].attr2;
  spr->hm    = (spr->attr1 & ATTR1_HF) != 0;
  spr->vm    = (spr->attr1 & ATTR1_VF) != 0;

  shp = ((odata[n].attr0 & ATTR0_SHAPE) >> 12) | ((odata[n].attr1 & ATTR1_SIZE) >> 14);
  spr->w = wmap[shp];
  spr->h = hmap[shp];
  if ((spr->attr0 & ATTR0_OM) == 0x0200)
    return -1;
  if (spr->x >= 240 || spr->y >= 160)
    return -1;
  printf("%.4x %.4x %.4x %x %x\n", spr->attr0, spr->attr1, spr->attr2, spr->vm, spr->hm);
  return 0;
}

/* +-+-+-+-+ hm +-+-+-+-+
 * |A|B|C|D|    |D|C|B|A|
 * +-+-+-+-*    +-+-+-+-+
 * |E|F|G|H|    |H|G|F|E|
 * +-+-+-+-+    +-+-+-+-+*/
void gba::drawsprites()
{
  int i, pb, bpp, sy, cbb;
  soam_t spr;
  
  if ((R_DISPCNT & DCNT_OBJ) == 0)
    return;
  cbb = 0x10000;
  for (i = 0; i < 128; i++) {
    if (getsprite(&spr, i))
      continue;

    /* Get Sprite shape, ID, Palette Buffer, BPP */
    pb  = 0x200 + ((spr.attr2 & ATTR2_PB) >> 13)*16;
    bpp = (spr.attr0 & ATTR0_CM) ? 8 : 4;

    flogger(1,"loadsprite[%3d] %3d %3d : id:%3x pb:%d bpp:%d shape:%2d,%2d %s\n",
	    i, spr.x, spr.y, spr.tid, pb, bpp, spr.w, spr.h,
	    ppu.obj.obj1d ? "1D" : "2D");
#if 0
    if (spr.hm)
      spr.x += spr.w-1;
    if (spr.vm)
      spr.y += spr.h-1;
    affine =
      translate(spr.x, spr.y) *
      scale(spr.hm ? -1 : 1, spr.vm ? -1 : 1);
    for (sy = 0; sy < spr.h; sy+=8) {
      _dts(0, sy, spr.w/8, 8, 1, cbb, spr.tid, pb, bpp);
      spr.tid += spr.w/8;
    }
    affine = scale(1,1);
#endif
  }
}

#define BB 20
void gba::genframe()
{
  static time_t fpstime = time(NULL);
  time_t now = time(NULL);
  float fps = (float)frame / (now - fpstime);
  uint32_t vr = 0;
  int mode;
  int dx = s.width / 9;   // 28x15=420
  
  /* Draw colors */
#if 0
  s.scrrect(TX, 0, TX+240, 160, MKRGB(0xAF, 0xAF, 0xAF));
  for (i = 0; i < 512; i++) {
    p = 0x110000;
    s.scrrect(TX + (i % 32) * 7, (i / 32) * 10, 6, 9, BGRRGB(p));
  }
#endif
#if 0
  int px, py;
  /* Draw Tile Tables */
  int cbb = 0x4000;
  for (int i = 0; i < 512; i+=dx) {
    _dts(TX + px, py, dx, 9, 1, cbb, i, palsel * 16, 8);
    py += 9;
  }
#endif
  
  /* Draw video graphics */
  mode = extract(R_DISPCNT, DCNT_MODE);
  flogger(0,"frame %6d mode = %x page:%x\n", frame, mode, extract(R_DISPCNT, DCNT_PAGE));
  switch (mode) {
  case 0:
    /* Draw BGR0 */
    drawnt(0);
    drawnt(1);
    drawnt(2);
    drawnt(3);
    drawsprites();
    break;
  case 3: // 240x160x16
    genbmp(vr, 240, 160, 16);
    break;
  case 4: // 240x160x8
    if (R_DISPCNT & DCNT_PAGE)
      vr += 0xA000;
    genbmp(vr, 240, 160, 8);
    break;
  case 5: // 160x128x16
    if (R_DISPCNT & DCNT_PAGE)
      vr += 0xA000;
    genbmp(vr, 160, 128, 16);
    break;
  }
  s.scrtext(0, s.height+BB, MKRGB(0x7f, 0x7f, 0x00), "frame:%d fps:%.2f", frame, fps);
  s.scrtext(0, s.height+BB+8, MKRGB(0x7f, 0x7f, 0x00),
	    "!mode: %x-%d%d%d%d.%d",
	    ppu.bgMode,
	    ppu.bg[0].bgEn,
	    ppu.bg[1].bgEn,
	    ppu.bg[2].bgEn,
	    ppu.bg[3].bgEn,
	    ppu.obj.objEn);
}

void gba::setkeymask(int key, int mask)
{
  if (s.key(key, true)) {
    R_KEYPAD &= ~mask;
  }
  else
    R_KEYPAD |= mask;
}

/* 4 cycles/pixel x (240+68) x (160 + 68) = 280896 cycles per frame */
void gba::ppu_tick()
{
  static int frmbit;
  int raster;

  frmbit++;
  raster = extract(R_DISPSTAT, DSTAT_VCT);
  if (scanline == raster && !clk) {
    setLYC(true);
  }
  if (scanline == VBLANK_START && !clk) {
    setVblank(true);
  }
  if (clk == HBLANK_START) {
    setHblank(true);
  }
  if (++clk == MAX_LX) {
    clk = 0;
    setHblank(false);
    if (++scanline == MAX_LY) {
      setVblank(false);
      scanline = 0;
      genframe();
      s.draw();
      frame++;
      printf("================ FRAME: %6d, %d cycles\n", frame, frmbit * 4);
      frmbit = 0;

      setkeymask(Key::K_UP,    KEYINPUT_UP);
      setkeymask(Key::K_DOWN,  KEYINPUT_DOWN);
      setkeymask(Key::K_LEFT,  KEYINPUT_LEFT);
      setkeymask(Key::K_RIGHT, KEYINPUT_RIGHT);

      setkeymask('i',    KEYINPUT_UP);
      setkeymask('k',  KEYINPUT_DOWN);
      setkeymask('j',  KEYINPUT_LEFT);
      setkeymask('l', KEYINPUT_RIGHT);
      
      setkeymask('s', KEYINPUT_SEL);
      setkeymask('a', KEYINPUT_START);
      setkeymask('z', KEYINPUT_A);
      setkeymask('x', KEYINPUT_B);
      setkeymask('q', KEYINPUT_L);
      setkeymask('e', KEYINPUT_R);
    }
  }
  if (frame == 100) {
    cpu_shutdown();
    exit(0);
  }
}

/*==========================================================*
 * Common CPU Read/Write routines
 *==========================================================*/
bus_t mmu(0x0FFFFFFF);

uint8_t cpu_read8(uint32_t off, int type) {
  iodata_t data;
  mmu.read(off, data);
  return data;
};

void cpu_write8(uint32_t off, uint8_t data, int type) {
  mmu.write(off, data);
}

uint16_t cpu_read16(uint32_t off, int type) {
  iodata_t data;
  mmu.read(off, data, _SZ16);
  return data;
};

void cpu_write16(uint32_t off, uint16_t data, int type) {
  mmu.write(off, data, _SZ16);
}

uint32_t cpu_read32(uint32_t off, int type) {
  iodata_t data;
  mmu.read(off, data, _SZ32);
  return data;
};

void cpu_write32(uint32_t off, uint32_t data, int type) {
  mmu.write(off, data, _SZ32);
}

/* New interface.can get read/write 8/16/32 */
int gbamem(void *arg, uint32_t off, int mode, iodata_t &data)
{
  uint8_t *buf = (uint8_t *)arg;
  
  switch(mode) {
  case _WR8:
    put8(buf + off, data);
    break;
  case  _WR16:
    put16(buf + off, data);
    break;
  case _WR32:
    put32(buf + off, data);
    break;
  case _RD8:
    data = get8(buf + off);
    break;
  case _RD16:
    data = get16(buf + off);
    break;
  case _RD32:
    data = get32(buf + off);
    break;
  }
  return 0;
}

int nbacc(void *arg, uint32_t off, int mode, iodata_t &data)
{
  if (mode == _RD16 || mode == _WR16 || mode == _RD32 || mode == _WR32)
    gbamem(arg, off, mode, data);
  else if (mode == _RD8)
    data = 0xFF;
}

void dodma(uint32_t dst, uint32_t src, uint16_t count, int incr)
{
  uint32_t v;

  while (count--) {
    switch (incr) {
    case 2:
      v = cpu_read16(src);
      cpu_write16(dst, v);
      break;
    case 4:
      v = cpu_read32(src);
      cpu_write32(dst, v);
      break;
    }
    src += incr;
    dst += incr;
  }
}

void rundma(uint32_t dad, uint32_t sad, uint16_t len, uint16_t cnt)
{
  printf("DMA enabled: %.8x %.8x %.4x %s\n", sad, dad, len, cnt & D10 ? "32" : "16");
  if (cnt & D15)
    dodma(dad, sad, len, cnt & D10 ? 4 : 2);
}

int gba::regio(uint32_t offset, int mode, iodata_t& data)
{
  int tmp;
  
  gbamem(ioreg, offset & 0x3FF, mode, data);
  if ((mode & 0xFF) != 'w')
    return 0;
  switch (offset) {
  case DISPCNT:
    printf("Set Display: %x\n", data);
    ppu.bgMode = data & DCNT_MODE;
    ppu.bgPage = data & DCNT_PAGE ? 1 : 0;
    ppu.bg[0].bgEn = (data & DCNT_BG0) != 0;
    ppu.bg[1].bgEn = (data & DCNT_BG1) != 0;
    ppu.bg[2].bgEn = (data & DCNT_BG2) != 0;
    ppu.bg[3].bgEn = (data & DCNT_BG3) != 0;
    ppu.obj.objEn  = (data & DCNT_OBJ) != 0;
    ppu.obj.obj1d  = (data & DCNT_OBJ_1D) != 0;
    printf("Mode:%d.%d-%d%d%d%d-%d%d\n",
	   ppu.bgMode, ppu.bgPage,
	   ppu.bg[0].bgEn,ppu.bg[1].bgEn,ppu.bg[2].bgEn,ppu.bg[3].bgEn,
	   ppu.obj.objEn, ppu.obj.obj1d);
    break;
  case BG0CNT:
  case BG1CNT:
  case BG2CNT:
  case BG3CNT:
    tmp = (offset - BG0CNT)/2;
    printf("SET CONTROL: BG%d: %.4x\n", tmp, data);
    // SS_s.ssss.C__c.ccpp
    ppu.bg[tmp].cbb = (data >> 0x2) & 0x03;
    ppu.bg[tmp].sbb = (data >> 0x8) & 0x1F;
    ppu.bg[tmp].pri = (data >> 0x0) & 0x03;
    ppu.bg[tmp].sz  = (data >> 0xE) & 0x03;
    ppu.bg[tmp].cm  = (data & (1L << 7)) ? 8 : 4;
    break;
  case DMA0CNT_L: rundma(R_DMA0DAD, R_DMA0SAD, R_DMA0CNT_L, R_DMA0CNT_H); break;
  case DMA1CNT_L: rundma(R_DMA1DAD, R_DMA1SAD, R_DMA1CNT_L, R_DMA1CNT_H); break;
  case DMA2CNT_L: rundma(R_DMA2DAD, R_DMA2SAD, R_DMA2CNT_L, R_DMA2CNT_H); break;
  case DMA3CNT_L: rundma(R_DMA3DAD, R_DMA3SAD, R_DMA3CNT_L, R_DMA3CNT_H); break;
    break;
  case BG0HOFS:
  case BG1HOFS:
  case BG2HOFS:
  case BG3HOFS:
    tmp = (offset - BG0HOFS)/4;
    //printf("SET BG%d HOFS:%.8x\n", tmp, data);
    break;
  case BG0VOFS:
  case BG1VOFS:
  case BG2VOFS:
  case BG3VOFS:
    tmp = (offset - BG0VOFS)/4;
    //printf("SET BG%d VOFS:%.8x\n", tmp, data);
    break;
  }
  return 0;
}

int vramio(void *arg, uint32_t offset, int mode, iodata_t& data)
{
  gbamem(arg, offset, mode, data);
  return 0;
}

extern int test(int npc);

uint8_t *mkbuf(size_t sz) {
  uint8_t *ptr = new uint8_t[sz]{0};
  assert(ptr != NULL);
  return ptr;
}

extern void test1();

void gba::init(uint8_t *buf, size_t len)
{
  size_t blen;
  
  gbahdr_t *hdr;
  hdr = (gbahdr_t *)buf;
  flogger(0,"loaded %d bytes\n", len);
  flogger(0,"Entry: %.8x\n", hdr->entry);
  
  bios   = loadrom("gba_bios.bin", blen);
  bram   = mkbuf(_256k);
  cram   = mkbuf(_32k);
  palram = mkbuf(_1k);
  vram   = mkbuf(_96k); // really 96k
  
  assert(bios && bram && cram && palram && vram);
  mmu.register_handler(0x00000000, 0x00003FFF, 0x03FFF,  gbamem, bios,  _RW, "BIOS");
  mmu.register_handler(0x02000000, 0x02FFFFFF, 0x3FFFF,  gbamem, bram,  _RW, "ERAM");
  mmu.register_handler(0x03000000, 0x03FFFFFF, 0x07FFF,  gbamem, cram,  _RW, "IRAM");
  mmu.register_handler(0x04000000, 0x040003FF, 0xFFFFFFF,regio,  ioreg, _RW, "I/O Registers");
  mmu.register_handler(0x05000000, 0x05FFFFFF, 0x003FF,  gbamem, palram,_RW, "Palette RAM");
  
  /* Splitting VRAM into lower and upper (sprite) blocks */
  for (int i = 0x06000000; i < 0x06ffffff; i += 0x20000) {
    mmu.register_handler(i+0x00000, i+0x0FFFF, 0xFFFF,   nbacc, vram,          _RW, "VRAM");
    mmu.register_handler(i+0x10000, i+0x1FFFF, 0x7FFF,   nbacc, vram+0x10000,  _RW, "VRAM/Tiles");
  }
  
  mmu.register_handler(0x07000000, 0x07FFFFFF, 0x3FF,    nbacc,  oam,   _RW, "OAM");
  mmu.register_handler(0x08000000, 0x09FFFFFF, 0x1FFFFFF,gbamem, buf,   _RD, "ROM Wait State 0");
  mmu.register_handler(0x0A000000, 0x0BFFFFFF, 0x1FFFFFF,gbamem, buf,   _RD, "ROM Wait State 1");
  mmu.register_handler(0x0C000000, 0x0DFFFFFF, 0x1FFFFFF,gbamem, buf,   _RD, "ROM Wait State 2");
  
  memset(palram, 0xaa, _1k);
  memset(oam, 0xaa, sizeof(oam));
  cpu_reset();
}

void gba::run()
{
  for(;;) {
    cpu_step();
    thegame.ppu_tick();
    if (frame == 300)
      break;
  }
}

extern void gentbl();

int main(int argc, char *argv[])
{
  uint8_t *buf;
  size_t len;
  int n;

  gentbl2();
  gentbl();
  setbuf(stdout, NULL);

  n = 1;
  if (argc > 1 && !strcmp(argv[1], "-t")) {
    printf("TRACE:\n");
    trace=1;
    --argc;
    n = 2;
  }
  if (argc > 1) {
    buf = loadrom(argv[n], len);
    thegame.init(buf, len);
    
    test1();
    if (argc > 2) {
      iztest = 1;
      test(strtoull(argv[n+1], NULL, 0));
    }
    else {
      //s.init();
    }
    s.clrmode = BGR;
    for (int i = 1; i < 128; i++) {
      s.setpalette(i, rand(), rand(), rand());
    }
    thegame.run();
  }
}
