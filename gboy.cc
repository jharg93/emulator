#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <inttypes.h>
#include <assert.h>
#include <stddef.h>
#include <time.h>
#include "util.h"
#include "bus.h"
#include "cpu.h"
#include "gr.h"
#include "gboy.h"

#define VRAM_BANK1 0x2000

uint8_t ntflag;

void dumpcfg(uint8_t *buf, size_t len, int off);
void runppu(int ticks);
void getpal(int *pp, uint8_t bgp);

struct sline {
  int valid;
  int pri;
  int pxl;
};

// 0 = white
// 1 = light gray
// 2 = dark grey
// 3 = black
int palbase;

static palclr gbpal[] = {
  { 0xFF,0xFF,0xFF }, { 0xBF,0xBF,0xBF }, { 0x3F,0x3F,0x3F }, { 0x00,0x00,0x00 },  // b&w
  { 0x1F,0xFF,0x1F }, { 0x1A,0xAA,0x1A }, { 0x15,0x55,0x15 }, { 0x00,0x11,0x00 },  // green
  { 0xFE,0xd0,0x18 }, { 0xd3,0x56,0x00 }, { 0x5e,0x12,0x10 }, { 0x0d,0x04,0x05 },  // orange
  { 0xda,0xff,0x22 }, { 0x48,0x9a,0x0d }, { 0x0b,0x3e,0x08 }, { 0x06,0x06,0x01 }   // dmg green
};

struct tileset {
  uint8_t *set = NULL;

  int      attr = 0;
  int      x = 0;
  int      y = 0;
  int      w = 8;
  int      h = 8;

  /* Set tileset resolion */
  void setres(int _x, int _y, int _w, int _h, int _a) {
    attr = _a;
    x = _x;
    y = _y;
    w = _w;
    h = _h;
  };
  /* 2BPP getpixel */
  int getpixel(int x, int y) {
    int off, a0, a1;

    assert(x < w && y < h);
    /* pixel 0 is leftmost, so flip shift */
    if ((attr & ATTR_FLIPX) == 0)
      x = w - x - 1;
    if (attr & ATTR_FLIPY)
      y = h - y - 1;
    off = y * 2;
    a0 = set[off+0];
    a1 = set[off+1] << 1;
    return ((a1 >> x) & 2) + ((a0 >> x) & 1);
  };
  int XPOS() const   { return x; };
  int YPOS() const   { return y; };
  int WIDTH() const  { return w; };
  int HEIGHT() const { return h; };
  int PRI() const    { return (attr & ATTR_PRIORITY) != 0; };
  int FLAGS() const  { return (attr & (ATTR_FLIPY|ATTR_FLIPX)); };
  int PAL() const    { return ((attr & 7) * 4); };
};

struct oam_t : public tileset {
};

struct tile_t {
  uint8_t *base;
  uint8_t  attr;
  int h;

  /* Set tile map base */
  void set(uint8_t *vram, int tid, uint8_t _attr, int _h = 8) {
    h = _h - 1;
    attr = _attr;
    base = &vram[tid * 16];
    if (attr & ATTR_BANK) {
      base += VRAM_BANK1;
    }
  };
  void getpixels(int *pxl, int y) {
    int sx, sy, a0, a1;
    if (!inrange(y, 0, h)) {
      return;
    }
    sy = (attr & ATTR_FLIPY) ? (h - y - 1) : y;
    a0 = base[(sy * 2) + 0];
    a1 = base[(sy * 2) + 1];

    for (int x = 0; x < 8; x++) {
      sx = (attr & ATTR_FLIPX) ? x : (8 - x - 1);
      pxl[sx] = (a0 & 0x1) + (a1 & 0x2);
      a0 >>= 1;
      a1 >>= 1;
    };
  }
};

/* Colors:
 * DMG:
 *  FF47 BGP   33.22.11.00
 *  FF48 OBP0  33.22.11.--
 *  FF49 OBP1  33.22.11.--
 *
 * CGB
 *  BGP0..7 BGR555,BGR555,BGR555,BGR555 (8 bytes)
 *  OBP0..7 BGR555,BGR555,BGR555,------ (8 bytes)
 *
 * Tile Attr: pvh-bnnn (CGB only)
 *   p: 1=BG Priority, 0=OAM priority
 *   v: vflip
 *   h: hflip
 *   b: vram bank (0,1)
 *   n: BGP0-7
 *
 * Sprite Attr: pvhPbnnn
 *   p: 1=OBJ behind BG1-3, 0=OBJ above BG
 *   v: vflip
 *   h: hflip
 *   P: 0=OBP0, 1=OBP1 (DMG)
 *   b: vram bank (0,1)
 *   n: OBP0-7         (CGB)
 *
 * Priority:
 *  LCDC.0=0: DMG BG=00; sprites always have priority
 *  LCDC.0=1: DMG WIN/BG have priority over sprites
 *
 *  LCDC.0=0: CGB sprites always have priority 
 *  LCDC.0=1: WIN/BG have priority over sprites
 *
 *  BGATTR.7=0, use oam priority
 *  BGATTR.7=1, BG has priority
 *  
 *  OAM.7=0: obj above bg
 *  OAM.7=1: obj behind bg 1-3
 *
 * NEW :
 *  lcdc.0=0: dmg: bg/window enable
 *  lcdc.0=0: cgb: sprites priority == 1, else sprite.priority == 0
 */
constexpr bool clip(const int x, const int y, const int x1, const int y1, const int x2, const int y2) {
  return !(x >= x1 && x < x2 && y >= y1 && y < y2);
}

Screen *scr;
void flogger(int, const char *, ...);

extern int SPC;

/* 4 bytes per sprite x 40 sprites = 160 bytes
 * Max 10 sprites per line
 */
struct gboam_t {
  uint8_t y;
  uint8_t x;
  uint8_t tid;
  uint8_t attr;
};
static uint8_t oamdata[0x100];
static gboam_t *gboam = (gboam_t *)oamdata;

static uint8_t vram[0x4000]; // two banks

int show_lyc;
int izhlt;
int trace=1;
uint64_t totcyc;
int rwtick;

time_t fpstime = time(NULL);

int isCGB = 0;
/* BGP0: 0x10
 * BGP1: 0x14
 * BGP2: 0x18
 * BGP3: 0x1C
 * BGP4: 0x20
 * BGP5: 0x24
 * BGP6: 0x28
 * BGP7: 0x2C
 */
static uint8_t bgp[0x40];
static uint8_t obp[0x40];

#define CGB_R(x) (((x) & 0x1F)*8)
#define CGB_G(x) ((((x) >> 5)&0x1F)*8)
#define CGB_B(x) ((((x) >> 10)&0x1F)*8)

static void rundma(uint32_t dst, uint32_t src, uint32_t len)
{
  while (len--) {
    uint8_t p = cpu_read8(src++, dstk::DMA);
    cpu_write8(dst++, p, dstk::DMA);
  }
}

// 4 * 8 palettes
void setpal(int base, uint8_t *bp)
{
  uint16_t pp;
  
  for (int i = 0; i < 32; i++) {
    pp = (bp[1] << 8) + bp[0];
    scr->setpalette(base++, CGB_R(pp), CGB_G(pp), CGB_B(pp));
    bp += 2;
  }
}

void flogger(int lvl, const char *fmt, ...)
{
  va_list ap;

  va_start(ap, fmt);
  if (lvl >= 0) {
    //fprintf(stdout, "%s |", cpu_getstate());
    //fprintf(stdout, "%s | ", gr_getstate());
  }
  vfprintf(stdout, fmt, ap);
}

/* Mainbus */
bus_t mb(0xFFFF);

uint8_t cpu_read8(uint32_t off, int type) {
  uint8_t data = 0xff;

  /* Tick if not DMA */
  if (type != dstk::DMA) {
    rwtick += 4;
    runppu(1);
  }
  mb.read(off, data);
  return data;
}

void cpu_write8(uint32_t off, uint8_t v, int type) {
  /* Tick if not DMA */
  if (type != dstk::DMA) {
    rwtick += 4;
    runppu(1);
  }
  mb.write(off, v);
};

/*===================================================*
 * Gameboy Functions
 *===================================================*/
int setmask(int key, uint8_t & k, uint8_t m)
{
  if (scr->KeyState[key]) {
    flogger(0, "keystate: '%c'\n", key);
    k |= m;
  }
  else
    k &= ~m;
  return 0;
}

/* Register name */
const char *rn(int offset) {
#define o(id, addr, rw, desc) case id: return #id;
  switch(offset) {
    CHIPREG(o);
  }
#undef o
  return "";
}

#define ffio(x) regs[x - 0xFF00]

void show_ioregs(uint8_t *regs)
{
  printf("SB:    %.2x\n", ffio(SB));
  printf("SC:    %.2x\n", ffio(SC));
  printf("DIV:   %.2x\n", ffio(DIV));
  printf("TIMA:  %.2x\n", ffio(TIMA));
  printf("TMA:   %.2x\n", ffio(TMA));
  printf("TAC:   %.2x\n", ffio(TAC));
  printf("IF:    %.2x\n", ffio(IF));
  printf("IE:    %.2x\n", ffio(IE));
  printf("LCDC:  %.2x\n", ffio(LCDC));
  printf("STAT:  %.2x\n", ffio(STAT));
  printf("SCX:   %.2x\n", ffio(SCX));
  printf("SCY:   %.2x\n", ffio(SCY));
  printf("LY     %.2x\n", ffio(LY));
  printf("LYC    %.2x\n", ffio(LYC));
}

/* CPU: 4194304 Hz / 256 = 16384 Hz (16320) */
/* DIV_DMG: 16384 = 4194304 / 256
 * DIV_SGB: 16779 = 4194304 / 250
 * DIV_CGB: 32768 = 4194304 / 128
 */
#define DIV_CYCLES (256/4)

// 00=4KHz, 01=256Khz, 10=64Khz, 11=16Khz
#define KHZ(n) ((1024*1024) / ((n) * 1024))

static int tac_count[] = { 1024/4, 16/4, 64/4, 256/4 };

#define GBREG(x,a) uint8_t& r_##x = ioregs[a & REGMASK]

#include "gboy_apu.cc"

struct bgmap {
  bool en = false;
  int  x = 0;
  int  y = 0;
  uint8_t *map = NULL;
  uint8_t *set = NULL;
};

struct gameboy : public crtc_t {
  /* Register area 0xFF00..0xFFFF */
  uint8_t  ioregs[0x100];

  uint8_t  r_STAT; // FF41

  gameboy() {
    crtc_t::init(64, 144-64, 144, 153-144);
  };

  /* CRTC overrides */
  void sethblank(bool state) {
    if (state == true) {
      setmode(0, STAT_MODE0_INTR);
      drawline(vPos);
    }
  };
  void setvblank(bool state) {
    if (state == true) {
      setmode(1, STAT_MODE1_INTR);
      requestirq(VBLANK_INTR, "VBLANK");
      drawppu();
    };
  };
  
  /* These registers are references to ioregs */
  GBREG(DIV,   0xFF04);
  GBREG(TIMA,  0xFF05);
  GBREG(TMA,   0xFF06);
  GBREG(TAC,   0xFF07);

  GBREG(P1,    0xFF00);
  GBREG(LCDC,  0xFF40);
  GBREG(SCY,   0xFF42);
  GBREG(SCX,   0xFF43);
  GBREG(LY,    0xFF44);
  GBREG(LYC,   0xFF45);
  GBREG(DMA,   0xFF46);
  GBREG(BGP,   0xFF47);
  GBREG(OBP0,  0xFF48);
  GBREG(OBP1,  0xFF49);
  GBREG(WX,    0xFF4B);
  GBREG(WY,    0xFF4A);
  GBREG(BGPI,  0xFF68);
  GBREG(OBPI,  0xFF6A);
  
  GBREG(IE,    0xFFFF);
  GBREG(IF,    0xFF0F);

  /* CGB */
  GBREG(VBK,   0xFF4F);
  GBREG(HDMA1, 0xFF51); // dma src hi xxxx.xxxx.xxxx.0000 | 0000-7FF0 or A000-DFF0
  GBREG(HDMA2, 0xFF52); // dma src lo
  GBREG(HDMA3, 0xFF53); // dma dst hi xxxx.xxxx.xxxx.0000 | 8000-9FF0
  GBREG(HDMA4, 0xFF54); // dma dst lo
  GBREG(HDMA5, 0xFF55); // dma len/start (len = ((val & 0x7f)+1) * 0x10

  /* Window position and lyc interrupt */
  int  wx;
  int  wy;
  bool lyc;

  Timer    div_tmr;
  Timer    tima_tmr;
  uint32_t tac_rate;
  
  uint8_t  romen;

  /* DMA channel */
  uint16_t dmasrc;
  uint16_t dmadst;
  uint16_t dmalen;
  uint8_t  controller[1];

  /* Background maps for window/screen */
  bgmap bg0;
  bgmap win;

  /* MBC */
  int    rombk = 0;
  int    rambk = 0;
  int    ramsel = false;
  int    ram_enabled = false;

  /* Memory banks */
  banksy rom1;      /* CART: 4000..7FFF */
  banksy vbank;     /* VRAM: 8000..9FFF */
  banksy ram1;      /* CRAM: A000..BFFF */
  banksy wbank;     /* IRAM: C000..CFFF */

  uint8_t *pg0;
  uint8_t *bootrom;
  uint8_t *cartrom;
  size_t  romsz;
  uint8_t *cartram;
  size_t  ramsz;
  
  uint8_t zpg[128];
  uint8_t iram[0x8000]; // bank 0-7
  
  #define INPUT_KEYS 0x0F
  #define INPUT_P14  0x10
  #define INPUT_P15  0x20
  void updateP1() {
    r_P1 |= 0xF;
    if (!(r_P1 & INPUT_P14))
      r_P1 &= 0xF0 | ((controller[0] & 0x0f) ^ 0x0f);
    if (!(r_P1 & INPUT_P15))
      r_P1 &= 0xF0 | (((controller[0] >> 4) & 0xF) ^ 0xF);
  }
  int gbregio(uint32_t offset, int mode, uint8_t &data) {
    /* Default update register via reference */
    memio(ioregs, offset & 0xFF, mode, data);
    if (mode == 'w' && (offset <= 0xff7f || offset == 0xffff)) {
      fprintf(stdout,"%.8x %.4x: Write %.4x[%5s] = %.2x\n",
	      rom1.bank, SPC, offset, rn(offset), data);
    }
    switch (offset) {
    case P1:
      if (mode == 'r') {
	data = 0xc0 | r_P1;
      }
      else {
	r_P1 = data & 0x30;
	updateP1();
      }
      break;
    case 0xFF50:
      // enable 1st page from bootrom
      memio(&romen, 0, mode, data);
      if (mode == 'w') {
	/* Override bus registration */
	pg0 = cartrom;
	flogger(0, "Dump REGISTERS\n");
	dump(ioregs, sizeof(ioregs), 32);
	show_ioregs(ioregs);
      }
      break;
    case STAT:
      if (mode == 'w') {
	flogger(0, "irq SET STAT:LYC=LY:%d OAM:%d VB:%d HB:%d COIN:%d [%.2x]\n",
		!!(data & 0x40),
		!!(data & 0x20),
		!!(data & 0x10),
		!!(data & 0x08),
		!!(data & 0x04),
		data);
	/* Set writeable bits */
	r_STAT = (r_STAT & 0x7) | (data & 0xF8);
      }
      else {
	data = r_STAT;
      }
      break;
    case TIMA:
      if (mode == 'w') {
	flogger(0, "set tima: %.2x %d %d\n", r_TIMA, tima_tmr.count, tima_tmr.reset);
      }
      else {
	flogger(0, "read tima: %.2x\n", r_TIMA);
      }
      break;
    default:
      if (rn(offset) != NULL) {
	break;
      }
      if (mode == 'w')
	flogger(0, "unknown port write: %.4x %.2x\n", offset,data);
      else
	flogger(0, "unknown port read:  %.4x %.2x\n", offset,data);
      assert(0);
      break;
    };

    if (mode != 'w') {
      return 0;
    }
    /* ===========================
     * Write mode only
     * ==========================*/
    switch (offset) {
    case LCDC:
      fprintf(stdout,"=========================================================\n");
      fprintf(stdout,"  Display Enabled:%c\n", data & LCDC_EN ? 'y' : 'n');
      fprintf(stdout,"  Sprite Enabled: %c/%d\n",
	      data & LCDC_SPRITE_EN ? 'y' : 'n',
	      data & LCDC_SPRITE_SZ ? 16 : 8);
      fprintf(stdout,"  BG Enabled    : %c nt:%.4x tile:%.4x\n",
	      data & LCDC_EN  ? 'y' : 'n',
	      data & LCDC_BG_MAP ? 0x9c00 : 0x9800,
	      data & LCDC_BG_TILESEL ? 0x8000 : 0x8800);
      fprintf(stdout,"  Window enabled: %c nt:%.4x\n",
	      data & LCDC_WINDOW_EN ? 'y' : 'n',
	      data & LCDC_WINDOW_MAP ? 0x9c00 : 0x9800);

      /* Setup map and tile pointers */
      bg0.map = &vram[(data & LCDC_BG_MAP)     ? 0x1c00 : 0x1800];
      bg0.set = &vram[(data & LCDC_BG_TILESEL) ? 0x0000 : 0x0800];
      win.map = &vram[(data & LCDC_WINDOW_MAP) ? 0x1c00 : 0x1800];
      win.set = bg0.set;
      if ((data & LCDC_BG_EN) != 0) {
	bg0.en = true;
	win.en = (data & LCDC_WINDOW_EN);
      }
      else if (isCGB) {
	bg0.en = true;
	win.en = (data & LCDC_WINDOW_EN);
      }
      else {
	bg0.en = false;
	win.en = false;
      }
      break;
    case BGPD:
      bgp[r_BGPI & 0x3F] = data;
      setpal(0x10, bgp);
      /* auto-advance index */
      if (r_BGPI & 0x80)
	r_BGPI = 0x80 + ((r_BGPI + 1) & 0x3F);
      break;
    case OBPD:
      obp[r_OBPI & 0x3F] = data;
      setpal(0x40, obp);
      /* auto-advance index */
      if (r_OBPI & 0x80)
	r_OBPI = 0x80 + ((r_OBPI + 1) & 0x3F);
      break;
    case DIV:
      flogger(0, "set DIV to %d %d\n", div_tmr.count, div_tmr.reset);
      r_DIV = 0;
      break;
    case TAC:
      tac_rate = tac_count[r_TAC & 0x03];
      tima_tmr.settimer(tac_rate, true, !!(r_TAC & 0x04), "TIMA");
      flogger(0, "set tac: %d %d %d\n", tac_rate, tima_tmr.count, tima_tmr.reset);
      break;
    case DMA:
      /* Setup DMA xfer */
      dmadst = 0xFE00;
      dmasrc = data * 0x100;
      dmalen = 0xA0;
      flogger(0, "DMA START: %.4x %.4x\n", dmadst, dmasrc);
      break;
    case HDMA5:
      {
	uint16_t src = (r_HDMA1 << 8) + (r_HDMA2 & 0xF0);
	uint16_t dst = (r_HDMA3 << 8) + (r_HDMA4 & 0xF0);
	uint16_t len = ((data & 0x7F) + 1) * 0x10;

	flogger(0, "HDMA: %.4x -> %.4x, %.4x bytes\n", src, dst, len);
	rundma(dst, src, len);
      }
      break;
    case VBK:
      if (isCGB) {
	setbank(&vbank, data & 1);
      }
      break;
    case WBANK:
      if (isCGB) {
	setbank(&wbank, data & 7);
      }
      break;
    case WX:
      wx = data - 7;
      break;
    }
    return 0;
  };

  int bgpri(uint8_t attr, sline& s, int pxl);

  void drawppu(void);
  void drawline(int ly);

  bool tick();
  void setmode(int, int);
  void requestirq(int flag, const char *what);

  void init(uint8_t *buf, int len);
  int getsprite(oam_t *o, int id, int y);
  int getsprites(int y, oam_t *o);
};

struct gameboy thegame;

bool gameboy::tick() {
  /* Tick clocks */
  apu.tick();
  if (div_tmr.tick()) {
    r_DIV++;
  }
  if (tima_tmr.tick()) {
    /* if timer expires, reset */
    if (++r_TIMA == 0) {
      requestirq(TIMER_INTR, "TIMER");
      r_TIMA = r_TMA;
    }
  }
  /* xfer OAM data via DMA, 1 cycle */
  if (dmalen > 0) {
    rundma(dmadst++, dmasrc++, 1);
    dmalen--;
  }

  // set ly register to scanline
  r_LY = vPos;
  if (hPos == 0) {
    /* Start of line... set LYC */
    if (r_LY == r_LYC) {
      lyc = true;
      r_STAT |= STAT_LYC_COINC;
      if (r_STAT & STAT_LYC_INTR) {
	/* generate coincidence interrupt */
	requestirq(LCDC_INTR, "LYC INTR");
      }
    } else {
      lyc = false;
      r_STAT &= ~STAT_LYC_COINC;
    }
  }
  if (vPos < vBlank) {
    /* visible area
     * 0..19   = OAM
     * 20..63  = Pixel (44 ticks)
     * 64..113 = HBLANK
     */
    if (hPos < 20) {
      /* OAM */
      setmode(2, STAT_MODE2_INTR);
    }
    else if (hPos < hBlank) {
      /* Drawing */
      setmode(3, 0);
    }
  }
  if (crtc_t::tick()) {
    /* End of frame, reset clocks, window counter, scanline */
    wy = 0;
    r_LY = 0;
    apu_end_frame();
    flogger(0, "--- frame %d\n", frame);
  }
  return false;
}

/*======================================================================*
 * Tile pattern { AABB, AABB, ... }
 *   00 = white
 *   01 = lightgrey
 *   10 = darkgrey
 *   11 = black
 * 
 * 256x256 pixel display, 160x144 viewport
 *
 * 144 lines:
 *   20 ticks:  oam search (80 dots)
 *   43 ticks:  pixel transfer [160pixels] (168 to 291 cycles)
 *   51 ticks:  hBlank (85 to 208 dots)
 * 10 lines:
 *   114 ticks: vblank (4560 dots)
 * 114 x 154 = 17556 clocks per screen
 *
 * 4.194 MHz dot clock, 154 scanlines = 70224 dots
 *======================================================================*/
static int pg0io(void *arg, uint32_t offset, int mode, uint8_t& data)
{
  gameboy *gb = (gameboy *)arg;

  return memio(gb->pg0, offset, mode, data);
}

static int regio(void *arg, uint32_t offset, int mode, uint8_t& data)
{
  gameboy *gb = (gameboy *)arg;

  return gb->gbregio(offset, mode, data);
}

static int mbcram(void *arg, uint32_t offset, int mode, uint8_t &data)
{
  gameboy *gb = (gameboy *)arg;
  
  if (!gb->ram_enabled) {
    data = 0xFF;
  }
  else {
    banksyio(&gb->ram1, offset, mode, data);
  }
  return 0;
}

static int mbcramen(void *arg, uint32_t offset, int mode, uint8_t &data)
{
  gameboy *gb = (gameboy *)arg;

  printf("mvc ram enable: %.4x = %.2x\n", offset, data);
  if ((data & 0xF) == 0x0A)
    gb->ram_enabled = true;
  else if ((data & 0xF) == 0x00)
    gb->ram_enabled = false;
  return 0;
}

/*===========================*
 * MBC1
 * 0000..1FFF: enable ram
 * 2000..3FFF: lo bank
 * 4000..5FFF: hi bank
 * 6000..7FFF: rom/ram
 *===========================*/
static int realbk(int n, int romsz) {
  int nbk = romsz / 0x4000;
  
  printf("nbk: %x %x\n", n, nbk);
  if ((n & 0xF) == 0)
    n++;
  return n;
}

static int mbc1io(void *arg, uint32_t offset, int mode, uint8_t &data)
{
  gameboy *gb = (gameboy *)arg;
  int& rombk  = gb->rombk;
  int& rambk  = gb->rambk;
  int& ramsel = gb->ramsel;
  
  if (offset <= 0x3FFF) {
    // Bank Lo: ___x.xxxx
    if (ramsel == 0) {
      rombk = (rombk & 0xE0) | (data & 0x1F);
    }
    else {
      rombk = (data & 0x1F);
    }
    setbank(&gb->rom1, realbk(rombk, gb->romsz), "MBC1.LO");
    flogger(0, "Setting mbc1.lo bank: [%.2x] %.2x of %.2x -> %.2x\n", data, rombk, gb->rom1.nbank, gb->rom1.bank);
    hexdump(gb->rom1.current, 32, 32);
  }
  else if (offset <= 0x5FFF) {
    if (ramsel) {
      // RAM bank
      rambk = data & 3;
      setbank(&gb->ram1, rambk, "MBC1.RAM1");
    }
    else {
      // Bank Hi: _xx_.____
      rombk = (rombk & 0x1F) | ((data << 5) & 0xE0);
      setbank(&gb->rom1, realbk(rombk, gb->romsz), "MBC1.HI");
      flogger(0, "Setting mbc1.hi bank: [%.2x] %.2x of %.2x -> %.2x\n", data, rombk, gb->rom1.nbank, gb->rom1.bank);
    }
  }
  else if (offset <= 0x7FFF) {
    flogger(0, "Setting mbc1.sel %d\n", data & 1);
    ramsel = (data & 0x1) == 1;
    setbank(&gb->rom1, realbk(gb->rom1.bank, gb->romsz), "MBC1.ROMSEL");
    setbank(&gb->ram1, gb->ram1.bank, "MBC1.RAMSEL");
  }
  else {
    flogger(0, "PC:%.4X mbc1io %.4x\n", SPC, offset);
  }
  return 0;
}

/*===========================*
 * MBC2
 * 0000-3FFF ram enable, rom bank number
 * bit 8 determines if ram enable or rom bank
 *===========================*/
static int mbc2io(void *arg, uint32_t offset, int mode, uint8_t &data)
{
  gameboy *gb = (gameboy *)arg;
  int& rombk = gb->rombk;

  if (offset & 0x100) {
    rombk = (data & 0xF);
    setbank(&gb->rom1, rombk);
  } else {
    gb->ram_enabled = ((data & 0xF) == 0xA);
  }
  return 0;
}

static int mbc2ram(void *arg, uint32_t offset, int mode, uint8_t &data)
{
  gameboy *gb = (gameboy *)arg;
  
  if (!gb->ram_enabled) {
    data = 0xFF;
  }
  else if (mode == 'r') {
    data = gb->cartram[offset];
  }
  else {
    gb->cartram[offset] = data;
  }
  printf("rram: %c %.4x %x\n", mode, offset, data);
  return 0;
}

/*
 * 0000-1FFF: wo RAM/Timer enable
 * 2000-3FFF: wo ROM bank number
 * 4000-5FFF: wo RAM bank or RTC register
 * 6000-7FFF: wo Latch Clock Data
 * A000-BFFF: rw RAM Bank 00-03
 * A000-BFFF: rw RTC Register 08-0C
 */
static int mbc3io(void *arg, uint32_t offset, int mode, uint8_t &data)
{
  gameboy *gb = (gameboy *)arg;
  int& rambk = gb->rambk;
  
  flogger(0, "PC:%.4X mbc3io %.4x %c %.2x\n", SPC, offset, mode, data);
  if (offset <= 0x3FFF) {
    int bk = (data & 0x7F);
    if (bk == 0)
      bk = 1;
    setbank(&gb->rom1, bk, "MBC3.ROM1");
  }
  else if (offset <= 0x5FFF) {
    rambk = data;
    if (rambk < 4)
      setbank(&gb->ram1, rambk, "MBC3.RAM");
  }
  else if (offset >= 0x4000 && offset <= 0x5FFF)
    flogger(0, " %s\n", data >= 0x00 && data <= 0x7 ? "RAMBANK" : "RTC");
  return 0;
}

static int mbc5io(void *arg, uint32_t offset, int mode, uint8_t &data)
{
  gameboy *gb = (gameboy *)arg;
  int& rombk = gb->rombk;
  
  flogger(0, "%.6x PC:%.4X mbc5io %.4x %c %.2x\n", gb->rom1.bank, SPC, offset, mode, data);
  if (offset <= 0x1FFF) {
    flogger(0, "mbc5 RAM %s\n", data ? "ENABLE" : "DISABLE");
  }
  else if (offset <= 0x2FFF) {
    rombk = (rombk & 0xFF00) | data;
    flogger(0, "mbc5.romlo: %.4x\n", rombk);
    setbank(&gb->rom1, rombk, "mbc5.romlo");
  }
  else if (offset <= 0x3FFF) {
    rombk = (rombk & 0x00FF) | (data << 8);
    flogger(0, "mbc5.romhi: %.4x\n", rombk);
    setbank(&gb->rom1, rombk, "mbc5.romhi");
  }
  else if (offset <= 0x5FFF) {
    flogger(0, "mbc5.ram: %.4x\n", data & 0xF);
    setbank(&gb->ram1, data & 0xF, "mbc5.ram");
  }
  return 0;
}

#define KBIT(n) (((n) * 1024) / 8)
int gbRomSz(int n) {
  printf("GB Rom Sz: %d\n", n);
  if (n <= 8)
    return 32768LL << n;
  assert(0);
}

int gbRamSz(int n) {
  return 1024 * 1024;
}

void gameboy::init(uint8_t *buf, int len)
{
  gbhdr_t *hdr = (gbhdr_t *)(buf + 0x100);
  int nbk;
  
  // Cartridge memory buffer
  size_t bootlen;
  romsz = gbRomSz(hdr->rom_sz);
  ramsz = gbRamSz(hdr->ram_sz);

  /* Check if declared size > ROM size */
  cartrom = buf;
  if (romsz > len) {
    cartrom = new uint8_t[romsz]{0};
    memcpy(cartrom, buf, len);
  }
  bootrom = loadrom("roms/DMG_ROM.bin", bootlen);
  if (!bootrom) {
    printf("No Bootrom!!\n");
    exit(0);
  }
  pg0 = bootrom;
  
  /* Init banks */
  cartram = new uint8_t[ramsz]{0};
  nbk = romsz / 0x4000;

  /* Setup rom bank pointer */
  rom1.init(256, "ROM1");
  for (int i = 0; i < 0x100; i++) {
    rom1.banks[i] = cartrom + (i % nbk) * 0x4000;
  }
  rom1.setbank(1);

  vbank.init(vram,   16384,  0x2000, 0, "VRAM"); // 0x8000-0x9fff. 8k
  ram1.init(cartram, ramsz,  0x2000, 0, "CRAM"); // 0xa000-0xbfff, 8k
  wbank.init(iram,   0x8000, 0x1000, 1, "IRAM"); // 0xc000-0xcfff, 4k
  // set bank 0 -> bank 1
  wbank.banks[0] = wbank.banks[1];
  
  fprintf(stdout,"Game Title: '%.*s'\n", 0xB, hdr->title);
  fprintf(stdout,"ROM size  : %.2x %d  banks=%d\n", hdr->rom_sz, (int)romsz, rom1.nbank);
  fprintf(stdout,"RAM size  : %.2x %d  banks=%d\n", hdr->ram_sz, (int)ramsz, ram1.nbank);
  fprintf(stdout,"mapper    : %.2x\n", hdr->type);
  fprintf(stdout,"CGB mode  : %x\n",   hdr->clrcompat);

  isCGB = hdr->clrcompat & 0x80;

 /* Register memory region handlers */
#ifdef NOBIOS
  mb.register_handler(0x0000, 0x3FFF, 0x3FFF,  memio,  buf,      _RD, "ROM Bank 0");
#else
  mb.register_handler(0x0000, 0x00FF, 0xFFFF,  pg0io,  this,     _RD, "BootROM:ROM Bank 0");
  mb.register_handler(0x0100, 0x3FFF, 0x3FFF,  memio,  buf,      _RD, "ROM Bank 0");
#endif
  mb.register_handler(0x4000, 0x7FFF, 0x3FFF,  banksyio, &rom1,  _RD, "ROM Bank 1-N");
  mb.register_handler(0x8000, 0x9FFF, 0x1FFF,  banksyio, &vbank, _RW, "VRAM CHR/BG area");
  mb.register_handler(0xA000, 0xBFFF, 0x1FFF,  mbcram,   this,   _RW, "Cartridge RAM - Bank 0-N");
  mb.register_handler(0xC000, 0xCFFF, 0x0FFF,  memio,  iram,     _RW, "Internal RAM - Bank 0");

  mb.register_handler(0xD000, 0xDFFF, 0x0FFF,  banksyio, &wbank, _RW, "Internal RAM - Bank 1-N");
  mb.register_handler(0xE000, 0xEFFF, 0x0FFF,  memio,  iram,     _RW, "Echo RAM - Bank 0");
  mb.register_handler(0xF000, 0xFDFF, 0x0FFF,  banksyio, &wbank, _RW, "Echo RAM - Bank 1-N");
  mb.register_handler(0xFE00, 0xFEFF, 0x00FF,  memio,  oamdata,  _RW, "OAM");
  mb.register_handler(0xFF00, 0xFF0F, 0xFFFF,  regio,  this,     _RW, "Registers");
  mb.register_handler(0xFF40, 0xFF7F, 0xFFFF,  regio,  this,     _RW, "Registers");
  mb.register_handler(0xFFFF, 0xFFFF, 0xFFFF,  regio,  this,     _RW, "Registers");
  mb.register_handler(0xFF10, 0xFF3F, 0xFFFF,  apu_io, this,     _RW, "APU Registers");
  mb.register_handler(0xFF80, 0xFFFE, 0x007F,  memio,  zpg,      _RW, "GB Zero Page");

  /* Setup mappers */
  switch (hdr->type) {
  case 1 ... 3:
    // 00
    // 01 512k
    // 02 1Mb
    // 03 2Mb
    // 04 4Mb
    // 05 8Mb
    // 06 16Mb
    for (int i = 0; i < 0x100; i += 0x20) {
      rom1.banks[i] = rom1.banks[i+1];
    }
    mb.register_handler(0x0000, 0x1FFF, 0xFFFF, mbcramen, this, _WR, "MBC1.RAM enable");
    mb.register_handler(0x2000, 0x3FFF, 0xFFFF, mbc1io, this, _WR, "MBC1.ROM1 bank");
    mb.register_handler(0x4000, 0x5FFF, 0xFFFF, mbc1io, this, _WR, "MBC1 ROM/RAM banking");
    mb.register_handler(0x6000, 0x7FFF, 0xFFFF, mbc1io, this, _WR, "MBC1 ROM/RAM sel");
    break;
  case 5 ... 6:
    rom1.banks[0] = rom1.banks[1];
    mb.register_handler(0x0000, 0x3FFF, 0xFFFF, mbc2io, this, _WR, "MBC2.RAM enable/ROM1 bank");
    mb.register_handler(0xA000, 0xBFFF, 0x01FF, mbc2ram,this, _RW, "MBC2 RAM");
    break;
  case 0x0F ... 0x13:
    for (int i = 0; i < 0x100; i += 0x20) {
      rom1.banks[i] = rom1.banks[i+1];
    }
    mb.register_handler(0x0000, 0x1FFF, 0xFFFF, mbcramen, this, _WR, "MBC3 RAM enable");
    mb.register_handler(0x2000, 0x3FFF, 0xFFFF, mbc3io, this, _WR, "MBC3 ROM1 bank");
    mb.register_handler(0x4000, 0x5FFF, 0xFFFF, mbc3io, this, _WR, "MBC3 RAM bank");
    mb.register_handler(0x6000, 0x7FFF, 0xFFFF, mbc3io, this, _WR, "MBC3 latch");
    break;
  case 0x19 ... 0x1e:
    mb.register_handler(0x0000, 0x1FFF, 0xFFFF, mbcramen, this, _WR, "MBC5 RAM enable");
    mb.register_handler(0x2000, 0x2FFF, 0xFFFF, mbc5io, this, _WR, "MBC5 ROM1 bank.lo");
    mb.register_handler(0x3000, 0x3FFF, 0xFFFF, mbc5io, this, _WR, "MBC5 ROM1 bank.hi");
    mb.register_handler(0x4000, 0x5FFF, 0xFFFF, mbc5io, this, _WR, "MBC5 RAM bank");
    break;
  case 0:
    break;
  }
  mb.dump();

  /* Setup DIV timer: 16384 Hz or 16779Hz on SGB, 32768 on CGB 2X */
  div_tmr.settimer(DIV_CYCLES, true, true, "DIV");

  //scr = new Screen(160+180+180, 144+150, 0, 40, xsizeof(gbpal), gbpal);
  scr = new Screen(160, 144, 0, 40, xsizeof(gbpal), gbpal);
  scr->init();
  apu_init();
}

// 16-bytes AABBAABB
void drawtile(int sx, int sy, uint8_t *ptr, int bgp, int hm=0, int vm=0, int trans=0, int mx=160, int my=144)
{
  int j, i, l, m;
  int pal[4], off, pp, a0, a1;

  hm ^= 0x7;
  getpal(pal, bgp);
  for (i = 0; i < 8; i++) {
    off = (i ^ vm) * 2;
    /* Get pointer to tile data */
    a0  = ptr[off];
    a1  = ptr[off+1];
    for (j=0; j<8; j++) {
      l = (a0 >> (j^hm)) & 1;
      m = (a1 >> (j^hm)) & 1;
      pp = (m << 1) | l;
      if (clip(sx + j, sy + i, 0, 0, mx, my))
	continue;
      scr->setpixel(sx + j, sy + i, pal[pp] + palbase + 4);
    }
  }
}

/* Draw a tile */
void drawtt(int tilebase, int tx, int ty, int mod = 16, int n = 256)
{
  int i, x, y;
  uint8_t attr, bgp, vm, hm, *vbk;
  int c = isCGB;

  isCGB = 0;
  x = y = 0;
  tilebase &= 0x3FFF;
  for (i = 0; i < n*16; i+= 16) {
    vbk = &vram[tilebase + i];
    if (isCGB) {
      /* +---+---+---+---+---+---+---+---+
       * |PRI| VF| HF|   |BNK|    PAL    |
       * +---+---+---+---+---+---+---+---+ */
      attr = vram[tilebase + i + VRAM_BANK1];
      vm  = (attr & 0x40) ? 7 : 0;
      hm  = (attr & 0x20) ? 7 : 0;
      bgp = (attr & 0x07) * 4 + 0x10;
      if (attr & 0x08) {
	vbk += VRAM_BANK1;
      }
    }
    else {
      vm = 0;
      hm = 0;
      bgp = 0x1b;
    }
    drawtile(tx + x * 9, ty + y * 9, vbk, bgp, hm, vm, 0, scr->width, scr->height);
    if (++x == mod) {
      y++;
      x = 0;
    }
  }
  isCGB = c;
}

void drawnt(int nt, int tt, int tx, int ty)
{
  int i, tile;

  for (i = 0; i < 32*32; i++) {
    tile = vram[nt + i];
    if (tt)
      tile = 256 + (int8_t)tile;
    drawtile(tx + (i % 32)*8, ty + (i / 32)*8, &vram[tile * 16], thegame.r_BGP);
  }
}

void showclr(int x, int y, int sp, int np)
{
  int i, sx;

  sx = 0;
  for (i = 0; i < np; i++) {
    scr->scrrect(x + sx, y, 8, 8, i+sp);
    sx += 9;
    if (sx >= 4*9) {
      y += 9;
      sx = 0;
    }
  }
}

/* 76543210 ->   7_5_3_1_._6_4_2_0  AA55
 * 76543210 -> 7._5_3_1__.6_4_2_0_  aa
 */

#define MAX_SPRITE 40
#define MAX_SPRITE_PER_LINE 10

/* Get common sprite header 
 * 8x8 : 16 bytes
 * 8x16: 32 bytes
 * 
 * Sprite tile data always at 8000-8FFF (256 x 16 bytes)
 */

int gameboy::getsprite(oam_t *o, int id, int y)
{
  int tid;
  
  if (id >= MAX_SPRITE)
    return -1;

  /* Get sprite position, size and attributes */
  o->setres(gboam[id].x - 8, gboam[id].y - 16,
	    8,thegame.r_LCDC & LCDC_SPRITE_SZ ? 16 : 8,
	    gboam[id].attr);
  if (!inrange(y, o->YPOS(), o->HEIGHT()))
    return -1;
  
  tid = gboam[id].tid;

  /* 16-pixel sprites start on even tile addr */
  if (o->h == 16) {
    tid &= 0xFE;
  }

  /* Get pointer to tileset */
  o->set = &vram[tid * 16];
  if (isCGB && (o->attr & ATTR_BANK) != 0) {
    o->set += VRAM_BANK1;
  }
  return 0;
}

void gameboy::drawppu()
{
  time_t now;
  float fps;
  static uint8_t tr;
  
  if (!(r_LCDC & LCDC_EN))
    return;
  setmask('t', tr, 0x1);
  trace = tr;
  
  setmask(';', ntflag, 0x1); // sprites
  setmask('/', ntflag, 0x2); // bg
  
  setmask('x', controller[0], 0x10); 
  setmask('z', controller[0], 0x20);
  setmask('a', controller[0], 0x40);
  setmask('s', controller[0], 0x80); 
  setmask(Key::K_UP, controller[0], 0x4);
  setmask(Key::K_DOWN, controller[0], 0x8);
  setmask(Key::K_LEFT, controller[0], 0x2);
  setmask(Key::K_RIGHT, controller[0], 0x1);
  updateP1();
  if (scr->key('u',true)) {
    palbase = (palbase + 4) % xsizeof(gbpal);
  }
  if (scr->key('y',true)) {
    show_lyc ^= 1;
  }
#if 0
  /* Show VRAM tiles */
  drawtt(0x0000, 180, 0, 36, 512);
  drawtt(0x2000, 180, 140, 36, 512);

  /* Show BGP and OBP */
  showclr(10, 160, 0x10, 32);
  showclr(80, 160, 0x40, 32);
#endif
  now = time(NULL);
  fps = (float)frame / (now - fpstime);
  scr->scrtext(0, scr->height+5, 0x31, "frame:%d fps:%.2f", frame, fps);
  scr->scrtext(0, scr->height+15, 0x31, "s:%d b:%d", !(ntflag & 1), !(ntflag & 2));
  scr->draw();
  scr->clear();
}

/* Get 4 palette colors */
void getpal(int *pp, uint8_t bgp)
{
  if (isCGB) {
    /* colors are consecutive */
    pp[0] = bgp+0;
    pp[1] = bgp+1;
    pp[2] = bgp+2;
    pp[3] = bgp+3;
  }
  else {
    /* DMG colors */
    pp[0] = palbase + (bgp & 3);
    pp[1] = palbase + (bgp >> 2) & 3;
    pp[2] = palbase + (bgp >> 4) & 3;
    pp[3] = palbase + (bgp >> 6) & 3;
  }
}

/* Screen is 256x256
 * Visible screen is 160x144
 * Window is 160x144 over sprites/bg
 */

/* Sort sprites by x-coord */
int sprsort(const void *a, const void *b)
{
  const oam_t *sa = (const oam_t *)a;
  const oam_t *sb = (const oam_t *)b;

  /* Check if sprites overlap at all */
  return sa->x - sb->x;
}

/* check if bg has priority
 *  returns 0 (sprite color)
 *  returns 1 (bg color
 */
int gameboy::bgpri(uint8_t attr, sline& s, int pxl)
{
  // check if disabled
  if (ntflag & 0x2)
    return 0;

  // if cgb, check pixel
  if (isCGB) {
    if ((r_LCDC & LCDC_BG_EN) != 0) {
      if ((attr & 0x80) && pxl) {
	return 1;
      }
    }
  }
  if (s.valid && (pxl == 0 || !s.pri)) {
    return 0;
  }
  return 1;
}

/* Get sprites on this line */
int gameboy::getsprites(int y, oam_t *spr)
{
  int nspr = 0;
  
  if ((r_LCDC & LCDC_SPRITE_EN) == 0) {
    return 0;
  }
  for (int i = 0; i < MAX_SPRITE; i++) {
    if (getsprite(&spr[nspr], i, y) == 0) {
      printf("sprite %2d: %3d,%3d pri:%d lcdc:%d\n",
	     i, spr[nspr].XPOS(), spr[nspr].YPOS(),
	     spr[nspr].PRI(),
	     r_LCDC & 0x1);
      nspr++;
    }
    if (nspr >= MAX_SPRITE_PER_LINE)
      break;
  }
  return nspr;
}

/* Pixel FIFO
 * 5 steps = 2 dots each
 * Get Tile: 9800:9C00
 */

/* Line rendering code = 456 dots / 6 = 76 dots per N
 * mode 0 hBlank         (85 to 208 dots, 20-49us)
 * mode 1 vblank         (4560 dots, 10 scanlines, 1087us)
 * mode 2 oam xy         (80 dots, 19us)
 * mode 3 read oam/vram  (168..291 dots, 40-60us)
 *
 * every 456 dots... cycles through mode 2,3,0
 * vblank = mode 1
 *  Mode 2  2_____2_____2_____2_____2_____2___________________2____
 *  Mode 3  _33____33____33____33____33____33__________________3___
 *  Mode 0  ___000___000___000___000___000___000________________000
 *  Mode 1  ____________________________________11111111111111_____
 */
// https://github.com/alloncm/MagenTests/tree/master/src/color_bg_oam_priority
#if 0
#endif
void gameboy::drawline(int y)
{
  int tile, tid, pxl;
  uint8_t sx, sy, attr, nspr = 0;
  oam_t spr[MAX_SPRITE_PER_LINE];
  sline   sprline[160] = {};
  tileset bg;
  int wy_incr = 0;
  int pal[4];
  
  /* Return if display not enabled */
  if (!(r_LCDC & LCDC_EN) || y >= 144)
    return;

  /* Get Sprites on this line */
  nspr = getsprites(y, spr);
  if (ntflag & 0x1)
    nspr = 0;

  /* Sort by X-priority for DMG */
  if (!isCGB) {
    qsort(spr, nspr, sizeof(oam_t), sprsort);
  }

  /* Draw sprite line */
  for (int i = nspr-1; i >= 0; i--) {
    oam_t s = spr[i];
    sx = s.XPOS();
    sy = y - s.YPOS();
    if (isCGB) {
      getpal(pal, ((s.attr & 7 ) * 4) + 0x40);
    }
    else {
      getpal(pal, s.attr & ATTR_PALETTE ? r_OBP1 : r_OBP0);
    }
    for (int x = 0; x < s.WIDTH(); x++) {
      pxl = s.getpixel(x, sy);
      if (pxl) {
	sprline[sx+x].valid = true;
	sprline[sx+x].pri = s.PRI();
	sprline[sx+x].pxl = pal[pxl];
      }
    }
  }
  
  /* Draw background/window */
  for (int x = 0; x < 160; x++) {
    uint8_t *map, *set;
    /* Background/Window enable or Sprite priority in CGB */
    if (win.en && y >= r_WY && x >= wx) {
      /* Window Tile Map 32x32 */
      sx = x - wx; // x + (160 - wx) % 160
      sy = wy;
      wy_incr = 1;
      map = win.map;
      set = win.set;
    }
    else {
      /* BG Tile Map 32x32 */
      sx = x + r_SCX;
      sy = y + r_SCY;
      map = bg0.map;
      set = bg0.set;
    }
    
    /* Get tile ID */
    tid = ((sy / 8) * 32) + (sx / 8);
    tile = map[tid];
    if ((r_LCDC & LCDC_BG_TILESEL) == 0) {
      /* 1=8000..8FFF unsigned
       * 0=8800..97FF signed
       *
       * 8000               1;00-7F
       * 8800 0:00-7F>80-FF 1:80-FF
       * 9000 0:80-FF>00-7F
       */
      tile ^= 0x80;
    }
    bg.set = &set[tile * 16];
    if (isCGB) {
      /* ATTR: pvh-bnnn */
      attr = map[tid + VRAM_BANK1];
      if (attr & ATTR_BANK) {
	/* Set addr in 2nd bank */
	bg.set += VRAM_BANK1;
	tile |= 0x100;
      }
      getpal(pal, ((attr & 7) * 4) + 0x10);
    }
    else {
      attr = 0;
      getpal(pal, r_BGP);
    }
    bg.attr = attr;
    if (!bg0.en && !win.en) {
      // neither background or window enabled, use palette 0 bg
      pxl = pal[0];
    }
    else {
      // Get BG pixel color
      pxl = bg.getpixel(sx % 8, sy % 8);
      if (bgpri(attr, sprline[x], pxl)) {
	pxl = pal[pxl];
      }
      else {
	pxl = sprline[x].pxl;
      }
    }
    scr->setpixel(x, y, pxl);
  }

  /* Show LYC line */
  if (lyc && show_lyc) {
    scr->scrline(0, r_LY, 160, 0x9);
  }

  /* Increment window y counter */
  wy += wy_incr;
}

/* Set mode:
 *   0 = HBLANK, interrupt, STAT.D3
 *   1 = VBLANK, interrupt, STAT.D4
 *   2 = OAM,    interrupt, STAT.D5
 *   3 = Drawing
 */
void gameboy::setmode(int mode, int flag)
{
  if ((r_STAT & STAT_MODE) == mode) {
    return;
  }
  if ((r_STAT & flag) != 0) {
    requestirq(LCDC_INTR, "SETMODE");
  }
  rmw(r_STAT, mode, STAT_MODE);
}

void gameboy::requestirq(int flag, const char *what)
{
  r_IF |= flag;
}

void runppu(int ticks)
{
  while (ticks--) {
    thegame.tick();
  }
}

struct irqevent gbirq[] = {
  { TIMER_INTR,  0x50, "TIMER"  },
  { LCDC_INTR,   0x48, "LCDC"   },
  { VBLANK_INTR, 0x40, "VBLANK" },
  { SERIAL_INTR, 0x58, "SERIAL" },
};

/* Check if IRQ flag is set */
void doirq(int flag, int n, const char *what)
{
  if (thegame.r_IE & thegame.r_IF & flag) {
    if (cpu_irq(n)) {
      thegame.r_IF ^= flag;
    }
  }
}

const char *gr_getstate()
{
  static char dstr[64];

  snprintf(dstr, sizeof(dstr), "frame:%5d line:%5d clk:%3d",
	   thegame.frame, thegame.vPos, thegame.hPos);
  return dstr;
}

int main(int argc, char *argv[])
{
  int ncyc;
  uint8_t *buf;
  size_t len;
    
  //tester_run(&flags, &ops);
  //test();
  if (argc <= 1) {
    printf("usage; gboy <romfile>\n");
    return -1;
  }
  buf = loadrom(argv[1], len);
  if (!buf)
    return -1;

  if (argc > 3) {
    dumpcfg(buf, len, 0x100);
  }

  thegame.init(buf, len);

  /* Game loop */
  for(;;) {
    /* Unfreeze halted instruction */
    if ((thegame.r_IE && thegame.r_IF) && izhlt)
      izhlt = 0;

    /* Check interrupts */
    doirq(TIMER_INTR,  0x50, "TIMER");
    doirq(LCDC_INTR,   0x48, "LCDC");
    doirq(VBLANK_INTR, 0x40, "VBLANK");
    doirq(SERIAL_INTR, 0x58, "SERIAL");

    ncyc = 4;
    rwtick = 0;
    if (!izhlt) {
      ncyc = cpu_step();
      totcyc += ncyc;
    }
    if (rwtick > ncyc) {
      printf("TOO MANY TICKS %d %d\n", rwtick, ncyc);
      assert(0);
    }
    else if (rwtick < ncyc) {
      if (!rwtick && !izhlt) {
	printf("%.4x NOT ENOUGH TICKS: %d, %d %d\n", SPC, rwtick, ncyc, izhlt);
      }
      runppu((ncyc - rwtick)/4);
    }
  }
}
