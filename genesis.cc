#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <inttypes.h>
#include "util.h"
#include "cpu.h"
#include "bus.h"
#include "gr.h"
#include "genesis.h"
#include "cpu/cpu_m68k.h"
#include "cpu/cpu_z80.h"

extern int trace;

static const int vh_map[] = { 32, 64, -1, 128 };
static uint32_t irq_sts = 0;
static int ntflag = 7;

#define SCROLLA_ADDR (vdp_reg[2] << SCROLLA_ADDR_SHIFT)
#define SCROLLB_ADDR (vdp_reg[4] << SCROLLB_ADDR_SHIFT)
#define WINDOW_ADDR  (vdp_reg[3] << WINDOW_ADDR_SHIFT)
#define HSCROLL_ADDR (vdp_reg[13] << HSCROLL_ADDR_SHIFT)

struct vdp_t;

struct sprite_t {
  uint8_t ypos[2];
  uint8_t attr0[2];
  uint8_t attr1[2];
  uint8_t xpos[2];

  /* Sprite Attribute (8 bytes), big-endian
   * 256 32-cells: 64 sprites per frame, 16 per scanline
   * 320 40-cells: 80 sprites per frame, 20 per scanline
   * HSZ/VSZ
   *  00 8
   *  01 16
   *  10 24
   *  11 32
   *    15  14  13  12  10  11  9   8   7   6   5   4   3   2   1   0
   *   +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
   *   | 0 | 0 | 0 | 0 | 0 | 0 |             ypos                      | ypos
   *   +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
   *   | 0 | 0 | 0 | 0 |  HSZ  |  VSZ  | 0 |        next               | attr0
   *   +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
   *   |PR |  PL   | VF| HF|               tile                        | attr1
   *   +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
   *   | 0 | 0 | 0 | 0 | 0 | 0 |            xpos                       | xpos
   *   +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
   */
  int XPOS()   const { return (xpos[1] | ((xpos[0] & 0x3F) << 8)) - 128; };
  int YPOS()   const { return (ypos[1] | ((ypos[0] & 0x3F) << 8)) - 128; };
  int HEIGHT() const { return ((attr0[0] >> 0) & 0x3) * 8 + 8; };
  int WIDTH()  const { return ((attr0[0] >> 2) & 0x3) * 8 + 8; };
  int NEXT()   const { return (attr0[1] & 0x7f); };
  int PRI()    const { return ((attr1[0] >> 7) & 0x1); };
  int PAL()    const { return ((attr1[0] >> 5) & 0x3) * 16; };
  int FLAGS()  const { return (attr1[0] & (HFLIP|VFLIP)); };
  int TILE()   const { return (attr1[1] + ((attr1[0] & 0x7) << 8)) * 0x20; };

  int SXPOS(int x) const {
    if (FLAGS() & HFLIP) {
      return XPOS() + WIDTH() - x - 1;
    };
    return XPOS() + x;
  };
  int SYPOS(int y) const {
    if (FLAGS() & VFLIP) {
      return YPOS() + HEIGHT() - y - 1;
    };
    return y - YPOS();
  };
  int getpix(int y, int *line, vdp_t *v);
};

static void request_irq(int lvl)
{
  irq_sts |= (1L << lvl);
}

static void check_irq()
{
  for (int i = 0; i < 10; i++) {
    if (irq_sts & (1L << i)) {
      irq_sts &= ~(1L << i);
      cpu_irq(i);
      return;
    }
  }
}

/* Video Procesor */
struct vdp_t : public crtc_t {
  Screen *scr;

  int  xres;
  int  yres;

  /* 64k VRAM, 128 bytes of cram */
  uint8_t  vram[64 * 1024] = {};
  uint16_t cram[0x40] = {};
  uint16_t vsram[0x80] = {};

  /* vdp status default bits */
  uint16_t vdp_status = 0x3400;
  uint8_t  vdp_reg[0x20] = {};

  bool     vdp_cstate = false;
  uint8_t  vdp_ctrl;
  uint16_t vdp_addr;

  /* Scrolling info */
  uint16_t hscroll_mask;
  uint16_t hscroll_shift;
  uint16_t vscroll_mask;
  uint16_t vscroll_shift;

  /* Window info */
  int wleft, ww;
  int wtop,  wh;

  /* Nametable width&height */
  int ntw;
  int nth;
  int hcounter;

  /* number of sprites per line/frame */
  int max_sprites_per_line;
  int max_sprites_per_frame;
  int frame_sprites;
  
  int      dma_fill;
  uint16_t dma_len;
  uint32_t dma_src;
  int io_reg[16] = { 0xa0, 0x7f, 0x7f, 0x7f, 0x0, 0x0, 0x0, 0xff, 0x0, 0x0, 0xff, 0x0, 0x0, 0xff, 0x0, 0x0 };
  int btn_state[3] = { 0, 0, 0 };
  int pad_state[3] = { 0, 0, 0 };

  vdp_t();
  void getscroll(int y, int *h, int *v);
  void write_data(uint32_t nv);
  bool tick();
  void drawframe();

  void renderline(int y);
  void renderbg(int *line, int y, int pri);
  void renderspr(int *line, int y, int pri, int, sprite_t **);
  int  getsprites(int y, sprite_t **);
  
  void setvblank(bool state);
  void sethblank(bool state);
  void showregs();

  uint32_t io(uint32_t addr, int size, uint32_t data = 0, bool wr = false);
  void setreg(int r, uint8_t v);

  sprite_t *sprite_base(int n) {
    return &((sprite_t *)&vram[vdp_reg[5] << SPRITE_ADDR_SHIFT])[n];
  };
  
  void setkeystate(int vk, int mask);

  /* Read/Write pad registers */
  int pad_read(int pad) {
    int val = pad_state[pad] & 0x40;
    val |= 0x3f;
    if (val & 0x40) {
      val = ~(btn_state[pad] & 0x3f);
    }
    else {
      val = ~(0xc | (btn_state[pad] & 3) | ((btn_state[pad] >> 2) & 0x30));
    }
    return val;
  };
  void pad_write(int pad, int value) {
    int mask = io_reg[pad + 4];
    pad_state[pad] &= ~mask;
    pad_state[pad] |= value & mask;
  };
};

/* Get RGB color from color ram */
static int getclr(int i, const uint16_t *cram) {
  // ----.bbb-.ggg-.rrr-
  int clr = cram[i & 0x3f];
  return MKRGB((clr << 4) & 0xe0, clr & 0xe0, (clr >> 4) & 0xe0);
};

/* Get 16-color palette, color 0 is transparent (-1) */
static int getpal(int *clr, int pb, const uint16_t *cram) {
  clr[0] = -1;
  for (int i = 1; i < 16; i++) {
    clr[i] = getclr(pb + i, cram);
  }
  return 0;
}

vdp_t::vdp_t()
{
  /* Initialize CRTC:
   * 256x224 -> 292x260
   */
  crtc_t::init(256, 292, 224, 260);

  scr = new Screen(320, 240, 120, 50);
  scr->xs = 2;
  scr->ys = 2;
}

#define TH_START 0x2000
#define TH_A     0x1000
#define TH_C     0x0020
#define TH_B     0x0010
#define TH_RIGHT 0x0008
#define TH_LEFT  0x0004
#define TH_DOWN  0x0202
#define TH_UP    0x0101

#define THPAD_UP    (1L << 0)
#define THPAD_DOWN  (1L << 1)
#define THPAD_LEFT  (1L << 2)
#define THPAD_RIGHT (1L << 3)
#define THPAD_B     (1L << 4)
#define THPAD_C     (1L << 5)
#define THPAD_A     (1L << 6)
#define THPAD_S     (1L << 7)

void vdp_t::setkeystate(int vk, int mask)
{
  if (scr->key(vk, true)) {
    printf("key %c on : %.4x\n", vk, mask);
    btn_state[0] |= mask;
  }
  else {
    btn_state[0] &= ~mask;
  }}


void vdp_t::showregs() {
  for (int i = 0; i < 24; i++) {
    printf("%2d: %.2x\n", i, vdp_reg[i]);
  }
  printf(" DISP:%d IE0:%d DMA:%d V:%d\n",
	 !!(vdp_reg[1] & VDPR1_DISP), !!(vdp_reg[1] & VDPR1_IE0),
	 !!(vdp_reg[1] & VDPR1_M1), vdp_reg[1] & VDPR1_M2 ? 30 : 28);
  printf(" IE1:%d M3:%d\n", !!(vdp_reg[0] & VDPR0_IE1), !!(vdp_reg[0] & VDPR0_M3));
  printf(" IE2:%d VSC:%d HSC:%d LSC:%d\n",
	 !!(vdp_reg[11] & VDPR11_IE2),
	 !!(vdp_reg[11] & VDPR11_VSCR),
	 !!(vdp_reg[11] & VDPR11_HSCR),
	 !!(vdp_reg[11] & VDPR11_LSCR));
  printf(" h:%d v:%d ste:%d lsm1:%d lsm0:%d\n",
	 vdp_reg[12] & VDPR12_RS0 ? 40 : 32,
	 vdp_reg[12] & VDPR12_RS1 ? 40 : 32,
	 !!(vdp_reg[12] & VDPR12_STE),
	 !!(vdp_reg[12] & VDPR12_LSM1),
	 !!(vdp_reg[12] & VDPR12_LSM0));
  printf(" VRAM_SCROLLB: %.4x\n", SCROLLB_ADDR);
  hexdump(&vram[SCROLLB_ADDR], 32);
  printf(" VRAM_SCROLLA: %.4x\n", SCROLLA_ADDR);
  hexdump(&vram[SCROLLA_ADDR], 32);
  printf(" VRAM_WINDOW:  %.4x\n", WINDOW_ADDR);
  hexdump(&vram[WINDOW_ADDR], 32);
  printf(" VRAM_SPRITE : %.4x\n", vdp_reg[5] << 9);
  hexdump(&vram[vdp_reg[5] << 9], 32);
  printf(" VRAM_HSCROLL: %.4x\n", (vdp_reg[13] << 10));
  hexdump(&vram[vdp_reg[13] << 10], 32);
  printf(" BGCOLOR: %.2x\n", vdp_reg[7]);
  printf(" HCOUNTER: %.2x\n", vdp_reg[10]);
  printf(" auto-incr: %.2x\n", vdp_reg[15]);
  printf(" hs:%d vs:%d\n",
	 vh_map[vdp_reg[16] & 3],
	 vh_map[(vdp_reg[16] >> 4) & 3]);
  hexdump(cram, sizeof(cram));
}

/* Get sroll parameters for a given scanline
 * 0 : scrolla bbbb
 * 1 : scrollb ffff
 * 2 : window
 */
void vdp_t::getscroll(int y, int *hs, int *vs)
{
  uint16_t *htab = (uint16_t *)&vram[HSCROLL_ADDR];
  uint16_t addr;

  /* horizontal mask/shift
   * full screen: mask=0000/0 FFFF BBBB
   * 1-cell:      mask=fff8/4 FFFF BBBB 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000|FFFF BBBB ...
   * scanline:    mask=FFFF/1 BBBB|FFFF BBBB ....
   */
  addr = ((y & hscroll_mask) << hscroll_shift);
  hs[0] = -(int16_t)get16be(&htab[addr + 1]);
  hs[1] = -(int16_t)get16be(&htab[addr + 0]);
  hs[2] = 20;

  /* vertical mask/shift
   * full screen: mask=0000/1  FFFF BBBB
   * 2-cell:      mask=FFF0/4  FFFF BBBB|FFFF BBBB ....
   */
  addr = ((y & vscroll_mask) >> vscroll_shift);
  vs[0] = (int16_t)vsram[addr + 1];
  vs[1] = (int16_t)vsram[addr + 0];
  vs[2] = 0;
}

/* 16x16: tile ordering
 * 01 03
 * 02 04
 *
 * 32x32: tile ordering
 * 01 05 09 13
 * 02 06 10 14
 * 03 07 11 15
 * 04 08 12 16
 */

/* Set line of pixels to single color */
static void rline(int *pxl, int w, int clr) {
  while (w--) {
    *pxl++ = clr;
  }
}

/* decode 4 bits per pixel */
static void getbmp2(int *pxl, const uint8_t *mem, int *clr, int len = 8)
{
  for (int i = 0; i < len; i+=2) {
    int p = *mem++;
    *pxl++ = clr[(p >> 4) & 0xF];
    *pxl++ = clr[(p >> 0) & 0xF];
  }
}

/* Decode 8 sprite pixels at a time */
int sprite_t::getpix(int y, int *line, vdp_t *v)
{
  int sx, sy, pxl[64], clr[16];
  uint16_t addr;

  /* Get Sprite colors 0..15 */
  getpal(clr, PAL(), v->cram);
  
  /* Get sprite tile y address, with flip */
  sy = SYPOS(y);

  /* 32-bits (8 pixels) per line */
  addr = TILE() + sy * 4;

  /* Get sprite pixels */
  for (int x = 0; x < WIDTH(); x++) {
    if ((x & 7) == 0) {
      /* Get every 8 pixels */
      getbmp2(pxl, (uint8_t *)&v->vram[addr], clr);
      addr += HEIGHT() * 4;
    }
    /* Get sprite x address */
    sx = SXPOS(x);
    if (inrange(sx, 0, v->xres) && pxl[x & 7] != -1) {
      if (line[sx] != -1) {
	/* sprite collision */
	v->vdp_status |= (1L << 7);
      }
      line[sx] = pxl[x & 7];
    }
  }
  return 0;
}

/* Get list of sprites on this line */
int vdp_t::getsprites(int y, sprite_t **spr)
{
  int n = 0, nspr = 0;
  sprite_t *s;

  /* Get sprite queue */
  do {
    s = sprite_base(n);

    /* Check if sprite is in scanline */
    if (!inrange(y, s->YPOS(), s->HEIGHT())) {
      continue;
    }
    spr[nspr++] = s;
    if (nspr >= max_sprites_per_line) {
      /* Sprite Overflow */
      vdp_status |= VDP_STATUS_SO;
      break;
    }
    /* Get Next sprite offset */
  } while ((n = s->NEXT()) != 0);
  return nspr;
}

void vdp_t::renderspr(int *line, int y, int pri, int nspr, sprite_t **spr)
{
  /* Draw each sprite into line */
  while (nspr--) {
    if (spr[nspr]->PRI() == pri) {
      spr[nspr]->getpix(y, line, this);
    }
  }
}

/* ScrollB, ScrollA, Window pri 0
 * Sprites pri 0
 * ScrollB, ScrollA, Windiw pri 1
 * Sprites pri 1
 */
void vdp_t::renderbg(int *line, int y, int pri)
{
  uint16_t ntaddr, attr, tile, flags;
  int hscroll[3], vscroll[3], tx, ty, pxl[8], clr[16], txx, tyy;
  uint16_t addr[3];

  getscroll(y, hscroll, vscroll);

  /* Background priority base address */
  addr[0] = SCROLLB_ADDR;
  addr[1] = SCROLLA_ADDR;
  addr[2] = WINDOW_ADDR;
  for (int scr = 0; scr <= 2; scr++) {
    /* debug hide scroll a/b */
    if (scr == 0 && !(ntflag & 1))
      continue;
    if (scr == 1 && !(ntflag & 2))
      continue;
    for (int x = 0; x < xres; x++) {
      /* check if window visible */
      if (scr == 2 && !inrange(y, wtop, wh))
	continue;
      /* Get tile address */
      tx = ((x + hscroll[scr] + ntw) % ntw);
      ty = ((y + vscroll[scr] + nth) % nth);
      ntaddr = NT_ADDR(tx, ty, ntw) + addr[scr];

      /* Get tile attribute */
      attr = NT_ATTR(&vram[ntaddr]);
      if (pri != NT_PRI(attr))
	continue;
      tile = NT_TILE(attr); 
      flags = NT_FLAGS(attr);

      /* Get tile palette */
      getpal(clr, NT_PAL(attr), cram);

      /* Get tile data, 32 bits for 8 pixels */
      txx = (tx % 8) ^ ((flags & HFLIP) ? 7 : 0);
      tyy = (ty % 8) ^ ((flags & VFLIP) ? 7 : 0);
      getbmp2(pxl, (uint8_t *)&vram[tile + (tyy * 4)], clr);

      // -1 is transparent color
      if (pxl[txx] != -1) {
	line[x] = pxl[txx];
      }
    }
  }
}

void vdp_t::renderline(int y)
{
  sprite_t *spr[max_sprites_per_line];
  int xd = (scr->width - xres) / 2;
  int yd = (scr->height - yres) / 2;
  int nspr, clr, line[400];

  /* display disabled? */
  if ((vdp_reg[1] & VDPR1_DISP) == 0) {
    return;
  }

  /* Get sprites on this line*/
  nspr = getsprites(y, spr);

  /* set line to background color */
  clr = getclr(vdp_reg[7], cram);
  rline(line, 400, clr);

  /* Priority: BG0, SPR0, BG1, SPR1 */
  renderbg(line, y, 0x0);
  renderspr(line, y, 0x0, nspr, spr);
  renderbg(line, y, 0x1);
  renderspr(line, y, 0x1, nspr, spr);
  drawline(scr, line, 320, xd, yd+y, 0);

  /* Draw line on hcounter */
  if (y == vdp_reg[10]) {
    scr->scrline(0, yd+vdp_reg[10], 320, MKRGB(255,255,255));
  }
}

void vdp_t::drawframe()
{
  static time_t fpstime = time(NULL);
  time_t now;
  float fps;

  /* Get keypress state */
  setkeystate('i', THPAD_UP);
  setkeystate('k', THPAD_DOWN);
  setkeystate('j', THPAD_LEFT);
  setkeystate('l', THPAD_RIGHT);
  setkeystate('n', THPAD_S);
  setkeystate('m', THPAD_A);
  setkeystate(',', THPAD_B);
  setkeystate('.', THPAD_C);

  printf("--- frame: %d IE:%d-%d-%d ctr:%d\n",
	 frame,
	 !!(vdp_reg[1] & VDPR1_IE0),
	 !!(vdp_reg[0] & VDPR0_IE1),
	 !!(vdp_reg[11] & VDPR11_IE2),
	 vdp_reg[10]);

  now = time(NULL);
  fps = (float)frame / (now - fpstime);
  scr->scrtext(0, vBlank + 30, MKRGB(255,255,0),
	       "frame:%d fps:%.2f PC:%.8x %dx%d",
	       frame, fps, SPC,
	       vdp_reg[12] & 1 ? 320 : 256,
	       vdp_reg[1] & 8 ? 240 : 224);
  scr->scrtext(0, vBlank + 40, MKRGB(255,255,0),
	       "%dx%d %d %d", ntw/8, nth/8,
	       max_sprites_per_frame, max_sprites_per_line);
  scr->draw();
  scr->clear();

  if (scr->key('a', true))
    ntflag = (ntflag+1) & 7;

  frame++;
}

void vdp_t::setreg(int r, uint8_t v)
{
  if (vdp_reg[4] & 0x4 || r <= 10) {
    printf("setreg: %.2x = %.2x\n", r, v);
    vdp_reg[r] = v;
    switch (r) {
    case 1:
      yres = (v & 8) ? 240 : 224;
      break;
    case 10:
      hcounter = v;
      break;
    case 11:
      // Setup scrolling
      if ((v & 3) == 0) {
	// horiz: screen
	printf("hscroll:screen ");
	hscroll_mask = 0x0000;
	hscroll_shift = 0;
      }
      else if ((v & 3) == 1) {
	// horiz: 8x8 cell
	printf("hscroll:cell   ");
	hscroll_mask = 0xfff8;
	hscroll_shift = 4;
      }
      else if ((v & 3) == 3) {
	// horiz: scanline
	printf("hscroll:line   ");
	hscroll_mask = 0xffff;
	hscroll_shift = 1;
      }
      if ((v & 4) == 0) {
	printf("vscroll:screen\n");
	vscroll_mask = 0x0000;
	vscroll_shift = 0;
      }
      else {
	printf("vscroll:2cell\n");
	vscroll_mask = 0xfff0;
	vscroll_shift = 3;
      }
      break;
    case 12:
      /* Set screen resolution */
      if (v & 1) {
	xres = 320;
	max_sprites_per_line = 20;
	max_sprites_per_frame = 80;
      }
      else {
	xres = 256;
	max_sprites_per_line = 16;
	max_sprites_per_frame = 64;
      }
      break;
    case 16:
      /* Get nametable width/height */
      ntw = 8 * vh_map[v & 3];
      nth = 8 * vh_map[(v >> 4) & 3];
      break;
    case 0x11:
      ww = (v & 0x1F) * 8;
      wleft = (v & 0x80) ? xres - ww : 0;
      break;
    case 0x12:
      wh = (v & 0x1F) * 8;
      wtop = (v & 0x80) ? yres - wh : 0;
      break;
    }
  }
}

// scanline = (2560+120), hblank.on, (64+313+259), hblank.off, 104, renderline
// vblank.on, 588, vdp_status|=0x80, 200, irq.6, execute(3420-788)
// execute(3420*lines_per_frame)

// 292 x 262 = 76504 video ticks, 896840 cpu cycles
// roughly 11 cpu cycles per video tick
//
// 3420 cycles per scanline
// 896040 ntsc cycles per frame (262)
// 1067040 pal cycles per frame (312)
bool vdp_t::tick() {
  bool rc = crtc_t::tick();

  if ((vdp_reg[0] & VDPR0_IE1) && (vdp_reg[10] == vPos)) {
    // hcounter irq
    request_irq(4);
  }
  // crtc end-of-frame
  if (rc) {
    drawframe();
  }
  return rc;
}

void vdp_t::setvblank(bool state) {
  if (state && (vdp_reg[1] & VDPR1_IE0) != 0) {
    // vblank irq
    printf("VBLANK IRQ\n");
    request_irq(6);
  }
  setclr(vdp_status, VDP_STATUS_VB, state);
}

void vdp_t::sethblank(bool state) {
  if (state == true) {
    if (inrange(vPos, 0, vBlank)) {
      renderline(vPos);
    }
  }
  setclr(vdp_status, VDP_STATUS_HB, state);
}

struct genesis : public bus_t {
  size_t   romsz;
  uint8_t *rom;
  uint8_t *ram;

  vdp_t vdp;
  void init(uint8_t *buf, int size);
  void run(int n);

  bool z80_reset;
  cpu_z80 z80;
};

static genesis sys;

/* Do IO reg */
static int z80_io1(void *arg, uint32_t addr, int mode, iodata_t& io)
{
  vdp_t *v = (vdp_t *)arg;
  int mask;
  
  mode &= 0xff;
  if (mode == 'r') {
    addr = (addr & 0x1f) >> 1;
    if (addr >= 1 && addr < 4) {
      mask = 0x80 | v->io_reg[addr + 3];
      io =   v->io_reg[addr] & mask;
      io |=  v->pad_read(addr - 1) & ~mask;
    }
    else {
      io = v->io_reg[addr];
    }
  }
  else {
    addr = (addr & 0x1f) >> 1;
    if (addr >= 1 && addr < 0x4) {
      v->io_reg[addr] = io;
      v->pad_write(addr - 1, io);
    }
    else if (addr >= 4 && addr < 0x7) {
      if (v->io_reg[addr] != io) {
	v->io_reg[addr] = io;
	v->pad_write(addr - 4, v->io_reg[addr - 3]);
      }
    }
  }
  return 0;
}

/* Set Z80 */
static int z80_io2(void *arg, uint32_t addr, int mode, iodata_t& io)
{
  genesis *g = (genesis *)arg;

  mode &= 0xff;
  if (addr == 0x00a11100 && mode == 'w') {
    g->z80.bus_ack = (io != 0);
  }
  if (addr == 0x00a11100 && mode == 'r') {
    io = !(g->z80.bus_ack && g->z80_reset);
  }
  if (addr == 0x00a11200 && mode == 'w') {
    g->z80_reset = (io != 0);
    if (g->z80_reset) {
      printf("z80 reset: %.2x %.2x\n", g->z80_reset, g->z80.bus_ack);
    }
  }
  return 0;
}

/* I/O to VDP device */
static int vdpio(void *arg, uint32_t addr, int mode, iodata_t& io)
{
  vdp_t *v = (vdp_t *)arg;

  switch (mode) {
  case _WR8:
  case _WR16:
    v->io(addr, 2, io, true);
    break;
  case _WR32:
    v->io(addr+0, 2, io>>16, true);
    v->io(addr+2, 2, io, true);
    break;
  case _RD16:
    io = v->io(addr, 2, 0, false);
    break;
  default:
    printf(" vdp size: %.8x %x\n", addr, mode & 0xff);
    break;
  }
  return 0;
}

/*
 * register write: (val & 0xE000) == 0x8000
 *  00c00004: w 100r.rrrr.dddd.dddd
 * vram write:
 *  00c00004: w 01aa.aaaa.aaaa.aaaa
 *  00c00004: w 0000.0000.c000.00aa
 *  00c00000: w dddd.dddd.dddd.dddd
 * vram read:
 *  00c00004: w 00aa.aaaa.aaaa.aaa-
 *  00c00004: w 0000.0000.0000.00aa
 *  00c00000: r dddd.dddd.dddd.dddd
 * cram write: (byte write, hi=00c00000, lo=00c00001)
 *  00c00004: w 1100.0000.0aaa.aaaa
 *  00c00004: w 0000.0000.0000.0000
 *  00c00000: w 0000.bbb0.ggg0.rrr0
 * cram read:
 *  00c00004: w 0000.0000.0aaa.aaaa
 *  00c00004: w 0000.0000.0010.0000
 *  00c00000: r ----.bbb-.ggg-.rrr-
 * vsram write:
 *  00c00004: w 0100.0000.0aaa.aaaa
 *  00c00004: w 0000.0000.0001.0000
 *  00c00000: w ----.-ddd,dddd,dddd
 * vsram read:
 *  00c00004: w 0000.0000.0aaa.aaaa
 *  00c00004: w 0000.0001.0000.0000
 *  00c00000:
 *
 * dma:
 *  00c00004: w ccdd.dddd.aaaa.aaaa
 *  00c00004: w 0000.0000.100c.00dd
 */
void vdp_t::write_data(uint32_t nv)
{
  if ((vdp_ctrl & 0xe) == 0) {
    bememio(vram, vdp_addr & 0xFFFF, _WR16, nv);
  }
  else if ((vdp_ctrl & 0xe) == 2) {
    // format is 0bgr
    cram[(vdp_addr >> 1) & 0x3F] = nv;
  }
  else if ((vdp_ctrl & 0xe) == 4) {
    vsram[(vdp_addr >> 1) & 0x7F] = nv;
  }
  vdp_addr += vdp_reg[15];
}

/* +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
 * | 1 | 0 | 0 |rs4|rs3|rs2|rs1|rs0|d7 |d6 |d5 |d4 |d3 |d2 |d1 |d0 | Write data reg
 * +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
 *
 * +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
 * |CD1|CD0|A13|A12|A11|A10|A09|A08|A07|A06|A05|A04|A03|A02|A01|A00| 1st write
 * +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
 * | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 |CD5|CD4|CD3|CD2| 0 | 0 |A15|A14| 2nd write
 * +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
 *
 * +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+ vram.read
 * | 0 | 0 |A13|A12|A11|A10|A09|A08|A07|A06|A05|A04|A03|A02|A01|A00| 1st write
 * +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
 * | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 |A15|A14| 2nd write
 * +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
 */ 
uint32_t vdp_t::io(uint32_t addr, int size, uint32_t nv, bool iswr) {
#if 0
  flogger(0,"vdpio: %c:%d:%.8x %.8x [%d]\n",
	  iswr ? 'w' : 'r', size, addr, nv,
	  vdp_cstate);
#endif
  addr &= 0x1F;
  if (!iswr) {
    /* Return status register */
    if (addr >= 0x4 && addr < 0x8) {
      return vdp_status;
    }
    return hPos;
  }
  else {
    if (addr < 0x4) {
      write_data(nv);
      if (dma_fill) {
	dma_fill = 0;
	dma_len = vdp_reg[19] + (vdp_reg[20] << 8);
	while (dma_len--) {
	  vram[vdp_addr] = nv >> 8;
	  vdp_addr += vdp_reg[15];
	}
      }
    }
    else if (addr < 0x8) {
      if ((nv & 0xE000) == 0x8000) {
	// Register write 100rrrrr.dddddddd
	setreg((nv >> 8) & 0x1f, nv & 0xff);
      }
      else if (vdp_cstate == false) {
	// First write YYXX.XXXX.XXXX.XXXX
	// addr: __XX.XXXX.XXXX.XXXX
	// ctrl: ____.____.____.__YY
	vdp_cstate = true;
	rmw(vdp_addr, nv, 0x3FFF);
	rmw(vdp_ctrl, nv >> 14, 0x3);
      }
      else {
	// Second write ____.___C.CCCC.__XX
	// addr: XX__.____.____.____
	// ctrl: ____.____._CCC.CC__
	vdp_cstate = false;
	rmw(vdp_addr, nv << 14, 0xC000);
	rmw(vdp_ctrl, nv >> 2,  0x7C);

	// dma enabled
	if ((vdp_ctrl & 0x20) && (vdp_reg[1] & VDPR1_M1)) {
	  if ((vdp_reg[23] >> 6) == 2 && (vdp_ctrl & 7) == 1) {
	    dma_fill = 1;
	    printf("dma fill\n");
	  }
	  else if ((vdp_reg[23] >> 6) == 3) {
	    printf("dma copy\n");
	  }
	  else {
	    dma_len = (vdp_reg[19] + (vdp_reg[20] << 8));
	    dma_src = ((vdp_reg[21]) + (vdp_reg[22] << 8) + (vdp_reg[23] << 16));
	    dma_src <<= 1;

	    while (dma_len--) {
	      uint16_t src = cpu_read16(dma_src);
	      write_data(src);
	      dma_src+=2;
	    }
	  }
	}
      }
    }
  }
  return 0;
}

void genesis::init(uint8_t *buf, int size) {
  uint32_t rommask = 1;

  rom = buf;
  romsz = size;
  ram = zmalloc(0x200000);
  z80.ram = zmalloc(64 * 1024);
  
  // get size of rom in power-of-2
  while (rommask < romsz) {
    rommask <<= 1;
  }

  // Register i/o handlers
  bus_t::init(0xFFFFFF);
  register_handler(0x00000000, 0x003fffff, rommask - 1,bememio, rom, _RD, "ROM");
  register_handler(0x00e00000, 0x00ffffff, 0x001fffff, bememio, ram, _RW, "RAM");
  register_handler(0x00a00000, 0x00a03fff, 0x00007fff, bememio, z80.ram, _RW, "Z80:RAM");
  register_handler(0x00a10000, 0x00a1001f, 0x00ff001f, z80_io1, &vdp, _RW, "Z80:IO");
  register_handler(0x00a11100, 0x00a112ff, 0x00ffffff, z80_io2, this, _RW, "Z80:CTRL");
  register_handler(0x00c00000, 0x00dfffff, 0x00ffffff, vdpio, &vdp, _RW, "VDP");

  // dump interrupt vectors
  dumpvec(0x0);

  // set rgb mode
  vdp.scr->clrmode = 1;
  vdp.scr->init(1);
}

/* Display output for:
 * oooo.xxx.XXX.YYY.yyy
 * oooo.xxx.oss.YYY.yyy : F1C0
*/
void genesis::run(int nn)
{
  uint16_t op;
  bool dodump = (nn > 2);
  
  gentbl();
  cpu_reset(0);
  setbuf(stdout, NULL);
  if (dodump)
    dumpcfg(PC, 0,romsz);
  for(;;) {
    SPC = PC;
    check_irq();
#if 1
    if (visited[PC] == 0)
      visited[PC] = 1;
#endif
    op = cpu_fetch(Word);
    decode_68k(op);
    for (int i = 0; i < 5; i++)
      vdp.tick();
  }
}

/*==================================================*
 * CPU read/write handlers
 *==================================================*/
uint8_t cpu_read8(const uint32_t addr, int mode) {
  iodata_t io;

  sys.read(addr, io);
  return io;
}

uint16_t cpu_read16(const uint32_t addr, int mode) {
  iodata_t io;

  sys.read(addr, io, _SZ16);
  return io;
}

uint32_t cpu_read32(const uint32_t addr, int mode) {
  iodata_t io;

  sys.read(addr, io, _SZ32);
  return io;
}

void cpu_write8(const uint32_t addr, const uint8_t val, int mode) {
  sys.write(addr, val);
}

void cpu_write16(const uint32_t addr, const uint16_t val, int mode) {
  sys.write(addr, val, _SZ16);
}

void cpu_write32(const uint32_t addr, const uint32_t val, int mode) {
  sys.write(addr, val, _SZ32);
}

void m68k_emul1111(uint16_t op) {
  m68k_trap(true, VECTOR_1111);
}

void m68k_emul1010(uint16_t op) {
  m68k_trap(true, VECTOR_1010);
}

void cpu_shutdown()
{
  FILE *fp;

  fp = fopen("dumpcfg-gen.txt", "w+");
  for (auto x : visited) {
    int xy = x.first;
    fprintf(fp, "%.8x: %.4x %s\n", xy, x.second, fnname(xy));
  }
  fclose(fp);
}

void cpu_reset(uint32_t addr)
{
  addr = cpu_read32(0x04);

  PC = addr & 0xffffff;
  printf("reset addr: %8x\n", addr);
  SP = cpu_read32(0x00);
  Sf = 1;
}

void flogger(int n, const char *fmt, ...)
{
  va_list ap;
  va_start(ap, fmt);
  printf("%.8x: ", SPC);
  vprintf(fmt, ap);
}

/* Get extra symbols for dumpcfg */
void getextra(dstk& s, uint32_t base, uint32_t size)
{
  FILE *fp;
  char line[128];
  uint32_t b;

  fp = fopen("dumpcfg-gen.txt", "r");
  while (fgets(line, sizeof(line), fp) != NULL) {
    sscanf(line, "%x", &b);
    s.push(b, 1, dstk::PENDING);
  }
}

int main(int argc, char *argv[])
{
  uint8_t *rom;
  size_t size;

  trace = 0;
  rom = loadrom(argv[1], size);
  if (!rom) {
    printf("no rom\n");
    exit(1);
  }
  hexdump(rom, 256);
  sys.init(rom, size);
  sys.run(argc);
  return 0;
}
