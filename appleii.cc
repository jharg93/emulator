#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>
#include <unistd.h>
#include <string.h>
#include <stdarg.h>
#include "cpu.h"
#include "dstk.h"
#include "bus.h"
#include "util.h"
#include "gr.h"
#include "apple2_font.h"

const char *comment(int addr) {
  return "xxx";
}

// https://retrocomputing.stackexchange.com/questions/2534/what-are-the-screen-holes-in-apple-ii-graphics
// https://6502disassembly.com/a2-rom/AutoF8ROM.html

extern int trace, SPC;
int frame;

/* VIDEO TEXT MODE  400, 480, 500, 580 ... 7d0 for 24 lines
 * HIRES MODE       2000+400xrln, 2080 + 400xrln, ... 23d0 + 0x400 * rln for 192 lines
*/
static palclr apple_pal[] = {
  { 0,   0,   0 },
  { 221, 0,   51 },
  { 0,   0,   153 },
  { 221, 34,  221 },
  { 0,   119, 34 },
  { 85,  85,  85 },
  { 34,  34,  255 },
  { 102, 170, 255 },
  { 136, 85,  0 },
  { 255, 102, 0 },
  { 170, 170, 170 },
  { 255, 153, 136 },
  { 17,  221, 0 },
  { 255, 255, 0 },
  { 68,  255, 255 },
  { 255, 255, 255 },
};

#define CHIPREG(o) \
  o(0xc000, KEYBOARD,    _RD, "keyboard data") \
  o(0xc000, CLR80COL,    _WR, "Clear 80 cols") \
  o(0xc001, SET80COL,    _WR, "Set80 cols")     \
  o(0xc002, CLRAUXRD,    _WR, "if 80STORE off: read main mem $0200-BFFF") \
  o(0xc003, SETAUXRD,    _WR, "if 80STORE off: read aux mem $0200-BFFF") \
  o(0xc004, CLRAUXWR,    _WR, "if 80STORE off: write main mem $0200-BFFF") \
  o(0xc005, SETAUXWR,    _WR, "if 80STORE off: write aux mem $0200-BFFF") \
  o(0xc006, CLRCXROM,    _WR, "external slot rom") \
  o(0xc007, SETCXROM,    _WR, "external slot rom") \
  o(0xc008, CLRAUXZP,    _WR, "use main stack and zero page") \
  o(0xc009, SETAUXZP,    _WR, "use aux stack and zero page") \
  o(0xc00a, CLRC3ROM,    _WR, "use external slot c3 ROM") \
  o(0xc00b, SETC3ROM,    _WR, "use external slot c3 ROM") \
  o(0xc00c, CLR80VID,    _WR, "disable 80-column display mode") \
  o(0xc00d, SET80VID,    _WR, "enable 80-column display mode") \
  o(0xc00e, CLRALTCH,    _WR, "use primary char set") \
  o(0xc00f, SETALTCH,    _WR, "use alternate char set") \
  o(0xc010, STROBE,      _RW, "keyboard strobe") \
  o(0xc011, RDLCBNK2,    _RD, "bit 7: reading from LC bank 2 ($Dx)") \
  o(0xc012, RDLCRAM,     _RD, "bit 7: reading from LC RAM") \
  o(0xc013, RDRAMRD,     _RD, "bit 7: reading from aux/alt 48K") \
  o(0xc014, RDRAMWR,     _RD, "bit 7: writing to aux/alt 48K") \
  o(0xc015, RDCXROM,     _RD, "using external slot rom") \
  o(0xc016, RDAUXZP,     _RD, "using aux zp, stack and lc") \
  o(0xc017, RDC3ROM,     _RD, "using external slot c3 rom") \
  o(0xc018, RD80COL,     _RD, "bit 7: 80STORE is on") \
  o(0xc019, RDVBLBAR,    _RD, "not VBlank") \
  o(0xc01a, RDTEXT,      _RD, "read text mode C050") \
  o(0xc01b, RDMIXED,     _RD, "read mixed mode C052") \
  o(0xc01c, RDPAGE2,     _RD, "read page 2 C054") \
  o(0xc01d, RDHIRES,     _RD, "read hires C056") \
  o(0xc01e, RDALTCH,     _RD, "read altchar C00E") \
  o(0xc01f, RD80VID,     _RD, "read 80cols C00C") \
  o(0xc030, SPKR,        _RW, "toggle speaker") \
  o(0xc050, CLRTEXT,     _RW, "TXTCLR:enable text only") \
  o(0xc051, SETTEXT,     _RW, "TXTSET:enable text only") \
  o(0xc052, CLRMIXED,    _RW, "MIXCLR:enable mixed mode") \
  o(0xc053, SETMIXED,    _RW, "MIXSET:enable mixed mode") \
  o(0xc054, TXTPAGE1,    _RW, "LOWSCR:display page 1") \
  o(0xc055, TXTPAGE2,    _RW, "HISCR:display page 2") \
  o(0xc056, CLRHIRES,    _RW, "LORES:set lores mode") \
  o(0xc057, SETHIRES,    _RW, "HIRES:set hires mode") \
  o(0xc058, SETAN0,      _RW, "annunciator 0 off") \
  o(0xc05a, SETAN1,      _RW, "annunciator 1 off") \
  o(0xc05d, CLRAN2,      _RW, "annunciator 2 on") \
  o(0xc05f, CLRAN3,      _RW, "annunciator 3 on") \

enum {
#define o(addr, name, rw, dsc) name = (addr),
  CHIPREG(o)
#undef o
};
  
const char *rname(uint32_t off) {
#define o(addr, name, rw, str) if (off == addr) return str;
  CHIPREG(o)
#undef o
  return "none";
}

/* Memory map
 * 0000-BFFF RAM
 * 0400-07FF Text Screen
 * 2000-3FFF HiRes graphics page 1
 * 4000-5FFF HiRes graphics page 2
 *
 * C000-CFFF I/O
 * D000-F7FF Basic
 * F800-FFFF system monitor
 * D000-FFFF RAM or ROM
 *
 * C100 : slot 1
 * C200 : slot 2
 * C800-CFFF : 2k ROM for each card
 * E000 : BASIC
 *
 * Page 04-07 graphics
 * Page 08-0b graphics
 * Page 20-3F hi-res primary page
 * Page 40-5F hi-res secondary page
 * Page C0 : i/o
 * Page C1-C7 i/o shared rom
 * Page D0-D7 empty rom socket 1
 * Page D8-DF empty rom socket 2
 * Page E0-E7 Integer Basic
 * Page F8-FF Monitor
 *
 * C052 : all graphics
 * C053 : mixed graphics/text
 * C055 : Screen 2 (txt=0800-0BFF)
 */

/* Low-Res: 
 *   40x40 + 4 text, 16 clrs
 *   40x48, 16 clrs
 * 128 bytes = 3 rows of 40 bytes

 * 40 x 40, 4 rows of text
 * 40 x 48, no rows of text
 *
 * Hi-res 280x192 = effective 140x192
 *   Black, Green, Purple, White
 *   Black, Orange, Blue, White
 * 40 blocks of 7 pixels, single byte
 *  +---+---+---+---+---+---+---+---+
 *  | H |   |   |   |   |   |   |   |
 *  +---+---+---+---+---+---+---+---+
 */
struct appleii : public bus_t {
  uint8_t *rom;
  uint8_t *ram;
  size_t   romsz;
  void init(const char *file, int);
  void run();
  crtc_t crtc;

  Screen *scr;
  
  static int io(void *arg, uint32_t addr, int mode, iodata_t&io);

  void drawtextline(uint8_t *m,int r);
  void drawhiresline(uint8_t *m, int r);
  void drawtext(uint8_t *m);
  void drawhires(uint8_t *m);
  
  int flash, keycode = 0;
  int mode = 0;
  int page = 0;

  enum {
    TXT,
    LORES,
    HIRES,

    MIXED = 0x10,
  };
};

appleii sys;

int appleii::io(void *arg, uint32_t addr, int mode, iodata_t&io)
{
  appleii *a = (appleii *)arg;

  if (addr == 0xc000)
    io = a->keycode;
  else if (addr == 0xc010)
    a->keycode = 0;
  else if (addr == 0xc051)
    a->mode = TXT;
  else if (addr == 0xc056)
    a->mode = LORES;
  else if (addr == 0xc057)
    a->mode = HIRES;
  else if (addr == 0xc054)
    a->page = 0;
  else if (addr == 0xc055)
    a->page = 1;
  else if (addr == 0xc052)
    a->mode &= ~MIXED;
  else if (addr == 0xc053)
    a->mode |= MIXED;
  if (addr != 0xc000)
    printf("%.4x: HAZ IO[%s]: %x.%c.%x\n", SPC, rname(addr), addr, mode, io);
  return 0;
}

void appleii::init(const char *file, int romoff) {
  size_t fsz;
  
  bus_t::init(0xFFFF);
  ram = new uint8_t[64 * 1024]{0};
  rom = loadrom(file, romsz);

  memset(ram, 0xff, 64 * 1024);
  register_handler(0x0000, 0xBFFF, 0xFFFF, memio, ram,  _RW, "RAM");

  register_handler(0xc000, 0xc0ff, 0xffff, appleii::io, this, _RW, "IO");
  register_handler(0xd000, 0xdfff, 0x0fff, memio, &rom[romoff+0x0000], _RD, "RO<:d000");
  register_handler(0xe000, 0xefff, 0x0fff, memio, &rom[romoff+0x1000], _RD, "RO<:e000");
  register_handler(0xf000, 0xffff, 0x0fff, memio, &rom[romoff+0x2000], _RD, "RO<:f000");

  hexdump(&rom[romoff], 0x2000);
  
  crtc.init(280, 384-280, 192, 262-192);
  scr = new Screen(280, 192, 120, 50, 16, apple_pal);
  scr->init();
};

uint8_t visited[64*1024];

/* 40x25
 * 00-3F : inverse
 * 40-7F : flash
 * 80-BF : normal
 * C0-FF : normal
 */
bool fontgb(uint8_t bits, int idx) { return ((bits << idx) & 0x80) != 0; }


/* +---+---+---+---+---+---+---+---+ +---+---+---+---+---+---+---+---+
 * |   |   |   |   |   |   |   |   | |   |   |   |   |   |   |   |   |
 * +---+---+---+---+---+---+---+---+ +---+---+---+---+---+---+---+---+
 * 00 = black/black
 * 01 = green/orange
 * 10 = purple/blue
 * 11 = white/white
*/
#define FLASH_FREQ 10
const uint16_t txt_off[] = {
  0x0000, 0x0080, 0x0100, 0x0180, 0x0200, 0x0280, 0x0300, 0x0380,
  0x0028, 0x00a8, 0x0128, 0x01a8, 0x0228, 0x02a8, 0x0328, 0x03a8,
  0x0050, 0x00d0, 0x0150, 0x01d0, 0x0250, 0x02d0, 0x0350, 0x03d0,
};

void appleii::drawtextline(uint8_t *vidram, int row)
{
  uint8_t ch, bg, fg;
  static int bgc;

  for (int i = 0; i < 40; i++) {
    ch = *vidram++;
    if ((mode == (LORES|MIXED) && row < 20) || (mode == LORES)) {
      /* Draw low-res 40 or 48-line mode */
      scr->scrrect(i * 7, row * 8 + 0, 7, 4, ch & 0xF);
      scr->scrrect(i * 7, row * 8 + 4, 7, 4, ch >> 4);
    } else {
      bg = 0x1;
      fg = 0x1;
      if (ch >= 0x80) {
	bg = 0x1;
	fg = 0xf;
      }
      else if (ch >= 0x40) {
	bg = (flash < FLASH_FREQ) ? 0x1 : 0xf;
	fg = (flash < FLASH_FREQ) ? 0xf : 0x1;
      }
      ch &= 0x7f;
      scr->drawglyph(&gfont[ch * 8], 8, i * 7, row * 8, fg, bg, NULL); //fontgb);
    }
  }
}

/* Draw text and lowres modes */
void appleii::drawtext(uint8_t *vidram)
{
  for (int row = 0; row < 24; row++) {
    drawtextline(vidram+txt_off[row], row);
  }
}

/* 2000  line 0*8
 *  2400 line 1
 *  2800 line 2
 *  2c00 line 3
 *  3000 line 4
 *  3400 line 5
 *  3800 line 6
 *  3c00 line 7
 * 2080  line 8
 * ....
 * -------------------
 * 2028  line 8*8 = 64
 *  2428 line 65
 *  2828 line 66
 *  ...
 * 20a8 line 9*8 = 65
 *  24a8 line 66
 *  ... 
 * -------------------
 * 2050  line 8*16 = 128
 *  2450 line 129
 */

/* A|B|CD|EF|GH.I|JK|LM|NO|P  [hg,fe,dc,pb,on,ml,kj]
 * 0x0x00x00x00.0x00x00x00x0 = bk,bk,bk,bk,bk,bk,bk
 * 0x0x00x00x11.0x00x00x00x0 = wt,bk,bk,bk,bk,bk,bk
 * 0x0x00x11x11.0x00x00x00x0 = wt,wt,bk,bk,bk,bk,bk
 * 0x1x11x11x11.0x00x00x00x1 = wt,wt,wt,wt,bk,bk,bk
 * 0x1x11x11x11.0x11x11x11x1 = wt,wt,wt,wt,wt,wt,wt
 * 0x1x01x01x01.0x01x01x01x0 = vt,vt,vt,vt,vt,vt,vt
 * 0x0x10x10x10.0x10x10x10x1 = gn,gn,gn,gn,gn,gn,gn
 * 0x1x10x10x10.0x01x01x01x1 = gn,gn,gn,wt,vt,vt,vt
 * 1x1x01x01x01.1x01x01x01x0 = bl,bl,bl,bl,bl,bl,bl
 * 1x0x10x10x10.1x10x10x10x1 = or,or,or,or,or,or,or
 * 1x1x10x10x10.1x01x01x01x1 = or,or,or,wt,bl,bl,bl
 */

// purple=2, orange=5, green=1, blue=6
// 0.00 = black
// 0.01 = green
// 0.10 = purple
// 0.11 = white
// 1.01 = orange
// 1.10 = blue

// memory offsets of each line
const uint16_t hires_off[] = {
  0x0000, 0x0400, 0x0800, 0x0c00, 0x1000, 0x1400, 0x1800, 0x1c00,
  0x0080, 0x0480, 0x0880, 0x0c80, 0x1080, 0x1480, 0x1880, 0x1c80,
  0x0100, 0x0500, 0x0900, 0x0d00, 0x1100, 0x1500, 0x1900, 0x1d00,
  0x0180, 0x0580, 0x0980, 0x0d80, 0x1180, 0x1580, 0x1980, 0x1d80,
  0x0200, 0x0600, 0x0a00, 0x0e00, 0x1200, 0x1600, 0x1a00, 0x1e00,
  0x0280, 0x0680, 0x0a80, 0x0e80, 0x1280, 0x1680, 0x1a80, 0x1e80,
  0x0300, 0x0700, 0x0b00, 0x0f00, 0x1300, 0x1700, 0x1b00, 0x1f00,
  0x0380, 0x0780, 0x0b80, 0x0f80, 0x1380, 0x1780, 0x1b80, 0x1f80,
  0x0028, 0x0428, 0x0828, 0x0c28, 0x1028, 0x1428, 0x1828, 0x1c28,
  0x00a8, 0x04a8, 0x08a8, 0x0ca8, 0x10a8, 0x14a8, 0x18a8, 0x1ca8,
  0x0128, 0x0528, 0x0928, 0x0d28, 0x1128, 0x1528, 0x1928, 0x1d28,
  0x01a8, 0x05a8, 0x09a8, 0x0da8, 0x11a8, 0x15a8, 0x19a8, 0x1da8,
  0x0228, 0x0628, 0x0a28, 0x0e28, 0x1228, 0x1628, 0x1a28, 0x1e28,
  0x02a8, 0x06a8, 0x0aa8, 0x0ea8, 0x12a8, 0x16a8, 0x1aa8, 0x1ea8,
  0x0328, 0x0728, 0x0b28, 0x0f28, 0x1328, 0x1728, 0x1b28, 0x1f28,
  0x03a8, 0x07a8, 0x0ba8, 0x0fa8, 0x13a8, 0x17a8, 0x1ba8, 0x1fa8,
  0x0050, 0x0450, 0x0850, 0x0c50, 0x1050, 0x1450, 0x1850, 0x1c50,
  0x00d0, 0x04d0, 0x08d0, 0x0cd0, 0x10d0, 0x14d0, 0x18d0, 0x1cd0,
  0x0150, 0x0550, 0x0950, 0x0d50, 0x1150, 0x1550, 0x1950, 0x1d50,
  0x01d0, 0x05d0, 0x09d0, 0x0dd0, 0x11d0, 0x15d0, 0x19d0, 0x1dd0,
  0x0250, 0x0650, 0x0a50, 0x0e50, 0x1250, 0x1650, 0x1a50, 0x1e50,
  0x02d0, 0x06d0, 0x0ad0, 0x0ed0, 0x12d0, 0x16d0, 0x1ad0, 0x1ed0,
  0x0350, 0x0750, 0x0b50, 0x0f50, 0x1350, 0x1750, 0x1b50, 0x1f50,
  0x03d0, 0x07d0, 0x0bd0, 0x0fd0, 0x13d0, 0x17d0, 0x1bd0, 0x1fd0,
};

void appleii::drawhiresline(uint8_t *vidmem, int row)
{
  int clrs[] = { 0, 3, 12, 15, 0, 6, 9, 15 };
  int ch0, ch1, pxl;

  for (int x = 0; x < 40; x+=2) {
    ch0 = *vidmem++;
    ch1 = *vidmem++;

    pxl = (ch0 & 0x7f) + (ch1 << 7);
    ch0 = (ch0 & 0x80) >> 5;
    ch1 = (ch1 & 0x80) >> 5;

    for (int i = 0; i < 14; i+=2) {
      int p0 = (pxl & 3);
      if (i < 7)
	p0 += ch0;
      else
	p0 += ch1;
      pxl >>= 2;
      scr->setpixel(x * 7 + i + 0, row, clrs[p0]);
      scr->setpixel(x * 7 + i + 1, row, clrs[p0]);
    }
  }
}

void appleii::drawhires(uint8_t *vidmem)
{
  for (int row = 0; row < 192; row++) {
    drawhiresline(vidmem+hires_off[row], row);
  }
}

// 0x88 = return
int kk = 0x80;
void appleii::run()
{
  int cycs;

  cpu_reset(0);
  for(;;) {
    trace = 0;
    cycs = cpu_step();

    if (crtc.tick()) {
      printf("-- frame: %d\n", frame++);
      if (mode & HIRES) {
	drawhires(&ram[page ? 0x4000 : 0x2000]);
      } else {
	drawtext(&ram[page ? 0x0800 : 0x0400]);
      }
      if (scr->key(Key::K_ENTER, true)) {
	keycode = 0x8d;
      }
      for (int i = ' '; i <= '@'; i++) {
	if (scr->key(i, true)) {
	  keycode = (i - ' ') + 0xa0;
	}
      }
      for (int i = 'a'; i <= 'z'; i++) {
	if (scr->key(i, true)) {
	  keycode = (i - 'a') + 0xc1;
	}
      }
      scr->scrtext(0, 200, 0xf, "mode=%x,%x", mode,page);
      scr->draw();
      scr->clear();
      flash = (flash + 1) % (2*FLASH_FREQ);
    }
  }
}

uint8_t cpu_read8(uint32_t offset, int type)
{
  iodata_t v;

  sys.read(offset, v);
  return v;
}

void  cpu_write8(uint32_t offset, uint8_t v, int type)
{
  sys.write(offset, v);
}

void flogger(int lvl, const char *fmt, ...)
{
  va_list ap;

  va_start(ap, fmt);
  vprintf(fmt, ap);
}

int main(int argc, char *argv[])
{
  size_t fsz;
  
  sys.init("roms/APPLE2.ROM", 0x2000);
  sys.run();
}
