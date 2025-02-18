#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <stdarg.h>
#include <inttypes.h>
#include "bus.h"
#include "cpu.h"
#include "gr.h"
#include "util.h"

// https://github.com/mausimus/atre/blob/master/src/Tests.hpp
// https://github.com/optixx/atari800xl
// https://github.com/mausimus/atre/blob/master/src/ANTIC.cpp
// https://github.com/dmlloyd/atari800/blob/master/DOC/cart.txt
// https://gury.atari8.info/
int frame;

extern void dumpcfg(int noff, int *offs, int base, int size);
//
/* 6 text modes
 *  0 40x24
 *  1 20x24
 *  2 40x12
 * 8 bitmap modes
 *  
 * Bottom 4 rows are text
 *
 * Antic::read:
 *   VCOUNT == scanline/2
 *
 * Antic::write
 *   WSYNC: wait until cycle 105 if < 104, else cycles_per_scanline - 104, dot
 *   NMIRES:
 *   NMIEN:
 *   
 */
enum {
  // GTIA
  GTIA_START = 0xD000,
  GTIA_END = 0xD01F,
  GITA_MASK = 0x1F,
  
  PAL    = 0xD014,
  COLBK  = 0xD01A,
  COLPF0 = 0xD016,
  COLPF1 = 0xD017,
  COLPF2 = 0xD018,
  COLPF3 = 0xD019,
  PRIOR  = 0xD01B,
  VDELAY = 0xD01C,
  CONSOL = 0xD01F,
  GRAFP0 = 0xD00D,
  GRAFP1 = 0xD00E,
  GRAFP2 = 0xD00F,
  GRAFP3 = 0xD010,
  GRAFM  = 0xD011,
  GRACTL = 0xD01D,
  HPOSP0 = 0xD000,
  HPOSP1 = 0xD001,
  HPOSP2 = 0xD002,
  HPOSP3 = 0xD003,
  HPOSM0 = 0xD004,
  HPOSM1 = 0xD005,
  HPOSM2 = 0xD006,
  HPOSM3 = 0xD007,
  SIZEP0 = 0xD008,
  SIZEP1 = 0xD009,
  SIZEP2 = 0xD00A,
  SIZEP3 = 0xD00B,
  SIZEM  = 0xD00C,
  COLPM0 = 0xD012,
  COLPM1 = 0xD013,
  COLPM2 = 0xD014,
  COLPM3 = 0xD015,
  TRIG0  = 0xD010,
  TRIG1  = 0xD011,
  M0PF   = 0xD000,
  M1PF   = 0xD001,
  M2PF   = 0xD002,
  M3PF   = 0xD003,
  P0PF   = 0xD004,
  P1PF   = 0xD005,
  P2PF   = 0xD006,
  P3PF   = 0xD007,
  M0PL   = 0xD008,
  M1PL   = 0xD009,
  M2PL   = 0xD00A,
  M3PL   = 0xD00B,
  P0PL   = 0xD00C,
  P1PL   = 0xD00D,
  P2PL   = 0xD00E,
  P3PL   = 0xD00F,
  HITCLR = 0xD01E,

  // POKEY
  STIMER = 0xD209,
  KBCODE = 0xD209,
  IRQEN  = 0xD20E,
  IRQST  = 0xD20E,
  SEROUT = 0xD20D,
  SERIN  = 0xD20D,
  SKSTAT = 0xD20F,
  AUDF1  = 0xD200,
  AUDF2  = 0xD202,
  AUDF3  = 0xD204,
  AUDF4  = 0xD206,
  AUDCTL = 0xD208,

  // PIA
  PORTA = 0xD300,
  PORTB = 0xD301,

  // ANTIC
  VCOUNT = 0xD40B,
  NMIEN  = 0xD40E,
  NMIST  = 0xD40F,
  NMIRES = 0xD40F,
  DLISTL = 0xD402,
  DMACTL = 0xD400,
  PMBASE = 0xD407,
  CHBASE = 0xD409,
  CHACTL = 0xD401,
  WSYNC  = 0xD40A,
  HSCROL = 0xD404
};

/*========================================
 * Device handling
 *========================================*/
struct device_t {
  bool enabled = true;
  virtual void init() { };
  virtual void tick() { };
  device_t *next;
};

static device_t *devices;
static void dev_add(device_t *dev) {
  dev->init();
  dev->next = devices;
  devices = dev;
}
static void dev_tick(int nclks) {
  while (nclks--) {
    for (auto dev = devices; dev; dev = dev->next) {
      if (dev->enabled) {
	dev->tick();
      }
    }
  }
}

struct xbank_t {
  uint8_t  *ptr;
  char     name[32];
  void set(uint8_t *p) {
    ptr = p;
  };
};
struct atari : public bus_t, public device_t {
  Screen   *scr;
  uint8_t  ram[65536] = { 0 };

  uint8_t *rom;
  size_t   romsz;

  int      banksz;
  xbank_t  banks[256];
  uint8_t *cart;
  size_t   cartsz;
  
  crtc_t crtc;
  void tick();
  void draw();
  void drawchar(uint16_t base, int x, int y, int flag, int fg, int bg);
  void init(const char *romname, const char *cartname);

  void mkbank(int n, int *pb, int start, int size);
  void setbank(int bk, int n) {
    if (n < 0) {
      n += (cartsz / banksz);
    }
    banks[bk].set(cart + (n * banksz));
    printf("setbank: %d = %d %p\n", bk, n, banks[bk].ptr);
    hexdump(banks[bk].ptr, banksz);
  };
  static constexpr uint32_t palette[] = {
    0x00000000, 0x00101010, 0x00393939, 0x00636363, 0x007B7B7B, 0x00A5A5A5, 0x00C6C6C6, 0x00EFEFEF,
    0x00100000, 0x00312100, 0x005A4200, 0x00846B00, 0x009C8400, 0x00C6AD00, 0x00E7D629, 0x00FFF74A,
    0x00310000, 0x005A0800, 0x007B2900, 0x00A55200, 0x00BD6B00, 0x00E79429, 0x00FFB552, 0x00FFDE73,
    0x004A0000, 0x006B0000, 0x00941000, 0x00BD3929, 0x00D65242, 0x00FF7B6B, 0x00FFA594, 0x00FFC6B5,
    0x004A0000, 0x00730029, 0x0094004A, 0x00BD2973, 0x00D64294, 0x00FF6BB5, 0x00FF94DE, 0x00FFB5FF,
    0x0039004A, 0x00630073, 0x008C0094, 0x00AD21BD, 0x00CE42D6, 0x00EF63FF, 0x00FF8CFF, 0x00FFB5FF,
    0x0021007B, 0x004200A5, 0x006B00C6, 0x009429EF, 0x00AD42FF, 0x00D66BFF, 0x00F794FF, 0x00FFB5FF,
    0x00000094, 0x002100BD, 0x004210DE, 0x006B39FF, 0x008452FF, 0x00AD7BFF, 0x00CE9CFF, 0x00F7C6FF,
    0x0000008C, 0x000000B5, 0x001829D6, 0x004252FF, 0x005A6BFF, 0x008494FF, 0x00A5B5FF, 0x00CEDEFF,
    0x00000063, 0x0000218C, 0x000042AD, 0x00186BD6, 0x003984F7, 0x005AADFF, 0x0084CEFF, 0x00ADF7FF,
    0x00001021, 0x0000394A, 0x00005A73, 0x00008494, 0x00219CB5, 0x004AC6DE, 0x006BE7FF, 0x0094FFFF,
    0x00002100, 0x00004A00, 0x00006B21, 0x0000944A, 0x0018AD6B, 0x0042D68C, 0x0063F7B5, 0x008CFFDE,
    0x00002900, 0x00004A00, 0x00007300, 0x00109C08, 0x0029B521, 0x0052DE4A, 0x0073FF6B, 0x009CFF94,
    0x00002100, 0x00004A00, 0x00006B00, 0x00299400, 0x0042AD00, 0x006BD610, 0x0094FF39, 0x00B5FF5A,
    0x00001000, 0x00083900, 0x00296300, 0x00528400, 0x006BA500, 0x0094C600, 0x00B5EF18, 0x00DEFF42,
    0x00080000, 0x00312100, 0x00524A00, 0x007B6B00, 0x00948C00, 0x00BDB500, 0x00E7D621, 0x00FFFF4A
  };    
};

static atari sys;

static int gtia_io(void *arg, uint32_t addr, int mode, iodata_t& io)
{
  memio(sys.ram, addr, mode, io);
  return 0;
}

static int pokey_io(void *arg, uint32_t addr, int mode, iodata_t& io)
{
  return 0;
}

static int pia_io(void *arg, uint32_t addr, int mode, iodata_t& io)
{
  return 0;
}

int antic_io(void *arg, uint32_t addr, int mode, iodata_t& io)
{
  if (mode == 'r') {
    switch(addr) {
    case VCOUNT:
      io = sys.crtc.vPos/2;
      break;
    };
  }
  else {
    switch(addr) {
    case WSYNC:
      printf("WSYNC....\n");
      break;
    }
  }
  return 0;
}

int romio(void *arg, uint32_t addr, int mode, iodata_t& io)
{
  if (mode == 'w') {
    printf("write rom: %.4x %.2x\n", addr, mode);
    sys.ram[addr] = io;
  }
  else {
    switch(addr) {
    case 0xc000 ... 0xcfff:
    case 0xd800 ... 0xffff:
      io = sys.rom[addr - 0xc000];
      break;
    case 0x5000 ... 0x57ff:
      io = sys.rom[addr - 0x5000 + 0x1000];
      break;
    }
  }
  return 0;
}

int rwbankio(void *arg, uint32_t addr, int mode, iodata_t& io)
{
  xbank_t *bank = (xbank_t *)arg;
  
  printf("cartio: PORTB: %c %.4x %.2x %p\n", mode, addr, sys.ram[PORTB], bank->ptr);
  if (mode == 'r') {
    memio(bank->ptr, addr, mode, io);
  }
  else {
    assert(0);
  }
  return 0;
}

int rwset(void *arg, uint32_t addr, int mode, iodata_t& io)
{
  if (mode == 'w') {
    sys.setbank(0, (io & 0x3));
  };
}

/* d000.d7ff io.read
 * c000.cfff rom[addr - c000] if !(PORTB & 1)
 * a000.bfff cartrom[addr - a000] if !(PORTB & 2)
 * 5000.57ff osrom[addr - 0x5000 + 0x1000], if !(PORTB & 0x80) && (PORTB & 1)
 * d800.ffff osrom[addr - 0xc000] if (PORTB & 1)
 * else ram[addr]
 *
 * Write:
 * d800.d7ff io.write
 * c000.cfff rom[addr - 0xc000]
 * d800.dfff rom[addr - 0xc000] ram[addr] if !(PORTB & 1)
 * a000.bfff ram[addr] if (PORTB & 2)
 * 5000.5800 ram[addr] if (PORTB & 1) || (PORTB & 0x80)

 * c000.cfff = rom[0000..0fff] 4k
 * d800.ffff = rom[1800..3fff]
 * 5000.57ff = rom[1000..17ff]
 */
extern int trace;

void atari::tick()
{
  if (crtc.tick()) {
    printf("---- doneframe\n");
  }
}

/* Draw character at memory offset with color */
void atari::drawchar(uint16_t base, int x, int y, int dd, int fg, int bg)
{
  uint8_t pxl;
  
  for (int h = 0; h < 8; h++) {
    pxl = cpu_read8(base++);
    for (int w = 0; w < 8; w++) {
      for (int wd = 0; wd < dd; wd++) {
	scr->setpixel(x+(w*dd)+wd, y+h, (pxl & (0x80 >> w)) ? fg : bg);
      }
    }
  }
}

void atari::draw()
{
  int x = 0, y = 0;
  int addr = 0xe000;
  int flag = 2;
  
  printf("-- frame: %d\n", frame);
  frame++;

  for (int i = 0; i < 128; i++) {
    drawchar(0xe000 + i * 8, x, y, flag, i, 0);
    x = x + 8 * flag;
    if (x >= scr->width) {
      x = 0;
      y = y + 8;
    }
  }
  //scr->scrtext(0, 200, MKRGB(255,255,0), "frame:%d", frame);
  scr->scrtext(0, 200, 128, "frame:%d", frame);
  scr->draw();
}

/* http://www.atarimania.com/faq-atari-400-800-xl-xe-what-is-an-atari-8-bit-computer_3.html
 * MACHINE CYCLES per FRAME:
 *    29859 (NTSC machines) (1.7897725MHz / 59.94Hz)
 *    35568 (PAL/SECAM machines) (1.7734470MHz / 49.86Hz)
 *
 * SCAN LINES per FRAME
 *    262 (NTSC machines)
 *    312 (PAL/SECAM machines)
 *
 * MACHINE CYCLES per SCAN LINE
 *     114        (NTSC machines: 29859 cycles/frame / 262 lines/frame;
 *            PAL/SECAM machines: 35568 cycles/frame / 312 lines/frame)
 *
 */
#define CYCLES_PER_FRAME 29859
void atari::mkbank(int n, int *ibank, int start, int size) {
  banksz = size;
  for (int i = 0; i < n; i++) {
    snprintf(banks[i].name, sizeof(banks[i].name), "BANK%d", i);
    setbank(i, ibank[i]);
    register_handler(start, start + size - 1, size - 1, rwbankio, &banks[i], _RD|_DBG, banks[i].name);
    start += size;
  };
}

void atari::init(const char *rom_file, const char *cart_file)
{
  palclr p[129] = { 0 };
  int cartType = 0;
  int ibank[256] = {0};

  // Add this device as ticker
  dev_add(this);
  
  rom = loadrom(rom_file, romsz);
  if (!rom) {
    return;
  }
  cart = loadrom(cart_file, cartsz);
  if (!cart) {
    return;
  }
  bus_t::init(0xFFFF);

  if (!memcmp(cart, "CART", 4)) {
    printf("CARTRIDGE: type:%.2x\n", cart[0x7]);
    cartType = cart[0x7];
    cart += 0x10;
    cartsz -= 0x10;
  }
  printf("Loaded rom: %x %x\n", romsz, cartsz);
  register_handler(0x0000, 0x4FFF, 0xFFFF, memio,    ram,  _RW, "RAM");
  register_handler(0xD000, 0xD0FF, 0xFFFF, gtia_io,  this, _RW|_DBG, "GTIA");
  register_handler(0xD200, 0xD2FF, 0xFFFF, pokey_io, this, _RW|_DBG, "POKEY");
  register_handler(0xD300, 0xD3FF, 0xFFFF, pia_io,   this, _RW|_DBG, "PIA");
  register_handler(0xD400, 0xD4FF, 0xFFFF, antic_io, this, _RW|_DBG, "ANTIC");
  register_handler(0xC000, 0xCFFF, 0xFFFF, romio,    &rom[0x0000], _RW, "ROM.c000");
  register_handler(0x5000, 0x57FF, 0xFFFF, romio,    &rom[0x1000], _RW, "ROM.5000");
  register_handler(0xD800, 0xFFFF, 0xFFFF, romio,    &rom[0x1800], _RW, "ROM.d800");
  register_handler(0xE000, 0xEFFF, 0xFFFF, romio,    &rom[0x2000], _RW, "ROM.e000");
  register_handler(0xF000, 0xFFFF, 0xFFFF, romio,    &rom[0x3000], _RW, "ROM.f000");

  switch(cartType) {
  case 0x00:
    register_handler(0x5800, 0x9FFF, 0xFFFF, memio,    ram,  _RW, "RAM");
    ibank[0] = 0;
    ibank[1] = 1;
    mkbank(2, ibank, 0x8000, 0x2000);
    break;
  case 0x0d: // xgs 64k
    ibank[0] = 0;
    ibank[1] = -1;
    register_handler(0x5800, 0x7FFF, 0xFFFF, memio,    ram,  _RW, "RAM");
    mkbank(2, ibank, 0x8000, 0x2000); // two 8k banks
    break;
  }
  int cycs, totcyc = CYCLES_PER_FRAME;
  uint32_t r = cpu_read16(0xfffc);
  printf("reset = %x\n", r);

  trace = 0;
  cpu_reset(0);

  /* Dump code */
  int nxt[32];
  nxt[0] = cpu_read16(0xfffc); // reset
  nxt[1] = cpu_read16(0xfffe); // irq
  nxt[2] = cpu_read16(0xfffa); // nmi;
  nxt[3] = cpu_read16(0xbffa); // cart start
  nxt[4] = cpu_read16(0xbffe); // cart init
  if (!nxt[4])
    nxt[4] = -1;
  
  dumpcfg(5, nxt, 0x0000, 0xffff);

  // setup palette
  for (int i = 0; i < 128; i++) {
    p[i].r = (palette[i] >> 16) & 0xff;
    p[i].g = (palette[i] >> 8) & 0xff;
    p[i].b = (palette[i] & 0xFF);
  }
  p[128].r = 0xff;
  p[128].g = 0xff;
  p[128].b = 0;
  crtc.init(256, 320-256, 248, 262-248);

  scr = new Screen(320, 200, 20, 20, 129, p);
  //scr->clrmode = 1;
  //scr->init(1);
  scr->init();
  cpu_reset(0);
  for(;;) {
    int npc;
    
    npc = cpu_getstate(NULL);
    if (npc == 0xc2b3)
      trace = 0; //1;
    cycs = cpu_step();

    /* Tick devices */
    dev_tick(cycs);

    totcyc += cycs;
    if (totcyc >= CYCLES_PER_FRAME) {
      totcyc -= CYCLES_PER_FRAME;
      draw();
    };
  }
  
}

void flogger(int lvl, const char *fmt, ...) {
  va_list ap;

  va_start(ap, fmt);
  vprintf(fmt, ap);
}

uint8_t cpu_read8(uint32_t addr, int type) {
  iodata_t data = 0;
  sys.read(addr, data);
  return data;
}

void cpu_write8(uint32_t addr, uint8_t v, int type) {
  iodata_t data = v;
  sys.write(addr, data);
}

#define ce(n) { n, #n }
struct cenum {
  int id;
  const char *str;
};
const char *comment(int addr) {
  static constexpr cenum cc[] = {
    //GTIA
    ce(PAL),
    ce(COLBK),
    ce(COLPF0),
    ce(COLPF1),
    ce(COLPF2),
    ce(COLPF3),
    ce(PRIOR),
    ce(VDELAY),
    ce(CONSOL),
    ce(GRAFP0),
    ce(GRAFP1),
    ce(GRAFP2),
    ce(GRAFP3),
    ce(GRAFM),
    ce(GRACTL),
    ce(HPOSP0),
    ce(HPOSP1),
    ce(HPOSP2),
    ce(HPOSP3),
    ce(HPOSM0),
    ce(HPOSM1),
    ce(HPOSM2),
    ce(HPOSM3),
    ce(SIZEP0),
    ce(SIZEP1),
    ce(SIZEP2),
    ce(SIZEP3),
    ce(SIZEM),
    ce(COLPM0),
    ce(COLPM1),
    ce(COLPM2),
    ce(COLPM3),
    ce(TRIG0),
    ce(TRIG1),
    ce(M0PF),
    ce(M1PF),
    ce(M2PF),
    ce(M3PF),
    ce(P0PF),
    ce(P1PF),
    ce(P2PF),
    ce(P3PF),
    ce(M0PL),
    ce(M1PL),
    ce(M2PL),
    ce(M3PL),
    ce(P0PL),
    ce(P1PL),
    ce(P2PL),
    ce(P3PL),
    ce(HITCLR),

    // POKEY
    ce(STIMER),
    ce(KBCODE),
    ce(IRQEN),
    ce(IRQST),
    ce(SEROUT),
    ce(SERIN),
    ce(SKSTAT),
    ce(AUDF1),
    ce(AUDF2),
    ce(AUDF3),
    ce(AUDF4),
    ce(AUDCTL),

    // PIA
    ce(PORTA),
    ce(PORTB),

    // ANTIC
    ce(VCOUNT),
    ce(NMIEN),
    ce(NMIST),
    ce(NMIRES),
    ce(DLISTL),
    ce(DMACTL),
    ce(PMBASE),
    ce(CHBASE),
    ce(CHACTL),
    ce(WSYNC),
    ce(HSCROL),
    {},
  };
  for (auto c = cc; c->str; c++) {
    if (c->id == addr)
      return c->str;
  }
  return "";
}

int main(int argc, char *argv[])
{
  setbuf(stdout, NULL);
  if (argc > 1) {
    sys.init("atarirom/REV03.ROM", argv[1]);
  }
}
