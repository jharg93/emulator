#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>
#include <stdarg.h>
#include <map>
#include <queue>
#include "cpu.h"
#include "bus.h"
#include "gr.h"
#include "util.h"
#include "crtc.h"
#include "atarist.h"
#include "cpu/cpu_m68k.h"

extern int trace;

constexpr auto WHITE = MKRGB(255,255,255);

/* CART Header
 * 01 = init
 * 02 = init/post isr
 * 04 = system init donw
 * 08 = after gemdos
 * 10 =
 * 20 = desktop acc
 * 40 = tos app
 * 80 = ttp
 */
void flogger(int n, const char *fmt, ...) {
  va_list ap;
  printf("flogger: ");
  va_start(ap, fmt);
  vprintf(fmt, ap);
}

auto pclr = [](uint16_t *p, uint16_t &mask) {
  int clr = 0;
  if (p[0] & mask) clr |= 0x1;
  if (p[1] & mask) clr |= 0x2;
  if (p[2] & mask) clr |= 0x4;
  if (p[3] & mask) clr |= 0x8;
  mask >>= 1;
  return clr;
 };
//https://github.com/dmcoles/EstyJs/tree/master
/*
 * timer::tick() {
 *   int ic = ctrl & 0xF;
 *   if (ctrl > 0 && ctrl < 8) {
 *     cntr2++;
 *     int n = scale * clockCyclesPerCpuCycle;
 *     while (cntr2 > n) {
 *       cntr2 -= n;
 *       cntr--;
 *       if (cntr == 0) {
 *         cntr = data;
 *         raise(id);
 *       }
 *     }
 *   }
 * }
 */    
 

/* Atari ST memory map
 * https://temlib.org/AtariForumWiki/index.php/Exception_vectors_and_basic_RAM_%26_ROM_locations
 * https://temlib.org/AtariForumWiki/index.php/Memory_Map_for_Atari_ST,STE,TT_and_Falcon
 * https://sarnau.info/atari-st-book-bios-xbios-rom-listing/
 * https://docs.sidecartridge.com/sidecartridge-multidevice/hardware_interface/
 * http://cd.textfiles.com/ataricompendium/BOOK/PDF/APPENDB.PDF
 * https://www.atari-wiki.com/index.php?title=Atari_ST/STe/MSTe/TT/F030_Hardware_Register_Listing
 *
 * https://atari.8bitchip.info/cartST.html
 * 
 * CPU: 68000
 * 
 * 320x200x16, 3R3G3B = 512 palette colors
 * 640x200x4
 * 640x400x1
 *
 * IRQ:
 *  lvl1: tt vme
 *  lvl2: hbl
 *  lvl3: tt vme
 *  lvl4: vbl
 *  lvl5:
 *  lvl6: mfp
 *  lvl7:
 */

/* FDC 1772
 * MFP 68901
 * ACIA 6850
 * SND YM-2149

 * 00.0000 - 07.FFFF 512k RAM
 * F0.0000 - FB.FFFF 128K ROM
 * FC.0000 - FE.FFFF 192k System ROM
 * FF.8000 - FF.81FF Configuration Registers
 *    8000 - TT mmu memory config
 *    8001 - ST mmu memory config
 *    8007 - falcon30 bus control
 * FF.8200 - FF.83FF Display Registers
 *    8201 - vbase_hi
 *    8203 - vbase_mid
 *    8205 - vaddr_hi
 *    8207 - vaddr_mid
 *    8209 - vaddr_lo
 *    820a - synch mode
 *    820d - vbase_lo
 *    820e - offset to next line
 *    820f - extra width of scanline
 *    8210 - width of scanline in words
 * FF.8600 - FF.87FF DMA Registers
 * FF.F800 - FF.FBFF I/O Area
 *
 * 00.0008 - CF.FFFF 14MB RAM
 * E0.0000 - F0.003F 1MB ROM
 * F0.0040 - F9.FFFF ILLEGAL
 * FA.0000 - FB.FFFF CATRIDGE EXPANSION
 * FC.0000 - FE.FFFF 192K ROM
 *
 * YM2149/AY-3-8910 SOUND
 */

/* 4 planes: PLANE0|PLANE1|PLANE2|PLANE3
 * 2 planes: PLANE0|PLANE1|PLANE0|PLANE1
 * 1 plane:  PLANE0|PLANE0|PLANE0|PLANE0
 */

/* 0         1         2         3         4         5         6         7
 * xxxx.xxxx.xxxx.xxxx|xxxx.xxxx.xxxx.xxxx|xxxx.xxxx.xxxx.xxxx|xxxx.xxxx.xxxx.xxxx  20x16 = 320x4 bpp
 * 1                   2                   3                   4
 *
 * 0         1         2         3         
 * xxxx.xxxx.xxxx.xxxx|xxxx.xxxx.xxxx.xxxx| 40x16 = 640x2 bpp
 * 1                   2
 */

/* 60hz
 * 262 lines per frame
 * 508 cycles per scanline
 * 50hz
 * 313 lines per frame
 * 512 cycles per scanline
 */

void m68k_emul1010(uint16_t) { }
void m68k_emul1111(uint16_t) { }

void getextra(dstk& s, uint32_t base, uint32_t size)
{
  FILE *fp;
  char line[128];
  uint32_t b;

  fp = fopen("atarist_dumpcfg.txt", "r");
  while (fgets(line, sizeof(line), fp) != NULL) {
    sscanf(line, "%x", &b);
    s.push(b, 1, dstk::PENDING);
  }
}

void cpu_shutdown() {
#if 0
  FILE *fp;
  
  fp = fopen("atarist_dumpcfg.txt", "w+");
  for (auto x : visited) {
    int xy = x.first;
    fprintf(fp, "%.8x: %.4x %s\n", xy, x.second,fnmap[xy].c_str());
  }
  fclose(fp);
#endif
}

void cpu_reset(uint32_t addr) {
  SP = cpu_read32(0);
  PC = cpu_read32(4);
}

enum MfpRegs {
  GPIP = 0x01, // General Purpose I / O Data Register
  AER  = 0x03, // Active Edge Register
  DDR  = 0x05, // Data Direction Register
  IERA = 0x07, // interrupt enable chan 8..15
  IERB = 0x09, // interrupt enable chan 0..7
  IPRA = 0x0b, // interrupt pending chan 8..15
  IPRB = 0x0d, // interrupt pending chan 0..7
  ISRA = 0x0f, // interrupt in-service 8..15
  ISRB = 0x11, // interrupt in-service 0..7
  IMRA = 0x13, // interrupt mask 8..15
  IMRB = 0x15, // interrupt mask 0..7
  VR   = 0x17, // Vector Register

  // timers
  //  a/b/c/d = prescalar
  //  m = mode (0=delay, 1=pulse)
  //  r = reset
  TACR = 0x19, // timer a control    ---rmaaa
  TBCR = 0x1b, // timer b control    ---rmbbb
  CDCR = 0x1d, // timer c/d control  -ccc-ddd
  TADR = 0x1f, // timer a data
  TBDR = 0x21, // timer b data
  TCDR = 0x23, // timer c data
  TDDR = 0x25, // timer d data

  // uart
  SCR  = 0x27, // Synchronous Character Register
  UCR  = 0x29, // USART Control Register
  RSR  = 0x2b, // Receiver Status Register
  TSR  = 0x2d, // Transmitter Status Register
  UDR  = 0x2f, // USART Data Register

  CH_TIMERA = 13, // interrupt 13
  CH_TIMERB = 8,  // interrupt 8
  CH_TIMERC = 5,  // interrupt 5
  CH_TIMERD = 4,  // interrupt 4
  CH_ACIA   = 7,  // interrupt 7 (GPIP/I5, keyboard+MIDI ACIA, wired-OR)
};

struct keymap {
  int key, code;
};
const auto mkkeys() {
  std::vector<keymap> kbdcodes {
    { '1', 0x02 },
    { '2', 0x03 },
    { '3', 0x04 },
    { '4', 0x05 },
    { '5', 0x06 },
    { '6', 0x07 },
    { '7', 0x08 },
    { '8', 0x09 },
    { '9', 0x0a },
    { '0', 0x0b },

    { 'q', 0x10 },
    { 'w', 0x11 },
    { 'e', 0x12 },
    { 'r', 0x13 },
    { 't', 0x14 },
    { 'y', 0x15 },
    { 'u', 0x16 },
    { 'i', 0x17 },
    { 'o', 0x18 },
    { 'p', 0x19 },
    { '[', 0x1a },
    { ']', 0x1b },

    { 'a', 0x1e },
    { 's', 0x1f },
    { 'd', 0x20 },
    { 'f', 0x21 },
    { 'g', 0x22 },
    { 'h', 0x23 },
    { 'j', 0x24 },
    { 'k', 0x25 },
    { 'l', 0x26 },
    { ';', 0x27 },
    { '\'', 0x29 },

    { 'z', 0x2c },
    { 'x', 0x2d },
    { 'c', 0x2e },
    { 'v', 0x2f },
    { 'b', 0x30 },
    { 'n', 0x31 },
    { 'm', 0x32 },
    { ',', 0x33 },
    { '.', 0x34 },
  };
  return kbdcodes;
};
const auto keytbl = mkkeys();

// https://www.nxp.com/docs/en/reference-manual/MC68901UM.pdf
// https://www.lynn3686.com/mfp.html
// https://github.com/frno7/cf68901
// Timer C is 200Hz clock
struct mfptimer {
  uint8_t  ctrl = 0;
  uint8_t  data = 0;

  int32_t  counter = 0;
  int      id = 0;
  char     k;
  mfptimer(int n, int m) {
    id = n;
    k = m;
  };

  // get timer scale
  int prescale(int n) const {
    constexpr int d[] = { 0, 4, 10, 16, 50, 64, 100, 200 };
    return d[n & 7];
  };
  // start new timer
  void start(uint8_t nctrl, uint8_t ndata) {
    nctrl &= 0xF;
    printf("timer start%c: ctrl:%.2x/%.2x data:%.2x/%.2x\n",
	   k,
	   ctrl, nctrl,
	   data, ndata);
    if (!ctrl && nctrl) {
      printf("start timer....: %c\n", k);
      data = ndata;
      counter = data * prescale(nctrl);
    } else if (ctrl && !nctrl) {
      printf("stop timer...: %c\n", k);
      counter = -1;
    }
    ctrl = nctrl;
  };
  bool tick() {
    if (!(ctrl & 7)) {
      return false;
    }
    if (counter > 0)
      counter--;
    if (counter == 0) {
      counter = data * prescale(ctrl);
      return true;
    }
    return false;
  };
};

/* Read cycle
 * if (_CS && _DS && R/_W) {
 *   D07 = read_reg(RS15);
 *   _DTACK = 1;  -> processor negates _DS
 * }
 * Write cycle
 * if (_CS && _DS && !R/_W) {
 *   write_reg(RS15, D07);
 *   _DTACK = 1;
 * }
 */
struct mfp_t {
  uint8_t regs[256];

  // setup timers
  mfptimer ta = { CH_TIMERA, 'a' };
  mfptimer tb = { CH_TIMERB, 'b' };
  mfptimer tc = { CH_TIMERC, 'c' };
  mfptimer td = { CH_TIMERD, 'd' };

  int dp(int h) const { return (regs[h] << 8) + regs[h+2]; };
  int ier(int mask=-1) const { return mask & dp(IERA); }; // enabled
  int ipr(int mask=-1) const { return mask & dp(IPRA); }; // pending
  int isr(int mask=-1) const { return mask & dp(ISRA); }; // servicing
  int imr(int mask=-1) const { return mask & dp(IMRA); }; // mask

  // raise interrupt
  void raise(int ch) {
    const uint16_t mask = 1 << ch;
    if ((ier() & imr() & mask) != 0) {
      regs[IPRA] |= (mask >> 8);
      regs[IPRB] |= (mask & 0xff);
      printf("RAISE: mask:%.4x en:%.4x pend:%.4x mask:%.4x\n", mask, ier(), ipr(), imr());
    }
  };
  int pending() {
    for (int i = 15; i >= 0; i--) {
      int mask = (1L << i);
      if (ier() & ipr() & imr() & mask) {
	regs[ISRA] |= (mask >> 8);
	regs[ISRB] |= (mask & 0xff);
	regs[IPRA] &= ~(mask >> 8);
	regs[IPRB] &= ~(mask & 0xff);
	printf("service: %x\n", i);
	return i;
      }
    }
    return -1;
  }
  uint32_t mclk_acc;

  void tick() {
    static const int MFP_HZ = 2457600;
    static const int CPU_HZ = 8000000;
    mclk_acc += MFP_HZ;
    while (mclk_acc > CPU_HZ) {
      mclk_acc -= CPU_HZ;
      if (ta.tick()) raise(ta.id);
      if (tb.tick()) raise(tb.id);
      if (tc.tick()) raise(tc.id);
      if (td.tick()) raise(td.id);
    };
  };
};

const char *sk[] = {
  "gpip", "aer", "ddr", "iera", "ierb", "ipra", "iprb", "isra", "isrb", "imra", "imrb", "vr",
  "tacr", "tbcr", "cdcr", "tadr", "tbdr", "tcdr", "tddr", "scr", "ucr", "rsr", "tsr", "udr"
};

static int cartio(void *arg, uint32_t addr, int mode, iodata_t& io) {
  io = -1;
  return 0;
}

static int mfpio(void *arg, uint32_t addr, int mode, iodata_t& io) {
  mfp_t *m = (mfp_t *)arg;
  uint8_t *regs = m->regs;
  
  bememio(m->regs, addr, mode, io);
  if ((mode & 0xff) == 'r') {
    return 0;
  }
  switch (addr) {
  case TACR:
  case TADR:
    m->ta.start(regs[TACR], regs[TADR]);
    break;
  case TBCR:
  case TBDR:
    m->tb.start(regs[TBCR], regs[TBDR]);
    break;
  case CDCR:
    m->tc.start(regs[CDCR]>>4, regs[TCDR]);
    m->td.start(regs[CDCR], regs[TDDR]);
    break;
  case TCDR:
    m->tc.start(regs[CDCR]>>4, regs[TCDR]);
    break;
  case TDDR:
    m->td.start(regs[CDCR], regs[TDDR]);
    break;
  }
  return 0;
}

/* Video
 *  FF8201 : base_hi
 *  FF8203 : base_mid
 *  FF820D : base_lo
 *  FF820A : syncmod
 *  FF8260 : v_shf_mod
 *  FF8262 : shift_tt
 *  FF8262 : spshift
 *  FF8240 : palette
 *  FF8260 : resolution
 */
enum ShifterRegs {
  VBASE_HI   = 0x01,
  VBASE_MID  = 0x03,
  VBASE_LO   = 0x0D,
  RESOLUTION = 0x60,
};

struct shifter : public crtc_t {
  uint8_t regs[256];

  // NTSC:
  //  508 cycles per line, start =     visible=320
  //  263 lines per frame, start = 63, visible=200
  // PAL:
  //  512 cycles per line, start = 96, visible=320
  //  313 lines per frame, start = 63, visible=200
  /*
   * DE goes high around cycle 52–60 depending on "wakestate" — the ST's horizontal state machine has a documented
   * quirk where the exact DE-start cycle jitters by a few cycles (in units of 4) depending on which 68000 bus phase
   * the line boundary lands on. One measured trace: wakestate 1 has HSYNC end at cycle 504, BLANK end at cycle 32,
   * DE high at cycle 60, LOAD (first fetch) at cycle 66.
   * DE goes low again around cycle 372–380 for low/mid res (320 active cycles), matching the 52 + 320 = 372
   * window I used earlier.
  */
  const int x0 = 66;
  const int x1 = 66+320;
  const int y0 = 63;
  const int y1 = 63+200;

  bool de;
  uint32_t vaddr;
  shifter() {
    crtc_t::init(x1, 512, y1, 313);
  };
  void sethblank(bool en) {
    if (en) {
      cpu_irq(2);
    }
  };
  void setvblank(bool en) {
    if (en) {
      cpu_irq(4);
    }
  };
  bool tick() {
    de = (hPos == hBlank && vPos >= y0 && vPos <= y1);
    if (crtc_t::tick()) {
      // end-of-frame
      vaddr = vbase();
      printf("vbase = %x\n", vaddr);
      return true;
    }
    return false;
  };
  // loaded at vblank
  void setvbase(uint32_t addr) {
    regs[VBASE_HI] = addr >> 16;
    regs[VBASE_MID] = addr >> 8;
    regs[VBASE_LO] = addr;
  };
  uint32_t vbase() const {
    return (regs[VBASE_HI] << 16) + (regs[VBASE_MID] << 8) + regs[VBASE_LO];
  };
  // color is 0rgb
  uint32_t palette(int n) const {
    uint16_t c = get16be(&regs[0x40 + (n * 2)]);
    int r3 = (c >> 8) & 7;
    int g3 = (c >> 4) & 7;
    int b3 = (c >> 0) & 7;
    r3 = (r3 << 5) | (r3 << 2) | (r3 >> 1);
    g3 = (g3 << 5) | (g3 << 2) | (g3 >> 1);
    b3 = (b3 << 5) | (b3 << 2) | (b3 >> 1);
    return MKRGB(r3, g3, b3);
  };
  int planes() const {
    switch (regs[RESOLUTION] & 3) {
    case 0: return 4;
    case 1: return 2;
    case 2: return 1;
    }
    return 0;
  };
  int xres() const {
    switch (regs[RESOLUTION] & 3) {
    case 0: case 1: return 320;
    case 2: return 640;
    };
    return 0;
  };
  void renderline(int *pxl) {
    uint16_t pdata[4] = { 0 };

    int width = xres();
    for (int i = 0; i < width; i += 16) {
      for (int p = 0; p < planes(); p++) {
	pdata[p] = cpu_read16(vaddr);
	vaddr += 2;
      }
      uint16_t mask = 0x8000;
      for (int p = 0; p < 16; p++) {
	int clr = pclr(pdata, mask);
	*pxl++ = palette(clr);
	if (width == 320)
	  *pxl++ = palette(clr);
      }
    }
  };
};

/* Keyboard ACIA (6850)
 * FF.FC00 - control (write) / status (read)
 * FF.FC02 - data
 *
 * The real IKBD talks to the host over this ACIA as a serial link: each
 * keypress arrives as a single byte, the raw scancode for a press and the
 * same value with bit7 set for a release. We skip modelling the 6301 command
 * protocol (resets, mouse/joystick packets, etc) and just push key events
 * from the host keyboard directly into the receive fifo.
 */
enum AciaRegs {
  ACIA_CR = 0x00, // control (w) / status (r)
  ACIA_DR = 0x02, // data
};
enum {
  ACIA_SR_RDRF = 0x01, // receive data register full
  ACIA_SR_TDRE = 0x02, // transmit data register empty (always ready, we don't model tx)
  ACIA_SR_IRQ  = 0x80,
  ACIA_CR_IRQ  = 0x80, // receive interrupt enable
};

struct ikbd_t {
  std::queue<uint8_t> q;
  uint8_t sr = 0;
  uint8_t cr = 0;

  void write_ctrl(uint8_t data) {
    cr = data;
    if ((cr & 0x3) == 3) {
      while (!q.empty())
	q.pop();
      sr |= ACIA_SR_TDRE;
    };
  };
  uint8_t status() const {
    return sr;
  };
  void push(int b) {
    q.push(b);
    sr |= ACIA_SR_RDRF;
    if (cr & ACIA_CR_IRQ)
      sr |= ACIA_SR_IRQ;
  };
  uint8_t pop() {
    if (q.empty()) {
      return 0;
    }
    auto b = q.front();
    q.pop();
    if (q.empty()) {
      sr &= ~(ACIA_SR_RDRF | ACIA_SR_IRQ);
    }
    return b;
  };
};

// Atari ST keyboard scancodes (make code; release = code | 0x80). These
// match the PC/XT set for the keys ST and PC share.
uint8_t ascii_scancode(int ch) {
  switch (ch) {
  case '1': return 0x02; case '2': return 0x03; case '3': return 0x04;
  case '4': return 0x05; case '5': return 0x06; case '6': return 0x07;
  case '7': return 0x08; case '8': return 0x09; case '9': return 0x0A;
  case '0': return 0x0B; case '-': return 0x0C; case '=': return 0x0D;
  case 'q': return 0x10; case 'w': return 0x11; case 'e': return 0x12;
  case 'r': return 0x13; case 't': return 0x14; case 'y': return 0x15;
  case 'u': return 0x16; case 'i': return 0x17; case 'o': return 0x18;
  case 'p': return 0x19;
  case 'a': return 0x1E; case 's': return 0x1F; case 'd': return 0x20;
  case 'f': return 0x21; case 'g': return 0x22; case 'h': return 0x23;
  case 'j': return 0x24; case 'k': return 0x25; case 'l': return 0x26;
  case ';': return 0x27; case '\'':return 0x28; case '`': return 0x29;
  case '\\':return 0x2B;
  case 'z': return 0x2C; case 'x': return 0x2D; case 'c': return 0x2E;
  case 'v': return 0x2F; case 'b': return 0x30; case 'n': return 0x31;
  case 'm': return 0x32; case ',': return 0x33; case '.': return 0x34;
  case '/': return 0x35;
  case ' ': return 0x39;
  }
  return 0;
};

uint8_t key_scancode(int key) {
  switch (key) {
  case Key::K_ENTER:  return 0x1C;
  case Key::K_TAB:    return 0x0F;
  case Key::K_DEL:    return 0x0E; // backspace
  case Key::K_LSHIFT: return 0x2A;
  case Key::K_RSHIFT: return 0x36;
  case Key::K_LCTRL:
  case Key::K_RCTRL:  return 0x1D;
  case Key::K_UP:     return 0x48;
  case Key::K_DOWN:   return 0x50;
  case Key::K_LEFT:   return 0x4B;
  case Key::K_RIGHT:  return 0x4D;
  case Key::K_F1:     return 0x3B;
  case Key::K_F2:     return 0x3C;
  case Key::K_F3:     return 0x3D;
  case Key::K_F4:     return 0x3E;
  case Key::K_F5:     return 0x3F;
  case Key::K_F6:     return 0x40;
  case Key::K_F7:     return 0x41;
  case Key::K_F8:     return 0x42;
  case Key::K_F9:     return 0x43;
  case Key::K_F10:    return 0x44;
  }
  return 0;
};

int kbdio(void *arg, uint32_t addr, int mode, iodata_t& data) {
  ikbd_t *kbd = (ikbd_t *)arg;
  bool wr = (mode & 0xff) == 'w';

  switch (addr) {
  case ACIA_CR:
    if (wr)
      kbd->write_ctrl(data);
    else
      data = kbd->status();
    break;
  case ACIA_DR:
    if (wr) {
      // host->IKBD command byte (reset, mouse mode, etc) - not modelled
      printf("kbd: ignoring host command %.2x\n", data);
    } else {
      data = kbd->pop();
    }
    break;
  default:
    data = 0xff;
    break;
  }
  return 0;
}

int rtcio(void *arg, uint32_t addr, int mode, iodata_t& data) {
}

int dcfg;

struct atarist : public bus_t, public shifter {
  Screen *scr;

  mfp_t mfp;
  ikbd_t kbd;
  uint8_t ram[520*1024];
  int showvec;
  
  atarist() : bus_t(0xffffff) {
  };
  void bus_error(uint32_t addr, int mode) {
    uint32_t bb = cpu_read32(0x08);
    printf("%.8x flogger: bus error @ %.8x %c [%.8x]\n",
	   PC, addr, mode, bb);
    if (bb == 0x00fc0f3a) {
      m68k_trapa(true, VECTOR_BUS_ERROR, SPC, IR[0], Sf ? 0x7e : 0x7a, "bus_error");
    }
  }
  void init() {
    size_t romsz;
    size_t cartsz;

    auto rom = loadrom("tos104us.img", romsz);
    auto cart = loadrom("JOUMPCRT.STC", cartsz);
    //auto cart = loadrom("SWCRT4.STC", cartsz);
    //auto cart = loadrom("MISCCART.STC", cartsz);
    printf("got rom: %d %d\n", romsz, cartsz);

    if (cart != NULL) {
      register_handler(0xfa0000, 0xfaffff,0x1ffff, bememio, cart + 4, _RD, "CART");
    }
    register_handler(0xfc0000, 0xff3fff, 0x03ffff, bememio, rom, _RD, "ROM");
    register_handler(0x000000, 0x000007, 0xffffff, bememio, rom, _RD, "OVERLAY");
    register_handler(0x000008, 0x0003ff, 0xffffff, bememio, ram, _RW|_DBG, "RAM|dbg");
    register_handler(0x000400, 0x07FFFF, 0xffffff, bememio, ram, _RW, "RAM");
    
    register_handler(0xff8200, 0xff82ff, 0x0000ff, bememio, shifter::regs,_RW,"SHIFTER");
    register_handler(0xfffa00, 0xfffaff, 0x0000ff, mfpio,   &mfp, _RW, "MFP");
    register_handler(0xfffc00, 0xfffc02, 0x00000f, kbdio,   &kbd, _RW, "KBD");
    register_handler(0xfffc20, 0xfffcff, 0x0000ff, rtcio,   NULL, _RW, "RTC");

    scr = new Screen(640, 200, 30, 50, 0, NULL);
    scr->xs = 2;
    scr->ys = 2;
    scr->init(1);

    if (dcfg) {
      dumpcfg(0xfc0000, 0xfc0000, romsz);
    }
    
    Sf = 1;
    PC = 0xfc0000;
    int dv = 0;
    for(;;) {
      // hack WAITVBL
      if (PC == 0xfc0030) {
	visited[PC]++;
	if (visited[PC] > 1) {
	  exit(0);
	}
      }
#if 0
      if (PC == 0xfc0de8) {
	if (shifter::vPos == shifter::vBlank) {
	  PC = 0xffc0df8;
	}
      }
#endif
      if (PC == 0xFC91CE) {
	dumpvec(0, 0x3fc/4);
      }
      cpu_step();
      for (int i = 0; i < 5; i++)
	ppu_tick();
    }
  };
  void ppu_tick() {
    mfp.tick();
    auto pend = mfp.pending();
    if (pend >= 0) {
      cpu_irq(6);
    }
    if (PC == 0x0fc0de6 && !shifter::hPos && !shifter::vPos) {
      PC = 0xfc0df8;
    }
    if (shifter::tick()) {
      printf("================= frame %d\n", frame);
      // end-of-frame
      for (auto kp : keytbl) {
	if (scr->key(kp.key, true)) {
	  printf("keydown: '%c'\n", kp.key);
	  kbd.push(kp.code);
	  kbd.push(kp.code | 0x80);
	}
      }
      for (int i = 0; i < 16; i++) {
	scr->scrrect(5 + i * 16, 210, 15, 5, shifter::palette(i));
      }
      scr->scrtext(0, 220, WHITE, "frame: %d vbase:%.6x res:%.2x tmr:%.2x.%.2x.%.2x",
		   shifter::frame,
		   shifter::vbase(),
		   shifter::regs[RESOLUTION],
		   mfp.regs[TACR],
		   mfp.regs[TBCR],
		   mfp.regs[CDCR]);
      scr->scrtext(0, 228, WHITE, "PC:%.8x mfp:%.4x %.4x %.4x %.4x",
		   PC, mfp.ier(), mfp.ipr(), mfp.isr(), mfp.imr());
      scr->draw();
      scr->clear();
    }
    else if (shifter::de) {
      // visible
      int pixels[640] = { 0 };
      shifter::renderline(pixels);
      drawline(scr, pixels, 640, 5, vPos-shifter::y0+5, 0);
    }
  }
  int cpu_step() {
    uint16_t op;

    SPC = PC;
    op = cpu_fetch(Word, "step");
    return decode_68k(op);
  };
};
atarist st{};

uint8_t cpu_read8(const uint32_t addr, int mode) {
  iodata_t io{};

  st.read(addr, io);
  return io;
}

uint16_t cpu_read16(const uint32_t addr, int mode) {
  iodata_t io;

  st.read(addr, io, _SZ16);
  return io;
}

uint32_t cpu_read32(const uint32_t addr, int mode) {
  iodata_t io;

  st.read(addr, io, _SZ32);
  return io;
}

void cpu_write8(const uint32_t addr, const uint8_t val, int mode) {
  st.write(addr, val);
};

void cpu_write16(const uint32_t addr, const uint16_t val, int mode) {
  st.write(addr, val, _SZ16);
}

void cpu_write32(const uint32_t addr, const uint32_t val, int mode) {
  st.write(addr, val, _SZ32);
}

int main(int argc, char *argv[]) {
  dcfg = (argc > 2);
  setbuf(stdout, NULL);
  gentbl();
  st.init();
};
