//https://zsmith.co/intel.php
//https://c9x.me/x86/
//https://wiki.osdev.org/VGA_Hardware#Read.2FWrite_logic
//http://www.minuszerodegrees.net/misc/PC%20System%20Programming%20-%20Chap%2010.pdf  pg528
//https://pdos.csail.mit.edu/6.828/2018/readings/hardware/vgadoc/PCJR.TXT
#define CPU_HZ 1193181

#include <stdarg.h>
#include <stdio.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <sys/time.h>
#include <sys/mman.h>
#include <inttypes.h>
#include <pthread.h>
#include "dstk.h"
#include "cpu.h"
#include "util.h"
#include "gr.h"
#include "bus.h"
#include "8086_dev.h"
#include <map>

int zprintf(const char *fmt, ...);
#include "x86_audio.h"

#define cpu_readv cpu_read16

int zprintf(const char *fmt, ...)
{
  static pthread_mutex_t ptt = PTHREAD_MUTEX_INITIALIZER;
  va_list ap;

  va_start(ap,fmt);
  pthread_mutex_lock(&ptt);
  vprintf(fmt, ap);
  pthread_mutex_unlock(&ptt);
  return 0;
}

typedef uint32_t arg_t;

Screen *scr;

extern void (*sdl_keyhook)(int vk, int vmod, int state);

bool     x86_irq(const bool test, int irq);
static void     x86_setpc(const bool cond, const uint16_t ncs, const uint16_t npc);
static uint32_t x86_setfl(uint32_t vv, uint32_t sz);

static void     x86_set(const arg_t& arg, uint32_t v, bool setflags = false);
static uint32_t x86_get(const arg_t& arg);

typedef int (*icb_t)(void *);
void register_irq(int n, icb_t fn, void *arg);

static void div8(arg_t q, arg_t r, uint32_t a, uint32_t b, uint32_t m);
static void idiv8(arg_t q, arg_t r, int32_t a, int32_t b, int32_t m);

uint8_t kx = 0xff;

int frame;
int SPC;
int trace=0;
uint64_t cycs;

bool crtc_changed;
bool gr_changed;
bool seq_changed;

uint32_t lastop;
uint32_t extracyc;

Timer framectr;
void audio_tick();

static bus_t mb(0xFFFFF);
static bus_t io(0xFFFF);

/* Screen border */
#define EX 8
#define EY 8

/* all the opcode arguments use a common uint32_t format:
 * +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
 * |      type             |      size             |                   value                       |
 * +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
 *
 * +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
 * |        |    op     |  seg   |  ss |   index   |    reg    |   baserm  |pfx  |ad16 |           |
 * +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
*/
enum {
  TYPE_SHIFT  = 24,
  TYPE_MASK   = (0xFF << TYPE_SHIFT),
  TYPE_EA     = ('E' << TYPE_SHIFT),    // Reg/Mem:  MRR.rrr
  TYPE_EAMEM  = ('M' << TYPE_SHIFT),    // Memory:   MRR.rrr
  TYPE_EAREG  = ('G' << TYPE_SHIFT),    // Register: MRR.ggg
  TYPE_IMM    = ('I' << TYPE_SHIFT),    // Immediate
  TYPE_JMP    = ('J' << TYPE_SHIFT),    // Relative jump
  TYPE_OFFSET = ('O' << TYPE_SHIFT),    // Absolute memory offset
  TYPE_ESDI   = ('Y' << TYPE_SHIFT),    // es:di
  TYPE_DSSI   = ('X' << TYPE_SHIFT),    // ds:si
  TYPE_REG    = ('r' << TYPE_SHIFT),    // Register: specific
  TYPE_EMBREG = ('g' << TYPE_SHIFT),    // Register: part of opcode
  
  SIZE_SHIFT  = 16,
  SIZE_MASK   = (0xFF << SIZE_SHIFT),
  SIZE_BYTE   = ('b' << SIZE_SHIFT),    // 8-bit
  SIZE_WORD   = ('w' << SIZE_SHIFT),    // 16-bit
  SIZE_DWORD  = ('d' << SIZE_SHIFT),    // 32-bit
  SIZE_QWORD  = ('q' << SIZE_SHIFT),    // 64-bit
  SIZE_VWORD  = ('v' << SIZE_SHIFT),    // operand size (16/32-bit)
  SIZE_SEGREG = ('s' << SIZE_SHIFT),    // segment register (16-bit)
  SIZE_PTR    = ('p' << SIZE_SHIFT),    // ptr16:16 or ptr16:32
  SIZE_EMB    = ('i' << SIZE_SHIFT),    // embedded value (VAL_MASK)

  VAL_MASK    = 0xFFFF,

  /* ggg part of opcode  : --ggg--- */
  gb = TYPE_EMBREG+SIZE_BYTE,
  gv = TYPE_EMBREG+SIZE_VWORD,

  /* ggg part of mrr byte: mmgggrrr */
  Gb = TYPE_EAREG+SIZE_BYTE,
  Gv = TYPE_EAREG+SIZE_VWORD,
  Sw = TYPE_EAREG+SIZE_SEGREG,

  /* rrr part of mrr byte: mmgggrrr reg or mem */
  Eb = TYPE_EA+SIZE_BYTE,
  Ew = TYPE_EA+SIZE_WORD,
  Ev = TYPE_EA+SIZE_VWORD,

  /* rrr part of mrr byte, mmgggrrr, mem only */
  Mb = TYPE_EAMEM+SIZE_BYTE,
  Mw = TYPE_EAMEM+SIZE_WORD,
  Mv = TYPE_EAMEM+SIZE_VWORD,
  Mp = TYPE_EAMEM+SIZE_PTR,

  /* Direct memory offset [xxxx] */
  Ob = TYPE_OFFSET+SIZE_BYTE,
  Ov = TYPE_OFFSET+SIZE_VWORD,

  /* Immediate values */
  Ib = TYPE_IMM+SIZE_BYTE,
  Sb = TYPE_IMM+SIZE_BYTE+1,
  Iw = TYPE_IMM+SIZE_WORD,
  Iv = TYPE_IMM+SIZE_VWORD,
  i1 = TYPE_IMM+SIZE_EMB+1,
  i3 = TYPE_IMM+SIZE_EMB+3,
  Ap = TYPE_IMM+SIZE_PTR,
  
  /* Relative-jump */
  Jb = TYPE_JMP+SIZE_BYTE,
  Jw = TYPE_JMP+SIZE_WORD,
  Jv = TYPE_JMP+SIZE_VWORD,

  /* String operations ES:DI, DS:SI */
  Yb = TYPE_ESDI+SIZE_BYTE,
  Yw = TYPE_ESDI+SIZE_WORD,
  Yv = TYPE_ESDI+SIZE_VWORD,
  Xb = TYPE_DSSI+SIZE_BYTE,
  Xw = TYPE_DSSI+SIZE_WORD,
  Xv = TYPE_DSSI+SIZE_VWORD,
  
  /* Register ordering */
  rAL = TYPE_REG+SIZE_BYTE,
  rCL,
  rDL,
  rBL,
  rAH,
  rCH,
  rDH,
  rBH,

  rAX = TYPE_REG+SIZE_WORD,
  rCX,
  rDX,
  rBX,
  rSP,
  rBP,
  rSI,
  rDI,
  r8w,
  r9w,
  r10w,
  r11w,
  r12w,
  r13w,
  r14w,
  r15w,
  rDAX,

  rvAX = TYPE_REG+SIZE_VWORD,
  rvCX,
  rvDX,
  rvBX,
  rvSP,
  rvBP,
  rvSI,
  rvDI,
  rv8,
  rv9,
  rv10,
  rv11,
  rv12,
  rv13,
  rv14,
  rv15,
  rvDAX,

  rES = TYPE_REG+SIZE_SEGREG,
  rCS,
  rSS,
  rDS,
  rFS,
  rGS,

  IDT_BASE = 0x0,
};

const uint32_t zmask[] = {
  0x000000FF,
  0x0000FFFF,
  0xFFFFFFFF,
};
const uint32_t nmask[] = {
  0x00000080,
  0x00008000,
  0x80000000,
};

/* keep track of instruction state */
struct instr_t {
  uint32_t osize    = SIZE_WORD;
  uint32_t asize    = SIZE_WORD;
  uint32_t segpfx   = 0;
  uint32_t flag     = 0;
  uint32_t mrr_base = 0;
  uint8_t  mrr      = 0;

  /* opcode and arguments */
  uint8_t  op;
  uint32_t opfn;
  uint32_t a0;
  uint32_t a1;
  uint32_t a2;

  /* disassembly */
  const char *mnem;
};

instr_t i;
uint32_t& osize = i.osize;
uint8_t&  mrr = i.mrr;
uint32_t  mrr_imm;
uint32_t& mrr_base = i.mrr_base;
uint32_t  mrr_seg  = 0;
uint32_t& segpfx = i.segpfx;
uint32_t& pfx = i.flag;

constexpr uint32_t vsize(uint32_t arg, const uint32_t mask = -1) {
  /* Convert vsize to current opize */
  if ((arg & SIZE_MASK) == SIZE_VWORD)
    arg ^= (SIZE_VWORD ^ osize);
  return arg & mask;
}

constexpr int szb(const uint32_t sz) {
  switch(vsize(sz, SIZE_MASK)) {
  case SIZE_BYTE: return 0;
  case SIZE_WORD: return 1;
  case SIZE_DWORD:return 2;
  }
  return 0;
};

/*=====================================*
 * CPU Registers 
 *=====================================*/
union reg_t {
  uint8_t  b[2];
  uint16_t w;
  uint32_t d;
  uint64_t q;
};

struct flags_t {
  bool Cf;
  bool Pf;
  bool Af;
  bool Zf;
  bool Sf;
  bool Df;
  bool Of;
};

static reg_t     regs[8];
static uint16_t  sregs[8];

/* pointers to 8-bit register values */
static uint8_t * bregs[] = {
  &regs[0].b[0], &regs[1].b[0], &regs[2].b[0], &regs[3].b[0],
  &regs[0].b[1], &regs[1].b[1], &regs[2].b[1], &regs[3].b[1],
};

static uint16_t  PC;
static uint16_t  eflags = 0;

static uint16_t& AX=regs[0].w;
static uint16_t& CX=regs[1].w;
static uint16_t& DX=regs[2].w;
static uint16_t& BX=regs[3].w;
static uint16_t& SP=regs[4].w;
static uint16_t& BP=regs[5].w;
static uint16_t& SI=regs[6].w;
static uint16_t& DI=regs[7].w;

static uint8_t&  AL=regs[0].b[0];
static uint8_t&  CL=regs[1].b[0];
static uint8_t&  DL=regs[2].b[0];
static uint8_t&  BL=regs[3].b[0];
static uint8_t&  AH=regs[0].b[1];
static uint8_t&  CH=regs[1].b[1];
static uint8_t&  DH=regs[2].b[1];
static uint8_t&  BH=regs[3].b[1];

static uint16_t& ES=sregs[0];
static uint16_t& CS=sregs[1];
static uint16_t& SS=sregs[2];
static uint16_t& DS=sregs[3];

/* CPU Flags 
 * ----odit.sz-a-p-c
 */
template <const uint16_t mask> struct cpuflag {
  operator bool() const {
    return eflags & mask;
  };
  cpuflag& operator=(const bool test) {
    eflags = test ? (eflags | mask) : (eflags & ~mask);
    return *this;
  };
};

static cpuflag<0x001> Cf;
static cpuflag<0x004> Pf;
static cpuflag<0x010> Af;
static cpuflag<0x040> Zf;
static cpuflag<0x080> Sf;
static cpuflag<0x100> Tf;
static cpuflag<0x200> If;
static cpuflag<0x400> Df;
static cpuflag<0x800> Of;

// ----.xxxx.xx0x.0x1x
// ----.odit.sz-a.-p-c
#define FSET (0x0002)
#define FCLR (0xF000 | 0x0020 | 0x0008)

void cpu_setflags(uint32_t f)
{
  eflags = (f | FSET) & ~FCLR;
}

uint32_t cpu_getflags()
{
  return (eflags | FSET) & ~FCLR;
}

const char *cpu_flagstr() {
  static char fs[32];
  snprintf(fs, sizeof(fs), "%.4x:%c%c%c%c%c%c-%c-%c-%c",
	   eflags,
	   "-o"[!!Of],
	   "-d"[!!Df],
	   "-i"[!!If],
	   "-t"[!!Tf],
	   "-s"[!!Sf],
	   "-z"[!!Zf],
	   "-a"[!!Af],
	   "-p"[!!Pf],
	   "-c"[!!Cf]);
  return fs;
}

/* Return register name */
const char *regname(const uint32_t k)
{
  const char *bn[] = { "al", "cl", "dl", "bl", "ah", "ch", "dh", "bh" };
  const char *wn[] = { "ax", "cx", "dx", "bx", "sp", "bp", "si", "di" };
  const char *dn[] = { "eax","ecx","edx","ebx","esp","ebp","esi","edi"};
  const char *sn[] = { "es", "cs", "ss", "ds", "??", "??", "??", "??" };
  const int vv = (k & 7);
  
  switch (vsize(k, TYPE_MASK|SIZE_MASK)) {
  case TYPE_REG+SIZE_BYTE:
    return bn[vv];
  case TYPE_REG+SIZE_WORD:
    return wn[vv];
  case TYPE_REG+SIZE_SEGREG:
    return sn[vv];
  }
  return "xx";
}

/* Return memory offset for a given segment base and offset */
uint32_t segbase(int seg, uint16_t off, int spfx = 0)
{
  /* Check for segment override */
  if (spfx) {
    seg = spfx;
  }
  mrr_seg = sregs[seg & 7] << 4;
  return mrr_seg + off;
}

#define CSIP segbase(rCS, PC)

void cpu_reset(uint32_t addr)
{
}

void x86_reset(const uint32_t ncs, const uint32_t npc)
{
  CS = ncs;
  PC = npc;
  zprintf("CSIP: %.4x:%.4x\n", CS, PC);
}

/*============================================================*
 * Common cpu read/write
 *============================================================*/
uint8_t cpu_read8(const uint32_t addr, int type)
{
  iodata_t val = 0;
  mb.read(addr, val);
  return val;
}

void cpu_write8(const uint32_t addr, uint8_t val, int type)
{
  mb.write(addr, val);
}

uint16_t cpu_pop16()
{
  auto sp = postinc(SP, 2);
  
  return cpu_read16(segbase(rSS, sp), dstk::STACK);
}

void cpu_push16(uint16_t v)
{
  auto sp = predec(SP, 2);

  return cpu_write16(segbase(rSS, sp), v, dstk::STACK);
}

static uint8_t cpu_fetch8()
{
  uint32_t addr = segbase(rCS, PC++);
  assert(addr >= 0x400);
  return cpu_read8(addr, dstk::CODE);
}

static uint16_t cpu_fetch16()
{
  uint16_t lo, hi;

  lo = cpu_fetch8();
  hi = cpu_fetch8();
  return (hi << 8) + lo;
}

/* CPU I/O Handler */
void io_write(uint16_t addr, uint32_t val, int sz)
{
  sz &= SIZE_MASK;
  if (sz == SIZE_BYTE) {
    io.write(addr, val);
  }
  else {
    io.write(addr, val);
    io.write(addr+1, val >> 8);
  }
}

uint32_t io_read(uint16_t addr, int sz)
{
  iodata_t val, tmp;

  sz &= SIZE_MASK;
  if (sz == SIZE_BYTE) {
    io.read(addr, val);
  }
  else {
    io.read(addr, val);
    io.read(addr+1, tmp);
    val += (tmp << 8);
  }
  return val;
}

struct x86 {
  void init();

  uint8_t *vid;
  uint8_t *ram;
  uint8_t *rom;
  uint8_t *font;

  /* Cursor x, y, page */
  int cx;
  int cy;
  int cp;

  uint8_t *vidbase(uint32_t offset) {
    if (vga_page_valid >= 3) {
      return &ram[vga_page + offset];
    }
    if (offset < 0xa0000)
      offset += 0x18000;
    return &vid[offset];
  }
  
  uint8_t *vidptr(uint32_t addr) {
    return &vid[addr - 0xa0000];
  };
  
  int vidmode = 0;
  int scr_cols = 40;
  int scr_rows = 25;
  int scr_pal = 0;
  
  /* RTC:
   * 1 = millisec/10
   * 2 = sec
   * 3 = min
   * 4 = hour
   * 5 = dow
   * 6 = day
   * 7 = month
   * 8 = year % 100
   */
  void drawscreen();
  void show_grregs();
  
  uint8_t mga_idx;
  uint8_t mga_regs[256];

  uint8_t attr_flop, attr_idx, attr_regs[32];
  uint8_t crtc_idx, crtc_regs[256];
  uint8_t seq_idx,  seq_regs[16];
  uint8_t gr_idx,   gr_regs[16];
  uint8_t latch[4];
  
  uint8_t crtc_modesel;
  uint8_t crtc_colorsel;
  uint8_t crtc_status;
  uint16_t vga_status;
  uint32_t vga_page = 0;
  int vga_page_valid = 0;
  
  int crtc_mode() {
    return (crtc_modesel & 2);
  }
  void crtc_txtres(int &nc, int &nr) {
    /* Get text screen resolution */
    nc = (crtc_modesel & 0x01) ? 80 : 40;
    nr = 25;
  };
  void crtc_grres(int &nc, int &nr, int& bpp) {
    /* Get graphics mode resolution */
    if (crtc_modesel & 0x10) {
      nc = 640;
      nr = 200;
      bpp = 1;
    }
    else {
      nc = 320;
      nr = 200;
      bpp = 2;
    }
  };

  /* i8259 IRQ stuff */
  uint16_t irq_mask;
  uint16_t irq_pending;

  uart_t  *com1;
  uart_t  *com2;
  lpt_t   *lpt1;
  irqx_t  *i8259;
  dma_t   *i8237;
  i8255_t *i8255;
  i8253_t *i8253;
  
  uint16_t  pal_index;
  palclr    pal_clrs[258];

  void vidram(const uint32_t addr, int mode, iodata_t& val);
};

x86 cpu;

void cpu_showregs() {
  zprintf("=== %.4x:%.4x | ", CS, SPC);
  zprintf("ax:%.4x bx:%.4x cx:%.4x dx:%.4x sp:%.4x bp:%.4x si:%.4x di:%.4x | ",
	  AX, BX, CX, DX, SP, BP, SI, DI);
  zprintf("ss:%.4x ds:%.4x es:%.4x | ",
	  SS, DS, ES);
  zprintf("%s | ", cpu_flagstr());
}

bool fontgb(uint8_t bits, int idx) { return ((bits << idx) & 0x80) != 0; }

struct keymap {
  int ch;
  int code;
} kmap[] = {
  { Key::K_F1,     0x3B },
  { Key::K_F2,     0x3C },
  { Key::K_F3,     0x3D },
  { Key::K_F4,     0x3E },
  { Key::K_F5,     0x3F },
  { Key::K_F6,     0x40 },
  { Key::K_F7,     0x41 },
  { Key::K_F8,     0x42 },
  { Key::K_F9,     0x43 },
  { Key::K_F10,    0x44 },
  { Key::K_LEFT,   0x4B },
  { Key::K_RIGHT,  0x4D },
  { Key::K_UP,     0x48 },
  { Key::K_DOWN,   0x50 },
  { Key::K_LSHIFT, 0x2a },
  { Key::K_RSHIFT, 0x36 }, // shift (fire)
  { Key::K_LCTRL,  0x1d }, // ctrl (left)
  { Key::K_WINDOWS,0x38 }, // alt (right)
  { Key::K_ENTER,  0x1c },
  { Key::K_DEL,    0x0e },
  { '1',           0x02 },
  { '2',           0x03 },
  { '3',           0x04 },
  { '4',           0x05 },
  { '5',           0x06 },
  { '6',           0x07 },
  { '7',           0x08 },
  { '8',           0x09 },
  { '9',           0x0a },
  { '0',           0x0B },
  { '-',           0x0c },
  { '=',           0x0d },
  { 'q',           0x10 },
  { 'w',           0x11 },
  { 'e',           0x12 },
  { 'r',           0x13 },
  { 't',           0x14 },
  { 'y',           0x15 },
  { 'u',           0x16 },
  { 'i',           0x17 },
  { 'o',           0x18 },
  { 'p',           0x19 },
  { '[',           0x1a },
  { ']',           0x1b },
  { 'a',           0x1e },
  { 's',           0x1f },
  { 'd',           0x20 },
  { 'f',           0x21 },
  { 'g',           0x22 },
  { 'h',           0x23 },
  { 'j',           0x24 },
  { 'k',           0x25 },
  { 'l',           0x26 },
  { ';',           0x27 },
  { '\'',          0x28 },
  { '\\',          0x2b },
  { '/',           0x35 },
  { 'z',           0x2c },
  { 'x',           0x2d },
  { 'c',           0x2e },
  { 'v',           0x2f },
  { 'b',           0x30 },
  { 'n',           0x31 },
  { 'm',           0x32 },
  { ',',           0x33 },
  { '.',           0x34 },
  { ' ',           0x39 },
  { -1, -1 },
};

/*====================================*
 * Video Modes
 * 00 40x25    T   bw
 * 01 40x25    T   16
 * 02 80x25    T   bw
 * 03 80x25    T   16
 * 04 320x200  G   4  2-interlace b800,ba00
 * 05 320x200  G   4  2-interlace b800,ba00
 * 06 640x200  G   2  2-interlace b800,ba00
 * 07 720x400  T   bw
 * 08 160x200  G   16 2-interlace b800,ba00
 * 09 320x200  G   16 4-interlace b800,ba00,bc00,be00
 * 0a
 * 0b
 * 0c
 * 0d 320x200 G   16  a000 4-plane
 * 0e 640x200 G   16  a000
 * 0f 640x350 G   bw  a000
 * 10 640x350 G   16  a000 4-plane
 * 11 640x480 G   2   a000
 * 12 640x480 G   16  a000 4-plane
 * 13 320x200 G   256 a000
 *====================================*/
enum {
  TEXT,

  /* Bits per pixel mode */
  PXL_BPP    = 0x0010,
  PXL_1BPP   = PXL_BPP+1,
  PXL_2BPP   = PXL_BPP+2,
  PXL_4BPP   = PXL_BPP+4,
  PXL_8BPP   = PXL_BPP+8,

  /* Plane pixels */
  PXL_PLANE  = 0x0020,
  PXL_PLANE1 = PXL_PLANE+1,
  PXL_PLANE4 = PXL_PLANE+4,

  ILACE2 = 0x0200,
  ILACE4 = 0x0400,
};

struct vmode {
  int w, h;
  int nclr;
  int mode;
  int pal;

  int bpp() {
    assert(mode & PXL_BPP);
    return mode & 0xF;
  };
  int nplanes() {
    assert(mode & PXL_PLANE);
    return mode & 0xF;
  };
};

static vmode vmodes[] = {
  { 40,   25,  2,  TEXT },     // b800 40x25 grey/composite
  { 40,   25,  16, TEXT },     // b800 40x25 color
  { 80,   25,  2,  TEXT },     // b800 80x25 grey/composite
  { 80,   25,  16, TEXT },     // b800 80x25 color

  { 320, 200,  4,  PXL_2BPP }, // 320x200x4 ilace2 [green/red/brown] or [cyan/magenta/gray]
  { 320, 200,  4,  PXL_2BPP }, // 320x200x4 ilace2 [cyan/red/gray]
  { 640, 200,  2,  PXL_1BPP},  // 640x200x2 ilace2
  { 80,   25,  2,  TEXT },

  // 8,9,a = pcjr/tandy modes
  { 160, 200, 16,  PXL_4BPP }, // ilace2 
  { 320, 200, 16,  PXL_4BPP }, // ilace4
  { 640, 200,  4,  PXL_4BPP }, // ilace4
  { },

  { },
  { 320, 200, 16,  PXL_PLANE4 }, // a000 320x200x16
  { 640, 200, 16,  PXL_PLANE4 }, // a000 640x200x16
  { 640, 350,  2,  PXL_PLANE1 }, // a000 640x350x1

  { 640, 350, 16,  PXL_PLANE4 }, // a000 640x350x16
  { 640, 480,  2,  PXL_1BPP },   // a000 640x480x2
  { 640, 480, 16,  PXL_PLANE4 }, // a000 640x480x16
  { 320, 200, 256, PXL_8BPP },   // a000 320x200x256
};

static palclr egapal[64];
palclr cgapal[256] = {
  { 0x00, 0x00, 0x00 }, // black
  { 0x00, 0x00, 0xaa }, // blue
  { 0x00, 0xaa, 0x00 }, // green
  { 0x00, 0xaa, 0xaa }, // cyan
  { 0xaa, 0x00, 0x00 }, // red
  { 0xaa, 0x00, 0xaa }, // magenta
  { 0xaa, 0x55, 0x00 }, // brown
  { 0xaa, 0xaa, 0xaa }, // lightgray
  { 0x55, 0x55, 0x55 }, // dkgray
  { 0x55, 0x55, 0xff }, // ltblue
  { 0x55, 0xff, 0x55 }, // ltgreen
  { 0x55, 0xff, 0xff }, // ltcyan
  { 0xff, 0x55, 0x55 }, // ltred
  { 0xff, 0x55, 0xff }, // ltmagenta
  { 0xff, 0xff, 0x55 }, // yellow
  { 0xff, 0xff, 0xff }, // white

  // 640x200 composite
#if 0
  { 0, 0, 0 },     // black
  { 0, 108, 108 }, // cyan
  { 18, 5, 149 },  // blue
  { 0,189, 253 },  // ltcyan
  { 170,76,0 },
  { 16,160,63 },
  { 185,162,173 },
  { 150,240,255 },
  { 198,0,34 },
  { 167,205,255 },
  { 220,117,255 },
  { 185,195,255 },
  { 206,45,0 },
  { 237,255,204 },
  { 255,178,166 },
  { 255,255,255 },
#else
  { 0x00, 0x00, 0x00 }, // black
  { 0x00, 0x6e, 0x31 }, // dkgreen
  { 0x31, 0x99, 0xff }, // blue
  { 0x00, 0x8a, 0xff }, // ltblue
  { 0xa7, 0x00, 0x31 }, // red
  { 0x76, 0x76, 0x76 }, // dkgray
  { 0xec, 0x11, 0xff }, // magenta
  { 0xbb, 0x92, 0xff }, // pink 
  { 0x31, 0xea, 0x00 }, // dkdkgreen
  { 0x00, 0xdb, 0x00 }, // ltgreen
  { 0x76, 0x76, 0x76 }, // dkgray
  { 0x45, 0xf7, 0xba }, // ltltgreen
  { 0xec, 0x63, 0x00 }, // orange
  { 0xbb, 0xe4, 0x00 }, // yellowgreen
  { 0xf7, 0x7f, 0xdb }, // pink
  { 0xff, 0xff, 0xff }, // white
#endif
  
  /* CGA exta palette - apple 2 colors */
  { 0x00, 0x00, 0x00 }, // black
  { 0xcc, 0x00, 0x33 },
  { 0x00, 0x00, 0x99 },
  { 0xcc, 0x33, 0xcc },
  { 0x00, 0x66, 0x33 },
  { 0x66, 0x66, 0x66 },
  { 0x33, 0x33, 0xff },
  { 0x66, 0x99, 0xff },
  { 0x99, 0x66, 0x00 },
  { 0xff, 0x66, 0x00 },
  { 0x99, 0x99, 0x99 },
  { 0xff, 0x99, 0x99 },
  { 0x00, 0xcc, 0x00 },
  { 0xff, 0xff, 0x00 },
  { 0x33, 0xff, 0x99 },
  { 0xff, 0xff, 0xff },
};

vmode *vidcfg = &vmodes[0];

int cgamap[256][4] = {
  { 0, 2, 4, 6  }, // mode 4, 1
  { 0, 10,12,14 },
  { 0, 3, 5, 7  }, // mode 4, 2
  { 0, 11,13,15 },
  { 0, 3, 4, 7  }, // mode 5
  { 0, 11,12,15 },
  { 0, 15 }, // mode 6

  { 0,1,1,3},
  { 4,5,6,7},
  { 8,9,10,11},
  { 12,13,14,15},
};

void printtxt(int x, int y, int attr, const char *str, uint8_t *mem, int stride)
{
  mem = &mem[(y * stride) + x*2];
  
  while (*str) {
    *mem++ = *str++;
    *mem++ = attr;
  }
}

/* Draw text screen
 *   int cols     : number of screen columns (40, 80)
 *   int rows     : number of screen rows
 *   uint8_t *mem : Pointer to video memory
 *   uint8_t *font: Pointer to font pattern (8x8)
 *
 * +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
 * |             char              |B/I| R   G   B |f/i| r   g   b | bytes_per_row = 2 * cols
 * +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
 * 
 * BI = Blink (8 background colors)
 *   0x03D8.3 = BLINK
 * fi = 2nd font set (8 foreground colors)
 *
 * Mode   TxtRes  Char  Pixels
 *  7     80x25   9x16  720x350
 *  6     80x25   8x8   640x200
 * 0,1    40x25   8x8   320x200
 *  2     80x25   8x8   640x200
 * 2,3    80x25   9x16  720x400
 * 10h    80x25   8x14  640x350
 * 11h    80x30   8x16  640x480
 */

#define BLINK_MASK  0x08
void drawtext(int cols, int rows, int border, uint32_t flags, const uint8_t *mem,
	      uint8_t *font, int fontw = 8, int fonth = 8)
{
  uint8_t ch, attr, fg, bg;

  /* Set border color */
  scr->scrrect(0, 0, scr->width + 2*EX, scr->height + 2*EY, border);
  for (int y = 0; y < rows; y++) {
    for (int x = 0; x < cols; x++) {
      ch =   *mem++;
      attr = *mem++;

      /* Get FG/BG colors. Set FG to BG if blinking on */
      fg = (attr & 0xF);
      bg = (attr >> 4);
      if ((bg & flags & BLINK_MASK) != 0) {
	fg = bg;
      }
      scr->drawglyph(font + (ch * fonth), fonth,
		     EX + x * fontw, EY + y * fonth, fg, bg,
		     fontgb);
    }
  }
}

/*
 *              +---+---+---+---+---+---+---+---+
 * 320x200x256  |               0               | bytes_per_row = (width / 1) = 320
 *              +---+---+---+---+---+---+---+---+
 * 320x200x16   |       0       |       1       | bytes_per_row = (width / 2) = 160
 *              +---+---+---+---+---+---+---+---+
 * 320x200x4    |   0   |   1   |   2   |   3   | bytes_per_row = (width / 4) = 80
 *              +---+---+---+---+---+---+---+---+
 * 640x200x2    | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | bytes_per_row = (width / 8) = 80
 *              +---+---+---+---+---+---+---+---+
 */

/*======================
 * bitplanes
 *       +----------+
 *     +----------+ |
 *   +----------+ | |
 * +----------+ | |-+
 * |          | |-+
 * |          |-+
 * +----------+
 *======================*/
void drawplane(int width, int height, uint8_t *mem, int (*pmap)(int p), int nplanes, int planestep)
{
  int pxl, clr;

  if (width == 320)
    scr->xs = 4;
  else
    scr->xs = 2;
  for (int y = 0; y < height; y++) {
    for (int x = 0; x < width; x += 8) {
      for (int j = 0; j < 8; j++) {
	clr = 0;
	for (int p = 0; p < nplanes; p++) {
	  pxl = mem[p * planestep];
	  clr |= (pxl & (0x80 >> j)) ? (1 << p) : 0x00;
	}
	scr->setpixel(EX + x + j, EY + y, pmap(clr));
      }
      mem++;
    }
  }
}

void drawgraphics(int width, int height, const uint8_t *mem, int (*pmap)(int p), int bpp, int y0, int ystep)
{
#if 1
  if (width == 160)
    scr->xs = 8;
  else if (width == 320)
    scr->xs = 4;
  else
    scr->xs = 2;
#endif
  int line[width];
  for (int y = y0; y < height; y += ystep) {
    /* Grab bitmap */
    mem = genbpp(line, width, mem, bpp);

    /* Map colors */
    for (int x = 0; x < width; x++) {
      line[x] = pmap(line[x]);
    }

    /* Draw bitmap */
    drawline(scr, line, width, EX, EY + y, 0);
  }
}

void x86::show_grregs()
{
  const char *wm[] = { "set", "and", "or", "xor" };
  zprintf(" gc:Set/Reset:          %x\n", gr_regs[0] & 0xf);
  zprintf(" gc:Enable Set/Reset:   %x\n", gr_regs[1] & 0xf);
  zprintf(" gc:Color Compare:      %x\n", gr_regs[2] & 0xf);
  zprintf(" gc:Rotate:             %3s,%d\n", wm[(gr_regs[3] >> 3) & 3], gr_regs[3] & 7);
  zprintf(" gc:Read Map Select:    %x\n", gr_regs[4] & 3);
  zprintf(" gc:Write Mode:         %x\n", gr_regs[5] & 3);
  zprintf(" gc:Read Mode:          %x\n", !!(gr_regs[5] & 0x8));
  zprintf(" gc:OE,Reg,256:         %d,%d,%d\n", testbit(gr_regs[5], 4), testbit(gr_regs[5], 5), testbit(gr_regs[5], 6));
  zprintf(" gc:Bitmask:            %.2x\n", gr_regs[8]);
}

/* PC JR palette 0..15 */
static int jrclr(int pxl) {
  return (pxl & 0xF);
};

/* Composite color map */
static int cmppal(int pxl) {
  return ((pxl & 0xF) + 16);
}

static int _egamap[16] = {
  0, 1, 2, 3, 4, 5, 20, 7,
  56,57,58,59,60,61,62,63
};

static int egaclr(int pxl) {
  return _egamap[pxl & 0xF];
}

static int vgaclr(int pxl) {
  return pxl;
}

static int cgaclr(int pxl) {
  return cgamap[cpu.scr_pal][pxl & 0x3];
}

void x86::drawscreen()
{
  static time_t fpstime = time(NULL);
  time_t now = time(NULL);
  float fps = (float)frame / (now - fpstime);
  int w = 0, h = 0, bpp = 0;
  static int blink;

  crtc_changed = false;
  if (crtc_changed) {
    zprintf(" gr:Mode Select:          %x\n", crtc_modesel);
    switch(crtc_modesel & 0b10111) {
    case 0b00100: zprintf("40x25 bw\n"); break;
    case 0b00000: zprintf("40x25 clr\n"); break;
    case 0b10001: zprintf("80x25 clr\n"); break;
    case 0b00010: zprintf("160x200x16,320x200x4\n"); break;
    case 0b00110: zprintf("320x200xbw\n"); break;
    case 0b00011: zprintf("320x200x16\n"); break;
    case 0b10010: zprintf("640x200x2\n"); break;
    case 0b10011: zprintf("640x200x4\n"); break;
    }
    zprintf(" gr:Horiz Total:          %d\n", crtc_regs[0]);
    zprintf(" gr:Horiz Displayed:      %d\n", crtc_regs[1]);
    zprintf(" gr:Horiz Sync position:  %d\n", crtc_regs[2]);
    zprintf(" gr:Horiz/Vert sync width:%d,%d\n", crtc_regs[3] & 0xf, crtc_regs[3] >> 4);
    zprintf(" gr:Vertical Total:       %d\n", crtc_regs[4]);
    zprintf(" gr:Vertical total adjust:%d\n", crtc_regs[5]);
    zprintf(" gr:vertical displayed:   %d\n", crtc_regs[6]);
    zprintf(" gr:vertical sync pos:    %d\n", crtc_regs[7]);
    zprintf(" gr:interlace/skew        %d\n", crtc_regs[8]);
    zprintf(" gr:max raster:           %d\n", crtc_regs[9]);
    zprintf(" gr:cursor start/end:     %.2x,%.2x\n", crtc_regs[10], crtc_regs[11]);
    zprintf(" gr:display address:      %.2x.%.2x\n", crtc_regs[12], crtc_regs[13]);
    zprintf(" gr:cursor address:       %.2x,%.2x\n", crtc_regs[14], crtc_regs[15]);
    crtc_changed = 0;
  }
  
  blink++;
  w = vidcfg->w;
  h = vidcfg->h;
  if (vidmode == 0x09) {
    /* Tandy/PCjr 320x200x16 - draw 4 interlaced sections [sq -t] */
    bpp = vidcfg->bpp();
    drawgraphics(w, h, vidbase(0x0000), jrclr, bpp, 0, 4);
    drawgraphics(w, h, vidbase(0x2000), jrclr, bpp, 1, 4);
    drawgraphics(w, h, vidbase(0x4000), jrclr, bpp, 2, 4);
    drawgraphics(w, h, vidbase(0x6000), jrclr, bpp, 3, 4);
  }
  else if (vidmode == 0x08) {
    /* Tandy/PCjr 160x200x16 - draw 4 interlaced sections */
    bpp = vidcfg->bpp();
    // default video address is in RAM
    drawgraphics(w, h, vidbase(0x0000), jrclr, bpp, 0, 2);
    drawgraphics(w, h, vidbase(0x2000), jrclr, bpp, 1, 2);
  }
  else if (vidmode == 0x13) {
    /* VGA 320x200x256 */
    bpp = vidcfg->bpp();
    drawgraphics(320, 200, vidptr(0xA0000), vgaclr, bpp, 0, 1);
  }
  else if (vidmode == 0x0d) {
    /* EGA 320x200x16 [sq -e] */
    drawplane(w, h, vidptr(0xA0000), egaclr, 4, 65536);
  }
  else if (vidmode == 0x10) {
    /* EGA 640x350x16 */
    scr->xs = 2;
    scr->ys = 2;
    drawplane(w, h, vidptr(0xA0000), egaclr, 4, 65536);
  }
  else if (crtc_mode()) {
    /* CGA modes */
    crtc_grres(w, h, bpp);
    if (vidmode == 4) {
      scr_pal = (crtc_colorsel & 0x20) ? 2 : 0;
    }
    else if (vidmode == 6) {
      /* 640x200x1 : colorsel sets FG */
      scr_pal = 6;
      w = 640;
      h = 200;
      bpp = 1;
      cgamap[scr_pal][1] = crtc_colorsel & 7;
    }
    else {
      /* Other modes: colorsel sets BG */
      cgamap[scr_pal][0] = crtc_colorsel & 7;
    }
    /* Draw interlaced screen */
    drawgraphics(w, h, vidptr(0xB8000), cgaclr, bpp, 0, 2); // even lines
    drawgraphics(w, h, vidptr(0xBA000), cgaclr, bpp, 1, 2); // odd lines
  }
  else {
    uint32_t flag = 0;

    /* draw text screen */
    crtc_txtres(w, h);
    if (blink % 40 > 20)
      flag |= BLINK_MASK;

    drawtext(w, h, crtc_colorsel & 0xF, flag, vidptr(0xB8000), font);
  }

  /* Scan keypress */
  if (scr->key('k', false)) {
    int fd = open("x86.mem", O_CREAT | O_TRUNC | O_WRONLY);
    write(fd, ram, 0xc0000);
    close(fd);
  }
  for (int i = 0; kmap[i].ch >= 0; i++) {
    if (scr->key(kmap[i].ch, true)) {
      zprintf("gotkey: %c %.2x\n", kmap[i].ch, kmap[i].code);
      kx = kmap[i].code;
      x86_irq(true, 9);
      break;
    }
    else if (kx == kmap[i].code) {
      zprintf("relkey: %c %.2x\n", kmap[i].ch, kmap[i].code);
      kx = 0x80;
      x86_irq(true, 9);
    }
  }
  zprintf("-- frame: %d %ld\n", frame, cycs);
  scr->scrtext(0, scr->height+2, 255, "frm:%d fps:%.2f", frame, fps);
  scr->scrtext(0, scr->height+10,255, "vm:%.2x %.4x %.4x\n", vidmode, crtc_modesel, crtc_status);

  int cx = 0, cy = scr->height+18;
  for (int x = 0; x < 256; x++) {
    scr->scrrect(cx, cy, 7, 7, x);
    cx += 8;
    if (cx+8 >= scr->width) {
      cx = 0;
      cy += 8;
    }
  }
  scr->draw();
  frame++;
}

/* DMA I/O
   00: DMA Channel 0 address
   01: DMA Channel 0 counter
   02: DMA Channel 1 address
   03: DMA Channel 1 counter
   04: DMA Channel 2 address
   05: DMA Channel 2 counter
   06: DMA Channel 3 address
   07: DMA Channel 3 counter
   08: DMA Command
   0a: single channel mask
   0b: mode
   0d: master reset
   0f: multi channel mask
   80: set page 0
   81: set page 1
   82: set page 2
   83: set page 3
*/
int i8237_t::io(void *arg, const uint32_t addr, int mode, iodata_t& val)
{
  i8237_t *d = (i8237_t *)arg;
  int ch;

  ch = (addr / 2);
  if (addr <= 7) {
    if (addr & 1) {
      dmachan[ch].count &= ~(0xFF << d->flipflop);
      dmachan[ch].count |=   (val << d->flipflop);
      zprintf("DMA channel %d counter: %.8x\n",
	      ch, dmachan[ch].count);
      d->flipflop ^= 8;
    }
    else {
      dmachan[ch].src &= ~(0xFF << d->flipflop);
      dmachan[ch].src |=   (val << d->flipflop);
      zprintf("DMA channel %d address: %.8x\n",
	      ch, dmachan[ch].src);
      d->flipflop ^= 8;
    }
  }
  else if (addr >= 0x80) {
    int chmap[] = { -1, 2, 3, 1, -1, -1, -1, 0, -1, 6, 7, 5, -1, -1, -1, -1 };
    
    ch = chmap[addr & 0xF];
    if (ch >= 0) {
      dmachan[ch].src &= 0xFFFF;
      dmachan[ch].src |= (val << 16);
      zprintf("DMA channel %d page: %.8x\n",
	      ch, dmachan[ch].src);
      hexdump(&cpu.ram[dmachan[ch].src], 256, 32);
    }
  }
  else if (addr == 0x0C) {
    d->flipflop = 0;
  }
  return 0;
}

// irq -> interrupt mapping
// 08-0F : IRQ 0-7
// 70-78 : IRQ 8-15
#define IRQ0 8

// 1193181 Hz / 65536 = 18.2065 Hz
void i8253_tick()
{
  pit_t *pit = cpu.i8253->pit;

  /* Tick all timers */
  for (int i = 0; i < 3; i++) {
    if (pit[i].tick()) {
      if (i == 0) {
	// Timer IRQ
	x86_irq(true, IRQ0);
      }
      if (i == 2 && pcspkr.enabled) {
	// Toggle Speaker on/off
	pcspkr.duty |= 2;
      }
    }
  }
}

int i8253_t::io(void *arg, const uint32_t addr, int mode, iodata_t& val)
{
  static uint8_t t = 0;

  /* 0x40 = channel 0 
   * 0x41 = channel 1
   * 0x42 = channel 2
   * 0x43 = mode/command
   *
   * ss.aa.ooo.b
   * ss=00 channel 0
   * ss=01 channel 1
   * ss=10 channel 2
   *
   * rl=00 latch count
   * rl=01 lobyte
   * rl=10 hibyte
   * rl=11 lohi
   *
   * ooo=000 interrupt on terminal
   * ooo=001 one-shot
   * ooo=010 rate generator
   * ooo=011 square wave
   * ooo=100 software strobe
   * ooo=101 hardware strobe
   * ooo=110 rate generator (010)
   * ooo=111 square wave (011)
   */
  t = t+11;
  memio(&t, 0, mode, val);
  switch(addr) {
  case 0x40: case 0x41: case 0x42:
    flogger(0, "pit:%c chan:%.2x %.2x\n", mode, addr, val);
    if (mode == 'w') {
      pit_t *px = &cpu.i8253->pit[addr & 3];
      if (px->flipflop == 0) {
	px->freq = (px->freq & 0xFF00) | val;
	px->flipflop ^= 1;
      }
      else {
	char sname[8];

	snprintf(sname, sizeof(sname), "PIT%d", (addr & 3));
	px->freq = (px->freq & 0x00FF) | (val << 8);
	px->settimer(px->freq == 0 ? 65536 : px->freq, 1, 1, sname);
	if ((addr & 3) == 0)
	  pcspkr.enabled = false;
	px->flipflop ^= 1;
      }
    }
    break;
  case 0x43:
    flogger(0, "pit:%c chan:%d access:%d mode:%d %s\n",
	    mode,
	    (val >> 6),
	    (val >> 4) & 3,
	    (val >> 1) & 7,
	    val & 1 ? "BCD" : "BIN");
    break;
  default:
    flogger(0, "pit:%c %.2x %.2x\n", mode, addr, val);
    break;
  }
  return 0;
}

/* Port 0x60 - 0x65 
 * 0.rd -> return scancode
 * 1.rd -> return portb (^ 0x10 occasionally)
 * 2.rd -> return sw2 hi or lo (portb 0x8)
 * 0.wr -> scancode = 0xaa
 * 1.wr -> speaker, . (portb = (portb & 0xEF) | (val & 0x10)
 */
int i8255_t::io(void *arg, const uint32_t addr, int mode, iodata_t& val) {
  memio(&kx, 0, mode, val);
  if (addr == 0x0061 && mode == 'w') {
    pcspkr.enabled = ((val & 3) == 3);
  }
  return 0;
};

/* i8259: PIT */
int i8259_t::io(void *arg, const uint32_t addr, int mode, iodata_t& val)
{
  i8259_t *c = (i8259_t *)arg;
  
  switch (addr) {
  case 0x20:
  case 0xA0:
    // command
    break;
  case 0x21:
    memio(&c->irq_mask, 0, mode, val);
    zprintf("IRQ: masklo: %.4x\n", c->irq_mask);
    break;
  case 0xA1:
    memio(&c->irq_mask, 1, mode, val);
    zprintf("IRQ: maskhi: %.4x\n", c->irq_mask);
    break;
  default:
    zprintf("IRQ EOI %.2x:%.2x\n", addr, val);
    break;
  }
  return 0;
}

int rtcio(void *arg, const uint32_t addr, int mode, iodata_t& val)
{
  return 0;
}

//#define WOODY
#ifdef WOODY
#include "woody/woodyopl.cpp"

struct adlib_t *adlib_new(uint16_t samplerate)
{
  OPLChipClass *opl = new OPLChipClass(0);
  opl->adlib_init(samplerate, true);
  opl->adlib_write(0x01, 1 << 5, 0);
  return (struct adlib_t *)(opl);
}

uint8_t adlib_read(struct adlib_t *adlib)
{
  OPLChipClass *opl = (OPLChipClass *)(adlib);
  return opl->status | 6;
}

void adlib_write(struct adlib_t *adlib, uint8_t addr, uint8_t data)
{
  OPLChipClass *opl = (OPLChipClass *)(adlib);
  opl->adlib_write(addr, data, 0);
}

void adlib_getsample(struct adlib_t *adlib, short *buf, size_t samples)
{
  OPLChipClass *opl = (OPLChipClass *)(adlib);
  opl->adlib_getsample(buf, samples);
}

void adlib_free(struct adlib_t *adlib)
{
  OPLChipClass *opl = (OPLChipClass *)(adlib);
  delete opl;
}

struct adlib_t *a;

struct opl2 : public soundgen_t {
  int idx;
  opl2(bus_t *io, int port);

  static int opl2io(void *arg, const uint32_t addr, int mode, iodata_t& val);
};

int opl2::opl2io(void *arg, const uint32_t addr, int mode, iodata_t& val)
{
  static int idx;
  
  if (mode == 'w') {
    if (addr == 0x388)
      idx = val;
    else
      adlib_write(a, idx, val);
  }
  else {
    if (addr == 0x388) {
      val = adlib_read(a);
      printf("OPL2 read status: %.2x\n", val);
    }
  }
  return 0;
}

opl2::opl2(bus_t *io, int port)
{
  a = adlib_new(44100);
  io->register_handler(port, port+1, 0xFFFF, opl2::opl2io, this, _RW, "OPL2");
}

#endif

/*==========================
 * Video Ports
 *
 * 03B4: Mono CRTC Index
 * 03B5: Mono CRTC Data
 * 03C0: Attribute Register Index
 * 03C4: Sequencer Register Index [00..04]
 * 03C5: Sequencer Register Data
 * 03C8: DAC ADDRESS WRITE
 * 03C9: DAC DATA
 * 03CE: Graphics Register Index [00..08]
 * 03CF: Graphics Register Data
 * 03D4: CRTC INDEX [00..18]
 * 03D5: CRTC DATA
 * 03D8: 
 * 03D9:
 * 03DA: PCJr VGA
 * 03DE:
 *
 * Graphics registers
 * +---+---+---+---+---+---+---+---+
 * | 7 | 6 | 5 | 4 | 3 | 2 | 1 | 0 |
 * +---+---+---+---+---+---+---+---+
 * |               |   Set/Reset    | 0
 * +---+---+---+---+---+---+---+---+
 * |               |Enable SetReset| 1
 * +---+---+---+---+---+---+---+---+
 * |               |  ColorCompare | 2
 * +---+---+---+---+---+---+---+---+
 * |           | LogOp |   Rotate  | 3
 * +---+---+---+---+---+---+---+---+
 * |                       |MapSel | 4
 * +---+---+---+---+---+---+---+---+
 * |   |256|reg|O/E|rmd|   |wrmode | 5
 * +---+---+---+---+---+---+---+---+
 * |               |mmsel  |chn|afa| 6
 * +---+---+---+---+---+---+---+---+
 * |               |ColorDontCare  | 7
 * +---+---+---+---+---+---+---+---+
 * |         bitmask               | 8
 * +---+---+---+---+---+---+---+---+
 *
 * Sequencer Registers
 * +---+---+---+---+---+---+---+---+
 * +---+---+---+---+---+---+---+---+
 * +---+---+---+---+---+---+---+---+
 * +---+---+---+---+---+---+---+---+
 * +---+---+---+---+---+---+---+---+
 * +---+---+---+---+---+---+---+---+
 *
 */

/* enable set/reset bit:
 *   0 = color comes from data byte
 *   1 = color comes from set/reset
 */
static int logic(int fn, int pxl, int latch)
{
  switch(fn) {
  case 0: return pxl;
  case 1: return pxl & latch;
  case 2: return pxl | latch;
  case 3: return pxl ^ latch;
  }
  return 0;
}

static int vrot(int pxl)
{
  int vr = cpu.gr_regs[3] & 7;
  return (pxl >> vr) | (pxl << (8 - vr));
}

static void setreset(uint32_t offset, uint8_t *latch, uint8_t data)
{
  int wrmode = cpu.gr_regs[5] & 3;
  int mask = cpu.gr_regs[8];
  int lfn  = (cpu.gr_regs[3] >> 3) & 3;
  int pxl, bank;

#if 0
  if (cpu.gr_regs[0x5] & 0x10) {
    printf("odd-even\n");
    bank = (offset & 1) * 65536;
    cpu.vid[bank + (offset >> 1)] = data;
    return;
  }
  if (cpu.seq_regs[0x4] & 0x8) {
    printf("chain4\n");
    bank = (offset & 3) * 65536;
    cpu.vid[bank + (offset >> 2)] = data;
    return;
  }
#endif
  printf("writemode: %x data:%x\n", wrmode, data);
  for (int n = 0; n < 4; n++) {
    int p = (1L << n);

    /* Check if sequencer map enabled */
    if ((cpu.seq_regs[2] & p) == 0)
      continue;
    switch (wrmode) {
    case 0x0:
      if (cpu.gr_regs[1] & p) {
	pxl = (cpu.gr_regs[0] & p) ? 0xFF : 0x00;
      }
      else {
	pxl = vrot(data);
      }
      pxl = logic(lfn, pxl, latch[n]);
      pxl = (pxl & mask) | (latch[n] & ~mask);
      cpu.vid[offset + n * 65535] = pxl;
      break;
    case 0x1:
      break;
    case 0x2:
      // data byte is used as color
      pxl = (data & p) ? 0xFF : 0x00;
      pxl = logic(lfn, pxl, latch[n]);
      pxl = (pxl & mask) | (latch[n] & ~mask);
      cpu.vid[offset + n * 65536] = pxl;
      break;
    case 0x3:
      break;
    }
  }
}

void x86::vidram(const uint32_t addr, int mode, iodata_t& val)
{
  uint32_t bank = 65536;
  int mapsel, rdmode, bitmask;

  if (vidmode == 0x0d) {
    mapsel = gr_regs[4] & 3;
    rdmode = (gr_regs[5] >> 3) & 1;
    bitmask = gr_regs[8];
    if (mode == 'r') {
      latch[0] = vid[addr + bank*0];
      latch[1] = vid[addr + bank*1];
      latch[2] = vid[addr + bank*2];
      latch[3] = vid[addr + bank*3];
      switch (rdmode) {
      case 0x00:
	val = latch[mapsel];
	return;
      }
      assert(0);
    }
    else if (mode == 'w') {
      setreset(addr, latch, val);
      return;
    }
  }
  memio(vid, addr, mode, val);
}

static int vidramio(void *arg, const uint32_t addr, int mode, iodata_t& val)
{
  cpu.vidram(addr, mode, val);
  return 0;
}

static int vidio(void *arg, const uint32_t addr, int mode, iodata_t& val)
{
  x86 *c = (x86 *)arg;
  static int retrace;

  printf("== %.4x:%.4x vidio: %c %.4x %.2x\n", CS, SPC, mode, addr, val);
  switch (addr) {
  case 0x3c0:
    if (c->attr_flop == 0) {
      memio(&c->attr_idx, 0, mode, val);
      c->attr_flop ^= 1;
    }
    else if (mode == 'w') {
      printf("Attribute Data: %.2x %.2x\n",  c->attr_idx, val);
      memio(c->attr_regs, c->attr_idx & 0x1F, mode, val);
      if (c->attr_idx <= 0xF) {
	_egamap[c->attr_idx] = val;
      }
      c->attr_flop ^= 1;
    }
    break;
  case 0x3c4:
    memio(&c->seq_idx, 0, mode, val);
    break;
  case 0x3c5:
    /* Sequencer register */
    printf("Sequencer Data: %.2x:%.2x\n", c->seq_idx, val);
    memio(&c->seq_regs, c->seq_idx & 0xF, mode, val);
    break;
  case 0x3ce:
    memio(&c->gr_idx, 0, mode, val);
    break;
  case 0x3cf:
    /* Graphics register */
    printf("==Graphics Data: %.2x:%.2x\n", c->gr_idx, val);
    memio(c->gr_regs, c->gr_idx & 0xF, mode, val);
    break;
  case 0x3d4:
    memio(&c->crtc_idx, 0, mode, val);
    break;
  case 0x3d5:
    /* CRTC register */
    memio(c->crtc_regs, c->crtc_idx, mode, val);
    if (mode == 'w') {
      crtc_changed = true;
      zprintf(" gr:==================== 3D5 %.2x < %.2x\n",
	      c->crtc_idx, val);
    }
    break;
  case 0x3d8:
    /* Mode control register */
    memio(&c->crtc_modesel, 0, mode, val);
    if (mode == 'w') {
      crtc_changed = true;
      zprintf(" gr:==================== 3D8 : %.2x\n", val);
      zprintf(" gr:width    : %d\n", (val & 1) ? 80 : 40);
      zprintf(" gr:mode     : %s\n", (val & 2) ? "graphics" : "text");
      zprintf(" gr:composite: %s\n", (val & 4) ? "color" : "b&w");
      zprintf(" gr:enable   : %d\n", !!(val & 8));
      zprintf(" gr:grmode   : %s\n", (val & 0x10) ? "640x200" : "320x200");
      zprintf(" gr:blink    : %d\n", !!(val & 0x20));
    }
    break;
  case 0x3d9:
    // color select register
    // --gg.IRGB
    memio(&c->crtc_colorsel, 0, mode, val);
    zprintf(" gr:colorsel: 0x3d9 %.2x\n", val);
    break;
  case 0x3da:
    if (mode == 'w') {
      zprintf(" gr:================ 3DA: %.2x\n", val);
      if ((c->vga_status & 0x100) == 0) {
	c->vga_status = 0x100 | val;
      }
      else {
	printf("SET PCJRVGA REG: %.4x %.2x\n", c->vga_status, val);
	if (c->vga_status >= 0x110 && c->vga_status <= 0x11f) {
	  // palette
	  int idx = c->vga_status & 0xF;
	  c->pal_clrs[idx] = cgapal[val & 0xF];
	  scr->setpalette(256, c->pal_clrs);
	}
	c->vga_status = 0;
      }
    }
    else {
      c->vga_status = 0;
    }
    if (retrace < 50) {
      c->crtc_status |= 0x88;
    }
    else {
      c->crtc_status &= ~0x88;
    }
    retrace = (retrace + 1) % 1000;
    if (mode == 'r') {
      val = c->crtc_status | 0x01; // | 0x01;
    }
    break;
  case 0x3c7: // w DAC Adddress Read Mode Register, r DAC State
    break;
  case 0x3c8: // rw DAC Address Write Mode register
    if (mode == 'w') {
      printf("write dac1: %.2x\n", val);
      c->pal_index = val * 3;
    }
    break;
  case 0x3c9: // rw DAC Data Register (palette color)
    if (mode == 'w') {
      int pi = c->pal_index / 3;
      printf("  write dac2: %.2x.%x: %.2x\n", pi, c->pal_index % 3, val);
      if (pi >= 255)
	break;
      switch (c->pal_index % 3) {
      case 0: c->pal_clrs[pi].r = val << 2; break;
      case 1: c->pal_clrs[pi].g = val << 2; break;
      case 2: c->pal_clrs[pi].b = val << 2; break;
      }
      c->pal_index++;
      
      c->pal_clrs[255].r = 0xff;
      c->pal_clrs[255].g = 0xff;
      c->pal_clrs[255].b = 0xff;
      scr->setpalette(256, c->pal_clrs);
    }
    break;
  case 0x3DF:
    // PCjr VGA page
    c->vga_page_valid |= 0x3;
    if (mode == 'w') {
      c->vga_page = 16 * 1024 * (val & 7);
      printf("WRITE vga: page: %.2x : %.5x %.5x mode:%x\n", val, c->vga_page, 16*1024*((val >> 3) & 7), c->vidmode);
    }
    break;
  default:
    zprintf("gr: unknown register: %.4x %.2x\n", addr, val);
    if (mode == 'r')
      val = 0xff;
    break;
  }
  return 0;
}

/* CGA Palette:
 *   0.lo blk,grn,red,brn
 *   0.hi blk,ltgn,ltrd,ylw
 *   1.lo blk,cyan,mag,ltgy
 *   1.hi blk,ltcy,ltmag,white
 *   2.hi blk,cyan,red,ltgy
 *   2.lo blk,ltcy,ltred,white
 */
enum {
  CGA_BLACK     = 0x000000,
  CGA_BLUE      = 0x0000AA,
  CGA_GREEN     = 0x00AA00,
  CGA_CYAN      = 0x00AAAA,
  CGA_RED       = 0xAA0000,
  CGA_MAGENTA   = 0xAA00AA,
  CGA_BROWN     = 0xAA5500,
  CGA_LTGRAY    = 0xAAAAAA,

  CGA_DKGRAY    = 0x555555,
  CGA_LTBLUE    = 0x5555FF,
  CGA_LTGREEN   = 0x55FF55,
  CGA_LTCYAN    = 0x55FFFF,
  CGA_LTRED     = 0xFF5555,
  CGA_LTMAGENTA = 0xFF55FF,
  CGA_YELLOW    = 0xFFFF55,
  CGA_WHITE     = 0xFFFFFF,
};

static int int21(void *arg)
{
  time_t now;
  struct tm *tm;
  
#if 0
  uint32_t base = segbase(rDS, DX);
  uint8_t *ptr = &cpu.ram[base];
  zprintf("\n=== INT21 %.4x\n", AX);
  zprintf("FCB: %.8s.%.3s seq:%.4x rand:%.8x len:%.4x tot:%.8x\n",
	  ptr+1, ptr+9, ptr[0x20], get32(ptr+0x21) & 0xFFFF, get16(ptr+0xe), get32(ptr+0x10));
  hexdump(ptr, 0x40, 32);
#endif
  if ((AX & 0xFF00) == 0x2A00) {
    time(&now);
    tm = localtime(&now);

    // get date: CX=year, DH=Mon, DL=Day
    CX = tm->tm_year + 1900;
    DX = ((tm->tm_mon + 1) << 8) | tm->tm_mday;

    AX = 0x2A03;
    return 1;
  }
  return 0;
}

static void m9_setpixel(int x, int y, int clr)
{
  uint8_t *vid = cpu.vidptr(0xa0000);
  
  if (cpu.vidmode == 0x13) {
    /* 320x200x256: 00000000 */
    int base = (y * 320) + x;
    uint8_t *mem = &vid[base];
    *mem = clr;
  }
  else if (cpu.vidmode == 4) {
    /* 320x200x4: 00112233 */
    int base = (y % 2) * 0x2000 + (y / 2) * 80 + (x / 4);
    uint8_t *mem = &vid[base + 0x18000];
    clr &= 3;
    switch  (x % 3) {
    case 0:
      *mem = (*mem & 0x3F) | (clr << 6);
      break;
    case 1:
      *mem = (*mem & 0xCF) | (clr << 4);
      break;
    case 2:
      *mem = (*mem & 0xF3) | (clr << 2);
      break;
    case 3:
      *mem = (*mem & 0xFC) | (clr << 0);
      break;
    }
  }
  else if (cpu.vidmode == 0x0d) {
    /* 320x200x16 EGA 4 planes */
    int base = ((y * 40) + (x / 8));
    int   xb = (0x80 >> (x & 7));
    for (int p = 0; p < 4; p++) {
      if (clr & (1L << p)) {
	vid[base + p * 65536] |= xb;
      }
      else {
	vid[base + p * 65536] &= ~xb;
      }
    }
  }
  else if (cpu.vidmode == 0x09) {
    /* 320x200x16: 00001111 */
    int base = (y % 4) * 0x2000 + (y / 4) * 160 + (x / 2);
    uint8_t *mem = cpu.vidbase(base);
    clr &= 0xF;
    if (x & 1) {
      *mem = (*mem & 0xF0) | clr;
    }
    else {
      *mem = (*mem & 0x0F) | (clr << 4);
    }
  }
  else if (cpu.vidmode == 0x08) {
    /* 160x200x16: 00001111 */
    int base = (y % 2) * 0x2000 + (y / 2) * 80 + (x / 2);
    uint8_t *mem = cpu.vidbase(base);
    clr &= 0xF;
    if (x & 1) {
      *mem = (*mem & 0xF0) | clr;
    }
    else {
      *mem = (*mem & 0x0F) | (clr << 4);
    }
  }
}

static int m9_getpixel(int x, int y)
{
  uint8_t *vid = cpu.vidptr(0xa0000);

  if (cpu.vidmode == 0x13) {
    /* 320x200x256 */
    int base = (y * 320) + x;
    return vid[base];
  }
  else if (cpu.vidmode == 0x04) {
    /* 320x200x4 2x interlaced */
    int base = ((y % 2) * 0x2000) + ((y / 2) * 160) + (x/4);
    uint8_t *mem = &vid[base + 0x18000];

    switch(x % 3) {
    case 0: return (*mem >> 6) & 0x3;
    case 1: return (*mem >> 4) & 0x3;
    case 2: return (*mem >> 2) & 0x3;
    case 3: return (*mem >> 0) & 0x3;
    }
  }
  else if (cpu.vidmode == 0x0d) {
    /* 320x200x16 4-planes */
    int base = ((y * 40) + (x / 8));
    int xb = (~x & 7);
    int pxl = 0;
    int nplanes = 4;
    
    for (int plane = 0; plane < nplanes; plane++) {
      pxl |= ((vid[base + (plane << 16L)] >> xb) & 1) << plane;
    }
    return pxl;
  }
  else if (cpu.vidmode == 0x09) {
    /* 320x200x16 4x interlaced TGA */
    int base = ((y % 4) * 0x2000) + ((y / 4) * 160) + (x/2);
    uint8_t *mem = cpu.vidbase(base);
    
    if (x & 1) {
      return *mem & 0xF;
    }
    return  *mem >> 4;
  }
  else if (cpu.vidmode == 0x08) {
    /* 160x200x16 2x interlaced TGA */
    int base = ((y % 2) * 0x2000) + ((y / 2) * 80) + (x/2);
    uint8_t *mem = cpu.vidbase(base);
    
    if (x & 1) {
      return *mem & 0xF;
    }
    return  *mem >> 4;
  }
  return 0;
}

static void rawglyph(int x, int y, int fg, int bg, const uint8_t *font)
{
  const int fw = 8;
  const int fh = 8;
  int clr, f;

  for (int i = 0; i < fh; i++) {
    f = *font++;
    for (int j = 0; j < fw; j++) {
#if 1
      clr = (f & 0x80) ? fg : bg;
      m9_setpixel(x*8+j, y*8+i, clr);
#else
      if (cpu.vidmode == 9) {
	clr = m9_getpixel(x*8+j, y*8+i);
	if (f & 0x80)
	  clr ^= fg;
	m9_setpixel(x*8+j, y*8+i, clr);
      }
      else {
	clr = (f & 0x80) ? fg : bg;
	m9_setpixel(x*8+j, y*8+i, clr);
      }
#endif
      f <<= 1;
    }
  }
}

static void pfont(uint8_t *bits)
{
  const char *ps = " *";;
  int f;

  return;
  zprintf("---- pfont\n");
  for (int i = 0; i < 8; i++) {
    f = *bits++;
    zprintf(" %c%c%c%c%c%c%c%c\n",
	    ps[((f >> 7) & 1)],
	    ps[((f >> 6) & 1)],
	    ps[((f >> 5) & 1)],
	    ps[((f >> 4) & 1)],
	    ps[((f >> 3) & 1)],
	    ps[((f >> 2) & 1)],
	    ps[((f >> 1) & 1)],
	    ps[((f >> 0) & 1)]);
  }
}

static int ttych(x86 *c, int x, int y, int ch, int n, int fg, int bg, bool istty, const char *lbl)
{
  uint8_t *font = c->font;
  if (ch & 0x80) {
    /* Use IRQ 0x1F as font select */
    uint32_t off = get16(&c->ram[0x1F*4 + 0]);
    uint32_t seg = get16(&c->ram[0x1F*4 + 2]);
    ch &= 0x7F;
    font = &c->ram[(seg << 4) + off];
    pfont(&font[ch * 8]);
  }
  if (c->vidmode == 0x09 || c->vidmode == 0x0d || c->vidmode == 0x08) {
    printf("mode:%.2x drawch: %.2x '%c' %.2x %.2x %s\n", c->vidmode, ch, ch, fg, bg, lbl);
    /* draw raw font */
    if (istty && ch == 0x08) {
      if (c->cx >= 0)
	c->cx--;
    } else if (istty && ch == 0xA) {
      c->cy++;
    }
    else if (istty && ch == 0x0d) {
      c->cx = 0;
    }
    else {
      rawglyph(x, y, fg, bg, &font[ch * 8]);
      c->ram[0x450 + (c->cp * 2)] = ++c->cx;
    }
    return 1;
  }
  return 0;
}

static int int10(void *arg)
{
  x86 *c = (x86 *)arg;
  static int w, h;

  zprintf("%.4x:%.4x int10: AX=%.4x mode=%.2x\n", CS, SPC, AX & 0xFF00, c->vidmode);
  switch (AX & 0xFF00) {
  case 0x0000: // set video mode
    c->vidmode = AX & 0x7F;
    if (c->vidmode <= 0x13) {
      vidcfg = &vmodes[c->vidmode];
    };
    if (c->vidmode == 0x13) {
      for (int i = 0; i < 254; i++) {
	c->pal_clrs[i] = cgapal[i & 0xF];
      }
      c->pal_clrs[255].r = 0xff;
      c->pal_clrs[255].g = 0xff;
      c->pal_clrs[255].b = 0xff;
      scr->setpalette(256, c->pal_clrs);
    }
    // pcjr for kingsquest
    //c->vga_page_valid = 3;
    if (c->vidmode == 0x09) {
      w = 80;
      h = 25;
      AX = 0x4;
      if (c->vga_page_valid) {
	c->vga_page = 0x18000;
      }
    }
    if (c->vidmode == 0x08) {
      w = 40;
      h = 25;
      AX = 0x04;
      if (c->vga_page_valid) {
	c->vga_page = 0x1c000;
      }
    }
    if (c->vidmode == 0x0d || c->vidmode == 0x10) {
      for (int i = 0; i < 64; i++) {
	c->pal_clrs[i] = egapal[i];
      }
      scr->setpalette(256, c->pal_clrs);
    }
    zprintf(" gr:Setting video mode: %x %x\n", c->vidmode, AX);
    break;
  case 0x0200: // set cursor pos, BH=page, DL=col, DH=row
    c->cp = BH; //(BX >> 8);
    c->cx = DL; //(DX & 0xFF);
    c->cy = DH; //(DX >> 8);
    //zprintf(" gr:Set cursor pos page%d: %d,%d\n", cp, cx, cy);
    break;
  case 0x0300: // get cursor pos, page=BH
    //zprintf(" gr:Get cursor pos\n");
    break;
  case 0x0900: // write char and attr
  case 0x0a00: // write char only
    {
      static char t0[32];
      snprintf(t0, sizeof(t0), "%.4x %.2x", AX & 0xFF00, BL); 
      if (ttych(c, c->cx, c->cy, AL, CX, BL & 0xF, 0, false, t0)) {
	return 1;
      }
    }
    break;
  case 0x0e00: // write char tty
    if (ttych(c, c->cx, c->cy, AL, 1, BL & 0xF, 0, true, "0e")) {
      return 1;
    }
    break;
  case 0x0600: // scroll up
  case 0x0700: // scroll down
    {
      /* CL,CH = upper left
       * DL,DH = lower right
       * AL = # of lines
       * BH = attribute
       */
      int lx = CL; //CX & 0xFF;
      int ly = CH; //(CX >> 8);
      int rx = DL; //DX & 0xFF;
      int ry = DH; //(DX >> 8);

      zprintf("Scroll: %d,%d - %d,%d %.2x %.4x\n", lx, ly, rx, ry, (BX >> 8), AX);
      if (c->vidmode == 0x09 || c->vidmode == 0x0d) { // || c->vidmode == 0x04 || c->vidmode == 0x13) {
	/* Really want to blitter here */
	for (int i = ly * 8; i < ry*8; i++) {
	  for (int j = lx * 8; j <= rx* 8 + 7; j++) {
	    m9_setpixel(j, i, m9_getpixel(j, i+8));
	  }
	}
	for (int i = 0; i < 8; i++) {
	  for (int j = lx * 8; j <= rx * 8+7; j++) {
	    m9_setpixel(j, ry*8 + i, BX >> 8);
	  }
	}
	return 1;
      }
    }
    break;
  case 0x0f00: // get video mode
    if (c->vidmode == 0x09) {
      AX = (80 << 8) | c->vidmode;
      return 1;
    }
    break;
  default:
    zprintf("int10: %x\n", AX & 0xFF00);
    break;
  }
  return 0;
}

static int kromio(void *arg, const uint32_t addr, int mode, iodata_t& val)
{
  return memio(arg, addr - 0xF0100, mode, val);
}

void x86::init()
{
  size_t romsz;
  uint8_t *r;
  int grey[] = { 0, 5, 8, 11, 14, 17, 20, 24, 28, 32, 36, 40, 45, 50, 56, 63 };

  /* Load CGA font and allocate memory */
  font = loadrom("CGA-TH.F08", romsz);
  ram = new uint8_t[2 * 1024 * 1024]{0};
  for (int i = 0; i < 16; i++) {
    cgapal[i+32].r = grey[i];
    cgapal[i+32].g = grey[i];
    cgapal[i+32].b = grey[i];
  }
  // setup egapalette
  // https://en.wikipedia.org/wiki/Enhanced_Graphics_Adapter#/media/File:EGA_Table.svg
  for (int i = 0; i < 64; i++) {
    egapal[i].b += !!(i & 0x01) * 0xaa;
    egapal[i].g += !!(i & 0x02) * 0xaa;
    egapal[i].r += !!(i & 0x04) * 0xaa;
    egapal[i].b += !!(i & 0x08) * 0x55;
    egapal[i].g += !!(i & 0x10) * 0x55;
    egapal[i].r += !!(i & 0x20) * 0x55;
  };
  seq_regs[2] = 0xF;
  vid = new uint8_t[512*1024]{0};
  mb.register_handler(0x00000, 0xFFFFF, 0xFFFFF, memio, ram, _RW, "RAM");
  mb.register_handler(0xA0000, 0xBFFFF, 0x1FFFF, vidramio, vid, _RW, "VIDEO");

#if 0
  /* testing */
  r = loadrom("pcbios/new_bios.rom", romsz);
  mb.register_handler(0xF0000, 0xFFFFF, 0xFFFF, memio, r, _RW, "F0100");
  x86_reset(0xF000, 0xFFF0);
#else
  r = loadrom("pcbios/bios.rom", romsz);
  mb.register_handler(0xF0100, 0xF0100+romsz-1, 0xFFFFF, kromio, r, _RW, "F0100");
  x86_reset(0xF000, 0x0100);
#endif
  /* Register ports */
  io.register_handler(0x0240, 0x0257, 0xFFFF, rtcio,   this, _RD, "RTC");
  io.register_handler(0x03B0, 0x03DF, 0xFFFF, vidio,   this, _RW, "VIDEO.REGS");

  /* Setup some devices */
  i8255 = new i8255_t(&io); // pic
  i8237 = new i8237_t(&io); // dma
  i8259 = new i8259_t(&io); // pic
  i8253 = new i8253_t(&io); // pit
  com1 = new uart_t(&io, 0x3F8, "COM1");
  com2 = new uart_t(&io, 0x2F8, "COM2");
  lpt1 = new lpt_t(&io, 0x378, "LPT1");

  /* setup sound */
  sound = new tandysnd(&io);
  //sound = new sblaster(0x220);
  //sound = new opl2(&io, 0x388);

  cgapal[255].r = 0xff;
  cgapal[255].g = 0xff;
  cgapal[255].b = 0xff;
  scr = new Screen(640+EX*2, 200+EY*2, 10, 80, 256, cgapal);
  scr->xs = 2;
  scr->ys = 3;
  scr->init();

  /* Override int10 and int21 */
  register_irq(0x10, int10, this);
  register_irq(0x21, int21, this);
  // update sectors per track...
}

/* Calculate parity */
static const uint8_t parity[256] = {
  1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 
  0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0, 
  0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0, 
  1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 
  0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0, 
  1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 
  1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 
  0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0, 
  0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0, 
  1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 
  1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 
  0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0, 
  1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 
  0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0, 
  0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0, 
  1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1,
};

/* Calculate sign/zero flag */
constexpr bool zf(const uint32_t val, const uint32_t sz) {
  return (zmask[szb(sz)] & val) == 0;
};

constexpr bool sf(const uint32_t val, const uint32_t sz) {
  return (nmask[szb(sz)] & val) != 0;
}

/* Set CPU flags */
static uint32_t x86_setfl(uint32_t vv, uint32_t sz)
{
  /* Calculate parity */
  Pf = parity[vv & 0xFF];
  Zf = zf(vv, sz);
  Sf = sf(vv, sz);
  return vv;
}

// Set Overflow register
// add: of = (dst ^ v1) & (dst ^ v2), af= (dst ^ a ^ b ) & 10 == 10
// adc: of = (dst ^ v1) & (dst ^ v2)
// sbb: of = (dst ^ v1) & (v1  ^ v2)
// sub: of = (dst ^ v1) & (v1  ^ v2)
// af = (a ^ b ^ c)
static uint32_t x86_setoc(const arg_t& arg, uint32_t res, uint32_t of, uint32_t af, bool setcf = true)
{
  x86_setfl(res, arg);
  Af = (af & 0x10) == 0x10;
  Of = sf(of, arg);
  
  switch (vsize(arg, SIZE_MASK)) {
  case SIZE_BYTE:
    if (setcf)
      Cf = (res & ~0xFF) != 0;
    break;
  case SIZE_WORD:
    if (setcf)
      Cf = (res & ~0xFFFF) != 0;
    break;
  default:
    assert(0);
  }
  x86_set(arg, res);
  return res;
}

/* Conditionally set new CS:IP */
static void x86_setpc(const bool cond, const uint16_t ncs, const uint16_t npc)
{
  if (cond) {
    CS = ncs;
    PC = npc;
  }
}

/*===============================*
 * Opcode handler
 *===============================*/
#define OF(dst,v1,v2) ((dst ^ v1) & (dst ^ v2))
#define AF(dst,v1,v2) ((dst ^ v1 ^ v2))

#define SUBOF OF(a, b, c)
#define STDOF OF(c, a, b)
#define STDAF AF(c, a, b)

/*===============================*
 * Opcode table
 *===============================*/

enum {
  MRRF = 0,

  FLAG_PFX = 0x01, 
  FLAG_MRR = 0x02,
  FLAG_GRP = 0x06,
  
  PFX  = FLAG_PFX,
  MRR  = FLAG_MRR,

  /* Flag if group */
  GRP  = 0xF000+MRR,
  GRP1 = 0x1000+MRR,
  GRP2 = 0x2000+MRR,
  GRP3 = 0x3000+MRR,
  GRP4 = 0x4000+MRR,
  GRP5 = 0x5000+MRR,
  GRPF = 0x7000+MRR,
  GRP0 = 0x8000+MRR,

  x86_segpfx = 0,
  x86_grp = 0,
  x86_osize = 0x66,
  x86_asize = 0x67,
  
  /* These map direct to opcodes */
  x86_daa = 0x27,
  x86_das = 0x2f,
  x86_aaa = 0x37,
  x86_aas = 0x3f,
  x86_pusha = 0x60,
  x86_popa = 0x61,
  x86_lea = 0x8d,
  x86_nop = 0x90,
  x86_cbw = 0x98,
  x86_wait = 0x9b,
  x86_pushf = 0x9c,
  x86_popf = 0x9d,
  x86_sahf = 0x9e,
  x86_lahf = 0x9f,
  x86_enter = 0xc8,
  x86_leave = 0xc9,
  x86_movsb = 0xa4,
  x86_movsw = 0xa5,
  x86_cmpsb = 0xa6,
  x86_cmpsw = 0xa7,
  x86_stosb = 0xaa,
  x86_stosw = 0xab,
  x86_lodsb = 0xac,
  x86_lodsw = 0xad,
  x86_scasb = 0xae,
  x86_scasw = 0xaf,
  x86_les = 0xc4,
  x86_lds = 0xc5,
  x86_lss = 0xfb2,
  x86_lfs = 0xfb4,
  x86_lgs = 0xfb5,
  x86_into = 0xce,
  x86_iret = 0xcf,
  x86_aam = 0xd4,
  x86_aad = 0xd5,
  x86_xlat = 0xd7,
  x86_loopnz = 0xe0,
  x86_loopz = 0xe1,
  x86_loop = 0xe2,
  x86_jcxz = 0xe3,
  x86_lock = 0xf0,
  x86_repnz = 0xf2,
  x86_repz = 0xf3,
  x86_hlt = 0xf4,
  x86_cmc = 0xf5,
  x86_clc = 0xf8,
  x86_stc = 0xf9,
  x86_cli = 0xfa,
  x86_sti = 0xfb,
  x86_cld = 0xfc,
  x86_std = 0xfd,
  x86_cwd = 0x99,

  x86_jo = 0x70,
  x86_jno = 0x71,
  x86_jb = 0x72,
  x86_jnb = 0x73,
  x86_jz = 0x74,
  x86_jnz = 0x75,
  x86_jbe = 0x76,
  x86_ja = 0x77,
  x86_js = 0x78,
  x86_jns = 0x79,
  x86_jpe = 0x7a,
  x86_jpo = 0x7b,
  x86_jl = 0x7c,
  x86_jge = 0x7d,
  x86_jle = 0x7e,
  x86_jg = 0x7f,

  // first instance
  x86_in = 0xe4,
  x86_out = 0xe5,
  x86_ins = 0x6c,
  x86_outs = 0x6e,
  x86_ret = 0xc2,
  x86_retf = 0xca,
  x86_mov = 0x88,
  x86_xchg = 0x86,
  x86_int = 0xcd,
  x86_imul3 = 0x69,
  x86_pop = 0x8f,
  
  // 386
  x86_bsf = MRRF + 0xfbc,
  x86_bsr = MRRF + 0xfbd,
  
  // SSE
  x86_addps = MRRF + 0x0f58,
  x86_addpd = MRRF + 0x660f58,
  x86_addss = MRRF + 0xf30f58,
  x86_addsd = MRRF + 0xf20f58,

  x86_andps = MRRF + 0x0f54,
  x86_andpd = MRRF + 0x660f54,
  x86_andnps = MRRF + 0x0f55,
  x86_andnpd = MRRF + 0x660f55,

  x86_xorps = MRRF + 0x0f57,
  x86_xorpd = MRRF + 0x660f57,

  x86_divps = MRRF + 0x0f5e,
  x86_divpd = MRRF + 0x660f5e,
  x86_divss = MRRF + 0xf30f5e,
  x86_divsd = MRRF + 0xf20f5e,
  
  /*====================================*
   * Group ocodes. Decoded by mrr.ggg
   *====================================*/
  /* 80, 81, 82, 83  */
  x86_add   = GRP1 + 0,
  x86_or    = GRP1 + 1,
  x86_adc   = GRP1 + 2,
  x86_sbb   = GRP1 + 3,
  x86_and   = GRP1 + 4,
  x86_sub   = GRP1 + 5,  
  x86_xor   = GRP1 + 6,
  x86_cmp   = GRP1 + 7,

  /* c0, c1, d0, d1, d2, d3 */
  x86_rol   = GRP2 + 0,
  x86_ror   = GRP2 + 1,
  x86_rcl   = GRP2 + 2,
  x86_rcr   = GRP2 + 3,
  x86_shl   = GRP2 + 4,
  x86_shr   = GRP2 + 5,
  x86_sal   = GRP2 + 6,
  x86_sar   = GRP2 + 7,

  /* f6, f7 */
  x86_test  = GRP3 + 0,
  x86_not   = GRP3 + 2,
  x86_neg   = GRP3 + 3,
  x86_mul   = GRP3 + 4,
  x86_imul  = GRP3 + 5,
  x86_div   = GRP3 + 6,
  x86_idiv  = GRP3 + 7, // rAX, Eb or rvDX:AX, Ev

  /* fe */
  x86_inc   = GRP4 + 0,
  x86_dec   = GRP4 + 1,

  /* ff */
  x86_inc5  = GRP5 + 0,
  x86_dec5  = GRP5 + 1,
  x86_call  = GRP5 + 2,
  x86_callf = GRP5 + 3,
  x86_jmp   = GRP5 + 4,
  x86_jmpf  = GRP5 + 5,
  x86_push  = GRP5 + 6,
};

struct opcode_t {
  const char *mnem;
  int opfn;
  int flag;
  uint32_t arg0;
  uint32_t arg1;
  uint32_t arg2;
};

const char *grp1[] = { "add", "or",  "adc", "sbb", "and", "sub", "xor", "cmp" };
const char *grp2[] = { "rol", "ror", "rcl", "rcr", "shl", "shr", "sal", "sar" };
const char *grp3[] = { "test","???", "not", "neg", "mul", "imul","div", "idiv"};
const char *grp4[] = { "inc", "dec", "???", "???", "???", "???", "???", "???" };
const char *grp5[] = { "inc", "dec", "call","callf","jmp", "jmpf","push","???" };

const char **Grps[] = {
  grp1, grp2, grp3, grp4, grp5
};

#define m(mf,b...) { .mnem=#mf, .opfn=x86_##mf, .flag=FLAG_MRR, b }
#define g(mf,b...) { .mnem=#mf, .opfn=mf,       .flag=FLAG_GRP, b }
#define p(mf,b...) { .mnem=#mf, .opfn=0,        .flag=FLAG_PFX, b }
#define _(mf,b...) { .mnem=#mf, .opfn=x86_##mf, .flag=0,        b }
#define _xxx { }

int opcnt[256];

static int cycles_8086[] = {
  [x86_aaa] = 8,
  [x86_aad] = 60,
  [x86_aam] = 83,
  [x86_aas] = 8,
  [x86_cbw] = 2,
  [x86_cwd] = 5,
  [x86_clc] = 2,
  [x86_cld] = 2,
  [x86_cli] = 2,
  [x86_cmc] = 2,
  [x86_daa] = 4,
  [x86_das] = 4,
  [x86_hlt] = 2,
  [x86_iret] = 44,
  [x86_lahf] = 4,
  [x86_lock] = 2,
  [x86_lodsb] = 16,
  [x86_lodsw] = 16,
  [x86_movsb] = 16,
  [x86_movsw] = 16,
  [x86_nop] = 3,
  [x86_popf] = 12,
  [x86_pushf] = 14,
  [x86_sahf] = 4,
  [x86_scasb] = 19,
  [x86_scasw] = 19,
  [x86_stc] = 2,
  [x86_std] = 2,
  [x86_sti] = 2,
  [x86_stosb] = 11,
  [x86_stosw] = 15,
  [x86_wait] = 4,
  [x86_xlat] = 11,
};

const bool isseg(int arg)
{
  switch (arg) {
  case rES: case rCS: case rSS: case rDS: case rFS: case rGS:
    return true;
  }
  return false;
}

int x86_cycles(int op, opcode_t *opc)
{
  int opfn = opc->opfn;
  int a0 = opc->arg0;
  
  if (cycles_8086[opfn] > 0)
    return cycles_8086[opfn];
  if (opfn == x86_push) {
    if (isseg(a0))
      return 14;
  }
  if (opfn == x86_inc) {
    if (a0 == gv)
      return 3;
  }
  if (opfn == x86_dec) {
    if (a0 == gv)
      return 3;
  }
  return 0;
}

/*         cmp    adc    add
 * reg,reg 3      3
 * reg,imm 4      4
 * mem,reg 13+ea  24+ea
 * reg,mem 13+ea  13+ea
 * mem,imm 13+ea  23+ea
 */
constexpr opcode_t mkop(int id, int opfn, uint32_t a0, uint32_t a1, uint32_t a2, int cyc,
			uint32_t flag, const char *dis)
{
  opcode_t o = { };

  assert(id <= 0xFF);
  o.opfn = opfn;
  o.arg0 = a0;
  o.arg1 = a1;
  o.arg2 = a2;
  o.flag = flag;
  o.mnem = dis;
  
  return o;
}

#define __ 0

/* Build up X86 opcode table */
constexpr opcode_t opix[] = {
  mkop(0x00, x86_add,    Eb,  Gb, __, __, MRR,   "add      %Eb, %Gb"),
  mkop(0x01, x86_add,    Ev,  Gv, __, __, MRR,   "add      %Ev, %Gv"),
  mkop(0x02, x86_add,    Gb,  Eb, __, __, MRR,   "add      %Gb, %Eb"),
  mkop(0x03, x86_add,    Gv,  Ev, __, __, MRR,   "add      %Gv, %Ev"),
  mkop(0x04, x86_add,    rAL, Ib, __,  4, __,    "add      al, %Ib"),
  mkop(0x05, x86_add,    rvAX,Iv, __,  4, __,    "add      %v0, %Iv"),
  mkop(0x06, x86_push,   rES, __, __, 14, __,    "push     es"),
  mkop(0x07, x86_pop,    rES, __, __, 12, __,    "pop      es"),
  mkop(0x08, x86_or,     Eb,  Gb, __, __, MRR,   "or       %Eb, %Gb"),
  mkop(0x09, x86_or,     Ev,  Gv, __, __, MRR,   "or       %Ev, %Gv"),
  mkop(0x0a, x86_or,     Gb,  Eb, __, __, MRR,   "or       %Gb, %Eb"),
  mkop(0x0b, x86_or,     Gv,  Ev, __, __, MRR,   "or       %Gv, %Ev"),
  mkop(0x0c, x86_or,     rAL, Ib, __,  4, __,    "or       al, %Ib"),
  mkop(0x0d, x86_or,     rvAX,Iv, __,  4, __,    "or       %v0, %Iv"),
  mkop(0x0e, x86_push,   rCS, __, __, 14, __,    "push     cs"),
  mkop(0x0f, x86_pop,    rCS, __, __, 12, __,    "pop      cs"),

  mkop(0x10, x86_adc,    Eb,  Gb, __, __, MRR,   "adc      %Eb, %Gb"),
  mkop(0x11, x86_adc,    Ev,  Gv, __, __, MRR,   "adc      %Ev, %Gv"),
  mkop(0x12, x86_adc,    Gb,  Eb, __, __, MRR,   "adc      %Gb, %Eb"),
  mkop(0x13, x86_adc,    Gv,  Ev, __, __, MRR,   "adc      %Gv, %Ev"),
  mkop(0x14, x86_adc,    rAL, Ib, __,  4, __,    "adc      al, %Ib"),
  mkop(0x15, x86_adc,    rvAX,Iv, __,  4, __,    "adc      %v0, %Iv"),
  mkop(0x16, x86_push,   rSS, __, __, 14, __,    "push     ss"),
  mkop(0x17, x86_pop,    rSS, __, __, 12, __,    "pop      ss"),
  mkop(0x18, x86_sbb,    Eb,  Gb, __, __, MRR,   "sbb      %Eb, %Gb"),
  mkop(0x19, x86_sbb,    Ev,  Gv, __, __, MRR,   "sbb      %Ev, %Gv"),
  mkop(0x1a, x86_sbb,    Gb,  Eb, __, __, MRR,   "sbb      %Gb, %Eb"),
  mkop(0x1b, x86_sbb,    Gv,  Ev, __, __, MRR,   "sbb      %Gv, %Ev"),
  mkop(0x1c, x86_sbb,    rAL, Ib, __,  4, __,    "sbb      al, %Ib"),
  mkop(0x1d, x86_sbb,    rvAX,Iv, __,  4, __,    "sbb      %v0, %Iv"),
  mkop(0x1e, x86_push,   rDS, __, __, 14, __,    "push     ds"),
  mkop(0x1f, x86_pop,    rDS, __, __, 12, __,    "pop      ds"),

  mkop(0x20, x86_and,    Eb,  Gb, __, __, MRR,   "and      %Eb, %Gb"),
  mkop(0x21, x86_and,    Ev,  Gv, __, __, MRR,   "and      %Ev, %Gv"),
  mkop(0x22, x86_and,    Gb,  Eb, __, __, MRR,   "and      %Gb, %Eb"),
  mkop(0x23, x86_and,    Gv,  Ev, __, __, MRR,   "and      %Gv, %Ev"),
  mkop(0x24, x86_and,    rAL, Ib, __,  4, __,    "and      al, %Ib"),
  mkop(0x25, x86_and,    rvAX,Iv, __,  4, __,    "and      %v0, %Iv"),
  mkop(0x26, x86_segpfx, rES, __, __, __, PFX,   "<es>"),
  mkop(0x27, x86_daa,    __,  __, __, __, __,    "daa"),
  mkop(0x28, x86_sub,    Eb,  Gb, __, __, MRR,   "sub      %Eb, %Gb"),
  mkop(0x29, x86_sub,    Ev,  Gv, __, __, MRR,   "sub      %Ev, %Gv"),
  mkop(0x2a, x86_sub,    Gb,  Eb, __, __, MRR,   "sub      %Gb, %Eb"),
  mkop(0x2b, x86_sub,    Gv,  Ev, __, __, MRR,   "sub      %Gv, %Ev"),
  mkop(0x2c, x86_sub,    rAL, Ib, __,  4, __,    "sub      al, %Ib"),
  mkop(0x2d, x86_sub,    rvAX,Iv, __,  4, __,    "sub      %v0, %Iv"),
  mkop(0x2e, x86_segpfx, rCS, __, __, __, PFX,   "<cs>"),
  mkop(0x2f, x86_das,    __,  __, __, __, __,    "das"),

  mkop(0x30, x86_xor,    Eb,  Gb, __, __, MRR,   "xor      %Eb, %Gb"),
  mkop(0x31, x86_xor,    Ev,  Gv, __, __, MRR,   "xor      %Ev, %Gv"),
  mkop(0x32, x86_xor,    Gb,  Eb, __, __, MRR,   "xor      %Gb, %Eb"),
  mkop(0x33, x86_xor,    Gv,  Ev, __, __, MRR,   "xor      %Gv, %Ev"),
  mkop(0x34, x86_xor,    rAL, Ib, __,  4, __,    "xor      al, %Ib"),
  mkop(0x35, x86_xor,    rvAX,Iv, __,  4, __,    "xor      %v0, %Iv"),
  mkop(0x36, x86_segpfx, rSS, __, __, __, PFX,   "<ss>"),
  mkop(0x37, x86_aaa,    __,  __, __, __, __,    "aaa"),
  mkop(0x38, x86_cmp,    Eb,  Gb, __, __, MRR,   "cmp      %Eb, %Gb"),
  mkop(0x39, x86_cmp,    Ev,  Gv, __, __, MRR,   "cmp      %Ev, %Gv"),
  mkop(0x3a, x86_cmp,    Gb,  Eb, __, __, MRR,   "cmp      %Gb, %Eb"),
  mkop(0x3b, x86_cmp,    Gv,  Ev, __, __, MRR,   "cmp      %Gv, %Ev"),
  mkop(0x3c, x86_cmp,    rAL, Ib, __, __, __,    "cmp      al, %Ib"),
  mkop(0x3d, x86_cmp,    rvAX,Iv, __, __, __,    "cmp      %v0, %Iv"),
  mkop(0x3e, x86_segpfx, rDS, __, __, __, PFX,   "<ds>"),
  mkop(0x3f, x86_aas,    __,  __, __, __, __,    "aas"),

  mkop(0x40, x86_inc,    gv,  __, __,  3, __,    "inc      %gv"),
  mkop(0x41, x86_inc,    gv,  __, __,  3, __,    "inc      %gv"),
  mkop(0x42, x86_inc,    gv,  __, __,  3, __,    "inc      %gv"),
  mkop(0x43, x86_inc,    gv,  __, __,  3, __,    "inc      %gv"),
  mkop(0x44, x86_inc,    gv,  __, __,  3, __,    "inc      %gv"),
  mkop(0x45, x86_inc,    gv,  __, __,  3, __,    "inc      %gv"),
  mkop(0x46, x86_inc,    gv,  __, __,  3, __,    "inc      %gv"),
  mkop(0x47, x86_inc,    gv,  __, __,  3, __,    "inc      %gv"),
  mkop(0x48, x86_dec,    gv,  __, __,  3, __,    "dec      %gv"),
  mkop(0x49, x86_dec,    gv,  __, __,  3, __,    "dec      %gv"),
  mkop(0x4a, x86_dec,    gv,  __, __,  3, __,    "dec      %gv"),
  mkop(0x4b, x86_dec,    gv,  __, __,  3, __,    "dec      %gv"),
  mkop(0x4c, x86_dec,    gv,  __, __,  3, __,    "dec      %gv"),
  mkop(0x4d, x86_dec,    gv,  __, __,  3, __,    "dec      %gv"),
  mkop(0x4e, x86_dec,    gv,  __, __,  3, __,    "dec      %gv"),
  mkop(0x4f, x86_dec,    gv,  __, __,  3, __,    "dec      %gv"),

  mkop(0x50, x86_push,   gv,  __, __, 15, __,    "push     %gv"),
  mkop(0x51, x86_push,   gv,  __, __, 15, __,    "push     %gv"),
  mkop(0x52, x86_push,   gv,  __, __, 15, __,    "push     %gv"),
  mkop(0x53, x86_push,   gv,  __, __, 15, __,    "push     %gv"),
  mkop(0x54, x86_push,   gv,  __, __, 15, __,    "push     %gv"),
  mkop(0x55, x86_push,   gv,  __, __, 15, __,    "push     %gv"),
  mkop(0x56, x86_push,   gv,  __, __, 15, __,    "push     %gv"),
  mkop(0x57, x86_push,   gv,  __, __, 15, __,    "push     %gv"),
  mkop(0x58, x86_pop,    gv,  __, __, 12, __,    "pop      %gv"),
  mkop(0x59, x86_pop,    gv,  __, __, 12, __,    "pop      %gv"),
  mkop(0x5a, x86_pop,    gv,  __, __, 12, __,    "pop      %gv"),
  mkop(0x5b, x86_pop,    gv,  __, __, 12, __,    "pop      %gv"),
  mkop(0x5c, x86_pop,    gv,  __, __, 12, __,    "pop      %gv"),
  mkop(0x5d, x86_pop,    gv,  __, __, 12, __,    "pop      %gv"),
  mkop(0x5e, x86_pop,    gv,  __, __, 12, __,    "pop      %gv"),
  mkop(0x5f, x86_pop,    gv,  __, __, 12, __,    "pop      %gv"),

  mkop(0x60, x86_pusha,  __,  __, __, __, __,    "pusha"),
  mkop(0x61, x86_popa,   __,  __, __, __, __,    "popa"),
  mkop(0x62, __,         __,  __, __, __, __,    NULL),
  mkop(0x63, __,         __,  __, __, __, __,    NULL),
  mkop(0x64, x86_segpfx, rFS, __, __, __, PFX,   "<fs>"),
  mkop(0x65, x86_segpfx, rGS, __, __, __, PFX,   "<gs>"),
  mkop(0x66, x86_osize,  0x10,__, __, __, PFX,   "<osize>"),
  mkop(0x67, x86_asize,  0x20,__, __, __, PFX,   "<asize>"),
  mkop(0x68, x86_push,   Iv,  __, __, __, __,    "push     %Iv"),
  mkop(0x69, x86_imul3,  Gv,  Ev, Iv, __, MRR,   "imul     %Gv, %Ev, %Iv"),
  mkop(0x6a, x86_push,   Ib,  __, __, __, __,    "push     %Ib"),
  mkop(0x6b, x86_imul3,  Gv,  Ev, Ib, __, MRR,   "imul     %Gv, %Ev, %Ib"),
  mkop(0x6c, x86_ins,    Yb,  rDX,__, __, __,    "insb"),
  mkop(0x6d, x86_ins,    Yv,  rDX,__, __, __,    "insw"),
  mkop(0x6e, x86_outs,   rDX, Xb, __, __, __,    "outsb"),
  mkop(0x6e, x86_outs,   rDX, Xv, __, __, __,    "outsw"),

  mkop(0x70, x86_jo,     Jb,  __, __, __, __,    "jo       %Jb"),
  mkop(0x71, x86_jno,    Jb,  __, __, __, __,    "jno      %Jb"),
  mkop(0x72, x86_jb,     Jb,  __, __, __, __,    "jb       %Jb"),
  mkop(0x73, x86_jnb,    Jb,  __, __, __, __,    "jnb      %Jb"),
  mkop(0x74, x86_jz,     Jb,  __, __, __, __,    "jz       %Jb"),
  mkop(0x75, x86_jnz,    Jb,  __, __, __, __,    "jnz      %Jb"),
  mkop(0x76, x86_jbe,    Jb,  __, __, __, __,    "jbe      %Jb"),
  mkop(0x77, x86_ja,     Jb,  __, __, __, __,    "ja       %Jb"),
  mkop(0x78, x86_js,     Jb,  __, __, __, __,    "js       %Jb"),
  mkop(0x79, x86_jns,    Jb,  __, __, __, __,    "jns      %Jb"),
  mkop(0x7a, x86_jpe,    Jb,  __, __, __, __,    "jpe      %Jb"),
  mkop(0x7b, x86_jpo,    Jb,  __, __, __, __,    "jpo      %Jb"),
  mkop(0x7c, x86_jl,     Jb,  __, __, __, __,    "jl       %Jb"),
  mkop(0x7d, x86_jge,    Jb,  __, __, __, __,    "jge      %Jb"),
  mkop(0x7e, x86_jle,    Jb,  __, __, __, __,    "jle      %Jb"),
  mkop(0x7f, x86_jg,     Jb,  __, __, __, __,    "jg       %Jb"),

  mkop(0x80, GRP1,       Eb,  Ib, __, __, GRP1,  "%grp1    %Eb, %Ib"),
  mkop(0x81, GRP1,       Ev,  Iv, __, __, GRP1,  "%grp1    %Ev, %Iv"),
  mkop(0x82, GRP1,       Eb,  Ib, __, __, GRP1,  "%grp1    %Eb, %Ib"),
  mkop(0x83, GRP1,       Ev,  Sb, __, __, GRP1,  "%grp1    %Ev, %Sb"),
  mkop(0x84, x86_test,   Gb,  Eb, __, __, MRR,   "test     %Gb, %Eb"),
  mkop(0x85, x86_test,   Gv,  Ev, __, __, MRR,   "test     %Gv, %Ev"),
  mkop(0x86, x86_xchg,   Gb,  Eb, __, __, MRR,   "xchg     %Gb, %Eb"),
  mkop(0x87, x86_xchg,   Gv,  Ev, __, __, MRR,   "xchg     %Gv, %Ev"),
  mkop(0x88, x86_mov,    Eb,  Gb, __, __, MRR,   "mov      %Eb, %Gb"),
  mkop(0x89, x86_mov,    Ev,  Gv, __, __, MRR,   "mov      %Ev, %Gv"),
  mkop(0x8a, x86_mov,    Gb,  Eb, __, __, MRR,   "mov      %Gb, %Eb"),
  mkop(0x8b, x86_mov,    Gv,  Ev, __, __, MRR,   "mov      %Gv, %Ev"),
  mkop(0x8c, x86_mov,    Ew,  Sw, __, __, MRR,   "mov      %Ew, %Sw"),
  mkop(0x8d, x86_lea,    Gv,  Mp, __, __, MRR,   "lea      %Gv, %Mp"),
  mkop(0x8e, x86_mov,    Sw,  Ew, __, __, MRR,   "mov      %Sw, %Ew"),
  mkop(0x8f, x86_pop,    Ev,  __, __, __, MRR,   "pop      %Ev"),

  mkop(0x90, x86_nop,    __,  __, __, __, __,    "nop"),
  mkop(0x91, x86_xchg,   rvAX,gv, __,  4, __,    "xchg     %v0, %gv"),
  mkop(0x92, x86_xchg,   rvAX,gv, __,  4, __,    "xchg     %v0, %gv"),
  mkop(0x93, x86_xchg,   rvAX,gv, __,  4, __,    "xchg     %v0, %gv"),
  mkop(0x94, x86_xchg,   rvAX,gv, __,  4, __,    "xchg     %v0, %gv"),
  mkop(0x95, x86_xchg,   rvAX,gv, __,  4, __,    "xchg     %v0, %gv"),
  mkop(0x96, x86_xchg,   rvAX,gv, __,  4, __,    "xchg     %v0, %gv"),
  mkop(0x97, x86_xchg,   rvAX,gv, __,  4, __,    "xchg     %v0, %gv"),
  mkop(0x98, x86_cbw,    __,  __, __, __, __,    "cbw"),
  mkop(0x99, x86_cwd,    __,  __, __, __, __,    "cwd"),
  mkop(0x9a, x86_callf,  Ap,  __, __, 36, __,    "call     %Ap"),
  mkop(0x9b, x86_wait,   0x8, __, __, __, PFX,   "wait"),
  mkop(0x9c, x86_pushf,  __,  __, __, __, __,    "pushf"),
  mkop(0x9d, x86_popf,   __,  __, __, __, __,    "popf"),
  mkop(0x9e, x86_sahf,   __,  __, __, __, __,    "sahf"),
  mkop(0x9f, x86_lahf,   __,  __, __, __, __,    "lahf"),

  mkop(0xa0, x86_mov,    rAL, Ob, __, 14, __,    "mov      al, %Ob"),
  mkop(0xa1, x86_mov,    rvAX,Ov, __, 14, __,    "mov      %v0, %Ov"),
  mkop(0xa2, x86_mov,    Ob,  rAL,__, 14, __,    "mov      %Ob, al"),
  mkop(0xa3, x86_mov,    Ov,  rvAX,__,14, __,    "mov      %Ov, %v0"),
  mkop(0xa4, x86_movsb,  Yb,  Xb, __, __, __,    "%rmovsb"),
  mkop(0xa5, x86_movsw,  Yv,  Xv, __, __, __,    "%rmovsw"),
  mkop(0xa6, x86_cmpsb,  Yb,  Xb, __, __, __,    "%rcmpsb"),
  mkop(0xa7, x86_cmpsw,  Yv,  Xv, __, __, __,    "%rcmpsw"),
  mkop(0xa8, x86_test,   rAL, Ib, __,  4, __,    "test     al, %Ib"),
  mkop(0xa9, x86_test,   rvAX,Iv, __,  4, __,    "test     %v0, %Iv"),
  mkop(0xaa, x86_stosb,  Yb,  rAL,__, __, __,    "%rstosb"),
  mkop(0xab, x86_stosw,  Yv,  rvAX,__,__, __,    "%rstosw"),
  mkop(0xac, x86_lodsb,  rAL, Xb, __, __, __,    "%rlodsb"),
  mkop(0xad, x86_lodsw,  rvAX,Xv, __, __, __,    "%rlodsw"),
  mkop(0xae, x86_scasb,  rAL, Yb, __, __, __,    "%rscasb"),
  mkop(0xaf, x86_scasw,  rvAX,Yv, __, __, __,    "%rscasw"),

  mkop(0xb0, x86_mov,    gb,  Ib, __,  4, __,    "mov      %gb, %Ib"),
  mkop(0xb1, x86_mov,    gb,  Ib, __,  4, __,    "mov      %gb, %Ib"),
  mkop(0xb2, x86_mov,    gb,  Ib, __,  4, __,    "mov      %gb, %Ib"),
  mkop(0xb3, x86_mov,    gb,  Ib, __,  4, __,    "mov      %gb, %Ib"),
  mkop(0xb4, x86_mov,    gb,  Ib, __,  4, __,    "mov      %gb, %Ib"),
  mkop(0xb5, x86_mov,    gb,  Ib, __,  4, __,    "mov      %gb, %Ib"),
  mkop(0xb6, x86_mov,    gb,  Ib, __,  4, __,    "mov      %gb, %Ib"),
  mkop(0xb7, x86_mov,    gb,  Ib, __,  4, __,    "mov      %gb, %Ib"),
  mkop(0xb8, x86_mov,    gv,  Iv, __,  4, __,    "mov      %gv, %Iv"),
  mkop(0xb9, x86_mov,    gv,  Iv, __,  4, __,    "mov      %gv, %Iv"),
  mkop(0xba, x86_mov,    gv,  Iv, __,  4, __,    "mov      %gv, %Iv"),
  mkop(0xbb, x86_mov,    gv,  Iv, __,  4, __,    "mov      %gv, %Iv"),
  mkop(0xbc, x86_mov,    gv,  Iv, __,  4, __,    "mov      %gv, %Iv"),
  mkop(0xbd, x86_mov,    gv,  Iv, __,  4, __,    "mov      %gv, %Iv"),
  mkop(0xbe, x86_mov,    gv,  Iv, __,  4, __,    "mov      %gv, %Iv"),
  mkop(0xbf, x86_mov,    gv,  Iv, __,  4, __,    "mov      %gv, %Iv"),

  mkop(0xc0, GRP2,       Eb,  Ib, __, __, GRP2,  "%grp2    %Eb, %Ib"),
  mkop(0xc1, GRP2,       Ev,  Ib, __, __, GRP2,  "%grp2    %Ev, %Ib"),
  mkop(0xc2, x86_ret,    Iw,  __, __, 24, __,    "ret      %Iw"),
  mkop(0xc3, x86_ret,    __,  __, __, 20, __,    "ret"),
  mkop(0xc4, x86_les,    Gv,  Mp, __, __, MRR,   "les      %Gv, %Mp"),
  mkop(0xc5, x86_lds,    Gv,  Mp, __, __, MRR,   "lds      %Gv, %Mp"),
  mkop(0xc6, x86_mov,    Eb,  Ib, __, __, MRR,   "mov      %Eb, %Ib"),
  mkop(0xc7, x86_mov,    Ev,  Iv, __, __, MRR,   "mov      %Ev, %Iv"),
  mkop(0xc8, x86_enter,  Iw,  Ib, __, __, __,    "enter    %Iw, %Ib"),
  mkop(0xc9, x86_leave,  __,  __, __, __, __,    "leave"),
  mkop(0xca, x86_retf,   Iw,  __, __, 33, __,    "retf     %Iw"),
  mkop(0xcb, x86_retf,   __,  __, __, 34, __,    "retf"),
  mkop(0xcc, x86_int,    i3,  __, __, 72, __,    "int      3"),
  mkop(0xcd, x86_int,    Ib,  __, __, 71, __,    "int      %Ib"),
  mkop(0xce, x86_into,   __,  __, __, __, __,    "into"),
  mkop(0xcf, x86_iret,   __,  __, __, 44, __,    "iret"),

  mkop(0xd0, GRP2,       Eb,  i1, __, __, GRP2,  "%grp2,   %Eb, 1"),
  mkop(0xd1, GRP2,       Ev,  i1, __, __, GRP2,  "%grp2,   %Ev, 1"),
  mkop(0xd2, GRP2,       Eb,  rCL,__, __, GRP2,  "%grp2,   %Eb, cl"),
  mkop(0xd3, GRP2,       Ev,  rCL,__, __, GRP2,  "%grp2,   %Ev, cl"),
  mkop(0xd4, x86_aam,    Ib,  __, __, 83, __,    "aam      %Ib"),
  mkop(0xd5, x86_aad,    Ib,  __, __, 60, __,    "aad      %Ib"),
  mkop(0xd6, __,         __,  __, __, __, __,    NULL),
  mkop(0xd7, x86_xlat,   __,  __, __, 11, __,    "xlat"),
  mkop(0xd8, x86_grp,    Eb,  Gb, __, __, __,    "esc0"),
  mkop(0xd9, x86_grp,    Eb,  Gb, __, __, __,    "esc1"),
  mkop(0xda, x86_grp,    Eb,  Gb, __, __, __,    "esc2"),
  mkop(0xdb, x86_grp,    Eb,  Gb, __, __, __,    "esc3"),
  mkop(0xdc, x86_grp,    Eb,  Gb, __, __, __,    "esc4"),
  mkop(0xdd, x86_grp,    Eb,  Gb, __, __, __,    "esc5"),
  mkop(0xde, x86_grp,    Eb,  Gb, __, __, __,    "esc6"),
  mkop(0xdf, x86_grp,    Iv,  Gb, __, __, __,    "esc7"),

  mkop(0xe0, x86_loopnz, Jb,  __, __, __, __,    "loopnz   %Jb"),
  mkop(0xe1, x86_loopz,  Jb,  __, __, __, __,    "loopz    %Jb"),
  mkop(0xe2, x86_loop,   Jb,  __, __, __, __,    "loop     %Jb"),
  mkop(0xe3, x86_jcxz,   Jb,  __, __, __, __,    "jcxz     %Jb"),
  mkop(0xe4, x86_in,     rAL, Ib, __, 14, __,    "in       al, %Ib"),
  mkop(0xe5, x86_in,     rvAX,Ib, __, 14, __,    "in       %v0, %Ib"),
  mkop(0xe6, x86_out,    Ib,  rAL,__, 14, __,    "out      %Ib, al"),
  mkop(0xe7, x86_out,    Ib,  rvAX,__,14, __,    "out      %Ib, %v0"),
  mkop(0xe8, x86_call,   Jv,  __, __, 23, __,    "call     %Jv"),
  mkop(0xe9, x86_jmp,    Jv,  __, __, 15, __,    "jmp      %Jv"),
  mkop(0xea, x86_jmpf,   Ap,  __, __, 15, __,    "jmp      %Ap"),
  mkop(0xeb, x86_jmp,    Jb,  __, __, 15, __,    "jmp      %Jb"),
  mkop(0xec, x86_in,     rAL, rDX,__, 12, __,    "in       al, dx"),
  mkop(0xed, x86_in,     rvAX,rDX,__, 12, __,    "in       %v0, dx"),
  mkop(0xee, x86_out,    rDX, rAL,__, 12, __,    "out      dx, al"),
  mkop(0xef, x86_out,    rDX,rvAX,__, 12, __,    "out      dx, %v0"),

  mkop(0xf0, x86_lock,   0x1, __, __,  2, PFX,   "lock"),
  mkop(0xf1, __,         __,  __, __, __, __,    NULL),
  mkop(0xf2, x86_repnz,  0x2, __, __, __, PFX,   "repnz"),
  mkop(0xf3, x86_repz,   0x4, __, __, __, PFX,   "repz"),
  mkop(0xf4, x86_hlt,    __,  __, __,  2, __,    "hlt"),
  mkop(0xf5, x86_cmc,    __,  __, __,  2, __,    "cmc"),
  mkop(0xf6, GRP3,       Eb,  __, __, __, GRP3,  "%grp3    %Eb"),
  mkop(0xf7, GRP3,       Ev,  __, __, __, GRP3,  "%grp3    %Ev"),
  mkop(0xf8, x86_clc,    __,  __, __,  2, __,    "clc"),
  mkop(0xf9, x86_stc,    __,  __, __,  2, __,    "stc"),
  mkop(0xfa, x86_cli,    __,  __, __,  2, __,    "cli"),
  mkop(0xfb, x86_sti,    __,  __, __,  2, __,    "sti"),
  mkop(0xfc, x86_cld,    __,  __, __,  2, __,    "cld"),
  mkop(0xfd, x86_std,    __,  __, __,  2, __,    "std"),
  mkop(0xfe, GRP4,       Eb,  __, __, __, GRP4,  "%grp4    %Eb"),
  mkop(0xff, GRP5,       Ev,  __, __, __, GRP5,  "%grp5    %Ev"),
};


void showop(opcode_t *op) {
  printf("%.4x: %.8x %.8x %.8x %s\n", op->opfn, op->arg0, op->arg1, op->arg2, op->mnem);
}

#define PFX_LOCK   0x01
#define PFX_REPNZ  0x02  /* f2:repne cmps, repne scas */
#define PFX_REPZ   0x04  /* f3:rep ins,rep movs,rep outs,rep lods,rep stos,repe cmps,repe scas */

#define mrr_mm(x)  ((x >> 6) & 3)
#define mrr_ggg(x) ((x >> 3) & 7)
#define mrr_rrr(x) ((x & 7))

#define cx(x) ((x) ? (x) : ' ')
#define cc(a) cx(((a) & TYPE_MASK) >> TYPE_SHIFT), cx(((a) & SIZE_MASK) >> SIZE_SHIFT), ('0' + ((a) & 7))

/* Return condition codes */
constexpr bool x86_testcond(uint8_t op) {
  const bool c = (op & 1) == 0;

  switch(op & 0xE) {
  case 0x0: return (Of == c);               // jo/jno
  case 0x2: return (Cf == c);               // jb/jnb
  case 0x4: return (Zf == c);               // jz/jnz
  case 0x6: return (Cf || Zf) == c;         // jbe/ja
  case 0x8: return (Sf == c);               // js/jns
  case 0xa: return (Pf == c);               // jpe/jpo
  case 0xc: return (Sf != Of) == c;         // jl/jge
  case 0xe: return (Zf || (Sf != Of)) == c; // jle/jg
  }
  return false;
}

arg_t mkreg(int size, int vvv)
{
  return TYPE_REG+(size & SIZE_MASK)+vvv;
}

/* Return 16-bit effective address */
static uint32_t ea16(uint8_t rrr, int16_t mem)
{
  switch (rrr) {
  case 0: return segbase(rDS, BX+SI+mem, segpfx);
  case 1: return segbase(rDS, BX+DI+mem, segpfx);
  case 2: return segbase(rSS, BP+SI+mem, segpfx);
  case 3: return segbase(rSS, BP+DI+mem, segpfx);
  case 4: return segbase(rDS, SI+mem, segpfx);
  case 5: return segbase(rDS, DI+mem, segpfx);
  case 6: return segbase(rSS, BP+mem, segpfx);
  case 7: return segbase(rDS, BX+mem, segpfx);
  }
  return 0;
}

/* pre-decode opcode argument */
arg_t getarg(uint32_t arg, uint32_t op)
{
  const int ggg = mrr_ggg(mrr);
  const int rrr = mrr_rrr(mrr);
  const int mm  = mrr_mm(mrr);

  if (arg == Ap) {
    /* Convert to Mp */
    mrr_base = CSIP;
    PC += 4;
    return TYPE_EAMEM+SIZE_PTR;
  }
  // return embedded integer
  // tttttttt|ssssssss|vvvvvvvvvvvvvvvv
  if (arg == Ib) {
    return TYPE_IMM+SIZE_EMB+cpu_fetch8();
  }
  if (arg == Iw) {
    return TYPE_IMM+SIZE_EMB+cpu_fetch16();
  }
  switch (arg & TYPE_MASK) {
  case TYPE_EMBREG:
    return mkreg(arg, op & 7);
  case TYPE_EAREG:
    return mkreg(arg, ggg);
  case TYPE_EA:
  case TYPE_EAMEM:
    if (mm == 3) {
      /* register only */
      assert((arg & TYPE_MASK) != TYPE_EAMEM);
      return mkreg(arg, rrr);
    }
    if (mm == 1) {
      /* 8-bit offset */
      mrr_base = ea16(rrr, (int8_t)cpu_fetch8());
    }
    else if (mm == 2) {
      /* 16-bit offset */
      mrr_base = ea16(rrr, cpu_fetch16());
    }
    else if (rrr != 6) {
      /* base+index */
      mrr_base = ea16(rrr, 0);
    }
    else {
      /* Special case: direct 16-bit offset */
      mrr_base = segbase(rDS, cpu_fetch16(), segpfx);
    }
    return TYPE_EAMEM+(arg & SIZE_MASK);
  case TYPE_OFFSET:
    /* Convert Ob/Ov to EA address */
    mrr = 0x06;
    mrr_base = segbase(rDS, cpu_fetch16(), segpfx);
    return TYPE_EAMEM+(arg & SIZE_MASK);
  }
  return arg;
}

/* Advance string pointer ES:DI or DS:SI */
static uint32_t advstr(int arg, int delta)
{
  uint32_t base = 0;
  
  if (Df)
    delta = -delta;
  if (arg == rvDI) {
    base = segbase(rES, DI);
    DI += delta;
  }
  else {
    base = segbase(rDS, SI, segpfx);
    SI += delta;
  }
  return base;
}

/* Get value of opcode argument */
static uint32_t x86_get(const arg_t& arg)
{
  uint32_t tmp;

  switch (vsize(arg)) {
  case rDAX:
    return (DX << 16) + AX;
  case Ib:
    return cpu_fetch8();
  case Sb:
    tmp = (uint16_t)(int8_t)cpu_fetch8();
    return tmp;
  case Iw:
    return cpu_fetch16();
  case TYPE_IMM+SIZE_EMB+0x0000 ... TYPE_IMM+SIZE_EMB+0xffff:
    // embedded immediate
    return arg & VAL_MASK;
  case rAL ... rBH:
    return *bregs[arg & 7];
  case rAX ... rDI:
    return regs[arg & 7].w;
  case rES ... rGS:
    return sregs[arg & 7];
  case Jb:
    tmp = (int8_t)cpu_fetch8();
    return (PC + tmp) & 0xFFFF;
  case Jw:
    tmp = (int16_t)cpu_fetch16();
    return (PC + tmp) & 0xFFFF;
  case Mb:
    return cpu_read8(mrr_base);
  case Mw:
  case Mp:
    return cpu_read16(mrr_base);
  case Xb:
    tmp = advstr(rvSI, 1);
    return cpu_read8(tmp);
  case Xw:
    tmp = advstr(rvSI, 2);
    return cpu_read16(tmp);
  case Yb:
    tmp = advstr(rvDI, 1);
    return cpu_read8(tmp);
  case Yw:
    tmp = advstr(rvDI, 2);
    return cpu_read16(tmp);
  case 0x00:
    break;
  default:
    zprintf("Unknown src: %c%c\n", (arg >> 24) & 0xFF, (arg >> 16) & 0xFF);
    assert(0);
  }
  return 0;
}

/* Set value to opcode destination, optionally setting Zf,Sf,Af flags */
static void x86_set(const arg_t& arg, uint32_t v, bool setflags)
{
  uint32_t tmp;

  switch (vsize(arg)) {
  case rDAX:
    AX = v;
    DX = v >> 16;
    break;
  case rAL ... rBH: // 8-bit registers
    *bregs[arg & 7] = v;
    break;
  case rAX ... rDI: // 16-bit registers
    regs[arg & 7].w = v;
    break;
  case rES ... rDS: // segment registers
    sregs[arg & 7] = v;
    break;
  case Mb: // 8-bit memory
    cpu_write8(mrr_base, v);
    break;
  case Mw: // 16-bit memory
    cpu_write16(mrr_base, v);
    break;
  case Yb: // 8-bit memory, string op
    tmp = advstr(rvDI, 1);
    cpu_write8(tmp, v);
    break;
  case Yw: // 16-bit memory, string op
    tmp = advstr(rvDI, 2);
    cpu_write16(tmp, v);
    break;
  case SIZE_BYTE:
  case SIZE_WORD:
    break;
  default:
    zprintf("Unknown dest: %c%c\n", (arg >> 24) & 0xFF, (arg >> 16) & 0xFF);
    assert(0);
  }
  if (setflags) {
    x86_setfl(v, arg & SIZE_MASK);
  }
}

static uint32_t ror(const uint32_t v, const uint32_t msb_bit, const uint32_t lsb_bit = 0x01) {
  Cf = (v & lsb_bit);
  return (v >> 1) | msb_bit;
}

static uint32_t rol(const uint32_t v, const uint32_t msb_bit, const uint32_t lsb_bit) {
  Cf = (v & msb_bit);
  return (v << 1) | lsb_bit;
}

bool mksign(uint32_t&a, uint32_t& b, uint32_t sbit)
{
  bool s;

  s = ((a ^ b) & sbit);
  if (a & sbit)
    a = -a & (sbit | (sbit - 1));
  if (b & sbit)
    b = -b & (sbit | (sbit - 1));
  return s;
}

static void mul8(arg_t d, uint32_t a, uint32_t b, uint32_t m) {
  uint32_t c;

  c = a * b;

  Of = (c > m);
  Cf = (c > m);
  x86_setfl(c, m == 0xFF ? SIZE_BYTE : SIZE_WORD);
  x86_set(d, c, false);
}

static void imul(arg_t d, uint32_t a, uint32_t b, uint32_t sz) {
  uint32_t c, s;

  switch (vsize(sz, SIZE_MASK)) {
  case SIZE_BYTE:
    s = mksign(a, b, 0x80);
    c = a * b;
    Of = (c > 0xFF) || (((c ^ s) & 0x80) != 0);
    break;
  case SIZE_WORD:
    s = mksign(a, b, 0x8000);
    c = a * b;
    Of = (c > 0xFFFF) || (((c ^ s) & 0x8000) != 0);
    break;
  }
  Cf = Of;
  if (s)
    c = -c;
  x86_setfl(c, sz);
  x86_set(d, c, false);
}

static void div8(arg_t q, arg_t r, uint32_t a, uint32_t b, uint32_t m) {
  uint32_t quot, rem;
  
  if (b == 0) {
    PC = SPC;
    x86_irq(true, 0);
    return;
  }
  quot = a / b;
  rem = a % b;
  if (quot > m) {
    PC = SPC;
    x86_irq(true, 0);
    return;
  }
  x86_set(q, quot, false);
  x86_set(r, rem, quot != 0);
}

// Divide sz1 by sz2
static void idiv8(arg_t q, arg_t r, int32_t a, int32_t b, int32_t m) {
  int32_t quot, rem;

  if (b == 0) {
    PC = SPC;
    x86_irq(true, 0);
    return;
  }
  quot = (a / b);
  rem = (a % b);
  if (quot > m || quot < -m) {
    PC = SPC;
    x86_irq(true, 0);
    return;
  }
  x86_set(q, quot);
  x86_set(r, rem);
}

void x86_shiftop(int op, const arg_t& arg0, const arg_t& arg1) {
  const uint32_t msb_bit = ((arg0 & SIZE_MASK) == SIZE_BYTE) ? 0x80 : 0x8000;
  uint32_t v, count;
  bool setflag = false;
  
  v = x86_get(arg0);
  count = x86_get(arg1) & 0x1F;
  switch (op) {
  case x86_rol:
    for (int i = 1; i <= count; i++) {
      v = rol(v, msb_bit, !!(v & msb_bit));
      Of = Cf ^ !!(v & msb_bit);
    }
    break;
  case x86_ror:
    for (int i = 1; i <= count; i++) {
      v = ror(v, (v & 1) * msb_bit);
      Of = !!((v ^ (v << 1)) & msb_bit);
    }
    break;
  case x86_rcl:
    count %= ((arg0 & SIZE_MASK) == SIZE_BYTE) ? 9 : 17;
    for (int i = 1; i <= count; i++) {
      v = rol(v, msb_bit, !!Cf);
      Of = Cf ^ !!(v & msb_bit);
    }
    break;
  case x86_rcr:
    count %= ((arg0 & SIZE_MASK) == SIZE_BYTE) ? 9 : 17;
    // of ok
    Of = !!Cf ^ !!(v & msb_bit);
    for (int i = 1; i <= count; i++) {
      v = ror(v, !!Cf * msb_bit);
    }
    break;
  case x86_shl:
  case x86_sal:
    for (int i = 1; i <= count; i++) {
      v = rol(v, msb_bit, 0);
      Of = Cf ^ !!(v & msb_bit);
    }
    setflag = count > 0;
    break;
  case x86_shr:
    for (int i = 1; i <= count; i++) {
      Of = !!(v & msb_bit);
      v = ror(v, 0);
    }
    setflag = count > 0;
    break;
  case x86_sar:
    if (count == 1)
      Of = 0;
    for (int i = 1; i <= count; i++) {
      v = ror(v, v & msb_bit);
    }
    setflag = count > 0;
    break;
  }
  x86_set(arg0, v, setflag);
}

void x86_mathop(int op, const arg_t& arg0, const arg_t& arg1)
{
  uint32_t a, b, c;

  a = x86_get(arg0);
  b = x86_get(arg1);
  switch(op) {
  case x86_add: // ok
    c = a + b;
    x86_setoc(arg0, c, STDOF, STDAF);
    break;
  case x86_adc: // ok
    c = a + b + !!Cf;
    x86_setoc(arg0, c, STDOF, STDAF);
    break;
  case x86_sbb: // ok
    c = a - b - !!Cf;
    x86_setoc(arg0, c, SUBOF, STDAF);
    break;
  case x86_sub: // ok
    c = a - b;
    x86_setoc(arg0, c, SUBOF, STDAF);
    break;
  case x86_cmp: // ok
    c = a - b;
    x86_setoc(arg0 & SIZE_MASK, c, SUBOF, STDAF);
    break;
  case x86_or: // ok
    x86_setoc(arg0, a | b, 0, 0);
    break;
  case x86_and: // ok
    x86_setoc(arg0, a & b, 0, 0);
    break;
  case x86_xor: // ok
    x86_setoc(arg0, a ^ b, 0, 0);
    break;
  case x86_test:
    x86_setoc(arg0 & SIZE_MASK, a & b, 0, 0);
    break;
  case x86_xchg:
    /* swap values */
    x86_set(arg0, b);
    x86_set(arg1, a);
    break;
  case x86_inc:
  case x86_inc5:
    /* inc doesn't affect carry */
    b  = 1;
    c  = a + b;
    x86_setoc(arg0, c, STDOF, STDAF, false);
    break;
  case x86_dec:
  case x86_dec5:
    /* inc doesn't affect carry */
    b  = 1;
    c  = a - b;
    x86_setoc(arg0, c, SUBOF, STDAF, false);
    break;
  case x86_not:
    x86_set(arg0, ~a, false);
    break;
  case x86_neg:
    b = a;
    a = 0;
    c = a - b;
    x86_setoc(arg0, c, SUBOF, STDAF);
    break;
  case x86_mul:
    if ((arg0 & SIZE_MASK) == SIZE_BYTE) {
      // ax = al * eb
      mul8(rAX, AL, a, 0xFF);
    }
    else {
      // vdx:vax = vax * ev
      mul8(rvDAX, x86_get(rvAX), a, 0xFFFF);
    }
    break;
  case x86_imul:
    // rvAX   = Eb, rAL
    // rvDAX  = Ev, rvAX
    if ((arg0 & SIZE_MASK) == SIZE_BYTE) {
      imul(rAX, (int8_t)AL, (int8_t)a, SIZE_BYTE);
    }
    else {
      // vdx:vax = vax * ev
      imul(rvDAX, (int16_t)x86_get(rvAX), (int16_t)a, SIZE_WORD);
    }
    break;
  case x86_imul3:
    // Gv, Ev, Iv
    // Gv, Ev, Ib
    // AX  Eb, AL
    // DAX, Ev, AX
    imul(arg0, (int16_t)a, (int16_t)b, SIZE_WORD);
    break;
  case x86_div:
    if ((arg0 & SIZE_MASK) == SIZE_BYTE) {
      // ah:al = div(AX, Eb)
      div8(rAL, rAH, AX, a, 0xFF);
    }
    else {
      // dx:ax = div(DAX, Ev)
      div8(rvAX, rvDX, x86_get(rvDAX), a, 0xFFFF);
    }
    break;
  case x86_idiv:
    if ((arg0 & SIZE_MASK) == SIZE_BYTE) {
      // ah:al = b:ax / a:Eb
      idiv8(rAL, rAH, (int32_t)(int16_t)AX, (int32_t)(int16_t)(int8_t)a, 0x7F);
    }
    else {
      // dx:ax = dx:ax / Ev
      idiv8(rAX, rDX, (int32_t)x86_get(rvDAX), (int32_t)(int16_t)a, 0x7FFF);
    }
    break;
  default:
    zprintf("Unknown func: %x\n", op);
    assert(0);
  }
}

void x86_bcdop(int op, const arg_t& arg)
{
  uint8_t& AH = regs[0].b[1];
  uint8_t& AL = regs[0].b[0];
  const uint8_t imm = x86_get(arg);
  uint16_t tmp;

  switch (op) {
  case x86_aaa:
    if (Af || (AL & 0x0F) > 9) {
      AX += 0x0106;
      Af = 1;
      Cf = 1;
    }
    else {
      Af = 0;
      Cf = 0;
    }
    x86_set(rAL, AL & 0x0F, true);
    break;
  case x86_aas:
    if (Af || (AL & 0x0F) > 9) {
      AL = AL  - 0x06;
      AH = AH - 1;
      Af = 1;
      Cf = 1;
    }
    else {
      Af = 0;
      Cf = 0;
    }
    x86_set(rAL, AL & 0x0F, true);
    break;
  case x86_aad:
    tmp = (AH * imm) + AL;
    x86_set(rAL, tmp, true);
    x86_set(rAH, 0);
    Of = 0;
    Cf = 0;
    break;
  case x86_aam:
    div8(rAH, rAL, AL, imm, 0xFF);
    break;
  case x86_daa:
    if (Af || (AL & 0xF) > 0x09) {
      AL += 0x06;
      Af = 1;
    }
    if (Cf || AL > 0x99) {
      AL += 0x60;
      Cf = 1;
    }
    x86_set(rAL, AL, true);
    break;
  case x86_das:
    if (Cf || AL > 0x99) {
      AL -= 0x60;
      Cf = 1;
    }
    if (Af || (AL & 0x0f) > 0x09) {
      AL -= 0x06;
      Af = 1;
    }
    x86_set(rAL, AL, true);
    break;
  default:
    assert(0);
  }
}

/* a4 movsb mov(es:di, ds:si) ds overridden
 * a5 movsv mov(es:di, ds:si)
 * a6 cmpsb rsb(es:di, ds:si) ds overridden
 * a7 cmpsv rsb(es:di, ds:si)
 * aa stosb mov(es:di, rAL)   can't override es
 * ab stosv mov(es:di, rvAX)
 * ac lodsb mov(rAL  , ds:si) ds overridden
 * ad lodsv mov(rvAX , ds:si)
 * ae scasb rsb(es:di, rAL)   can't override es
 * af scasv rsb(es:di, rvAX)
 *
 * f2: repnz cmps,scas
 * f3: repz  ins,movs,outs,lods,stos,cmps,scas
 */
int x86_strop2(int opfn, arg_t a0, arg_t a1)
{
  int count = (pfx & (PFX_REPZ|PFX_REPNZ)) ? CX : 1;

  //hexdump(&cpu.ram[segbase(rDS, SI, segpfx)], 128, 32);
  //hexdump(&cpu.ram[segbase(rES, DI)], 128, 32);
  while (count > 0) {
    x86_mathop(opfn, a0, a1);
    count--;
    if ((pfx & PFX_REPZ) && !Zf)
      break;
    if ((pfx & PFX_REPNZ) && Zf)
      break;
  }
  if (pfx & (PFX_REPZ|PFX_REPNZ))
    CX = count;
  return 1;
}

/* Handle stos,lods,movs */
void x86_strop3(int lhs, int rhs)
{
  int count = 1;
  
  if (pfx & (PFX_REPZ|PFX_REPNZ)) {
    count = CX;
    CX = 0;
  }
  while (count > 0) {
    x86_set(lhs, x86_get(rhs));
    count--;
  }
}

/*=================================================*
 * IRQ Handler code
 *=================================================*/
struct {
  icb_t handler;
  void *arg;
} ifn[256];

/* Registter an IRQ hook to get callback */
void register_irq(int n, icb_t fn, void *arg) {
  ifn[n].handler = fn;
  ifn[n].arg = arg;
}

bool x86_irq(bool test, int irq)
{
  uint16_t npc;
  uint16_t ncs;

  if (!test) {
    return false;
  }
  //zprintf("IRQ: %.2x\n", irq);
  /* check if handler exists. if returns true then handled already */
  if (ifn[irq].handler && ifn[irq].handler(ifn[irq].arg)) {
    zprintf("Overriding interrupt %.2x\n", irq);
    return true;
  }
  cpu_push16(cpu_getflags());
  cpu_push16(CS);
  cpu_push16(PC);
  If = false;
  Tf = false;
  
  npc = cpu_readv(IDT_BASE + irq * 4);
  ncs = cpu_read16(IDT_BASE + irq * 4 + 2);
  x86_setpc(true, ncs, npc);
  return true;
}

/*==============================================*
 * Disassembler
 *==============================================*/
int x86_disarg(char *dst, uint32_t arg)
{
  const char *ea16[] = {
    "bx+si","bx+di","bp+si","bp+di","si","di","bp","bx"
  };
  switch(arg) {
  case Ib:
    return snprintf(dst, 16, "0x%.2x", cpu_read8(CSIP));
  case Iw:
  case Iv:
    return snprintf(dst, 16, "0x%.4x", cpu_read16(CSIP));
  case Sb:
    return snprintf(dst, 16, "0x%.4x", (int16_t)cpu_read8(CSIP));
  case i3: case i1:
    return snprintf(dst, 16, "%x", arg & 7);
  case rES ... rDS:
  case rAL ... rBH:
  case rAX ... rDI:
  case rvAX ... rvDI:
    return snprintf(dst, 16, "%s", regname(arg));
  case Jb:
    return snprintf(dst, 16, "0x%.4x", (PC + (int8_t)cpu_read8(CSIP) + 1) & 0xffff);
  case Jv:
    return snprintf(dst, 16, "0x%.4x", (PC + (int16_t)cpu_read16(CSIP) + 2) & 0xffff);
  case Ap:
    return snprintf(dst, 16, "%.4x:%.4x", cpu_read16(CSIP + 2), cpu_read16(CSIP));
  case Mb:
  case Mv:
    return snprintf(dst, 16, "[%s:0x%x]", ea16[mrr & 7], mrr_base);
    break;
  }
  return snprintf(dst, 16, "%c%c%c", cc(arg));
}

const char *x86_dis(uint8_t op, uint32_t a0, uint32_t a1, uint32_t a2)
{
  const int ggg = mrr_ggg(mrr);
  opcode_t opc = opix[op];
  static char dstr[64];
  const char *src;
  char *dst = dstr;

  src = opc.mnem;
  if (pfx == PFX_REPNZ) {
    dst += snprintf(dst, 16, "repnz ");
  }
  else if (pfx == PFX_REPZ) {
    dst += snprintf(dst, 16, "repz   ");
  }
  if (!(replace(&src, "GRP1", &dst, "%-8s", grp1[ggg]) ||
	replace(&src, "GRP2", &dst, "%-8s", grp2[ggg]) ||
	replace(&src, "GRP3", &dst, "%-8s", grp3[ggg]) ||
	replace(&src, "GRP4", &dst, "%-8s", grp4[ggg]) ||
	replace(&src, "GRP5", &dst, "%-8s", grp5[ggg])))
    {
      dst += snprintf(dst, 16, "%-8s", src);
    }
  if (segpfx)  {
    dst += snprintf(dst, 16, "%s:", regname(segpfx));
  }
  if (a0) {
    dst += x86_disarg(dst, a0);
    if (a1) {
      dst += snprintf(dst, 16, ", ");
      dst += x86_disarg(dst, a1);
    }
    if (a2) {
      dst += snprintf(dst, 16, ", ");
      dst += x86_disarg(dst, a2);
    }
  }
  *dst = 0;
  return dstr;
}

void dd(uint8_t *a, uint8_t *b, int l)
{
  for (int i = 0; i < l; i++) {
    if (*a != *b) {
      zprintf("%3x: %.2x %.2x\n", i, *a, *b);
    }
    a++;
    b++;
  }
}

/* Load segment + base pointer (les, lds, etc) */
static void x86_loadptr(arg_t seg, arg_t ofs)
{
  uint32_t addr = mrr_base;

  /* Size=16/32 */
  x86_set(ofs, cpu_read16(addr + 0));
  x86_set(seg, cpu_readv(addr + 2));
}

/* Execute an opcode with its arguments:
 *  push:    r:arg0
 *  enter:   r:arg0, arg1
 *  mov:     r:arg1
 *  ret:     r:arg0
 *  retf:    r:arg0
 *  call:    r:arg0
 *  callf:
 */
int cpu_exec(int opfn, arg_t arg0, arg_t arg1, arg_t arg2)
{
  uint32_t tmp;
  uint16_t ncs;

  switch(opfn) {
  case x86_push:
    cpu_push16(x86_get(arg0));
    break;
  case x86_pop:
    x86_set(arg0, cpu_pop16());
    break;
  case x86_pushf:
    cpu_push16(cpu_getflags());
    break;
  case x86_popf:
    cpu_setflags(cpu_pop16());
    break;
  case x86_leave:
    SP = BP;
    BP = cpu_pop16();
    break;
  case x86_enter:
    cpu_push16(BP);
    BP = SP;
    x86_get(arg0);
    SP -= x86_get(arg1);
    break;
  case x86_mov:
    x86_set(arg0, x86_get(arg1));
    break;
  case x86_ret:
    tmp = cpu_pop16();
    SP += x86_get(arg0);
    x86_setpc(true, CS, tmp);
    break;
  case x86_retf:
    tmp = cpu_pop16();
    ncs = cpu_pop16();
    SP += x86_get(arg0);
    x86_setpc(true, ncs, tmp);
    break;
  case x86_iret:
    tmp = cpu_pop16();
    ncs = cpu_pop16();
    cpu_setflags(cpu_pop16());
    x86_setpc(true, ncs, tmp);
    break;
  case x86_call:  // call Jv/Ev
    tmp = x86_get(arg0);
    cpu_push16(PC);
    x86_setpc(true, CS, tmp);
    break;
  case x86_callf: // call Ap/Mp
    tmp = cpu_readv(mrr_base);
    ncs = cpu_read16(mrr_base + 2);
    cpu_push16(CS);
    cpu_push16(PC);
    x86_setpc(true, ncs, tmp);
    break;
  case 0x70 ... 0x7f: // jcc Jb (cycs = 16/4)
    x86_setpc(x86_testcond(opfn), CS, x86_get(arg0));
    if (x86_testcond(opfn))
      extracyc = 12;
    else
      extracyc = 4;
    break;
  case x86_jmp:  // jmp Jb/Jv/Ev
    x86_setpc(true, CS, x86_get(arg0));
    break;
  case x86_jmpf: // jmp Ap/Mp
    If = false;
    tmp = cpu_readv(mrr_base);
    ncs = cpu_read16(mrr_base + 2);
    x86_setpc(true, ncs, tmp);
    break;
  case x86_jcxz: // jcxz Jb
    tmp = x86_get(rvCX);
    x86_setpc(tmp == 0, CS, x86_get(arg0));
    break;
  case x86_loop: // loop Jb (cycs = 17/5)
    tmp = x86_get(rvCX) - 1;
    x86_setpc(tmp != 0, CS, x86_get(arg0));
    x86_set(rvCX, tmp);
    break;
  case x86_loopnz: // loopnz Jb
    tmp = x86_get(rvCX) - 1;
    x86_setpc(tmp != 0 && !Zf, CS, x86_get(arg0));
    x86_set(rvCX, tmp);
    break;
  case x86_loopz: // loopz Jb
    tmp = x86_get(rvCX) - 1;
    x86_setpc(tmp != 0 && Zf, CS, x86_get(arg0));
    x86_set(rvCX, tmp);
    break;
  case x86_cmc: // cmc
    Cf = !Cf;
    break;
  case x86_clc: // clc
  case x86_stc: // stc
    Cf = (opfn == x86_stc);
    break;
  case x86_cli: // cli
  case x86_sti: // sti
    If = (opfn == x86_sti);
    break;
  case x86_cld: // cld
  case x86_std: // std
    Df = (opfn == x86_std);
    break;
  case x86_in:
    x86_set(arg0, io_read(x86_get(arg1), arg0));
    break;
  case x86_out:
    io_write(x86_get(arg0), x86_get(arg1), arg1);
    break;
  case x86_sahf: // sahf
    cpu_setflags((eflags & 0xFF00) | AH);
    break;
  case x86_lahf: // lahf
    AH = cpu_getflags();
    break;
  case x86_rol: // grp2
  case x86_ror:
  case x86_rcl:
  case x86_rcr:
  case x86_shl:
  case x86_shr:
  case x86_sal:
  case x86_sar:
    x86_shiftop(opfn, arg0, arg1);
    break;
  case x86_add: //setoc
  case x86_sub: //setoc
  case x86_or:  //setoc 0,0
  case x86_and: //setoc 0,0
  case x86_xor: //setoc 0,0
  case x86_sbb: //setoc
  case x86_adc: //setoc
  case x86_cmp: //setoc
  case x86_test://setoc 0,0
  case x86_not: //set
  case x86_neg: //setoc
  case x86_mul:
  case x86_imul:
  case x86_div:
  case x86_idiv:
  case x86_inc:  // setoc false
  case x86_inc5:
  case x86_dec:  // setoc false
  case x86_dec5:
  case x86_xchg:
    x86_mathop(opfn, arg0, arg1);
    break;
  case x86_movsb:
  case x86_movsw:
  case x86_lodsb:
  case x86_lodsw:
  case x86_stosb:
  case x86_stosw:
    x86_strop3(arg0, arg1);
    break;
  case x86_scasb:
  case x86_scasw:
  case x86_cmpsb:
  case x86_cmpsw:
    x86_strop2(x86_cmp, arg0, arg1);
    break;
  case x86_cbw: // cbw
    AX = (int8_t)AL;
    break;
  case x86_cwd: // cwd ax -> dx:ax
    tmp = (int16_t)AX;
    DX = (tmp >> 16);
    break;
  case x86_nop: // nop
    break;
  case x86_hlt: // hlt
    zprintf("HALT!!!!\n");
    exit(0);
  case x86_daa:
  case x86_das:
  case x86_aaa:
  case x86_aas:
  case x86_aam:
  case x86_aad:
    x86_bcdop(opfn, arg0);
    break;
  case x86_int: // int NN
    x86_irq(true, x86_get(arg0));
    break;
  case x86_into: // into
    x86_irq(Of, 4);
    break;
  case x86_xlat: // xlat
    tmp = segbase(rDS, BX + AL, segpfx);
    AL = cpu_read8(tmp);
    break;
  case x86_lea: // lea
    x86_set(arg0, mrr_base - mrr_seg);
    break;
  case x86_lds:
    x86_loadptr(rDS, arg0);
    break;
  case x86_les:
    x86_loadptr(rES, arg0);
    break;
  case x86_lfs:
    x86_loadptr(rFS, arg0);
    break;
  case x86_lgs:
    x86_loadptr(rGS, arg0);
    break;
  case x86_lss:
    x86_loadptr(rSS, arg0);
    break;
  default:
    /* did not execute */
    printf("unknown opcode: %x\n", opfn);
    assert(0);
    return 1;
  };
  return 0;
}

uint8_t *diskdata;
size_t   disksize;

struct vmrtc {
  uint32_t tm_sec;
  uint32_t tm_min;
  uint32_t tm_hour;
  uint32_t tm_mday;
  uint32_t tm_mon;
  uint32_t tm_year;
  uint32_t tm_wday;
  uint32_t tm_yday;
  uint32_t tm_dst;
  uint32_t tm_msec;
};

/* VM code used by 8086 tiny bios */
static void cpu_vm()
{
  int op;
  uint32_t base = 0;
  uint32_t sssp;
  
  op = cpu_fetch8();
  if (op == 0x00) {
    zprintf("VM:WRITECH: '%c'\n", AL);
  }
  else if (op == 0x01) {
    struct timespec ts;
    struct tm *tm;

    /* Get RTC */
    base = segbase(rES, BX);
    zprintf("VM:GETRTC: %.8x\n", base);
    clock_gettime(CLOCK_REALTIME, &ts);
    tm = localtime(&ts.tv_sec);

    zprintf("%2d/%2d/%d  %2d:%2d:%2d\n",
	    tm->tm_mday, tm->tm_mon+1, tm->tm_year + 1900,
	    tm->tm_hour, tm->tm_min, tm->tm_sec);
    memcpy(&cpu.ram[base], tm, sizeof(*tm));
    *(uint32_t *)&cpu.ram[base+36] = ts.tv_nsec / 1000000;
    //hexdump(&cpu.ram[base], 64, 32);
  }
  else if (op == 0x02) {
    base = segbase(rES, BX);
    sssp = segbase(rSS, SP);

    zprintf("VM:DISKREAD: disk:%.2x sector:%.4x:%.4x size:%.4x %.4x:%.4x = [%.8x], %.8x\n",
	    DL,   // disk
	    SI,   // sector
	    BP,
	    AX,   // size
	    ES, BX, base, sssp);
    memcpy(&cpu.ram[base], &diskdata[BP * 512], AX);
#if 0
    hexdump(&cpu.ram[base], AX);
#endif
    AL = 0;
  }
  else if (op == 0x03) {
    base = segbase(rES, BX);
    zprintf("VM:DISKWRITE: disk:%.2x sector:%.4x:%.4x size:%.4x %.4x:%.4x = %.8x\n",
	    DL, SI, BP, AX, ES, BX, base);
    memcpy(&diskdata[BP * 512], &cpu.ram[base], AX);
    AL = 0;
  }
}

// cycles: https://www2.math.uni-wuppertal.de/~fpf/Uebungen/GdR-SS02/opcode_i.html
// http://bitsavers.informatik.uni-stuttgart.de/pdf/intel/_dataBooks/1981_iAPX_86_88_Users_Manual.pdf
/* Exec an x86 opcode */
uint8_t visited[1024*1024];

int cpu_step()
{
  arg_t arg0, arg1, arg2;
  opcode_t opc;
  int op;
  
  /* Decode opcode */
  cpu_setflags(eflags);
  SPC = PC;
  pfx = 0;
  segpfx = 0;
  mrr_seg = 0;
  mrr_base = 0;
  if (SPC < 1024*1024)
    visited[SPC] = 1;
  for(;;) {
    op = cpu_fetch8();

    /* count # of opcodes */
    opcnt[op]++;
    if (op == 0x0f) {
      /* Tiny BIOS uses 0x0f byte as emulator trap */
      cpu_vm();
      op = cpu_fetch8();
    }

    /* copy opcode */
    opc = opix[op];
    if (!opc.mnem) {
      zprintf("ACK: %.2x\n", op);
      assert(opc.mnem);
    }

    /* get mrr byte and opfn */
    if (opc.flag & MRR) {
      mrr = cpu_fetch8();
      if ((opc.flag & GRP) != MRR) {
	opc.opfn += mrr_ggg(mrr);
      }
    }

    /* Special cases */
    switch ((op << 16) + opc.opfn) {
    case 0xf60000 + x86_test:
      opc.arg1 = Ib;
      break;
    case 0xf70000 + x86_test:
      opc.arg1 = Iv;
      break;
    }

    /* Get prefix */
    if ((opc.flag & PFX) == 0)
      break;
    if ((opc.arg0 & TYPE_MASK) == TYPE_REG) {
      // segment prefix
      segpfx = opc.arg0;
    }
    else {
      // other prefix. lock, repz, repnz, etc
      pfx |= opc.arg0;
    }
  }
  lastop = opc.opfn;
  
  /* Pre-decode opcode args */
  extracyc = x86_cycles(op, &opc);
  arg0 = getarg(opc.arg0, op);
  arg1 = getarg(opc.arg1, op);
  arg2 = getarg(opc.arg2, op);
  trace = 0;
  if (trace) {
    cpu_showregs();
    zprintf("%.4x:%.2x %s\n",
	    opc.opfn, op, x86_dis(op, arg0, arg1, arg2));
    visited[(CS << 4) + SPC] = 0;
  }

  /* Execute opcode */
  if (cpu_exec(opc.opfn, arg0, arg1, arg2)) {
    zprintf("%.2x %-6s %c%c%c %c%c%c  - unknown op\n",op, opc.mnem, cc(arg0), cc(arg1));
    assert(0);
  }
  return extracyc;
}

/* Flag tests
 * Cf (x > mask)
 * Sf (x & ((mask >> 1) + 1))
 * Pf
 * Of
 * Af
 * Zf (x & mask) == 0
 */
#include "emit.cc"
void getextra(dstk *stk)
{
  FILE *fp;
  char  line[1024];
  uint32_t base;

  if ((fp = fopen("dumpcfg.x86", "r")) != NULL) {
    while (fgets(line, sizeof(line), fp) != NULL) {
      sscanf(line, "%x", &base);
      printf("push: %x\n", base);
      stk->push(base, dstk::PENDING);
    }
  }
}

const char *xdis(int op, const char *src, uint8_t *mem, int off, int rep)
{
  static char dst[256], *d;

  if (!src)
    return "xxx";
  d = dst;
  while (*src) {
    if (replace(&src, "%Ib", &d, "0x%.2x", mem[off])) {
      off++;
    }
    else if (replace(&src, "%Iv", &d, "0x%.4x", get16(&mem[off]))) {
      off += 2;
    }
    else if (replace(&src, "%Iw", &d, "0x%.4x", get16(&mem[off]))) {
      off += 2;
    }
    else if (replace(&src, "%Sw", &d, "%s", regname(rES + mrr_ggg(mem[off+1])))) {
      off ++;
    }
    else if (replace(&src, "%Sw", &d, "%s", regname(rES + mrr_ggg(mem[off+1])))) {
      off += 2;
    }
    else if (replace(&src, "%Jb", &d, "0x%.4x", off + 1 + (int8_t)mem[off])) {
      off++;
    }
    else if (replace(&src, "%Jv", &d, "0x%.4x", off + 2 +(int16_t)get16(&mem[off]))) {
      off+=2;
    }
    else if (replace(&src, "%Ov", &d, "[%.4x]", get16(&mem[off]))) {
      off += 2;
    }
    else if (replace(&src, "%Ob", &d, "[%.4x]", get16(&mem[off]))) {
      off += 2;
    }
    else if (replace(&src, "%Ap", &d, "%.4x:%.4x", get16(&mem[off+2]), get16(&mem[off]))) {
      off += 4;
    }
    else if (replace(&src, "%gv", &d, "%s", regname(rvAX + (op & 7)))) {
    }
    else if (replace(&src, "%r", &d, "%s", rep ? "rep      " : "")) {
    }
    else if (replace(&src, "%gb", &d, "%s", regname(rAL + (op & 7)))) {
    }
    else {
      *d++ = *src++;
    }
  }
  *d = 0;
  return dst;
}

bool argiz(opcode_t& o, int n) {
  return (o.arg0 == n || o.arg1 == n || o.arg2 == n);
}

void dumpcfg(uint8_t *data, size_t size)
{
  uint32_t off;
  uint8_t op, mrr;
  int nxt[2], jpos, start, rep = 0;
  bool reset = true;
  opcode_t opc;
  dstk *stk;

  stk = new dstk(size, zprintf);
  getextra(stk);
  while ((off = stk->pop()) != -1) {
    if (reset) {
      zprintf("=============================\n");
      reset = false;
    }
    start = off;
    do {
      op = data[off++];
      opc = opix[op];

      switch (opc.opfn) {
      case x86_repz:
      case x86_repnz:
	rep = opc.opfn;
	break;
      }
    } while (opc.flag & FLAG_PFX || op == 0x0f);
    if (opc.flag & FLAG_MRR) {
      mrr = data[off++];
      if ((mrr & 0xC7) == 0x06 || ((mrr & 0xC0) == 0x80)) {
	// word offset
	off += 2;
      }
      else if ((mrr & 0xC0) == 0x40) {
	// byte offset
	off += 1;
      }
    }
    zprintf("%.4x: %.2x %s\n", start, op, xdis(op, opc.mnem, data, off, rep));
    jpos = -1;

    // hack x87 codes
    if (op == 0xdf)
      off += 2;
    if (argiz(opc, Ib))
      off += 1;
    if (argiz(opc, Iv) || argiz(opc, Ob) || argiz(opc, Ov)) {
      off += 2;
    }
    if (opc.arg0 == Jb) {
      jpos = off + *(int8_t *)&data[off] + 1;
      off += 1;
    }
    else if (opc.arg0 == Jv) {
      jpos = off + *(int16_t *)&data[off] + 2;
      off += 2;
    }
    nxt[0] = off;
    nxt[1] = -1;
    switch (opc.opfn) {
    case x86_jo ... x86_jg:
    case x86_loopnz:
    case x86_loopz:
    case x86_loop:
    case x86_jcxz:
    case x86_call:
      // conditional or call
      nxt[1] = jpos;
      reset = true;
      break;
    case x86_jmp: //0xe9:
    case 0xeb: // jmp Jb
      // unconditional
      nxt[0] = jpos;
      reset = true;
      break;
    case x86_aam:
      off += 1;
      nxt[0] = off;
      break;
    case x86_ret:
    case x86_retf:
    case x86_iret:
    case x86_jmpf:
      // ret/jmp terminate
      nxt[0] = -1;
      reset = true;
      break;
    case x86_int:
      reset = true;
      break;
    }
    stk->push(start, off-start, dstk::CODE);
    for (int i = 0; i<2; i++) {
      stk->push(nxt[i], 1, dstk::PENDING);
    }
  }
  stk->showstk(64);
}

void runme()
{
  cpu_step();
  i8253_tick();

  // 32/3 source?
  for (int o = 0; o <= 9; o++)
    audio_tick();
  if (framectr.tick()) {
    cpu.drawscreen();
  }
  cycs++;
}

void testx()
{
  idiv8(0, 0, 0x44440000, 0x0022, SIZE_WORD); 
  idiv8(0, 0, 0xf0700000, 0x1f20, SIZE_WORD); // 8000.0000
  idiv8(0, 0, 0x08090000, 0x1012, SIZE_WORD); // OF:8093
  idiv8(0, 0, 0x09680000, 0x12d0, SIZE_WORD); // OF:8093
  idiv8(0, 0, 0xf3ac0000, 0x18a8, SIZE_WORD); // 8000.0000

  cpu.ram[0xcd6] = 0xaa;
  cpu.ram[0xcd7] = 0xab;
  cpu.ram[0x1000] = 0x8b;
  cpu.ram[0x1001] = 0x3e;
  cpu.ram[0x1002] = 0xd6;
  cpu.ram[0x1003] = 0x0c;

  CS = 0;
  PC = 0x1000;
  BP = 0x1234;
  cpu_step();
  cpu_showregs();
  exit(0);
}

void cpu_shutdown(void)
{
  for (int i = 0; i < 256; i++) {
    zprintf("%.2x: %10d %s\n", i, opcnt[i], opix[i].mnem);
  }
}

void flogger(int lvl, const char *fmt, ...)
{
  va_list ap;

  va_start(ap, fmt);
  zprintf("%.4x:%.4x | ", CS, SPC);
  vprintf(fmt, ap);
}

uint8_t *getdma(uint32_t addr) {
  return &cpu.ram[addr];
};

#include "json/88json.cc"
int main(int argc, char *argv[])
{
  //parse_json("8088test/00.json");
  
  diskdata = loadrom("pcbios/disk01.img", disksize);

  setdmp(zprintf);

  for (int i = 0xf0000; i <= 0xfffff; i++) {
    visited[i] = 1;
  }
  if (argc > 1 && !strcmp(argv[1], "-dumpcfg")) {
    dumpcfg(diskdata, disksize);
    exit(0);
  }
  //__asm__ __volatile__("mov $0x2233,%ax; add $0x3322,%ax");
  cpu.init();
  switch (disksize) {
  case 320*1024:
    cpu_write8(0xf13fd, 0x8);
    break;
  case 360*1024:
    cpu_write8(0xf13fd, 0x9);
    break;
  }

  /* Set frame counter to CPU Hz / 60 Hz */
  framectr.settimer(CPU_HZ / 60, 1, 1, "framecounter");

  audio_init();
  for (;;) {
    runme();
  }
}
