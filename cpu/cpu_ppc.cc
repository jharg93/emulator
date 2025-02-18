#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <execinfo.h>
#include "util.h"
#include "cpu.h"
#include "bus.h"
#include <map>

uint32_t xlate(int bat, uint32_t base);
static void ppc_set_spr(int sprid, uint32_t nv);

enum  {
  IBAT0U = 528,
  IBAT0L,
  IBAT1U,
  IBAT1L,
  IBAT2U,
  IBAT2L,
  IBAT3U,
  IBAT3L,
  DBAT0U,
  DBAT0L,
  DBAT1U,
  DBAT1L,
  DBAT2U,
  DBAT2L,
  DBAT3U,
  DBAT3L,
};

#include "gamecube.h"

/* Define some useful macros for opcode handlers */
#define  SFRd     fpregs[i.D].f
#define  SFRa     fpregs[i.A].f
#define  SFRb     fpregs[i.B].f
#define  DFRi     fpregs[i.D].i
#define  DFRd     fpregs[i.D].d
#define  DFRa     fpregs[i.A].d
#define  DFRb     fpregs[i.B].d
#define  DFRbi    fpregs[i.B].i
#define  DFRc     fpregs[i.C].d
#define  SFRc     fpregs[i.C].d

#if 1
#define  Rd *i.rd
#define  Rs *i.rd
#define  Ra *i.ra
#define  Rb *i.rb
#else
#define  Rd       regs[i.D]
#define  Rs       regs[i.D]
#define  Ra       regs[i.A]
#define  Rb       regs[i.B]
#endif

#define  SIMM     (int16_t)i.op
#define  UIMM     (uint16_t)i.op
#define  ZRa      i.zra

/* PowerPC instruction encoding:
 *       30    28    26    24    22    20    18    16    14    12    10    8     6     4     2     0
 * 0     2     4     6     8     10    12    14    16    18    20    22    24    26    28    30
 * @--+--+--+--+--+--+--+--@--+--+--+--+--+--+--+--@--+--+--+--+--+--+--+--@--+--+--+--+--+--+--+--@
 * |     func        |     D/S      |      A       |      B       |OE|      subfunc             |Rc|
 * @--+--+--+--+--+--+--+--@--+--+--+--+--+--+--+--@--+--+--+--+--+--+--+--@--+--+--+--+--+--+--+--@
 * |     func        |      D       |      A       |      B       |      C       |  subfunc     |Rc|
 * @--+--+--+--+--+--+--+--@--+--+--+--+--+--+--+--@--+--+--+--+--+--+--+--@--+--+--+--+--+--+--+--@
 * |     func        |     D/S      |      A       |      IMM                                      |
 * @--+--+--+--+--+--+--+--@--+--+--+--+--+--+--+--@--+--+--+--+--+--+--+--@--+--+--+--+--+--+--+--@
 * |     func        |      BO      |     BI       |     BD                                  |AA|LK|
 * @--+--+--+--+--+--+--+--@--+--+--+--+--+--+--+--@--+--+--+--+--+--+--+--@--+--+--+--+--+--+--+--@
 * |     func        |      LI                                                               |AA|LK|
 * @--+--+--+--+--+--+--+--@--+--+--+--+--+--+--+--@--+--+--+--+--+--+--+--@--+--+--+--+--+--+--+--@
 *
 * OE = Overflow
 * RC = GT/LT/EQ
 * 17 potential bits for opcode
 */

/* Registers 
 *  Condition reg CR
 * @--+--+--+--+--+--+--+--@--+--+--+--+--+--+--+--@--+--+--+--+--+--+--+--@--+--+--+--+--+--+--+--@
 * |CR0        |CR1        |CR2        |CR3        |CR4        |CR5        |CR6        |CR7        |
 * @--+--+--+--+--+--+--+--@--+--+--+--+--+--+--+--@--+--+--+--+--+--+--+--@--+--+--+--+--+--+--+--@
 *  LT GT EQ SO|FX FEXVX OX|FL FG FE FU
 *                          LT GT EQ SO
 *
 * Floating point status FPSCR
 * @--+--+--+--+--+--+--+--@--+--+--+--+--+--+--+--@--+--+--+--+--+--+--+--@--+--+--+--+--+--+--+--@
 * |FXFEX|VX|OX|UX|ZX|XX|NANISIIDIZDZIMZVC|FR|FI|FPRF          |0 |SFTSRTCVIVE|OE|UE|ZE|XE|NI|RN   |
 * @--+--+--+--+--+--+--+--@--+--+--+--+--+--+--+--@--+--+--+--+--+--+--+--@--+--+--+--+--+--+--+--@
 *
 * XER
 * @--+--+--+--+--+--+--+--@--+--+--+--+--+--+--+--@--+--+--+--+--+--+--+--@--+--+--+--+--+--+--+--@
 * |SO|OV|CA|                                                              | Byte count            |
 * @--+--+--+--+--+--+--+--@--+--+--+--+--+--+--+--@--+--+--+--+--+--+--+--@--+--+--+--+--+--+--+--@
 *
 * MSR
 * @--+--+--+--+--+--+--+--@--+--+--+--+--+--+--+--@--+--+--+--#--+--+--+--@--+--+--+--#--+--+--+--@
 * |  |  |  |                                       EE|PR|FP|ME|F0|SE|BE|F1|00|IP|IR|DR|00000|RI|LE|
 * @--+--+--+--+--+--+--+--@--+--+--+--+--+--+--+--@--+--+--+--#--+--+--+--@--+--+--+--#--+--+--+--@
 * LINK
 * CTR
 */

/* opcode flags */
enum {
  None,

  OE_bit = 0x0400,
  RC_bit = 0x0001,
  LK_bit = 0x0001,
  AA_bit = 0x0002,

  /* Rc/OE bits in opcode or implied Rc */
  OE    = 0x0001,
  RC    = 0x0002,
  AA    = 0x0004,
  LK    = 0x0008,
  CY    = 0x0010, // carry
  IMPRC = 0x0020, // implied RC
  FRC   = 0x0040, // floating point RC
};

constexpr uint32_t ppcbit(int n) {
  return (0x80000000 >> (31 - n));
}

/* Flag bits, use Big Endian bit numbering */
struct flag_t {
  const uint32_t mask;
  uint32_t &flag;

  flag_t(uint32_t &f, int bit) : flag(f), mask(0x80000000 >> bit) { };
  flag_t& operator=(const bool state) {
    flag = state ?
      (flag | mask) :
      (flag & ~mask);
    return *this;
  }
  operator bool() const {
    return !!(flag & mask);
  };
};

static uint32_t regs[32];
static uint32_t PC, MSR, SR;

/* Special purpose registers */
static uint32_t spr[1024];
static uint32_t& LINK = spr[8];
static uint32_t& CTR  = spr[9];
static uint32_t& XER  = spr[1];
static uint32_t *IBAT = &spr[528];
static uint32_t *DBAT = &spr[536];

static flag_t XER_SO(XER, 0);
static flag_t XER_OV(XER, 1);
static flag_t XER_CA(XER, 2);

/* Floating point register */
union {
  uint32_t i;
  double   d;
  float    f;
} fpregs[32];
static uint32_t FPSCR[32];

/* Condition reg: Cheat use 1 uint32 per bit... */
static uint32_t  CR[32];
static uint32_t& CR_LT = CR[0];
static uint32_t& CR_GT = CR[1];
static uint32_t& CR_EQ = CR[2];
static uint32_t& CR_OV = CR[3];
static uint32_t& CR_FL = CR[8];
static uint32_t& CR_FG = CR[9];
static uint32_t& CR_FE = CR[10];
static uint32_t& CR_FU = CR[11];

/* Set Condition reg */
void ppc_set_cr(const uint32_t val) {
  uint32_t mask = 0x80000000;
  for (int i = 0; i < 32; i++) {
    CR[i] = !!(mask & val);
    mask >>= 1;
  };
}

/* Get condition reg */
static uint32_t ppc_get_cr() {
  uint32_t val = 0;
  for (int i = 0; i < 32; i++) {
    val <<= 1;
    val |= !!CR[i];
  }
  return val;
}

/* Get MSR register */
static uint32_t ppc_get_msr() {
  return MSR;
}

/* Set MSR register */
static void ppc_set_msr(uint32_t msr) {
  MSR = msr;
}

/* Get status register */
static uint32_t ppc_get_sr() {
  return SR;
}

/* Set status register */
static void ppc_set_sr(uint32_t sr) {
  SR = sr;
}

void ppc_set_fpscr(const uint32_t val) {
  uint32_t mask = 0x80000000;
  for (int i = 0; i < 32; i++) {
    FPSCR[i] = !!(mask & val);
    mask >>= 1;
  };
}

/* Get special purpose register */
static int sprid(int b, int a) {
  return (b << 5) | a;
};

static uint32_t ppc_get_spr(int sprid) {
  printf("GET SPR: %.4x\n", sprid);
  return spr[sprid % 1023];
}

/* Set special purpose register */
static void ppc_set_spr(int sprid, uint32_t nv) {
  printf("SET SPR: %.4x\n", sprid);
  spr[sprid % 1023] = nv;
}

/* virtual to physical address
 *                     1            2           3
 *   VIRT  0123 4567 8901 2345 6789 0123 4567 8901
 *         BBBB.BBBB.BBBB.BBBp.pppp.oooo.oooo.oooo
 *
 *   IBATU BBBB.BBBB.BBBB.BBB-.---l.llll.llll.llsp
 *   IBATL NNNN.NNNN.NNNN.NNN-.----.----.-www.w-pp
 */
#define BEPI_MASK 0xFFFE0000

uint32_t xlate(int bat, uint32_t base)
{
  uint32_t v_bepi = (base & BEPI_MASK);
  uint32_t offset = (base & 0x00000FFF);
  uint32_t b_bepi, b_bprn, b_bl;

  for (int i = 0; i < 4; i++) {
    b_bepi = spr[bat + i * 2] & BEPI_MASK;
    b_bprn = spr[bat + i * 2 + 1] & BEPI_MASK;
    b_bl   = (spr[bat + i * 2] & 0x00001FFC) << 10;
    
    printf("%d: %.8x %.8x\n", i, v_bepi, b_bepi);
    if ((v_bepi & ~b_bl) == b_bepi) {
      return ((b_bprn | (base & b_bl)) + offset);
    }
  }
  return base;
}

/* Copy CRF bits */
static void ppc_move_bits(uint32_t *dst, uint32_t *src, int n)
{
  while (n--) {
    *dst++ = *src++;
  }
}

struct instr_t {
  const uint32_t D, A, B, C;
  const uint32_t op;
  uint32_t flag = 0;
  bool overflow = false;

  /* Keep pointer to register */
  uint32_t *rb;
  uint32_t *ra;
  uint32_t *rd;
  
  /* Holder for memory access */
  uint32_t ea;
  uint32_t dst;
  uint32_t zra = 0;
  
  instr_t(const uint32_t& _op) :
    /* Extract registers
     * ------dd.dddaaaaa.bbbbb---.--------
     */
    D((_op >> 21) & 0x1F),
    A((_op >> 16) & 0x1F),
    B((_op >> 11) & 0x1F),
    C((_op >> 6) & 0x1F),
    op(_op)
  {
    printf("DAB: %.2x %.2x %.2x C:%.2x\n", D, A, B, C);
    setregs();
  };
  instr_t(const uint32_t& d, const uint32_t& a, const uint32_t& b) :
    D(d & 0x1F),
    A(a & 0x1F),
    B(b & 0x1F),
    C(0),
    op((d << 21) | (a << 16) | (b) << 11)
  {
    setregs();
    printf("DAB: %.2x %.2x %.2x %.2x\n", D, A, B, C);
  };
  void setregs() {
    ra = &regs[A];
    rb = &regs[B];
    rd = &regs[D];
    if (A != 0)
      zra = regs[A];
  };
};

static double setrc(instr_t& i, const double src, int base=4) {
  printf("Set Double FP flag:%x base:%x res: %f\n", i.flag, base, src);
  if (!(i.flag & RC))
    return src;
  i.flag = 0;
  CR_FE = (src == 0.0);
  CR_FL = (src < 0.0);
  CR_FG = (src > 0.0);
  return src;
}

static float setrc(instr_t& i, const float src, int base=4) {
  printf("Set Float FP flag:%x base:%x res: %f\n", i.flag, base, (double)src);
  if (!(i.flag & RC))
    return src;
  i.flag = 0;
  CR_FE = (src == 0.0);
  CR_FL = (src < 0.0);
  CR_FG = (src > 0.0);
  return src;
}

static uint32_t setdst(instr_t& i, const uint32_t r, const char *str) {
  i.dst = r;
  return r;
};

/* Reverse bytes in 32-bit value:
 *  ABCD -> DCBA */
constexpr uint32_t revbytes(const uint32_t a)
{
  return
    ((a & 0xFF000000) >> 24) |
    ((a & 0x00FF0000) >> 8)  |
    ((a & 0x0000FF00) << 8)  |
    ((a & 0x000000FF) << 24);
}

/*====================================*
 * Memory Load/Store
 *
 * Wii memory map
 *  80000000 .. 817FFFFF 00000000 MEM1 (cached)
 *  C0000000 .. C17FFFFF 00000000 MEM1 (uncached)
 *  90000000 .. 93FFFFFF 10000000 MEM2 (cached)
 *  D0000000 .. D3FFFFFF 10000000 MEM2 (uncached)
 *  CD000000 .. CD007FFF D0000000 Hollywood registers
 *====================================*/
// 24 mb mem1
#define PPC_REGS(o) \
  o(0x800000C4, "User Interrupt mask") \
  o(0x800000DC, "Earliest created thread") \
  o(0x800000E0, "Recently created thread") \
  o(0x800000E4, "Current thread") \
  o(0x800000F0, "Simulated memory size") \
  o(0x80003040, "OSInterrupt Table") \
  o(0x80003100, "Physical MEM1 Size") \
  o(0x80003104, "Simulated MEM1 Size") \
  o(0x80003118, "Physical MEM2 Size") \
  o(0x80003138, "Hollywood Version") 

template <class T>
static uint32_t ppc_load(const uint32_t addr, uint32_t* EA = NULL) {
  if (EA)
    *EA = addr;
  return cpu_read<T>(addr);
}

/* Store and update index */
template <class T>
static void ppc_store(const T val, const uint32_t addr, uint32_t* EA = NULL) {
  if (EA)
    *EA = addr;
  printf("store: size=%3d addr:%.8x val:%x\n", int(sizeof(val)), addr, (uint32_t)val);
  cpu_write<T>(addr, val);
}

template <class T>
static void ppc_loadr(uint32_t& dst, const uint32_t addr) {
  dst = 0;
}

/* Fetch CPU opcode: use IBAT lookup */
static uint32_t cpu_fetch()
{
  printf("fetch... %.8x\n", PC);
  uint32_t v = cpu_read32be(xlate(IBAT0U, PC));
  PC = PC + 4;
  return v;
}

static void ppc_load_multiple(int rs, int re, int step, uint32_t addr)
{
  for (int i = rs; i <= re; i++) {
    regs[i] = cpu_read32be(addr);
    addr += 4;
  }
}

static void ppc_store_multiple(int rs, int re, int step, uint32_t addr)
{
  for (int i = rs; i <= re; i++) {
    cpu_write32be(addr, regs[i]);
    addr += 4;
  }
}

/*====================================*
 * Opcode handlers
 *====================================*/
//#define __sf printf(" %x = %s(%x,%x)\n", (uint32_t)i.dst, __FUNCTION__, (uint32_t)ra, (uint32_t)rb)
#define __sf 0

template <typename T>
static void ppc_cmp(instr_t& i, const T& ra, const T& rb)
{
  int crfd = (i.op >> 21) & 0x1C;

  i.dst = (ra - rb);
  __sf;
  
  printf("cmp: %d\n", crfd);
  CR[crfd+0] = (ra < rb);  // c = 0b100
  CR[crfd+1] = (ra > rb);  // c = 0b010
  CR[crfd+2] = (ra == rb); // c = 0b001
  CR[crfd+3] = XER_SO;
}

#define BO(n) (((op) >> (25-n)) & 0x1)

/* b
 * bl
 * ba
 * bla
 *
 * bc
 * bca
 * bcl
 * bcla
 *
 * bcctr   
 * bcctrl
 *
 * bclr
 * bclrl
 */
//constexpr auto BO0=ppcbit(6);
//constexpr auto BO1=ppcbit(7);
//constexpr auto BO2=ppcbit(8);
//constexpr auto BO3=ppcbit(8);

static void ppc_b(const uint32_t flag, uint32_t npc)
{
  /* If relative address, add to current PC */
  if (flag & AA)
    npc += PC;
  /* Set LINK register */
  if (flag & LK)
    LINK = PC+4;
  printf("SETNEWPC: %.8x %.8x\n", PC, npc);
  PC = npc & ~3;
}

/* B0: condvalid
 * B1: CR[BI]
 * B2: 0:DEC CTR, 1:n/a
 * B3: 0:CTR!=0,  1:CTR==0
 * B4:
 * BLT:  12,0
 * BNE:  4,10
 * BDNZ: 16,0
 */
static void ppc_bc(instr_t& i)
{
  const uint32_t op = i.op;
  const uint32_t BI = (op >> 16) & 0x1F;
  const uint32_t BO = (op >> 21) & 0x1F;
  const uint32_t npc = signex(op, 16);
  bool ctr_ok = true, cond_ok = true;

  printf("BC: %.8x BO:%.2x BI:%.2x\n", npc, BO, BI);
  if ((BO & 4) == 0) {
    /* Decrement counter */
    CTR--;
  }
  ctr_ok = (BO & 0x04) || ((CTR != 0) == !(BO & 0x2));
  cond_ok = (BO & 0x10) || (!CR[BI] == !(BO & 0x8));
  if (ctr_ok && cond_ok)
    ppc_b(i.flag, npc);
}

static void ppc_bcctr(instr_t& i)
{
  const uint32_t op = i.op;
  const uint32_t BI = (op >> 16) & 0x1F;
  const uint32_t BO = (op >> 21) & 0x1F;
  const uint32_t npc = CTR;
  bool cond_ok;

  printf("BCCTR: %.8x BO:%.2x BI:%.2x\n", npc, BO, BI);
  cond_ok = (BO & 0x10) || (!CR[BI] == !(BO & 0x08));
  if (cond_ok)
    ppc_b(i.flag, npc);
}

static void ppc_bclr(instr_t& i)
{
  const uint32_t op = i.op;
  const uint32_t BI = (op >> 16) & 0x1F;
  const uint32_t BO = (op >> 21) & 0x1F;
  const uint32_t npc = LINK;
  bool cond_ok = true, ctr_ok = true;

  printf("BCLR: %.8x BO:%.2x BI:%.2x\n", npc, BO, BI);
  if (!(BO & 0x4))
    CTR--;
  ctr_ok = (BO & 0x04) || ((CTR != 0) == !(BO & 0x2));
  cond_ok = (BO & 0x10) || (!CR[BI] == !(BO & 0x8));
  if (ctr_ok && cond_ok)
    ppc_b(i.flag, npc);
}

static void ppc_condbranch(const uint32_t& op)
{
  const uint32_t BI=(op >> 16) & 0x1F;
  uint32_t flag = 0;
  uint32_t npc = 0;
  bool cond = true;

  if ((op & 0xFF000000) == 0x42000000) {
    // BDNZ
    flag = (op & 3);
    npc  = signex(op, 16);
    cond = (--CTR == 0);
  }
  else if ((op & 0xFC000000) == 0x48000000) {
    // branch
    flag = (op & 3);
    npc  = signex(op, 26);
  }
  else if ((op & 0xFC000000) == 0x40000000) {
    // branch conditional
    // 0000- DEC CTR, Branch if 
    flag = (op & 3);
    npc  = signex(op, 16);
  }
  else if ((op & 0xFC0007FE) == 0x4C000420) {
    // branch conditional: CTR (bcctr)
    flag = (op & 1) | AA_bit;
    npc  = (CTR & ~0x3);
    cond = (BO(0) || (CR[BI] == BO(1)));
  }
  else if ((op & 0xFC0007FE) == 0x4C000020) {
    // branch conditional: LINK (bclr)
    flag = (op & 1) | AA_bit;
    npc = (LINK & ~0x3);
    if (!BO(2))
      CTR--;
    bool cond1 = (BO(0) || (CR[BI] == BO(1)));
  }
  printf("Cond: %d\n", cond);
  if (cond)
    ppc_b(flag, npc);
}

/* Set overflow */
void setov(const bool state)
{
  XER_OV = state;
  XER_SO = XER_SO | state;
}

/*===============================================*
 * Opcode Helpers
 *===============================================*/
static uint32_t ppc_clz(uint32_t src)
{
  uint32_t mask = 0x80000000;
  
  for (int i = 0; i < 32; i++) {
    if (src & mask)
      return i;
    mask >>= 1;
  }
  return 32;
}

template <const uint32_t flag>
const uint32_t ppc_add(instr_t& i, const uint32_t ra, const uint32_t rb, const uint32_t cy = 0)  {
  uint32_t rd;
  
  rd = setdst(i, ra + rb + cy, __FUNCTION__);
  if constexpr((flag & OE) != 0) {
    i.overflow = VFLAG32(ra, rb, rd);
  }
  if constexpr((flag & CY) != 0) {
    XER_CA = CFLAG32(ra, rb, rd);
  }
  return rd;
}

/* Move/Store */
template <class T, const uint32_t flag>
static void ppc_mov(instr_t& i, T& rd, const T rs) {
  rd = rs;
  if constexpr((flag & RC) != 0) {
    if (i.op & RC_bit) {
      printf("setrc: %x\n", (uint32_t)rd);
      CR_EQ = rd == 0;
      CR_LT = ((int32_t)rd < 0);
      CR_GT = ((int32_t)rd > 0);
      CR_OV = (bool)XER_SO;
      i.flag = 0;
    }
  };
  if constexpr((flag & FRC) != 0) {
    if (i.op & RC_bit) {
      printf("setrc: %f\n", (double)rd);
      CR_FE = (rd == 0.0);
      CR_FL = (rd < 0.0);
      CR_FG = (rd > 0.0);
      i.flag = 0;
    }
  }
}

static uint32_t ppc_and(instr_t& i, const uint32_t ra, const uint32_t rb)  {
  return setdst(i, ra & rb, __FUNCTION__);
}

static uint32_t ppc_nand(instr_t& i, const uint32_t ra, const uint32_t rb) {
  return setdst(i, ~(ra & rb), __FUNCTION__);
}

static uint32_t ppc_or(instr_t& i, const uint32_t ra, const uint32_t rb) {
  return setdst(i, ra | rb, __FUNCTION__);
}

static uint32_t ppc_nor(instr_t& i, const uint32_t ra, const uint32_t rb) {
  return setdst(i, ~(ra | rb), __FUNCTION__);
}

static uint32_t ppc_xor(instr_t& i, const uint32_t ra, const uint32_t rb)  {
  return setdst(i, ra ^ rb, __FUNCTION__);
}

static uint32_t ppc_eqv(instr_t& i, const uint32_t ra, const uint32_t rb)  {
  return setdst(i, ~(ra ^ rb), __FUNCTION__);
}

static void ppc_shl(instr_t& i, uint32_t& rd, const uint32_t ra, const uint32_t rb) {
  rd = (rb >= 0x20 && rb <= 0x3f) ? 0 : (ra << (rb & 0x1F));
  setdst(i, rd, __FUNCTION__);
}

static void ppc_shr(instr_t& i, uint32_t& rd, const uint32_t ra, const uint32_t rb)  {
  rd = (rb >= 0x20 && rb <= 0x3F) ? 0 : (ra >> (rb & 0x1F));
  setdst(i, rd, __FUNCTION__);
}

static void ppc_sar(instr_t& i, uint32_t& rd, const int32_t ra, const uint32_t rb)  {
  bool cy = (ra < 0);
  int   n = (rb & 0x3F);
  int32_t ta = ra;
  
  while(n--) {
    if ((ta & 1) && cy) {
      XER_CA = true;
    }
    ta = (ta >> 1);
  }
  rd = setdst(i, ta, __FUNCTION__);
}

constexpr uint32_t rotmask(const uint32_t mb, const uint32_t me) {
  const uint32_t bm = (0xFFFFFFFF >> mb);
  const uint32_t em = (0xFFFFFFFF << (31-me));
  return (mb <= me) ? (bm & em) : (bm | em);
}

constexpr uint32_t rol(const uint32_t val, const uint32_t n) {
  return (val << n) | (val >> (32-n));
}

static void ppc_rol(instr_t & i, uint32_t& rd, const uint32_t rs, const uint32_t rb, const uint32_t zra = 0) {
  const uint32_t n  = (rb & 0x1F);
  const uint32_t mb = (i.op >> 6) & 0x1F;
  const uint32_t me = (i.op >> 1) & 0x1F;
  const uint32_t mask = rotmask(mb, me);

  rd = (rol(rs, n) & mask) | (zra & ~mask);
  setdst(i, rd, __FUNCTION__);
}

template <class T>
static void ppc_mla(T& rd, const T ra, const T rb, const T rc)
{
  rd = (ra * rb) + rc;
  printf("mla: %lf= %lf*%lf + %lf\n", (double)rd, (double)ra, (double)rb, (double)rc);
}

template <class T, int flag>
static void ppc_mul(instr_t& i, uint32_t& rd, const T ra, const T rb, const int32_t shift) {
  int64_t prod = ((int64_t)ra * (int64_t)rb) >> shift;

  rd = setdst(i, prod, __FUNCTION__);
  if constexpr((flag & OE) != 0) {
    i.overflow = prod != (int64_t)(int32_t)prod;
  }
}

template <typename T>
static void ppc_div(instr_t& i, uint32_t& rd, const T ra, const T rb)  {
  i.overflow = (rb == 0);
  if constexpr(std::is_signed<T>::value) {
    if (ra == 0x80000000 && rb == -1)
      i.overflow = true;
  }
  if (i.overflow) {
    rd = (ra < 0) ? 0xFFFFFFFF : 0x00000000;
  }
  else {
    rd = ra / rb;
  }
  setdst(i, rd, __FUNCTION__);
}

#define fnarg instr_t& i

struct opcode_t {
  uint32_t mask;
  uint32_t bits;
  uint32_t flag;
  const char *mnem;
  void (*eval)(fnarg);
  bool used;
};

/* bx:    oooooo##.########.########.######AL 18
 * bcx:   ooooooOO.OOOIIIII.########.######AL 16
 * bcctr: ooooooOO.OOOIIIII.00000---.------AL 19:528
 * bclr:  ooooooOO.OOOIIIII.00000---.------AL 19:16
 *
 * BO:
 *  0000y : dec CTR, Branch if CTR!=0 and cond=false
 *  0100y : dec CTR, Branch if CTR!=0 and cond=true
 *  0001y : dec CTR, Branch if CTR=0  and cond=false
 *  0101y : dec CTR, Branch if CTR=0  and cond=true
 *  1z00y : dec CTR, branch if CTR!=0
 *  1z01y : dec CTR, branch if CTR=0
 *  001zy : branch if cond=false
 *  011zy : branch if cond=true
 *  1z1zz : branch always
 *
 * 010000|10000|00000
 */

/* Aliases:
 *    addi %rD, 0x00, <nnnn> -> lis %Rd, <nnnn>
 *    mfspr rd,8             -> mflr rd
 *    mfspr rd,1             -> mfxer rd
 *    mfspr rd,9             -> mfctr rd
 */
#define NOTIMP assert(0)

/*===================================*
 * Opcode Table
 *===================================*/

typedef void (*opfn_t)(instr_t& i);

constexpr opcode_t mkop(const char *bits, int flag, const char *mnem, opfn_t fn) {
  opcode_t o = {};
  char ch = 0;
  
  while ((ch = *bits++) != 0) {
    if (ch == '.')
      continue;
    o.bits <<= 1;
    o.mask <<= 1;
    if (ch == '0' || ch == '1') {
      o.mask |= 0x1;
      o.bits |= (ch - '0');
    }
  }
  o.flag = flag;
  o.mnem = mnem;
  o.eval = fn;
  return o;
}

/* Create opcode table */
#define o(z,cyc,flag,m,func...) mkop(z, flag, m, [](fnarg) func )
opcode_t optab[] = {
  o("010010.LLLLL.LLLLL.LLLLL.LLLLLLLLLAL", __2, AA|LK    , "b         ___, ___, ___"            , { ppc_b(i.flag, signex(i.op, 26)); }),
  o("010000.OOOOO.IIIII.DDDDD.DDDDDDDDDAL", __2, AA|LK    , "bc        ___, ___, ___"            , { ppc_bc(i); }),
  o("010011.OOOOO.IIIII.00000.1000010000L", ___, LK       , "bcctr     ___, ___, ___"            , { ppc_bcctr(i); }),
  o("010011.OOOOO.IIIII.00000.0000010000L", __2, LK       , "bclr      ___, ___, ___"            , { ppc_bclr(i); }),
  o("010001.00000.00000.00000.00000000010", __2, None     , "sc"                                 , { NOTIMP; }),
  o("011111.00000.00000.00000.00001100100", ___, None     , "rfi"                                , { NOTIMP; }),
  o("011111.sssss.aaaaa.00000.0000011010r", __y, RC       , "cntlzw    %rA, %rS"                 , { ppc_mov<uint32_t, RC>(i, Ra, ppc_clz(Rs)); }),
  o("010011.DDD00.SSS00.00000.00000000000", ___, None     , "mcrf      %crD, %crS"               , { ppc_move_bits(&CR[i.D], &CR[i.A], 4); }),
  o("111111.DDD00.SSS00.00000.00010000000", ___, None     , "mcrfs     %crD, %crS"               , { NOTIMP; }),
  o("011111.DDD00.00000.00000.10000000000", ___, None     , "mcrxr     %crD"                     , { NOTIMP; }),
  o("011111.ddddd.00000.00000.00000100110", ___, None     , "mfcr      %rD"                      , { Rd = ppc_get_cr(); }),
  o("011111.ddddd.00000.00000.00010100110", __y, None     , "mfmsr     %rD"                      , { Rd = ppc_get_msr(); }),
  o("011111.sssss.00000.00000.00100100100", __y, None     , "mtmsr     %rS"                      , { ppc_set_msr(Rs); }),
  o("011111.ddddd.0rrrr.00000.10010100110", ___, None     , "mfsr      %rD, ___, ___"            , { NOTIMP; }),
  o("011111.ddddd.00000.bbbbb.10100100110", ___, None     , "mfsrin    %rD, %rB"                 , { NOTIMP; }),
  o("011111.sssss.0rrrr.00000.00110100100", __y, None     , "mtsr      ___, %rS, ___"            , { NOTIMP; }),
  o("011111.sssss.00000.bbbbb.00111100100", ___, None     , "mtsrin    %rS, %rB"                 , { NOTIMP; }),
  o("111111.ddddd.00000.00000.1001000111r", ___, RC       , "mffs%x    %fD"                      , { NOTIMP; }),
  o("111111.0ffff.ffff0.bbbbb.1011000111r", ___, RC       , "mtfsf%x   ___, %fB"                 , { ppc_set_fpscr(fpregs[i.B].i); }),
  o("111111.DDD00.00000.iiii0.0010000110r", ___, RC       , "mtfsfi%x  ___, ___"                 , { FPSCR[i.D] = i.B; }),
  o("011111.sssss.0mmmm.mmmmm.00100100000", ___, None     , "mtcrf     ___, %rS"                 , { NOTIMP; }),
  o("011111.ddddd.rrrrr.rrrrr.01010100110", __y, None     , "mfspr     %rD, ___"                 , { Rd = ppc_get_spr(sprid(i.B, i.A)); }),
  o("011111.sssss.rrrrr.rrrrr.01110100110", __y, None     , "mtspr     ___, %rS, ___"            , { ppc_set_spr(sprid(i.B, i.A), Rs); }),
  o("011111.ddddd.rrrrr.rrrrr.01011100110", __y, None     , "mftb      %rD, ___, ___"            , { Rd = ppc_get_spr(sprid(i.B, i.A)); }),
  o("011111.DDDDD.00000.00000.0001000110r", ___, RC       , "mtfsb0%x  %crD"                     , { FPSCR[i.D] = 0; }),
  o("011111.DDDDD.00000.00000.0000100110r", ___, RC       , "mtfsb1%x  %crD"                     , { FPSCR[i.D] = 1; }),
  o("011111.sssss.aaaaa.00000.1110111010r", ___, RC       , "extsb%x   %rA, %rS"                 , { ppc_mov<uint32_t, RC>(i, Ra, (int8_t)Rs); }),
  o("011111.sssss.aaaaa.00000.1110011010r", __y, RC       , "extsh%x   %rA, %rS"                 , { ppc_mov<uint32_t, RC>(i, Ra, (int16_t)Rs); }),
  o("010011.ddddd.aaaaa.bbbbb.01000000010", ___, None     , "crand     %crD, %crA, %crB"         , { CR[i.D] = ppc_and(i, CR[i.A], CR[i.B]); }),
  o("010011.ddddd.aaaaa.bbbbb.00100000010", ___, None     , "crandc    %crD, %crA, %crB"         , { CR[i.D] = ppc_and(i, CR[i.A], !CR[i.B]); }),
  o("010011.ddddd.aaaaa.bbbbb.01001000010", ___, None     , "creqv     %crD, %crA, %crB"         , { CR[i.D] = ppc_eqv(i, CR[i.A], CR[i.B]); }),
  o("010011.ddddd.aaaaa.bbbbb.00111000010", ___, None     , "crnand    %crD, %crA, %crB"         , { CR[i.D] = ppc_nand(i,CR[i.A], CR[i.B]); }),
  o("010011.ddddd.aaaaa.bbbbb.00001000010", ___, None     , "crnor     %crD, %crA, %crB"         , { CR[i.D] = ppc_nor(i, CR[i.A], CR[i.B]); }),
  o("010011.ddddd.aaaaa.bbbbb.01110000010", ___, None     , "cror      %crD, %crA, %crB"         , { CR[i.D] = ppc_or(i,  CR[i.A], CR[i.B]); }),
  o("010011.ddddd.aaaaa.bbbbb.01101000010", ___, None     , "crorc     %crD, %crA, %crB"         , { CR[i.D] = ppc_or(i,  CR[i.A], !CR[i.B]); }),
  o("010011.ddddd.aaaaa.bbbbb.00110000010", __2, None     , "crxor     %crD, %crA, %crB"         , { CR[i.D] = ppc_xor(i, CR[i.A], CR[i.B]); }),
  o("011111.ddddd.aaaaa.bbbbb.o100001010r", __y, OE|RC    , "add%x     %rD, %rA, %rB"            , { Rd = ppc_add<OE|00>(i, Ra, Rb); }),
  o("011111.ddddd.aaaaa.bbbbb.o000001010r", ___, OE|RC    , "addc%x    %rD, %rA, %rB"            , { Rd = ppc_add<OE|CY>(i, Ra, Rb); }),
  o("011111.ddddd.aaaaa.bbbbb.o010001010r", ___, OE|RC    , "adde%x    %rD, %rA, %rB"            , { Rd = ppc_add<OE|CY>(i, Ra, Rb, XER_CA); }),
  o("011111.ddddd.aaaaa.00000.o011101010r", ___, OE|RC    , "addme%x   %rD, %rA"                 , { Rd = ppc_add<OE|CY>(i, Ra, -1, XER_CA); }),
  o("011111.ddddd.aaaaa.00000.o011001010r", ___, OE|RC    , "addze%x   %rD, %rA"                 , { Rd = ppc_add<OE|CY>(i, Ra, 0, XER_CA); }),
  o("001101.ddddd.aaaaa.iiiii.iiiiiiiiiii", __2, IMPRC    , "addic.    %rD, %rA, %SIMM"          , { Rd = ppc_add<00|CY>(i, Ra, SIMM); }),
  o("001100.ddddd.aaaaa.iiiii.iiiiiiiiiii", __2, None     , "addic     %rD, %rA, %SIMM"          , { Rd = ppc_add<00|CY>(i, Ra, SIMM); }),
  o("001110.ddddd.00000.iiiii.iiiiiiiiiii", __2, None     , "lis       %rD, %SIMM"               , { ppc_mov<uint32_t, 00>(i, Rd, SIMM); /*alias*/}),
  o("001110.ddddd.aaaaa.iiiii.iiiiiiiiiii", __2, None     , "addi      %rD, %zrA, %SIMM"         , { Rd = ppc_add<00|00>(i, ZRa, SIMM); }),
  o("001111.ddddd.aaaaa.iiiii.iiiiiiiiiii", __2, None     , "addis     %rD, %zrA, %SIMM"         , { Rd = ppc_add<00|00>(i, ZRa, SIMM<<16); }),
  o("011111.ddddd.aaaaa.bbbbb.o000101000r", __y, OE|RC    , "subf%x    %rD, %rA, %rB"            , { Rd = ppc_add<OE|00>(i, ~Ra, Rb, 1); }),
  o("011111.ddddd.aaaaa.bbbbb.o000001000r", ___, OE|RC    , "subfc%x   %rD, %rA, %rB"            , { Rd = ppc_add<OE|CY>(i, ~Ra, Rb, 1); }),
  o("011111.ddddd.aaaaa.bbbbb.o010001000r", ___, OE|RC    , "subfe%x   %rD, %rA, %rB"            , { Rd = ppc_add<OE|CY>(i, ~Ra, Rb, XER_CA); }),
  o("011111.ddddd.aaaaa.00000.o011101000r", ___, OE|RC    , "subfme%x  %rD, %rA"                 , { Rd = ppc_add<OE|CY>(i, ~Ra, -1, XER_CA); }),
  o("011111.ddddd.aaaaa.00000.o011001000r", ___, OE|RC    , "subfze%x  %rD, %rA"                 , { Rd = ppc_add<OE|CY>(i, ~Ra, 0, XER_CA); }),
  o("001000.ddddd.aaaaa.iiiii.iiiiiiiiiii", __2, None     , "subfic    %rD, %rA, %SIMM"          , { Rd = ppc_add<00|CY>(i, ~Ra, SIMM, 1); }),
  o("011111.ddddd.aaaaa.00000.o001101000r", ___, OE|RC    , "neg%x     %rD, %rA"                 , { Rd = ppc_add<OE|00>(i, ~Ra, 1); }),
  o("011111.ddddd.aaaaa.bbbbb.0000001011r", ___, RC       , "mulhwu%x  %rD, %rA, %rB"            , { ppc_mul<uint32_t,00>(i, Rd, Ra, Rb, 32); }),
  o("011111.ddddd.aaaaa.bbbbb.0001001011r", ___, RC       , "mulhw%x   %rD, %rA, %rB"            , { ppc_mul<int32_t,00>(i, Rd, Ra, Rb, 32); }),
  o("011111.ddddd.aaaaa.bbbbb.o011101011r", ___, OE|RC    , "mullw%x   %rD, %rA, %rB"            , { ppc_mul<int32_t,OE>(i, Rd, Ra, Rb, 0); }),
  o("000111.ddddd.aaaaa.iiiii.iiiiiiiiiii", __2, None     , "mulli     %rD, %rA, %SIMM"          , { ppc_mul<int32_t,00>(i, Rd, Ra, SIMM, 0); }),
  o("011111.ddddd.aaaaa.bbbbb.o111101011r", ___, OE|RC    , "divw%x    %rD, %rA, %rB"            , { ppc_div<int32_t>(i, Rd, Ra, Rb); }),
  o("011111.ddddd.aaaaa.bbbbb.o111001011r", ___, OE|RC    , "divwu%x   %rD, %rA, %rB"            , { ppc_div<uint32_t>(i, Rd, Ra, Rb); }),
  o("011111.sssss.aaaaa.bbbbb.0000011100r", __y, RC       , "and%x     %rA, %rS, %rB"            , { Ra = ppc_and(i, Rs, Rb); }),
  o("011111.sssss.aaaaa.bbbbb.0000111100r", __y, RC       , "andc%x    %rA, %rS, %rB"            , { Ra = ppc_and(i, Rs, ~Rb); }),
  o("011100.sssss.aaaaa.iiiii.iiiiiiiiiii", __y, IMPRC    , "andi.     %rA, %rS, %UIMM"          , { Ra = ppc_and(i, Rs, UIMM); }),
  o("011101.sssss.aaaaa.iiiii.iiiiiiiiiii", ___, IMPRC    , "andis.    %rA, %rS, %UIMM"          , { Ra = ppc_and(i, Rs, UIMM<<16); }),
  o("011111.sssss.aaaaa.bbbbb.0111011100r", ___, RC       , "nand%x    %rA, %rS, %rB"            , { Ra = ppc_nand(i,Rs, Rb); }),
  o("011111.sssss.aaaaa.bbbbb.0001111100r", __y, RC       , "nor%x     %rA, %rS, %rB"            , { Ra = ppc_nor(i, Rs, Rb); }),
  o("011111.sssss.aaaaa.bbbbb.0110111100r", __y, RC       , "or%x      %rA, %rS, %rB"            , { Ra = ppc_or(i,  Rs, Rb); }),
  o("011111.sssss.aaaaa.bbbbb.0110011100r", ___, RC       , "orc%x     %rA, %rS, %rB"            , { Ra = ppc_or(i,  Rs, ~Rb); }),
  o("011000.sssss.aaaaa.iiiii.iiiiiiiiiii", __y, None     , "ori       %rA, %rS, %UIMM"          , { Ra = ppc_or(i,  Rs, UIMM); }),
  o("011001.sssss.aaaaa.iiiii.iiiiiiiiiii", __y, None     , "oris      %rA, %rS, %UIMM"          , { Ra = ppc_or(i,  Rs, UIMM<<16); }),
  o("011111.sssss.aaaaa.bbbbb.0100111100r", ___, RC       , "xor%x     %rA, %rS, %rB"            , { Ra = ppc_xor(i, Rs, Rb); }),
  o("011010.sssss.aaaaa.iiiii.iiiiiiiiiii", ___, None     , "xori      %rA, %rS, %UIMM"          , { Ra = ppc_xor(i, Rs, UIMM); }),
  o("011011.sssss.aaaaa.iiiii.iiiiiiiiiii", ___, None     , "xoris     %rA, %rS, %UIMM"          , { Ra = ppc_xor(i, Rs, UIMM<<16); }),
  o("011111.DDD0L.aaaaa.bbbbb.00000000000", __y, None     , "cmp       %crD, %L, %rA, %rB"       , { ppc_cmp<int32_t>(i, Ra, Rb); }),
  o("001011.DDD0L.aaaaa.iiiii.iiiiiiiiiii", __2, None     , "cmpi      %crD, %L, %rA, %SIMM"     , { ppc_cmp<int32_t>(i, Ra, SIMM); }),
  o("011111.DDD0L.aaaaa.bbbbb.00001000000", __y, None     , "cmpl      %crD, %L, %rA, %rB"       , { ppc_cmp<uint32_t>(i, Ra, Rb); }),
  o("001010.DDD0L.aaaaa.iiiii.iiiiiiiiiii", __2, None     , "cmpli     %crD, %L, %rA, %SIMM"     , { ppc_cmp<uint32_t>(i, Ra, UIMM); }),
  o("011111.sssss.aaaaa.bbbbb.0100011100r", ___, RC       , "eqv%x     %rA, %rS, %rB"            , { Ra = ppc_eqv(i,Rs, Rb); }),
  o("011111.sssss.aaaaa.bbbbb.0000011000r", __y, RC       , "slw%x     %rA, %rS, %rB"            , { ppc_shl(i, Ra, Rs, Rb); }),
  o("011111.sssss.aaaaa.bbbbb.1000011000r", __y, RC       , "srw%x     %rA, %rS, %rB"            , { ppc_shr(i, Ra, Rs, Rb); }),
  o("011111.sssss.aaaaa.bbbbb.1100011000r", ___, RC       , "sraw%x    %rA, %rS, %rB"            , { ppc_sar(i, Ra, Rs, Rb); }),
  o("011111.sssss.aaaaa.BBBBB.1100111000r", ___, RC       , "srawi%x   %rA, %rS, %iB"            , { ppc_sar(i, Ra, Rs, i.B); }),
  o("010100.sssss.aaaaa.iiiii.BBBBBEEEEEr", __y, RC       , "rlwimi%x  %rA, %rS, %iB, %MB, %ME"  , { ppc_rol(i, Ra, Rs, i.B, Ra); }),
  o("010101.sssss.aaaaa.iiiii.BBBBBEEEEEr", __y, RC       , "rlwinm%x  %rA, %rS, %iB, %MB, %ME"  , { ppc_rol(i, Ra, Rs, i.B); }),
  o("010111.sssss.aaaaa.bbbbb.BBBBBEEEEEr", ___, RC       , "rlwnm%x   %rA, %rS, %rB, %MB, %ME"  , { ppc_rol(i, Ra, Rs, Rb); }),
  o("100010.ddddd.aaaaa.iiiii.iiiiiiiiiii", __y, None     , "lbz       %rD, %SIMM(%zrA)"         , { Rd = ppc_load<uint8_t>(ZRa + SIMM); }),
  o("100011.ddddd.aaaaa.iiiii.iiiiiiiiiii", __y, None     , "lbzu      %rD, %SIMM(%rA)"          , { Rd = ppc_load<uint8_t>(Ra + SIMM, &Ra); }),
  o("101010.ddddd.aaaaa.iiiii.iiiiiiiiiii", ___, None     , "lha       %rD, %SIMM(%zrA)"         , { Rd = ppc_load<int16_t>(ZRa + SIMM); }),
  o("101011.ddddd.aaaaa.iiiii.iiiiiiiiiii", ___, None     , "lhau      %rD, %SIMM(%rA)"          , { Rd = ppc_load<int16_t>(Ra + SIMM, &Ra); }),
  o("101000.ddddd.aaaaa.iiiii.iiiiiiiiiii", __y, None     , "lhz       %rD, %SIMM(%zrA)"         , { Rd = ppc_load<uint16_t>(ZRa + SIMM); }),
  o("101001.ddddd.aaaaa.iiiii.iiiiiiiiiii", __y, None     , "lhzu      %rD, %SIMM(%rA)"          , { Rd = ppc_load<uint16_t>(Ra + SIMM, &Ra); }),
  o("100000.ddddd.aaaaa.iiiii.iiiiiiiiiii", __y, None     , "lwz       %rD, %SIMM(%zrA)"         , { Rd = ppc_load<uint32_t>(ZRa + SIMM); }),
  o("100001.ddddd.aaaaa.iiiii.iiiiiiiiiii", __y, None     , "lwzu      %rD, %SIMM(%rA)"          , { Rd = ppc_load<uint32_t>(Ra + SIMM, &Ra); }),
  o("011111.ddddd.aaaaa.bbbbb.00010101110", ___, None     , "lbzx      %rD, %rA, %rB"            , { Rd = ppc_load<uint8_t>(ZRa + Rb); }),
  o("011111.ddddd.aaaaa.bbbbb.00011101110", ___, None     , "lbzux     %rD, %rA, %rB"            , { Rd = ppc_load<uint8_t>(Ra + Rb, &Ra); }),
  o("011111.ddddd.aaaaa.bbbbb.01010101110", ___, None     , "lhax      %rD, %rA, %rB"            , { Rd = ppc_load<int16_t>(ZRa + Rb); }),
  o("011111.ddddd.aaaaa.bbbbb.01011101110", ___, None     , "lhaux     %rD, %rA, %rB"            , { Rd = ppc_load<int16_t>(Ra + Rb, &Ra); }),
  o("011111.ddddd.aaaaa.bbbbb.01000101110", ___, None     , "lhzx      %rD, %rA, %rB"            , { Rd = ppc_load<uint16_t>(ZRa + Rb); }),
  o("011111.ddddd.aaaaa.bbbbb.01001101110", ___, None     , "lhzux     %rD, %rA, %rB"            , { Rd = ppc_load<uint16_t>(Ra + Rb, &Ra); }),
  o("011111.ddddd.aaaaa.bbbbb.00000101110", __y, None     , "lwzx      %rD, %rA, %rB"            , { Rd = ppc_load<uint32_t>(ZRa + Rb); }),
  o("011111.ddddd.aaaaa.bbbbb.00001101110", ___, None     , "lwzux     %rD, %rA, %rB"            , { Rd = ppc_load<uint32_t>(Ra + Rb, &Ra); }),
  o("101110.ddddd.aaaaa.iiiii.iiiiiiiiiii", __y, None     , "lmw       %rD, %SIMM(%rA)"          , { ppc_load_multiple(i.D, 31, 4, ZRa + SIMM); }),
  o("011111.ddddd.aaaaa.BBBBB.10010101010", ___, None     , "lswi      %rD, %zrA, ___"           , { ppc_load_multiple(i.D, i.B ? 32 : i.B, 1, ZRa); }),
  o("011111.ddddd.aaaaa.bbbbb.10000101010", ___, None     , "lswx      %rD, %zrA, %rB"           , { ppc_load_multiple(i.D, XER & 0xFF, 1, ZRa + Rb); }),
  o("011111.ddddd.aaaaa.bbbbb.00000101000", ___, None     , "lwarx     %rD, %zrA, %rB"           , { NOTIMP; }),
  o("100110.sssss.aaaaa.iiiii.iiiiiiiiiii", __y, None     , "stb       %rS, %SIMM(%zrA)"         , { ppc_store<uint8_t>(Rs, ZRa + SIMM); }),
  o("100111.sssss.aaaaa.iiiii.iiiiiiiiiii", __y, None     , "stbu      %rS, %SIMMO(%rA)"         , { ppc_store<uint8_t>(Rs, Ra + SIMM, &Ra); }),
  o("101100.sssss.aaaaa.iiiii.iiiiiiiiiii", __y, None     , "sth       %rS, %SIMM(%zrA)"         , { ppc_store<uint16_t>(Rs, ZRa + SIMM); }),
  o("101101.sssss.aaaaa.iiiii.iiiiiiiiiii", ___, None     , "sthu      %rS, %SIMM(%rA)"          , { ppc_store<uint16_t>(Rs, Ra + SIMM, &Ra); }),
  o("100100.sssss.aaaaa.iiiii.iiiiiiiiiii", __y, None     , "stw       %rS, %SIMM(%zrA)"         , { ppc_store<uint32_t>(Rs, ZRa + SIMM); }),
  o("100101.sssss.aaaaa.iiiii.iiiiiiiiiii", __y, None     , "stwu      %rS, %SIMM(%rA)"          , { ppc_store<uint32_t>(Rs, Ra + SIMM, &Ra); }),
  o("011111.sssss.aaaaa.bbbbb.00110101110", ___, None     , "stbx      %rS, %rA, %rB"            , { ppc_store<uint8_t>(Rs, ZRa + Rb); }),
  o("011111.sssss.aaaaa.bbbbb.00111101110", ___, None     , "stbux     %rS, %rA, %rB"            , { ppc_store<uint8_t>(Rs, Ra + Rb, &Ra); }),
  o("011111.sssss.aaaaa.bbbbb.01100101110", ___, None     , "sthx      %rS, %rA, %rB"            , { ppc_store<uint16_t>(Rs, ZRa + Rb); }),
  o("011111.sssss.aaaaa.bbbbb.01101101110", ___, None     , "sthux     %rS, %rA, %rB"            , { ppc_store<uint16_t>(Rs, Ra + Rb, &Ra); }),
  o("011111.sssss.aaaaa.bbbbb.00100101110", __y, None     , "stwx      %rS, %rA, %rB"            , { ppc_store<uint32_t>(Rs, ZRa + Rb); }),
  o("011111.sssss.aaaaa.bbbbb.00101101110", ___, None     , "stwux     %rS, %rA, %rB"            , { ppc_store<uint32_t>(Rs, Ra + Rb, &Ra); }),
  o("011111.ddddd.aaaaa.bbbbb.11000101100", ___, None     , "lhbrx     %rD, %rA, %rB"            , { ppc_loadr<uint16_t>(Rd, ZRa + Rb); }),
  o("011111.ddddd.aaaaa.bbbbb.10000101100", ___, None     , "lwbrx     %rD, %rA, %rB"            , { ppc_loadr<uint32_t>(Rd, ZRa + Rb); }),
  o("011111.sssss.aaaaa.bbbbb.11100101100", ___, None     , "sthbrx    %rS, %rA, %rB"            , { ppc_store<uint16_t>(revbytes(Rs), ZRa + Rb); }),
  o("011111.sssss.aaaaa.bbbbb.10100101100", ___, None     , "stwbrx    %rS, %rA, %rB"            , { ppc_store<uint32_t>(revbytes(Rs), ZRa + Rb); }),
  o("011111.sssss.aaaaa.bbbbb.11110101110", ___, None     , "stfiwx    %fS, %zrA, %rB"           , { ppc_store<uint32_t>(fpregs[i.B].i, ZRa + Rb); }),
  o("101111.sssss.aaaaa.bbbbb.11110101110", __y, None     , "stmw      %rS, %SIMM(%zrA)"         , { ppc_store_multiple(i.D, 31, 1, ZRa + SIMM); }),
  o("011111.sssss.aaaaa.BBBBB.10110101010", ___, None     , "stswi     %rS, %zrA, ___"           , { ppc_store_multiple(i.D, i.B ? 32 : i.B, 1, ZRa); }),
  o("011111.sssss.aaaaa.bbbbb.10100101010", ___, None     , "stswx     %rS, %zrA, %rB"           , { ppc_store_multiple(i.D, XER & 0xFF, 1, ZRa + Rb); }),
  o("011111.sssss.aaaaa.bbbbb.00100101101", ___, IMPRC    , "stwcx.    %rS, %zrA, %rB"           , { NOTIMP; }),
  o("110000.ddddd.aaaaa.iiiii.iiiiiiiiiii", __y, None     , "lfs       %fD, %SIMM(%zrA)"         , { SFRd = ppc_load<float>(ZRa + SIMM); }),
  o("110001.ddddd.aaaaa.iiiii.iiiiiiiiiii", ___, None     , "lfsu      %fD, %SIMM(%rA)"          , { SFRd = ppc_load<float>(Ra + SIMM, &Ra); }),
  o("011111.ddddd.aaaaa.bbbbb.10000101110", ___, None     , "lfsx      %fD, %rA, %rB"            , { SFRd = ppc_load<float>(ZRa + Rb); }),
  o("011111.ddddd.aaaaa.bbbbb.10001101110", ___, None     , "lfsux     %fD, %rA, %rB"            , { SFRd = ppc_load<float>(Ra + Rb, &Ra); }),
  o("110010.ddddd.aaaaa.iiiii.iiiiiiiiiii", __y, None     , "lfd       %fD, %SIMM(%zrA)"         , { DFRd = ppc_load<double>(ZRa + SIMM); }),
  o("110011.ddddd.aaaaa.iiiii.iiiiiiiiiii", ___, None     , "lfdu      %rD, %SIMM(%rA)"          , { DFRd = ppc_load<double>(Ra + SIMM, &Ra); }),
  o("011111.ddddd.aaaaa.bbbbb.10010101110", ___, None     , "lfdx      %fD, %rA, %rB"            , { DFRd = ppc_load<double>(ZRa + Rb); }),
  o("011111.ddddd.aaaaa.bbbbb.10011101110", ___, None     , "lfdux     %fD, %rA, %rB"            , { DFRd = ppc_load<double>(Ra + Rb, &Ra); }),
  o("110100.sssss.aaaaa.iiiii.iiiiiiiiiii", __y, None     , "stfs      %rD, %SIMM(%zrA)"         , { ppc_store<float>(SFRd, ZRa + SIMM); }),
  o("110101.sssss.aaaaa.iiiii.iiiiiiiiiii", __y, None     , "stfsu     %fD, %SIMM(%rA)"          , { ppc_store<float>(SFRd, Ra + SIMM, &Ra); }),
  o("110110.sssss.aaaaa.iiiii.iiiiiiiiiii", ___, None     , "stfd      %fD, %SIMM(%zrA)"         , { ppc_store<double>(DFRd, ZRa + SIMM); }),
  o("110111.sssss.aaaaa.iiiii.iiiiiiiiiii", ___, None     , "stfdu     %fD, %SIMM(%rA)"          , { ppc_store<double>(DFRd, Ra + SIMM, &Ra); }),
  o("011111.sssss.aaaaa.bbbbb.10100101110", ___, None     , "stfsx     %fS, %rA, %rB"            , { ppc_store<float>(SFRd, ZRa + Rb); }),
  o("011111.sssss.aaaaa.bbbbb.10101101110", ___, None     , "stfsux    %fS, %rA, %rB"            , { ppc_store<float>(SFRd, Ra + Rb, &Ra); }),
  o("011111.sssss.aaaaa.bbbbb.10110101110", ___, None     , "stfdx     %fS, %rA, %rB"            , { ppc_store<double>(DFRd, ZRa + Rb); }),
  o("011111.sssss.aaaaa.bbbbb.10111101110", ___, None     , "stfdux    %fS, %rA, %rB"            , { ppc_store<double>(DFRd, Ra + Rb, &Ra); }),
  o("111111.ddddd.00000.bbbbb.0100001000r", ___, RC       , "fabs%x    %fD, %fB"                 , { ppc_mov<double, FRC>(i, DFRd, fabs(DFRb)); }),
  o("111111.ddddd.00000.bbbbb.0010001000r", ___, RC       , "fnabs%x   %fD, %fB"                 , { ppc_mov<double, FRC>(i, DFRd, -fabs(DFRb)); }),
  o("111111.ddddd.00000.bbbbb.0000101000r", ___, RC       , "fneg%x    %fD, %fB"                 , { ppc_mov<double, FRC>(i, DFRd, -DFRb); }),
  o("111111.ddddd.aaaaa.bbbbb.0000010101r", ___, RC       , "fadd%x    %fD, %fA, %fB"            , { ppc_mov<double, FRC>(i, DFRd, DFRa + DFRb); }),
  o("111111.ddddd.aaaaa.bbbbb.0000010100r", ___, RC       , "fsub%x    %fD, %fA, %fB"            , { ppc_mov<double, FRC>(i, DFRd, DFRa - DFRb); }),
  o("111111.ddddd.aaaaa.00000.ccccc11001r", ___, RC       , "fmul%x    %fD, %fA, %fC"            , { ppc_mov<double, FRC>(i, DFRd, DFRa * DFRb); }),
  o("111111.ddddd.aaaaa.bbbbb.0000010010r", ___, RC       , "fdiv%x    %fD, %fA, %fB"            , { ppc_mov<double, FRC>(i, DFRd, DFRa / DFRb); }),
  o("111011.ddddd.aaaaa.bbbbb.0000010101r", ___, RC       , "fadds%x   %fD, %fA, %fB"            , { ppc_mov<float, FRC>(i, SFRd, SFRa + SFRb); }),
  o("111011.ddddd.aaaaa.bbbbb.0000010100r", ___, RC       , "fsubs%x   %fD, %fA, %fB"            , { ppc_mov<float, FRC>(i, SFRd, SFRa - SFRb); }),
  o("111011.ddddd.aaaaa.00000.ccccc11001r", ___, RC       , "fmuls%x   %fD, %fA, %fC"            , { ppc_mov<float, FRC>(i, SFRd, SFRa * SFRb); }),
  o("111011.ddddd.aaaaa.bbbbb.0000010010r", ___, RC       , "fdivs%x   %fD, %fA, %fB"            , { ppc_mov<float, FRC>(i, SFRd, SFRa / SFRb); }),
  o("111111.ddddd.00000.bbbbb.0001001000r", __y, RC       , "fmr%x     %fD, %fB"                 , { ppc_mov<double, FRC>(i, DFRd, DFRb); }),
  o("111111.DDD00.aaaaa.bbbbb.00001000000", ___, None     , "fcmpo     %crD, %fA, %fB"           , { setrc(i, DFRa - DFRb, i.D); }),
  o("111111.DDD00.aaaaa.bbbbb.00000000000", ___, None     , "fcmpu     %crD, %fA, %fB"           , { setrc(i, DFRa - DFRb, i.D); }),
  o("111111.ddddd.00000.bbbbb.0000001110r", ___, RC       , "fctiw%x   %fD, %fB"                 , { NOTIMP; }),
  o("111111.ddddd.00000.bbbbb.0000001111r", ___, RC       , "fctiwz%x  %fD, %fB"                 , { NOTIMP; }),
  o("111111.ddddd.aaaaa.bbbbb.ccccc11101r", ___, RC       , "fmadd%x   %fD, %fA, %fB, %fC"       , { ppc_mla<double>(DFRd, DFRa,  DFRc, DFRb); }),
  o("111011.ddddd.aaaaa.bbbbb.ccccc11101r", ___, RC       , "fmadds%x  %fD, %fA, %fB, %fC"       , { ppc_mla<float>(SFRd,  SFRa,  SFRc, SFRb); }),
  o("111111.ddddd.aaaaa.bbbbb.ccccc11100r", ___, RC       , "fmsub%x   %fD, %fA, %fB, %fC"       , { ppc_mla<double>(DFRd, DFRa,  DFRc,-DFRb); }),
  o("111011.ddddd.aaaaa.bbbbb.ccccc11100r", ___, RC       , "fmsubs%x  %fD, %fA, %fB, %fC"       , { ppc_mla<float>(SFRd,  SFRa,  SFRc,-SFRb); }),
  o("111111.ddddd.aaaaa.bbbbb.ccccc11111r", ___, RC       , "fnmadd%x  %fD, %fA, %fC, %fB"       , { ppc_mla<double>(DFRd,-DFRa,  DFRc,-DFRb); }),
  o("111011.ddddd.aaaaa.bbbbb.ccccc11111r", ___, RC       , "fnmadds%x %fD, %fA, %fC, %fB"       , { ppc_mla<float>(SFRd, -SFRa,  SFRc,-SFRb); }),
  o("111111.ddddd.aaaaa.bbbbb.ccccc11110r", ___, RC       , "fnmsub%x  %fD, %fA, %fC, %fB"       , { ppc_mla<double>(DFRd,-DFRa,  DFRc, DFRb); }),
  o("111011.ddddd.aaaaa.bbbbb.ccccc11110r", ___, RC       , "fnmsubs%x %fD, %fA, %fC, %fB"       , { ppc_mla<float>(SFRd, -SFRa,  SFRc, SFRb); }),
  o("111011.ddddd.00000.bbbbb.0000011000r", ___, RC       , "fres%x    %fD, %fB"                 , { NOTIMP; }),
  o("111111.ddddd.00000.bbbbb.0000001100r", ___, RC       , "frsp%x    %fD, %fB"                 , { NOTIMP; }),
  o("111111.ddddd.00000.bbbbb.0000011010r", ___, RC       , "frsqrte%x %fD, %fB"                 , { NOTIMP; }),
  o("111111.ddddd.aaaaa.bbbbb.ccccc10111r", ___, RC       , "fsel%x    %fD, %fA, %fC, %fB"       , { DFRd = (DFRa >= 0.0 ? DFRc : DFRb); }),
  o("011111.00000.aaaaa.bbbbb.00010101100", __y, None     , "dcbf      %rA, %rB"                 , { printf("@@dcbf\n"); }),
  o("011111.00000.aaaaa.bbbbb.01000101100", ___, None     , "dcbt      %rA, %rB"                 , { printf("@@dcbt\n"); }),
  o("011111.00000.aaaaa.bbbbb.01110101100", __y, None     , "dcbi      %rA, %rB"                 , { printf("@@dcbi\n"); }),
  o("011111.00000.aaaaa.bbbbb.00001101100", ___, None     , "dcbst     %rA, %rB"                 , { printf("@@dcbst\n"); }),
  o("011111.00000.aaaaa.bbbbb.00111101100", ___, None     , "dcbtst    %rA, %rB"                 , { printf("@@dcbtst\n"); }),
  o("011111.00000.aaaaa.bbbbb.11111101100", ___, None     , "dcbz      %rA, %rB"                 , { printf("@@dcbz\n"); }),
  o("000100.00000.aaaaa.bbbbb.11111101100", ___, None     , "dcbz_l    %rA, %rB"                 , { printf("@@dcbz_l\n"); }),
  o("011111.00000.00000.00000.11010101100", ___, None     , "eieio"                              , { printf("@@eieio\n"); }),
  o("011111.00000.aaaaa.bbbbb.11110101100", __y, None     , "icbi      %rA, %rB"                 , { printf("@@icbi\n"); }),
  o("010011.00000.00000.00000.00100101100", __2, None     , "isync"                              , { printf("@@isync\n"); }),
  o("011111.00000.00000.00000.10010101100", __y, None     , "sync"                               , { printf("@@sync\n"); }),
  o("011111.00000.00000.bbbbb.01001100100", ___, None     , "tlbie     %rB"                      , { NOTIMP; }),
  o("011111.00000.00000.00000.10001101100", ___, None     , "tlbsync"                            , { printf("@@tlbsync\n"); }),
  o("011111.OOOOO.aaaaa.bbbbb.00000001000", ___, None     , "tw        ___, %rA, %rB"            , { NOTIMP; }),
  o("000011.OOOOO.aaaaa.iiiii.iiiiiiiiiii", __2, None     , "twi       ___, %rA, %SIMM"          , { NOTIMP; }),
  o("011111.ddddd.aaaaa.bbbbb.01001101100", ___, None     , "eciwx     %rD, %rA, %rB"            , { NOTIMP; }),
  o("011111.sssss.aaaaa.bbbbb.01101101100", ___, None     , "ecowx     %rS, %rA, %rB"            , { NOTIMP; }),
  { },
};
#undef f

std::map <uint32_t, opcode_t *> opmap;

constexpr uint32_t opfn(const uint32_t op)
{
  const uint32_t fn = (op >> 26) & 0x3F;

  if (fn == 19 || fn == 31 || fn == 59 || fn == 63)
    return (fn << 16) + (op & 0x7FF);
  return fn;
}

void addop(int n, opcode_t *o) {
  if (opmap[n] != NULL) {
    printf("opmap: %.8x haz: %s:%s\n", n, opmap[n]->mnem, o->mnem);
  }
  opmap[n] = o;
}

void gentbl()
{
  for (int i = 0; optab[i].mnem; i++) {
    uint32_t fn;

    fn = opfn(optab[i].bits);

    /* if function has OE/RC bits it can have up to 4 masks.
     * 0x7ff
     * 0x7fe
     * 0x3ff
     * 0x3fe
     */
    addop(fn, &optab[i]);
    if (optab[i].flag & OE) {
      addop(fn | OE_bit, &optab[i]);
    }
    if (optab[i].flag & RC) {
      addop(fn | RC_bit, &optab[i]);
    }
    if ((optab[i].flag & (OE|RC)) == (OE|RC)) {
      addop(fn | OE_bit | RC_bit, &optab[i]);
    }
  }
  for (auto h : opmap) {
    printf("%.8x %s\n", h.first, h.second->mnem);
  }
  printf("Map size is: %x\n", opmap.size());
}

const char *regname(const int n, const bool zra = false)
{
  static char rn[32];

  if (zra && !n)
    return "0x0";
  if (n >= 0x100) {
    snprintf(rn, sizeof(rn), "fr%d", n);
    return rn;
  }
  snprintf(rn, sizeof(rn), "r%d", n);
  return rn;
}

/* Disassemble opcode from mnemonic string */
const char *ppc_dis(uint32_t op, uint32_t flag, const char *src)
{
  // ______.ddddd.aaaaa.bbbbb.ccccc.mmmmm._
  const uint32_t rd = (op >> 21) & 0x1F;
  const uint32_t ra = (op >> 16) & 0x1F;
  const uint32_t rb = (op >> 11) & 0x1F;
  const uint32_t rc = (op >> 6) & 0x1F;
  const uint32_t me = (op >> 1) & 0x1F;
  static char dstr[128];
  char oe_rc[32], *dst = dstr;

  /* Prepare zra and OERC bits */
  snprintf(oe_rc, sizeof(oe_rc), "%s%s",
	   ((flag & OE_bit) ? "o" : ""),
	   ((flag & RC_bit) ? "." : ""));
  while (*src) {
    if (replace(&src, "%rA",  &dst, regname(ra)) ||
	replace(&src, "%zrA", &dst, regname(ra, true)) ||
        replace(&src, "%rB",  &dst, regname(rb)) ||
        replace(&src, "%rD",  &dst, regname(rd)) ||
	replace(&src, "%rS",  &dst, regname(rd)) ||
        replace(&src, "%fA",  &dst, regname(0x100 + ra)) ||
        replace(&src, "%fB",  &dst, regname(0x100 + rb)) ||
        replace(&src, "%fD",  &dst, regname(0x100 + rd)) ||
	replace(&src, "%fC",  &dst, regname(0x100 + rc)) ||
        replace(&src, "%crD", &dst, "0x%x", rd) ||
        replace(&src, "%crA", &dst, "0x%x", ra) ||
        replace(&src, "%crB", &dst, "0x%x", rb) ||
        replace(&src, "%iB",  &dst, "0x%x", rb) ||
        replace(&src, "%MB",  &dst, "0x%x", rc) ||
        replace(&src, "%ME",  &dst, "0x%x", me) ||
        replace(&src, "%x",   &dst, "%-2s", oe_rc) ||
        replace(&src, "%SIMM",&dst, "0x%x", (int16_t)op) ||
        replace(&src, "%UIMM",&dst, "0x%x", (uint16_t)op)) {
      continue;
    }
    *dst++ = *src++;
  }
  *dst = 0;
  return dstr;
}

void cpu_showregs() {
  printf("PC:%.8x ------ registers : %.8x XER:%.8x\n", PC, ppc_get_cr(), XER);
  for (int i=0; i<32; i++) {
    printf("R%.2d: %.8x ", i, regs[i]);
    if ((i & 7) == 7)
      printf("\n");
  }
  for (int i=0; i<32; i++) {
    printf("FP%.2d: %8lf ", i, fpregs[i].d);
    if ((i & 7) == 7)
      printf("\n");
  }
  printf("CTR:%.8x LINK:%.8x SR:%.8x MSR:%.8x\n", CTR, LINK, SR, MSR);
}

void showop()
{
  for (int i = 0; optab[i].mnem; i++) {
    printf("%-8s: %s\n", optab[i].used ? "used" : "unused", optab[i].mnem);
  }
}

void ppc_exec(uint32_t op, uint32_t mask = 0)
{
  opcode_t *opc;
  uint32_t flag = 0;

  opc = opmap[opfn(op)];
  opc->used = true;
  
  printf("================= PPCTEST: %.8x %.8x\n", PC, op);
  if (opc == NULL || opc->eval == NULL) {
    printf("%.8x (%d,%d): Unknown\n", op, (op >> 26), (op & 0x7FE) >> 1);
    return;
  }
  
  cpu_showregs();
  instr_t i(op | mask);
  /* Setup instruction flag bits */

  /* AA bit is reversed: 0 = add, 1 = don't add */
  if ((opc->flag & AA) && !(op & AA_bit))
    i.flag |= AA;
  if ((opc->flag & LK) && (op & LK_bit))
    i.flag |= LK;
  if ((opc->flag & OE) && (op & OE_bit))
    i.flag |= OE;

  /* Explicit or implied RC flag */
  if ((opc->flag & RC) && (op & RC_bit))
    i.flag |= RC;
  if (opc->flag & IMPRC)
    i.flag |= RC;

  if (opc->flag & OE)
    flag |= (op & OE_bit);
  if (opc->flag & RC)
    flag |= (op & RC_bit);
  printf("Exec: %s\n", ppc_dis(op, flag, opc->mnem));
  opc->eval(i);

  /* Set result flags */
  if (i.flag & OE) {
    printf("SETOV : %x\n", i.overflow);
    setov(i.overflow);
  }
  /* Set flags after operation is complete */
  if (i.flag & RC) {
    printf("SETRC: %.8x\n", i.dst);
    CR_EQ = (i.dst == 0);
    CR_LT = ((int32_t)i.dst < 0);
    CR_GT = ((int32_t)i.dst > 0);
    CR_OV = (bool)XER_SO;
  }
}

#include "ppc.h"
#include <map>

#define fnname(x) ""
std::map<uint32_t,const char *> m;

void dumpcfg(uint32_t off, uint32_t base, uint32_t size)
{
  dstk stk(size, printf);
  uint32_t op, fn, nxt[2], ipc;

  printf("dumpcfg----\n");
  stk.setbase(base);
  stk.push(off, 1, dstk::PENDING, "first");
  while ((off = stk.pop()) != -1) {
    printf("\n------------------------------ %.8x [%s]\n",
	   off, fnname(off));
    PC = off;
    do {
      ipc = PC;

      op = cpu_read32(PC, dstk::CODE);
      PC += 4;
      
      fn = opfn(op);

      printf("%.8x: %.8x %.8x %s", ipc, op, fn, (const char *)m[ipc]);
      nxt[0] = PC;
      nxt[1] = -1;
      switch (fn) {
      case 0x00130020:
	// blr : terminate
	nxt[0] = -1;
	break;
      case 0x10:
	// conditional jmp
	nxt[1] = signex(op & 0xFFFF, 16);
	if ((op & 0x2) == 0)
	  nxt[1] += ipc;
	printf("bcc : %x\n", nxt[1]);
	break;
      case 0x12:
	// procedure call
	nxt[1] = signex(op & 0x03FFFFFC, 26);
	if ((op & 0x2) == 0)
	  nxt[1] += ipc;
	printf("b : %x\n", nxt[1]);
	break;
      }
      /* Push next addresses */
      stk.push(ipc, 4, dstk::VISITED);
      for (int n = 0; n < 2; n++) {
	stk.push(nxt[n], 1, dstk::PENDING, "nxt");
      }
    } while (nxt[0] != -1 && nxt[1] == -1);
  }
  stk.showstk(128);
  exit(0);
}

int loaddol(const char *file)
{
  int fd;
  uint8_t *buf;
  size_t size;
  uint32_t entry, op;
  struct {
    uint32_t offset;
    uint32_t address;
    uint32_t size;
  } text[8], data[12], bss;
  
  fd = open(file, O_RDONLY);
  if (fd < 0)
    return -1;

  thegame.init();

  size = lseek(fd, 0, SEEK_END);
  buf = (uint8_t *)malloc(size);
  memset(buf, 0, size);
  pread(fd, buf, size, 0);
  hexdump(buf, size, 32);
  close(fd);

  assert(size >= 0xe4);
  for (int i = 0; i < 7; i++) {
    text[i].offset  = get32be(buf + (i * 4) + 0x00);
    text[i].address = get32be(buf + (i * 4) + 0x48);
    text[i].size    = get32be(buf + (i * 4) + 0x90);
    printf("text%2d: %.8x %.8x %.8x\n", i, text[i].address, text[i].size, text[i].offset);
    if (text[i].size) {
      entry = xlate(IBAT0U, text[i].address);
      thegame.write(entry, buf + text[i].offset, text[i].size);
    }
  }
  for (int i = 0; i < 11; i++) {
    data[i].offset  = get32be(buf + (i * 4) + 0x1c);
    data[i].address = get32be(buf + (i * 4) + 0x64);
    data[i].size    = get32be(buf + (i * 4) + 0xac);
    printf("data%2d: %.8x %.8x %.8x\n", i, data[i].address, data[i].size, data[i].offset);
  }
  bss.address = get32be(buf + 0xd8);
  bss.size = get32be(buf + 0xdc);
  entry = get32be(buf + 0xe0);
  
  printf("bss   : %.8x %.8x\n", bss.address, bss.size);
  printf("entry : %.8x : %.8x\n", entry, xlate(IBAT0U, entry));

  PC = entry;
  for(;;) {
    int SPC = PC;
    op = cpu_fetch();
    printf("====== %.8x | %.8x\n", SPC, op);
    ppc_exec(op);
  }
  return 0;
}

void loadfile(const char *file)
{
  FILE *fp;
  char  line[256];
  uint32_t addr, a,b,c,d, op, maddr = 0;
  
  if ((fp = fopen(file, "r")) == NULL)
    exit(0);
  thegame.init();
  for (int i = 0; i < 32; i++) {
    regs[i] = i * 0x01010101;
    fpregs[i].d = 1.1 * i;
  }
  while (fgets(line, sizeof(line), fp) != NULL) {
    if (sscanf(line, "%x: %x %x %x %x", &addr, &a, &b, &c, &d) == 5) {
      op = (a << 24) | (b << 16) | (c << 8) | d;
      addr = addr & 0xFFFFF;
      cpu_write32(addr, op);
      m[addr] = strdup(line);
      if (addr > maddr)
	maddr = addr;
    }
  }
  printf("maddr: %x\n", maddr);
  dumpcfg(0x0004000, 0x0004000, maddr - 0x0000000);

  PC = 0x80004000;
  for (int j = 0;; j++) {
    int IPC = PC;
    
    op = cpu_read32(PC, dstk::CODE);
    printf("######## %.8x %s", PC, (const char *)(m[IPC]));
    if (strstr((char *)m[IPC], "memcpy")) {
      printf("== memcpy(%.8x,%.8x,%.8x)\n",
             regs[3], regs[4], regs[5]);
    }
    if (strstr((char *)m[IPC], "memset")) {
      printf("== memcpy(%.8x,%.8x,%.8x)\n",
             regs[3], regs[4], regs[5]);
    }
    ppc_exec(op);
    if (IPC == PC)
      PC += 4;
    printf("\n");
  }
}

bool cpu_irq(int n)
{
}

int main(int argc, char *argv[])
{
  uint32_t op;

  setbuf(stdout, NULL);
  gentbl();
  if (argc > 3) {
    loadfile("wiiasteroids/wspr.asm");
    //loaddol("/Users/jordan/Source/github/rustcube/tests/data/optipong.dol");
    exit(0);
  }

  xer_test();
  if (argc < 2) {
    exit(1);
  }
  op = strtoull(argv[1], NULL, 0);
  // R[3] = R[1] op R[2]
  printf("OP: %.8x : ", op);
  for (int i=2; i<argc; i++) {
    printf("%s ",argv[i]);
  }
  printf("\n");

  regs[0] = 0x0;
  regs[1] = 0x11;
  regs[2] = 0x22;
  regs[3] = 0x55;
  ppc_set_cr(0x0);
  CR[1] = false;

  for (int i = 0; i < 32; i++)
    regs[i] = i * 0x01010101;
  //instr_t instr(0x3, 0x2, 0x1);
  instr_t instr(op);
  XER_CA = 1;
  ppc_exec(op);
}
