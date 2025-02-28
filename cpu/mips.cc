/*
  mips.cc : MIPS CPU emulator

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
#include <fcntl.h>
#include <inttypes.h>
#include <assert.h>
#include "cpu.h"
#include "util.h"

extern const char *fnname(uint32_t addr);
extern void mips_syscall(uint32_t call);

/*==================================
 * CPU Registers
 *==================================*/
// v[0..1]=R2..R3
// a[0..3]=R4..R7
// t[0..7]=R8..R15
// s[0..7]=R16..R23
// t[8..9]=R24..R25
// k[0..1]=R26..R27
// gp=R28
// sp=R29
// fp=R30
// ra=R31
uint32_t copr[4][64];
struct mips_cpu {
  uint32_t regs[35];

  uint32_t& sp=regs[29];
  uint32_t& PC=regs[34];
  uint32_t& ra=regs[31];
  uint32_t& rlo=regs[32];
  uint32_t& rhi=regs[33];

  uint32_t jmpslot[2];
};
extern void mips_copr(mips_cpu *c, uint32_t op, int id);
#if 0
static uint32_t  regs[35];
static uint32_t  copr[4][64];

static uint32_t& sp=regs[29];
static uint32_t& ra=regs[31];
static uint32_t& rlo=regs[32];
static uint32_t& rhi=regs[33];
static uint32_t& PC=regs[34];

static uint32_t jmpslot[2];
#endif

static const char *regname[] = {
  "zero", "at", "v0", "v1", "a0", "a1", "a2", "a3",
  "t0",   "t1", "t2", "t3", "t4", "t5", "t6", "t7",
  "s0",   "s1", "s2", "s3", "s4", "s5", "s6", "s7",
  "t8",   "t9", "k0", "k1", "gp", "sp", "fp", "ra"
};

/* MIPS opcode encoding
 *  000000ss.sssttttt.dddddiii.iiffffff register
 *  ooooooss.sssttttt.iiiiiiii.iiiiiiii immediate
 *  ooooooii.iiiiiiii.iiiiiiii.iiiiiiii jump
 *  11xxccss.sssttttt.iiiiiiii.iiiiiiii coproc lwc/swc
 */

/* Extract parts of opcode */
#define rs   ((op >> 21) & 0x1F)
#define rt   ((op >> 16) & 0x1F)
#define rd   ((op >> 11) & 0x1F)

/* Unsigned values */
#define Rs   c->regs[rs]
#define Rt   c->regs[rt]
#define Rd   c->regs[rd]
#define CRt  copr[(op >> 26) & 3][rt]

/* Signed values */
#define SRs (int32_t)Rs
#define SRt (int32_t)Rt

/* Immediate values */
#define i5  ((op >> 6) & 0x1F)
#define i16 (op & 0xFFFF)
#define s16 signex(op, 16)
#define j26 _j26(c, op)
#define j16 _j16(c, op)

int trace, SPC;

static uint32_t _j26(mips_cpu *c, uint32_t op)
{
  op = (op & 0x03FFFFFF);
  return (op * 4) + (c->PC & 0xF0000000);
}

static uint32_t _j16(mips_cpu *c, uint32_t op)
{
  op = s16;
  return (op * 4) + c->PC;
}

/* Opcode formats:
 *  ttttt  = Rt
 *  sssss  = Rs
 *  ddddd  = Rd
 *  oooooo = opcode
 *  fffff  = opcode function
 *
 *     : oooo.ooss.ssst.tttt.dddd.daaa.aaff.ffff
 *  00 : 0000.00--.----.----.----.----.--ff.ffff
 *  01 : 0000.01--.---f.ffff.----.----.----.---- 
 *  xx : oooo.oo--.----.----.----.----.----.----
 */
static const int opfn(uint32_t op) {
  const uint32_t opcode = (op >> 26) & 0x3F;
  const uint32_t func1  = (op >> 16) & 0x1F;
  const uint32_t func   = (op & 0x3F);

  if (opcode == 0)
    return func;
  else if (opcode == 1)
    return (0x40 + func1);
  return (0x100 + opcode);
};

/*==================================================================*
 * Opcode Handlers
 *==================================================================*/
static void mips_set(mips_cpu *c, uint32_t &d, uint32_t src1, uint32_t src2)  {
  d = src1;
}
static void mips_add(mips_cpu *c, uint32_t &d, uint32_t src1, uint32_t src2)  {
  d = src1 + src2;
}
static void mips_addu(mips_cpu *c, uint32_t &d, uint32_t src1, uint32_t src2) {
  d = src1 + src2;
}
static void mips_sub(mips_cpu *c, uint32_t &d, uint32_t src1, uint32_t src2)  {
  d = src1 - src2;
}
static void mips_subu(mips_cpu *c, uint32_t &d, uint32_t src1, uint32_t src2) {
  d = src1 - src2;
}
static void mips_and(mips_cpu *c, uint32_t &d, uint32_t src1, uint32_t src2)  {
  d = src1 & src2;
}
static void mips_or(mips_cpu *c, uint32_t &d, uint32_t src1, uint32_t src2)   {
  d = src1 | src2;
}
static void mips_xor(mips_cpu *c, uint32_t &d, uint32_t src1, uint32_t src2)  {
  d = src1 ^ src2;
}
static void mips_nor(mips_cpu *c, uint32_t &d, uint32_t src1, uint32_t src2)  {
  d = ~(src1 | src2);
}

/* Load/Store */
static void mips_lw(mips_cpu *c, uint32_t &d, uint32_t src1, uint32_t src2=0)  {
  d = cpu_read32(src1+src2);
}
static void mips_lhu(mips_cpu *c, uint32_t &d, uint32_t src1, uint32_t src2=0) {
  d = cpu_read16(src1+src2);
}
static void mips_lbu(mips_cpu *c, uint32_t &d, uint32_t src1, uint32_t src2=0) {
  d = cpu_read8(src1+src2);
}
static void mips_lh(mips_cpu *c, uint32_t &d, uint32_t src1, uint32_t src2=0)  {
  d = (int16_t)cpu_read16(src1+src2);
}
static void mips_lb(mips_cpu *c, uint32_t &d, uint32_t src1, uint32_t src2=0)  {
  d = (int8_t)cpu_read8(src1+src2);
}

/* Ugh, ugly */
static void mips_lwl(mips_cpu *c, uint32_t &d, uint32_t src1, uint32_t src2=0)  {
  const uint32_t shift[] = { 24, 16, 8, 0 };
  const uint32_t _mask[] = { 0xFF000000, 0xFFFF0000, 0xFFFFFF00, 0xFFFFFFFF };
  uint32_t val = 0;
  uint32_t mask = 0;
  int n;

  src1 += src2;
  n = (src1 & 3);
  mask = _mask[n];
  for (int i = 0; i <= n; i++) {
    val |= cpu_read8(src1--) << shift[i];
  }
  rmw(d, val, mask);
}

/* Ugh, ugly */
static void mips_lwr(mips_cpu *c, uint32_t &d, uint32_t src1, uint32_t src2=0)  {
  const uint32_t shift[] = { 24, 16, 8, 0 };
  uint32_t val = 0;
  uint32_t mask = 0;
  int n;

  src1 += src2;
  n = (src1 & 3);
  for (int i = 3; i >= n; i--) {
    val |= cpu_read8(src1++) << shift[i];
    mask |= 0xff << shift[i];
  }
  rmw(d, val, mask);
}

static void mips_sw(mips_cpu *c, uint32_t &d, uint32_t src1, uint32_t src2=0)  {
  cpu_write32(src1+src2, d);
}
static void mips_sh(mips_cpu *c, uint32_t &d, uint32_t src1, uint32_t src2=0)  {
  cpu_write16(src1+src2, d);
}
static void mips_sb(mips_cpu *c, uint32_t &d, uint32_t src1, uint32_t src2=0)  {
  cpu_write8(src1+src2, d);
}

static void mips_swl(mips_cpu *c, uint32_t &d, uint32_t src1, uint32_t src2=0) {
  uint32_t tmp = d;

  src1 += src2;
  for (int n = src1 & 3; n >= 0; n--) {
    cpu_write8(src1--, tmp >> 24);
    tmp <<= 8;
  }
}

static void mips_swr(mips_cpu *c, uint32_t &d, uint32_t src1, uint32_t src2=0) {
  uint32_t tmp = d;

  src1 += src2;
  for (int n = src1 & 3; n <= 3; n++) {
    cpu_write8(src1++, tmp);
    tmp >>= 8;
  }
}

static void mips_shl(mips_cpu *c, uint32_t &d, uint32_t src1, uint32_t src2)  {
  d = src1 << src2;
}
static void mips_shr(mips_cpu *c, uint32_t &d, uint32_t src1, uint32_t src2)  {
  d = src1 >> src2;
}
static void mips_sar(mips_cpu *c, uint32_t &d, uint32_t src1, uint32_t src2)  {
  d = (int32_t)src1 >> src2;
}
static void mips_slt(mips_cpu *c, uint32_t &d, uint32_t src1, uint32_t src2)  {
  d = !!((int32_t)src1 < (int32_t)src2);
}
static void mips_sltu(mips_cpu *c, uint32_t &d, uint32_t src1, uint32_t src2) {
  d = !!(src1 < src2);
}

static void mips_mult(mips_cpu *c, int32_t src1, int32_t src2) {
  uint64_t r = (int64_t)src1 * src2;

  c->rhi = (uint32_t)(r >> 32);
  c->rlo = (uint32_t)r;
}

static void mips_multu(mips_cpu *c, uint32_t src1, uint32_t src2) {
  uint64_t r = (uint64_t)src1 * src2;

  c->rhi = (uint32_t)(r >> 32);
  c->rlo = (uint32_t)r;
}

static void mips_div(mips_cpu *c, int32_t a, int32_t b) {
  c->rlo = a / b;
  c->rhi = a % b;
}

static void mips_divu(mips_cpu *c, uint32_t a, uint32_t b) {
  c->rlo = a / b;
  c->rhi = a % b;
}

/* Set MIPS PC jump slot, optionally setting link register */
static void mips_setpc(mips_cpu *c, bool test, uint32_t npc, uint32_t *tra)
{
  if (test) {
    if (trace) 
      printf("---> Set JMPSLOT = %.8x\n", npc);
    c->jmpslot[1] = npc;
    if (tra) {
      /* Set link register */
      *tra = c->PC + 4;
    }
  }
}

typedef void (*evalfn_t)(mips_cpu *c, uint32_t&, uint32_t, uint32_t);

struct opcode_t {
  int flag;
  const char *mnem;
  evalfn_t fn;

  uint32_t *dest;
  uint32_t *arg0;
  uint32_t *arg1;
};

enum {
  ________ = 0x0,
  RdRtI5,
  RdRtRs,
  RdRsRt,
  RtRsS16,
  RtRsI16,
  RtI16,
  CRtRsS16,

  copr0 = 0x1000,
  copr1,
  copr2,
  copr3,
};


static uint32_t _rs;
static uint32_t _rd;
static uint32_t _rt;
static uint32_t _i5;
static uint32_t _i16;
static uint32_t _s16;

constexpr const opcode_t mkop(evalfn_t fn, const int flag, const char *mnem = NULL) {
  opcode_t o = {};

  o.flag = flag;
  o.mnem = mnem;
  o.fn = fn;
  return o;
}

static const opcode_t optab[] = {
  /* Opcode Mode 0 */
  [0x000] = mkop(mips_shl,  RdRtI5,   "sll    %Rd, %Rt, %i5"),
  [0x002] = mkop(mips_shr,  RdRtI5,   "srl    %Rd, %Rt, %i5"),
  [0x003] = mkop(mips_sar,  RdRtI5,   "sra    %Rd, %Rt, %i5"),
  [0x004] = mkop(mips_shl,  RdRtRs,   "sllv   %Rd, %Rt, %Rs"),
  [0x006] = mkop(mips_shr,  RdRtRs,   "srlv   %Rd, %Rt, %Rs"),
  [0x007] = mkop(mips_sar,  RdRtRs,   "srav   %Rd, %Rt, %Rs"),
  [0x008] = mkop(NULL,      ________, "jr     %Rs"),
  [0x009] = mkop(NULL,      ________, "jalr   %Rs.%Rd"),
  [0x00c] = mkop(NULL,      ________),
  [0x00d] = mkop(NULL,      ________),
  [0x010] = mkop(mips_set,  ________, "mfhi   %Rd"),
  [0x011] = mkop(mips_set,  ________, "mthi   %Rs"),
  [0x012] = mkop(mips_set,  ________, "mflo   %Rd"),
  [0x013] = mkop(mips_set,  ________, "mtlo   %Rs"),
  [0x018] = mkop(NULL,      ________, "mult   %Rs, %Rt"),
  [0x019] = mkop(NULL,      ________, "multu  %Rs, %Rt"),
  [0x01a] = mkop(NULL,      ________, "div    %Rs, %Rt"),
  [0x01b] = mkop(NULL,      ________, "divu   %Rs, %Rt"),
  [0x020] = mkop(mips_add,  RdRsRt,   "add    %Rd, %Rs, %Rt"),
  [0x021] = mkop(mips_addu, RdRsRt,   "addu   %Rd, %Rs, %Rt"),
  [0x022] = mkop(mips_sub,  RdRsRt,   "sub    %Rd, %Rs, %Rt"),
  [0x023] = mkop(mips_subu, RdRsRt,   "subu   %Rd, %Rs, %Rt"),
  [0x024] = mkop(mips_and,  RdRsRt,   "and    %Rd, %Rs, %Rt"),
  [0x025] = mkop(mips_or,   RdRsRt,   "or     %Rd, %Rs, %Rt"),
  [0x026] = mkop(mips_xor,  RdRsRt,   "xor    %Rd, %Rs, %Rt"),
  [0x027] = mkop(mips_nor,  RdRsRt,   "nor    %Rd, %Rs, %Rt"),
  [0x02a] = mkop(mips_slt,  RdRsRt,   "slt    %Rd, %Rs, %Rt"),
  [0x02b] = mkop(mips_sltu, RdRsRt,   "sltu   %Rd, %Rs, %Rt"),
  /* Opcode Mode 1 */
  [0x040] = mkop(NULL,      ________, "bltz   %Rs, %j16"),
  [0x041] = mkop(NULL,      ________, "bgez   %Rs, %j16"),
  [0x050] = mkop(NULL,      ________, "bltzal %Rs, %j16"),
  [0x051] = mkop(NULL,      ________, "bgezal %Rs, %j16"),
  /*  Opcode Mode 2 */
  [0x102] = mkop(NULL,      ________, "j      %j26"),
  [0x103] = mkop(NULL,      ________, "jal    %j26"),
  [0x104] = mkop(NULL,      ________, "beq    %Rs, %Rt, %j16"),
  [0x105] = mkop(NULL,      ________, "bne    %Rs, %Rt, %j16"),
  [0x106] = mkop(NULL,      ________, "blez   %Rs, %j16"),
  [0x107] = mkop(NULL,      ________, "bgtz   %Rs, %j16"),
  [0x108] = mkop(mips_add,  RtRsS16 , "addi   %Rt, %Rs, %i16"),
  [0x109] = mkop(mips_addu, RtRsS16 , "addiu  %Rt, %Rs, %i16"),
  [0x10a] = mkop(mips_slt,  RtRsS16,  "slti   %Rt, %Rs, %i16"),
  [0x10b] = mkop(mips_sltu, RtRsS16,  "sltiu  %Rt, %Rs, %i16"),
  [0x10c] = mkop(mips_and,  RtRsI16,  "andi   %Rt, %Rs, %i16"),
  [0x10d] = mkop(mips_or,   RtRsI16,  "ori    %Rt, %Rs, %i16"),
  [0x10e] = mkop(mips_xor,  RtRsI16,  "xori   %Rt, %Rs, %i16"),
  [0x10f] = mkop(mips_shl,  RtI16,    "lui    %Rt, %i16"),
  [0x110] = mkop(NULL,      copr0,    "cop0"),
  [0x111] = mkop(NULL,      copr1,    "cop1"),
  [0x112] = mkop(NULL,      copr2,    "cop2"),
  [0x113] = mkop(NULL,      copr3,    "cop3"),
  [0x120] = mkop(mips_lb,  RtRsS16,   "lb     %Rt, %i16(%Rs)"),
  [0x121] = mkop(mips_lh,  RtRsS16,   "lh     %Rt, %i16(%Rs)"),
  [0x122] = mkop(mips_lwl, RtRsS16,   "lwl    %Rt, %i16(%Rs)"),
  [0x123] = mkop(mips_lw,  RtRsS16,   "lw     %Rt, %i16(%Rs)"),
  [0x124] = mkop(mips_lbu, RtRsS16,   "lbu    %Rt, %i16(%Rs)"),
  [0x125] = mkop(mips_lhu, RtRsS16,   "lhu    %Rt, %i16(%Rs)"),
  [0x126] = mkop(mips_lwr, RtRsS16,   "lwr    %Rt, %i16(%Rs)"),
  [0x128] = mkop(mips_sb,  RtRsS16,   "sb     %Rt, %i16(%Rs)"),
  [0x129] = mkop(mips_sh,  RtRsS16,   "sh     %Rt, %i16(%Rs)"),
  [0x12a] = mkop(mips_swl, RtRsS16,   "swl    %Rt, %i16(%Rs)"),
  [0x12b] = mkop(mips_sw,  RtRsS16,   "sw     %Rt, %i16(%Rs)"),
  [0x12e] = mkop(mips_swr, RtRsS16,   "swr    %Rt, %i16(%Rs)"),
  [0x130] = mkop(mips_lw,  CRtRsS16,  "lwc0   %Rt, %i16(%Rs)"),
  [0x131] = mkop(mips_lw,  CRtRsS16,  "lwc1   %Rt, %i16(%Rs)"),
  [0x132] = mkop(mips_lw,  CRtRsS16,  "lwc2   %Rt, %i16(%Rs)"),
  [0x133] = mkop(mips_lw,  CRtRsS16,  "lwc3   %Rt, %i16(%Rs)"),
  [0x138] = mkop(mips_sw,  CRtRsS16,  "swc0   %Rt, %i16(%Rs)"),
  [0x139] = mkop(mips_sw,  CRtRsS16,  "swc1   %Rt, %i16(%Rs)"),
  [0x13a] = mkop(mips_sw,  CRtRsS16,  "swc2   %Rt, %i16(%Rs)"),
  [0x13b] = mkop(mips_sw,  CRtRsS16,  "swc3   %Rt, %i16(%Rs)"),
  { -1 },
};

static opcode_t *getop(uint32_t op)
{
  uint32_t fn = opfn(op);
  static int init;

  if (!init) {
    for (int i = 0; optab[i].flag != -1; i++) {
      printf("%.4x: %-30s %p %p %p\n", i, optab[i].mnem ? optab[i].mnem : "----",
	     optab[i].dest, optab[i].arg0, optab[i].arg1);

    }
    init = 1;
  }
  if (fn < 0 || fn > 0x211)
    return NULL;
  return (opcode_t *)&optab[fn];
}

static const char *disstr(uint32_t op) {
  opcode_t *o = getop(op);

  if (!o || !o->mnem)
    return "nop";
  return o->mnem;
};

void disasm(mips_cpu *c, uint32_t pc, uint32_t op)
{
  const char *src = disstr(op);
  char dstr[128], *dst;

  c->PC = pc;
  if (!src)
    src = "xxx";
  dst = dstr;
  while (*src) {
    if (replace(&src, "%Rt", &dst, "%s", regname[rt]) ||
	replace(&src, "%Rd", &dst, "%s", regname[rd]) ||
	replace(&src, "%Rs", &dst, "%s", regname[rs]) ||
	replace(&src, "%i16", &dst, "0x%x", i16) ||
	replace(&src, "%i5", &dst, "0x%x", i5) ||
	replace(&src, "%j16", &dst, "0x%x <%s>", j16, fnname(j16)) ||
	replace(&src, "%j26", &dst, "0x%x <%s>", j26, fnname(j26))) {
      continue;
    }
    *dst++ = *src++;
  }
  *dst = 0;
  printf("%s\n", dstr);
}

#define fi assert(0)

void cpu_showregs(mips_cpu *c)
{
  for (int i = 0; i < 32; i++) {
    printf("%3s:%.8x ", regname[i], c->regs[i]);
    if ((i & 0xF) == 0xF)
      printf("\n");
  }
}

void _cpu_step(mips_cpu *c) {
  uint32_t op, func, memoff, newpc, dummy;

  c->jmpslot[0] = c->jmpslot[1];
  c->jmpslot[1] = 0xFFFFFFFF;

  // reset zero reg
  c->regs[0] = 0;
  SPC = c->PC;
  op = cpu_read32(c->PC);
  func = opfn(op);
  if (trace) {
    cpu_showregs(c);
    printf("GOT CODEZ: %.8x [%.4x] : ", op, func);
    disasm(c, SPC, op);
  }
  c->PC += 4;

  /* Lookup opcode and call function */
  opcode_t* o = getop(op);
  if (o->flag && o->mnem) {
    switch (o->flag) {
    case RdRtI5: o->fn(c, Rd, Rt, i5); return;
    case RdRtRs: o->fn(c, Rd, Rt, Rs); return;
    case RdRsRt: o->fn(c, Rd, Rs, Rt); return;
    case RtRsS16: o->fn(c, Rt, Rs, s16); return;
    case RtRsI16: o->fn(c, Rt, Rs, i16); return;
    case RtI16: o->fn(c, Rt, i16, 16); return;
    case CRtRsS16: o->fn(c, CRt, Rs, s16); return;
    case copr0 ... copr3: mips_copr(c,op, o->flag - copr0); break;
    }
  }

  memoff = Rs+s16;
  newpc = c->PC+s16*4;

  bool islt = (int32_t)Rs < 0;
  bool islte = (int32_t)Rs <= 0;
  
  switch (func) {
    /* Function */
  case 0x008: mips_setpc(c, true, Rs, NULL); break; // jr
  case 0x009: mips_setpc(c, true, Rs, &Rd); break;  // jalr
  case 0x00c: mips_syscall(c->regs[4]); break; // syscall
  case 0x00d: fi; // break;
  case 0x010: Rd = c->rhi; break; // mfhi
  case 0x012: Rd = c->rlo; break; // mflo
  case 0x011: c->rhi = Rs; break; // mthi
  case 0x013: c->rlo = Rs; break; // mtlo

  case 0x018: mips_mult(c, SRs, SRt); break; // mult
  case 0x019: mips_multu(c, Rs, Rt);  break; // multu
  case 0x01a: mips_div(c, SRs, SRt);  break; // div
  case 0x01b: mips_divu(c, Rs, Rt);   break; // divu

    /* Opcode mode 1 */
  case 0x040: mips_setpc(c, islt,  newpc, NULL); break; // bltz
  case 0x041: mips_setpc(c, !islt, newpc, NULL); break; // bgez
  case 0x050: mips_setpc(c, islt,  newpc, &c->ra); break; // bltzal
  case 0x051: mips_setpc(c, !islt, newpc, &c->ra); break; // bgezal

    /* Opcode mode 2 */
  case 0x102: mips_setpc(c, true, j26, NULL); break; // j
  case 0x103: mips_setpc(c, true, j26, &c->ra); break; // jal
  case 0x104: mips_setpc(c, Rs == Rt, newpc, NULL); break; // beq
  case 0x105: mips_setpc(c, Rs != Rt, newpc, NULL); break; // bne
  case 0x106: mips_setpc(c, islte,  newpc, NULL); break; // blez
  case 0x107: mips_setpc(c, !islte, newpc, NULL); break; // bgtz

  case 0x110: mips_copr(c, op, 0); break; // copr0
  case 0x111: mips_copr(c, op, 1); break; // copr1
  case 0x112: mips_copr(c, op, 2); break; // copr2
  case 0x113: mips_copr(c, op, 3); break; // copr3
    
  default:
    printf("Error: Unknown opcode: @ %.8x %.8x\n", SPC, op);
    exit(0);
    break;
  }
}
