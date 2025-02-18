/* Implement Gameboy CPU */
#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <inttypes.h>
#include <assert.h>
#include <stddef.h>
#include <time.h>
#include "bus.h"
#include "cpu.h"
#include "dstk.h"
#include "gr.h"
#include "gboy.h"

extern int trace;
extern int izhlt;
extern uint8_t *bootrom;

extern void  flogger(int, const char *, ...);

static dstk thestk(65536);
dstk *stk = &thestk;

static void stkpush(int addr, int n, int type) {
}

#define fnargs uint8_t *ib, int dst, int src

enum {
  VAL_MASK = 0x8FFF,

  TYPE_SHIFT = 24,
  TYPE_MASK  = 0xff << TYPE_SHIFT,
  TYPE_IMM   = 1 << TYPE_SHIFT,
  TYPE_REG   = 2 << TYPE_SHIFT,
  TYPE_RP    = 3 << TYPE_SHIFT,
  TYPE_MEM   = 4 << TYPE_SHIFT,
  TYPE_OPARG = 6 << TYPE_SHIFT,

  SIZE_SHIFT = 16,
  SIZE_BYTE  = 'b' << SIZE_SHIFT,
  SIZE_WORD  = 'w' << SIZE_SHIFT,

  TYPE_MEM8  = TYPE_MEM+SIZE_BYTE,
  TYPE_MEM16 = TYPE_MEM+SIZE_WORD,
  
  imp = TYPE_OPARG,
  r8,
  ri,
  SP8,
  MHLm, // [HL-]
  MHLp, // [HL+]
  MB16, // [nnnn], xx
  MW16, // [nnnn], xxxx
  MBC,  // [BC]
  MDE,  // [DE]
  MHL,  // [HL]
  MRC,  // [FF00 + C]
  MA8,  // [FF00 + I8]
  I8,
  I16,

  /* Registers */
  RB  = TYPE_REG + 0,
  RC  = TYPE_REG + 1,
  RD  = TYPE_REG + 2,
  RE  = TYPE_REG + 3,
  RH  = TYPE_REG + 4,
  RL  = TYPE_REG + 5,
  RF  = TYPE_REG + 6,
  RA  = TYPE_REG + 7,

  RBC = TYPE_RP  + 0,
  RDE = TYPE_RP  + 2,
  RHL = TYPE_RP  + 4,
  RAF = TYPE_RP  + 6,
  RSP = TYPE_RP  + 8,
};
int SPC;

typedef uint32_t arg_t;

static int  gb_getval(uint8_t *ib, const arg_t& type);
static void gb_setval(uint8_t *ib, const arg_t& type, int vv);

//=============================================
// CPU Registers
//=============================================
static uint16_t PC, SP;
static uint8_t regs[8];
static uint8_t  &B = regs[0];
static uint8_t  &C = regs[1];
static uint8_t  &D = regs[2];
static uint8_t  &E = regs[3];
static uint8_t  &H = regs[4];
static uint8_t  &L = regs[5];
static uint8_t  &A = regs[7];

/* Takes place of F (regs[6]) */
static bool   flags[8];
static bool&  Zf = flags[7];
static bool&  Nf = flags[6];
static bool&  Hf = flags[5];
static bool&  Cf = flags[4];
static bool   If;

uint8_t cpu_getflags(uint8_t f = 0) {
  for (int i = 4; i < 8; i++) {
    f |= (!!flags[i] << i);
  }
  return f;
}

void cpu_setflags(uint8_t f) {
  for (int i = 4; i < 8; i++) {
    flags[i] = ((f >> i) & 1);
  }
}

/* Register Pair type */
template <int h, int l>class rpreg {
  int getval() {
    return (regs[h] << 8) + regs[l];
  }
  void setval(int nv) {
    regs[h] = nv >> 8;
    regs[l] = nv;
  }
public:
  operator int() {
    return getval();
  };
  int operator=(int nv) {
    setval(nv);
    return nv;
  };
  int operator--(int) {
    int cv = getval();
    setval(cv - 1);
    return cv;
  };
  int operator++(int) {
    int cv = getval();
    setval(cv + 1);
    return cv;
  };
};

rpreg<0,1> BC;
rpreg<2,3> DE;
rpreg<4,5> HL;

const char *cpu_getstate()
{
  static char dstr[128];
  int F = cpu_getflags();
  
  snprintf(dstr, sizeof(dstr),
	   "PC:%.4x | BC=%.2x%.2x DE=%.2x%.2x HL=%.2x%.2x AF=%.2x%.2x SP=%.4x [%c%c%c%c%c]",
	   SPC, B, C, D, E, H, L, A, F, SP,
	   Zf ? 'z' : ' ',
	   Nf ? 'n' : ' ',
	   Hf ? 'h' : ' ',
	   Cf ? 'c' : ' ',
	   If ? 'i' : ' ');
  return dstr;
}

/*=========================================================================*
 *  Flag helper functions
 *=========================================================================*/
uint8_t znhc(const uint8_t v, const uint8_t n, const uint8_t h, const uint8_t c)
{
  Cf = c;
  Nf = n;
  Hf = h;
  Zf = (v == 0);
  return v;
}

//==============================
// opcode argument helpers
//==============================
constexpr uint8_t RdBit(uint8_t v) {
  return 1L << ((v & 0x38) >> 3);
}

static int carry(int a, int b, int c, int bits) {
  int mask = (1L << bits) - 1;
  
  return ((a & mask) + (b & mask) + (c & mask)) > mask;
}

/* Used for ADD SP, s8; LD HL, SP+s8 */
static int SPadd(int8_t src) {
  znhc(0xFF, 0, carry(SP, src, 0, 4), carry(SP, src, 0, 8));
  return SP + src;
}

static int _add(int src, int cf) {
  int sum = A + src + cf;

  return znhc(sum, 0, carry(A, src, cf, 4), sum > 0xFF);
}

static int _sub(int src, int cf) {
  int sum = A - src - cf;

  return znhc(sum, 1, ((A & 0xF) - (src & 0xF) - cf) < 0x0, sum < 0x0);
}

int pcflag;
static void gb_setpc(bool test, int npc) {
  if (test) {
    PC = npc;
    pcflag = 1;
  }
}

static void gb_callcc(bool test, int npc) {
  if (test) {
    cpu_push16(PC);
    PC = npc;
    pcflag = 1;
  }
}

static void gb_retcc(bool test) {
  if (test) {
    PC = cpu_pop16();
    pcflag = 1;
  }
}

/*=================================================
 * SHIFT/ROTATE
 *=================================================*/

/* Generic shift left. CF=old Bit7, Bit0=input */
static uint8_t gb_shl(int src, bool bit0) {
  Nf = 0;
  Hf = 0;
  Cf = !!(src & 0x80);
  return (src << 1) | (bit0 ? 0x01 : 0x00);
}

/* Generic Shift Right. CF=old Bit0, Bit7=input */
static uint8_t gb_shr(int src, bool bit7) {
  Nf = 0;
  Hf = 0;
  Cf = !!(src & 0x01);
  return (src >> 1) | (bit7 ? 0x80 : 0x00);
}

static void gb_rlca(fnargs) {
  A = gb_shl(A, A & 0x80);
  Zf = 0;
}
static void gb_rrca(fnargs) {
  A = gb_shr(A, A & 0x01);
  Zf = 0;
}
static void gb_rla(fnargs)  {
  A = gb_shl(A, Cf);
  Zf = 0;
}
static void gb_rra(fnargs)  {
  A = gb_shr(A, Cf);
  Zf = 0;
}

static void gb_rlc(fnargs)  {
  src = gb_shl(src, src & 0x80);
  gb_setval(ib, dst, src);
  Zf = (src == 0);
}
static void gb_rrc(fnargs)  {
  src = gb_shr(src, src & 0x01);
  gb_setval(ib, dst, src);
  Zf = (src == 0);
}
static void gb_rl(fnargs)   {
  src = gb_shl(src, Cf);
  Zf = (src == 0);
  gb_setval(ib, dst, src);
} 
static void gb_rr(fnargs)   {
  src = gb_shr(src, Cf);
  Zf = (src == 0);
  gb_setval(ib, dst, src);
}
static void gb_sla(fnargs)  {
  src = gb_shl(src, 0x0);
  gb_setval(ib, dst, src);
  Zf  = (src == 0);
}
static void gb_srl(fnargs)  {
  src = gb_shr(src, 0x0);
  gb_setval(ib, dst, src);
  Zf  = (src == 0);
}
static void gb_sra(fnargs)  {
  src = gb_shr(src, src & 0x80);
  gb_setval(ib, dst, src);
  Zf  = (src == 0);
}
static void gb_swap(fnargs) {
  src = (src >> 4) | (src << 4);
  gb_setval(ib, dst, src);
  znhc(src, 0, 0, 0);
}

/*======================================
 * Bit operations
 *======================================*/
static void gb_bit(fnargs)  {
  uint8_t bit = RdBit(ib[1]);

  Zf = (src & bit) == 0;
  Hf = 1;
  Nf = 0;
}
static void gb_res(fnargs) {
  uint8_t bit = RdBit(ib[1]);

  gb_setval(ib, dst, src & ~bit);
}
static void gb_set(fnargs) {
  uint8_t bit = RdBit(ib[1]);

  gb_setval(ib, dst, src | bit);
}

/*===================================
 * LOAD/STORE
 *===================================*/
static void gb_ld(fnargs)   { gb_setval(ib, dst, src); }
static void gb_ldh(fnargs)  { gb_setval(ib, dst, src); }
static void gb_ldhl(fnargs) { HL = SPadd(src); }

/*===================================
 * Jump/Jump Relative
 *===================================*/
static void gb_jp(fnargs)    { gb_setpc(true, src); }
static void gb_jr(fnargs)    { gb_setpc(true, src); }
static void gb_jpnz(fnargs)  { gb_setpc(!Zf,  src); }
static void gb_jrnz(fnargs)  { gb_setpc(!Zf,  src); }
static void gb_jpz(fnargs)   { gb_setpc(Zf,   src); }
static void gb_jrz(fnargs)   { gb_setpc(Zf,   src); }
static void gb_jpnc(fnargs)  { gb_setpc(!Cf,  src); }
static void gb_jrnc(fnargs)  { gb_setpc(!Cf,  src); }
static void gb_jpc(fnargs)   { gb_setpc(Cf,   src); }
static void gb_jrc(fnargs)   { gb_setpc(Cf,   src); }

/*===================================
 * Call/Reset
 *===================================*/
static void gb_rst(fnargs)    { gb_callcc(true, src); }
static void gb_call(fnargs)   { gb_callcc(true, src); }
static void gb_callnz(fnargs) { gb_callcc(!Zf, src); }
static void gb_callz(fnargs)  { gb_callcc(Zf,  src); }
static void gb_callnc(fnargs) { gb_callcc(!Cf, src); }
static void gb_callc(fnargs)  { gb_callcc(Cf,  src); }

/*===================================
 * Return/Interrupt Return
 *===================================*/
static void gb_reti(fnargs)  { gb_retcc(true); If = true; }
static void gb_ret(fnargs)   { gb_retcc(true); }
static void gb_retnz(fnargs) { gb_retcc(!Zf); }
static void gb_retz(fnargs)  { gb_retcc(Zf); }
static void gb_retnc(fnargs) { gb_retcc(!Cf); }
static void gb_retc(fnargs)  { gb_retcc(Cf); }

static void gb_daa(fnargs)  {
  if (Nf) {
    if (Cf) A -= 0x60;
    if (Hf) A -= 0x06;
  }
  else {
    if (Cf || A > 0x99) {
      A += 0x60;
      Cf = 1;
    }
    if (Hf || (A & 0xF) > 0x9) {
      A += 0x06;
    }
  }
  Hf = 0;
  Zf = (A == 0);
}

/*===================================
 * Math/Logical
 * add:Special cases for RSP/RHL
 *===================================*/
static void gb_add(fnargs)  {
  if (dst == RA)
    A = _add(src, 0);
  else if (dst == RSP)
    SP = SPadd(src);
  else {
    /* HL = HL+xx */
    Nf = 0;
    Hf = carry(HL, src, 0, 12); //(HL & 0xFFF) + (src & 0xFFF) > 0xFFF;
    Cf = carry(HL, src, 0, 16); //(HL + src) > 0xFFFF;
    HL = HL + src;
  }
}
static void gb_adc(fnargs)  { A = _add(src, Cf); }
static void gb_sub(fnargs)  { A = _sub(src, 0); }
static void gb_sbc(fnargs)  { A = _sub(src, Cf); }
static void gb_and(fnargs)  { A = znhc(A & src, 0, 1, 0); };
static void gb_xor(fnargs)  { A = znhc(A ^ src, 0, 0, 0); };
static void gb_or(fnargs)   { A = znhc(A | src, 0, 0, 0); };
static void gb_cp(fnargs)   { _sub(src, 0); }

static void gb_inc(fnargs)  {
  // Z0H-
  gb_setval(ib, dst, ++src);
  if ((dst & TYPE_MASK) != TYPE_RP)
    znhc(src, 0, (src & 0xF) == 0x0, Cf);
};
static void gb_dec(fnargs)  {
  // Z1H-
  gb_setval(ib, dst, --src);
  if ((dst & TYPE_MASK) != TYPE_RP)
    znhc(src, 1, (src & 0xF) == 0xF, Cf);
}

static void gb_stop(fnargs) { }
static void gb_hlt(fnargs)  { izhlt = 1; }
static void gb_push(fnargs) { cpu_push16(src); }
static void gb_pop(fnargs)  { gb_setval(ib, dst, cpu_pop16()); }

/* i8080:CMA */
static void gb_cpl(fnargs)  {
  A = ~A;
  Nf = true;
  Hf = true;
};

/* i8080:CMC */
static void gb_ccf(fnargs)  { Nf=0; Hf=0; Cf = !Cf; }

/* i8080:STC */
static void gb_scf(fnargs)  { Nf=0; Hf=0; Cf = true; }

static void gb_di(fnargs)   { If = false; fprintf(stdout,"%x:IRQ dis\n",SPC); }
static void gb_ei(fnargs)   { If = true;  fprintf(stdout,"%x:IRQ en\n",SPC);  }

static void gb_nop(fnargs)  { }

/*=====================================================================*
 * Opcode Table
 *=====================================================================*/
/* Number of cycles for branches */
#define _br(taken, nottaken) ((taken << 8) | (nottaken))
#define BRANCH_TAKEN 0xFF00

/* RET:   16
 *   ir=fetch
 *   z=read(sp) ; sp++
 *   w=read(sp) ; sp++
 *   pc=wz
 * RETcc: 8/20
 *   ir=fetch
 *   check cond
 *    z=read sp ; sp++
 *    w=read sp ; sp++
 *    pc=wz
 * JP:    16
 *   fetch
 *   z=fetch
 *   w=fetch
 *   pc=wz
 * JPcc:  12/16
 *   fetch
 *   z=fetch
 *   w=fetch; check cond
 *    pc=wz
 * CALL:  24
 *   fetch
 *   z=fetch
 *   w=fetch
 *   sp--
 *   write(sp,pch); sp--
 *   write(sp,pcl); pc=wz
 * CALLcc:12/24
 *   fetch
 *   z=fetch
 *   w=fetch; check cond
 *    sp--
 *    write[sp,pch] ; sp--
 *    write[sp,pcl] ; pc=wz
 * RETI:  16
 *   fetch
 *   z=read sp++
 *   w=read sp++
 *   pc=wz
 */
#define BRRET  _br(20,8)
#define BRJP   _br(16,12)
#define BRCALL _br(24,12)

#define _(m, a0, a1, nb, c, flg, ds...) { #m, gb_##m, a0, a1, nb, c, ds }
#define _xxx { 0 }
#define ___ imp

#define J12 ((12 << 8) | 8)
#define J20 ((20 << 8) | 8)

struct opcode_t {
  const char *mnem;
  void (*fn)(fnargs);
  int arg0;
  int arg1;
  int nb;    // number of bytes
  int cycs;  // cycles per instruction
  const char *dstr;
};

static opcode_t cbmap[] = {
  _(rlc,    RB,  RB, 2, 8,   "Z00C", "rlc   %rs"),
  _(rlc,    RC,  RC, 2, 8,   "Z00C", "rlc   %rs"),
  _(rlc,    RD,  RD, 2, 8,   "Z00C", "rlc   %rs"),
  _(rlc,    RE,  RE, 2, 8,   "Z00C", "rlc   %rs"),
  _(rlc,    RH,  RH, 2, 8,   "Z00C", "rlc   %rs"),
  _(rlc,    RL,  RL, 2, 8,   "Z00C", "rlc   %rs"),
  _(rlc,   MHL, MHL, 2,16,   "Z00C", "rlc   %rs"),
  _(rlc,    RA,  RA, 2, 8,   "Z00C", "rlc   %rs"),
  _(rrc,    RB,  RB, 2, 8,   "Z00C", "rrc   %rs"),
  _(rrc,    RC,  RC, 2, 8,   "Z00C", "rrc   %rs"),
  _(rrc,    RD,  RD, 2, 8,   "Z00C", "rrc   %rs"),
  _(rrc,    RE,  RE, 2, 8,   "Z00C", "rrc   %rs"),
  _(rrc,    RH,  RH, 2, 8,   "Z00C", "rrc   %rs"),
  _(rrc,    RL,  RL, 2, 8,   "Z00C", "rrc   %rs"),
  _(rrc,   MHL, MHL, 2,16,   "Z00C", "rrc   %rs"),
  _(rrc,    RA,  RA, 2, 8,   "Z00C", "rrc   %rs"),

  _(rl,     RB,  RB, 2, 8,   "Z00C", "RL    %rs"),
  _(rl,     RC,  RC, 2, 8,   "Z00C", "RL    %rs"),
  _(rl,     RD,  RD, 2, 8,   "Z00C", "RL    %rs"),
  _(rl,     RE,  RE, 2, 8,   "Z00C", "RL    %rs"),
  _(rl,     RH,  RH, 2, 8,   "Z00C", "RL    %rs"),
  _(rl,     RL,  RL, 2, 8,   "Z00C", "RL    %rs"),
  _(rl,    MHL, MHL, 2,16,   "Z00C", "RL    %rs"),
  _(rl,     RA,  RA, 2, 8,   "Z00C", "RL    %rs"),
  _(rr,     RB,  RB, 2, 8,   "Z00C", "RR    %rs"),
  _(rr,     RC,  RC, 2, 8,   "Z00C", "RR    %rs"),
  _(rr,     RD,  RD, 2, 8,   "Z00C", "RR    %rs"),
  _(rr,     RE,  RE, 2, 8,   "Z00C", "RR    %rs"),
  _(rr,     RH,  RH, 2, 8,   "Z00C", "RR    %rs"),
  _(rr,     RL,  RL, 2, 8,   "Z00C", "RR    %rs"),
  _(rr,    MHL, MHL, 2,16,   "Z00C", "RR    %rs"),
  _(rr,     RA,  RA, 2, 8,   "Z00C", "RR    %rs"),

  _(sla,    RB,  RB, 2, 8,   "Z00C", "sla   %rs"),
  _(sla,    RC,  RC, 2, 8,   "Z00C", "sla   %rs"),
  _(sla,    RD,  RD, 2, 8,   "Z00C", "sla   %rs"),
  _(sla,    RE,  RE, 2, 8,   "Z00C", "sla   %rs"),
  _(sla,    RH,  RH, 2, 8,   "Z00C", "sla   %rs"),
  _(sla,    RL,  RL, 2, 8,   "Z00C", "sla   %rs"),
  _(sla,   MHL, MHL, 2,16,   "Z00C", "sla   %rs"),
  _(sla,    RA,  RA, 2, 8,   "Z00C", "sla   %rs"),
  _(sra,    RB,  RB, 2, 8,   "Z00C", "sra   %rs"),
  _(sra,    RC,  RC, 2, 8,   "Z00C", "sra   %rs"),
  _(sra,    RD,  RD, 2, 8,   "Z00C", "sra   %rs"),
  _(sra,    RE,  RE, 2, 8,   "Z00C", "sra   %rs"),
  _(sra,    RH,  RH, 2, 8,   "Z00C", "sra   %rs"),
  _(sra,    RL,  RL, 2, 8,   "Z00C", "sra   %rs"),
  _(sra,   MHL, MHL, 2,16,   "Z00C", "sra   %rs"),
  _(sra,    RA,  RA, 2, 8,   "Z00C", "sra   %rs"),

  _(swap,   RB,  RB, 2, 8,   "Z00C", "swap  %rs"),
  _(swap,   RC,  RC, 2, 8,   "Z00C", "swap  %rs"),
  _(swap,   RD,  RD, 2, 8,   "Z00C", "swap  %rs"),
  _(swap,   RE,  RE, 2, 8,   "Z00C", "swap  %rs"),
  _(swap,   RH,  RH, 2, 8,   "Z00C", "swap  %rs"),
  _(swap,   RL,  RL, 2, 8,   "Z00C", "swap  %rs"),
  _(swap,  MHL, MHL, 2,16,   "Z00C", "swap  %rs"),
  _(swap,   RA,  RA, 2, 8,   "Z00C", "swap  %rs"),
  _(srl,    RB,  RB, 2, 8,   "Z00C", "srl   %rs"),
  _(srl,    RC,  RC, 2, 8,   "Z00C", "srl   %rs"),
  _(srl,    RD,  RD, 2, 8,   "Z00C", "srl   %rs"),
  _(srl,    RE,  RE, 2, 8,   "Z00C", "srl   %rs"),
  _(srl,    RH,  RH, 2, 8,   "Z00C", "srl   %rs"),
  _(srl,    RL,  RL, 2, 8,   "Z00C", "srl   %rs"),
  _(srl,   MHL, MHL, 2,16,   "Z00C", "srl   %rs"),
  _(srl,    RA,  RA, 2, 8,   "Z00C", "srl   %rs"),

  _(bit,   ___,  RB, 2, 8,   "Z01_", "bit   0,%rs"),
  _(bit,   ___,  RC, 2, 8,   "Z01_", "bit   0,%rs"),
  _(bit,   ___,  RD, 2, 8,   "Z01_", "bit   0,%rs"),
  _(bit,   ___,  RE, 2, 8,   "Z01_", "bit   0,%rs"),
  _(bit,   ___,  RH, 2, 8,   "Z01_", "bit   0,%rs"),
  _(bit,   ___,  RL, 2, 8,   "Z01_", "bit   0,%rs"),
  _(bit,   ___, MHL, 2,12,   "Z01_", "bit   0,%rs"),
  _(bit,   ___,  RA, 2, 8,   "Z01_", "bit   0,%rs"),
  _(bit,   ___,  RB, 2, 8,   "Z01_", "bit   1,%rs"),
  _(bit,   ___,  RC, 2, 8,   "Z01_", "bit   1,%rs"),
  _(bit,   ___,  RD, 2, 8,   "Z01_", "bit   1,%rs"),
  _(bit,   ___,  RE, 2, 8,   "Z01_", "bit   1,%rs"),
  _(bit,   ___,  RH, 2, 8,   "Z01_", "bit   1,%rs"),
  _(bit,   ___,  RL, 2, 8,   "Z01_", "bit   1,%rs"),
  _(bit,   ___, MHL, 2,12,   "Z01_", "bit   1,%rs"),
  _(bit,   ___,  RA, 2, 8,   "Z01_", "bit   1,%rs"),

  _(bit,   ___,  RB, 2, 8,   "Z01_", "bit   2,%rs"),
  _(bit,   ___,  RC, 2, 8,   "Z01_", "bit   2,%rs"),
  _(bit,   ___,  RD, 2, 8,   "Z01_", "bit   2,%rs"),
  _(bit,   ___,  RE, 2, 8,   "Z01_", "bit   2,%rs"),
  _(bit,   ___,  RH, 2, 8,   "Z01_", "bit   2,%rs"),
  _(bit,   ___,  RL, 2, 8,   "Z01_", "bit   2,%rs"),
  _(bit,   ___, MHL, 2,12,   "Z01_", "bit   2,%rs"),
  _(bit,   ___,  RA, 2, 8,   "Z01_", "bit   2,%rs"),
  _(bit,   ___,  RB, 2, 8,   "Z01_", "bit   3,%rs"),
  _(bit,   ___,  RC, 2, 8,   "Z01_", "bit   3,%rs"),
  _(bit,   ___,  RD, 2, 8,   "Z01_", "bit   3,%rs"),
  _(bit,   ___,  RE, 2, 8,   "Z01_", "bit   3,%rs"),
  _(bit,   ___,  RH, 2, 8,   "Z01_", "bit   3,%rs"),
  _(bit,   ___,  RL, 2, 8,   "Z01_", "bit   3,%rs"),
  _(bit,   ___, MHL, 2,12,   "Z01_", "bit   3,%rs"),
  _(bit,   ___,  RA, 2, 8,   "Z01_", "bit   3,%rs"),

  _(bit,   ___,  RB, 2, 8,   "Z01_", "bit   4,%rs"),
  _(bit,   ___,  RC, 2, 8,   "Z01_", "bit   4,%rs"),
  _(bit,   ___,  RD, 2, 8,   "Z01_", "bit   4,%rs"),
  _(bit,   ___,  RE, 2, 8,   "Z01_", "bit   4,%rs"),
  _(bit,   ___,  RH, 2, 8,   "Z01_", "bit   4,%rs"),
  _(bit,   ___,  RL, 2, 8,   "Z01_", "bit   4,%rs"),
  _(bit,   ___, MHL, 2,12,   "Z01_", "bit   4,%rs"),
  _(bit,   ___,  RA, 2, 8,   "Z01_", "bit   4,%rs"),
  _(bit,   ___,  RB, 2, 8,   "Z01_", "bit   5,%rs"),
  _(bit,   ___,  RC, 2, 8,   "Z01_", "bit   5,%rs"),
  _(bit,   ___,  RD, 2, 8,   "Z01_", "bit   5,%rs"),
  _(bit,   ___,  RE, 2, 8,   "Z01_", "bit   5,%rs"),
  _(bit,   ___,  RH, 2, 8,   "Z01_", "bit   5,%rs"),
  _(bit,   ___,  RL, 2, 8,   "Z01_", "bit   5,%rs"),
  _(bit,   ___, MHL, 2,12,   "Z01_", "bit   5,%rs"),
  _(bit,   ___,  RA, 2, 8,   "Z01_", "bit   5,%rs"),

  _(bit,   ___,  RB, 2, 8,   "Z01_", "bit   6,%rs"),
  _(bit,   ___,  RC, 2, 8,   "Z01_", "bit   6,%rs"),
  _(bit,   ___,  RD, 2, 8,   "Z01_", "bit   6,%rs"),
  _(bit,   ___,  RE, 2, 8,   "Z01_", "bit   6,%rs"),
  _(bit,   ___,  RH, 2, 8,   "Z01_", "bit   6,%rs"),
  _(bit,   ___,  RL, 2, 8,   "Z01_", "bit   6,%rs"),
  _(bit,   ___, MHL, 2,12,   "Z01_", "bit   6,%rs"),
  _(bit,   ___,  RA, 2, 8,   "Z01_", "bit   6,%rs"),
  _(bit,   ___,  RB, 2, 8,   "Z01_", "bit   7,%rs"),
  _(bit,   ___,  RC, 2, 8,   "Z01_", "bit   7,%rs"),
  _(bit,   ___,  RD, 2, 8,   "Z01_", "bit   7,%rs"),
  _(bit,   ___,  RE, 2, 8,   "Z01_", "bit   7,%rs"),
  _(bit,   ___,  RH, 2, 8,   "Z01_", "bit   7,%rs"),
  _(bit,   ___,  RL, 2, 8,   "Z01_", "bit   7,%rs"),
  _(bit,   ___, MHL, 2,12,   "Z01_", "bit   7,%rs"),
  _(bit,   ___,  RA, 2, 8,   "Z01_", "bit   7,%rs"),

  _(res,    RB,  RB, 2, 8,   "____", "res   0,%rs"),
  _(res,    RC,  RC, 2, 8,   "____", "res   0,%rs"),
  _(res,    RD,  RD, 2, 8,   "____", "res   0,%rs"),
  _(res,    RE,  RE, 2, 8,   "____", "res   0,%rs"),
  _(res,    RH,  RH, 2, 8,   "____", "res   0,%rs"),
  _(res,    RL,  RL, 2, 8,   "____", "res   0,%rs"),
  _(res,   MHL, MHL, 2,16,   "____", "res   0,%rs"),
  _(res,    RA,  RA, 2, 8,   "____", "res   0,%rs"),
  _(res,    RB,  RB, 2, 8,   "____", "res   1,%rs"),
  _(res,    RC,  RC, 2, 8,   "____", "res   1,%rs"),
  _(res,    RD,  RD, 2, 8,   "____", "res   1,%rs"),
  _(res,    RE,  RE, 2, 8,   "____", "res   1,%rs"),
  _(res,    RH,  RH, 2, 8,   "____", "res   1,%rs"),
  _(res,    RL,  RL, 2, 8,   "____", "res   1,%rs"),
  _(res,   MHL, MHL, 2,16,   "____", "res   1,%rs"),
  _(res,    RA,  RA, 2, 8,   "____", "res   1,%rs"),

  _(res,    RB,  RB, 2, 8,   "____", "res   2,%rs"),
  _(res,    RC,  RC, 2, 8,   "____", "res   2,%rs"),
  _(res,    RD,  RD, 2, 8,   "____", "res   2,%rs"),
  _(res,    RE,  RE, 2, 8,   "____", "res   2,%rs"),
  _(res,    RH,  RH, 2, 8,   "____", "res   2,%rs"),
  _(res,    RL,  RL, 2, 8,   "____", "res   2,%rs"),
  _(res,   MHL, MHL, 2,16,   "____", "res   2,%rs"),
  _(res,    RA,  RA, 2, 8,   "____", "res   2,%rs"),
  _(res,    RB,  RB, 2, 8,   "____", "res   3,%rs"),
  _(res,    RC,  RC, 2, 8,   "____", "res   3,%rs"),
  _(res,    RD,  RD, 2, 8,   "____", "res   3,%rs"),
  _(res,    RE,  RE, 2, 8,   "____", "res   3,%rs"),
  _(res,    RH,  RH, 2, 8,   "____", "res   3,%rs"),
  _(res,    RL,  RL, 2, 8,   "____", "res   3,%rs"),
  _(res,   MHL, MHL, 2,16,   "____", "res   3,%rs"),
  _(res,    RA,  RA, 2, 8,   "____", "res   3,%rs"),

  _(res,    RB,  RB, 2, 8,   "____", "res   4,%rs"),
  _(res,    RC,  RC, 2, 8,   "____", "res   4,%rs"),
  _(res,    RD,  RD, 2, 8,   "____", "res   4,%rs"),
  _(res,    RE,  RE, 2, 8,   "____", "res   4,%rs"),
  _(res,    RH,  RH, 2, 8,   "____", "res   4,%rs"),
  _(res,    RL,  RL, 2, 8,   "____", "res   4,%rs"),
  _(res,   MHL, MHL, 2,16,   "____", "res   4,%rs"),
  _(res,    RA,  RA, 2, 8,   "____", "res   4,%rs"),
  _(res,    RB,  RB, 2, 8,   "____", "res   5,%rs"),
  _(res,    RC,  RC, 2, 8,   "____", "res   5,%rs"),
  _(res,    RD,  RD, 2, 8,   "____", "res   5,%rs"),
  _(res,    RE,  RE, 2, 8,   "____", "res   5,%rs"),
  _(res,    RH,  RH, 2, 8,   "____", "res   5,%rs"),
  _(res,    RL,  RL, 2, 8,   "____", "res   5,%rs"),
  _(res,   MHL, MHL, 2,16,   "____", "res   5,%rs"),
  _(res,    RA,  RA, 2, 8,   "____", "res   5,%rs"),

  _(res,    RB,  RB, 2, 8,   "____", "res   6,%rs"),
  _(res,    RC,  RC, 2, 8,   "____", "res   6,%rs"),
  _(res,    RD,  RD, 2, 8,   "____", "res   6,%rs"),
  _(res,    RE,  RE, 2, 8,   "____", "res   6,%rs"),
  _(res,    RH,  RH, 2, 8,   "____", "res   6,%rs"),
  _(res,    RL,  RL, 2, 8,   "____", "res   6,%rs"),
  _(res,   MHL, MHL, 2,16,   "____", "res   6,%rs"),
  _(res,    RA,  RA, 2, 8,   "____", "res   6,%rs"),
  _(res,    RB,  RB, 2, 8,   "____", "res   7,%rs"),
  _(res,    RC,  RC, 2, 8,   "____", "res   7,%rs"),
  _(res,    RD,  RD, 2, 8,   "____", "res   7,%rs"),
  _(res,    RE,  RE, 2, 8,   "____", "res   7,%rs"),
  _(res,    RH,  RH, 2, 8,   "____", "res   7,%rs"),
  _(res,    RL,  RL, 2, 8,   "____", "res   7,%rs"),
  _(res,   MHL, MHL, 2,16,   "____", "res   7,%rs"),
  _(res,    RA,  RA, 2, 8,   "____", "res   7,%rs"),

  _(set,    RB,  RB, 2, 8,   "____", "set   0,%rs"),
  _(set,    RC,  RC, 2, 8,   "____", "set   0,%rs"),
  _(set,    RD,  RD, 2, 8,   "____", "set   0,%rs"),
  _(set,    RE,  RE, 2, 8,   "____", "set   0,%rs"),
  _(set,    RH,  RH, 2, 8,   "____", "set   0,%rs"),
  _(set,    RL,  RL, 2, 8,   "____", "set   0,%rs"),
  _(set,   MHL, MHL, 2,16,   "____", "set   0,%rs"),
  _(set,    RA,  RA, 2, 8,   "____", "set   0,%rs"),
  _(set,    RB,  RB, 2, 8,   "____", "set   1,%rs"),
  _(set,    RC,  RC, 2, 8,   "____", "set   1,%rs"),
  _(set,    RD,  RD, 2, 8,   "____", "set   1,%rs"),
  _(set,    RE,  RE, 2, 8,   "____", "set   1,%rs"),
  _(set,    RH,  RH, 2, 8,   "____", "set   1,%rs"),
  _(set,    RL,  RL, 2, 8,   "____", "set   1,%rs"),
  _(set,   MHL, MHL, 2,16,   "____", "set   1,%rs"),
  _(set,    RA,  RA, 2, 8,   "____", "set   1,%rs"),

  _(set,    RB,  RB, 2, 8,   "____", "set   2,%rs"),
  _(set,    RC,  RC, 2, 8,   "____", "set   2,%rs"),
  _(set,    RD,  RD, 2, 8,   "____", "set   2,%rs"),
  _(set,    RE,  RE, 2, 8,   "____", "set   2,%rs"),
  _(set,    RH,  RH, 2, 8,   "____", "set   2,%rs"),
  _(set,    RL,  RL, 2, 8,   "____", "set   2,%rs"),
  _(set,   MHL, MHL, 2,16,   "____", "set   2,%rs"),
  _(set,    RA,  RA, 2, 8,   "____", "set   2,%rs"),
  _(set,    RB,  RB, 2, 8,   "____", "set   3,%rs"),
  _(set,    RC,  RC, 2, 8,   "____", "set   3,%rs"),
  _(set,    RD,  RD, 2, 8,   "____", "set   3,%rs"),
  _(set,    RE,  RE, 2, 8,   "____", "set   3,%rs"),
  _(set,    RH,  RH, 2, 8,   "____", "set   3,%rs"),
  _(set,    RL,  RL, 2, 8,   "____", "set   3,%rs"),
  _(set,   MHL, MHL, 2,16,   "____", "set   3,%rs"),
  _(set,    RA,  RA, 2, 8,   "____", "set   3,%rs"),

  _(set,    RB,  RB, 2, 8,   "____", "set   a,b"),
  _(set,    RC,  RC, 2, 8,   "____", "set   a,c"),
  _(set,    RD,  RD, 2, 8,   "____", "set   a,d"),
  _(set,    RE,  RE, 2, 8,   "____", "set   a,e"),
  _(set,    RH,  RH, 2, 8,   "____", "set   a,h"),
  _(set,    RL,  RL, 2, 8,   "____", "set   a,l"),
  _(set,   MHL, MHL, 2,16,   "____", "set   a,%rs"),
  _(set,    RA,  RA, 2, 8,   "____", "set   3,%rs"),
  _(set,    RB,  RB, 2, 8,   "____", "set   a,b"),
  _(set,    RC,  RC, 2, 8,   "____", "set   a,c"),
  _(set,    RD,  RD, 2, 8,   "____", "set   a,d"),
  _(set,    RE,  RE, 2, 8,   "____", "set   a,e"),
  _(set,    RH,  RH, 2, 8,   "____", "set   a,h"),
  _(set,    RL,  RL, 2, 8,   "____", "set   a,l"),
  _(set,   MHL, MHL, 2,16,   "____", "set   a,%rs"),
  _(set,    RA,  RA, 2, 8,   "____", "set   3,%rs"),

  _(set,    RB,  RB, 2, 8,   "____", "set   3,%rs"),
  _(set,    RC,  RC, 2, 8,   "____", "set   3,%rs"),
  _(set,    RD,  RD, 2, 8,   "____", "set   3,%rs"),
  _(set,    RE,  RE, 2, 8,   "____", "set   3,%rs"),
  _(set,    RH,  RH, 2, 8,   "____", "set   3,%rs"),
  _(set,    RL,  RL, 2, 8,   "____", "set   a,a"),
  _(set,   MHL, MHL, 2,16,   "____", "set   a,a"),
  _(set,    RA,  RA, 2, 8,   "____", "set   a,a"),
  _(set,    RB,  RB, 2, 8,   "____", "set   a,a"),
  _(set,    RC,  RC, 2, 8,   "____", "set   a,a"),
  _(set,    RD,  RD, 2, 8,   "____", "set   a,a"),
  _(set,    RE,  RE, 2, 8,   "____", "set   a,a"),
  _(set,    RH,  RH, 2, 8,   "____", "set   a,a"),
  _(set,    RL,  RL, 2, 8,   "____", "set   a,a"),
  _(set,   MHL, MHL, 2,16,   "____", "set   a,a"),
  _(set,    RA,  RA, 2, 8,   "____", "set   a,a"),
 };

static opcode_t optab[256] = {
  /* 0x00 */
  _(nop,   ___, ___, 1, 4,   "____", "nop"),
  _(ld,    RBC, I16, 3, 12,  "____", "ld    bc,$d16"),
  _(ld,    MBC,  RA, 1, 8,   "____", "ld    (bc),a"),
  _(inc,   RBC, RBC, 1, 8,   "____", "inc   bc"),
  _(inc,   RB,   RB, 1, 4,   "Z0H-", "inc   b"),
  _(dec,   RB,   RB, 1, 4,   "Z1H-", "dec   b"),
  _(ld,    RB,   I8, 2, 8,   "____", "ld    b,$d8"),
  _(rlca,  RA,  ___, 1, 4,   "000C", "rlca"),
  _(ld,    MW16,RSP, 3, 20,  "____", "ld    (d16),SP"),
  _(add,   RHL, RBC, 1, 8,   "-0HC", "add   hl,bc"),
  _(ld,    RA,  MBC, 1, 8,   "____", "ld    a,(bc)"),
  _(dec,   RBC, RBC, 1, 8,   "____", "dec   bc"),
  _(inc,   RC,   RC, 1, 4,   "Z0H-", "inc   c"),
  _(dec,   RC,   RC, 1, 4,   "Z1H-", "dec   c"),
  _(ld,    RC,   I8, 2, 8,   "____", "ld    c,$d8"),
  _(rrca,  RA,  ___, 1, 4,   "000C", "rrca"),

  /* 0x10 */
  _(stop,  ___, ___, 1, 4,   "____", "stop"),
  _(ld,    RDE, I16, 3, 12,  "____", "ld    de,$d16"),
  _(ld,    MDE, RA,  1, 8,   "____", "ld    [de],a"),
  _(inc,   RDE, RDE, 1, 8,   "____", "inc   de"),
  _(inc,   RD,   RD, 1, 4,   "Z0H-", "inc   d"),
  _(dec,   RD,   RD, 1, 4,   "Z1H-", "dec   d"),
  _(ld,    RD,  I8,  2, 8,   "____", "ld    d,$d8"),
  _(rla,   RA,  ___, 1, 4,   "000C", "rla"),
  _(jr,   ___,   r8, 2, 12,  "____", "jr    0xr8"),
  _(add,   RHL, RDE, 1, 8,   "-0HC", "add   hl,de"),
  _(ld,    RA,  MDE, 1, 8,   "____", "ld    a,[de]"),
  _(dec,   RDE, RDE, 1, 8,   "____", "dec   de"),
  _(inc,   RE,   RE, 1, 4,   "Z0H-", "inc   e"),
  _(dec,   RE,   RE, 1, 4,   "Z1H-", "dec   e"),
  _(ld,    RE,   I8, 2, 8,   "____", "ld    e,$d8"),
  _(rra,   RA,  ___, 1, 4,   "000C", "rra"),

  /* 0x20 */
  _(jrnz, ___,   r8, 2, J12, "____", "jr    nz,0xr8"),
  _(ld,   RHL,  I16, 3, 12,  "____", "ld    hl,0xd16"),
  _(ld,  MHLp,   RA, 1, 8,   "____", "ld    (hl+),a"),
  _(inc,  RHL,  RHL, 1, 8,   "____", "inc   hl"),
  _(inc,   RH,   RH, 1, 4,   "Z0H-", "inc   h"),
  _(dec,   RH,   RH, 1, 4,   "Z1H-", "dec   h"),
  _(ld,    RH,   I8, 2, 8,   "____", "ld    h,0xd8"),
  _(daa,   ___, ___, 1, 4,   "Z-0C", "daa"),
  _(jrz,   ___,  r8, 2, J12, "____", "jr    z,0xr8"),
  _(add,   RHL, RHL, 1, 8,   "-0HC", "add   hl,hl"),
  _(ld,     RA,MHLp, 1, 8,   "____", "ld    a,(hl+)"),
  _(dec,   RHL, RHL, 1, 8,   "____", "dec   hl"),
  _(inc,    RL,  RL, 1, 4,   "Z0H-", "inc   l"),
  _(dec,    RL,  RL, 1, 4,   "Z1H-", "dec   l"),
  _(ld,     RL,  I8, 2, 8,   "____", "ld    l,0xd8"),
  _(cpl,   ___, ___, 1, 4,   "-11-", "cpl"),

  /* 0x30 */
  _(jrnc,  ___,  r8, 2, J12, "____", "jr    nc,0xr8"),
  _(ld,    RSP, I16, 3, 12,  "____", "ld    sp,0xd16"),
  _(ld,   MHLm,  RA, 1, 8,   "____", "ld    (hl-),a"),
  _(inc,   RSP, RSP, 1, 8,   "____", "inc   sp"),
  _(inc,   MHL, MHL, 1, 12,  "Z0H-", "inc   (hl)"),
  _(dec,   MHL, MHL, 1, 12,  "Z1H-", "dec   (hl)"),
  _(ld,    MHL,  I8, 2, 12,  "____", "ld    (hl),0xd8"),
  _(scf,   ___, ___, 1, 4,   "-001", "scf"),
  _(jrc,   ___,  r8, 2, J12, "____", "jr    c,0xr8"),
  _(add,   RHL, RSP, 1, 8,   "-0HC", "add   hl,sp"),
  _(ld,     RA,MHLm, 1, 8,   "____", "ld    a,(hl-)"),
  _(dec,   RSP, RSP, 1, 8,   "____", "dec   sp"),
  _(inc,    RA,  RA, 1, 4,   "Z0H-", "inc   a"),
  _(dec,    RA,  RA, 1, 4,   "Z1H-", "dec   a"),
  _(ld,     RA,  I8, 2, 8,   "____", "ld    a,0xd8"),
  _(ccf,   ___, ___, 1, 4,   "-00C", "ccf"),

  /* 0x40 */
  _(ld,     RB,   RB, 1, 4,  "____", "ld    %rd,%rs"),
  _(ld,     RB,   RC, 1, 4,  "____", "ld    %rd,%rs"),
  _(ld,     RB,   RD, 1, 4,  "____", "ld    %rd,%rs"),
  _(ld,     RB,   RE, 1, 4,  "____", "ld    %rd,%rs"),
  _(ld,     RB,   RH, 1, 4,  "____", "ld    %rd,%rs"),
  _(ld,     RB,   RL, 1, 4,  "____", "ld    %rd,%rs"),
  _(ld,     RB,  MHL, 1, 8,  "____", "ld    b,(HL"),
  _(ld,     RB,   RA, 1, 4,  "____", "ld    %rd,%rs"),
  _(ld,     RC,   RB, 1, 4,  "____", "ld    %rd,%rs"),
  _(ld,     RC,   RC, 1, 4,  "____", "ld    %rd,%rs"),
  _(ld,     RC,   RD, 1, 4,  "____", "ld    %rd,%rs"),
  _(ld,     RC,   RE, 1, 4,  "____", "ld    %rd,%rs"),
  _(ld,     RC,   RH, 1, 4,  "____", "ld    %rd,%rs"),
  _(ld,     RC,   RL, 1, 4,  "____", "ld    %rd,%rs"),
  _(ld,     RC,  MHL, 1, 8,  "____", "ld    c,(hl)"),
  _(ld,     RC,   RA, 1, 4,  "____", "ld    %rd,%rs"),

  /* 0x50 */
  _(ld,     RD,   RB, 1, 4,  "____", "ld    %rd,%rs"),
  _(ld,     RD,   RC, 1, 4,  "____", "ld    %rd,%rs"),
  _(ld,     RD,   RD, 1, 4,  "____", "ld    %rd,%rs"),
  _(ld,     RD,   RE, 1, 4,  "____", "ld    %rd,%rs"),
  _(ld,     RD,   RH, 1, 4,  "____", "ld    %rd,%rs"),
  _(ld,     RD,   RL, 1, 4,  "____", "ld    %rd,%rs"),
  _(ld,     RD,  MHL, 1, 8,  "____", "ld    d,(hl)"),
  _(ld,     RD,   RA, 1, 4,  "____", "ld    %rd,%rs"),
  _(ld,     RE,   RB, 1, 4,  "____", "ld    %rd,%rs"),
  _(ld,     RE,   RC, 1, 4,  "____", "ld    %rd,%rs"),
  _(ld,     RE,   RD, 1, 4,  "____", "ld    %rd,%rs"),
  _(ld,     RE,   RE, 1, 4,  "____", "ld    %rd,%rs"),
  _(ld,     RE,   RH, 1, 4,  "____", "ld    %rd,%rs"),
  _(ld,     RE,   RL, 1, 4,  "____", "ld    %rd,%rs"),
  _(ld,     RE,  MHL, 1, 8,  "____", "ld    e,(hl)"),
  _(ld,     RE,   RA, 1, 4,  "____", "ld    %rd,%rs"),

  /* 0x60 */
  _(ld,     RH,   RB, 1, 4,  "____", "ld    %rd,%rs"),
  _(ld,     RH,   RC, 1, 4,  "____", "ld    %rd,%rs"),
  _(ld,     RH,   RD, 1, 4,  "____", "ld    %rd,%rs"),
  _(ld,     RH,   RE, 1, 4,  "____", "ld    %rd,%rs"),
  _(ld,     RH,   RH, 1, 4,  "____", "ld    %rd,%rs"),
  _(ld,     RH,   RL, 1, 4,  "____", "ld    %rd,%rs"),
  _(ld,     RH,  MHL, 1, 8,  "____", "ld    h,(hl)"),
  _(ld,     RH,   RA, 1, 4,  "____", "ld    %rd,%rs"),
  _(ld,     RL,   RB, 1, 4,  "____", "ld    %rd,%rs"),
  _(ld,     RL,   RC, 1, 4,  "____", "ld    %rd,%rs"),
  _(ld,     RL,   RD, 1, 4,  "____", "ld    %rd,%rs"),
  _(ld,     RL,   RE, 1, 4,  "____", "ld    %rd,%rs"),
  _(ld,     RL,   RH, 1, 4,  "____", "ld    %rd,%rs"),
  _(ld,     RL,   RL, 1, 4,  "____", "ld    %rd,%rs"),
  _(ld,     RL,  MHL, 1, 8,  "____", "ld    l,(hl)"),
  _(ld,     RL,   RA, 1, 4,  "____", "ld    %rd,%rs"),

  /* 0x70 */
  _(ld,    MHL,   RB, 1, 8,  "____", "ld    (hl),b"),
  _(ld,    MHL,   RC, 1, 8,  "____", "ld    (hl),c"),
  _(ld,    MHL,   RD, 1, 8,  "____", "ld    (hl),d"),
  _(ld,    MHL,   RE, 1, 8,  "____", "ld    (hl),e"),
  _(ld,    MHL,   RH, 1, 8,  "____", "ld    (hl),H"),
  _(ld,    MHL,   RL, 1, 8,  "____", "ld    (hl),l"),
  _(hlt,   ___,  ___, 1, 4,  "____", "hlt"),
  _(ld,    MHL,   RA, 1, 8,  "____", "ld    (hl),a"),
  _(ld,     RA,   RB, 1, 4,  "____", "ld    %rd,%rs"),
  _(ld,     RA,   RC, 1, 4,  "____", "ld    %rd,%rs"),
  _(ld,     RA,   RD, 1, 4,  "____", "ld    %rd,%rs"),
  _(ld,     RA,   RE, 1, 4,  "____", "ld    %rd,%rs"),
  _(ld,     RA,   RH, 1, 4,  "____", "ld    %rd,%rs"),
  _(ld,     RA,   RL, 1, 4,  "____", "ld    %rd,%rs"),
  _(ld,     RA,  MHL, 1, 8,  "____", "ld    a,(hl)"),
  _(ld,     RA,   RA, 1, 4,  "____", "ld    %rd,%rs"),

  /* 0x80 */
  _(add,    RA,   RB, 1, 4,  "Z0HC", "add   a,%rs"),
  _(add,    RA,   RC, 1, 4,  "Z0HC", "add   a,%rs"),
  _(add,    RA,   RD, 1, 4,  "Z0HC", "add   a,%rs"),
  _(add,    RA,   RE, 1, 4,  "Z0HC", "add   a,%rs"),
  _(add,    RA,   RH, 1, 4,  "Z0HC", "add   a,%rs"),
  _(add,    RA,   RL, 1, 4,  "Z0HC", "add   a,%rs"),
  _(add,    RA,  MHL, 1, 8,  "Z0HC", "add   a,(hl)"),
  _(add,    RA,   RA, 1, 4,  "Z0HC", "add   a,%rs"),
  _(adc,    RA,   RB, 1, 4,  "Z0HC", "adc   a,%rs"),
  _(adc,    RA,   RC, 1, 4,  "Z0HC", "adc   a,%rs"),
  _(adc,    RA,   RD, 1, 4,  "Z0HC", "adc   a,%rs"),
  _(adc,    RA,   RE, 1, 4,  "Z0HC", "adc   a,%rs"),
  _(adc,    RA,   RH, 1, 4,  "Z0HC", "adc   a,%rs"),
  _(adc,    RA,   RL, 1, 4,  "Z0HC", "adc   a,%rs"),
  _(adc,    RA,  MHL, 1, 8,  "Z0HC", "adc   a,(hl)"),
  _(adc,    RA,   RA, 1, 4,  "Z0HC", "adc   a,%rs"),

  /* 0x90 */
  _(sub,    RA,   RB, 1, 4,  "Z1HC", "sub   a,%rs"),
  _(sub,    RA,   RC, 1, 4,  "Z1HC", "sub   a,%rs"),
  _(sub,    RA,   RD, 1, 4,  "Z1HC", "sub   a,%rs"),
  _(sub,    RA,   RE, 1, 4,  "Z1HC", "sub   a,%rs"),
  _(sub,    RA,   RH, 1, 4,  "Z1HC", "sub   a,%rs"),
  _(sub,    RA,   RL, 1, 4,  "Z1HC", "sub   a,%rs"),
  _(sub,    RA,  MHL, 1, 8,  "Z1HC", "sub   a,(hl)"),
  _(sub,    RA,   RA, 1, 4,  "Z1HC", "sub   a,%rs"),
  _(sbc,    RA,   RB, 1, 4,  "Z1HC", "sbc   a,%rs"),
  _(sbc,    RA,   RC, 1, 4,  "Z1HC", "sbc   a,%rs"),
  _(sbc,    RA,   RD, 1, 4,  "Z1HC", "sbc   a,%rs"),
  _(sbc,    RA,   RE, 1, 4,  "Z1HC", "sbc   a,%rs"),
  _(sbc,    RA,   RH, 1, 4,  "Z1HC", "sbc   a,%rs"),
  _(sbc,    RA,   RL, 1, 4,  "Z1HC", "sbc   a,%rs"),
  _(sbc,    RA,  MHL, 1, 8,  "Z1HC", "sbc   a,(hl)"),
  _(sbc,    RA,   RA, 1, 4,  "Z1HC", "sbc   a,%rs"),

  /* 0xa0 */
  _(and,    RA,   RB, 1, 4,  "Z010", "and   a,%rs"),
  _(and,    RA,   RC, 1, 4,  "Z010", "and   a,%rs"),
  _(and,    RA,   RD, 1, 4,  "Z010", "and   a,%rs"),
  _(and,    RA,   RE, 1, 4,  "Z010", "and   a,%rs"),
  _(and,    RA,   RH, 1, 4,  "Z010", "and   a,%rs"),
  _(and,    RA,   RL, 1, 4,  "Z010", "and   a,%rs"),
  _(and,    RA,  MHL, 1, 8,  "Z010", "and   a,(hl)"),
  _(and,    RA,   RA, 1, 4,  "Z010", "and   a,%rs"),
  _(xor,    RA,   RB, 1, 4,  "Z000", "xor   a,%rs"),
  _(xor,    RA,   RC, 1, 4,  "Z000", "xor   a,%rs"),
  _(xor,    RA,   RD, 1, 4,  "Z000", "xor   a,%rs"),
  _(xor,    RA,   RE, 1, 4,  "Z000", "xor   a,%rs"),
  _(xor,    RA,   RH, 1, 4,  "Z000", "xor   a,%rs"),
  _(xor,    RA,   RL, 1, 4,  "Z000", "xor   a,%rs"),
  _(xor,    RA,  MHL, 1, 8,  "Z000", "xor   a,(hl)"),
  _(xor,    RA,   RA, 1, 4,  "Z000", "xor   a,%rs"),

  /* 0xb0 */
  _(or,     RA,   RB, 1, 4,   "Z000", "or   a,%rs"),
  _(or,     RA,   RC, 1, 4,   "Z000", "or   a,%rs"),
  _(or,     RA,   RD, 1, 4,   "Z000", "or   a,%rs"),
  _(or,     RA,   RE, 1, 4,   "Z000", "or   a,%rs"),
  _(or,     RA,   RH, 1, 4,   "Z000", "or   a,%rs"),
  _(or,     RA,   RL, 1, 4,   "Z000", "or   a,%rs"),
  _(or,     RA,  MHL, 1, 8,   "Z000", "or   a,(hl)"),
  _(or,     RA,   RA, 1, 4,   "Z000", "or   a,%rs"),
  _(cp,     RA,   RB, 1, 4,   "Z1HC", "cp   a,%rs"),
  _(cp,     RA,   RC, 1, 4,   "Z1HC", "cp   a,%rs"),
  _(cp,     RA,   RD, 1, 4,   "Z1HC", "cp   a,%rs"),
  _(cp,     RA,   RE, 1, 4,   "Z1HC", "cp   a,%rs"),
  _(cp,     RA,   RH, 1, 4,   "Z1HC", "cp   a,%rs"),
  _(cp,     RA,   RL, 1, 4,   "Z1HC", "cp   a,%rs"),
  _(cp,     RA,  MHL, 1, 8,   "Z1HC", "cp   a,(hl)"),
  _(cp,     RA,   RA, 1, 4,   "Z1HC", "cp   a,%rs"),

  /* 0xc0
     20,8 : retnz,retz,retnc,retc
     16,12 : jpnz,jpz,jpnc,jpc
     24,12 : callnz,callz,callnc,callc
     
  */
  _(retnz,  ___, ___, 1, BRRET,     "____", "retnz"),
  _(pop,    RBC, ___, 1, 12,        "____", "pop   bc"),
  _(jpnz,   ___, I16, 3, BRJP,      "____", "jp    nz,0xd16"),
  _(jp,     ___, I16, 3, 16,        "____", "jp    0xd16"),
  _(callnz, ___, I16, 3, BRCALL,    "____", "call  nz,0xd16"),
  _(push,   ___, RBC, 1, 16,        "____", "push  bc"),
  _(add,     RA,  I8, 2, 8,         "____", "add   a,$d8"),
  _(rst,    ___,  ri, 1, 16,        "____", "rst   00"),
  _(retz,   ___, ___, 1, BRRET,     "____", "retz"),
  _(ret,    ___, ___, 1, 16,        "____", "ret"),
  _(jpz,    ___, I16, 3, BRJP,      "____", "jp    z,0xd16"),
  _xxx,
  _(callz,  ___, I16, 3, BRCALL,    "____", "call  z,0xd16"),
  _(call,   ___, I16, 3, 24,        "____", "call  0xd16"),
  _(adc,     RA,  I8, 2, 8,         "____", "adc   a,$d8"),
  _(rst,    ___,  ri, 1, 16,        "____", "rst   08"),

  /* 0xd0 */
  _(retnc,  ___, ___, 1, BRRET,     "____", "retnc"),
  _(pop,    RDE, ___, 1, 12,        "____", "pop   de"),
  _(jpnc,   ___, I16, 3, BRJP,      "____", "jp    nc,0xd16"),
  _xxx,
  _(callnc, ___, I16, 3, BRCALL,    "____", "call  nc,0xd16"),
  _(push,   ___, RDE, 1, 16,        "____", "push  de"),
  _(sub,     RA,  I8, 2, 8,         "____", "sub   a,$d8"),
  _(rst,    ___,  ri, 1, 16,        "____", "rst   10"),
  _(retc,   ___, ___, 1, BRRET,     "____", "retc"),
  _(reti,   ___, ___, 1, 16,        "____", "reti"),
  _(jpc,    ___, I16, 3, BRJP,      "____", "jp    c,0xd16"),
  _xxx,
  _(callc,  ___, I16, 3, BRCALL,    "____", "call  c,0xd16"),
  _xxx,
  _(sbc,     RA,  I8, 2, 8,         "____", "sbc   a,$d8"),
  _(rst,    ___,  ri, 1, 16,        "____", "rst   18"),

  /* 0xe0 */
  _(ldh,    MA8, RA,  2, 12,        "____", "ldh   [ffd8],a"),
  _(pop,    RHL, ___, 1, 12,        "____", "pop   hl"),
  _(ld,     MRC, RA,  1, 8,         "____", "ld    [c],a"),
  _xxx,
  _xxx,
  _(push,   ___, RHL, 1, 16,        "____", "push  hl"),
  _(and,     RA,  I8, 2, 8,         "Z010", "and   a,$d8"),
  _(rst,    ___,  ri, 1, 16,        "____", "rst   20"),
  _(add,    RSP,  I8, 2, 16,        "00HC", "add   sp,$d8"),
  _(jp,     ___, RHL, 1, 4,         "____", "jp    hl"),
  _(ld,    MB16,  RA, 3, 16,        "____", "ld    [d16],a"),
  _xxx,
  _xxx,
  _xxx,
  _(xor,     RA,  I8, 2, 8,         "Z000", "xor   a,$d8"),
  _(rst,    ___,  ri, 1, 16,        "____", "rst   28"),

  /* 0xf0 */
  _(ldh,     RA, MA8, 2, 12,        "____", "ldh   a,[ffd8]"),
  _(pop,    RAF, ___, 1, 12,        "ZNHC", "pop   af"),
  _(ld,     RA,  MRC, 1, 8,         "____", "ld    a,[c]"),
  _(di,     ___, ___, 1, 4,         "____", "di"),
  _xxx,
  _(push,    ___,RAF, 1, 16,        "____", "push  af"),
  _(or,      RA,  I8, 2, 8,         "Z000", "or    a,$d8"),
  _(rst,    ___,  ri, 1, 16,        "____", "rst   30"),
  _(ldhl,   RHL, SP8, 2, 12,        "00HC", "ld    hl, [sp+d8]"),
  _(ld,     RSP, RHL, 1, 8,         "____", "ld    sp, hl"),
  _(ld,      RA,MB16, 3, 16,        "____", "ld    a, [d16]"),
  _(ei,     ___, ___, 1, 4,         "____", "ei"),
  _xxx,
  _xxx,
  _(cp,      RA,  I8, 2, 8,         "Z1HC", "cp    a,$d8"),
  _(rst,    ___,  ri, 1, 16,        "____", "rst   38"),
};

/*===================================================*
 * CPU Functions
 *===================================================*/
static void gb_setval(uint8_t *ib, const arg_t& type, int nv) {
  int vv = (type & VAL_MASK);

  switch (type) {
  case RB ... RA:
    regs[vv] = nv;
    break;
  case RBC ... RHL:
    regs[vv+0] = nv >> 8;
    regs[vv+1] = nv;
    break;
  case RAF:
    A = (nv >> 8);
    cpu_setflags(nv);
    break;
  case RSP:
    SP = nv;
    break;
  case MBC:
    cpu_write8(BC, nv);
    break;
  case MDE:
    cpu_write8(DE, nv);
    break;
  case MHL:
    cpu_write8(HL, nv);
    break;
  case MHLm:
    cpu_write8(HL--, nv);
    break;
  case MHLp:
    cpu_write8(HL++, nv);
    break;
  case MRC:
    cpu_write8(0xFF00 + C, nv);
    break;
  case MA8:
    cpu_write8(0xFF00 + ib[1], nv);
    break;
  case MB16:
    cpu_write8((ib[2] << 8) + ib[1], nv);
    break;
  case MW16:
    cpu_write16((ib[2] << 8) + ib[1], nv);
    break;
  case imp:
    fprintf(stdout,"imp: %.2x\n", ib[0]);
    break;
  default:
    fprintf(stdout,"error...: %x %.2x\n", type, ib[0]);
    break;
  }
}

/* Returns value of referenced object */
static int gb_getval(uint8_t *ib, const arg_t& type) {
  int vv = (type & VAL_MASK);

  switch (type) {
  case RB ... RA:
    return regs[vv];
  case RBC ... RHL:
    return (regs[vv] << 8) + regs[vv+1];
  case RAF:
    return (A << 8) | cpu_getflags();
  case RSP:
    return SP;
  case SP8:
  case I8:
    return ib[1];
  case I16:
    return (ib[2] << 8) + ib[1];
  case ri:
    return ib[0] & 0x38;
  case r8:
    return PC + (int8_t)ib[1];
  case MBC:
    return cpu_read8(BC);
  case MDE:
    return cpu_read8(DE);
  case MHL:
    return cpu_read8(HL);
  case MHLm:
    return cpu_read8(HL--);
  case MHLp:
    return cpu_read8(HL++);
  case MRC:
    return cpu_read8(0xFF00 + C);
  case MA8:
    return cpu_read8(0xFF00 + ib[1]);
  case MB16:
    return cpu_read8((ib[2] << 8) + ib[1]);
  case imp:
    return 0;
  default:
    fprintf(stdout,"error...: %x %.2x\n", type, ib[0]);
    break;
  }
  return 0;
}

static const char *regname[] = {
  "b", "c", "d", "e", "h", "l", "(hl)", "a"
};

static const char *disasm(int pc, uint8_t *ib) {
  static char dstr[128];
  const char *s;
  char *d;

  d = dstr;
  memset(dstr, 0, sizeof(dstr));
  if (*ib == 0xCB) {
    ib++;
    s = cbmap[*ib].dstr;
  }
  else {
    s = optab[*ib].dstr;
  }
  if (!s) {
    printf("NO OP: %x\n", *ib);
    return "xxx";
  }
  while (*s) {
    if (replace(&s, "d8", &d, "%.2X", ib[1]) ||
	replace(&s, "%rs",&d, regname[*ib & 7]) ||
	replace(&s, "%rd",&d, regname[(*ib >> 3) & 7]) ||
	replace(&s, "d16",&d, "%.2X%.2X", ib[2], ib[1]) ||
	replace(&s, "r8", &d, "%.4X", pc+(int8_t)ib[1]))
      continue;
    *d++ = *s++;
  }
  return dstr;
}

static opcode_t *getib(uint8_t *ib, uint8_t& nib) {
  opcode_t *op;

  /* Decode opcode byte */
  ib[nib++] = cpu_read8(PC++, dstk::CODE);
  if (ib[0] == 0xCB) {
    ib[nib++] = cpu_read8(PC++, dstk::CODE);
    op = &cbmap[ib[1]];
  }
  else {
    op = &optab[ib[0]];
  }
  if (!op->mnem) {
    fprintf(stdout, "No opcode: %.4x:%.2x\n", SPC, ib[0]);
    exit(0);
  }
  /* Read instruction bytes */
  while (nib < op->nb) {
    ib[nib++] = cpu_read8(PC++, dstk::CODE);
  }
  stkpush(SPC, nib, dstk::CODE);
  return op;
}

/*================================================
 * External CPU interface
 *================================================*/
void cpu_push16(const uint16_t v) {
  SP -= 2;
  stkpush(SP, 2, dstk::STACK);
  cpu_write16(SP, v, dstk::STACK);
}

uint16_t cpu_pop16() {
  uint16_t tmp = SP;

  SP += 2;
  stkpush(tmp, 2, dstk::STACK);
  return cpu_read16(tmp, dstk::STACK);
}

void cpu_reset(uint32_t addr) {
  PC = 0x00;
  A = B = C = D = E = H = L = 0x0;
  cpu_setflags(0);
  //mb.register_handler(0x0000, 0x00FF, 0xFFFF,  memio,  bootrom,   _RD|_OVR, "BootROM");
}

bool cpu_irq(int irq) {
  if (If == false)
    return false;
  //fprintf(stdout,"cpu_irq: %d\n", irq);
  If = false;
  cpu_push16(PC);
  PC = irq;
  return true;
}

int cpu_step() {
  uint8_t ib[3], nib = 0;
  int src, dst, ncyc;
  opcode_t *op;
  extern int isCGB;

  if (PC == 0x0100 && isCGB) {
    A = 0x11;
  }
  SPC = PC;

  memset(ib, 0xaa, sizeof(ib));
  op = getib(ib, nib);

  dst = op->arg0;
  src = gb_getval(ib, op->arg1);
  if (trace) {
    fprintf(stdout, "%s | %.2x %.2x %.2x | '%s'\n", cpu_getstate(), ib[0], ib[1], ib[2],
	    disasm(PC, ib));
  }
  pcflag = 0;
  op->fn(ib, dst, src);
  ncyc = op->cycs;
  if (pcflag && (ncyc & BRANCH_TAKEN)) {
    ncyc >>= 8;
  }
  ncyc &= ~BRANCH_TAKEN;
  return ncyc;
}

bool cpu_irqenabled() {
  return If;
}

//#define GBIT
#ifdef GBIT
#include "gbit/lib/tester.h"
#include "gbtest.cc"

static size_t ims;
static int num_mem_accesses;
static struct mem_access mem_accesses[16];

int maio(void *arg, uint32_t offset, int mode, uint8_t &data) {
  if (mode == 'r' && offset >= ims) {
    data = 0xAA;
  }
  else if (mode == 'r') {
    memio(arg, offset, mode, data);
  }
  else {
    mem_accesses[num_mem_accesses].type = MEM_ACCESS_WRITE;
    mem_accesses[num_mem_accesses].addr = offset;
    mem_accesses[num_mem_accesses].val  = data;
    num_mem_accesses++;
  }
  return 0;
}

void gbit_init(size_t imssz, uint8_t *imem)
{
  ims = imssz;
  mb.register_handler(0x0000, 0xFFFF, 0xFFFF, maio,  imem, _RW, "MEMA");
}

void gbit_set_state(struct state *state)
{
  A = state->reg8.A;
  B = state->reg8.B;
  C = state->reg8.C;
  D = state->reg8.D;
  E = state->reg8.E;
  F = state->reg8.F;
  H = state->reg8.H;
  L = state->reg8.L;
  PC = state->PC;
  SP = state->SP;
  If = state->interrupts_master_enabled;
  izhlt = state->halted;
  num_mem_accesses = 0;
}

void gbit_get_state(struct state *state)
{
  state->reg8.A = A;
  state->reg8.F = F;
  state->reg8.B = B;
  state->reg8.C = C;
  state->reg8.D = D;
  state->reg8.E = E;
  state->reg8.H = H;
  state->reg8.L = L;
  state->PC = PC;
  state->SP = SP;
  state->halted = izhlt;
  state->interrupts_master_enabled = If;
  state->num_mem_accesses = num_mem_accesses;
  memcpy(state->mem_accesses, mem_accesses, sizeof(mem_accesses));
}

int gbit_step()
{
  return cpu_step();
}

struct tester_operations ops = {
  gbit_init,
  gbit_set_state,
  gbit_get_state,
  gbit_step
};

struct tester_flags flags = {
  .keep_going_on_mismatch = 1,
  .enable_cb_instruction_testing = 1,
  .print_tested_instruction = 1,
  .print_verbose_inputs = 0,
};
#endif

const char *fnname(uint32_t addr) {
  return "";
}

void dumpcfg(uint8_t *buf, size_t len, int off)
{
  int op, pc, next[2];
  opcode_t *opc;
  dstk stk(len, printf);
  
  stk.push(off, 1, dstk::PENDING, "first");
  while ((off = stk.pop()) != -1) {
    printf("\n------------------------------ %.8x [%s]\n",
	   off, fnname(off));
    do {
      /* Get code */
      pc = off;
      op = buf[pc];
      opc = &optab[op];

      if (op == 0xCB) {
	op = buf[pc+1];
	opc = &cbmap[op];
      }
      stk.push(pc, opc->nb, dstk::CODE, "code");

      next[0] = pc + opc->nb;
      next[1] = -1;
      off = next[0];
      printf("off: %.5x %.2x %s\n", pc, op, disasm(pc, &buf[pc]));
      
      switch(op) {
      case 0xc0: case 0xc8: case 0xc9: case 0xd0: case 0xd8: case 0xd9: case 0xe9:
	// retxx, jphl
	next[0] = -1;
	break;
      case 0xc7: case 0xcf: case 0xd7: case 0xdf: case 0xe7: case 0xef: case 0xf7: case 0xff:
	// rst
	next[0] = -1;
	break;
      case 0xc3:
	// jmp, uncond
	next[0] = -1;
	next[1] = *(uint16_t *)&buf[pc+1];
	break;
      case 0xc2: case 0xca: case 0xd2: case 0xda:
	// jmp, cond
	next[0] = *(uint16_t *)&buf[pc+1];
	break;
      case 0x18:
	// jr, uncond
	next[0] = pc + (int8_t)buf[pc+1] + 2;
	break;
      case 0x20: case 0x28: case 0x30: case 0x38:
	// jr, cond
	next[1] = pc + (int8_t)buf[pc+1] + 2;
	break;
      case 0xc4: case 0xcc: case 0xcd: case 0xd3: case 0xdc:
	// call
	next[1] = *(uint16_t *)&buf[pc+1];
	break;
      default:
	break;
      }
      for (int n = 0; n < 2; n++) {
	stk.push(next[n], 1, dstk::PENDING, "nxt");
      }
    } while (next[0] != -1 && next[1] == -1);
  };
  stk.showstk(128);
  exit(0);
}

#ifdef TEST
int trace, izhlt, isCGB;

char *sarg(int n)
{
  switch(n) {
  case 0: return "___";
  case ri:  return "ri";
  case imp: return "___";
  case r8:  return "r8";
  case MHLp: return "MHLp";
  case MA8: return "MA8";
  case MW16: return "MW16";
  case MBC: return "MBC";
  case MDE: return "MDE";
  case MRC: return "MRC";
  case MHL: return "MHL";
  case I16: return "I16";
  case I8:  return "I8";
  case RBC: return "RBC";
  case RDE: return "RDE";
  case RHL: return "RHL";
  case RAF: return "RAF";
  case RSP: return "RSP";
  case RB:  return "RB";
  case RC:  return "RC";
  case RD:  return "RD";
  case RE:  return "RE";
  case RH:  return "RH";
  case RL:  return "RL";
  case RA:  return "RA";
  }
  return "ZZZ";
}

void dumptab()
{
  int i;
  opcode_t *t = optab;
  char str[128];
  
  for (i = 0; i < 256; i++) {
    printf("_(%-6s, %-4s, %-4s, %d, %4d, ",
	   t->mnem ? t->mnem : "NULL", sarg(t->arg0), sarg(t->arg1),
	   t->nb, t->cycs);
    snprintf(str, sizeof(str), "\"%s\"", t->mnem);
    printf("%-30s ),\n", str);
    t++;
  }
}

void cpu_write8(const uint32_t a, uint8_t v, int t)
{
}
uint8_t cpu_read8(const uint32_t a, int t)
{
  return 0;
}

void  flogger(int lvl, const char *fmt, ...)
{
}


int main(int argc, char *argv[])
{
  uint8_t *buf;
  size_t   len;

  dumptab();
  buf = loadrom(argv[1], len);
  dumpcfg(buf, len);
  return 0;
}

#endif

