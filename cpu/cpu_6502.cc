/*
  cpu_6502.cc : 6502 CPU emulator

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
#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <assert.h>
#include <string.h>

#define __NEEDCPU16
#include "cpu.h"
#include "bus.h"
#include "util.h"

/* Logger interface */
void flogger(int, const char *, ...);

#ifdef TEST
#define comment(x) ""
#else
extern const char *comment(int);
#endif

enum {
  IMP,
  ACC,
  PCRET,
  
  IMM = 0x10,
  ZPG,
  ZPX,
  ZPY,
  IXX,
  IXY,
  PCREL,

  ABS = 0x20,
  ABX,
  ABY,
  PCABS,
  PCIND,

  // cpu flags
  _Bf = 0x10,

  // these determine the type of opcode argument
  // register:  0x4____N (A=0,X=1,Y=2)
  // immediate: 0x1___NN
  // memory:    0x2_NNNN
  TYPE_MEM =  0x400000,
  TYPE_IMM =  0x100000,
  TYPE_REG =  0x200000,

  SKIP     =  0,
  CROSS    =  0x010000,
  BRANCH   =  0x020000,
  WRONLY   =  0x040000,
};

bool hlt = false;
int SPC;

/* Keep track of cycles */
uint64_t totcyc;
uint64_t totinst;
int extracyc;
int trace;

static int irq_nest;

/* CPU Registers */
struct cpu6502 {
  uint8_t tA,tX,tY,tS;
  uint16_t tPC;
  
  uint8_t flags[8];
  uint8_t& tCf = flags[0];
  uint8_t& tZf = flags[1];
  uint8_t& tIf = flags[2];
  uint8_t& tDf = flags[3];
  uint8_t& tBf = flags[4];
  uint8_t& tRf = flags[5];
  uint8_t& tVf = flags[6];
  uint8_t& tNf = flags[7];
};
cpu6502 thecpu;

#define PC thecpu.tPC

#define A thecpu.tA
#define X thecpu.tX
#define Y thecpu.tY
#define S thecpu.tS

#define Cf thecpu.tCf
#define Zf thecpu.tZf
#define If thecpu.tIf
#define Df thecpu.tDf
#define Bf thecpu.tBf
#define Rf thecpu.tRf
#define Vf thecpu.tVf
#define Nf thecpu.tNf

uint8_t cpu_getflags() {
  int flag = 0;

  Rf = true;
  for (int i = 0; i < 8; i++) {
    flag |= (thecpu.flags[i]) ? (1 << i) : 0;
  }
  return flag;
}

void cpu_setflags(uint8_t r) {
  for (int i = 0; i < 8; i++) {
    thecpu.flags[i] = (r & 1);
    r >>= 1;
  }
  Rf = true;
}

/* Special cases:
 * ABX does dummy read on page crossing (xx - 0x100)
 * ABY does dummy read on page crossing (xx - 0x100)
 * IXY does dummy read on page crossing (xx - 0x100)
 *
 * INC/DEC/ASL/ASL/ROL/ROR read original, write original, write new
 * Branches read next opcode, page crossing reads 
 */

/*===============================*
 * CPU Helper functions
 *===============================*/
void cpu_push8(uint8_t v) {
  /* Post-Decrement */
  cpu_write8(0x100 + S, v, dstk::STACK);
  S--;
}

uint8_t cpu_pop8() {
  /* Pre-increment */
  S++;
  return cpu_read8(0x100 + S, dstk::STACK);
}

uint8_t cpu_fetch8() {
  return cpu_read8(PC++, dstk::CODE);
}

uint32_t cpu_getstate(cpustate_t *cs) {
  if (cs) {
    cs->regs[0] = A;
    cs->regs[1] = X;
    cs->regs[2] = Y;
    cs->regs[3] = S;
  }
  return PC;
}

static inline char f(int f, char c) {
  return f ? c : '-';
}

const char *cpu_getstate()
{
  static char cs[64];

  snprintf(cs, sizeof(cs), "PC:%.4x AX:%.2x X:%.2x Y:%.2x S:%.2X [%c%c%c%c%c%c%c%c]",
	   SPC, A, X, Y, S,
	   f(Nf,'n'), f(Vf,'v'),'x',f(Bf,'b'), f(Df,'d'), f(If,'i'), f(Zf,'z'), f(Cf,'c'));
  return cs;
}

/*===============================*
 * Opcode Helper functions
 *===============================*/
#define snz  0x0100
#define clrc 0x0200
#define setc 0x0400

static uint32_t setmask;

static uint8_t nz(cpu6502 *c, const uint8_t nv) {
  // Save result for delayed calculation of N/Z
  setmask = nv;
  return nv;
}

static uint8_t nzc(const bool c, const uint8_t nv) {
  // Save result for delayed calculation of N/Z/C */
  // if c == 0, then mask == clrc
  // if c == 1, then mask == setc
  setmask = nv | (clrc << c);
  return nv;
}

static int _setpc(cpu6502 *c, const int npc) {
  PC = npc;
  if (npc == SPC) {
    printf("loop @ %.4x\n", npc);
  }
  return 0;
}

int setpc(cpu6502 *c, const bool test, const int npc) {
  if (test) {
    /* One or two exta cycles on branch */
    extracyc += ((PC & 0xFF00) != (npc & 0xFF00)) ? 2 : 1;
    _setpc(c, npc);
  }
  return 0;
}

static uint8_t add(const uint8_t src) {
  int sum;

  sum = A + src + !!Cf;
  Vf  = VFLAG(A, src, sum, 0x80); //~(A ^ src) & (A ^ sum) & 0x80;
  if (Df) {
    sum = (A & 0x0F) + (src & 0x0F) + !!Cf;

    if (sum > 0x09) {
      sum = ((sum + 6) & 0xF) + 0x10;
    }
    sum += (A & 0xF0) + (src & 0xF0);
    if (sum > 0x99) {
      sum += 0x60;
    }
  }
  return nzc(sum >= 0x100, sum);
}

int cbit(cpu6502 *c, int n) {
  return (c->tCf ? n : 0);
}

/*===============================*
 * Opcode functions
 *===============================*/
static void NOP(cpu6502 *c, int& dst, const int src)    { }

static void ADC(cpu6502 *c, int& dst, const int src)    { A = add(src);  }
static void SBC(cpu6502 *c, int& dst, const int src)    { A = add(Df ? (0x99 - src) : ~src); }

static void ROL(cpu6502 *c, int& dst, const int src)    { dst = nzc(src & 0x80, (src << 1) | cbit(c, 0x01)); }
static void ROR(cpu6502 *c, int& dst, const int src)    { dst = nzc(src & 0x01, (src >> 1) | cbit(c, 0x80)); }
static void ASL(cpu6502 *c, int& dst, const int src)    { dst = nzc(src & 0x80, src << 1); }
static void LSR(cpu6502 *c, int& dst, const int src)    { dst = nzc(src & 0x01, src >> 1); }

static void BPL(cpu6502 *c, int& dst, const int src)    { setpc(c, !c->tNf, src); }
static void BMI(cpu6502 *c, int& dst, const int src)    { setpc(c, c->tNf,  src); }
static void BVC(cpu6502 *c, int& dst, const int src)    { setpc(c, !c->tVf, src); }
static void BVS(cpu6502 *c, int& dst, const int src)    { setpc(c, c->tVf,  src); }
static void BCC(cpu6502 *c, int& dst, const int src)    { setpc(c, !c->tCf, src); }
static void BCS(cpu6502 *c, int& dst, const int src)    { setpc(c, c->tCf,  src); }
static void BNE(cpu6502 *c, int& dst, const int src)    { setpc(c, !c->tZf, src); }
static void BEQ(cpu6502 *c, int& dst, const int src)    { setpc(c, c->tZf,  src); }

static void AND(cpu6502 *c, int& dst, const int src)    { A = nz(c, A & src); }
static void EOR(cpu6502 *c, int& dst, const int src)    { A = nz(c, A ^ src); }
static void ORA(cpu6502 *c, int& dst, const int src)    { A = nz(c, A | src); }

static void CMP(cpu6502 *c, int& dst, const int src)    { nzc(c->tA >= src, c->tA - src); }
static void CPX(cpu6502 *c, int& dst, const int src)    { nzc(c->tX >= src, c->tX - src); }
static void CPY(cpu6502 *c, int& dst, const int src)    { nzc(c->tY >= src, c->tY - src); }

static void JMP(cpu6502 *c, int& dst, const int src)    { _setpc(c, src); }
static void JSR(cpu6502 *c, int& dst, const int src)    { cpu_push16(PC-1); _setpc(c, src); }
static void RTS(cpu6502 *c, int& dst, const int src)    { _setpc(c, cpu_pop16()+1); }
static void RTI(cpu6502 *c, int& dst, const int src)    { irq_nest--; cpu_setflags(cpu_pop8()); _setpc(c, cpu_pop16()); }

static void INC(cpu6502 *c, int& dst, const int src)    { dst = nz(c, src + 1); }
static void INX(cpu6502 *c, int& dst, const int src)    { c->tX = nz(c, c->tX + 1); }
static void INY(cpu6502 *c, int& dst, const int src)    { c->tY = nz(c, c->tY + 1); }
static void DEC(cpu6502 *c, int& dst, const int src)    { dst = nz(c, src - 1); }
static void DEX(cpu6502 *c, int& dst, const int src)    { c->tX = nz(c, c->tX - 1); }
static void DEY(cpu6502 *c, int& dst, const int src)    { c->tY = nz(c, c->tY - 1); }

static void CLI(cpu6502 *c, int& dst, const int src)    { c->tIf = false; }
static void SEI(cpu6502 *c, int& dst, const int src)    { c->tIf = true;  }
static void CLD(cpu6502 *c, int& dst, const int src)    { c->tDf = false; }
static void SED(cpu6502 *c, int& dst, const int src)    { c->tDf = true;  }
static void CLC(cpu6502 *c, int& dst, const int src)    { c->tCf = false; }
static void SEC(cpu6502 *c, int& dst, const int src)    { c->tCf = true;  }
static void CLV(cpu6502 *c, int& dst, const int src)    { c->tVf = false; }

static void LDA(cpu6502 *c, int& dst, const int src)    { A = nz(c, src); }
static void LDX(cpu6502 *c, int& dst, const int src)    { X = nz(c, src); }
static void LDY(cpu6502 *c, int& dst, const int src)    { Y = nz(c, src); }

static void STA(cpu6502 *c, int& dst, const int src)    { dst = A; }
static void STX(cpu6502 *c, int& dst, const int src)    { dst = X; }
static void STY(cpu6502 *c, int& dst, const int src)    { dst = Y; }

static void PHA(cpu6502 *c, int& dst, const int src)    { cpu_push8(A); }
static void PLA(cpu6502 *c, int& dst, const int src)    { A = nz(c, cpu_pop8()); }

static void PHP(cpu6502 *c, int& dst, const int src)    { cpu_push8(cpu_getflags() | _Bf); }
static void PLP(cpu6502 *c, int& dst, const int src)    { cpu_setflags(cpu_pop8() & ~_Bf); }

static void TAX(cpu6502 *c, int& dst, const int src)    { X = nz(c, A); }
static void TXA(cpu6502 *c, int& dst, const int src)    { A = nz(c, X); }
static void TAY(cpu6502 *c, int& dst, const int src)    { Y = nz(c, A); }
static void TYA(cpu6502 *c, int& dst, const int src)    { A = nz(c, Y); }
static void TSX(cpu6502 *c, int& dst, const int src)    { X = nz(c, S); }
static void TXS(cpu6502 *c, int& dst, const int src)    { S = X; }

static void BRK(cpu6502 *c, int& dst, const int src)    {
  Bf = true;
  cpu_push16(PC+1);
  cpu_push8(cpu_getflags());
  PC = cpu_read16(0xFFFE);
  If = true;
}

static void BIT(cpu6502 *c, int& dst, const int src)    {
  Zf = (A & src) == 0;
  Nf = (src & 0x80) != 0;
  Vf = (src & 0x40) != 0;
}

static void HLT(cpu6502 *c, int& dst, const int src)    {  printf("HALT!!!\n"); hlt=true; }

/*======================================*
 * Undocumented opcodes 
 *======================================*/
static void ALR(cpu6502 *c, int& dst, const int src) {
  /* AND imm ; LSR ACC */
  AND(c, dst, src);
  A = nzc(A & 0x80, A >> 1);
}
static void SLO(cpu6502 *c, int& dst, const int src) {  // ASO
  /* ASL mem ; ORA mem */
  ASL(c, dst, src);
  ORA(c, dst, dst);
}
static void RLA(cpu6502 *c, int& dst, const int src) {
  /* ROL mem ; AND mem */
  ROL(c, dst, src);
  AND(c, dst, dst);
}
static void LSE(cpu6502 *c, int& dst, const int src) {
  /* LSR mem ; EOR mem */
  LSR(c, dst, src);
  EOR(c, dst, dst);
};
static void RRA(cpu6502 *c, int& dst, const int src) {
  /* ROR mem ; ADC mem */
  ROR(c, dst, src);
  ADC(c, dst, dst);
}
static void AXS(cpu6502 *c, int& dst, const int src) {
  dst = nz(c, A & X);
}
static void LAX(cpu6502 *c, int& dst, const int src) {
  /* LDA mem ; LDX mem */
  LDA(c, dst, src);
  LDX(c, dst, dst);
};
static void DCM(cpu6502 *c, int& dst, const int src) {
  /* DEC mem ; CMP mem */
  DEC(c, dst, src);
  CMP(c, dst, dst);
}

static void INS(cpu6502 *c, int& dst, const int src) {
  INC(c, dst, src);
  SBC(c, dst, dst);
}
static void ARR(cpu6502 *c, int& dst, const int src) {
  AND(c, dst, src);
  A = nzc(A & 0x01, (A >> 1) | (Cf ? 0x80 : 0x00));
}
static void XAA(cpu6502 *c, int& dst, const int src) { TXA(c, dst, src); AND(c, dst, dst); }
static void OAL(cpu6502 *c, int& dst, const int src) { A &= 0xEE; AND(c, dst, src); TAX(c, dst, dst); }
static void SKB(cpu6502 *c, int& dst, const int src) { } //skip byte (NOP)
static void SKW(cpu6502 *c, int& dst, const int src) { } //skip word (NOP)
static void XAS(cpu6502 *c, int& dst, const int src) { assert(0); }
static void AAC(cpu6502 *c, int& dst, const int src) { A = nz(c, A & src); Cf=Nf; }
static void AXA(cpu6502 *c, int& dst, const int src) { assert(0); }
static void SYA(cpu6502 *c, int& dst, const int src) { dst = nz(c, Y & ((src & 0xF0) + 1)); }
static void SXA(cpu6502 *c, int& dst, const int src) { dst = nz(c, X & ((src & 0xF0) + 1)); }
static void LAR(cpu6502 *c, int& dst, const int src) { A=X=S=nz(c, src & S); }
static void SAX(cpu6502 *c, int& dst, const int src) { dst = A & X; }

/*===============================*
 * Opcode Table
 *===============================*/
#define _(op, m, a, f)  opmap[op] = { m, a, f, #m "    " sfx_##a }
#define U(op, m, a, f)  opmap[op] = { m, a, f, #m "*   " sfx_##a }

typedef void (*opfn_t)(cpu6502 *c, int& dst, const int src);

struct opcode_t {
  opfn_t      eval;
  int         arg;
  int         flag;

  const char *dstr;
};

int instr_len(opcode_t *opc) {
  return ((opc->arg >> 4) & 0xF) + 1;
}

opcode_t opmap[256];

/* Format string for disassembly */
#define sfx_IMP   ""
#define sfx_ACC   "A"
#define sfx_IMM   "#@b"
#define sfx_ZPG   "$@b"
#define sfx_ZPX   "$@b,X"
#define sfx_ZPY   "$@b,Y"
#define sfx_ABS   "$@w"
#define sfx_ABX   "$@w,X"
#define sfx_ABY   "$@w,Y"
#define sfx_IXX   "($@b,X)"
#define sfx_IXY   "($@b),Y"
#define sfx_PCIND "(@w)"
#define sfx_PCABS "@w"
#define sfx_PCREL "@j"
#define sfx_PCRET ""

static void mktab() {
  // SZVC : dst = A + src + C
  _(0x69, ADC, IMM, 2);
  _(0x65, ADC, ZPG, 3);
  _(0x75, ADC, ZPX, 4);
  _(0x6D, ADC, ABS, 4);
  _(0x7D, ADC, ABX, 4+CROSS);
  _(0x79, ADC, ABY, 4+CROSS);
  _(0x61, ADC, IXX, 6);
  _(0x71, ADC, IXY, 5+CROSS);

  // SZ : dst = A & src
  _(0x29, AND, IMM, 2);
  _(0x25, AND, ZPG, 3);
  _(0x35, AND, ZPX, 4);
  _(0x2D, AND, ABS, 4);
  _(0x3D, AND, ABX, 4+CROSS);
  _(0x39, AND, ABY, 4+CROSS);
  _(0x21, AND, IXX, 6);
  _(0x31, AND, IXY, 5+CROSS);

  // SZC, dst = src << 1
  _(0x0A, ASL, ACC, 2);
  _(0x06, ASL, ZPG, 5);
  _(0x16, ASL, ZPX, 6);
  _(0x0E, ASL, ABS, 6);
  _(0x1E, ASL, ABX, 7);

  // NVZ
  _(0x24, BIT, ZPG, 3);
  _(0x2C, BIT, ABS, 4);

  // Conditional branch
  _(0x10, BPL, PCREL, 2+BRANCH);
  _(0x30, BMI, PCREL, 2+BRANCH);
  _(0x50, BVC, PCREL, 2+BRANCH);
  _(0x70, BVS, PCREL, 2+BRANCH);
  _(0x90, BCC, PCREL, 2+BRANCH);
  _(0xB0, BCS, PCREL, 2+BRANCH);
  _(0xD0, BNE, PCREL, 2+BRANCH);
  _(0xF0, BEQ, PCREL, 2+BRANCH);

  _(0x4C, JMP, PCABS, 3); // PC=_get16
  _(0x6C, JMP, PCIND, 5); // PC=get16(_get16)
  _(0x20, JSR, PCABS, 6); // push(PC+2); PC=_get16()
  _(0x00, BRK, IMP,   7);

  // SZC
  _(0xC9, CMP, IMM, 2);
  _(0xC5, CMP, ZPG, 3);
  _(0xD5, CMP, ZPX, 4);
  _(0xCD, CMP, ABS, 4);
  _(0xDD, CMP, ABX, 4+CROSS);
  _(0xD9, CMP, ABY, 4+CROSS);
  _(0xC1, CMP, IXX, 6);
  _(0xD1, CMP, IXY, 5+CROSS);

  // SZC
  _(0xE0, CPX, IMM, 2);
  _(0xE4, CPX, ZPG, 3);
  _(0xEC, CPX, ABS, 4);

  // SZC
  _(0xC0, CPY, IMM, 2);
  _(0xC4, CPY, ZPG, 3);
  _(0xCC, CPY, ABS, 4);

  // SZ : dst = src - 1
  _(0xC6, DEC, ZPG, 5);
  _(0xD6, DEC, ZPX, 6);
  _(0xCE, DEC, ABS, 6);
  _(0xDE, DEC, ABX, 7);
  _(0xCA, DEX, IMP, 2);
  _(0x88, DEY, IMP, 2);

  // SZ : dst = A ^ src
  _(0x49, EOR, IMM, 2);
  _(0x45, EOR, ZPG, 3);
  _(0x55, EOR, ZPX, 4);
  _(0x4D, EOR, ABS, 4);
  _(0x5D, EOR, ABX, 4+CROSS);
  _(0x59, EOR, ABY, 4+CROSS);
  _(0x41, EOR, IXX, 6);
  _(0x51, EOR, IXY, 5+CROSS);

  _(0x18, CLC, IMP, 2);  //C=0
  _(0x38, SEC, IMP, 2);  //C=1
  _(0x58, CLI, IMP, 2);  //I=0
  _(0x78, SEI, IMP, 2);  //I=1
  _(0xB8, CLV, IMP, 2);  //V=0
  _(0xD8, CLD, IMP, 2);  //D=0
  _(0xF8, SED, IMP, 2);  //D=1

  // SZ : dst = src + 1
  _(0xE6, INC, ZPG, 5);
  _(0xF6, INC, ZPX, 6);
  _(0xEE, INC, ABS, 6);
  _(0xFE, INC, ABX, 7);
  _(0xE8, INX, IMP, 2);
  _(0xC8, INY, IMP, 2);

  // SZ : dst = src
  _(0xA9, LDA, IMM, 2); //ok
  _(0xA5, LDA, ZPG, 3); //ok
  _(0xB5, LDA, ZPX, 4); //ok
  _(0xAD, LDA, ABS, 4); //ok
  _(0xBD, LDA, ABX, 4+CROSS); //ok
  _(0xB9, LDA, ABY, 4+CROSS); //ok
  _(0xA1, LDA, IXX, 6);       //ok
  _(0xB1, LDA, IXY, 5+CROSS); //ok

  // SZ : dst = src
  _(0xA2, LDX, IMM, 2);
  _(0xA6, LDX, ZPG, 3);
  _(0xB6, LDX, ZPY, 4);
  _(0xAE, LDX, ABS, 4);
  _(0xBE, LDX, ABY, 4+CROSS);

  // SZ : dst = src
  _(0xA0, LDY, IMM, 2);
  _(0xA4, LDY, ZPG, 3);
  _(0xB4, LDY, ZPX, 4);
  _(0xAC, LDY, ABS, 4);
  _(0xBC, LDY, ABX, 4+CROSS);

  // SZC : dst = src >> 1
  _(0x4A, LSR, ACC, 2);
  _(0x46, LSR, ZPG, 5);
  _(0x56, LSR, ZPX, 6);
  _(0x4E, LSR, ABS, 6);
  _(0x5E, LSR, ABX, 7);

  _(0xEA, NOP, IMP, 2);

  // SZ : dst = A | src
  _(0x09, ORA, IMM, 2);
  _(0x05, ORA, ZPG, 3);
  _(0x15, ORA, ZPX, 4);
  _(0x0D, ORA, ABS, 4);
  _(0x1D, ORA, ABX, 4+CROSS);
  _(0x19, ORA, ABY, 4+CROSS);
  _(0x01, ORA, IXX, 6);
  _(0x11, ORA, IXY, 5+CROSS);

  // SZ : dst = src
  _(0xAA, TAX, IMP, 2);
  _(0x8A, TXA, IMP, 2);
  _(0xA8, TAY, IMP, 2);
  _(0x98, TYA, IMP, 2);
  _(0x9A, TXS, IMP, 2);
  _(0xBA, TSX, IMP, 2);

  // SZC : dst = (src << 1) | C
  _(0x2A, ROL, ACC, 2);
  _(0x26, ROL, ZPG, 5);
  _(0x36, ROL, ZPX, 6);
  _(0x2E, ROL, ABS, 6);
  _(0x3E, ROL, ABX, 7);

  // SZC : dst = (src >> 1) | (C << 7)
  _(0x6A, ROR, ACC, 2);
  _(0x66, ROR, ZPG, 5);
  _(0x76, ROR, ZPX, 6);
  _(0x6E, ROR, ABS, 6);
  _(0x7E, ROR, ABX, 7);

  // SVZC : dst = A-src-C
  _(0xE9, SBC, IMM, 2);
  _(0xE5, SBC, ZPG, 3);
  _(0xF5, SBC, ZPX, 4);
  _(0xED, SBC, ABS, 4);
  _(0xFD, SBC, ABX, 4+CROSS);
  _(0xF9, SBC, ABY, 4+CROSS);
  _(0xE1, SBC, IXX, 6);
  _(0xF1, SBC, IXY, 5+CROSS);

  // dst = src
  _(0x85, STA, ZPG, 3+WRONLY);
  _(0x95, STA, ZPX, 4+WRONLY);
  _(0x8D, STA, ABS, 4+WRONLY);
  _(0x9D, STA, ABX, 5+WRONLY);
  _(0x99, STA, ABY, 5+WRONLY);
  _(0x81, STA, IXX, 6+WRONLY);
  _(0x91, STA, IXY, 6+WRONLY);

  // dst = src
  _(0x86, STX, ZPG, 3+WRONLY);
  _(0x96, STX, ZPY, 4+WRONLY);
  _(0x8E, STX, ABS, 4+WRONLY);

  // dst = src
  _(0x84, STY, ZPG, 3+WRONLY);
  _(0x94, STY, ZPX, 4+WRONLY);
  _(0x8C, STY, ABS, 4+WRONLY);

  // FLAGS=POP; PC=POP
  _(0x40, RTI, PCRET, 6);

  // PC=POP+1
  _(0x60, RTS, PCRET, 6);

  // PUSH A / A=POP
  _(0x48, PHA, IMP, 3);
  _(0x68, PLA, IMP, 4);

  // PUSH FLAGS / POP FLAGS
  _(0x08, PHP, IMP, 3);
  _(0x28, PLP, IMP, 4);

  /* Fake opcodes */
  U(0x0F, SLO, ABS, 6);  // A |= (asl.MEM)
  U(0x1F, SLO, ABX, 7);
  U(0x1B, SLO, ABY, 7);
  U(0x07, SLO, ZPG, 5);
  U(0x17, SLO, ZPX, 6);
  U(0x03, SLO, IXX, 8);
  U(0x13, SLO, IXY, 8);

  U(0x2F, RLA, ABS, 6);  // A &= (rol.MEM)
  U(0x3F, RLA, ABX, 7);
  U(0x3B, RLA, ABY, 7);
  U(0x27, RLA, ZPG, 5);
  U(0x37, RLA, ZPX, 6);
  U(0x23, RLA, IXX, 8);
  U(0x33, RLA, IXY, 8);

  U(0x4F, LSE, ABS, 6);  // A ^= (lsr.MEM)
  U(0x5F, LSE, ABX, 7);
  U(0x5B, LSE, ABY, 7);
  U(0x47, LSE, ZPG, 5);
  U(0x57, LSE, ZPX, 6);
  U(0x43, LSE, IXX, 8);
  U(0x53, LSE, IXY, 8);

  U(0x6F, RRA, ABS, 6);  // A += (ror.MEM) + C
  U(0x7F, RRA, ABX, 7);
  U(0x7B, RRA, ABY, 7);
  U(0x67, RRA, ZPG, 5);
  U(0x77, RRA, ZPX, 6);
  U(0x63, RRA, IXX, 8);
  U(0x73, RRA, IXY, 8);

  U(0x8F, AXS, ABS, 4); // MEM=A&X
  U(0x87, AXS, ZPG, 3);
  U(0x97, AXS, ZPY, 4);
  U(0x83, AXS, IXX, 6);

  U(0xAF, LAX, ABS, 4);  // A=X=MEM
  U(0xBF, LAX, ABY, 4+CROSS);
  U(0xA7, LAX, ZPG, 3);
  U(0xB7, LAX, ZPY, 4);
  U(0xA3, LAX, IXX, 6);
  U(0xB3, LAX, IXY, 5+CROSS);

  U(0xCF, DCM, ABS, 6);
  U(0xDF, DCM, ABX, 7);
  U(0xDB, DCM, ABY, 7);
  U(0xC7, DCM, ZPG, 5);  // DEC(MEM); CMP(MEM);
  U(0xD7, DCM, ZPX, 6);
  U(0xC3, DCM, IXX, 8);
  U(0xD3, DCM, IXY, 8);

  U(0xEF, INS, ABS, 6);  // INC(MEM); SBC(MEM);
  U(0xFF, INS, ABX, 7);
  U(0xFB, INS, ABY, 7);
  U(0xE7, INS, ZPG, 5);
  U(0xF7, INS, ZPX, 6);
  U(0xE3, INS, IXX, 8);
  U(0xF3, INS, IXY, 8);

  U(0x4B, ALR, IMM, 2); // LSR(A & imm)
  U(0x6B, ARR, IMM, 2); // ROR(A & imm)

  U(0x8b, XAA, IMM, 2); // A=(X & imm)

  U(0xab, OAL, IMM, 2); // A=X=((A | 0xEE) & imm)

  U(0xCB, SAX, IMM, 2); // X=((A & X) - imm)

  U(0x1A, NOP, IMP, 2);
  U(0x3A, NOP, IMP, 2);
  U(0x5A, NOP, IMP, 2);
  U(0x7A, NOP, IMP, 2);
  U(0xDA, NOP, IMP, 2);
  U(0xFA, NOP, IMP, 2);

  // DOP (double NOP)
  U(0x04, SKB, ZPG, 3+SKIP);
  U(0x14, SKB, ZPX, 4+SKIP);
  U(0x34, SKB, ZPX, 4+SKIP);
  U(0x44, SKB, ZPG, 3+SKIP);
  U(0x54, SKB, ZPX, 4+SKIP);
  U(0x64, SKB, ZPG, 3+SKIP);
  U(0x74, SKB, ZPX, 4+SKIP);
  U(0x80, SKB, IMM, 2+SKIP);
  U(0x82, SKB, IMM, 2+SKIP);
  U(0x89, SKB, IMM, 2+SKIP);
  U(0xC2, SKB, IMM, 2+SKIP);
  U(0xD4, SKB, ZPX, 4+SKIP);
  U(0xE2, SKB, IMM, 2+SKIP);
  U(0xF4, SKB, ZPX, 4+SKIP);

  U(0x0C, SKW, ABS, 4+SKIP);
  U(0x1C, SKW, ABX, 4+CROSS+SKIP);
  U(0x3C, SKW, ABX, 4+CROSS+SKIP);
  U(0x5C, SKW, ABX, 4+CROSS+SKIP);
  U(0x7C, SKW, ABX, 4+CROSS+SKIP);
  U(0xDC, SKW, ABX, 4+CROSS+SKIP);
  U(0xFC, SKW, ABX, 4+CROSS+SKIP);

  U(0x02, HLT, IMP, 2);
  U(0x12, HLT, IMP, 2);
  U(0x22, HLT, IMP, 2);
  U(0x32, HLT, IMP, 2);
  U(0x42, HLT, IMP, 2);
  U(0x52, HLT, IMP, 2);
  U(0x62, HLT, IMP, 2);
  U(0x72, HLT, IMP, 2);
  U(0x92, HLT, IMP, 2);
  U(0xb2, HLT, IMP, 2);
  U(0xd2, HLT, IMP, 2);
  U(0xf2, HLT, IMP, 2);

  U(0x0b, AAC, IMM, 2);
  U(0x2b, AAC, IMM, 2);

  U(0x9F, AXA, ABY, 5);
  U(0x93, AXA, IXY, 6);

  U(0x9B, XAS, ABY, 5);

  U(0x9c, SYA, ABX, 5);
  U(0x9e, SXA, ABY, 5);

  U(0xbb, LAR, ABY, 4+CROSS);

  U(0xeb, SBC, IMM, 2);

  for (int i = 0; i < 256; i++) {
    printf("%.3s:%.2x ", opmap[i].dstr ? opmap[i].dstr : "---", opmap[i].arg);
    if ((i & 0xF) == 0xf) {
      printf("\n");
    }
  }
};

constexpr bool crosspg(const uint16_t offset, const uint8_t delta) {
  return ((offset & 0xFF) + delta) > 0xFF;
}

const int mkreg(uint8_t& reg) {
  return TYPE_REG | reg;
}

const int mkimm(int v) {
  return TYPE_IMM | v;
}

const int mkmem(int off, uint8_t delta) {
  int nv = off + delta;

  if (crosspg(off, delta))
    nv |= CROSS;
  return TYPE_MEM | nv;
}

int _zpg(uint8_t base, uint8_t delta) {
  base += delta;
  return TYPE_MEM | base;
}
int _abs(uint16_t base, uint8_t delta) {
  // masks out upper bits if passed in zpg
  int rv = TYPE_MEM + base + delta;
  if (crosspg(base, delta)) {
    rv |= CROSS;
  }
  return rv;
}

/* Get argument type from opcode bytes
 *   TYPE_MEM
 *   TYPE_IMM
 *   TYPE_REG
 */
int getarg(int arg, const uint8_t *ib) {
  int hi, w = ib[0] + (ib[1] << 8);
  uint8_t lo;

  lo = ib[0];
  switch (arg) {
  case ACC:
    return mkreg(A);
  case IMM:
    return mkimm(lo);
  case ZPG:
    return _zpg(lo, 0);
  case ZPX:
    return _zpg(lo, X);
  case ZPY:
    return _zpg(lo, Y);
  case ABS:
    return _abs(w, 0);
  case ABX:
    return _abs(w, X);
  case ABY:
    return _abs(w, Y);
  case IXX:
    /* lo will auto-wrap */
    lo = _zpg(lo, X);
    return _abs(cpu_read16(lo), 0);
  case IXY:
    /* load word at address lo */
    lo = _zpg(lo, 0);
    return _abs(cpu_read16(lo), Y);
  case PCABS:
    return mkimm(w);
  case PCREL:
    w = PC + (int8_t)lo;
    if ((w & 0xFF00) != (PC & 0xFF00)) {
      w |= CROSS;
    }
    return mkimm(PC + (int8_t)lo);
  case PCIND:
    if (lo != 0xFF) {
      return mkimm(cpu_read16(w));
    }
    /* Special case, read across page */
    lo = cpu_read8(w);
    hi = cpu_read8(w - 0xFF);
    return mkimm((hi << 8)+lo);
  case PCRET:
  case IMP:
    return 0;
  }
  printf("NOPARSE: %x\n", arg);
  exit(0);
  return 0;
}

/* Generate CPU NMI
 * Vector: FFFA nmi
 */
void cpu_nmi() {
  cpu_push16(PC);
  cpu_push8(cpu_getflags());
  If = 1;
  PC = cpu_read16(0xFFFA);
}

/* Generate CPU IRQ
 * Vector: FFFE irq
 */
bool cpu_irq(int n) {
  if (If)
    return false;
  cpu_push16(PC);
  cpu_push8(cpu_getflags());
  PC = cpu_read16(0xFFFE);
  printf("\nIRQ: %d %.4x\n", n, PC);
  If = true;
  return true;
}

/* Reset CPU and set initial PC
 * Vector: FFFC reset
 */
void cpu_reset(uint32_t ipc) {
  mktab();
  A = X = Y;
  S = 0x0;
  cpu_setflags(0);
  if (ipc == 0)
    ipc = cpu_read16(0xFFFC);
  PC = ipc;
  printf("Reset PC: %.4x\n", PC);
}

/* Disassemble instruction */
const char *cpu_dis(const char *src, uint8_t *ib) {
  static char dst[64], *p;

  p = dst;
  memset(dst, 0, sizeof(dst));
  while (*src != 0) {
    if (replace(&src, "@b", &p, "%.2x", ib[0]) ||
	replace(&src, "@w", &p, "%.2x%.2x", ib[1], ib[0]) ||
	replace(&src, "@j", &p, "%.4x", SPC + 2 + (int8_t)ib[0])) {
      continue;
    }
    *p++ = *src++;
  };
  *p = 0;
  return dst;
}

static void mkhex(char *dst, const uint8_t *src, int n)
{
  const char hext[] = "0123456789abcdef";
  for (int i = 0; i <= n; i++) {
    *dst++ = hext[(*src >> 4)];
    *dst++ = hext[*src & 0xF];
    src++;
  }
}

/* Returns number of cycles executed */
#define nib(x) (((x)->arg >> 4) & 0xF)

int cpu_step() {
  uint8_t op, ib[3];
  char bstr[12];
  opcode_t *opc;
  int offset = -1, src, dst = -1, cyc;

  if (hlt) {
    printf("halted...\n");
    totcyc += 1;
    return 1;
  }
  SPC = PC;

  /* Fetch instruction bytes */
  totinst++;
  op = cpu_fetch8();
  ib[0] = op;
  opc = &opmap[op];
  for (int i = 1; i <= nib(opc); i++) {
    ib[i] = cpu_fetch8();
  }
  strcpy(bstr, "------");
  mkhex(bstr, ib, nib(opc));
  
  /* Get Source */
  offset = getarg(opc->arg, &ib[1]);
  if ((offset & TYPE_MEM) && !(opc->flag & WRONLY)) {
    src = cpu_read8(offset & 0xFFFF);
  }
  else {
    src = offset & 0xFFFF;
  }
  /* Get number of cycles */
  extracyc = 0;
  cyc = opc->flag & 0xF;
  if ((offset & opc->flag) & CROSS) {
    cyc++;
  }
  if (trace) {
    assert(opc->dstr);
    flogger(trace, "%s| cyc=%d %s %s\n",
	    cpu_getstate(), cyc, bstr, cpu_dis(opc->dstr, ib+1));
    //flogger(-2,"  src = %.8x %.8x\n", offset, src);
  }

  dst = -1;
  setmask = snz;
  opc->eval(&thecpu, dst, src & 0xFFFF);

  /* Set result flags at end of operation (snz is cleared) */
  if ((setmask & snz) == 0) {
    Zf = (setmask & 0xFF) == 0;
    Nf = (setmask & 0x80) != 0;
  }
  if ((setmask & (clrc|setc)) != 0) {
    Cf = (setmask & setc) != 0;
  }

  /* Set destination */
  if (dst != -1) {
    if (offset & TYPE_MEM)
      cpu_write8(offset & 0xFFFF, dst);
    else if (offset & TYPE_REG)
      A = dst;
    else {
      exit(0);
    }
  }
  cyc += extracyc;
  totcyc += cyc;
  
  return cyc;
}

#if (defined(TEST) || defined(DSTK))
#include <fcntl.h>
#include <stdarg.h>
#include "dstk.h"

uint8_t ram[65536];

void flogger(int lvl, const char *fmt, ...) {
  va_list ap;

  va_start(ap, fmt);
  vprintf(fmt, ap);
}

uint8_t cpu_read8(const uint32_t addr, int mode) {
  assert(addr <= 0xFFFF);
  //printf("Read : %.4x = %.2x\n", addr, ram[addr]);
  return ram[addr];
}

void cpu_write8(const uint32_t addr, uint8_t nv, int mode) {
  assert(addr <= 0xFFFF);
  //printf("Write: %.4x = %.2x\n", addr, nv);
  ram[addr] = nv;
}

#define get16(x) *(uint16_t *)(x)
#endif

dstk stk(65536, printf);

/* create control-flow of executable code */
void dumpcfg(int noff, int *o, int base, int size)
{
  uint8_t ib[3], op, nib;
  opcode_t *opc;
  int nxt[3], addr, off;
  const char *sep = "";

  for (int i = 0; i < noff; i++) {
    printf(">> push %.4x\n", *o);
    stk.push(*o++, 1, dstk::PENDING, "first");
  }
  while ((off = stk.pop()) != -1) {
    printf("%s%.4x ========================== %.4x\n", sep, off, off);
    PC = off;
    do {
      sep = "";
      off = PC;
      op  = cpu_fetch8();
      opc = &opmap[op];
      addr = -1;
      
      /* Get instruction bytes */
      ib[0] = op;
      ib[1] = 0;
      ib[2] = 0;
      nib = instr_len(opc);
      for (int i=1; i < nib; i++) {
	ib[i] = cpu_fetch8();
      }

      /* Set next visit offset */
      nxt[0] = PC;
      nxt[1] = -1;
      switch (opc->arg) {
      case PCREL:
	/* COND jump, 2 offsets */
	nxt[0] = PC;
	nxt[1] = PC + (int8_t)ib[1];
	/* Poke absolute value back into ib for disassembly */
	put16(&ib[1], nxt[1]);
	break;
      case PCABS:
	/* COND or UNCOND jump, 1 or 2 offsets */
	nxt[0] = PC;
	nxt[1] = get16(&ib[1]);
	if (op == 0x4C) {
	  // uncond jmp
	  nxt[0] = -1;
	}
	break;
      case PCRET:
      case PCIND:
	/* Return: TERMINATE */
	nxt[0] = -1;
	nxt[1] = -1;
	sep = "\n\n";
	break;
      case ABS:
      case ABX:
      case ABY:
	/* memory reads */
	addr = get16(&ib[1]);
	stk.push(addr, 1, dstk::MEM, "abs");
	break;
      case ZPG:
      case ZPX:
      case ZPY:
	/* memory reads */
	addr = ib[1];
	stk.push(addr, 1, dstk::MEM, "zpg");
	break;
      case IXX:
      case IXY:
	/* memory reads */
	addr = ib[1];
	stk.push(addr, 2, dstk::MEM, "ixx");
	break;
      }
      printf("%.4x: %.2x%.2x%.2x | %s\t\t%s\n",
	     off, ib[0], ib[1], ib[2], cpu_dis(opc->dstr, ib+1),
	     comment(addr));

      /* Mark visited */
      stk.push(off, nib, dstk::CODE, "codez");

      /* Ignore next destination if out of range */
      if (nxt[1] >= base+size || nxt[1] < base) {
	nxt[1] = -1;
      }
      stk.push(nxt[0], 1, dstk::PENDING, "nxt1");
      stk.push(nxt[1], 1, dstk::PENDING, "nxt2");
      /* Keep in same codeblock if we can */
    } while (nxt[0] != -1 && nxt[1] == -1);
  }
  stk.showstk(128);
}

#if (defined(TEST) || defined(DSTK))
/* Start CPU at one address, run until it hits another address */
void runtest(int start, int end, const char *file) {
  int fd;
  
  if ((fd = open(file, O_RDONLY)) < 0)
    return;
  printf("-- test: %s\n", file);
  read(fd, ram, 65536);
  close(fd);

  cpu_reset(start);
  while (PC != end) {
    cpu_step();
  }
  printf("final PC: %x\n", PC);
}
int main(int argc, char *argv[]) {
  int fd, cycs;
  size_t sz;
  uint8_t *ptr;

  setbuf(stdout, NULL);
#ifdef DSTK
  uint32_t base = 0x0000, start = 0x8000;

  fd = open(argv[1], O_RDONLY);
  if (fd < 0)
    return -1;
  sz = lseek(fd, 0, SEEK_END);
  lseek(fd, 0, SEEK_SET);
  ptr = (uint8_t *)malloc(sz);
  read(fd, ptr, sz);
  
  if (strstr(argv[1], ".crt") || strstr(argv[1],".CRT")) {
    base  = 0x7FB0;
    start = *(uint16_t *)&ptr[0x50];
  }
  if (strstr(argv[1], ".nes")) {
    base  = 0x7FB0;
    start = *(uint16_t *)&ptr[0x50];
  }
  if (argc > 3) {
    for (int i = 4; i < argc; i++) {
      base = strtoul(argv[i], NULL, 0);
      stk.push(base, 1, dstk::PENDING, "extra");
    }
    base  = strtoul(argv[2], NULL, 0);
    start = strtoul(argv[3], NULL, 0);
  }
  printf("base+size: %.4x %.4x\n", base+sz, start);
  assert(base + sz <= 0x10000);
  memcpy(&ram[base], ptr, sz);
  cpu_reset(0);

  assert(start <= 0xFFFE);
  PC = *(uint16_t *)&ram[start];
  
  printf ("PC = %.4x\n", start);
  dumpcfg(start, base, sz);
#else
  /* 6502_functional_test.bin ends with 0xf0 */
  trace=0;
  runtest(0x400, 0x3469, "6502_functional_test.bin");
  printf("total cycles: %lld\n", totcyc);
  printf("total instrs: %lld\n", totinst);
#endif
}
#endif
