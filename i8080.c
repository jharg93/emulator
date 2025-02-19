#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <inttypes.h>
#include "gr.h"
#include "dstk.h"
#include <functional>

static uint8_t get8(const void *p) { return *(uint8_t *)p; }
static void    put8(void *p, uint8_t v) { *(uint8_t *)p = v; }

static uint16_t get16(const void *p) { return *(uint16_t *)p; }

#ifndef _SDL
#include <time.h>
uint32_t SDL_GetTicks() {
  struct timespec ts;
  clock_gettime(CLOCK_MONOTONIC, &ts);
  return (ts.tv_sec * 1000 + (ts.tv_nsec / 1000000.0));
}
#endif

//#define TEST
int trace=0;
int totcyc;

Screen *s;

void flogger(int lvl, const char *lbl, ...)
{
  va_list ap;

  va_start(ap,lbl);
  vfprintf(stdout, lbl, ap);
}

#define BRANCH_TAKEN 0xFF00

enum {
  TYPE_SHIFT = 24,
  TYPE_MASK  = (0xFF << TYPE_SHIFT),
  TYPE_IMM   = 0x1 << TYPE_SHIFT,
  TYPE_REG   = 0x2 << TYPE_SHIFT,
  TYPE_RP    = 0x3 << TYPE_SHIFT,
  TYPE_MEM   = 0x4 << TYPE_SHIFT,
  TYPE_OPARG = 0x5 << TYPE_SHIFT,

  VAL_MASK   = 0xFFFF,
  JMP,
  JCC,
  CALL,
  RET,
  RCC,
  RST,

  RB = TYPE_REG+0,
  RC,
  RD,
  RE,
  RH,
  RL,
  RF,
  RA,

  RBC = TYPE_RP+0,
  RDE = TYPE_RP+2,
  RHL = TYPE_RP+4,
  RAF = TYPE_RP+6,
  RSP = TYPE_RP+8,
  
  imp = TYPE_OPARG,
  MHL,
  I16,
  I8,

};

uint8_t reg[9];

/* Xor the addresses by 1, makes HL/DE/BC little endian */
uint8_t& B = reg[0];
uint8_t& C = reg[1];
uint8_t& D = reg[2];
uint8_t& E = reg[3];
uint8_t& H = reg[4];
uint8_t& L = reg[5];
uint8_t& F = reg[6];
uint8_t& A = reg[7];

int SP, PC, fI = 1;

template <int mask> struct cpuflag {
  operator bool() const { return F & mask; }
  cpuflag& operator=(bool v) {
    F = v ? (F | mask) : (F & ~mask);
    return *this;
  }
};
cpuflag<0x01> fC;
cpuflag<0x04> fP;
cpuflag<0x10> fA;
cpuflag<0x40> fZ;
cpuflag<0x80> fS;

int  Rp(int op);
void setRp(int rp, int v);

uint8_t mem[65536];
uint8_t ports[65536];

dstk stk(65536);

enum {
  None = 0,
  SET_S = 0x80,
  SET_Z = 0x40,
  SET_P = 0x04,
  SET_ZP = SET_Z|SET_P,
  SET_SP = SET_S|SET_P,
  SET_SZP = SET_S|SET_Z|SET_P,
};

// Fast set flags szp
static uint8_t flagtbl[] = {
    SET_ZP, None,   None,   SET_P,  None,   SET_P,  SET_P,  None,   None,   SET_P,  SET_P,  None,   SET_P,  None,   None,   SET_P,
    None,   SET_P,  SET_P,  None,   SET_P,  None,   None,   SET_P,  SET_P,  None,   None,   SET_P,  None,   SET_P,  SET_P,  None,
    None,   SET_P,  SET_P,  None,   SET_P,  None,   None,   SET_P,  SET_P,  None,   None,   SET_P,  None,   SET_P,  SET_P,  None,
    SET_P,  None,   None,   SET_P,  None,   SET_P,  SET_P,  None,   None,   SET_P,  SET_P,  None,   SET_P,  None,   None,   SET_P,
    None,   SET_P,  SET_P,  None,   SET_P,  None,   None,   SET_P,  SET_P,  None,   None,   SET_P,  None,   SET_P,  SET_P,  None,
    SET_P,  None,   None,   SET_P,  None,   SET_P,  SET_P,  None,   None,   SET_P,  SET_P,  None,   SET_P,  None,   None,   SET_P,
    SET_P,  None,   None,   SET_P,  None,   SET_P,  SET_P,  None,   None,   SET_P,  SET_P,  None,   SET_P,  None,   None,   SET_P,
    None,   SET_P,  SET_P,  None,   SET_P,  None,   None,   SET_P,  SET_P,  None,   None,   SET_P,  None,   SET_P,  SET_P,  None,
    SET_S,  SET_SP, SET_SP, SET_S,  SET_SP, SET_S,  SET_S,  SET_SP, SET_SP, SET_S,  SET_S,  SET_SP, SET_S,  SET_SP, SET_SP, SET_S,
    SET_SP, SET_S,  SET_S,  SET_SP, SET_S,  SET_SP, SET_SP, SET_S,  SET_S,  SET_SP, SET_SP, SET_S,  SET_SP, SET_S,  SET_S,  SET_SP,
    SET_SP, SET_S,  SET_S,  SET_SP, SET_S,  SET_SP, SET_SP, SET_S,  SET_S,  SET_SP, SET_SP, SET_S,  SET_SP, SET_S,  SET_S,  SET_SP,
    SET_S,  SET_SP, SET_SP, SET_S,  SET_SP, SET_S,  SET_S,  SET_SP, SET_SP, SET_S,  SET_S,  SET_SP, SET_S,  SET_SP, SET_SP, SET_S,
    SET_SP, SET_S,  SET_S,  SET_SP, SET_S,  SET_SP, SET_SP, SET_S,  SET_S,  SET_SP, SET_SP, SET_S,  SET_SP, SET_S,  SET_S,  SET_SP,
    SET_S,  SET_SP, SET_SP, SET_S,  SET_SP, SET_S,  SET_S,  SET_SP, SET_SP, SET_S,  SET_S,  SET_SP, SET_S,  SET_SP, SET_SP, SET_S,
    SET_S,  SET_SP, SET_SP, SET_S,  SET_SP, SET_S,  SET_S,  SET_SP, SET_SP, SET_S,  SET_S,  SET_SP, SET_S,  SET_SP, SET_SP, SET_S,
    SET_SP, SET_S,  SET_S,  SET_SP, SET_S,  SET_SP, SET_SP, SET_S,  SET_S,  SET_SP, SET_SP, SET_S,  SET_SP, SET_S,  SET_S,  SET_SP,
};  

/*===============================================
 * I/O ports
 *===============================================*/
int test_done = 0;

enum {
  rINP0    = 0x00,
  //   0x01 ; CREDIT 1
  //   0x02 : 27P start
  //   0x04 : 1P start
  //   0x08 : '1'
  //   0x10 : 1p shot
  //   0x20 : 1p left
  //   0x40 : 1p right
  rINP1    = 0x01,
  rINP2    = 0x02,
  rSHIFTIN = 0x03,

  wSHIFTAMT  = 0x02,
  wSHIFTDATA = 0x04,
  wSOUND1    = 0x03,
  wSOUND2    = 0x05,
};

uint16_t shift_amt;
uint16_t shift_data;

uint8_t rdport(int port) {
  if (port == rSHIFTIN)
    return (shift_data >> shift_amt);
  else if (port >= rINP0 && port <= rINP2)
    return ports[port];
  return 0;
}

void wrport(int port, uint8_t v) {
  if (port == wSHIFTAMT)
    shift_amt = (8 - v);
  else if (port == wSHIFTDATA)
    shift_data = (v << 8) | (shift_data >> 8);
  if (port == 0x0)
    test_done = 1;
  else if (port == 1) {
    if (C == 0x09) {
      for (int i = Rp(RDE); mem[i] != '$'; i++) {
	fputc(mem[i], stdout);
      }
    }
    if (C == 0x02) {
      fprintf(stdout, "%c", E);
    }
  }    
}

/*====================================================
 * Memory Read/Write
 *===================================================*/
void cpu_reset(uint32_t r)
{
}

uint8_t cpu_read8(uint32_t off, int mode = dstk::MEM) {
  stk.push(off, 1, mode);
  return mem[off];
}

void cpu_write8(uint32_t off, uint8_t v, int mode = dstk::MEM) {
  stk.push(off, 1, mode);
  mem[off] = v;
}

uint16_t cpu_read16(uint32_t off, int mode = dstk::MEM) {
  uint16_t lo, hi;

  lo = cpu_read8(off, mode);
  hi = cpu_read8(off+1, mode);
  return (hi << 8) | lo;
}

void cpu_write16(uint32_t off, uint16_t nv, int mode = dstk::MEM) {
  cpu_write8(off, nv, mode);
  cpu_write8(off+1, nv >> 8, mode);
}

// Register pair, special cases for SP
int Rp(int op) {
  int n = op & VAL_MASK;
  
  assert((op & TYPE_MASK) == TYPE_RP);
  if (op == RSP)
    return SP;
  return (reg[n] << 8) + reg[n+1];
}

// Set Register pair, special case for SP
void setRp(int op, int v) {
  int n = op & VAL_MASK;
  
  assert((op & TYPE_MASK) == TYPE_RP);
  if (op == RSP)
    SP = v;
  else {
    reg[n]   = v >> 8;
    reg[n+1] = v;
  }
}

// register
static uint8_t& Rd(int op) {
  assert((op & TYPE_MASK) == TYPE_REG);
  return reg[op & VAL_MASK];
}

/*===================================
 * Return opcode argument value
 *===================================*/
static int getval(int op, int arg) {
  switch (arg) {
  case RB ... RA:   return Rd(arg);
  case RBC ... RSP: return Rp(arg);
  case RST:         return (op & 0x38);
  case MHL:         return cpu_read8(Rp(RHL));
  case I8:          return cpu_read8(PC++, dstk::CODE);
  case I16: PC+=2;  return cpu_read16(PC-2, dstk::CODE);
  case imp:
    break;
  case JMP ... RCC:
    break;
  default:
    fprintf(stdout,"unknown getval: %x, %x\n", op, arg);
    exit(0);
  }
  return 0;
}

/*=========================================
 * Set result argument value
 *=========================================*/
static void setval(int dst, int val) {
  switch (dst) {
  case RB ... RA:
    Rd(dst) = val;
    break;
  case RBC ... RSP:
    setRp(dst, val);
    break;
  case MHL:
    cpu_write8(Rp(RHL), val);
    break;
  default:
    fprintf(stdout,"Unknown setval...\n");
    assert(0);
  }
}

/*====================================================
 * Stack push/pop
 *===================================================*/
void cpu_push16(uint16_t v) {
  SP -= 2;
  cpu_write16(SP, v, dstk::STACK);
}

uint16_t cpu_pop16() {
  SP += 2;
  return cpu_read16(SP-2, dstk::STACK);
}

/* Set CPU flags of result */
static void setaf(int af) {
  fA = !!af;
}

static int setfl(int v) {
  fC = (v & ~0xFF) != 0;
  F = (F & ~SET_SZP) | flagtbl[v & 0xFF];
  return v;
}

/* Shift left and set carry bit */
static void _shl(bool c, int bit0) {
  fC = c;
  A = (A << 1) | bit0;
}

/* Shift right and set carry bit */
static void _shr(bool c, int bit7) {
  fC = c;
  A = (A >> 1) | bit7;
}

void setpc(bool test, int nv) {
  if (test)
    PC = nv;
}

/*===============================================*
 * Opcode Handler
 *===============================================*/
static void _nop(int dst, int src, int &cycs)  { };
static void _hlt(int dst, int src, int &cycs)  { };

/* Port I/O */
static void _in(int dst, int src, int &cycs)   { A = rdport(src); };
static void _out(int dst, int src, int &cycs)  { wrport(src, A);  };

static void add(uint8_t src) {
  int res = A + src;
  fA = ((A & 0xF) + (src & 0xF)) >= 0x10;
  A = setfl(res);
}
static void sub(uint8_t src, int cy) {
  add(~src + !cy);
  fC = !fC;
}

/* Math/Logical operations. Set flags */
static void _add(int dst, int src, int &cycs) { add(src); };
static void _adc(int dst, int src, int &cycs) { add(src + fC); }
static void _sub(int dst, int src, int &cycs) { sub(src, 0); }     
static void _sbb(int dst, int src, int &cycs) { sub(src, fC); }
static void _ana(int dst, int src, int &cycs) { setaf((A | src) & 0x8); A = setfl(A & src); }      /* A = setfl(A & Rs); */
static void _xra(int dst, int src, int &cycs) { setaf(0); A = setfl(A ^ src); }      /* A = setfl(A ^ Rs); */
static void _ora(int dst, int src, int &cycs) { setaf(0); A = setfl(A | src); }
static void _cmp(int dst, int src, int &cycs) { setfl(A - src); setaf(~(A ^ (A - src) ^ src) & 0x10); }
static void _cma(int dst, int src, int &cycs) { A = ~A; }
static void _dad(int dst, int src, int &cycs) {
  src += Rp(RHL);
  setRp(RHL, src);
  /* Set carry flag */
  fC = (src >= 0x10000);
}

/* These are identical to above functions, src=I8 */
static void _adi(int dst, int src, int &cycs) { add(src); };
static void _aci(int dst, int src, int &cycs) { add(src+fC); };
static void _sui(int dst, int src, int &cycs) { sub(src,0); };
static void _sbi(int dst, int src, int &cycs) { sub(src,fC); };
static void _ani(int dst, int src, int &cycs) { _ana(dst, src, cycs); };
static void _xri(int dst, int src, int &cycs) { _xra(dst, src, cycs); };
static void _ori(int dst, int src, int &cycs) { _ora(dst, src, cycs); };
static void _cpi(int dst, int src, int &cycs) { _cmp(dst, src, cycs); };

static void _daa(int dst, int ksrc, int &cycs) {
  int cf = fC;
  int delta = 0;
  if (fA || (A & 0x0f) > 0x09)
    delta += 0x06;
  if (cf || (A & 0xF0) > 0x90 || ((A & 0xF0) >= 0x90) && (A & 0xF) > 9) {
    delta += 0x60;
    cf = 1;
  }
  add(delta);
  fC = cf;
}

/* Load/Store */
static void _mov(int dst,  int src, int &cycs) { setval(dst, src); };         /* Rd = Rs */
static void _mvi(int dst,  int src, int &cycs) { setval(dst, src); };         /* Rd = I8 */
static void _lxi(int dst,  int src, int &cycs) { setval(dst, src); };         /* Rp = I16 */
static void _sphl(int dst, int src, int &cycs) { SP = Rp(RHL); };             /* SP = HL */
static void _pchl(int dst, int src, int &cycs) { PC = Rp(RHL); };             /* PC = HL */
static void _shld(int dst, int src, int &cycs) { cpu_write16(src, Rp(RHL)); };      /* mem[I16] = HL */
static void _sta(int dst,  int src, int &cycs) { cpu_write8(src, A); };              /* mem[I16] = A */
static void _stax(int dst, int src, int &cycs) { cpu_write8(src, A); };              /* mem[Rp]  = A */
static void _lhld(int dst, int src, int &cycs) { setRp(RHL, cpu_read16(src)); };   /* HL = mem[I16] */
static void _lda(int dst,  int src, int &cycs) { A = cpu_read8(src); };             /* A = mem[I16] */
static void _ldax(int dst, int src, int &cycs) { A = cpu_read8(src); };             /* A = mem[Rp]  */
static void _push(int dst, int src, int &cycs) { cpu_push16(src); };              /* mem[SP] = Rp */
static void _pop(int dst, int src, int &cycs)  { setRp(dst, cpu_pop16()); };      /* Rp = mem[SP] */

/* Rotate/Shift */
static void _ral(int dst, int src, int &cycs) { _shl(src & 0x80, fC ? 0x01 : 0x00); }
static void _rar(int dst, int src, int &cycs) { _shr(src & 0x01, fC ? 0x80 : 0x00); }
static void _rlc(int dst, int src, int &cycs) { _shl(src & 0x80, A >> 7); }
static void _rrc(int dst, int src, int &cycs) { _shr(src & 0x01, A << 7); }

/* Increment/Decrement */
static void _inx(int dst, int src, int &cycs) { setRp(dst, src+1);  };
static void _dcx(int dst, int src, int &cycs) { setRp(dst, src-1);  };
static void _inr(int dst, int src, int &cycs) {
  ++src;
  setaf((src & 0xF) == 0x0);
  setval(dst, setfl(src));
}
static void _dcr(int dst, int src, int &cycs) {
  --src;
  setaf(!((src & 0xF) == 0xF));
  setval(dst, setfl(src));
}

/* CPU Flags */
static void _di(int dst,  int src, int &cycs) { fI=0; }
static void _ei(int dst,  int src, int &cycs) { fI=1; }
static void _stc(int dst, int src, int &cycs) { fC=1; }
static void _cmc(int dst, int src, int &cycs) { fC=!fC; }

/* Control Flow */
static void _rst(int dst,  int src, int &cycs) { cpu_push16(PC); setpc(true,src); };
static void _call(int dst, int src, int &cycs) { cpu_push16(PC); setpc(true,src); };
static void _jmp(int dst,  int src, int &cycs) { setpc(true, src); };
static void _ret(int dst,  int src, int &cycs) { setpc(true, cpu_pop16()); };

/* Conditional jump */
static void _jnz(int dst, int src, int &cycs) { setpc(!fZ, src); }
static void _jz (int dst, int src, int &cycs) { setpc(fZ,  src); }
static void _jnc(int dst, int src, int &cycs) { setpc(!fC, src); }
static void _jc (int dst, int src, int &cycs) { setpc(fC,  src); }
static void _jpo(int dst, int src, int &cycs) { setpc(!fP, src); }
static void _jpe(int dst, int src, int &cycs) { setpc(fP,  src); }
static void _jp (int dst, int src, int &cycs) { setpc(!fS, src); }
static void _jm (int dst, int src, int &cycs) { setpc(fS,  src); }

/* Conditional return */
void retcc(bool test, int & cycs) {
  if (test) {
    if (cycs & BRANCH_TAKEN)
      cycs >>= 8;
    setpc(true, cpu_pop16());
  }
};
static void _rnz(int dst, int src, int &cycs) { retcc(!fZ, cycs); };
static void _rz (int dst, int src, int &cycs) { retcc(fZ,  cycs); };
static void _rnc(int dst, int src, int &cycs) { retcc(!fC, cycs); };
static void _rc (int dst, int src, int &cycs) { retcc(fC,  cycs); };
static void _rpo(int dst, int src, int &cycs) { retcc(!fP, cycs); };
static void _rpe(int dst, int src, int &cycs) { retcc(fP,  cycs); };
static void _rp (int dst, int src, int &cycs) { retcc(!fS, cycs); };
static void _rm (int dst, int src, int &cycs) { retcc(fS,  cycs); };

/* Conditional call */
void callcc(bool test, int src, int &cycs) {
  if (test) {
    if (cycs & BRANCH_TAKEN)
      cycs >>= 8;
    cpu_push16(PC);
    setpc(true, src);
  }
}
static void _cnz(int dst, int src, int &cycs) { callcc(!fZ, src, cycs); };
static void _cz (int dst, int src, int &cycs) { callcc(fZ,  src, cycs); };
static void _cnc(int dst, int src, int &cycs) { callcc(!fC, src, cycs); };
static void _cc (int dst, int src, int &cycs) { callcc(fC,  src, cycs); };
static void _cpo(int dst, int src, int &cycs) { callcc(!fP, src, cycs); };
static void _cpe(int dst, int src, int &cycs) { callcc(fP,  src, cycs); };
static void _cp (int dst, int src, int &cycs) { callcc(!fS, src, cycs); };
static void _cm (int dst, int src, int &cycs) { callcc(fS,  src, cycs); };

/* Exchange DE/HL */
static void _xchg(int dst, int src, int &cycs) {
  src = Rp(RDE);
  setRp(RDE, Rp(RHL));
  setRp(RHL, src);
};

/* Exchange HL with top of stack */
static void _xhtl(int dst, int src, int &cycs) {
  src = cpu_pop16();
  cpu_push16(Rp(RHL));
  setRp(RHL, src);
}

/* Clocks */
#define C5  ((11<<8)|5)  /* RETCC:   5 or 11 if ret taken */
#define C11 ((17<<8)|11) /* CALLCC: 11 or 17 if call taken */

typedef void (*evalfn_t)(int, int, int&);

struct optab_t {
  evalfn_t eval;
  const char *mnem;
  int a0, a1;
  int cyc;
};

constexpr optab_t mkop(evalfn_t eval, const char *mnem, int a0, int a1, int cyc) {
  optab_t o = {};
  o.eval = eval;
  o.mnem = mnem;
  o.a0 = a0;
  o.a1 = a1;
  o.cyc = cyc;
  return o;
};

//#define _(m, c, d, s) { .eval=_##m, .mnem=#m, .a0 = d, .a1 = s, .cyc=c }
#define _(m, c, d, s) mkop(_##m, #m, d, s, c)

optab_t optab[256] = {
  _(nop, 4,imp,imp), _(lxi,10,RBC,I16), _(stax, 7, RA, RBC),_(inx, 5, RBC, RBC),_(inr, 5, RB, RB),  _(dcr, 5, RB, RB),    _(mvi,  7, RB, I8), _(rlc, 4, RA, RA),
  _(nop, 4,imp,imp), _(dad,10,RHL,RBC), _(ldax, 7, RA, RBC),_(dcx, 5, RBC, RBC),_(inr, 5, RC, RC),  _(dcr, 5, RC, RC),    _(mvi,  7, RC, I8), _(rrc, 4, RA, RA),
  /* 0x10 */
  _(nop, 4,imp,imp), _(lxi,10,RDE,I16), _(stax, 7, RA, RDE),_(inx, 5, RDE, RDE),_(inr, 5, RD, RD),  _(dcr, 5, RD, RD),    _(mvi,  7, RD, I8), _(ral, 4, RA, RA),
  _(nop, 4,imp,imp), _(dad,10,RHL,RDE), _(ldax, 7, RA, RDE),_(dcx, 5, RDE, RDE),_(inr, 5, RE, RE),  _(dcr, 5, RE, RE),    _(mvi,  7, RE, I8), _(rar, 4, RA, RA),
  /* 0x20 */
  _(nop, 4,imp,imp), _(lxi,10,RHL,I16), _(shld,16, RHL,I16),_(inx, 5, RHL,RHL), _(inr, 5, RH, RH),  _(dcr, 5, RH, RH),    _(mvi,  7, RH, I8), _(daa, 4, RA, RA),
  _(nop, 4,imp,imp), _(dad,10,RHL,RHL), _(lhld,16, RHL,I16),_(dcx, 5, RHL,RHL), _(inr, 5, RL, RL),  _(dcr, 5, RL, RL),    _(mvi,  7, RL, I8), _(cma, 4, RA, RA),
  /* 0x30 */
  _(nop, 4,imp,imp), _(lxi,10,RSP,I16), _(sta, 13, RA,I16), _(inx, 5, RSP,RSP), _(inr,10,MHL,MHL),  _(dcr,10,MHL,MHL),    _(mvi, 10,MHL, I8), _(stc, 4, imp, imp),
  _(nop, 4,imp,imp), _(dad,10,RHL,RSP), _(lda, 13, RA,I16), _(dcx, 5, RSP,RSP), _(inr, 5, RA, RA),  _(dcr, 5, RA, RA),    _(mvi,  7, RA, I8), _(cmc, 4, imp, imp),
  /* 0x40 */
  _(mov, 5, RB, RB), _(mov, 5, RB, RC), _(mov, 5, RB, RD),  _(mov, 5, RB, RE),  _(mov, 5, RB, RH),  _(mov, 5, RB, RL),    _(mov, 7, RB, MHL), _(mov, 5, RB, RA),
  _(mov, 5, RC, RB), _(mov, 5, RC, RC), _(mov, 5, RC, RD),  _(mov, 5, RC, RE),  _(mov, 5, RC, RH),  _(mov, 5, RC, RL),    _(mov, 7, RC, MHL), _(mov, 5, RC, RA),
  /* 0x50 */
  _(mov, 5, RD, RB), _(mov, 5, RD, RC), _(mov, 5, RD, RD),  _(mov, 5, RD, RE),  _(mov, 5, RD, RH),  _(mov, 5, RD, RL),    _(mov, 7, RD, MHL), _(mov, 5, RD, RA),
  _(mov, 5, RE, RB), _(mov, 5, RE, RC), _(mov, 5, RE, RD),  _(mov, 5, RE, RE),  _(mov, 5, RE, RH),  _(mov, 5, RE, RL),    _(mov, 7, RE, MHL), _(mov, 5, RE, RA),
  /* 0x60 */
  _(mov, 5, RH, RB), _(mov, 5, RH, RC), _(mov, 5, RH, RD),  _(mov, 5, RH, RE),  _(mov, 5, RH, RH),  _(mov, 5, RH, RL),    _(mov, 7, RH, MHL), _(mov, 5, RH, RA),
  _(mov, 5, RL, RB), _(mov, 5, RL, RC), _(mov, 5, RL, RD),  _(mov, 5, RL, RE),  _(mov, 5, RL, RH),  _(mov, 5, RL, RL),    _(mov, 7, RL, MHL), _(mov, 5, RL, RA),
  /* 0x70 */
  _(mov, 7, MHL,RB), _(mov, 7, MHL,RC), _(mov, 7, MHL,RD),  _(mov, 7, MHL,RE),  _(mov, 7, MHL,RH),  _(mov, 7, MHL,RL),    _(hlt, 7,imp,imp),  _(mov, 7, MHL,RA),
  _(mov, 5, RA, RB), _(mov, 5, RA, RC), _(mov, 5, RA, RD),  _(mov, 5, RA, RE),  _(mov, 5, RA, RH),  _(mov, 5, RA, RL),    _(mov, 7, RA, MHL), _(mov, 5, RA, RA),
  /* 0x80 */
  _(add, 4, RA, RB), _(add, 4, RA, RC), _(add, 4, RA, RD),  _(add, 4, RA, RE),  _(add, 4, RA, RH),  _(add, 4, RA, RL),    _(add, 7, RA, MHL), _(add, 4, RA, RA),
  _(adc, 4, RA, RB), _(adc, 4, RA, RC), _(adc, 4, RA, RD),  _(adc, 4, RA, RE),  _(adc, 4, RA, RH),  _(adc, 4, RA, RL),    _(adc, 7, RA, MHL), _(adc, 4, RA, RA),
  /* 0x90 */
  _(sub, 4, RA, RB), _(sub, 4, RA, RC), _(sub, 4, RA, RD),  _(sub, 4, RA, RE),  _(sub, 4, RA, RH),  _(sub, 4, RA, RL),    _(sub, 7, RA, MHL), _(sub, 4, RA, RA),
  _(sbb, 4, RA, RB), _(sbb, 4, RA, RC), _(sbb, 4, RA, RD),  _(sbb, 4, RA, RE),  _(sbb, 4, RA, RH),  _(sbb, 4, RA, RL),    _(sbb, 7, RA, MHL), _(sbb, 4, RA, RA),
  /* 0xa0 */
  _(ana, 4, RA, RB), _(ana, 4, RA, RC), _(ana, 4, RA, RD),  _(ana, 4, RA, RE),  _(ana, 4, RA, RH),  _(ana, 4, RA, RL),    _(ana, 7, RA, MHL), _(ana, 4, RA, RA),
  _(xra, 4, RA, RB), _(xra, 4, RA, RC), _(xra, 4, RA, RD),  _(xra, 4, RA, RE),  _(xra, 4, RA, RH),  _(xra, 4, RA, RL),    _(xra, 7, RA, MHL), _(xra, 4, RA, RA),
  /* 0xb0 */
  _(ora, 4, RA, RB), _(ora, 4, RA, RC), _(ora, 4, RA, RD),  _(ora, 4, RA, RE),  _(ora, 4, RA, RH),  _(ora, 4, RA, RL),    _(ora, 7, RA, MHL), _(ora, 4, RA, RA),
  _(cmp, 4, RA, RB), _(cmp, 4, RA, RC), _(cmp, 4, RA, RD),  _(cmp, 4, RA, RE),  _(cmp, 4, RA, RH),  _(cmp, 4, RA, RL),    _(cmp, 7,imp, MHL), _(cmp, 4, RA, RA),
  /* 0xc0 */
  _(rnz,C5,RCC,imp), _(pop,10,RBC,imp), _(jnz,10, JCC,I16), _(jmp,10,JMP, I16), _(cnz,C11,CALL,I16),_(push,11,imp,RBC),   _(adi, 7, RA, I8),  _(rst,11, RST,imp),
  _(rz, C5,RCC,imp), _(ret,10,RET,imp), _(jz, 10, JCC,I16), _(jmp,10,JMP, I16), _(cz, C11,CALL,I16),_(call,17,CALL,I16),  _(aci, 7, RA, I8),  _(rst,11, RST,imp),
  /* 0xd0 */
  _(rnc,C5,RCC,imp), _(pop,10,RDE,imp), _(jnc,10, JCC,I16), _(out,10, RA, I8),  _(cnc,C11,CALL,I16),_(push,11,imp,RDE),   _(sui, 7, RA, I8),  _(rst,11, RST,imp),
  _(rc, C5,RCC,imp), _(ret,10,RET,imp), _(jc, 10, JCC,I16), _(in, 10, RA, I8),  _(cc, C11,CALL,I16),_(call,17,CALL,I16),  _(sbi, 7, RA, I8),  _(rst,11, RST,imp),
  /* 0xe0 */
  _(rpo,C5,RCC,imp), _(pop,10,RHL,imp), _(jpo,10, JCC,I16), _(xhtl,18,imp,imp), _(cpo,C11,CALL,I16), _(push,11,imp,RHL),  _(ani, 7, RA, I8),  _(rst,11, RST,imp),
  _(rpe,C5,RCC,imp), _(pchl,5,imp,imp), _(jpe,10, JCC,I16), _(xchg, 4,imp,imp), _(cpe,C11,CALL,I16), _(call,17,CALL,I16), _(xri, 7, RA, I8),  _(rst,11, RST,imp),
  /* 0xf0 */
  _(rp, C5,RCC,imp), _(pop,10,RAF,imp), _(jp, 10, JCC,I16), _(di,  4, imp,imp), _(cp, C11,CALL,I16), _(push,11,imp,RAF),  _(ori, 7, RA, I8),  _(rst,11, RST,imp),
  _(rm, C5,RCC,imp), _(sphl,5,imp,imp), _(jm, 10, JCC,I16), _(ei,  4, imp,imp), _(cm, C11,CALL,I16), _(call,17,CALL,I16), _(cpi, 7,imp, I8),  _(rst,11, RST,imp),
};

const char *distab[] = {
  "",    "b, d16", "b",  "b", "b", "b", "b, d8", "",   "", "b", "b",  "b", "c", "c", "c, d8", "",
  "",    "d, d16", "d",  "d", "d", "d", "d, d8", "",   "", "d", "e",  "e", "e", "e", "e, d8", "",
  "",    "h, d16", "d16","h", "h", "h", "h, d8", "",   "", "h", "d16","h", "l", "l", "l, d8", "",
  "",    "sp,d16", "d16","sp","m", "m", "m, d8", "",   "", "sp","d16","sp","a", "a", "a, d8", "",
  
  /* 0x40: mov */
  "b,b", "b,c", "b,d","b,e","b,h","b,l","b,m","b,a",  "c,b", "c,c", "c,d","c,e","c,h","c,l","c,m","c,a",
  "d,b", "d,c", "d,d","d,e","d,h","d,l","d,m","d,a",  "e,b", "e,c", "e,d","e,e","e,h","e,l","e,m","e,a",
  "h,b", "h,c", "h,d","h,e","h,h","h,l","h,m","h,a",  "l,b", "l,c", "l,d","l,e","l,h","l,l","l,m","l,a",
  "m,b", "m,c", "m,d","m,e","m,h","m,l","",   "m,a",  "a,b", "a,c", "a,d","a,e","a,h","a,l","a,m","a,a",
  
  /* 0x80: math */
  "b", "c", "d", "e", "h", "l", "m", "a", "b", "c", "d", "e", "h", "l", "m", "a", 
  "b", "c", "d", "e", "h", "l", "m", "a", "b", "c", "d", "e", "h", "l", "m", "a", 
  "b", "c", "d", "e", "h", "l", "m", "a", "b", "c", "d", "e", "h", "l", "m", "a", 
  "b", "c", "d", "e", "h", "l", "m", "a", "b", "c", "d", "e", "h", "l", "m", "a", 
  
  /* 0xc0: control flow/misc */
  "",  "b", "d16", "d16", "d16", "b", "d8", "0",    "",  "", "d16", "d16", "d16", "d16", "d8", "1",
  "",  "d", "d16", "d8",  "d16", "d", "d8", "2",    "",  "", "d16", "d8",  "d16", "d16", "d8", "3",
  "",  "h", "d16", "",    "d16", "h", "d8", "4",    "",  "", "d16", "",    "d16", "d16", "d8", "5",
  "", "psw","d16", "",    "d16","psw","d8", "8",    "",  "", "d16", "",    "d16", "d16", "d8", "7",
};

const char *disasm(uint8_t *mem) {
  static char dstr[32], *p;

  /* Create disasm from mnemonic and argstr */
  snprintf(dstr, sizeof(dstr), "%-6s%s",
	   optab[*mem].mnem,
	   distab[*mem]);

  /* Replace arg string with hex */
  if ((p = strstr(dstr, "d8")) != NULL)
    snprintf(p, 4, "$%02x", mem[1]);
  else if ((p = strstr(dstr, "d16")) != NULL)
    snprintf(p, 6, "$%02x%02x", mem[2], mem[1]);
  return dstr;
}

const char *fbits(const char *s, int flag)
{
  static char fstr[32];

  memset(fstr, 0, sizeof(fstr));
  for (int i = 0; s[i]; i++) {
    fstr[i] = s[i];
    if ((flag & (0x80 >> i)) == 0) {
      fstr[i] = '-';
    }
  }
  return fstr;
}

int used[256];

int cpu_step() {
  int op;

  op = cpu_read8(PC++, dstk::CODE);
  used[op]|=1;
  int cyc = optab[op].cyc;

  if (trace) {
#if 0
    /* Display disasm */
    fprintf(stdout,"%6d %.4x A=%.2x F=%.2x B=%.2x C=%.2x D=%.2x E=%.2x H=%.2x L=%.2x SP=%.4x [%c%c%c%c%c%c%c%c] %.2x %s\n",
	    totcyc, PC-1, A, F, B, C, D, E, H, L, SP,
	    fS ? 's' : ' ', fZ ? 'z' : ' ',
	    '-',	    fA ? 'a' : ' ',
	    '-',	    fP ? 'p' : ' ',
	    '-',	    fC ? 'c' : ' ',
	    op, disasm(&mem[PC-1]));
#else
    const char *s = disasm(&mem[PC-1]);
    fprintf(stdout,"PC: %.4X, [%s] AF: %.2X%.2X, BC: %.2X%.2X, DE: %.2X%.2X, HL: %.2X%.2X, SP: %.4X, CYC: %-5d\t(%.2X %.2X %.2X %.2X) %s\n",
	    PC-1, fbits("sz-a-p-c", F), A, F, B, C, D, E, H, L, SP,
	    totcyc, mem[PC-1], mem[PC], mem[PC+1], mem[PC+2], s);
#endif
  }
  int src;

  src = getval(op, optab[op].a1);
  optab[op].eval(optab[op].a0, src, cyc);
  totcyc += (cyc & ~BRANCH_TAKEN);
  
  return cyc & ~BRANCH_TAKEN;
}

void cpu_reset() {
  PC = 0;
}

void cpu_irq(int addr)
{
  if (fI) {
    cpu_push16(PC);
    PC = addr;
  }
  fI = 0;
}

void cpu_run(int cycles) {
  int i = 0;

  while (i < cycles) {
    int c = cpu_step();
    i += c;
  }
}

#ifdef TEST
/* Dump Control Flow Graph */
const char *rtype(int n) {
  switch(n) {
  case RST: return ";;RST";
  case RET: return ";;RET";
  case RCC: return ";;RCC";
  case JMP: return ";;JMP";
  case JCC: return ";;JCC";
  case CALL: return ";;CALL";
  }
  return "";
}

void dumpcfg(uint8_t *buf, int sz, int off) {
  int op, nxt[2];

  stk.push(off,  1, dstk::PENDING);
  while ((off = stk.pop()) != -1) {
    op = buf[off];

    fprintf(stdout,"%.4X %.2x %-32s %s\n", off, op, disasm(&buf[off]),
	    rtype(optab[op].a0));
    /* Get operand size */
    sz = 1;
    if (optab[op].a1 == I8)
      sz = 2;
    else if (optab[op].a1 == I16)
      sz = 3;

    /* Now determine continuation */
    nxt[0] = off+sz;
    nxt[1] = -1;
    switch (optab[op].a0) {
    case RET:
      /* return: [] */
      nxt[1] = -1;
      nxt[0] = -1;
      break;
    case RCC:
      /* conditional return: [nxt] */
      break;
    case JMP:
      /* jump: [dst] */
      nxt[0] = get16(buf+off+1);
      break;
    case JCC:
      /* conditional jump: [nxt,dst] */
      nxt[1] = get16(buf+off+1);
      break;
    case CALL:
      /* (conditional) call: [nxt] */
      nxt[1] = get16(buf+off+1);
      break;
    case RST:
      nxt[1] = op & 0x38;
      break;
    }
    /* Push current code and pending */
    stk.push(off, sz, dstk::CODE);
    for (int i=0; i<2; i++)
      stk.push(nxt[i],  1, dstk::PENDING);
  }
  stk.showstk(128);
}
#endif

int loadrom(const char *file, int off) {
  FILE *fp;
  int n;

  if ((fp = fopen(file, "rb")) == NULL)
    return 0;
  while ((n = fread(&mem[off], 1, 4096, fp)) > 0)
    off += n;
  fclose(fp);
  return n;
}

/*=============================================================*
 * SPACE INVADERS code goes here 
 *=============================================================*/
#define CYCLES_PER_MS 2000  // 8080 runs at 2 Mhz
#define FRAME_TIME (1000.0 / 60.0)  // Milliseconds per tic
#define CYCLES_PER_FRAME (CYCLES_PER_MS * FRAME_TIME)

void setmask(int sk, uint8_t &k, int mask) {
  k = s->KeyState[sk] ? (k | mask) : (k & ~mask);
}

SDL_Texture *txt;
SDL_Surface *surface;
SDL_Color palette[2] = {
  { 0,244,0 }, { 255,255,255 },
};
/* Space invaders is 244 x 256 but rotated 90 degrees */
void draw_screen() {
  int cx = 0;
  int cy = 0;
  int clr;

#if 0
  // Create texture, rotate 270 degrees
  if (!txt) {
    txt = SDL_CreateTexture(s->renderer, SDL_PIXELFORMAT_INDEX1LSB, SDL_TEXTUREACCESS_STREAMING, 256, 244);
  }
  SDL_UpdateTexture(txt, NULL, &mem[0x2400], 0x4000-0x2400);
  SDL_RenderClear(s->renderer);
  SDL_RenderCopyEx(s->renderer, txt, NULL, NULL, 270, NULL, SDL_FLIP_NONE);
  SDL_DestroyTexture(txt);
#elif 0
  if (!surface) {
    surface = SDL_CreateRGBSurfaceWithFormat(SDL_SWSURFACE, 244, 256, 1,
					     SDL_PIXELFORMAT_INDEX1LSB);
    SDL_SetPaletteColors(surface->format->palette, palette, 0, 2);
  }
  memcpy(surface->pixels, &mem[0x2400], 0x4000-0x2400);
  txt = SDL_CreateTextureFromSurface(s->renderer, surface);
  SDL_RenderClear(s->renderer);
  SDL_RenderCopyEx(s->renderer, txt, NULL, NULL, 270, NULL, SDL_FLIP_NONE);
  SDL_DestroyTexture(txt);
  //SDL_FreeSurface(surface);
  
  s->draw(1);
#else
  /* Loop through video RAM */
  for (int i = 0x2400; i <= 0x3FFF; i++) {
    for (int j = 0; j < 8; j++) {
      /* gradient every 16 scanlines */
      if (cy >= 24 && cy <= 48)
	clr = 2;
      else if (cy >= 220 && cy <= 240)
	clr = 3;
      else clr = 1;
      s->setpixel(cx, 255-cy, mem[i] & (0x01 << j) ? clr : 0);
      if (++cy == 256) {
	cx++;
	cy = 0;
      }
    }
  }
  s->draw();
#endif
  
  /* Check input */
  setmask('c', ports[1], 0x01); // coin
  setmask('s', ports[1], 0x04); // P1 select
  setmask('w', ports[1], 0x10); // P1 fire
  setmask('a', ports[1], 0x20); // P1 left
  setmask('d', ports[1], 0x40); // P1 right

  if (s->KeyState['t'])
    trace ^= 1;
  if (s->KeyState['p'])
    stk.showstk(128);
}

void cpu_shutdown() {
}

void run()
{
  uint32_t last_frame;
  
  PC = 0x0;
  loadrom("invaders.h", 0x0000);
  loadrom("invaders.g", 0x0800);
  loadrom("invaders.f", 0x1000);
  loadrom("invaders.e", 0x1800);
  //dumpcfg(mem, 0x2400, 0);
  //dumpcfg(mem, 0x2400, 0x08);
  //dumpcfg(mem, 0x2400, 0x10);
  s = new Screen(256, 244, 0, 0);
  
  s->setpalette(0, 0x10, 0x10, 0x10);
  s->setpalette(1, 0xe0, 0xe0, 0xe0);

  for (int i = 2; i < 18; i++) {
    s->setpalette(i,
		  0xff + i * (0x00 - 0xff)/16.0,
		  0x00 + i * (0x00 - 0x00)/16.0,
		  0x00 + i * (0xff - 0x00)/16.0);
  }
  s->setpalette(2, 0xc0, 0x10, 0x10);
  s->setpalette(3, 0x10, 0xc0, 0x10);
  s->init();

  last_frame = SDL_GetTicks();
  for(;;) {
    if ((SDL_GetTicks() - last_frame) >= FRAME_TIME) {
      last_frame = SDL_GetTicks();

      /* Run half a frane of cycles, generate half frame interrupt */
      cpu_run(CYCLES_PER_FRAME/2);
      cpu_irq(0x08);
      /* Run half a frane of cycles, generate end frame interrupt */
      cpu_run(CYCLES_PER_FRAME/2);
      cpu_irq(0x10);
      
      draw_screen();
      
      if (SDL_GetTicks() - last_frame > FRAME_TIME) {
	fprintf(stdout,"TOO SLOW\n");
      }
    }
  }
}

int main() {
  F = 0x02;
#ifdef TEST
  int i, c, tc;

  trace = 1;
  //int n = loadrom("TST8080.COM", 0x100);
  //int n = loadrom("CPUTEST.COM", 0x100);
  //int n = loadrom("8080EXM.COM", 0x100);
  int n = loadrom("cpudiag.bin", 0x100);
  mem[0] = 0xd3;
  mem[1] = 0x00;
  mem[5] = 0xd3;
  mem[6] = 0x01;
  mem[7] = 0xc9;
  //dumpcfg(mem, n+0x100, 0x100);
  PC=0x100;
  tc = 0;
  while (!test_done) {
    c = cpu_step();
    tc += c;
  }
  fprintf(stdout, "cycs: %d\n", tc);
  for (int i = 0; i < 256; i++) {
    fprintf(stdout,"%-4s ", used[i] ? "___" : optab[i].mnem);
    if ((i & 0xF) == 0xF) {
      fprintf(stdout,"\n");
    }
  }
  stk.showstk(128);
#else
  run();
#endif
}
