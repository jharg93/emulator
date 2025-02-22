#ifndef __cpu_m68k_h__
#define __cpu_m68k_h__
/* Implement 68000 CPU core */
#include <map>
#include <string>
#include <bitset>
#include <setjmp.h>
#include <time.h>


#define Assert(x) do { if (!(x)) { printf("assertion failed : %s %s\n", __FUNCTION__, #x); assert(x); }}  while(0);

static bool m68k_trap(bool cond, int n);
static bool m68k_trapa(bool cond, int n, uint32_t addr, int ir = 0, int code = 0, const char *lbl="");
void m68k_emul1010(uint16_t);
void m68k_emul1111(uint16_t);

void load_syms();
static uint32_t sp_inc(int delta);

void getextra(dstk& s, uint32_t base, uint32_t size);
void flogger(int lvl, const char *fmt, ...);
static const char *flagstr(const char *bits, int flags, ...);

static uint32_t open_bus;
static int _VBR;
static uint32_t leaval;
static jmp_buf trapjmp;
int SPC, trace;
int ipl = 0;
int sushi = 0;
int stopped;

uint32_t m68k_busmask = -1;

std::map<uint32_t, uint32_t> visited;
std::map<uint32_t, std::string> fnmap;
std::map<uint32_t, std::string> rmap;

#define ta(a,b) (((T_ ## a) << 8) + (T_ ## b))

static uint32_t ctick;

static void TR(int cyc, int rd=0, int wr=0) {
  ctick += (cyc << 8) + (rd << 4) + wr;
};
static int EATR(int cyc, int rd, int wr) {
  return (cyc << 8) + (rd << 4) + wr + 0x80000000;
};

enum {
  None  = 0x0,
  Byte  = 0x1,
  Word  = 0x2,
  Long  = 0x4,
  Any   = 0x7,
  Any2  = 0xF,

  T_0    = 0,

  // effective address values
  T_EA   = 0x7f,   // ____.___.___.yyy.yyy z
  T_Dy   = 0x40,   // ____.___.___.000.yyy z
  T_Ay   = 0x41,   // ____.___.___.001.yyy z
  T_iAy  = 0x43,   // ____.___.___.011.yyy z
  T_dAy  = 0x44,   // ____.___.___.100.yyy z
  T_Ay16 = 0x45,   // ____.___.___.110.yyy z
  T_IMM  = 0x4b,   // ____.___.___.111.100

  T_EA2  = 0xff,   // ____.xxx.xxx.___.___ z
  T_Dx   = 0x80,   // ____.xxx.000.___.___ z
  T_Ax   = 0x81,   // ____.xxx.001.___.___ z
  T_iAx  = 0x83,   // ____.xxx.011.___.___ z
  T_dAx  = 0x84,   // ____.xxx.100.___.___ z

  // misc arg types
  T_Q3     = 0x50, // ____.xxx.___.___.___
  T_Q8     = 0x51, // ____.___._xx.xxx.xxx
  T_IMM4   = 0x52, // ____.___.___.__x.xxx
  T_IW     = 0x53,
  T_SR     = 0x54,
  T_BRANCH = 0x55,
  T_PC16   = 0x56,

  EA_reg_Dn    = 0x0,
  EA_reg_An    = 0x1,
  EA_mem_An    = 0x2,
  EA_inc_An    = 0x3,
  EA_dec_An    = 0x4,
  EA_d16_An    = 0x5,
  EA_Xn8_An    = 0x6,
  EA_off_16    = 0x7+0,
  EA_off_32    = 0x7+1,
  EA_d16_PC    = 0x7+2,
  EA_Xn8_PC    = 0x7+3,
  EA_imm_sz    = 0x7+4,

  /* 68020+ */
  EA_Xn0_An    = 0x10, // (bd,An,Xn)
  EA_Xn1_An    = 0x11, // ([bd,An,Xn],od)
  EA_Xn2_An    = 0x12, // ([bd,An],Xn,od)
  EA_Xn0_PC    = 0x13, // (bd,Pc,Xn)
  EA_Xn1_PC    = 0x14, // ([bd,PC,Xn],od)
  EA_Xn2_PC    = 0x15, // ([bd,PC],Xn,od)

  Imm      = ta(IMM,0),      // ____.___.___.___.___ iiii.iiii.iiii.iiii
  Imm_SR   = ta(IMM,SR),     // ____.___.___.___.___ iiii.iiii.iiii.iiii 
  Imm_Ay   = ta(IMM,Ay),     // ____.___.___.___.yyy iiii.iiii.iiii.iiii 
  PC16_Dy  = ta(PC16,Dy),    // ____.___.___.___.yyy iiii.iiii.iiii.iiii 
  Imm_EA   = ta(IMM,EA),     // ____.___.___.mmm.yyy iiii.iiii.iiii.iiii
  Iw_EA    = ta(IW,EA),      // ____.___.___.mmm.yyy iiii.iiii.iiii.iiii
  Imm4     = ta(IMM4,0),     // ____.___.___.__i.iii
  q8_Dx    = ta(Q8,Dx),      // ____.xxx._ii.iii.iii
  q3_Dy    = ta(Q3,Dy),      // ____.iii.___.___.yyy
  Ax_Ay    = ta(Ax,Ay),
  Dx_Ay    = ta(Dx,Ay),
  
  rAy     = ta(Ay,0),       // ____.___.___.___.yyy :notused Ay (ref)
  rDy     = ta(Dy,0),       // ____.___.___.___.yyy :notused Dy (ref)
  Ay16_Dx = ta(Ay16,Dx),    // ____.xxx.___.___.yyy
  iAyx    = ta(iAy,iAx),    // ____.xxx.___.___.yyy :cmpm          
  dAyx    = ta(dAy,dAx),    // ____.xxx.___.___.yyy :sbcd,subx,abcd,addx         
  Dy_Dx   = ta(Dy,Dx),      // ____.xxx.___.___.yyy :notused
  Dx_Dy   = ta(Dx,Dy),      // ____.xxx.___.___.yyy :notused
  SR_EA   = ta(SR,EA),      // ____.___.___.mmm.yyy :notused SR
  EA_SR   = ta(EA,SR),      // ____.___.___.mmm.yyy :notused SR
  EA      = ta(EA,EA),      // ____.___.___.mmm.yyy
  Dx_EA   = ta(Dx,EA),      // ____.xxx.___.mmm.yyy :notused Dx
  EA_Ax   = ta(EA,Ax),      // z ____.xxx.___.mmm.yyy
  q3_EA   = ta(Q3,EA),      // ____.iii.___.mmm.yyy
  EA_Dx   = ta(EA,Dx),      // z ____.xxx.___.mmm.yyy :notused Dx
  EA_EA2  = ta(EA,EA2),     // ____.xxx.MMM.mmm.yyy
  Branch  = ta(BRANCH,0),

  aXOR = 0x100,
  aAND,
  aOR,
  aCMP,
  aBTST,
  aBSET,
  aBCHG,
  aBCLR,
};

static char eamap[] = {
  EA_reg_Dn, EA_reg_Dn, EA_reg_Dn, EA_reg_Dn, EA_reg_Dn, EA_reg_Dn, EA_reg_Dn, EA_reg_Dn,  // Dn
  EA_reg_An, EA_reg_An, EA_reg_An, EA_reg_An, EA_reg_An, EA_reg_An, EA_reg_An, EA_reg_An,  // An
  EA_mem_An, EA_mem_An, EA_mem_An, EA_mem_An, EA_mem_An, EA_mem_An, EA_mem_An, EA_mem_An,  // (An)
  EA_inc_An, EA_inc_An, EA_inc_An, EA_inc_An, EA_inc_An, EA_inc_An, EA_inc_An, EA_inc_An,  // (An)+
  EA_dec_An, EA_dec_An, EA_dec_An, EA_dec_An, EA_dec_An, EA_dec_An, EA_dec_An, EA_dec_An,  // -(An)
  EA_d16_An, EA_d16_An, EA_d16_An, EA_d16_An, EA_d16_An, EA_d16_An, EA_d16_An, EA_d16_An,  // (An, d16)
  EA_Xn8_An, EA_Xn8_An, EA_Xn8_An, EA_Xn8_An, EA_Xn8_An, EA_Xn8_An, EA_Xn8_An, EA_Xn8_An,  // (An, Xn, d8)
  EA_off_16, EA_off_32, EA_d16_PC, EA_Xn8_PC, EA_imm_sz, None,      None,      None,
};

int countTime(const char *s, int sz, int ew=-1, int el=-1, const char *lbl="") {
  char ch;
  int cyc=0,rd=0,wr=0;

  auto tick_rd = [&](int n) { rd+=n; cyc+=4*n; };
  auto tick_wr = [&](int n) { wr+=n; cyc+=4*n; };
  for (int i = 0; (ch=s[i]) != 0; i++) {
    switch(ch) {
    case 'n': // 2 cycs
      cyc+=2;
      break;
    case 'N': // 4 cycs
      cyc+=4;
      break;
    case 'z': // word=0clk, long=4clk
      if (sz == Long) {
	cyc += 4;
      }
      break;
    case 'p': // word = 1rd,4clk
    case 'P': // long = 2rd,8clk
      tick_rd(sz == Long && ch == 'P' ? 2 : 1);
      break;
    case 'r': // word = 1rd/4clk
    case 'R': // long = 2rd/8clk
      tick_rd(sz == Long && ch == 'R' ? 2 : 1);
      break;
    case 'w': // word = 1wr/4clk
    case 'W': // long = 2wr/8clk
      tick_wr(sz == Long && ch == 'W' ? 2 : 1);
      break;
    case 'u': // stack
      tick_rd(1);
      break;
    }
  }
  int ct = (cyc << 8) + (rd << 4) + wr;
  if (ew != -1 && sz == Word && ew != ct) {
    printf("countword: exp:%.4x got:%.4x %s [%s]\n", ew, ct, s, lbl);
  }
  if (el != -1 && sz == Long && el != ct) {
    printf("countlong: exp:%.4x got:%.4x %s [%s]\n", el, ct, s, lbl);
  }
  return ct;
}

void ea_decode(uint16_t op, int size, int arg) {
  // 80=dy       10.000.000 0 ext,swap,dbcc,sbcd,subx,abcd,exg,addx,shifts
  // 88=ay       10.001.000 0 link,unlk,moveusp
  // 90=(ay)     10.010.000 0
  // 98=(ay)+    10.011.000 0 cmpm
  // a0=-(ay)    10.100.000 0 sbcd,subx,abcd,addx
  // a8=(ay,d16) 10.101.000 1 movep
  // b0=(ay,xn)  10.110.000 1 
  // b8=w        10.111.000 1
  // b9=l        10.111.001 2
  // ba=(pc,d16) 10.111.010 1 dbcc
  // bb=(pc,xn)  10.111.011 1
  // bc=imm      10.111.100 ? ori,andi,subi,addi,eori,cmpi,btst,bchg,bclr,bset,btst,link
  // bf=EA       10.111.111 ? xxx

  // c0=dx       11.000.000 0 many
  // c8=ax       11.001.000 0 movea,lea,suba,cmpa,exp,adda
  // d0=(ax)     11.010.000 0
  // d8=(ax)+    11.011.000 0 cmpm
  // e0=-(ax)    11.100.000 0 sbcd,subx,abcd,addx
  // e8=(ax,d16) 11.101.000 1
  // f0=(ax,xn)  11.110.000 1
  // f8=w        11.111.000 1
  // f9=w        11.111.001 2
  // fa=(pc,d16) 11.111.010 1
  // fb=(pc,xn)  11.111.011 1
  // fc=imm      11.111.100 ?
  // ff=EA2      11.111.111 ? xxx
};

enum {
  VECTOR_0 = 0,
  VECTOR_1,
  VECTOR_BUS_ERROR,
  VECTOR_ADDRESS_ERROR,
  VECTOR_ILLEGAL_INSTRUCTION,
  VECTOR_DIVIDE_BY_ZERO,
  VECTOR_CHK,
  VECTOR_TRAPV,
  VECTOR_PRIVILEGE,
  VECTOR_TRACE,
  VECTOR_1010,
  VECTOR_1111,
  
  NOSET  = 0x0,
  _XNzVC = 0x2,
  _XNZVC = 0x3,
};

static uint32_t   regs[20];
static uint32_t   *D = &regs[0];
static uint32_t   *A = &regs[8];
static uint32_t&  PC = regs[16];
static uint32_t&  SP = A[7];
static uint32_t& usp = regs[17];
static uint32_t& ssp = regs[18];

static uint32_t   SR;
static uint8_t&   CCR = *(uint8_t *)&SR;

#define USP       ((SR & 0x2000) ? usp : SP)

const char *regnames[] = {
  "D0","D1","D2","D3","D4","D5","D6","D7",
  "A0","A1","A2","A3","A4","A5","A6","A7",
};

const char *condstr[] = {
  "ra", "sr", "hi", "ls", "cc", "cs", "ne", "eq",
  "vc", "vs", "pl", "mi", "ge", "lt", "gt", "le"
};

static uint32_t sregs[20];

/* ADR modes
 * 0  000 rrr  Dn                   0(0/0)   0(0/0)
 * 1  001 rrr  An                   0(0/0)   0(0/0)
 * 2  010 rrr  (An)                 4(1/0)   8(2/0)
 * 3  011 rrr  (An)+                4(1/0)   8(2/0)
 * 4  100 rrr  -(An)                6(1/0)  10(2/0)
 * 5  101 rrr  (d16,An)      imm    8(2/0)  12(3/0) 
 * 6  110 rrr  (d8,An,Xn)    imm   10(2/0)  14(3/0) 
 * 7  111 000  (xxx).W       imm    8(2/0)  12(3/0)
 * 8  111 001  (xxx).L       imm   12(3/0)  16(4/0)
 * 9  111 010  (d16,PC)      imm    8(2/0)  12(3/0)
 * a  111 011  (d8,PC,Xn)    imm   10(2/0)  14(3/0)
 * b  111.100  #imm          imm    4(1/0)   8(2/0)
 * c  111.101  ---
 * d  111.110  ---
 * e  111.111  ---
 *
 * mode 6, a
 * +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
 * |D/A|  reg      | sz| scale | 0 |           displacement        |
 * +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
 *
 * +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
 * |D/A|  reg      | sz| scale | 1 | bs|is | bdsz  | 0 | i/is      |
 * +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
 * |  base displacement 0, 1, 2 words
 * +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
 * |  outer displacement 0, 1, 2 wordsd
 * +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
 */

static uint16_t ir;
static uint16_t IR[3];
static uint32_t DST;
static uint32_t JMPDST;

#define Ax      A[i.xxx]
#define Ay      A[i.yyy]
#define Dx      D[i.xxx]
#define Dy      D[i.yyy]

#define SRCADDR i.src.value
#define SRC     i.src.get()
#define IMM     i.imm

static void setval(uint32_t& dst, const uint32_t src, const int size, const uint32_t mask = -1) {
  if (size == Byte) {
    put8(&dst, src & mask);
  }
  else if (size == Word) {
    put16(&dst, src & mask);
  }
  else {
    dst = src;
  }
}

/* Status/Condition register
 * +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
 * | trace | S | M | 0 |    ipl    | 0 | 0 | 0 | X | N | Z | V | C |
 * +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
 */

// needded for m68test.bin
//SR_VALIDBITS_HI=0xA700;
//SR_VALIDBITS_LO=0x00FF;
uint16_t SR_VALIDBITS_HI=0xA700;
uint16_t SR_VALIDBITS_LO=0x001F;

#define SR_VALIDBITS    (SR_VALIDBITS_HI + SR_VALIDBITS_LO)

template<uint16_t bit>struct flag_t {
  const uint32_t mask = (1L << bit);
  uint32_t& flag = SR;

  operator bool() const { return !!(flag & mask); };
  flag_t& operator=(const bool v) {
    flag = v ? (flag | mask) : (flag & ~mask);
    return *this;
  };
};

flag_t<0>  Cf;
flag_t<1>  Vf;
flag_t<2>  Zf;
flag_t<3>  Nf;
flag_t<4>  Xf;
flag_t<12> Mf;
flag_t<13> Sf;

void cpu_setflags(uint32_t flags)
{
}

uint32_t cpu_getflags()
{
  return SR;
}

/* condition code is part of opcode
 * +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
 * |   |   |   |   | c | c | c | c |   |   |   |   |   |   |   |   |
 * +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
 */
enum {
  ccT,  ccF,  ccHI, ccLS, ccCC, ccCS, ccNE, ccEQ,
  ccVC, ccVS, ccPL, ccMI, ccGE, ccLT, ccGT, ccLE
};

/* there are 16 possible values for NZVC
 * use this as the index to a table
 * there are 16 possible condition code checks
 * eg. cond(0) == true... so bit 0 is always set...
 * cond(1) == false, so bit 1 is always clear...
 */
constexpr uint16_t condmap[] = {
  // a = le b = gt c = lt d = ge
  // e = mi f = pl g = vs h = vc
  // i = eq j = ne k = cs l = cc
  // m = ls n = hi o = f  p = t
  //abcd efgh ijkl mnop
  0b0101'0101'0101'0101,
  0b0101'0101'0101'1001,
  0b1010'0110'0101'0101,
  0b1010'0110'0110'1001,
  0b1001'0101'1001'1001,
  0b1001'0101'1010'1001,
  0b1010'0110'1001'1001,
  0b1010'0110'1010'1001,
  0b1010'1001'0101'0101,
  0b1010'1001'0110'1001,
  0b0101'1010'0101'0101,
  0b0101'1010'0101'1001,
  0b1010'1001'1001'1001,
  0b1010'1001'1010'1001,
  0b1001'1010'1001'1001,
  0b1001'1010'1010'1001,
};

bool testcond(const uint16_t op) {
  switch ((op >> 8) & 0xF) {
  case ccT:  return true;  // bra
  case ccF:  return false; // bsr
  case ccHI: return !Cf && !Zf;
  case ccLS: return Cf || Zf;
  case ccCC: return !Cf;
  case ccCS: return Cf;
  case ccNE: return !Zf;
  case ccEQ: return Zf;
  case ccVC: return !Vf;
  case ccVS: return Vf;
  case ccPL: return !Nf;
  case ccMI: return Nf;
  case ccGE: return (Nf ^ Vf) == 0; //(Nf && Vf) || (!Nf && !Vf);
  case ccLT: return (Nf ^ Vf) != 0; //(Nf && !Vf) || (!Nf && Vf);
  case ccGT: return (Nf && Vf && !Zf) || (!Nf && !Vf && !Zf);
  case ccLE: return Zf || (Nf && !Vf) || (!Nf && Vf);
  }
  return false;
}
/* Return operand size: ____.___._ss.___.___ */
constexpr int opsize(const uint16_t op, const int sz) {
  const int szmap[] = { Byte, Word, Long, Any };
  
  if (sz == Any) {
    return szmap[(op >> 6) & 3];
  }
  if (sz == Any2) {
    // for CAS
    return szmap[(op >> 9) & 3];
  }
  return sz;
}

const char *iorname(uint32_t addr) {
  return rmap[addr].c_str();
}

const char *fnname(uint32_t addr) {
  return fnmap[addr].c_str();
}

static void m68k_setpc(uint32_t npc, bool call, const char *lbl)
{
  if (lbl){
    //printf("%.8x: setpc: %.8x [%s] %d <%s>\n", SPC, npc, lbl, call, fnname(npc));
  }
  if (npc & 1) {
    PC = npc - 2;
    printf("BAD PC: %.8x\n", npc);
    m68k_trapa(true, VECTOR_ADDRESS_ERROR, npc, IR[0], Sf ? 0x7e : 0x7a, "setpc");
    longjmp(trapjmp, 0xdeadbee0);
  }
  if (npc == SPC || npc == 0) {
    printf("jsr: %.8x no go... sr=%.4x %.8x [%s]\n", SPC, cpu_getflags(), npc, lbl);
    cpu_shutdown();
    exit(0);
  }
  if (call) {
    cpu_push32(PC);
  }
  PC = npc & m68k_busmask;
}

static void _m68k_setsr(uint32_t nv)
{
  uint16_t tsr = cpu_getflags();
  nv &= SR_VALIDBITS;
  
  //printf("%.8x ---- m68k_setsr: new:%.4x old:%.4x\n", SPC, nv, SR);
  if ((tsr & 0x2000) && !(nv & 0x2000)) {
    // going out of supervisor
    ssp = SP;
    SP = usp;
  }
  else if ((nv & 0x2000) && !(tsr & 0x2000)) {
    // going into supervisor
    usp = SP;
    SP = ssp;
  }
  ipl = (nv >> 8) & 0x7;
  SR = nv;
}

static void m68k_setsr(uint32_t nv, int size)
{
  if (size == Byte) {
    SR = (SR & SR_VALIDBITS_HI) + (nv & SR_VALIDBITS_LO);
  }
  else if (!m68k_trap(!Sf, VECTOR_PRIVILEGE)) {
    _m68k_setsr(nv);
  }
}

/* Dump CPU Vector */
static void dumpvec(uint32_t base = 0) {
    const char *vector[] = {
    "sp", "pc", "bus error", "address error",
    "illegal", "div0", "chk", "trapv",
    "priv", "trace", "1010", "1111",
    "---", "---", "format", "uninit",
    "unassigned",
    "unassigned",
    "unassigned",
    "unassigned",
    "unassigned",
    "unassigned",
    "unassigned",
    "unassigned",
    "spurious", "lvl1", "lvl2", "lvl3",
    "lvl4", "lvl5", "lvl6", "lvl7",
    "trap0", "trap1", "trap2", "trap3",
    "trap4", "trap5", "trap6", "trap7",
    "trap8", "trap9", "trap10", "trap11",
    "trap12", "trap13", "trap14", "trap15",
  };
  for (int i = 0; i < 48; i++) {
    printf(" %.2x %.4x %-16s: %.8x\n", i, i * 4, vector[i], cpu_read32(base + i *  4));
  }
}

bool cpu_irq(int n) {
  uint32_t npc = 0;
  uint16_t tsr = cpu_getflags();
  
  if (sushi) {
    printf("sushi irq: %d\n", n);
    //m68k_set_irq(n);
    return true;
  }
  ipl = (SR >> 8) & 7;
  if (ipl >= n) {
    return false;
  }
  stopped = false;
  npc = cpu_read32(_VBR + (n + 24) * 4);
  printf("\n%.8x ===== IRQ:%.8x : %d/%d -> %.8x\n", SPC, PC, ipl, n, npc);
  
  /* Set new IPL */
  ipl = n;
  _m68k_setsr(tsr | 0x2000 | (ipl << 8));
  cpu_push32(PC);
  cpu_push16(tsr);
  m68k_setpc(npc, false, "IRQ");
  return true;
}

bool m68k_trap(bool cond, int n) {
  uint16_t tsr = cpu_getflags();

  if (!cond) {
    return false;
  }
  /* set supervisor mode: PC is saved on Supervisor stack */
  flogger(0, "TRAP: %x %.8x %.8x\n", n, SPC, PC);
  _m68k_setsr((tsr & 0x071F) | 0x2000);

  // push original pc (or chk==new pc)
  cpu_push32(SPC);
  cpu_push16(tsr);
  PC = cpu_read32(_VBR + (n * 4));
  return true;
}

/* Trap Frame A address
 * +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
 * |           |           |   1  1 |rw|in|  func  |
 * +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
 * in = 0: instruction, 1 = not
 * rw = 0:
 *
 *       r.ifff
 * 75 0111.0101
 * 7e 0111.1110
 * 95 1001.0101 clr
 * b5 1011.0101 clr
 */
static bool m68k_trapa(bool cond, int n, uint32_t addr, int ir, int code, const char *lbl)
{
  uint16_t tsr = cpu_getflags();

  if (!cond)
    return false;
  
  /* set supervisor mode: PC is saved on Supervisor stack */
  printf("TRAPA: %x\n", n);
  _m68k_setsr((tsr & 0x071F) | 0x2000);
  cpu_push32(PC - 2);
  cpu_push16(tsr);
  cpu_push16(ir);
  cpu_push32(addr);
  cpu_push16((ir & 0xFFE0) + (code & 0x1F));
  PC = cpu_read32(_VBR + (n * 4));
  return true;
}

static void m68k_check_address(uint32_t addr, int size, int code, const char *lbl)
{
  if ((size == Word && (addr & 1)) ||
      (size == Long && (addr & 1))) {
    printf("--- address error: %x\n", addr);
    m68k_trapa(true, VECTOR_ADDRESS_ERROR, addr, IR[0], code, lbl);
    longjmp(trapjmp, 0xdeadbeef);
  }
}

const uint32_t nmask(int size) {
  switch (size) {
  case Byte: return 0x80;
  case Word: return 0x8000;
  case Long: return 0x80000000;
  }
  return 0;
}

/* Keep track of instruction */
struct val_t {
  char     eastr[50];
  uint32_t type;
  uint32_t value;
  uint32_t size;
  uint32_t easrc();
  
  val_t(int t = -1, int sz = -1) {
    type = t;
    size = sz;
    if (t != -1) {
      value = easrc();
    }
  };
  uint32_t get();
  void set(uint32_t nv, int nsz);

  void trace_fmt(const char *fmt, ...) {
    va_list ap;

    /* only decode if tracing is on */
    memset(eastr, 0, sizeof(eastr));
    if (trace) {
      va_start(ap, fmt);
      vsnprintf(eastr, sizeof(eastr), fmt, ap);
    }
  };
};

struct instr_t {
  uint16_t  op;
  int       size;
  int       xxx;
  int       yyy;
  val_t     src;
  val_t     dst;
  uint32_t  imm;
  uint32_t  nmask;
  uint32_t  zmask;

  uint32_t  rflag;
  uint32_t  dstval;
  uint32_t  dstsz;
  
  void set(uint16_t _op, int _size) {
    op = _op;
    xxx = (op >> 9) & 0x7;
    yyy = (op >> 0) & 0x7;
    size = _size;
    if (size == Byte) {
      nmask = 0x80;
      zmask = 0xFF;
    }
    else if (size == Word) {
      nmask = 0x8000;
      zmask = 0xFFFF;
    }
    else {
      nmask = 0x80000000;
      zmask = 0xFFFFFFFF;
    };
  };
};

static const char *szstr(int n) {
  const char *szm[] = {
    [Byte] = ".b",
    [Word] = ".w",
    [Long] = ".l",
  };
  if (n <= Long)
    return szm[n];
  return ".?";
}

// Get list of register names for movem
const char *movem_str(uint16_t bits) {
  static char mmstr[128];
  int mask, start, end, pos = 0;

  mask = 0x0001;
  pos = snprintf(mmstr, sizeof(mmstr), "[%.4x] ", bits);
  for (int i = 0; i < 16; i++) {
    if (bits & (mask << i)) {
      start = i;
      for (end = start; end < 16; end++) {
	if ((bits & (mask << end)) == 0)
	  break;
      }
      if (start != end) {
	pos += snprintf(&mmstr[pos], 128, "%s-%s/", regnames[start], regnames[end-1]);
      }
      else {
	pos += snprintf(&mmstr[pos], 128, "%s/", regnames[start]);
      }
      i = end;
    }
  };
  return mmstr;
}

const char *m68k_disasm(instr_t& i, const char *dstr)
{
  static char str[256], *pos;
  const char *cc, *comment = NULL;

  if (!dstr) {
    return "<null>";
  }

  /* Get conditional string */
  cc = condstr[(i.op >> 8) & 0xF];
  if ((i.op & 0xFF00) == 0x5100)
    cc = "ra";
  pos = str;
  while (*dstr) {
    if (replace(&dstr, "%x",  &pos, "%d",  i.xxx) ||
	replace(&dstr, "%y",  &pos, "%d",  i.yyy) ||
	replace(&dstr, "%i",  &pos, "%x",  i.imm) ||
	replace(&dstr, "%m",  &pos, "%s",  movem_str(i.imm)) ||
	replace(&dstr, "%cc", &pos, "%s ", cc) ||
	replace(&dstr, "%s",  &pos, "%s",  szstr(i.size)))
      continue;
    if (!strncmp(dstr, "%ea", 3)) {
      comment = iorname(leaval+i.src.value);
      pos += snprintf(pos, 128, "%s", i.src.eastr);
      dstr += 3;
    }
    else if (!strncmp(dstr, "%dst", 4)) {
      comment = iorname(leaval+i.dst.value);
      pos += snprintf(pos, 128, "%s", i.dst.eastr);
      dstr += 4;
    }
    else if (replace(&dstr, "%f", &pos, "<%s>", fnname(JMPDST))) {
      // symbols
      continue;
    }
    else {
      *pos++ = *dstr++;
    }
  }
  if (comment) {
    while (pos < &str[45]) {
      *pos++ = ' ';
    }
    *pos++ = ';';
    *pos++ = ' ';
    while ((*pos++ = *comment++) != 0)
      ;
  }
  *pos = 0;
  return str;
}

typedef void (*opfn_t)(instr_t&);

struct opcode_t {
  uint16_t    opbit_mask;
  uint16_t    opbit_val;
  uint16_t    eabit_src;
  uint16_t    eabit_dst;
  int         size;
  uint32_t    arg0;
  uint32_t    arg1;
  uint32_t    arg2;
  const char *mnem;
  opfn_t      fn;
  int         usage;
};

static opcode_t *opmap[65536];

/* Let the compiler do the work.... compile time evaluator of opcode bits */
constexpr opcode_t mkop(const char *bits, const char *eabits, int sz, uint32_t a0, uint32_t a1, const char *mnem, opfn_t fn)
{
  char ch = 0;
  opcode_t o = { 0 };

  o.size = sz;
  o.arg0 = a0;
  o.arg1 = a1;
  o.mnem = mnem;
  
  o.fn = fn;
  o.usage = 0;

  /* convert opcode argument types to mask */
  while ((ch = *bits++) != 0) {
    if (ch == '.')
      continue;
    o.opbit_mask <<= 1;
    o.opbit_val <<= 1;
    if (ch == '0' || ch == '1') {
      o.opbit_mask |= 0x1;
      o.opbit_val |= (ch - '0');
    }
  }

  /* Convert eabits to mask */
  for (uint16_t mask = 0x0001; (ch = *eabits++) != 0; mask <<= 1) {
    if (ch == '1') {
      o.eabit_src |= mask;
    }
    else if (ch == '3') {
      o.eabit_src |= mask;
      o.eabit_dst |= mask;
    }
  }

  /* See if we can figure out what we are */
  return o;
}

/* Check if effective address bits are valid */
constexpr bool eabit(uint16_t op, int m, int mask) {
  int y = 0;
  if (!mask) {
    return true;
  }
  if (m == 0x3F) {
    m = (op >> 3) & 7;
    y = (op & 7);
  }
  else if (m == 0x0FC0) {
    m = (op >> 6) & 7;
    y = (op >> 9) & 7;
  }
  if (m == 7)
    m += y;
  return (mask >> m) & 1;
}

// Return Effective Address ID (0..3F)
val_t mkea(int ea, int size)
{
  return val_t(ea, size);
}

val_t mkea(int eat, int nnn, int size)
{
  return val_t((eat << 3) | nnn, size);
}

static int easz(int n, int sz) {
  // stack access are word only
  switch (sz) {
  case Byte: return (n == 0x17) ? 2 : 1;
  case Word: return 2;
  case Long: return 4;
  }
  Assert(0);
}

static void nz00(uint32_t val, int size)
{
  switch (size) {
  case Byte:
    Nf = (int8_t)val < 0;
    Zf = (val & 0xFF) == 0;
    break;
  case Word:
    Nf = (int16_t)val < 0;
    Zf = (val & 0xFFFF) == 0;
    break;
  case Long:
    Nf = (int32_t)val < 0;
    Zf = (val == 0);
    break;
  }
  Vf = Cf = 0;
}

static void setea(instr_t& i, uint32_t val, int size, bool setnz = true)
{
  i.dst.set(val, size);
  /* Set NZVC flags */
  if (setnz) {
    nz00(val, size);
  }
}
static uint32_t setea2(instr_t& i, uint32_t val, int size, bool setnz=true)
{
  i.rflag = (setnz ? 0x3 : 0x1);
  i.dstval = val;
  i.dstsz = size;
  if (setnz) {
    // mulu, muls, add, sub, lsr, lsl, asr, asl
    nz00(val, size);
  }
  return val;
}

template <class T>
static void m68k_mov(instr_t& i, uint32_t src1, const bool setflag = true)
{
  i.rflag = setflag ? 0x7 : 0x1;
  i.dstval = (T)src1;
  i.dstsz = i.size;
}

// https://edwardhalferty.com/2020/12/16/decoding-the-extended-addressing-modes-of-the-68000/
uint32_t DnXn(uint32_t base, uint32_t& xn)
{
  uint32_t xxx, off = 0;

  // 68000 doesn't use scale
  // Dxxx.Wss0.oooo.oooo
  // Dxxx.Wss1.biBB.0III
  //    b = ignore base
  //    i = ignore index
  //   BB = base offset
  //  III = i/is mode
  xn = cpu_fetch(Word, "DnXn");
  xxx = (xn >> 12) & 7;

  // Only support simple mode
  if (xn & 0x100) {
    flogger(0, "@ mode:%x bs:%x is:%x bdsz:%x iis:%x\n",
	   !!(xn & 0x100),
	   !!(xn & 0x080),
	   !!(xn & 0x040),
	   (xn >> 4) & 3,
	   (xn & 7));
  }

  // Get Index
  off = ((xn & 0x8000)? A[xxx] : D[xxx]);
  if ((xn & 0x0800) == 0) {
    // word mode
    off = (int16_t)off;
  }
  return base + off + (int8_t)xn;
}

uint32_t AnX(uint32_t& an, int delta) {
  uint32_t src = an;

  an += delta;
  if (delta < 0) {
    return src = an;
  }
  return src;
}

static uint32_t d16(uint32_t src, uint32_t& xn, const char *s) {
  xn = cpu_fetch(Word, s);
  return src + (int16_t)xn;
}

/*========================================
 * Opcode arguments
 *========================================*/
uint32_t val_t::easrc() {
  uint32_t src = 0;
  uint32_t xn = 0;
  
  int n = (type & 7);
  switch (eamap[type]) {
  case EA_reg_Dn:
    trace_fmt("D%d", n);
    return D[n];
  case EA_reg_An:
    trace_fmt("A%d", n);
    return A[n];
  case EA_mem_An:
    trace_fmt("(A%d)", n);
    return A[n];
  case EA_inc_An:
    trace_fmt("(A%d)+", n);
    return AnX(A[n], easz(0x10+n, size));
  case EA_dec_An:
    trace_fmt("-(A%d)", n);
    return AnX(A[n],-easz(0x10+n, size));
  case EA_d16_An:
    src = d16(A[n], xn, "d16an");
    trace_fmt("0x%X(A%d)", xn, n);
    return src;
  case EA_Xn8_An:
    src = DnXn(A[n], xn);
    trace_fmt("0x%X(A%d,%c%d.%c)",
	      (xn & 0xFF), n,
	      (xn & 0x8000) ? 'A' : 'D',
	      (xn >> 12) & 7,
	      (xn & 0x800) ? 'l' : 'w');
    return src;
  case EA_off_16:
    src = (int16_t)cpu_fetch(Word, ".w");
    trace_fmt("(%.4x).W", src);
    return src;
  case EA_off_32:
    src = cpu_fetch(Long, ".l");
    trace_fmt("(%.8x).L", src);
    return src;
  case EA_d16_PC:
    src = d16(PC, xn, "d16pc");
    trace_fmt("0x%X(PC)", xn);
    return src;
  case EA_Xn8_PC:
    src = DnXn(PC, xn);
    trace_fmt("0x%X(A%d,%c%d.%c)",
	      (xn & 0xFF), n,
	      (xn & 0x8000) ? 'A' : 'D',
	      (xn >> 12) & 7,
	      (xn & 0x800) ? 'l' : 'w');
    return src;
  case EA_imm_sz:
    src = cpu_fetch(size, "imm");
    trace_fmt("#0x%x", src);
    return src;
  default:
    Assert(0);
  }
  return src;
}

/* Return value of effective address */
uint32_t val_t::get()
{
  Assert(type >= 0);

  if (type < 0x10 || type == 0x3c)
    // immediate, D/A already stored
    return value;
  else if (type < 0x3c)
    // read memory
    return cpu_read(value, size);
  printf("unknown type: %x\n", type);
  Assert(0);
}

/* Store value to effective address */
void val_t::set(uint32_t val, int nsz)
{
  int n = type & 7;

  if (type < 0x08) {
    setval(D[n], val, nsz);
  }
  else if (type < 0x10) {
    A[n] = val;
  }
  else if (type < 0x3c) {
    cpu_write(value, val, nsz);
  }
  else {
    flogger(0, "Unknown destination type: %x\n", type);
    Assert(0);
  }
}

void mkit(instr_t& i, val_t &v, uint16_t op, int size, int arg) {
  int xxx, yyy;

  xxx = (op >> 9) & 7;
  yyy = (op >> 0) & 7;
  v.type = -1;
  switch (arg) {
  case 0x0:
    break;
  case T_Q3:
    i.imm = xxx ? xxx : 8;
    break;
  case T_Q8:
    i.imm = (int8_t)op;
    break;
  case T_IMM4:
    i.imm = (op & 0xF);
    break;
  case T_Dy:
  case T_Ay:
  case T_iAy:
  case T_dAy:
  case T_Ay16:
    v = mkea(arg & 0xF, yyy, size);
    break;
  case T_EA:
    v = mkea((op >> 3) & 0x7, yyy, size);
    break;
  case T_Dx:
  case T_Ax:
  case T_iAx:
  case T_dAx:
    v = mkea(arg & 0xF, xxx, size);
    break;
  case T_EA2:
    v = mkea((op >> 6) & 7, xxx, size);
    break;
  case T_BRANCH:
    i.imm = PC;
    if ((op & 0xFF) == 0xFF)
      i.imm += cpu_fetch(Long,"br.l");
    else if ((op & 0xFF) == 0x00)
      i.imm += (int16_t)cpu_fetch(Word,"br.w");
    else
      i.imm += (int8_t)op;
    JMPDST = i.imm;
    break;
  default:
    printf("unknown.... %x\n", arg);
    assert(0);
  }
}

/* This decodes the instruction */
instr_t getargs(uint16_t op, int size, int arg)
{
  instr_t i = { 0 };
  size = opsize(op, size);
  Assert(size != Any);

  i.set(op, size);
  i.src.type = -1;
  i.dst.type = -1;
  int yyy = i.yyy;
  int mmm = (op >> 3) & 0x7;

  switch (arg) {
  case EA:
  case Dx_EA:
    i.src = mkea(mmm, yyy, size);
    JMPDST = SRCADDR;
    break;
  case EA_Ax:
  case EA_Dx:
  case EA_EA2:
  case Ay16_Dx:
  case rDy: // notused
  case rAy: // notused
  case None:
  case Dy_Dx: // abcd.sbcd,subx,addx
  case Dx_Dy: // notused
  case Branch: // bsr, bra, jmp, bcc
    mkit(i, i.src, op, size, (arg >> 8));
    mkit(i, i.dst, op, size, (arg & 0xff));
    break;
  case Imm_EA:
    i.imm = cpu_fetch(size, "immea");
    i.src = mkea(mmm, yyy, size);
    break;
  case Imm_SR:
    i.imm = cpu_fetch(size, "immea");
    break;
  case Imm_Ay: 
    i.imm = cpu_fetch(size,"immay");
    break;
  case Imm4: // trap
    mkit(i, i.src, op, size, arg >> 8); 
    break;
  case Imm: // oriccr/sr, andiccr/sr, eoriccr/sr, stop, 
    i.imm = cpu_fetch(size,"imm");
    break;
  case EA_SR:
  case SR_EA:
    i.src = mkea(mmm, yyy, size);
    break;
  case Iw_EA:
    i.imm = cpu_fetch(Word, "iwea");
    i.src = mkea(mmm, yyy, size);
    break;
  case q3_EA: // addq, subq
  case q3_Dy: // shifts
  case q8_Dx: // moveq
    mkit(i, i.src, op, size, arg >> 8); 
    mkit(i, i.src, op, size, arg & 0xff); 
    break;
  case iAyx: // (Ay)+,(Ax)+
  case dAyx:  // -(Ay),-(Ax)
    mkit(i, i.src, op, size, arg >> 8); 
    mkit(i, i.dst, op, size, arg & 0xff); 
    DST = i.dst.get(); //cpu_read(i.dst.value, size);
    break;
  case PC16_Dy: // dy.notused (PCd16,Dy)
    mmm = 0x0;
    i.imm = PC;
    i.imm += (int16_t)cpu_fetch(Word,"imm.dy");
    i.src = mkea(mmm, yyy, size);
    break;
  default:
    printf("unknown src: %.4x %x\n", op, arg);
    exit(0);
  }
  if (i.dst.type == -1) {
    i.dst = i.src;
  }
  return i;
}

/* ____.ccc.cnn.nnn.nnn
 *      000.0 bra
 *      000.1 bsr
 */
constexpr const char *bstr[16] = {
  "BRA", "BSR", NULL, NULL, NULL, NULL, NULL, NULL,
  NULL,  NULL,  NULL, NULL, NULL, NULL, NULL, NULL,
};
static void m68k_bcc(uint16_t op, uint32_t src)
{
  int c = (op >> 8) & 0xF;

  if (c == 1) {
    // BSR
    m68k_setpc(src, true, bstr[c]);
  }
  else if (testcond(op)) {
    m68k_setpc(src, false, bstr[c]);
  }
}

/* Conditional set, 0x00 or 0xff */
static void m68k_scc(instr_t& i)
{
  // rflag=1,nonz
  setea2(i, testcond(i.op) ? 0xFF : 0x00, Byte, false);
}

// if condition, decrement Dy, if != 0xffff, jump */
static void m68k_dbcc(instr_t& i, uint32_t& Dn, const uint32_t src)
{
  if (!testcond(i.op)) {
    // rflag=1,nonz
    uint16_t db = --Dn;
    if (db != 0xffff) {
      m68k_setpc(src, false, "");
    }
  }
}

/* Privileged move Ay,SP or move SP,Ay */
static void m68k_moveusp(uint32_t& dst, const uint32_t src)
{
  if (!m68k_trap(!Sf, VECTOR_PRIVILEGE))
    dst = src;
}

/* Swap upper and lower words Dy */
static void m68k_swap(uint32_t& dy)
{
  dy = (dy >> 16) | (dy << 16);
  nz00(dy, Long);
}

/* Load effective address to Ax */
static void m68k_lea(instr_t& i, uint32_t& dst, const uint32_t src)
{
  dst = src;
}

/* Push effective address */
static void m68k_pea(const uint32_t src)
{
  cpu_push32(src);
}

/* Test and set 0x80 */
static void m68k_tas(instr_t& i, const uint32_t src1)
{
  nz00(src1, Byte);
  // rflag=1,nonz
  setea2(i, src1 | 0x80, Byte, false);
}

/* Set N flag and trap if out of range */
static void m68k_chk(instr_t& i, int32_t dx, int32_t src)
{
  printf("cchk: %x\n", dx);
  if (dx < 0) {
    Nf = 1;
    // m68test wants original pc?
    SPC=PC;
    m68k_trap(true, VECTOR_CHK);
  }
  else if (dx > src) {
    Nf = 0;
    // m68test wants original pc?
    SPC=PC;
    m68k_trap(true, VECTOR_CHK);
  }
}

/* Unsigned multiply */
static void m68k_mulu(instr_t& i, uint16_t dx, uint16_t src)
{
  // clocks = 32+2n, n = # of 1 bits in EA
  //rflag=3,nz
  setea2(i, (uint32_t)dx * src, Long);
}

/* Signed multiply */
static void m68k_muls(instr_t& i, int16_t dx, int16_t src)
{
  // clocks = 32+2n
  //rflag=3,nz
  setea2(i, (int32_t)dx * src, Long);
}

/* Unsigned divide */
static void m68k_divu(uint32_t& dx, uint32_t src)
{
  EATR(140,1,0);
  if (src == 0) {
    m68k_trap(true, VECTOR_DIVIDE_BY_ZERO);
  }
  else {
    uint32_t q = dx / src;
    uint32_t r = dx % src;

    if (q <= 0xFFFF) {
      dx = (r << 16) | q;
      nz00(q, Word);
    }
    else {
      /* Overflow */
      Vf = 1;
    }
  }
}

/* Signed divide */
static void m68k_divs(uint32_t& dx, int16_t src)
{
  EATR(158,1,0);
  if (src == 0) {
    m68k_trap(true, VECTOR_DIVIDE_BY_ZERO);
  }
  else if (dx == 0x80000000 && src == -1) {
    nz00(0x1234, Word);
    dx = 0;
  }
  else {
    int32_t q = (int32_t)dx / src;
    int32_t r = (int32_t)dx % src;

    if (q >= -0x7FFF && q <= 0x7FFF) {
      /* Quotient in range */
      dx = (r << 16) | q;
      nz00(q, Word);
    }
    else {
      /* Overflow */
      Vf = 1;
    }
  }
}

// https://github.com/flamewing/68k-bcd-verifier
static void m68k_abcd(instr_t& i, uint8_t src1, uint8_t src2, bool src3)
{
  uint8_t sum = 0;

  Xf = Cf = Vf = 0;
  sum += (src1 & 0x0F) + (src2 & 0x0F) + src3;
  if (sum >= 0xA) {
    sum += 0x06;
  }
  sum += (src1 & 0xF0) + (src2 & 0xF0);
  if (sum >= 0xA0) {
    Xf = Cf = 1;
    sum += 0x60;
  }
  Nf = (sum & 0x80) != 0;
  if (sum != 0)
    Zf = 0;
  setea(i, sum, Byte, false);
  printf("abcd: %x %x %x -> %x.%x\n", src1, src2, src3, int(Cf), sum);
}

static void m68k_sbcd(instr_t& i, uint16_t src1, uint16_t src2, bool src3)
{
  uint16_t sum;

  Vf = 0;
  sum = src1 - src2 - src3;
  if ((src1 & 0xF) < (src2 & 0xF))
    sum -= 0x06;
  if (sum > 0x99)
    sum -= 0x60;
  Cf = (sum & 0xff00) != 0;
  if (sum & 0xff)
    Zf = 0;
  setea(i, sum, Byte, false);
}

static void m68k_add(instr_t& i, uint32_t src1, uint32_t src2, uint32_t src3, int setflag)
{
  uint32_t sum = src1 + src2 + src3;
  int zf = Zf;

  // rflag=1,nonz or rflag=3,nz
  setea2(i, sum, i.size, setflag != 0);
  if (setflag) {
    Vf = VFLAG(src1, src2, sum, i.nmask);
    Cf = CFLAG(src1, src2, sum, i.nmask);
    Xf = Cf;
  }
  // ADDX/SUBX doesn't clear zf once set
  if (setflag == _XNzVC && Zf) {
    Zf = zf;
  }
}

static void m68k_sub(instr_t& i, uint32_t src1, uint32_t src2, uint32_t src3, int setflag)
{
  uint32_t sum = src1 - src2 - src3;
  int zf = Zf;

  // rflag=1,nonz or rflag=3,nz
  setea2(i, sum, i.size, setflag != 0);
  if (setflag) {
    Vf = VFLAG_SUB(src1, src2, sum, i.nmask);
    Cf = CFLAG_SUB(src1, src2, sum, i.nmask);
    Xf = Cf;
  }
  // NEGX/SUBX
  if (setflag == _XNzVC && Zf) {
    Zf = zf;
  }
}

/* flag bits:
 * v = 0, no change, v=1, clear v
 * c = 0, no change, c=1, clear c
 */
static void m68k_cmp(instr_t& i, uint32_t src1, uint32_t src2, int size)
{
  uint32_t sum = src1 - src2;

  nz00(sum, size);
  Vf = VFLAG_SUB(src1, src2, sum, nmask(size));
  Cf = CFLAG_SUB(src1, src2, sum, nmask(size));
}

static void m68k_alu(instr_t& i, int opfn, uint32_t src1, uint32_t src2) {
  i.rflag = 0x07;
  i.dstsz = i.size;
  switch (opfn) {
  case aXOR:
    i.dstval = src1 ^ src2;
    break;
  case aAND:
    i.dstval = src1 & src2;
    break;
  case aOR:
    i.dstval = src1 | src2;
    break;
  }
}

/* Swap two registers */
static void m68k_exg(uint32_t& src1, uint32_t& src2)
{
  uint32_t tmp;

  tmp  = src1;
  src1 = src2;
  src2 = tmp;
}

// Write 2 or 4 bytes to even address
static void writebn(uint32_t addr, uint32_t val, int n)
{
  while (n--) {
    cpu_write8(addr, val >> 24);
    addr += 2;
    val <<= 8;
  }
}

// Read 2 or 4 bytes from even addresses
static uint32_t readbn(uint32_t addr, int n)
{
  uint32_t v = 0;
  
  while (n--) {
    v <<= 8;
    v |= cpu_read(addr, Byte);
    addr += 2;
  }
  return v;
}

// Movep opcode
template <int mode>
static void m68k_movep(instr_t& i, uint32_t& Dn, const char *f)
{
  if (mode == 0) {
    rmw(Dn, readbn(i.src.value, 2), 0xffff);
  }
  else if (mode == 1) {
    rmw(Dn, readbn(i.src.value, 4), 0xffffffff);
  }
  else if (mode == 2) {
    writebn(i.src.value, Dn << 16, 2);
  }
  else if (mode == 3) {
    writebn(i.src.value, Dn, 4);
  }
  else {
    Assert(0);
  }
}

/* movem <reglist>, ea
 * movem ea, <reglist>
 *
 *           r>m m>r
 *  dn       -   -
 *  an       -   -
 *  (an)     y   y
 *  (an)+    -   y
 *  -(an)    y   -
 *  (an,d16) y   y
 *  (an,xn)  y   y
 *  w        y   y
 *  l        y   y
 *  (pc,d16) -   -
 *  (pc,xn)  -   -
 *  imm      -   -
 */
template <int mode>
static void m68k_movem(instr_t& i, uint32_t list, const char *f)
{
  uint32_t base = i.src.value;
  uint32_t *reg;
  int eat, delta, ix;

  eat = eamap[i.src.type];
  delta = easz(0, i.size);
  if (eat == EA_dec_An) {
    /* Decrease memory offset
     * Initial pre-decrement already occurred in easrc
     */
    A[i.yyy] += delta;
    delta = -delta;
  }
  for (int r = 0; r < 16; r++) {
    if ((list & (1L << r)) == 0)
      continue;
    /* Get register index */
    if (eat == EA_dec_An) {
      ix = 15 - r;
    }
    else {
      ix = r;
    }
    reg = (ix <= 7) ? &D[ix] : &A[ix - 8];
    switch (mode) {
    case 0:
      /* regmem.w */
      cpu_write(base, *reg, Word);
      break;
    case 1:
      /* regmem.l */
      cpu_write(base, *reg, Long);
      break;
    case 2:
      /* memreg.w */
      *reg = (int16_t)cpu_read(base, Word);
      break;
    case 3:
      /* memreg.l */
      *reg = cpu_read(base, Long);
      break;
    }
    base += delta;
  }
  /* If pre-dec or post-inc, set new value */
  if (eat == EA_dec_An) {
    /* undo final decrement */
    A[i.yyy] = base - delta;
  }
  else if (eat == EA_inc_An) {
    A[i.yyy] = base;
  }
}

/* NOP */
static void m68k_nop() {
}

static void m68k_rts() {
  m68k_setpc(cpu_pop32(), false, "RTS");
}

static void m68k_rte() {
  /* Privileged instruction */
  if (m68k_trap(!Sf, VECTOR_PRIVILEGE))
    return;
  uint16_t tsr = cpu_pop16();
  uint32_t npc = cpu_pop32();

  m68k_setsr(tsr, Word);
  m68k_setpc(npc, false, "RTE");
  ipl = 0;
}

static void m68k_rtr() {
  uint16_t tsr = cpu_pop16();
  uint32_t npc = cpu_pop32();

  m68k_setsr(tsr, Byte);
  m68k_setpc(npc, false, "RTR");
}

static void m68k_stop(int imm) {
  /* Privileged instruction */
  if (m68k_trap(!Sf, VECTOR_PRIVILEGE)) {
    return;
  }
  if (!stopped) {
    stopped = 1;
    //printf("%.8x CPU STOPPED %x, %x\n", SPC, imm, SR);
  }
  SR = imm;
}

static void m68k_reset() {
  /* Privileged instruction */
  if (m68k_trap(!Sf, VECTOR_PRIVILEGE))
    return;
  /* Not a real reset */
  printf("reset:%.8x\n", SPC);
}

static void m68k_link(uint32_t& An, int16_t imm) {
  // SP=SP-4, M[SP] = An; An=SP; SP += d
  SP -= 4;
  cpu_write32(SP, An);
  An = SP;
  SP += imm;
}

static void m68k_unlink(uint32_t& An) {
  SP = An;
  An = cpu_pop32();
}

/* Get bitmask for bit type operations
 * If destination is Dy, it is Long, else Byte
 */
static uint32_t bmask(instr_t& i, uint32_t v, int& size)
{
  // Dn bitmask
  if (eamap[i.dst.type] == EA_reg_Dn) {
    size = Long;
    return (1L << (v & 0x1F));
  }
  // An bitmask
  size = Byte;
  return (1L << (v & 0x07));
}

// p = 4(1/0)
// r = 4(1/0)
// imm.dy    static.reg.long   10(2/0)  read.p, read.imm, read.dy   pE (6.1.0)
// imm.mem   static.mem.byte   8(2/0)+  read.p, read.imm, read.ea   pE (4.1.0)
// dx,dy     dynamic.reg.long  6(1/0)   read.p  read.dx,  read.dy   pE (2.0.0)
// dx,mem    dynamic.reg.byte  4(1/0)+  read.p, read.dx,  read.ea   pE (0.0.0)
#if 0
int rt() {
  ddx = (op & 0x38);
  switch ((op >> 6) & 7) {
  case 0b000: btst imm return ddx ? EATR(8,2,0,op)  : TR(10,2,0);
  case 0b001: bchg imm return ddx ? EATR(12,2,1,op) : TR(12,2,0);
  case 0b010: bclr imm return ddx ? EATR(12,2,1,op) : TR(14,2,0);
  case 0b011: bset imm return ddx ? EATR(12,2,1,op) : TR(12,2,0);
  case 0b100: btst ex  return ddx ? EATR(4,1,0,op) : TR(6,1,0);
  case 0b101: bchg dx  return ddx ? EATR(8,1,1,op) : TR(8,1,0);
  case 0b110: bclr dx  return ddx ? EATR(8,1,1,op) : TR(10,1,0);
  case 0b111: bset dx  return ddx ? EATR(8,1,1,op) : TR(8,1,0);
  }
}
#endif
static void m68k_btst(instr_t& i, uint32_t src1, uint32_t src2, int size=None)
{
  src2 = bmask(i, src2, size);

  Zf = (src1 & src2) == 0;
}

static void m68k_bchg(instr_t& i, uint32_t src1, uint32_t src2, int size=None)
{
  src2 = bmask(i, src2, size);

  // rflag=1,nonz
  setea2(i, src1 ^ src2, size, false);
  Zf = (src1 & src2) == 0;
}

static void m68k_bset(instr_t& i, uint32_t src1, uint32_t src2, int size=None)
{
  src2 = bmask(i, src2, size);
  
  // rflag=1,nonz
  setea2(i, src1 | src2, size, false);
  Zf = (src1 & src2) == 0;
}

static void m68k_bclr(instr_t& i, uint32_t src1, uint32_t src2, int size=None)
{
  src2 = bmask(i, src2, size);
  
  // rflag=1,nonz
  setea2(i, src1 & ~src2, size, false);
  Zf = (src1 & src2) == 0;
}

/* Calculate rotate bits */
static int roxbits(instr_t& i, uint32_t& v) {
  v &= i.zmask;

  switch (i.size) {
  case Byte: return 7;
  case Word: return 15;
  case Long: return 31;
  }
  Assert(0);
};

/* Clocks for shift operations
 *     dn          an       m
 * bw  6+2n(1/0)            8(1/1)+
 *  l  8+2n(1/0)
 */
static void m68k_lsr(instr_t& i, uint32_t src1, uint32_t src2, int size = None)
{
  bool c = 0;

  roxbits(i, src1);
  for (uint32_t i = 0; i < src2; i++) {
    c = testbit(src1, 0);
    src1 >>= 1;
    Xf = c;
  }
  // rflag=3,nz
  setea2(i, src1, i.size);
  Cf = c;
}

static void m68k_lsl(instr_t& i, uint32_t src1, uint32_t src2)
{
  int bits;
  bool c = 0;

  bits = roxbits(i, src1);
  for (uint32_t i = 0; i < src2; i++) {
    c = testbit(src1, bits);
    src1 <<= 1;
    Xf = c;
  }
  // rflag=3,nz
  setea2(i, src1, i.size);
  Cf = c;
}

/* X = last bit shifted out, n/a if count == 0
 * N = msb of result
 * Z = result = 0
 * V = msb changed anytime, else clear
 * C = last bit shifted out, n/a if count == 0
 */
static void m68k_asr(instr_t& i, uint32_t src1, uint32_t src2)
{
  uint32_t bits, m;
  bool c = 0;

  bits = roxbits(i, src1);
  m = (src1 & (1L << bits)); // msb
  for (uint32_t i = 0; i < src2; i++) {
    c = testbit(src1, 0);
    src1 = (src1 >> 1) | m;
    Xf = c;
  }
  // rflag=3,nz
  setea2(i, src1, i.size);
  Cf = c;
}

static void m68k_asl(instr_t& i, uint32_t src1, uint32_t src2)
{
  uint32_t bits, r;
  bool c = 0;
  bool v = 0;
  
  bits = roxbits(i, src1);
  for (uint32_t i = 0; i < src2; i++) {
    c = testbit(src1, bits);
    r = (src1 << 1);
    if (testbit(r ^ src1, bits)) {
      v = 1;
    }
    src1 = r;
    Xf = c;
  }
  // rflag=3,nz
  setea2(i, src1, i.size);
  Cf = c;
  Vf = v;
}

static void m68k_roxr(instr_t& i, uint32_t src1, uint32_t src2)
{
  int bits;
  bool c = Xf;
  
  bits = roxbits(i, src1);
  for (uint32_t i = 0; i < src2; i++) {
    c = testbit(src1, 0);
    src1 = (src1 >> 1) | (Xf << bits);
    Xf = c;
  }
  // rflag=3,nz
  setea2(i, src1, i.size);
  Cf = c;
}

static void m68k_roxl(instr_t& i, uint32_t src1, uint32_t src2)
{
  int bits;
  bool c = Xf;
  
  bits = roxbits(i, src1);
  for (uint32_t i = 0; i < src2; i++) {
    c = testbit(src1, bits);
    src1 = (src1 << 1) | Xf;
    Xf = c;
  }
  // rflag=3,nz
  setea2(i, src1, i.size);
  Cf = c;
}

static void m68k_ror(instr_t& i, uint32_t src1, uint32_t src2)
{
  int bits;
  bool c = 0;
  
  bits = roxbits(i, src1);
  for (uint32_t i = 0; i < src2; i++) {
    c = testbit(src1, 0);
    src1 = (src1 >> 1) | (c << bits);
  }
  // rflag=3,nz
  setea2(i, src1, i.size);
  Cf = c;
  Vf = 0;
}

static void m68k_rol(instr_t& i, uint32_t src1, uint32_t src2)
{
  int bits;
  bool c = 0;
  
  bits = roxbits(i, src1);
  for (uint32_t i = 0; i < src2; i++) {
    c = testbit(src1, bits);
    src1 = (src1 << 1) | c;
  }
  // rflag=3,nz
  setea2(i, src1, i.size);
  Cf = c;
  Vf = 0;
}

/* Decode opcode */
int decode_68k(const uint16_t op)
{
  opcode_t *opc;
  uint32_t sj;
  uint32_t 

  ir = 0;
  IR[ir++] = op;
  for (int i = 0; i < 16; i++)
    sregs[i] = regs[i];
  if ((sj = setjmp(trapjmp)) != 0) {
    printf("Return from trap... %x\n", sj);
    return -1;
  }
  ctick = 0;
  opc = opmap[op];
  if (opc && opc->fn) {
    instr_t i = getargs(op, opc->size, opc->arg0);
    opc->usage++;
    if (trace & 1) {
      cpu_showregs();
    }
    if (trace & 2) {
      printf("!%.6X  %s\n", SPC, m68k_disasm(i, opc->mnem));
    }
    i.rflag = 0;
    opc->fn(i);
    if (i.rflag & 0x3) {
      // write result
      i.dst.set(i.dstval, i.dstsz);
    }
    if (i.rflag & 0x4) {
      // post-nz00
      nz00(i.dstval, i.dstsz);
    }
    //printf("%.4x TR: %4d(%2d/%2d) %s\n", op, (ctick >> 8), (ctick >> 4) & 0xF, ctick & 0xF, opc->mnem);
    return 0;
  }
  printf("missing: %.8x %.4x\n", SPC, op);

  /* Execute Illegal Instruction opcode */
  m68k_trap(true, VECTOR_ILLEGAL_INSTRUCTION);
  return 0;
}

/* Create opcode table then opmap for all decode options */
#define fnarg instr_t& i

#define o(bits, eabits, flags, rwarg, size, arg, a1, mnem, fn) mkop(bits, eabits, size, arg, 0, mnem, [](fnarg) fn)

opcode_t optab[] = {
  o("0000.000.000.111.100", "___________x", "______", "pNNpp-------", Byte, Imm_SR,   SR,   "or.b    %i, CCR",       { m68k_setsr(cpu_getflags() | IMM, Byte); }),
  o("0000.000.001.111.100", "___________x", "S_____", "pNNpp-------", Word, Imm_SR,   SR,   "or.w    %i, SR",        { m68k_setsr(cpu_getflags() | IMM, Word); }),
  o("0000.000.0ss.mmm.yyy", "1_1111111___", "__NZ00", "------------", Any,  Imm_EA,   EA,   "or%s    %i, %ea",       { m68k_alu(i, aOR, SRC, IMM); }),
  o("0000.001.000.111.100", "___________x", "______", "pNNpp-------", Byte, Imm_SR,   SR,   "and.b   %i, CCR",       { m68k_setsr(cpu_getflags() & IMM, Byte); }),
  o("0000.001.001.111.100", "___________x", "S_____", "pNNpp-------", Word, Imm_SR,   SR,   "and.w   %i, SR",        { m68k_setsr(cpu_getflags() & IMM, Word); }),
  o("0000.001.0ss.mmm.yyy", "1_1111111___", "__NZ00", "------------", Any,  Imm_EA,   EA,   "and%s   %i, %ea",       { m68k_alu(i, aAND, SRC, IMM); }), // gand
  o("0000.010.0ss.mmm.yyy", "1_1111111___", "_XNZVC", "------------", Any,  Imm_EA,   EA,   "sub%s   %i, %ea",       { m68k_sub(i, SRC, IMM, 0, _XNZVC); }), // gsub
  o("0000.011.0ss.mmm.yyy", "1_1111111___", "_XNZVC", "------------", Any,  Imm_EA,   EA,   "add%s   %i, %ea",       { m68k_add(i, SRC, IMM, 0, _XNZVC); }), // gadd
  o("0000.101.000.111.100", "___________x", "______", "pNNpp-------", Byte, Imm_SR,   SR,   "eor.b   %i, CCR",       { m68k_setsr(cpu_getflags() ^ IMM, Byte); }),
  o("0000.101.001.111.100", "___________x", "S_____", "pNNpp-------", Word, Imm_SR,   SR,   "eor.w   %i, SR",        { m68k_setsr(cpu_getflags() ^ IMM, Word); }),
  o("0000.101.0ss.mmm.yyy", "1_1111111___", "__NZ00", "------------", Any,  Imm_EA,   EA,   "eor%s   %i, %ea",       { m68k_alu(i, aXOR, SRC, IMM); }), // gxor
  o("0000.110.0ss.mmm.yyy", "1_1111111___", "__NZVC", "------------", Any,  Imm_EA,   EA,   "cmp%s   %i, %ea",       { m68k_cmp(i,  SRC, IMM, i.size); }),
  o("0000.100.000.mmm.yyy", "1_111111111_", "___Z__", "------------", Byte, Imm_EA,   EA,   "btst    %i, %ea",       { m68k_btst(i, SRC, IMM); }),
  o("0000.100.001.mmm.yyy", "1_1111111___", "___Z__", "------------", Byte, Imm_EA,   EA,   "bchg    %i, %ea",       { m68k_bchg(i, SRC, IMM); }),
  o("0000.100.010.mmm.yyy", "1_1111111___", "___Z__", "------------", Byte, Imm_EA,   EA,   "bclr    %i, %ea",       { m68k_bclr(i, SRC, IMM); }),
  o("0000.100.011.mmm.yyy", "1_1111111___", "___Z__", "------------", Byte, Imm_EA,   EA,   "bset    %i, %ea",       { m68k_bset(i, SRC, IMM); }),
  o("0000.ddd.100.mmm.yyy", "1_1111111111", "___Z__", "------------", Byte, Dx_EA,    EA,   "btst    D%x, %ea",      { m68k_btst(i, SRC, Dx); }),
  o("0000.ddd.101.mmm.yyy", "1_1111111___", "___Z__", "------------", Byte, Dx_EA,    EA,   "bchg    D%x, %ea",      { m68k_bchg(i, SRC, Dx); }),
  o("0000.ddd.110.mmm.yyy", "1_1111111___", "___Z__", "------------", Byte, Dx_EA,    EA,   "bclr    D%x, %ea",      { m68k_bclr(i, SRC, Dx); }),
  o("0000.ddd.111.mmm.yyy", "1_1111111___", "___Z__", "------------", Byte, Dx_EA,    EA,   "bset    D%x, %ea",      { m68k_bset(i, SRC, Dx); }),
  o("0000.ddd.100.001.aaa", "_x__________", "______", "------------", Word, Ay16_Dx,  None, "movep.w %ea, D%x",      { m68k_movep<0>(i, Dx, "memreg.w"); }),
  o("0000.ddd.101.001.aaa", "_x__________", "______", "------------", Long, Ay16_Dx,  None, "movep.l %ea, D%x",      { m68k_movep<1>(i, Dx, "memreg.l"); }),
  o("0000.ddd.110.001.aaa", "_x__________", "______", "------------", Word, Ay16_Dx,  None, "movep.w D%x, %ea",      { m68k_movep<2>(i, Dx, "regmem.w"); }),
  o("0000.ddd.111.001.aaa", "_x__________", "______", "------------", Long, Ay16_Dx,  None, "movep.l D%x, %ea",      { m68k_movep<3>(i, Dx, "regmem.l"); }),

  o("0010.aaa.001.mmm.yyy", "111111111111", "______", "------------", Long, EA_Ax,    Ax,   "movea.l %ea, A%x",      { m68k_mov<uint32_t>(i, SRC, false); }),
  o("0011.aaa.001.mmm.yyy", "111111111111", "______", "------------", Word, EA_Ax,    Ax,   "movea.w %ea, A%x",      { m68k_mov<int16_t>(i,  SRC, false); }),
  o("0001.xxx.MMM.mmm.yyy", "3_3333333111", "__NZ00", "------------", Byte, EA_EA2,   EA2,  "move.b  %ea, %dst",     { m68k_mov<uint8_t>(i, SRC); }),
  o("0010.xxx.MMM.mmm.yyy", "313333333111", "__NZ00", "------------", Long, EA_EA2,   EA2,  "move.l  %ea, %dst",     { m68k_mov<uint32_t>(i, SRC); }),
  o("0011.xxx.MMM.mmm.yyy", "313333333111", "__NZ00", "------------", Word, EA_EA2,   EA2,  "move.w  %ea, %dst",     { m68k_mov<uint16_t>(i, SRC); }),

  o("0100.000.011.mmm.yyy", "1_1111111___", "______", "------------", Word, SR_EA,    EA,   "move.w  SR, %ea",       { m68k_mov<uint32_t>(i, cpu_getflags(), false); }),
  o("0100.010.011.mmm.yyy", "1_1111111111", "______", "E.NNp-------", Word, EA_SR,    SR,   "move.b  %ea, CCR",      { m68k_setsr(SRC, Byte); }),
  o("0100.011.011.mmm.yyy", "1_1111111111", "S_____", "E.NNp-------", Word, EA_SR,    SR,   "move.w  %ea, SR",       { m68k_setsr(SRC, Word); }),
  o("0100.000.0ss.mmm.yyy", "1_1111111___", "_XNZVC", "------------", Any,  EA,       None, "negx%s  %ea",           { m68k_sub(i, 0, SRC, Xf, _XNzVC); }), // sub
  o("0100.001.0ss.mmm.yyy", "1_1111111___", "__0100", "------------", Any,  EA,       None, "clr%s   %ea",           { m68k_mov<uint32_t>(i, 0); }),
  o("0100.010.0ss.mmm.yyy", "1_1111111___", "_XNZVC", "------------", Any,  EA,       None, "neg%s   %ea",           { m68k_sub(i, 0, SRC, 0, _XNZVC); }),
  o("0100.011.0ss.mmm.yyy", "1_1111111___", "__NZ00", "------------", Any,  EA,       None, "not%s   %ea",           { m68k_mov<uint32_t>(i, ~SRC); }),
  o("0100.100.010.000.ddd", "x___________", "__NZ00", "p-----------", Word, rDy,      None, "ext.w   D%y",           { m68k_mov<int8_t>(i, Dy); }),
  o("0100.100.011.000.ddd", "x___________", "__NZ00", "p-----------", Long, rDy,      None, "ext.l   D%y",           { m68k_mov<int16_t>(i, Dy); }),
  o("0100.100.000.mmm.yyy", "1_1111111___", "_X?z?C", "------------", Byte, EA,       None, "nbcd    %ea",           { m68k_sbcd(i, 0, SRC, Xf); }),
  o("0100.100.001.000.ddd", "x___________", "__NZ00", "p-----------", Word, rDy,      None, "swap    D%y",           { m68k_swap(Dy); }),
  o("0100.100.001.mmm.yyy", "__1__111111_", "S_____", "------------", Long, EA,       None, "pea     %ea",           { m68k_pea(SRCADDR); }),
  o("0100.101.011.111.100", "___________x", "______", "------------", None, None,     None, "illegal",               { m68k_trap(true, VECTOR_ILLEGAL_INSTRUCTION); }),
  o("0100.101.011.mmm.yyy", "1_1111111___", "__NZ00", "------------", Byte, EA,       None, "tas     %ea",           { m68k_tas(i, SRC); }),
  o("0100.101.0ss.mmm.yyy", "1_111111111_", "__NZ00", "E.p---------", Any,  EA,       None, "tst%s   $%ea",          { m68k_cmp(i, SRC, 0, i.size); }),
  o("0100.111.001.00v.vvv", "xx__________", "______", "------------", None, Imm4,     None, "trap    %i",            { m68k_trap(true, 32 + IMM); }),
  o("0100.111.001.010.aaa", "__x_________", "______", "------------", Word, Imm_Ay,   None, "link    A%y, %i",       { m68k_link(Ay, IMM); }),
  o("0100.111.001.011.aaa", "___x________", "______", "ppp---------", None, rAy,      None, "unlk    A%y",           { m68k_unlink(Ay); }),
  o("0100.111.001.100.aaa", "____x_______", "S_____", "p-----------", Long, rAy,      None, "move    A%y, USP",      { m68k_moveusp(USP, Ay); }),
  o("0100.111.001.101.aaa", "_____x______", "S_____", "p-----------", Long, rAy,      None, "move    USP, A%y",      { m68k_moveusp(Ay, USP); }),
  o("0100.111.001.110.000", "______x_____", "S_____", "------------", None, None,     None, "reset",                 { m68k_reset(); }),
  o("0100.111.001.110.001", "______x_____", "______", "p-----------", None, None,     None, "nop",                   { m68k_nop(); }),
  o("0100.111.001.110.010", "______x_____", "SXNZVC", "nn----------", Word, Imm,      None, "stop",                  { m68k_stop(IMM); }),
  o("0100.111.001.110.011", "______x_____", "S_____", "uuupp-------", None, None,     None, "rte",                   { m68k_rte(); }),
  o("0100.111.001.110.101", "______x_____", "______", "uupp--------", None, None,     None, "rts",                   { m68k_rts(); }),
  o("0100.111.001.110.110", "______x_____", "______", "------------", None, None,     None, "trapv",                 { m68k_trap(Vf, VECTOR_TRAPV); }),
  o("0100.111.001.110.111", "______x_____", "_XNZVC", "------------", None, None,     None, "rtr",                   { m68k_rtr(); }),
  o("0100.111.010.mmm.yyy", "__1__111111_", "______", "------------", Long, EA,       None, "jsr     %ea %f",        { m68k_setpc(SRCADDR, true, "JSR"); }),
  o("0100.111.011.mmm.yyy", "__1__111111_", "______", "------------", Long, EA,       None, "jmp     %ea",           { m68k_setpc(SRCADDR, false, "JMP"); }),
  o("0100.100.010.mmm.yyy", "__1_11111___", "______", "------------", Word, Iw_EA,    EA,   "movem%s %m, %ea",       { m68k_movem<0>(i, IMM, "regmem.w"); }),
  o("0100.100.011.mmm.yyy", "__1_11111___", "______", "------------", Long, Iw_EA,    EA,   "movem%s %m, %ea",       { m68k_movem<1>(i, IMM, "regmem.l"); }),
  o("0100.110.010.mmm.yyy", "__11_111111_", "______", "------------", Word, Iw_EA,    EA,   "movem%s %ea, %m",       { m68k_movem<2>(i, IMM, "memreg.w"); }),
  o("0100.110.011.mmm.yyy", "__11_111111_", "______", "------------", Long, Iw_EA,    EA,   "movem%s %ea, %m",       { m68k_movem<3>(i, IMM, "memreg.l"); }),
  o("0100.aaa.111.mmm.yyy", "__1__111111_", "______", "------------", Long, EA_Ax,    None, "lea     %ea, A%x",      { m68k_lea(i, Ax, SRCADDR); }),
  o("0100.ddd.110.mmm.yyy", "1_1111111111", "__N???", "------------", Word, EA_Dx,    None, "chk     D%x, %ea",      { m68k_chk(i, Dx, SRC); }),

  o("0101.ccc.c11.001.ddd", "_x__________", "______", "------------", Word, PC16_Dy,  None, "db%cc   D%y, %i",       { m68k_dbcc(i, Dy, IMM); }),
  o("0101.ccc.c11.mmm.yyy", "1_1111111___", "______", "------------", Byte, EA,       None, "s%cc    %ea",           { m68k_scc(i); }),
  o("0101.qqq.0ss.mmm.yyy", "_1__________", "______", "------------", Any,  q3_EA,    None, "addq%s  %i, %ea",       { m68k_add(i, SRC, IMM, 0, NOSET); }), // gadd
  o("0101.qqq.0ss.mmm.yyy", "1_1111111___", "_XNZVC", "------------", Any,  q3_EA,    None, "addq%s  %i, %ea",       { m68k_add(i, SRC, IMM, 0, _XNZVC); }), // gadd
  o("0101.qqq.1ss.mmm.yyy", "_1__________", "______", "------------", Any,  q3_EA,    None, "subq%s  %i, %ea",       { m68k_sub(i, SRC, IMM, 0, NOSET); }), // gsub
  o("0101.qqq.1ss.mmm.yyy", "1_1111111___", "_XNZVC", "------------", Any,  q3_EA,    None, "subq%s  %i, %ea",       { m68k_sub(i, SRC, IMM, 0, _XNZVC); }), // gsub

  o("0110.ccc.cnn.nnn.nnn", "____________", "______", "------------", Long, Branch,   None, "b%cc    %i %f",         { m68k_bcc(i.op, IMM); }),
  o("0111.ddd.0qq.qqq.qqq", "____________", "__NZ00", "------------", Long, q8_Dx,    None, "moveq   %i, D%x",       { m68k_mov<uint32_t>(i, IMM); }),

  o("1000.ddd.0ss.mmm.yyy", "1_1111111111", "__NZ00", "------------", Any,  EA_Dx,    None, "or%s    %ea, D%x",      { m68k_alu(i, aOR, SRC, Dx); }),
  o("1000.ddd.1ss.mmm.yyy", "__1111111___", "__NZ00", "------------", Any,  Dx_EA,    None, "or%s    D%x, %ea",      { m68k_alu(i, aOR, Dx, SRC); }),
  o("1000.ddd.011.mmm.yyy", "1_1111111111", "__NZV0", "------------", Word, EA_Dx,    None, "divu    %ea, D%x",      { m68k_divu(Dx, SRC); }),
  o("1000.ddd.111.mmm.yyy", "1_1111111111", "__NZV0", "------------", Word, EA_Dx,    None, "divs    %ea, D%x",      { m68k_divs(Dx, SRC); }),
  o("1000.ddd.100.000.ddd", "x___________", "_X?z?C", "------------", Byte, Dy_Dx,    None, "sbcd    D%y, D%x",      { m68k_sbcd(i, Dx, Dy, Xf); }),
  o("1000.xxx.100.001.yyy", "_x__________", "_X?z?C", "------------", Byte, dAyx,     None, "sbcd    -(A%y),-(A%x)", { m68k_sbcd(i, DST, SRC, Xf); }),

  o("1001.aaa.011.mmm.yyy", "111111111111", "______", "------------", Word, EA_Ax,    None, "suba.w  %ea, A%x",      { setval(Ax, Ax - (int16_t)SRC, Long); }),
  o("1001.aaa.111.mmm.yyy", "111111111111", "______", "------------", Long, EA_Ax,    None, "suba.l  %ea, A%x",      { m68k_sub(i, Ax,  SRC, 0, NOSET); }),  // gsub
  o("1001.ddd.0ss.mmm.yyy", "111111111111", "_XNZVC", "------------", Any,  EA_Dx,    None, "sub%s   %ea, D%x",      { m68k_sub(i, Dx,  SRC, 0, _XNZVC); }), // gsub ->
  o("1001.ddd.1ss.mmm.yyy", "s_1111111___", "_XNZVC", "------------", Any,  Dx_EA,    None, "sub%s   D%x, %ea",      { m68k_sub(i, SRC, Dx,  0, _XNZVC); }), // gsub <-
  o("1001.ddd.1ss.000.ddd", "x___________", "_XNzVC", "------------", Any,  Dy_Dx,    None, "subx%s  D%y, D%x",      { m68k_sub(i, Dx,  Dy,  Xf, _XNzVC); }), // gsub
  o("1001.xxx.1ss.001.yyy", "_x__________", "_XNzVC", "------------", Any,  dAyx,     None, "subx%s  -(A%y),-(A%x)", { m68k_sub(i, DST, SRC, Xf, _XNzVC); }), // gsub

  o("1011.aaa.011.mmm.yyy", "111111111111", "__NZVC", "------------", Word, EA_Ax,    None, "cmpa.w  %ea, A%x",      { m68k_cmp(i, Ax, (int16_t)SRC, Long); }),
  o("1011.aaa.111.mmm.yyy", "111111111111", "__NZVC", "------------", Long, EA_Ax,    None, "cmpa.l  %ea, A%x",      { m68k_cmp(i, Ax, SRC, i.size); }),
  o("1011.ddd.0ss.mmm.yyy", "111111111111", "__NZVC", "------------", Any,  EA_Dx,    None, "cmp%s   %ea, D%x",      { m68k_cmp(i, Dx, SRC, i.size); }),
  o("1011.xxx.1ss.001.yyy", "_x__________", "__NZVC", "------------", Any,  iAyx,     None, "cmpm%s  (A%y)+,(A%x)+", { m68k_cmp(i, DST, SRC, i.size); }),
  o("1011.ddd.1ss.mmm.yyy", "1_1111111___", "__NZ00", "------------", Any,  Dx_EA,    None, "eor%s   D%x, %ea",      { m68k_alu(i, aXOR, SRC, Dx); }),

  o("1100.ddd.0ss.mmm.yyy", "1_1111111111", "__NZ00", "------------", Any,  EA_Dx,    None, "and%s   %ea, D%x",      { m68k_alu(i, aAND, SRC, Dx); }),
  o("1100.ddd.1ss.mmm.yyy", "s_1111111___", "__NZ00", "------------", Any,  Dx_EA,    None, "and%s   D%x, %ea",      { m68k_alu(i, aAND, Dx, SRC); }),
  o("1100.ddd.011.mmm.yyy", "1_1111111111", "__NZV0", "------------", Word, EA_Dx,    None, "mulu    %ea, D%x",      { m68k_mulu(i, Dx, SRC); }),
  o("1100.ddd.111.mmm.yyy", "1_1111111111", "__NZV0", "------------", Word, EA_Dx,    None, "muls    %ea, D%x",      { m68k_muls(i, Dx, SRC); }),
  o("1100.ddd.100.000.ddd", "x___________", "_X?z?C", "------------", Byte, Dy_Dx,    None, "abcd    D%y, D%x",      { m68k_abcd(i, Dy, Dx, Xf); }),
  o("1100.xxx.100.001.yyy", "_x__________", "_X?z?C", "------------", Byte, dAyx,     None, "abcd    -(A%y),-(A%x)", { m68k_abcd(i, SRC, DST, Xf); }),
  o("1100.ddd.101.000.ddd", "x___________", "______", "pn----------", Long, Dx_Dy,    None, "exg%s   D%y, D%x",      { m68k_exg(Dy, Dx); }),
  o("1100.aaa.101.001.aaa", "_x__________", "______", "pn----------", Long, Dx_Dy,    None, "exg%s   A%y, A%x",      { m68k_exg(Ay, Ax); }),
  o("1100.ddd.110.001.aaa", "_x__________", "______", "pn----------", Long, Dx_Dy,    None, "exg%s   A%y, D%x",      { m68k_exg(Ay, Dx); }),

  o("1101.ddd.0ss.mmm.yyy", "111111111111", "_XNZVC", "------------", Any,  EA_Dx,    None, "add%s   %ea, D%x",      { m68k_add(i, SRC, Dx, 0, _XNZVC); }), // gadd ->
  o("1101.ddd.1ss.mmm.yyy", "s_1111111___", "_XNZVC", "------------", Any,  Dx_EA,    None, "add%s   D%x, %ea",      { m68k_add(i, Dx,  SRC,0, _XNZVC); }), // gadd <-
  o("1101.aaa.011.mmm.yyy", "111111111111", "______", "------------", Word, EA_Ax,    None, "adda.w  %ea, A%x",      { setval(Ax, Ax + (int16_t)SRC, Long); }),
  o("1101.aaa.111.mmm.yyy", "111111111111", "______", "------------", Long, EA_Ax,    None, "adda.l  %ea, A%x",      { m68k_add(i, Ax,  SRC, 0, 0); }), // gadd
  o("1101.ddd.1ss.000.ddd", "x___________", "_XNzVC", "------------", Any,  Dy_Dx,    None, "addx%s  D%y, D%x",      { m68k_add(i, Dy,  Dx, Xf, _XNzVC); }), // gadd
  o("1101.xxx.1ss.001.yyy", "_x__________", "_XNzVC", "------------", Any,  dAyx,     None, "addx%s  -(A%y),-(A%x)", { m68k_add(i, SRC, DST,Xf, _XNzVC); }), // gadd

  o("1110.000.011.mmm.yyy", "s_1111111___", "_XNZVC", "E.rw--------", Word, EA,       None, "asr     %ea",           { m68k_asr(i,  SRC, 1); }),// gshift
  o("1110.000.111.mmm.yyy", "s_1111111___", "_XNZVC", "E.rw--------", Word, EA,       None, "asl     %ea",           { m68k_asl(i,  SRC, 1); }),// gshift
  o("1110.001.011.mmm.yyy", "s_1111111___", "_XNZ0C", "E.rw--------", Word, EA,       None, "lsr     %ea",           { m68k_lsr(i,  SRC, 1); }),// gshift
  o("1110.001.111.mmm.yyy", "s_1111111___", "_XNZ0C", "E.rw--------", Word, EA,       None, "lsl     %ea",           { m68k_lsl(i,  SRC, 1); }),// gshift
  o("1110.010.011.mmm.yyy", "s_1111111___", "_XNZ0C", "E.rw--------", Word, EA,       None, "roxr    %ea",           { m68k_roxr(i, SRC, 1); }),// gshift
  o("1110.010.111.mmm.yyy", "s_1111111___", "_XNZ0C", "E.rw--------", Word, EA,       None, "roxl    %ea",           { m68k_roxl(i, SRC, 1); }),// gshift
  o("1110.011.011.mmm.yyy", "s_1111111___", "__NZ0C", "E.rw--------", Word, EA,       None, "ror     %ea",           { m68k_ror(i,  SRC, 1); }),// gshift
  o("1110.011.111.mmm.yyy", "s_1111111___", "__NZ0C", "E.rw--------", Word, EA,       None, "rol     %ea",           { m68k_rol(i,  SRC, 1); }),// gshift
  o("1110.qqq.0ss.000.ddd", "x___________", "_XNZVC", "------------", Any,  q3_Dy,    None, "asr%s   %i, D%y",       { m68k_asr(i,  Dy,  IMM); }),// gshift
  o("1110.qqq.1ss.000.ddd", "x___________", "_XNZVC", "------------", Any,  q3_Dy,    None, "asl%s   %i, D%y",       { m68k_asl(i,  Dy,  IMM); }),// gshift
  o("1110.qqq.0ss.001.ddd", "_x__________", "_XNZ0C", "------------", Any,  q3_Dy,    None, "lsr%s   %i, D%y",       { m68k_lsr(i,  Dy,  IMM); }),// gshift
  o("1110.qqq.1ss.001.ddd", "_x__________", "_XNZ0C", "------------", Any,  q3_Dy,    None, "lsl%s   %i, D%y",       { m68k_lsl(i,  Dy,  IMM); }),// gshift
  o("1110.qqq.0ss.010.ddd", "__x_________", "_XNZ0C", "------------", Any,  q3_Dy,    None, "roxr%s  %i, D%y",       { m68k_roxr(i, Dy,  IMM); }),// gshift
  o("1110.qqq.1ss.010.ddd", "__x_________", "_XNZ0C", "------------", Any,  q3_Dy,    None, "roxl%s  %i, D%y",       { m68k_roxl(i, Dy,  IMM); }),// gshift
  o("1110.qqq.0ss.011.ddd", "___x________", "__NZ0C", "------------", Any,  q3_Dy,    None, "ror%s   %i, D%y",       { m68k_ror(i,  Dy,  IMM); }),// gshift
  o("1110.qqq.1ss.011.ddd", "___x________", "__NZ0C", "------------", Any,  q3_Dy,    None, "rol%s   %i, D%y",       { m68k_rol(i,  Dy,  IMM); }),// gshift
  o("1110.ddd.0ss.100.ddd", "____x_______", "_XNZVC", "------------", Any,  Dx_Dy,    None, "asr%s   D%x, D%y",      { m68k_asr(i,  Dy,  Dx & 0x3f); }),// gshift
  o("1110.ddd.1ss.100.ddd", "____x_______", "_XNZVC", "------------", Any,  Dx_Dy,    None, "asl%s   D%x, D%y",      { m68k_asl(i,  Dy,  Dx & 0x3f); }),// gshift
  o("1110.ddd.0ss.101.ddd", "_____x______", "_XNZ0C", "------------", Any,  Dx_Dy,    None, "lsr%s   D%x, D%y",      { m68k_lsr(i,  Dy,  Dx & 0x3f); }),// gshift
  o("1110.ddd.1ss.101.ddd", "_____x______", "_XNZ0C", "------------", Any,  Dx_Dy,    None, "lsl%s   D%x, D%y",      { m68k_lsl(i,  Dy,  Dx & 0x3f); }),// gshift
  o("1110.ddd.0ss.110.ddd", "______x_____", "_XNZ0C", "------------", Any,  Dx_Dy,    None, "roxr%s  D%x, D%y",      { m68k_roxr(i, Dy,  Dx & 0x3f); }),// gshift
  o("1110.ddd.1ss.110.ddd", "______x_____", "_XNZ0C", "------------", Any,  Dx_Dy,    None, "roxl%s  D%x, D%y",      { m68k_roxl(i, Dy,  Dx & 0x3f); }),// gshift
  o("1110.ddd.0ss.111.ddd", "_______x____", "__NZ0C", "------------", Any,  Dx_Dy,    None, "ror%s   D%x, D%y",      { m68k_ror(i,  Dy,  Dx & 0x3f); }),// gshift
  o("1110.ddd.1ss.111.ddd", "_______x____", "__NZ0C", "------------", Any,  Dx_Dy,    None, "rol%s   D%x, D%y",      { m68k_rol(i,  Dy,  Dx & 0x3f); }),// gshift

  o("1010.---.---.---.---", "____________", "______", "------------", None, None,     None, "1010",                  { m68k_emul1010(i.op); }),
  o("1111.---.---.---.---", "____________", "______", "------------", None, None,     None, "1111",                  { m68k_emul1111(i.op); }),

  o("0100.111.001.110.100", "____________", "______", "------------", Word, None,     None, "1:rtd", {}),
  o("0100.100.001.001.vvv", "____________", "______", "------------", None, None,     None, "1:bkpt", {}),
  o("0100.101.011.111.010", "____________", "______", "------------", None, None,     None, "?:bgnd", {}),
  o("0000.0ss.011.mmm.yyy", "__1__111111_", "______", "------------", Any2, None,     None, "2:chk2", {}),
  o("0000.111.0ss.mmm.yyy", "__1111111___", "______", "------------", Any,  None,     None, "1:moves", {}),
  o("0000.011.011.mmm.yyy", "11----------", "_XNZVC", "------------", None, None,     None, "?:rtm", {}),
  o("0000.011.011.mmm.yyy", "__1__111111_", "_XNZVC", "------------", None, None,     None, "?:callm", {}),
  o("0000.110.011.111.100", "------------", "_XNZVC", "------------", Word, None,     None, "?:cas2.w", {}),
  o("0000.111.011.111.100", "------------", "_XNZVC", "------------", Long, None,     None, "2:cas2.l", {}),
  o("1110.100.011.mmm.yyy", "1_1__111111_", "__NZ00", "------------", Word, None,     None, "2:bftst", {}),
  o("1110.101.011.mmm.yyy", "1_1__1111___", "__NZ00", "------------", Word, None,     None, "2:bfchg", {}),
  o("1110.110.011.mmm.yyy", "1_1__1111___", "__NZ00", "------------", Word, None,     None, "2:bfclr", {}),
  o("1110.111.011.mmm.yyy", "1_1__1111___", "__NZ00", "------------", Word, None,     None, "2:bfset", {}),
  o("1110.100.111.mmm.yyy", "1_1__111111_", "__NZ00", "------------", Word, None,     None, "2:bfextu", {}),
  o("1110.101.111.mmm.yyy", "1_1__111111_", "__NZ00", "------------", Word, None,     None, "2:bfexts", {}),
  o("1110.110.111.mmm.yyy", "1_1__111111_", "__NZ00", "------------", Word, None,     None, "2:bffo", {}),
  o("1110.111.111.mmm.yyy", "1_1__1111___", "__NZ00", "------------", Word, None,     None, "2:bfins", {}),
  { },
};
#undef o

void ckit(const char *s, int ew, int el, const char *lbl) {
  countTime(s, Word, ew, el, lbl);
  countTime(s, Long, ew, el, lbl);
}

const char *et[] = {
  "____", //dn
  "____", //an
  "___R", //(an)
  "___R", //(an)+
  "n__R", //-(an)
  "__pR", //(d16,an)
  "n_pR", //(xn,an)
  "__pR", //w
  "_ppR", //l
  "__pR", //(d16,pc)
  "n_pR", //(xn,pc)
  "__P_", //#imm
};

void testtiming() {
  // Effective address times
  ckit("____", 0x000, 0x000, "dn"); // dn/an
  ckit("___R", 0x410, 0x820, "(an)"); // (an)
  ckit("___R", 0x410, 0x820, "(an)+"); // (an)+
  ckit("n__R", 0x610, 0xa20, "-(an)"); // -(an)
  ckit("__pR", 0x820, 0xc30, "(an,16)"); // (an,16)
  ckit("n_pR", 0xa20, 0xe30, "(an,xn)"); // (an,xn)
  ckit("__pR", 0x820, 0xc30, "w"); // w
  ckit("_ppR", 0xc30, 0x1040,"l");// l
  ckit("__pR", 0x820, 0xc30, "(pc,16)"); // (pc,16)
  ckit("n_pR", 0xa20, 0xe30, "(pc,xn)"); // (pc,xn)
  ckit("__P_", 0x410, 0x820, "imm"); // imm

  // eori
  ckit("P.E.pz", 0x820, 0x1030, "eori.dn"); // dn
  ckit("P.E.pW", 0xc21, 0x1432, "eori.m");  // m
  
  // oriccr
  // 0000
  ckit("pNNpp", 0x1430, 0x1430, "oriccr");

  // cmpi
  ckit("P.E.pz", 0x820, 0xe30, "cmpi.dn");
  ckit("P.E.p", 0x820, 0x0c30, "cmpi.m");

  // movefromsr,nbcd
  ckit("E.pn", 0x610, 0x610, "movefrmsr.reg");
  ckit("E.pw", 0x811, 0x811, "movefmsr.mem");

  // movetoccr/sr
  ckit("E.NNp", 0xc10, 0xc10, "movetoccr/sr");

  // swap, ext, moveusp, nop
  ckit("p", 0x410, 0x410, "swap");

  // tst
  ckit("E.p", 0x410, 0x410, "tst");

  // tas
  ckit("E.p", 0x410, 0x410, "tas.reg");
  ckit("E.nwp", 0xa11,0xa11, "tas.mem");

  // unlk
  ckit("ppp", 0xc30,0xc30, "unlk");

  // stop
  ckit("nn", 0x400, 0x400, "stop");

  // rts
  ckit("uupp", 0x1040, 0x1040, "rts");
  ckit("uuupp", 0x1450, 0x1450, "rte");

  // rotate memory 1,ea
  ckit("E.rw", 0x811, 0x811, "rotate");
}

// ffff.xxx.-ss.mmm.yyy
void gentbl()
{
  opcode_t *ot;
  int nentry = 0;

  for (ot = optab; ot->mnem; ot++) {
    uint16_t mask = ot->opbit_mask;
    uint16_t val  = ot->opbit_val;

    printf("%.4x %.4x %.8x %.4x %.4x %s\n", mask, val, ot->arg0, ot->eabit_src, ot->eabit_dst, ot->mnem);
    for (int op = 0; op < 65536; op++) {
      if ((op & mask) != val)
	continue;

      /* Check opcode size */
      if (opsize(op, ot->size) == Any)
	continue;
      
      /* Check source EA */
      if (!eabit(op, 0x003F, ot->eabit_src)) {
	continue;
      }

      /* Check dest EA */
      if (!eabit(op, 0x0FC0, ot->eabit_dst)) {
	continue;
      }
      if (opmap[op]) {
	printf(" %.4x collide: %s,%s\n", op, ot->mnem, opmap[op]->mnem);
	exit(0);
      }
      else {
	opmap[op] = ot;
	nentry++;
      }
    }
  }
  printf("Table is %d entries\n", nentry);
  load_syms();
  // now try some decoders
}

static uint32_t sp_dec(int delta) {
  return predec(SP, delta);
}

static uint32_t sp_inc(int delta) {
  return postinc(SP, delta);
}

/*=======================================*
 * CPU Helper functions
 *=======================================*/

static const char *flagstr(const char *bits, int flags, ...) {
  va_list ap;
  static char fbits[32];

  strcpy(fbits, bits);
  va_start(ap, flags);
  for (int i = 0; bits[i]; i++) {
    auto bit = va_arg(ap, int);
    if (!(flags & (1L << bit))) {
      fbits[i] = '-';
    }
  }
  return fbits;
}
 
void cpu_showregs()
{
  int sr = cpu_getflags();
  
  printf("%.8x: %.4x %.4x [%s] ",
	 SPC, cpu_read16(SPC), sr,
	 flagstr("sxnzvc", sr, 13, 4, 3, 2, 1, 0));
  for (int i = 0; i < 8; i++) {
    printf("%.8x ", sregs[i]);
  };
  printf(" | ");
  for (int i = 0; i < 8; i++) {
    printf("%.6x ", sregs[i + 8]);
  }
  printf("\n");
}

uint32_t cpu_fetch(const int size, const char *lbl) {
  switch(size) {
  case Byte: return cpu_fetch16(PC) & 0xFF;
  case Word: return cpu_fetch16(PC);
  case Long: return cpu_fetch32(PC);
  default:
    Assert(0);
  }
}

/* Generic cpu read/write */
uint32_t cpu_read(uint32_t addr, int size) {
  m68k_check_address(addr, size, 0x15, "read");
  switch (size) {
  case Byte: TR(4,1,0); return cpu_read8(addr);
  case Word: TR(4,1,0); return cpu_read16(addr);
  case Long: TR(8,2,0); return cpu_read32(addr);
  default:
    Assert(0);
  }
}

void cpu_write(uint32_t addr, uint32_t val, int size) {
  m68k_check_address(addr, size, 0x15, "write");
  switch(size) {
  case Byte: TR(4,0,1); cpu_write8(addr, val); break;
  case Word: TR(4,0,1); cpu_write16(addr, val); break;
  case Long: TR(8,0,2); cpu_write32(addr, val); break;
  default:
    Assert(0);
  }
}

void cpu_push16(uint16_t v) {
  uint32_t addr = sp_dec(2);
  
  printf("push: %x %x\n", addr, v);
  cpu_write16(addr, v, dstk::STACK);
}

uint16_t cpu_pop16() {
  return cpu_read16(sp_inc(2), dstk::STACK);
}

void cpu_push32(uint32_t v) {
  uint32_t addr = sp_dec(4);
  
  printf("push: %x %x\n", addr, v);
  cpu_write32(addr, v, dstk::STACK);
}

uint32_t cpu_pop32() {
  uint32_t v = cpu_read32(sp_inc(4), dstk::STACK);
  return v;
}

void load_syms()
{
  char line[1024], name[1024];
  uint32_t fn;
  FILE *fp;

  /* Load chipreg names */
#define o(addr, rw, reg, name) rmap[addr] = #reg;
  CHIPREG(o);
#undef o

  /* Load symbol tablex */
  if ((fp = fopen("syms.txt", "r")) != NULL) {
    while (fgets(line, sizeof(line), fp) != NULL) {
      sscanf(line, "%x %s", &fn, name);
      fnmap[fn] = name;
    }
    fclose(fp);
  }
}

#define MV_LEA  0xf1c041c0
#define MV_JMP  0xffc04ec0
#define MV_BCC  0xf0006000
#define MV_DBCC 0xf0f850c8
#define MV_RTS  0xffff4e75
#define MV_RTE  0xffff4e73
#define MV_JSR  0xffc04e80

// 111.000 w
// 111.001 l
// 111.010 pc.16
// 111.011 pc.xn
// 111.100 imm
const int isimm(int eat)
{
  if (eat == 0x39)
    return 1;
  return 0;
}

const uint32_t mv(opcode_t* o) {
  return (o->opbit_mask << 16L) + o->opbit_val;
}

void dumpcfg(int off, int base, int size)
{
  dstk stk(size, printf);
  int nxt[2], op;

  trace = 2;
  stk.setbase(base);
  getextra(stk, base, size);
  stk.push(off, 1, dstk::PENDING, "first");
  while ((off = stk.pop()) != -1) {
    printf("\n------------------------------ %.8x [%s]\n",
	   off, fnname(off));
    PC = off;
    do {
      SPC = PC;
      op = cpu_fetch(Word);

      auto opc = opmap[op];
      if (opc == NULL) {
	printf("illlegal op trap : %.4x\n", op);
	break;
      }

      instr_t i = getargs(op, opc->size, opc->arg0);
      printf("  %.8x : %.4x %s\n", SPC,
	     op,  m68k_disasm(i, opc->mnem));
      stk.push(SPC, PC - SPC, dstk::CODE, "code");

      nxt[0] = PC;
      nxt[1] = -1;
      switch(mv(opc)) {
      case MV_LEA:
	leaval = i.src.value;
	break;
      case MV_JMP:
      case MV_JSR:
	/* Unconditional jump */
	nxt[0] = -1;
	if (isimm(i.src.type)) {
	  nxt[1] = SRCADDR;
	}
	break;
      case MV_DBCC:
	/* conditional jump */
	nxt[1] = IMM;
	break;
      case MV_BCC: 
	if ((op & 0xFF00) == 0x6000) {
	  /* unconditional jump */
	  nxt[0] = -1;
	  nxt[1] = IMM;
	}
	else {
	  /* conditional jump */
	  nxt[1] = IMM;
	}
	break;
      case MV_RTS:
      case MV_RTE:
	/* terminate */
	nxt[0] = -1;
	break;
      }
      /* Push next addresses */
      for (int n = 0; n < 2; n++) {
	stk.push(nxt[n], 1, dstk::PENDING, "nxt");
      }
    } while (nxt[0] != -1 && nxt[1] == -1);
    leaval = 0;
  }
  stk.showstk(128);
  exit(0);
}

void tt(int exp, int n, ...)
{
  int pc = 0x1000;
  va_list ap;
  uint32_t nn[6];

  va_start(ap, n);
  m68k_setpc(0x1000, false, "xx");
  for (int i = 0; i < n; i++) {
    nn[i] = va_arg(ap, int);
    cpu_write16(pc, nn[i]);
    pc += 2;
  }
  decode_68k(cpu_fetch(Word, "op"));
  if (ctick != exp) {
    printf("ticks: exp:%.6x got:%.6x\n", exp, ctick);
  }
}

void test_timing()
{
  trace = 1;
  tt(0x0, 1, 0b1101'000'000'000'001);
}
#endif
