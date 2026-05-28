#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>
#include <assert.h>
#include <array>
#include <string>
#include <functional>

struct dma {
  uint32_t src;
  uint32_t dst;
  uint32_t type;
  uint32_t len;
  uint32_t count = 0;
};

// HuC6280
enum {
  None = 0,
  IMP,
  ACC,
  IMM,
  ZPG,
  ZPX,
  ZPY,
  IND,
  IXX,
  IXY,
  ABS,
  ABX,
  ABY,
  REL,
  BLK,
  IMZP,
  IMZPX,
  IMAB,
  IMABX,

  // =========================================================
  // INVALID / NOP
  // =========================================================
  NOP,

  // =========================================================
  // LOAD / STORE
  // =========================================================
  LDA, LDX, LDY,
  STA, STX, STY, STZ,

  // =========================================================
  // TRANSFERS
  // =========================================================
  TAX, TAY, TXA, TYA,
  TSX, TXS,

  // =========================================================
  // STACK
  // =========================================================
  PHA, PLA, PHP, PLP,
  PHX, PLX, PHY, PLY,

  // =========================================================
  // ARITHMETIC
  // =========================================================
  ADC, SBC,

  // =========================================================
  // LOGIC
  // =========================================================
  AND, ORA, EOR,

  // =========================================================
  // COMPARE
  // =========================================================
  CMP, CPX, CPY,

  // =========================================================
  // INCREMENT / DECREMENT
  // =========================================================
  INC, DEC,
  INX, INY,
  DEX, DEY,

  // =========================================================
  // SHIFTS / ROTATES
  // =========================================================
  ASL, LSR, ROL, ROR,

  // =========================================================
  // BIT / TEST OPERATIONS
  // =========================================================
  BIT, TSB, TRB,

  // HuC6280 extended TST
  TST,

  // =========================================================
  // FLAGS
  // =========================================================
  CLC, SEC,
  CLI, SEI,
  CLV,
  CLD, SED,
  SET,
  CLA,
  CLX,
  CLY,

  // =========================================================
  // CONTROL FLOW
  // =========================================================
  JMP, JSR, RTS, RTI, BRK,

  BRA, BSR,
  BCC, BCS,
  BEQ, BNE,
  BMI, BPL,
  BVC, BVS,

  // =========================================================
  // BIT BRANCHES
  // =========================================================
  BBRi,
  BBSi,

  // =========================================================
  // MEMORY BIT SET/CLEAR
  // =========================================================
  RMBi,
  SMBi,

  // =========================================================
  // HU6280 SPECIAL REGISTERS / IO
  // =========================================================
  ST0, ST1, ST2,
  TMA, TAM,
  CSL, CSH,

  // =========================================================
  // BLOCK TRANSFERS
  // =========================================================
  TII, TDD, TIN, TIA, TAI,

  SXY, SAX, SAY,
};

struct CpuHu6280 {
  uint8_t  A,X,Y,S;
  uint16_t PC;

  bool Nf, Vf, Tf, Bf, Df, If, Zf, Cf;

  int speed;
  
  void exec(uint8_t op);
  void cpu_push8(uint8_t nv);
  void cpu_push16(uint16_t nv) {
    cpu_push8(nv >> 8);
    cpu_push8(nv);
  };
  uint8_t pop(uint8_t &r);

  uint8_t cpu_fetch8();
  uint16_t cpu_fetch16();

  void setflag(uint8_t p) {
    Nf = (p & 0x80) != 0;
    Vf = (p & 0x40) != 0;
    Tf = (p & 0x20) != 0;
    Bf = (p & 0x10) != 0;
    Df = (p & 0x08) != 0;
    If = (p & 0x04) != 0;
    Zf = (p & 0x02) != 0;
    Cf = (p & 0x01) != 0;
  };
  uint8_t getflag() const {
    uint8_t xp = 0;
    if (Nf) xp |= 0x80;
    if (Vf) xp |= 0x40;
    if (Tf) xp |= 0x20;
    if (Bf) xp |= 0x10;
    if (Df) xp |= 0x08;
    if (If) xp |= 0x04;
    if (Zf) xp |= 0x02;
    if (Cf) xp |= 0x01;
    return xp & 0xEF;
  };
  void sfstr(char *d, const char *s, ...) {
    va_list ap;
    va_start(ap, s);
    for (int i = 0; s[i]; i++) {
      bool bf = va_arg(ap, int);
      d[i] = bf ? s[i] : '-';
    }
  };
  std::string flagstr() {
    char sf[10] = {};
    sfstr(sf, "nvtbdizc", Nf, Vf, Tf, Bf, Df, If, Zf, Cf);
    return sf;
  };
};

using cpu = CpuHu6280;

constexpr auto vflag(auto a, auto b, auto r, auto m) {
  return ((((a ^ r) & (b ^ r)) & m) != 0);
}

// mpr regs
// r.rrrr.rrro.oooo.oooo.oooo
int mpr[8];

int lastmpr = 0;

int getmpr(uint8_t n) {
  printf("getmpr: %x\n", n);
  if (!n) {
    return lastmpr;
  };
  lastmpr = 0;
  for (int i = 0; i < 8; i++) {
    if (n & (1L << i)) {
      lastmpr |= (mpr[i] >> 13);
      printf("mpx: %x %x\n", i, lastmpr);
    }
  }
  return lastmpr;
};

static uint8_t ram[0x1000000];

static constexpr uint32_t mprit(uint16_t off) {
  int bank = (off >> 13);
  off &= 0x1fff;
  return mpr[bank] + off;
}

static uint8_t cpu_read8(uint16_t off) {
  auto moff = mprit(off);
  return ram[moff];
};
static uint16_t cpu_read16(uint16_t off) {
  printf("read16: %x %x\n", off, mpr[(off >> 13)] + (off & 0x1fff));
  uint16_t lo = cpu_read8(off);
  uint16_t hi = cpu_read8(off+1);
  return (hi << 8) + lo;
};

static void cpu_write8(uint16_t off, uint8_t n) {
  auto moff = mprit(off);
  ram[moff] = n;
}

bool dma_xfer(dma& d) {
  if (d.len > 0) {
    d.len--;
    auto v = cpu_read8(d.src);
    cpu_write8(d.dst, v);
    if (d.type == TAI) {
      d.dst++;
      d.src += (d.count & 1) ? -1 : 1;
    }
    else if (d.type == TIA) {
      d.dst += (d.count & 1) ? -1 : 1;
      d.src++;
    }
    else if (d.type == TII) {
      d.dst++;
      d.src++;
    }
    else if (d.type == TDD) {
      d.dst--;
      d.src--;
    }
    else if (d.type == TIN) {
      d.src++;
    }
    d.count++;
    return true;
  }
  return false;
}

uint8_t cpu::cpu_fetch8() {
  return cpu_read8(PC++);
};
uint16_t cpu::cpu_fetch16() {
  uint16_t lo = cpu_fetch8();
  uint16_t hi = cpu_fetch8();
  return (hi << 8) + lo;
};

uint8_t cpu::pop(uint8_t& r) {
  int addr = 0x2100 + ++S;
  r = cpu_read8(addr);
  return r;
}
void cpu::cpu_push8(uint8_t nv) {
  cpu_write8(0x2100 + S, nv);
  S--;
}

struct opcode {
  int fn, arg, cycs, flag;
  const char *mnem;
};

// p = (p & ~flag) | (flag >> 8)
// +--------+--------+--------+
// | special|   set  |  clear |
// +--------+--------+--------+
constexpr int sb(char ch, int mask) {
  if (ch == '0') {
    // clear bit
    return mask;
  }
  else if (ch == '1') {
    // set bit
    return mask << 8;
  }
  else if (ch != '_') {
    // special bit
  }
  return 0;
};

// https://dn721601.ca.archive.org/0/items/PCEDev/HuC6280%20-%20CMOS%208-bit%20Microprocessor%20Software%20Manual.pdf
// https://www.nxp.com/docs/en/reference-manual/MPC8260UM.pdf
// https://www.chrismcovell.com/PCEdev/HuC6280_opcodes.html
constexpr auto mktbl() {
  std::array<opcode, 256> tbl{};

  // p = (p & flag) | (flag >> 8)
  auto _m = [&](int id, int fn, int arg, const char *m, int cycs, const char *flags, const char *sfn, const char *sarg) {
    int flag = 0;
    if (flags[0] == 'N')
      flag |= 0x80;
    if (flags[1] == 'V')
      flag |= 0x40;
    if (flags[2] == '0')
      flag |= 0x20;
    if (flags[6] == 'Z')
      flag |= 0x02;
    if (flags[7] == 'C')
      flag |= 0x01;
    tbl[id] = { fn, arg, cycs, flag, m };
  };
#define mkop(id, fn, arg, cycs, flag) _m(id, fn, arg, #fn, cycs, flag, #fn, #arg)

  // ADC
  mkop(0x61, ADC, IXX, 7, "NV0___ZC");
  mkop(0x65, ADC, ZPG, 4, "NV0___ZC");
  mkop(0x69, ADC, IMM, 2, "NV0___ZC");
  mkop(0x6D, ADC, ABS, 5, "NV0___ZC");
  mkop(0x71, ADC, IXY, 7, "NV0___ZC");
  mkop(0x72, ADC, IND, 7, "NV0___ZC");
  mkop(0x75, ADC, ZPX, 4, "NV0___ZC");
  mkop(0x79, ADC, ABY, 5, "NV0___ZC");
  mkop(0x7D, ADC, ABX, 5, "NV0___ZC");

  // SBC
  mkop(0xE1, SBC, IXX, 7, "NV0___ZC");
  mkop(0xE5, SBC, ZPG, 4, "NV0___ZC");
  mkop(0xE9, SBC, IMM, 2, "NV0___ZC");
  mkop(0xED, SBC, ABS, 5, "NV0___ZC");
  mkop(0xF1, SBC, IXY, 7, "NV0___ZC");
  mkop(0xF2, SBC, IND, 7, "NV0___ZC");
  mkop(0xF5, SBC, ZPX, 4, "NV0___ZC");
  mkop(0xF9, SBC, ABY, 5, "NV0___ZC");
  mkop(0xFD, SBC, ABX, 5, "NV0___ZC");

  // AND / ORA / EOR
  mkop(0x21, AND, IXX, 7, "N_0___Z_");
  mkop(0x25, AND, ZPG, 4, "N_0___Z_");
  mkop(0x29, AND, IMM, 2, "N_0___Z_");
  mkop(0x2D, AND, ABS, 5, "N_0___Z_");
  mkop(0x31, AND, IXY, 7, "N_0___Z_");
  mkop(0x32, AND, IND, 7, "N_0___Z_");
  mkop(0x35, AND, ZPX, 4, "N_0___Z_");
  mkop(0x39, AND, ABY, 5, "N_0___Z_");
  mkop(0x3D, AND, ABX, 5, "N_0___Z_");

  mkop(0x01, ORA, IXX, 7, "N_0___Z_");
  mkop(0x05, ORA, ZPG, 4, "N_0___Z_");
  mkop(0x09, ORA, IMM, 2, "N_0___Z_");
  mkop(0x0D, ORA, ABS, 5, "N_0___Z_");
  mkop(0x11, ORA, IXY, 7, "N_0___Z_");
  mkop(0x12, ORA, IND, 7, "N_0___Z_");
  mkop(0x15, ORA, ZPX, 4, "N_0___Z_");
  mkop(0x19, ORA, ABY, 5, "N_0___Z_");
  mkop(0x1D, ORA, ABX, 5, "N_0___Z_");

  mkop(0x41, EOR, IXX, 7, "N_0___Z_");
  mkop(0x45, EOR, ZPG, 4, "N_0___Z_");
  mkop(0x49, EOR, IMM, 2, "N_0___Z_");
  mkop(0x4D, EOR, ABS, 5, "N_0___Z_");
  mkop(0x51, EOR, IXY, 7, "N_0___Z_");
  mkop(0x52, EOR, IND, 7, "N_0___Z_");
  mkop(0x55, EOR, ZPX, 4, "N_0___Z_");
  mkop(0x59, EOR, ABY, 5, "N_0___Z_");
  mkop(0x5D, EOR, ABX, 5, "N_0___Z_");

  // LDA
  mkop(0xA1, LDA, IXX, 7, "N_0___Z_");
  mkop(0xA5, LDA, ZPG, 4, "N_0___Z_");
  mkop(0xA9, LDA, IMM, 2, "N_0___Z_");
  mkop(0xAD, LDA, ABS, 5, "N_0___Z_");
  mkop(0xB1, LDA, IXY, 7, "N_0___Z_");
  mkop(0xB2, LDA, IND, 7, "N_0___Z_");
  mkop(0xB5, LDA, ZPX, 4, "N_0___Z_");
  mkop(0xB9, LDA, ABY, 5, "N_0___Z_");
  mkop(0xBD, LDA, ABX, 5, "N_0___Z_");

  // LDX / LDY
  mkop(0xA2, LDX, IMM, 2, "N_0___Z_");
  mkop(0xA6, LDX, ZPG, 4, "N_0___Z_");
  mkop(0xB6, LDX, ZPY, 4, "N_0___Z_");
  mkop(0xAE, LDX, ABS, 5, "N_0___Z_");
  mkop(0xBE, LDX, ABY, 5, "N_0___Z_");

  mkop(0xA0, LDY, IMM, 2, "N_0___Z_");
  mkop(0xA4, LDY, ZPG, 4, "N_0___Z_");
  mkop(0xB4, LDY, ZPX, 4, "N_0___Z_");
  mkop(0xAC, LDY, ABS, 5, "N_0___Z_");
  mkop(0xBC, LDY, ABX, 5, "N_0___Z_");
  // STA
  mkop(0x85, STA, ZPG, 4, "__0_____"); // zp
  mkop(0x95, STA, ZPX, 4, "__0_____"); // zp,x
  mkop(0x92, STA, IND, 7, "__0_____"); // (ind)
  mkop(0x81, STA, IXX, 7, "__0_____"); // (ind,x)
  mkop(0x91, STA, IXY, 7, "__0_____"); // (ind),y
  mkop(0x8D, STA, ABS, 5, "__0_____"); // abs
  mkop(0x99, STA, ABY, 5, "__0_____"); // abs,y
  mkop(0x9D, STA, ABX, 5, "__0_____"); // abs,x

  // STX / STY
  mkop(0x86, STX, ZPG, 4, "__0_____");
  mkop(0x96, STX, ZPY, 4, "__0_____");
  mkop(0x8E, STX, ABS, 5, "__0_____");

  mkop(0x84, STY, ZPG, 4, "__0_____");
  mkop(0x94, STY, ZPX, 4, "__0_____");
  mkop(0x8C, STY, ABS, 5, "__0_____");

  mkop(0x64, STZ, ZPG, 4, "__0_____");
  mkop(0x74, STZ, ZPX, 4, "__0_____");
  mkop(0x9C, STZ, ABS, 5, "__0_____");
  mkop(0x9E, STZ, ABX, 5, "__0_____");

  mkop(0x06, ASL, ZPG, 6, "N_0___ZC");
  mkop(0x16, ASL, ZPX, 6, "N_0___ZC");
  mkop(0x0E, ASL, ABS, 7, "N_0___ZC");
  mkop(0x1E, ASL, ABX, 7, "N_0___ZC");
  mkop(0x0A, ASL, ACC, 2, "N_0___ZC");
  
  mkop(0x46, LSR, ZPG, 6, "N_0___ZC");
  mkop(0x56, LSR, ZPX, 6, "N_0___ZC");
  mkop(0x4E, LSR, ABS, 7, "N_0___ZC");
  mkop(0x5E, LSR, ABX, 7, "N_0___ZC");
  mkop(0x4A, LSR, ACC, 2, "N_0___ZC");

  mkop(0x26, ROL, ZPG, 6, "N_0___ZC");
  mkop(0x36, ROL, ZPX, 6, "N_0___ZC");
  mkop(0x2E, ROL, ABS, 7, "N_0___ZC");
  mkop(0x3E, ROL, ABX, 7, "N_0___ZC");
  mkop(0x2A, ROL, ACC, 2, "N_0___ZC");

  mkop(0x66, ROR, ZPG, 6, "N_0___ZC");
  mkop(0x76, ROR, ZPX, 6, "N_0___ZC");
  mkop(0x6E, ROR, ABS, 7, "N_0___ZC");
  mkop(0x7E, ROR, ABX, 7, "N_0___ZC");
  mkop(0x6A, ROR, ACC, 2, "N_0___ZC");

  mkop(0xE6, INC, ZPG, 6, "N_0___Z_");
  mkop(0xF6, INC, ZPX, 6, "N_0___Z_");
  mkop(0xEE, INC, ABS, 7, "N_0___Z_");
  mkop(0xFE, INC, ABX, 7, "N_0___Z_");
  mkop(0x1A, INC, ACC, 2, "N_0___Z_");
  mkop(0xE8, INX, None, 2,"N_0___Z_");
  mkop(0xC8, INY, None, 2,"N_0___Z_");
  
  mkop(0xC6, DEC, ZPG, 6, "N_0___Z_");
  mkop(0xD6, DEC, ZPX, 6, "N_0___Z_");
  mkop(0xCE, DEC, ABS, 7, "N_0___Z_");
  mkop(0xDE, DEC, ABX, 7, "N_0___Z_");
  mkop(0x3A, DEC, ACC, 2, "N_0___Z_");
  mkop(0xCA, DEX, None, 2, "N_0___Z_");
  mkop(0x88, DEY, None, 2, "N_0___Z_");

  mkop(0xC9, CMP, IMM,  2, "N_0___ZC");
  mkop(0xC5, CMP, ZPG,  4, "N_0___ZC");
  mkop(0xD5, CMP, ZPX,  4, "N_0___ZC");
  mkop(0xD2, CMP, IND,  7, "N_0___ZC");
  mkop(0xC1, CMP, IXX,  7, "N_0___ZC");
  mkop(0xD1, CMP, IXY,  7, "N_0___ZC");
  mkop(0xCD, CMP, ABS,  5, "N_0___ZC");
  mkop(0xDD, CMP, ABX,  5, "N_0___ZC");
  mkop(0xD9, CMP, ABY,  5, "N_0___ZC");

  mkop(0xE0, CPX, IMM,  2, "N_0___ZC");
  mkop(0xE4, CPX, ZPG,  4, "N_0___ZC");
  mkop(0xEC, CPX, ABS,  5, "N_0___ZC");

  mkop(0xC0, CPY, IMM,  2, "N_0___ZC");
  mkop(0xC4, CPY, ZPG,  4, "N_0___ZC");
  mkop(0xCC, CPY, ABS,  5, "N_0___ZC");

  mkop(0x83, TST, ZPG,  7, "__0_____");
  mkop(0xA3, TST, ZPX,  7, "__0_____");
  mkop(0x93, TST, ABS,  8, "__0_____");
  mkop(0xB3, TST, ABX,  8, "__0_____");
  
  mkop(0x00, BRK, None, 8, "__0_____");
  mkop(0x20, JSR, ABS, 7,  "__0_____");
  mkop(0x40, RTI, None, 7, "________");
  mkop(0x60, RTS, None, 7, "__0_____");
  mkop(0x80, BRA, REL, 4,  "__0_____");
  mkop(0x90, BCC, REL, 2,  "__0_____");
  mkop(0xB0, BCS, REL, 2,  "__0_____");
  mkop(0xF0, BEQ, REL, 2,  "__0_____");
  mkop(0xD0, BNE, REL, 2,  "__0_____");
  mkop(0x30, BMI, REL, 2,  "__0_____");
  mkop(0x10, BPL, REL, 2,  "__0_____");
  mkop(0x50, BVC, REL, 2,  "__0_____");
  mkop(0x70, BVS, REL, 2,  "__0_____");
  mkop(0x44, BSR, REL, 8,  "__0_____");
  mkop(0x4C, JMP, ABS, 4,  "__0_____"); // ABS
  mkop(0x6C, JMP, ABS, 7,  "__0_____"); // (ABS)
  mkop(0x7C, JMP, ABX, 7,  "__0_____"); // (ABS,X)

  mkop(0x62, CLA, None, 2, "__0_____");
  mkop(0x82, CLX, None, 2, "__0_____");
  mkop(0xC2, CLY, None, 2, "__0_____");
  mkop(0x18, CLC, None, 2, "__0_____");
  mkop(0x38, SEC, None, 2, "__0_____");
  mkop(0x58, CLI, None, 2, "__0_____");
  mkop(0x78, SEI, None, 2, "__0_____");
  mkop(0xB8, CLV, None, 2, "__0_____");
  mkop(0xD8, CLD, None, 2, "__0_____");
  mkop(0xF8, SED, None, 2, "__0_____");

  mkop(0x48, PHA, None, 3, "__0_____");
  mkop(0x68, PLA, None, 4, "N_0___Z_");
  mkop(0x08, PHP, None, 3, "__0_____");
  mkop(0x28, PLP, None, 4, "________");

  mkop(0x5A, PHY, None, 3, "__0_____");
  mkop(0x7A, PLY, None, 4, "N_0___Z_");
  mkop(0xDA, PHX, None, 3, "__0_____");
  mkop(0xFA, PLX, None, 4, "N_0___Z_");

  mkop(0xAA, TAX, None, 2, "N_0___Z_");
  mkop(0xA8, TAY, None, 2, "N_0___Z_");
  mkop(0xBA, TSX, None, 2, "N_0___Z_");
  mkop(0x8A, TXA, None, 2, "N_0___Z_");
  mkop(0x9A, TXS, None, 2, "__0_____");
  mkop(0x98, TYA, None, 2, "N_0___Z_");

  mkop(0x03, ST0, IMM, 4, "__0_____");
  mkop(0x13, ST1, IMM, 4, "__0_____");
  mkop(0x23, ST2, IMM, 4, "__0_____");

  mkop(0x43, TMA, IMM, 4, "__0_____");
  mkop(0x53, TAM, IMM, 5, "__0_____");
  mkop(0x54, CSL, None, 2, "__0_____");
  mkop(0xD4, CSH, None, 2, "__0_____");

  mkop(0x73, TII, BLK, 17, "__0_____");
  mkop(0xC3, TDD, BLK, 17, "__0_____");
  mkop(0xD3, TIN, BLK, 17, "__0_____");
  mkop(0xE3, TIA, BLK, 17, "__0_____");
  mkop(0xF3, TAI, BLK, 17, "__0_____");

  mkop(0x89, BIT, IMM, 2, "__0_____");
  mkop(0x24, BIT, ZPG, 4, "__0_____");
  mkop(0x34, BIT, ZPX, 4, "__0_____");
  mkop(0x2C, BIT, ABS, 5, "__0_____");
  mkop(0x3C, BIT, ABX, 5, "__0_____");

  mkop(0x87, SMBi, ZPG, 7, "__0_____");
  mkop(0x97, SMBi, ZPG, 7, "__0_____");
  mkop(0xA7, SMBi, ZPG, 7, "__0_____");
  mkop(0xB7, SMBi, ZPG, 7, "__0_____");
  mkop(0xC7, SMBi, ZPG, 7, "__0_____");
  mkop(0xD7, SMBi, ZPG, 7, "__0_____");
  mkop(0xE7, SMBi, ZPG, 7, "__0_____");
  mkop(0xF7, SMBi, ZPG, 7, "__0_____");

  mkop(0x07, RMBi, ZPG, 7, "__0_____");
  mkop(0x17, RMBi, ZPG, 7, "__0_____");
  mkop(0x27, RMBi, ZPG, 7, "__0_____");
  mkop(0x37, RMBi, ZPG, 7, "__0_____");
  mkop(0x47, RMBi, ZPG, 7, "__0_____");
  mkop(0x57, RMBi, ZPG, 7, "__0_____");
  mkop(0x67, RMBi, ZPG, 7, "__0_____");
  mkop(0x77, RMBi, ZPG, 7, "__0_____");

  mkop(0x22, SAX,  None, 3, "__0_____");
  mkop(0x02, SXY,  None, 3, "__0_____");
  mkop(0x42, SAY,  None, 3, "__0_____");
  
  mkop(0x04, TSB,  ZPG,  6, "__0_____");
  mkop(0x0c, TSB,  ABS,  7, "__0_____");
  mkop(0x14, TRB,  ZPG,  6, "__0_____");
  mkop(0x1C, TRB,  ABS,  7, "__0_____");

  mkop(0x0f, BBRi, ZPG,  6, "__0_____");
  mkop(0x1f, BBRi, ZPG,  6, "__0_____");
  mkop(0x2f, BBRi, ZPG,  6, "__0_____");
  mkop(0x3f, BBRi, ZPG,  6, "__0_____");
  mkop(0x4f, BBRi, ZPG,  6, "__0_____");
  mkop(0x5f, BBRi, ZPG,  6, "__0_____");
  mkop(0x6f, BBRi, ZPG,  6, "__0_____");
  mkop(0x7f, BBRi, ZPG,  6, "__0_____");

  mkop(0x8f, BBSi, ZPG,  6, "__0_____");
  mkop(0x9f, BBSi, ZPG,  6, "__0_____");
  mkop(0xaf, BBSi, ZPG,  6, "__0_____");
  mkop(0xbf, BBSi, ZPG,  6, "__0_____");
  mkop(0xcf, BBSi, ZPG,  6, "__0_____");
  mkop(0xdf, BBSi, ZPG,  6, "__0_____");
  mkop(0xef, BBSi, ZPG,  6, "__0_____");
  mkop(0xff, BBSi, ZPG,  6, "__0_____");

  mkop(0xEA, NOP, None, 2, "__0_____");
  mkop(0xF4, SET, None, 2, "________");
  
  return tbl;
};

auto optbl = mktbl();

// zpg starts at 0x2000?
constexpr int zpg(cpu&c, int off = 0) {
  return ((cpu_read8(c.PC++) + off) & 0xFF) + 0x2000;
}

constexpr int abs(int addr, int off, bool mask = false) {
  uint16_t lo, hi;
  
  lo = cpu_read8(addr);
  if ((addr & 0xff) == 0xff && mask) {
    // page wrap at 0xff
    hi = cpu_read8(addr & 0xff00);
  }
  else {
    hi = cpu_read8(addr+1);
  }
  return (hi << 8) + lo + off;
}

const char *mode(int n) {
  switch (n) {
  case IMM: return "imm";
  case ZPG: return "zpg";
  case ZPX: return "zpx";
  case ZPY: return "zpy";
  case IND: return "ind";
  case IXX: return "ixx";
  case IXY: return "ixy";
  case ABS: return "abs";
  case ABX: return "abx";
  case ABY: return "aby";
  }
  return "xxx";
};

constexpr int opaddr(cpu& c, uint8_t op) {
  int arg = optbl[op].arg;
  int addr = 0;
  
  switch (arg) {
  case IMM: return c.PC++;
  case ZPG: return zpg(c,0);
  case ZPX: return zpg(c,c.X);
  case ZPY: return zpg(c,c.Y);
  case IND: return abs(zpg(c, 0), 0, true);
  case IXX: return abs(zpg(c,c.X), 0, true);
  case IXY: return abs(zpg(c,0), c.Y, true);
  case ABS: addr = c.PC; c.PC += 2; return abs(addr, 0);
  case ABX: addr = c.PC; c.PC += 2; return abs(addr, c.X);
  case ABY: addr = c.PC; c.PC += 2; return abs(addr, c.Y);
  }
  return -1;
};

constexpr auto op_ld = [](uint8_t& dst, uint8_t src) {
  dst = src;
  return dst;
};

int handled[256];
void cpu::exec(uint8_t op) {
  int fn, arg, flag;
  int addr = -1;
  uint8_t res = 0;
  int setres = 0;

  auto set_a = [&](uint8_t nv) {
    A = nv;
  };
  auto set_wr = [&](uint8_t nv) {
    cpu_write8(addr, nv);
  };
  auto set_t = [&](uint8_t nv) {
    cpu_write8(0x2000 + X, nv);
  };
  
  flag = optbl[op].flag;
  fn = optbl[op].fn;
  arg = optbl[op].arg;
  auto seta = [&](uint8_t v) {
    if (arg == ACC)
      A = v;
    else
      cpu_write8(addr, v);
    return v;
  };
  auto op_bcc = [&](bool cond, uint8_t nv) {
    if (cond)
      PC += (int8_t)nv;
  };
  auto op_shl = [&](uint8_t v, int lsb) {
    Cf = (v & 0x80) != 0;
    return seta((v << 1) | lsb);
  };
  auto op_shr = [&](uint8_t v, int msb) {
    Cf = (v & 0x01) != 0;
    return seta((v >> 1) | msb);
  };
  auto op_cmp = [&](uint8_t a, uint8_t b) {
    Cf = (a >= b);
    return a - b;
  };
  auto op_bbsi = [&](uint8_t m, uint8_t bit) {
    int8_t rel = cpu_read8(PC++);
    
    bit = (1L << bit);
    if (m & bit) {
      PC += rel;
    };
  };
  auto op_rmbi = [&](uint8_t m, uint8_t bit) {
    bit = (1L << bit);
    cpu_write8(addr, m & ~bit);
  };
  auto op_smbi = [&](uint8_t m, uint8_t bit) {
    printf("smbi: %x %x %x\n", addr, m, bit);
    bit = (1L << bit);
    cpu_write8(addr, m | bit);
  };
  auto op_adc = [&](uint8_t op1, uint8_t src) {
    uint16_t sum = op1 + src + Cf;
    if (Df) {
      sum = (op1 & 0xF) + (src & 0xF) + Cf;
      if (sum > 0x9) {
	sum = ((sum + 6) & 0xF) + 0x10;
      }
      sum += (op1 & 0xF0) + (src & 0xF0);
      printf("sum2: %x\n", sum);
      if (sum > 0x9f) {
	sum += 0x60;
      }
    }
    else {
      Vf = vflag(op1, src, sum, 0x80);
    }
    Cf = sum > 0xff;
    return sum;
  };
  auto op_sbc = [&](uint8_t op1, uint8_t src) {
    uint8_t isrc = ~src;
    uint16_t sum = op1 + isrc + Cf;
    
    if (Df) {
      sum = (op1 & 0xF) + (isrc & 0xF) + Cf;
      if (sum <= 0xF)
	sum -= 0x6;
      sum = (op1 & 0xF0) + (isrc & 0xF0) + (sum > 0xF ? 0x10 : 0) + (sum & 0xF);
      if (sum <= 0xff)
	sum -= 0x60;
    } else {
      Vf = (~(op1 ^ isrc) & (op1 ^ sum) & 0x80) != 0;
    }
    Cf = sum > 0xFF;
    printf("sbc = %x\n", sum);
    return sum;
  };
  auto op_st = [&](uint8_t nv) {
    setres = 'r';
    return nv;
  };
  auto op_plp = [&]() {
    uint8_t p;
    setflag(pop(p));
  };
  auto op_php = [&]() {
    uint8_t p = getflag() & ~0x20;
    cpu_push8(p | 0x10);
  };
  auto op_rts = [&]() {
    uint8_t hi, lo;
    pop(lo);
    pop(hi);
    PC = ((hi << 8) + lo) + 1;
  };
  auto op_rti = [&]() {
    op_plp();
    op_rts();
    PC--;
  };
  auto op_blk = [&](int fn) {
    dma d{};

    d.type = fn;
    d.src = cpu_fetch16();
    d.dst = cpu_fetch16();
    d.len = cpu_fetch16();
    if (!d.len)
      d.len = 0x10000;
    cpu_push8(Y);
    cpu_push8(A);
    cpu_push8(X);
    printf("dma_xfer: %.4x[%.8x] %.4x[%.8x] [%.8x] %.4x : %.2x\n",
	   d.src, mprit(d.src),
	   d.dst, mprit(d.dst),
	   mprit(0x2100 + S),
	   d.len, d.type);
    while (dma_xfer(d)) {
    };
    pop(X);
    pop(A);
    pop(Y);
  };
  auto op_inc = [&](uint8_t& dst) {
    dst++;
    if (arg == ACC)
      A = dst;
    else if (fn == INC)
      setres = 'r';
    return dst;
  };
  auto op_dec = [&](uint8_t& dst) {
    dst--;
    if (arg == ACC)
      A = dst;
    else if (fn == DEC)
      setres = 'r';
    return dst;
  };
  auto op_bit = [&](uint8_t a, uint8_t b) {
    Zf = (a & b) == 0;
    Nf = (b & 0x80) != 0;
    Vf = (b & 0x40) != 0;
  };
  // get op1 value
  int op1 = A;
  if (fn == TST)
    op1 = cpu_fetch8();
  else if (fn == SBC || fn == ADC || fn == ORA || fn == EOR || fn == AND) {
    if (Tf) {
      op1 = cpu_read8(X + 0x2000);
      setres = 't';
    }
    else {
      setres = 'a';
    }
  }
  // get op2 value
  if (arg == ACC)
    res = A;
  else if (arg == REL)
    res = (int8_t)cpu_fetch8();
  else if ((addr = opaddr(*this, op)) != -1) {
    if (fn != JMP) {
      res = cpu_read8(addr);
    }
  }
  // switch on function
  const int bit = ((op >> 4) & 7);
  printf("--- pexec: [%s] %x a:%.2x x:%.2x y:%.2x s:%.2x pc:%.4x addr:%x [%.2x] %s\n",
	 flagstr().c_str(),
	 op, A, X, Y, S, PC, addr, res, optbl[op].mnem);
  switch (fn) {
  case BBSi: op_bbsi(res, bit); break;
  case BBRi: op_bbsi(~res,bit); break;
  case RMBi: op_rmbi(res, bit); break;
  case SMBi: op_smbi(res, bit); break;
  case INC: res=op_inc(res); break;
  case INX: res=op_inc(X); break;
  case INY: res=op_inc(Y); break;
  case DEC: res=op_dec(res); break;
  case DEX: res=op_dec(X); break;
  case DEY: res=op_dec(Y); break;
  case TXS: res=op_ld(S, X); break;
  case TAX: res=op_ld(X, A); break;
  case TXA: res=op_ld(A, X); break;
  case TAY: res=op_ld(Y, A); break;
  case TYA: res=op_ld(A, Y); break;
  case TSX: res=op_ld(X, S); break;
  case ASL: res=op_shl(res, 0); break;
  case LSR: res=op_shr(res, 0); break;
  case ROL: res=op_shl(res, Cf ? 0x01 : 0x0); break;
  case ROR: res=op_shr(res, Cf ? 0x80 : 0x0); break;
  case SET: Tf=1; break;
  case SEC: case CLC: Cf = (fn == SEC); break;
  case SED: case CLD: Df = (fn == SED); break;
  case SEI: case CLI: If = (fn == SEI); break;
  case CLV: Vf = 0; break;
  case CSH: case CSL: speed = (fn == CSH); break;
  case BNE: op_bcc(!Zf, res); break;
  case BEQ: op_bcc(Zf, res); break;
  case BCC: op_bcc(!Cf, res); break;
  case BCS: op_bcc(Cf, res); break;
  case BVC: op_bcc(!Vf, res); break;
  case BVS: op_bcc(Vf, res); break;
  case BPL: op_bcc(!Nf, res); break;
  case BMI: op_bcc(Nf, res); break;
  case BRA: op_bcc(true, res); break;
  case CMP: res = op_cmp(A, res); break;
  case CPX: res = op_cmp(X, res); break;
  case CPY: res = op_cmp(Y, res); break;
  case ORA: res = op1 | res; break;
  case EOR: res = op1 ^ res; break;
  case AND: res = op1 & res; break;
  case ADC: res = op_adc(op1, res); break;
  case SBC: res = op_sbc(op1, res); break;
  case LDA: res = op_ld(A, res); break;
  case LDX: res = op_ld(X, res); break;
  case LDY: res = op_ld(Y, res); break;
  case CLA: res = op_ld(A, 0); break;
  case CLX: res = op_ld(X, 0); break;
  case CLY: res = op_ld(Y, 0); break;
  case STA: res = op_st(A); break;
  case STX: res = op_st(X); break;
  case STY: res = op_st(Y); break;
  case STZ: res = op_st(0); break;
  case PHA: cpu_push8(A); break;
  case PHX: cpu_push8(X); break;
  case PHY: cpu_push8(Y); break;
  case PLA: res = pop(A); break;
  case PLX: res = pop(X); break;
  case PLY: res = pop(Y); break;
  case SXY: std::swap(X, Y); break;
  case SAX: std::swap(A, X); break;
  case SAY: std::swap(A, Y); break;
  case RTS: op_rts();  break;
  case RTI: op_rti();  break;
  case BRK:
    Bf = true;
    cpu_push16(PC + 1);
    op_php();
    PC = cpu_read16(0xfff6);
    Df = false;
    If = true;
    break;
  case BSR:
    cpu_push16(PC - 1);
    op_bcc(true, res);
    break;
  case JMP:
    if (op != 0x4c)
      addr = abs(addr, 0);
    PC = addr;
    break;
  case JSR:
    cpu_push16(PC - 1);
    PC = addr;
    break;
  case TSB:
    op_bit(A, res);
    cpu_write8(addr, A | res);
    break;
  case TRB:
    op_bit(A, res);
    cpu_write8(addr, ~A & res);
    break;
  case BIT: op_bit(A, res); break;
  case TST: op_bit(op1, res); break;
  case PHP: op_php(); break;
  case PLP: op_plp(); break;
  case TAI: case TDD: case TII: case TIA: case TIN:
    op_blk(fn);
    break;
  case TMA:
    A = getmpr(res);
    break;
  case TAM:
    printf("tam: %d\n", res);
    break;
  case NOP: break;
  default:
    handled[op] |= 0x1;
    break;
  };
  switch (setres) {
  case 't': cpu_write8(0x2000 + X, res); break;
  case 'r': cpu_write8(addr, res); break;
  case 'a': A = res; break;
  }
  // zn flags
  // N_____Z_
  if ((flag & 0x82) == 0x82) {
    Zf = (res == 0);
    Nf = (res & 0x80) != 0;
  }
  // t flag
  // __0_____
  if ((flag & 0x20) != 0) {
    Tf = 0;
  }
  printf("--- exec: %x a:%.2x x:%.2x y:%.2x s:%.2x pc:%.4x\n", op, A, X, Y, S, PC);
}

void readtsv(const char *file) {
  FILE *fp;
  char *saveptr;;
  char *tag, *key, *val;
  char  line[256];
  uint32_t k, v, ki, vi;
  char *ch;
  cpu c{};
  int errors = 0;
  int op = -1;
  
  int rid = 0;
  if ((fp = fopen(file, "r")) == NULL) {
    return;
  }
  auto ck = [&](bool init, auto&a, auto b, const char *k, const char *lbl) {
    if (strcmp(k, lbl))
      return;
    if (init) {
      a = b;
    }
    else if (a != b) {
      errors++;
      printf("mismatch: %s got:%x expect:%x\n", lbl, a, b);
    }
  };
  while (fgets(line, sizeof(line), fp) != NULL) {
    if ((ch = strchr(line, '\n')) != NULL)
      *ch = 0;
    tag = strtok_r(line, "\t", &saveptr);
    key = strtok_r(NULL, "\t", &saveptr);
    val = strtok_r(NULL, "\t", &saveptr);

    if (key)
      ki = strtoull(key, NULL, 0);
    if (val)
      vi = strtoull(val, NULL, 0);
    if (!strcmp(tag, "ROW")) {
      printf("== row: %s\n", key);
      c.A = c.X = c.Y = c.S = c.PC = 0;
      memset(ram, 0, sizeof(ram));
    }
    else if (!strcmp(tag, "IR")) {
      ck(true, c.A, vi, key, "A");
      ck(true, c.X, vi, key, "X");
      ck(true, c.Y, vi, key, "Y");
      ck(true, c.S, vi, key, "S");
      ck(true, c.PC,vi, key, "PC");
      if (!strcmp(key, "P"))  {
	printf("set flags: %x\n", vi);
	c.setflag(vi);
      }
    }
    else if (!strcmp(tag, "IM")) {
      printf("ram: %x %x\n", ki, vi);
      ram[ki] = vi;
      ck(true, ram[ki], vi,"ram", "ram");
    }
    else if (!strcmp(tag, "MPR")) {
      printf("mpr%d = %.2x %.8x\n", ki, vi, vi << 13);
      mpr[ki] = vi << 13;
    }
    else if (!strcmp(tag, "EXEC")) {
      op = c.cpu_fetch8();
      c.exec(op);
    }
    else if (!strcmp(tag, "FR")) {
      auto p = c.getflag();
      ck(false, c.A, vi, key, "A");
      ck(false, c.X, vi, key, "X");
      ck(false, c.Y, vi, key, "Y");
      ck(false, c.S, vi, key, "S");
      ck(false, c.PC,vi, key, "PC");
      ck(false, p, vi, key, "P");
    }
    else if (!strcmp(tag, "FM")) {
      auto p = ram[ki];
      printf("expect: %.8x got:%x exp:%x\n", ki, p, vi);
      ck(false, p, vi, "ram", "ram");
    }
  }
  if (!errors) {
    handled[op] = 0xf;
    printf("%.2x:%s good\n", op, optbl[op].mnem);
  }
}

int main(int argc, char *argv[]) {
  cpu c{};

  printf("argc is %x\n", argc);
  for (int i = 1; i < argc; i++) {
    printf("%s\n", argv[i]);
    readtsv(argv[i]);
  }
  for (int i = 0; i < 256; i++) {
    printf("%x:%6s ", handled[i], handled[i] == 0x0f ? "___" : optbl[i].mnem);
    if ((i & 0xF) == 0xF)
      printf("\n");
  }
}
