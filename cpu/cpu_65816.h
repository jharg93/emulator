/* Implement 65816 CPU for SNES */
extern int trace;

uint32_t cpu_read24(uint32_t addr);

const char *cpu_dis(const uint8_t *ib);

enum {
  Byte = true,
  Word = false,
  
  SMASK = 0xFFF,
  DMASK = 0xFFF,

  IMP = 0x0,
  IMM = 0x1,
      
  RA  = 0x10,
  DPG = 0x20, DPX, DPY, DPS,
  ABS = 0x30, ABX, ABY, ABL, ABLX,
  IXZ = 0x40, IXX, IXY,
  ILZ = 0x50, ILY, ISY,

  PCREL8 = 0x60,   // PC + nn
  PCREL16,         // PC + nnnn
  PCIND,           // [nnnn]   jmp
  PCABS,           // nnnn     jmp,jsr
  PCABSL,          // nnnnnn   jmp
  PCINDX,          // nnnn + X jmp
  PCINDL,          // [[nnnn].24] jml
  
  /* Clocks/Flag */
  STORE = 0x8000,
  DI = 0x2000,
  CROSS = 0x4000,
  BRANCH = 0x1000,
  
  /* Increment if m == 0 */
  M1 = 0x10,
  M2 = 0x20,

  /* Increment if x == 0 */
  X1 = 0x100,
  
  /* Increment if e == 0 */
  E1 = 0x00,

  C7 = 0x00,

  NOFLAG = -1,
};

typedef uint32_t arg_t;

/* Opcode Handlers */
#define SIZE_WORD 0x40000000  // size = word
#define SIZE_BYTE 0x00000000  // size = byte
#define TYPE_IMM  0x80000000  // immediate value
#define TYPE_ACC  0x20000000  // accumulator
#define TYPE_RES  0x10000000  // value holds result
#define TYPE_MASK 0xFF000000

#define MODE_6502   0x01
#define MODE_NATIVE 0x00

/* CPU Registers */
static uint16_t _A, _X, _Y, _S, PC, SPC;

static uint16_t _D;  // Direct Register  (PHD)
static uint32_t PBR; // ProgramBank      (PHK)
static uint32_t DBR; // Data Bank        (PHB) rr

static bool flags[8];
static bool& fC = flags[0];
static bool& fZ = flags[1];
static bool& fI = flags[2];
static bool& fD = flags[3];
static bool& fX = flags[4];
static bool& fM = flags[5];
static bool& fV = flags[6];
static bool& fN = flags[7];
static bool& fB = fX;
static bool& fK = fM;
static bool fE = true;

const uint32_t szmask[] = {
  0x000000FF,
  0x0000FFFF,
  0xFFFFFFFF
};
const uint32_t sfmask[] = {
  0x00000080,
  0x00008000,
  0x80000000
};

/*==============================================
 * Helper functions
 *==============================================*/
const uint32_t zmask[] = { 0xFFFF, 0xFF };
const uint32_t nmask[] = { 0x8000, 0x80 };

/* Set New PC: --------.PPPPPPPP.oooooooo.oooooooo */
static uint32_t setpc(bool test, uint32_t npc)
{
  if (test) {
    /* Set bank and PC */
    PBR = npc & 0xFF0000;
    PC  = npc & 0x00FFFF;
  }
  return NOFLAG;
}

static uint16_t setzf(const uint16_t src, const bool _8bit) {
  fZ = (src & zmask[_8bit]) == 0;
  return src;
}

static uint16_t setnf(const uint16_t src, const bool _8bit) {
  fN = (src & nmask[_8bit]) != 0;
  return src;
}

static uint16_t setnz(const uint16_t src, const bool _8bit) {
  setzf(src, _8bit);
  setnf(src, _8bit);
  return src;
}

static uint16_t nzv(uint16_t src, const bool _8bit) {
  setnz(src, _8bit);
  return src;
}

/* Set value result based on X/M flags */
static uint32_t storev(arg_t &dst, const uint16_t src, const bool _8bit = true) {
  if (_8bit) {
    dst = (src & 0xFF) | TYPE_RES;
  }
  else {
    dst = src | TYPE_RES | SIZE_WORD;
  }
  return src;
}

/* Set register based on X/M flags */
static void setvnz(uint16_t& dst, const uint16_t src, const bool _8bit) {
  const uint16_t mask = zmask[_8bit];

  dst = (dst & ~mask) | (src & mask);
  setnz(src, _8bit);
}

/* Set destination don't set flags */
static uint32_t setv(uint16_t& dst, const uint16_t src, const bool _8bit) {
  const uint16_t mask = zmask[_8bit];

  dst = (dst & ~mask) | (src & mask);
  return src;
}

/* Set destination to A flag or result */
static void setva(arg_t& src, uint16_t val, const bool _8bit) {
  if (src & TYPE_ACC)
    setvnz(_A, val, _8bit);
  else {
    setvnz(val, val, _8bit);
    src = ((src | TYPE_RES) & TYPE_MASK) | val;
  }
}

/* Store cpu flags as array reference */
uint8_t cpu_getflags() {
  uint8_t p = 0;

  for (int i = 0; i < 8; i++) {
    if (flags[i]) {
      p |= (1L << i);
    }
  }
  return p;
}

void cpu_setflags(uint8_t mask) {
  for (int i = 0; i < 8; i++) {
    flags[i] = mask & 1;
    mask >>= 1;
  }
}

const char *cpu_flagstr() {
  static char fb[12];
  snprintf(fb, sizeof(fb), "%c[%c%c%c%c%c%c%c%c]",
	   "-E"[!!fE],
	   "-n"[!!fN],
	   "-v"[!!fV],
	   "-m"[!!fM],
	   "-x"[!!fX],
	   "-d"[!!fD],
	   "-i"[!!fI],
	   "-z"[!!fZ],
	   "-c"[!!fC]);
  return fb;
}

void cpu_showregs()
{
  printf("%.2x%.4x A:%.4x X:%.4x Y:%.4x S:%.4x D:%.4x DB:%.2x %s  ",
	 PBR>>16, SPC, _A, _X, _Y, _S, _D, DBR, cpu_flagstr());
}

/*==============================================*
 * CPU memory read functions
 *==============================================*/
uint8_t cpu_fetch8() {
  uint8_t v = cpu_read8(PBR + PC);
  PC += 1;
  return v;
}

uint16_t cpu_fetch16() {
  uint16_t v = cpu_read16(PBR + PC);
  PC += 2;
  return v;
}

uint32_t cpu_fetch24() {
  uint32_t v = cpu_read24(PBR + PC);
  PC += 3;
  return v;
}

void cpu_push8(uint8_t v) {
  cpu_write8(_S, v, dstk::STACK);
  setv(_S, _S - 1, fE == MODE_6502);
}

uint8_t cpu_pop8() {
  setv(_S, _S + 1, fE == MODE_6502);
  return cpu_read8(_S, dstk::STACK);
}

/* CPU Vectors:
 *  native:
 *   00:FFE0 unused
 *   00:FFE2 unused
 *   00:FFE4 COP
 *   00:FFE6 BRK
 *   00:FFE8 ABORT
 *   00:FFEA NMI
 *   00:FFEC unused
 *   00:FFEE IRQ Vector  HSYNC, VSYNC or Timer
 *  emulation
 *   00:FFF0 unused
 *   00:FFF2 unused
 *   00:FFF4 COP
 *   00:FFF6 unused
 *   00:FFF8 ABORT
 *   00:FFFA NMI
 *   00:FFFC RESET
 *   00:FFFE IRQ/BRK
 */
bool cpu_irq(int n)
{
  return false;
}

void cpu_reset(unsigned int x) {
  fE = MODE_6502;
  PBR = 0;
  DBR = 0;
  _S = 0x1ff;
  PC = cpu_read16(PBR + 0xFFFC);
}

/* Compare values */
const uint32_t cmpv(const uint16_t dst, const uint16_t src, const bool _8bit = true) {
  if (_8bit) {
    fC = (uint8_t)dst >= (uint8_t)src;
  }
  else {
    fC = dst >= src;
  }
  return dst - src;
}

static uint16_t popv(const bool _8bit = true) {
  if (_8bit) {
    return cpu_pop8();
  }
  return cpu_pop16();
}

static uint32_t pushv(const uint16_t val, const bool _8bit = true) {
  if (_8bit) {
    cpu_push8(val); 
  }
  else {
    cpu_push16(val);
  }
  return NOFLAG;
}

static int bcdadd(int s1, int s2)
{
  int sum = 0;

  sum += (s1 & 0xF) + (s2 & 0xf);
  if (sum >= 0xA)
    sum += 0x6;
  sum += (s1 & 0xF0) + (s2 & 0xF0);
  if (sum >= 0xA0)
    sum += 0x60;
  sum += (s1 & 0xF00) + (s2 & 0xF00);
  if (sum >= 0xA00)
    sum += 0x600;
  sum += (s1 & 0xF000) + (s2 & 0xF000);
  if (sum >= 0xA000)
    sum += 0x6000;
  return sum;
}

static int bcdsub(int sum)
{
  if ((sum & 0xF) >= 0xA)
    sum -= 0x6;
  if ((sum & 0xF0) >= 0xA0)
    sum -= 0x60;
  if ((sum & 0xF00) >= 0xA00)
    sum -= 0x600;
  if ((sum & 0xF000) >= 0xA000)
    sum -= 0x6000;
  return sum;
}

static uint32_t adc(uint16_t src, const bool _8bit)
{
  const uint32_t mask  = zmask[_8bit];
  uint32_t sum = 0;

  sum = (_A & mask) + (src & mask) + !!fC;
  fV = VFLAG(_A, src, sum, nmask[_8bit]);
  if (fD) {
    sum = bcdadd(_A & mask, (src & mask) + !!fC);
  }
  fC = (sum > mask);
  return setv(_A, sum, _8bit);
}

static uint32_t sbc(uint16_t src, const bool _8bit)
{
  const uint32_t mask  = zmask[_8bit];
  uint32_t sum = 0;
  
  src = ~src;
  sum = (_A & mask) + (src & mask) + !!fC;
  fV = VFLAG(_A, src, sum, nmask[_8bit]);
  if (fD) {
    sum = bcdsub(sum);
  }
  fC = (sum > mask);
  return setv(_A, sum, _8bit);
}

/* Get opcode argument 
 * 00000000.MMMMMMMM.MMMMMMMM.MMMMMMMM = memory address,byte
 * 01000000.MMMMMMMM.MMMMMMMM.MMMMMMMM = memory address,word
 * 10000000.00000000.00000000.IIIIIIII = immediate byte
 * 11000000.00000000.IIIIIIII.IIIIIIII = immediate word
 * 00100000.--------.--------.-------- = A, 8-bit
 * 01100000.--------.--------.-------- = A, 16-bit
 * 10010000.--------.--------.-------- = Set Result(Write to Address), 8-bit
 * 11010000.--------.--------.-------- = Set Result(Write to Address), 16-bit
 */

/*                       +----+----+----+
 * Absolute         ABS  | B  |  HHLL   |
 * Absolute, X      ABX  | B  |  HHLL+X |  lda $0000,x        absolute, indexed with x
 * Absolute, Y      ABY  | B  |  HHLL+Y |  lda $0000,y        absolute, indexed with y
 * Long Absolute X  ABL                    lda $7e0000,x      absolute, long indexed with x
 * (inAbsolute)          | 0  |  HHLL   |  jmp ($0000)        absolute, indirect
 * [Absolute]            | 0  |  HHLL   |
 * (inAbsolute,X)   ABLX | K  |  HHLL+X |  jmp ($0000,x)      absolute indexed with x, indirect
 * Direct           DPG  | D  |  00LL   |
 * Direct,X         DPX  | D  |  00LL+X |  lda $00,x          direct, indexed with x
 * Direct,Y         DPY  | D  |  00LL+Y |  lda $00,y          direct, indexed with y
 * (inDirect)            | D  |  00LL   |  lda ($00)          direct, indirect
 * [inDirect]       ILZ  | D  |  00LL   |  lda [$00]          direct, indirect long
 * [inDirect],Y     ILY  | D  |  00LL   |  lda [$00],Y        direct, indirect indexed long with y
 * (inDirect,X)     IXX  | 0  |  D+LL+X |  lda ($00,x)        direct, indexed with x, indirect
 * (inDirect),Y     IXY  | 0  |  D+LL   |  lda ($00),y        direct, indirect indexed with y
 * Stack Relative   DPS                    lda $00,s          stack relative
 * Sxs              ISY                    lda ($01,s),y      stack relative, indirect indeeed with y
 *                       +----+----+----+
 */
static arg_t mkimm(int val) {
  return TYPE_IMM + val;
};

static arg_t mkmem(uint32_t off, uint16_t delta) {
  return DBR + off + delta;
}

static arg_t getarg(int arg, bool _8bit)
{
  uint16_t addr;
  uint8_t  lo;
  
  switch (arg) {
  case IMP:
    return mkimm(0);
  case RA:
    if (fE || fM) {
      // _8-bit A register
      return (uint8_t)_A | TYPE_IMM | TYPE_ACC;
    }
    return _A | TYPE_IMM | TYPE_ACC | SIZE_WORD;
  case IMM:
    return _8bit ? mkimm(cpu_fetch8()) : mkimm(cpu_fetch16() | SIZE_WORD);
  case ABS:   // [nnnn]
    return mkmem(cpu_fetch16(), 0);
  case ABX:   // abs,X [nnnn+X]
    return mkmem(cpu_fetch16(), _X);
  case ABY:   // abs, Y [nnnn+Y]
    return mkmem(cpu_fetch16(), _Y);
  case DPG:   // dir [nn]
    return mkmem(0, cpu_fetch8());
  case DPX:   // dir,X [nn+x]
    lo = cpu_fetch8() + _X;
    return mkmem(0, lo);
  case DPY:   // dir,Y [nn+y]
    lo = cpu_fetch8() + _Y;
    return mkmem(0, lo);
  case ABL:   // long [nnnnnn]
    return mkmem(cpu_fetch24(), 0);
  case ABLX:   // long,X [nnnnnn+x]
    return mkmem(cpu_fetch24(), _X);
  case IXZ: // [16[d:nn]]
    addr = cpu_fetch8();
    return PBR + cpu_read16(DBR+addr);
  case ILZ: // [24[d:nn]]
    addr = cpu_fetch8();
    return PBR + cpu_read24(DBR+addr);
  case IXX: // [16[d:nn+x]]
    lo = cpu_fetch8() + _X;
    return PBR + cpu_read16(DBR+lo);
  case IXY: // [16[d:nn]+y]
    addr = cpu_fetch8();
    return PBR + cpu_read16(DBR+addr) + _Y;
  case ILY: // 24[[nn]+y]
    addr = cpu_fetch8();
    return cpu_read24(DBR+addr) + _Y;
  case DPS: // [nn+S]
    addr = cpu_fetch8();
    return addr + _S;
  case ISY: // [16[nn+s]+y]
    addr = cpu_fetch8() + _S;
    return cpu_read16(addr) + _Y;
  case PCREL8:  // rel8 nn
    addr = cpu_fetch8();
    return mkimm(PC + (int8_t)addr + PBR);
  case PCREL16: // rel16 nnnn
    addr = cpu_fetch16();
    return mkimm(PC + (int16_t)addr + PBR);
  case PCABSL: // nnnnnn
    addr = cpu_fetch24();
    return mkimm(addr);
  case PCABS: // nnnn
    return mkimm(PBR + cpu_fetch16());
  case PCIND:
    addr = cpu_fetch16();
    return mkimm(PBR + cpu_read16(PBR+addr));
  case PCINDX:
    addr = cpu_fetch16() + _X;
    return mkimm(PBR + cpu_read16(PBR+addr));
  case PCINDL:
    addr = cpu_fetch16();
    return mkimm(cpu_read24(addr));
  default:
    printf("Unknown aRG: %x\n", arg);
    assert(0);
    return -1;
  }
  return addr;
}

/*=============================================================*
 * Opcode handlers
 *=============================================================*/
bool halt;

static uint32_t op_CLC(arg_t& src, const bool _8bit) { fC=0; return NOFLAG; }
static uint32_t op_CLD(arg_t& src, const bool _8bit) { fD=0; return NOFLAG; }
static uint32_t op_CLI(arg_t& src, const bool _8bit) { fI=0; return NOFLAG; }

static uint32_t op_CLV(arg_t& src, const bool _8bit) { fV=0; return NOFLAG; }
static uint32_t op_SEC(arg_t& src, const bool _8bit) { fC=1; return NOFLAG; }
static uint32_t op_SED(arg_t& src, const bool _8bit) { fD=1; return NOFLAG; }
static uint32_t op_SEI(arg_t& src, const bool _8bit) { fI=1; return NOFLAG; }

static uint32_t op_ORA(arg_t& src, const bool _8bit) { return setv(_A, _A | src, _8bit); }
static uint32_t op_AND(arg_t& src, const bool _8bit) { return setv(_A, _A & src, _8bit); }
static uint32_t op_EOR(arg_t& src, const bool _8bit) { return setv(_A, _A ^ src, _8bit); }

static uint32_t op_ADC(arg_t& src, const bool _8bit) { return adc(src, _8bit); }
static uint32_t op_SBC(arg_t& src, const bool _8bit) { return sbc(src, _8bit); }

static uint32_t op_CMP(arg_t& src, const bool _8bit) { return cmpv(_A, src, _8bit); }
static uint32_t op_CPX(arg_t& src, const bool _8bit) { return cmpv(_X, src, _8bit);}
static uint32_t op_CPY(arg_t& src, const bool _8bit) { return cmpv(_Y, src, _8bit); }

static uint32_t op_INC(arg_t& src, const bool _8bit) { return storev(src, src + 1, _8bit); }
static uint32_t op_INX(arg_t& src, const bool _8bit) { return setv(_X, _X+1, _8bit); }
static uint32_t op_INY(arg_t& src, const bool _8bit) { return setv(_Y, _Y+1, _8bit); }
static uint32_t op_INA(arg_t& src, const bool _8bit) { return setv(_A, _A+1, _8bit); }
static uint32_t op_DEC(arg_t& src, const bool _8bit) { return storev(src, src - 1, _8bit); }
static uint32_t op_DEX(arg_t& src, const bool _8bit) { return setv(_X, _X-1, _8bit); }
static uint32_t op_DEY(arg_t& src, const bool _8bit) { return setv(_Y, _Y-1, _8bit); }
static uint32_t op_DEA(arg_t& src, const bool _8bit) { return setv(_A, _A-1, _8bit); }

static uint32_t op_BCC(arg_t& src, const bool _8bit) { return setpc(!fC, src); }
static uint32_t op_BCS(arg_t& src, const bool _8bit) { return setpc(fC,  src); }
static uint32_t op_BEQ(arg_t& src, const bool _8bit) { return setpc(fZ,  src); }
static uint32_t op_BNE(arg_t& src, const bool _8bit) { return setpc(!fZ, src); }
static uint32_t op_BMI(arg_t& src, const bool _8bit) { return setpc(fN,  src); }
static uint32_t op_BPL(arg_t& src, const bool _8bit) { return setpc(!fN, src); }
static uint32_t op_BVC(arg_t& src, const bool _8bit) { return setpc(!fV, src); }
static uint32_t op_BVS(arg_t& src, const bool _8bit) { return setpc(fV,  src); }

static uint32_t op_BRA(arg_t& src, const bool _8bit) { return setpc(true, src); }
static uint32_t op_BRL(arg_t& src, const bool _8bit) { return setpc(true, src); }
static uint32_t op_JMP(arg_t& src, const bool _8bit) { return setpc(true, src); }
static uint32_t op_JML(arg_t& src, const bool _8bit) { return setpc(true, src); }

static uint32_t op_NOP(arg_t& src, const bool _8bit) { return NOFLAG; }

static uint32_t op_LDA(arg_t& src, const bool _8bit) { return setv(_A, src, _8bit); }
static uint32_t op_LDX(arg_t& src, const bool _8bit) { return setv(_X, src, _8bit); }
static uint32_t op_LDY(arg_t& src, const bool _8bit) { return setv(_Y, src, _8bit); }

static uint32_t op_STA(arg_t& src, const bool _8bit) { storev(src, _A, _8bit); return NOFLAG; } // fe||fm
static uint32_t op_STX(arg_t& src, const bool _8bit) { storev(src, _X, _8bit); return NOFLAG; } // fe||fx
static uint32_t op_STY(arg_t& src, const bool _8bit) { storev(src, _Y, _8bit); return NOFLAG; } // fe||fx
static uint32_t op_STZ(arg_t& src, const bool _8bit) { storev(src, 0,  _8bit); return NOFLAG; } // fe||fm

static uint32_t op_PHA(arg_t& src, const bool _8bit) { return pushv(_A, _8bit); }
static uint32_t op_PHX(arg_t& src, const bool _8bit) { return pushv(_X, _8bit); }
static uint32_t op_PHY(arg_t& src, const bool _8bit) { return pushv(_Y, _8bit); }

static uint32_t op_PHP(arg_t& src, const bool _8bit) { cpu_push8(cpu_getflags()); return NOFLAG; }
static uint32_t op_PHB(arg_t& src, const bool _8bit) { cpu_push8(DBR >> 16); return NOFLAG; }
static uint32_t op_PHK(arg_t& src, const bool _8bit) { cpu_push8(PBR >> 16); return NOFLAG; }
static uint32_t op_PHD(arg_t& src, const bool _8bit) { cpu_push16(_D); return NOFLAG; }
static uint32_t op_PEA(arg_t& src, const bool _8bit) { cpu_push16(src); return NOFLAG; }
static uint32_t op_PEI(arg_t& src, const bool _8bit) { cpu_push16(src); return NOFLAG; }
static uint32_t op_PER(arg_t& src, const bool _8bit) { cpu_push16(src); return NOFLAG; }

static uint32_t op_PLA(arg_t& src, const bool _8bit) { return setv(_A, popv(_8bit), _8bit); }
static uint32_t op_PLX(arg_t& src, const bool _8bit) { return setv(_X, popv(_8bit), _8bit); }
static uint32_t op_PLY(arg_t& src, const bool _8bit) { return setv(_Y, popv(_8bit), _8bit); }

static uint32_t op_PLB(arg_t& src, const bool _8bit) { DBR = nzv(cpu_pop8(), true) << 16; return NOFLAG; }
static uint32_t op_PLD(arg_t& src, const bool _8bit) { _D  = nzv(cpu_pop16(), false); return NOFLAG; }
static uint32_t op_REP(arg_t& src, const bool _8bit) { cpu_setflags(cpu_getflags() & ~src); return NOFLAG; } // if (e) |= xm
static uint32_t op_SEP(arg_t& src, const bool _8bit) { cpu_setflags(cpu_getflags() |  src); return NOFLAG; }

static uint32_t op_TAY(arg_t& src, const bool _8bit) { return setv(_Y, _A, _8bit); }
static uint32_t op_TAX(arg_t& src, const bool _8bit) { return setv(_X, _A, _8bit); }
static uint32_t op_TSX(arg_t& src, const bool _8bit) { return setv(_X, _S, _8bit); }
static uint32_t op_TYA(arg_t& src, const bool _8bit) { return setv(_A, _Y, _8bit); }
static uint32_t op_TXA(arg_t& src, const bool _8bit) { return setv(_A, _X, _8bit); }
static uint32_t op_TXY(arg_t& src, const bool _8bit) { return setv(_Y, _X, _8bit); }
static uint32_t op_TYX(arg_t& src, const bool _8bit) { return setv(_X, _Y, _8bit); }
static uint32_t op_TDC(arg_t& src, const bool _8bit) { return setv(_A, _D, _8bit); }
static uint32_t op_TCD(arg_t& src, const bool _8bit) { return setv(_D, _A, _8bit); } 
static uint32_t op_TSC(arg_t& src, const bool _8bit) { _A=_S; return NOFLAG; }
static uint32_t op_TXS(arg_t& src, const bool _8bit) { _S=_X; return NOFLAG; } //e
static uint32_t op_TCS(arg_t& src, const bool _8bit) { _S=_A; return NOFLAG; } //e?


static uint32_t op_PLP(arg_t& src, const bool _8bit) {
  uint8_t x = cpu_pop8();
  cpu_setflags(x);
  if (fE) { // |= xm
    fM = fX = 1;
  }
  return NOFLAG;
}

static uint32_t op_RTS(arg_t& src, const bool _8bit) { PC = popv(Word)+1; return NOFLAG; }

static uint32_t op_RTI(arg_t& src, const bool _8bit) {
  cpu_setflags(cpu_pop8());
  PC = cpu_pop16();
  if (!fE) {
    PBR = cpu_pop8() << 16;
  }
  return NOFLAG;
}


static uint32_t op_ASL(arg_t& src, const bool _8bit) {
  fC = src & nmask[_8bit];
  setva(src, src << 1, _8bit);
  return NOFLAG;
}
    
static uint32_t op_LSR(arg_t& src, const bool _8bit) {
  fC = src & 1;
  setva(src, src >> 1, _8bit);
  return NOFLAG;
}

static uint32_t op_ROL(arg_t& src, const bool _8bit) {
  uint16_t bit = fC;

  fC = src & nmask[_8bit];
  setva(src, (src << 1) | bit, _8bit);
  return NOFLAG;
}

static uint32_t op_ROR(arg_t& src, const bool _8bit) {
  uint16_t bit = 0;

  bit = fC ? nmask[_8bit] : 0;
  fC = src & 1;
  setva(src, (src >> 1) | bit, _8bit);
  return NOFLAG;
};
  
static uint32_t op_BIT(arg_t& src, const bool _8bit) {
  setnz(_A & src, _8bit);
  setnf(src, _8bit);
  if (_8bit) {
    fV = (src & 0x40) != 0;
  }
  else {
    fV = (src & 0x4000) != 0;
  }
  fV = (src & (_8bit ? 0x40 : 0x4000)) != 0;
  return NOFLAG;
}

/* Immediate bit doesn't set N/V flags */
static uint32_t op_BITI(arg_t& src, const bool _8bit) {
  setzf(_A & src, _8bit);
  return NOFLAG;
}

static uint32_t op_JSL(arg_t& src, const bool _8bit) {
  cpu_push8(PBR >> 16);
  cpu_push16(PC-1);
  return setpc(true, src);
}

static uint32_t op_RTL(arg_t& src, const bool _8bit) {
  PC  = cpu_pop16() + 1;
  PBR = cpu_pop8() << 16;
  return NOFLAG;
};

static uint32_t op_JSR(arg_t& src, const bool _8bit) {
  cpu_push16(PC - 1);
  return setpc(true, src);
}

static uint32_t op_MVP(arg_t& src, const bool _8bit) {
  uint32_t sb, db;
  uint8_t s;
  
  printf("MVP: %x A:%.4x X:%.4x Y:%.4x\n", src, _A, _X, _Y);
  db = (src & 0xFF) << 16;
  sb = ((src >> 8) & 0xFF) << 16;
  while (_A != 0xFFFF) {
    if (_8bit) {
      s = cpu_read8(sb + (_X & 0xFF));
      cpu_write8(db + (_Y & 0xFF), s);
    }
    else {
      s = cpu_read8(sb + _X);
      cpu_write8(db + _Y, s);
    }
    setvnz(_X, _X - 1, _8bit);
    setvnz(_Y, _Y - 1, _8bit);
    setvnz(_A, _A - 1, false);
  }
  return NOFLAG;
}

static uint32_t op_MVN(arg_t& src, const bool _8bit) {
  uint32_t sb, db;
  uint8_t s;
  
  printf("MVN: %x A:%.4x X:%.4x Y:%.4x\n", src, _A, _X, _Y);
  db = (src & 0xFF) << 16;
  sb = ((src >> 8) & 0xFF) << 16;
  while (_A != 0xFFFF) {
    if (_8bit) {
      s = cpu_read8(sb + (_X & 0xFF));
      cpu_write8(db + (_Y & 0xFF), s);
    }
    else {
      s = cpu_read8(sb + _X);
      cpu_write8(db + _Y, s);
    }
    setvnz(_X, _X + 1, _8bit);
    setvnz(_Y, _Y + 1, _8bit);
    setvnz(_A, _A - 1, false);
  }
  return NOFLAG;
}

static uint32_t op_COP(arg_t& src, const bool _8bit) {
  if (fE) {
    cpu_push16(PC);
    cpu_push8(cpu_getflags());
    fI = 1;
    fD = 0;
    setpc(true, cpu_read16(0xfff4));
  }
  else {
    cpu_push8(PBR >> 16);
    cpu_push16(PC);
    cpu_push8(cpu_getflags());
    fI = 1;
    fD = 0;
    setpc(true, cpu_read16(0xffe4));
  }
  return NOFLAG;
};

static uint32_t op_BRK(arg_t& src, const bool _8bit) {
  if (fE) {
    cpu_push16(PC+1);
    fB = 1;
    cpu_push8(cpu_getflags() | 0x30);
    fI = 1;
    fD = 0;
    setpc(true, cpu_read16(0xfffe));
  }
  else {
    cpu_push8(PBR>>16);
    cpu_push16(PC);
    cpu_push8(cpu_getflags());
    fI = 1;
    fD = 0;
    setpc(true, cpu_read16(0xffe6));
  }
  return NOFLAG;
};

static uint32_t op_TRB(arg_t& src, const bool _8bit) {
  setzf(src & _A, _8bit);
  storev(src, src & ~_A, _8bit);
  return NOFLAG;
}

static uint32_t op_TSB(arg_t& src, const bool _8bit) {
  setzf(src & _A, _8bit);
  storev(src, src | _A, _8bit);
  return NOFLAG;
}

static uint32_t op_STP(arg_t& src, const bool _8bit) {
  printf("STOPPED...\n");
  halt = 1;
  return NOFLAG;
}

static uint32_t op_WAI(arg_t& src, const bool _8bit) {
  printf("WAIT....\n");
  halt = 1;
  return NOFLAG;
}

static uint32_t op_WDM(arg_t& src, const bool _8bit) {
  if (src == 0x01) {
    /* Write output A */
    printf("WDMOUT: %.2x '%c'\n", _A, _A);
  }
  return NOFLAG;
}

static uint32_t op_XBA(arg_t& src, const bool _8bit) {
  _A = (_A >> 8) | (_A << 8);
  fN = (_A & 0x80) > 0;
  fZ = (_A & 0xFF) == 0;
  return NOFLAG;
}

static uint32_t op_XCE(arg_t& src, const bool _8bit) {
  bool oe = fE;
  fE=fC;
  fC=oe;
  
  if (fE == MODE_6502) {
    _S = 0x100 | (uint8_t)_S;
    fB = 1;
    fK = 1;
  }
  if (fX) {
    _X &= 0xFF;
    _Y &= 0xFF;
  }
  printf("Switching mode: %d\n", fE);
  return NOFLAG;
}

#define M3 3
#define X3 3

enum {
  // size depends on x-flag
  X____ = 0x1000,
  X__NZ,
  X_CNZ,
  XVCNZ,

  // size depends on m-flag
  M____ = 0x2000,
  M__NZ,
  M_CNZ,
  MVCNZ,
  M___Z,

  // word
  W____ = 0x4000,
  W__NZ,

  // byte
  B____ = 0x8000,
  B__NZ,
};

struct opcode_t {
  const char *mnem;
  int flag, arg, clks;
  uint32_t (*fn)(arg_t&, const bool);
} opmap[256];

#define _(b,flag,mnem,arg,clks) opmap[b] = (opcode_t){ #mnem, flag, arg, clks, op_##mnem };
int mkop() {
  setbuf(stdout, NULL);
  
  _(0xAA, X__NZ, TAX, IMP, 2); //x
  _(0xA8, X__NZ, TAY, IMP, 2); //x
  _(0xBA, X__NZ, TSX, IMP, 2); //x
  _(0x8A, M__NZ, TXA, IMP, 2); //x
  _(0x9B, X__NZ, TXY, IMP, 2); //x
  _(0x98, M__NZ, TYA, IMP, 2); //x
  _(0xBB, X__NZ, TYX, IMP, 2); //x
  _(0x7B, W__NZ, TDC, IMP, 2); //x
  _(0x5B, W__NZ, TCD, IMP, 2); //x
  _(0x3B, W__NZ, TSC, IMP, 2); //x
  _(0x9A, W____, TXS, IMP, 2); //x
  _(0x1B, W____, TCS, IMP, 2); //x
  
  _(0xA9, M__NZ, LDA, IMM, 2+M1);
  _(0xA5, M__NZ, LDA, DPG, 3+M1+DI);
  _(0xB5, M__NZ, LDA, DPX, 4+M1+DI);
  _(0xAD, M__NZ, LDA, ABS, 4+M1);
  _(0xBD, M__NZ, LDA, ABX, 4+M1+CROSS);
  _(0xB9, M__NZ, LDA, ABY, 4+M1+CROSS);
  _(0xAF, M__NZ, LDA, ABL, 5+M1);
  _(0xBF, M__NZ, LDA, ABLX,5+M1);
  _(0xB2, M__NZ, LDA, IXZ, 5+M1+DI);
  _(0xA1, M__NZ, LDA, IXX, 6+M1+DI);
  _(0xB1, M__NZ, LDA, IXY, 5+M1+DI+CROSS);
  _(0xA3, M__NZ, LDA, DPS, 4+M1);
  _(0xB3, M__NZ, LDA, ISY, 7+M1);
  _(0xA7, M__NZ, LDA, ILZ, 6+M1+DI);
  _(0xB7, M__NZ, LDA, ILY, 6+M1+DI);

  _(0xA2, X__NZ, LDX, IMM, 2+X1);
  _(0xA6, X__NZ, LDX, DPG, 3+X1+DI);
  _(0xB6, X__NZ, LDX, DPY, 4+X1+DI);
  _(0xAE, X__NZ, LDX, ABS, 4+X1);
  _(0xBE, X__NZ, LDX, ABY, 4+X1+CROSS);

  _(0xA0, X__NZ, LDY, IMM, 2+X1);
  _(0xA4, X__NZ, LDY, DPG, 3+X1+DI);
  _(0xB4, X__NZ, LDY, DPX, 4+X1+DI);
  _(0xAC, X__NZ, LDY, ABS, 4+X1);
  _(0xBC, X__NZ, LDY, ABX, 4+X1+CROSS);

  _(0x85, M____, STA, DPG, 3+M1+DI+STORE);
  _(0x95, M____, STA, DPX, 4+M1+DI+STORE);
  _(0x83, M____, STA, DPS, 4+M1+STORE);
  _(0x8D, M____, STA, ABS, 4+M1+STORE);
  _(0x9D, M____, STA, ABX, 5+M1+STORE);
  _(0x99, M____, STA, ABY, 5+M1+STORE);
  _(0x8F, M____, STA, ABL, 5+M1+STORE); 
  _(0x9F, M____, STA, ABLX,5+M1+STORE);
  _(0x81, M____, STA, IXX, 6+M1+DI+STORE);
  _(0x91, M____, STA, IXY, 6+M1+DI+STORE);
  _(0x92, M____, STA, IXZ, 5+M1+DI+STORE);
  _(0x93, M____, STA, ISY, 7+M1+STORE);
  _(0x87, M____, STA, ILZ, 6+M1+DI+STORE);
  _(0x97, M____, STA, ILY, 6+M1+DI+STORE);

  _(0x86, X____, STX, DPG, 3+X1+DI+STORE);
  _(0x96, X____, STX, DPY, 4+X1+DI+STORE);
  _(0x8E, X____, STX, ABS, 4+X1+STORE);
  
  _(0x84, X____, STY, DPG, 3+X1+DI+STORE);
  _(0x94, X____, STY, DPY, 4+X1+DI+STORE); 
  _(0x8C, X____, STY, ABS, 4+X1+STORE);

  _(0x64, M____, STZ, DPG, 3+M1+DI+STORE);
  _(0x74, M____, STZ, DPX, 4+M1+DI+STORE);
  _(0x9C, M____, STZ, ABS, 4+M1+STORE);
  _(0x9E, M____, STZ, ABX, 5+M1+STORE);

  _(0x09, M__NZ, ORA, IMM, 2+M1);
  _(0x05, M__NZ, ORA, DPG, 0);
  _(0x15, M__NZ, ORA, DPX, 0);
  _(0x0D, M__NZ, ORA, ABS, 0);
  _(0x1D, M__NZ, ORA, ABX, 0);
  _(0x19, M__NZ, ORA, ABY, 0);
  _(0x01, M__NZ, ORA, IXX, 0);
  _(0x11, M__NZ, ORA, IXY, 0);
  _(0x12, M__NZ, ORA, IXZ, 0);
  _(0x03, M__NZ, ORA, DPS, 0);
  _(0x13, M__NZ, ORA, ISY, 0);
  _(0x07, M__NZ, ORA, ILZ, 0);
  _(0x17, M__NZ, ORA, ILY, 0);
  _(0x0F, M__NZ, ORA, ABL, 0);
  _(0x1F, M__NZ, ORA, ABLX, 0);

  _(0x29, M__NZ, AND, IMM, 2+M1);
  _(0x25, M__NZ, AND, DPG, 3+M1+DI);
  _(0x35, M__NZ, AND, DPX, 4+M1+DI);
  _(0x2D, M__NZ, AND, ABS, 4+M1);
  _(0x3D, M__NZ, AND, ABX, 4+M1+CROSS);
  _(0x39, M__NZ, AND, ABY, 4+M1+CROSS);
  _(0x21, M__NZ, AND, IXX, 6+M1+DI);
  _(0x31, M__NZ, AND, IXY, 5+M1+DI+CROSS);
  _(0x32, M__NZ, AND, IXZ, 5+M1+DI);
  _(0x23, M__NZ, AND, DPS, 4+M1);
  _(0x33, M__NZ, AND, ISY, 7+M1);
  _(0x27, M__NZ, AND, ILZ, 6+M1+DI);
  _(0x37, M__NZ, AND, ILY, 6+M1+DI);
  _(0x2F, M__NZ, AND, ABL, 5+M1);
  _(0x3F, M__NZ, AND, ABLX,5+M1);

  _(0x49, M__NZ, EOR, IMM, 2+M1);
  _(0x45, M__NZ, EOR, DPG, 3+M1+DI);
  _(0x55, M__NZ, EOR, DPX, 4+M1+DI);
  _(0x4D, M__NZ, EOR, ABS, 4+M1);
  _(0x5D, M__NZ, EOR, ABX, 4+M1+CROSS);
  _(0x59, M__NZ, EOR, ABY, 4+M1+CROSS);
  _(0x41, M__NZ, EOR, IXX, 6+M1+DI);
  _(0x51, M__NZ, EOR, IXY, 4+M1+DI+CROSS);

  _(0x52, M__NZ, EOR, IXZ, 5+M1+DI);
  _(0x43, M__NZ, EOR, DPS, 4+M1);
  _(0x53, M__NZ, EOR, ISY, 7+M1);
  _(0x47, M__NZ, EOR, ILZ, 6+M1+DI);
  _(0x57, M__NZ, EOR, ILY, 6+M1+DI);
  _(0x4F, M__NZ, EOR, ABL, 5+M1);
  _(0x5F, M__NZ, EOR, ABLX,5+M1);

  _(0x69, MVCNZ, ADC, IMM, 2+M1);
  _(0x65, MVCNZ, ADC, DPG, 3+M1+DI);
  _(0x75, MVCNZ, ADC, DPX, 4+M1+DI);
  _(0x6D, MVCNZ, ADC, ABS, 4+M1);
  _(0x7D, MVCNZ, ADC, ABX, 4+M1+CROSS);
  _(0x79, MVCNZ, ADC, ABY, 4+M1+CROSS);
  _(0x61, MVCNZ, ADC, IXX, 6+M1+DI);
  _(0x71, MVCNZ, ADC, IXY, 5+M1+DI+CROSS);
  _(0x72, MVCNZ, ADC, IXZ, 5+M1+DI);  //direct page indirect
  _(0x63, MVCNZ, ADC, DPS, 4+M1); // stack relative
  _(0x73, MVCNZ, ADC, ISY, 7+M1);  // sr indirect indexed, y
  _(0x67, MVCNZ, ADC, ILZ, 6+M1+DI);  //direct page indirect long
  _(0x77, MVCNZ, ADC, ILY, 6+M1+DI);  // direct page long, indexed y
  _(0x6F, MVCNZ, ADC, ABL, 5+M1); // absolute long
  _(0x7F, MVCNZ, ADC, ABLX,5+M1);     // absolute long indexed x
  
  _(0xE9, MVCNZ, SBC, IMM, 0); //z
  _(0xE5, MVCNZ, SBC, DPG, 0); //z
  _(0xF5, MVCNZ, SBC, DPX, 0); //z
  _(0xED, MVCNZ, SBC, ABS, 0); //z
  _(0xFD, MVCNZ, SBC, ABX, 0); //z
  _(0xF9, MVCNZ, SBC, ABY, 0); //z
  _(0xE1, MVCNZ, SBC, IXX, 0); //z
  _(0xF1, MVCNZ, SBC, IXY, 0); //z
  _(0xF2, MVCNZ, SBC, IXZ, 0);
  _(0xE3, MVCNZ, SBC, DPS, 0);
  _(0xF3, MVCNZ, SBC, ISY, 0);
  _(0xE7, MVCNZ, SBC, ILZ, 0);
  _(0xF7, MVCNZ, SBC, ILY, 0);
  _(0xEF, MVCNZ, SBC, ABL, 0);
  _(0xFF, MVCNZ, SBC, ABLX,0);

  _(0xC9, M_CNZ, CMP, IMM, 2+M1);
  _(0xC5, M_CNZ, CMP, DPG, 3+M1+DI);
  _(0xD5, M_CNZ, CMP, DPX, 4+M1+DI);
  _(0xCD, M_CNZ, CMP, ABS, 4+M1);
  _(0xDD, M_CNZ, CMP, ABX, 4+M1+CROSS);
  _(0xD9, M_CNZ, CMP, ABY, 4+M1+CROSS);
  _(0xC1, M_CNZ, CMP, IXX, 6+M1+DI);
  _(0xD1, M_CNZ, CMP, IXY, 5+M1+DI+CROSS);
  _(0xD2, M_CNZ, CMP, IXZ, 5+M1+DI);
  _(0xC3, M_CNZ, CMP, DPS, 4+M1);
  _(0xD3, M_CNZ, CMP, ISY, 7+M1);
  _(0xC7, M_CNZ, CMP, ILZ, 6+M1+DI);
  _(0xD7, M_CNZ, CMP, ILY, 6+M1+DI);
  _(0xCF, M_CNZ, CMP, ABL, 5+M1);
  _(0xDF, M_CNZ, CMP, ABLX,5+M1);

  _(0xc0, X_CNZ, CPY, IMM, 2+X1);
  _(0xc4, X_CNZ, CPY, DPG, 4+X1);
  _(0xcc, X_CNZ, CPY, ABS, 3+X1+DI);

  _(0xe0, X_CNZ, CPX, IMM, 2+X1);
  _(0xe4, X_CNZ, CPX, DPG, 4+X1);
  _(0xec, X_CNZ, CPX, ABS, 3+X1+DI);
  
  _(0x48, M____, PHA, IMP, M3);
  _(0xDA, X____, PHX, IMP, X3);
  _(0x5A, X____, PHY, IMP, X3);
  _(0x8B, B____, PHB, IMP, 3);
  _(0x0B, W____, PHD, IMP, 4);
  _(0x4B, B____, PHK, IMP, 3);
  _(0x08, B____, PHP, IMP, 3);

  _(0x68, M__NZ, PLA, IMP, 4);
  _(0xFA, X__NZ, PLX, IMP, 4);
  _(0x7A, X__NZ, PLY, IMP, 4);
  _(0x2B, W__NZ, PLD, IMP, 5);
  _(0xAB, B__NZ, PLB, IMP, 4);
  _(0x28, B____, PLP, IMP, 4);

  _(0xD4, W____, PEI, DPG, 6+DI); //x
  _(0xF4, W____, PEA, IMM, 5); //x
  _(0x62, W____, PER, PCREL16, 6); //x

  _(0xE6, M__NZ, INC, DPG, 5+M2+DI);
  _(0xF6, M__NZ, INC, DPX, 6+M2);
  _(0xEE, M__NZ, INC, ABS, 6+M2);
  _(0xFE, M__NZ, INC, ABX, 7+M2);
  _(0x1A, M__NZ, INA, IMP, 2); //x
  _(0xE8, X__NZ, INX, IMP, 2); //x
  _(0xC8, X__NZ, INY, IMP, 2); //x

  _(0xC6, M__NZ, DEC, DPG, 5+M2+DI);
  _(0xD6, M__NZ, DEC, DPX, 6+M2+DI);
  _(0xCE, M__NZ, DEC, ABS, 6+M2);
  _(0xDE, M__NZ, DEC, ABX, 7+M2);
  _(0x3A, M__NZ, DEA, IMP, 2); //x
  _(0xCA, X__NZ, DEX, IMP, 2); //x
  _(0x88, X__NZ, DEY, IMP, 2); //x

  _(0x04, M___Z, TSB, DPG, 5+M2+DI);
  _(0x0C, M___Z, TSB, ABS, 6+M2);
  _(0x14, M___Z, TRB, DPG, 5+M2+DI);
  _(0x1C, M___Z, TRB, ABS, 6+M2);
  
  _(0x18, B____, CLC, IMP, 2); //x
  _(0x58, B____, CLI, IMP, 2); //x
  _(0xD8, B____, CLD, IMP, 2); //x
  _(0xB8, B____, CLV, IMP, 2); //x
  _(0x38, B____, SEC, IMP, 2); //x
  _(0x78, B____, SEI, IMP, 2); //x
  _(0xF8, B____, SED, IMP, 2); //x
  _(0xC2, B____, REP, IMM, 3); //x
  _(0xE2, B____, SEP, IMM, 3); //x
  _(0xFB, B____, XCE, IMP, 2); //x

  _(0xDB, B____, STP, IMP, 0);
  _(0xEB, B____, XBA, IMP, 3);
  _(0xCB, B____, WAI, IMP, 0);
  _(0x42, B____, WDM, IMM, 2);
  _(0xEA, B____, NOP, IMP, 2);

  _(0x0A, M____, ASL, RA,  2);
  _(0x06, M____, ASL, DPG, 5+M2+DI);
  _(0x16, M____, ASL, DPX, 6+M2+DI);
  _(0x0E, M____, ASL, ABS, 6+M2);
  _(0x1E, M____, ASL, ABX, 7+M2);
  
  _(0x4A, M____, LSR, RA,  2);
  _(0x46, M____, LSR, DPG, 5+M1+DI);
  _(0x56, M____, LSR, DPX, 6+M1+DI);
  _(0x4E, M____, LSR, ABS, 6+M1);
  _(0x5E, M____, LSR, ABX, 7+M1);
  
  _(0x2A, M____, ROL, RA,  2);
  _(0x26, M____, ROL, DPG, 5+M1+DI);
  _(0x36, M____, ROL, DPX, 6+M1+DI);
  _(0x2E, M____, ROL, ABS, 6+M1);
  _(0x3E, M____, ROL, ABX, 7+M1);
  
  _(0x6A, M____, ROR, RA,  2);
  _(0x66, M____, ROR, DPG, 5+M1+DI);
  _(0x76, M____, ROR, DPX, 6+M1+DI);
  _(0x6E, M____, ROR, ABS, 6+M1);
  _(0x7E, M____, ROR, ABX, 7+M1);

  _(0x80, B____, BRA, PCREL8, 0);
  _(0x82, B____, BRL, PCREL16, 0);
  _(0x4C, B____, JMP, PCABS, 3); //x 
  _(0x5C, B____, JMP, PCABSL, 4); //x
  _(0x6C, B____, JMP, PCIND, 5); //x 
  _(0x7C, B____, JMP, PCINDX, 6); //x
  _(0xDC, B____, JML, PCINDL, 6); //x
  _(0x20, B____, JSR, PCABS, 6); //x 
  _(0x22, B____, JSL, PCABSL, 8); //x
  _(0xFC, B____, JSR, PCINDX, 8); //x
  _(0x40, B____, RTI, IMP, 6+E1); //x
  _(0x6B, B____, RTL, IMP, 6); //x
  _(0x60, B____, RTS, IMP, 6); //x

  _(0x10, B____, BPL, PCREL8, 2+BRANCH+E1);
  _(0x30, B____, BMI, PCREL8, 2+BRANCH+E1);
  _(0x50, B____, BVC, PCREL8, 2+BRANCH+E1);
  _(0x70, B____, BVS, PCREL8, 2+BRANCH+E1);
  _(0x90, B____, BCC, PCREL8, 2+BRANCH+E1);
  _(0xB0, B____, BCS, PCREL8, 2+BRANCH+E1);
  _(0xD0, B____, BNE, PCREL8, 2+BRANCH+E1);
  _(0xF0, B____, BEQ, PCREL8, 2+BRANCH+E1);

  _(0x24, M____, BIT, DPG, 3+M1+DI);
  _(0x2C, M____, BIT, ABS, 4+M1);
  _(0x34, M____, BIT, DPX, 4+M1+DI);
  _(0x3C, M____, BIT, ABX, 4+M1+CROSS);
  _(0x89, M____, BIT, IMM, 2+M1); 
  
  _(0x44, X____, MVP, PCABS, C7); // move block,  immw, 7 cycles per byte moved
  _(0x54, X____, MVN, PCABS, C7); // move block,  immw, 7 cycles per byte moved
  
  _(0x00, B____, BRK, IMM, 7+E1);
  _(0x02, B____, COP, IMM, 7+E1);

  /* Gah. Broken opcode */
  opmap[0x89].fn = op_BITI;
  return 0;
};

int cpu_step() {
  const char *pfx="";
  uint8_t op, ib[4];
  uint32_t addr, src, flag;
  bool _8bit;
  opcode_t *opc;

  SPC = PC;
  op = cpu_fetch8();
  opc = &opmap[op];
  if (opc == NULL)
    return -1;
  
  /* Get opcode size
   * B = always 8-bit
   * W = always 16-bit
   * X = depends on X/E flag
   * M = depends on M/E flag
   */
  switch (opc->flag & 0xF000) {
  case B____: _8bit = true; break;
  case W____: _8bit = false; break;
  case X____: _8bit = fE || fX; break;
  case M____: _8bit = fE || fM; break;
  default:
    // assume 8bit?
    _8bit = true;
    break;
  }
  pfx = _8bit ? "b" : "w";

  /* Decode opcode argument */
  addr = getarg(opc->arg, _8bit);
  src = addr;

  /* If not immediate value, read it */
  if (!(addr & TYPE_IMM) && !(opc->clks & STORE)) {
    /* Read source */
    if (_8bit) {
      src = mkimm(cpu_read8(addr));
    }
    else {
      src = mkimm(cpu_read16(addr) | SIZE_WORD);
    }
  }
  if (trace == 3) {
    cpu_showregs();

    ib[0] = op;
    ib[1] = cpu_read8(SPC + 1);
    ib[2] = cpu_read8(SPC + 2);
    ib[3] = cpu_read8(SPC + 3);
    printf("%.2x: %s:%-20s %.8x [%.8x]\n", op, pfx, cpu_dis(ib), addr, src);
  }
  assert(addr != -1);

  /* Set NZ bits on return */
  flag = opc->fn(src, _8bit);
  if (flag != NOFLAG) {
    setnz(flag, _8bit);
  }

  /* Store result to destination */
  if (src & TYPE_RES) {
    if (src & SIZE_WORD) {
      cpu_write16(addr, src);
    }
    else {
      cpu_write8(addr, src);
    }
  }
  return 0;
}

/* Return number of bytes per instruction */
int instr_len(int o)
{
  opcode_t *op = &opmap[o];
  
  switch (op->arg) {
  case IMP: return 1;
  case DPG: return 2;
  case DPX: return 2;
  case DPY: return 2;
  case PCREL8: return 2;
  case ABS: return 3;
  case ABX: return 3;
  case ABY: return 3;
  case IMM: return 3;
  case PCABS: return 3;
  case ABL: return 4;
  case ABLX:return 4;
  }
  if (o == 0xc2 || o == 0xe2)
    return 2;
  printf("No len: [%.2x] %.2x %s\n", o, op->arg, op->mnem);
  exit(0);
}

// disassembly formatting
const char *disfmt(int arg) {
  switch(arg) {
  case IMP:   return "";
  case IMM:   return "#$@b"; // immediate
  case RA:    return "A";    // A-register
  case DPG:   return "$@b";  // direct page
  case DPX:   return "$@b,X";// direct page indexed x
  case DPY:   return "$@b,Y";// direct page indexed y
  case IXZ:   return "($@w)"; // direct page indirect
  case IXX:   return "($@b,X)";// direct page indirect x
  case IXY:   return "($@b),Y"; // direct page indirect indexed y
  case ILZ:   return "[$@b]"; // direct page indirect long
  case ILY:   return "[$@b],Y";// direct page indirect long indexed y
  case ABS:   return "$@w";   // absolute
  case ABX:   return "$@w,X"; // absolute indexed x
  case ABY:   return "$@w,Y"; // absolute indexed y
  case ABL:   return "$@t";   // absolute long
  case ABLX:  return "$@t,X"; // absolute long indexed x
  case DPS:   return "$@b,S";// stack relative
  case ISY:   return "($@b,S),Y";// stack relative indirect indexed y
  case PCREL8:return "$@j";
  case PCIND: return "($@w)";
  case PCABS: return "$@w"; // ABS
  case PCABSL:return "$@t"; // ABL
  case PCINDX:return "($@w,X)";
  case PCINDL:return "[$@w]";
  }
  return "xxx";
}

const char *cpu_dis(const uint8_t *ib)
{
  static char dstr[32], *dpos;
  const char *spos;
  opcode_t *opc;

  opc = &opmap[*ib++];
  if (!opc->mnem) {
    return "xxx";
  }
  dpos = dstr;

  dpos += snprintf(dstr, sizeof(dstr), "%-6s", opc->mnem);

  /* Get disassembly format string */
  spos = disfmt(opc->arg);
  uint16_t npc = (int8_t)ib[0];
  while (*spos) {
    if (replace(&spos, "@b", &dpos, "%.2x", ib[0]) ||
	replace(&spos, "@w", &dpos, "%.2x%.2x", ib[1], ib[0]) ||
	replace(&spos, "@t", &dpos, "%.2x%.2x%.2x", ib[2], ib[1], ib[0]) ||
	replace(&spos, "@j", &dpos, "%.4x:jmp", npc)) {
      continue;
    }
    else {
      *dpos++ = *spos++;
    }
  }
  *dpos = 0;
  return dstr;
}

/* generate control-flow disassembly */
void dumpcfg(int off, int base, int size)
{
  dstk stk(0xFFFFFF, printf);
  uint8_t ib[3], op, nib;
  opcode_t *opc;
  int nxt[3];

  setbuf(stdout, NULL);
  stk.push(off, 1, dstk::PENDING, "first");
  while ((off = stk.pop()) != -1) {
    printf("\n------------------------------ %.8x [%s]\n", off, "");
    PC = off;
    do {
      off = PC;

      op  = cpu_fetch8();
      opc = &opmap[op];

      ib[0] = op;
      ib[1] = cpu_read8(off + 1);
      ib[2] = cpu_read8(off + 2);

      printf("%.4x %.20x %.2x %.2x %s\n", off, op, ib[1], ib[2], cpu_dis(ib));

      nib = instr_len(op);
      nxt[0] = off + nib;
      nxt[1] = -1;
      switch (opc->arg) {
      case PCREL8:
	// BCC PCREL8
	nxt[1] = off + (int8_t)ib[1] + 1;
	break;
      case PCABS:
	if (opc->fn == op_JMP) {
	  // JMP PCABS
	  nxt[0] = PBR + get16(&ib[1]);
	}
	else {
	  // JSR PCABS
	  nxt[1] = PBR + get16(&ib[1]);
	}
	break;
      }
      stk.push(off, nib, dstk::CODE);
      for (int i = 0; i < 2; i++) {
	stk.push(nxt[i], 1, dstk::PENDING);
      }
    } while (nxt[0] != -1 && nxt[1] == -1);
  }
  exit(0);
}
