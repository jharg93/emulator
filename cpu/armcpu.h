#ifndef __armcpu_h__
#define __armcpu_h__

// https://github.com/Goomble/Arm-Emulator/blob/master/armemu.c
//
#define test(x) testt()
#define test1()
#define gentbl()

#define NBIT (1L << 31)

constexpr uint32_t ror32(const uint32_t v, const uint32_t s) {
  return (v >> s) | (v << (32 - s));
}

enum {
  _P = (1L << 24),
  _BL = (1L << 24),
  _U = (1L << 23),
  _B = (1L << 22),
  _US= (1L << 22),
  _W = (1L << 21),
  _L = (1L << 20),
  _S = (1L << 20),

  // _b_l
  // 0000 str
  // 0001 ldr
  // 0010 strh
  // 0011 ldrh
  // 0100 strb
  // 0101 ldrb
  // 0110
  // 0111
  // 1000
  // 1001
  // 1010
  // 1011 ldrsh
  // 1100
  // 1101 ldrsb
  // 1110
  // 1111
  STR = 0x0,
  STRB = _B,
  LDR = _L,
  LDRB = _L|_B,

  STRH = 0x0 | 0xb0,
  LDRH = _L  | 0xb0,
  LDRSB = _L|_B | 0xd0,
  LDRSH = _L | 0xf0,

  MOV = 0x1000,
  ADD,
  SUB,
  CMP,
  BCC,
  LSL,
  LSR,
  ASR,
  B,
  ALU = 0x00,

  RkRkI8 = 0x2000,
  RdRsI5,
  I8,
  
  ____ = 0x0000
};

static const char *regname[] = {
  "r0", "r1", "r2", "r3", "r4","r5", "r6", "r7",
  "r8", "r9", "r10","fp.11","ip.12","sp.13", "lr.14", "pc.15"
};

static const char *alufn[] = {
  "and", "eor", "sub", "rsb", "add", "adc", "sbc", "rsc",
  "tst", "teq", "cmp", "cmn", "orr", "mov", "bic", "mvn",
  "str", "ldr", "strh","ldrh","strb","ldrb","xxx", "xxx",
  "xxx", "xxx", "xxx", "ldrsh","xxx","ldrsb","xxx","xxx",
};

enum {
  DATA   = 0x1,
  MUL,
  MULL,
  SWP,
  BX,
  HALFREG,
  HALFIMM,
  XFER,
  BLOCK,
  BRANCH,
  SWI,
  MSR,
  MRS,
};

// bitmask of types
// 0001.0010.1111.1111.1111.0001.mmmm bx rm              12.1

// 0001.0000.1111.dddd.0000.0000.0000 mrs rd,cpsr        10.0
// 0001.0100.1111.dddd.0000.0000.0000 mrs rd,spsr        14.0
// 0001.0010.1000.1111.0000.0000.mmmm msr cpsr_f, rm     12.0
// 0001.0010.1001.1111.0000.0000.mmmm msr cpsr_fc, rm    12.0
// 0001.0110.1000.1111.0000.0000.mmmm msr spsr_f, rm     16.0
// 0001.0110.1001.1111.0000.0000.mmmm msr spsr_fc, rm    16.0

// 0011.0010.0001.1111.rrrr.iiii.iiii msr cpsr_c, rii    32
// 0011.0010.1000.1111.rrrr.iiii.iiii msr cpsr_f, rii    32
// 0011.0010.1001.1111.rrrr.iiii.iiii msr cpsr_fc, rii   32
// 0011.0110.0001.1111.rrrr.iiii.iiii msr spsr_c, rii    36
// 0011.0110.1000.1111.rrrr.iiii.iiii msr spsr_f, rii    36
// 0011.0110.1001.1111.rrrr.iiii.iiii msr spsr_fc, rii   36
static constexpr int subfn[] = {
  0x000,0x100,0x000,0x100,0x000,0x100,0x000,0x100,
  0x000,0x120,0x000,0x140,0x000,0x160,0x000,0x180
};

static const char *adis[512] = {
  // 0x00 rmi
  "and",  "ands", "eor",  "eors", "sub",  "subs", "rsb",  "rsbs", "add",  "adds", "adc",  "adcs", "sbc",  "sbcs", "rsc",  "rscs",
  "mrs",  "tst",  "msr",  "teq",  "mrs",  "cmp",  "msr",  "cmn",  "orr",  "orrs", "mov",  "movs", "bic",  "bics", "mvn",  "mvns",

  // 0x20 rii
  "and",  "ands", "eor",  "eors", "sub",  "subs", "rsb",  "rsbs", "add",  "adds", "adc",  "adcs", "sbc",  "sbcs", "rsc",  "rscs",
  NULL,   "tst",  "msr",  "teq",  NULL,   "cmp",  "msr",  "cmn",  "orr",  "orrs", "mov",  "movs", "bic",  "bics", "mvn",  "mvns",

  // 0x40 xfer ooo
  "str",  "ldr",  "str",  "ldr",  "strb", "strb", "ldrb", "ldrb", "str",  "ldr",  "str",  "ldr",  "strb", "strb", "ldrb", "ldrb",
  "str",  "ldr",  "str",  "ldr",  "strb", "strb", "ldrb", "ldrb", "str",  "ldr",  "str",  "ldr",  "strb", "strb", "ldrb", "ldrb",

  // 0x60 xfer rmi (xxx)
  "str",  "ldr",  "str",  "ldr",  "strb", "strb", "ldrb", "ldrb", "str",  "ldr",  "str",  "ldr",  "strb", "strb", "ldrb", "ldrb",
  "str",  "ldr",  "str",  "ldr",  "strb", "strb", "ldrb", "ldrb", "str",  "ldr",  "str",  "ldr",  "strb", "strb", "ldrb", "ldrb",

  // 0x80 block
  "blk",  "blk",  "blk",  "blk",  "blk",  "blk",  "blk",  "blk",  "blk",  "blk",  "blk",  "blk",  "blk",  "blk",  "blk",  "blk",
  "blk",  "blk",  "blk",  "blk",  "blk",  "blk",  "blk",  "blk",  "blk",  "blk",  "blk",  "blk",  "blk",  "blk",  "blk",  "blk",

  // 0xa0 branch
  "b",    "b",    "b",    "b",    "b",    "b",    "b",    "b",    "b",    "b",    "b",    "b",    "b",    "b",    "b",    "b",
  "bl",   "bl",   "bl",   "bl",   "bl",   "bl",   "bl",   "bl",   "bl",   "bl",   "bl",   "bl",   "bl",   "bl",   "bl",   "bl",

  [0xf0] =
  "swi",  "swi",  "swi",  "swi",  "swi",  "swi",  "swi",  "swi",  "swi",  "swi",  "swi",  "swi",  "swi",  "swi",  "swi",  "swi",

  // 0x100 rms
  "and",  "ands", "eor",  "eors", "sub",  "subs", "rsb",  "rsbs", "add",  "adds", "adc",  "adcs", "sbc",  "sbcs", "rsc",  "rscs",
  NULL,   "tst",  "bx",   "teq",  NULL,   "cmp",  "msr",  "cmn",  "orr",  "orrs", "mov",  "movs", "bic",  "bics", "mvn",  "mvns",

  // 0x120 mul/swp
  "mul",  "muls", "mla",  "mlas", NULL,   NULL,   NULL,   NULL,   "umull","umulls","umlal","umlals","smull","smulls","smlal","smlals",
  "swp",  NULL,   NULL,   NULL,   "swpb", NULL,   NULL,   NULL,   NULL,   NULL,   NULL,   NULL,   NULL,   NULL,   NULL,   NULL,

  // 0x140 half
  "strh", "ldrh", "strh", "ldrh", "strh", "ldrh", "strh", "ldrh", "strh", "ldrh", "strh", "ldrh", "strh", "ldrh", "strh", "ldrh", 
  "strh", "ldrh", "strh", "ldrh", "strh", "ldrh", "strh", "ldrh", "strh", "ldrh", "strh", "ldrh", "strh", "ldrh", "strh", "ldrh",

  // 0x160 half
  NULL,   "ldrsb",NULL,   "ldrsb",NULL,   "ldrsb",NULL,   "ldrsb",NULL,   "ldrsb",NULL,   "ldrsb",NULL,   "ldrsb",NULL,   "ldrsb",
  NULL,   "ldrsb",NULL,   "ldrsb",NULL,   "ldrsb",NULL,   "ldrsb",NULL,   "ldrsb",NULL,   "ldrsb",NULL,   "ldrsb",NULL,   "ldrsb",

  // 0x180 half
  NULL,   "ldrsh",NULL,   "ldrsh",NULL,   "ldrsh",NULL,   "ldrsh",NULL,   "ldrsh",NULL,   "ldrsh",NULL,   "ldrsh",NULL,   "ldrsh",
  NULL,   "ldrsh",NULL,   "ldrsh",NULL,   "ldrsh",NULL,   "ldrsh",NULL,   "ldrsh",NULL,   "ldrsh",NULL,   "ldrsh",NULL,   "ldrsh",
};

static int atbl[512] = {
  // 0x00 cccc.000o.ooos.nnnn.dddd.iiii.itt0.mmmm rmi
  DATA,   DATA,   DATA,   DATA,   DATA,   DATA,   DATA,   DATA,   DATA,   DATA,   DATA,   DATA,   DATA,   DATA,   DATA,   DATA,   
  MRS,    DATA,   MSR,    DATA,   MRS,    DATA,   MSR,    DATA,   DATA,   DATA,   DATA,   DATA,   DATA,   DATA,   DATA,   DATA,   

  // 0x20 cccc.001o.ooos.nnnn.dddd.rrrr.iiii.iiii rii
  DATA,   DATA,   DATA,   DATA,   DATA,   DATA,   DATA,   DATA,   DATA,   DATA,   DATA,   DATA,   DATA,   DATA,   DATA,   DATA,   
  ____,   DATA,   MSR,    DATA,   ____,   DATA,   MSR,    DATA,   DATA,   DATA,   DATA,   DATA,   DATA,   DATA,   DATA,   DATA,   

  // 0x40 cccc.010p.ubwl.nnnn.dddd.iiii.iiii.iiii | str, strb, ldr, ldrb
  XFER,   XFER,   XFER,   XFER,   XFER,   XFER,   XFER,   XFER,   XFER,   XFER,   XFER,   XFER,   XFER,   XFER,   XFER,   XFER,   
  XFER,   XFER,   XFER,   XFER,   XFER,   XFER,   XFER,   XFER,   XFER,   XFER,   XFER,   XFER,   XFER,   XFER,   XFER,   XFER,   

  // 0x60 cccc.011p.ubwl.nnnn.dddd.iiii.itt0.mmmm | str, strb, ldr, ldrb
  XFER,   XFER,   XFER,   XFER,   XFER,   XFER,   XFER,   XFER,   XFER,   XFER,   XFER,   XFER,   XFER,   XFER,   XFER,   XFER,   
  XFER,   XFER,   XFER,   XFER,   XFER,   XFER,   XFER,   XFER,   XFER,   XFER,   XFER,   XFER,   XFER,   XFER,   XFER,   XFER,   

  // 0x80 cccc.100p.uswl.rrrr.iiii.iiii.iiii.iiii | block
  BLOCK,  BLOCK,  BLOCK,  BLOCK,  BLOCK,  BLOCK,  BLOCK,  BLOCK,  BLOCK,  BLOCK,  BLOCK,  BLOCK,  BLOCK,  BLOCK,  BLOCK,  BLOCK,  
  BLOCK,  BLOCK,  BLOCK,  BLOCK,  BLOCK,  BLOCK,  BLOCK,  BLOCK,  BLOCK,  BLOCK,  BLOCK,  BLOCK,  BLOCK,  BLOCK,  BLOCK,  BLOCK,  

  // 0xa0 cccc.101L.iiii.iiii.iiii.iiii.iiii.iiii | b, bl
  BRANCH, BRANCH, BRANCH, BRANCH, BRANCH, BRANCH, BRANCH, BRANCH, BRANCH, BRANCH, BRANCH, BRANCH, BRANCH, BRANCH, BRANCH, BRANCH, 
  BRANCH, BRANCH, BRANCH, BRANCH, BRANCH, BRANCH, BRANCH, BRANCH, BRANCH, BRANCH, BRANCH, BRANCH, BRANCH, BRANCH, BRANCH, BRANCH, 

  // 0xc0 cccc.110p.uxwl.
  0x0800, 0x0800, 0x0800, 0x0800, 0x0800, 0x0800, 0x0800, 0x0800, 0x0800, 0x0800, 0x0800, 0x0800, 0x0800, 0x0800, 0x0800, 0x0800, 
  0x0800, 0x0800, 0x0800, 0x0800, 0x0800, 0x0800, 0x0800, 0x0800, 0x0800, 0x0800, 0x0800, 0x0800, 0x0800, 0x0800, 0x0800, 0x0800, 

  // 0xe0
  0x3000, 0x3000, 0x3000, 0x3000, 0x3000, 0x3000, 0x3000, 0x3000, 0x3000, 0x3000, 0x3000, 0x3000, 0x3000, 0x3000, 0x3000, 0x3000, 
  SWI,    SWI,    SWI,    SWI,    SWI,    SWI,    SWI,    SWI,    SWI,    SWI,    SWI,    SWI,    SWI,    SWI,    SWI,    SWI,

  // 0x100 rms
  // cccc.000o.ooos.nnnn.dddd.ssss.0tt1.mmmm | rms
  // cccc.0001.0010.1111.1111.1111.0001.mmmm | bx
  DATA,   DATA,   DATA,   DATA,   DATA,   DATA,   DATA,   DATA,   DATA,   DATA,   DATA,   DATA,   DATA,   DATA,   DATA,   DATA,   
  ____,   DATA,   BX,     DATA,   ____,   DATA,   ____,   DATA,   DATA,   DATA,   DATA,   DATA,   DATA,   DATA,   DATA,   DATA,   

  // 0x120 mul/swp
  // cccc.0000.00as.dddd.nnnn.ssss.1001.mmmm | mul
  // cccc.0000.1uas.hhhh.llll.ssss.1001.mmmm | mull
  // cccc.0001.0b00.0000.0000.0000.1001.mmmm | swp
  MUL,    MUL,    MUL,    MUL,    ____,   ____,   ____,   ____,   MULL,   MULL,   MULL,   MULL,   MULL,   MULL,   MULL,   MULL,
  SWP,    ____,   ____,   ____,   SWP,    ____,   ____,   ____,   ____,   ____,   ____,   ____,   ____,   ____,   ____,   ____,

  // 0x140 half
  // cccc.000p.u0wl.nnnn.dddd.0000.1011.mmmm half.reg strh, ldrh
  // cccc.000p.u1wl.nnnn.dddd.iiii.1011.iiii half.imm strh, ldrh
  HALFREG,HALFREG,HALFREG,HALFREG,HALFIMM,HALFIMM,HALFIMM,HALFIMM,HALFREG,HALFREG,HALFREG,HALFREG,HALFIMM,HALFIMM,HALFIMM,HALFIMM,
  HALFREG,HALFREG,HALFREG,HALFREG,HALFIMM,HALFIMM,HALFIMM,HALFIMM,HALFREG,HALFREG,HALFREG,HALFREG,HALFIMM,HALFIMM,HALFIMM,HALFIMM,

  // 0x160 half
  // cccc.000p.u0wl.nnnn.dddd.0000.1101.mmmm half.reg ldrsb
  // cccc.000p.u1wl.nnnn.dddd.iiii.1101.iiii half.imm ldrsb
  ____,   HALFREG,____,   HALFREG,____,   HALFIMM,____,   HALFIMM,____,   HALFREG,____,   HALFREG,____,   HALFIMM,____,    HALFIMM,
  ____,   HALFREG,____,   HALFREG,____,   HALFIMM,____,   HALFIMM,____,   HALFREG,____,   HALFREG,____,   HALFIMM,____,    HALFIMM,

  // 180 half
  // cccc.000p.u0wl.nnnn.dddd.0000.1111.mmmm half.reg ldrsh
  // cccc.000p.u1wl.nnnn.dddd.iiii.1111.iiii half.imm ldrsh
  ____,   HALFREG,____,   HALFREG,____,   HALFIMM,____,   HALFIMM,____,   HALFREG,____,   HALFREG,____,   HALFIMM,____,    HALFIMM,
  ____,   HALFREG,____,   HALFREG,____,   HALFIMM,____,   HALFIMM,____,   HALFREG,____,   HALFREG,____,   HALFIMM,____,    HALFIMM,
};

static const char *mnem(uint32_t op, int opfn) {
  return adis[opfn];
}

int trace;
int iztest;

void fbit(char *dst, const char *src, ...)
{
  char ch;
  
  va_list ap;
  va_start(ap, src);
  while ((ch = *src++) != 0) {
    if (va_arg(ap, bool) == false)
      ch = '-';
    *dst++ = ch;
  }
  *dst = 0;
}

struct arm_cpu {
  enum {
    MODE_USER       = 0b10000,
    MODE_FIQ        = 0b10001,
    MODE_SUPERVISOR = 0b10011,
    MODE_ABORT      = 0b10111,
    MODE_IRQ        = 0b10010,
    MODE_UNDEFINED  = 0b11011,
    MODE_SYSTEM     = 0b11111,

    THUMB_MODE = (1L << 5),

  };
  uint32_t prefetch[2];
  uint32_t regs[16];
  uint32_t &PC = regs[15];
  uint32_t &LR = regs[14];
  uint32_t &SP = regs[13];
  uint32_t cpsr;
  struct {
    bool N,Z,C,V;
  } flags;
  void reset(uint32_t addr);
  int  step();
  uint32_t fetch();

  uint32_t getcpsr();
  void setcpsr(uint32_t nv, uint32_t mask = 0xF000000FF);

  void arm_exec(uint32_t);
  void thumb_exec(uint16_t);

  bool testcond(const uint32_t cond) const;
  void arm_alu(uint32_t op, int,int rd, int rn);
  void arm_branch(uint32_t op);
  void arm_bx(uint32_t op, int rm);
  void arm_xfer(uint32_t op, uint32_t mode, int rd, int rn, uint32_t off, bool imm = false);
  void arm_block(uint32_t op, int rn, uint32_t list);

  void thumb_block(uint16_t op, int rn, uint32_t list) {
  };
  uint8_t arm_ldrb(uint32_t base, uint32_t off) {
    return cpu_read8(base+off);
  };
  uint16_t arm_ldrh(uint32_t base, uint32_t off) {
    return cpu_read16(base+off);
  };
  uint32_t arm_ldr(uint32_t base, uint32_t off) {
    return cpu_read32(base+off);
  };
  void arm_strb(uint32_t base, uint32_t off, uint8_t val) {
    cpu_write8(base+off, val);
  };
  void arm_strh(uint32_t base, uint32_t off, uint16_t val) {
    cpu_write16(base+off, val);
  };
  void arm_str(uint32_t base, uint32_t off, uint32_t val) {
    cpu_write32(base+off, val);
  };
  
  uint32_t shifty(int tt, uint32_t v, uint32_t r, bool imm = false);

  /* Current opcode info */
  uint32_t nv;
  uint32_t nc;
  uint32_t res;
  uint32_t setnz(const uint32_t v) {
    res = v;
    return v;
  };
  // 0000
  void arm_and(uint32_t &dst, uint32_t src1, uint32_t src2) {
    dst = setnz(src1 & src2);
  };
  // 0001
  void arm_eor(uint32_t& dst, uint32_t src1, uint32_t src2) {
    dst = setnz(src1 ^ src2);
  };
  // 0010
  void arm_sub(uint32_t& dst, uint32_t src1, uint32_t src2) {
    dst = setnz(src1 - src2);
    nc = CFLAG_SUB(src1, src2, dst, NBIT);
    nv = VFLAG_SUB(src1, src2, dst, NBIT);
    printf("dst: %.8x %.8x %.8x %x\n", dst, src1, src2, nc);
  };
  // 0100
  void arm_add(uint32_t& dst, uint32_t src1, uint32_t src2) {
    dst = setnz(src1 + src2);
    printf("add: %x %x %x\n", src1, src2, dst);
    nc = CFLAG(src1, src2, dst, NBIT);
    nv = VFLAG(src1, src2, dst, NBIT);
  };
  // 0101
  void arm_adc(uint32_t& dst, uint32_t src1, uint32_t src2, uint32_t c) {
    arm_add(dst, src1, src2 + c);
  };
  // 0110
  void arm_sbc(uint32_t& dst, uint32_t src1, uint32_t src2, uint32_t c) {
    arm_sub(dst, src1, src2 + c - 1);
  };
  // 0111
  void arm_rsc(uint32_t& dst, uint32_t src1, uint32_t src2, uint32_t c) {
    arm_sub(dst, src2, src1 + c - 1);
  };
  // 1000
  void arm_tst(uint32_t& dst, uint32_t src1, uint32_t src2) {
    arm_and(src1, src1, src2);
  };
  // 1001
  void arm_teq(uint32_t& dst, uint32_t src1, uint32_t src2) {
    arm_eor(src1, src1, src2);
  };
  // 1010
  void arm_cmp(uint32_t& dst, uint32_t src1, uint32_t src2) {
    arm_sub(src1, src1, src2);
  };
  // 1011
  void arm_cmn(uint32_t& dst, uint32_t src1, uint32_t src2) {
    arm_add(src1, src1, src2);
  };
  // 1100
  void arm_orr(uint32_t& dst, uint32_t src1, uint32_t src2) {
    dst = setnz(src1 | src2);
  };
  // 1101
  void arm_mov(uint32_t& dst, uint32_t src1, uint32_t src2) {
    dst = setnz(src2);
  };
  // 1110
  void arm_bic(uint32_t& dst, uint32_t src1, uint32_t src2) {
    dst = setnz(src1 & ~src2);
  };
  // 1111
  void arm_mvn(uint32_t& dst, uint32_t src1, uint32_t src2) {
    dst = setnz(~src2);
  };
  
  const char *arm_flagstr() {
    static char fbits[5];

    fbit(fbits, "nzcv", flags.N, flags.Z, flags.C, flags.V);
    return fbits;
  };
  int arm_lsl(uint32_t& dst, uint32_t val, uint32_t shift) {
    /* imm: 0-31
     *  lsl 0:  co=ci, res=rm
     * reg:
     *  lsl 32: co=rm.0 res=0
     *  lsl > 32: co=0, res=0
     */
    int ccf = flags.C;
    if (shift > 32) {
      dst = setnz(0);
      ccf = 0;
    } else if (shift == 32) {
      dst = setnz(0);
      ccf = (val & 0x1);
    } else if (shift > 0) {
      dst = setnz(val << shift);
      ccf = ((val >> (32 - shift)) & 1);
    }
    return ccf;
  };
  int arm_lsr(uint32_t& dst, uint32_t val, uint32_t shift) {
    /* lsr = 0: lsr 32 (res=0, co=Rm.31
     * reg:
     *   lsr 32 : res=0, co=Rm.31
     *   lsr >32: res=0, co=0
     */
    int ccf = flags.C;
    if (shift > 32) {
      dst = setnz(0);
      ccf = false;
    } else if (shift == 32) {
      dst = setnz(0);
      ccf = ((val >> 31) & 1);
    } else {
      dst = setnz(val >> shift);
      ccf = ((val >> (shift-1)) & 1);
    }
    return ccf;
  }
  int arm_asr(uint32_t& dst, uint32_t val, uint32_t shift) {
    int ccf = flags.C;
    if (shift >= 32) {
      dst = setnz(val & NBIT ? -1 : 0);
      ccf = dst & 1;
    } else {
      dst = setnz((int32_t)val >> shift);
      ccf = (val >> (shift-1)) & 0x01;
    }
    return ccf;
  };
  int arm_ror(uint32_t& dst, uint32_t val, uint32_t shift) {
    int ccf = flags.C;
    
    dst = ror32(val, shift & 0x1F);
    printf("dst is : %.8x : %x\n", dst, dst >> 31);
    return (dst >> 31) & 1;
  }
  void arm_disasm(uint32_t op, const char *fmt);
  void thumb_disasm(uint16_t op, const char *fmt);

  uint32_t arm_branchoff(uint32_t op) const {
    uint32_t off = (op & 0xffffff);
    if ((off & (1L << 23)) != 0) {
      off |= 0xFF000000;
    }
    off = PC + (off << 2);
    return off;
  };
};

void arm_cpu::arm_bx(uint32_t op, int reg) {
  uint32_t npc = regs[reg];
  
  assert((op & 0x0FFFFFF0) == 0x012fff10); 
  printf("@ BX %s [%.8x]\n", regname[reg], npc);
  if (cpsr & THUMB_MODE && !(npc & 1)) {
    printf("out of thumb\n");
    cpsr &= ~THUMB_MODE;
  } else if (!(cpsr & THUMB_MODE) && (npc & 1)) {
    printf("to thumb mode\n");
    cpsr |= THUMB_MODE;
  }
  PC = npc & ~0x3;
  fetch();
}

void arm_cpu::arm_disasm(uint32_t op, const char *src) {
  char dst[128], *d;
  d = dst;
  while (*src != 0) {
    if (replace(&src, "%Rm", &d, "%s", regname[op & 0xf]) ||
	replace(&src, "%Rs", &d, "%s", regname[(op >> 8) & 0xf]) ||
	replace(&src, "%Rd", &d, "%s", regname[(op >> 12) & 0xf]) ||
	replace(&src, "%Rn", &d, "%s", regname[(op >> 16) & 0xf])) {
      continue;
    }
    *d++ = *src++;
  }
  *d = 0;
  printf("@@@ %s\n", dst);
}

// ____.__nn.nnss.sddd
// ____._kkk.____.____
void arm_cpu::thumb_disasm(uint16_t op, const char *src) {
  char dst[128], *d;
  const int kkk = (op >> 8) & 7;
  const int nnn = (op >> 6) & 7;
  const int sss = (op >> 3) & 7;
  const int ddd = (op >> 0) & 7;
  
  d = dst;
  while (*src != 0) {
    if (replace(&src, "%Rk", &d, "%s", regname[kkk]) ||
	replace(&src, "%Rn", &d, "%s", regname[nnn]) ||
	replace(&src, "%Rs", &d, "%s", regname[sss]) ||
	replace(&src, "%Rd", &d, "%s", regname[ddd]) ||
	replace(&src, "%i3", &d, "0x%x", nnn) ||
	replace(&src, "%i8", &d, "0x#x", op & 0xff)) {
      continue;
    }
    *d++ = *src++;
  }
  *d = 0;
  printf("@@@ %s\n", dst);
}

// cpsr bits:
// nzcv
void arm_cpu::setcpsr(uint32_t nv, uint32_t mask) {
  cpsr = (cpsr & ~mask) | (nv & mask);
  flags.N = (cpsr >> 31) & 1;
  flags.Z = (cpsr >> 30) & 1;
  flags.C = (cpsr >> 29) & 1;
  flags.V = (cpsr >> 28) & 1;
};

uint32_t arm_cpu::getcpsr() {
  uint32_t r;
  r = cpsr & 0x0FFFFFFF;
  r |= (flags.N << 31);
  r |= (flags.Z << 30);
  r |= (flags.C << 29);
  r |= (flags.V << 28);
  return r;
}

enum {
  EQ, // Z
  NE,
  CS, // C
  CC,
  MI, // N
  PL,
  VS, // V
  VC,
  HI, // C/Z
  LS,
  GE, // N/V
  LT,
  GT, // Z/N/V
  LE,
  AL
};

bool arm_cpu::testcond(const uint32_t cond) const {
  switch (cond) {
  case EQ: return flags.Z;
  case NE: return !flags.Z;
  case CS: return flags.C;
  case CC: return !flags.C;
  case MI: return flags.N;
  case PL: return !flags.N;
  case VS: return flags.V;
  case VC: return !flags.V;
  case HI: return flags.C && !flags.Z;
  case LS: return !flags.C || flags.Z;
  case GE: return flags.N == flags.V;
  case LT: return flags.N != flags.V;
  case GT: return !flags.Z && (flags.N == flags.V);
  case LE: return flags.Z || (flags.N != flags.V);
  }
  return true;
}

/* rmi:
 * x LSL=0   => Rm, cf=cf
 *   LSR=0:32=> 0,  cf=Rm.31
 *   ASR=0:32=> 0|-1, cf=Rm.31
 *   ROR=0:RRX : cf:(Rm>>1), cf=Rm.0
 * rms:
 * x LSL=0   => Rm, cf=cf
 *   LSR=0   => Rm, cf=cf
 *   ASR=0   => Rm, cf=cf
 *   ROR=0   => Rm, cf=cf
 *
 * x LSL=32  => 0, cf=Rm.0
 * x LSL>32  => 0, cf=0
 *   LSR=32  => 0, cf=Rm.31
 *   LSR>32  => 0, cf=0
 *
 *   ASR>=32 => 0|ffffffff, cf=Rm.31
 *   ROR=32  => Rm, cf=Rm.31
 *   ROR>32  => ROR(n-32)
 */
uint32_t arm_cpu::shifty(int tt, uint32_t v, uint32_t r, bool imm)
{
  uint32_t base = v;
  
  switch (tt) {
  case 0: // lsl
    nc = arm_lsl(base, v, r);
    printf("shift.lsl: %.8x << %.8x : %.8x\n", v, r, base, flags.C);
    break;
  case 1: // lsr
    if (imm && !r)
      r = 32;
    nc = arm_lsr(base, v, r);
    printf("shift.lsr: %.8x >> %.8x : %.8x\n", v, r, base);
    break;
  case 2: // asr
    if (imm && !r)
      r = 32;
    nc = arm_asr(base, v, r);
    printf("shift.asr: %.8x >> %.8x : %.8x\n", v, r, base);
    break;
  case 3: // ror
    nc = arm_ror(base, v, r);
    printf("shift.ror: %.8x >> %.8x  : %.8x\n", v, r, base);
    break;
  }
  return base;
}

/*=============================*
 * Draw screen
 *=============================*/
static char screen[80*25+1];

void setscreen(int x, int y, char ch) {
  screen[(y * 80) + x] = ch;
}

void drawscreen() {
  char line[82] = { 0 };
  
  for (int y = 0; y < 25; y++) {
    memcpy(line, &screen[y * 80], 80);
    printf("%s\n", line);
  }
}

void arm_cpu::arm_branch(uint32_t op)
{
  uint32_t off;

  off = arm_branchoff(op);
  printf("arm_branch: %x\n", off);
  if (op & _BL) {
    LR = PC - 4;
  }
  /* BIOS calls */
  if (off == 0x080003F4) {
    printf("draw_char(%d,%d,'%c')\n",
	   regs[0], regs[1], regs[2]);
    setscreen(regs[0]/8, regs[1]/8, regs[2]);
    drawscreen();
  } else if (off == 0x0800046C) {
    printf("draw_word(%d,%d,len=%x,%x,'",
	   regs[0], regs[1], regs[3], regs[2]);
    for (int i = 0; i < regs[3]; i++) {
      char ch = cpu_read8(regs[2] + i);
      if (ch < ' ' || ch > 'z')
	ch = '_';
      setscreen((regs[0]/8)+i, (regs[1]/8), ch);
      printf("%c", ch);
    }
    printf("')\n");
    drawscreen();
  } else if (off == 0x08000494) {
    char hex[32];
    sprintf(hex, "%x", regs[2]);
    for (int i = 0; hex[i]; i++) {
      setscreen((regs[0]/8)+i, regs[1]/8, hex[i]);
    }
    printf("draw_hex(%d,%d,%x)\n",
	   regs[0], regs[1], regs[2]);
    drawscreen();
  }
  else {
    PC = off;
    fetch();
  }
}

// cccc.000p.u0wl.nnnn.dddd.0000.1sh1.mmmm = half.reg (off = regs[rm])
// cccc.000p.u1wl.nnnn.dddd.iiii.1sh1.iiii = half.imm (off = ((op >> 4) & 0xf0) + (op & 0xF))
// cccc.010p.ubwl.nnnn.dddd.iiii.iiii.iiii = xfer imm (off = (op & 0xfff))
// cccc.011p.ubwl.nnnn.dddd.iiii.itt0.mmmm = xfer reg (off = shifty(regs[rm]))
// cccc.100p.uswl.nnnn.iiii.iiii.iiii.iiii = block    (off = 4)
// rd, [rn], -rm
// rd, [rn], #-nn
// rd, [rn], rn
// rd, [rn], #nn
// rd, [rn, -rm]
// rd, [rn, -rm]!
// rd, [rn, #-nn]
// rd, [rn, #-nn]!
// rd, [rn, rm]
// rd, [rn, rm]!
// rd, [rn, #nn]
// rd, [rn, #nn]!
// Post-indexed: writeback is redundant (W=0)
// Privileged mode, W=1 implies user address
void arm_cpu::arm_xfer(uint32_t op, uint32_t mode, int rd, int rn, uint32_t off, bool imm)
{
  char o[32], s[32];
  uint32_t base;
  
  /* Get suffix of operation */
  if (imm) {
    snprintf(o, sizeof(o), "#%s%x", (off == 0) || (op & _U) ? "" : "-", off);
  }
  else {
    snprintf(o, sizeof(o), "%s%s", op & _U ? "" : "-", regname[op & 0xf]);
  }
  if (op & _P) {
    snprintf(s, sizeof(s), "%s, [%s, %s]", regname[rd], regname[rn], o);
  }
  else {
    snprintf(s, sizeof(s), "%s, [%s], %s", regname[rd], regname[rn], o);
  }
  if (op & _W) {
    strcat(s, "!");
  }
  if ((op & _U) == 0) {
    off = -off;
  }
  base = regs[rn];
  if ((op & _P) != 0) {
    // pre-inc/dec
    base += off;
  }
  switch (mode) {
  case STR:
    printf("@@@ str %s <- %.8x:%.8x\n", s, base, regs[rd]);
    cpu_write32(base, regs[rd]);
    break;
  case STRB:
    printf("@@@ strb %s <- %.8x:%.8x\n", s, base, regs[rd]);
    cpu_write8(base, regs[rd]);
    break;
  case STRH:
    printf("@@@ strh %s <- %.8x:%.8x\n", s, base, regs[rd]);
    cpu_write16(base, regs[rd]);
    break;
  case LDR:
    regs[rd] = cpu_read32(base);
    printf("@@@ ldr %s -> %.8x:%.8x\n", s, base, regs[rd]);
    break;
  case LDRB:
    regs[rd] = cpu_read8(base);
    printf("@@@ ldrb %s -> %.8x:%.8x\n", s, base, regs[rd]);
    break;
  case LDRH:
    regs[rd] = cpu_read16(base);
    printf("@@@ ldrh %s -> %.8x:%.8x\n", s, base, regs[rd]);
  case LDRSB:
    regs[rd] = (int8_t)cpu_read8(base);
    printf("@@@ ldrb %s -> %.8x:%.8x\n", s, base, regs[rd]);
    break;
  case LDRSH:
    regs[rd] = (int16_t)cpu_read8(base);
    printf("@@@ ldrb %s -> %.8x:%.8x\n", s, base, regs[rd]);
    break;
  default:
    printf("unk: %x\n", mode);
    exit(0);
  }
  if ((op & _P) == 0) {
    // post inc/dec
    base += off;
    // implied writeback for post-indexed mode
    op |= _W;
  }
  if (op & _W) {
    regs[rn] = base;
  }
}

/* 100p.uswl
 * post increment: rn=1000 (1000=r1,1004=r5,1008=r7) -> rn=100c
 * pre-increment:  rn=1000 (1004=r1,1008=r5,100c=r7) -> rn=100c
 * post-decrement: rn=1000 (ff8=r1,ffc=r5,1000=r7) -> rn=ff4
 * pre-decrement:  rn=1000 (ff4=r1,ff8=r5,ffc=r7) -> rn=ff4
 *
 * S-bit and LDM with R15:
 *   spsr_<mode> is tranferred to cpsr
 * S-bit and STM with R15:
 *   user mode registers, do not use writeback
 * S-bit and LDM or STM without R15:
 *   user mode registers.  Using LDM needs dummy cycle
 * R15 should not be base
 *
 * STM writeback base, where base is first value, writes old value
 * LDM always overwrites updated base
 */
void arm_cpu::arm_block(uint32_t op, int rn, uint32_t list)
{
  // transfer lowest to highest, lowest register to lowest memory address
  printf("block: %s, { ");

  bool first = true;
  for (int i = 0; i < 16; i++) {
    if (list & (1L << i)) {
      printf("%s%s", first ? "" : ",", regname[i]);
      first = false;
    }
  }
  printf(" }\n");

  /* Iteratively call xfer */
  uint32_t fm = op & (_P|_U|_W|_L);
  for (int i = 0; i < 15; i++) {
    int n = (op & _U) ? i : 15-i;
    if (op & (1L << n)) {
      arm_xfer(fm, op & _L, n, rn, 4, true);
    }
  }
}

const char *stype[] = { "lsl", "lsr", "asr", "ror" };

void arm_cpu::arm_alu(uint32_t op, int opfn, int rd, int rn)
{
  const int rs = (op >> 8) & 0xF;
  const int rm = (op >> 0) & 0xF;
  uint32_t op2 = 0;
  
  nc = flags.C;
  nv = flags.V;
  if (opfn <= 0x1f) {
    // iiii.itt0.mmmm rmi
    op2 = shifty((op >> 5) & 3, regs[rm], (op >> 7) & 0x1F, true);
    printf("@@@@ %-6s %s, %s, %s shift #%d [rmi]\n",
	   alufn[(op >> 21) & 0xF], regname[rd], regname[rn], regname[rm], (op >> 7) & 0x1F);
  } else if (opfn <= 0x3f) {
    // rrrr.iiii.iiii rii
    op2 = ror32(op & 0xff, (op >> 7) & 0x1E);
    printf("@@@@ %-6s %s, %s, #%x [rii]\n",
	   alufn[(op >> 21) & 0xF], regname[rd], regname[rn], op2);
  } else {
    // ssss.0tt1.mmmm rms
    op2 = shifty((op >> 5) & 3, regs[rm], regs[rs] & 0xff, false);
    printf("@@@@ %-6s %s, %s, %s %s %s [rms]\n",
	   alufn[(op >> 21) & 0xF], regname[rd], regname[rn], regname[rm],
	   stype[(op >> 5) & 3],
	   regname[rs]);
  }
  bool ccf = flags.C;
  
  uint32_t &Rd = regs[rd];
  uint32_t  Rn = regs[rn];
  switch((op >> 21) & 0xF) {
  case 0x0: arm_and(Rd, Rn, op2); break;
  case 0x1: arm_eor(Rd, Rn, op2); break;
  case 0x2: arm_sub(Rd, Rn, op2); break;
  case 0x3: arm_sub(Rd, op2, Rn); break;
  case 0x4: arm_add(Rd, Rn, op2); break;
  case 0x5: arm_adc(Rd, Rn, op2, ccf); break;
  case 0x6: arm_sbc(Rd, Rn, op2, ccf); break;
  case 0x7: arm_rsc(Rd, Rn, op2, ccf); break;
  case 0x8: arm_tst(Rd, Rn, op2); break;
  case 0x9: arm_teq(Rd, Rn, op2); break;
  case 0xa: arm_cmp(Rd, Rn, op2); break;
  case 0xb: arm_cmn(Rd, Rn, op2); break;
  case 0xc: arm_orr(Rd, Rn, op2); break;
  case 0xd: arm_mov(Rd, Rn, op2); break;
  case 0xe: arm_bic(Rd, Rn, op2); break;
  case 0xf: arm_mvn(Rd, Rn, op2); break;
  default:
    // f  = F00000000
    // c  = 00000001F
    // fc = F000000FF
    assert(0);
    break;
  }
  // setflag
  if (op & _S) {
    printf("setflag: %x\n", res);
    printf("flags.c %x %x %s\n", flags.C, nc, arm_flagstr());
    flags.Z = res == 0;
    flags.N = (res >> 31);
    flags.C = nc;
    flags.V = nv;
  }
}

void arm_cpu::arm_exec(uint32_t op) {
  const int rm = (op >> 0) & 0xF;
  const int rd = (op >> 12) & 0xF;
  const int rn = (op >> 16) & 0xF;
  int opfn = (op >> 20) & 0xFF;
  uint32_t op2 = 0;
  int optype;

  // check if this is conditional
  if (!testcond(op >> 28)) {
    return;
  }

  /* Get opcode function and lookup type */
  res = 0;
  if (opfn <= 0x1f) {
    opfn += subfn[(op >> 4) & 0xF];
  }
  optype = atbl[opfn];
  printf("  dis: [%.4x %s] ", opfn, mnem(op, opfn));

  // Now got mask for instruction */
  switch (optype) {
  case BRANCH: // cccc.101L.iiii.iiii.iiii.iiii.iiii.iiii
    arm_branch(op);
    break;
  case BX:
    arm_bx(op, rm);
    break;
  case DATA: // cccc.00Io.ooos.nnnn.dddd.OOOO.OOOO.OOOO
    arm_alu(op, opfn, rd, rn);
    break;
  case HALFREG:
    // cccc.00xp.u0wl.nnnn.dddd.0000.1sh1.mmmm
    arm_xfer(op, (op & _L) | (op & 0xf0), rd, rn, regs[rm]); // Rd,Rn,Rm
    break;
  case HALFIMM:
    // cccc.00xp.u1wl.nnnn.dddd.oooo.1sh1.oooo
    arm_xfer(op, (op & _L) | (op & 0xf0), rd, rn, ((op >> 4) & 0xF0) | rm, true); // Rd,Rm,imm
    break;
  case BLOCK:
    // cccc.100p.uswl.nnnn.iiii.iiii.iiii.iiii
    arm_block(op, (op >> 16) & 0xF, op & 0xffff);
    break;
  case XFER:
    if (opfn <= 0x5f) {
      // cccc.010p.ubwl.nnnn.dddd.iiii.iiii.iiii
      arm_xfer(op, (op & (_L|_B)), rd, rn, op & 0xfff, true);
    } else {
      exit(0);
    }
    break;
  case MSR:
  case MRS:
    if (opfn >= 0x20) {
      op2 = ror32(op & 0xff, (op >> 7) & 0x1E);
    }
    switch ((op >> 16) & 0xfff) {
    case 0x10f: printf("mrs rd, cpsr\n", regname[rd]); regs[rd] = getcpsr(); break;
    case 0x14f: printf("mrs rd, spsr\n", regname[rd]); regs[rd] = 0; break;

    case 0x128: printf("msr cpsr_f, %s\n", regname[rm]); setcpsr(regs[rm], 0xF0000000); break;
    case 0x129: printf("msr cpsr_fc, %s\n", regname[rm]); setcpsr(regs[rm], 0xF00000FF); break;
    case 0x168: printf("msr spsr_f, %s\n", regname[rm]); break;
    case 0x169: printf("msr spsr_fc, %s\n", regname[rm]); break;
    case 0x321: printf("msr cpsr_c, rii\n"); setcpsr(op2, 0x000000FF); break;
    case 0x328: printf("msr cpsr_f, rii\n"); setcpsr(op2, 0xF0000000); break;
    case 0x329: printf("msr cpsr_fc, rii\n"); setcpsr(op2, 0xF00000FF); break;
    case 0x361: printf("msr spsr_c, rii\n"); break;
    case 0x368: printf("msr spsr_f, rii\n"); break;
    case 0x369: printf("msr spsr_fc, rii\n"); break;
    }
    break;
  default:
    printf("got mask: %.8x\n", optype);
    exit(0);
  }
}

/* Format:
 *    
 * 1  000o.ooii.iiss.sddd 00,08,10    lsl,lsr,asr
 * 2  0001.1ooi.iiss.sddd 18          add,sub    (subformat 2a,2b)
 * 3  001o.oddd.iiii.iiii 20,28,30,38 mov,cmp,add,sub
 * 4  0100.00oo.ooss.sddd 40 x        and,eor,lsl,lsr,asr,adc,sbc,ror,tst,neg,cmp,cmn,orr,mul,bic,mvn
 * 5  0100.01oo.hhss.sddd 40 x        add,cmp,mov,bx
 * 6  0100.1ddd.iiii.iiii 48          ldr
 * 7  0101.lb0o.oobb.bddd 50 58       xfer [str,strb,ldr,ldrb] reg
 * 8  0101.hs1o.oobb.bddd 50 58       xfer [strh,ldrh,ldrsb,ldrsh] reg
 * 9  011B.Liii.iibb.bddd 60 68 70 78 xfer [str,ldr] imm
 * 10 1000.Liii.iibb.bddd 80 88       xfer [strh,ldrh] imm
 * 11 1001.Lddd.iiii.iiii 90 98       xfer [ldr,str] sp
 * 12 1010.Sddd.iiii.iiii a0 a8       xfer addr pc/sp
 * 13 1011.0000.Siii.iiii b0          add to sp
 * 14 1011.L10R.iiii.iiii b0 b8       push/pop
 * 15 1100.Lbbb.iiii.iiii c0 c8       block
 * 16 1101.cccc.iiii.iiii d0 d8       bcc
 * 17 1101.1111.iiii.iiii d8          swi
 * 18 1110.0iii.iiii.iiii e0          b
 * 19 1111.Hiii.iiii.iiii f0 f8       bl
 */
const int f01 = 0x01000000;  // 000o.ooii.iiss.sddd
const int f2a = 0x2a000000;  // 0001.1oon.nnss.sddd
const int f2b = 0x2b000000;  // 0001.1ooi.iiss.sddd
const int f03 = 0x03000000;  // 001o.oddd.iiii.iiii
const int f04 = 0x04000000;  // 0100.00oo.ooss.sddd
const int f05 = 0x05000000;  // 0100.01oo.hhss.sddd
const int f06 = 0x06000000;  // 0100.1ddd.iiii.iiii
const int f07 = 0x07000000;  // 0101.LB0o.oobb.bddd
const int f08 = 0x08000000;  // 0101.HS1o.oobb.bddd
const int f09 = 0x09000000;  // 011B.Liii.iibb.bddd
const int f10 = 0x0a000000;  // 1000.Liii.iibb.bddd
const int f11 = 0x0b000000;  // 1001.Lddd.iiii.iiii
const int f12 = 0x0c000000;  // 1010.Sddd.iiii.iiii
const int f13 = 0x0d000000;  // 1011.0000.siii.iiii
const int f14 = 0x0e000000;  // 1011.L10R.iiii.iiii
const int f15 = 0x0f000000;  // 1100.Lbbb.iiii.iiii
const int f16 = 0x10000000;  // 1101.cccc.iiii.iiii
const int f17 = 0x11000000;  // 1101.1111.iiii.iiii
const int f18 = 0x12000000;  // 1110.0iii.iiii.iiii
const int f19 = 0x13000000;  // 1111.Hiii.iiii.iiii

const int htab[] = {
  // 00 lsl,lsr  000.ooiiiiisssddd
  f01+LSL,  f01+LSL,  f01+LSL,  f01+LSL,  f01+LSL,  f01+LSL,  f01+LSL,  f01+LSL,
  f01+LSR,  f01+LSR,  f01+LSR,  f01+LSR,  f01+LSR,  f01+LSR,  f01+LSR,  f01+LSR,
  // 10
  f01+ASR,  f01+ASR,  f01+ASR,  f01+ASR,  f01+ASR,  f01+ASR,  f01+ASR,  f01+ASR,
  f2a+ADD,  f2a+ADD,  f2a+SUB,  f2a+SUB,  f2b+ADD,  f2b+ADD,  f2b+SUB,  f2b+SUB,
  // 20 001o.okkk.iiii.iiii (mov,cmp)   "%Rk, %Rk, %i8"
  f03+MOV,  f03+MOV,  f03+MOV,  f03+MOV,  f03+MOV,  f03+MOV,  f03+MOV,  f03+MOV,
  f03+CMP,  f03+CMP,  f03+CMP,  f03+CMP,  f03+CMP,  f03+CMP,  f03+CMP,  f03+CMP,
  // 30 001o.okkk.iiii.iiii (add,sub)   "%Rk, %Rk, %i8"
  f03+ADD,  f03+ADD,  f03+ADD,  f03+ADD,  f03+ADD,  f03+ADD,  f03+ADD,  f03+ADD,
  f03+SUB,  f03+SUB,  f03+SUB,  f03+SUB,  f03+SUB,  f03+SUB,  f03+SUB,  f03+SUB,
  // 40 alu
  f04+ALU,  f04+ALU,  f04+ALU,  f04+ALU,  f04+ALU,  f04+ALU,  f04+ALU,  f04+ALU,  
  0x000020, 0x000020, 0x000020, 0x000020, 0x000020, 0x000020, 0x000020, 0x000020, 
  // 50 0101.LB0nnn.bbbddd, HS1nnnbbbddd
  f07+STR,  f07+STR,  f08+STRH, f07+STRH, f07+STRB, f07+STRB, f08+LDRH, f08+LDRH,
  f07+LDR,  f07+LDR,  f08+LDRSB,f08+LDRSB,f07+LDRB, f07+LDRB, f08+LDRSH,f08+LDRSH,
  // 60 011.BLiiiiibbbddd (str,ldr)     "%Rd, [%Rb, %i5]
  f09+STR,  f09+STR,  f09+STR,  f09+STR,  f09+STR,  f09+STR,  f09+STR,  f09+STR,
  f09+LDR,  f09+LDR,  f09+LDR,  f09+LDR,  f09+LDR,  f09+LDR,  f09+LDR,  f09+LDR,
  // 70 011.BLiiiiibbbddd (strb,ldrb)   "%Rd, [%Rb, %i5]
  f09+STRB, f09+STRB, f09+STRB, f09+STRB, f09+STRB, f09+STRB, f09+STRB, f09+STRB,
  f09+LDRB, f09+LDRB, f09+LDRB, f09+LDRB, f09+LDRB, f09+LDRB, f09+LDRB, f09+LDRB,
  // 80 1000.Liiiiibbbddd (strh,ldrh)   %Rd, [%Rb, %i5]
  f10+STRH, f10+STRH, f10+STRH, f10+STRH, f10+STRH, f10+STRH, f10+STRH, f10+STRH,
  f10+LDRH, f10+LDRH, f10+LDRH, f10+LDRH, f10+LDRH, f10+LDRH, f10+LDRH, f10+LDRH, 
  // 90 1001.Lddd.iii.iii  (str, ldr)   %Rd, [SP, %i5]
  f11+STR,  f11+STR,  f11+STR,  f11+STR,  f11+STR,  f11+STR,  f11+STR,  f11+STR,
  f11+LDR,  f11+LDR,  f11+LDR,  f11+LDR,  f11+LDR,  f11+LDR,  f11+LDR,  f11+LDR,
  // a0 xfer addr/pc
  0x000800, 0x000800, 0x000800, 0x000800, 0x000800, 0x000800, 0x000800, 0x000800, 
  0x000800, 0x000800, 0x000800, 0x000800, 0x000800, 0x000800, 0x000800, 0x000800, 
  // b0 add to sp
  0x001000, 0x000000, 0x000000, 0x000000, 0x002000, 0x002000, 0x000000, 0x000000, 
  0x000000, 0x000000, 0x000000, 0x000000, 0x002000, 0x002000, 0x000000, 0x000000, 
  // c0
  0x004000, 0x004000, 0x004000, 0x004000, 0x004000, 0x004000, 0x004000, 0x004000, 
  0x004000, 0x004000, 0x004000, 0x004000, 0x004000, 0x004000, 0x004000, 0x004000, 
  // d0
  f16+BCC,  f16+BCC,  f16+BCC,  f16+BCC,  f16+BCC,  f16+BCC,  f16+BCC,  f16+BCC,
  f16+BCC,  f16+BCC,  f16+BCC,  f16+BCC,  f16+BCC,  f16+BCC,  f16+BCC,  f17+SWI,
  // e0
  f18+B,    f18+B,    f18+B,    f18+B,    f18+B,    f18+B,    f18+B,    f18+B,    
  0x000000, 0x000000, 0x000000, 0x000000, 0x000000, 0x000000, 0x000000, 0x000000, 
  // f0
  0x040000, 0x040000, 0x040000, 0x040000, 0x040000, 0x040000, 0x040000, 0x040000, 
  0x040000, 0x040000, 0x040000, 0x040000, 0x040000, 0x040000, 0x040000, 0x040000,
};

#include <map>
static const std::map<int, const char *> dismap = {
  { f01+LSL, "lsl  %Rd, %Rs, %i5" },
  { f01+LSR, "lsr  %Rd, %Rs, %i5" },
  { f01+ASR, "asr  %Rd, %Rs, %i5" },
  { f2a+ADD, "add  %Rd, %Rs, %Rn" },
  { f2a+SUB, "sub  %Rd, %Rs, %Rn" },
  { f2b+ADD, "add  %Rd, %Rs, %i3" },
  { f2b+SUB, "sub  %Rd, %Rs, %i3" },
  { f03+MOV, "mov  %Rk, %i8" },
  { f03+CMP, "cmp  %Rk, %i8" },
  { f03+ADD, "add  %Rk, %i8" },
  { f03+SUB, "sub  %Rk, %i8" },
  { f10+STRH,"strh %Rd, [%Rs, #%i52]" },
  { f10+LDRH,"ldrh %Rd, [%Rs, #%i52]" },
  { f11+STR, "str  %Rd, [SP, %i84]" },
  { f11+LDR, "ldr  %Rd, [SP, %i84]" },
};  

int thumb_dis(uint16_t op) {
  const int rd = (op >> 0) & 7;
  const int rs = (op >> 3) & 7;
  const int rn = (op >> 6) & 7;
  const int i5 = (op >> 6) & 0x1f;
  const int rk = (op >> 8) & 7;
  const int i8 = (op & 0xff);
  const char *mnem;
  
  int opfn = htab[op >> 8];
  if (opfn < f01) {
    opfn = op >> 8;
  }
  switch (opfn) {
  case f01+LSL:
    printf("lsl %s,%s,#%x\n", regname[rd], regname[rs], i5);
    break;
  case f01+LSR:
    printf("lsr %s,%s,#%x\n", regname[rd], regname[rs], i5);
    break;
  case f01+ASR:
    printf("asr %s,%s,#%x\n", regname[rd], regname[rs], i5);
    break;
  case f2a+ADD:
    printf("add %s,%s,%s\n", regname[rd], regname[rs], regname[rn]);
    break;
  case f2a+SUB:
    printf("sub %s,%s,%s\n", regname[rd], regname[rs], regname[rn]);
    break;
  case f2b+ADD:
    printf("add %s,%s,#%x\n", regname[rd], regname[rs], rn);
    break;
  case f2b+SUB:
    printf("sub %s,%s,#%x\n", regname[rd], regname[rs], rn);
    break;
  case f03+MOV:
    printf("mov %s,%x\n", regname[rk], i8);
    break;
  case f03+CMP:
    printf("cmp %s,%x\n", regname[rk], i8);
    break;
  case f03+ADD:
    printf("add %s,%x\n", regname[rk], i8);
    break;
  case f03+SUB:
    printf("sub %s,%x\n", regname[rk], i8);
    break;
  case f10+STRH:
    // i5 -> i6
    printf("strh %s, [%s, #%x]\n", regname[rd], regname[rs], i5*2);
    break;
  case f10+LDRH:
    // i5 -> i6
    printf("ldrh %s, [%s, #%x]\n", regname[rd], regname[rs], i5*2);
    break;
  case f11+STR:
    // i8 -> i10
    printf("str %s, [SP, #%x]\n", regname[rd], i8*4);
    break;
  case f11+LDR:
    // i8 -> i10
    printf("ldr %s, [SP, #%x]\n", regname[rd], i8*4);
    break;
  default:
    return -1;
  }
  return 0;
}

void arm_cpu::thumb_exec(uint16_t op) {
  const int rd = (op >> 0) & 7;
  const int rs = (op >> 3) & 7;
  const int rn = (op >> 6) & 7;
  const int i5 = (op >> 6) & 0x1f;
  const int rk = (op >> 8) & 7;
  const int i8 = (op & 0xff);
  uint32_t off;

  // oooo.oooo.____.____
  switch (op >> 8) {
  case 0x00 ... 0x07:
    arm_lsl(regs[rk], regs[rk], i5);
    break;
  case 0x08 ... 0x0f:
    arm_lsr(regs[rk], regs[rk], i5);
    break;
  case 0x10 ... 0x17:
    arm_asr(regs[rk], regs[rk], i5);
    break;
  case 0x18 ... 0x19:
    arm_add(regs[rd], regs[rs], regs[rn]);
    break;
  case 0x1a ... 0x1b:
    arm_sub(regs[rd], regs[rs], regs[rn]);
    break;
  case 0x1c ... 0x1d:
    arm_add(regs[rd], regs[rs], rn);
    break;
  case 0x1e ... 0x1f:
    arm_sub(regs[rd], regs[rs], rn);
    break;
  case 0x20 ... 0x23:
    arm_mov(regs[rk], regs[rk], i8);
    break;
  case 0x24 ... 0x27:
    arm_cmp(regs[rk], regs[rk], i8);
    break;
  case 0x28 ... 0x2b:
    arm_add(regs[rk], regs[rk], i8);
    break;
  case 0x2c ... 0x2f:
    arm_sub(regs[rk], regs[rk], i8);
    break;
  case 0x80 ... 0x87:
    // i5 -> i6
    arm_strh(regs[rs], i5*2, regs[rd]);
    break;
  case 0x88 ... 0x8f:
    // i5 -> i6
    regs[rd] = arm_ldrh(regs[rs], i5*2);
    break;
  case 0x90 ... 0x97:
    // i8 -> i10
    arm_str(SP, i8*4, regs[rd]);
    break;
  case 0x98 ... 0x9f:
    // i8 -> i10
    regs[rd] = arm_ldr(SP, i8*4);
    break;
  case 0xc0 ... 0xcf:
    // LDMIA/STMIA
    thumb_block(op, rk, i8);
    break;
  case 0xd0 ... 0xde:
    // bcc
    if (testcond((op >> 8) & 0xF)) {
      PC += ((int8_t)i8) * 2;
    }
    break;
  case 0xe0 ... 0xe7:
    // near br
    off = ((int16_t)(op << 5) >> 4);
    PC += off;
    break;
  case 0xF0 ... 0xF7: // fmt19
    LR = ((op & 0x7FF) << 12) + PC;
    break;
  case 0xF8 ... 0xFF: // fmt19
    off = PC;
    PC = LR + ((op & 0x7FF) << 1);
    LR = (off + 2) | 1;
    break;
  default:
    printf("missing: %x\n", op >> 8);
  }
}

uint32_t arm_cpu::fetch() {
  prefetch[0] = prefetch[1];
  if (cpsr & THUMB_MODE) {
    prefetch[1] = cpu_read16(PC);
    PC += 2;
  } else {
    prefetch[1] = cpu_read32(PC);
    PC += 4;
  }
  return prefetch[0];
};

/* D = 1101
 * B = 1011
 * --- NZCV
 */
int arm_cpu::step() {
  uint32_t op;

  printf("\nPC:%.8x %.8x[%s]\n", PC, getcpsr(), arm_flagstr());
  for (int i = 0; i < 16; i++) {
    printf("%6s:%.8x ", regname[i], regs[i]);
    if ((i & 7) == 7) {
      printf("\n");
    }
  }
  
  op = fetch();
  printf("GOT FETCH: %x : ", op);
  if (cpsr & THUMB_MODE) {
    thumb_exec(op);
  } else {
    arm_exec(op);
  }
};

void arm_cpu::reset(uint32_t addr) {
  for (int i = 0; i < 16; i++) {
    regs[i] = 0;
  }
  SP = 0x03007fe0;
  PC = 0x08000000;
  setcpsr(MODE_SUPERVISOR);
  fetch();
};

static arm_cpu cpu;

int cpu_step() {
  return cpu.step();
}
void cpu_reset(uint32_t addr) {
  cpu.reset(addr);
}

void mkop(int *tbl, const char *bits, int id, int extra=0, const char *fmt="") {
  char ch;
  int mask = 0;
  int value = 0;
  printf("============: %s\n", bits);
  while ((ch = *bits++) != 0) {
    if (ch == '.' || ch == '<' || ch == '>')
      continue;
    mask <<= 1;
    value <<= 1;
    if (ch == '0' || ch == '1') {
      mask |= 1;
      value |= (ch - '0');
    }
  }
  printf("%.8x %.8x %.8x extra=%.8x\n", mask, value, id, extra);
  if (value >= 0x02000000) {
    for (int i = 0x20; i < 256; i++) {
      if (((mask >> 20) & i) == (value >> 20)) {
	tbl[i] = id;
      }
    }
  }
  else {
    int mv = (mask >> 8);
    int vv = (value >> 8);
    
    for (int i = 0; i < 256; i++) {
      if ((i & mv) == vv) {
	if (tbl[i] != 0) {
	  printf("  collide: %.2x : %2d %2d\n", i, tbl[i], id);
	}
	tbl[i] = id;
      }
    }
  }
}

int testt() {
}

void gentbl2()
{
  for (int i = 0 ; i < 80*25; i++) {
    screen[i] = ' ';
  }
  screen[80*25] = 0;
  for (int i = 0; i < 256; i++) {
    cpu.thumb_exec(i << 8);
  }
#if 1
  int tbl[256] = { 0 };
  int armtbl[512] = { 0 };

  mkop(tbl, "<000ooiii>iisssddd", 1, 0, "%Rd, %Rs, %i");  // shift                   00 08 10     [3] lsl,lsr,asr      i5
  mkop(tbl, "<00011ooi>iisssddd", 2, 0, "%Rd, %$s, %v");  // add/sub                 18           [4] add,sub          i3 or rn
  mkop(tbl, "<001ookkk>iiiiiiii", 3, 0, "%Rk, %i");       // alu imm8                20 28 30 38  [4] mov,cmp,add,sub  i8
  mkop(tbl, "<010000oooo>sssddd", 4, 0, "%Rd, %Rs, %Ro"); // alu                     40           [16] alu
  mkop(tbl, "<010001oo>HHsssddd", 5); // hireg/bx                40           [4] add,cmp,mov,bx
  mkop(tbl, "<01001kkk>iiiiiiii", 6, 0, "%Rd,[pc,#imm]"); // pc-relative load        48           [1] ldr                     rd,[pc,#imm]
  mkop(tbl, "<0101LB0n>nnbbbddd", 7, 0, "%Rd,[%Rb,%Rn]"); // load/store reg off      50 58        [4] str,strb,ldr,ldrb       rd,[rb,rn]
  mkop(tbl, "<0101HS1n>nnbbbddd", 8, 0, "%Rd,[%Rb,%Rn]"); // load/store sh           50 58        [4] strh,ldrh,ldrsb,ldrsh   rd,[rb,rn]
  mkop(tbl, "<011BLiii>iibbbddd", 9, 0, "%Rd,[$Rb,%i]");  // load store imm off      60 68 70 78  [4] str,ldr,strb,ldrb       rd,[rb,#imm x4 or x1]
  mkop(tbl, "<1000Liii>iibbbddd", 10, 0, "%Rd,[%Rb,%i]"); // load/store h            80 88        [2] strh,ldrh               rd,[rb,#imm x2]
  mkop(tbl, "<1001Lkkk>iiiiiiii", 11, 0, "%Rk,[sp,%i]");  // sp relative load-store  90 98        [2] str,ldr                 rd>[sp,#imm x4]
  mkop(tbl, "<1010Skkk>iiiiiiii", 12, 0, "%Rd,[pc/sp,%i]");// load addr              a0 a8        [2] add                     rd,pc,#imm x4|rd,sp,#imm x4
  mkop(tbl, "<10110000>Siiiiiii", 13, 0, "sp, %i");       // add off to stack        b0           [2] add
  mkop(tbl, "<1011L10R>iiiiiiii", 14, 0, "{%rlist}");     // pushpop                 b0 b8
  mkop(tbl, "<1100Lkkk>iiiiiiii", 15, 0, "%Rk, {%rlist}");// block load/store        c0 c8
  mkop(tbl, "<1101cccc>iiiiiiii", 16, 0, "%x");           // cond branch             d0 d8
  mkop(tbl, "<11011111>iiiiiiii", 17, 0, "%x");           // swi                     d8
  mkop(tbl, "<11100iii>iiiiiiii", 18, 0, "%x");           // uncond                  e0
  mkop(tbl, "<1111Hiii>iiiiiiii", 19, 0, "%x");           // long branch, link       f0 f8
  for (int i = 0; i < 256; i++) {
    if (!tbl[i]) {
      printf("________, ");
    } else {
      printf("0x%.6x, ", tbl[i]);
    }
    if ((i & 7) == 7)
      printf("\n");
  }

  /* ARM32 */
  mkop(armtbl, "cccc<000o.ooos>nnnn.dddd.iiii<itt0>mmmm", 0x10); // data rmi
  mkop(armtbl, "cccc<000o.ooos>nnnn.dddd.ssss<0tt1>mmmm", 0x11); // data rms 
  mkop(armtbl, "cccc<001o.ooos>nnnn.dddd.rrrr.iiii.iiii", 0x12); // data rii
  mkop(armtbl, "cccc<0000.00as>dddd.nnnn.ssss<1001>mmmm", 2, 0x120); // mul
  mkop(armtbl, "cccc<0000.1uas>hhhh.llll.ssss<1001>mmmm", 3, 0x120); // mull
  mkop(armtbl, "cccc<0001.0b00>nnnn.dddd.0000<1001>mmmm", 4, 0x120); // swp
  mkop(armtbl, "cccc<0001.0010>1111.1111.1111<0001>mmmm", 5, 0x100); // bx
  mkop(armtbl, "cccc<000p.u0wl>nnnn.dddd.0000<1sh1>mmmm", 6, 0x140); // half-reg
  mkop(armtbl, "cccc<000p.u1wl>nnnn.dddd.iiii<1sh1>iiii", 7, 0x140); // half-imm
  mkop(armtbl, "cccc<00x1.0xx0>xxxx.xxxx.xxxx<xxxx>xxxx", 0x13); // msr/mrs
  mkop(armtbl, "cccc<010p.ubwl>nnnn.dddd.iiii.iiii.iiii", 0x18, 0x040); // single xfer
  mkop(armtbl, "cccc<011p.ubwl>nnnn.dddd.iiii<itt0>mmmm", 0x19, 0x040); // single xfer
  mkop(armtbl, "cccc<011x.xxxx>xxxx.xxxx.xxxx.xxx1.xxxx", 9, 0x060); // invalid
  mkop(armtbl, "cccc<100p.uswl>nnnn.iiii.iiii.iiii.iiii", 10,0x080); // block xfer
  mkop(armtbl, "cccc<101L.iiii>iiii.iiii.iiii.iiii.iiii", 11,0x0a0); // branch
  mkop(armtbl, "cccc<110p.unwl>nnnn.DDDD.####.iiii.iiii", 12,0x0c0); // cop data xfer
  mkop(armtbl, "cccc<1110.oooo>nNNN.DDDD.####.ppp0.MMMM", 13,0x0e0); // cop data op
  mkop(armtbl, "cccc<1110.oooL>nNNN.DDDD.####.ppp1.MMMM", 14,0x0e0); // cop reg xfer
  mkop(armtbl, "cccc<1111.iiii>iiii.iiii.iiii.iiii.iiii", 15,0x0f0); // swi

  printf("--arm\n");
  for (int i = 0; i < 512; i++) {
    printf("%.4x, ", armtbl[i]);
    if ((i & 0xf) == 0xf)
      printf("\n");
  }
  testt();
#endif
}
#endif
