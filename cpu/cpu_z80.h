#ifndef __z80_h__
#define __z80_h__

enum {
  NOP = 0x1,
  LDR,
  LDRp,   // bc,de,hl,sp
  LDMRpA,
  LDAMRp,
  LDMHL,
  INCMHL,
  DECMHL,
  ADDHLRp,
  CALL,
  RLCA,
  RRA,
  RRCA,
  RLA,
  DAA,
  CPL,
  SCF,
  CCF,
  ADC,
  ADD,
  SUB,
  SBC,
  AND,
  XOR,
  OR,
  CP,
  RET,
  JR,
  JP,
  JPHL,
  DJNZ,
  RST,
  HLT,
  DI,
  DE,
  RLC,
  RRC,
  RL,
  RR,
  SRL,
  SLL,
  SRR,
  SRA,
  SLA,
  BIT,
  RES,
  SET,
  DECRd,
  INCRd,
  DECRp,
  INCRp,
  PUSHRp,  // bc,de,hl,af
  POPRp,   // bc,de,hl,af

  /* CFlag */
  CC_MASK = 0x0F00,
  cPE = 0x0100,
  cPO = 0x0200,
  cNC = 0x0300,
  cC  = 0x0400,
  cNZ = 0x0500,
  cZ  = 0x0600,
  cP  = 0x0700,
  cM  = 0x0800,

  /* Source */
  SRC_MASK = 0xF000,
  Ib   = 0x2000,
  Iw   = 0x3000,
  Rs   = 0x4000,
  MHL  = 0x5000,
  
  SNZ  = 0x100,
  SRD  = 0x200,
};

// https://map.grauw.nl/resources/z80instr.php
static int z80_edsz[] = {
  20,20,20,20,20,20,20,20,
  20,20,20,20,20,20,20,20,
  20,20,20,20,20,20,20,20,
  20,20,20,20,20,20,20,20,
  20,20,20,20,20,20,20,20,
  20,20,20,20,20,20,20,20,
  20,20,20,20,20,20,20,20,

  2, 2, 2, 4, 2, 2, 2, 2,
  2, 2, 2, 4,20, 2,20, 2,
  2, 2, 2, 4,20,20, 2, 2,
  2, 2, 2, 4,220,20,2, 2,
  2, 2, 2,20,20,20,20, 2,
  2, 2, 2,20,20,20,20, 2,
  2,20, 2, 4,20,20,20,20,
  2, 2, 2, 4,20,20,20,20,

  20,20,20,20,20,20,20,20,
  20,20,20,20,20,20,20,20,
  20,20,20,20,20,20,20,20,
  20,20,20,20,20,20,20,20,
  2, 2, 2, 2, 20,20,20,20,
  2, 2, 2, 2, 20,20,20,20,
  2, 2, 2, 2, 20,20,20,20,
  2, 2, 2, 2, 20,20,20,20,

  20,20,20,20,20,20,20,20,
  20,20,20,20,20,20,20,20,
  20,20,20,20,20,20,20,20,
  20,20,20,20,20,20,20,20,
  20,20,20,20,20,20,20,20,
  20,20,20,20,20,20,20,20,
  20,20,20,20,20,20,20,20,
  20,20,20,20,20,20,20,20,
};

// dd and fd
#define U2 2
#define U3 3  
#define __ 20
static int z80_fdsz[] = {
  __,__,__,__,U2,U2,U3,__,
  __,2, __,__,U2,U2,U3,__,
  __,__,__,__,U2,U2,U3,__,
  __,2, __,__,U2,U2,U3,__,
  __,4, 4, 2, U2,U2,U3,__,
  __,2, 4, 2, U2,U2,U3,__,
  __,__,__,__, 3, 3, 4,__,
  __,2, __,__,U2,U2,U3,__,

  U2,U2,U2,U2,U2,U2, 3,U2,//40
  U2,U2,U2,U2,U2,U2, 3,U2,//48
  U2,U2,U2,U2,U2,U2, 3,U2,//50
  U2,U2,U2,U2,U2,U2, 3,U2,//58
  U2,U2,U2,U2,U2,U2, 3,U2,//60
  U2,U2,U2,U2,U2,U2, 3,U2,//68
  3, 3, 3, 3, 3, 3, __,3, //70
  U2,U2,U2,U2,U2,U2, 3,U2,//78

  U2,U2,U2,U2,U2,U2, 3,U2,//80
  U2,U2,U2,U2,U2,U2, 3,U2,//88
  U2,U2,U2,U2,U2,U2, 3,U2,//90
  U2,U2,U2,U2,U2,U2, 3,U2,//98
  U2,U2,U2,U2,U2,U2, 3,U2,//a0
  U2,U2,U2,U2,U2,U2, 3,U2,//a8
  U2,U2,U2,U2,U2,U2, 3,U2,//b0
  U2,U2,U2,U2,U2,U2, 3,U2,//b8

  __,__,__,__,__,__,__,__,
  __,__,__,__,__,__,__,__,
  __,__,__,__,__,__,__,__,
  __,__,__,__,__,__,__,__,
  __,2 ,__,2 ,__,2 ,__,__,
  __,2 ,__,__,__,__,__,__,
  __,__,__,__,__,__,__,__,
  __,2 ,__,__,__,__,__,__,
};

static int z80sz[] = {
  // 00
  1, 3, 1, 1, 1, 1, 2, 1,
  1, 1, 1, 1, 1, 1, 2, 1,
  2, 3, 1, 1, 1, 1, 2, 1,
  2, 1, 1, 1, 1, 1, 2, 1,
  2, 3, 3, 1, 1, 1, 2, 1,
  2, 1, 3, 1, 1, 1, 2, 1,
  2, 3, 3, 1, 1, 1, 2, 1,
  2, 1, 3, 1, 1, 1, 2, 1,

  // 40
  1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1,

  // 80
  1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1,

  // c0
  1, 1, 3, 3, 3, 1, 2, 1,
  1, 1, 3, 2, 3, 3, 2, 1,
  1, 1, 3, 2, 3, 1, 2, 1,
  1, 1, 3, 2, 3,20, 2, 1,
  1, 1, 3, 1, 3, 1, 2, 1,
  1, 1, 3, 1, 3,20, 2, 1,
  1, 1, 3, 1, 3, 1, 2, 1,
  1, 1, 3, 1, 3,20, 2, 1
};

static int z80fn[256*3] = {
  NOP,      LDRp+Iw,  LDMRpA,   INCRp,    INCRd,    DECRd,    LDR+Ib,   RLCA,
  0,        ADDHLRp,  LDAMRp,   DECRp,    INCRd,    DECRd,    LDR+Ib,   RRCA,
  DJNZ,     LDRp+Iw,  LDMRpA,   INCRp,    INCRd,    DECRd,    LDR+Ib,   RLA,
  JR+Ib,    ADDHLRp,  LDAMRp,   DECRp,    INCRd,    DECRd,    LDR+Ib,   RRA,
  JR+cNZ,   LDRp+Iw,  Iw,       INCRp,    INCRd,    DECRd,    LDR+Ib,   DAA,
  JR+cZ,    ADDHLRp,  Iw,       DECRp,    INCRd,    DECRd,    LDR+Ib,   CPL,
  JR+cNC,   LDRp+Iw,  Iw,       INCRp,    INCMHL,   DECMHL,   LDMHL+Ib, SCF,
  JR+cC,    ADDHLRp,  Iw,       DECRp,    INCRd,    DECRd,    LDR+Ib,   CCF,

  LDR+Rs,   LDR+Rs,   LDR+Rs,   LDR+Rs,   LDR+Rs,   LDR+Rs,   LDR+MHL,  LDR+Rs,   
  LDR+Rs,   LDR+Rs,   LDR+Rs,   LDR+Rs,   LDR+Rs,   LDR+Rs,   LDR+MHL,  LDR+Rs,   
  LDR+Rs,   LDR+Rs,   LDR+Rs,   LDR+Rs,   LDR+Rs,   LDR+Rs,   LDR+MHL,  LDR+Rs,   
  LDR+Rs,   LDR+Rs,   LDR+Rs,   LDR+Rs,   LDR+Rs,   LDR+Rs,   LDR+MHL,  LDR+Rs,   
  LDR+Rs,   LDR+Rs,   LDR+Rs,   LDR+Rs,   LDR+Rs,   LDR+Rs,   LDR+MHL,  LDR+Rs,   
  LDR+Rs,   LDR+Rs,   LDR+Rs,   LDR+Rs,   LDR+Rs,   LDR+Rs,   LDR+MHL,  LDR+Rs,   
  LDMHL+Rs, LDMHL+Rs, LDMHL+Rs, LDMHL+Rs, LDMHL+Rs, LDMHL+Rs, HLT,      LDMHL+Rs,   
  LDR+Rs,   LDR+Rs,   LDR+Rs,   LDR+Rs,   LDR+Rs,   LDR+Rs,   LDR+MHL,  LDR+Rs,   

  ADD+Rs,   ADD+Rs,   ADD+Rs,   ADD+Rs,   ADD+Rs,   ADD+Rs,   ADD+MHL,  ADD+Rs,
  ADC+Rs,   ADC+Rs,   ADC+Rs,   ADC+Rs,   ADC+Rs,   ADC+Rs,   ADC+MHL,  ADC+Rs,
  SUB+Rs,   SUB+Rs,   SUB+Rs,   SUB+Rs,   SUB+Rs,   SUB+Rs,   SUB+MHL,  SUB+Rs,
  SBC+Rs,   SBC+Rs,   SBC+Rs,   SBC+Rs,   SBC+Rs,   SBC+Rs,   SBC+MHL,  SBC+Rs,
  AND+Rs,   AND+Rs,   AND+Rs,   AND+Rs,   AND+Rs,   AND+Rs,   AND+MHL,  AND+Rs,
  XOR+Rs,   XOR+Rs,   XOR+Rs,   XOR+Rs,   XOR+Rs,   XOR+Rs,   XOR+MHL,  XOR+Rs,
  OR+Rs,    OR+Rs,    OR+Rs,    OR+Rs,    OR+Rs,    OR+Rs,    OR+MHL,   OR+Rs, 
  CP+Rs,    CP+Rs,    CP+Rs,    CP+Rs,    CP+Rs,    CP+Rs,    CP+MHL,   CP+Rs, 

  RET+cNZ,  POPRp,    JP+cNZ,   JP,       CALL+cNZ, PUSHRp,   ADD+Ib,   RST,
  RET+cZ,   RET,      JP+cZ,    0,        CALL+cZ,  CALL,     ADC+Ib,   RST,
  RET+cNC,  POPRp,    JP+cNC,   0,        CALL+cNC, PUSHRp,   SUB+Ib,   RST,
  RET+cC,   0,        JP+cC,    0,        CALL+cC,  0,        SBC+Ib,   RST,
  RET+cPO,  POPRp,    JP+cPO,   0,        CALL+cPO, PUSHRp,   AND+Ib,   RST,
  RET+cPE,  JPHL,     JP+cPE,   0,        CALL+cPE, 0,        XOR+Ib,   RST,
  RET+cP,   POPRp,    JP+cP,    DI,       CALL+cP,  PUSHRp,   OR+Ib,    RST,
  RET+cM,   0,        JP+cM,    DE,       CALL+cM,  0,        CP+Ib,    RST,

  /* CB opcodes = 0x100 */
  RLC+Rs,   RLC+Rs,   RLC+Rs,   RLC+Rs,   RLC+Rs,   RLC+Rs,   RLC+MHL,  RLC+Rs,
  RRC+Rs,   RRC+Rs,   RRC+Rs,   RRC+Rs,   RRC+Rs,   RRC+Rs,   RRC+MHL,  RRC+Rs,
  RL+Rs,    RL+Rs,    RL+Rs,    RL+Rs,    RL+Rs,    RL+Rs,    RL+MHL,   RL+Rs,
  RR+Rs,    RR+Rs,    RR+Rs,    RR+Rs,    RR+Rs,    RR+Rs,    RR+MHL,   RR+Rs,
  SLA+Rs,   SLA+Rs,   SLA+Rs,   SLA+Rs,   SLA+Rs,   SLA+Rs,   SLA+MHL,  SLA+Rs,
  SRA+Rs,   SRA+Rs,   SRA+Rs,   SRA+Rs,   SRA+Rs,   SRA+Rs,   SRA+MHL,  SRA+Rs,
  SRR+Rs,   SRR+Rs,   SRR+Rs,   SRR+Rs,   SRR+Rs,   SRR+Rs,   SRR+MHL,  SRR+Rs,
  SRL+Rs,   SRL+Rs,   SRL+Rs,   SRL+Rs,   SRL+Rs,   SRL+Rs,   SRL+MHL,  SRL+Rs,

  BIT+Rs,   BIT+Rs,   BIT+Rs,   BIT+Rs,   BIT+Rs,   BIT+Rs,   BIT+MHL,  BIT+Rs,   
  BIT+Rs,   BIT+Rs,   BIT+Rs,   BIT+Rs,   BIT+Rs,   BIT+Rs,   BIT+MHL,  BIT+Rs,   
  BIT+Rs,   BIT+Rs,   BIT+Rs,   BIT+Rs,   BIT+Rs,   BIT+Rs,   BIT+MHL,  BIT+Rs,   
  BIT+Rs,   BIT+Rs,   BIT+Rs,   BIT+Rs,   BIT+Rs,   BIT+Rs,   BIT+MHL,  BIT+Rs,   
  BIT+Rs,   BIT+Rs,   BIT+Rs,   BIT+Rs,   BIT+Rs,   BIT+Rs,   BIT+MHL,  BIT+Rs,   
  BIT+Rs,   BIT+Rs,   BIT+Rs,   BIT+Rs,   BIT+Rs,   BIT+Rs,   BIT+MHL,  BIT+Rs,   
  BIT+Rs,   BIT+Rs,   BIT+Rs,   BIT+Rs,   BIT+Rs,   BIT+Rs,   BIT+MHL,  BIT+Rs,   
  BIT+Rs,   BIT+Rs,   BIT+Rs,   BIT+Rs,   BIT+Rs,   BIT+Rs,   BIT+MHL,  BIT+Rs,   

  RES+Rs,   RES+Rs,   RES+Rs,   RES+Rs,   RES+Rs,   RES+Rs,   RES+MHL,  RES+Rs,   
  RES+Rs,   RES+Rs,   RES+Rs,   RES+Rs,   RES+Rs,   RES+Rs,   RES+MHL,  RES+Rs,   
  RES+Rs,   RES+Rs,   RES+Rs,   RES+Rs,   RES+Rs,   RES+Rs,   RES+MHL,  RES+Rs,   
  RES+Rs,   RES+Rs,   RES+Rs,   RES+Rs,   RES+Rs,   RES+Rs,   RES+MHL,  RES+Rs,   
  RES+Rs,   RES+Rs,   RES+Rs,   RES+Rs,   RES+Rs,   RES+Rs,   RES+MHL,  RES+Rs,   
  RES+Rs,   RES+Rs,   RES+Rs,   RES+Rs,   RES+Rs,   RES+Rs,   RES+MHL,  RES+Rs,   
  RES+Rs,   RES+Rs,   RES+Rs,   RES+Rs,   RES+Rs,   RES+Rs,   RES+MHL,  RES+Rs,   
  RES+Rs,   RES+Rs,   RES+Rs,   RES+Rs,   RES+Rs,   RES+Rs,   RES+MHL,  RES+Rs,   

  SET+Rs,   SET+Rs,   SET+Rs,   SET+Rs,   SET+Rs,   SET+Rs,   SET+MHL,  SET+Rs,   
  SET+Rs,   SET+Rs,   SET+Rs,   SET+Rs,   SET+Rs,   SET+Rs,   SET+MHL,  SET+Rs,   
  SET+Rs,   SET+Rs,   SET+Rs,   SET+Rs,   SET+Rs,   SET+Rs,   SET+MHL,  SET+Rs,   
  SET+Rs,   SET+Rs,   SET+Rs,   SET+Rs,   SET+Rs,   SET+Rs,   SET+MHL,  SET+Rs,   
  SET+Rs,   SET+Rs,   SET+Rs,   SET+Rs,   SET+Rs,   SET+Rs,   SET+MHL,  SET+Rs,   
  SET+Rs,   SET+Rs,   SET+Rs,   SET+Rs,   SET+Rs,   SET+Rs,   SET+MHL,  SET+Rs,   
  SET+Rs,   SET+Rs,   SET+Rs,   SET+Rs,   SET+Rs,   SET+Rs,   SET+MHL,  SET+Rs,   
  SET+Rs,   SET+Rs,   SET+Rs,   SET+Rs,   SET+Rs,   SET+Rs,   SET+MHL,  SET+Rs,

  /* ED = 0x200 */
  [0x243] = Iw,
  [0x24b] = Iw,
  [0x253] = Iw,
  [0x25b] = Iw,
  [0x263] = Iw,
  [0x26b] = Iw,
  [0x273] = Iw,
  [0x27b] = Iw,
};

#if 0
int z80op(int opcode) {
  rs = (op & 7);
  rd = (op >> 3) & 7;
  src = regs[rs];
  if (opcode >= 0x40 && opcode <= 0x7f) {
    if (rs == 6 && rd == 6)
      return HLT;
    if (rs == 6)
      src = cpu_read(MHL);
    regs[rd] = src;
  }
  if (opcode >= 0x80 && opcode <= 0xBF) {
    if (rs == 6)
      src = cpu_read(MHL);
    aluop(alufn[rd], 7, src);
  }
}
#endif

#define UD(x) NULL
static const char *z80dis[256*3] = {
  "nop",           "ld   bc,%nn",   "ld   (bc),a",   "inc  bc",       "inc  b",        "dec  b",        "ld   b,%n",        "rlca",
  "ex   af,af",    "add  %hl,bc",   "ld   a,(bc)",   "dec  bc",       "inc  c",        "dec  c",        "ld   c,%n",        "rrca",
  "djnz %r",       "ld   de,%nn",   "ld   (de),a",   "inc  de",       "inc  d",        "dec  d",        "ld   d,%n",        "rla",
  "jr   %r",       "add  %hl,de",   "ld   a,(de)",   "dec  de",       "inc  e",        "dec  e",        "ld   e,%n",        "rra",
  "jr   nz,%r",    "ld   %hl,%nn",  "ld   (%nn),%hl","inc  %hl",      "inc  %h",       "dec  %h",       "ld   %h,%n",       "daa",
  "jr   z,%r",     "add  %hl,%hl",  "ld   %hl,(%nn)","dec  %hl",      "inc  %l",       "dec  %l",       "ld   %l,%n",       "cpl",
  "jr   nc,%r",    "ld   sp,%nn",   "ld   (%nn),a",  "inc  sp",       "inc  (%phl)",   "dec  (%phl)",   "ld   (%phl),%n",   "scf",
  "jr   c,%r",     "add  %hl,sp",   "ld   a,(%nn)",  "dec  sp",       "inc  a",        "dec  a",        "ld   a,%n",        "ccf",

  "ld   b,%Rs",    "ld   b,%Rs",    "ld   b,%Rs",    "ld   b,%Rs",    "ld   b,%h",     "ld   b,%l",     "ld   b,(%phl)",    "ld   b,a",
  "ld   c,%Rs",    "ld   c,%Rs",    "ld   c,%Rs",    "ld   c,%Rs",    "ld   c,%h",     "ld   c,%l",     "ld   c,(%phl)",    "ld   c,a",
  "ld   d,%Rs",    "ld   d,%Rs",    "ld   d,%Rs",    "ld   d,%Rs",    "ld   d,%h",     "ld   d,%l",     "ld   d,(%phl)",    "ld   d,a",
  "ld   e,%Rs",    "ld   e,%Rs",    "ld   e,%Rs",    "ld   e,%Rs",    "ld   e,%h",     "ld   e,%l",     "ld   e,(%phl)",    "ld   e,a",
  "ld   h,%Rs",    "ld   h,%Rs",    "ld   h,%Rs",    "ld   h,%Rs",    "ld   h,%h",     "ld   h,%l",     "ld   h,(%phl)",    "ld   h,a",
  "ld   %l,%Rs",   "ld   %l,%Rs",   "ld   %l,%Rs",   "ld   %l,%Rs",   "ld   %l,%h",    "ld   l,%l",     "ld   l,(%phl)",    "ld   l,a",
  "ld   (%phl),b", "ld   (%phl),c", "ld   (%phl),d", "ld   (%phl),e", "ld   (%phl),h", "ld   (%phl),l", "hlt",              "ld   (%phl),a",
  "ld   a,%Rs",    "ld   a,%Rs",    "ld   a,%Rs",    "ld   a,%Rs",    "ld   a,%h",     "ld   a,%l",     "ld   a,(%phl)",    "ld   a,a",

  "add  a,%Rs",    "add  a,%Rs",    "add  a,%Rs",    "add  a,%Rs",    "add  a,%h",     "add  a,%l",     "add  (%phl)",      "add  a",
  "adc  a,%Rs",    "adc  a,%Rs",    "adc  a,%Rs",    "adc  a,%Rs",    "adc  a,%h",     "adc  a,%l",     "adc  (%phl)",      "adc  a",
  "sub  b",        "sub  c",        "sub  d",        "sub  e",        "sub  %h",       "sub  %l",       "sub  (%phl)",      "sub  a",
  "sbc  b",        "sbc  c",        "sbc  d",        "sbc  e",        "sbc  %h",       "sbc  %l",       "sbc  (%phl)",      "sbc  a",
  "and  b",        "and  c",        "and  d",        "and  e",        "and  %h",       "and  %l",       "and  (%phl)",      "and  a",
  "xor  b",        "xor  c",        "xor  d",        "xor  e",        "xor  %h",       "xor  %l",       "xor  (%phl)",      "xor  a",
  "or   b",        "or   c",        "or   d",        "or   e",        "or   %h",       "or   %l",       "or   (%phl)",      "or   a",
  "cp   b",        "cp   c",        "cp   d",        "cp   e",        "cp   %h",       "cp   %l",       "cp   (%phl)",      "cp   a",

  "retnz",         "pop  bc",       "jp   nz,%nn",  "jp   %nn",      "call nz,%nn",   "push bc",       "add  a,%n",        "rst  00",
  "retz",          "ret",           "jpz  z,%nn",   "CB____",        "call z,%nn",    "call %nn",      "adc  a,%n",        "rst  08",
  "retnc",         "pop  de",       "jp   nc,%nn",  "out (%n),a",    "call nc,%nn",   "push de",       "sub  a,%n",        "rst  10",
  "retc",          "exx",           "jp   c,%nn",   "in  a, (%n)",   "call c,%nn",    "DD__IX",        "sbc  a,%n",        "rst  18",
  "retpo",         "pop  %hl",      "jp   po,%nn",  "ex  (sp),%hl",  "call po,%nn",   "push %hl",      "and  a,%n",        "rst  20",
  "retpe",         "jp   (%hl)",    "jp   pe,%nn",  "ex  de, hl",    "call pe,%nn",   "ED80xx",        "xor  a,%n",        "rst  28",
  "retp",          "pop  af",       "jp   p,%nn",   "di",            "call p, %nn",   "push af",       "or   a,%n",        "rst  30",
  "retm",          "ld   sp,%hl",   "jp   m,%nn",   "ei",            "call m,%nn",    "FD__IY",        "cp   a,%n",        "rst  38",

  /* CB 00 @ 0x100 */
  "rlc  b",        "rlc  c",        "rlc  d",        "rlc  e",        "rlc  %h",       "rlc  %l",       "rlc  (%phl)",     "rlc  a",
  "rrc  b",        "rrc  c",        "rrc  d",        "rrc  e",        "rrc  %h",       "rrc  %l",       "rrc  (%phl)",     "rrc  a",
  "rl   b",        "rl   c",        "rl   d",        "rl   e",        "rl   %h",       "rl   %l",       "rl   (%phl)",     "rl   a",
  "rr   b",        "rr   c",        "rr   d",        "rr   e",        "rr   %h",       "rr   %l",       "rr   (%phl)",     "rr   a",
  "sla  b",        "sla  c",        "sla  d",        "sla  e",        "sla  %h",       "sla  %l",       "sla  (%phl)",     "sla  a",
  "sra  b",        "sra  c",        "sra  d",        "sra  e",        "sra  %h",       "sra  %l",       "sra  (%phl)",     "sra  a",
  "sll  b",        "sll  c",        "sll  d",        "sll  e",        "sll  %h",       "sll  %l",       "sll  (%phl)",     "sll  a",
  "srl  b",        "srl  c",        "srl  d",        "srl  e",        "srl  %h",       "srl  %l",       "srl  (%phl)",     "srl  a",

  /* CB 40 */
  "bit  %b,%Rs",   "bit  %b,%Rs",   "bit  %b,%Rs",   "bit  %b,%Rs",   "bit  %b,%h",    "bit  %b,%l",    "bit  (%phl)",     "bit  %b,a",
  "bit  %b,%Rs",   "bit  %b,%Rs",   "bit  %b,%Rs",   "bit  %b,%Rs",   "bit  %b,%h",    "bit  %b,%l",    "bit  (%phl)",     "bit  %b,a",
  "bit  %b,%Rs",   "bit  %b,%Rs",   "bit  %b,%Rs",   "bit  %b,%Rs",   "bit  %b,%h",    "bit  %b,%l",    "bit  (%phl)",     "bit  %b,a",
  "bit  %b,%Rs",   "bit  %b,%Rs",   "bit  %b,%Rs",   "bit  %b,%Rs",   "bit  %b,%h",    "bit  %b,%l",    "bit  (%phl)",     "bit  %b,a",
  "bit  %b,%Rs",   "bit  %b,%Rs",   "bit  %b,%Rs",   "bit  %b,%Rs",   "bit  %b,%h",    "bit  %b,%l",    "bit  (%phl)",     "bit  %b,a",
  "bit  %b,%Rs",   "bit  %b,%Rs",   "bit  %b,%Rs",   "bit  %b,%Rs",   "bit  %b,%h",    "bit  %b,%l",    "bit  (%phl)",     "bit  %b,a",
  "bit  %b,%Rs",   "bit  %b,%Rs",   "bit  %b,%Rs",   "bit  %b,%Rs",   "bit  %b,%h",    "bit  %b,%l",    "bit  (%phl)",     "bit  %b,a",
  "bit  %b,%Rs",   "bit  %b,%Rs",   "bit  %b,%Rs",   "bit  %b,%Rs",   "bit  %b,%h",    "bit  %b,%l",    "bit  (%phl)",     "bit  %b,a",

  /* CB 80 */
  "res  %b,%Rs",   "res  %b,%Rs",   "res  %b,%Rs",   "res  %b,%Rs",   "res  %b,%h",    "res  %b,%l",    "res  (%phl)",     "res  %b,a",
  "res  %b,%Rs",   "res  %b,%Rs",   "res  %b,%Rs",   "res  %b,%Rs",   "res  %b,%h",    "res  %b,%l",    "res  (%phl)",     "res  %b,a",
  "res  %b,%Rs",   "res  %b,%Rs",   "res  %b,%Rs",   "res  %b,%Rs",   "res  %b,%h",    "res  %b,%l",    "res  (%phl)",     "res  %b,a",
  "res  %b,%Rs",   "res  %b,%Rs",   "res  %b,%Rs",   "res  %b,%Rs",   "res  %b,%h",    "res  %b,%l",    "res  (%phl)",     "res  %b,a",
  "res  %b,%Rs",   "res  %b,%Rs",   "res  %b,%Rs",   "res  %b,%Rs",   "res  %b,%h",    "res  %b,%l",    "res  (%phl)",     "res  %b,a",
  "res  %b,%Rs",   "res  %b,%Rs",   "res  %b,%Rs",   "res  %b,%Rs",   "res  %b,%h",    "res  %b,%l",    "res  (%phl)",     "res  %b,a",
  "res  %b,%Rs",   "res  %b,%Rs",   "res  %b,%Rs",   "res  %b,%Rs",   "res  %b,%h",    "res  %b,%l",    "res  (%phl)",     "res  %b,a",
  "res  %b,%Rs",   "res  %b,%Rs",   "res  %b,%Rs",   "res  %b,%Rs",   "res  %b,%h",    "res  %b,%l",    "res  (%phl)",     "res  %b,a",

  /* CB C0 */
  "set  %b,%Rs",   "set  %b,%Rs",   "set  %b,%Rs",   "set  %b,%Rs",   "set  %b,%h",    "set  %b,%l",    "set  (%phl)",     "set  %b,a",
  "set  %b,%Rs",   "set  %b,%Rs",   "set  %b,%Rs",   "set  %b,%Rs",   "set  %b,%h",    "set  %b,%l",    "set  (%phl)",     "set  %b,a",
  "set  %b,%Rs",   "set  %b,%Rs",   "set  %b,%Rs",   "set  %b,%Rs",   "set  %b,%h",    "set  %b,%l",    "set  (%phl)",     "set  %b,a",
  "set  %b,%Rs",   "set  %b,%Rs",   "set  %b,%Rs",   "set  %b,%Rs",   "set  %b,%h",    "set  %b,%l",    "set  (%phl)",     "set  %b,a",
  "set  %b,%Rs",   "set  %b,%Rs",   "set  %b,%Rs",   "set  %b,%Rs",   "set  %b,%h",    "set  %b,%l",    "set  (%phl)",     "set  %b,a",
  "set  %b,%Rs",   "set  %b,%Rs",   "set  %b,%Rs",   "set  %b,%Rs",   "set  %b,%h",    "set  %b,%l",    "set  (%phl)",     "set  %b,a",
  "set  %b,%Rs",   "set  %b,%Rs",   "set  %b,%Rs",   "set  %b,%Rs",   "set  %b,%h",    "set  %b,%l",    "set  (%phl)",     "set  %b,a",
  "set  %b,%Rs",   "set  %b,%Rs",   "set  %b,%Rs",   "set  %b,%Rs",   "set  %b,%h",    "set  %b,%l",    "set  (%phl)",     "set  %b,a",

  /* ED 00 @ 0x200 */
  [0x240] =
  "in  b,(c)",     "out  (c),b",    "sbc  hl,bc",    "ld  (%nn),bc",  "neg",           "retn",          "im   0",          "ld   i,a",
  "in  c,(c)",     "out  (c),c",    "adc  hl,bc",    "ld  bc,(%nn)",  NULL,            "reti",          NULL,              "ld   r,a",
  "in  d,(c)",     "out  (c),d",    "sbc  hl,de",    "ld  (%nn),de",  NULL,            NULL,            "im   1",          "ld   a,i",
  "in  e,(c)",     "out  (c),e",    "adc  hl,de",    "ld  de,(%nn)",  NULL,            NULL,            "im   2",          "ld   a,r",
  "in  h,(c)",     "out  (c),h",    "sbc  hl,hl",    "ld  (%nn),hl",  NULL,            NULL,            NULL,              "rrd",
  "in  l,(c)",     "out  (c),l",    "adc  hl,hl",    "ld  hl,(%nn)",  NULL,            NULL,            NULL,              "rld",
  UD("in (f),c"),  UD("out (c),0"), "sbc  hl,sp",    "ld  (%nn),sp",  NULL,            NULL,            NULL,              NULL,
  "in  a,(c)",     "out  (c),a",    "adc  hl,sp",    "ld  sp,(%nn)",  NULL,            NULL,            NULL,              NULL,
  [0x2a0] =
  "ldi",           "cpi",           "ini",           "outi",          NULL,            NULL,            NULL,              NULL,
  "ldd",           "cpd",           "ind",           "outd",          NULL,            NULL,            NULL,              NULL,
  "ldir",          "cpir",          "inir",          "otir",          NULL,            NULL,            NULL,              NULL,
  "lddr",          "cpdr",          "indr",          "otdr",          NULL,            NULL,            NULL,              NULL,
};

/* LDI:  LD (DE),(HL), DE++, PHL+, BC--
 * LDIR: loop while BC!=0, LDI
 * LDD:  LD (DE),(HL), DE--, HL--, BC--
 * LDDR: loop while BC!=0, LDD
 * CPI:  CP (HL); INC HL; DEC BC
 * CPD:  CP (HL); DEC HL; DEC BC
 * INI:  IN (HL),(C); HL--; B--
 * IND:  IN (HL),(C); HL--; B--
 */
const char *__z80dis(int op, int pfx, int imm)
{
  static char dstr[128];
  char *dst = dstr;
  const char *src = z80dis[op];

  while (*src) {
    if (!strncmp(src, "%phl", 4) && pfx != 0) {
      dst += snprintf(dst, 32, "%s+%d",
		      (pfx == 0xdd) ? "ix" : "iy",
		      imm);
      src += 4;
      continue;
    }
    if (replace(&src, "%nn", &dst, "0x%.4x", imm) ||
	replace(&src, "%n", &dst, "0x%.2x", imm) ||
	replace(&src, "%phl", &dst, "hl") || 
	replace(&src, "%hl", &dst, "%s",
		pfx == 0xdd ? "ix" :
		pfx == 0xfd ? "iy" :
		"hl")) {
      continue;
    }
    *dst++ = *src++;
  }
  *dst = 0;
  return dstr;
}

/* nn
 * CB nn
 * DD nn
 * DD CB nn
 * ED nn
 * FD nn
 * FD CB nn
 */
void _zdis(uint8_t *buf, int len, int off = 0) {
  dstk stk(len, printf);
  int nxt[2], op, pfx, fn, imm;
  uint8_t *pc;
  
  stk.push(off, 1, dstk::PENDING, "init");
  while ((off = stk.pop()) != -1) {
    do {
      pfx = 0;
      pc = &buf[off];
      op = *pc++;
      if (op == 0xdd) {
	pfx = op; // ix
	op = *pc++;
      }
      else if (op == 0xfd) {
	pfx = op; // iy
	op = *pc++;
      }
      if (op == 0xcb) {
	op = 0x100 + *pc++;
      }
      else if (op == 0xed) {
	op = 0x200 + *pc++;
      }
      imm = 0;
      if (pfx) {
	if ((z80fn[op] & SRC_MASK) == MHL ||
	    (z80fn[op] & 0xff) == LDMHL) { 
	  imm = *pc++;
	}
      }
      nxt[0] = off;
      nxt[1] = -1;
      fn = z80fn[op];
      if ((fn & 0xff) == JR) {
	if (op == 0x18)
	  nxt[0] = -1;
	nxt[1] = off+1+(int8_t)*pc++;
	imm = nxt[1];
      }
      else if ((fn & 0xff) == JP) {
	if (op == 0xc3)
	  nxt[0] = -1;
	nxt[1] = *(uint16_t *)pc;
	imm = nxt[1];
	pc += 2;
      }
      else if ((fn & 0xff) == JPHL) {
	nxt[0] = -1;
      }
      else if ((fn & 0xff) == CALL) {
	nxt[1] = *(uint16_t *)pc;
	imm = nxt[1];
	pc += 2;
      }
      if ((fn & 0xF000) == Ib) {
	imm = *pc;
	pc++;
      }
      if ((fn & 0xF000) == Iw) {
	imm = *(uint16_t *)pc;
	pc+=2;
      }
      if (nxt[0] == off) {
	nxt[0] += (pc - &buf[off]);
      }
      printf("%.4x: %.2x/%.4x %s\n", off, pfx, op, __z80dis(op, pfx, imm));
      stk.push(off, (pc - &buf[off]), dstk::CODE, "code");
      for (int i = 0; i < 2; i++) {
	if (nxt[i] < len && nxt[i] > 0) {
	  stk.push(nxt[i], 1, dstk::PENDING, "next");
	}
      }
      off += (pc - &buf[off]);
    } while (nxt[0] != -1 && nxt[1] == -1);
  }
}

struct z80_instr {
  int pfx = 0;
  int op = 0;
  int fn = 0;
  int sz = 0;

  int cys = 0;
  int args[3];
  const char *dis;
  int fetch(uint8_t *buf) {
    pfx = 0;
    op = *buf++;
    sz = z80sz[op];
    fn = z80fn[op];
    dis = z80dis[op];
    if (op == 0xdd) {
      pfx = op;
      op = *buf++;
      sz = z80_fdsz[op];
    }
    else if (op == 0xfd) {
      pfx = op;
      op = *buf++;
      sz = z80_fdsz[op];
    }
    else if (op == 0xed) {
      pfx = op;
      op = *buf++;
    }
    if (op == 0xcb) {
      int idx = *buf++;
      dis = z80dis[0x100 + idx];
    }
    printf("%.2x/%.2x %.8x [%.4x %.4x %.4x]\n",
	   pfx, op, fn & SRC_MASK,
	   args[0], args[1], args[2]);
    return 0;
  };
};

struct cpu_z80 {
  uint8_t  *ram;

  int bus_ack;
  int bus_req;
  int reset;
  
  /* CPU registers */
  uint8_t  regs[8];
  uint8_t &B = regs[0];
  uint8_t &C = regs[1];
  uint8_t &D = regs[2];
  uint8_t &E = regs[3];
  uint8_t &H = regs[4];
  uint8_t &L = regs[5];
  uint8_t &F = regs[5];
  uint8_t &A = regs[7];
  uint16_t PC;
  uint16_t SP;

  /* flags register  */
  bool flags[8];
  bool &nf = flags[7];
  bool &zf = flags[6];
  bool &hf = flags[4];
  bool &pf = flags[2];
  bool &af = flags[1];
  bool &cf = flags[0];

  int push16(uint16_t nv) {
    return 0;
  }
  int pop16() {
    return 0;
  };
  int _add(uint8_t& dst, uint8_t a, uint8_t b, uint8_t c) {
    dst = a + b + c;
    return 0;
  }
  int _shl(uint8_t& dst, uint8_t v, bool lsb) {
    cf = !!(v & 0x80);
    dst = (v << 1) | lsb;
    return 0;
  };
  int _shr(uint8_t &dst, uint8_t v, bool msb) {
    cf = !!(v & 0x01);
    dst = (v >> 1) | (msb << 7);
    return 0;
  };
  int getrp(int n) const {
    const int rp = (n >> 4) & 3;
    return (regs[rp] << 8) + regs[rp+1];
  }
  int setrp(int n, int v) {
    const int rp = (n >> 4) & 3;
    regs[rp+0] = v >> 8;
    regs[rp+1] = v;
    return 0;
  }
  int setpc(bool cond, int newpc, bool push = false) {
    if (!cond)
      return 0;
    if (push) {
      push16(PC);
    }
    PC = newpc;
    return 0;
  };
  int setres(uint8_t& dst, uint8_t src, int flag = 0) {
    dst = src;
    return src + flag;
  };
  int _eval(int op, int fn) {
    uint8_t rd = (op >> 3) & 7;
    uint8_t rs = (op >> 0) & 7;
    uint8_t& Rd = regs[rd];
    uint8_t& Rs = regs[rs];
    int src = Rs;

    switch (fn & 0xff) {
    case INCRd: return setres(Rd, Rd + 1);
    case DECRd: return setres(Rd, Rd - 1);
    case INCRp: return setrp(op, getrp(op) + 1);
    case DECRp: return setrp(op, getrp(op) - 1);
    case POPRp: return setrp(op, pop16());
    case PUSHRp: return push16(getrp(op));

    case ADD: return _add(A, A, src, 0);
    case ADC: return _add(A, A, src, cf);
    case SUB: return _add(A, A, -src, 0);
    case SBC: return _add(A, A, -src, -cf);
    case CP:  return _add(rs, A, -src, 0);   //dummy store
      
    case AND: return setres(A, A & src, SNZ);
    case XOR: return setres(A, A ^ src, SNZ);
    case OR:  return setres(A, A | src, SNZ);
    case SET: return setres(Rs, src | (1L << rd), SNZ);
    case RES: return setres(Rs, src & ~(1L << rd), SNZ);
    case LDR: return setres(Rd, src);
    case LDRp:return setrp(op, src);

      /* Show shift */
    case RLC: return _shl(Rs, src, !!(src & 0x80));
    case RRC: return _shr(Rs, src, !!(src & 0x01));
    case RL:  return _shl(Rs, src, cf);
    case RR:  return _shr(Rs, src, cf);
    case SLA: return _shl(Rs, src, 0);
    case SRA: return _shr(Rs, src, !!(src & 0x80));
    case SLL: return _shl(Rs, src, 1);
    case SRL: return _shr(Rs, src, 0);

      /* Shift A */
    case RLCA:return _shl(A, A, !!(A & 0x80));
    case RRCA:return _shr(A, A, !!(A & 0x01));
    case RLA: return _shl(A, A, cf);
    case RRA: return _shr(A, A, cf);

    case HLT:
    case JR:   return setpc(testcond(op), 0, false);
    case JP:   return setpc(testcond(op), 0, false);
    case CALL: return setpc(testcond(op), 0, true);
    case RET:  
    case NOP:  return 0;
    case RST:  return setpc(true, (op & 0x38), false);

    case SCF: cf = true; nf = hf = false; return 0;
    case CCF: cf = !cf; nf = hf = false; return 0;
    case CPL: A = ~A; nf = hf = true; return 0;
    }
    return -1;
  };
};

static const char *z80_dis(z80_instr& i, uint8_t *buf, int off)
{
  static char dstr[64], *dst;
  const char *lf, *hf, *hlf, *src;
  uint8_t *b = &buf[off];
  
  src = i.dis;
  if (buf[off] == 0xcb) {
    int idx = buf[++off] + 0x100;
    src = z80dis[idx];
  }
  if (i.pfx == 0xdd) {
    lf = "ixl";
    hf = "ixh";
    hlf = "ix";
  }
  else if (i.pfx == 0xfd) {
    lf = "iyl";
    hf = "iyh";
    hlf = "iy";
  }
  else {
    lf = "l";
    hf = "h";
    hlf = "hl";
  }
  dst = dstr;
  while (*src) {
    if (replace(&src, "%nn", &dst, "0x%.2x%.2x", b[2], b[1]) ||
	replace(&src, "%n", &dst, "0x%.2x", b[1]) ||
	replace(&src, "%r", &dst, "0x%.4x", off + (int8_t)b[1] + 2) ||
	replace(&src, "%b", &dst, "%x", (buf[off] >> 3) & 3) ||
	replace(&src, "%hl", &dst, "%s", hlf) ||
	replace(&src, "%h", &dst, "%s", hf) ||
	replace(&src, "%l", &dst, "%s", lf)) {
      continue;
    }
    if (i.pfx == 0 && replace(&src, "+", &dst, "")) {
      continue;
    }
    if (i.pfx != 0 && replace(&src, "+", &dst, "+%x", b[2])) {
      off += 2;
      continue;
    }
    *dst++ = *src++;
  }
  *dst = 0;
  return dstr;
}
#endif
