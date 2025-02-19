#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <inttypes.h>
#include <assert.h>
#include <stddef.h>
#include <time.h>
#include <map>

#include "dstk.h"
#include "util.h"
#include "bus.h"
#include "cpu.h"
#include "gr.h"
#include "n64_crc.h"

/* NEC VR4300 (MIPS R4300i) @ 93.75 MHz
 * CP1 = FPU, IEEE754
 * 4.5MB RAM, 9-bit bus (only GPU accesses .5)
 * CP0 = TLB
 */
#define xx(m) get32be(&(m))

static uint8_t *_emit(uint8_t *src, int nb, ...) {
  va_list ap;
  va_start(ap, nb);
  while (nb--) {
    *src++ = va_arg(ap, int);
  }
  return src;
}

constexpr auto min(auto a, auto b) {
  return (a < b) ? a : b;
};

int trace = 0;

// https://github.com/Dillonb/n64-tests/blob/master/src/templates/IMMEDIATE.tmpl
// https://github.com/Dillonb/n64-tests/tree/master/src
// https://github.com/rasky/r64emu
// https://github.com/PeterLemon/N64
// https://alinacierdem.com/drawing-triangles-on-n64/
// https://n64brew.dev/wiki/MIPS_III_instructions
void mips_syscall(uint32_t call) {
}
void mips_copr(uint32_t op, int id) {
}
void mips_lwc(uint16_t r, uint16_t v, int n, int sz) {
}

static const char *regname[] = {
  "zero", "at", "v0", "v1", "a0", "a1", "a2", "a3",
  "t0",   "t1", "t2", "t3", "t4", "t5", "t6", "t7",
  "s0",   "s1", "s2", "s3", "s4", "s5", "s6", "s7",
  "t8",   "t9", "k0", "k1", "gp", "sp", "fp", "ra"
};

/*
 *         r21   r16   r11   r6
 *  000000.sssss.ttttt.ddddd.aaaaa.oooooo register
 *  000001.ttttt.ooooo.iiiii.iiiii.iiiiii branch
 *  oooooo.sssss.ttttt.iiiii.iiiii.iiiiii immediate
 *  oooooo.iiiii.iiiii.iiiii.iiiii.iiiiii jump
 *  11xxcc.sssss.ttttt.iiiii.iiiii.iiiiii coproc lwc/swc
 *
 * CPU::regInstrs (oooooo == 000000)
 *     sll,  unk,  _srl,  sra,  sllv,    unk,   _srlv,   srav,  // 0x00-0x07
 *     jr,   jalr,  unk,  unk,  syscall, break_, unk,    unk,   // 0x08-0x0F
 *     mfhi, mthi,  mflo, mtlo,_dsllv,   unk,    dsrlv,  dsrav, // 0x10-0x17
 *     mult, multu, div,  divu, dmult,   dmultu, ddiv,   ddivu, // 0x18-0x1F
 *    _add, _addu,  sub,  subu,_and,    _or,    _xor,   _nor,   // 0x20-0x27
 *     unk,  unk,  _slt, _sltu,_dadd,   _daddu,  dsub,   dsubu, // 0x28-0x2F
 *     unk,  unk,   unk,  unk,  unk,     unk,    unk,    unk,   // 0x30-0x37
 *    _dsll, unk,   dsrl, dsra, dsll32,  unk,    dsrl32, dsra32 // 0x38-0x3F
 *
 *     oooooo.ttttt.sssss.iiiii.iiiii.iiiiii

 * CPU::immInstrs (oooooo >= 1)
 *     nullptr, nullptr, j,    jal,  _beq, _bne, _blez, _bgtz,  // 0x00-0x07
 *    _addi,   _addiu,  _slti,_sltiu,_andi,_ori, _xori, _lui,   // 0x08-0x0F
 *     cop0,    cop1,    unk,  unk,   beql, bnel, blezl, bgtzl, // 0x10-0x17
 *    _daddi,  _daddiu,  ldl,  ldr,   unk,  unk,  unk,   unk,   // 0x18-0x1F
 *    _lb,     _lh,      lwl, _lw,   _lbu, _lhu,  lwr,  _lwu,   // 0x20-0x27
 *    _sb,     _sh,      swl, _sw,    sdl,  sdr,  swr,   cache, // 0x28-0x2F
 *     unk,     lwc1,    unk,  unk,   unk,  ldc1, unk,  _ld,    // 0x30-0x37
 *     unk,     swc1,    unk,  unk,   unk,  sdc1, unk,  _sd     // 0x38-0x3F
 * 
 * CPU::extInstrs
 *     bltz,   bgez,   bltzl,   bgezl,   unk, unk, unk, unk, // 0x00-0x07
 *     unk,    unk,    unk,     unk,     unk, unk, unk, unk, // 0x08-0x0F
 *     bltzal, bgezal, bltzall, bgezall, unk, unk, unk, unk, // 0x10-0x17
 *     unk,    unk,    unk,     unk,     unk, unk, unk, unk  // 0x18-0x1F
 * 
 * CPU::CP0
 *  unk,  tlbr, tlbwi, unk, unk, unk, unk, unk, // 0x00-0x07
 *  tlbp, unk,  unk,   unk, unk, unk, unk, unk, // 0x08-0x0F
 *  unk,  unk,  unk,   unk, unk, unk, unk, unk, // 0x10-0x17
 *  eret, unk,  unk,   unk, unk, unk, unk, unk, // 0x18-0x1F
 *  unk,  unk,  unk,   unk, unk, unk, unk, unk, // 0x20-0x27
 *  unk,  unk,  unk,   unk, unk, unk, unk, unk, // 0x28-0x2F
 *  unk,  unk,  unk,   unk, unk, unk, unk, unk, // 0x30-0x37
 *  unk,  unk,  unk,   unk, unk, unk, unk, unk  // 0x38-0x3F
 *
 * CPU::CP1
 *   addS,    subS,    mulS,   divS,    sqrtS,   absS,    movS,   negS,    // 0x00-0x07
 *   roundLS, truncLS, ceilLS, floorLS, roundWS, truncWS, ceilWS, floorWS, // 0x08-0x0F
 *   unk,     unk,     unk,    unk,     unk,     unk,     unk,    unk,     // 0x10-0x17
 *   unk,     unk,     unk,    unk,     unk,     unk,     unk,    unk,     // 0x18-0x1F
 *   unk,     cvtDS,   unk,    unk,     cvtWS,   cvtLS,   unk,    unk,     // 0x20-0x27
 *   unk,     unk,     unk,    unk,     unk,     unk,     unk,    unk,     // 0x28-0x2F
 *   cf,      cunS,    ceqS,   cueqS,   coltS,   cultS,   coleS,  culeS,   // 0x30-0x37
 *   cf,      cunS,    ceqS,   cueqS,   coltS,   cultS,   coleS,  culeS    // 0x38-0x3F
 *
 * CPU::CP1
 *   addD,    subD,    mulD,   divD,    sqrtD,   absD,    movD,   negD,    // 0x00-0x07
 *   roundLD, truncLD, ceilLD, floorLD, roundWD, truncWD, ceilWD, floorWD, // 0x08-0x0F
 *   unk,     unk,     unk,    unk,     unk,     unk,     unk,    unk,     // 0x10-0x17
 *   unk,     unk,     unk,    unk,     unk,     unk,     unk,    unk,     // 0x18-0x1F
 *   cvtSD,   unk,     unk,    unk,     cvtWD,   cvtLD,   unk,    unk,     // 0x20-0x27
 *   unk,     unk,     unk,    unk,     unk,     unk,     unk,    unk,     // 0x28-0x2F
 *   cf,      cunD,    ceqD,   cueqD,   coltD,   cultD,   coleD,  culeD,   // 0x30-0x37
 *   cf,      cunD,    ceqD,   cueqD,   coltD,   cultD,   coleD,  culeD    // 0x38-0x3F
 *  
 * RSP::CP2
 *   vmulf, vmulu, unk,   unk,  vmudl, vmudm, vmudn, vmudh, // 0x00-0x07
 *   vmacf, vmacu, unk,   unk,  vmadl, vmadm, vmadn, vmadh, // 0x08-0x0F
 *   vadd,  vsub,  unk,   vabs, vaddc, vsubc, unk,   unk,   // 0x10-0x17
 *   unk,   unk,   unk,   unk,  unk,   vsar,  unk,   unk,   // 0x18-0x1F
 *   vlt,   veq,   vne,   vge,  vcl,   vch,   vcr,   vmrg,  // 0x20-0x27
 *   vand,  vnand, vor,   vnor, vxor,  vnxor, unk,   unk,   // 0x28-0x2F
 *   vrcp,  vrcpl, vrcph, vmov, vrsq,  vrsql, vrcph, unk,   // 0x30-0x37
 *   unk,   unk,   unk,   unk,  unk,   unk,   unk,   unk    // 0x38-0x3F
 *
 * RDP::commands
 * unknown,      unknown,       unknown,       unknown,       // 0x00-0x03
 * unknown,      unknown,       unknown,       unknown,       // 0x04-0x07
 * triangle,     triDepth,      triTexture,    triDepthTex,   // 0x08-0x0B
 * triShade,     triDepthSha,   triShadeTex,   triDepShaTex,  // 0x0C-0x0F
 * unknown,      unknown,       unknown,       unknown,       // 0x10-0x13
 * unknown,      unknown,       unknown,       unknown,       // 0x14-0x17
 * unknown,      unknown,       unknown,       unknown,       // 0x18-0x1B
 * unknown,      unknown,       unknown,       unknown,       // 0x1C-0x1F
 * unknown,      unknown,       unknown,       unknown,       // 0x20-0x23
 * texRectangle, unknown,       unknown,       unknown,       // 0x24-0x27
 * unknown,      syncFull,      unknown,       unknown,       // 0x28-0x2B
 * unknown,      setScissor,    unknown,       setOtherModes, // 0x2C-0x2F
 * loadTlut,     unknown,       setTileSize,   loadBlock,     // 0x30-0x33
 * loadTile,     setTile,       fillRectangle, setFillColor,  // 0x34-0x37
 * setFogColor,  setBlendColor, setPrimColor,  setEnvColor,   // 0x38-0x3B
 * setCombine,   setTexImage,   setZImage,     setColorImage  // 0x3C-0x3F
 */

/* NEC VR4300 @ 93.75 MHz */
template <uint32_t bit, uint32_t mask>
uint32_t extract(uint32_t v) {
  return ((v >> bit) & mask);
};

struct cpu;
typedef int64_t val_t;

uint32_t jmpslot[2];

enum {
  iJ = 0x002,
  iJAL,
  iBEQ,
  iBNE,
  iBLEZ,
  iBGTZ,
  iADDI, // x
  iADDIU, // x
  iSLTI,
  iSLTIU,
  iANDI, // x
  iORI,  // x
  iXORI, // x
  iLUI,  // x

  iBEQL = 0x014,
  iBNEL  = 0x015,
  iBLEZL = 0x016,
  iBGTZL = 0x017,
  iDADDI = 0x018, // x
  iDADDIU = 0x019, // x
  iLDL = 0x01a,
  iLDR = 0x01b,

  iLB = 0x20, // x
  iLH, // x
  iLWL,
  iLW, // x
  iLBU, // x
  iLHU, // x
  iLWR,
  iLWU, // x
  iSB,
  iSH,
  iSWL,
  iSW,
  iSDL,
  iSDR,
  iSWR,
  iCACHE,

  iLD = 0x37, // x
  iSD = 0x3F,

  iSLL = 0x100, // x
  iSRL = 0x102, // x
  iSRA,  // x
  iSLLV, // x
  iSRLV = 0x106, // x
  iSRAV, // x
  iJR,
  iJALR,
  iSYSCALL = 0x10c,
  iBREAK,

  iMFHI  = 0x110, // x
  iMTHI  = 0x111, // x
  iMFLO  = 0x112, // x
  iMTLO  = 0x113, // x
  iDSLLV = 0x114, // x
  iDSRLV = 0x116, // x
  iDSRAV = 0x117,

  iMULT,
  iMULTU,
  iDIV,
  iDIVU,
  iDMULT,
  iDMULTU,
  iDDIV,
  iDDIVU,
  iADD,  // x
  iADDU, // x
  iSUB,  // x
  iSUBU, // x
  iAND,  // x
  iOR,   // x
  iXOR,  // x
  iNOR,  // x

  iSLT = 0x12a,
  iSLTU   = 0x12b,
  iDADD   = 0x12c, // x
  iDADDU  = 0x12d, // x
  iDSUB   = 0x12e, // x
  iDSUBU  = 0x12f, // x.fail
  iDSLL   = 0x138,
  iDSRL   = 0x13a,
  iDSRA   = 0x13b,
  iDSLL32 = 0x13c,
  iDSRL32 = 0x13e,
  iDSRA32 = 0x13f,
};

struct cpu {
  val_t R[33] = {};
  val_t &LR = R[31];
  val_t &PC = R[32];
  val_t mhi, mlo;
  
  uint32_t j26(uint32_t npc) {
    npc &= 0x03ffffff;
    return (PC & 0xF0000000) + (npc << 2);
  };
  uint32_t j16(uint32_t npc) {
    return PC + (npc << 2);
  };
  void setpc(bool cond, uint64_t npc, val_t *link = NULL) {
    if (!cond) {
      return;
    }
    jmpslot[1] = npc;
    if (link) {
      *link = PC + 4;
    }
  };
  void showregs() {
    for (int i = 0; i < 32; i++) {
      printf("%4s:%.16llx ", regname[i], R[i]);
      if ((i & 7) == 7)
	printf("\n");
    };
  };

  /* oooooo.sssss.ttttt.ddddd.00000.ooooo */
  void mfhi(val_t& Rd, val_t Rt, val_t Rs) {
    Rd = mhi;
  }
  void mflo(val_t& Rd, val_t Rt, val_t Rs) {
    Rd = mlo;
  }
  void mthi(val_t& Rd, val_t Rt, val_t Rs) {
    mhi = Rs;
  }
  void mtlo(val_t& Rd, val_t Rt, val_t Rs) {
    mlo = Rs;
  }
  
  void mov(val_t& Rd, val_t Rs, val_t Rt) {
    Rd = Rs;
  };
  void dadd(val_t& Rd, val_t Rs, val_t Rt) {
    Rd = Rs + Rt;
  };
  void daddu(val_t& Rd, val_t Rs, val_t Rt) {
    Rd = (uint64_t)Rs + (uint64_t)Rt;
  };
  void dsub(val_t& Rd, val_t Rs, val_t Rt) {
    Rd = Rs - Rt;
  };
  void dsubu(val_t& Rd, val_t Rs, val_t Rt) {
    Rd = Rs - Rt;
  };
  void nor(val_t& Rd, val_t Rs, val_t Rt) {
    Rd = ~(Rs | Rt);
  };
  void sub(val_t& Rd, val_t Rs, val_t Rt) {
    Rd = (int32_t)((uint32_t)Rs - (uint32_t)Rt);
  };
  void subu(val_t& Rd, val_t Rs, val_t Rt) {
    Rd = (int32_t)((uint32_t)Rs - (uint32_t)Rt);
  };
  void addi(val_t &Rt, val_t Rs, val_t src2) {
    Rt = (int32_t)((uint32_t)Rs + src2);
  };
  void addiu(val_t &Rt, val_t Rs, val_t src2) {
    Rt = (int32_t)((uint32_t)Rs + src2);
  };

  void slt(val_t &Rd, val_t Rs, val_t Rt) {
    Rd = (int64_t)Rs < (int64_t)Rt;
  };
  void sltu(val_t &Rd, val_t Rs, val_t Rt) {
    Rd = Rs < Rt;
  };
  void andi(val_t &dst, val_t src1, val_t src2) {
    dst = src1 & src2;
  };
  void xori(val_t &dst, val_t src1, val_t src2) {
    dst = src1 ^ src2;
  };

  void lwc1(val_t &dst, val_t src1, val_t src2) {
  };
  void ldc1(val_t &dst, val_t src1, val_t src2) {
  };

  // ext
  void bltz(val_t &dst, val_t src1, val_t src2) {
  };
  void bgez(val_t &dst, val_t src1, val_t src2) {
  };
  void bltzl(val_t &dst, val_t src1, val_t src2) {
  };
  void bgezl(val_t &dst, val_t src1, val_t src2) {
  };
  void bltzal(val_t &dst, val_t src1, val_t src2) {
  };
  void bgezal(val_t &dst, val_t src1, val_t src2) {
  };
  void bltzall(val_t &dst, val_t src1, val_t src2) {
  };
  void bgezall(val_t &dst, val_t src1, val_t src2) {
  };
  // imm oooooo.sssss.ttttt.iiiii.iiiii.iiiiii
  // dst = rs
  // src1 = rt
  // src2 = imm
  void beq(val_t &Rt, val_t Rs, val_t src2) {
    setpc(Rs == Rt, j16(src2));
  };
  void beql(val_t &Rt, val_t Rs, val_t src2) {
    setpc(Rs == Rt, j16(src2), &LR);
  };
  void bnel(val_t &Rt, val_t Rs, val_t src2) {
    setpc(Rs != Rt, j16(src2), &LR);
  };
  void blez(val_t &Rt, val_t Rs, val_t src2) {
    setpc(Rs <= 0, j16(src2));
  };
  void blezl(val_t &Rt, val_t Rs, val_t src2) {
    setpc(Rs <= 0, j16(src2), &LR);
  };
  void bgtz(val_t &Rt, val_t Rs, val_t src2) {
    setpc(Rs > 0, j16(src2));
  };
  void bgtzl(val_t &Rt, val_t Rs, val_t src2) {
  };
  void j(val_t &Rt, val_t Rs, val_t src2) {
    printf("%.8x J: %.8x %.8x\n", (int)PC, (int)src2, j26(src2));
    setpc(true, j26(src2));
  }
  void jal(val_t &Rt, val_t Rs, val_t src2) {
    setpc(true, j26(src2));
  }
  // 000000.sssss.00000.00000.00000.001000
  void jr(val_t &Rd, val_t Rt, val_t Rs) {
    setpc(true, Rs);
  };
  void jalr(val_t &Rd, val_t Rt, val_t Rs) {
    setpc(true, Rs, &LR);
  };
  void reset(uint32_t addr) {
    jmpslot[0] = addr;
    jmpslot[1] = 0xffffffff;
  };
  void exec2(int opfn, val_t&, val_t, val_t, bool&);
  void exec();
  // 001111.00000.ttttt.kkkkk.kkkkk.kkkkkk lui
  void lui(val_t &Rt, val_t src1, val_t src2) {
    Rt = (int64_t)(int32_t)(int16_t)src2 << 16LL;
  };
  // 001101.sssss.ttttt.kkkkk.kkkkk.kkkkkk ori
  void ori(val_t &Rt, val_t Rs, val_t src2) {
    Rt = Rs | src2;
  };
  // 000101.sssss.ttttt.fffff.fffff.ffffff [5]
  void bne(val_t &Rt, val_t Rs, val_t src2) {
    setpc(Rs != Rt, j16(src2));
  };
  // 000000.00000.ttttt.ddddd.kkkkk.000000
  void sll(val_t& Rd, val_t Rt, val_t sa) {
    sa = sa & 0x1F;
    Rd = (int32_t)((uint32_t)Rt << sa);
  };
  void dsll(val_t& Rd, val_t Rt, val_t sa) {
    sa = sa & 0x3F;
    Rd = Rt << sa;
  };
  // 000000.00000.ttttt.ddddd.kkkkk.000000
  void srl(val_t& Rd, val_t Rt, val_t sa) {
    sa = sa & 0x1F;
    Rd = (int32_t)((uint32_t)Rt >> sa);
  };
  // 000000.00000.ttttt.ddddd.kkkkk.000000
  void sra(val_t& Rd, val_t Rt, val_t sa) {
    sa = sa & 0x1f;
    Rd = (int32_t)(Rt >> sa);
  };
  // ______.sssss.ttttt.iiiii.iiiii.iiiiii
  void lb(val_t &Rt, val_t src1, val_t src2) {
    Rt = (int8_t)cpu_read8(src1+src2);
  };
  // ______.sssss.ttttt.iiiii.iiiii.iiiiii
  void lbu(val_t &Rt,val_t src1, val_t src2) {
    Rt = cpu_read8(src1+src2);
  };
  // ______.sssss.ttttt.iiiii.iiiii.iiiiii
  void lh(val_t &Rt, val_t src1, val_t src2) {
    Rt = (int16_t)cpu_read16(src1+src2);
  };
  // ______.sssss.ttttt.iiiii.iiiii.iiiiii
  void lhu(val_t &Rt,val_t src1, val_t src2) {
    Rt = cpu_read16(src1+src2);
  };
  // ______.sssss.ttttt.iiiii.iiiii.iiiiii
  void lw(val_t &Rt, val_t src1, val_t src2) {
    Rt = (int32_t)cpu_read32(src1+src2);
  };
  // ______.sssss.ttttt.iiiii.iiiii.iiiiii
  void lwu(val_t &Rt,val_t src1, val_t src2) {
    Rt = cpu_read32(src1+src2);
  };
  // ______.sssss.ttttt.iiiii.iiiii.iiiiii
  void ld(val_t &Rt, val_t src1, val_t src2) {
    Rt = cpu_read64(src1+src2);
  };
  // ______.sssss.ttttt.iiiii.iiiii.iiiiii
  void sb(val_t &Rt, val_t src1, val_t src2) {
    cpu_write8(src1+src2, Rt);
  };
  // ______.sssss.ttttt.iiiii.iiiii.iiiiii
  void sh(val_t &Rt, val_t src1, val_t src2) {
    cpu_write16(src1+src2, Rt);
  };
  // ______.sssss.ttttt.iiiii.iiiii.iiiiii
  void sw(val_t &Rt, val_t src1, val_t src2) {
    cpu_write32(src1+src2, Rt);
  };
  // ______.sssss.ttttt.iiiii.iiiii.iiiiii
  void sd(val_t &Rt, val_t src1, val_t src2) {
    cpu_write64(src1+src2, Rt);
  };
};

static void request_irq(int n) {
  printf("request irq: %x\n", n);
}
static void clear_irq(int n) {
}

static void rundma(uint32_t dst, uint32_t src, uint32_t len) {
  printf("rundma: %.8x %.8x %x\n", dst, src, len);
  while (len--) {
    cpu_write8(dst++, cpu_read8(src++));
  }
}
  
/* 3 types:
 * n64 - little endian
 * v64 - byteswapped
 * z64 - big endian
 */

/*
 * https://ultra64.ca/files/documentation/silicon-graphics/SGI_Nintendo_64_RSP_Programmers_Guide.pdf
 */

/* Memory Map
 * https://github.com/SimoneN64/Kaizen/tree/master/src
 *
 * COP0
 * COP1 : fpu
 * COP2 : vector (registers = 8x16bit or 16x8bit)
 * COP3
 *
 * ram  0x00000000 ... 0x03EFFFFF rdram memory space
 *      0x03F00000 ... 0x03F7FFFF rdram registers
 *      0x03F80000 ... 0x03FFFFFF rdram registers
 *
 *      0x04000000 ... 0x04000FFF rsp data memory
 *      0x04001000 ... 0x04001FFF rsp instruction memory
 *      
 * mi   0x04300000 ... 0x043FFFFF mips interface           https://n64brew.dev/wiki/MIPS_Interface
 *      ______________ 0x04300000 _w mi_mode
 *      ______________ 0x04300004 __ mi_version
 *      ______________ 0x04300000 r_ mi_interrupt
 *      ______________ 0x04300000 rw mi_mask
 *
 * vi   0x04400000 ... 0x044FFFFF video interface         https://n64brew.dev/wiki/Video_Interface
 *      ______________ 0x04400000 _w vi_ctrl
 *      ______________ 0x04400004 _w vi_origin            00000000.aaaaaaaa.aaaaaaaa.aaaaaaaa
 *      ______________ 0x04400008 _w vi_width             00000000.00000000.0000wwww.wwwwwwww
 *      ______________ 0x0440000c __ vi_v_intr
 *      ______________ 0x04400010 _w vi_v_current         mi::clearintr(3)
 *      ______________ 0x04400014 __ vi_burst
 *      ______________ 0x04400018 __ vi_v_sync
 *      ______________ 0x0440001c __ vi_h_sync
 *      ______________ 0x04400020 __ vi_h_sync_leap
 *      ______________ 0x04400024 _w vi_h_video           00ssssss.ssssssss.00eeeeee.eeeeeeee
 *      ______________ 0x04400028 _w vi_v_video           00ssssss.ssssssss.00eeeeee.eeeeeeee
 *      ______________ 0x0440002c __ vi_v_burst
 *      ______________ 0x04400030 _w vi_x_scale
 *      ______________ 0x04400034 _w vi_y_scale
 *      ______________ 0x04400038 __ vi_test_addr
 *      ______________ 0x0440003c __ vi_staged_data
 *
 * ai   0x04500000 ... 0x045FFFFF audio interface        https://n64brew.dev/wiki/Video_Interface
 *      ______________ 0x04500000 _w ai_dram_addr
 *      ______________ 0x04500004 _w ai_length
 *      ______________ 0x04500008 _w ai_control
 *      ______________ 0x0450000c rw ai_status
 *      ______________ 0x04500010 _w ai_dacrate
 *      ______________ 0x04500014 __ ai_bitrate
 *
 * pi   0x04600000 ... 0x046FFFFF peripheral interface   https://n64brew.dev/wiki/Peripheral_Interface
 *      ______________ 0x04600000 __ pi_dram_addr
 *      ______________ 0x04600004 __ pi_cart_addr
 *      ______________ 0x04600008 __ pi_rd_len
 *      ______________ 0x0460000c __ pi_wr_len
 *      ______________ 0x04600010 __ pi_status
 *      ______________ 0x046000n4 __ pi_bsd_domn_lat
 *      ______________ 0x046000n8 __ pi_bsd_domn_pwd
 *      ______________ 0x046000nc __ pi_bsd_domn_pgs
 *      ______________ 0x046000n0 __ pi_bsd_domn_rls
 *
 * ri   0x04700000 ... 0x047FFFFF rdram interface       https://n64brew.dev/wiki/RDRAM_Interface
 *      ______________ 0x04700000 ri_mode
 *      ______________ 0x04700004 ri_config
 *      ______________ 0x04700008 ri_current_load
 *      ______________ 0x0470000c ri_select
 *      ______________ 0x04700010 ri_refresh
 *      ______________ 0x04700014 ri_latency
 *
 * si   0x04800000 ... 0x048FFFFF serial interface
 *      ______________ 0x04800000 __ si_dram_addr
 *      ______________ 0x04800004 __ si_pif_ad_rd64b
 *      ______________ 0x04800008 __ si_pif_ad_wr4b
 *      ______________ 0x04800010 __ si_pif_ad_wr64b
 *      ______________ 0x04800014 __ si_pif_ad_rd4b
 *      ______________ 0x04800018 __ si_STATUS
 */

struct RomHeader {
  uint32_t endian;
  uint32_t clock_rate;
  uint32_t program_counter;
  uint32_t release;
  uint32_t crc1;
  uint32_t crc2;
  uint32_t rsv1;
  uint32_t rsv2;
  char     image_name[20];
  uint32_t rsv3;
  uint32_t manufacturer_id;
  uint16_t cartridge_id;
  char     country_code[2];
  uint8_t  boot_code[4032];
};

static bus_t mb(0x0FFFFFFF);

struct mmio {
  uint8_t    *base;
  size_t     mask;
  void init(uint32_t start, uint32_t end, size_t s, void *b, const char *lbl) {
    base = (uint8_t*)b;
    mask = s - 1;
    mb.register_handler(start, end, mask, mmio::rawio, this, _RW|_DBG, lbl);
  };
  static int rawio(void *arg, uint32_t addr, const int mode, iodata_t& d) {
    mmio *m = (mmio *)arg;
    if ((mode & 0xFF) == 'w') {
      m->rdwr(addr, d, true);
    }
    else {
      d = m->rdwr(addr, d, false);
    }
    return 0;
  };
  virtual uint32_t rdwr(uint32_t addr, uint32_t value, bool write) {
    if (write) {
      put32be(base + (addr & mask), value);
      return 0;
    }
    return get32be(base + (addr & mask));
  };
};

/*================================================================*
 * RSP_CP0
 *================================================================*/
struct RSP_CP0 {
  enum {
    SP_MEM_ADDR  = 0x0,
    SP_DRAM_ADDR = 0x1,
    SP_RD_LEN    = 0x2,
    SP_WR_LEN    = 0x3,
    SP_STATUS    = 0x4,
  };
  uint32_t rsp_mem_addr;
  uint32_t rsp_dram_addr;
  uint32_t rsp_status;
  uint32_t rsp_semaphore;

  uint32_t rdwr(uint32_t addr, uint32_t value, bool write);
};

uint32_t RSP_CP0::rdwr(uint32_t addr, uint32_t value, bool write) {
  if (!write)
    return 0;
  switch (addr) {
  case SP_MEM_ADDR:
    rsp_mem_addr = (value & 0x1fff);
    break;
  case SP_DRAM_ADDR:
    rsp_dram_addr = (value & 0xFFFFFF);
    break;
  case SP_RD_LEN:
    rundma(rsp_dram_addr, rsp_mem_addr, (value & 0xFF8) + 1);
    break;
  case SP_WR_LEN:
    rundma(rsp_mem_addr, rsp_dram_addr, (value & 0xFF8) + 1);
    break;
  }
  return 0;
}

/*================================================================*
 * RSP_CP0
 *================================================================*/
struct MI : public mmio {
  enum {
    MI_BASE = 0x04300000,
    MI_END  = 0x043fffff,
  };
  MI() {
    init(MI_BASE, MI_END, 0x0f, &mi_mode, "MI.mmio");
  };
  uint32_t mi_mode;
  uint32_t mi_version;
  uint32_t mi_interrupt;
  uint32_t mi_mask;
};

/*================================================================*
 * Video Interface
 *================================================================*/
struct VI : public mmio {
  enum {
    VI_BASE           = 0x04400000,
    VI_END            = 0x044fffff,
    
    VI_CTRL           = 0x04400000,
    VI_ORIGIN         = 0x04400004,
    VI_WIDTH          = 0x04400008,
    VI_V_INTR         = 0x0440000c,
    VI_V_CURRENT      = 0x04400010,
    VI_BURST          = 0x04400014,
    VI_V_SYNC         = 0x04400018,
    VI_H_SYNC         = 0x0440001c,
    VI_H_SYNC_LEAP    = 0x04400020,
    VI_H_VIDEO        = 0x04400024,
    VI_V_VIDEO        = 0x04400028,
    VI_V_BURST        = 0x0440002c,
    VI_X_SCALE        = 0x04400030,
    VI_Y_SCALE        = 0x04400034,
    VI_TEST_ADDR      = 0x04400038,
    VI_STAGED_DATA    = 0x0440003c,
  };
  VI() {
    init(VI_BASE, VI_END, 0x40, &vi_ctrl, "VI.mmio");
  };
  uint32_t vi_ctrl;
  uint32_t vi_origin;      // 00000000.aaaaaaaa.aaaaaaaa.aaaaaaaa rdram (8-bytes aligned)
  uint32_t vi_width;       // 00000000.00000000.0000wwww.wwwwwwww (common=320,640)
  uint32_t vi_v_intr;      // 00000000.00000000.000000vv.vvvvvvvv
  uint32_t vi_v_current;   // 00000000.00000000.000000vv.vvvvvvvv
  uint32_t vi_burst;       // 00ssssss.ssssvvvv.bbbbbbbb.hhhhhhhh NTSC=0x03E52239
  uint32_t vi_v_sync;      // 00000000.00000000.000000vv.vvvvvvvv NTSC=0x20d non-interlace, 0x20c interlace
  uint32_t vi_h_sync;      // 00000000.000lllll.0000hhhh.hhhhhhhh NTSC=0xc15
  uint32_t vi_h_sync_leap; // 0000aaaa.aaaaaaaa.0000bbbb.bbbbbbbb NTSC=0xc15
  uint32_t vi_h_video;     // 000000ss.ssssssss.000000ee.eeeeeeee NTSC=0x6c, 0x2ec
  uint32_t vi_v_video;     // 000000ss.ssssssss.000000ee.eeeeeeee NTSC=0x25, 0x1ff
  uint32_t vi_v_burst;
  uint32_t vi_x_scale;
  uint32_t vi_y_scale;
  uint32_t vi_test_addr;
  uint32_t vi_staged_data;

  void show() {
    printf("ctrl:     %.8x\n", xx(vi_ctrl));
    printf("origin:   %.8x\n", xx(vi_origin));
    printf("width:    %.8x\n", xx(vi_width));
    printf("v_intr:   %.8x\n", xx(vi_v_intr));
    printf("v_current:%.8x\n", xx(vi_v_current));
    printf("burst:    %.8x\n", xx(vi_burst));
    printf("v_sync    %.8x\n", xx(vi_v_sync));
    printf("h_sync    %.8x\n", xx(vi_h_sync));
    printf("h_leap    %.8x\n", xx(vi_h_sync_leap));
    printf("h_video:  %.8x\n", xx(vi_h_video));
    printf("v_video:  %.8x\n", xx(vi_v_video));
    printf("v_burst:  %.8x\n", xx(vi_v_burst));
    printf("scale:    %.8x %.8x\n", xx(vi_x_scale), xx(vi_y_scale));
  };
};

/*================================================================*
 * Audio Interface
 *================================================================*/
struct AI : public mmio {
  enum {
    AI_BASE = 0x04500000,
    AI_END  = 0x045fffff,
  };
  AI() {
    init(AI_BASE, AI_END, 0x1F, &ai_dram_addr, "AI.mmio");
  };
  uint32_t ai_dram_addr;    // 00: w   dmaAddr[dmaCount] = val & 0xFFFFF8;
  uint32_t ai_length;       // 04: w   gen_irq(dmaCount==0, AI); dmaLen[dmaCount]  = val & 0xFFFFF8;
  uint32_t ai_control;      // 08: w   dmaEnable = val & 1
  uint32_t ai_status;       // 0c: rw  gen_irq(true, AI);
  uint32_t ai_decrate;      // 10: w   
  uint32_t ai_bitrate;      // 14: w

  uint32_t rdwr(uint32_t addr, uint32_t value, bool write);
};

/*================================================================*
 * Peripheral Interface
 *================================================================*/
struct PI : public mmio{
  enum {
    PI_BASE      = 0x04600000,
    PI_END       = 0x046fffff,
    
    PI_DRAM_ADDR = 0x04600000,
    PI_CART_ADDR = 0x04600004,
    PI_RD_LEN    = 0x04600008,
    PI_WR_LEN    = 0x0460000C,
    PI_STATUS    = 0x04600010,
  };
  PI() {
    init(PI_BASE, PI_END, 0x30, &pi_dram_addr, "PI.mmio");
  };
  uint32_t pi_dram_addr;
  uint32_t pi_cart_addr;
  uint32_t pi_rd_len;
  uint32_t pi_wr_len;
  uint32_t pi_status;
  struct {
    uint32_t pi_bsd_dom_lat;
    uint32_t pi_bsd_dom_pwd;
    uint32_t pi_bsd_dom_pgs;
    uint32_t pi_bsd_dom_rls;
  } bsd[2];

  uint32_t rdwr(uint32_t addr, uint32_t value, bool write);
};

uint32_t PI::rdwr(uint32_t addr, uint32_t value, bool write) {
  printf("writing: %x %x %x\n", addr, value, write);
  if (!write)
    return 0;
  switch (PI_BASE+addr) {
  case PI_DRAM_ADDR:
    pi_dram_addr = (value & 0xFFFFFF);
    break;
  case PI_CART_ADDR:
    pi_cart_addr = value;
    break;
  case PI_RD_LEN:
    rundma(pi_cart_addr, pi_dram_addr, (value & 0xFFFFFF) + 1);
    request_irq(4);
    break;
  case PI_WR_LEN:
    rundma(pi_dram_addr, pi_cart_addr, (value & 0xFFFFFF) + 1);
    request_irq(4);
    break;
  case PI_STATUS:
    if (value & 0x2) {
      clear_irq(4);
    };
    break;
  }
  return 0;
}

/*================================================================*
 * Serial Interface
 *   0x1FC00000 - 0x1CFCFFFF
 *   0x000 - 0x7BF PIF ROM
 *   0x7C0 - 0x7FF PIF RAM
 *================================================================*/
struct SI {
  enum {
    PIF_BASE        = 0x9fc00000,
    
    SI_BASE         = 0x04800000,
    SI_END          = 0x043fffff,
    
    SI_DRAM_ADDR    = 0x04800000,
    SI_PIF_AD_RD64B = 0x04800004, 
    SI_PIF_AD_WR4B  = 0x04800008,
    SI_PIF_AD_WR64B = 0x04800010,
    SI_PIF_AD_RD4B  = 0x04800014,
    SI_STATUS       = 0x04800018,
  };
  uint32_t si_dram_addr;

  uint32_t rdwr(uint32_t, uint32_t, bool);
};

uint32_t SI::rdwr(uint32_t addr, uint32_t value, bool write) {
  if (!write)
    return 0;
  switch (addr) {
  case SI_DRAM_ADDR:
    si_dram_addr = (value & 0xFFFFFF);
    break;
  case SI_PIF_AD_RD64B:
    // PIF::runCommand();
    rundma(si_dram_addr, PIF_BASE + (value & 0x7fc), 64);
    request_irq(1);
    break;
  case SI_PIF_AD_WR64B:
    rundma(PIF_BASE + (value & 0x7fc), si_dram_addr, 64);
    request_irq(1);
  case SI_STATUS:
    clear_irq(1);
    break;
  }
  return 0;
}

struct RI {
  uint32_t ri_mode;
  uint32_t ri_config;
  uint32_t ri_current_load;
  uint32_t ri_select;
  uint32_t ri_refresh;
  uint32_t ri_latency;
};

const char *fnname(uint32_t addr) {
  return "xxx";
}

static MI mi;
static VI vi;
static PI pi;

uint8_t cpu_read8(const uint32_t addr, int type) {
  iodata_t d;
  
  mb.read(addr, d);
  return d;
}

uint16_t cpu_read16(const uint32_t addr, int type) {
  iodata_t d;
  
  mb.read(addr, d, _SZ16);
  return d;
}

uint32_t cpu_read32(const uint32_t addr, int type) {
  iodata_t d;
  
  mb.read(addr, d, _SZ32);
  return d;
}

uint64_t cpu_read64(const uint32_t addr, int type) {
  uint64_t lo = cpu_read32(addr+4);
  uint64_t hi = cpu_read32(addr+0);
  return (hi << 32) | lo;
};

void cpu_write8(const uint32_t addr, uint8_t v, int type) {
  iodata_t d = v;
  
  mb.write(addr, d);
}

void cpu_write16(const uint32_t addr, uint16_t v, int type) {
  iodata_t d = v;
  
  mb.write(addr, d, _SZ16);
}

void cpu_write32(const uint32_t addr, uint32_t v, int type) {
  iodata_t d = v;
  
  mb.write(addr, d, _SZ32);
}

void cpu_write64(const uint32_t addr, uint64_t v, int type) {
  cpu_write32(addr+0, v >> 32, type);
  cpu_write32(addr+4, v, type);
}

void cpu_reset(uint32_t v) {
}

void flogger(int n, const char *fmt, ...)
{
  va_list ap;
  va_start(ap, fmt);
  vprintf(fmt, ap);
}

/* Render rectangle from memory */
void drawscr(Screen *scr, int sx, int sy, int w, int h, uint32_t *mem)
{
  for (int y = 0; y < h; y++) {
    for (int x = 0; x < w; x++) {
      uint32_t pxl = get32(mem++);
      scr->setpixel(sx+x, sy+y, MKRGB((pxl & 0xFF), (pxl & 0xFF00) >> 8, (pxl & 0xFF0000) >> 16));
    }
  }
}

/* Display character font data */
void drawfont(Screen *scr, uint32_t *rom, int x, int y, int xm = 256)
{
  for (int i = 0; i < 128; i++) {
    drawscr(scr, x, y, 8, 8, &rom[i*64]);
    x += 8;
    if (x == xm) {
      x = 0;
      y += 8;
    }
  }
}

/* Disassemble string */

/* Disassemble string */
const char *dis(const uint32_t op, const char *mnem)
{
  static char dstr[256], *d;
  const int rt = (op >> 16) & 0x1F;
  const int rs = (op >> 21) & 0x1F;
  const int rd = (op >> 11) & 0x1F;
  const int sa = (op >> 6) & 0x1F;
  
  d = dstr;
  while (*mnem) {
    if (replace(&mnem, "%Rt", &d, regname[rt]) ||
	replace(&mnem, "%Rs", &d, regname[rs]) ||
	replace(&mnem, "%Rd", &d, regname[rd]) ||
	replace(&mnem, "%sa", &d, "%d", sa) ||
	replace(&mnem, "%i", &d, "0x%x", (uint16_t)op)) {
      continue;
    }
    *d++ = *mnem++;
  }
  *d = 0;
  if (trace) {
    printf("%s\n", dstr);
  }
  return dstr;
}

char *dn(char *d, uint32_t& op, int n)
{
  for (int i = 0; i < n; i++) {
    *d++ = (op >> 31) + '0';
    op <<= 1;
  }
  *d++ = '.';
  return d;
}

const char *binop(uint32_t op) {
  static char dstr[128], *d;

  d = dn(dstr, op, 6); // fn
  d = dn(d, op, 5); // rs
  d = dn(d, op, 5); // rt
  d = dn(d, op, 5); // rd
  d = dn(d, op, 5); // sa
  d = dn(d, op, 6); // fn
  *d = 0;
  return dstr;
}

static constexpr int mkop(int n, const char *bits) {
  uint32_t val = 0;
  char ch = 0;

  return n;
  while ((ch = *bits) != 0) {
    if (ch == '.')
      continue;
    val <<= 1;
    if (ch == '0' || ch == '1') {
      val |= (ch - '0');
    }
  }
  return val;
}

typedef void (cpu::*efn)(val_t &, val_t, val_t);

struct etab {
  uint32_t opfn;
  uint32_t oparg;
  uint32_t flag;
  const char *dis;
  efn fn;
};

etab mkop(int n, const char *bits, int flag, efn fn, int ff, const char *m) {
  etab e = { };
  char ch;
  uint32_t v = 0;
  
  while ((ch = *bits++) != 0) {
    if (ch == '.')
      continue;
    v <<= 1;
    if (ch == '0' || ch == '1') {
      v |= (ch - '0');
    }
  }
  e.opfn = n;
  n = (v >> 26) & 0x3F;
  if (!n) {
    n = 0x100+(v & 0x3F);
  }
  if (n != e.opfn) {
    printf("bad: %s %.3x %.3x %.8x\n", m, n, e.opfn, v);
  }
  e.oparg = ff;
  e.fn = fn;
  e.dis = m;
  e.flag = flag;
  return e;
};

enum {
  none,
  RtRsS16,  // ok
  RtRsU16,  // ok
  RdRtRs,   // ok
  RdRsRt,   // ok
  RdRtSa,   // ok
  J16,
  J26,
};

#define ______ NULL

// 32 64-bit registers
// addi oooooo.ttttt.sssss.iiiii.iiiii.iiiiii
//   emit(load rax, regs[sss])
//   emit(add  rax, (int16_t)op)
//   emit(store regs[ttt], eax)
const etab efns[] = {
  mkop(0x002, "000010.kkkkk.kkkkk.kkkkk.kkkkk.kkkkkk", true,  &cpu::j,     J26,     "j      %j"),
  mkop(0x003, "000011.kkkkk.kkkkk.kkkkk.kkkkk.kkkkkk", true,  &cpu::jal,   J26,     "jal    %j"),
  mkop(0x004, "000100.sssss.ttttt.iiiii.iiiii.iiiiii", true,  &cpu::beq,   RtRsS16, "beq    %Rt, %Rs, %i"),
  mkop(0x005, "000101.sssss.ttttt.iiiii.iiiii.iiiiii", true,  &cpu::bne,   RtRsS16, "bne    %Rt, %Rs, %i"),
  mkop(0x006, "000110.sssss.ttttt.iiiii.iiiii.iiiiii", true,  &cpu::blez,  RtRsS16, "blez   %Rt, %Rs, %i"),
  mkop(0x007, "000111.sssss.ttttt.iiiii.iiiii.iiiiii", true,  &cpu::bgtz,  RtRsS16, "bgtz   %Rt, %Rs, %i"),

  mkop(0x008, "001000.sssss.ttttt.iiiii.iiiii.iiiiii", false, &cpu::addi,  RtRsS16, "addi   %Rt, %Rs, %i"),
  mkop(0x009, "001001.sssss.ttttt.iiiii.iiiii.iiiiii", false, &cpu::addiu, RtRsS16, "addiu  %Rt, %Rs, %i"),
  mkop(0x00a, "001010.sssss.ttttt.iiiii.iiiii.iiiiii", false, &cpu::slt,   RtRsS16, "slti   %Rt, %Rs, %i"),
  mkop(0x00b, "001011.sssss.ttttt.iiiii.iiiii.iiiiii", false, &cpu::sltu,  RtRsS16, "sltiu  %Rt, %Rs, %i"),
  mkop(0x00c, "001100.sssss.ttttt.uuuuu.uuuuu.uuuuuu", false, &cpu::andi,  RtRsU16, "andi   %Rt, %Rs, %i"),
  mkop(0x00d, "001101.sssss.ttttt.uuuuu.uuuuu.uuuuuu", false, &cpu::ori,   RtRsU16, "ori    %Rt, %Rs, %i"),
  mkop(0x00e, "001110.sssss.ttttt.uuuuu.uuuuu.uuuuuu", false, &cpu::xori,  RtRsU16, "xori   %Rt, %Rs, %i"),
  mkop(0x00f, "001111.sssss.ttttt.uuuuu.uuuuu.uuuuuu", false, &cpu::lui,   RtRsU16, "lui    %Rt, %i"),

  mkop(0x014, "010100.sssss.ttttt.iiiii.iiiii.iiiiii", false, &cpu::beql,  RtRsS16, "beql   %Rt, %Rs, %i"),
  mkop(0x015, "010101.sssss.ttttt.iiiii.iiiii.iiiiii", false, &cpu::bnel,  RtRsS16, "bnel   %Rt, %Rs, %i"),
  mkop(0x016, "010110.sssss.ttttt.iiiii.iiiii.iiiiii", false, &cpu::blezl, RtRsS16, "blezl  %Rt, %Rs, %i"),
  mkop(0x017, "010111.sssss.ttttt.iiiii.iiiii.iiiiii", false, &cpu::bgtzl, RtRsS16, "bgtzl  %Rt, %Rs, %i"),
  mkop(0x018, "011000.sssss.rrrrr.iiiii.iiiii.iiiiii", false, &cpu::dadd,  RtRsS16, "daddi  %Rt, %Rs, %i"),
  mkop(0x019, "011001.sssss.rrrrr.iiiii.iiiii.iiiiii", false, &cpu::daddu, RtRsS16, "daddiu %Rt, %Rs, %i"),

  mkop(0x020, "100000.sssss.ttttt.iiiii.iiiii.iiiiii", false, &cpu::lb,    RtRsS16, "lb     %Rt, %i(%Rs)"),
  mkop(0x021, "100001.sssss.ttttt.iiiii.iiiii.iiiiii", false, &cpu::lh,    RtRsS16, "lh     %Rt, %i(%Rs)"),
  mkop(0x023, "100011.sssss.ttttt.iiiii.iiiii.iiiiii", false, &cpu::lw,    RtRsS16, "lw     %Rt, %i(%Rs)"),
  mkop(0x024, "100100.sssss.ttttt.iiiii.iiiii.iiiiii", false, &cpu::lbu,   RtRsS16, "lbu    %Rt, %i(%Rs)"),
  mkop(0x025, "100101.sssss.ttttt.iiiii.iiiii.iiiiii", false, &cpu::lhu,   RtRsS16, "lhu    %Rt, %i(%Rs)"),
  mkop(0x027, "100111.sssss.ttttt.iiiii.iiiii.iiiiii", false, &cpu::lwu,   RtRsS16, "lwu    %Rt, %i(%Rs)"),
  mkop(0x028, "101000.sssss.ttttt.iiiii.iiiii.iiiiii", false, &cpu::sb,    RtRsS16, "sb     %Rt, %i(%Rs)"),
  mkop(0x029, "101001.sssss.ttttt.iiiii.iiiii.iiiiii", false, &cpu::sh,    RtRsS16, "sh     %Rt, %i(%Rs)"),
  mkop(0x02b, "101011.sssss.ttttt.iiiii.iiiii.iiiiii", false, &cpu::sw,    RtRsS16, "sw     %Rt, %i(%Rs)"),

  mkop(0x037, "110111.sssss.ttttt.iiiii.iiiii.iiiiii", false, &cpu::ld,    RtRsS16, "ld     %Rt, %i(%Rs)"),
  mkop(0x03f, "111111.sssss.ttttt.iiiii.iiiii.iiiiii", false, &cpu::sd,    RtRsS16, "sd     %Rt, %i(%Rs)"),

  mkop(0x100, "000000.-----.ttttt.ddddd.aaaaa.000000", false, &cpu::sll,   RdRtSa,  "sll    %Rd, %Rt, %sa"),
  mkop(0x102, "000000.-----.ttttt.ddddd.aaaaa.000010", false, &cpu::srl,   RdRtSa,  "srl    %Rd, %Rt, %sa"),
  mkop(0x103, "000000.-----.ttttt.ddddd.aaaaa.000011", false, &cpu::sra,   RdRtSa,  "sra    %Rd, %Rt, %sa"),

  mkop(0x104, "000000.sssss.ttttt.ddddd.-----.000100", false, &cpu::sll,   RdRtRs,  "sllv   %Rd, %Rt, %Rs"),
  mkop(0x106, "000000.sssss.ttttt.ddddd.-----.000110", false, &cpu::srl,   RdRtRs,  "srlv   %Rd, %Rt, %Rs"),
  mkop(0x107, "000000.sssss.ttttt.ddddd.-----.000111", false, &cpu::sra,   RdRtRs,  "srav   %Rd, %Rt, %Rs"),
  mkop(0x108, "000000.sssss.-----.-----.-----.001000", true,  &cpu::jr,    RdRtRs,  "jr     %Rs"),
  mkop(0x109, "000000.sssss.-----.-----.-----.001001", true,  &cpu::jalr,  RdRtRs,  "jalr   %Rs"),
  mkop(0x110, "000000.-----.-----.ddddd.-----.010000", false, &cpu::mfhi,  RdRtRs,  "mfhi   %Rd"),
  mkop(0x111, "000000.sssss.-----.-----.-----.010001", false, &cpu::mthi,  RdRtRs,  "mthi   %Rs"),
  mkop(0x112, "000000.-----.-----.ddddd.-----.010010", false, &cpu::mflo,  RdRtRs,  "mflo   %Rd"),
  mkop(0x113, "000000.sssss.-----.-----.-----.010011", false, &cpu::mtlo,  RdRtRs,  "mtlo   %Rs"),
  mkop(0x114, "000000.sssss.ttttt.ddddd.-----.010100", false, &cpu::dsll,  RdRtRs,  "dsllv  %Rd, %Rt, %Rs"),
  mkop(0x116, "000000.sssss.ttttt.ddddd.-----.010110", false, &cpu::srl,   RdRtRs,  "dsrlv  %Rd, %Rt, %Rs"),
  mkop(0x117, "000000.sssss.ttttt.ddddd.-----.010111", false, &cpu::sra,   RdRtRs,  "dsrav  %Rd, %Rt, %Rs"),

  mkop(0x120, "000000.sssss.ttttt.ddddd.-----.100000", false, &cpu::addi,  RdRsRt,  "add    %Rd, %Rs, %Rt"),
  mkop(0x121, "000000.sssss.ttttt.ddddd.-----.100001", false, &cpu::addiu, RdRsRt,  "addu   %Rd, %Rs, %Rt"),
  mkop(0x122, "000000.sssss.ttttt.ddddd.-----.100010", false, &cpu::sub,   RdRsRt,  "sub    %Rd, %Rs, %Rt"),
  mkop(0x123, "000000.sssss.ttttt.ddddd.-----.100011", false, &cpu::subu,  RdRsRt,  "subu   %Rd, %Rs, %Rt"),
  mkop(0x124, "000000.sssss.ttttt.ddddd.-----.100100", false, &cpu::andi,  RdRsRt,  "and    %Rd, %Rs, %Rt"),
  mkop(0x125, "000000.sssss.ttttt.ddddd.-----.100101", false, &cpu::ori,   RdRsRt,  "or     %Rd, %Rs, %Rt"),
  mkop(0x126, "000000.sssss.ttttt.ddddd.-----.100110", false, &cpu::xori,  RdRsRt,  "xor    %Rd, %Rs, %Rt"),
  mkop(0x127, "000000.sssss.ttttt.ddddd.-----.100111", false, &cpu::nor,   RdRsRt,  "nor    %Rd, %Rs, %Rt"),

  mkop(0x12a, "000000.sssss.ttttt.ddddd.-----.101010", false, &cpu::slt,   RdRsRt,  "slt    %Rd, %Rs, %Rt"),
  mkop(0x12b, "000000.sssss.ttttt.ddddd.-----.101011", false, &cpu::sltu,  RdRsRt,  "sltu   %Rd, %Rs, %Rt"),
  mkop(0x12c, "000000.sssss.ttttt.ddddd.-----.101100", false, &cpu::dadd,  RdRsRt,  "dadd   %Rd, %Rs, %Rt"), 
  mkop(0x12d, "000000.sssss.ttttt.ddddd.-----.101101", false, &cpu::daddu, RdRsRt,  "daddu  %Rd, %Rs, %Rt"), 
  mkop(0x12e, "000000.sssss.ttttt.ddddd.-----.101110", false, &cpu::dsub,  RdRsRt,  "dsub   %Rd, %Rs, %Rt"),
  mkop(0x12f, "000000.sssss.ttttt.ddddd.-----.101111", false, &cpu::dsub,  RdRsRt,  "dsubu  %Rd, %Rs, %Rt"),
  
  mkop(0x138, "000000.-----.ttttt.ddddd.aaaaa.111000", false, &cpu::dsll,  RdRtSa,  "dsll   %Rd, %Rt, %sa"),
  mkop(0x13a, "000000.-----.ttttt.ddddd.aaaaa.111010", false, &cpu::dsll,  RdRtSa,  "dsrl   %Rd, %Rt, %sa"),
  mkop(0x13b, "000000.-----.ttttt.ddddd.aaaaa.111011", false, &cpu::dsll,  RdRtSa,  "dsra   %Rd, %Rt, %sa"),
  mkop(0x13c, "000000.-----.ttttt.ddddd.aaaaa.111100", false, &cpu::dsll,  RdRtSa,  "dsll32 %Rd, %Rt, %sa"),
  mkop(0x13e, "000000.-----.ttttt.ddddd.aaaaa.111110", false, &cpu::dsll,  RdRtSa,  "dsrl32 %Rd, %Rt, %sa"),
  mkop(0x13f, "000000.-----.ttttt.ddddd.aaaaa.111111", false, &cpu::dsll,  RdRtSa,  "dsra32 %Rd, %Rt, %sa"),
  { },
};

const etab *emap[0x140];

const etab *getop(int opfn) {
  for (auto e = efns; e->dis; e++) {
    if (e->opfn == opfn)
      return e;
  }
  return NULL;
}

std::map<uint32_t, uint32_t> visited;

void cpu::exec2(int opfn, val_t& Rd, val_t Rs, val_t Rt, bool& found) {
  uint32_t memaddr = Rs + Rt;

  found = true;
  switch (opfn) {
  case iMFHI: Rd = mhi; return;
  case iMFLO: Rd = mlo; return;
  case iMTHI: mhi = Rs; return;
  case iMTLO: mlo = Rs; return;

  case iANDI:Rd = Rs & (uint16_t)Rt; return; // oooooo.ttttt.sssss.iiiii.iiiii.iiiiii : rt=rs&imm
  case iXORI:Rd = Rs ^ (uint16_t)Rt; return; // oooooo.ttttt.sssss.iiiii.iiiii.iiiiii : rt=rs^imm
  case iORI: Rd = Rs | (uint16_t)Rt; return; // oooooo.ttttt.sssss.iiiii.iiiii.iiiiii : rt=rs|imm
  case iLUI: Rd = Rt << 16; return;
    
  case iNOR: Rd = ~(Rs | Rt); return;
  case iAND: Rd = Rs & Rt; return;           // 000000.ttttt.sssss.ddddd.00000.oooooo : rd=rs&rt
  case iXOR: Rd = Rs ^ Rt; return;
  case iOR:  Rd = Rs | Rt; return;

  case iADDI: Rd = Rs + Rt; return;          // rt=rs+imm
  case iADDIU: Rd = Rs + Rt; return;
  case iADD: Rd = Rs + Rt; return;           // rd=rs+rt
  case iADDU: Rd = Rs + Rt; return;
  case iDADD: Rd = Rs + Rt; return;          // rd=rs+rt
  case iDADDU: Rd = Rs + Rt; return;
  case iDADDI: Rd = Rs + Rt; return;         // rt=rs+imm
  case iDADDIU: Rd = Rs + Rt; return;
    
  case iSUB: Rd = Rs - Rt; return;
  case iSUBU: Rd = Rs - Rt; return;
  case iDSUB: Rd = Rs - Rt; return;
  case iDSUBU: Rd = Rs - Rt; return;

  case iSLL: Rd = Rs << (Rt & 0x1F); return;
  case iSLLV: Rd = Rs << (Rt & 0x1F); return;
  case iDSLL: Rd = Rs << (Rt & 0x3F); return;
  case iDSLLV: Rd = Rs << (Rt & 0x3F); return;
  case iDSLL32: Rd = Rs << (Rt+32); return;
			   
  case iSRL: Rd = (uint32_t)Rs >> (Rt & 0x1F); return;
  case iSRLV: Rd = (uint32_t)Rs >> (Rt & 0x1F); return;
  case iDSRL: Rd = (uint64_t)Rs >> (Rt & 0x3F); return;
  case iDSRLV: Rd = (uint64_t)Rs >> (Rt & 0x3F); return;
  case iDSRL32: Rd = (uint64_t)Rs >> (Rt+32); return;
    
  case iSRA: Rd = Rs >> (Rt & 0x1F); return;
  case iSRAV: Rd = Rs >> (Rt & 0x1F); return;
  case iDSRA: Rd = Rs >> (Rt & 0x3F); return;
  case iDSRAV: Rd = Rs >> (Rt & 0x3F); return;
  case iDSRA32: Rd = Rs >> (Rt+32); return;

  case iLB:  Rd = (int8_t)cpu_read8(memaddr); return;
  case iLBU: Rd = cpu_read8(memaddr); return;
  case iLH:  Rd = (int16_t)cpu_read16(memaddr); return;
  case iLHU: Rd = cpu_read16(memaddr); return;
  case iLW:  Rd = (int32_t)cpu_read32(memaddr); return;
  case iLWU: Rd = cpu_read32(memaddr); return;
  case iLD:  Rd = cpu_read64(memaddr); return;

  case iSB: cpu_write8(memaddr, Rd); return;
  case iSH: cpu_write16(memaddr, Rd); return;
  case iSW: cpu_write32(memaddr, Rd); return;
  case iSD: cpu_write64(memaddr, Rd); return;
  }
  found = false;
}

void cpu::exec()
{
  uint32_t op;
  uint32_t rt, rs, rd, sa, opfn, imm;

  /* Check delay slot */
  if (jmpslot[0] != 0xFFFFFFFF) {
    PC = jmpslot[0];
  }
  jmpslot[0] = jmpslot[1];
  jmpslot[1] = 0xffffffff;
  
  op = cpu_read32(PC); 
  rs = (op >> 21) & 0x1f;
  rt = (op >> 16) & 0x1f;
  rd = (op >> 11) & 0x1f;
  sa = (op >> 6) & 0x1f;
  imm = (uint16_t)op;

  // immediate mode
  opfn = (op >> 26) & 0x3f;
  if (!opfn) {
    // register mode
    opfn = 0x100 + (op & 0x3F);
  }
  trace = 0;
  if (trace) {
    printf("\n%.8x ==== op: %.8x[%s] %.4x %.2x %.2x\n", (int)PC, op, binop(op), opfn, rt, rs);
    showregs();
  }
  PC += 4;

  /* Call etab */
  bool found = false;
  auto e = getop(opfn);
  if (e && e->fn) {
    dis(op, e->dis);
    switch(e->oparg) {
    case RtRsS16:
      exec2(e->opfn, R[rt], R[rs], (int16_t)imm, found);
      return;
    case RtRsU16:
      exec2(e->opfn, R[rt], R[rs], (int16_t)imm, found);
      if (!found)
	break;
      return;
    case RdRtSa:
      exec2(e->opfn, R[rd], R[rt], sa, found);
      if (!found)
	break;
      return;
    case RdRtRs:
      exec2(e->opfn, R[rd], R[rt], R[rs], found);
      if (!found)
	break;
      return;
    case RdRsRt:
      exec2(e->opfn, R[rd], R[rs], R[rt], found);
      if (!found)
	break;
      return;
    case J26:
      exec2(e->opfn, R[rt], R[rs], op, found);
      if (!found)
	break;
      return;
    }
  };
  if (opfn > 5)
    printf("miss: %.3x\n", opfn);
  // rt|rs,imm  addi,addiu,andi,daddi,daddiu,ori,slti,sltiu, xori
  // rd|rs,rt   add,addu, and, dadd, dsub, dsubu, nor, or, slt, sub, subu, xor
  // 
#if 1
  switch (opfn) {
  case 0x10f: printf("SYNC\n"); break;
  case mkop(0x002, "000010.kkkkk.kkkkk.kkkkk.kkkkk.kkkkkk"):  dis(op, "j      %j"); j(R[rt], R[rs], op); break;
  case mkop(0x003, "000011.kkkkk.kkkkk.kkkkk.kkkkk.kkkkkk"):  dis(op, "jal    %j"); jal(R[rt], R[rs], op); break;
  case mkop(0x004, "000100.sssss.ttttt.iiiii.iiiii.iiiiii"):  dis(op, "beq    %Rt, %Rs, %i"); beq(R[rt], R[rs],  (int16_t)imm); break;
  case mkop(0x005, "000101.sssss.ttttt.iiiii.iiiii.iiiiii"):  dis(op, "bne    %Rt, %Rs, %i"); bne(R[rt], R[rs],  (int16_t)imm); break;
  case mkop(0x006, "000110.sssss.ttttt.iiiii.iiiii.iiiiii"):  dis(op, "blez   %Rt, %Rs, %i"); blez(R[rt], R[rs], (int16_t)imm); break;
  case mkop(0x007, "000111.sssss.ttttt.iiiii.iiiii.iiiiii"):  dis(op, "bgtz   %Rt, %Rs, %i"); bgtz(R[rt], R[rs], (int16_t)imm); break;

  case mkop(0x008, "001000.sssss.ttttt.iiiii.iiiii.iiiiii"):  dis(op, "addi   %Rt, %Rs, %i"); addi(R[rt],R[rs],  (int16_t)imm); break;
  case mkop(0x009, "001001.sssss.ttttt.iiiii.iiiii.iiiiii"):  dis(op, "addiu  %Rt, %Rs, %i"); addiu(R[rt],R[rs], (int16_t)imm); break;
  case mkop(0x00a, "001010.sssss.ttttt.iiiii.iiiii.iiiiii"):  dis(op, "slti   %Rt, %Rs, %i"); slt(R[rt], R[rs],  (int16_t)imm); break; // ok
  case mkop(0x00b, "001011.sssss.ttttt.iiiii.iiiii.iiiiii"):  dis(op, "sltiu  %Rt, %Rs, %i"); sltu(R[rt], R[rs], (int16_t)imm); break; // ok
  case mkop(0x00c, "001100.sssss.ttttt.uuuuu.uuuuu.uuuuuu"):  dis(op, "andi   %Rt, %Rs, %i"); andi(R[rt], R[rs], imm); break; // ok
  case mkop(0x00d, "001101.sssss.ttttt.uuuuu.uuuuu.uuuuuu"):  dis(op, "ori    %Rt, %Rs, %i"); ori(R[rt], R[rs], imm); break; // ok
  case mkop(0x00e, "001110.sssss.ttttt.uuuuu.uuuuu.uuuuuu"):  dis(op, "xori   %Rt, %Rs, %i"); xori(R[rt], R[rs], imm); break; // ok
  case mkop(0x00f, "001111.sssss.ttttt.uuuuu.uuuuu.uuuuuu"):  dis(op, "lui    %Rt, %i");      lui(R[rt], R[rs], imm); break;

  case mkop(0x014, "010100.sssss.ttttt.iiiii.iiiii.iiiiii"):  dis(op, "beql   %Rt, %Rs, %i"); beql(R[rt], R[rs],  (int16_t)imm); break;
  case mkop(0x015, "010101.sssss.ttttt.iiiii.iiiii.iiiiii"):  dis(op, "bnel   %Rt, %Rs, %i"); bnel(R[rt], R[rs],  (int16_t)imm); break;
  case mkop(0x016, "010110.sssss.ttttt.iiiii.iiiii.iiiiii"):  dis(op, "blezl  %Rt, %Rs, %i"); blezl(R[rt], R[rs], (int16_t)imm); break;
  case mkop(0x017, "010111.sssss.ttttt.iiiii.iiiii.iiiiii"):  dis(op, "bgtzl  %Rt, %Rs, %i"); bgtzl(R[rt], R[rs], (int16_t)imm); break;

  case mkop(0x020, "100000.sssss.ttttt.iiiii.iiiii.iiiiii"):  dis(op, "lb     %Rt, %i(%Rs)"); lb(R[rt],  R[rs], (int16_t)imm); break;
  case mkop(0x021, "100001.sssss.ttttt.iiiii.iiiii.iiiiii"):  dis(op, "lh     %Rt, %i(%Rs)"); lh(R[rt],  R[rs], (int16_t)imm); break;
  case mkop(0x023, "100011.sssss.ttttt.iiiii.iiiii.iiiiii"):  dis(op, "lw     %Rt, %i(%Rs)"); lw(R[rt],  R[rs], (int16_t)imm); break;
  case mkop(0x024, "100100.sssss.ttttt.iiiii.iiiii.iiiiii"):  dis(op, "lbu    %Rt, %i(%Rs)"); lbu(R[rt], R[rs], (int16_t)imm); break;
  case mkop(0x025, "100101.sssss.ttttt.iiiii.iiiii.iiiiii"):  dis(op, "lhu    %Rt, %i(%Rs)"); lhu(R[rt], R[rs], (int16_t)imm); break;
  case mkop(0x027, "100111.sssss.ttttt.iiiii.iiiii.iiiiii"):  dis(op, "lwu    %Rt, %i(%Rs)"); lwu(R[rt], R[rs], (int16_t)imm); break;
  case mkop(0x028, "101000.sssss.ttttt.iiiii.iiiii.iiiiii"):  dis(op, "sb     %Rt, %i(%Rs)"); sb(R[rt],  R[rs], (int16_t)imm); break;
  case mkop(0x029, "101001.sssss.ttttt.iiiii.iiiii.iiiiii"):  dis(op, "sh     %Rt, %i(%Rs)"); sh(R[rt],  R[rs], (int16_t)imm); break;
  case mkop(0x02b, "101011.sssss.ttttt.iiiii.iiiii.iiiiii"):  dis(op, "sw     %Rt, %i(%Rs)"); sw(R[rt],  R[rs], (int16_t)imm); break;

  case mkop(0x037, "110111.sssss.ttttt.iiiii.iiiii.iiiiii"):  dis(op, "ld     %Rt, %i(%Rs)"); ld(R[rt],  R[rs], (int16_t)imm); break;
  case mkop(0x03f, "111111.sssss.ttttt.iiiii.iiiii.iiiiii"):  dis(op, "sd     %Rt, %i(%Rs)"); sd(R[rt],  R[rs], (int16_t)imm); break;

  case mkop(0x100, "000000.sssss.ttttt.ddddd.aaaaa.000000"):  dis(op, "sll    %Rd, %Rt, %sa"); sll(R[rd], R[rt], sa); break; // ok
  case mkop(0x102, "000000.sssss.ttttt.ddddd.aaaaa.000010"):  dis(op, "srl    %Rd, %Rt, %sa"); srl(R[rd], R[rt], sa); break; // ok
  case mkop(0x103, "000000.sssss.ttttt.ddddd.aaaaa.000011"):  dis(op, "sra    %Rd, %Rt, %sa"); sra(R[rd], R[rt], sa); break;
  case mkop(0x104, "000000.sssss.ttttt.ddddd.00000.000100"):  dis(op, "sllv   %Rd, %Rt, %Rs"); sll(R[rd], R[rt], R[rs]); break; // ok
  case mkop(0x106, "000000.sssss.ttttt.ddddd.00000.000110"):  dis(op, "srlv   %Rd, %Rt, %Rs"); srl(R[rd], R[rt], R[rs]); break; // ok
  case mkop(0x107, "000000.sssss.ttttt.ddddd.00000.000111"):  dis(op, "srav   %Rd, %Rt, %Rs"); sra(R[rd], R[rt], R[rs]); break;

  case mkop(0x108, "000000.sssss.00000.00000.00000.001000"):  dis(op, "jr     %Rs"); jr(R[rd], R[rt], R[rs]); break;
  case mkop(0x109, "000000.sssss.00000.00000.00000.001001"):  dis(op, "jalr   %Rs"); jalr(R[rd], R[rt], R[rs]); break;

  case mkop(0x110, "000000.00000.00000.ddddd.00000.010000"):  dis(op, "mfhi   %Rd"); mov(R[rd], mhi, 0); break;
  case mkop(0x111, "000000.sssss.00000.00000.00000.010001"):  dis(op, "mthi   %Rs"); mov(mhi, R[rs], 0); break;
  case mkop(0x112, "000000.00000.00000.ddddd.00000.010010"):  dis(op, "mflo   %Rd"); mov(R[rd], mlo, 0); break;
  case mkop(0x113, "000000.sssss.00000.00000.00000.010011"):  dis(op, "mtlo   %Rs"); mov(mlo, R[rs], 0); break;
  case mkop(0x114, "000000.sssss.ttttt.ddddd.aaaaa.010100"):  dis(op, "dsllv  %Rd, %Rt, %Rs"); dsll(R[rd], R[rt], R[rs]); break;
    
  case mkop(0x120, "000000.sssss.ttttt.ddddd.00000.100000"):  dis(op, "add    %Rd, %Rs, %Rt"); addi(R[rd], R[rs], R[rt]); break;
  case mkop(0x121, "000000.sssss.ttttt.ddddd.00000.100001"):  dis(op, "addu   %Rd, %Rs, %Rt"); addiu(R[rd], R[rs], R[rt]); break;
  case mkop(0x122, "000000.sssss.ttttt.ddddd.00000.100010"):  dis(op, "sub    %Rd, %Rs, %Rt"); sub(R[rd], R[rs], R[rt]); break;
  case mkop(0x123, "000000.sssss.ttttt.ddddd.00000.100011"):  dis(op, "subu   %Rd, %Rs, %Rt"); subu(R[rd], R[rs], R[rt]); break;
  case mkop(0x124, "000000.sssss.ttttt.ddddd.00000.100100"):  dis(op, "and    %Rd, %Rs, %Rt"); andi(R[rd], R[rs], R[rt]); break; // ok
  case mkop(0x125, "000000.sssss.ttttt.ddddd.00000.100101"):  dis(op, "or     %Rd, %Rs, %Rt"); ori(R[rd], R[rs], R[rt]); break; // ok
  case mkop(0x126, "000000.sssss.ttttt.ddddd.00000.100110"):  dis(op, "xor    %Rd, %Rs, %Rt"); xori(R[rd], R[rs], R[rt]); break; // ok
  case mkop(0x127, "000000.sssss.ttttt.ddddd.00000.100111"):  dis(op, "nor    %Rd, %Rs, %Rt"); nor(R[rd], R[rs], R[rt]); break; // ok

  case mkop(0x12a, "000000.sssss.ttttt.ddddd.00000.101010"):  dis(op, "slt    %Rd, %Rs, %Rt"); slt(R[rd], R[rs], R[rt]); break; // ok
  case mkop(0x12b, "000000.sssss.ttttt.ddddd.00000.101011"):  dis(op, "sltu   %Rd, %Rs, %Rt"); sltu(R[rd], R[rs], R[rt]); break; // ok
  case mkop(0x138, "000000.00000.ttttt.ddddd.aaaaa.111000"):  dis(op, "dsll   %Rd, %Rt, %sa"); dsll(R[rd], R[rt], sa); break;


  default:
    printf("\n%.8x ==== op: %.8x %.4x %.2x %.2x\n", (int)PC, op, opfn, rt, rs);
    showregs();
    vi.show();
  }
#endif
}

void dumpcfg(uint8_t *rom, size_t size, int off)
{
  dstk stk(size, printf);
  uint32_t op, opfn;
  int nxt[2];
  
  stk.push(off, 1, dstk::PENDING, "first");
  while ((off = stk.pop()) != -1) {
    printf("\n------------------------- %.8x [%s]\n", off, "");
    do {
      op = get32be(&rom[off]);
      opfn = (op >> 26) & 0x3F;
      if (!opfn) {
	opfn = (op & 0x3F) + 0x1;;
      }
      auto e = getop(opfn);
      if (!e) {
	printf("missing: %x\n", opfn);
	exit(0);
      }
      printf("%.8x | %.8x | %s\n", off, op, dis(op, e->dis));
      nxt[0] = off + 4;
      nxt[1] = -1;
      if (e->flag) {
	if (e->oparg == RtRsS16) {
	  nxt[1] = off + ((int16_t)op << 2) + 4;
	  printf(" nxt: %x\n", nxt[1]);
	}
	if (opfn == 0x002) {
	  nxt[1] = (op & 0x03FFFFFF) << 2;
	  printf(" nxt: %x\n", nxt[1]);
	}
	if (opfn == 0x108) {
	  nxt[0] = -1;
	}
	if (opfn == 0x109) {
	  nxt[1] = nxt[0];
	  nxt[0] = -1;
	}
      }
      stk.push(off, 4, dstk::CODE, "visit");
      for (int i = 0; i < 2; i++) {
	stk.push(nxt[i], 1, dstk::PENDING, "...");
      }
      off += 4;
    } while (nxt[0] != -1 && nxt[1] == -1);
  }
  exit(0);
}

void init(uint8_t *rom, size_t romsz, uint64_t pc)
{
  cpu c;
  Screen *scr;
  uint32_t cycles = 0;
  int frame = 0;
  
  //dumpcfg(rom, romsz, 0x1000);

  scr = new Screen(640, 480, 400, 50);
  scr->xs = 2;
  scr->ys = 2;

  mb.register_handler(0x00000000, romsz, 0x0fffffff, bememio, rom, _RW, "ROM");
  //set_page(0x80000000,   romsz, rom);
  scr->clrmode = 1;
  scr->init(1);
  
  c.reset(pc);
  for(;;) {
#if 0
    auto start = std::chrono::system_clock::now();
    auto end = std::chrono::system_clock::now();
    auto elapsed = std::chrono::duration_cast<std::chrono::nanoseconds>(end - start);
#endif
    c.exec();
    if (cycles++ == 13000) {
      cycles = 0;

      drawscr(scr, 0, 0, xx(vi.vi_width) & 0x3FF, 400, (uint32_t*)&rom[0x100000]);
      scr->scrtext(0, scr->height+5, MKRGB(255,255,0), "frame:%d %.8x %3d",
		   frame++, xx(vi.vi_origin), xx(vi.vi_width));
      
      /* Draw Font */
      scr->draw();
      scr->clear();
    }
  }
}

int main(int argc, char *argv[])
{
  uint8_t *rom;
  size_t romsz;
  RomHeader *hdr;

  if (argc > 1) {
    rom = loadrom(argv[1], romsz, true);
  } else {
    rom = loadrom("main.z64", romsz, true);
  }
  /* Make sure buffer is power of 2 */
  hdr = (RomHeader *)rom;
  printf("romsz: %x %x '%.20s'\n", (int)romsz, *(uint32_t *)rom, hdr->image_name);
  printf("PC: %.8x %.2x\n", get32be(&hdr->program_counter), rom[0]);
  printf("CRC: %.8x\n", crc32(&rom[0x40], 0x1000 - 0x40));
  init(rom, romsz, get32be(&hdr->program_counter));
}
