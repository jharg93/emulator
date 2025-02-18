#include <inttypes.h>

void testrig(uint16_t &dst, uint16_t val, uint32_t expflg, const char *test, int nbytes, const uint8_t *cb);
void ckval(uint16_t val, uint16_t expval, uint16_t flag, uint16_t expflag, const char *test);
uint8_t rig[256];

uint32_t flagmask = -1;

#define fC 0x001
#define fP 0x004
#define fA 0x010
#define fZ 0x040
#define fS 0x080
#define fT 0x100
#define fI 0x200
#define fD 0x400
#define fO 0x800

/* mod-reg-byte for group */
uint8_t mrr_opreg(int op, int reg) {
  return (3 << 6) | ((op & 7) << 3) | (reg & 7);
}

/* generic code emit */
uint8_t *emit_n(uint8_t *ptr, int n, ...)
{
  va_list ap;
  va_start(ap, n);
  while (n--) {
    *ptr++ = va_arg(ap, int);
  }
  return ptr;
}

/* Load a 8-bit register with an 8-bit value */
uint8_t *emit_movib(uint8_t *ptr, int reg, int ib) {
  *ptr++ = 0xb0 + (reg & 7);
  *ptr++ = ib;
  return ptr;
}

/* Load a 16-bit register with an 16-bit value */
uint8_t *emit_moviw(uint8_t *ptr, int reg, int iw, bool pfx = false) {
  // 16-bit prefix
  if (pfx)
    *ptr++ = 0x66;
  *ptr++ = 0xb8 + (reg & 7);
  *ptr++ = (iw & 0xFF);
  *ptr++ = (iw >> 8);
  return ptr;
}

/* perform math operation with 8-bit register + immediate byte */
uint8_t *emit_mathib(uint8_t *ptr, int op, int reg, int ib)
{
  switch (op) {
  case x86_daa: case x86_das: case x86_aaa: case x86_aas:
    *ptr++ = op;
    break;
  case x86_aam: case x86_aad:
    *ptr++ = op;
    *ptr++ = 0xa;
    break;
  case x86_add ... x86_cmp:
    // add, or, etc Eb, Ib
    if (reg == rAL) {
      // use special case for rAL,Ib
      *ptr++ = ((op & 7) << 3) + 0x4;
      *ptr++ = ib;
      break;
    }
    // use mrr GRP1 11.ggg.rrr
    *ptr++ = 0x80;
    *ptr++ = mrr_opreg(op, reg);
    *ptr++ = ib;
    break;
  case x86_rol ... x86_sar:
    // shl, rol, etc Eb, Ib
    // use mrr GRP2 11.ggg.rrr
    if (ib == 1) {
      *ptr++ = 0xd0;
      *ptr++ = mrr_opreg(op, reg);
      break;
    }
    *ptr++ = 0xc0;
    *ptr++ = mrr_opreg(op, reg);
    *ptr++ = ib;
    break;
  case x86_test:
    if (reg == rAL) {
      // use special case for rAL,Ib
      *ptr++ = 0xa8;
      *ptr++ = ib;
      break;
    }
    // test GRP3 11.000.rrr
    *ptr++ = 0xf6;
    *ptr++ = mrr_opreg(op, reg);
    *ptr++ = ib;
    break;
  case x86_not:
  case x86_neg:
  case x86_mul:
  case x86_div:
  case x86_imul3:
  case x86_idiv:
    // not GRP3 ll.010.rrr
    // neg GRP3 ll.011.rrr
    // mul GRP3 ll.100.rrr
    // div GRP3 ll.110.rrr
    *ptr++ = 0xf6;
    *ptr++ = mrr_opreg(op, reg);
    break;
  case x86_inc:
  case x86_dec:
    // inc GRP4 11.000.rrr
    // dec GRP4 11.001.rrr
    *ptr++ = 0xfe;
    *ptr++ = mrr_opreg(op, reg);
    break;
  default:
    assert(0);
  }
  return ptr;
}

/* perform math operation with 16-bit register + immediate word */
uint8_t *emit_mathiw(uint8_t *ptr, int op, int r1, int iw, bool pfx = false)
{
  if (pfx)
    *ptr++ = 0x66;
  switch (op) {
  case x86_add ... x86_cmp:
    // add, or, etc Eb, Ib
    if (r1 == rAX) {
      // use special case for rAX,Iw
      *ptr++ = ((op & 7) << 3) + 0x5;
      *ptr++ = iw;
      *ptr++ = iw >> 8;
      break;
    }
    // use mrr 11.ggg.rrr
    *ptr++ = 0x81;
    *ptr++ = mrr_opreg(op, r1);
    *ptr++ = iw;
    *ptr++ = iw >> 8;
    break;
  case x86_rol ... x86_sar:
    // shl, rol, etc Ew, Iw
    // use mrr 11.ggg.rrr
    if (iw == 1) {
      *ptr++ = 0xd1;
      *ptr++ = mrr_opreg(op, r1);
      break;
    }
    *ptr++ = 0xc1;
    *ptr++ = mrr_opreg(op, r1);
    *ptr++ = iw;
    break;
  case x86_test:
    if (r1 == rAX) {
      // use special case for rAX,Iw
      *ptr++ = 0xa9;
      *ptr++ = iw;
      *ptr++ = iw >> 8;
      break;
    }
    // use mrr 11.ggg.rrr
    *ptr++ = 0xf7;
    *ptr++ = mrr_opreg(op, r1);
    *ptr++ = iw;
    *ptr++ = iw >> 8;
    break;
  case x86_inc:
    // GRP4 11.000.rrr
    *ptr++ = 0xff;
    *ptr++ = mrr_opreg(0x0, r1);
    break;
  case x86_dec:
    // GRP4 11.001.rrr
    *ptr++ = 0xff;
    *ptr++ = mrr_opreg(0x1, r1);
    break;
  case x86_neg:
  case x86_not:
  case x86_mul:
  case x86_div:
  case x86_imul:
  case x86_idiv:
    // GRP3 ll.ggg.rrr
    *ptr++ = 0xf7;
    *ptr++ = mrr_opreg(op, r1);
    break;
  default:
    assert(0);
  }
  return ptr;
}

uint8_t *emit_ret(uint8_t *ptr) {
  *ptr++ = 0xc3;
  return ptr;
}

struct fnret {
  uint64_t val;
  uint64_t flag;
};

/* Test 8-bit operation */
void etest8(uint8_t *codez, int k, int j, int op, int reg, int cfop = x86_clc)
{
  fnret (*fn)() = (fnret (*)())codez;
  fnret exp;
  uint8_t *pc;

  /* Run on cpu, get result */
  pc = codez;
  memset(codez, 0, 512);
  pc = emit_n(pc, 3, 0x31, 0xc0, cfop);  // xor ax,ax ; stc/clc
  if (op == x86_mul || op == x86_imul3) {
    // mul reg
    pc = emit_movib(pc, rAL, k);
    pc = emit_movib(pc, reg, j);
  }
  else if (op == x86_div || op == x86_idiv) {
    pc = emit_moviw(pc, rAX, k, true);
    pc = emit_movib(pc, reg, j);
  }
  else {
    pc = emit_movib(pc, reg, k);         // mov reg, k
  }
  pc = emit_mathib(pc, op, reg, j);      // op  reg, j
  pc = emit_n(pc, 3, x86_pushf, 0x5a, 0xc3); // pushf ; pop dx ; ret
  hexdump(codez, pc - codez, 32);

  exp = fn();

  /* Emit test code to emulator */
  cpu_setflags(0x2); // set reserved
  If = 1;
  Zf = 1;
  Pf = 1;
  if (cfop == x86_stc)
    Cf = 1;
  for (int i = 0; i < 8; i++)
    regs[i].w = 0;
  if (op == x86_mul || op == x86_imul3) {
    x86_set(rAL, k);
    x86_set(reg, j);
  }
  else if (op == x86_div || op == x86_idiv) {
    x86_set(rAX, k);
    x86_set(reg, j);
  }
  else {
    x86_set(reg, k);
  }
  
  pc = codez;
  pc = emit_mathib(pc, op, reg, j); // testing this code
  testrig(AX, exp.val, exp.flag, "add al,ib", pc - codez, codez);
}

void etest16(uint8_t *codez, int k, int j, int op, int reg, int cfop = x86_clc)
{
  fnret (*fn)() = (fnret (*)())codez;
  fnret exp;
  uint8_t *pc;
  
  // xor ax,ax ; stc/clc ; pfx16.mov ax, xxx ; pfx16.<fn> al, xxx ; ret
  pc = codez;
  pc = emit_n(pc, 3, 0x31, 0xc0, cfop);  // xor ax,ax ; clc/stc ; code16
  if (op == x86_mul || op == x86_imul3) {
    // mul reg
    pc = emit_moviw(pc, rAL, k);
    pc = emit_moviw(pc, reg, j, true);
  }
  else if (op == x86_div || op == x86_idiv) {
    pc = emit_moviw(pc, rAX, k);
    pc = emit_moviw(pc, reg, j, true);
  }
  else {
    pc = emit_moviw(pc, reg, k, true);         // mov reg, k
  }
  pc = emit_mathiw(pc, op, reg, j, true);      // op  reg, j
  pc = emit_n(pc, 3, x86_pushf, 0x5a, 0xc3);   // pushf ; pop dx ; ret
  exp = fn();

  // Now run CPU emulator
  /* Emit test code to emulator */
  cpu_setflags(0x2);
  If = 1;
  Zf = 1;
  Pf = 1;
  if (cfop == x86_stc)
    Cf = 1;
  for (int i = 0; i < 8; i++)
    regs[i].w = 0;
  if (op == x86_mul || op == x86_imul3) {
    x86_set(rAX, k);
    x86_set(reg, j);
  }
  else if (op == x86_div || op == x86_idiv) {
    x86_set(rAX, k);
    x86_set(rDX, k >> 16);
    x86_set(reg, j);
  }
  else {
    x86_set(reg, k);
  }

  pc = codez;
  pc = emit_mathiw(pc, op, reg, j); // testing this code
  testrig(AX, exp.val, exp.flag, "add ax,iw", pc - codez, codez);
}

void ckval(uint16_t val, uint16_t expval, uint16_t flag, uint16_t expflag, const char *test)
{
  flag    &= flagmask;
  expflag &= flagmask;
  printf(" Expected flag:%.4x %.4x %s\n", expflag, flag, expflag == flag ? "" : "mismatch");
  printf(" Expected val :%.4x %.4x %s\n", expval, val, expval == val ? "" : "mismatch");
}

void testrig2(int nbytes, uint8_t *cb, const char *lbl, int ntest, ...)
{
  va_list ap;
  bool failed = false;
  
  SPC = -1;
  PC = 0x1000;
  for (int i = 0; i < nbytes; i++) {
    cpu_write8(0x1000 + i, cb[i]);
  }
  cpu_step();

  va_start(ap, ntest);
  for (int i = 0; i < ntest; i++) {
    uint32_t *result = va_arg(ap, uint32_t *);
    uint32_t  expect = va_arg(ap, uint32_t);

    if (result && *result != expect) {
      printf("  %d:Failed: %.8x != %.8x [%s]\n",
	     i, *result, expect, lbl);
      failed = true;
    }
    else if (result) {
      printf("  %d:ok (%.8x)\n", i, *result);
    }
  }
  if (!failed) {
    printf("  success: %s\n", lbl);
  }
  printf("\n\n");
}

/* Step instructions, check result value */
void testrig(uint16_t &dst, uint16_t val, uint32_t expflg, const char *test, int nbytes, const uint8_t *cb)
{
  uint8_t c;
  
  CS = 0x1000;
  PC = 0x0000;
  for (int i = 0; i < nbytes; i++) {
    c = cb[i];
    if (!i) {
      /* Poke which instructions have been tested */
      rig[c] = 0x11;
    }
    cpu_write8(0x10000 + i, c);
  }
  cpu_step();
  ckval(dst, val, cpu_getflags(), expflg, test);
}

void runtest()
{
  int i, a, b;
  uint8_t *codez;
  uint8_t cb[32];
  int reg;

  printf("codez\n");
  codez = (uint8_t *)mmap(NULL, 4096, PROT_READ|PROT_WRITE|PROT_EXEC,
			  MAP_PRIVATE|MAP_ANONYMOUS, -1, 0);
  if (codez != MAP_FAILED) {
    /* mov ax, -1; push eax ; popf ; pushf ; pop eax */
#if 0
    for (int i = 0; i < 256; i++) {
      etest8(codez, i, 0, x86_aaa, rAL, x86_clc);
      etest8(codez, i, 0, x86_aas, rAL, x86_clc);
      etest8(codez, i, 0, x86_aad, rAL, x86_clc);
      etest8(codez, i, 0, x86_aam, rAL, x86_clc);
      etest8(codez, i, 0, x86_daa, rAL, x86_clc);
      etest8(codez, i, 0, x86_das, rAL, x86_clc);
      etest8(codez, i, 0, x86_aaa, rAL, x86_stc);
      etest8(codez, i, 0, x86_aas, rAL, x86_stc);
      etest8(codez, i, 0, x86_aad, rAL, x86_stc);
      etest8(codez, i, 0, x86_aam, rAL, x86_stc);
      etest8(codez, i, 0, x86_daa, rAL, x86_stc);
      etest8(codez, i, 0, x86_das, rAL, x86_stc);
    }
#endif
    reg = rCX;
    etest8(codez,  0x07, 0x14, x86_mul, reg, x86_clc);
    etest8(codez,  0xff, 0x24, x86_mul, reg, x86_clc);
    etest8(codez,  0xff, 0x01, x86_mul, reg, x86_clc);
    etest8(codez,  0xff, 0xff, x86_mul, reg, x86_clc);
    etest8(codez,  0xc5, 0x00, x86_mul, reg, x86_clc);
    etest8(codez,  0xb5, 0xf9, x86_mul, reg, x86_clc);

    etest8(codez,  0xfff, 0x14, x86_div, reg, x86_clc);
    etest8(codez,  0xfff, 0x24, x86_div, reg, x86_clc);
    etest8(codez,  0xff, 0x01, x86_div, reg, x86_clc);
    etest8(codez,  0xff, 0xff, x86_div, reg, x86_clc);
    etest8(codez,  0xc5, 0x01, x86_div, reg, x86_clc);
    etest8(codez,  0xb5, 0xf9, x86_div, reg, x86_clc);

    etest8(codez,  0xfff, 0x14, x86_idiv, reg, x86_clc);
    etest8(codez,  0xfff, 0x24, x86_idiv, reg, x86_clc);
    etest8(codez,  0xff, 0x01, x86_idiv, reg, x86_clc);
    etest8(codez,  0xff, 0xff, x86_idiv, reg, x86_clc);
    etest8(codez,  0xc5, 0x01, x86_idiv, reg, x86_clc);
    etest8(codez,  0xb5, 0xf9, x86_idiv, reg, x86_clc);

    etest8(codez,  0x07, 0x14, x86_imul3, reg, x86_clc);
    etest8(codez,  0xff, 0x24, x86_imul3, reg, x86_clc);
    etest8(codez,  0xff, 0x01, x86_imul3, reg, x86_clc);
    etest8(codez,  0xff, 0xff, x86_imul3, reg, x86_clc);
    etest8(codez,  0xc5, 0x00, x86_imul3, reg, x86_clc);
    etest8(codez,  0xb5, 0xf9, x86_imul3, reg, x86_clc);
#if 0
    etest16(codez, 0x0007, 0x0003, x86_mul, reg, x86_clc);
    etest16(codez, 0xffff, 0xa320, x86_mul, reg, x86_clc);
    etest16(codez, 0xffff, 0x0001, x86_mul, reg, x86_clc);
    etest16(codez, 0xffff, 0xffff, x86_mul, reg, x86_clc);
    etest16(codez, 0x46db, 0x0000, x86_mul, reg, x86_clc);
    etest16(codez, 0x46db, 0xeeeb, x86_mul, reg, x86_clc);
    #endif
    exit(0);
#if 0
    reg = rCL;
    flagmask = -1;
    //flagmask = ~(fS|fZ|fA|fP);
    for (int i = 0; i < 256; i++) {
      for (int j = 0; j < 256; j++) {
	etest8(codez, i, j, x86_mul,   reg, x86_clc);
	etest8(codez, i, j, x86_mul,   reg, x86_stc);
	etest8(codez, i, j, x86_imul3, reg, x86_clc);
	etest8(codez, i, j, x86_imul3, reg, x86_stc);
      }
    }
    flagmask = -1;
#endif
#if 0
    reg = rAX;
    for (int i = 0; i < 256; i++) {
      for (int j = 0; j < 16; j++) {
	//flagmask = (j == 1) ? -1 : ~fO;
	etest8(codez, i, j, x86_rol, reg);
	etest8(codez, i, j, x86_ror, reg);
	//etest8(codez, i, j, x86_rcl, reg, x86_clc);
	//etest8(codez, i, j, x86_rcr, reg, x86_clc);
	//etest8(codez, i, j, x86_rcl, reg, x86_stc);
	//etest8(codez, i, j, x86_rcr, reg, x86_stc);
	//etest8(codez, i, j, x86_shl, reg);
	//etest8(codez, i, j, x86_sal, reg);
	//etest8(codez, i, j, x86_shr, reg);
	//etest8(codez, i, j, x86_sar, reg);
      }
    }
#endif
#if 0
    reg = rAX;
    for (int ii = 0; ii < 256; ii++) {
      for (int j = 0; j < 16; j++) {
	int i = rand();
	//flagmask = (j == 1) ? -1 : ~fO;
	etest16(codez, i, j, x86_rol, reg);
	etest16(codez, i, j, x86_ror, reg);
	etest16(codez, i, j, x86_rcl, reg, x86_clc);
	etest16(codez, i, j, x86_rcr, reg, x86_clc);
	etest16(codez, i, j, x86_rcl, reg, x86_stc);
	etest16(codez, i, j, x86_rcr, reg, x86_stc);
	etest16(codez, i, j, x86_shl, reg);
	etest16(codez, i, j, x86_sal, reg);
	etest16(codez, i, j, x86_shr, reg);
	etest16(codez, i, j, x86_sar, reg);
      }
    }
#endif
#if 0
    reg = rAX;
    for (int i = 0; i < 256; i++) {
      for (int j = 0; j < 256; j++) {
	reg = rAL;
	etest8(codez, i, j, x86_not, reg); // ok
	etest8(codez, i, j, x86_neg, reg); // ok
	etest8(codez, i, j, x86_test,reg); // ok
	etest8(codez, i, j, x86_inc, reg); // ok
	etest8(codez, i, j, x86_dec, reg); // ok
	etest8(codez, i, j, x86_add, reg); // ok
	etest8(codez, i, j, x86_adc, reg, x86_clc); // ok
	etest8(codez, i, j, x86_adc, reg, x86_stc); // ok
	etest8(codez, i, j, x86_or,  reg); // ok
	etest8(codez, i, j, x86_and, reg); // ok
	etest8(codez, i, j, x86_xor, reg); // ok
	etest8(codez, i, j, x86_sub, reg); // ok
	etest8(codez, i, j, x86_cmp, reg); // ok
	etest8(codez, i, j, x86_sbb, reg, x86_clc); // ok
	etest8(codez, i, j, x86_sbb, reg, x86_stc); // ok
      }
    }
#endif
#if 0
    for (int ii = 0; ii < 100; ii++) {
      for (int ij = 0; ij < 100; ij++ ) {
	reg = rAX;
	int i = rand();
	int j = rand();
	etest16(codez, i, j, x86_not, reg); // ok
	etest16(codez, i, j, x86_neg, reg); // ok
	etest16(codez, i, j, x86_test,reg); // ok
	etest16(codez, i, j, x86_inc, reg); // ok
	etest16(codez, i, j, x86_dec, reg); // ok
	etest16(codez, i, j, x86_add, reg); // ok
	etest16(codez, i, j, x86_adc, reg, x86_clc); // ok
	etest16(codez, i, j, x86_adc, reg, x86_stc); // ok
	etest16(codez, i, j, x86_or,  reg); // ok
	etest16(codez, i, j, x86_and, reg); // ok
	etest16(codez, i, j, x86_xor, reg); // ok
	etest16(codez, i, j, x86_sub, reg); // ok
	etest16(codez, i, j, x86_cmp, reg); // ok
	etest16(codez, i, j, x86_sbb, reg, x86_clc); // ok
	etest16(codez, i, j, x86_sbb, reg, x86_stc); // ok
      }
    }
#endif
  }
  AX = 0x007B;
  cb[0] = x86_cbw;
  testrig(AX, 0x007B, -1, "cbw", 1, cb);

  AX = 0x00FB;
  testrig(AX, 0xFFFB, -1, "cbw", 1, cb);
  Sf = Zf = Af = Pf = Cf = 1;
  testrig(PC, 0x2, -1, "jnc", 2, emit_n(codez, 2, 0x73, 0x55 ));
  testrig(PC, 0x2, -1, "jnz", 2, emit_n(codez, 2, 0x75, 0x55 ));
  testrig(PC, 0x2, -1, "jpo", 2, emit_n(codez, 2, 0x7b, 0x55 ));
  testrig(PC, 0x2, -1, "jns", 2, emit_n(codez, 2, 0x79, 0x55 ));

  Sf = Zf = Af = Pf = Cf = 0;
  testrig(PC, 0x2, -1, "jc",  2, emit_n(codez, 2, 0x72, 0x55 ));
  testrig(PC, 0x2, -1, "jz",  2, emit_n(codez, 2, 0x74, 0x55 ));
  testrig(PC, 0x2, -1, "jpe", 2, emit_n(codez, 2, 0x7a, 0x55 ));
  testrig(PC, 0x2, -1, "js",  2, emit_n(codez, 2, 0x78, 0x55 ));

  /* Test XCHG */
  for (i = 1; i < 8; i++) {
    a = rand();
    b = rand();
    
    regs[0].w = a;
    regs[i].w = b;
    cb[0] = 0x90 + i;
    testrig(AX, b, -1, "xchg", 1, cb);

    regs[0].w = b;
    regs[i].w = a;
    testrig(AX, a, -1, "xchg", 1, cb);
  }
  cpu_setflags(0x1);
  cb[0] = x86_clc;
  //testrig(eflags, 0x00, 0x00, "clc", 1, cb);

  cpu_setflags(0x0);
  cb[0] = x86_stc;
  //testrig(eflags, 0x01, 0x01, "stc", 1, cb);

  DS = 0xABCD;
  ES = 0xCAFE;
  SS = 0x1000;
  SP = 0x2000;

  cb[0] = 0x06;
  cb[1] = 0x0e;
  cb[2] = 0x16;
  cb[3] = 0x1e;
  testrig(*(uint16_t *)&cpu.ram[0x11FFE], ES, -1, "push es", 1, &cb[0]);
  testrig(*(uint16_t *)&cpu.ram[0x11FFC], CS, -1, "push cs", 1, &cb[1]);
  testrig(*(uint16_t *)&cpu.ram[0x11FFA], SS, -1, "push ss", 1, &cb[2]);
  testrig(*(uint16_t *)&cpu.ram[0x11FF8], DS, -1, "push ds", 1, &cb[3]);

  DS = 0xCAFE;
  ES = 0xABCD;
  cb[0] = 0x1f;
  cb[1] = 0x58;
  cb[2] = 0x58;
  cb[3] = 0x07;
  testrig(DS, 0xABCD, -1, "pop ds", 1, &cb[0]);
  testrig(AX, SS, -1, "pop ax", 1, &cb[1]);
  testrig(AX, CS, -1, "pop ax", 1, &cb[2]);
  testrig(ES, 0xCAFE, -1, "pop es", 1, &cb[3]);

#if 0
  /* Test mul */
  emit_mathib(cb, x86_mul, rCL, 0x00);
  for(int i = 0; i < 256; i++) {
    for (int j = 0; j < 256; j++) {
      x86_set(rAX, i);
      x86_set(rCL, j);
      testrig(AX, i * j, -1, "mul", 2 , cb);
    }
  }
  
  /* Test div */
  cb[0] = 0xf6;
  cb[1] = (3 << 6) | ((x86_div & 7) << 3) | (rCL & 7);
  for(int i = 0; i < 256; i++) {
    for (int j = 1; j < 256; j++) {
      c = rand() & 0xFFFF;
      x86_set(rAX, c);
      x86_set(rCL, j);

      a = (uint8_t)(c / j);
      b = (uint8_t)(c % j);
      testrig(AX, (b << 8) + a, -1, "div", 2 , cb);
    }
  }
#endif
  
  memset(&cpu.ram[0x1000], 0xaa, 0xffff);
  ES = 0x200;
  DI = 0x0000;
  AX = 0x0022;
  CX = 0x023;
  Df = 0;
  cb[0] = x86_repz;
  cb[1] = x86_stosb;
  testrig(DI, 0x23, -1, "stosb", 2, cb);
  hexdump(&cpu.ram[0x2000], 0x1000, 32);

  DS = 0x200;
  SI = 0x0000;
  ES = 0x0220;
  DI = 0x0000;
  CX = 0x23;
  cb[0] = x86_repz;
  cb[1] = x86_movsb;
  testrig(SI, 0x23, -1, "movsb", 2, cb);
  hexdump(&cpu.ram[0x2000], 0x1000, 32);

  DS = 0x220;
  SI = 0x22;
  ES = 0x240;
  DI = 0x22;
  CX = 0x11;
  Df = 1;
  cb[0] = x86_repz;
  cb[1] = x86_movsw;
  testrig(SI, 0x0000, -1, "movsb", 2, cb);
  hexdump(&cpu.ram[0x2000], 0x1000, 32);

  AX = 0;
  Cf = 0;
  cb[0] = x86_cmc;
  testrig(AX, 0, -1, "cmc", 1, cb);
  assert(Cf == 1);
  
  AX = 0;
  Cf = 1;
  cb[0] = x86_cmc;
  testrig(AX, 0, -1, "cmc", 1, cb);
  assert(Cf == 0);

  AX = 0;
  Cf = 1;
  cb[0] = x86_clc;
  testrig(AX, 0, -1, "clc", 1, cb);
  assert(Cf == 0);

  AX = 0;
  Cf = 0;
  cb[0] = x86_stc;
  testrig(AX, 0, -1, "clc", 1, cb);
  assert(Cf == 1);

  AX = 0;
  If = 1;
  cb[0] = x86_cli;
  testrig(AX, 0, -1, "cli", 1, cb);
  assert(If == 0);

  AX = 0;
  If = 0;
  cb[0] = x86_sti;
  testrig(AX, 0, -1, "sti", 1, cb);
  assert(If == 1);
  
  printf("===== SUCCESS\n");
  hexdump(rig, 256, 16);
  exit(0);
}

#if 0
GBOY:

/*
 * ld rd,rs : MOVRR(rd, rs)
 * ld rd,mhl: MOVRM(rd, HL)
 * ld mhl,rs: MOVMR(HL, REG+rs)
 * add rs   : ADD(rA, rs)
 * xor rs   : ADD(rA, rs)
 */
00
10
20
30
40 mov 
50
60
70
80
90
a0
b0
c0
d0
e0
f0


#endif
