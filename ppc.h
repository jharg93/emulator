#ifndef __ppc_h__
#define __ppc_h__

void xer_ov_test(const char *str, uint32_t op)
{
  regs[3] = 2;
  regs[4] = 2;
  XER     = 0xFFFFFFFF;
  ppc_exec(op);
  if (XER_OV) {
    printf("  ERROR: %-12s:%.8x OV should not be set!!\n", str, op);
  }
}

template <class T>
bool scanpos(const char *str, const char *match, T& v)
{
  T xxx;
  
  if (sscanf(str, match, &xxx) == 1) {
    v = xxx;
    return true;
  }
  return false;
}

void ppc_inttest()
{
  FILE *fp;
  char line[128], *pos;

  if ((fp = fopen("ppcinttests.csv", "r")) == NULL)
    exit(0);
  while (fgets(line, sizeof(line), fp) != NULL) {
    uint32_t dst = 0, cr = 0, xer, op, dstvalid;

    dst = cr = xer = op = dstvalid = 0;

    printf("==========================================\n");
    pos = strtok(line, ",");
    printf("%s:", line);
    while ((pos = strtok(NULL, ",")) != NULL) {
      printf("%s ", pos);
      scanpos<uint32_t>(pos, "0x%x", op);
      scanpos<uint32_t>(pos, "rA=0x%x", regs[3]);
      scanpos<uint32_t>(pos, "rB=0x%x", regs[4]);
      scanpos<uint32_t>(pos, "XER=0x%x", xer);
      scanpos<uint32_t>(pos, "CR=0x%x", cr);
      if (scanpos<uint32_t>(pos, "rD=0x%x", dst))
	dstvalid=1;
    }
    XER = 0;
    CR[0] = CR[1] = CR[2] = CR[3] = 0;
    ppc_exec(op);
    if (XER != xer) {
      printf("  ERROR: %-12s: XER %.8x expect:%.8x\n", line, XER, xer);
      assert(0);
    }
    if (ppc_get_cr() != cr) {
      printf("  ERROR: %-12s: CR %.8x expect:%.8x\n", line, ppc_get_cr(), cr);
      assert(0);
    }
    if ((dst != regs[3]) && dstvalid) {
      printf("  ERROR: %-12s  %.8x != expect:%.8x\n", line, regs[3], dst);
      assert(0);
    }
  }
  showop();
}

void ppc_floattest()
{
  FILE *fp;
  char line[512], *pos;
  uint32_t xer, op, dstvalid;
  uint64_t rd;
  
  if ((fp = fopen("ppcfloattests.csv", "r")) == NULL)
    exit(0);
  while (fgets(line, sizeof(line), fp) != NULL) {
    printf("==========================================\n");
    pos = strtok(line, ",");
    printf("%s:\n", line);
    while ((pos = strtok(NULL, ",")) != NULL) {
      dstvalid = 0;
      printf("pos:%s\n", pos);
      if (scanpos<uint32_t>(pos, "0x%x", op)) {
	printf("  d:%x a:%x b:%x c:%x\n", (op >> 21) & 0x1F, (op >> 16) & 0x1F, (op >> 11) & 0x1F, (op >> 6) & 0x1f);
      }
      if (scanpos<uint64_t>(pos, "frD=0x%llx", rd)) {
	*(uint64_t *)&fpregs[3].d = rd;
	printf(" floaty: %lf\n", fpregs[3].d);
	dstvalid = 1;
      }
      if (scanpos<double>(pos, "frA=%lf", fpregs[3].d)) {
	printf(" floaty: %lf\n", fpregs[3].d);
      }
      if (scanpos<double>(pos, "frB=%lf", fpregs[4].d)) {
	printf(" floaty: %lf\n", fpregs[4].d);
      }
      if (scanpos<double>(pos, "frC=%lf", fpregs[5].d)) {
	printf(" floaty: %lf\n", fpregs[5].d);
      }
    }
    ppc_exec(op);
    if (dstvalid && fpregs[3].d != *(double *)&rd) {
      printf("ERROR: frd mismatch: %lf %lf\n", fpregs[3].d, *(double *)&rd);
    }
  }
  showop();
}

/* PPCInttests.csv from: https://github.com/dingusdev/dingusppc/blob/master/cpu/ppc/test/ppcinttests.csv
 */
void xer_test()
{
  ppc_inttest();
#if 0  
  ppc_floattest();
  xer_ov_test("ADDCO", 0x7C632414);
  xer_ov_test("ADDCO.", 0x7C632415);
  xer_ov_test("ADDO", 0x7C632614);
  xer_ov_test("ADDO.", 0x7C632615);
  xer_ov_test("ADDEO", 0x7C632514);
  xer_ov_test("ADDEO.", 0x7C632515);
  xer_ov_test("ADDMEO", 0x7C6305D4);
  xer_ov_test("ADDMEO.", 0x7C6305D5);
  xer_ov_test("ADDZEO", 0x7C630594);
  xer_ov_test("ADDZEO.", 0x7C630595);
  xer_ov_test("DIVWO", 0x7C6327D6);
  xer_ov_test("DIVWO.", 0x7C6327D7);
  xer_ov_test("DIVWUO", 0x7C632796);
  xer_ov_test("DIVWUO.", 0x7C632797);
  xer_ov_test("MULLWO", 0x7C6325D6);
  xer_ov_test("MULLWO.", 0x7C6325D7);
  xer_ov_test("NEGO", 0x7C6304D0);
  xer_ov_test("NEGO.", 0x7C6304D1);
  xer_ov_test("SUBFO", 0x7C632450);
  xer_ov_test("SUBFO.", 0x7C632451);
  xer_ov_test("SUBFCO", 0x7C632410);
  xer_ov_test("SUBFCO.", 0x7C632411);
  xer_ov_test("SUBFEO", 0x7C632510);
  xer_ov_test("SUBFEO.", 0x7C632511);
  xer_ov_test("SUBFMEO", 0x7C6305D0);
  xer_ov_test("SUBFMEO.", 0x7C6305D1);
  xer_ov_test("SUBFZEO", 0x7C630590);
  xer_ov_test("SUBFZEO.", 0x7C630591);
#endif
}

#endif
