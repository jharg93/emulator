/* Code for running JSON tests */
int nerr;

void _assert(int got, int expect, const char *lbl)
{
  if (got != expect) {
    printf("%s assertion fails: %x expected:%x\n", lbl, got, expect);
    nerr++;
  }
}

/* Handle parsing regs by name and value */
struct rr_t {
  const char *name;
  uint32_t *val;
};

/* Get register from input. If test, check against current value, else store
 * "d0": xxxx, "d1": yyyy, ...
 */
// flag = 0x00 | read
// flag = 0x01 | check
// flag = 0x02 | read/print
void getreg(const char *str, rr_t *r, int flag) {
  const char *p;
  uint32_t v = 0;

  if ((p = strstr(str, r->name)) != NULL) {
    if ((p = strchr(p, ':')) == NULL) {
      printf("Ack no :\n");
      return;
    }
    if (sscanf(p, ": %d", &v) != 1) {
      printf("noread... %s\n", p);
      return;
    }
    if (flag & 0x1) {
      _assert(*r->val, v, r->name);
    }
    else if (flag & 0x2) {
      printf("%s = %.8x ", r->name, v);
    } else {
      *r->val = v;
    }
  } else {
    printf("Not found: %s\n", r->name);
  }
}

/* Get RAM values from input:
 *  "ram": [[addr, val], [addr, val], ... ]
 */
void getram(const char *str, uint8_t *mem, bool test = false)
{
  const char *pr;
  uint32_t pc, p;
  char addr[64];
  
  pr = strstr(str, "\"ram\":");
  if (pr == NULL)
    return;
  pr += 8;
  for(;;) {
    if (sscanf(pr, "[%d, %d]", &pc, &p) == 2) {
      printf(" ram: %.4x = %.2x\n", pc, p);
      if (test) {
	snprintf(addr, sizeof(addr), "mem [%d:%x]", pc, pc);
	_assert(mem[pc], p, addr);
      }
      else {
	mem[pc] = p;
      }
    }
    pr = strstr(pr, "], ");
    if (pr == NULL || pr[3] != '[')
      break;
    pr += 3;
  }
}

/* Prefetch instructions
 * "prefetch": [xxxx, yyyy]
 */
void getprefetch(const char *str, uint8_t *mem)
{
  const char *pr;
  uint32_t v1 = 0, v2 = 0;
  
  if ((pr = strstr(str, "prefetch")) == NULL) {
    return;
  }
  sscanf(pr+11, "[%d, %d]", &v1, &v2);
  mem[PC+0] = (v1 >> 8);
  mem[PC+1] = (v1 & 0xFF);
  mem[PC+2] = (v2 >> 8);
  mem[PC+3] = (v2 & 0xFF);
}

/* List registers */
static uint32_t tmpsr;
rr_t regread[] = {
  { "d0", &D[0] },
  { "d1", &D[1] },
  { "d2", &D[2] },
  { "d3", &D[3] },
  { "d4", &D[4] },
  { "d5", &D[5] },
  { "d6", &D[6] },
  { "d7", &D[7] },
  { "a0", &A[0] },
  { "a1", &A[1] },
  { "a2", &A[2] },
  { "a3", &A[3] },
  { "a4", &A[4] },
  { "a5", &A[5] },
  { "a6", &A[6] },
  { "ssp",&ssp },
  { "usp",&usp },
  { "pc", &PC },
  { "sr", &tmpsr },
  { },
};

// xnvzc
const char *srflags(uint32_t sr)
{
  static char sbits[12] = { 0 };
  static int tbit[] = { 0x10, 0x08, 0x04, 0x02, 0x01 };
  
  strcpy(sbits, "xnzvc");
  for (int i = 0; i < 5; i++) {
    if ((sr & tbit[i]) == 0) {
      sbits[i] = '-';
    }
  }
  return sbits;
}

void read_json(const char *f) {
  const int memsize = 0x1000000;
  
  uint8_t *mem = new uint8_t[memsize]{0};
  uint32_t op;
  char line[2048], *fin, *ini;
  FILE *fp;
  int toterr = 0;
  
  if ((fp = fopen(f, "r")) == NULL) {
    printf("can't open: '%s'\n", f);
    exit(1);
  }

  trace=3;
  sys.register_handler(0x000000, 0xFFFFFF, 0xFFFFFF, memio, mem, _RW, "ram");
  
  while (fgets(line, sizeof(line), fp) != NULL) {
    memset(mem, 0, memsize);

    /* get pointer to initial/final state */
    printf("-----------------------------------\n");
    //printf("-----------------------------------\n%s", line);
    if ((ini = strstr(line, "\"initial\"")) == NULL) 
      continue;
    *ini++ = 0;
    if ((fin = strstr(ini, "\"final\"")) == NULL)
      continue;
    *fin++ = 0;

    /* Load initial state */
    for (rr_t *r = regread; r->name; r++) {
      getreg(ini, r, 0x0);
    }
    /* Read initial memory and instruction */
    PC &= 0xFFFFFF;
    getram(ini, mem);
    getprefetch(ini, mem);

    /* Setup status register and stack pointer */
    SR = tmpsr;
    if (SR & 0x2000) {
      SP = ssp;
    }
    else {
      SP = usp;
    }

    /* Execute op */
    nerr = 0;
    SPC = PC;
    op = cpu_fetch(Word);

    printf("decode: %.4x\n", op);
    decode_68k(op);
    cpu_showregs();
    printf("\n");

    /* Compare result */
    tmpsr = SR;
    if (SR & 0x2000) {
      ssp = SP;
    }
    else {
      usp = SP;
    }
    uint32_t isr = tmpsr;
    
    /* Load final state and verify */
    for (rr_t *r = regread; r->name; r++) {
      getreg(fin, r, 0x01);
    }
    getram(fin, mem, 0x01);
    if (nerr) {
      printf("%s\n", line);
      printf("ini: %s ", srflags(isr));
      for (rr_t *r = regread; r->name; r++) {
	getreg(ini, r, 0x02);
      }
      printf("\nfin: %s ", srflags(tmpsr));
      for (rr_t *r = regread; r->name; r++) {
	getreg(fin, r, 0x02);
      }
      printf("\n");
    }
    printf("@ %s\n", nerr ? "error" : "success");
    toterr += nerr;
  }
  printf("done! : %d\n", toterr);
  exit(toterr == 0 ? 0 : 1);
};

