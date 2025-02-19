int myio(void *arg, uint32_t addr, int mode, iodata_t& val) {
  memio(arg, addr, mode, val);
  printf("myio: %.8x %c %x : %.2x%s\n", addr, mode, val, cpu_getflags(), cpu_flagstr());
  return 0;
}

void _assert(int got, int expect, const char *lbl)
{
  if (got != expect) {
    printf("%s assertion fails: %x expected:%x\n", lbl, got, expect);
  }
}

void read_json(const char *f) {
  FILE *fp;
  char line[2048], *p1, *fin;
  int pc, s, p, a, x, y, dbr, d, pbr, e, rc;

  uint8_t *mem = new uint8_t[0x1000000]{0};

  trace=3;
  mkop();
  mmu.register_handler(0x000000, 0xFFFFFF, 0xFFFFFF, myio, mem, _RW, "ram");
  if ((fp = fopen(f, "r")) == NULL)
    return;
  
  while (fgets(line, sizeof(line), fp) != NULL) {
    printf("-----------------------------------\n%s", line);
    fin = strstr(line, "\"final\"");
    if (fin == NULL)
      continue;
    *fin++ = 0;
    p1 = strstr(line, "\"pc\"");
    if (p1 == NULL)
      continue;
    printf("%s", p1);
    rc = sscanf(p1, "\"pc\": %d, \"s\": %d, \"p\": %d, \"a\": %d, \"x\": %d, \"y\": %d, \"dbr\": %d, \"d\": %d, \"pbr\": %d, \"e\": %d",
		&pc, &s, &p, &a, &x, &y, &dbr, &d, &pbr, &e);
    assert(rc == 10);
    printf("%x,%x,%x,%x,%x,%x,%x,%x,%x,%x\n", pc, s, p, a, x, y, dbr, d, pbr, e);

    /* Setup registers  */
    cpu_setflags(p);
    PC = pc;
    _S = s;
    _A = a;
    _X = x;
    _Y = y;
    _D = d;
    fE = e;
    DBR = dbr << 16;
    PBR = pbr << 16;

    p1 = strstr(p1, "\"ram\":");
    if (p1 == NULL)
      continue;
    p1 += 8;
    for(;;) {
      printf("---- %s\n", p1);
      if (sscanf(p1, "[%d, %d]", &pc, &p) == 2) {
	printf(" haz: %x %x\n", pc, p);
	mem[pc] = p;
      }
      p1 = strstr(p1, "], ");
      if (p1 == NULL || p1[3] != '[')
	break;
      p1 += 3;
    }
    cpu_step();
    SPC = PC;
    cpu_showregs();
    printf("\n");

    p1 = strstr(fin, "\"pc\"");
    if (p1 == NULL)
      continue;
    rc = sscanf(p1, "\"pc\": %d, \"s\": %d, \"p\": %d, \"a\": %d, \"x\": %d, \"y\": %d, \"dbr\": %d, \"d\": %d, \"pbr\": %d, \"e\": %d",
		&pc, &s, &p, &a, &x, &y, &dbr, &d, &pbr, &e);
    assert(rc == 10);

    printf("%x,%x,%x,%x,%x,%x,dbr:%x,d:%x,pbr:%x,e:%x\n", pc, s, p, a, x, y, dbr, d, pbr, e);
    _assert(p,   cpu_getflags(), "flags");
    _assert(PC , pc, "pc");
    _assert(_S , s, "s");
    _assert(_A , a, "a");
    _assert(_X , x, "x");
    _assert(_Y , y, "y");
    _assert(_D , d, "d");
    _assert(fE , e, "e");
    _assert(DBR , dbr << 16, "dbr");
    _assert(PBR , pbr << 16, "pbr");
  }
  exit(0);
};
