#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>

#include "genjson.h"
#include "json_parser.h"

#ifdef PJSON
/* Get registers from dict */
static int getreg(json_node *n, rr_t *r, int flag) {
  uint32_t rv;
  int error = 0;

  if (!n) {
    assert(0);
  }
  if (n->map.contains("regs")) {
    n = n->map["regs"];
  }
  while (r->name) {
    auto v = n->map[r->name];

    rv = v->toint();
    if (flag == 0) {
      // store value to pointer
      *r->val = rv;
      printf("%s <- %.8x\n", r->name, rv);
    }
    else if (flag == 1 && (rv != *r->val)) {
      // check if value is correct
      printf("%s <- %.8x\n", r->name, rv);
      printf("  assertion failure: %s got:%.8x expect:%.8x\n",
	     r->name, *r->val, rv);
      error++;
    }
    r++;
  }
  return error;
}

static int getmem(json_node *n, uint8_t *mem, int flag) {
  int error = 0;

  if (!n) {
    return 0;
  }
  n = n->map["ram"];
  for (auto v : n->list) {
    // should be a list with two entries in it
    assert(v->list.size() == 2);
    uint32_t memaddr = v->list[0]->toint();
    uint32_t rv = v->list[1]->toint();

    printf("  %.8x <- %.2x\n", memaddr, rv);
    if (flag == 0) {
      mem[memaddr] = rv;
    }
    else if (flag == 1 && (mem[memaddr] != rv)) {
      printf("  assertion failure: %.8x got:%.2x expect:%.2x\n",
	     memaddr, mem[memaddr], rv);
      error++;
    }
  }
  return error;
}

extern void done_json();

// Run single-step processor tests
// https://github.com/SingleStepTests/ProcessorTests
//   mem points to memory buffer
//   run executes code with possible prefetch
//   rr_t contains list of register names and a pointer (uint32_t)
template <Arch a>
void read_json(const char *file, rr_t *regread, uint8_t *mem, void (*run)(uint32_t *)) {
  uint32_t prefetch[2];
  JsonParser p;
  json_node r;
  int rc = 0;

  p.load(file);
  p.Parse(&r);
  for (auto json : r.list) {
    int errors;
    
    auto name = json->map["name"];
    if (!name)
      continue;
    printf("===========================\n");
    printf("test: %s\n", name->tostr());

    /* Get initial state */
    auto ini = json->map["initial"];
    auto fin = json->map["final"];
    if (!ini || !fin)
      continue;
    getreg(ini, regread, 0);
    getmem(ini, mem, 0);
    if (ini->map.contains("prefetch")) {
      auto pf = ini->map["prefetch"];
      prefetch[0] = pf->list[0]->toint();
      prefetch[1] = pf->list[1]->toint();
      printf("prefetch: %.8x %.8x\n", prefetch[0], prefetch[1]);
    }

    /* Run cpu single-step */
    PC -= 4;
    run(prefetch);
    
    /* Compare final state */
    errors = 0;
    errors += getreg(fin, regread, 1);
    errors += getmem(fin, mem, 1);
    if (!errors) {
      printf("state: PASS\n");
    }
    else {
      printf("state: FAIL\n");
      rc = 1;
    }
  }
  done_json();
  printf("exit: %d\n", rc);
  exit(rc);
}
#else
void read_json(const char *file) {
  JsonParser p;
  json_node r;
  
  p.load(file);
  p.Parse(&r);
  print_json(&r, 0);
}

int main(int argc, char *argv[]) {
  if (argc > 1) {
    read_json(argv[1]);
  }
}
#endif

