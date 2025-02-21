#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>

#include "genjson.h"
#include "json_parser.h"

#ifdef PJSON
/* Get registers from dict */
int getreg(json_node *n, rr_t *r, int flag) {
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
    }
    else if (flag == 1 && (rv != *r->val)) {
      // check if value is correct
      printf("  assertion failure: %s %.2x!=%.2x\n",
	     r->name, *r->val, rv);
      error++;
    }
    r++;
  }
  return error;
}

int getmem(json_node *n, uint8_t *mem, int flag) {
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

    //printf("  %.8x <- %.2x\n", memaddr, rv);
    if (flag == 0) {
      mem[memaddr] = rv;
    }
    else if (flag == 1 && (mem[memaddr] != rv)) {
      printf("  assertion failure: %.8x %x!=%x\n",
	     memaddr, mem[memaddr], rv);
      error++;
    }
  }
  return error;
}

// Run single-step processor tests
// https://github.com/SingleStepTests/ProcessorTests
//   mem points to memory buffer
//   run executes code with possible prefetch
//   rr_t contains list of register names and a pointer (uint32_t)
void read_json(const char *file, rr_t *regread, uint8_t *mem, void (*run)(uint32_t *)) {
  int fd, rc = 0;
  uint32_t prefetch[2];
  JsonParser p;
  json_node r;

  p.load(file);
  p.Parse(&r);
  for (auto l : r.list) {
    int errors;
    
    auto name = l->map["name"];
    if (!name)
      continue;
    printf("===========================\n");
    printf("test: %s\n", name->tostr());

    /* Get initial state */
    auto ini = l->map["initial"];
    if (!ini)
      continue;
    getreg(ini, regread, 0);
    getmem(ini, mem, 0);
    if (ini->map.contains("prefetch")) {
      auto pf = ini->map["prefetch"];
      prefetch[0] = pf->list[0]->toint();
      prefetch[1] = pf->list[1]->toint();
    }

    /* Run cpu */
    run(prefetch);
    
    /* Compare final state */
    auto fin = l->map["final"];
    if (!fin)
      continue;
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

