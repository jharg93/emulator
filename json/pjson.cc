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
      printf("  assertion failure: %s %x!=%x\n",
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

    if (flag == 0) {
      mem[memaddr] = rv;
      printf("  %.8x <- %.2x\n", memaddr, rv);
    }
    else if (flag == 1 && (mem[memaddr] != rv)) {
      printf("  assertion failure: %.8x %x!=%x\n",
	     memaddr, mem[memaddr], rv);
      error++;
    }
  }
  return error;
}

void read_json(const char *file, rr_t *regread, uint8_t *mem, void (*run)(uint32_t *)) {
  int fd, rc = 0;
  uint32_t prefetch[2];
  
  if ((fd = open(file, O_RDONLY)) < 0) {
    exit(0);
  }
  size = lseek(fd, 0, SEEK_END);
  buffer = new char[size];
  pread(fd, buffer, size, 0);
  close(fd);

  // parse json */
  json_node r;
  while (pos < size) {
    parse_json(&r);
  }
  for (auto l : r.list) {
    int errors;
    
    auto name = l->map["name"];
    if (!name)
      continue;
    printf("test: %s\n", name->tostr());

    /* Get initial state */
    auto ini = l->map["initial"];
    if (!ini)
      continue;
    getreg(ini, regread, 0);
    getmem(ini, mem, 0);
    if (ini->map.contains("prefetch")) {
      auto pf = ini->map["prefetch"];
      uint32_t pf0 = pf->list[0]->toint();
      uint32_t pf1 = pf->list[1]->toint();
      prefetch[0] = pf0;
      prefetch[1] = pf1;
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
  int fd;
  
  if ((fd = open(file, O_RDONLY)) < 0)
    return;
  size = lseek(fd, 0, SEEK_END);
  buffer = new char[size];
  pread(fd, buffer, size, 0);
  close(fd);

  json_node r;
  while (pos < size) {
    parse_json(&r);
  }
  print_json(&r, 0);
}

int main(int argc, char *argv[]) {
  if (argc > 1) {
    read_json(argv[1]);
  }
}
#endif

