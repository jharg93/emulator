#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <assert.h>
#include <vector>
#include <map>
#include <iostream>

#include "genjson.h"
#include "json_parser.h"

int pos, size, level, lastch = -1;
char *buffer;

int nextch(bool ws, const char *expect = NULL) {
  char ch;

  do {
    if (lastch != -1) {
      ch = lastch;
      lastch = -1;
    }
    else if (pos >= size) {
      return -1;
    }
    else {
      ch = buffer[pos++];
    }
    if (!ws)
      break;
  } while (isspace(ch));
  if (expect != NULL) {
    assert(strchr(expect, ch) != NULL);
  }
  return ch;
}

void parseDict(json_node *n) {
  auto ch = nextch(true);
  n->type = 'd';
  if (ch == '}' || ch < 0) {
    // empty dict
    return;
  }
  lastch = ch;
  level++;
  do {
    json_node k={};
    json_node *v = new json_node;
    
    parse_json(&k);
    printf("key: %s\n", k.string.c_str());
    ch = nextch(true, ":");
    if (ch < 0) {
      return;
    }
    parse_json(v);
    n->map[k.string] = v;
    
    ch = nextch(true);
    if (ch == '}') {
      break;
    }
    printf("dict nextch: %c\n", ch);
    assert(ch == ',');
  } while (true);
  level--;
}

void parseList(json_node *n) {
  auto ch = nextch(true);
  n->type = 'l';
  if (ch == ']' || ch < 0) {
    // empty list
    return;
  }
  lastch = ch;
  level++;
  do {
    auto j = new json_node;
    n->list.push_back(j);
    parse_json(j);
    ch = nextch(true);
    if (ch == ']') {
      // end-of-list
      break;
    }
    assert(ch == ',');
  } while (true);
  level--;
}

int parse_json(json_node *n) {
  char ch;
  
  ch = nextch(true);
  if (ch < 0) {
    return 0;
  }
  if (ch == '[') {
    parseList(n);
  }
  else if (ch == '{') {
    parseDict(n);
  }
  else if (ch == '\"') {
    n->type = 's';
    ch = nextch(false);
    while (ch >= 0 && ch != '\"') {
      n->string += ch;
      ch = nextch(false);
    };
  }
  else if (isalnum(ch)) {
    n->type = 's';
    do {
      n->string += ch;
      ch = nextch(false);
    } while (isalnum(ch));
    lastch = ch;
  }
  else {
    printf("unknown... '%c' %x\n", ch, ch);
    exit(1);
  }
  return 2;
}

void print_json(json_node *node, int lvl) {
  if (!node){
    return;
  }
  auto print_indent = [&](int lvl) {
    for (int i = 0; i < lvl; i++) {
      std::cout << "  ";
    }
  };
  if (node->type == 's') {
    std::cout << "\"" << node->string << "\"";
  }
  else if (node->type == 'l') {
    std::cout << "[\n";
    for (auto i=0; i < node->list.size(); i++) {
      print_indent(lvl+1);
      print_json(node->list[i], lvl+1);
      if (i < node->list.size() - 1) std::cout << ",";
      std::cout << "\n";
    }
    print_indent(lvl);
    std::cout << "]";
  }
  else if (node->type == 'd') {
    std::cout << "{\n";
    for (auto it = node->map.begin(); it != node->map.end(); ++it) {
      print_indent(lvl+1);
      std::cout << "\"" << it->first << "\": ";
      print_json(it->second, lvl+1);
      if (std::next(it) != node->map.end()) std::cout << ",";
      std::cout << "\n";
    }
    print_indent(lvl);
    std::cout << "}";
  }
}

#ifdef PJSON
/* Get registers from dict */
int getreg(json_node *n, rr_t *r, int flag) {
  uint32_t rv;
  int error = 0;

  if (!n) {
    assert(0);
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
      printf(" assertion failure: %s %x!=%x\n",
	     r->name, *r->val, rv);
      error++;
    }
    r++;
  }
  return error;
}
int getmem(json_node *n, uint8_t *mem, int flag) {
  int error = 0;
  
  for (auto v : n->list) {
    // should be a list with two entries in it
    assert(v->list.size() == 2);
    uint32_t memaddr = v->list[0]->toint();
    uint32_t rv = v->list[1]->toint();

    if (flag == 0) {
      mem[memaddr] = rv;
      printf("%.8x <- %.2x\n", memaddr, rv);
    }
    else if (flag == 1 && (mem[memaddr] != rv)) {
      printf(" assertion failure: %.8x %x!=%x\n",
	     memaddr, mem[memaddr], rv);
      error++;
    }
  }
  return error;
}

void read_json(const char *file, rr_t *regread, uint8_t *mem, void (*run)(uint32_t *)) {
  int fd;
  uint32_t prefetch[2];
  
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
  for (auto l : r.list) {
    int errors;
    
    auto name = l->map["name"];
    if (!name)
      continue;
    printf("test: %s\n", name->string.c_str());

    /* Get initial state */
    auto ini = l->map["initial"];
    if (!ini)
      continue;
    if (ini->map.contains("regs")) {
      getreg(ini->map["regs"], regread, 0);
    }
    else {
      getreg(ini, regread, 0);
    }
    getmem(ini->map["ram"], mem, 0);
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
    if (fin->map.contains("regs")) {
      errors += getreg(fin->map["regs"], regread, 0);
    }
    else {
      errors += getreg(fin, regread, 1);
    }
    errors += getmem(ini->map["ram"], mem, 1);
    if (!errors) {
      printf("PASS\n");
    }
    else {
      printf("FAIL\n");
    }
  }
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

