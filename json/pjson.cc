#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <assert.h>
#include <vector>
#include <map>

#include "genjson.h"

int pos, size, level, lastch = -1;
char *buffer;

struct json_node {
  int type;
  std::string string;
  std::vector<json_node *> list;
  std::map<std::string, json_node *> map;
};

int parse_json(json_node *n);

// scan for next character... if ws is true, skip whitespace
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
    ch = nextch(true, ":");
    if (ch < 0) {
      return;
    }
    parse_json(v);
    n->map[k.string] = v;

    ch = nextch(true, "},");
  } while (ch >= 0 && ch != '}');
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
    ch = nextch(true, "],");
  } while (ch >= 0 && ch != ']');
  level--;
}

int parse_json(json_node *n) {
  json_node root;
  char ch;
  
  ch = nextch(true);
  if (ch < 0) {
    return 0;
  }
  if (n == NULL) {
    n = &root;
  }
  if (ch == '[') {
    parseList(n);
  }
  else if (ch == '{') {
    parseDict(n);
  }
  else if (ch == '\"') {
    n->type = 's';
    do {
      n->string += ch;
      ch = nextch(false);
    } while (ch != '\"');
  }
  else if (isdigit(ch)) {
    n->type = 's';
    do {
      n->string += ch;
      ch = nextch(false);
    } while (isdigit(ch));
    lastch = ch;
  }
  else {
    printf("unknown... '%c' %x\n", ch, ch);
    exit(0);
  }
  return 2;
}

void print_json(json_node *n) {
  if (n->type == 's') {
    printf("string: '%s'\n", n->string.c_str());
  }
  if (n->type == 'l') {
    printf("list: %d entries\n", n->list.size());
  }
  if (n->type == 'd') {
    printf("dict: {");
    for (auto v : n->map) {
      printf(" %s, ", v.first.c_str());
    }
    printf("}\n");
  }
}

#ifdef PJSON
/* Get registers from dict */
int getreg(json_node *n, rr_t *r, int flag) {
  uint32_t rv;
  int error = 0;
  
  while (r->name) {
    auto v = n->map[r->name];

    rv = strtoull(v->string.c_str(), 0, 0);
    if (flag == 0) {
      // store value to pointer
      *r->val = rv;
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
void read_json(const char *file, rr_t *regread, void (*run)()) {
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
  for (auto l : r.list) {
    auto name = l->map["name"];
    if (!name)
      continue;
    printf("test: %s\n", name->string.c_str());
    
    auto ini = l->map["initial"];
    if (!ini)
      continue;
    getreg(ini->map["regs"], regread, 0);

    auto fin = l->map["final"];
    if (!fin)
      continue;
    getreg(fin->map["regs"], regread, 1);
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
  printf("root type: %c\n", r.type);
  for (auto l : r.map) {
    printf(" %s:\n", l.first.c_str());
  }
  for (auto l : r.list) {
    printf(" %c\n", l->type);
    if (l->type == 'd') {
      for (auto v : l->map) {
	printf("   %s\n", v.first.c_str());
      }
    }
  }
}

int main(int argc, char *argv[]) {
  if (argc > 1) {
    read_json(argv[1]);
  }
}
#endif
