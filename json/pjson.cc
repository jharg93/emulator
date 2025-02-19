#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <assert.h>
#include <vector>
#include <map>

struct json_node {
  int type;
  std::string string;
  std::vector<json_node *> list;
  std::map<std::string, json_node *> map;
};

int pos, size, level, lastch = -1;
char *buffer;

char nextch(bool ws) {
  char ch;

  do {
    if (lastch != -1) {
      ch = lastch;
      lastch = -1;
    }
    else {
      ch = buffer[pos++];
    }
    if (!ws)
      break;
  } while (ch == ' ' || ch == 0xa || ch == 0xd || ch == 0x9);
  return ch;
}

void print_node(json_node *n, int depth = -1) {
  if (!n) {
    return;
  }
  if (depth < 0) {
    printf("\n\n: print node\n");
  }
  if (n->type == 'l') {
    for (int i = 0; i < depth; i++) {
      printf(" ");
    }
    printf("[");
    for (auto v : n->list) {
      print_node(v, 0);
      printf(", ");
    }
    for (int i = 0; i < depth; i++) {
      printf(" ");
    }
    printf("]\n");
  }
  else if (n->type == 'd') {
    for (int i = 0; i < depth; i++) {
      printf(" ");
    }
    printf("{");
    for (auto v : n->map) {
      printf("%s: ", v.first.c_str());
      print_node(v.second, depth+1);
      printf(",\n");
    }
    for (int i = 0; i < depth; i++) {
      printf(" ");
    }
    printf("}\n");
  }
  else if (n->type == 's') {
    for (int i = 0; i < depth; i++) {
      printf(" ");
    }
    printf("%s", n->string.c_str());
  }
}

int parse_json(json_node *n, const char *lbl="") {
  json_node root;
  char name[128] = { };
  char ch;
  
  ch = nextch(true);
  if (n == NULL) {
    n = &root;
  }
  if (ch == '[') {
    ch = nextch(true);
    n->type = 'l';
    if (ch == ']') {
      return 3;
    }
    lastch = ch;
    level++;
    do {
      auto j = new json_node;
      n->list.push_back(j);
      auto li = parse_json(j, "list");
      ch = nextch(true);
      if (ch == ']') {
	// end-of-list
	break;
      }
      assert(ch == ',');
    } while (true);
    level--;
  }
  else if (ch == '{') {
    ch = nextch(true);
    n->type = 'd';
    if (ch == '}') {
      return 1;
    }
    lastch = ch;
    level++;
    do {
      json_node k={};
      json_node *v = new json_node;
      
      parse_json(&k, "key");
      
      ch = nextch(true);
      assert(ch == ':');

      parse_json(v, "value");
      n->map[k.string] = v;

      ch = nextch(true);
      if (ch == '}') {
	break;
      }
      assert(ch == ',');
    } while (true);
    level--;
  }
  else if (ch == '\"') {
    int npos = 0;
    do {
      name[npos++] = ch;
      ch = nextch(false);
    } while (ch != '\"');
    name[npos++] = ch;
    name[npos] = 0;
    n->type = 's';
    n->string = name;
  }
  else if (isdigit(ch)) {
    int npos = 0;
    do {
      name[npos++] = ch;
      ch = nextch(false);
    } while(isdigit(ch));
    lastch = ch;
    name[npos++] = 0;
    n->type = 's';
    n->string = name;
  }
  else {
    printf("unknown... %c\n", ch);
    exit(0);
  }
  return 2;
}

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
  for (auto l : r.list) {
    printf(" %c\n", l->type);
    if (l->type == 'd') {
      for (auto d : l->map) {
	printf("  %s\n", d.first.c_str());
      }
    }
  }
}

int main(int argc, char *argv[]) {
  read_json("../88json/00.json");
}
