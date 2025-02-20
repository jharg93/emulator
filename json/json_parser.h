#ifndef __json_parser_h__
#define __json_parser_h__
#include <assert.h>

#include <map>
#include <iostream>
#include <vector>

struct json_node {
  int type;
  std::string string;
  std::vector<json_node *> list;
  std::map<std::string, json_node *> map;
  uint32_t toint() {
    return strtoull(string.c_str(), 0, 0);
  };
  const char *tostr() {
    return string.c_str();
  };
};

int parse_json(json_node *n);
void print_json(json_node *n, int);

struct JsonParser {
  int pos = 0, size = 0, lastch = -1;
  char *buffer = NULL;

  int load(const char *file) {
    int fd;
    
    if ((fd = open(file, O_RDONLY)) < 0) {
      exit(0);
    }
    size = lseek(fd, 0, SEEK_END);
    buffer = new char[size];
    pread(fd, buffer, size, 0);
    close(fd);
  };
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
  };

  void parseDict(json_node *n) {
    auto ch = nextch(true);
    n->type = 'd';
    if (ch == '}' || ch < 0) {
      // empty dict
      return;
    }
    lastch = ch;
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
      
      ch = nextch(true);
      if (ch == '}') {
	break;
      }
      assert(ch == ',');
    } while (true);
  };

  void parseList(json_node *n) {
    auto ch = nextch(true);
    n->type = 'l';
    if (ch == ']' || ch < 0) {
      // empty list
      return;
    }
    lastch = ch;
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
  };
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
  };
  void Parse(json_node *r) {
    while (pos < size)
      parse_json(r);
  };
};

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

#endif
