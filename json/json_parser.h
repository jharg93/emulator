#ifndef __json_parser_h__
#define __json_parser_h__

struct json_node {
  int type;
  std::string string;
  std::vector<json_node *> list;
  std::map<std::string, json_node *> map;

  uint32_t toint() {
    return strtoull(string.c_str(), 0, 0);
  };
};

int parse_json(json_node *n);
void print_json(json_node *n);

#endif
