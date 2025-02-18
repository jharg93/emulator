
/* Json reader */
struct json_node {
  const char *str;
  int type;

  json_node *parent;
  json_node *child;
  json_node *next;

  const char *operator[](const char *str) {
  };
};

void parse_json(const char *file) {
  int level = 0;
  int lname = 0;
  
  FILE *fp = fopen(file, "r");
  if (fp == NULL) {
    printf("no fopen: %s\n", file);
    exit(0);
  }

  fseek(fp, 0, SEEK_END);
  size_t len = ftell(fp);
  fseek(fp, 0, SEEK_SET);
  
  char *buf = (char *)malloc(len);
  fread(buf, len, 1, fp);

  for (int pos = 0; pos < len; pos++) {
    char ch = buf[pos];
    
    if (isspace(ch)) {
      continue;
    }
    printf("base: '%c'\n", ch);
    if (ch == '{') {
      level++;
    }
    else if (ch == '}') {
      level--;
    }
    else if (ch == ':') {
    }
    else if (ch == '\"') {
    }
  }
}
