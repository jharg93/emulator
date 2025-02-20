#ifndef __genjson_h__
#define __genjson_h__

struct rr_t {
  const char *name;
  uint32_t *val;
};

void read_json(const char *file, rr_t *regread, uint8_t *mem, void (*run)(uint32_t *));

#endif
