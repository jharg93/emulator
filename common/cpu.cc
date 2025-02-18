#include <inttypes.h>
#include <assert.h>
#include <stdio.h>
#include "cpu.h"

#define WEAK __attribute__((weak))

uint8_t WEAK cpu_read8(const uint32_t off, int type) {
  printf("assert8\n");
  fflush(stdout);
  assert(0);
  return 0;
}

void WEAK cpu_write8(const uint32_t off, uint8_t v, int type) {
  assert(0);
}

uint16_t WEAK cpu_read16(const uint32_t off, int type) {
  uint16_t lo = cpu_read8(off+0, type);
  uint16_t hi = cpu_read8(off+1, type);
  return (hi << 8) + lo;
}

uint16_t WEAK cpu_read16be(const uint32_t off, int type) {
  uint16_t hi = cpu_read8(off+0, type);
  uint16_t lo = cpu_read8(off+1, type);
  return (hi << 8) + lo;
}

void WEAK cpu_write16(const uint32_t off, uint16_t v, int type) {
  cpu_write8(off+0, v, type);
  cpu_write8(off+1, v>>8, type);
}

void WEAK cpu_write16be(const uint32_t off, uint16_t v, int type) {
  cpu_write8(off+1, v, type);
  cpu_write8(off+0, v>>8, type);
}

uint32_t WEAK cpu_read32(const uint32_t off, int type) {
  uint32_t lo = cpu_read16(off+0);
  uint32_t hi = cpu_read16(off+2);
  return (hi << 16) | lo;
}

uint64_t WEAK cpu_read64(const uint32_t off, int type) {
  uint64_t lo = cpu_read32(off+0);
  uint64_t hi = cpu_read32(off+4);
  return (hi << 32) | lo;
}

uint32_t WEAK cpu_read32be(const uint32_t off, int type) {
  uint32_t hi = cpu_read16be(off+0);
  uint32_t lo = cpu_read16be(off+2);
  return (hi << 16) | lo;
}

void WEAK cpu_write32(const uint32_t off, const uint32_t v, int type) {
  cpu_write16(off+0, v);
  cpu_write16(off+2, v>>16);
}

void WEAK cpu_write64(const uint32_t off, const uint64_t v, int type) {
  cpu_write32(off+0, v);
  cpu_write32(off+2, v>>32);
}

void WEAK cpu_write32be(const uint32_t off, const uint32_t v, int type) {
  cpu_write16(off+2, v);
  cpu_write16(off+0, v>>16);
}

/*=======================================*
 * Stack handlers
 *=======================================*/
void WEAK cpu_push8(uint8_t v) {
  assert(0);
}

uint8_t WEAK cpu_pop8() {
  assert(0);
}

void WEAK cpu_push16(const uint16_t v) {
  cpu_push8(v >> 8);
  cpu_push8(v);
}

uint16_t WEAK cpu_pop16() {
  uint16_t lo = cpu_pop8();
  uint16_t hi = cpu_pop8();
  return (hi << 8) + lo;
}

void WEAK cpu_push32(const uint32_t v) {
  cpu_push16(v >> 16);
  cpu_push16(v);
}

uint32_t WEAK cpu_pop32() {
  uint32_t lo = cpu_pop16();
  uint32_t hi = cpu_pop16();
  return (hi << 8) + lo;
}

/*====================================*
 * CPU Handlers
 *====================================*/
void WEAK cpu_shutdown(void) {
}

void WEAK dumpmem(void) {
}

bool WEAK cpu_irq(int n)
{
  return false;
}

/* Check a list of irq bits to see if we generate an IRQ */
bool check_irq(uint32_t sts, uint32_t en, int n, irqevent *map) {
  bool rc = false;
  
  for (int i = 0; i < n; i++) {
    if ((en & sts & map->mask) != 0) {
      printf("Generate IRQ: %s\n", map->name);
      rc = cpu_irq(map->level);
      if (rc) {
	break;
      }
    }
    map++;
  }
  return rc;
}

int run_dma(uint32_t dst, int dstIncr, uint32_t src, int srcIncr, int len, int size)
{
  while (len--) {
    switch (size) {
    case 1: cpu_write8(dst, cpu_read8(src)); break;
    case 2: cpu_write16(dst, cpu_read16(src)); break;
    case 4: cpu_write32(dst, cpu_read32(src)); break;
    }
    dst += dstIncr;
    src += srcIncr;
  }
  return 0;
}
