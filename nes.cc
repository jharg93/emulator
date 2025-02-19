#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>
#include <fcntl.h>
#include <string.h>
#include <string>
#include <unistd.h>
#include <time.h>
#include <stdarg.h>
#include "gr.h"
#include "cart.h"
#include "dstk.h"

int rdy = 1;

Screen *s;
static cart *cart;

extern int clks, scanline, frame;
uint32_t clockticks6502;

uint8_t cpu_read8(uint32_t addr, int mode)
{
  uint8_t v = cart->read(addr);
  printf("cart read: %.4x = %x\n", addr, v);
  return v;
}

void cpu_write8(uint32_t addr, uint8_t v, int mode)
{
  cart->write(addr, v);
}

int ncycles;

extern int cpu_step();

extern int trace;

int cpu_tick(int n)
{
  trace=3;
  if (ncycles > 0) {
    ncycles--;
  }
  if (ncycles == 0) {
    ncycles = cpu_step();
  }
}

int main(int argc, char *argv[])
{
  setbuf(stdout, NULL);

  cart = new nescart(argv[1]);
  cart->run();

  return 0;
}
