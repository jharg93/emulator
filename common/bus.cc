#include <stdio.h>
#include <inttypes.h>
#include "bus.h"

int (*bus_hook)(uint32_t addr, int mode, iodata_t data);

uint8_t bus_t::bus_data = 0;

/*=======================================================*
 * BUS interface
 *=======================================================*/
bus_t::bus_t(uint32_t mask) {
  busmask = mask;
};

void bus_t::init(uint32_t mask) {
  busmask = mask;
}

handler_t *bus_t::find(uint32_t addr, const int flag) {
  addr &= busmask;
  for (auto h : handlers) {
    if ((h->flag & flag) && (addr >= h->start && addr <= h->end)) {
      return h; 
    }
  }
  return NULL;
}

/* Bus write */
int bus_t::write(uint32_t addr, iodata_t data, int sz) {
  handler_t *h;
  int rc;

  bus_data = data;
  if (bus_hook) {
    bus_hook(addr, 'w', data);
  }
  if ((h = find(addr, _WR)) == NULL) {
    //flogger(0, "Error: no write address: %.8x\n", addr);
    return -1;
  }
  rc = h->fn(h->arg, addr & h->mask, sz | 'w', data);
  if (h->flag & _DBG) {
    flogger(0, "Write%x: %.4x:%.8x[%s] %.8x {%d}\n", sz, addr, addr & h->mask, h->lbl, data, rc);
  }
  return rc;
};

/* Bus read */
int bus_t::read(uint32_t addr, iodata_t& data, int sz) {
  handler_t *h;
  int rc;
  
  data = 0;
  if (bus_hook) {
    bus_hook(addr, 'r', data);
  }
  if ((h = find(addr, _RD)) == NULL) {
    //flogger(0, "Error: no read address:  %.8x\n", addr);
    return -1;
  }
  rc = h->fn(h->arg, addr & h->mask, sz | 'r', data);
  if (h->flag & _DBG) {
    flogger(0, "Read%x : %.4x:%.8x[%s] %.8x {%d}\n", sz, addr, addr & h->mask, h->lbl, data, rc);
  }
  bus_data = data;
  return rc;
};

/* Register a memory region with the bus handler
 *  start/end : Bus address. Will be masked with the initial bus size specified in the constructor
 *  mask      : Mask address for the range (for creating 0-buffer offset)
 *  fn        : callback function
 *  arg       : callback arg
 *  flag      : read/write flag.  Can call separately for RD and WR on same region
 */
int bus_t::register_handler(uint32_t start, uint32_t end, uint32_t mask, iofn_t fn, void *arg, int flag, const char *lbl)
{
  handler_t* h;

  if (!flag)
    flag = _RW;
  flogger(0, "Adding %.8x:%.8x:%.8x flag:%.8x %s\n", start, end, mask, flag, lbl);
  h = new handler_t{0};
  h->start = start;
  h->end   = end;
  h->mask  = mask;
  h->fn    = fn;
  h->arg   = arg;
  h->flag  = flag;
  h->lbl   = lbl;
  handlers.push_front(h);
  return 0;
}

void bus_t::dump(int start, int end)
{
  for(auto h : handlers) {
    flogger(0, "%c%c %.8x-%.8x:%.8x %s\n",
	    h->flag & _RD ? 'r' : '-',
	    h->flag & _WR ? 'w' : '-',
	    h->start, h->end, h->mask,
	    h->lbl);
  }
}

/*==========================================================*
 * Generic memory buffer read/write routine
 *==========================================================*/
int memio(void *arg, uint32_t addr, int mode, iodata_t&data) {
  uint8_t *buf = (uint8_t *)arg;

  if (mode == 'w')
    buf[addr] = data;
  else if (mode == 'r')
    data = buf[addr];
  else
    assert(0);
  return 0;
}

int bememio(void *arg, uint32_t addr, int mode, iodata_t&data) {
  uint8_t *buf = (uint8_t *)arg + addr;
  
  switch(mode) {
  case _WR8:
    put8(buf, data);
    break;
  case _WR16:
    put16be(buf, data);
    break;
  case _WR32:
    put32be(buf, data);
    break;
  case _RD8:
    data = get8(buf);
    break;
  case _RD16:
    data = get16be(buf);
    break;
  case _RD32:
    data = get32be(buf);
    break;
  }
  return 0;
}

/*===========================================================
 * Setbank support
 *===========================================================*/
void setbank(bank_t *b, int nb, const char *lbl)
{
  if (!lbl)
    lbl = b->name;
  if (nb < 0)
    nb += b->nbank;
  if (nb >= b->nbank) {
    fprintf(stdout, "ERROR: %s bank out of range %d/%d\n", lbl, nb, b->nbank);
    nb %= b->nbank;
  }
  if (b->bank != (nb * b->banksz)) {
    flogger(0, "setbank[%s] %d/%d\n", lbl, nb, b->nbank);
    b->bank = nb * b->banksz;
  }
}

/* Initialize a new bank method
 *  bank_t *b      : Bank to initialize
 *  uint8_t *base  : Pointer to memory buffer
 *  int size       : Length of memory buffer
 *  int bsz        : Size of each bank ( <= size)
 *  int nb         : Value to initialize
 */
void initbank(bank_t *b, uint8_t *mem, int memsz, int bsz, int sb, const char *name)
{
  b->base   = mem;
  b->nbank  = memsz / bsz;
  b->banksz = bsz;
  b->name   = name;
  fprintf(stdout, "Initbank[%s]: %3d banks of %.8x = %.8x\n", name, b->nbank, bsz, memsz);
  setbank(b, sb);
}

/* Bank Select interface */
int bankio(void *arg, uint32_t offset, int mode, iodata_t& data)
{
  bank_t *b = (bank_t *)arg;

  /* Addr should already be properly masked out */
  return memio(b->base, b->bank + offset, mode, data);
}

/* Setbank based on address */
int banksel(void *arg, uint32_t offset, int mode, iodata_t& data)
{
  bank_t *b = (bank_t *)arg;
  
  setbank(b, offset - b->sel);
  return 0;
}

/* Setbank based on data */
int bankset(void *arg, uint32_t offset, int mode, iodata_t &data)
{
  bank_t *b = (bank_t *)arg;

  setbank(b, data);
  return 0;
}

/*==================================================*
 * Banksy banks: Have array of pointers to each bank
 * Allows disjoint mapping of banks, reassign pointers
 *==================================================*/
int banksyio(void *arg, uint32_t offset, int mode, iodata_t& data)
{
  banksy *b = (banksy *)arg;

  return memio(b->current, offset, mode, data);
}

/* initialize n bank pointers */
int banksy::init(int n, const char *lbl)
{
  name = lbl;
  nbank = n;
  banks = new uint8_t*[nbank]{0};
  return 0;
}

/* initialize n obank pointers to common memory buffer
 *   buf = rom buffer
 *   len = rom buffer length
 *    sz = chunk size
 *    cb = current bank
 */
int banksy::init(uint8_t *buf, int len, int sz, int cb, const char *lbl)
{
  init(len / sz, lbl);
  for (int i = 0; i < nbank; i++) {
    banks[i] = buf;
    buf += sz;
  }
  return setbank(cb);
}

int banksy::setbank(int nb, const char *lbl)
{
  if (nb < 0)
    nb += nbank;
  if (nb >= nbank) {
    fprintf(stderr, "Error: Bank out of range: %d/%d [%s]\n", nb, nbank, name);
    nb %= nbank;
  }
  if (bank != nb) {
    bank = nb;
    current = banks[nb];
    fprintf(stderr, "@@@@@@@@ Setbank[%s]: %d/%d\n", lbl ? lbl : name, bank, nbank);
  }
  return 0;
}

void setbank(banksy *b, int nb, const char *lbl)
{
  b->setbank(nb, lbl);
}

void initbank(banksy *b, uint8_t *mem, int memsz, int bsz, int sb, const char *name)
{
  b->init(mem, memsz, bsz, sb, name);
}

void putn(uint8_t *dst, uint32_t v, int n)
{
  switch (n) {
  case 1:
    put8(dst, v);
    break;
  case 2:
    put16(dst, v);
    break;
  case 4:
    put16(dst, v >> 16);
    put16(dst + 2, v);
    break;
  }
}

uint32_t getn(const uint8_t *src, int n)
{
  uint32_t lo, hi;
  
  switch (n) {
  case 1:
    return get8(src);
  case 2:
    return get16(src);
  case 4:
    hi = get16(src);
    lo = get16(src + 2);
    return (hi << 16) + lo;
  }
  return 0;
}

