#ifndef __i8255_h__
#define __i8255_h__

/*============================*
 * i8255 PPI
 *============================*/
struct i8255_t {
  i8255_t(bus_t *io) {
    io->register_handler(0x0060, 0x0065, 0x00FF, i8255_t::io, this, _RW, "i8255");
  };
  static int io(void *arg, const uint32_t addr, int mode, iodata_t& val);
};

#endif
