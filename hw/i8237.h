#ifndef __i8237_h__
#define __i8237_h__

/*========================================================*
 * i8237 DMA Controller
 *========================================================*/
struct dma {
  uint32_t dst;
  uint32_t src;
  uint32_t count;
} dmachan[8];

struct dma_t {
};

/*=============================*
 * i8237 DMA controller
 *  ports:0000-000F
 *  ports:0080-008F
 *=============================*/
struct i8237_t : public dma_t {
  int flipflop = 0;
  i8237_t(bus_t *io) {
    io->register_handler(0x0000, 0x000F, 0x00FF, i8237_t::io, this, _RW, "i8237 DMA");
    io->register_handler(0x0080, 0x008F, 0x00FF, i8237_t::io, this, _RW, "i8237 DMA");
  };
  static int io(void *arg, const uint32_t addr, int mode, iodata_t& val);
};

#endif
