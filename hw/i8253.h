#ifndef __i8253_h__
#define __i8253_h__

/*========================================================*
 * i8253 PIT: Programmable Interval Timer
 *  Used for timing and controling the PC Speaker
 *========================================================*/
struct pit_t : public Timer {
  bool     flipflop;
  uint8_t  rl;
  uint8_t  ooo;
  bool     bcd;
  uint16_t freq;

  void write(int addr, uint8_t val);
};

/*============================*
 * i8253 PIT
 *============================*/
struct i8253_t {
  pit_t pit[3];
  
  i8253_t(bus_t *io) {
    io->register_handler(0x0040, 0x0047, 0x00FF, i8253_t::io, this, _RW, "i8253 TIMER");
    pit[0].freq = 0xffff;
    pit[1].freq = 0xffff;
    pit[2].freq = 0xffff;
    pit[0].settimer(pit[0].freq, 1, 1, "PIT0");
    pit[1].settimer(pit[1].freq, 0, 0, "PIT1");
    pit[2].settimer(pit[2].freq, 0, 0, "PIT2");
  };
  static int io(void *arg, const uint32_t addr, int mode, iodata_t& val);

};

#endif
