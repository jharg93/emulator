#ifndef __cia8520_h__
#define __cia85200_h__

struct cia85200_t {
  uint8_t  pra;     // 0
  uint8_t  prb;     // 1
  uint8_t  ddra;    // 2
  uint8_t  ddrb;    // 3
  uint8_t  ta_lo;   // 4
  uint8_t  ta_hi;   // 5
  uint8_t  tb_lo;   // 6
  uint8_t  tb_hi;   // 7
  uint8_t  tod_lo;  // 8
  uint8_t  tod_mid; // 9
  uint8_t  tod_hi;  // a
  uint8_t  tod_xx;  // b
  uint8_t  sdr;     // c
  uint8_t  icr;     // d
  uint8_t  cra;     // e
  uint8_t  crb;     // f

  cia8250_t(bus_t *io, int addr) {
  };
  static int io(void *arg, const uint32_t addr, int mode, iodata_t& val) {
    cia8250_t *cia = (cia8250 *)arg;
    return 0;
  };
};
