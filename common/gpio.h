#ifndef __gpio_h__
#define __gpio_h__

typedef uint8_t (*cbfn)(void *, uint8_t, int);

struct gpio_t {
  uint8_t istate;
  uint8_t ostate;
  uint8_t ddr;
  void init(int d, int o = 0, int i = 0) {
    ddr = d;
    istate = i;
    ostate = o;
  };
  void write_ddr(uint8_t _ddr) {
    ddr = _ddr;
  };
  uint8_t read_ddr() const {
    return ddr;
  };
  uint8_t write(uint8_t nv) {
    uint8_t old = ostate;
    ostate = (istate & ~ddr) | (nv & ddr);
    if (rwcb) {
      rwcb(cbarg, ostate, 'w');
    }
    // report which bits changed
    return old ^ ostate;
  };
  uint8_t read() {
    if (rwcb) {
      istate = rwcb(cbarg, 0, 'r');
    }
    return (ostate & ddr) | (istate & ~ddr);
  };
  cbfn rwcb;
  void *cbarg;
  void set_rwcb(cbfn fn, void *arg) {
    rwcb = fn;
    cbarg = arg;
  };
};


#endif
