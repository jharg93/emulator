#ifndef __8086_dev_h__
#define __8086_dev_h__

/* Clock freq = 14.31818 Mhz
 * CPU / 3 = 4.77  (4.77273)
 * PIT / 12 = 1.19 Mhz
 *
 * CGA = 3.579545 NTSC (/4)
 */

#include "hw/i8237.h"
#include "hw/i8253.h"
#include "hw/i8255.h"

/*========================================================*
 * i8259 PIC: Programmable Interrupt Controller
 *========================================================*/
struct irqx_t {
  uint32_t irq_mask = 0;
  uint32_t irq_pending = 0;

  bool request_irq(int irq) {
    const uint32_t bit = (1L << irq);
    if ((irq_mask & bit) != 0 && (irq_pending & bit) == 0) {
      irq_pending |= bit;
      return true;
    }
    return false;
  };
  void end_irq(int irq) {
    irq_pending &= ~(1L << irq);
  };
};

/*============================*
 * i8259 PIC
 *============================*/
struct i8259_t : public irqx_t
{
  i8259_t(bus_t *io) {
    io->register_handler(0x0020, 0x0021, 0x00FF, i8259_t::io, this, _RW, "i8259 INTERRUPT MASTER");
    io->register_handler(0x00A0, 0x00A1, 0x00FF, i8259_t::io, this, _RW, "i8259 INTERRUPT SLAVE");
  };
  static int io(void *arg, const uint32_t addr, int mode, iodata_t& val);
};

/*============================*
 * LPT port
 *  LPT1: IRQ7, 378
 *  LPT2: IRQ6, 278
 *  LPT3: IRQ5, 3BC
 *============================*/
struct lpt_t {
  lpt_t(bus_t *io, int addr, const char *name) {
    io->register_handler(addr, addr+7, 0x07, lpt_t::io, this, _RW, name);
  };
  static int io(void *arg, const uint32_t addr, int mode, iodata_t& val) {
    return 0;
  };
};

/*============================*
 * COM port
 *  COM1: IRQ4, 3F8
 *  COM2: IRQ3, 2F8
 *  COM3: IRQ4, 3E8
 *  COM4: IRQ3, 2E8
 *============================*/
struct uart_t {
  uart_t(bus_t *io, int addr, const char *name) {
    io->register_handler(addr, addr+7, 0x07, uart_t::io, this, _RW, name);
  };
  static int io(void *arg, const uint32_t addr, int mode, iodata_t& val) {
    return 0;
  };
};

#endif
