#ifndef __cia_t__
#define __cia_t__

enum CiaReg {
  PRA,
  PRB,
  DDRA,
  DDRB,
  TALO,
  TAHI,
  TBLO,
  TBHI,
  TOD0,
  TOD1,
  TOD2,
  TOD3,
  SDR,
  ICR,
  CRA,
  CRB,
};

static const char *crb_mode[] = {
  "Timer B counts 02 pulses",
  "Timer B counts positive CNT transitions",
  "Timer B counts Timer A underflow pulses",
  "Timer B counts Timer A underflow pulses with CNT=high"
};
static const char *cra_mode[] = {
  "Timer A counts 02 pulses",
  "Timer A counts positive CNT transitions",
};
struct cia_t {
  int id;
  
  uint8_t  *regs;
  uint32_t tmra_freq;
  uint32_t tmrb_freq;

  Timer    tmra;
  Timer    tmrb;
  uint8_t  todAlarm[4];
  irq_t    cia_irq;

  void settod(int n, int v) {
    if (regs[CRB] & 0x80) {
      todAlarm[n] = v;
    }
    else {
      regs[TOD0+n] = v;
    }
  };
  int tlatch(int n) {
    int freq = (regs[n+1] << 8) + regs[n];
    if (freq == 0)
      freq = 65536;
    return freq;
  };
  void setreg(uint32_t addr, uint8_t val) {
    assert(addr <= 0xf);
    switch (addr) {
    case CiaReg::TAHI:
      /* In one-shot mode, a write to timer-high (register 5 for timer A, register
       * 7 for Timer B) will transfer the timer latch to the counter and initiate
       * counting regardless of the start bit. */
      regs[TAHI] = val;
      if (regs[CRA] & 0x08) {
	tmra_freq = tlatch(CiaReg::TALO);
	tmra.settimer(tmra_freq, false, true, "tmra");
      }
      break;
    case CiaReg::TBHI:
      regs[TBHI] = val;
      if (regs[CRB] & 0x08) {
	tmrb_freq = tlatch(CiaReg::TBLO);
	tmrb.settimer(tmrb_freq, false, true, "tmrb");
      }
      break;
    case CiaReg::TOD0:
      settod(0, val);
      break;
    case CiaReg::TOD1:
      settod(1, val);
      break;
    case CiaReg::TOD2:
      settod(2, val);
      break;
    case ICR:
      cia_irq.enable(val & 0x7f, val & 0x80);
      break;
    case CiaReg::CRA:
      regs[CRA] = val;
      if ((val & 0b11100110) != 0)  {
	printf("cia%d: cra icky\n", id);
      }
      printf("cia%d: cra %.2x %s [%s]\n", id, val, sbits8("-SILOops", val),
	     cra_mode[(val>>5) & 1]);
      tmra_freq = tlatch(CiaReg::TALO);
      tmra.settimer(tmra_freq, !(val & 0x8), (val & 0x1), "tmra");
      break;
    case CiaReg::CRB:
      // alrm|imod|imod|load|rmod|omod|pron|strt
      regs[CRB] = val;
      if ((val & 0b11100110) != 0)  {
	printf("cia%d: crb icky\n", id);
      }
      printf("cia%d: crb %.2x %s [%s]\n", id, val, sbits8("AIILOops", val),
	     crb_mode[(val>>5) & 3]);
      if (val & 0x80) {
	printf("cia CRB ALARM!\n");
      }
      tmrb_freq = tlatch(CiaReg::TBLO);
      tmrb.settimer(tmrb_freq, !(val & 0x8), (val & 0x1), "tmrb");
      break;
    default:
      regs[addr] = val;
      break;
    };
  };
  uint8_t getreg(uint32_t addr) {
    assert(addr <= 0xf);

    uint8_t val = regs[addr];
    switch (addr) {
    case CiaReg::TALO:
      return tmra.count;
    case CiaReg::TAHI:
      return tmra.count >> 8;
    case CiaReg::TBLO:
      return tmrb.count;
    case CiaReg::TBHI:
      return tmrb.count >> 8;
    case CiaReg::ICR:
      val = cia_irq.sts;
      cia_irq.sts = 0;
      break;
    }
    return val;
  };
};

#endif
