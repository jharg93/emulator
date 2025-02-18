#ifndef __gamecube_h__
#define __gamecube_h__

#define reg(name, offset) uint16_t& name = *(uint16_t *)&ioregs[offset]

struct gamecube {
  bus_t    mb;
  uint8_t *ram;
  uint8_t *rom;
  uint8_t *fb;
  void init();

  gamecube() {
    mb.init(0xFFFFFFFF);
  };
  void write(uint32_t addr, const void *buf, size_t len);
  void read(uint32_t addr, void *buf, size_t len);
};

static int cp_io(void *arg, const uint32_t addr, int mode, iodata_t& val)
{
  // nop
  return 0;
}
static int pixeng_io(void *arg, const uint32_t addr, int mode, iodata_t& val)
{
  // nop
  return 0;
}
static int video_io(void *arg, const uint32_t addr, int mode, iodata_t& val)
{
  // graphics!!
  return 0;
}
static int pi_io(void *arg, const uint32_t addr, int mode, iodata_t& val)
{
  // print
  return 0;
}
static int mi_io(void *arg, const uint32_t addr, int mode, iodata_t& val)
{
  // nop
  return 0;
}
static int dsp_io(void *arg, const uint32_t addr, int mode, iodata_t& val)
{
  // somestuff
  return 0;
}
static int dvd_io(void *arg, const uint32_t addr, int mode, iodata_t& val)
{
  // 00,04 = nop
  return 0;
}
static int serial_io(void *arg, const uint32_t addr, int mode, iodata_t& val)
{
  // nop
  return 0;
}
static int ei_io(void *arg, const uint32_t addr, int mode, iodata_t& val)
{
  // somestuff
  return 0;
}
static int audio_io(void *arg, const uint32_t addr, int mode, iodata_t& val)
{
  // 00 nopx0
  return 0;
}

void gamecube::init() {
  ram = new uint8_t[0x1800000]{0};
  rom = new uint8_t[0x100000]{0};
  fb  = new uint8_t[0x4000000]{0};
  
  mb.register_handler(0x00000000, 0x017FFFFF, 0x017FFFFF, memio,     ram,  _RW|_DBG, "RAM");
  mb.register_handler(0xFFF00000, 0xFFFFFFFF, 0x000FFFFF, memio,     rom,  _RD|_DBG, "BootROM");
  mb.register_handler(0x08000000, 0x0BFFFFFF, 0x003FFFFF, memio,     fb,   _RW|_DBG, "FrameBuffer");
  mb.register_handler(0x0C000000, 0x0C000FFF, 0x00000FFF, cp_io,     this, _RW|_DBG, "CommandProcessor");
  mb.register_handler(0x0C001000, 0x0C001FFF, 0x000000FF, pixeng_io, this, _RW|_DBG, "PixelEngine");
  mb.register_handler(0x0C002000, 0x0C002FFF, 0x00000FFF, video_io,  this, _RW|_DBG, "VideoInterface");
  mb.register_handler(0x0C003000, 0x0C003FFF, 0x00000FFF, pi_io,     this, _RW|_DBG, "ProcessorInterface");
  mb.register_handler(0x0C004000, 0x0C004FFF, 0x00000FFF, mi_io,     this, _RW|_DBG, "MemoryInterface");
  mb.register_handler(0x0C005000, 0x0C005200, 0x000001FF, dsp_io,    this, _RW|_DBG, "DspInterface");
  mb.register_handler(0x0C006000, 0x0C0063FF, 0x000003FF, dvd_io,    this, _RW|_DBG, "DvdInterface");
  mb.register_handler(0x0C006400, 0x0C0067FF, 0x000003FF, serial_io, this, _RW|_DBG, "SerialInterface");
  mb.register_handler(0x0C006800, 0x0C00693F, 0x0000013F, ei_io,     this, _RW|_DBG, "ExpansionInterface");
  mb.register_handler(0x0C006C00, 0x0C006C20, 0x0000001F, audio_io,  this, _RW|_DBG, "AudioInterface");

  ppc_set_spr(IBAT0U, 0x80001FFF);
  ppc_set_spr(IBAT0L, 0x00000002);
  ppc_set_spr(IBAT3U, 0xfff0001f);
  ppc_set_spr(IBAT3L, 0xfff00001);
};

void gamecube::write(uint32_t addr, const void *buf, size_t len)
{
  printf("WRITE: %lx @ %lx\n", len, addr);
  if (addr + len < 0x17FFFFF)
    memcpy(ram + addr, buf, len);
}

gamecube thegame;

uint8_t cpu_read8(const uint32_t addr, int type)
{
  iodata_t v = 0;

  thegame.mb.read(addr, v);
  return v;
}

void cpu_write8(const uint32_t addr, uint8_t v, int type)
{
  thegame.mb.write(addr, v);
}

template <class T>
static void cpu_write(const uint32_t addr, const T val) {
  uint32_t xaddr = xlate(IBAT0U, addr);
  printf("cpu_write%d: %.8x[%.8x] <- %.8x\n", (int)sizeof(T), addr, xaddr, (uint32_t)val);
  fflush(stdout);
  if constexpr(std::is_same_v<T, uint8_t>) {
    cpu_write8(xaddr, val);
  }
  else if constexpr(std::is_same_v<T, uint16_t>) {
    cpu_write16be(xaddr, val);
  }
  else if constexpr(std::is_same_v<T, uint32_t>) {
    cpu_write32be(xaddr, val);
  }
  else {
    assert(0);
  }
}

template <class T>
static T cpu_read(const uint32_t addr) {
  uint32_t xaddr = xlate(IBAT0U, addr);
  printf("cpu_read%d: %.8x[%.8x]\n", (int)sizeof(T), addr, xaddr);
  if constexpr(std::is_same_v<T, uint8_t>) {
    return cpu_read8(xaddr);
  }
  else if constexpr(std::is_same_v<T, uint16_t>) {
    return cpu_read16be(xaddr);
  }
  else if constexpr(std::is_same_v<T, uint32_t>) {
    return cpu_read32be(xaddr);
  }
  fflush(stdout);
  assert(0);
  return 0;
}

void flogger(int lvl, const char *fmt, ...) {
}
#endif
