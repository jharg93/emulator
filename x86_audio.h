#ifndef __x86_audio_h__
#define __x86_audio_h__

#include "SDL_audio.h"
#include <deque>

#define QSAMP 1024
#define MAX_SAMPLE 44100 * 2

extern int SPC;
extern uint8_t *getdma(uint32_t addr);

static int sndfd, sndin;
static uint8_t sndbuf[MAX_SAMPLE];
static uint32_t num_samples = 0;
static SDL_AudioDeviceID aid;
static Timer samp_tmr;

struct channel_t {
  Timer tone;
  int   counter = 0x3FF;
  int   freq = 0x3FF;
  int   vol  = 0;
  int   duty = 0;
  bool  enabled = true;
  void setfreq(int v, int mask) {
    rmw(freq, v, mask);
  };
  bool tick() {
    if (freq == 0 || freq == 1) {
      enabled = false;
      return false;
    }
    enabled = true;
    if (--counter == 0) {
      counter = freq;
      return true;
    }
    return false;
  }
};

struct soundgen_t {
  bool enabled = false;

  /* Return sample value. ch=0: LEFT, ch=1: RIGHT */
  virtual int sample(int ch) {
    return 0;
  };
  virtual int tick() {
    return 0;
  };
};

/*================================================*
 * PC Speaker.  On or off
 *================================================*/
struct pcspkr_t : public soundgen_t {
  int duty;

  int tick() {
    if (duty & 2)
      duty ^= 3;
    return 0;
  };
  int sample(int ch) {
    return (enabled && duty) ? 0xFF : 0x00;
  };
};

pcspkr_t pcspkr;
soundgen_t *sound = &pcspkr;

static void mixaudio(void *d, uint8_t *stream, int len)
{
#ifdef WOODY
  adlib_getsample(a, (short *)stream, len/2);
  write(sndfd, stream, len/2);
#else
  if (len > num_samples)
    len = num_samples;
  SDL_memset(stream, 0, len);
  memcpy(stream, sndbuf, len);
  memmove(sndbuf, &sndbuf[len], num_samples - len);
  num_samples -= len;
#endif
}

static void sdl_audio_init()
{
  SDL_AudioSpec fmt, efmt;

  memset(&fmt, 0, sizeof(fmt));
  fmt.freq = 44100;
#ifdef WOODY
  fmt.format = AUDIO_S16;
#else
  fmt.format = AUDIO_U8;
#endif
  fmt.channels = 2;
  fmt.samples  = QSAMP/2;
  fmt.callback = NULL;
  fmt.userdata = NULL;

  aid = SDL_OpenAudioDevice(NULL, 0, &fmt, &efmt, 0);
  if (aid < 0) {
    zprintf("Unable to open audio: %s\n", SDL_GetError());
    return;
  }
  zprintf("Want format: %d.%d.%d\n", fmt.channels, fmt.freq, fmt.format);
  zprintf("Got  format: %d.%d.%d\n", efmt.channels, efmt.freq, efmt.format);
  SDL_PauseAudioDevice(aid, 0);
}

#define SAMPLE_RATE 44100

#define AUDIO_FRAME (1000000000 / SAMPLE_RATE)

void audio_tick()
{
  uint8_t l, r, st;

  if (!sound)
    return;
  sound->tick();
  st = samp_tmr.tick();
  if (st) {
    l = sound->sample(0);
    r = sound->sample(1);
    write(sndfd, &l, sizeof(l));
    write(sndfd, &r, sizeof(r));
    if (num_samples < MAX_SAMPLE && sndbuf) {
      sndbuf[num_samples++] = l;
      sndbuf[num_samples++] = r;
    }
    if (num_samples >= QSAMP) {
      //SDL_QueueAudio(aid, sndbuf, num_samples);
      num_samples = 0;
    }
  }
}

void audio_init()
{
  sndfd = open("snd.u8", O_CREAT|O_TRUNC|O_WRONLY, 0666);
  
  /* Set to 44100 Hz */
  //sdl_audio_init();
  samp_tmr.settimer(CPU_HZ/SAMPLE_RATE, true, true, "samp");
}

/*====================================================*
 * Tandy/PCjr 3 voice SN76496N
 *  Port C0
 *===================================================*/
#define SNDHZ CPU_HZ
struct tandysnd : public soundgen_t {
  /* Channels */
  channel_t chan[4];
  
  uint16_t  r;
  uint32_t  lfsr = 0x10000;

  tandysnd(bus_t *);
  int getvol(int ch);
  int sample(int lr);
  int tick();
  int set(uint8_t);

  static int io(void *arg, const uint32_t addr, int mode, iodata_t& val);

  // Tandy freq = 3*1193181/(32 * count)
  // Works out as (119381*3/32) / freq
  // PC speaker: count = 1193181/freq
  // Want count = 1193181 / n
  int sf(int f) {
    if (!f)
      f = 1024;
    return (f * 32) / 3;
  };
};

/* Set Tandy sound
 * First Byte: 1cccllll
 * Second Byte:0hhhhhhh
 *
 * ccc
 * 000 channel 0 freq
 * 001 channel 0 vol
 * 010 channel 1 freq
 * 011 channel 1 vol
 * 100 channel 2 freq
 * 101 channel 2 vol
 * 110 channel 3 freq (noise)
 * 111 channel 3 vol  (noise)
 */
int tandysnd::set(uint8_t v)
{
  int ch;
  bool setnoise = false;

  enabled = true;
  if (v & 0x80) {
    /* First byte: 1rrr.vvvv */
    r = (v >> 4) & 7;
    ch = r / 2;
    if (r == 0 || r == 2 || r == 4 || r == 6) {
      // Set low bits of channel frequency (
      chan[ch].setfreq(v, 0xf);
    }
    else if (r == 1 || r == 3 || r == 5 || r == 7) {
      // set channel volume
      chan[ch].vol = 0xf - (v & 0xF);
    }
    setnoise = (r == 6);
  }
  else {
    /* 2nd byte: 00vv.vvvv */
    ch = r / 2;
    if (r == 0 || r == 2 || r == 4) {
      char tonez[32];
      // set upper bits of frequency
      chan[ch].setfreq(v << 4, 0x3f0);
      //snprintf(tonez, sizeof(tonez), "tone:%d", ch);
      //chan[ch].tone.settimer(sf(chan[ch].freq), 1, 1, tonez);
      setnoise = (r == 4);
    }
  }
  if (setnoise) {
    // Set noise frequency
    int n = chan[3].freq & 3;
    int period = (n == 3) ? chan[2].freq*2 : (1 << (5+n));
    //chan[3].tone.settimer(sf(period), 1, 1, "noise");
    chan[3].duty = lfsr;
  }
  return 0;
}

int tandysnd::tick()
{
  for (int i = 0; i < 3; i++) {
    if (chan[i].tick()) {
      /* Toggle frequency on/off */
      chan[i].duty ^= 1;
    }
  };
  if (chan[3].tick()) {
    /* Rotate and re-seed LFSR, bit 1 */
    int sbit = (chan[3].duty & 0x2) ? lfsr : 0x0;
    chan[3].duty = (chan[3].duty >> 1) | sbit;
  }
  return 0;
};

// vlog[vol]*17
static int voltab[] = {
  255, 203, 161, 128, 102, 81, 64, 51, 41, 32, 25, 20, 16, 13, 10, 0
};
static float vlog[] = {
  0.00000f, 0.59715f, 0.75180f, 0.94650f, 1.19145f, 1.50000f, 1.88835f, 2.37735f,
  2.99295f, 3.76785f, 4.74345f, 5.97165f, 7.51785f, 9.46440f, 11.9194f, 15.0000f
};

int tandysnd::getvol(int ch)
{
  if (!chan[ch].enabled || chan[ch].vol == 0x0 || !(chan[ch].duty & 0x1))
    return 0;
  return vlog[chan[ch].vol];
}

int tandysnd::sample(int ch)
{
  int s0, s1, s2, s3, fv = 0;
  static int maxfv = 0;
  
  s0 = getvol(0);
  s1 = getvol(1);
  s2 = getvol(2);
  s3 = getvol(3);
  fv = (s0 + s1 + s2 + s3);
  if (fv > maxfv) {
    printf("maxv: %d\n", fv);
    maxfv = fv;
  }
  return fv;
}

/* Tandy sound handler */
tandysnd::tandysnd(bus_t *io) {
  for (int i = 0; i < 4; i++) {
    chan[i].freq = 0;
    chan[i].vol = 0x0;
  }
  /* Set Noise LFSR */
  chan[3].duty = lfsr;
  io->register_handler(0x00c0, 0x00c7, 0xFFFF, tandysnd::io, this, _RW, "Tandy Sound");
};

int tandysnd::io(void *arg, const uint32_t addr, int mode, iodata_t& val)
{
  tandysnd *t = (tandysnd *)arg;

  zprintf("tandy %.4x: %.8x %c %.2x ", SPC, addr, mode, val);
  if ((val & 0x81) == 0x81) {
    printf(" chan %d vol: %x\n", (val >> 5) & 3, val & 0xF);
  }
  else if (val & 0x80) {
    printf(" chan %d freq.lo %x\n", (val >> 5) & 3, val & 0xf);
  }
  else {
    printf(" freq.hi %x\n", val & 0x3F);
  }
  if (mode == 'w')
    t->set(val);
  return 0;
}

/*====================================
 * SOUNDBLASTER
 *====================================*/
struct sblaster : public soundgen_t {
  uint8_t  cmdbuf[32];
  int      cmdlen = 0;
  int      dmalen = 0;
  uint8_t *dma;
  Timer    t;
  
  sblaster(bus_t *io, int port);
  
  int tick();
  int sample(int);
  void dspcmd(uint8_t cmd);
  int _cmdlen(uint8_t cmd);
  static int io(void *arg, const uint32_t addr, int mode, iodata_t& val);
};

/* IO ports
 * 2x6 wo : reset
 * 2xA ro : DSP read data
 * 2xC wr : DSP write command data
 * 2xC rd : read status
 * 2xE rd : read buffer status
 *
 * Registers:
 *  80h: Interrupt setup
 *  81h: DMA Setup
 *  82h: interrupt status
 *  
 */
int sblaster::_cmdlen(uint8_t cmd) {
  /* Get number of expected bytes for each cmd */
  switch (cmd) {
  case 0x40: return 2;
  case 0x14: return 3;
  }
  return 1;
}

void sblaster::dspcmd(uint8_t v)
{
  cmdbuf[cmdlen++] = v;
  if (cmdlen != _cmdlen(cmdbuf[0])) {
    return;
  }
  cmdlen = 0;
  switch(cmdbuf[0]) {
  case 0xd1:
    enabled = true;
    break;
  case 0xd3:
    enabled = false;
    break;
  case 0x14:
    dmalen = cmdbuf[1] + (cmdbuf[2] << 8);
    dma = getdma(dmachan[1].src);
    zprintf("dma8: %.4x\n", dmalen);
    break;
  case 0x40:
    zprintf("time constant: %.2x\n", cmdbuf[1]);
    t.settimer((CPU_HZ * (256 - v)) / 1000000, 1, 1, "dmatmr");
    break;
  }
}

int sblaster::io(void *arg, const uint32_t addr, int mode, iodata_t& val)
{
  sblaster *sb = (sblaster *)arg;
  
  zprintf("SBIO: %.4x %.2x\n", addr, val);
  if (mode == 'w' && (addr & 0xF) == 0xC) {
    sb->dspcmd(val);
  }
  return 0;
}

int sblaster::tick()
{
  if (!t.tick()) {
    return 0;
  }
  if (dmalen > 0) {
    zprintf("XFER:%x %d %d %d\n", dmalen, t.reset, t.count, t.enabled);
    dma++;
    dmalen--;
  }
  else {
    zprintf("SOUND DONE!!\n");
  }
  return 0;
}

int sblaster::sample(int ch)
{
  if (dmalen > 0) {
    return *dma;
  }
  return 0;
}

sblaster::sblaster(bus_t *io, int port)
{
  enabled = true;
  io->register_handler(port, port+0xF, 0xFFFF, sblaster::io, this, _RW, "SoundBlaster");
}

//#define WOODY
#ifdef WOODY
#include "woody/woodyopl.cpp"

struct adlib_t *adlib_new(uint16_t samplerate)
{
  OPLChipClass *opl = new OPLChipClass(0);
  opl->adlib_init(samplerate, true);
  opl->adlib_write(0x01, 1 << 5, 0);
  return (struct adlib_t *)(opl);
}

uint8_t adlib_read(struct adlib_t *adlib)
{
  OPLChipClass *opl = (OPLChipClass *)(adlib);
  return opl->status | 6;
}

void adlib_write(struct adlib_t *adlib, uint8_t addr, uint8_t data)
{
  OPLChipClass *opl = (OPLChipClass *)(adlib);
  opl->adlib_write(addr, data, 0);
}

void adlib_getsample(struct adlib_t *adlib, short *buf, size_t samples)
{
  OPLChipClass *opl = (OPLChipClass *)(adlib);
  opl->adlib_getsample(buf, samples);
}

void adlib_free(struct adlib_t *adlib)
{
  OPLChipClass *opl = (OPLChipClass *)(adlib);
  delete opl;
}

struct adlib_t *a;

struct opl2 : public soundgen_t {
  int idx;
  opl2(bus_t *io, int port);

  static int opl2io(void *arg, const uint32_t addr, int mode, iodata_t& val);
};

int opl2::opl2io(void *arg, const uint32_t addr, int mode, iodata_t& val)
{
  static int idx;
  
  if (mode == 'w') {
    if (addr == 0x388)
      idx = val;
    else
      adlib_write(a, idx, val);
  }
  else {
    if (addr == 0x388) {
      val = adlib_read(a);
      printf("OPL2 read status: %.2x\n", val);
    }
  }
  return 0;
}

opl2::opl2(bus_t *io, int port)
{
  a = adlib_new(44100);
  io->register_handler(port, port+1, 0xFFFF, opl2::opl2io, this, _RW, "OPL2");
}

#endif


#endif
