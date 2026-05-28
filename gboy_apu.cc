#include <deque>

typedef uint32_t Counter;

static int ctick(Counter&c, int reset=-1) {
  if (c > 0)
    c--;
  if (c != 0) {
    return 1;
  }
  if (reset != -1) {
    c = reset;
  }
  return 0;
};

static void init(Counter&c, int n) {
  c = n;
};

/* 4MH / 256 = 16KHz */
struct channel_t {
  int      id;
  
  bool     enabled;
  void     tick(int n);
  
  uint8_t  duty;
  uint16_t lfsr;
  uint8_t  poly;
  
  /* Length timer tick @ 256Hz
   * NR11/NR21/NR31/NR41
   */
  Counter  length;
  void     tick_length();

  /* Sweep timer tick @ 128Hz
   * NR10
   */
  int      sweep_shift;
  int      sweep_delta;
  int      sweep_freq;
  Counter  sweep_tmr;
  void     tick_sweep();

  /* Volume Envelope tick @ 64Hz
   * NR12/NR22/NR42
   */
  int      vol;
  int      vol_delta;
  Counter  volenv_tmr;
  void     tick_volenv();

  /* Frequency timer tick @ 4MHz (2048) */
  Timer    freq_tmr;
  int      freq;

  /* Current amplitude */
  int      samp;

  /* Wave RAM 0xFF30 .. 0xFF3F */
  uint8_t  ram[16];

  void setfreq(uint8_t hi, uint8_t lo) {
    freq = ((hi << 8) | lo) & 0x7FF;
  }
  void setduty(uint8_t n) {
    duty = n;
  };
  
};

channel_t ch[4];

struct apu_t {
  /* Timer runs at 4MHz / 44100 */
  Timer   samp_tmr;

  int     sndfd;
  
  /* Frame timer runs at 512 Hz : eg 1MHz / 2048 */
  Timer   frame_tmr;
  int     frame;
  uint8_t lvol, rvol;
  uint8_t chnl_mask;
  
  void    tick();
  int     get_sample();
} apu;

const char *chn(const char *fmt, int n) {
  static char chx[32];

  snprintf(chx, sizeof(chx), fmt, n);
  return chx;
}

static uint8_t _apuregs[0x30];
uint8_t& apureg(int offset) {
  return _apuregs[offset - NR10];
}

/* Square wave sample */
static int square_sample(channel_t *c) {
  int s;

  s = (c->duty & 1);
  c->duty = (c->duty >> 1) | (c->duty << 7);
  return (c->enabled && s) ? c->vol : 0;
}

/* FF30..FF3F is 32 samples 22221111 22221111 ... */
static int wave_sample(channel_t *c) {
  int s;

  /* Use duty field as index into wave ram */
  s = c->ram[c->duty/2];
  if (c->duty & 1) {
    s >>= 4;
  }
  c->duty = (c->duty + 1) % 32;
  return s & 0xF;
}

static int noise_sample(channel_t *c) {
  int s;

  s = (c->lfsr & 0x1) ^ ((c->lfsr >> 1) & 1);
  if (c->poly & 0x8) {
    /* Width mode */
    c->lfsr = (c->lfsr >> 1) | (s << 6);
  }
  else {
    c->lfsr = (c->lfsr >> 1) | (s << 14);
  }
  return c->enabled && !s ? c->vol : 0;
}

void channel_t::tick(int n)
{
  id = n;
  /* Check if frequency timer expires */
  if (freq_tmr.tick()) {
    if (!enabled)
      samp = 0;
    else if (n == 0 || n == 1)
      samp = square_sample(this);
    else if (n == 2)
      samp = wave_sample(this);
    else if (n == 3)
      samp = noise_sample(this);
  }
}

void channel_t::tick_length()
{
  if (ctick(length) == 0)
    enabled = false;
}

void channel_t::tick_sweep()
{
  int new_freq;

  if (ctick(sweep_tmr) > 0)
    return;
  new_freq = freq + (freq >> sweep_shift) * sweep_delta;
  if (new_freq > 2047) {
    new_freq = 0;
    enabled = false;
  }
  else {
    freq = new_freq;
    freq_tmr.settimer(2048 - freq, true, true, "freq");
  }
}

void channel_t::tick_volenv()
{
  int new_vol;

  /* Update volume */
  if (ctick(volenv_tmr) > 0)
    return;
  vol += vol_delta;
  if (vol < 0) {
    vol = 0x0;
  } else if (vol > 0xF) {
    vol = 0xF;
  }
}

static constexpr uint8_t duty[] = {
  0b00000001, // 12.5%
  0b10000001, // 25%
  0b10000111, // 50%
  0b01111110, // 75%
};

static void setDacEnabled(int n, uint8_t data) {
}

// NR10: -PPP.NSSS
static void setSweep(channel_t *ch, uint8_t data, const char *lbl) {
  /* Sweep timer tick: update frequency (reload) */
  ch->sweep_shift = data & 7;
  ch->sweep_delta = (data & 0x8) ? -1 : 1;
  init(ch->sweep_tmr, (data >> 4) & 7);
}

// NR11: DDLL.LLLL
// NR21: DDLL.LLLL
static void setDuty(channel_t *ch, uint8_t data) {
  ch->duty = data;
}

// NR11: DDLL.LLLL
// NR21: DDLL.LLLL
// NR31: LLLL.LLLL
// NR41: --LL.LLLL
static void setLength(channel_t *ch, uint8_t length, const char *lbl) {
  /* Length timer tick: disable channel (no reload) */
  init(ch->length, length);
}

/* Set volume envelope: VVVV.UDDD 
 *   VVVV = initial volume
 *      U = down(0), up(1)
 *    DDD = period (0 = off, n*64Hz */
static void setVolEnv(channel_t *ch, uint8_t data, const char *lbl) {
  ch->vol = (data >> 4) & 0xF;
  ch->vol_delta = (data & 0x8) ? 1 : -1;
  init(ch->volenv_tmr, data & 7);
}

static void setFreqLo(channel_t *ch, uint8_t data) {
  ch->freq = (ch->freq & 0xFF00) | data;
}

static void setFreqHi(channel_t *ch, uint8_t data) {
  ch->freq = (ch->freq & 0x00FF) | ((data & 7) << 8);
}

static void setTrigger(int n, uint8_t data) {
  if (data & 0x80) {
    if (n != 3) {
      flogger(-1,"Z%d: ========== Pulse Freq %d | vol:%d,%d,%d | sweep:%d,%d,%d\n",
	      n, ch[n].freq, ch[n].vol, ch[n].vol_delta, ch[n].volenv_tmr,
	      ch[n].sweep_shift, ch[n].sweep_delta, ch[n].sweep_tmr);
      ch[n].freq_tmr.settimer((2048 - ch[n].freq), true, true, chn("freq:%d", n));
    }
    ch[n].enabled = true;
  }
}

/* SSSS.WDDD
 *    SSSS = Shift Clock freq
 *    W = wide (0 = 15 bits, 1 = 7 bits)
 *    Frequency divide ratio
 */
static void setPoly(int n, uint8_t data) {
  ch[n].poly = data;
  flogger(-1, " SND: ch%d poly:%.2x\n", n, data);
}

struct samplet {
  int l, r;
};

samplet getsamp(channel_t *c, int mask) {
  samplet s;

  s.l = (mask & 0xF0) ? c->samp : 0;
  s.r = (mask & 0x0F) ? c->samp : 0;
  return s;
}

#include "SDL.h"
#include "SDL_audio.h"

int snd_cons;
int snd_prod;

uint8_t sndbuf[16384];

uint32_t num_samples = 0;

SDL_AudioDeviceID aid;

static void mixaudio(void *d, uint8_t *stream, int len)
{
  SDL_memset(stream, 0, len);
  SDL_MixAudio(stream, sndbuf, num_samples, SDL_MIX_MAXVOLUME-1);
  num_samples = 0;
}

static void sdl_audio_init()
{
  SDL_AudioSpec fmt, efmt;

  memset(&fmt, 0, sizeof(fmt));
  fmt.freq = 44100;
  fmt.format = AUDIO_U8;
  fmt.channels = 2;
  fmt.samples  = 512/2;
  fmt.callback = mixaudio;
  fmt.userdata = NULL;

  aid = SDL_OpenAudioDevice(NULL, 0, &fmt, &efmt, 0);
  if (aid < 0) {
    flogger(0, "Unable to open audio: %s\n", SDL_GetError());
    return;
  }
  SDL_PauseAudioDevice(aid, 0);
  fprintf(stdout, "Want format: %d.%d.%d\n", fmt.channels, fmt.freq, fmt.format);
  fprintf(stdout, "Got  format: %d.%d.%d\n", efmt.channels, efmt.freq, efmt.format);
}

void apu_t::tick()
{
  /* Run at 4Mhz */
  ch[0].tick(0);
  ch[1].tick(1);
  ch[2].tick(2);
  ch[3].tick(3);

  /* Generate new sample */
  if (samp_tmr.tick()) {
    samplet c1, c2, c3, c4;
    uint8_t l, r;

    ch[2].samp = 0;
    ch[3].samp = 0;
    c1 = getsamp(&ch[0], chnl_mask & 0x11);
    c2 = getsamp(&ch[1], chnl_mask & 0x22);
    c3 = getsamp(&ch[2], chnl_mask & 0x44);
    c4 = getsamp(&ch[3], chnl_mask & 0x88);
    l = (c1.l + c2.l + c3.l + c4.l) * 32;
    r = (c1.r + c2.r + c3.r + c4.r) * 32;
    write(sndfd, &l, sizeof(l));
    write(sndfd, &r, sizeof(r));

    sndbuf[num_samples++] = l;
    sndbuf[num_samples++] = r;
  }
  
  /* Run at 512 Hz */
  if (!frame_tmr.tick())
    return;
  if ((frame & 1) == 0) {
    /* 256 Hz */
    ch[0].tick_length();
    ch[1].tick_length();
    ch[2].tick_length();
    ch[3].tick_length();
  }
  if (frame == 2 || frame == 6) {
    /* 128 Hz */
    ch[0].tick_sweep();
  }
  if (frame == 7) {
    /* 64 Hz */
    ch[0].tick_volenv();
    ch[1].tick_volenv();
    ch[3].tick_volenv();
  }
  frame = (frame + 1) & 7;
}

void apu_init()
{
#if 0
  sdl_audio_init();
  
  apu.sndfd = open("snd.u8", O_CREAT|O_TRUNC|O_WRONLY, 0666);

  /* Set to 44100 Hz */
  apu.samp_tmr.settimer(4*1024*1024/44100, true, true, "samp");
  apu.frame_tmr.settimer(2048, true, true, "frame");
#endif
}

/* Reading value OR with last value written */
static uint8_t apumask(int offset) {
  const uint8_t mask[] = {
    0x80,0x3F,0x00,0xFF,0xBF, // nr10-14
    0xFF,0x3F,0x00,0xFF,0xBF, // nr20-24
    0x7F,0xFF,0x9F,0xFF,0xBF, // nr30-34
    0xFF,0xFF,0x00,0x00,0xBF, // nr40-44
    0x00,0x00,0x70,           // nr50-52
    0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF, // unused
    0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00, // wave table
    0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
  };
  return mask[offset-NR10];
}

int apu_rd(void *arg, uint32_t offset, int mode, iodata_t& data) {
  data = apureg(offset);
  printf("ZR:%.4x[%s] = %.2x\n", offset, rn(offset), data);
  if (offset == NR52) {
    data &= ~0xF;
    data |= (ch[0].enabled ? 0x1 : 0x0);
    data |= (ch[1].enabled ? 0x2 : 0x0);
    data |= (ch[2].enabled ? 0x4 : 0x0);
    data |= (ch[3].enabled ? 0x8 : 0x0);
  }
  data |= apumask(offset);
  //flogger(-1,"ZR:%.4x = %.2x %s\n", offset, data, cpu_getstate());
  return 0;
};

int apu_wr(void *arg, uint32_t offset, int mode, iodata_t& data) {
  printf("ZW:%.4x[%s] = %.2x\n", offset, rn(offset), data);
  if (offset == NR52) {
    /* Clear registers */
    
    memset(_apuregs, 0, 0x20);
    apureg(offset) = data;
    for (int i = 0; i < 4; i++) {
      ch[i].enabled = false;
    }
  }
  else if ((apureg(NR52) & 0x80) == 0) {
    return 0;
  }
  /* Only write registers if powered on */
  apureg(offset) = data;
  switch (offset) {
  case NR10: // -PPP.NSSS
    setSweep(&ch[0], data, "ch1");
    break;
  case NR11: // DDLL.LLLL
    setDuty(&ch[0], duty[data >> 6]);
    setLength(&ch[0], 64 - (data & 0x3F), "ch1");
    break;
  case NR12: // VVVV.APPP
    setVolEnv(&ch[0], data, "ch1");
    break;
  case NR13: // FFFF.FFFF
    setFreqLo(&ch[0], data);
    break;
  case NR14: // TL--.-FFF
    setFreqHi(&ch[0], data);
    setTrigger(0, data);
    break;
    
  case NR21: // NR21: DDLL.LLLL
    setDuty(&ch[1],   duty[data >> 6]);
    setLength(&ch[1], 64 - (data & 0x3F), "ch2");
    break;
  case NR22:
    setVolEnv(&ch[1], data, "ch2");
    break;
  case NR23: // FFFF.FFFF
    setFreqLo(&ch[1], data);
    break;
  case NR24: // TL--.-FFF
    setFreqHi(&ch[1], data);
    setTrigger(1, data);
    break;

  case NR30: // E---.----
    setDacEnabled(2, data & 0x80);
    break;
  case NR31: // NR31: LLLL.LLLL
    setLength(&ch[2], 256 - data, "ch3");
    break;
  case NR33: // FFFF.FFFF
    setFreqLo(&ch[2], data);
    break;
  case NR34: // TL--.-FFF
    setFreqHi(&ch[2], data);
    setTrigger(2, data);
    break;
    
  case NR41: // --LL.LLLL
    setLength(&ch[3], 64 - (data & 0x3F), "ch4");
    break;
  case NR42: // VVVV.APPP
    setVolEnv(&ch[3], data, "ch3");
    break;
  case NR43: // SSSS.WDDD
    setPoly(3, data);
    break;
  case NR44: // TL--.----
    setFreqHi(&ch[3], data);
    setTrigger(3, data);
    break;

  case NR50:
    apu.lvol = (data >> 4) & 0x7;
    apu.rvol = (data & 0x7);
    break;
  case NR51:
    apu.chnl_mask = data;
    break;
  }
  return 0;
}

void apu_end_frame()
{
#if 0
  SDL_QueueAudio(aid, sndbuf, num_samples);
  SDL_Delay(0);
  num_samples = 0;
#endif
}
