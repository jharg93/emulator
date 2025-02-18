#ifndef __audio_h__
#define __audio_h__

#include "SDL.h"
#include "SDL_audio.h"

SDL_AudioDeviceID aid;

void mixaudio(void *d, uint8_t *stream, int len)
{
#if 0
  SDL_memset(stream, 0, len);
  printf("MIXME: %d,%d\n", len, num_samples);
  SDL_MixAudio(stream, sndbuf, num_samples, SDL_MIX_MAXVOLUME-1);
  num_samples = 0;
#endif
}

void sdl_audio_init(uint32_t sndHz)
{
#if 0
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
#endif
  unlink("snd.u8");
}

struct soundgen_t {
  bool enabled = false;

  virtual int sample(int ch) {
    return 0;
  };
  virtual int tick() {
    return 0;
  };
};

#define NUM_SAMPLES 1024
struct apu_t {
  uint8_t samples[NUM_SAMPLES] = { 0 };
  int     num_samples = 0;
  Timer   samp_tmr;

  soundgen_t *sound = NULL;
  apu_t(int sampfreq, soundgen_t *s) {
    sound = s;
    samp_tmr.settimer(sampfreq, 1, 1, "Sample Timer");
  }
  void tick();
};

void apu_t::tick()
{
  int fd, st;
  
  if (!sound)
    return;
  sound->tick();
  st = samp_tmr.tick();
  if (st) {
    samples[num_samples++] = sound->sample(0);
    samples[num_samples++] = sound->sample(1);
    if (num_samples < NUM_SAMPLES)
      return;
    //hexdump(samples, NUM_SAMPLES);
    fd = open("snd.u8", O_WRONLY | O_APPEND | O_CREAT, 0644);
    if (fd >= 0) {
      write(fd, samples, num_samples);
      close(fd);
    }
    num_samples = 0;
  }
}

#endif
