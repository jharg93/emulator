#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <inttypes.h>
#include <unistd.h>
#include <stdarg.h>
#include <fcntl.h>
#include <thread>
#include "gr.h"
#include "font8.h"

extern void cpu_shutdown();

int nokbd;

static inline bool inrange(int v, int l, int delta) {
  return (v >= l && v < (l + delta));
}

constexpr int MS_PER_FRAME = 1000/59.94;

/* Return clock time in ms */
uint32_t GetClock() {
  struct timespec ts;
  clock_gettime(CLOCK_MONOTONIC, &ts);
  return (ts.tv_sec * 1000 + (ts.tv_nsec / 1000000.0));
}

void waitFrame() {
  static uint32_t last_frame, cur_frame;
  do {
    cur_frame = GetClock();
  } while ((cur_frame - last_frame) < MS_PER_FRAME);
  last_frame = cur_frame;
};  

// https://stackoverflow.com/questions/31999935/how-to-get-an-sdl-pixelformat-from-an-sdl-pixelformatenum-or-sdl-texture
#ifdef AUDIO
void *snd_thread(void *)
{
  for(;;) {
  }
}

void deviceCallback (void *userdata, Uint8 *stream, int len)
{
  float *signal = (float *)(stream);
  int size = len / sizeof(float);
  unsigned i;

  SDL_memset(stream, 0, len);
  for (i = 0; i < size; i++)
    signal[i] = (tsound.getsample() - 127) / 255.0;
}

void audio_init()
{
  SDL_AudioSpec want, have;
  int rc;
  DWORD h;
  SDL_zero(want);
  want.freq = FREQ;
  want.format = AUDIO_F32;
  want.channels = 1;
  want.samples = 512;
  want.callback = deviceCallback;
  want.userdata = nullptr;
  dev = SDL_OpenAudioDevice(nullptr, 0, &want, &have, 0);
  FREQ = have.freq;
  SDL_PauseAudioDevice(dev, 0);

  CreateThread(NULL, 0, (LPTHREAD_START_ROUTINE)tiasnd_foo, NULL, 0, &h);
}
#else
#define audio_init()
#endif

extern int trace;
int paused;

/* Size of each 'pixel' */
#define XSCALE 2
#define YSCALE 2

/* Colors for the grid */
palclr redPxl = { 128, 0, 0 };
palclr greenPxl = { 0, 128, 0 };

#ifdef _SDL
/* Map SDLK_xxx to Key::K_zzz */
int sdl_mapkey(SDL_Event *e)
{
  int sk = e->key.keysym.sym;
  int mod = e->key.keysym.mod;
  const char *lc = "1234567890-=;,./\'";
  const char *uc = "!@#$%^&*()_+:<>?\"";

  for (int i = 0; lc[i]; i++) {
    if (sk == lc[i] && (mod & KMOD_SHIFT)) {
      e->key.keysym.mod &= ~(KMOD_SHIFT);
      return uc[i];
    }
  }
  switch (sk) {
  case ' ' ... 'z':
    return sk;
  case SDLK_F1 ... SDLK_F12:
    printf("Fn: %d\n", sk - SDLK_F1);
    return Key::K_F1 + (sk - SDLK_F1);
  case SDLK_LEFT:
    return Key::K_LEFT;
  case SDLK_RIGHT:
    return Key::K_RIGHT;
  case SDLK_UP:
    return Key::K_UP;
  case SDLK_DOWN:
    return Key::K_DOWN;
  case SDLK_RETURN:
    return Key::K_ENTER;
  case SDLK_DELETE:
    return Key::K_DEL;
  case SDLK_BACKSPACE:
    return Key::K_DEL;
  case SDLK_TAB:
    return Key::K_TAB;
  case SDLK_LGUI:
    return Key::K_WINDOWS;
  }
  return 0;
}

void (*sdl_keyhook)(int vk, int vmod, int state);

void Screen::setKeyState(SDL_Event& event, bool state)
{
  int sk;

  if (sdl_keyhook) {
    sdl_keyhook(event.key.keysym.sym, event.key.keysym.mod, state);
  }
  sk = sdl_mapkey(&event);
  KeyState[sk] = state;
  printf("keystate: %.2x[%c] = %d\n", sk, sk >= ' ' && sk <= 'z' ? sk: 'x', state);
  if (event.key.keysym.mod & KMOD_LSHIFT) {
    KeyState[Key::K_LSHIFT] = state;
  }
  if (event.key.keysym.mod & KMOD_RSHIFT) {
    KeyState[Key::K_RSHIFT] = state;
  }
  if (event.key.keysym.mod & KMOD_LCTRL) {
    KeyState[Key::K_LCTRL] = state;
  }
  if (event.key.keysym.mod & KMOD_RCTRL) {
    KeyState[Key::K_RCTRL] = state;
  }
}

/* Draw a filled rectangle at screen resolution */
void Screen::sdl_rect(int x, int y, int w, int h, const palclr& p)
{
  SDL_Rect rect = { tx+x, tx+y, w, h };

  if (x >= 0 && y >= 0 && x+w <= (xs * tw) && y+h <= (ys * th)) {
    SDL_SetRenderDrawColor(renderer, p.r, p.g, p.b, 0);
    SDL_RenderFillRect(renderer, &rect);
  }
}
#endif

/* Create a graphics screen: Width x Height and Palette */
Screen::Screen(int w, int h, int ex, int ey, int nPal, palclr *palette) : width(w), height(h) {
  /* X and Y-scale factor */

  xs = XSCALE;
  ys = YSCALE;
  
  /* Add extra height (for debug area below) */
  th = height + ey;
  tw = width;
  scrBuf = new color[tw * th]{0};
  for (int i=0; i<256; i++) {
    defpal[i].r = 0x40;
    defpal[i].g = 0x40;
    defpal[i].b = 0x40;
  }
  if (palette != NULL) {
    setpalette(nPal, palette);
  }
  audio_init();
};

palclr Screen::pxl2rgb(color c)
{
  palclr p;
  
  if (!clrmode)
    return defpal[c];
  /* Color mode rgb */
  p.r = (c >> 16) & 0xFF;
  p.g = (c >> 8) & 0xFF;
  p.b = c & 0xFF;
  return p;
}

int Screen::getmouse(int&x, int&y) {
  x = mouse_x;
  y = mouse_y;
  return mouse_btn;
}

void Screen::setpalette(int i, int r, int g, int b) {
  defpal[i].r = r;
  defpal[i].g = g;
  defpal[i].b = b;
}

void Screen::setpalette(int n, const palclr *pal) {
  for (int i = 0; i < n; i++)
    defpal[i] = pal[i];
}

void Screen::setpixel(int x, int y, color c) {
  if (inrange(x, 0, tw) && inrange(y, 0, th)) {
    scrBuf[(y * tw) + x] = c;
  }
};

int Screen::getpixel(int x, int y) {
  if (inrange(x, 0, tw) && inrange(y, 0, height))
    return scrBuf[(y * tw) + x];
  return 0;
};

static bool glyphbits(uint8_t bits, int n) {
  return (bits >> n) & 0x1;
}

void Screen::drawglyph(const uint8_t *bmp, int fontHeight, int x, int y, int fg, int bg, bool (*gbit)(uint8_t, int)) {
  if (!gbit)
    gbit = glyphbits;
  for (int i = 0; i<fontHeight; i++) {
    for (int j = 0; j < 8; j++) {
      if (gbit(bmp[i], j))
	setpixel(x + j, y + i, fg);
      else if (bg != -1)
	setpixel(x + j, y + i, bg);
    }
  }
}
  
/* Draw text on the screen */
void Screen::scrtext(int x, int y, int clr, const char *fmt, ...)
{
  char dstr[128];
  va_list ap;
  int ch;
  
  va_start(ap, fmt);
  vsnprintf(dstr, sizeof(dstr), fmt, ap);
  va_end(ap);

  for (int i = 0; (ch = dstr[i]) != 0; i++) {
    drawglyph(font[ch], 8, x, y, clr, -1);
    x += 8;
  }
}

/* Draw a horizontal line on the screen */
void Screen::scrline(int x, int y, int len, int fg) {
  int i;

  if (!inrange(x, 0, tw) || !inrange(y, 0, th))
    return;
  if (x + len > tw)
    len = tw - x;
  for (i = 0; i < len; i++) {
    setpixel(x+i, y, fg);
  }
}

/* Draw a filled rectangle on the screen */
void Screen::scrrect(int x, int y, int w, int h, int fg) {
  for (int j = 0; j < h; j++) {
    for (int i = 0; i < w; i++) {
      setpixel(x+i, y+j, fg);
    }
  }
}

#ifdef _NOSDL
#include <termios.h>
#define VK_LEFT   0x1b5b44
#define VK_RIGHT  0x1b5b43
#define VK_DOWN   0x1b5b42
#define VK_UP     0x1b5b41

char getch(){
  /*#include <unistd.h>   //_getch*/
  /*#include <termios.h>  //_getch*/
  char buf=0;
  struct termios old={0};
  fflush(stdout);
  if(tcgetattr(0, &old)<0)
    perror("tcsetattr()");
  old.c_lflag&=~ICANON;
  old.c_lflag&=~ECHO;
  old.c_cc[VMIN]=1;
  old.c_cc[VTIME]=0;
  if(tcsetattr(0, TCSANOW, &old)<0)
    perror("tcsetattr ICANON");
  if(read(0,&buf,1)<0)
    perror("read()");
  old.c_lflag|=ICANON;
  old.c_lflag|=ECHO;
  if(tcsetattr(0, TCSADRAIN, &old)<0)
    perror ("tcsetattr ~ICANON");
  return buf;
}

int _getch()
{
  int ch;
  
  ch = getch();
  if (ch == 0x1b) {
    ch = getch();
    if (ch == 0x5b)
      ch = 0x1b5b00 + getch();
  }
  return ch;
}
#endif

int grid = false;
extern void cpu_reset(uint32_t addr);
extern int frame;
#define JOY_U D4
#define JOY_D D5
#define JOY_L D6
#define JOY_R D7

//#define HALFBLOCK "▀▀▀"
#define HALFBLOCK "▀"

void Screen::init(int cm) {
#ifdef _SDL
  printf("sdl init\n");
  SDL_Init(SDL_INIT_EVERYTHING);
#ifdef OPENGL
  win = SDL_CreateWindow("test", 30, 30, tx*2 + tw * xs, ty*2 + th * ys, SDL_WINDOW_OPENGL);
  glcontext = SDL_GL_CreateContext(win);
  glClearColor(0.2,0.2,0.2,1);
  glClear(GL_COLOR_BUFFER_BIT);
  SDL_GL_SwapWindow(win);
  glClearColor(0.2,0.2,0.2,1);
  glClear(GL_COLOR_BUFFER_BIT);
  SDL_GL_SwapWindow(win);
#else
  win = SDL_CreateWindow("test", 30, 30, tx*2 + tw * xs, ty*2 + th * ys, 0);
  renderer = SDL_CreateRenderer(win, -1, SDL_RENDERER_ACCELERATED | SDL_RENDERER_TARGETTEXTURE);
#endif
#endif
  clrmode = cm;

#ifdef _SDL
  int njoy = SDL_NumJoysticks();
  for (int i = 0; i < njoy; i++) {
    fprintf(stdout, "joystick %d:%s\n", i, SDL_JoystickNameForIndex(i));
  }
  if (njoy >= 0) {
    joy = SDL_JoystickOpen(0);
    naxes = SDL_JoystickNumAxes(joy);
    nbuttons = SDL_JoystickNumButtons(joy);
    fprintf(stdout, "axes:%d buttons:%d\n", naxes, nbuttons);
    if (naxes > 0)
      axes = new int16_t[naxes]{0};
    if (nbuttons > 0)
      buttons = new uint8_t[nbuttons]{0};
  }
#endif
}

/* Clear memory buffer/Render for next frame */
void Screen::clrmem(int clr) {
  int i;

  for (i=0; i<tw*th; i++)
    scrBuf[i] = clr;
#ifdef _SDL
  SDL_RenderClear(renderer);
#endif
}

// millisec = 1000
// microsec = 1000000
// nanosec  = 1000000000

#define CPU_HZ 4*1024*1024L
#define GB_FPS 60.0
#define CYCPERFRAME 69905

#define CYCLES_PER_MS 4194.304
#define MS_PER_FRAME  16.666

uint32_t last_frame;

/* Draw screen and poll keys */
void Screen::draw() {
#ifdef _SDL
  SDL_Event event = { 0 };

#if 0
  static uint32_t max_delta = 0;
  int delta;
  
  uint32_t cur_frame = SDL_GetTicks();

  delta = cur_frame - last_frame;
  printf("delta: %d\n", delta);
  if (delta < MS_PER_FRAME) {
    SDL_Delay(MS_PER_FRAME - delta);
  }
  if (delta > max_delta) {
    printf("max delta: %d\n", delta);
    max_delta = delta;
  }
  last_frame = cur_frame;
#endif
  
#ifndef OPENGL
  SDL_SetRenderDrawColor(renderer, 0, 0, 0, 0);
  SDL_RenderClear(renderer);

  for (int y=0; y<th; y++) {
    for (int x=0; x < tw; x++) {
      int c = pixel(x, y);
      if (xs > 1 && ys > 1)
	sdl_rect(x * xs, y * ys, xs, ys, pxl2rgb(c));
      else
	sdl_rect(x, y, 1, 1, pxl2rgb(c));
    }
  }
  /* Draw grid */
  if (grid) {
    for (int i = 0; i < tw; i += 16) {
      sdl_rect(i * xs, 0, xs, height * ys, redPxl);
    }
    for (int i = 0; i < height; i += 16) {
      sdl_rect(0, i * ys, tw * xs, ys, greenPxl);
    }
  }
  SDL_RenderPresent(renderer);
#endif
  do {
    if (SDL_PollEvent(&event)) {
      if (event.type == SDL_QUIT) {
	SDL_DestroyRenderer(renderer);
	SDL_DestroyWindow(win);
	exit(0);
      }
      if (event.type == SDL_JOYAXISMOTION) {
	assert(event.jaxis.axis < naxes);
	axes[event.jaxis.axis] = event.jaxis.value;
      }
      if (event.type == SDL_JOYBUTTONDOWN ||
	  event.type == SDL_JOYBUTTONUP) {
	assert(event.jbutton.button < nbuttons);
	buttons[event.jbutton.button] = event.jbutton.state;
	fprintf(stdout, "button: %d,%d\n", event.jbutton.button, event.jbutton.state);
      }
      if (event.type == SDL_MOUSEMOTION) {
	//SDL_GetMouseState(&mouse_x, &mouse_y);
	mouse_x = clip((event.motion.x - tx) / xs, 0, width);
	mouse_y = clip((event.motion.y - ty) / ys, 0, height);
      }
      if (event.type == SDL_MOUSEBUTTONDOWN ||
	  event.type == SDL_MOUSEBUTTONUP) {
	mouse_btn = 0;
	switch (event.button.button) {
	case SDL_BUTTON_LEFT:
	  mouse_btn |= (event.button.state == SDL_PRESSED) ? 0x1 : 0x0;
	  break;
	case SDL_BUTTON_RIGHT:
	  mouse_btn |= (event.button.state == SDL_PRESSED) ? 0x2 : 0x0;
	  break;
	case SDL_BUTTON_MIDDLE:
	  mouse_btn |= (event.button.state == SDL_PRESSED) ? 0x4 : 0x0;
	  break;
	}
	mouse_x = clip((event.button.x - tx) / xs, 0, width);
	mouse_y = clip((event.button.y - ty) / ys, 0, height);
      }
      if (event.type == SDL_KEYDOWN) {
	setKeyState(event, true);
	/* Handle some common keys */
	if (event.key.keysym.sym == 0x1b) {
	  /* ESC:murder the console */
	  SDL_DestroyRenderer(renderer);
	  SDL_DestroyWindow(win);
	  cpu_shutdown();
	  exit(0);
	}
	if (nokbd) {
	  if (key('r', true)) {
	    cpu_reset(0);
	  }
	  else if (key('g'))
	    grid ^= 1;
	  else if (key('t'))
	    trace ^= 3;
	  else if (key('`', true)) {
	    paused ^= 1;
	    printf("pasued: %d\n", paused);
	  }
	}
      }
      if (event.type == SDL_KEYUP) {
	setKeyState(event, false);
      }
    }
  } while (paused);
#ifdef OPENGL
#if 0
  glClearColor(.2f, 0.2f, 0.2f, 1.0f);
  glScissor(tx, ty, width, height);
  glEnable(GL_SCISSOR_TEST);
  glClear(GL_COLOR_BUFFER_BIT);
  glDisable(GL_SCISSOR_TEST);

  glColor3f(1.0, 0.0, 0.0);
  glBegin(GL_QUADS);
  glVertex2f(-0.5f, -0.5f);
  glVertex2f(0.5f, -0.5f);
  glVertex2f(0.5f, 0.5f);
  glVertex2f(-0.5f, 0.5f);
  glEnd();
#endif
  SDL_GL_SwapWindow(win);
  SDL_GL_SwapWindow(win);
#endif
#else
  int y, x, lc, ld, c, d;
  /* Move cursor to 0,0 & hide cursor */
  printf("\033[0;0H\033[?25l");
  for (y = 0; y < th; y+=2) {
    lc = ld = -1;
    for (x = 0; x < tw; x++) {
      c = pixel(x, y);
      d = pixel(x, y+1);
      if (c != lc || d != ld) {
	palclr pc, pd;
	pc = pxl2rgb(c);
	pd = pxl2rgb(d);
	/* Set ansi foreground/background color.
	 * Only set if colors change */
	printf("\033[38;2;%d;%d;%d;48;2;%d;%d;%dm",
	       pc.r, pc.g, pc.b,
	       pd.r, pd.g, pd.b);
      }
      /* Use halfblock character to use FG/BG */
      printf("%s", HALFBLOCK);
      lc = c;
      ld = d;
    }
    printf("\n");
  }
  /* Set foreground back to white */
  printf("\033[30;107m");
#endif
  clrmem();
};

void Screen::clear(int clr) {
  clrmem(clr);
  //printf("\033[?25l\033[0;0Hm");
  //printf("\033c");
  //printf("\033[31m;0;0H\n\n\n----------------------------------------");
}

// Display (text mode) graphics colors
void printpix(int fg, int bg, palclr *palette)
{
  if (fg == -1) {
    printf("\033[0m    ");
  }
  else {
    printf("\033[48;2;%d;%d;%d;38;2;%d;%d;%dm    ",
	   palette[fg].r,
	   palette[fg].g,
	   palette[fg].b,
	   palette[bg].r,
	   palette[bg].g,
	   palette[bg].b);
  }
}

// Show all ATARI colors
void showClrs(Screen *s, int np)
{
  int i;

#ifndef _SDL
  for (i=0; i<np; i++) {
    printpix(i, 0, s->defpal);
    printf("%3x ", i);
    if ((i & 15) == 15)
      printf("\n");
  }
#else
  int scale = 3;
  int x, y, xs, ys;
  SDL_Event event;
  
  SDL_Init(SDL_INIT_EVERYTHING);
  s->win = SDL_CreateWindow("test", SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED,
			    s->width * scale, s->height * scale, 0);
  s->renderer = SDL_CreateRenderer(s->win, -1, SDL_RENDERER_ACCELERATED | SDL_RENDERER_TARGETTEXTURE);

  xs = (s->width * scale) / 8;
  ys = (s->height * scale) / (np / 8);
  x = 0;
  y = 0;
  for (i = 0; i < np; i++) {
    s->sdl_rect(x, y, xs-2, ys-2, s->defpal[i]);
    x += xs;
    if ((i & 7) == 7) {
      x = 0;
      y += ys;
    }
  }
  SDL_RenderPresent(s->renderer);
  SDL_Delay(50000);
  while (SDL_PollEvent(&event)) {
  }
  SDL_DestroyRenderer(s->renderer);
  SDL_DestroyWindow(s->win);
#endif
}

const uint8_t *genbpp(int *line, int w, const uint8_t *mem, const int bpp)
{
   for (int x = 0; x < w; x += 8 / bpp) {
    uint8_t pxl = *mem++;
    if (bpp == 1) {
      *line++ = ((pxl >> 7) & 0x1);
      *line++ = ((pxl >> 6) & 0x1);
      *line++ = ((pxl >> 5) & 0x1);
      *line++ = ((pxl >> 4) & 0x1);
      *line++ = ((pxl >> 3) & 0x1);
      *line++ = ((pxl >> 2) & 0x1);
      *line++ = ((pxl >> 1) & 0x1);
      *line++ = ((pxl >> 0) & 0x1);
    } else if (bpp == 2) {
      *line++ = ((pxl >> 6) & 0x3);
      *line++ = ((pxl >> 4) & 0x3);
      *line++ = ((pxl >> 2) & 0x3);
      *line++ = ((pxl >> 0) & 0x3);
    } else if (bpp == 4) {
      *line++ = ((pxl >> 4) & 0xF);
      *line++ = ((pxl >> 0) & 0xF);
    } else if (bpp == 8) {
      *line++ = pxl;
    }
  }
  return mem;
}

/* NES:
 *   getplane(line, 8, &chrom[id*16], 2, 1);
 */
const uint8_t *getplane(int *line, int w, const uint8_t *mem, const int bpp, int step) {
  int mask;
  
  for (int x = 0; x < w; x++) {
    *line = 0;
    mask = 0x80 >> (x & 7);
    for (int p = 0; p < bpp; p++) {
      if (mem[p * step] & mask) {
	*line |= (1 << p);
      }
    }
    if ((x & 7) == 7) {
      mem++;
    }
  }
  return mem;
};

struct Vec3 {
  float x, y, z;
  Vec3(float _x=0, float _y=0, float _z=0) {
    x = _x;
    y = _y;
    z = _z;
  };
  Vec3 operator-(const Vec3 &rhs) const {
    return Vec3(x - rhs.x, y - rhs.y, z - rhs.z);
  };
  Vec3 operator+(const Vec3 &rhs) const {
    return Vec3(x + rhs.x, y + rhs.y, z + rhs.z);
  };
  Vec3 operator*(const float s) const {
    return Vec3(x*s, y*s, z*s);
  };
  Vec3 operator/(const float s) const {
    return Vec3(x/s, y/s, z/s);
  };
};

Vec3 rgb2vec(int c) {
  return Vec3((c >> 16) & 0xFF, (c >> 8) & 0xff, c & 0xff);
};

int vec2rgb(Vec3 c) {
  return ((int)c.x << 16) + ((int)c.y << 8) + ((int)c.z);
};

void draw_gradient(Screen *scr,
		   int x0, int y0, int c0,
		   int x1, int y1, int c1,
		   int x2, int y2, int c2)
{
  Vec3 v0(x0, y0, c0);
  Vec3 v1(x1, y1, c1);
  Vec3 v2(x2, y2, c2);

  if (v0.y > v1.y) {
    std::swap(v0, v1);
  }
  if (v1.y > v2.y) {
    std::swap(v1, v2);
  }
  if (v0.y > v1.y) {
    std::swap(v0, v1);
  }
  auto lerp = [](auto a, auto b, float t) {
    return a + (b - a) * t;
  };

  auto edge_function = [](int x0, int y0, int x1, int y1, int x, int y) {
    return (y - y0) * (x1 - x0) - (x - x0) * (y1 - y0);
  };
  
  int min_x = std::max(0, std::min({x0, x1, x2}));
  int max_x = std::min(scr->width - 1, std::max({x0, x1, x2}));
  int min_y = v0.y;
  int max_y = v2.y;
  
  float area = edge_function(x0, y0, x1, y1, x2, y2);

  Vec3 _c0 = rgb2vec(c0);
  Vec3 _c1 = rgb2vec(c1);
  Vec3 _c2 = rgb2vec(c2);
  for (int y = min_y; y <= max_y; ++y) {
    for (int x = min_x; x <= max_x; ++x) {
      float w0 = edge_function(x1, y1, x2, y2, x, y);
      float w1 = edge_function(x2, y2, x0, y0, x, y);
      float w2 = edge_function(x0, y0, x1, y1, x, y);
      
      if (w0 >= 0 && w1 >= 0 && w2 >= 0) {
	w0 /= area;
	w1 /= area;
	w2 /= area;
	
	Vec3 color = _c0 * w0 + _c1 * w1 + _c2 * w2;
	scr->setpixel(x, y, vec2rgb(color));
      }
    }
  }
};


void draw_bpp(Screen *scr, int w, int h, const uint8_t *mem, const int pb, const int bpp)
{
  int line[w];

  for (int y = 0; y < h; y++) {
    mem = genbpp(line, w, mem, bpp);
    drawline(scr, line, w, 0, y, pb);
  }
}

void drawline(Screen *s, int *line, int w, int sx, int sy, int pb)  {
  for (int x = 0; x < w; x++) {
    if (line[x] != -1) {
      s->setpixel(sx + x, sy, line[x] + pb);
    }
  }
}
