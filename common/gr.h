#ifndef __gr_h__
#define __gr_h__

#include <assert.h>

#ifdef _SDL
#include <SDL.h>
#undef main
#ifdef _WSDL
#include <windows.h>
#endif
#ifdef OPENGL
#include <SDL_opengl.h>
#endif
#endif

/* Standard color definitions for a pixel and palette */
typedef uint32_t color;
struct palclr {
  uint16_t r,g,b;
  palclr(uint32_t v = 0) {
    r = (v >> 16) & 0xff;
    g = (v >> 8) & 0xff;
    b = (v >> 0) & 0xff;
  };
  palclr(int _r, int _g, int _b) {
    r = _r;
    g = _g;
    b = _b;
  };
};

enum Key {
  K_NONE,

  // function keys
  K_F1, K_F2, K_F3, K_F4, K_F5, K_F6, K_F7, K_F8, K_F9, K_F10, K_F11, K_F12,  

  // arrow keys
  K_LEFT, K_UP, K_RIGHT, K_DOWN,
  K_DEL, K_ENTER, K_TAB,
  K_LSHIFT, K_RSHIFT,
  K_LCTRL, K_RCTRL,
  K_WINDOWS,
  
  // Regular keys
  K_SPACE = ' ',
  K_0 = '0',
  K_A = 'A',
  K_a = 'a',
};

#define BGR 1

class ScreenBmp {
  color *scrBuf = NULL;
public:
  int width;
  int height;
  ScreenBmp(int w, int h, int cc = 0) {
    scrBuf = new color[w * h]{ 0 };
    width = w;
    height = h;
    clear(cc);
  };
  ~ScreenBmp() {
    if (scrBuf) {
      delete [] scrBuf;
    }
  };
  void clear(int clr) {
    for (int y = 0; y < height; y++) {
      for (int x = 0; x < width; x++) {
	setpixel(x, y, clr);
      }
    }
  };
  int getpixel(int x, int y) {
    if (x >= 0 && x < width && y >= 0 && y < height)
      return scrBuf[(y * width) + x];
    return 0;
  };
  void setpixel(int x, int y, int c) {
    if (x >=0 && x < width && y >= 0 && y < height)
      scrBuf[(y * width) + x] = c;
  };
};

class Screen {
 public:
#ifdef _SDL
  SDL_Window *win;
  SDL_Renderer *renderer;
  void sdl_rect(int x, int y, int w, int h, const palclr& p);
  void setKeyState(SDL_Event&, bool);
#ifdef OPENGL
  SDL_GLContext glcontext;
#endif
#endif
  color   *scrBuf;
  palclr  defpal[256];
  int     width, height, th, tw;
  int     tx = 20, ty = 20;
  int     xs = 1, ys = 1;
  int     KeyState[256] = { 0 };
  int     clrmode;
  int     mouse_x, mouse_y, mouse_btn;
  
#ifdef _SDL
  SDL_Joystick *joy = NULL;
#endif
  int           naxes = 0;
  int           nbuttons = 0;
  int16_t      *axes = NULL;
  uint8_t      *buttons = NULL;
  
  Screen(int w, int h, int ex, int ey, int nPal=0, palclr *pal = NULL);
  color pixel(int x, int y) const {
    if (x < width && y < th) 
      return scrBuf[(y * width) + x];
    return 0;
  }
  palclr pxl2rgb(color c);

  int  getpixel(int x, int y);
  void setpixel(int x, int y, color c);
  void setpalette(int, const palclr *);
  void setpalette(int, int, int, int);
  void drawbits(int x, int y, uint8_t bits, uint8_t *map, int n, int fg, int bg, int trans);
  void scrline(int x, int y, int len, int fg);
  void scrrect(int x, int y, int w, int h, int fb);
  void draw();
  void clear(int clr = 0);
  void clrmem(int clr = 0);
  void scrtext(int x, int y, int clr, const char *str, ...);
  void drawglyph(const uint8_t *bmp, int fontHeight, int x, int y, int fg, int bg, bool (*gb)(uint8_t, int) = NULL);

  void init(int cm=0);

  bool key(int ch, bool clr = false) {
    if (ch >= 256)
      return false;
    int k = KeyState[ch];
    if (clr && k)
      KeyState[ch] = 0;
    return k;
  };
  int getmouse(int&x, int& y);
};

void waitFrame();

static inline color MKRGB(int r, int g, int b) {
  return ((r << 16) + (g << 8) + b);
}
static inline int RGB_R(int p) { return ((p >> 10) & 0x1F) * 4; }
static inline int RGB_G(int p) { return ((p >> 5) & 0x1F)  * 4; }
static inline int RGB_B(int p) { return ((p >> 0) & 0x1F)  * 4; }

static inline int BGR_B(int p) { return ((p >> 10) & 0x1F) * 8; }
static inline int BGR_G(int p) { return ((p >> 5) & 0x1F)  * 8; }
static inline int BGR_R(int p) { return ((p >> 0) & 0x1F)  * 8; }

static inline int BGRRGB(int p)
{
  int r = BGR_R(p);
  int g = BGR_G(p);
  int b = BGR_B(p);
  return MKRGB(r,g,b);
}

const uint8_t *genbpp(int *, int, const uint8_t *, const int);

void drawline(Screen *s, int *line, int w, int sx, int sy, int pb);
void draw_bpp(Screen *, int w, int h, const uint8_t *m, const int pb, const int bpp);

extern int mouse_x, mouse_y, mouse_btn;

inline int clip(const int v, const int l, const int r) {
  if (v < l)
    return l;
  else if (v >= r)
    return r;
  return v;
}

inline void xline(int *p, int w, int c) {
  for (int i = 0; i < w; i++) {
    *p++ = c;
  }
};

void draw_gradient(Screen *scr,
		   int x0, int y0, int c0,
		   int x1, int y1, int c1,
		   int x2, int y2, int c2);

/* emulate a crtc */
#include "crtc.h"

// Format is: 0bbbbbgggggrrrrr
// Want       00000000rrrrrrrrggggggggbbbbbbbb
#endif
