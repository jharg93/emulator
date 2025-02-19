#ifndef __cartridge_h__
#define __cartridge_h__

#include "bus.h"
#include "util.h"
#include "crtc.h"

struct mapper_t {
  int bankStart = -1, bankEnd = -1, bankSz = -1, nBank = 0, curpg = -1;
  uint8_t *cartmem, *page;
  
  virtual bool write(int addr, uint8_t nv);
  virtual bool read(int addr, uint8_t &nv);

  uint8_t *setbank(int pg);

  virtual int chrBase(int addr) {
    return 0;
  };
  virtual int prgBase(int addr) {
    return 0;
  };
};

/*============================================*/
/* Generic Cartridge                          */
/*============================================*/
class cart {
protected:
  Screen   *scr;
  uint8_t  *data;
  size_t    dataSz;
public:
  int bank;
  cart(const char *file) {
    data = loadrom(file, dataSz);
  };

  /* Provide read/write routines */
  virtual uint8_t read(int addr) = 0;
  virtual void write(int addr, uint8_t data) = 0;
  virtual void run() { };
};

/*============================================*/
/* NES Cartridge                              */
/*============================================*/
class nescart : public cart, crtc_t {
  struct nes_header {
    char     sig[4];
    uint8_t  prgRomSz; // size in 8kb
    uint8_t  chrRomSz; // size in 
    uint8_t  mapper1;
    uint8_t  mapper2;
    uint8_t  prgRamSz; // prg ram size
    uint8_t  flags9;   // tv system
    uint8_t  flags10;  // tv system
    uint8_t  reserved[5];
  };
  nes_header *hdr;
  uint8_t    *prgRom;
  uint8_t    *prgRam;
  uint8_t    *chrRom;
  int         prgRomSz;
  int         prgRamSz;
  int         chrRomSz;
  uint32_t    romMask;
  uint8_t     nesram[0x800] = { 0 };
  uint8_t     prgram[0x2000] = { 0 };
 public:
  nescart(const char *);
  uint8_t read(int addr);
  void write(int addr, uint8_t data);
  void run();
  void drawnt(int, int, int, int);
  void dumpnt(int);
  void drawpat(int x, int y, int base, int pid, int hm=0, int vm=0, int sprid=-1, int xm = 0);

  void gr_tick();

  void evalbg();
  void evalsprite();
  void drawpixel();
  void setvblank();
  void clrvblank();
  void drawframe();
  int nSprite, dwpat=-2, nnid=0, ppid = 0, sps=0;

  uint8_t controller[2];
  uint8_t controller_state[2];
  bus_t mb;
};

/*============================================*/
/* Atari 2600 cartridge                       */
/*============================================*/
class ataricart : public cart {
  mapper_t   *mapper;
  uint8_t    ram[0x300];
 public:
  ataricart(const char *);
  uint8_t read(int addr);
  void write(int addr, uint8_t data);
  void run();
};

void flogger(int lvl, const char *fmt, ...);
#endif

/* Mappers [2K,4K,CV,F8,F6,F4,FE,E0,3F,FA,E7,F0,UA][3F,3E,0840,MC,EF,X07,
 * 8D F8 FF : STA FFF8
 * 8D F9 FF : STA FFF9
 * 8D F9 1F : STA 1FF9
 * AD F8 1F : LDA 1FF8
 * AD F9 1F : LDA 1FF9
 */

