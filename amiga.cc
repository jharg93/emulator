#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <inttypes.h>
#include <unistd.h>
#include <string.h>
#include <assert.h>
#include "cpu.h"
#include "bus.h"
#include "util.h"
#include "gr.h"
#include "amiga.h"
#include "cpu/cpu_m68k.h"
#include "gpio.h"

int totdma;
int lcnt;
#define GREY  MKRGB(177,177,177)
#define RED   MKRGB(255,0,0)
#define GREEN MKRGB(0, 255, 0)
#define WHITE MKRGB(255,255,255)

auto movebit = [](auto val, auto testbit, auto setbit) {
  return ((val >> testbit) & 0x1) << setbit;
 };

/* Convert plane bits to color */
auto pclr = [](uint16_t *p, uint16_t mask, int mode = 0) {
  int clr = 0;
  // 2 planes, 4 colors
  if (p[0] & mask) clr |= 0x01;
  if (p[1] & mask) clr |= 0x02;
  if (mode == 0) {
    // 4 planes, 16 colors
    if (p[2] & mask) clr |= 0x04;
    if (p[3] & mask) clr |= 0x08;
  }
  else if (mode == 99) {
    // sprite 4 planes, 16 colors
    if (p[4] & mask) clr |= 0x04;
    if (p[5] & mask) clr |= 0x08;
  }
  return clr;
 };

auto pclrp = [](const uint16_t mask, const uint16_t a, const uint16_t b, const uint16_t c, const uint16_t d) {
  int clr = 0;
  
  if (a & mask) clr |= 0x1;
  if (b & mask) clr |= 0x2;
  if (c & mask) clr |= 0x4;
  if (d & mask) clr |= 0x8;
  return clr;
 };

/* de-amiga-os-120
https://pastraiser.com/
https://wandel.ca/homepage/execdis/

found sig: off:00fc00b6 00fc00b6 end:00fc323a  0 21  9 78 00fc00a8 00fc0018 extra:00fc00d2 [exec.library]
https://wandel.ca/homepage/execdis/exec_disassembly.txt
found sig: off:00fc323a 00fc323a end:00fc3254  1 21  0  5 00fc322d 00fc322d extra:00fc30ec [alert.hook]
https://wandel.ca/homepage/execdis/exec_disassembly.txt
found sig: off:00fc34cc 00fc34cc end:00fc43f4 81 21  3 28 00fc34e6 00fc34f4 extra:00fc350e [audio.device]
found sig: off:00fc450c 00fc450c end:00fc4790  1 21  8 50 00fc4526 00fc4550 extra:00fc4578 [cia.resource]
https://pastraiser.com/computers/amiga/cia.resource_disassembly.txt
 FC4578 init code
 FC4610 interrupt service routine
 FC4690 AddICRVector                (-6)
 FC46E6 RemICRVector                (-12)
 FC470A AbleICR                     (-18)
 FC4728 SetICR                      (-24)
found sig: off:00fc4794 00fc4794 end:00fc4af4  1 21  8 46 00fc47ae 00fc47bc extra:00fc47d8 [disk.resource]
https://pastraiser.com/computers/amiga/disk.resource_disassembly.txt
 FC47D8 init code
 FC4996 GetUnit                     (-18)
 FC4A0E GiveUnit                    (-24)
 FC4A62 AllocUnit                   (-6)
 FC4A6E FreeUnit                    (-12)
 FC4A74 GetUnitID                   (-30)
found sig: off:00fc4afc 00fc4afc end:00fc516c 81 21  9 6e 00fc4b48 00fc4b16 extra:00fc4b38 [expansion.library]
found sig: off:00fc5378 00fc5378 end:00fd0a3c  1 21  9 41 00fc5392 00fc53a3 extra:00fcabe2 [graphics.library]
found sig: off:00fd3f5c 00fd3f5c end:00fe0378 81 21  9  a 00fd3f9a 00fd3f30 extra:00fd3f76 [intuition.library]
found sig: off:00fe0d90 00fe0d90 end:00fe0df8  1 21  9 1f 00fe0d80 00fe0daa extra:00fe0e18 [layers.library]
found sig: off:00fe424c 00fe424c end:00fe472c 81 21  9  0 00fe4266 00fe4276 extra:00fe4294 [mathffp.library]
found sig: off:00fe4774 00fe4774 end:00fe487c  1 20  8 46 00fe478e 00fe479c extra:00fe47b8 [misc.resource]
https://pastraiser.com/computers/amiga/misc.resource_disassembly.txt
 FE47B8 init code
 FE482A AllocMiscResource           (-6)
 FE4862 FreeMiscResource            (-12)
found sig: off:00fe4880 00fe4880 end:00fe49c8  1 21  8 64 00fe489a 00fe48aa extra:00fe48c8 [potgo.resource]
found sig: off:00fe49cc 00fe49cc end:00fe4f6c 80 21  9 46 00fe4a60 00fe49e6 extra:00fe4a04 [ramlib.library]
found sig: off:00fe4fe4 00fe4fe4 end:00fe502e  1 21  8 64 00fe4ffe 00fe500e extra:00fe83c8 [keymap.resource]
https://pastraiser.com/computers/amiga/keymap.resource_disassembly.txt
found sig: off:00fe502e 00fe502e end:00fe507a  1 21  3 3c 00fe5048 00fe5058 extra:00fe53e8 [keyboard.device]
found sig: off:00fe507a 00fe507a end:00fe50c6  1 21  3 3c 00fe5094 00fe50a4 extra:00fe5854 [gameport.device]
found sig: off:00fe50c6 00fe50c6 end:00fe510e  1 21  3 28 00fe50e0 00fe50ee extra:00fe5f74 [input.device]
found sig: off:00fe510e 00fe510e end:00fe66d8  1 21  3 14 00fe5128 00fe5138 extra:00fe6b88 [console.device]
found sig: off:00fe8884 00fe8884 end:00fe88c0  1 21  0 c4 00fe889e 00fe88a4 extra:00fe88d6 [strap]
https://pastraiser.com/computers/amiga/strap_disassembly.txt
found sig: off:00fe90ec 00fe90ec end:00fe98d8  1 21  3 32 00fe9106 00fe9114 extra:00fe9174 [timer.device]
 AddTime a0 a1
 CmpTime d0 < a0 a1
 GetSysTime a9
 GetUpTime
 MicroDelay
 ReadEClock d0 < a0
 SubTime a0 a1
found sig: off:00fe98e4 00fe98e4 end:00feb3dc  1 21  3 14 00fe98fe 00fe9910 extra:00fe9b3e [trackdisk.device]
found sig: off:00feb400 00feb400 end:00ff34d8  0 21  1  0 00feb44e 00feb3e4 extra:00feb41a [workbench.task]
found sig: off:00ff425a 00ff425a end:00ff4290  0 21  9  0 00ff43c4 00ff4274 extra:00ff4290 [dos.library]
*/

/*
 * 00.0000-03FFFF 256k Chip RAM
 * 04.0000-07FFFF 256k Chip RAM
 * 08.0000-0FFFFF 512k Extended chip RAM
 * 10.0000-1FFFFF Reserved
 * 20.0000-9FFFFF 8MB Auto-config space
 * A0.0000-BEFFFF Reserved
 * BF.Dx00-BFDxFE 8520-B even
 * BF.Ex01-BFExFF 8520-A odd
 * C0.0000-DFEFFF Reserved
 *    C0.0000 - D7.FFFF Slow ram
 * DF.F000-DFFFFF Chip Registers
 * E0.0000-E7FFFF Reserved
 * E8.0000-E8FFFF Auto-config
 * E9.0000-EFFFFF Secondary auto-config
 * F0.0000-FBFFFF Reserved
 * FC.0000-FFFFFF 256k ROM
 *
 * 640x400x16
 * 320x200x4k
 *
 * 68k vector
 *  https://wiki.neogeodev.org/index.php?title=68k_vector_table
 *  000000 : 00 Initial SSP value       1111xxxx
 *  000004 : 01 Initial PC value        00FC00D2
 *  000008 : 02 bus error               0000FFFF
 *  00000c : 03 address error           002100B4 <version>
 *  000010 : 04 illegal instruction     002100C0 <version>
 *  000014 : 05 divide-by-zero          FFFFFFFF
 *  000018 : 06 CHK instruction
 *  00001C : 07 TRAPV instruction
 *  000020 : 08 Privilege violation
 */

/* Timer = .715909 MHz
 * 14.31818 MHz crystal
 *  3.579545 MHz NTSC
 * AppleII = 1.023 MHz (1/14 crystal, 3.5 pixels per cpu cycle)
 * 2600 = 1.19 MHz (1/12 crystal, 3 pixels per cpu cycle)
 * Atari 400 = 1.79 MHz (1/8 crystal, 2 pixels per cpu cycle)
 *
 * Copper - video
 *  Requests bus during odd cycles
 *  
 * Every other line is 228 clocks, 227 clocks
 */

struct rect {
  int x0, y0, x1, y1;
};

// cia.a = 0x8008 1000.0000.0000.1000
// cia.b = 0xa000 1100.0000.0000.0000
/* IRQ levels:
 * 1 0001 serial
 * 1 0002 disk block
 * 1 0004 softint
 * 2 0008 ciaa
 * 3 0010 graphics
 * 3 0020 vbi
 * 3 0040 blitter done
 * 4 0080 aud0
 * 4 0100 aud1
 * 4 0200 aud2
 * 4 0400 aud3
 * 5 0800 serial full
 * 5 1000 disk sync
 * 6 2000 ciab
 * 6 4000 master enable
 */
constexpr const char *bits_irqname[] = {
  // lvl1
  "1:serial tx empty",
  "1:disk block complete",
  "1:softint",
  // lvl2
  "2:ext int2/ciaa",
  // lvl3
  "3:graphics coproc",
  "3:vbi",
  "3:blitter done",
  // lvl4
  "4:aud2",
  "4:aud0",
  "4:aud3",
  "4:aud1",
  // lvl5
  "5:serial rx full",
  "5:disk sync pattern",
  // lvl6
  "6:ext int6/ciab",
  "master enable",
};

static const char *bits_dmaconr[] = {
  "aud0",     // 0x0001
  "aud1",     // 0x0002
  "aud2",     // 0x0004
  "aud3",     // 0x0008
  "disk",     // 0x0010
  "sprite",   // 0x0020
  "blitter",  // 0x0040
  "copper",   // 0x0080
  "bitplane", // 0x0100
  "alldma",   // 0x0200
  "bltpri",   // 0x0400
  "",         // 0x0800
  "",         // 0x1000
  "bzero",    // 0x2000
  "bbusy",    // 0x4000
  "enable",   // 0x8000
};

static const char *bits_bplcon0[] = {
  "ecsena",
  "ersy",
  "lace",
  "lpen",
  "bpu3",
  "bypass",
  "shres",
  "uhres",
  "gaud",
  "color",
  "dpf",
  "ham",
  "bpu0",
  "bpu1",
  "bpu2",
  "hires",
};

static const char *bits_adkconr[] = {
  "aud","aud","aud","aud","aud","aud","aud","aud",
  "fast","msbsync","wordsync","mfmprec","precomp0",
  "precomp1","setclr"
};

static void showbits(uint16_t bits, const char *bitnames[], const char *lbl) {
  if (!bits) {
    return;
  }
  printf("showbits %.4x %s:\n", bits, lbl);
  for (int i = 0; i < 16; i++) {
    if ((bits & (1L << i))) {
      printf(" %s\n", bitnames[i]);
    }
  }
}

void cpu_shutdown()
{
  FILE *fp;

  fp = fopen("dumpcfg.txt", "w+");
  for (auto x : visited) {
    int xy = x.first;
    fprintf(fp, "%.8x: %.4x %s\n", xy, x.second,fnmap[xy].c_str());
  }
  fclose(fp);

  for (int i = 0; optab[i].mnem; i++) {
    printf("%10d %s\n", optab[i].usage, optab[i].mnem);
  }
}

void flogger(int lvl, const char *fmt, ...)
{
  va_list ap;

  printf("----: %.4x : ", SPC);
  va_start(ap, fmt);
  vprintf(fmt, ap);
}

/* 8 32-bit registers */
static uint8_t    mem[0x10000000];

void dumpscr(int addr, int h, int w=320) {
{
  char line[700] = { 0 };
  char *ds;
  uint8_t *p = &mem[addr+(w/8)];

  for (int i = 0; i < w; i++) {
    line[i] = '0' + (i % 10);
  }
  printf("ds: %s\n", line);
  for (int y = 0; y < h; y++) {
    ds = line;
    for (int x = 0; x < w/8; x++) {
      int clr = *p++;
      for (int i = 0; i < 8; i++) {
	*ds++ = (clr & 0x80) ? '*' : ' ';
	clr <<= 1;
      }
    }
    printf("ds: %s\n", line);
  }
 }
}

struct sprite_t {
  // sprite control registers
  int pos;
  int ctl;
  // sprite position
  int x0, y0, y1;
  // number of lines remaining
  int count;
  uint16_t p[4];
  int XPOS() const {
    return x0;
  }
  int YPOS() const {
    return y0;
  }
  int HEIGHT() const {
    return y1-y0;
  }
  int WIDTH() const {
    return 16;
  };
  int PRI() const {
    return 0;
  };
} sprites[8];

// check if sprite is attached
bool attached(int n) {
  if ((n & 1) != 0 || n < 0 || n >= 8)
    return false;
  return ((sprites[n].pos == sprites[n+1].pos) &&
	  (sprites[n].ctl == sprites[n+1].ctl));
}

/* CIA8250 chip
 *   BFEx01 = CIA A
 *     0.pra   = |FIR1|FIR0|RDY |TK0 |WPRO|CHNG|LED |OVL 
 *     1.prb   = parallel port
 *     2.ddra  = direction for port A (0 = input, 1 = output)
 *     3.ddrb  = direction for port B (0 = input, 1 = output)
 *     4.talo  = Timer A lo (.715909 MHz)
 *     5.tahi  = Timer A hi 
 *     6.tblo  = Timer B lo
 *     7.tbhi  = Timer B hi
 *     8.todlo = xx Hz counter.lo
 *     9.todmd = xx Hz counter.mid
 *     a.todhi = xx Hz counter.hi
 *     c.sdr   = serial data (keyboard)
 *     d.icr   = interrupt control register  |SETCL|      |       |FLG |SP     |ALRM   |TB  |TA 
 *     e.cra   = control a                   |xxxxx|SPMODE|INTMODE|LOAD|RUNMODE|OUTMODE|PRON|START
 *     f.crb   = control b                   |ALARM|INMODE|INMODE |LOAD|RUNMODE|OUTMODE|PRON|START
 *   BFDx00 = CIA B
 *     0.pra   = |DTR |RTS |CD  |CTS |DSR |SEL |POUT|BUSY
 *     1.prb   = |MTR |SEL3|SEL2|SEL1|SEL0|SIDE|DIR |STEP
 *     2.ddra  = direction for port A
 *     3.ddrb  = direction for port B
 *     4.talo  = Timer A lo (.715909 MHz)
 *     5.tahi  = Timer A hi 
 *     6.tblo  = Timer B lo
 *     7.tbhi  = Timer B hi
 *     8.todlo = Horiz Sync counter.lo
 *     9.todmd = Horiz Sync counter mid
 *     a.todhi = Horiz Sync counter hi
 *     d.icr   = interrupt control register
 *     e.cra   = control a
 *     f.crb   = control b
 */
#include "cia.h"
struct cia8250 : public cia_t {  // CIAA   CIAB
  uint8_t Regs[16];
  enum {
    IRQ_TMRA = 0x01,
    IRQ_TMRB = 0x02,
    IRQ_ALRM = 0x04,
    IRQ_SP   = 0x08,
    IRQ_FLG  = 0x10,
    IRQ_IR   = 0x80,

    SH0 = 0x08,
    SH1 = 0x10,
    SH2 = 0x01,

    SV0 = 0x04,
    SV9 = 0x40,

    EV8 = 0x02,
    EV9 = 0x20,
    ATT = 0x80,
  };
  uint8_t& pra    = Regs[0x0];
  uint8_t& prb    = Regs[0x1];
  uint8_t& ddra   = Regs[0x2];
  uint8_t& ddrb   = Regs[0x3];

  /* Timer A: .715909 NTSC, .709379 PAL */
  uint8_t& talo   = Regs[0x4];
  uint8_t& tahi   = Regs[0x5];

  /* Timer B: .715909 NTSC, .709379 PAL */
  uint8_t& tblo   = Regs[0x6];
  uint8_t& tbhi   = Regs[0x7];

  /* CIA.A 50/60 Hz event counter (VSync)
   * CIA.B Horizontal sync counter
   */
  uint8_t& todlo  = Regs[0x8];  
  uint8_t& todmid = Regs[0x9];
  uint8_t& todhi  = Regs[0xa];
  uint8_t& xx     = Regs[0xb];

  /* CIA.A Serial (keyboard)
   * CIA.B Serial (unused)
   */
  uint8_t& sdr    = Regs[0xc];

  /* CIA.A Interrupt Control */
  uint8_t& icr    = Regs[0xd];

  /* CIA.A Control A */
  uint8_t& cra    = Regs[0xe];

  /* CIA.A Control B */
  uint8_t& crb    = Regs[0xf];

  uint8_t   todAlarm[4] = { 0 };
  
  uint32_t getTod(int n=0) {
    if (n == 0) {
      return (todhi << 16) + (todmid << 8) + todlo;
    }
    return (todAlarm[2] << 16) + (todAlarm[1] << 8) + todAlarm[0];
  }
  void setTod(uint32_t v) {
    todlo = v;
    todmid = v >> 8;
    todhi = v >> 16;
  };
  cia8250(int _id, int _pra = 0x0) {
    regs = Regs;

    id = _id;
    prb = 0xff;
  };
  void show(const char *);
  void Setreg(int n, uint8_t v);
  uint8_t Getreg(int n);
};
cia8250 cia_a('A', 0b0011'0111);
cia8250 cia_b('B');

bool cia_tod(cia8250 *c);
bool cia_tick(cia8250 *c);

struct disk {
  int idMode = 0;
  int index = 0;
  int cylinder = 0;
  int side = 0;
  uint8_t pra = 0;
  uint8_t prb = 0;
  uint8_t mfmdata[12798*82*2];
} disks[4];

int disk_sel = 0;

void showdisk() {
  printf("--- disks ---\n");
  for (int i = 0; i < 4; i++) {
    printf("%d%c: id:%d index:%d cyl:%d side:%d pra:%.2x prb:%.2x\n",
	   i, i==disk_sel ? '*' : ' ',
	   disks[i].idMode,
	   disks[i].index,
	   disks[i].cylinder,
	   disks[i].side,
	   disks[i].pra, disks[i].prb);
  }
}

static void floppyInit(int drive) {
  auto d = &disks[drive];
  d->idMode = -1;
  d->index = 0;
  d->cylinder = 0;
  d->side = 0;
  disks[disk_sel].pra &= 0b1111'1011;
  disks[disk_sel].pra &= 0b1110'1111;
}

static void encodeBlock(uint8_t *src, uint8_t *dst, int size) {
  for (int i = 0; i < size; i++) {
    dst[i] = (src[i] >> 1) & 0x55;
    dst[i+size] = (src[i] & 0x55);
  }
}

uint8_t addClockBits(uint8_t prev, uint8_t nxt) {
  nxt &= 0x55;

  uint8_t l = (nxt << 1);
  uint8_t r = (nxt >> 1) | (prev << 7);
  uint8_t cbits = (l | r) ^ 0xaa;
  return nxt | cbits;
}

static void floppyLoad(int drive, const char *name) {
  auto d = &disks[drive];
  int fd;

  uint8_t *mfm = d->mfmdata;
  if ((fd = open(name, O_RDONLY)) < 0)
    return;
  int size =(int) lseek(fd, 0, SEEK_END);
  
  //printf("FloppySize: %d\n",size);
  
  //512 bytes per sector
  int sectors = size/512;
  
  //22 sectors per track (each side 11 sectors)
  int tracks = (sectors / 22);
  
  //each track is 12798 bytes in size, multiplied by 2 because there are 2 sides
  //int mfmSize = tracks * (12798 * 2);
  
  uint8_t adf[size];
  // uint8_t mfm[mfmSize];
  
  uint8_t lowlevelSector[544]; //bytes per low level sector
  
  lseek(fd, 0, SEEK_SET);
  read(fd, adf, size);
  
  
  int count = 0;
  int s = 0;
  
  for(int track = 0;track<tracks;track++){
    for(int side=0;side<2;side++){
      for(int sector=0;sector<11;sector++){
	
	//int secCountDown = 11 - sector;
        
        
	//printf("%d: Track %d, Side: %d, Sector %d (%d)\n",s,track,side,sector,secCountDown);
        
	//Build the sector
	lowlevelSector[0] = 0x0;
	lowlevelSector[1] = 0x0;
        
	lowlevelSector[2] = 0xA1; // will be a sync mark
	lowlevelSector[3] = 0xA1; // will be a sync mark
        
	//sector info
        
	lowlevelSector[4] = 0xFF;
	lowlevelSector[5] = track << 1 | side;
	lowlevelSector[6] = sector;
	lowlevelSector[7] = 11 - sector;
        
	//Sector label
	lowlevelSector[8]  = 0x0;
	lowlevelSector[9]  = 0x0;
	lowlevelSector[10] = 0x0;
	lowlevelSector[11] = 0x0;
        
	lowlevelSector[12] = 0x0;
	lowlevelSector[13] = 0x0;
	lowlevelSector[14] = 0x0;
	lowlevelSector[15] = 0x0;
        
	lowlevelSector[16] = 0x0;
	lowlevelSector[17] = 0x0;
	lowlevelSector[18] = 0x0;
	lowlevelSector[19] = 0x0;
        
	lowlevelSector[20] = 0x0;
	lowlevelSector[21] = 0x0;
	lowlevelSector[22] = 0x0;
	lowlevelSector[23] = 0x0;
        
        
	//data
	for(int i=0;i<512;++i){
	  lowlevelSector[32+i] = adf[i+(count*512)];
          
	}
	//Encode
        
	mfm[s+0] = 0xAA;
	mfm[s+1] = 0xAA;
	mfm[s+2] = 0xAA;
	mfm[s+3] = 0xAA;
        
	mfm[s+4] = 0x44;
	mfm[s+5] = 0x89;
	mfm[s+6] = 0x44;
	mfm[s+7] = 0x89;
        
	//info
	encodeBlock(&lowlevelSector[4], &mfm[s+8], 4); // adds 8 bytes
        
	//Disklabel
	encodeBlock(&lowlevelSector[8], &mfm[s+16], 16);//adds 32 bytes
        
	//Data section
	encodeBlock(&lowlevelSector[32], &mfm[s+64], 512);
        
        
	//Header checksum
	uint8_t hcheck[4] = { 0, 0, 0, 0 };
	for(unsigned i = 8; i < 48; i += 4) {
	  hcheck[0] ^= mfm[s+i];
	  hcheck[1] ^= mfm[s+i+1];
	  hcheck[2] ^= mfm[s+i+2];
	  hcheck[3] ^= mfm[s+i+3];
	}
        
	lowlevelSector[24] = hcheck[0];
	lowlevelSector[25] = hcheck[1];
	lowlevelSector[26] = hcheck[2];
	lowlevelSector[27] = hcheck[3];
        
	//header checksum
	encodeBlock(&lowlevelSector[24], &mfm[s+48], 4); //adds 8 bytes
        
	// Data checksum
	uint8_t dcheck[4] = { 0, 0, 0, 0 };
	for(unsigned i = 64; i < 1088; i += 4) {
	  dcheck[0] ^= mfm[s+i];
	  dcheck[1] ^= mfm[s+i+1];
	  dcheck[2] ^= mfm[s+i+2];
	  dcheck[3] ^= mfm[s+i+3];
	}
        
	lowlevelSector[28] = dcheck[0];
	lowlevelSector[29] = dcheck[1];
	lowlevelSector[30] = dcheck[2];
	lowlevelSector[31] = dcheck[3];
        
	//Encode Data checksum
	encodeBlock(&lowlevelSector[28], &mfm[s+56], 4); //adds 8 bytes
	//Add clocking bits
	for(int i=8;i<1088;i++){
	  uint8_t previous = mfm[s+i-1];
	  mfm[s+i] = addClockBits(previous,mfm[s+i]);
	}
	s += 1088;    //Why not 1088, which is the size of the data we've produced
	count +=1;
      }

      //Add clocking bits to the track gap
      mfm[s]   = addClockBits(mfm[s-1],0);
      mfm[s+1] = 0xA8;
      mfm[s+2] = 0x55;
      mfm[s+3] = 0x55;
      mfm[s+4] = 0xAA;
      
      for(int i=5;i<700;i++){
	uint8_t previous = mfm[s+i-1];
	mfm[s+i] = addClockBits(previous,0);
      }
      s += 830;   //pad track to make 12798 bytes to meet the ADF-EXT spec.
      //printf("\n");
    }
  }
}

static void floppyState() {
  cia_a.pra &= 0b1100'0011;
  cia_a.pra |= disks[disk_sel].pra;
};

static void floppyInsert(int dd) {
  auto d = &disks[dd];

  printf("disk insert %d\n", dd);
  // chng
  if ((d->pra & 0x4) == 0x4) {
    d->pra &= 0b1111'1011;
    printf("disk ejected %d\n", dd);
  }
  else {
    d->pra |= 0b0000'0100;
    printf("disk inserted %d\n", dd);
  }
}

// 80 cylinders, 512 bytes per sector x 11 sectors per track
static uint16_t floppyReadWord() {
  auto d = &disks[disk_sel];
  uint32_t pos, dat;

  pos = (d->cylinder * (12798 * 2)) + (d->side * 12798) + (d->index);
  d->index += 2;
  if (d->index >= 12668) {
    d->index = 0;
    printf("index irq\n");
    cia_a.setreg(ICR, 0x90);
  }
  dat = (d->mfmdata[pos] << 8) + d->mfmdata[pos+1];
  return dat;
}

static void floppySetState() {
  static uint8_t prb;

  prb = cia_b.prb;
  switch (prb & PRB_SEL) {
  case 0x78:
    cia_a.pra &= 0b1100'0011;
    return;
  case 0x70:
    disk_sel = 0;
    break;
  case 0x68:
    disk_sel = 1;
    break;
  case 0x58:
    disk_sel = 2;
    break;
  case 0x38:
    disk_sel = 3;
    break;
  default:
    break;
 }
  disk *d = &disks[disk_sel];
  if (d->idMode > 0) {
    // id counter [rdy]
    d->pra &= 0b1101'1111;
    d->idMode--;
    cia_a.pra |= (d->pra & 0b0011'1100);
    printf("id counter... %d\n", d->idMode);
    return;
  }
  if (prb == d->prb) {
    floppyState();
    printf("disk %d no change...\n", disk_sel);
    return;
  }
  if (prb & 0x80) {
    // motor off
    printf("disk %d motor off\n", disk_sel);
    if (d->idMode == -1) {
      // start id counter
      printf("disk %d set idmode\n", disk_sel);
      d->idMode = 32;
    }
    d->pra |= 0b0010'0000;
  } else {
    // motor on
    printf("disk %d Motor on\n", disk_sel);
    if (d->pra & 0b0000'0100) {
      d->pra &= 0b1101'1111;
      printf("disk %d ready...\n", disk_sel);
    }
  }
  if ((prb & 0b0000'0001) && !(d->prb & 0b0000'0001)) {
    d->cylinder += (prb & 0b0000'0010) ? -1 : 1;
    minmax(d->cylinder, 0, 79);
    printf("disk %d step cylinder : %d\n", disk_sel, d->cylinder);
  }
  if (d->cylinder == 0) {
    printf("disk %d at track 0\n", disk_sel);
    d->pra &= 0b1110'1111;
  } else {
    printf("disk %d not track 0\n", disk_sel);
    d->pra |= 0b0001'0000;
  }
  d->side = (prb & 0b000'0100) ? 0 : 1;
  printf("disk %d side: %x\n", disk_sel, d->side);

  d->prb = prb;
  cia_a.pra &= 0b1100'0011;
  cia_a.pra |= d->pra;
}

/*=====================================================*
 * Common CPU read/write/stack routines
 *=====================================================*/
static void *buswrite(uint32_t addr, uint32_t val, int size);
static void *busread(uint32_t addr, uint32_t&val, int size);

uint8_t cpu_read8(const uint32_t addr, int mode) {
  uint32_t val = 0;
  void *ptr;

  if ((ptr = busread(addr, val, 1)) != NULL) {
    val = get8(ptr);
  }
  return val;
}

uint16_t cpu_read16(const uint32_t addr, int mode) {
  uint32_t val = 0;
  void *ptr;

  if ((ptr = busread(addr, val, 2)) != NULL) {
    val = get16be(ptr);
  }
  return val;
}

uint32_t cpu_read32(const uint32_t addr, int mode) {
  uint32_t val = 0;
  void *ptr;
  
  if ((ptr = busread(addr, val, 4)) != NULL) {
    val = get32be(ptr);
  }
  return val;
}

void cpu_write8(const uint32_t addr, const uint8_t val, int mode) {
  void *ptr;
  
  if ((ptr = buswrite(addr, val, 1)) != NULL) {
    put8(ptr, val);
  }
}

void cpu_write16(const uint32_t addr, const uint16_t val, int mode) {
  void *ptr;

  if ((ptr = buswrite(addr, val, 2)) != NULL) {
    put16be(ptr, val);
  }
}

void cpu_write32(const uint32_t addr, const uint32_t val, int mode) {
  void *ptr;

  if ((ptr = buswrite(addr, val, 4)) != NULL) {
    put32be(ptr, val);
  }
}

/*============================================*
 * Support for MUASHI
 *============================================*/
extern "C" {
#if 0
#include "m68k2.h"

unsigned int cpu_read_byte(unsigned int address) { return cpu_read8(address); }
unsigned int cpu_read_word(unsigned int address) { return cpu_read16(address); }
unsigned int cpu_read_long(unsigned int address) { return cpu_read32(address); }
unsigned int cpu_read_word_dasm(unsigned int address) { return cpu_read16(address); }
unsigned int cpu_read_long_dasm(unsigned int address) { return cpu_read32(address); }

void cpu_write_byte(unsigned int address, unsigned int value) { cpu_write8(address, value); }
void cpu_write_word(unsigned int address, unsigned int value) { cpu_write16(address, value); }
void cpu_write_long(unsigned int address, unsigned int value) { cpu_write32(address, value); }

void cpu_pulse_reset(void)
{
  printf("%.8x RESET\n", SPC);
  PC = 2;
  assert(0);
}
#endif
};

// CIA.A /FIR1 /FIR0 /RDY /TK0 /WPRO /CHNG /LED OVL
const char *ciaa_pra[] = {
  "/fir1","/fir0","/rdy","/tk0","/wpro","/chng","/led","ovl",
};
// CIA.A Parallel Port
const char *ciaa_prb[] = {
  "pp0","pp0","pp0","pp0","pp0","pp0","pp0","pp0",
};
// CIA.B /DTR /RTS /CO /CTS /DSR SEL POUT BUSY */
const char *ciab_pra[] = {
  "/dtr","/rts","/cd","/cts","/dsr","sel","pout","busy"
};
// CIA.B /MTR  /SEL3 /SEL2 /SEL1 /SEL0 /SIDE  DIR  /STEP */
const char *ciab_prb[] = {
  "/mtr","/sel3","/sel2","/sel1","/sel0","/side","dir","/step",
};

void show_gpio(uint8_t pra, uint8_t ddra, const char *names[]) {
  int n = 0;
  printf("-- gpio\n");
  for (int i = 0x80; i != 0; i >>= 1) {
    printf("%c%d %s\n",
	   ddra & i ? 'o' : 'i',
	   !!(pra & i),
	   names[n++]);
  }
};

/*=================================*
 * M68k Helper Functions
 *=================================*/
void cpu_reset(uint32_t addr)
{
  /* Read SP from vector 0, PC from vector 1 */
  /* Set Interrupt level in SR to 7  */
  /* 124 clock cycles */
  cia_a.pra |= 0x1;
  SP = cpu_read32(0);
  PC = cpu_read32(4);
  printf("PC Reset: %x\n", PC);
}

void checkpc()
{
  if (fnmap.count(PC)) {
    //printf("symbol: %s\n", fnmap[PC].c_str());
  }
  if (PC == 0xfe8d1a || PC == 0xfe8d40 || PC == 0xfed72) {
    printf("%.8x grSetAPen: %x\n", PC, D[0]);
  }
  if (PC == 0xfe8d28) {
    printf("%.8x grMove: %d, %d\n", PC, D[0], D[1]);
  }
  if (PC == 0xFE8D50) {
    printf("%.8x grFlood: %d, %d mode=%d\n", PC, D[0], D[1], D[2]);
  }
  if (PC == 0xfe8d64) {
    printf("%.8x grDraw: %d, %d\n", PC, D[0], D[1]);
  }
  if (PC == 0xfe8db2) {
    printf("%.8x grBltTemplate\n", PC);
  }
  if (PC == 0xFE8D76) {
    printf("%.8x bitmap\n", PC);
  }
  switch(PC) {
  case 0xFC118e:
    printf("@SetIntVec: %x[%s] %x\n", D[0], (D[0] < 16) ? bits_irqname[D[0]] : "xxx", A[1]);
    break;
  case 0xFC4690:
    printf("@AddICRVector : %x\n", D[0]);
    break;
  case 0xFC470A:
    printf("@AbleICR: %x\n", D[0]);
    break;
  case 0xfc4728:
    printf("@SetICR: %x\n", D[0]);
    break;
  case 0xfc1438:
    printf("@OpenLibrary: %s\n", (char *)&mem[A[1]]);
    break;
  case 0xFC1C34:
    printf("@OpenResource: %s\n", (char *)&mem[A[1]]);
    break;
  case 0xfc0666:
    printf("@OpenDevice:%s,%x,%x,%x\n", (char *)&mem[A[0]], D[0], A[1], D[1]);
    break;
  }
}

size_t loadrom(const char *name, uint32_t addr)
{
  int fd, n, off = 0;

  if ((fd = open(name, O_RDONLY)) < 0) {
    printf("missing rom:%s @ %.8x\n", name, addr);
    exit(-1);
  }
  while ((n = read(fd, &mem[addr + off], 4096)) > 0) {
    assert(addr + off < sizeof(mem));
    off += n;
  }
  close(fd);
  printf("Read %d bytes %s\n", off, name);
  return off;
}

// use ioreg16/ioreg32 to access big-endian memory registers
#define IOREG16(name, addr) ioreg16 name = ioreg16(addr);
struct ioreg16 {
  void *ptr;
  explicit ioreg16(uint32_t addr = 0) {
    ptr = &mem[addr];
  };
  operator uint16_t() const {
    return get16be(ptr);
  };
  ioreg16& operator=(uint16_t nv) {
    put16be(ptr, nv);
    return *this;
  }
};

// 32-bit register (xxxPTH/xxxPTL)
#define IOREG32(name, addr) ioreg32 name = ioreg32(addr);
struct ioreg32 {
  void *ptr;
  explicit ioreg32(uint32_t addr) {
    ptr = &mem[addr];
  };
  operator uint32_t() const {
    return get32be(ptr) & 0xFFFFFFFE;
  };
  ioreg32& operator=(uint32_t nv) {
    put32be(ptr, nv & 0xFFFFFFFE);
    return *this;
  };
};

/* Set or clear read interrupt bits */
static void xsetclr(auto & val, uint16_t bits, const char *lbl)
{
  if (bits & 0x8000) {
    /* enable */
    val = val | (bits & 0x7FFF);
  }
  else {
    /* disable */
    val = val & ~bits;
  }
}

/*=========================================*
 *
 *
 *=========================================*/
struct amiga;

struct blitter_t {
  //========================= BLITTER
  enum {
    USE_A = (1L << 11),
    USE_B = (1L << 10),
    USE_C = (1L << 9),
    USE_D = (1L << 8),
    DESC  = (1L << 1),

    EFE   = (1L << 4),
    IFE   = (1L << 3),
    FCI   = (1L << 2),
    
    LF_MASK = 0xFF,
  };

  int pen;
  uint32_t blt_count;
  uint32_t blt_width;
  uint16_t prev_a, prev_b;
  IOREG32(bltaptr, BLTAPTH);
  IOREG32(bltbptr, BLTBPTH);
  IOREG32(bltcptr, BLTCPTH);
  IOREG32(bltdptr, BLTDPTH);
  
  // BLTCON0
  //   aaaa.uuuu.llll.llll line=0
  //   aaaa.1011.llll.llll line=1
  IOREG16(bltcon0, BLTCON0);
  // BLTCON1
  //   bbbb.0000.d00e.ifd0 line=0
  //   bbbb.0000.dsos.sas1 line=1
  IOREG16(bltcon1, BLTCON1);
  IOREG16(bltamod, BLTAMOD);
  IOREG16(bltbmod, BLTBMOD);
  IOREG16(bltcmod, BLTCMOD);
  IOREG16(bltdmod, BLTDMOD);
  IOREG16(bltadat, BLTADAT);
  IOREG16(bltbdat, BLTBDAT);
  IOREG16(bltcdat, BLTCDAT);
  IOREG16(bltddat, BLTDDAT);
  IOREG16(bltafwm, BLTAFWM);
  IOREG16(bltalwm, BLTALWM);
  IOREG16(bltsizh, BLTSIZH);
  IOREG16(bltsizv, BLTSIZV);

  uint16_t  fillbit(uint16_t d);
  bool      blt_tick();
  void      blt_line();
  void      blt_setsize(uint16_t sz);
  uint16_t  blt_read(char ch, ioreg32&ptr, int delta);
  void      blt_write(char ch, ioreg32& ptr, uint16_t v, int delta);
  void      blt_mod(ioreg32& ptr, int16_t delta, int dec, const char *lbl);

  // Misc
  IOREG16(joy0dat, JOY0DAT);
  IOREG16(joy1dat, JOY1DAT);
  IOREG16(potimp, POTGOR);
  IOREG16(clxdat, CLXDAT);

  // DISK
  // CIAA.PRA
  //   +----+----+----+----+----+----+----+----+
  //   |    |    |RDY |TK0 |PROT|CHG |    |    |
  //   +----+----+----+----+----+----+----+----+
  // CIAA.PRB
  //   +----+----+----+----+----+----+----+----+
  //   |MTR |SEL3|SEL2|SEL1|SEL0|SIDE|DIR |STEP|
  //   +----+----+----+----+----+----+----+----+
  //
  // ADKCON/ADKCONR
  IOREG32(dskptr,  DSKPTH);
  IOREG16(dskdat,  DSKDAT);
  IOREG16(dsklen,  DSKLEN);
  IOREG16(dskbytr, DSKBYTR);
  IOREG16(dsksync, DSKSYNC);
  IOREG16(adkconr, ADKCONR);

  void showblt();

  amiga    *sys;
};

struct amiga : public bus_t, crtc_t, blitter_t {
  Screen *scr;

  uint32_t romsz;

  // CHIP Registers
  uint8_t *regs = &mem[0xDFF000];

  uint16_t R16(const int off) const {
    return get16be(&regs[off & 0xfff]);
  };
  uint32_t R32(const int off) const {
    uint32_t lo, hi;

    hi = R16(off + 0);
    lo = R16(off + 2);
    return (hi << 16) + lo;
  };
  void W16(const int off, uint16_t n) {
    put16be(&regs[off & 0xfff], n);
  };
  void W32(const int off, uint32_t n) {
    W16(off + 0, n >> 16);
    W16(off + 2, n);
  };

  //========================= AUDIO
  void showaud();
  ioreg32 AUDxPTR(const int n) {
    return ioreg32(AUD0LCH + (n * 0x10));
  }
  ioreg16 AUDxLEN(const int n) {
    return ioreg16(AUD0LEN + (n * 0x10));
  };
  ioreg16 AUDxPER(const int n) {
    return ioreg16(AUD0PER + (n * 0x10));
  };
  ioreg16 AUDxVOL(const int n) {
    return ioreg16(AUD0LEN + (n * 0x10));
  };
  ioreg16 AUDxDAT(const int n) {
    return ioreg16(AUD0DAT + (n * 0x10));
  }

  //========================= COLOR
  uint32_t cpal[256];
  uint32_t ARGB(const int clr) {
    int r = (clr >> 4) & 0xF0;
    int g = (clr >> 0) & 0xF0;
    int b = (clr << 4) & 0xF0;
    return MKRGB(r, g, b);
  };
  uint16_t COLORxx(const int n) const {
    // LOCT=0 T000.rrrr.gggg.bbbb
    // LOCT=1 0000.rrrr.gggg.bbbb
    return ioreg16(COLOR00 + (n * 2));
  };
  // get palettes
  void getpal(int *clr, int start, int n) {
    for (int i = 0; i < n; i++) {
      clr[i] = cpal[start + i];
    }
  };

  // collision test:
  // spr0 = 0x0001
  // spr1 = 0x0002
  // spr2 = 0x0004
  // spr3 = 0x0008
  // spr4 = 0x0010
  // spr5 = 0x0020
  // spr6 = 0x0040
  // spr7 = 0x0080
  // bp0  = 0x0100 even 0x500
  // bp1  = 0x0200 odd  0xa00
  // bp2  = 0x0400 even
  // bp3  = 0x0800 odd
  // bp4  = 0x1000
  // bp5  = 0x2000
  // bp6  = 0x3000
  // bp7  = 0x4000
  enum {
    CXPF2  = 0x500, // 0b0101'0000'0000
    CXPF1  = 0xa00, // 0b1010'0000'0000
    CXSPR0 = 0x003, // 0b0000'0000'0011
    CXSPR2 = 0x00c, // 0b0000'0000'1100
    CXSPR4 = 0x030, // 0b0000'0011'0000
    CXSPR6 = 0x0c0, // 0b0000'1100'0000

    // spr0.spr2 : (c & CXSPR0) && (c & CXSPR2)
  };
  int collide[640];
  int bgline[640];
  void sethblank(bool);

  //========================= COPPER
  IOREG32(cop1ptr, COP1LCH);
  IOREG32(cop2ptr, COP2LCH);
  uint32_t runcop(uint32_t pc, int, int);
  uint32_t coppc;
  int copstate;

  //========================= BITPLANE
  // +----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+
  // |HIRS|BPU2|BPU1|BPU0|HAM |DBPF|COLR|GAUD| xx | xx | xx | xx |LPEN|LACE|ERSY| xx | BPLCON0
  // +----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+
  // | xx | xx | xx | xx | xx | xx | xx | xx |P2H3|P2H2|P2H1|P2H0|P1H3|P1H2|P1H1|P1H0| BPLCON1
  // +----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+
  // | xx | xx | xx | xx | xx | xx | xx | xx | xx |P2PR|P2P2|P2P1|P2P0|P1P2|P1P1|P1P0| BPLCON2
  // +----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+
  // | BANKx        |   PF2OFx     |LOCT| xx | SPRRESx |BRDB|BRDT| xx |ZDCK|BRDS|EBLK| BPLCON3
  // +----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+
  // BPLxPTL:
  // BPLxPTH:
  // BPLxMOD:
  // BPLxDAT:
  void showbp();
  IOREG16(bpl1mod, BPL1MOD);
  IOREG16(bpl2mod, BPL2MOD);
  IOREG16(bplcon0, BPLCON0);
  IOREG16(bplcon1, BPLCON1);
  IOREG16(bplcon2, BPLCON2);

  // return ioreg32 for bitplane pointer & data
  ioreg32 BPLxPTR(const int n) {
    return ioreg32(BPL1PTH + (n * 4));
  };
  ioreg16 BPLxDAT(const int n) {
    return ioreg16(BPL1DAT + (n * 2));
  };

  // return number of planes
  int bpu;
  int wdma;

  void renderbg(int y, int count, int dmapos);
  bool vid_tick();

  /* DMA Handlers */
  void dodma(int y);
  int  bpdma(int n);
  int  sprdma(int n);
  int  auddma(int n);
  int  diskdma();
  
  // ntsc = 320x200
  //  pal = 320x256
  // DIWSTRT/DIWSTOP in pixels
  // +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  // |  vpos=0x2c = 44               |  hpos=0x81 = 129              | DIWSTRT
  // +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  // |  vpos=0xf4 = 244 (200)        |  hpos=0xc1+256 = 449 (320)    | NTSC DIWSTOP
  // +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  // |  vpos=0x2c+256 = 300 (256)    |  hpos=0xc1+256 = 449 (320)    | PAL DIWSTOP
  // +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  //
  // +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  // |                               |  hpos=0x38                    | ddfstrt
  // |                               |  lores=(DIWSTRT/2)-8.5        |
  // |                               |  hires=(DIWSTRT/2)-4.5        |
  // +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  // |                               |  hpos=0xd0                    | ddfstop
  // |                               |  lores=(DDFSTRT+8*(dma-1)     | 0x38 + 8*(20-1) = $d0
  // |                               |  hires=(DDFSTRT+4*(dma-2)     | 0x38 + 4*(40-2) = $d0
  // +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  rect screen;
  IOREG16(diwstrt, DIWSTRT);
  IOREG16(diwstop, DIWSTOP);
  IOREG16(ddfstrt, DDFSTRT); // DMA start
  IOREG16(ddfstop, DDFSTOP); // DMA stop
  IOREG16(vposr,   VPOSR);
  IOREG16(vhposr,  VHPOSR);
  IOREG16(intenar, INTENAR); // interrupt enabled
  IOREG16(intreqr, INTREQR); // interrupt request
  IOREG16(dmaconr, DMACONR); // dma enabled

  //========================= SPRITE
  // 8 sprites per scanline
  // 16-pixels wide
  // 2 planes (3 colors, 0 is transparent)
  // attach bit makes 4 planes (15 colors, 0 is transparent)
  //
  // groups are 0+1,2+3,4+5,6+7
  //   0+1 = colors 16..19 16 transparent
  //   2+3 = colors 20..23 20 transparent
  //   4+5 = colors 24..27 24 transparent
  //   6+7 = colors 28..31 28 transparent
  // attached sprites:
  //   16-31, 16 transparent
  //
  // SPRxPOS: |sv7|sv6|sv5|sv4|sv3|sv2|sv1|sv0|shA|sh7|sh6|sh5|sh4|sh3|sh2|sh1|
  // SPRxCTL: |ev7|ev6|ev5|ev4|ev3|ev2|ev1|ev0|ATT|---|---|---|---|sv8|ev8|sh0|
  void showspr();
  ioreg32 SPRxPTR(int n)  {
    return ioreg32(SPR0PTH + (n * 4));
  };
  ioreg16 SPRxPOS(int n)  {
    return ioreg16(SPR0POS + (n * 8));
  };
  ioreg16 SPRxCTL(int n)  {
    return ioreg16(SPR0CTL + (n * 8));
  };
  ioreg16 SPRxDATA(int n) {
    return ioreg16(SPR0DATA + (n * 8));
  }
  ioreg16 SPRxDATB(int n) {
    return ioreg16(SPR0DATB + (n * 8));
  }
  void renderspr(int y, int n, int mode, uint32_t *palette);

  /* Generic */
  void init(const char *);
  bool tick();
  
  void drawframe();
  void check_irq();

  uint32_t chip_read(uint32_t addr, int n);
  void     chip_write(uint32_t addr, uint32_t val, int n = 2);

  bool dma_enabled(int);

  /* disk info */
  int disk = -1;
  int rdy = -1;

  amiga() {
    blitter_t::sys = this;
  };
  int check_collide();

  void xx_request_irq(uint16_t bits, const char *who);
  int rxdma(ioreg16 dat, ioreg32 ptr, int dmamask, const char *lbl);

  uint32_t rtc_read(uint32_t addr) {
    printf("RTC read: %.8x\n", addr);
    return 0xaa;
  }
  void rtc_write(uint32_t addr, uint32_t val) {
    printf("RTC write: %.8x = %.8x\n", addr, val);
  }
};

amiga thegame;

/* Request IRQ */
void amiga::xx_request_irq(uint16_t bits, const char *who)
{
  if ((intenar & IRQ_INTEN) == 0)
    return;
  printf(">> request irq: %.4x [%.4x %.4x] [%s]\n", bits,
	 (int)intenar, (int)intreqr, who);
  chip_write(INTREQ, IRQ_SET | bits, 2);
}

/* Convert to BCD for RTC */
int bcd(int n)
{
  return ((n / 10) * 16) + (n % 10);
}

void cia_gettime(cia8250 *c)
{
  struct tm *rtc;
  time_t now;
  
  now = time(NULL);
  rtc = localtime(&now);
}

static const char *ciareg[] = {
  "pra",  "prb",  "ddra", "ddrb",
  "talo", "tahi", "tblo", "tbhi",
  "tod0", "tod1", "tod2", "irqsts",
  "sdr",  "irqen","cra",  "crb"
};

void cia8250::show(const char *pfx) {
  printf("%-6s cia%c: pa:%.2x pb:%.2x da:%.2x db:%.2x ta:%.4x/%.8x tb:%.4x/%.8x tod:%.6x alrm:%.6x sdr:%.2x icr:%.2x[%.2x/%.2x] cra:%.2x crb:%.2x\n",
	 pfx, id, pra, prb, ddra, ddrb, tmra_freq, tmra.count, tmrb_freq,
	 tmrb.count, getTod(), getTod(1), sdr, icr, cia_irq.en, cia_irq.sts, cra, crb);
};

const char *bs(const char *fs, int bit) {
  static char str[10];

  memset(str, 0, sizeof(str));
  for (int i = 0; i < 8; i++) {
    str[i] = (bit & 0x80) ? fs[i] : '-';
    bit <<= 1;
  }
  return str;
}

/*
 * N=CRA && D[4]:
 *   ta_counter <= tmra_freq
 * N=CRA:
 *   ta_running=D[0]
 *   ta_oneshot=D[3]
 * N=TAHI && (!ta_running || ta_oneshot)
 *   ta_counter <= (D, tmra_freq.lo)
 *   if ta_oneshot: ta_running=1
 * if ta_running:
 *   if ta_counter == 0:
 *     ta_counter <= tmra_freq
 *   else
 *     ta_counter <= ta_counter - 1
 * if ta_underflowing && ta_oneshot
 *   ta_running=1
 *======================================
 * N=CRB && D[4]:
 *   tb_counter <= tmrb_freq
 * N=CRB:
 *   tb_oneshot=D[3]
 *   tb_running=D[0]
 *   tb_count_ta_underflow=D[6]
 *   tod_set_alarm=D[7]
 * N=TBHI && (!tb_running || tb_oneshot)
 *   tb_counter <= (D, tmrb_freq.lo)
 *   if tb_oneshot: tb_running=1
 * N=TODHI
 *   tod_latch <= tod_counter
 *   tod_latched=1
 * N=TODLO:
 *   tod_latched=1
 *   if tod_set_alarm:
 *     alarm_set=1
 * if tb_running && (!tb_count_ta_underflow || ta_underflowing):
 *   if ta_counter == 0:
 *     tb_counter <= tmrb_freq
 *   else
 *     tb_counter <= tb_counter - 1
 * if tb_underflowing && tb_oneshot
 *   tb_running=1
 *======================================
 */
void cia8250::Setreg(int n, uint8_t val)
{
  //printf("%.8x cia%c: write    %s = %.2x\n",  SPC, id, ciareg[n], val);
  cia_t::setreg(n, val);
  if (n == PRA) {
    pra = val & 0x3f;
  }
  if (n == PRB) {
    prb = val;
    floppySetState();
  }
}

uint8_t cia8250::Getreg(int n)
{
  uint8_t v;

  v = cia_t::getreg(n);
  if (n == PRA) {
    floppyState();
    v = pra;
  }
  //printf("%.8x cia%c: read     %s = %.2x\n", SSPC, id, ciareg[n], v);
  return v;
}

void blitter_t::showblt() {
  int bc0 = bltcon0;
  int bc1 = bltcon1;
  
  printf("----- blitter ---- dma:%.4x irq:%.4x\n",
	 (int)sys->dmaconr & (DMA_BLTEN|DMA_BLTPRI|DMA_BZERO|DMA_BBUSY),
	 sys->intenar & IRQ_BLIT);
  if (bltcon0 & 1) {
  } else {
    printf("con0:%.4x ashift:%x a:%x b:%x c:%x d:%x func:%.2x\n",
	   bc0, (bc0 >> 12),
	   !!(bc0 & 0x800),!!(bc0 & 0x400),!!(bc0 & 0x200),!!(bc0 & 0x100),
	   bc0 & 0xFF);
    printf("con1:%.4x bshift:%x efe:%x ife:%x fci:%x desc:%x\n",
	   bc1, bc1 >> 12, !!(bc1 & 0x10), !!(bc1 & 0x08), !!(bc1 & 0x04));
  }
  printf("bltaptr: %.8x mod:%.4x dat:%.4x fwm:%.4x lwm:%.4x\n",
	 (int)bltaptr, (int)bltamod, (int)bltadat,
	 (int)bltafwm, (int)bltalwm);
  printf("bltbptr: %.8x mod:%.4x dat:%.4x\n", (int)bltbptr, (int)bltbmod, (int)bltbdat);
  printf("bltcptr: %.8x mod:%.4x dat:%.4x\n", (int)bltcptr, (int)bltcmod, (int)bltcdat);
  printf("bltdptr: %.8x mod:%.4x dat:%.4x\n", (int)bltdptr, (int)bltdmod, 0x0);
}

void amiga::showaud() {
  printf("----- audio ---- dma:%x irq:%x\n",
	 (dmaconr >> 0) & 0xF,
	 (intenar >> 7) & 0xF);
  hexdump(&mem[AUD0LCH], 0x100);
  for (int i = 0; i < 4; i++) {
    uint32_t ptr = AUDxPTR(i);
    uint16_t len = AUDxLEN(i);
    uint16_t per = AUDxPER(i);
    uint16_t dat = AUDxDAT(i);
    printf("%d: audptr:%.8x len:%.4x period:%.4x dat:%.4x\n",
	   i, ptr, len, per, dat);
  }
}

void amiga::showbp() {
  printf("----- bitplane ---- dma:%x bpu:%x\n",
	 dma_enabled(DMA_BPEN), bpu);
  for (int i = 0; i < 8; i++) {
    uint32_t ptr = BPLxPTR(i);
    uint16_t dat = BPLxDAT(i);
    uint16_t mod = (i & 1) ? bpl2mod : bpl1mod;
    printf("%d: bplptr: %.8x mod:%.4x dat:%.4x\n",
	   i, ptr, mod, dat);
  }
}

void amiga::showspr() {
  printf("----- sprites ---- dma:%x\n",
	 dma_enabled(DMA_SPREN));
  for (int i = 0; i < 8; i++) {
    uint32_t ptr = SPRxPTR(i);
    uint16_t pos = SPRxPOS(i);
    uint32_t ctl = SPRxCTL(i);
    uint32_t dta = SPRxDATA(i);
    uint32_t dtb = SPRxDATB(i);

    printf("%d: sprptr:%.8x pos:%.4x ctl:%.4x data:%.4x datb:%.4x\n",
	   i, ptr, pos, ctl, dta, dtb);
  }
};

// advance pointer by mod
void blitter_t::blt_mod(ioreg32& ptr, int16_t delta, int dec, const char *lbl)
{
  if (dec < 0) {
    delta = -delta;
  }
  if (ptr && delta) {
    printf("blt_mod:%.8x %.8x %s\n", (int)ptr, delta, lbl);
  }
  ptr = ptr + delta;
}

/* blitter write memmory and advance pointer */
void blitter_t::blt_write(char ch, ioreg32& addr, uint16_t v, int delta)
{
  //printf("blt%c_write: %.8x = %.4x [%.8x]\n", ch, (int)addr, v, delta);
  put16be(&mem[addr & ~1], v);
  addr = addr + delta;
}

/* blitter read memmory and advance pointer */
uint16_t blitter_t::blt_read(char ch, ioreg32& addr, int delta)
{
  uint16_t v = get16be(&mem[addr & ~1]);
  //printf("blt%c_read:  %.8x = %.4x [%.8x]\n", ch, (int)addr, v, delta);
  addr = addr + delta;
  return v;
}

void blitter_t::blt_setsize(uint16_t sz)
{
  lcnt = 0;
  blt_width = (sz & 0x3F);
  blt_count = blt_width * (sz >> 6);

  bltsizv = (sz >> 6);
  bltsizh = (sz & 63);
  if (bltsizv == 0)
    bltsizv = 1024;
  if (bltsizh == 0)
    bltsizh = 64;

  prev_a = 0;
  prev_b = 0;
  printf("--------\n");
  printf("setsize: %.4x %.4x x %.4x con0:%.4x con1:%.4x dma:%s %s\n",
	 sz, blt_width, blt_count, (int)bltcon0, (int)bltcon1,
	 (int)sys->dmaconr & DMA_BLTEN ? "en" : "di",
	 (int)bltcon1 & 1 ? "line" : "blit");
  if (bltcon1 & DESC) {
    printf("desc\n");
  }
  showblt();
  printf("--------\n");

  // enable blitter
  if (!blt_count) {
    return;
  }
  pen = (bltcon1 & FCI) != 0;
  // set blitter busy
#if 1
  while(blt_count) {
    blt_tick();
  }
#endif
}

/* Calculate blitter functions
 */
using fbt = uint32_t;
using fastbltfn = fbt (*)(const fbt, const fbt, const fbt);

constexpr fbt fb(uint8_t fn, fbt a, fbt b, fbt c)
{
  uint32_t d = 0;

  if (fn & 0x80)
    d |= a & b & c;
  if (fn & 0x40)
    d |= a & b & ~c;
  if (fn & 0x20)
    d |= a & ~b & c;
  if (fn & 0x10)
    d |= a & ~b & ~c;
  if (fn & 0x08)
    d |= ~a & b & c;
  if (fn & 0x04)
    d |= ~a & b & ~c;
  if (fn & 0x02)
    d |= ~a & ~b & c;
  if (fn & 0x01)
    d |= ~a & ~b & ~c;
  return d;
}

template <int op>
fbt FB(const fbt a, const fbt b, const fbt c) {
  return fb(op, a, b, c);
}

constexpr fastbltfn fbtab[] = {
  FB<0x00>, FB<0x01>, FB<0x02>, FB<0x03>, FB<0x04>, FB<0x05>, FB<0x06>, FB<0x07>, 
  FB<0x08>, FB<0x09>, FB<0x0A>, FB<0x0B>, FB<0x0C>, FB<0x0D>, FB<0x0E>, FB<0x0F>, 
  FB<0x10>, FB<0x11>, FB<0x12>, FB<0x13>, FB<0x14>, FB<0x15>, FB<0x16>, FB<0x17>, 
  FB<0x18>, FB<0x19>, FB<0x1A>, FB<0x1B>, FB<0x1C>, FB<0x1D>, FB<0x1E>, FB<0x1F>, 
  FB<0x20>, FB<0x21>, FB<0x22>, FB<0x23>, FB<0x24>, FB<0x25>, FB<0x26>, FB<0x27>, 
  FB<0x28>, FB<0x29>, FB<0x2A>, FB<0x2B>, FB<0x2C>, FB<0x2D>, FB<0x2E>, FB<0x2F>, 
  FB<0x30>, FB<0x31>, FB<0x32>, FB<0x33>, FB<0x34>, FB<0x35>, FB<0x36>, FB<0x37>, 
  FB<0x38>, FB<0x39>, FB<0x3A>, FB<0x3B>, FB<0x3C>, FB<0x3D>, FB<0x3E>, FB<0x3F>, 
  FB<0x40>, FB<0x41>, FB<0x42>, FB<0x43>, FB<0x44>, FB<0x45>, FB<0x46>, FB<0x47>, 
  FB<0x48>, FB<0x49>, FB<0x4A>, FB<0x4B>, FB<0x4C>, FB<0x4D>, FB<0x4E>, FB<0x4F>, 
  FB<0x50>, FB<0x51>, FB<0x52>, FB<0x53>, FB<0x54>, FB<0x55>, FB<0x56>, FB<0x57>, 
  FB<0x58>, FB<0x59>, FB<0x5A>, FB<0x5B>, FB<0x5C>, FB<0x5D>, FB<0x5E>, FB<0x5F>, 
  FB<0x60>, FB<0x61>, FB<0x62>, FB<0x63>, FB<0x64>, FB<0x65>, FB<0x66>, FB<0x67>, 
  FB<0x68>, FB<0x69>, FB<0x6A>, FB<0x6B>, FB<0x6C>, FB<0x6D>, FB<0x6E>, FB<0x6F>, 
  FB<0x70>, FB<0x71>, FB<0x72>, FB<0x73>, FB<0x74>, FB<0x75>, FB<0x76>, FB<0x77>, 
  FB<0x78>, FB<0x79>, FB<0x7A>, FB<0x7B>, FB<0x7C>, FB<0x7D>, FB<0x7E>, FB<0x7F>, 
  FB<0x80>, FB<0x81>, FB<0x82>, FB<0x83>, FB<0x84>, FB<0x85>, FB<0x86>, FB<0x87>, 
  FB<0x88>, FB<0x89>, FB<0x8A>, FB<0x8B>, FB<0x8C>, FB<0x8D>, FB<0x8E>, FB<0x8F>, 
  FB<0x90>, FB<0x91>, FB<0x92>, FB<0x93>, FB<0x94>, FB<0x95>, FB<0x96>, FB<0x97>, 
  FB<0x98>, FB<0x99>, FB<0x9A>, FB<0x9B>, FB<0x9C>, FB<0x9D>, FB<0x9E>, FB<0x9F>, 
  FB<0xA0>, FB<0xA1>, FB<0xA2>, FB<0xA3>, FB<0xA4>, FB<0xA5>, FB<0xA6>, FB<0xA7>, 
  FB<0xA8>, FB<0xA9>, FB<0xAA>, FB<0xAB>, FB<0xAC>, FB<0xAD>, FB<0xAE>, FB<0xAF>, 
  FB<0xB0>, FB<0xB1>, FB<0xB2>, FB<0xB3>, FB<0xB4>, FB<0xB5>, FB<0xB6>, FB<0xB7>, 
  FB<0xB8>, FB<0xB9>, FB<0xBA>, FB<0xBB>, FB<0xBC>, FB<0xBD>, FB<0xBE>, FB<0xBF>, 
  FB<0xC0>, FB<0xC1>, FB<0xC2>, FB<0xC3>, FB<0xC4>, FB<0xC5>, FB<0xC6>, FB<0xC7>, 
  FB<0xC8>, FB<0xC9>, FB<0xCA>, FB<0xCB>, FB<0xCC>, FB<0xCD>, FB<0xCE>, FB<0xCF>, 
  FB<0xD0>, FB<0xD1>, FB<0xD2>, FB<0xD3>, FB<0xD4>, FB<0xD5>, FB<0xD6>, FB<0xD7>, 
  FB<0xD8>, FB<0xD9>, FB<0xDA>, FB<0xDB>, FB<0xDC>, FB<0xDD>, FB<0xDE>, FB<0xDF>, 
  FB<0xE0>, FB<0xE1>, FB<0xE2>, FB<0xE3>, FB<0xE4>, FB<0xE5>, FB<0xE6>, FB<0xE7>, 
  FB<0xE8>, FB<0xE9>, FB<0xEA>, FB<0xEB>, FB<0xEC>, FB<0xED>, FB<0xEE>, FB<0xEF>, 
  FB<0xF0>, FB<0xF1>, FB<0xF2>, FB<0xF3>, FB<0xF4>, FB<0xF5>, FB<0xF6>, FB<0xF7>, 
  FB<0xF8>, FB<0xF9>, FB<0xFA>, FB<0xFB>, FB<0xFC>, FB<0xFD>, FB<0xFE>, FB<0xFF>,
};

constexpr uint32_t bltfn(uint8_t fn, uint32_t a, uint32_t b, uint32_t c)
{
#if 0
  return fb(fn, a, b, c);
#else
  return fbtab[fn](a,b,c);
#endif
}

uint16_t blitter_t::fillbit(uint16_t d) {
  // ignore if fill mode not used
  if ((bltcon1 & (IFE|EFE)) == 0) {
    return d;
  }
  // fill mode uses right to left
  for (uint16_t mask = 0x0001; mask <= 0x8000; mask <<= 1) {
    if (d & mask) {
      if (pen && (bltcon1 & EFE)) {
	d &= ~mask;
      }
      pen = !pen;
    }
    if (pen) {
      d |= mask;
    }
  }
  return d;
};

/* Line draw mode:
 * BLTCON1..432
 *   SUD(minor=up/down)
 *   SUL(minor=up/left)
 *   AUL(major=up/left)
 * 000 2 down,right
 * 001 7 up,right
 * 010 3 down,left
 * 011 6 up,left
 * 100 1 right,down
 * 101 4 left,down
 * 110 8 right,up
 * 111 5 left,up
 *
 */
/* bltaptr = 4*dmin - 2*dmaj (if negative, BLTCON1.SIGN)
 * bltamod = 4*(dmin - dmaj)
 * bltbmod = 4*dmin
 * bltcmod = width in bytes
 * bltdmod = width in bytes
 * bltadat = 0x8000
 * bltbdat = pattern or 0xffff
 * bshift  = bit to start pattern
 * bltcpt, bltcmod
 * bltdpt, bltdmod
 * bltheight = lenth of line (dx+1), width=2
 */
void blitter_t::blt_line()
{
  int cmod = 0;
  int ashift = (bltcon0 >> 12) & 0xF;
  int sign = !!(bltcon1 & BLTCON1_SIGN);
  int o = (bltcon1 >> 2) & 7;
  uint16_t a = (bltadat & bltafwm) >> ashift;
  uint16_t b = (bltbdat & 1) ? 0xffff : 0;
  int16_t D = bltaptr;

  printf("line: o:%d D:%.8x amd:%.8x bmd:%.8x bdt:%.4x con0:%.4x con1:%.4x shift:%d\n",
	 o, D, (int16_t)bltamod, (int16_t)bltbmod, (int)bltbdat, (int)bltcon0, (int)bltcon1, ashift);
  
  auto incx = [&](int c, int l) {
    ashift += c ? -1 : 1;
    if (ashift < 0) {
      ashift = 15;
      cmod -= 2;
    }
    else if (ashift > 15) {
      ashift = 0;
      cmod += 2;
    }
  };
  auto incy = [&](int c) {
    if (c) {
      cmod -= bltcmod;
    }
    else {
      cmod += bltcmod;
    }
  };

  /* sign sud sul aul  incx          incy
   *    0   0   0   0  0             ----,0
   *    0   0   0   1  0             ----,1
   *    0   0   1   0  1             ----,0
   *    0   0   1   1  1             ----,1
   *    0   1   0   0  ----,0        0
   *    0   1   0   1  ----,1        0
   *    0   1   1   0  ----,0        1
   *    0   1   1   0  ----,1        1
   */
  const bool sud = !!(bltcon1 & BLTCON1_SUD);
  const bool sul = !!(bltcon1 & BLTCON1_SUL);
  const bool aul = !!(bltcon1 & BLTCON1_AUL);
  if (!sign) {
    sud ? incy(sul) : incx(sul, 1);
  }
  sud ? incx(aul, 2) : incy(aul);

  // read input bitplane
  bltcdat = blt_read('c', bltcptr, cmod);
  bltddat = bltfn(bltcon0 & 0xff, a, b, bltcdat);

  // write output data
  blt_write('d', bltdptr, bltddat, cmod);

  // write new shift back to bltcon0
  bltcon0 = (bltcon0 & 0x0FFF) + (ashift << 12);

  // rotate pattern
  bltbdat = (bltbdat << 1) | (bltbdat >> 15);

  // increment
  if (D < 0) {
    D += (int16_t)(bltbmod & 0xfffe);
  }
  else {
    D += (int16_t)(bltamod & 0xfffe);
  }
  bltcon1 = (bltcon1 & ~BLTCON1_SIGN);
  if (D < 0) {
    bltcon1 = (bltcon1 | BLTCON1_SIGN);
  }
  bltaptr = D;
  blt_count -= 2;
}

auto lshift = [](uint16_t& hi, uint16_t lo, int s) {
  uint16_t res = (hi >> (16-s)) | (lo << s);
  hi = lo;
  return res;
 };
auto rshift = [](uint16_t& hi, uint16_t lo, int s) {
  uint16_t res = (hi << (16-s)) | (lo >> s);
  hi = lo;
  return res;
 };
// bltamod = 4*(dy - dx)
// bltbmod = 4*dy
// bltcon0.ash = (x1 mod 15)
// bltcon1.bsh = bit of texture
bool blitter_t::blt_tick()
{
  uint16_t a, b, c;
  int ashift, bshift;
  int delta = 2;

  // don't run if no blitter dma
  if (blt_count == 0)
    return false;
  if (bltcon1 & 1) {
    // line mode
    blt_line();
    return false;
  }
  ashift = (bltcon0 >> 12) & 0xF;
  bshift = (bltcon1 >> 12) & 0xF;
  if (bltcon1 & DESC) {
    /* Descending address */
    delta = -delta;
  }

  /* Read in source values */
  if (bltcon0 & USE_A)
    bltadat = blt_read('a', bltaptr, delta);
  if (bltcon0 & USE_B)
    bltbdat = blt_read('b', bltbptr, delta);
  if (bltcon0 & USE_C)
    bltcdat = blt_read('c', bltcptr, delta);

  a = bltadat;
  b = bltbdat;
  c = bltcdat;
  if (lcnt == 0)
    a &= bltafwm;
  if (lcnt == (blt_width-1))
    a &= bltalwm;
  //auto tmpa = a;
  if (bltcon1 & DESC) {
    a = lshift(prev_a, a, ashift);
    b = lshift(prev_b, b, bshift);
  } else {
    a = rshift(prev_a, a, ashift);
    b = rshift(prev_b, b, bshift);
  }
  //prev_a = tmpa;
  //prev_b = bltbdat;
  
  /* Calculate result and write out */
  bltddat = bltfn(bltcon0, a, b, c);
  if (bltcon0 & USE_D) {
    blt_write('d', bltdptr, bltddat, delta);
  }
  
  /* If reached end of width, advance mod pointers */
  if (lcnt++ == (blt_width-1)) {
    blt_mod(bltaptr, bltamod, delta, "aptr");
    blt_mod(bltbptr, bltbmod, delta, "bptr");
    blt_mod(bltcptr, bltcmod, delta, "cptr");
    blt_mod(bltdptr, bltdmod, delta, "dptr");
    prev_a = 0;
    prev_b = 0;
    lcnt = 0;
  }
  if (--blt_count == 0) {
    //pen = (bltcon1 & FCI) != 0;
    // signal blitter done
    //xx_request_irq(IRQ_BLIT, "BLItter");
    printf("--blt done\n");
    sys->dmaconr = sys->dmaconr & 0xBFFF;
    sys->chip_write(INTREQ, 0x8040, 2);
  }
  return true;
}

/* Video screen:
 *  VPOSR  L______._____A98
 *  VPOSW
 *  VHPOSR 7654321.87654321
 *  VHPOSW
 *
 * NTSC: htotal = 226 (227.5 clocks per line)
 * E3 = 227
 *
 * NTSC: 262
 *  PAL: 312
*/
#define HORIZ_COUNT 0xe4
#define VERT_COUNT  0x106

/* Copper format:
 *  MOVE: 0000.000r.rrrr.rrr0   dddd.dddd.dddd.dddd
 *  WAIT: vvvv.vvvv.hhhh.hhh1   BVVV.VVVV.HHHH.HHH0
 *  SKIP: ____.____.____.___1   ____.____.____.___1
 */
void showcop(uint32_t pc, const char *lbl);

enum DmaCycle {
  Even,Odd,Dram,Disk,

  // low nybble is channel
  Aud0=0x10,
  Aud1=0x11,
  Aud2=0x12,
  Aud3=0x13,

  // low nybble sssD : s=spritenum, D=sprpos/sprctl or sprdata/sprdatb
  Spr0a=0x20,
  Spr1a=0x22,
  Spr2a=0x24,
  Spr3a=0x26,
  Spr4a=0x28,
  Spr5a=0x2a,
  Spr6a=0x2c,
  Spr7a=0x2e,

  Spr0b=0x21,
  Spr1b=0x23,
  Spr2b=0x25,
  Spr3b=0x27,
  Spr4b=0x29,
  Spr5b=0x2b,
  Spr6b=0x2d,
  Spr7b=0x2f,

  // low nybble
  // D = even/odd cycle
  // nnnD
  // 0000 1->0
  // 0011 2->3
  // 0100 3->4
  // 0110 4->6
  // 1000 5->8
  // 1010 6->a
  bpl1=0x30,  // even
  bpl2=0x83,  // odd
  bpl3=0x34,  // even
  bpl4=0x36,  // even
  bpl5=0x38,  // even
  bpl6=0x3a,  // even
  bpl1h=0x40, // even

  // copper is even cycle
  // blitter is odd cycle
};

static int lodma[] = {
  // 00
  Even, Dram, Even, Dram, Even, Dram, Even, Disk,
  Even, Disk, Even, Disk, Even, Aud0, Even, Aud1,
  // 10
  Even, Aud2, Even, Aud3, Even, Spr0a,Even, Spr0b,
  Even, Spr1a,Even, Spr1b,Even, Spr2a,Even, Spr2b,
  // 20
  Even, Spr3a,Even, Spr3b,Even, Spr4a,Even, Spr4b,
  Even, Spr5a,Even, Spr5b,Even, Spr6a,Even, Spr6b,
  // 30
  Even, Spr7a,Even, Spr7b,Even, Odd,  Even, Odd,
  // 38 : 2 words
  Even, bpl4, bpl6, bpl2, Even, bpl3, bpl5, bpl1,
  Even, bpl4, bpl6, bpl2, Even, bpl3, bpl5, bpl1,
  // 40 : 2 words
  Even, bpl4, bpl6, bpl2, Even, bpl3, bpl5, bpl1,
  Even, bpl4, bpl6, bpl2, Even, bpl3, bpl5, bpl1,
  // 58 : 2 words
  Even, bpl4, bpl6, bpl2, Even, bpl3, bpl5, bpl1,
  Even, bpl4, bpl6, bpl2, Even, bpl3, bpl5, bpl1,
  // 68 : 2 words
  Even, bpl4, bpl6, bpl2, Even, bpl3, bpl5, bpl1,
  Even, bpl4, bpl6, bpl2, Even, bpl3, bpl5, bpl1,
  // 78 : 2 words
  Even, bpl4, bpl6, bpl2, Even, bpl3, bpl5, bpl1,
  Even, bpl4, bpl6, bpl2, Even, bpl3, bpl5, bpl1,
  // 88 : 2 words
  Even, bpl4, bpl6, bpl2, Even, bpl3, bpl5, bpl1,
  Even, bpl4, bpl6, bpl2, Even, bpl3, bpl5, bpl1,
  // 98 : 2 words
  Even, bpl4, bpl6, bpl2, Even, bpl3, bpl5, bpl1,
  Even, bpl4, bpl6, bpl2, Even, bpl3, bpl5, bpl1,
  // a8 : 2 words
  Even, bpl4, bpl6, bpl2, Even, bpl3, bpl5, bpl1,
  Even, bpl4, bpl6, bpl2, Even, bpl3, bpl5, bpl1,
  // b8 : 2 words
  Even, bpl4, bpl6, bpl2, Even, bpl3, bpl5, bpl1,
  Even, bpl4, bpl6, bpl2, Even, bpl3, bpl5, bpl1,
  // c8 : 2 words
  Even, bpl4, bpl6, bpl2, Even, bpl3, bpl5, bpl1,
  Even, bpl4, bpl6, bpl2, Even, bpl3, bpl5, bpl1,
  // d8
  Even, bpl4, bpl6, bpl2, Even, bpl3, bpl5, bpl1,
  Even, bpl4, bpl6, bpl2, Even, bpl3, bpl5, bpl1,
  // e8
  Even, bpl4, bpl6, bpl2, Even, bpl3, bpl5, bpl1,
  Even, bpl4, bpl6, bpl2, Even, bpl3, bpl5, bpl1,
  // f8
  Even, bpl4, bpl6, bpl2, Even, bpl3, bpl5, bpl1,
  Even, bpl4, bpl6, bpl2, Even, bpl3, bpl5, bpl1,
};

static int hidma[] = {
  // 00
  Even, Dram, Even, Dram, Even, Dram, Even, Disk,
  Even, Disk, Even, Disk, Even, Aud0, Even, Aud1,
  // 10
  Even, Aud2, Even, Aud3, Even, Spr0a,Even, Spr0b,
  Even, Spr1a,Even, Spr1b,Even, Spr2a,Even, Spr2b,
  // 20
  Even, Spr3a,Even, Spr3b,Even, Spr4a,Even, Spr4b,
  Even, Spr5a,Even, Spr5b,Even, Spr6a,Even, Spr6b,
  // 30
  Even, Spr7a,Even, Spr7b,Even, Odd,  Even, Odd,
  // 38 : 4 words
  bpl4, bpl2, bpl3, bpl1h,bpl4, bpl2, bpl3, bpl1h,
  bpl4, bpl2, bpl3, bpl1h,bpl4, bpl2, bpl3, bpl1h,
  // 40 : 4 words
  bpl4, bpl2, bpl3, bpl1h,bpl4, bpl2, bpl3, bpl1h,
  bpl4, bpl2, bpl3, bpl1h,bpl4, bpl2, bpl3, bpl1h,
  // 58 : 4 words
  bpl4, bpl2, bpl3, bpl1h,bpl4, bpl2, bpl3, bpl1h,
  bpl4, bpl2, bpl3, bpl1h,bpl4, bpl2, bpl3, bpl1h,
  // 68 : 4 words
  bpl4, bpl2, bpl3, bpl1h,bpl4, bpl2, bpl3, bpl1h,
  bpl4, bpl2, bpl3, bpl1h,bpl4, bpl2, bpl3, bpl1h,
  // 78 : 4 words
  bpl4, bpl2, bpl3, bpl1h,bpl4, bpl2, bpl3, bpl1h,
  bpl4, bpl2, bpl3, bpl1h,bpl4, bpl2, bpl3, bpl1h,
  // 88 : 4 words
  bpl4, bpl2, bpl3, bpl1h,bpl4, bpl2, bpl3, bpl1h,
  bpl4, bpl2, bpl3, bpl1h,bpl4, bpl2, bpl3, bpl1h,
  // 98 : 4 words
  bpl4, bpl2, bpl3, bpl1h,bpl4, bpl2, bpl3, bpl1h,
  bpl4, bpl2, bpl3, bpl1h,bpl4, bpl2, bpl3, bpl1h,
  // a8 : 4 words
  bpl4, bpl2, bpl3, bpl1h,bpl4, bpl2, bpl3, bpl1h,
  bpl4, bpl2, bpl3, bpl1h,bpl4, bpl2, bpl3, bpl1h,
  // b8 : 4 words
  bpl4, bpl2, bpl3, bpl1h,bpl4, bpl2, bpl3, bpl1h,
  bpl4, bpl2, bpl3, bpl1h,bpl4, bpl2, bpl3, bpl1h,
  // c8 : 4 words
  bpl4, bpl2, bpl3, bpl1h,bpl4, bpl2, bpl3, bpl1h,
  bpl4, bpl2, bpl3, bpl1h,bpl4, bpl2, bpl3, bpl1h,
  // d8 : 4 words
  bpl4, bpl2, bpl3, bpl1h,bpl4, bpl2, bpl3, bpl1h,
  bpl4, bpl2, bpl3, bpl1h,bpl4, bpl2, bpl3, bpl1h,
  // e8
  bpl4, bpl2, bpl3, bpl1h,bpl4, bpl2, bpl3, bpl1h,
  bpl4, bpl2, bpl3, bpl1h,bpl4, bpl2, bpl3, bpl1h,
  // f8
  bpl4, bpl2, bpl3, bpl1h,bpl4, bpl2, bpl3, bpl1h,
  bpl4, bpl2, bpl3, bpl1h,bpl4, bpl2, bpl3, bpl1h,
};

// check if our mask of dma is enabled
bool amiga::dma_enabled(int mask) {
  mask |= DMA_DMAEN;
  return ((dmaconr & mask) == mask);
};

// sprite 7 dma not usable if ddfstart == 0x30
int amiga::sprdma(int m) {
  sprite_t *s;
  int y, n;

  if (m & 1)
    return Odd;
  y = vPos;
  n = (m & 0xF) >> 1;
  s = &sprites[n];

  auto addr = SPRxPTR(n);
  if (addr == 0)
    return Odd;
  if (s->count == 0) {
    s->pos = cpu_read16(addr + 0);
    s->ctl = cpu_read16(addr + 2);
    s->y0 = (s->pos >> 8) + movebit(s->ctl, 2, 8);
    s->y1 = (s->ctl >> 8) + movebit(s->ctl, 1, 8);
    s->x0 = ((s->pos & 0xFF) << 1) | (s->ctl & 1);
    s->count = s->y1 - s->y0;
    if (s->pos || s->ctl) {
      blt_mod(addr, 4, 0, "sdma");
    }
  }
  else if (s->count > 0 && (y >= s->y0)) {
    rxdma(SPRxDATA(n), addr, DMA_SPREN, "sprdata");
    rxdma(SPRxDATB(n), addr, DMA_SPREN, "sprdatb");
    s->count--;
  }
  return m;
}

int amiga::auddma(int m) {
  int channel = m & 0xF;

  if (rxdma(AUDxDAT(channel), AUDxPTR(channel), (1L << channel), "aud")) {
    // run odd cycle if dma not enabled
    return Odd;
  }
  return m;
}

void amiga::dodma(int dc) {
  int m = (bplcon0 & 0x8000) ? hidma[dc] : lodma[dc];

  /* Run different dma cycles */
  if (m >= bpl1) {
    m = bpdma(m);
  }
  else if (m >= Spr0a) {
    m = sprdma(m);
  }
  else if (m >= Aud0) {
    m = auddma(m);
  }
  else if (m == Disk) {
    m = diskdma();
  }
  // m could have changed here... either Even or odd
}

int turbo = 8;
int floppySync;

int amiga::diskdma() {
  if (dma_enabled(DMA_DSKEN) && (dsklen & 0x3fff)) {
#if 0
    printf("--- disk dma:%d ptr:%.8x len:%.4x dat:%.4x sync:%.4x bytr:%.4x\n",
	   dma_enabled(DMA_DSKEN), (int)dskptr, (int)dsklen, (int)dskdat,
	   (int)dsksync, (int)dskbytr);
#endif
    for (int i = 0; i < turbo; i++) {
      dskdat = floppyReadWord();
      if (dskdat == dsksync) {
	chip_write(INTREQ, IRQ_SET | IRQ_DSKSYN, 2);
	dskbytr = dskbytr | 0x1000;
	if (!floppySync) {
	  floppySync=1;
	  return Disk;
	}
      }
      if (floppySync == 1) {
	blt_write('d', dskptr, dskdat, 2);
	dsklen = dsklen - 1;
	if ((dsklen & 0x3FFF) == 0) {
	  chip_write(INTREQ, IRQ_SET | IRQ_DSKBLK, 2);
	  floppySync = 0;
	  return Disk;
	}
      }
    }
    return Disk;
  }
  return Odd;
}

int amiga::rxdma(ioreg16 dat, ioreg32 ptr, int dmamask, const char *lbl) {
  if (!dma_enabled(dmamask)) {
    return -1;
  }
  uint32_t addr = ptr;
  if (ptr != 0) {
    dat = blt_read('p', ptr, 2);
  }
  //printf("%.4x %.4x[%.4x] rxdma:%.8x=%.4x[%s]\n", vPos, hPos, (int)ddfstrt, addr, (int)dat, lbl); 
  return 0;
}

// Render playfields
void amiga::renderbg(int y, int count, int dmapos)
{
  uint16_t *pd = (uint16_t *)&mem[BPL1DAT];
  uint16_t mask = 0x0080;
  int line[640] = { 0 };
  int bx;

  /* Find x pos since data start */
  bx = dmapos * 16;
  printf("Render: %.2x %.2x-%.2x [%.2x] %d\n", hPos, (int)ddfstrt, (int)ddfstop, (hPos - ddfstrt), bx); 

  // build line
  assert(count*16 < 640);
  for (int x = 0; x < count*16; x++) {
    line[x] = pclr(pd, mask);
    mask = ror16(mask, 1);
  }

  // get colors and collision
  for (int x = 0; x < count*16; x++) {
    if (x+bx < 640) {
      collide[x+bx] = (line[x] << 8);
      bgline[x+bx] = cpal[line[x]];
    }
  }
}

int amiga::bpdma(int mode) {
  if (vPos >= screen.y0 && vPos < screen.y1) {
    if (mode == bpl1 || mode == bpl1h) {
      if (totdma++ > wdma) {
	return Even;
      }
      for (int i = 0; i < bpu; i++) {
	// rundma... if hackbltro, only run if frame==3
	if (hPos < ddfstrt) {
	  BPLxDAT(i) = 0;
	}
	else {
	  rxdma(BPLxDAT(i), BPLxPTR(i), DMA_BPEN, "bpl");
	}
	if (hPos >= ddfstop) {
	  BPLxDAT(i) = 0;
	}
      }
      renderbg(vPos, 1, totdma - 1);
    }
  }
  return mode;
}

/* DMA timing
 * https://www.markwrobel.dk/post/amiga-machine-code-letter4-dma-revisited/dmaTiming.png
 * odd, Even
 * lowres: _462_351
 * hires : 43214321
 *
 * 4 cycles mem refresh
 * 3 cycles disk dma
 * 4 cycles audio (2 bytes per channel)
 * 16 cycles sprite dma
 * 80 cycles bitplane dma
 * 68k: Even cycle
 */
uint16_t *spraddr(int n) {
  return (uint16_t *)&mem[SPR0DATA + (n * 8)];
}

void amiga::renderspr(int y, int n, int mode, uint32_t *palette)
{
  sprite_t *s = &sprites[n];
  int x0 = s->x0;
  uint16_t mask = 0x0080;
  uint16_t *pd;
  
  if (y < s->y0 || y >= s->y1) {
    return;
  }
  pd = spraddr(n);
  for (int i = 0; i < 16; i++) {
    int clr = pclr(pd, mask, mode);
    if (clr != 0 && (x0 + i) < 640) {
      // 0 is transparent
      bgline[x0 + i] = palette[clr];
      collide[x0 + i] |= (0x1 << n);
    }
    mask = ror16(mask, 1);
  }
}

int amiga::check_collide() {
  int coll = 0;
  auto cxbit = [&](int cond, int bit, const char *msg) {
    bit = (1L << bit);
    if (cond != 0 && !(coll & bit)) {
      //printf("coll:%s\n", msg);
      coll |= bit;
    }
  };
  // scan this line for collision
  for (int i = 0; i < 640; i++) {
    int ci = collide[i];
    if (ci & CXSPR0) {
      cxbit(ci & CXSPR2, 9,  "spr01->spr23");
      cxbit(ci & CXSPR4, 10, "spr01->spr45");
      cxbit(ci & CXSPR6, 11, "spr01->spr67");
      cxbit(ci & CXPF2,  5,  "spr01->Even");
      cxbit(ci & CXPF1,  1,  "spr01->odd");
    }
    if (ci & CXSPR2) {
      cxbit(ci & CXSPR4, 12, "spr23->spr45");
      cxbit(ci & CXSPR6, 13, "spr23->spr67");
      cxbit(ci & CXPF2,  6,  "spr23->Even");
      cxbit(ci & CXPF1,  2,  "spr23->odd");
    }
    if (ci & CXSPR4) {
      cxbit(ci & CXSPR6, 14, "spr45->spr67");
      cxbit(ci & CXPF2,  7,  "spr45->even");
      cxbit(ci & CXPF1,  3,  "spr45->odd");
    }
    if (ci & CXSPR6) {
      cxbit(ci & CXPF2, 8, "spr67->even");
      cxbit(ci & CXPF1, 4, "spr67->odd");
    }
    if (ci & CXPF2) {
      cxbit(ci & CXPF1, 0, "even->odd");
    }
    // reset
    collide[i] = 0;
  }
  clxdat = coll;
  return coll & ~0x1ff;;
};

void amiga::sethblank(bool state) {
  if (state) {
    return;
  }
  int y = vPos - 1;
  printf("--eol %d [%d]\n", vPos, screen.y0);
  
  if (vPos >= screen.y0) {
    // render sprite from back to front....
    // covers priority for sprites
    for (int i = 6; i >= 0; i-=2) {
      if (attached(i)) {
	renderspr(y, i, 99, &cpal[16]);
      }
      else {
	renderspr(y, i, 2, &cpal[16+i*4]);
	renderspr(y, i+1, 2, &cpal[16+i*4]);
      }
    }
    
    // Check for any collisions
    int cx = check_collide();
    if (cx) {
      xline(&bgline[630], 10, RED);
    }
    
    // draw bgline color
    int totlen = 0;
    int start = 0;
    if (bplcon0 & 0x8000) {
      totlen = (ddfstop - ddfstrt) * 4;
      start = 640 - totlen - 16;
    }
    else {
      totlen = (ddfstop - ddfstrt) * 2;
      start = 320 - totlen - 16;
    }
    drawline(scr, bgline+start, totlen, 0, y - screen.y0 + 10, 0);
    xline(bgline, 640, 0);
    
    totdma = 0;
    for (int i = 0; i < 8; i++) {
      // increase plane mod
      auto addr = BPLxPTR(i);
      uint16_t mod = (i & 1) ? bpl2mod : bpl1mod;
      blt_mod(addr, mod, 0, "bplmod");
    }
  }
  // tick cia_b counter -> per line
  cia_tod(&cia_b);
}

int ddelta = 0;

bool amiga::vid_tick()
{
  bool rc = false;
  int ndma;

  vposr = 0x1000 | (vPos >> 8);
  vhposr = (vPos << 8) | hPos;

  if (hPos == 0) {
    ndma = (ddfstop - ddfstrt);
    if (bplcon0 & 0x8000) {
      wdma = (ndma / 4) + 1 + ddelta;
    }
    else {
      // ick??
      wdma = (ndma / 8) + ddelta;
    }
  }
  coppc = runcop(coppc, hPos, vPos);
  dodma(hPos);

  rc = crtc_t::tick();
  if (rc) {
    // end-of-frame
    static int old_x, old_y;

    if (scr->KeyState['q'])  {
      ddelta--;
      scr->KeyState['q']=false;
    }
    if (scr->KeyState['w']) {
      ddelta++;
      scr->KeyState['w']=false;
    }
    if (scr->key(Key::K_F1)) {
      static int inserted;
      printf("F1\n");
      if (inserted++ == 0) {
	floppyInsert(0);
      }
    }

    // Adjust mouse position
    int smx, smy, sbtn;

    sbtn = scr->getmouse(smx, smy);
    cia_a.pra |= 0b0100'0000;
    potimp = potimp | 1024;
    if (sbtn & 1) {
      cia_a.pra &= 0b0011'1111;
    }
    if (sbtn & 2) {
      potimp = potimp & 0xFBFF;
    }
    
    uint8_t tx = (joy0dat & 0xFF) + (int8_t)(smx - old_x)*2;
    uint8_t ty = (joy0dat >> 8) + (int8_t)(smy - old_y)*2;
    joy0dat = (ty << 8) | tx;
    old_x = smx;
    old_y = smy;
    
    // tick cia_a counter -> per frame
    cia_tod(&cia_a);
    xx_request_irq(IRQ_VERTB, "VBI");
    if (dma_enabled(DMA_COPEN)) {
      //showcop(cop1ptr, "cop1");
      //showcop(cop2ptr, "cop2");
    }
    if (dma_enabled(DMA_COPEN)) {
      chip_write(COPJMP1, 0, 2);
    }
    drawframe();
  }
  return rc;
}

void amiga::init(const char *name)
{
  romsz = loadrom(name, 0xF80000);
  scr = new Screen(640, 280, 30, 50, 0, NULL);
  scr->xs = 2;
  scr->ys = 2;
  scr->init(1);

  vEnd = 256+44;
  hEnd = 0xe0;
  chip_write(DIWSTRT, 0x2c81, 2);
  chip_write(DIWSTOP, 0xf4c1, 2);
  chip_write(DSKSYNC, 0x4489, 2);
  Zf = 1;
}

uint32_t amiga::chip_read(uint32_t addr, int n) {
  uint32_t val = 0;
  if (n == 4) {
    uint32_t lo, hi;
    hi = get16be(&mem[addr+0]);
    lo = get16be(&mem[addr+2]);
    val = (hi << 16) + lo;
  }
  else if (n == 2) {
    val = get16be(&mem[addr]);
  }
  else {
    val = mem[addr];
  }
  switch(addr) {
  case CLXDAT:
    // reset collision register
    clxdat = 0;
    break;
  }
  printf("%.8x %.4x %.4x read  chipreg:%.8x %-12s  [%.8x]\n", SPC, vPos, hPos, addr, iorname(addr), val);
  return val;
}

/* Copper format:
 *  MOVE: 0000.000r.rrrr.rrr0   dddd.dddd.dddd.dddd
 *  WAIT: vvvv.vvvv.hhhh.hhh1   BVVV.VVVV.HHHH.HHH0
 *  SKIP: ____.____.____.___1   ____.____.____.___1
 */
void showcop(uint32_t pc, const char *lbl)
{
  uint32_t code0, code1;

  printf("----- showcop: %.8x dma:%x irq:%x %s\n", pc, 
	 thegame.dma_enabled(DMA_COPEN), !!(thegame.intenar & IRQ_COPER), lbl);
  hexdump(&mem[pc], 32);
  for(;;) {
    code0 = cpu_read16(pc + 0);
    code1 = cpu_read16(pc + 2);
    if (!code0 && !code1) {
      return;
    }
    if ((code0 & 1) == 0) {
      uint32_t cr = code0 + 0xdff000;
      printf("%-4s %.4x %.4x MOVE: %.8x[%s] %.4x\n", lbl,
	     code0, code1, 
	     cr, iorname(cr), code1);
     }
    else if ((code1 & 1) == 0) {
      printf("%-4s %.4x %.4x WAIT: %d,%d\n", lbl,
	     code0, code1, 
	     (code0 & code1) >> 8,
	     (code0 & code1) & 0xff);
    }
    else {
      printf("%-4s SKIP: %.4x:%.4xx\n", lbl, code0, code1);
    }
    if (code0 == 0xffff)
	break;
    pc += 4;
  }  
}

void amiga::chip_write(uint32_t addr, uint32_t val, int n)
{
  printf("%.8x %.4x %.4x write chipreg:%.8x %-12s  [%.8x]\n", SPC, vPos, hPos, addr, iorname(addr), val);
  if (n == 4) {
    printf("chipwrite4: %.8x:%.4x %.8x:%.4x\n",
	   addr+2, val & 0xffff,
	   addr+0, val >> 16);
    put16be(&mem[addr+2], val);
    put16be(&mem[addr+0], val >> 16);
  }
  else if (n == 2) {
    put16be(&mem[addr], val);
  }
  else {
    mem[addr] = val;
  }
  switch (addr) {
  case INTENA: // interrupt enable
    xsetclr(intenar, val, "intena");
    break;
  case INTREQ: // interrupt request
    xsetclr(intreqr, val, "intreq");
    break;
  case DMACON: // dma control
    xsetclr(dmaconr, val, "dmacon");
    showbits(dmaconr, bits_dmaconr, "dmaconr");
    break;
  case ADKCON: // disk control
    xsetclr(adkconr, val, "adkcon");
    break;
  case DSKLEN:
    // write DMA_DSKEN to DMACON
    // write 0x4000 to DSKLEN
    // write length to disklen
    // write length again to disklen
    // dma happens
    // write 0x4000 to DSKLEN to disable writes
    break;
  case BLTSIZH:
    //xsetclr(dmaconr, 0xc000, "sizh");
    break;
  case BLTSIZE:
    blt_setsize(val);
    break;
  case BPLCON0:
    if (bplcon0 & BPL_BPU3) {
      bpu = 8;
    }
    else {
      bpu = (bplcon0 >> 12) & 0x7;
    }
    printf("%4d bplcon0: bpu:%d HAM:%d dbpf:%d lace:%d %s\n",
	   vPos, bpu, !!((val >> 11) & 1),
	   !!((val >> 10) & 1),
	   !!((val >> 3) & 1),
	   (bplcon0 & 0x8000)?"hires":"lores");
    break;
  case BPLCON1:
    printf("%4d bplcon1: pf2:%x pf1:%x\n",
	   vPos,
	   ((val >> 4) & 0xf),
	   ((val >> 0) & 0xf));
    break;
  case BPLCON2:
    printf("%4d bplcon2: pf2:%d spr:%x %x\n",
	   vPos,
	   (val >> 6) & 1,
	   (val >> 3) & 7,
	   (val >> 0) & 7);
    break;
  case DIWSTRT:
    /* Get UL screen dimensions */
    screen.x0 = (diwstrt & 0xff);
    screen.y0 = (diwstrt >> 8);
    printf("screen: %d, %d\n", screen.x0, screen.y0);
    break;
  case DIWSTOP:
    /* Get LR screen dimensions
     * X is always + 256
     * Y is +256 if < 128  (128..255.000..127 -> 256..384)
     */
    screen.x1 = 256 + (diwstrt & 0xff);
    screen.y1 = (diwstrt >> 8);
    if ((screen.y1 & 0x80) == 0)
      screen.y1 += 256;
    printf("screen res: %d,%d\n", screen.x1, screen.y1);
    break;
  case DDFSTRT:
    /* Fetch start, horiz */
    printf("ddfstrt: %.4x\n", val);
    break;
  case DDFSTOP:
    printf("ddfstop: %.4x\n", val);
    break;
  case COPJMP1:
    printf("copjmp1: %.8x\n", (int)cop1ptr);
    if (cop1ptr) {
      showcop(cop1ptr, "--> cop1");
      coppc = cop1ptr;
      copstate = 1;
    }
    break;
  case COPJMP2:
    printf("copjmp2: %.8x\n", (int)cop2ptr);
    if (cop2ptr) {
      showcop(cop2ptr, "--> cop2");
      coppc = cop2ptr;
      copstate = 2;
    }
    break;
  case SPR0PTH:
  case SPR1PTH:
  case SPR2PTH:
  case SPR3PTH:
  case SPR4PTH:
  case SPR5PTH:
  case SPR6PTH:
  case SPR7PTH:
    // reset sprite count
    val = (addr - SPR0PTH)/4;
    sprites[val].count = 0;
    break;
  case COLOR00 ... COLOR31:
    // set color palette
    cpal[(addr - COLOR00) / 2] = ARGB(val);
    break;
  }
}

/* memory map:
 * 00000000 .. 003FFFFF memory
 * ........ .. 00BFCFFF n/a
 * 000BFD00 .. 00BFDFFF cia2
 * 00BFE000 .. 00BFEFFF cia1
 * 00C00000 .. 00D7FFFF memory
 * 00DA0000 .. 00DBFFFF ide
 * 00DC0000 .. 00DCFFFF rtc
 * 00DFF000 .. 00DFFFFF chip registers
 * 00F80000 .. 00FBFFFF Kickstart ROM (256k)
 * 00FC0000 .. 00FFFFFF Kickstart ROM (256k)
 */
static void *busread(uint32_t addr, uint32_t& val, int size)
{
  addr &= 0xFFFFFF;
  if (addr <= 0xFFFF && (cia_a.pra & 1)) {
    // overlay
    printf("lowmem: %x %.8x\n", cia_a.pra, addr);
    addr += 0x00f80000;
  }
  switch(addr) {
  case 0x00000000 ... 0x001fffff: // chip ram
  case 0x00c00000 ... 0x00dbffff:
  case 0x00f80000 ... 0x00ffffff: // rom
    return &mem[addr];
  case 0x00BFE000 ... 0x00BFEFFF:
    if (addr & 1) {
      val = cia_a.Getreg((addr >> 8) & 0xF);
    }
    break;
  case 0x00BFD000 ... 0x00BFDFFF:
    if (!(addr & 1)) {
      val = cia_b.Getreg((addr >> 8) & 0xF);
    }
    break;
  case 0x00dc0000 ... 0x00dcffff:
    // rtc https://eab.abime.net/attachment.php?attachmentid=14647&d=1188908610
    val = thegame.rtc_read(addr);
    break;
  case 0x00DFF000 ... 0x00DFFFFF:
    /* access chip registers */
    val = thegame.chip_read(addr, size);
    break;
  }
  return NULL;
}

static void *buswrite(uint32_t addr, uint32_t val, int size)
{
  addr &= 0xFFFFFF;
  switch (addr) {
  case 0x00000000 ... 0x001FFFFF:
  case 0x00C00000 ... 0x00DBFFFF:
  case 0x00F80000 ... 0x00FFFFFF:
    return &mem[addr];
  case 0x00BFE000 ... 0x00BFEFFF:
    if (addr & 1) {
      cia_a.Setreg((addr >> 8) & 0xF, val);
    }
    break;
  case 0x00BFD000 ... 0x00BFDFFF:
    if (!(addr & 1)) {
      cia_b.Setreg((addr >> 8) & 0xF, val);
    }
    break;
  case 0x00DC0000 ... 0x00DCFFFF:
    // rtc https://eab.abime.net/attachment.php?attachmentid=14647&d=1188908610
    thegame.rtc_write(addr, val);
    break;
  case 0x00DFF000 ... 0x00DFFFFF:
    /* write little-endian memory */
    thegame.chip_write(addr, val, size);
    break;
  }
  return NULL;
}

uint32_t amiga::runcop(uint32_t pc, int hpos, int vpos)
{
  uint16_t cpos, code0, code1, waitpos;
  
  // don't run if not enabled
  if (!dma_enabled(DMA_COPEN)) {
    return pc;
  }
  if (!pc || pc == -1) {
    return pc;
  }
  /* Copper format:
   *  MOVE: 0000.000r.rrrr.rrr0   dddd.dddd.dddd.dddd
   *  WAIT: vvvv.vvvv.hhhh.hhh1   BVVV.VVVV.HHHH.HHH0
   *  SKIP: ____.____.____.___1   ____.____.____.___1
   */
  cpos = (vpos << 8) + hpos;
  code0 = cpu_read16(pc + 0);
  code1 = cpu_read16(pc + 2);
  if ((code0 & 1) == 0) {
#if 0
    uint32_t cr = code0 + 0xdff000;
    printf("%.4x %.4x/%.4x copper move: %.8x[%s] = %.4x\n",
	   cpos, code0, code1, cr, iorname(cr), code1);
#endif
    if (code0 >= 0 && code0 <= 0x1FF) {
      chip_write(0xDFF000 + code0, code1, 2);
    }
    if (coppc != pc) {
      printf("pointer changes..... : %.8x %.8x %x\n", coppc, pc, dma_enabled(DMA_COPEN));
      if (coppc < 0xc00000) {
	return coppc;
      }
    }
    return pc + 4;
  }
  waitpos = (code0 & code1) & 0xfffe;
  if (cpos >= waitpos) {
    //printf("%.4x/%.4x copper skip or wait\n", cpos, waitpos);
    pc += (code1 & 1) ? 8 : 4;
  }
  return pc;
}

void amiga::drawframe()
{
  static time_t fpstime = time(NULL);
  int h = 280;
  float fps;
  time_t now;

  printf("===== frame: %d int[%.4x:%.4x] aud:%x spr:%d blt:%d cop:%d bpl:%d pra:%.2x\n",
	 frame,
	 (int)intenar, (int)intreqr,
	 dmaconr & 0xf,
	 dma_enabled(DMA_SPREN),
	 dma_enabled(DMA_BLTEN),
	 dma_enabled(DMA_COPEN),
	 dma_enabled(DMA_BPEN),
	 cia_a.pra);

  scr->scrbox(screen.x0, 0, screen.x1, screen.y1-screen.y0, MKRGB(255,255,0));

  /* Draw CIA pra/prb state */
  for (int i = 0; i < 8; i++) {
    int flsh, pramask = 0x2;
    int m = (0x80 >> i);
    
    // ciaa.pra ciaa.prb, ciaa.cra, ciaa.crb
    flsh = ((cia_a.pra ^ pramask) & m) ? GREEN : GREY;
    scr->scrrect(10+(i*6), h+5, 5, 5, flsh);
    flsh = (cia_a.prb & m) ? GREEN : GREY;
    scr->scrrect(60+(i*6), h+5, 5, 5, flsh);
    flsh = (cia_a.cra & m) ? GREEN : GREY;
    scr->scrrect(120+(i*6), h+5, 5, 5, flsh);
    flsh = (cia_a.crb & m) ? GREEN : GREY;
    scr->scrrect(180+(i*6), h+5, 5, 5, flsh);

    // icr
    flsh = GREY;
    if (cia_a.cia_irq.en & cia_a.cia_irq.sts & m)
      flsh = GREEN;
    else if (cia_a.cia_irq.en & m)
      flsh = RED;
    scr->scrrect(240+(i*6), h+5, 5, 5, flsh);
    
    // ciab.pra ciab.prb
    flsh = (cia_b.pra & m) ? GREEN : GREY;
    scr->scrrect(10+(i*6), h+15, 5, 5, flsh);
    flsh = (cia_b.prb & m) ? GREEN : GREY;
    scr->scrrect(60+(i*6), h+15, 5, 5, flsh);
    flsh = (cia_b.cra & m) ? GREEN : GREY;
    scr->scrrect(120+(i*6), h+15, 5, 5, flsh);
    flsh = (cia_b.crb & m) ? GREEN : GREY;
    scr->scrrect(180+(i*6), h+15, 5, 5, flsh);

    // icr
    flsh = GREY;
    if (cia_b.cia_irq.en & cia_b.cia_irq.sts & m)
      flsh = GREEN;
    else if (cia_b.cia_irq.en & m)
      flsh = RED;
    scr->scrrect(240+(i*6), h+15, 5, 5, flsh);
  }
  // disks
  for (int i = 0; i < 4; i++) {
    scr->scrrect(10 + (i * 10), 0, 8, 8, disks[i].pra ? GREEN : RED);
  }
  for (int i = 0; i < 32; i++) {
    scr->scrrect(10 + (i * 6), h + 25, 5, 5, cpal[i]);
  }

  int ds, de;
  int count;
  
  ds = R16(DDFSTRT);
  de = R16(DDFSTOP);
  
  count = de - ds;
  if (bplcon0 & D15) {
    // hires
    count = (count / 4) + 1;
  }
  else {
    count = (count / 8) + 1;
  }
  now = time(NULL);
  fps = (float)frame / (now - fpstime);
  scr->scrtext(0, h + 32, WHITE, "(%d,%d)-(%d,%d) %.2x:%.2x %2d %dx%dx%d",
	       screen.x0, screen.y0, screen.x1, screen.y1, ds, de, count,
	       count * 16, screen.y1 - screen.y0, 1L << bpu);
  scr->scrtext(0, h + 40, WHITE, "frame:%d fps:%.2f wdma:%d", frame, fps, wdma);
#if 0
  draw_gradient(scr, 180, 250, 0xFF0000,
		100,0, 0x0000FF,
		200, 125, 0x00FF00);
#endif
  scr->draw();
  scr->clear();
}

/* 1000.0000.0000.1000 8008 (external int2/ciaa)
 * 1010.0000.0000.0000 A000 (external int6/ciab)
 */
//https://wiki.amigaos.net/wiki/Exec_Interrupts

void amiga::check_irq()
{
  const int irq[] = { 1,1,1, 2, 3,3,3, 4,4,4,4, 5,5, 6 };
  uint16_t en, sts;
  int lvl = 0;
  
  en  = intenar;
  sts = intreqr & en;
  if (!(en & IRQ_INTEN) || !sts) {
    return;
  }
  for (int i = 13; i >= 0; i--) {
    if ((sts >> i) & 1) {
      stopped = false;
      lvl = irq[i];
      if (cpu_irq(lvl)) {
	break;
      }
    }
  }
}

void getextra(dstk& s, uint32_t base, uint32_t size)
{
  FILE *fp;
  char line[128];
  uint32_t b;

  fp = fopen("dumpcfg.txt", "r");
  while (fgets(line, sizeof(line), fp) != NULL) {
    sscanf(line, "%x", &b);
    s.push(b, 1, dstk::PENDING);
  }
#if 0
  uint32_t end, name, extra;
  printf("hazbase: %.8x\n", base);
  for (uint32_t off = base; off < base+size; off += 2) {
    uint16_t nb = cpu_read16(off);
    // found a romtag
    if (nb == 0x4AfC && cpu_read32(off + 2) == off) {
      end = cpu_read32(off + 6);
      name = cpu_read32(off + 14);
      extra = cpu_read32(off + 22);
      printf("found sig: off:%.8x %.8x end:%.8x %2x %2x %2x %2x %.8x %.8x extra:%.8x [%s]\n",
	     off, cpu_read32(off + 2),
	     cpu_read32(off + 6),
	     cpu_read8(off + 10),
	     cpu_read8(off + 11),
	     cpu_read8(off + 12),
	     cpu_read8(off + 13),
	     name,
	     cpu_read32(off + 18),
	     extra,
	     (char *)&mem[name]);
      for (int i = extra; i < end; i++) {
	s.push(extra, 1, dstk::PENDING);
      }
    }
  }
#endif
}

bool cia_tod(cia8250 *c)
{
  uint32_t tod, alrm;

  alrm = c->getTod(1);
  tod = c->getTod();
  c->setTod(tod + 1);
  if (tod == alrm && alrm) {
    printf("fire alarm.... : %.8x : %.2x %.2x\n", alrm, c->crb, c->cia_irq.en);
  }
  return true;
}

bool cia_tick(cia8250 *c)
{
  bool cret = false;

  // show interrupt enabled
  if (c->cia_irq.en & c->cia_irq.sts) {
    int sts = (c->cia_irq.en & c->cia_irq.sts);
    printf("cia%d irq pending: %.4x ", sts);
    if (sts) {
      cret = true;
    }
    if (sts & 0x1) {
      printf("TimerA ");
    }
    if (sts & 0x2) {
      printf("TimerB ");
    }
    if (sts & 0x4) {
      printf("alarm ");
    }
    if (sts & 0x8) {
      printf("serial ");
    }
    if (sts & 0x10) {
      printf("flag ");
    }
    printf("\n");
  }
  if (c->tmra.tick()) {
    printf("ticka fired : %.4x bmode:%d\n", c->tmra_freq, (c->crb >> 5) & 3);
    if (c->cia_irq.set(0x01)) {
      printf("ticka irq\n");
    }
    cret = true;
  }
  if (c->tmrb.tick()) {
    printf("tickb fired : %.4x\n", c->tmrb_freq);
    if (c->cia_irq.set(0x02)) {
      printf("tickb irq\n");
    }
    cret = true;
  }
  return cret;
}

bool amiga::tick()
{
  static int ecounter;

  // 60hz
  if (++ecounter == 5) {
    if (cia_tick(&cia_a)) {
      xx_request_irq(IRQ_CIAA, "CIA.A tick");
    }
    if (cia_tick(&cia_b)) {
      xx_request_irq(IRQ_CIAB, "CIA.B tick");
    }
    ecounter = 0;
  }
  //blt_tick();
  vid_tick();
  return false;
}

struct area_t {
  uint32_t start, end;
  const char *lbl;
} area[] = {
  { 0x00fc34cc, 0x00fc43f4, "audio" },
  { 0x00fc450c, 0x00fc4790, "cia" },
  { 0x00fc4794, 0x00fc4af4, "disk" },
  { 0x00fc4afc, 0x00fc516c, "expansion" },
  { 0x00fc5378, 0x00fd0a3c, "graphics" },
  { 0x00fd3f5c, 0x00fe0378, "intution" },
  { 0x00fe0d90, 0x00fe0df8, "layers" },
  { 0x00fe424c, 0x00fe472c, "mathfp" },
  { 0x00fe4774, 0x00fe487c, "misc" },
  { 0x00fe4880, 0x00fe49c8, "potgo" },
  { 0x00fe49cc, 0x00fe4f6c, "ramlib" },
  { 0x00fe4fe4, 0x00fe502e, "keymap" },
  { 0x00fe502e, 0x00fe507a, "keyboard" },
  { 0x00fe507a, 0x00fe50c6, "gameport" },
  { 0x00fe50c6, 0x00fe510e, "input" },
  { 0x00fe510e, 0x00fe66d8, "console" },
  { 0x00fe8884, 0x00fe88c0, "strap" },
  { 0x00fe90ec, 0x00fe98d8, "timer" },
  { 0x00fe98e4, 0x00feb3dc, "trackdisk" },
  { 0x00feb400, 0x00ff34d8, "workbench" },
  { 0x00ff425a, 0x00ff4290, "dos" },
  { },
};
int cpu_step()
{
  uint16_t op;

  SPC = PC;
  checkpc();
  thegame.check_irq();
  if (stopped)
    return 0;
  if (visited[PC] == 0) {
    visited[PC]++;
  }
  op = cpu_fetch(Word, "step");
  return decode_68k(op);
}

void m68k_emul1010(uint16_t) { }
void m68k_emul1111(uint16_t) { }

void waitfor(int vPos, int hPos) {
  for(;;) {
    if ((vPos == -1 || vPos == thegame.vPos) &&
	(hPos == -1 || hPos == thegame.hPos)) {
      return;
    }
    thegame.tick();
  }
}

int hackbltro()
{
  uint16_t op;
  static int last;
  int nc = 1;
  
  op = get16be(&mem[PC]);
  printf("%.4x %d %.2x %.2x\n", thegame.hPos, thegame.vPos, thegame.ddfstop, thegame.hEnd);
  if (PC >= 0xf80292) {
    trace=2;
    if (op == 0x4CDE) {
      // movem.l (A6)+, [3ffe] D1-A5
      waitfor(-1, 0xda);
      last = 11;
    }
    if (op == 0x2087) {
      // move.l d7,(a0)
      waitfor(-1, 0x31);
      nc = 6;
    }
    if (op == 0x2ede || op == 0x2e9e) {
      // move.l (a6)+,(a7)+
      printf("last iz: %d\n", last);
      nc = last;
      last = 10;
    }
    if ((op & 0xFFF0) == 0x4850) {
      // pea (an)
      nc = 6;
    }
    if (op == 0x2244 || op == 0x2445 || op == 0x2646) {
      // movea.l d4,a1
      // movea.l d5,a2
      // movea.l d6,a3
      nc = 2;
    }
    if (op == 0x2e81 || op == 0x2e82 || op == 0x2ec3) {
      // move.l d1,(a7)
      // move.l d2,(a7)
      // move.l d3,(a7)+
      nc = 6;
    }
  };
  return nc;
}

int main(int argc, char *argv[])
{
  trace = 0;
  gentbl();
  setbuf(stdout, NULL);

  floppyLoad(0, "original5.ADF");
  for (int i=1; i < argc; i++) {
    floppyLoad(i, argv[i]);
  }
  
  // https://github.com/nicodex/amiga-ocs-cpubltro/blob/main/cpubltro.asm
  thegame.init("roms/de-amiga-os-204.rom");
  //thegame.init("cpubltro-0f8.rom");
  //thegame.init("emutos-amiga-rom-1.3//emutos-amiga.rom");
  //thegame.init("DiagROM/16bit.bin");
  
  //test();
  //test_blt();
  //tod_test(&cia_a, 1);
  //tod_test(&cia_b, 263);
  //exit(0);

  extern int nokbd;
  nokbd=1;
  cpu_reset();
  if (argc > 4) {
    dumpcfg(PC, 0xF80000, thegame.romsz);
  }
  trace=0;
  for (int i = 0; i < 8; i++) {
    thegame.BPLxDAT(i) = 0;
  }
  while(1) {
    int nc = 1;

    //nc = hackbltro();
    cpu_step();
    for (int i = 0; i < nc; i++) {
      thegame.tick();
    }
  }
  cpu_shutdown();
  return 0;
}
