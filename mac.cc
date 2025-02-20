#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>
#include <unistd.h>
#include <string.h>
#include <stdarg.h>
#include "cpu.h"
#include "bus.h"
#include "util.h"
#include "gr.h"
#include "mac.h"
#include "cpu/cpu_m68k.h"
#include "gpio.h"

struct mac68k;
struct drive_t;
const void *mptr(uint32_t m);
typedef uint8_t (*cbfn_t)(void *, uint8_t, int);

void runcode(uint32_t npc);

static uint32_t audio_base = 0x3ffd00;
static uint32_t video_base = 0x3fa700;

void flogger(int n, const char *fmt, ...)
{
  va_list ap;

  va_start(ap, fmt);
  printf("%.8x: ", SPC);
  vprintf(fmt, ap);
}

/* https://www.osdata.com/system/physical/memmap.htm
 * A23 A22 A21 A20 A19
 *   0   0   X   X   X 00.0000 - 3F.FFFF RAM OVERLAY=0
 *   0   1   X   0   X 40.0000 - 4F.FFFF ROM OVERLAY=0
 *   0   1   0   0   X 40.0000 - 4F.FFFF ROM OVERLAY=1
 *   0   1   1   1   X 60.0000 - 7F.FFFF RAM OVERLAY=1
 *   1   0   X   0   X 70.0000 - 0F.FFFF ROM OVERLAY=1
 *   1   0   X   0   X 80.0000 - 8F.FFFF ROM OVERLAY=X, SCC
 *   1   1   0   0   X C0.0000 - CF.FFFF ROM OVERLAY=X, IWM
 *   1   0   0   X   X 80.0000 - 9F.FFFF SCC   (not 1000X)
 *   1   0   1   X   X A0.0000 - BF.FFFF SCC   (not 1010X)
 *   1   1   0   X   X C0.0000 - DF.FFFF IWM   (not 1100X)
 *   1   1   1   0   X E0.0000 - EF.FFFF VIA   (not 11100)
 *   1   1   1   X   0 E0.0000 - E7.FFFF PHASE (not 11100)
 */

/* 76543210 VIA PORTA
 * |||||||+ PA0 o VOL0
 * ||||||+- PA1 o VOL1
 * |||||+-- PA2 o VOL2
 * ||||+--- PA3 o /SND PG 2
 * |||+---- PA4 o OVERLAY
 * ||+----- PA5 o SEL
 * |+------ PA6 o /VID PG 2
 * +------- PA7 i /SCC
 */
static const char *prta[] = {
  "VOL0",
  "VOL1",
  "VOL2",
  "/SND PG 2",
  "OVERLAY",
  "SEL",
  "/VID PG 2",
  "/SCC",
};

/* abcdefgh VIA PORTB
 * |||||||+ PB0 v RTC DATA
 * ||||||+- PB1 o RTC CLOCK
 * |||||+-- PB2 o /RTC ENABLE
 * ||||+--- PB3 i MOUSESW
 * |||+---- PB4 i MOUSEV2
 * ||+----- PB5 i MOUSEX2
 * |+------ PB6 i HBLANK
 * +------- PB7 o SNDRESET
 */
static const char *prtb[] = {
  "RTC DATA",
  "RTC CLOCK",
  "/RTC EN",
  "/MOUSESW",
  "MOUSEY2",
  "MOUSEX2",
  "HBLANK",
  "SNDRESET",
};

/* Base clock 7.83 Mhz
 * line 22.255 KHz
 * 352 clock cycles per line
 * 4 cycles per read/write
 * 88 memory access slots
 * 512 pixels or 32 (16-bit) words
 * CPU gets 24 slots per horizontal retrace
 * CPU gets all slots per vertical retrace
 * ~32560 slots per frame
 * 342 lines per screen = 342x32 = 10944 video reads
 */
int sndfd;
Timer samp_tmr;
#define SAMPLE_RATE 22500

void audio_init()
{
  sndfd = open("snd.u8", O_CREAT|O_TRUNC|O_WRONLY, 0666);

  /* Set to 44100 Hz */
  samp_tmr.settimer(1193181/SAMPLE_RATE, true, true, "samp");
}

void writesnd(uint8_t v) {
  write(sndfd, &v, 1);
  write(sndfd, &v, 1);
}

/* 6522 chip
 *
 * Interrupt registers : Level 1
 * +---+---+---+---+---+---+---+---+
 * |SET|T1 |T2 |CB1|CB2|SR |CA1|CA2|
 * +---+---+---+---+---+---+---+---+
 *     T1 = Timer 1 interrupt
 *     T2 = Timer 2 interrupt
 *    CB1 = KBD.clk interrupt
 *    CB2 = KBD.data interrupt
 *     SR = KBD.data
 *    CA1 = VSYNC
 *    CA2 = 1sec interrupt
 */
struct via_t : public gendev_t {
  mac68k *m;
  gpio_t  porta;
  gpio_t  portb;

  uint8_t ier = 0; // interrupt enable mask
  uint8_t ifr = 0; // interrupt status mask

  Timer  t1;
  Timer  t2;
  uint16_t t1_latch = 0;
  uint16_t t2_latch = 0;
  
  uint8_t rtc_idx = 0;
  uint8_t rtc_data = 0;

  uint8_t pci;
  uint8_t aux;
  uint8_t shift;
  
  int io(uint32_t addr, int mode, iodata_t& val);
  void clr_ifr(const uint8_t v, const char *lbl = "") {
    ifr &= ~(v & 0x7f);
  }
  void set_ifr(const uint8_t v, bool state, const char *lbl = "") {
    if (!state) {
      clr_ifr(v, lbl);
    } else {
      ifr |= (v | 0x80);
    }
  };
  void clr_ier(const uint8_t v) {
    ier = ~(v & 0x7F);
  }
  void set_ier(const uint8_t v, bool state) {
    if (!state) {
      clr_ier(v);
    } else {
      ier |= (v | 0x80);
    };
  };

  void init(mac68k *);
};

struct drive_t {
  int installed; // disk drive exists [0x00 = yes, 0x80 = no]
  int present;   // disk present      [0x00 = yes]
  int sides;     // number of sides   [0x00 = single, 0x80 = double]
  int track;
  int trackdir;
  int motor;
  int tach;
  void *data;
  size_t len;
} drive[2];

int read_disk(drive_t *, uint32_t base, uint32_t size, void *data);

// IOParam struct, A[0]
enum {
  qLink = 0x0,
  qType = 0x4,
  ioTrap = 0x6,
  ioCmdAddr = 0x8,
  ioCompletion = 0xc,
  ioResult = 0x10,
  ioNamePtr = 0x12,
  ioVRefNum = 0x16,   // driveno
  ioRefNum = 0x18,
  ioVersNum = 0x1a,
  ioPermssn = 0x1b,
  ioMisc = 0x1c,
  ioBuffer = 0x20,    // buffer
  ioReqCnt = 0x24,    // length
  ioActCnt = 0x28,
  ioPosMode = 0x2c,
  ioPosOffset = 0x2e, // offset

  csCode = 26,
  csParam = 28,
};

// DCtlEntry struct, A[1]
enum {
  dCtlDriver = 0,   // Ptr
  dCtlFlags = 4,    // short
  dCtlQHdr = 6,     // QHdr
  dCtlPosition = 16,// long
  dCtlStorage = 20, // Handle
  dCtlRefNum = 24,  // short
  dCtlCurTicks = 26,// long
  dCtlWindow = 30,  // GrafPtr
  dCtlDelay = 34,   // short
  dCtlEMask = 36,   // short
  dCtlMenu = 38,    // short
  dCtlSlot = 40,    // char
  dCtlSlotId = 41,  // char
  dCtlDevBase = 42, // long
  dCtlOwner = 46,   // Ptr
  dCtlExtDev = 50,  // char
  dCtlFillByte = 51,
  dCtlNodeID = 52
};

// DrvSts struct, passed to AddDrive
enum {
  dsTrack = 0,
  dsWriteProt     = 2,
  dsDiskInPlace   = 3,
  dsInstalled     = 4,
  dsSides         = 5,
  dsQLink         = 6,
  dsQType         = 10,
  dsQDrive        = 12,
  dsQRefNum       = 14,
  dsQFID          = 16,
  dsTwoSideFmt    = 18,
  dsNewIntf       = 19,
  dsMFMDrive      = 22,
  dsMFMDisk       = 23,
  dsTwoMegFmt     = 24,
};

enum {
  /* flags used in the driver header and device control entry */
  dNeedLockMask = 0x4000, /* set if driver must be locked in memory as
                             soon as it’s opened */
  dNeedTimeMask = 0x2000, /* set if driver needs time for performing
                             periodic tasks */
  dNeedGoodByeMask = 0x1000, /* set if driver needs to be called before the
                                application heap is initialized */
  dStatEnableMask = 0x0800, /* set if driver responds to status requests */
  dCtlEnableMask  = 0x0400, /* set if driver responds to control requests */
  dWritEnableMask = 0x0200, /* set if driver responds to write requests */
  dReadEnableMask = 0x0100, /* set if driver responds to read requests */
};

enum {
  noErr                 = 0,
  controlErr            = -17,
  statusErr             = -18,
  readErr               = -19,
  writErr               = -20,
  badUnitErr            = -21,
  unitEmptyErr          = -22,
  openErr               = -23,
  closErr               = -24,
  abortErr              = -27,
  notOpenErr            = -28,
  dskFulErr             = -34,
  nsvErr                = -35,
  ioErr                 = -36,
  bdNamErr              = -37,
  fnOpnErr              = -38,
  eofErr                = -39,
  posErr                = -40,
  tmfoErr               = -42,
  fnfErr                = -43,
  wPrErr                = -44,
  fLckdErr              = -45,
  fBsyErr               = -47,
  dupFNErr              = -48,
  paramErr              = -50,
  rfNumErr              = -51,
  permErr               = -54,
  nsDrvErr              = -56,
  extFSErr              = -58,
  noDriveErr            = -64,
  offLinErr             = -65,
  noNybErr              = -66,
  noAdrMkErr            = -67,
  dataVerErr            = -68,
  badCksmErr            = -69,
  badBtSlpErr           = -70,
  noDtaMkErr            = -71,
  badDCksum             = -72,
  badDBtSlp             = -73,
  wrUnderrun            = -74,
  cantStepErr           = -75,
  tk0BadErr             = -76,
  initIWMErr            = -77,
  twoSideErr            = -78,
  spdAdjErr             = -79,
  seekErr               = -80,
  sectNFErr             = -81,
  fmt1Err               = -82,
  fmt2Err               = -83,
  verErr                = -84,
  memFullErr            = -108,
  dirNFErr              = -120
};

static void show_addisk(int n, uint32_t mm)
{
  printf(" ---- adddrive: %.4x %.8x\n", n, mm);
  printf(" track:     %.8x\n", cpu_read16(mm + 0));
  printf(" wrprot:    %.8x\n", cpu_read8(mm + 2));   // dsWriteProt
  printf(" diskinplc: %.8x\n", cpu_read8(mm + 3));   // dsDiskInPlace
  printf(" installed: %.8x\n", cpu_read8(mm + 4));   // dsInstalled
  printf(" sides:     %.8x\n", cpu_read8(mm + 5));   // dsSides
  printf(" qlink:     %.8x\n", cpu_read32(mm + 6));
  printf(" qtype:     %.8x\n", cpu_read16(mm + 10)); // dsQType
  printf(" qdrive:    %.8x\n", cpu_read16(mm + 12)); 
  printf(" refnum:    %.8x\n", cpu_read16(mm + 14));
  printf(" qfsid:     %.8x\n", cpu_read16(mm + 16));
  printf(" twosdide:  %.8x\n", cpu_read8(mm + 18));  // dsTwoSideFmt
  printf(" newintf:   %.8x\n", cpu_read8(mm + 19));  // dsNewIntfn
  printf(" diskerrs:  %.8x\n", cpu_read16(mm + 20));
  printf(" mfmdrive:  %.8x\n", cpu_read8(mm + 22));  // dsMFMDrive
  printf(" mfmdisk:   %.8x\n", cpu_read8(mm + 23));  // dsMFMDisk
  printf(" twomegfmt: %.8x\n", cpu_read8(mm + 24));  // dsTwoMegFmt
}

static void show_cs(uint32_t mm)
{
  printf(" --- address: %.8x\n", mm);
  printf(" sts:qlink:      %.8x\n", cpu_read32(mm + qLink));
  printf(" sts:qtype:      %.4x\n", cpu_read16(mm + qType));
  printf(" sts:iotrap:     %.4x\n", cpu_read16(mm + ioTrap));
  printf(" sts:cmdaddr:    %.8x\n", cpu_read32(mm + ioCmdAddr));
  printf(" sts:completion: %.8x\n", cpu_read32(mm + ioCompletion));
  printf(" sts:result:     %.8x\n", cpu_read16(mm + ioResult));
  printf(" sts:nameptr:    %.8x\n", cpu_read32(mm + ioNamePtr));
  printf(" sts:vrefnum:    %.8x\n", cpu_read16(mm + ioVRefNum));
  printf(" sts:refnum:     %.8x\n", cpu_read16(mm + ioRefNum));
  printf(" sts:csCode:     %.8x\n", cpu_read16(mm + csCode));
}

static void show_io(uint32_t mm)
{
  printf(" --- address: %.8x\n", mm);
  printf(" Read:qlink:      %.8x\n", cpu_read32(mm + qLink));
  printf(" Read:qtype:      %.4x\n", cpu_read16(mm + qType));
  printf(" Read:iotrap:     %.4x\n", cpu_read16(mm + ioTrap));
  printf(" Read:cmdaddr:    %.8x\n", cpu_read32(mm + ioCmdAddr));
  printf(" Read:completion: %.8x\n", cpu_read32(mm + ioCompletion));
  printf(" Read:result:     %.8x\n", cpu_read16(mm + ioResult));
  printf(" Read:nameptr:    %.8x\n", cpu_read32(mm + ioNamePtr));
  printf(" Read:vrefnum:    %.8x\n", cpu_read16(mm + ioVRefNum));
  printf(" Read:refnum:     %.8x\n", cpu_read16(mm + ioRefNum));
  printf(" Read:versnum:    %.8x\n", cpu_read8(mm + ioVersNum));
  printf(" Read:permssn:    %.8x\n", cpu_read8(mm + ioPermssn));
  printf(" Read:misc:       %.8x\n", cpu_read32(mm + ioMisc));
  printf(" Read:actcnt:     %.8x\n", cpu_read32(mm + ioActCnt));
  printf(" Read:posmode:    %.8x\n", cpu_read16(mm + ioPosMode));
  printf(" Read:pos:        %.8x\n", cpu_read32(mm + ioPosOffset));
  printf(" Read:buf:        %.8x\n", cpu_read32(mm + ioBuffer));
  printf(" Read:size:       %.8x\n", cpu_read32(mm + ioReqCnt));
}

static void show_dce(uint32_t mm)
{
  printf(" ---- dce: %.8x\n", mm);
  printf(" driver:          %.8x\n", cpu_read32(mm + dCtlDriver));
  printf(" flags:           %.8x\n", cpu_read16(mm + dCtlFlags));
  printf(" xx0:             %.8x\n", cpu_read32(mm + 6));
  printf(" xx0:             %.8x\n", cpu_read32(mm + 10));
  printf(" xx0:             %.8x\n", cpu_read16(mm + 14));
  printf(" position:        %.8x\n", cpu_read32(mm + dCtlPosition));
  printf(" storage:         %.8x\n", cpu_read32(mm + dCtlStorage));
  printf(" refnum:          %.8x\n", cpu_read16(mm + dCtlRefNum));
  printf(" curticks:        %.8x\n", cpu_read32(mm + dCtlCurTicks));
  printf(" window:          %.8x\n", cpu_read32(mm + dCtlWindow));
  printf(" delay:           %.8x\n", cpu_read16(mm + dCtlDelay));
  printf(" emask:           %.8x\n", cpu_read16(mm + dCtlEMask));
  printf(" menu:            %.8x\n", cpu_read16(mm + dCtlMenu));
  printf(" slot:            %.8x\n", cpu_read8(mm + dCtlSlot));
  printf(" slotid:          %.8x\n", cpu_read8(mm + dCtlSlotId));
}

static void setDrive(int n, void *ptr, size_t len)
{
  if (n < 0 || n > 1) {
    return;
  }
  drive_t *drv = &drive[n];
  if (ptr == NULL) {
    drv->installed = false;
  } else {
    drv->installed = true;
    drv->present = 0x0;
    drv->sides = 2;
    drv->data = ptr;
    drv->len = len;
    drv->motor = false;
    drv->track = 0;
    drv->trackdir = 0;
    drv->tach = 0;
  }
}

struct iwm_t : public gendev_t {
  uint8_t iwm = 0;
  uint8_t sel = 0;
  uint8_t status = 0;
  uint8_t mode = 0;

  enum {
    CA0   = D0,
    CA1   = D1,
    CA2   = D2,
    LSTRB = D3,
    MOTOR = D4,
    SEL   = D5,
    Q6    = D6,
    Q7    = D7,

    DRTIN = 0x0,
    RDDATA0 = 0x01,
    CSTIN = 0x02,
    RDDATA1 = 0x03,
    STEP = 0x04,
    WRTPRT = 0x05,
    MOTORON = 0x08,
    SIDES = 0x09,
    TK0 = 0x0a,
    READY = 0x0b,
    SWITCHED = 0x0c,
    INSTALLED = 0x0d,
    TACH = 0x0e,
    DRVIN = 0x0f,
  };
  static uint8_t iwm_gpio(void *arg, uint8_t v, int);

  int io(uint32_t addr, int mode, iodata_t& val);

  int iwm_rw(uint32_t addr, uint8_t v, int mode);

  int iwm_reg();

  void init(bus_t *b);
};

/* Serial Port:
 * a23 a22 a21 a20
 *   1   0   0   0 0080.0000 008F.FFFF rom
 *   1   0   0   1 0090.0000 009F.FFFF scc_read
 *   1   0   1   0 00A0.0000 00AF.FFFF rom
 *   1   0   1   1 00B0.0000 00BF.FFFF scc_write
 *
 *  a2  a1  a0
 *   x   x   0  LDS=0
 *   x   x   1  LDS=1
 *   x   0   x  ChannelB
 *   x   1   x  ChannelA
 *   0   x   x  Control
 *   1   x   x  Data
 *
 * Read:  read a0=0
 * Reset: read a0=1
 * Write:write a0=1
 *
 */
struct scc_t : public gendev_t {
  int io(uint32_t addr, int mode, iodata_t& val);
  void init(bus_t *b);
};

struct scsi_t : public gendev_t {
  int io(uint32_t addr, int mode, iodata_t& val);
  void init(bus_t *);
};

struct mac68k : public bus_t, crtc_t {
  Screen *scr;

  via_t via;
  iwm_t iwm;
  scc_t scc;
  scsi_t scsi;

  void init(uint8_t *rom, int len);
  void run();

  uint8_t *overlay;
  uint8_t *ram;
  uint8_t *rombase;
  uint32_t romsz;

  bool vid_tick();
  void drawframe();

  void sethblank(bool);
  void setvblank(bool);
  void check_irq();

  int oldbtn;
};

mac68k sys;

void dumpmem(uint32_t addr, uint32_t len) {
  addr &= 0x00ffffff;
  if (addr >= 0x00400000)
    return;
  hexdump(sys.ram + addr, len, 16);
}

/* 60.14 Hz
 * 22255 Hz line
 * 512 / 16 = 32 words
 * 32 + 32 + 24 = 88 words per line
 * 370 lines
 */
#define HBLANK    512
#define HORIZ_MAX (HBLANK+24*8)

#define VBLANK    342
#define VERT_MAX  (VBLANK+28)

static time_t fpstime = time(NULL);

void mac68k::drawframe()
{
  uint8_t *vidram;
  time_t now;
  float fps;

  //video_base = cpu_read32be(0x148);
  printf("video base: %x\n", video_base);
  vidram = &ram[video_base];
  
  now = time(NULL);
  fps = (float)frame / (now - fpstime);
  draw_bpp(scr, HBLANK, VBLANK, vidram, 1, 1);
  scr->scrtext(0, VBLANK + 30, 3, "frame:%d fps:%.2f PC:%.8x %.8x", frame, fps, SPC, cpu_read32be(0x0824, 0));

  scr->draw();
  scr->clear();
}

/* IRQ Level:
 *   level 1: VIA
 *   level 2: SCC
 *   level 3: VIA+SCC
 *   level 4: debug switch
 *   level 5: debug switch+VIA
 *   level 6: debug switch+SCC
 *   level 7: debug switch+VIA+SCC
 */
void mac68k::check_irq()
{
  int sts;

  sts = via.ifr & via.ier;
  if (sts & (IFR_VSYNC|IFR_TMR1|IFR_TMR2|IFR_SR)) {
    cpu_irq(1);
  }
}

void mac68k::sethblank(bool state) {
}

void mac68k::setvblank(bool state) {
  if (state) {
    via.set_ifr(IFR_VSYNC, true, "vsync");
  }
}


#if 0
static void pushregs()
{
  for (int i = 0; i < 16; i++) {
    cpu_push32(regs[i]);
  }
}

static void popregs() {
  for (int i = 15; i >= 0; i--) {
    regs[i] = cpu_pop32();
  }
}
#endif

void dumpr(uint8_t *ram, int off, const char *name) {
  if (off >= 0x100 && off < 0x1000) {
    printf("%-20s %.8x\n", name, get32be(&ram[off]));
  }
}

/* P_SCCInt_AChng_MouseH:
 *  bit4 = x2, +-082a byte
 * P_SCCInt_BChng_MouseV:
 *  bit5 = y2, +-0828 byte
 * MBState = 0172
 */
// 342 x (32 words = pixels, 5 right border, 2 sync level, 5 left border) = 342x44
// 12 lines border, 3 sync, 11 border = 26 x 44
// 16192 cycles per frame?
int old_mousex;
int old_mousey;
bool mac68k::vid_tick()
{
  if (via.t1.tick()) {
    via.set_ifr(IFR_TMR1, true, "timer1");
  }
  if (via.t2.tick()) {
    via.set_ifr(IFR_TMR2, true, "timer2");
  }
  if (crtc_t::tick()) {
    printf("-- frame: %d tick:%x\n", frame, cpu_read32(0x16a));

    if (scr->key('a', true)) {
      dumpvec(0x0);
    }
    // mbstate
    int mouse_x, mouse_y, mouse_btn;

    mouse_btn = scr->getmouse(mouse_x, mouse_y);
    cpu_write16(0x172, mouse_btn ? 0xffff : 0x0);

    // set mouse position
    cpu_write16(0x828, mouse_y); // MTemp
    cpu_write16(0x82a, mouse_x);
    cpu_write16(0x82c, mouse_y); // RawMouse
    cpu_write16(0x82e, mouse_x);

    if (mouse_x != old_mousex ||
	mouse_y != old_mousey) {
      cpu_write8(0x8ce, cpu_read8(0x8cf));
    }
    old_mousex = mouse_x;
    old_mousey = mouse_y;
#if 0    
    hexdump(&ram[0x800], 512);
    dumpvec(0x0);
#define o(addr, xx, name, str) dumpr(ram, addr, #name);
    CHIPREG(o)
#undef o
#endif
    //cpu_write8(0x8ce, cpu_read8(0x8cf)); // 

    printf("button: %x %x\n", oldbtn, mouse_btn);
#if 0
    if (oldbtn != mouse_btn) {
      uint32_t adb = cpu_read32(0xcf8);

      printf("adb: %x\n", adb);
      dumpmem(adb, 512);
      cpu_write8(adb + 0x163, 2);
      cpu_write8(adb + 0x164, (mouse_btn & 1) ? 0x80 : 0x0);
      cpu_write8(adb + 0x165, (mouse_btn & 2) ? 0x80 : 0x0);
      oldbtn = mouse_btn;
      dumpvec(0x0);
      printf("ifr: %.2x %.2x\n", via.ifr, via.ier);
      pushregs();
      A[0] = adb + 0x163;
      A[1] = cpu_read32(adb + 16);
      A[2] = 0x0;
      A[3] = adb;
      D[0] = (0x63 << 4) | 0xc;
      printf("Mouse_Base: %.8x %.8x\n", A[1], A[2]);
      trace = 3;
      runcode(A[1] & 0xffffff);
      trace = 0;
      popregs();
      via.ifr = 0x84;
    }
#endif
    drawframe();
    frame++;
    return true;
  }
  return false;
}

int scc_t::io(uint32_t addr, int mode, iodata_t& val)
{
  val = random();
  flogger(0, "scc_io: %.8x [%s] %c %.8x\n", addr, iorname(addr), mode, val);
  return 0;
}

void scc_t::init(bus_t *b) {
  b->register_handler(0x900000, 0x9FFFFF, 0xFFFFFF, gendev_t::io, this, _RD, "SCC.read");
  b->register_handler(0xB00000, 0xBFFFFF, 0xFFFFFF, gendev_t::io, this, _WR, "SCC.write");
}

int scsi_t::io(uint32_t addr, int mode, iodata_t& val)
{
  static uint32_t flag;

  flag ^= -1;
  if (mode == 'r')
    val = flag;
  flogger(0, "scsi_io: %.8x [%s] %c %.8x\n", addr, iorname(addr), mode, val);
  return 0;
}

void scsi_t::init(bus_t *b) {
  b->register_handler(0x580000, 0x5FFFFF, 0xFFFFFF, gendev_t::io, this, _RW, "SCSI");
}

/* read:
 *  ---r.rrv-.----.----              bit
 *  e1 0.0001 000: ca0=0  phase0.lo  _______0
 *  e3 0.0011 200: ca0=1  phase0.hi  _______1
 *  e5 0.0101 400: ca1=0  phase1.lo  ______0_
 *  e7 0.0111 600: ca1=1  phase1.hi  ______1_
 *  e9 0.1001 800: ca2=0  phase2.lo  _____0__
 *  eb 0.1011 a00: ca2=1  phase2.hi  _____1__
 *  ed 0.1101 c00:        phase3.lo  ____0___
 *  ef 0.1111 e00:        phase3.hi  ____1___ 0->1 write drive reg
 *  f1 1.0001 1000:       motor.off  ___0____ (disable)
 *  f3 1.0011 1200:       motor.on   ___1____ (enable)
 *  f5 1.0101 1400:       sel.1      __0_____
 *  f7 1.0111 1600:       sel.2      __1_____
 *  f9 1.1001 1800:       q6.lo      _0______
 *  fb 1.1011 1a00:       q6.hi      _1______
 *  fd 1.1101 1c00:       q7.lo      0_______ 
 *  ff 1.1111 1e00:       q7.hi      1_______
 *
 *  00-0---- : read all 1s
 *  00-10ddd : read data register
 *  01-x---- : read status register
 *  10-x---- : read write handshake register
 *  11-0---- : write mode register (if motor.off)
 *  11-11ddd : write data register (if motor.on)
 *
 * [ca1,ca0,sel,ca2] = register
 *   00 0000 = drtin
 *   01 0001 = rddata0
 *   02 0010 = cstin [0=disk inserted]
 *   03 0011 = rddata1
 *   04 0100 = step [1=step done]
 *   05 0101
 *   06 0110 = wrtprt [0=locked]
 *   07 0111
 *   08 1000 = motoron [0=on]
 *   09 1001 = sides
 *   0a 1010 = tk0 [0=at track 0]
 *   0b 1011 = ready
 *   0c 1100
 *   0d 1101 = installed [0=yes]
 *   0e 1110 = tach
 *   0f 1111 = drvin
 *
 *  Write regs:
 *   0000 00 drtin=0 (toward higher tracks)
 *   0001 01 drtin=1 (toward lower tracks)
 *   0011 03
 *   0100 04 step 1 track in current direction
 *   1000 08 turn on motor
 *   1001 09 turn off motor
 *   1101 0b eject drive
 *
 * +---+---+---+---+---+---+---+---+
 * | - | - | - | S | C | M | H | L | Mode register
 * +---+---+---+---+---+---+---+---+
 * | I | - | E | S | C | M | H | L | Status register
 * +---+---+---+---+---+---+---+---+
 */
uint8_t iwm_t::iwm_gpio(void *arg, uint8_t v, int mode) {
  iwm_t *c = (iwm_t *)arg;

  if (mode == 'w') {
    c->sel = !!(v & PA5);
  }
  printf("iwm_gpio: %.2x.%x\n", c->iwm, c->sel);
  return 0;
}

const char *iwmreg[] = {
  "DRTIN",  "RDDAT0", "CSTIN",  "RDDAT1",
  "STEP",   "??5",    "WRTPRT", "??7",
  "MOTORON","SIDES",  "TK0",    "READY",
  "SWITCHED?","INSTALLED", "TACH", "DRVIN",
};

int iwm_t::iwm_reg() {
  int reg;

  // CA1 CA0 --- ---
  reg  = (iwm & 3) << 2;
  // CA1 CA0 SEL ---
  reg |= (sel & 1) << 1;
  // CA1 CA0 SEL CA2
  reg |= (iwm >> 2) & 1;

  return reg;
};

const char *iwm_regname(iwm_t *c)
{
  int reg = c->iwm_reg();
  
  return iwmreg[reg];
}

int iwm_t::iwm_rw(uint32_t addr, uint8_t val, int mode) {
  drive_t *drv = &drive[iwm & SEL ? 1 : 0];
  int reg = 0;

  reg = iwm_reg();
  flogger(0, "---> [%.8x] %s to drive %s %s\n",
          addr & (0xF << 9),
          mode == 'w' ? "write" : "read",
          iwm & SEL ? "EXT" : "INT",
          iwm & MOTOR ? "ON " : "OFF");

  // 1101.xxxx
  switch (iwm & 0xD0) {
  case 0x00:
    // motor=0, q7=0, q6=0, read 1s
    return 0xff;
  case 0x010:
    // motor=1, q7=0, q6=0, read data
    break;
  case 0x40:
    // motor=0, q7=0, q6=0, read status
    val = (status & 0x7F);
    val |= drv->installed ? 0x80 : 0x00;
    printf("  read status: %x\n", val);
    break;
  case 0x50:
    // motor=0, q7=0, q6=0, read status
    val = 0x80;
    switch (reg) {
    case SIDES:
      // 0 = single, 1 = double
      val = drv->sides == 1 ? 0x00 : 0xff;
      break;
    case INSTALLED:
      val = drv->installed ? 0x00 : 0xff;
      break;
    case DRVIN:
      val = drv->present;
      break;
    case CSTIN:
      // 0 = disk inserted
      val = drv->installed ? 0x00 : 0xff;
      break;
    case MOTORON:
      // 0 = on, 1 = off
      val = drv->motor ? 0x00 : 0xff;
      break;
    case STEP:
      // 0 = stepping, 1 = step done
      val = drv->trackdir != 0 ? 0x00 : 0xff;
      break;
    case WRTPRT:
      // 0=locked, 1=unlocked
      val = 0xff;
      break;
    case TK0:
      val = drv->track == 0 ? 0x00 : 0xff;
      break;
    case TACH:
      val = (drv->tach <= 1) ? 0x00 : 0xff;
      drv->tach = (drv->tach + 1) & 3;;
      break;
    }
    flogger(0,"  read data: %x [%s]\n",
            val, iwm_regname(this));
    break;
  case 0x80:
  case 0x90:
    // read handshake
    break;
  case 0xC0:
    // write mode
    status &= ~0x1F;
    status |= (val & 0x1f);
    printf("  write mode: %x\n", val);
    break;
  case 0xD0:
    // write data
    break;
  }
  return val;
};

int iwm_t::io(uint32_t addr, int mode, iodata_t& val)
{
  int m = 1L << ((addr >> 10) & 7);
  int reg = 0;
  int old = iwm;
  
  /* Set IWM bit */
  reg = iwm_reg();
  if ((addr >> 9) & 1) {
    iwm |= m;
  }
  else {
    iwm &= ~m;
  }
  if (!(old & LSTRB) && (iwm & LSTRB)) {
    printf("=================== write reg: %x\n", reg);
  }
  if (mode == 'r') {
    if (addr == IWM_PHASE3_HI) {
      drive_t *drv = &drive[iwm & SEL ? 1 : 0];
      flogger(0, "---> write to drive %s %s reg:%x val:%x\n",
              iwm & SEL ? "EXT" : "INT",
              iwm & MOTOR ? "ON " : "OFF",
              reg, reg & 1);
      switch(reg) {
      case 0x0: drv->trackdir = 1; break;
      case 0x1: drv->trackdir = -1; break;
      case 0x4:
        drv->track = clip(drv->track + drv->trackdir, 0, 80);
        if (drv->track == 0 || drv->track == 80)
          drv->trackdir = 0;
        break;
      case 0x8: drv->motor = true;  break; // motor on
      case 0x9: drv->motor = false; break; // motor off
      }
    }
    else {
      val = iwm_rw(addr, 0, 'r');
    }
  }
  else {
    iwm_rw(addr, val, 'w');
  }
  return 0;
}

void iwm_t::init(bus_t *b) {
  b->register_handler(0xD00000, 0xDFFFFF, 0xFFFFFF, gendev_t::io, this, _RW, "IWM");
}

/* odd counter/latch writes:
 *   04 : t1.latch_lo     rd t1.ctr.lo
 *   05 : t1.latch_hi     rd t1.ctr.hi
 *        t1.ctr_hi
 *        t1.ctr.lo=t1.latch.lo
 *        reset ifr
 *   06 : t1.latch_lo     rd t1.latch.lo
 *   07 : t1.latch_hi     rd t1.latch.hi
 *   08 : t2.latch_lo
 *   09 : t2.counter_hi
 */
int via_t::io(uint32_t addr, int mode, iodata_t& val)
{
  uint8_t old;

  if (mode == 'w') {
    switch (addr) {
    case VIA_IORB:
      /* Write Port B */
      old = portb.write(val);
      clr_ifr(IFR_CB1|IFR_CB2, "iorb.w");
      if (val & RTC_EN) {
        /* enable */
        rtc_idx = 0;
        rtc_data = 0;
      }
      else if ((portb.ddr & RTC_DATA) != 0 && (val & RTC_CLOCK) == 0) {
        /* tick data out */
        rtc_data = (rtc_data << 1) | (portb.ostate & 0x1);
        if (++rtc_idx == 8) {
          printf("Got RTC: %.2x\n", rtc_data);
          rtc_idx = 0;
        }
      }
      else if ((portb.ddr & RTC_DATA) == 0 && (val & RTC_CLOCK) == 0) {
        /* tick data in */
        portb.istate &= 0xFE;
        portb.istate |= (rtc_data >> 7);
        rtc_data <<= 1;
      }
      break;
    case VIA_IORAH:
    case VIA_IORA:
      /* Write Port A */
      old = porta.write(val);
      clr_ifr(IFR_CA1|IFR_CA2, "iora.w");
      if (old & OVL) {
        printf("OVERLAY!\n");
	m->overlay = m->ram;
      }
      break;
    case VIA_DDRB:
      /* Write Port B direction */
      if ((portb.ddr & RTC_DATA) && !(val & RTC_DATA)) {
        printf("rtc in: %x\n", rtc_data);
        rtc_data = 0xa8;
      }
      portb.write_ddr(val);
      break;
    case VIA_DDRA:
      /* Write Port A direction */
      porta.write_ddr(val);
      break;
    case VIA_IER:
      /* Set interrupt Enable  */
      set_ier(val & 0x7f, val & 0x80);
      break;
    case VIA_IFR:
      /* Set interrupt Request */
      set_ifr(val & 0x7f, val & 0x80, "ifr");
      break;
    case VIA_TALO:
      rmw(t1_latch, val, 0x00FF, "talo");
      printf("set timer: %x\n", t1_latch);
      t1.settimer(t1_latch, true, true, "timer1");
      break;
    case VIA_TBLO:
      rmw(t2_latch, val, 0x00FF, "tblo");
      printf("set timer: %x\n", t2_latch);
      t2.settimer(t2_latch, true, true, "timer2");
      break;
    case VIA_TAHI:
      clr_ifr(IFR_TMR1, "tmr1");
      rmw(t1_latch, val << 8, 0xFF00, "tahi");
      break;
    case VIA_TBHI:
      clr_ifr(IFR_TMR2, "tmr2");
      rmw(t2_latch, val << 8, 0xFF00, "tbhi");
      break;
    case VIA_SHIFT: // 0x08
      flogger(0, "Write To Shift: %x, aux:%x, %x\n", pci, aux, val);
      shift = val;
      break;
    case VIA_AUX: // 0x09
      aux = val;
      break;
    case VIA_PCI: // 0xa
      pci = val;
      break;
    default:
      flogger(0, "via_io: %.8x [%s] %c %.8x [%d,%d]\n", addr, iorname(addr), mode, val, sys.hPos, sys.vPos);
      break;
    }
  }
  else if (mode == 'r') {
    switch (addr) {
    case VIA_IORA:
      /* Read Port A */
      val = porta.read();
      break;
    case VIA_IORB:
      /* Read Port B */
      val = portb.read();
      break;
    case VIA_DDRA:
      /* Read Port A ddr */
      val = porta.read_ddr();
      break;
    case VIA_DDRB:
      /* Read Port B ddr */
      val = portb.read_ddr();
      break;
    case VIA_IER:
      /* Read Interrupt Enable */
      val = ier;
      break;
    case VIA_IFR:
      /* Read Interrupt Request */
      val = ifr | 0x80;
      break;
    case VIA_SHIFT:
      flogger(0, "read shift: %x aux:%x %x\n", pci, aux, shift);
      val = rand();
      break;
    default:
      flogger(0, "via_io: %.8x [%s] %c %.8x [%d,%d]\n", addr, iorname(addr), mode, val, sys.hPos, sys.vPos);
      break;
    }
  }
  return 0;
}

void via_t::init(mac68k *mac) {
  mac->register_handler(0xE80000, 0xEFFFFF, 0xFFFFFF, gendev_t::io, this,  _RW, "VIA");
  porta.init(0b01111111, OVL, 0); // overlay
  portb.init(0b10000110, 0, 0);
  porta.set_rwcb(iwm_t::iwm_gpio, &mac->iwm);
  m = mac;
}

static int ovrly_io(void *arg, uint32_t addr, int mode, iodata_t& val)
{
  mac68k *m = (mac68k *)arg;

  if (mode == 'w') {
    flogger(0, "owrite: %.8x << %.8x %c\n", addr, val, mode);
  }
  return memio(m->overlay, addr, mode, val);
}

static int phase_io(void *arg, uint32_t addr, int mode, iodata_t& val)
{
  static uint32_t phase = 1;

  printf("read phase: %.8x %x\n", addr, phase);
  if ((addr & 1) == 1) {
    val = phase;
    phase++;
  }
  else {
    val = phase >> 8;
  }
  return 0;
}

static int vector_io(void *arg, uint32_t addr, int mode, iodata_t& val)
{
  printf("Vector io: %x\n", addr);
  return 0;
}

void mac68k::init(uint8_t *rom, int len)
{
  ram = new uint8_t[4096 * 1024]{0};
  overlay = rom;

  rombase = rom;
  romsz   = (len + 0xFFFF) & ~0xFFFF;

  palclr macpal[] = {
    { 0x00, 0x00, 0x00 },  // black
    { 0xff, 0xff, 0xff },  // white
    { 0x00, 0x00, 0x00 },  // black
    { 0x00, 0xff, 0x00 },  // green
  };
  crtc_t::init(512, HORIZ_MAX - 512, 342, VERT_MAX - 342);
  
  scr = new Screen(HBLANK, VBLANK, 30, 50, 4, macpal);
  scr->xs = 2;
  scr->ys = 2;
  scr->init();

  printf("ROM SIZE: %x\n", romsz);
  hexdump(rom, 256);

  bus_t::init(0xFFFFFF);
  register_handler(0x000000, 0x3FFFFF,0xFFFFFF, ovrly_io, this,  _RW, "ROM:RAM");
  register_handler(0x400000, 0x41FFFF,0x01FFFF, memio,    rom,   _RD, "ROM");
  register_handler(0xF00000, 0xF7FFFF,0xFFFFFF, phase_io, this,  _RD, "PHASE READ");
  register_handler(0xFFFFF0, 0xFFFFFF,0xFFFFFF, vector_io,this,  _RD, "AUTO VECTOR");

  via.init(this);
  scsi.init(this);
  iwm.init(this);
  scc.init(this);

  dumpvec(0x0);
}

static void *ioptr(uint32_t addr, int mode)
{
  addr &= 0xFFFFFF;
  if (addr <= 0x003FFFFF && sys.overlay == sys.ram) {
    return &sys.ram[addr];
  }
  return NULL;
}

/* CPU memory read/write */
uint8_t cpu_read8(uint32_t addr, int type) {
  void *ptr;

  addr &= 0xffffff;
  if ((ptr = ioptr(addr, 'r')) == NULL) {
    iodata_t v = 0;

    sys.read(addr, v);
    return v;
  }
  return get8(ptr);
}

uint16_t cpu_read16(uint32_t addr, int type) {
  void *ptr;
  uint16_t v;

  addr &= 0xffffff;
  if ((ptr = ioptr(addr, 'r')) == NULL) {
    v = cpu_read16be(addr, type);
  }
  else {
    v =  get16be(ptr);
  }
  if (type == dstk::CODE)
    open_bus = v;
  return v;
}

uint32_t cpu_read32(uint32_t addr, int type) {
  void *ptr;

  addr &= 0xffffff;
  if ((ptr = ioptr(addr, 'r')) == NULL) {
    return cpu_read32be(addr, type);
  }
  return get32be(ptr);
}

void cpu_write8(uint32_t addr, uint8_t v, int type) {
  void *ptr;

  if (addr >= audio_base && addr <= (audio_base + 0x2e4)) {
    // write sound file
    writesnd(v);
  }
  if ((ptr = ioptr(addr, 'w')) != NULL) {
    put8(ptr, v);
  }
  else {
    sys.write(addr, v);
  }
}

void cpu_write16(uint32_t addr, uint16_t v, int type) {
  void *ptr;

  if ((ptr = ioptr(addr, 'w')) != NULL) {
    put16be(ptr, v);
  }
  else {
    cpu_write16be(addr, v, type);
  }
}

void cpu_write32(uint32_t addr, uint32_t v, int type) {
  void *ptr;

  if ((ptr = ioptr(addr, 'w')) != NULL) {
    put32be(ptr, v);
  }
  else {
    cpu_write32be(addr, v, type);
  }
}

void cpu_reset(uint32_t addr)
{
  if (addr == 0) {
    //addr = cpu_read32(0x04 + 0x00400000);
    addr = cpu_read32(0x04);
  }
  printf("reset addr: %8x\n", addr);
  PC = addr;
  Sf = 1;
}

int elvl;

void runtrap(uint16_t op)
{
  uint32_t opc = SPC;
  uint16_t oldc;
  
  PC = 0xFE;
  oldc = cpu_read16(0xFE);
  cpu_write16(0xFE, op);
  while (PC != 0x100) {
    printf("check: %x %x [%.4x]\n", PC, opc, op);
    SPC = PC;
    op = cpu_fetch(Word);
    decode_68k(op);
    sys.vid_tick();
  }
  cpu_write16(0xFE, oldc);
  PC = SPC;
}

void runcode(uint32_t npc) {
  uint32_t opc = PC;
  uint16_t op;

  cpu_showregs();
  flogger(0, "runcode @ %x\n", npc);
  m68k_setpc(npc, true, "runcode");
  cpu_showregs();
  while (PC != opc) {
    printf("2check: %x %x [%.4x]\n", PC, opc, op);
    SPC = PC;
    op = cpu_fetch(Word);
    decode_68k(op);
    sys.vid_tick();
  }
};

/* PB == show_io */
void set_drv_err(int n)
{
  cpu_write16(z_DskErr, n);
  D[0] = n;
}

// IoParamBlock = A0
// DCTL = A1
static uint32_t dskmap[32];

// Floppy disk icon
static int SonyDiskAddr;
static const uint8_t SonyDiskIcon[258] = {
  0x7f, 0xff, 0xff, 0xf8, 0x81, 0x00, 0x01, 0x04, 0x81, 0x00, 0x71, 0x02, 0x81, 0x00, 0x89, 0x01,
  0x81, 0x00, 0x89, 0x01, 0x81, 0x00, 0x89, 0x01, 0x81, 0x00, 0x89, 0x01, 0x81, 0x00, 0x89, 0x01,
  0x81, 0x00, 0x71, 0x01, 0x81, 0x00, 0x01, 0x01, 0x80, 0xff, 0xfe, 0x01, 0x80, 0x00, 0x00, 0x01,
  0x80, 0x00, 0x00, 0x01, 0x80, 0x00, 0x00, 0x01, 0x80, 0x00, 0x00, 0x01, 0x80, 0x00, 0x00, 0x01,
  0x80, 0x00, 0x00, 0x01, 0x80, 0x00, 0x00, 0x01, 0x87, 0xff, 0xff, 0xe1, 0x88, 0x00, 0x00, 0x11,
  0x88, 0x00, 0x00, 0x11, 0x88, 0x00, 0x00, 0x11, 0x88, 0x00, 0x00, 0x11, 0x88, 0x00, 0x00, 0x11,
  0x88, 0x00, 0x00, 0x11, 0x88, 0x00, 0x00, 0x11, 0x88, 0x00, 0x00, 0x11, 0x88, 0x00, 0x00, 0x11,
  0x88, 0x00, 0x00, 0x11, 0x88, 0x00, 0x00, 0x11, 0x88, 0x00, 0x00, 0x11, 0x7f, 0xff, 0xff, 0xfe,

  0x7f, 0xff, 0xff, 0xf8, 0xff, 0xff, 0xff, 0xfc, 0xff, 0xff, 0xff, 0xfe, 0xff, 0xff, 0xff, 0xff,
  0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
  0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
  0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
  0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
  0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
  0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
  0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x7f, 0xff, 0xff, 0xfe,

  0, 0
};

// Floppy drive icon
static int SonyDriveAddr;
static const uint8_t SonyDriveIcon[258] = {
  0x7f, 0xff, 0xff, 0xf8, 0x81, 0x00, 0x01, 0x04, 0x81, 0x00, 0x71, 0x02, 0x81, 0x00, 0x89, 0x01,
  0x81, 0x00, 0x89, 0x01, 0x81, 0x00, 0x89, 0x01, 0x81, 0x00, 0x89, 0x01, 0x81, 0x00, 0x89, 0x01,
  0x81, 0x00, 0x71, 0x01, 0x81, 0x00, 0x01, 0x01, 0x80, 0xff, 0xfe, 0x01, 0x80, 0x00, 0x00, 0x01,
  0x80, 0x00, 0x00, 0x01, 0x80, 0x00, 0x00, 0x01, 0x80, 0x00, 0x00, 0x01, 0x80, 0x00, 0x00, 0x01,
  0x80, 0x00, 0x00, 0x01, 0x80, 0x00, 0x00, 0x01, 0x87, 0xff, 0xff, 0xe1, 0x88, 0x00, 0x00, 0x11,
  0x88, 0x00, 0x00, 0x11, 0x88, 0x00, 0x00, 0x11, 0x88, 0x00, 0x00, 0x11, 0x88, 0x00, 0x00, 0x11,
  0x88, 0x00, 0x00, 0x11, 0x88, 0x00, 0x00, 0x11, 0x88, 0x00, 0x00, 0x11, 0x88, 0x00, 0x00, 0x11,
  0x88, 0x00, 0x00, 0x11, 0x88, 0x00, 0x00, 0x11, 0x88, 0x00, 0x00, 0x11, 0x7f, 0xff, 0xff, 0xfe,

  0x7f, 0xff, 0xff, 0xf8, 0xff, 0xff, 0xff, 0xfc, 0xff, 0xff, 0xff, 0xfe, 0xff, 0xff, 0xff, 0xff,
  0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
  0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
  0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
  0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
  0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
  0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
  0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x7f, 0xff, 0xff, 0xfe,

  0, 0
};

static void sony_open(uint32_t pb, uint32_t dce)
{
  uint32_t v, mm = 0;

  dce &= 0xFFFFFF;
  show_dce(dce);
  
  v = cpu_read32(0x11c);
  printf("Utab: %.8x %.8x %.8x\n", v,
         cpu_read32(v + 4),
         cpu_read32(v + 16));
  cpu_write32(v + 4, cpu_read32(v + 16));
  cpu_write32(z_SonyVars, 0xdeadbee0);

  // Allocate system memory
  D[0] = 0x500;
  runtrap(0xa71e); // NewPtrSysClear
  mm = A[0];

  // Copy in icon bitmaps
  dskmap[0] = mm;
  SonyDiskAddr = mm + 0x200;
  SonyDriveAddr = mm + 0x310;
  memcpy(sys.ram + SonyDiskAddr, SonyDiskIcon, sizeof(SonyDiskIcon));
  memcpy(sys.ram + SonyDriveAddr, SonyDriveIcon, sizeof(SonyDriveIcon));

  printf("GOT POINTER: %x\n", mm);
#if 1
  cpu_write8(mm + dsInstalled, 1);
  cpu_write8(mm + dsSides, 0xff);
#else
  cpu_write16(mm + dsQType, 0);
  cpu_write8(mm + dsInstalled, 1);
  cpu_write8(mm + dsSides, 0xFF);
  cpu_write8(mm + dsTwoSideFmt, 0xff);
  cpu_write8(mm + dsNewIntf, 0xff);
  cpu_write8(mm + dsMFMDrive, 0xff);
  cpu_write8(mm + dsMFMDisk, 0xff);
  cpu_write8(mm + dsTwoMegFmt, 0xff);
  cpu_write8(mm + dsDiskInPlace, 1);
  cpu_write8(mm + dsWriteProt, 0xff);
#endif
  D[0] = (1 << 16) | 0xfffb;
  A[0] = mm + dsQLink;
  runtrap(0xa04e); // AddDrive
}

static void sony_prime(uint32_t pb, uint32_t dce)
{
  uint32_t mm = 0;
  
  pb &= 0xFFFFFF;
  dce &= 0xFFFFFF;
  show_io(pb);
  show_dce(dce);
  
  cpu_write32(pb + ioActCnt, 0);

  // set disk in place to 2
  mm = dskmap[0];
  cpu_write8(mm + dsDiskInPlace, 2);
  show_addisk(0, mm);

  uint32_t pos = cpu_read32(pb + ioPosOffset);
  uint32_t len = cpu_read32(pb + ioReqCnt);
  uint32_t buf = cpu_read32(pb + ioBuffer);

  if ((cpu_read16(pb + ioTrap) & 0xff) == 0x02) {
    printf("pos, len, buf: %.8x %.8x %.8x\n", pos, len, buf);
    read_disk(&drive[0], pos, len, &sys.ram[buf]);

    cpu_write32(pb + ioActCnt, len);
    cpu_write32(pb + ioResult, 0x0);
    //cpu_write32(dce + dCtlPosition, cpu_read32(dce + dCtlPosition) + len);
    set_drv_err(0);
  }
  else {
    printf("WRITE DRIVE\n");
    dumpmem(buf, len);
    set_drv_err(noErr);
  }
}

static void sony_control(uint32_t pb, uint32_t dce)
{
  int err = 0;

  pb &= 0xFFFFFF;
  show_cs(pb);
  int code = cpu_read16(pb + csCode);
  switch (code) {
  case 7: // eject
    break;
  case 9: // track cache control
    err = 0; //xffffffc8;
    break;
  case 21: // get drive icon
    cpu_write32(pb + csParam, SonyDriveAddr);
    break;
  case 22: // get disk icon
    cpu_write32(pb + csParam, SonyDiskAddr);
    break;
  default:
    printf("unknown code: %x\n", code);
    assert(0);
  }
  set_drv_err(err);
}

static void sony_status(uint32_t pb, uint32_t dce)
{
  int err = 0;
  
  pb &= 0xFFFFFF;
  show_cs(pb);
  switch (cpu_read16(pb + csCode)) {
  case 8:
    show_addisk(0, dskmap[0]);
    memcpy(&sys.ram[pb + csParam], &sys.ram[dskmap[0]], 22);
    break;
  default:
    assert(0);
  }
  set_drv_err(err);
}

void mac68k::run()
{
  uint16_t op;

  cpu_reset();
  setbuf(stdout, NULL);
  for(;;) {
    if (PC == 0x00400288) {
      printf("READ RTC BYTE: %x\n", D[2]);
    }
    if (PC == 0x004019ca) {
      // enqueue
      printf("Enqueue: a0:%.8x[%.8x] a1:%.8x[%.8x]\n",
             A[0], cpu_read32(A[0]),
             A[1], cpu_read32(A[1]));
    }
    if (PC == 0x004185FE) {
      // Sony_ReadDriveRegAdr
      flogger(0, "@@ iwm read %s drive reg adr %.2x %s\n",
              iwm.iwm & iwm_t::SEL ? "EXT" : "INT",
              D[0], iwmreg[D[0] & 0xF]);
    }
    if (PC == 0x00418600) {
      // Sony_ReadDriveReg
      flogger(0, "@@ iwm read %s drive reg %.2x.%x %s\n",
              iwm.iwm & iwm_t::SEL ? "EXT" : "INT",
              iwm.iwm, iwm.sel,
              iwm_regname(&iwm));
    }
    if (PC == 0x0041861a) {
      // Sony_WriteDriveReg
      flogger(0, "@@ iwm write reg %.2x.%x %s\n",
              iwm.iwm, iwm.sel,
              iwm_regname(&iwm));
    }
    if (PC == 0x00417D9E) {
      // sonyopen      printf("SonyOpen:\n");
      sony_open(A[0], A[1]);
      PC = 0x00417d9c;
    }
    if (PC == 0x0041806a) {
      // sonyprime
      printf("SonyPrime:\n");
      sony_prime(A[0], A[1]);
      cpu_push32(cpu_read32(0x8fc));
      PC = 0x00418068;
    }
    if (PC == 0x00417F12) {
      printf("SonyControl:\n");
      sony_control(A[0], A[1]);
      cpu_push32(cpu_read32(0x8fc));
      PC = 0x00417F16;
    }
    if (PC == 0x0041801C) {
      printf("SonyStatus:\n");
      sony_status(A[0], A[1]);
      cpu_push32(cpu_read32(0x8fc));
      PC = 0x00418068;
    }
    if (PC == 0x0040f804) {
      printf(" CompareString(%.8x)\n", D[0]);
      hexdump(mptr(A[0]), 32);
      hexdump(mptr(A[1]), 32);
    }
    SPC = PC;

    check_irq();
    op = cpu_fetch(Word);
    decode_68k(op);
    vid_tick();
  }
}

const void *mptr(uint32_t m) {
  static char dummy[32];
  m &= 0xffffff;
  if (m >= 0x500000)
    return (void *)dummy;
  if (m >= 0x400000)
    return &sys.rombase[m];
  return &sys.ram[m];
}

const char *mstr(uint32_t m) {
  static char sstr[300];
  const char *sp;

  if (m > 0x4FFFFF)
    return "outofram";
  if (m > 0x400000)
    sp = (const char *)&sys.rombase[m & 0x1FFFF];
  else
    sp = (const char *)&sys.ram[m];
  memset(sstr, 0, sizeof(sstr));
  memcpy(sstr, sp+1, *sp);
  return sstr;
}

#define DISK "6.0 System Tools.image"
#define DSKOFF 0x54
//#define DISK "System Tools.img"
//#define DSKOFF  0

int read_disk(drive_t *d, uint32_t base, uint32_t size, void *data)
{
  if (d->data != NULL) {
    memcpy(data, (uint8_t *)d->data + base, size);
    hexdump(data, size);
    return 0;
  }
  return -1;
}

void m68k_emul1010(uint16_t op) {
  //printf("%.8x lvl:%d EMULATE: %.4x [%s]\n", SPC, elvl++, op, fnmap[0xff000000 + op].c_str());
  //cpu_showregs();
  switch(op) {
  case 0xa03c:
    printf(" emulate: CmpString(%x)\n", D[0]);
    hexdump(mptr(A[0]), 32);
    hexdump(mptr(A[1]), 32);
    break;
  case 0xa000:
    printf(" emulate: Open(%s)\n", mstr(A[1]));
    break;
  case 0xa9a0:
    break;
  case 0xa04e:
    // adddrive
    show_addisk(D[0], A[0] - 6);
    break;
  case 0x6660:
    printf(" CompareString(%.8x)\n", D[0]);
    hexdump(mptr(A[0]), 32);
    hexdump(mptr(A[1]), 32);
    break;
  case 0xa03d:
    printf("DriverInstall: %x %x\n", A[0], A[1]);
    hexdump(mptr(A[0]), 32);
    hexdump(mptr(A[1]), 32);
    break;
  case 0xa002:
  case 0xa003:
    show_io(A[0] & 0xFFFFFF);
    break;
  }
  m68k_trap(true, VECTOR_1010);
}

void m68k_emul1111(uint16_t op) {
  //flogger(0, "EMULATE: %.4x\n", op);
  m68k_trap(true, VECTOR_1111);
}

void getextra(dstk& s, uint32_t base, uint32_t size)
{
  FILE *fp;
  char line[128];
  uint32_t b;

  fp = fopen("dumpcfg_mac.txt", "r");
  while (fgets(line, sizeof(line), fp) != NULL) {
    sscanf(line, "%x", &b);
    s.push(b, 1, dstk::PENDING);
  }
}

#define PJSON
#include "json/pjson.cc"

static uint32_t tmpsr;
rr_t regread[] = {
  { "d0", &D[0] },
  { "d1", &D[1] },
  { "d2", &D[2] },
  { "d3", &D[3] },
  { "d4", &D[4] },
  { "d5", &D[5] },
  { "d6", &D[6] },
  { "d7", &D[7] },
  { "a0", &A[0] },
  { "a1", &A[1] },
  { "a2", &A[2] },
  { "a3", &A[3] },
  { "a4", &A[4] },
  { "a5", &A[5] },
  { "a6", &A[6] },
  { "ssp",&ssp },
  { "usp",&usp },
  { "pc", &PC },
  { "sr", &tmpsr },
  { },
};

void runjson(uint32_t *prefetch) {
  uint16_t op;

  SR = tmpsr;
  if (SR & 0x2000){
    SP = ssp;
  } else {
    SP = usp;
  }
  trace=3;
  cpu_write16be(PC, prefetch[0]);
  cpu_write16be(PC+2, prefetch[1]);
  SPC = PC;
  op = cpu_fetch(Word);
  printf("decode: %.4x %.8x\n", op, PC);
  decode_68k(op);
  tmpsr = SR;
  if (SR & 0x2000) {
    ssp = SP;
  }
  else {
    usp = SP;
  }
}

int main(int argc, char *argv[])
{
  size_t sz;
  uint8_t *buf;
  const char *romfile = "Mac-Plus.ROM";

  gentbl();
  if (argc > 2) {
    const int memsize = 0x1000000;
    uint8_t *mem = new uint8_t[memsize]{0};
    sys.register_handler(0x000000, 0xFFFFFF, 0xFFFFFF, memio, mem, _RW, "ram");
    read_json("test.json", regread, mem, runjson);
  }
  // Load disk image
  buf = loadrom(DISK, sz);
  setDrive(0, buf + DSKOFF, sz - DSKOFF);
  setDrive(1, NULL, 0);

  // Load rom
  buf = loadrom(romfile, sz);
  sys.init(buf, sz);

  audio_init();

  _VBR = 0; //0x00400000;
  A[7] = 0x100000;
  sys.run();

  return 0;
}
