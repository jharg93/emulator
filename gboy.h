#ifndef __gboy_h__
#include <util.h>

/* Gameboy Memory map
 *
 * 0000 .. 00FF Restart/Interrupt vectors
 * 0100 .. 014F Cartridge Header
 *      0100 .. 0103 jp
 *      0104 .. 0133 Nintendo Logo
 *      0134 .. 013E Game Title
 *      013F .. 0142 4-byte game designation
 *              0143 Color compatability
 *      0144 .. 0145 New Licensee code
 *              0146 SGB Compatability
 *              0147 Cart Type      [mapper]
 *              0148 Cart ROM Size  [32k,64k,128k,256k,512k,1M,2M,4M,8M]
 *              0149 Cart RAM Size  [0k,2k,8k,32k/4,128k/16,64k/8]
 *              014A Destination code
 *              014B Old Licensee code
 *              014C Mask ROM version
 *              014D Complement checksum
 *      014E .. 014F Checksum
 * 0150 .. 3FFF : Cart ROM - Bank 0 (fixed)
 * 4000 .. 7FFF : Cart ROM - Banks [1-NN]
 *-------------------------------------------------------
 * 8000 .. 97FF : Character RAM [6144] (256x16 bytes per tile) 
 *              8000 .. 87FF map 0 (tiles 0-127)                LCDC.D4=1 (unsigned)
 *              8800 .. 8FFF map 1 (tiles 128-255 or -1..-128)  LCDC.D4=0 (signed)
 *              9000 .. 97FF map 2 (tiles 0-127)
 * 9800 .. 9BFF : BG Map Data 1 (1024=32x32)  LCDC.D6=0, LCDC.D3=0
 * 9C00 .. 9FFF : BG Map Data 2 (1024=32x32)  LCDC.D6=1, LCDC.D3=1
 *-------------------------------------------------------
 * A000 .. BFFF : Cartridge RAM - Banks [0-NN]
 *-------------------------------------------------------
 * C000 .. CFFF : Internal RAM - Bank 0 (fixed)
 * D000 .. DFFF : Internal RAM - Banks [1-7] (on CGB)
 * E000 .. FDFF : Echo RAM (not used)
 * FE00 .. FE9F : OAM [40x4] sprites x.y,tile.attr
 * FEA0 .. FEFF : Unusable
 * FF00 .. FF7F : Registers
 *              FF04 DIV   incremented 16Khz
 *              FF05 TIMA  
 *              FF06 TMA   TIMA reset
 *              FF07 TAC   timer control
 *              FF0F IF    interrupt request
 *              FF40 LCDC
 *              FF41 STAT
 *              FF42 SCY   Scroll X
 *              FF43 SCX   Scroll Y
 *              FF44 LY    Scanline
 *              FF45 LYC   Scanline Match
 *              FF46 DMA
 *              FF47 BGP   background palette  33221100
 *              FF48 OBP0  sprite palette 0    332211--
 *              FF49 OBP1  sprite palette 1    332211--
 *              FF4A WY    Window Y
 *              FF4B WX    Window X
 *              FF4C
 *              FF4D
 *              FF4E
 *              FF4F VBK
 *              FF50 RomEN Enable Boot ROM
 *              FF51 HDMA1
 *              FF52 HDMA2
 *              FF53 HDMA3
 *              FF54 HDMA4
 *              FF55 HDMA5
 *              FF68 BGPS/BGPI
 *              FF69 BCPD/BGPD
 *              FF6A OCPS/OBPI
 *              FF6B OCPD/OBPD
 *              FF70 WRAM BANK
 * FF80 .. FFFE Zero Page (127 bytes)
 * FFFF .. FFFF Interrupt Enable
 *              00001 : VBlank   (INT40)
 *              00010 : LCD STAT (INT48)
 *              00100 : Timer    (INT50)
 *              01000 : Serial   (INT58)
 *              10000 : Joystick (INT60)
 */

/* Screen resolution: 160x144 (20x18)
 * Virtual screen   : 256x256 (32x32)
 * BG Window control:
 *  SCX: 0..255
 *  SCY: 0..255
 * FG Window control:
 *   WX: 0..166 (X-7)
 *   WY: 0..143
 * 4194304 cycles per second  (4MHz)
 * 456 cycles per line = 114 clocks
 * 70224 cycles per frame
 *
 * DMG: 4.194304 MHz / 59.73 == 70224 cycles per frame
 * CGB: 8.388688 MHz / 59.73 == 140448 cycles per frame
 * SGB: 4.295454 MHz / 61.17 ==
 *
 * LY  = 0..143 (visible) 144..153 (VBlank)
 * INT40 - VBlank interrupt (144.0)
 * INT48 - Status interrupt (STAT)
 * INT50 - Timer interrupt  (TIMA > 0xFF)
 *
 * 1 pixel = 1 clock = 1/4 cycle
 * mode 2 (searching) 80 clocks/20 cycles
 * mode 3 (drawing)   172-2987 clocks/43-72 cycles
 * mode 0 (hblank)    87?-204 clocks/22-51 cycles
 * Tile address: YYYYYyyy XXXXXxxx YYYYYXXXXX
 * 
 * VBLANK: 144..153 (10 lines, 4560 clocks/1140 cycles)
 *
 * OAM Search: 20 clocks                x4 = 80
 * Pixel Xfer: 43 clocks                x4 = 172
 * H-Blank   : 51 clocks                x4 = 204
 * V-Blank   :             10 lines        = 456
 *            114 x (144+10) = 17556 clocks per screen
 */

/*
 * MBC1[1-3] [max 2MB ROM, 32K RAM]
 *  0x0000 .. 0x3FFF (ro) Rom Bank 00
 *  0x4000 .. 0x7FFF (ro) Rom Bank 01-7F       set=wr.2000..3FFF
 *  0xA000 .. 0xBFFF (rw) RAM Bank 00-03       set=wr.4000..5FFF 
 *  0x6000 .. 0x7FFF (wo) ROM/RAM mode select
 *  
 * MBC2[5-6] [max 256k ROM, RAM]
 *  0x0000 .. 0x3FFF (ro) Rom Bank 00
 *  0x4000 .. 0x7FFF (ro) Rom Bank 01-0F       set=wr.2000..3FFF
 *  0xA000 .. 0xA1FF (rw) RAM
 *  0x0000 .. 0x1FFF (wo) RAM Enable
 *
 * MBC3[0f-13] [max 2MB ROM, 64k RAM]
 *  0x0000 .. 0x3FFF (ro) Rom Bank 00
 *  0x4000 .. 0x7FFF (ro) Rom Bank 01-7F       set=wr.2000..3FFF
 *  0xA000 .. 0xBFFF (rw) RAM Bank 00-03       set=wr.4000..5FFF (<4), enable=wr.0000..1FFF
 *  0xA000 .. 0xBFFF (rw) RTC registers        set=wr.4000..5FFF (>=4)
 *  0x6000 .. 0x7FFF (wo) Latch clock data
 *
 * MBC5 [max 8MB ROM, 128k RAM]
 *  0x0000 .. 0x3FFF (ro) Rom Bank 00
 *  0x4000 .. 0x7FFF (ro) Rom Bank 00-1FF      sel.lo=wr.2000..2FFF, sel.hi=wr.3000..3FFF
 *  0xA000 .. 0xBFFF (rw) RAM Bank 00-0F       sel=wr.4000..5FFF
 */

/* Cartridge Header */
struct gbhdr_t {
  uint8_t jmp[4];         // entry point
  uint8_t logo[0x30];
  uint8_t title[0xB];
  uint8_t desig[4];       // manufacturer code
  uint8_t clrcompat;      // BIT 6/7 = CGB mode
  uint8_t licensee[2];
  uint8_t sgb;            // ==3
  uint8_t type;           // mapper (ROM/MBC1/MBC2/MBC3/etc)
  uint8_t rom_sz;
  uint8_t ram_sz;
  uint8_t destination;
  uint8_t oldlicensee;
  uint8_t maskromver;
  uint8_t complement;
  uint8_t cksum[2];       // header/global checksum
};

enum gbreg_t {
  REGMASK = 0xFF,
#if 0
  /* Joypad */
  P1   = 0xFF00,

  /* Serial */
  SB   = 0xFF01,   // rw
  SC   = 0xFF02,   // rw

  /* Timer */
  DIV  = 0xFF04,   // rw
  TIMA = 0xFF05,   // rw
  TMA  = 0xFF06,   // rw
  TAC  = 0xFF07,   // rw

  /* Interrupt flag and enable */
  IF   = 0xFF0F,
  IE   = 0xFFFF,

  /* Sound */
  NR10 = 0xFF10,  // rw ch1 -PPP.NSSS sweep
  NR11 = 0xFF11,  // rw ch1 DDLL.LLLL sound length/wave pattern duty (64-L)
  NR12 = 0xFF12,  // rw ch1 VVVV.APPP volume envelope
  NR13 = 0xFF13,  // rw ch1 FFFF.FFFF freq lo
  NR14 = 0xFF14,  // rw ch1 TL--.-FFF freq hi

  NR21 = 0xFF16,  // rw ch2 DDLL.LLLL sound length/wave pattern duty (64-L)
  NR22 = 0xFF17,  // rw ch2 VVVV.APPP volume envelope
  NR23 = 0xFF18,  // rw ch2 FFFF.FFFF freq lo
  NR24 = 0xFF19,  // rw ch2 TL--.-FFF freq hi

  NR30 = 0xFF1A,  // rw ch3 E---.---- sound on/off
  NR31 = 0xFF1B,  // rw ch3 LLLL.LLLL sound length/wave pattern duty (256-L)
  NR32 = 0xFF1C,  // rw ch3 -VV-.---- volume envelope
  NR33 = 0xFF1D,  // rw ch3 FFFF.FFFF freq lo
  NR34 = 0xFF1E,  // rw ch3 TL--.-FFF freq hi
  
  NR41 = 0xFF20,  // rw ch4 --LL.LLLL sound length (64-L)
  NR42 = 0xFF21,  // rw ch4 VVVV.APPP volume envelope
  NR43 = 0xFF22,  // rw ch4 SSSS.WDDD poly counter
  NR44 = 0xFF23,  // rw ch4 TL--.---- counter

  NR50 = 0xFF24,  // ALLL.BRRR channel control on/off  volume
  NR51 = 0xFF25,  // NW21.NW21 sound output terminal
  NR52 = 0xFF26,  // P---.NW21 sound on/off

  /* PPU */
  LCDC = 0xFF40,  // rw
  STAT = 0xFF41,  // rw
  SCY  = 0xFF42,  // rw ScrollY
  SCX  = 0xFF43,  // rw ScrollX
  LY   = 0xFF44,  // r  Scanline
  LYC  = 0xFF45,  // rw Scanline Compare
  DMA  = 0xFF46,  // wo DMA Start Address
  BGP  = 0xFF47,  // rw DMG Background Palette
  OBP0 = 0xFF48,  // rw DMG Sprite Palette 0
  OBP1 = 0xFF49,  // rw DMG Sprite Palette 1
  WY   = 0xFF4A,  // rw WindowY
  WX   = 0xFF4B,  // rw WindowX (-7)

  /* CGB Specific registers */
  BGPI = 0xFF68,  // Background Palette Index (00-3F)
  BGPD = 0xFF69,  // Background Palette Data
  OBPI = 0xFF6A,  // Sprite Palette Index     (00-3F)
  OBPD = 0xFF6B,  // Sprite Palette Data
  VBK  = 0xFF4F,  // VRAM Bank (0/1)
  HDMA1= 0xFF51,  // DMA Source, lo (0000-7FF0 or A000-DFF0)
  HDMA2= 0xFF52,  // DMA Source, hi
  HDMA3= 0xFF53,  // DMA Target, lo (8000-9FF0)
  HDMA4= 0xFF54,  // DMA Target, hi
  HDMA5= 0xFF55,  // DMA Length+Start

  WBANK = 0xFF70,
#endif
};

enum {
  /* LCDC Flag
   * +----+----+----+----+----+----+----+----+
   * | en |winm|wine|bgtl|bgmp|spsz|spen|bgen|
   * +----+----+----+----+----+----+----+----+*/
  LCDC_EN              = D7,  // 0=off        1=on
  LCDC_WINDOW_MAP      = D6,  // 0=9800-9BFF  1=9C00-9FFF [win.map]
  LCDC_WINDOW_EN       = D5,  // 0=off        1=on        [win.en]
  LCDC_BG_TILESEL      = D4,  // 0=8800-97FF  1=8000-8FFF [bg.sel, win.sel]
  LCDC_BG_MAP          = D3,  // 0=9800-9BFF  1=9C00-9FFF [bg.map]
  LCDC_SPRITE_SZ       = D2,  // 0=8x8,       1=8x16
  LCDC_SPRITE_EN       = D1,  // 0=off        1=on
  LCDC_BG_EN           = D0,  // 0=off        1=on        [bg.en]

  /* STAT Flag
   * +----+----+----+----+----+----+----+----+
   * |    |lyc |mod2|mod1|mod0|coin|  mode   |
   * +----+----+----+----+----+----+----+----+*/
  STAT_LYC_INTR        = D6,   // rw LY/LYC coincidence intr
  STAT_MODE2_INTR      = D5,   // rw mode 2 [oam]
  STAT_MODE1_INTR      = D4,   // rw mode 1 [vblank]
  STAT_MODE0_INTR      = D3,   // rw mode 0 [hblank]
  STAT_LYC_COINC       = D2,   // ro LY/LYC coincidence flag
  STAT_MODE            = D0|D1,// ro 0=hblank, 1=vblank, 2=oam, 3=pixels
  STAT_USERBITS        = 0xF8,

  /* Interrupt Flag: IF/IE
   * +----+----+----+----+----+----+----+----+
   * |    |    |    |ctrl|ser |tmr |lcdc|vbl |
   * +----+----+----+----+----+----+----+----+*/
  VBLANK_INTR          = D0, // irq 40
  LCDC_INTR            = D1, // irq 48
  TIMER_INTR           = D2, // irq 50
  SERIAL_INTR          = D3, // irq 58
  CONTROL_INTR         = D4, // irq 60

  /* OAM Attr:
   * +----+----+----+----+----+----+----+----+
   * |prio|flpy|flpx|dpal|bank|     cgbpal   |
   * +----+----+----+----+----+----+----+----+*/
  ATTR_PRIORITY        = D7,        // 0 = above BG, 1 = below BG
  ATTR_FLIPY           = D6,        // 0 = normal,   1 = flipx
  ATTR_FLIPX           = D5,        // 0 = normal,   1 = flipy
  ATTR_PALETTE         = D4,        // 0 = OBP0,     1 = OBP1  (DMG)
  ATTR_BANK            = D3,        // 0 = BANK0,    1 = BANK1 (CGB)
  ATTR_CGBPAL          = D2|D1|D0,  // Palette OBP0-7          (CGB)
};

/* Draw BG:
 *   set = (LCDC & LCDC_BG_TILESEL) ? TILESET8000 : TILESET8800;
 *   map = (LCDC & LCDC_BG_MAP)     ? TILEMAP9C00 : TILEMAP9800;
 *   X   = SCX;
 *   Y   = SCY;
 *
 * Draw Win:
 *   set = (LCDC & LCDC_
 */

/*
  Tiles are 8x8x2 bits = 16 bytes, 2 bytes per line
  0.lsb 76543210
  0.msb 76543210
  1.lsb 76543210
  1.msb 76543210
  
  SNES 2bpp tile: 16 bytes per tile
  SNES 4bpp tile: 32 bytes per tile
  SNES 8bpp tile: 64 bytes per tile
  0.plane[0]
  0.plane[1]
  0.plane[2]
  0.plane[3]
  0.plane[4]
  0.plane[5]
  0.plane[6]
  0.plane[7]
*/
struct TileMap {
  uint8_t *map;
  uint8_t *set;
  int w;
  int h;
  int bpp;
};

#define CHIPREG(o)							\
  o(P1,     0xFF00,  "rw", "joypad" )					\
  o(SB,     0xFF01,  "rw", "Serial B" )					\
  o(SC,     0xFF02,  "rw", "Serial C" )					\
  o(DIV,    0xFF04,  "rw", "Timer DIV" )				\
  o(TIMA,   0xFF05,  "rw", "Timer Counter" )				\
  o(TMA,    0xFF06,  "rw", "Timer Reset" )				\
  o(TAC,    0xFF07,  "rw", "Timer Freq" )				\
  o(IF,     0xFF0F,  "rw", "interrupt status" )				\
  o(IE,     0xFFFF,  "rw", "interrupt enable" )				\
  o(NR10,   0xFF10,  "rw", "ch1 -PPP.NSSS sweep" )			\
  o(NR11,   0xFF11,  "rw", "ch1 DDLL.LLLL sound length/wave pattern duty (64-L)" ) \
  o(NR12,   0xFF12,  "rw", "ch1 VVVV.APPP volume envelope" )		\
  o(NR13,   0xFF13,  "rw", "ch1 FFFF.FFFF freq lo" )			\
  o(NR14,   0xFF14,  "rw", "ch1 TL--.-FFF freq hi" )			\
  o(NR21,   0xFF16,  "rw", "ch2 DDLL.LLLL sound length/wave pattern duty (64-L)" ) \
  o(NR22,   0xFF17,  "rw", "ch2 VVVV.APPP volume envelope" )		\
  o(NR23,   0xFF18,  "rw", "ch2 FFFF.FFFF freq lo" )			\
  o(NR24,   0xFF19,  "rw", "ch2 TL--.-FFF freq hi" )			\
  o(NR30,   0xFF1A,  "rw", "ch3 E---.---- sound on/off" )		\
  o(NR31,   0xFF1B,  "rw", "ch3 LLLL.LLLL sound length/wave pattern duty (256-L)" ) \
  o(NR32,   0xFF1C,  "rw", "ch3 -VV-.---- volume envelope" )		\
  o(NR33,   0xFF1D,  "rw", "ch3 FFFF.FFFF freq lo" )			\
  o(NR34,   0xFF1E,  "rw", "ch3 TL--.-FFF freq hi" )			\
  o(NR41,   0xFF20,  "rw", "ch4 --LL.LLLL sound length (64-L)" )	\
  o(NR42,   0xFF21,  "rw", "ch4 VVVV.APPP volume envelope" )		\
  o(NR43,   0xFF22,  "rw", "ch4 SSSS.WDDD poly counter" )		\
  o(NR44,   0xFF23,  "rw", "ch4 TL--.---- counter" )			\
  o(NR50,   0xFF24,  "rw", " ALLL.BRRR channel control on/off  volume" ) \
  o(NR51,   0xFF25,  "rw", " NW21.NW21 sound output terminal" )		\
  o(NR52,   0xFF26,  "rw", " P---.NW21 sound on/off" )			\
  o(LCDC,   0xFF40,  "rw", "" )						\
  o(STAT,   0xFF41,  "rw", "" )						\
  o(SCY,    0xFF42,  "rw", "ScrollY" )					\
  o(SCX,    0xFF43,  "rw", "ScrollX" )					\
  o(LY,     0xFF44,  "rw", " r  Scanline" )				\
  o(LYC,    0xFF45,  "rw", "Scanline Compare" )				\
  o(DMA,    0xFF46,  "rw", " wo DMA Start Address" )			\
  o(BGP,    0xFF47,  "rw", "DMG Background Palette" )			\
  o(OBP0,   0xFF48,  "rw", "DMG Sprite Palette 0" )			\
  o(OBP1,   0xFF49,  "rw", "DMG Sprite Palette 1" )			\
  o(WY,     0xFF4A,  "rw", "WindowY" )					\
  o(WX,     0xFF4B,  "rw", "WindowX (-7)" )				\
  o(BGPI,   0xFF68,  "rw", "Background Palette Index (00-3F)" )		\
  o(BGPD,   0xFF69,  "rw", "Background Palette Data" )			\
  o(OBPI,   0xFF6A,  "rw", "Sprite Palette Index     (00-3F)" )		\
  o(OBPD,   0xFF6B,  "rw", "Sprite Palette Data" )			\
  o(VBK,    0xFF4F,  "rw", "VRAM Bank (0/1)" )				\
  o(HDMA1,  0xFF51,  "rw", "DMA Source, lo (0000-7FF0 or A000-DFF0)" )	\
  o(HDMA2,  0xFF52,  "rw", " DMA Source, hi" )				\
  o(HDMA3,  0xFF53,  "rw", " DMA Target, lo (8000-9FF0)" )		\
  o(HDMA4,  0xFF54,  "rw", " DMA Target, hi" )				\
  o(HDMA5,  0xFF55,  "rw", " DMA Length+Start" )			\
  o(WBANK,  0xFF70,  "rw", "IRAM bank 1-7" )				\


enum {
#define o(id, addr, rw, desc) id = addr,
  CHIPREG(o)
#undef o
};

#endif
