#ifndef __genesis_h__
#define __genesis_h__

/*===========================================================
 * 4 planes: sprite, scroll a/window, scroll b, background
 *  32x28 cell = 256x224, 64 sprites
 *  40x28 cell = 320x224, 80 sprites
 * scrolling fields RS0/RS1:
 *  32x32  : 0x0800 bytes
 *  64x32  : 0x1000 bytes
 *  128x32 : 0x2000 bytes
 *  32x64  : 0x1000 bytes
 *  64x64  : 0x2000 bytes
 *  32x128 : 0x2000 bytes
 * sprites:
 *  8x8,  16x8,  24x8,  32x8
 *  8x16, 16x16, 24x16, 32x16
 *  8x24, 16x24, 24x24, 32x24
 *  8x32, 16x32, 24x32, 32x32
 *
 * VRAM: 64k
 * VSRAM: 80 bytes
 * CRAM:  128 bytes (64 colors x 2)
 * CRTC:
 *
 * Interrupts:
 *    1E0 V-sync   LVL6
 *    1E1 H-sync   LVL4
 *    1E2 External LVL2
 *
 * +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
 * | x | x | x | x | x | x |EMP|FUL| F |SOV| C |ODD|DT3| VB| HB|PAL| CONTROL: C00004 : read
 * +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
 *   VB/HB = vert/horiz blank
 *   
 *============================================================*/

/* VDP Registers: https://segaretro.org/Sega_Mega_Drive/VDP_registers
 *   +---+---+---+---+---+---+---+---+
 *   | 0 | 0 | 0 |IE1| 0 | 1 |M3 | 0 | 0 mode set #1
 *   +---+---+---+---+---+---+---+---+
 *      IE1 = 0=disable/1=enable HBLANK, irq.4 (reg 10)
 *      M3 = 0=enable/1=disable HV counter
 *   +---+---+---+---+---+---+---+---+
 *   | 0 |DSP|IE0|M1 |M2 | 1 | 0 | 0 | 1 mode set #2
 *   +---+---+---+---+---+---+---+---+
 *      DSP = 0=disable/1=enable display
 *      IE0 = 0=disable/1=enable VBLANK, irq.6
 *      M1 = 0=disable/1=enable dma
 *      M2 = 0=V28 cell mode (NTSC/PAL)/1=V30 cell mode (PAL)
 *   +---+---+---+---+---+---+---+---+
 *   | 0 | 0 |A15|A14|A13| 0 | 0 | 0 | 2 Scroll A Name Table
 *   +---+---+---+---+---+---+---+---+
 *      VRAM: aaa0.0000.0000.0000
 *   +---+---+---+---+---+---+---+---+
 *   | 0 | 0 |W15|W14|W13|W12|W11| 0 | 3 Window Name Table
 *   +---+---+---+---+---+---+---+---+
 *      VRAM: aaaa.a000.0000.0000 in H32-cell
 *      VRAM: aaaa.0000.0000.0000 in H40-cell (w11 == 0)
 *   +---+---+---+---+---+---+---+---+
 *   | 0 | 0 | 0 | 0 | 0 |S15|S14|S13| 4 Scroll B Name Table
 *   +---+---+---+---+---+---+---+---+
 *      VRAM: aaa0.0000.0000.0000
 *   +---+---+---+---+---+---+---+---+
 *   | 0 |A15|A14|A13|A12|A11|A10|A9 | 5 Sprite Table
 *   +---+---+---+---+---+---+---+---+
 *      VRAM: aaaa.aaa0.0000.0000 in H32-cell
 *      VRAM: aaaa.aa00.0000.0000 in H40-cell (a9 == 0)
 *   +---+---+---+---+---+---+---+---+
 *   | 0 | 0 |A16| 0 | 0 | 0 | 0 | 0 | 6 Sprite Table bit 16
 *   +---+---+---+---+---+---+---+---+
 *   | 0 | 0 |PL1|PL0|CL3|CL2|CL1|CL0| 7 Background color palette/index
 *   +---+---+---+---+---+---+---+---+
 *   | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 8
 *   +---+---+---+---+---+---+---+---+
 *   | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 9
 *   +---+---+---+---+---+---+---+---+
 *   |b7 |b6 |b5 |b5 |b3 |b2 |b1 |b0 | 10 interrupt register
 *   +---+---+---+---+---+---+---+---+
 *       hcounter, if IE=1
 *   +---+---+---+---+---+---+---+---+
 *   | 0 | 0 | 0 | 0 |IE2|VSC|HSC|LCR| 11 mode set #3
 *   +---+---+---+---+---+---+---+---+
 *      IE2 = enable external interrupt (irq.2)
 *      VSCR = 0=full scroll/1=2-cell scroll
 *       HL
 *       00 full scroll
 *       01 n/a
 *       10 1 cell scroll (8)
 *       11 1 line scroll (1)
 *   +---+---+---+---+---+---+---+---+
 *   |RS0| 0 | 0 | 0 |STE|LS1|LS0|RS1| 12 mode set #4
 *   +---+---+---+---+---+---+---+---+
 *      RS0 = H-cell 32/40
 *      RS1 = H-cell 32/40
 *      STE = shadow/highlight
 *      LSM= interlace mode
 *   +---+---+---+---+---+---+---+---+
 *   | 0 | 0 |H15|H14|H13|H12|H11|H10| 13 Horiz Scroll base
 *   +---+---+---+---+---+---+---+---+
 *      VRAM: aaaa.aa00.0000.0000
 *   +---+---+---+---+---+---+---+---+
 *   | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 14
 *   +---+---+---+---+---+---+---+---+
 *   | i7| i6| i5| i4| i3| i2| i1| i0| 15 auto increment data
 *   +---+---+---+---+---+---+---+---+
 *   | 0 | 0 |vs1|vs0| 0 | 0 |hs1|hs2| 16 scroll size
 *   +---+---+---+---+---+---+---+---+
 *      VSZ = 32/64/xx/128
 *      HSZ = 32/64/xx/128
 *   +---+---+---+---+---+---+---+---+
 *   |rgt| 0 | 0 |wh5|wh4|wh3|wh2|wh1| 17 window h pos
 *   +---+---+---+---+---+---+---+---+
 *   |dwn| 0 | 0 |wv5|wv4|wv3|wv2|wv1| 18 window v pos
 *   +---+---+---+---+---+---+---+---+
 *   |    dma counter lo             | 19
 *   +---+---+---+---+---+---+---+---+
 *   |    dma counter hi             | 20
 *   +---+---+---+---+---+---+---+---+
 *   |    dma source lo              | 21
 *   +---+---+---+---+---+---+---+---+
 *   |    dma source mid             | 22
 *   +---+---+---+---+---+---+---+---+
 *   |dm1|dm0|    dma source hi      | 23
 *   +---+---+---+---+---+---+---+---+
 *     0 d23 memory to vram
 *     1 0   vram fill
 *     1 1   vram copy
 */

/* Nametable entry: (ty * scroll_h) + tx
 *   +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
 *   |PR |   PL  | V | H |               tile                        |
 *   +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
 *     PR = priority (0=low, 1=high)
 *     PL = palette 0-3
 *     V  = vertical flip
 *     H  = horizontal flip
 *     tile = tile index (address = index*0x20)
 */
#define NT_SIZE          2
#define NT_ADDR(x,y,w)   ((((y/8))*(w/8) + (x/8))*NT_SIZE)
#define NT_ATTR(x)       get16be(x)
#define NT_PRI(x)        (((x) & 0x8000) != 0)
#define NT_PAL(x)        ((((x) >> 13) & 3) * 16)
#define NT_FLAGS(x)      (((x) >> 8) & (HFLIP|VFLIP))
#define NT_TILE(x)       (((x) & 0x7FF) * 0x20)

/*
 * Horiz scrolling: reg 0xd, 0xb
 *   whole screen, 4 bytes, FFFF BBBB
 *   8-pixel strips:
 *   per-scanline
 */
#define CHIPREG(o)				\
  o(0x00c00000, "w-", VDP_DATA, "VDP Data")	\
  o(0x00c00004, "w-", VDP_CTRL, "VDP Control")	\
  o(0x00a10001, "rw", Z80_DATA0, "DATA0" )	\
  o(0x00a10003, "rw", Z80_DATA1, "DATA1" )	\
  o(0x00a10005, "rw", Z80_DATA2, "DATA2" )	\
  o(0x00a10007, "rw", Z80_DATA3, "DATA3" )	\
  o(0x00a10009, "rw", Z80_CTRL1, "CTRL1" )	\
  o(0x00a1000b, "rw", Z80_CTRL2, "CTRL2" )	\
  o(0x00a1000d, "rw", Z80_CTRL3, "CTRL3" )	\
  o(0x00a1000f, "rw", Z80_TXDATA1, "TXDATA1" )	\
  o(0x00a10011, "rw", Z80_RXDATA1, "RXDATA1" )	\
  o(0x00a10013, "rw", Z80_SCTRL1, "SCTRL1" ) 


enum {
  VFLIP = 0x10,
  HFLIP = 0x08,

  VDP_STATUS_PAL = 0x01,
  VDP_STATUS_DMA = 0x02,
  VDP_STATUS_HB = 0x04,
  VDP_STATUS_VB = 0x08,
  VDP_STATUS_OD = 0x10,
  VDP_STATUS_SC = 0x20,
  VDP_STATUS_SO = 0x40,
  VDP_STATUS_VI = 0x80,
  VDP_STATUS_F = 0x100,
  VDP_STATUS_E = 0x200,

  MAX_SPRITES = 80,

  SCROLLA_ADDR_SHIFT = 10, // reg 2
  SCROLLB_ADDR_SHIFT = 13, // reg 4
  WINDOW_ADDR_SHIFT = 10,  // reg 3
  HSCROLL_ADDR_SHIFT = 10, // reg 13
  SPRITE_ADDR_SHIFT = 9,   // reg 5
  
  VDPR0_IE1 = D4,  // hcounter, irq.4
  VDPR0_M3  = D2,

  VDPR1_DISP = D6,
  VDPR1_IE0 = D5,  // vblank irq.6
  VDPR1_M1 = D4,   // dna enable
  VDPR1_M2 = D3,   // 28/30 mode

  VDPR11_IE2 = D3,
  VDPR11_VSCR = D2,
  VDPR11_HSCR = D1,
  VDPR11_LSCR = D0,

  // screen width/height = 40x40 or 32x32
  VDPR12_RS0 = D7,
  VDPR12_STE = D3,
  VDPR12_LSM1 = D2,
  VDPR12_LSM0 = D1,
  VDPR12_RS1 = D0,

};
#endif
