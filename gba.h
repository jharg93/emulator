#ifndef __gba_h__
#define __gba_h__

/* GBA Memory Map
 * https://problemkaputt.de/gbatek.htm#gbamemorymap
 *
 * 00000000-00003FFF ro BIOS ROM
 * 00004000-01FFFFFF -- not used
 * 02000000-0203FFFF rw Board Work RAM (256k)
 * 02040000-02FFFFFF -- not used
 * 03000000-03007FFF rw Chip Work RAM (32k)
 *    03007F00..03007FFF Interrupt Vector/Interrupt Stack/BIOS Call Stack
 *    03007F00           Initial SP
 * 03008000-03FFFFFF -- not used
 * 04000000-040003FE -- I/O registers
 *    04000000 2 rw R_DISPCNT
 *    04000002 2 rw
 *    04000004 2 rw R_DISPSTAT (STAT/LYC)
 *                  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
 *                  | VCT                           | ----- |VCI|HBI|VBI|VCS|HBS|VBS|
 *                  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
 *                  VBS: IF (LY >= 160);
 *                  HBS: IF (CLK >= 240);
 *                  VCS: IF (REG_VCOUNT == REG_DISPSTAT[8:15]);
 *                  VBI: IF (CLKS == 0 && LY == 260) CPU_IRQ(VBLANK)
 *                  HBI: IF (CLKS == 240) CPU_IRQ(HBLANK)
 *                  VCI: IF (REG_VCOUNT == REG_DISPSTAT[8:15]);
 *    04000006 2 r  R_VCOUNT   (LY) [0-227]
 *    04000008 2 rw R_BG0CNT
 *    0400000a 2 rw R_BG1CNT
 *    0400000c 2 rw R_BG2CNT
 *    0400000e 2 rw R_BG3CNT
 * 04000400-04FFFFFF -- not used
 *
 * 05000000-050003FF rw BG/OBJ Palette RAM (1Kb)
 * 05000400-05FFFFFF -- not used
https://www.coranac.com/tonc/text/video.htm * 06000000-06017FFF rw VRAM (96Kb)
 * 06018000-06FFFFFF -- not used
 * 07000000-070003FF rw OAM
 * 07000400-07FFFFFF -- not used
 *
 * 08000000-09FFFFFF ro ROM (Cartridge) (32MB)
 * 0A000000-0BFFFFFF ro ROM
 * 0C000000-0DFFFFFF ro ROM
 * 0E000000-0E00FFFF rw RAM
 * 0E010000-0FFFFFFF -- not used
 *
 * 10000000-FFFFFFFF -- not used (bus width=28)
 */
struct gbahdr_t {
  uint32_t    entry;        // B xxx instruction
  uint8_t     logo[156];    // compressed logo
  char        title[12];    // game title
  char        code[4];      // game code
  char        mkcode[2];    // maker code
  uint8_t     fix96;        // fixed value = 96h
  uint8_t     mucode;       // main unit code = 00h
  uint8_t     devicetype;
  uint8_t     rsvd[7];
};

/* https://www.coranac.com/tonc/text/video.htm
 * Screen: 240x160
 * HBlank: 68
 * VBlank: 68
 * 4 cycles/pixel x (240+68) x (160 + 68) = 280896 cycles per frame
 *
 * 4 backgrounds 32x32 or 128x128 tiles  [R_BGxCNT]
 * 128 Sprites: 8x8 to 64x64
 */
struct oam_t {
  // | Shape |CM |Mos|  GM   |  OM   |               Y               |
  // +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  // | Size  |VF |HF |   -       |                X                  |
  // +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  // |    PB         |  Pr   |                  TID                  |
  //
  // VF = Vert Flip
  // HF = Horiz Flip
  //  Y = Y coord     (0..255)
  //  X = X coord     (0..511)
  // TID= Tile ID     (0..1023)
  uint16_t attr0;  
  uint16_t attr1; 
  uint16_t attr2;
  uint16_t fill;
};

/* 16k = 512x16x2 bytes (0-3) */
volatile uint16_t *tileset(int cbb) {
  return (volatile uint16_t *)(0x06000000 + cbb * 0x4000);
}

/* 2k = 32x32x2 bytes : 0-31 */
volatile uint16_t *tilemap(int sbb) {
  return (volatile uint16_t *)(0x06000000 + sbb * 0x0800);
}

enum {
  BG_REG_32x32 = 0,
  BG_REG_64x32 = 1,
  BG_REG_32x64 = 2,
  BG_REG_64x64 = 3
};

#endif
