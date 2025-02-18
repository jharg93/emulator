#ifndef __snes_h__
#define __snes_h__

struct snes_header {
  char     name[21];
  uint8_t  map_mode;   // xxAAxxxB
  uint8_t  rom_type;
  uint8_t  rom_size;   // 09,0a,0b,0c,0d [2048^val]
  uint8_t  sram_size;
  uint8_t  dest_code;
  uint8_t  fixed;
  uint8_t  version;
  uint16_t cmc_check;
  uint16_t cksum;
};


enum {
  /* Tile size : BGMODE */
  _8x8   = 0x0808,
  _8x16  = 0x0810,
  _16x8  = 0x1008,
  _16x16 = 0x1010,

  /* BG size : BGxSC */
  _32x32 = 0x2020,
  _64x32 = 0x4020,
  _32x64 = 0x2040,
  _64x64 = 0x4040,
  _128x128 = 0x8080,

  /*========== Graphics modes, # of color per BG
   * Screen resolution = 32x28 (256x224)
   * mode0:  4   4   4   4   BG0=0-31,  BG1=32-63, BG2=64-95, BG3=96-127, 8x8, 16x16
   * mode1:  16  16  4   -   BG0=0-127, BG1=0-127, BG2=0-31, 8x8, 16x16
   * mode2:  16  16  -   -   offset-change-per-column, 8x8, 16x16
   * mode3:  256 16  -   -   BG0=0-255, BG1=0-127, direct color rrrr.gggg.bbb, 8x8, 16x16
   * mode4:  256  4  -   -   offset-change-per-column, 8x8, 16x16
   * mode5:  16   4  -   -   512x224
   * mode6:  16   -  -   -
   * mode7:  256  -  -   -   matrix
   * 7extBG: 256 128 -   -
   *================*/
  MODE0_BG0 = 0x00,
  MODE0_BG1 = 0x01,
  MODE0_BG2 = 0x02,
  MODE0_BG3 = 0x03,
  MODE1_BG0 = 0x10,
  MODE1_BG1 = 0x11,
  MODE1_BG2 = 0x12,
  MODE2_BG0 = 0x20,
  MODE2_BG1 = 0x21,
  MODE3_BG0 = 0x30,
  MODE3_BG1 = 0x31,
  MODE4_BG0 = 0x40,
  MODE4_BG1 = 0x41,
  MODE5_BG0 = 0x50,
  MODE5_BG1 = 0x51,
  MODE6_BG0 = 0x60,
  MODE7_BG0 = 0x70,
};

/* Memory map
 * 00-3F: 0000-1FFF LowRAM
 *        2000-20FF Unused
 *        2100-21FF PPU1/APU registers
 *        2200-2FFF Unused
 *        3000-3FFF DSP
 *        4000-40FF joypad registers
 *        4100-41FF Unused
 *        4200-44FF PPU2/DMA registers
 *        4500-5FFF Unused
 *        6000-7FFF Reserved
 *        8000-FFFF LowROM 00=000000-007FFF, 01=008000-00FFFF, etc
 * 40-6F: 0000-7FFF unused or mapped
 *        8000-FFFF LowROM 40=200000-207FFF, 41=208000-20FFFF, etc
 * 70-7D: 0000-7FFF SRAM (448k)
 *        8000-FFFF LowROM 70=380000-387FFF, 71=388000-38FFFF, etc
 * 7E     0000-1FFF LowRAM
 *        2000-7FFF HighRAM
 *        8000-FFFF ExtendedRAM
 * 7F     0000-FFFF ExtendedRAM
 * 80-BF  Mirror 00-3F
 * C0-EF  Mirror 40-6F
 * F0-FD  Mirror 70-7D
 */
#endif
