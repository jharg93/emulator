#ifndef __nes_h__
#define __nes_h__

/* NES memory map:
 *   0x0000 - 0x07FF : 2k internal ram
 *   0x0800 - 0x0FFF : mirrors 0000..07FF
 *   0x1000 - 0x17FF : mirrors 0000..07FF
 *   0x1800 - 0x1FFF : mirrors 0000..07FF
 *
 *   0x2000 - 0x2007 : ppu registers
 *   0x2008 - 0x3FFF : mirrors 2000..2007
 *
 *   0x4000 - 0x4017 : APU & I/O registers
 *   0x4018 - 0x401F : disabled
 * 
 *   0x8000 - 0xFFFF : PRG ROM
 */

/* PPU memory map:
 *   0000-0FFF : pattern table 0 [chr rom] (4k = 256 tiles * 16 bytes per tile)
 *   1000-1FFF : pattern table 1 [chr rom] (4k = 256 tiles * 16 bytes per tile)
 *   2000-23FF : name table 1
 *   2400-27FF : name table 2
 *   2800-2Bff : name table 3
 *   2C00-2FFF : name table 4
 *   3000-33FF : nt1 mirror
 *   3400-37FF : nt2 mirror
 *   3800-3BFF : nt3 mirror
 *   3C00-3EFF : nt4 mirror
 *   3F00-3F1F : palette
 *               0123 4567 89ab cdef
 *               -123 -567 -9ab -def  : '-' is transparent
 *   3F20-3FFF : palette mirror
 */

/* mapping of nametables */
#define MIRROR_4SCR  0x0123
#define MIRROR_HORZ  0x0022
#define MIRROR_VERT  0x0101
#define MIRROR_LEFT  0x0000
#define MIRROR_RIGHT 0x1111

struct nesMapper;
int     selio(void *, uint32_t, int, uint8_t&);

int     ppuGetMirror();
void    ppuSetMirror(int);

void    apu_init();
void    apu_tick();
void    apu_write(int addr, uint8_t v);
uint8_t apu_read(int addr);
void    apu_run_frame();

palclr nespalette[] = {
  { 84, 84, 84 },
  { 0, 30, 116 },
  { 8, 16, 144 },
  { 48, 0, 136 },
  { 68, 0, 100 },
  { 92, 0, 48 },
  { 84, 4, 0 },
  { 60, 24, 0 },
  { 32, 42, 0 },
  { 8, 58, 0 },
  { 0, 64, 0 },
  { 0, 60, 0 },
  { 0, 50, 60 },
  { 0, 0, 0 },
  { 0, 0, 0 },
  { 0, 0, 0 },

  { 152, 150, 152 },
  { 8, 76, 196 },
  { 48, 50, 236 },
  { 92, 30, 228 },
  { 136, 20, 176 },
  { 160, 20, 100 },
  { 152, 34, 32 },
  { 120, 60, 0 },
  { 84, 90, 0 },
  { 40, 114, 0 },
  { 8, 124, 0 },
  { 0, 118, 40 },
  { 0, 102, 120 },
  { 0, 0, 0 },
  { 0, 0, 0 },
  { 0, 0, 0 },

  { 236, 238, 236 },
  { 76, 154, 236 },
  { 120, 124, 236 },
  { 176, 98, 236 },
  { 228, 84, 236 },
  { 236, 88, 180 },
  { 236, 106, 100 },
  { 212, 136, 32 },
  { 160, 170, 0 },
  { 116, 196, 0 },
  { 76, 208, 32 },
  { 56, 204, 108 },
  { 56, 180, 204 },
  { 60, 60, 60 },
  { 0, 0, 0 },
  { 0, 0, 0 },

  { 236, 238, 236 },
  { 168, 204, 236 },
  { 188, 188, 236 },
  { 212, 178, 236 },
  { 236, 174, 236 },
  { 236, 174, 212 },
  { 236, 180, 176 },
  { 228, 196, 144 },
  { 204, 210, 120 },
  { 180, 222, 120 },
  { 168, 226, 144 },
  { 152, 226, 180 },
  { 160, 214, 228 },
  { 160, 162, 160 },
  { 0, 0, 0 },
  { 0, 0, 0 },
};

#endif
