#ifndef __snesreg_h__
#define __snesreg_h__

#include <map>
#include <string>
/*
  o(// HDMA
  o(// 43x0 DMAxPARAM DA-IINNN
  o(// 43x1 DMAxADDB  bbbbbbbb ppu address lo (+2100)
  o(// 43x2 DMAxADDAL aaaaaaaa 24-bit cpu address, lo
  o(// 43x3 DMAxADDAM aaaaaaaa 24-bit cpu address, hi
  o(// 43x4 DMAxADDAH aaaaaaaa 24-bit cpu address, bank
  o(// 43x5 DMAxAMTL  cccccccc 16-bit counter, lo
  o(// 43x6 DMAxAMTH  cccccccc 16-bit counter, hi
  o(// 43x7 DMAxINDH  bbbbbbbb bank of cpu address
  o(// 43x8 DMAxADD2L aaaaaaaa - auto increment or decrement during dma transfer (bank = 43x4)
  o(// 43x9 DMAxADD2H aaaaaaaa
  o(// 43xA DMAxLINES llllllll
  o(// 43xB
  o(// 420b MDMAEN    enable std dma xfer
  o(// 420c HDMAEN    enable hdma xfer

  * 2107 aaaaaass BG1 Tile Map Location [a*2048 = address, ss = 32x32, 64x32, 32x64, 64x64
  * 2108 aaaaaass BG2 Tile Map Location [a*2048 = address, ss = 32x32, 64x32, 32x64, 64x64
  * 2109 aaaaaass BG3 Tile Map Location [a*2048 = address, ss = 32x32, 64x32, 32x64, 64x64
  * 210a aaaaaass BG3 Tile Map Location [a*2048 = address, ss = 32x32, 64x32, 32x64, 64x64
  *
  * 210b 22221111 BG1 CHR Address [a*8192 = address]
  * 210c 44443333
  *
  * 2115 i---ffrr : Inrement mode (i=0: 2118/2139, i=1: 2119/213a, rr=2, 64, 128, 256)
  *
  * 2116 aaaaaaaa : VRAM address [a*2 = address]
  * 2117 aaaaaaaa
  *
  * 2118 dddddddd : VRAM data write
  * 2119 dddddddd
  *
  * 2139 dddddddd : VRAM data read
  * 213a dddddddd
*/

// https://problemkaputt.de/fullsnes.htm
#define SNESREG(o)							\
  o(INIDISP      , 0x2100, "w-")					\
  o(OBSEL        , 0x2101, "w-")					\
  o(BGMODE       , 0x2105, "w-")					\
  o(MOSAIC       , 0x2106, "w-")					\
  o(BG1SC        , 0x2107, "w-")					\
  o(BG2SC        , 0x2108, "w-")					\
  o(BG3SC        , 0x2109, "w-")					\
  o(BG4SC        , 0x210A, "w-")					\
  o(BG12NBA      , 0x210B, "w-")					\
  o(BG34NBA      , 0x210C, "w-")					\
  o(BG1HOFS      , 0x210D, "w-:2")					\
  o(BG1VOFS      , 0x210E, "w-:2")					\
  o(BG2HOFS      , 0x210F, "w-:2")					\
  o(BG2VOFS      , 0x2110, "w-:2")					\
  o(BG3HOFS      , 0x2111, "w-:2")					\
  o(BG3VOFS      , 0x2112, "w-:2")					\
  o(BG4HOFS      , 0x2113, "w:-2")					\
  o(BG4VOFS      , 0x2114, "w:-2")					\
									\
  o(OAMADDL      , 0x2102, "w-")					\
  o(OAMADDH      , 0x2103, "w-")					\
  o(OAMDATA      , 0x2104, "w-:2")					\
  o(RDOAM        , 0x2138, "r-:2")					\
									\
  o(VMAIN        , 0x2115, "w-")					\
  o(VMADDL       , 0x2116, "w-")					\
  o(VMADDH       , 0x2117, "w-")					\
  o(VMDATAL      , 0x2118, "w-")					\
  o(VMDATAH      , 0x2119, "w-")					\
  o(RDVRAML      , 0x2139, "r-")					\
  o(RDVRAMH      , 0x213A, "r-")					\
									\
  o(CGADD        , 0x2121, "w-")					\
  o(CGDATA       , 0x2122, "w-")					\
  o(RDCGRAM      , 0x213B, "r-:2")					\
									\
  o(M7SEL        , 0x211A, "w-")					\
  o(M7A          , 0x211B, "w-:2")					\
  o(M7B          , 0x211C, "w-:2")					\
  o(M7C          , 0x211D, "w-:2")					\
  o(M7D          , 0x211E, "w-:2")					\
  o(M7X          , 0x211F, "w-:2")					\
  o(M7Y          , 0x2120, "w-:2")					\
									\
  o(W12SEL       , 0x2123, "w-")					\
  o(W34SEL       , 0x2124, "w-")					\
  o(WOBJSEL      , 0x2125, "w-")					\
  o(WH0          , 0x2126, "w-")					\
  o(WH1          , 0x2127, "w-")					\
  o(WH2          , 0x2128, "w-")					\
  o(WH3          , 0x2129, "w-")					\
  o(WBGLOG       , 0x212A, "w-")					\
  o(WOBJLOG      , 0x212B, "w-")					\
  o(TM           , 0x212C, "w-")					\
  o(TS           , 0x212D, "w-")					\
  o(TMW          , 0x212E, "w-")					\
  o(TSW          , 0x212F, "w-")					\
  o(CGWSEL       , 0x2130, "w-")					\
  o(CGADSUB      , 0x2131, "w-")					\
  o(COLDATA      , 0x2132, "w-")					\
  o(SETINI       , 0x2133, "w-")					\
  o(MPYL         , 0x2134, "r-")					\
  o(MPYM         , 0x2135, "r-")					\
  o(MPYH         , 0x2136, "r-")					\
  o(SLHV         , 0x2137, "r-")					\
  o(OPHCT        , 0x213C, "r-:2")					\
  o(OPVCT        , 0x213D, "r-:2")					\
  o(STAT77       , 0x213E, "r-")					\
  o(STAT78       , 0x213F, "r-")					\
									\
  o(APUI00       , 0x2140, "rw")					\
  o(APUI01       , 0x2141, "rw")					\
  o(APUI02       , 0x2142, "rw")					\
  o(APUI03       , 0x2143, "rw")					\
									\
  o(WMDATA       , 0x2180, "rw")					\
  o(WMADDL       , 0x2181, "w-")					\
  o(WMADDM       , 0x2182, "w-")					\
  o(WMADDH       , 0x2183, "w-")					\
									\
  o(NMITIMEN     , 0x4200, "ow")					\
  o(WRIO         , 0x4201, "ow")					\
  o(WRMPYA       , 0x4202, "ow")					\
  o(WRMPYB       , 0x4203, "ow")					\
  o(WRDIVL       , 0x4204, "ow")					\
  o(WRDIVH       , 0x4205, "ow")					\
  o(WRDIVB       , 0x4206, "ow")					\
									\
  o(HTIMEL       , 0x4207, "ow")					\
  o(HTIMEH       , 0x4208, "ow")					\
  o(VTIMEL       , 0x4209, "ow")					\
  o(VTIMEH       , 0x420A, "ow")					\
  o(MDMAEN       , 0x420B, "ow")					\
  o(HDMAEN       , 0x420C, "ow")					\
  o(MEMSEL       , 0x420D, "ow")					\
  o(RDNMI        , 0x4210, "r-")					\
  o(TIMEUP       , 0x4211, "r-")					\
  o(HVBJOY       , 0x4212, "r-")					\
  o(RDIO         , 0x4213, "r-")					\
  o(RDDIVL       , 0x4214, "r-")					\
  o(RDDIVH       , 0x4215, "r-")					\
  o(RDMPYL       , 0x4216, "r-")					\
  o(RDMPYH       , 0x4217, "r-")					\
									\
  o(JOY1L        , 0x4218, "r-")					\
  o(JOY1H        , 0x4219, "r-")					\
  o(JOY2L        , 0x421A, "r-")					\
  o(JOY2H        , 0x421B, "r-")					\
  o(JOY3L        , 0x421C, "r-")					\
  o(JOY3H        , 0x421D, "r-")					\
  o(JOY4L        , 0x421E, "r-")					\
  o(JOY4H        , 0x421F, "r-")					\
									\
  o(DMA0PARAM    , 0x4300, "w-")					\
  o(DMA0PPU      , 0x4301, "w-")					\
  o(DMA0ADDRL    , 0x4302, "w-")					\
  o(DMA0ADDRM    , 0x4303, "w-")					\
  o(DMA0ADDRH    , 0x4304, "w-")					\
									\
  o(DMA1PARAM    , 0x4310, "w-")					\
  o(DMA1PPU      , 0x4311, "w-")					\
  o(DMA1ADDRL    , 0x4312, "w-")					\
  o(DMA1ADDRM    , 0x4313, "w-")					\
  o(DMA1ADDRH    , 0x4314, "w-")					\
									\
  o(DMA2PARAM    , 0x4320, "w-")					\
  o(DMA2PPU      , 0x4321, "w-")					\
  o(DMA2ADDRL    , 0x4322, "w-")					\
  o(DMA2ADDRM    , 0x4323, "w-")					\
  o(DMA2ADDRH    , 0x4324, "w-")					\
									\
  o(DMA3PARAM    , 0x4330, "w-")					\
  o(DMA3PPU      , 0x4331, "w-")					\
  o(DMA3ADDRL    , 0x4332, "w-")					\
  o(DMA3ADDRM    , 0x4333, "w-")					\
  o(DMA3ADDRH    , 0x4334, "w-")					\
									\
  o(DMA4PARAM    , 0x4340, "w-")					\
  o(DMA4PPU      , 0x4341, "w-")					\
  o(DMA4ADDRL    , 0x4342, "w-")					\
  o(DMA4ADDRM    , 0x4343, "w-")					\
  o(DMA4ADDRH    , 0x4344, "w-")					\
									\
  o(DMA5PARAM    , 0x4350, "w-")					\
  o(DMA5PPU      , 0x4351, "w-")					\
  o(DMA5ADDRL    , 0x4352, "w-")					\
  o(DMA5ADDRM    , 0x4353, "w-")					\
  o(DMA5ADDRH    , 0x4354, "w-")					\
									\
  o(DMA6PARAM    , 0x4360, "w-")					\
  o(DMA6PPU      , 0x4361, "w-")					\
  o(DMA6ADDRL    , 0x4362, "w-")					\
  o(DMA6ADDRM    , 0x4363, "w-")					\
  o(DMA6ADDRH    , 0x4364, "w-")					\
									\
  o(DMA7PARAM    , 0x4370, "w-")					\
  o(DMA7PPU      , 0x4371, "w-")					\
  o(DMA7ADDRL    , 0x4372, "w-")					\
  o(DMA7ADDRM    , 0x4373, "w-")					\
  o(DMA7ADDRH    , 0x4374, "w-")					\


enum {
#define o(name, addr, f) name = addr,
  SNESREG(o)
#undef o
};

uint8_t ioregs[0x43FF];

#define SNESREG8(name)  uint8_t&  R_##name = ioregs[name]
#define SNESREG16(name) uint16_t& R_##name = *(uint16_t *)&ioregs[name]
#define SNESREG24(name) uint24_t  R_##name = uint24_t(&ioregs[name])

SNESREG8(APUI00);
SNESREG8(APUI01);
SNESREG8(APUI02);
SNESREG8(APUI03);
SNESREG8(CGADD);
SNESREG8(VMAIN);
SNESREG8(RDNMI);
SNESREG8(HVBJOY);
SNESREG8(JOY1H);
SNESREG8(JOY1L);
SNESREG8(HDMAEN);
SNESREG16(VMADDL);
SNESREG24(WMADDL);

static inline volatile uint8_t& sr8(const int addr) {
  return ioregs[addr];
}
static inline volatile uint16_t& sr16(const int addr) {
  return *(volatile uint16_t *)&ioregs[addr];
}

uint32_t sr24(int addr) {
  uint32_t ll, mm, hh;
  ll = ioregs[addr + 0];
  mm = ioregs[addr + 1];
  hh = ioregs[addr + 2];
  return (ll + (mm << 8) + (hh << 16));
}

std::map<uint32_t, std::string> rmap;

const char *iorname(int addr) {
  return rmap[addr].c_str();
}

static int iorinit()
{
#define o(name, addr, xxx) rmap[addr] = #name;
  SNESREG(o)
#endif
    return rmap.size();
}

static int ior_done = iorinit();
