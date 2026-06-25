#ifndef __c64_h__
#define __c64_h__

/* Memory map
 * 0001-0001 Memory Map config
 * 0400-07FF Screen Memory 1000 bytes   (40x25)
 * D800-DBE7 Color RAM     1000 nybbles (40x25) ----cccc
 *
 * 8000-9FFF RAM CARTLO
 * A000-BFFF RAM CARTHI   BASIC
 * C000-CFFF RAM
 * D000-DFFF RAM CHARROM  IO
 * E000-FFFF RAM CARTHI   KERNAL
 *
 * -------- VIC registers
 * D000 SPRITE_X
 * D001 SPRITE_Y
 * ....
 * D010 SPRITE_X8             |M7X8|M6X8|M6X8|M6X8|M6X8|M6X8|M6X8|M6X8|
 * D011 CTRL1                 |RST8|ECM |BMM |DEN |RSEL|YSCROLL       |
 * D012 RASTER                |RST7|RST6|RST5|RST4|RST3|RST2|RST1|RST0|
 * D013 LIGHTPEN_X
 * D014 LIGHTPEN_Y
 * D015 SPRITE_EN             |M7E |M7E |M7E |M7E |M7E |M7E |M7E |M7E |
 * D016 CTRL2                 |    |    |RES |MCM |CSEL|XSCROLL       |
 * D017 SPRITE_Y2
 * D018 MEMPTR                |VM13|VM12|VM11|VM10|CB13|CB12|CB11|    |
 * D019 INTSTS                |EVT |              |LP  |SSCL|SBCL|RSTR|
 * D01A INTEN
 * D01B SPRITE_PRI
 * D01C SPRITE_MC
 * D01D SPRITE_X2
 * D01E SPRITE_SPR_COLL      Sprite-sprite collision
 * D01F SPRITE_BG_COLL       Sprite-background collision
 * D020 BORDER_CLR           ----cccc
 * D021 BG0_CLR              ----cccc
 * D022 BG1_CLR              ----cccc
 * D023 BG2_CLR              ----cccc
 * D024 BG3_CLR              ----cccc
 * D025 SPRITE_MM0           ----cccc
 * D026 SPRITE_MM1           ----cccc
 * D027 SPRITE_CLR 0
 * D028 SPRITE_CLR 1
 * D029 SPRITE_CLR 2
 * D02A SPRITE_CLR 3
 * D02B SPRITE_CLR 4
 * D02C SPRITE_CLR 5
 * D02D SPRITE_CLR 6
 * D02E SPRITE_CLR 7
 * D02F ... unusable
 *
 * D040-D3FF mirror
 *
 * SID
 * ---- Voice 1
 * D400 Freq Lo               ffffffff
 * D401 Freq Hi               ffffffff
 * D402 Duty Lo               dddddddd
 * D403 Duty Hi               ----dddd
 * D404 Control               npstTRSG
 * D405 Attack/decay          aaaadddd
 * D406 Sustain/release       ssssrrrr
 * ---- Voice 2
 * D407 Freq Lo               ffffffff
 * D408 Freq Hi               ffffffff
 * D409 Duty Lo               dddddddd
 * D40a Duty Hi               ----dddd
 * D40b Control               npstTRSG
 * D40c Attack/decay          aaaadddd
 * D40d Sustain/release       ssssrrrr
 * ---- Voice 3
 * D40e Freq Lo               ffffffff
 * D40f Freq Hi               ffffffff
 * D410 Duty Lo               dddddddd
 * D411 Duty Hi               ----dddd
 * D412 Control               npstTRSG
 * D413 Attack/decay          aaaadddd
 * D414 Sustain/release       ssssrrrr
 * D500..D7FF mirrored
 */

/* VIC memory map. Select=CIA2.0x0A[0:1]
 * ---- Bank 0
 * 0000-0FFF RAM
 * 1000-1FFF Char ROM
 * 2000-2FFF RAM
 * 3000-3FFF RAM
 * ---- Bank 1
 * 4000-4FFF RAM
 * 5000-5FFF RAM
 * 6000-6FFF RAM
 * 7000-7FFF RAM
 * ---- Bank 2
 * 8000-8FFF RAM
 * 9000-9FFF Char ROM
 * A000-AFFF RAM
 * B000-BFFF RAM
 * ---- Bank 3
 * C000-CFFF RAM
 * D000-DFFF RAM
 * E000-EFFF RAM
 * F000-FFFF RAM
 */
/*
 * 320x200, Char 2 colors, 8 pixels per byte
 *   Screen:  0400-07FF (40x25)
 *   Charset  D000-DFFF (256*8)
 *   CharBase:D018:---ccc- 
 *   abcdefgh
 *      0 : Background 0  :D021
 *      1 : Color         :D800-DBFF
 *
 * 320x200, Bitmap 2 colors, 8 pixels per byte
 *   Screen Ram is bg/fg color
 *   Charset 0000/2000 is 1bpp
 * 
 * 160x200 MChar, 4 colors, 4 pixels per byte
 *   Screen: 0400-07FF (40x25)
 *   Charset D000-DFFF (256*8)
 *   CharBase:D018:---ccc- 
 *   aabbccdd
 *      00 : Background 0 :D021
 *      01 : Background 1 :D022
 *      10 : Background 2 :D023
 *      11 : Color        :D800-DBFF
 *
 * I/O
 * D016 Multicolor   ---m.----
 * D018 Charset base ----.bbb- 0000/0800/1000/1800/2000/2800/3000/3800
 *
 * D000 Charset Low
 * D800 Charset High
 */

/* CIA1 registers
 * +-----+-----+-----+-----+-----+-----+-----+-----+
 * |krow7|krow6|krow5|krow4|krow3|krow2|krow1|krow0| pra
 * |     |     |     |j2f  |j2rt |j2lf |j2dn |j2up | 
 * +-----+-----+-----+-----+-----+-----+-----+-----+
 * |kcol7|kcol6|kcol5|kcol4|kcol3|kcol2|kcol1|kcol0| pra
 * |     |     |     |j1f  |j1rt |j1lf |j1dn |j1up | 
 * +-----+-----+-----+-----+-----+-----+-----+-----+
 * CIA2
 * +-----+-----+-----+-----+-----+-----+-----+-----+
 * |datai|clki |datao|clko |atno |txd  |va15 |va14 |
 * +-----+-----+-----+-----+-----+-----+-----+-----+
 * |uptmb|uptmb|pb5  |pb4  |pb3  |pb2  |pb1  |pb0  |
 * +-----+-----+-----+-----+-----+-----+-----+-----+
 *  cia2.ddra
 *    3,4,5 = o
 *    6,7 = i
 */
#define CHIPREG(o)				\
  o(0x0000, CPU_DDR)				\
  o(0x0001, CPU_PORT)				\
  o(0x0314, IRQ_ADDR)				\
  o(0x0316, BRK_ADDR)				\
  o(0x0318, NMI_ADDR)				\
  o(0xD000, SPRITE0_X)				\
  o(0xD001, SPRITE0_Y)				\
  o(0xD002, SPRITE1_X)				\
  o(0xD003, SPRITE1_Y)				\
  o(0xD004, SPRITE2_X)				\
  o(0xD005, SPRITE2_Y)				\
  o(0xD006, SPRITE3_X)				\
  o(0xD007, SPRITE3_Y)				\
  o(0xD008, SPRITE4_X)				\
  o(0xD009, SPRITE4_Y)				\
  o(0xD00A, SPRITE5_X)				\
  o(0xD00B, SPRITE5_Y)				\
  o(0xD00C, SPRITE6_X)				\
  o(0xD00D, SPRITE6_Y)				\
  o(0xD00E, SPRITE7_X)				\
  o(0xD00F, SPRITE7_Y)				\
  o(0xD010, SPRITE_X8)				\
  o(0xD011, CTRL1)				\
  o(0xD012, RASTER)				\
  o(0xD013, LP_X)				\
  o(0xD014, LP_Y)				\
  o(0xD015, SPRITE_EN)				\
  o(0xD016, CTRL2)				\
  o(0xD017, SPRITE_Y2)				\
  o(0xD018, MEMPTR)				\
  o(0xD019, INTSTS)				\
  o(0xD01A, INTEN)				\
  o(0xD01B, SPRITE_PRI)				\
  o(0xD01C, SPRITE_MC)				\
  o(0xD01D, SPRITE_X2)				\
  o(0xD01E, SPRITE_SPRITE)			\
  o(0xD01F, SPRITE_DATA)			\
  o(0xD020, BORDER_CLR)				\
  o(0xD021, BG0_CLR)				\
  o(0xD022, BG1_CLR)				\
  o(0xD023, BG2_CLR)				\
  o(0xD024, BG3_CLR)				\
  o(0xD025, SPRITE_MC0)				\
  o(0xD026, SPRITE_MC1)				\
  o(0xD027, SPRITE0_CLR)			\
  o(0xD028, SPRITE1_CLR)			\
  o(0xD029, SPRITE2_CLR)			\
  o(0xD02A, SPRITE3_CLR)			\
  o(0xD02B, SPRITE4_CLR)			\
  o(0xD02C, SPRITE5_CLR)			\
  o(0xD02D, SPRITE6_CLR)			\
  o(0xD02E, SPRITE7_CLR)			\
  o(0xD400, SID0_FREQLO)			\
  o(0xD401, SID0_FREQHI)			\
  o(0xD402, SID0_PWMLO)				\
  o(0xD403, SID0_PWMHI)				\
  o(0xD404, SID0_CTL)				\
  o(0xD405, SID0_AD)				\
  o(0xD406, SID0_SR)				\
  o(0xD407, SID1_FREQLO)			\
  o(0xD408, SID1_FREQHI)			\
  o(0xD409, SID1_PWMLO)				\
  o(0xD40A, SID1_PWMHI)				\
  o(0xD40B, SID1_CTL)				\
  o(0xD40C, SID1_AD)				\
  o(0xD40D, SID1_SR)				\
  o(0xD40E, SID2_FREQLO)			\
  o(0xD40F, SID2_FREQHI)			\
  o(0xD410, SID2_PWMLO)				\
  o(0xD411, SID2_PWMHI)				\
  o(0xD412, SID2_CTL)				\
  o(0xD413, SID2_AD)				\
  o(0xD414, SID2_SR)				\
  o(0xD415, SID_FCLO)				\
  o(0xD416, SID_FCHI)				\
  o(0xD417, SID_RES)				\
  o(0xD418, SID_MODEVOL)			\
  o(0xD419, SID_POTX)				\
  o(0xD41A, SID_POTY)				\
  o(0xD41B, SID_OSC3)				\
  o(0xD41C, SID_ENV3)				\
  o(0xDC00, CIA1_PRA)				\
  o(0xDC01, CIA1_PRB)				\
  o(0xDC02, CIA1_DDRA)				\
  o(0xDC03, CIA1_DDRB)				\
  o(0xDC04, CIA1_TALO)				\
  o(0xDC05, CIA1_TAHI)				\
  o(0xDC06, CIA1_TBLO)			\
  o(0xDC07, CIA1_TBHI)				\
  o(0xDC08, CIA1_TOD10THS)			\
  o(0xDC09, CIA1_TODSEC)			\
  o(0xDC0a, CIA1_TODMIN)			\
  o(0xDC0b, CIA1_TODHR)				\
  o(0xDC0c, CIA1_SDR)				\
  o(0xDC0d, CIA1_ICR)				\
  o(0xDC0e, CIA1_CRA)				\
  o(0xDC0f, CIA1_CRB)				\
  o(0xDD00, CIA2_PRA)				\
  o(0xDD01, CIA2_PRB)				\
  o(0xDD02, CIA2_DDRA)				\
  o(0xDD03, CIA2_DDRB)				\
  o(0xDD04, CIA2_TALO)				\
  o(0xDD05, CIA2_TAHI)				\
  o(0xDD06, CIA2_TBLO)				\
  o(0xDD07, CIA2_TBHI)				\
  o(0xDD08, CIA2_TOD10THS)			\
  o(0xDD09, CIA2_TODSEC)			\
  o(0xDD0a, CIA2_TODMIN)			\
  o(0xDD0b, CIA2_TODHR)				\
  o(0xDD0c, CIA2_SDR)				\
  o(0xDD0d, CIA2_ICR)				\
  o(0xDD0e, CIA2_CRA)				\
  o(0xDD0f, CIA2_CRB)				\

enum {
#define o(a,n) n = a,
  CHIPREG(o)
#undef o
};

#endif
