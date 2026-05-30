#ifndef __amiga_h__
#define __amiga_h__

#define RAM_START     0x000000
#define RAM_END       0x1FFFFF

#define ERAM_START    0xC00000
#define ERAM_END      0xDEFFFF

#define CHIPREG_START 0xDFF000
#define CHIPREG_END   0xDFFFFF

#define ROM_START     0xF80000
#define ROM_END       0xFFFFFF

#define CIAA_START    0xBFE000
#define CIAA_END      0xBFEFFF

#define CIAB_START    0xBFD000
#define CIAB_END      0xBFDFFF

/* Interrupt lines: INTENA/INTREQ
 * https://sites.google.com/one-n.co.uk/amiga-guides/amiga-interrupt-signals
 *  bit lvl name
 *   00  1  TBE           Serial TX Buffer Empty
 *   01  1  DSKBLK        Disk Block Complete
 *   02  1  SOFT          Software interrupt
 *   ---------------
 *   03  2  PORTS         INT2 and CIA_A
 *   ---------------
 *   04  3  COPER         Copper
 *   05  3  VERTB         Vertical Blank
 *   06  3  BLIT          Blitter finished
 *   ---------------
 *   07  4  AUD0          Audio 0 finished
 *   08  4  AUD1          Audio 1 finished
 *   09  4  AUD2          Audio 2 finished
 *   10  4  AUD3          Audio 3 finished
 *   ---------------
 *   11  5  RBF           Serial RX Buffer Full
 *   12  5  DSKSYN        Disk sync register
 *   ---------------
 *   13  6  EXTER         INT6 and CIA_B
 *   14  6  INTEN         Master interrupt (INENA only)
 *   ---------------
 *   15  -  SETCLR        set/clear control bit
 */
enum {
  IRQ_TBE    = 0x0001,
  IRQ_DSKBLK = 0x0002,
  IRQ_SOFT   = 0x0004,
  IRQ_PORTS  = 0x0008,
  IRQ_CIAA   = 0x0008,
  IRQ_COPER  = 0x0010,
  IRQ_VERTB  = 0x0020,
  IRQ_BLIT   = 0x0040,
  IRQ_AUD0   = 0x0080,
  IRQ_AUD1   = 0x0100,
  IRQ_AUD2   = 0x0200,
  IRQ_AUD3   = 0x0400,
  IRQ_RBF    = 0x0800,
  IRQ_DSKSYN = 0x1000,
  IRQ_EXTER  = 0x2000,
  IRQ_CIAB   = 0x2000,
  IRQ_INTEN  = 0x4000,
  
  IRQ_SET    = 0x8000,
  IRQ_CLR    = 0x0000,
};

/* IRQ slots */
enum {
  INTEN_IRQ  = (0x0e),
  EXTER_IRQ  = (0x6d),
  DSKSYN_IRQ = (0x5c), 
  RBF_IRQ    = (0x5b), 
  AUD0_IRQ   = (0x47), 
  AUD1_IRQ   = (0x48), 
  AUD2_IRQ   = (0x49), 
  AUD3_IRQ   = (0x4a), 
  BLIT_IRQ   = (0x36), 
  VERB_IRQ   = (0x35),
  COPER_IRQ  = (0x34),
  PORTS_IRQ  = (0x23),
  SOFT_IRQ   = (0x12),
  DSKBLK_IRQ = (0x11),
  TBE_IRQ    = (0x10),
};

/* DMACON */
enum dmacon {
  DMA_AUD0EN = 0x0001,
  DMA_AUD1EN = 0x0002,
  DMA_AUD2EN = 0x0004,
  DMA_AUD3EN = 0x0008,
  DMA_DSKEN  = 0x0010,
  DMA_SPREN  = 0x0020,
  DMA_BLTEN  = 0x0040,
  DMA_COPEN  = 0x0080,
  DMA_BPEN   = 0x0100,
  DMA_DMAEN  = 0x0200,
  DMA_BLTPRI = 0x0400,
  DMA_BZERO  = 0x2000,
  DMA_BBUSY  = 0x4000,
};

/* BPLCON
 * |HIRES|BPU2 |BPU1 |BPU0 |HOMOD|DBLPF|COLOR|GAUD |XXXXX|XXXXX|XXXXX|XXXXX|LPEN |LACE |ERSY |XXXXX|
 * |     |     |     |     |     |     |     |     |PF2H3|PF2H2|PF2H1|PF2H0|PF1H3|HF1H2|PF1H1|PF1H0|
 * |     |     |     |     |     |     |     |     |     |PF2PR|PF2P2|PF2P1|PF2P0|PF1P2|PF1P1|PF1P0|
 */
enum bplcon {
  BPL_ECSENA = 0x0001,
  BPL_ERSY   = 0x0002,
  BPL_LACE   = 0x0004,
  BPL_LPEN   = 0x0008,
  BPL_BPU3   = 0x0010,
  BPL_BYPASS = 0x0020,
  BPL_SHRES  = 0x0040,
  BPL_UHRES  = 0x0080,
  BPL_GAUD   = 0x0100,
  BPL_COLOR  = 0x0200,
  BPL_DPF    = 0x0400,
  BPL_HAM    = 0x0800,
  BPL_BPU0   = 0x1000,
  BPL_BPU1   = 0x2000,
  BPL_BPU2   = 0x4000,
  BPL_HIRES  = 0x8000,
};

enum bltcon {
  BLTCON1_SIGN = (1L << 6),
  BLTCON1_SUD  = (1L << 4),
  BLTCON1_SUL  = (1L << 3),
  BLTCON1_AUL  = (1L << 2),

  BLTCON1_EFE  = (1L << 4),
  BLTCON1_IFE  = (1L << 3),
  BLTCON1_FCE  = (1L << 2),
  BLTCON1_DESC = (1L << 1),
};

enum ciacon {
    /* Port A */
  PA7       = 0x80,
  PA6       = 0x40,
  PA5       = 0x20, // disk rdy
  PA4       = 0x10, // disk at track 0
  PA3       = 0x08, // disk write protect?
  PA2       = 0x04, // disk change
  PA1       = 0x02, // power led
  PA0       = 0x01, // rom overlay

  /* Port B */
  PB7       = 0x80, // motor
  PB6       = 0x40, // sel
  PB5       = 0x20, // sel
  PB4       = 0x10, // sel
  PB3       = 0x08, // sel
  PB2       = 0x04, // side
  PB1       = 0x02, // dir
  PB0       = 0x01, // step

  // PRA
  PRA_RDY   = PA5, // disk ready
  PRA_TK0   = PA4, // disk at track 0

  // PRB
  ___       = 0,
  PRB_MTR   = PB7,              // motor
  PRB_SEL   = PB6|PB5|PB4|PB3,  // select mask
  PRB_SEL0  = PB6|PB5|PB4|___,  // disk 0
  PRB_SEL1  = PB6|PB5|___|PB3,  // disk 1
  PRB_SEL2  = PB6|___|PB4|PB3,  // disk 2
  PRB_SEL3  = ___|PB5|PB4|PB3,  // disk 3
  PRB_SIDE  = PB2, // disk side
  PRB_DIR   = PB1, // disk direction
  PRB_STEP  = PB0, // disk step

  // CRA
  CRx_START   = 0x01,
  CRx_PBON    = 0x02,
  CRx_OUTMODE = 0x04,
  CRx_ONESHOT = 0x08,
  CRx_LOAD    = 0x10,
  CRx_INMODE  = 0x20,
  CRx_SPMODE  = 0x40,

  ICR_TA      = 0x01,
  ICR_TB      = 0x02,
  ICR_ALRM    = 0x04,
  ICR_SP      = 0x08,
  ICR_FLAG    = 0x10,
};
// https://github.com/niklasekstrom/cia-verilog/blob/master/cia.v
// pins PA[n] = ddra[n] ? pra[n] : 1;
// pins PB[n] = ddrb[n] ? prb[n] : 1;
/* CIA.A -> INT2
 *   +----+----+----+----+----+----+----+----+
 * 0 |fir1|fir0|rdy |tk0 |wpro|chng|led |ovl | pra
 *   +----+----+----+----+----+----+----+----+
 * 1 |    parallel port                      | prb
 *   +----+----+----+----+----+----+----+----+
 * 2 |    direction for port a               | ddra 03 0000.0011
 *   +----+----+----+----+----+----+----+----+
 * 3 |    direction for port b               | ddrb
 *   +----+----+----+----+----+----+----+----+
 * 4 |    timer a low                        | talo
 *   +----+----+----+----+----+----+----+----+
 * 5 |    timer a hi                         | tahi
 *   +----+----+----+----+----+----+----+----+
 * 6 |    timer b low                        | tblo
 *   +----+----+----+----+----+----+----+----+
 * 7 |    timer h hi                         | tbhi
 *   +----+----+----+----+----+----+----+----+
 * 8 |    50/60 hz timer                     | todlo
 *   +----+----+----+----+----+----+----+----+
 * 9 |    50/60 hz timer                     | todmid
 *   +----+----+----+----+----+----+----+----+
 * a |    50/60 hz timer                     | todhi
 *   +----+----+----+----+----+----+----+----+
 * b |    unused                             |
 *   +----+----+----+----+----+----+----+----+
 * c |    sdr (keyboard)                     | sdr
 *   +----+----+----+----+----+----+----+----+
 * d | IR | 0  | 0  |FLG |SP  |ALRM| TA | TB | icr
 *   +----+----+----+----+----+----+----+----+
 * e |    |SPMD|INMD|LOAD|RNMD|OUTM|PBON|STRT| cra
 *   |     0   START timera
 *   |     1   PBON
 *   |     2   OUTMODE
 *   |     3   RUNMODE (1=oneshot, 0=continuous)
 *   |     4   LOAD
 *   |     5   INMODE
 *   |     6   SPMODE
 *   +----+----+----+----+----+----+----+----+
 * f |ALRM|IMMD|INMD|LOAD|RNMD|OUTM|PBON|STRT| crb
 *   |     0   START timerb
 *   |     1   PBON
 *   |     2   OUTMODE
 *   |     3   RUNMODE (1=oneshot, 0=continuous)
 *   |     4   LOAD
 *   |     5,6 INMODE
 *   |     7   ALARM
 *   +----+----+----+----+----+----+----+----+
 *
 * CIA.B -> INT6
 *   +----+----+----+----+----+----+----+----+
 *   |dtr |rts |cd  |cts |dsr |sel |pout|busy| pra (serial)
 *   +----+----+----+----+----+----+----+----+
 *   |mtr |sel3|sel2|sel1|sel0|side|dir |step| prb (floppy) 
 *   +----+----+----+----+----+----+----+----+
 *   |    direction for port a               | ddra c0 1100.0000
 *   +----+----+----+----+----+----+----+----+
 *   |    direction for port b               | ddrb ff 1111.1111
 *   +----+----+----+----+----+----+----+----+
 *   |    timer a lo                         | talo
 *   +----+----+----+----+----+----+----+----+
 *   |    timer a hi                         | tahi
 *   +----+----+----+----+----+----+----+----+
 *   |    timer b lo                         | tblo
 *   +----+----+----+----+----+----+----+----+
 *   |    timer b hi                         | tbhi
 *   +----+----+----+----+----+----+----+----+
 *   |    horiz sync                         | todlo
 *   +----+----+----+----+----+----+----+----+
 *   |    horiz sync                         | todmid
 *   +----+----+----+----+----+----+----+----+
 *   |    horiz sync                         | todhi
 *   +----+----+----+----+----+----+----+----+
 *   |    unused                             |
 *   +----+----+----+----+----+----+----+----+
 *   |    sdr (keyboard)                     | sdr
 *   +----+----+----+----+----+----+----+----+
 *   | IR | 0  | 0  |FLG |SP  |ALRM| TA | TB | icr
 *   +----+----+----+----+----+----+----+----+
 *   | xx |SPMD|INMD|LOAD|RNMD|OTMD|PRON|STRT| cra
 *   +----+----+----+----+----+----+----+----+
 *   |ALRM| INMODE  |LOAD|RNMD|OTMD|PRON|STRT| crb
 *   +----+----+----+----+----+----+----+----+
 */
#define CHIPREG(o) \
  o(0xbfe001, "RW", CIAA_PRA,  "CIA.A pra")				\
  o(0xbfe101, "RW", CIAA_PRB,  "CIA.A prb")				\
  o(0xbfe201, "RW", CIAA_DDRA, "CIA.A ddra")				\
  o(0xbfe301, "RW", CIAA_DDRB, "CIA.A ddrb")				\
  o(0xbfe401, "RW", CIAA_TALO, "CIA.A talo")				\
  o(0xbfe501, "RW", CIAA_TAHI, "CIA.A tahi")				\
  o(0xbfe601, "RW", CIAA_TBLO, "CIA.A tblo")				\
  o(0xbfe701, "RW", CIAA_TBHI, "CIA.A tbhi")				\
  o(0xbfe801, "RW", CIAA_TOD0, "CIA.A tod0")				\
  o(0xbfe901, "RW", CIAA_TOD1, "CIA.A tod1")				\
  o(0xbfea01, "RW", CIAA_TOD2, "CIA.A tod2")				\
  o(0xbfeb01, "RW", CIAA_TOD3, "CIA.A tod3")				\
  o(0xbfec01, "RW", CIAA_SDR,  "CIA.A sdr")				\
  o(0xbfed01, "RW", CIAA_ICR,  "CIA.A icr")				\
  o(0xbfee01, "RW", CIAA_CRA,  "CIA.A cra")				\
  o(0xbfef01, "RW", CIAA_CRB,  "CIA.A crb")				\
  o(0xbfd000, "RW", CIAB_PRA,  "CIA.B pra")				\
  o(0xbfd100, "RW", CIAB_PRB,  "CIA.B prb")				\
  o(0xbfd200, "RW", CIAB_DDRA, "CIA.B ddra")				\
  o(0xbfd300, "RW", CIAB_DDRB, "CIA.B ddrb")				\
  o(0xbfd400, "RW", CIAB_TALO, "CIA.B talo")				\
  o(0xbfd500, "RW", CIAB_TAHI, "CIA.B tahi")				\
  o(0xbfd600, "RW", CIAB_TBLO, "CIA.B tblo")				\
  o(0xbfd700, "RW", CIAB_TBHI, "CIA.B tbhi")				\
  o(0xbfd800, "RW", CIAB_TOD0, "CIA.B tod0")				\
  o(0xbfd900, "RW", CIAB_TOD1, "CIA.B tod1")				\
  o(0xbfda00, "RW", CIAB_TOD2, "CIA.B tod2")				\
  o(0xbfdb00, "RW", CIAB_TOD3, "CIA.B tod3")				\
  o(0xbfdc00, "RW", CIAB_SDR,  "CIA.B sdr")				\
  o(0xbfdd00, "RW", CIAB_ICR,  "CIA.B icr")				\
  o(0xbfde00, "RW", CIAB_CRA,  "CIA.B cra")				\
  o(0xbfdf00, "RW", CIAB_CRB,  "CIA.B crb")				\
  o(0xDFF000, "ER", BLTDDAT, "Blitter destination early read (unusable)") \
  o(0xDFF002, "R-", DMACONR, "DMA control (and blitter status) read")	\
  o(0xDFF004, "R-", VPOSR,   "Read vertical raster position bit 9 (and interlace odd/even frame)") \
  o(0xDFF006, "R-", VHPOSR,  "Rest of raster XY position - High byte: vertical, low byte: horizontal") \
  o(0xDFF008, "ER", DSKDATR, "Disk data early read (unusable)")		\
  o(0xDFF00A, "R-", JOY0DAT, "Joystick/mouse 0 data")			\
  o(0xDFF00C, "R-", JOY1DAT, "Joystick/mouse 1 data")			\
  o(0xDFF00E, "R-", CLXDAT,  "Poll (read and clear) sprite collision state") \
  o(0xDFF010, "R-", ADKCONR, "Audio, disk control register read")	\
  o(0xDFF012, "R-", POT0DAT, "Pot counter pair 0 data")			\
  o(0xDFF014, "R-", POT1DAT, "Pot counter pair 1 data")			\
  o(0xDFF016, "R-", POTGOR,  "Pot pin data read")			\
  o(0xDFF018, "R-", SERDATR, "Serial port data and status read")	\
  o(0xDFF01A, "R-", DSKBYTR, "Disk data byte and status read")		\
  o(0xDFF01C, "R-", INTENAR, "Interrupt enable bits read (intena)")	\
  o(0xDFF01E, "R-", INTREQR, "Interrupt request bits read (intreq)")	\
  o(0xDFF020, "W-", DSKPTH,  "Disk track buffer pointer (high 5 bits)") \
  o(0xDFF022, "W-", DSKPTL,  "Disk track buffer pointer (low 15 bits)") \
  o(0xDFF024, "W-", DSKLEN,  "Disk track buffer length")		\
  o(0xDFF026, "W-", DSKDAT,  "Disk DMA data write")			\
  o(0xDFF028, "W-", REFPTR,  "AGA: Refresh pointer")			\
  o(0xDFF02A, "W-", VPOSW,   "Write vert most sig. bits (and frame flop)") \
  o(0xDFF02C, "W-", VHPOSW,  "Write vert and horiz pos of beam")	\
  o(0xDFF02E, "W-", COPCON,  "Coprocessor control register (CDANG)")	\
  o(0xDFF030, "W-", SERDAT,  "Serial port data and stop bits write")	\
  o(0xDFF032, "W-", SERPER,  "Serial port period and control")		\
  o(0xDFF034, "W-", POTGO,   "Pot count start, pot pin drive enable data") \
  o(0xDFF036, "W-", JOYTEST, "Write to all 4 joystick/mouse counters at once") \
  o(0xDFF038, "S-", STREQU,  "Strobe for horiz sync with VBLANK and EQU") \
  o(0xDFF03A, "S-", STRVBL,  "Strobe for horiz sync with VBLANK")	\
  o(0xDFF03C, "S-", STRHOR,  "Strobe for horiz sync")			\
  o(0xDFF03E, "S-", STRLONG, "Strobe for identification of long/short horiz line") \
  o(0xDFF040, "W-", BLTCON0, "Blitter control reg 0")			\
  o(0xDFF042, "W-", BLTCON1, "Blitter control reg 1")			\
  o(0xDFF044, "W-", BLTAFWM, "Blitter first word mask for source A")	\
  o(0xDFF046, "W-", BLTALWM, "Blitter last word mask for source A")	\
  o(0xDFF048, "W-", BLTCPTH, "Blitter pointer to source C (high 5 bits)") \
  o(0xDFF04A, "W-", BLTCPTL, "Blitter pointer to source C (low 15 bits)") \
  o(0xDFF04C, "W-", BLTBPTH, "Blitter pointer to source B (high 5 bits)") \
  o(0xDFF04E, "W-", BLTBPTL, "Blitter pointer to source B (low 15 bits)") \
  o(0xDFF050, "W-", BLTAPTH, "Blitter pointer to source A (high 5 bits)") \
  o(0xDFF052, "W-", BLTAPTL, "Blitter pointer to source A (low 15 bits)") \
  o(0xDFF054, "W-", BLTDPTH, "Blitter pointer to destination D (high 5 bits)") \
  o(0xDFF056, "W-", BLTDPTL, "Blitter pointer to destination D (low 15 bits)") \
  o(0xDFF058, "W-", BLTSIZE, "Blitter start and size (win/width, height)") \
  o(0xDFF05A, "W-", BLTCON0L, "Blitter control 0 lower 8 bits (minterms)") \
  o(0xDFF05C, "W-", BLTSIZV, "Blitter V size (for 15 bit vert size)")	\
  o(0xDFF05E, "W-", BLTSIZH, "ECS: Blitter H size & start (for 11 bit H size)") \
  o(0xDFF060, "W-", BLTCMOD, "Blitter modulo for source C")		\
  o(0xDFF062, "W-", BLTBMOD, "Blitter modulo for source B")		\
  o(0xDFF064, "W-", BLTAMOD, "Blitter modulo for source A")		\
  o(0xDFF066, "W-", BLTDMOD, "Blitter modulo for destination D")	\
  o(0xDFF070, "W-", BLTCDAT, "Blitter source C data reg")		\
  o(0xDFF072, "W-", BLTBDAT, "Blitter source B data reg")		\
  o(0xDFF074, "W-", BLTADAT, "Blitter source A data reg")		\
  o(0xDFF078, "W-", SPRHDAT, "AGA: Ext logic UHRES sprite pointer and data identifier") \
  o(0xDFF07A, "W-", BPLHDAT, "AGA: Ext logic UHRES bit plane identifier") \
  o(0xDFF07C, "W-", LISAID, "AGA: Chip revision level for Denise/Lisa") \
  o(0xDFF07E, "W-", DSKSYNC, "Disk sync pattern")			\
  o(0xDFF080, "W-", COP1LCH, "Write Copper pointer 1 (high 5 bits)")	\
  o(0xDFF082, "W-", COP1LCL, "Write Copper pointer 1 (low 15 bits)")	\
  o(0xDFF084, "W-", COP2LCH, "Write Copper pointer 2 (high 5 bits)")	\
  o(0xDFF086, "W-", COP2LCL, "Write Copper pointer 2 (low 15 bits)")	\
  o(0xDFF088, "S-", COPJMP1, "Trigger Copper 1 (any value)")		\
  o(0xDFF08A, "S-", COPJMP2, "Trigger Copper 2 (any value)")		\
  o(0xDFF08C, "W-", COPINS, "Coprocessor inst fetch identify")		\
  o(0xDFF08E, "W-", DIWSTRT, "Display window start (upper left vert-hor pos)") \
  o(0xDFF090, "W-", DIWSTOP, "Display window stop (lower right vert-hor pos)") \
  o(0xDFF092, "W-", DDFSTRT, "Display bitplane data fetch start.hor pos") \
  o(0xDFF094, "W-", DDFSTOP, "Display bitplane data fetch stop.hor pos") \
  o(0xDFF096, "W-", DMACON, "DMA control write (clear or set)")		\
  o(0xDFF098, "W-", CLXCON, "Write Sprite collision control bits")	\
  o(0xDFF09A, "W-", INTENA, "Interrupt enable bits (clear or set bits)") \
  o(0xDFF09C, "W-", INTREQ, "Interrupt request bits (clear or set bits)") \
  o(0xDFF09E, "W-", ADKCON, "Audio, disk and UART control")		\
  o(0xDFF0A0, "W-", AUD0LCH, "Audio channel 0 pointer (high 5 bits)")	\
  o(0xDFF0A2, "W-", AUD0LCL, "Audio channel 0 pointer (low 15 bits)")	\
  o(0xDFF0A4, "W-", AUD0LEN, "Audio channel 0 length")			\
  o(0xDFF0A6, "W-", AUD0PER, "Audio channel 0 period")			\
  o(0xDFF0A8, "W-", AUD0VOL, "Audio channel 0 volume")			\
  o(0xDFF0AA, "W-", AUD0DAT, "Audio channel 0 data")			\
  o(0xDFF0B0, "W-", AUD1LCH, "Audio channel 1 pointer (high 5 bits)")	\
  o(0xDFF0B2, "W-", AUD1LCL, "Audio channel 1 pointer (low 15 bits)")	\
  o(0xDFF0B4, "W-", AUD1LEN, "Audio channel 1 length")			\
  o(0xDFF0B6, "W-", AUD1PER, "Audio channel 1 period")			\
  o(0xDFF0B8, "W-", AUD1VOL, "Audio channel 1 volume")			\
  o(0xDFF0BA, "W-", AUD1DAT, "Audio channel 1 data")			\
  o(0xDFF0C0, "W-", AUD2LCH, "Audio channel 2 pointer (high 5 bits)")	\
  o(0xDFF0C2, "W-", AUD2LCL, "Audio channel 2 pointer (low 15 bits)")	\
  o(0xDFF0C4, "W-", AUD2LEN, "Audio channel 2 length")			\
  o(0xDFF0C6, "W-", AUD2PER, "Audio channel 2 period")			\
  o(0xDFF0C8, "W-", AUD2VOL, "Audio channel 2 volume")			\
  o(0xDFF0CA, "W-", AUD2DAT, "Audio channel 2 data")			\
  o(0xDFF0D0, "W-", AUD3LCH, "Audio channel 3 pointer (high 5 bits)")	\
  o(0xDFF0D2, "W-", AUD3LCL, "Audio channel 3 pointer (low 15 bits)")	\
  o(0xDFF0D4, "W-", AUD3LEN, "Audio channel 3 length")			\
  o(0xDFF0D6, "W-", AUD3PER, "Audio channel 3 period")			\
  o(0xDFF0D8, "W-", AUD3VOL, "Audio channel 3 volume")			\
  o(0xDFF0DA, "W-", AUD3DAT, "Audio channel 3 data")			\
  o(0xDFF0E0, "W-", BPL1PTH, "Bitplane pointer 1 (high 5 bits)")	\
  o(0xDFF0E2, "W-", BPL1PTL, "Bitplane pointer 1 (low 15 bits)")	\
  o(0xDFF0E4, "W-", BPL2PTH, "Bitplane pointer 2 (high 5 bits)")	\
  o(0xDFF0E6, "W-", BPL2PTL, "Bitplane pointer 2 (low 15 bits)")	\
  o(0xDFF0E8, "W-", BPL3PTH, "Bitplane pointer 3 (high 5 bits)")	\
  o(0xDFF0EA, "W-", BPL3PTL, "Bitplane pointer 3 (low 15 bits)")	\
  o(0xDFF0EC, "W-", BPL4PTH, "Bitplane pointer 4 (high 5 bits)")	\
  o(0xDFF0EE, "W-", BPL4PTL, "Bitplane pointer 4 (low 15 bits)")	\
  o(0xDFF0F0, "W-", BPL5PTH, "Bitplane pointer 5 (high 5 bits)")	\
  o(0xDFF0F2, "W-", BPL5PTL, "Bitplane pointer 5 (low 15 bits)")	\
  o(0xDFF0F4, "W-", BPL6PTH, "Bitplane pointer 6 (high 5 bits)")	\
  o(0xDFF0F6, "W-", BPL6PTL, "Bitplane pointer 6 (low 15 bits)")	\
  o(0xDFF0F8, "W-", BPL7PTH, "AGA: Bitplane pointer 7 (high 5 bits)")	\
  o(0xDFF0FA, "W-", BPL7PTL, "AGA: Bitplane pointer 7 (low 15 bits)")	\
  o(0xDFF0FC, "W-", BPL8PTH, "AGA: Bitplane pointer 8 (high 5 bits)")	\
  o(0xDFF0FE, "W-", BPL8PTL, "AGA: Bitplane pointer 8 (low 15 bits)")	\
  o(0xDFF100, "W-", BPLCON0, "Bitplane depth and screen mode)")		\
  o(0xDFF102, "W-", BPLCON1, "Bitplane/playfield horizontal scroll values") \
  o(0xDFF104, "W-", BPLCON2, "Sprites vs. Playfields priority")		\
  o(0xDFF106, "W-", BPLCON3, "AGA: Bitplane control reg (enhanced features)") \
  o(0xDFF108, "W-", BPL1MOD, "Bitplane modulo (odd planes)")		\
  o(0xDFF10A, "W-", BPL2MOD, "Bitplane modulo (even planes)")		\
  o(0xDFF10C, "W-", BPLCON4, "AGA: Bitplane control reg (bitplane & sprite masks)") \
  o(0xDFF10E, "W-", CLXCON2, "AGA: Write Extended sprite collision control bits") \
  o(0xDFF110, "W-", BPL1DAT, "Bitplane 1 data (parallel to serial convert)") \
  o(0xDFF112, "W-", BPL2DAT, "Bitplane 2 data (parallel to serial convert)") \
  o(0xDFF114, "W-", BPL3DAT, "Bitplane 3 data (parallel to serial convert)") \
  o(0xDFF116, "W-", BPL4DAT, "Bitplane 4 data (parallel to serial convert)") \
  o(0xDFF118, "W-", BPL5DAT, "Bitplane 5 data (parallel to serial convert)") \
  o(0xDFF11A, "W-", BPL6DAT, "Bitplane 6 data (parallel to serial convert)") \
  o(0xDFF11C, "W-", BPL7DAT, "AGA: Bitplane 7 data (parallel to serial convert)") \
  o(0xDFF11E, "W-", BPL8DAT, "AGA: Bitplane 8 data (parallel to serial convert)") \
  o(0xDFF120, "W-", SPR0PTH, "Sprite 0 pointer (high 5 bits)")		\
  o(0xDFF122, "W-", SPR0PTL, "Sprite 0 pointer (low 15 bits)")		\
  o(0xDFF124, "W-", SPR1PTH, "Sprite 1 pointer (high 5 bits)")		\
  o(0xDFF126, "W-", SPR1PTL, "Sprite 1 pointer (low 15 bits)")		\
  o(0xDFF128, "W-", SPR2PTH, "Sprite 2 pointer (high 5 bits)")		\
  o(0xDFF12A, "W-", SPR2PTL, "Sprite 2 pointer (low 15 bits)")		\
  o(0xDFF12C, "W-", SPR3PTH, "Sprite 3 pointer (high 5 bits)")		\
  o(0xDFF12E, "W-", SPR3PTL, "Sprite 3 pointer (low 15 bits)")		\
  o(0xDFF130, "W-", SPR4PTH, "Sprite 4 pointer (high 5 bits)")		\
  o(0xDFF132, "W-", SPR4PTL, "Sprite 4 pointer (low 15 bits)")		\
  o(0xDFF134, "W-", SPR5PTH, "Sprite 5 pointer (high 5 bits)")		\
  o(0xDFF136, "W-", SPR5PTL, "Sprite 5 pointer (low 15 bits)")		\
  o(0xDFF138, "W-", SPR6PTH, "Sprite 6 pointer (high 5 bits)")		\
  o(0xDFF13A, "W-", SPR6PTL, "Sprite 6 pointer (low 15 bits)")		\
  o(0xDFF13C, "W-", SPR7PTH, "Sprite 7 pointer (high 5 bits)")		\
  o(0xDFF13E, "W-", SPR7PTL, "Sprite 7 pointer (low 15 bits)")		\
  o(0xDFF140, "W-", SPR0POS, "Sprite 0 vert-horiz start pos data")	\
  o(0xDFF142, "W-", SPR0CTL, "Sprite 0 position and control data")	\
  o(0xDFF144, "W-", SPR0DATA, "Sprite 0 low bitplane data")		\
  o(0xDFF146, "W-", SPR0DATB, "Sprite 0 high bitplane data")		\
  o(0xDFF148, "W-", SPR1POS, "Sprite 1 vert-horiz start pos data")	\
  o(0xDFF14A, "W-", SPR1CTL, "Sprite 1 position and control data")	\
  o(0xDFF14C, "W-", SPR1DATA, "Sprite 1 low bitplane data")		\
  o(0xDFF14E, "W-", SPR1DATB, "Sprite 1 high bitplane data")		\
  o(0xDFF150, "W-", SPR2POS, "Sprite 2 vert-horiz start pos data")	\
  o(0xDFF152, "W-", SPR2CTL, "Sprite 2 position and control data")	\
  o(0xDFF154, "W-", SPR2DATA, "Sprite 2 low bitplane data")		\
  o(0xDFF156, "W-", SPR2DATB, "Sprite 2 high bitplane data")		\
  o(0xDFF158, "W-", SPR3POS, "Sprite 3 vert-horiz start pos data")	\
  o(0xDFF15A, "W-", SPR3CTL, "Sprite 3 position and control data")	\
  o(0xDFF15C, "W-", SPR3DATA, "Sprite 3 low bitplane data")		\
  o(0xDFF15E, "W-", SPR3DATB, "Sprite 3 high bitplane data")		\
  o(0xDFF160, "W-", SPR4POS, "Sprite 4 vert-horiz start pos data")	\
  o(0xDFF162, "W-", SPR4CTL, "Sprite 4 position and control data")	\
  o(0xDFF164, "W-", SPR4DATA, "Sprite 4 low bitplane data")		\
  o(0xDFF166, "W-", SPR4DATB, "Sprite 4 high bitplane data")		\
  o(0xDFF168, "W-", SPR5POS, "Sprite 5 vert-horiz start pos data")	\
  o(0xDFF16A, "W-", SPR5CTL, "Sprite 5 position and control data")	\
  o(0xDFF16C, "W-", SPR5DATA, "Sprite 5 low bitplane data")		\
  o(0xDFF16E, "W-", SPR5DATB, "Sprite 5 high bitplane data")		\
  o(0xDFF170, "W-", SPR6POS, "Sprite 6 vert-horiz start pos data")	\
  o(0xDFF172, "W-", SPR6CTL, "Sprite 6 position and control data")	\
  o(0xDFF174, "W-", SPR6DATA, "Sprite 6 low bitplane data")		\
  o(0xDFF176, "W-", SPR6DATB, "Sprite 6 high bitplane data")		\
  o(0xDFF178, "W-", SPR7POS, "Sprite 7 vert-horiz start pos data")	\
  o(0xDFF17A, "W-", SPR7CTL, "Sprite 7 position and control data")	\
  o(0xDFF17C, "W-", SPR7DATA, "Sprite 7 low bitplane data")		\
  o(0xDFF17E, "W-", SPR7DATB, "Sprite 7 high bitplane data")		\
  o(0xDFF180, "W-", COLOR00, "Palette color 00")			\
  o(0xDFF182, "W-", COLOR01, "Palette color 1")				\
  o(0xDFF184, "W-", COLOR02, "Palette color 2")				\
  o(0xDFF186, "W-", COLOR03, "Palette color 3")				\
  o(0xDFF188, "W-", COLOR04, "Palette color 4")				\
  o(0xDFF18A, "W-", COLOR05, "Palette color 5")				\
  o(0xDFF18C, "W-", COLOR06, "Palette color 6")				\
  o(0xDFF18E, "W-", COLOR07, "Palette color 7")				\
  o(0xDFF190, "W-", COLOR08, "Palette color 8")				\
  o(0xDFF192, "W-", COLOR09, "Palette color 9")				\
  o(0xDFF194, "W-", COLOR10, "Palette color 10")			\
  o(0xDFF196, "W-", COLOR11, "Palette color 11")			\
  o(0xDFF198, "W-", COLOR12, "Palette color 12")			\
  o(0xDFF19A, "W-", COLOR13, "Palette color 13")			\
  o(0xDFF19C, "W-", COLOR14, "Palette color 14")			\
  o(0xDFF19E, "W-", COLOR15, "Palette color 15")			\
  o(0xDFF1a0, "W-", COLOR16, "Palette color 16")			\
  o(0xDFF1a2, "W-", COLOR17, "Palette color 17")			\
  o(0xDFF1a4, "W-", COLOR18, "Palette color 18")			\
  o(0xDFF1a6, "W-", COLOR19, "Palette color 19")			\
  o(0xDFF1a8, "W-", COLOR20, "Palette color 20")			\
  o(0xDFF1aA, "W-", COLOR21, "Palette color 21")			\
  o(0xDFF1aC, "W-", COLOR22, "Palette color 22")			\
  o(0xDFF1aE, "W-", COLOR23, "Palette color 23")			\
  o(0xDFF1b0, "W-", COLOR24, "Palette color 24")			\
  o(0xDFF1b2, "W-", COLOR25, "Palette color 25")			\
  o(0xDFF1b4, "W-", COLOR26, "Palette color 26")			\
  o(0xDFF1b6, "W-", COLOR27, "Palette color 27")			\
  o(0xDFF1b8, "W-", COLOR28, "Palette color 28")			\
  o(0xDFF1bA, "W-", COLOR29, "Palette color 29")			\
  o(0xDFF1bC, "W-", COLOR30, "Palette color 30")			\
  o(0xDFF1bE, "W-", COLOR31, "Palette color 31")			\
  o(0xDFF1c0, "W-", HTOTAL, "AGA: Highest number count in horiz line (VARBEAMEN = 1)") \
  o(0xDFF1c2, "W-", HSSTOP, "AGA: Horiz line pos for HSYNC stop")	\
  o(0xDFF1c4, "W-", HBSTRT, "AGA: Horiz line pos for HBLANK start")	\
  o(0xDFF1c6, "W-", HBSTOP, "AGA: Horiz line pos for HBLANK stop")	\
  o(0xDFF1c8, "W-", VTOTAL, "AGA: Highest numbered vertical line (VARBEAMEN = 1)") \
  o(0xDFF1cA, "W-", VSSTOP, "AGA: Vert line for Vsync stop")		\
  o(0xDFF1cC, "W-", VBSTRT, "AGA: Vert line for VBLANK start")		\
  o(0xDFF1cE, "W-", VBSTOP, "AGA: Vert line for VBLANK stop")		\
  o(0xDFF1d0, "W-", SPRHSTRT, "AGA: UHRES sprite vertical start")	\
  o(0xDFF1d2, "W-", SPRHSTOP, "AGA: UHRES sprite vertical stop")	\
  o(0xDFF1d4, "W-", BPLHSTRT, "AGA: UHRES bit plane vertical start")	\
  o(0xDFF1d6, "W-", BPLHSTOP, "AGA: UHRES bit plane vertical stop")	\
  o(0xDFF1d8, "W-", HHPOSW, "AGA: DUAL mode hires H beam counter write") \
  o(0xDFF1dA, "W-", HHPOSR, "AGA: DUAL mode hires H beam counter read") \
  o(0xDFF1dC, "W-", BEAMCON0, "Beam counter control register")		\
  o(0xDFF1dE, "W-", HSSTRT, "AGA: Horizontal sync start (VARHSY)")	\
  o(0xDFF1e0, "W-", VSSTRT, "AGA: Vertical sync start (VARVSY)")	\
  o(0xDFF1e2, "W-", HCENTER, "AGA: Horizontal pos for vsync on interlace") \
  o(0xDFF1e4, "W-", DIWHIGH, "AGA: Display window upper bits for start/stop") \
  o(0xDFF1e6, "W-", BPLHMOD, "AGA: UHRES bit plane modulo")		\
  o(0xDFF1e8, "W-", SPRHPTH, "AGA: UHRES sprite pointer (high 5 bits)") \
  o(0xDFF1eA, "W-", SPRHPTL, "AGA: UHRES sprite pointer (low 15 bits)") \
  o(0xDFF1eC, "W-", BPLHPTH, "AGA: VRam (UHRES) bitplane pointer (high 5 bits)") \
  o(0xDFF1eE, "W-", BPLHPTL, "AGA: VRam (UHRES) bitplane pointer (low 15 bits)") \
  o(0xDFF1fC, "W-", FMODE, "AGA: Write Fetch mode (0=OCS compatible)")	\
  o(0xDFF1fE, "W-", CUSTOM_NOOP, "No operation/NULL (Copper NOP instruction)") 

/* Use macro to create enums */
enum {
#define o(addr, mode, name, str) name = addr,
  CHIPREG(o)
#undef o
};

enum {
  HSTART_SHIFT = 1,
  HSTART_MASK = 0xff,
  HSTART8_SHIFT = 16,
  HSTART8_MASK  = 0x1,
  
  VSTART_SHIFT = 8,
  VSTART_MASK = 0xff,
};

/* video modes
 * LORES 320x200x[n planes]
 * HIRES 640x200x[n planes]
 * EHB: 6 planes
 *   0xxxxx = color index (32)
 *   1xxxxx = half color index
 * HAM: 6 planes
 *   00xxxx = color index (16)
 *   01bbbb = prev.RG_ + b
 *   10rrrr = prev._GB + r
 *   11gggg = prev.R_B + g
 * HAM: 8 planes (AGA)
 */
#endif
