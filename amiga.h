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
  o(0xbfd001, "RW", CIAB_PRA,  "CIA.B pra")				\
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
  o(0xdff000, "ER", BLTDDAT, "Blitter destination early read (unusable)") \
  o(0xdff002, "R-", DMACONR, "DMA control (and blitter status) read")	\
  o(0xdff004, "R-", VPOSR,   "Read vertical raster position bit 9 (and interlace odd/even frame)") \
  o(0xdff006, "R-", VHPOSR,  "Rest of raster XY position - High byte: vertical, low byte: horizontal") \
  o(0xdff008, "ER", DSKDATR, "Disk data early read (unusable)")		\
  o(0xdff00a, "R-", JOY0DAT, "Joystick/mouse 0 data")			\
  o(0xdff00c, "R-", JOY1DAT, "Joystick/mouse 1 data")			\
  o(0xdff00e, "R-", CLXDAT,  "Poll (read and clear) sprite collision state") \
  o(0xdff010, "R-", ADKCONR, "Audio, disk control register read")	\
  o(0xdff012, "R-", POT0DAT, "Pot counter pair 0 data")			\
  o(0xdff014, "R-", POT1DAT, "Pot counter pair 1 data")			\
  o(0xdff016, "R-", POTGOR,  "Pot pin data read")			\
  o(0xdff018, "R-", SERDATR, "Serial port data and status read")	\
  o(0xdff01a, "R-", DSKBYTR, "Disk data byte and status read")		\
  o(0xdff01c, "R-", INTENAR, "Interrupt enable bits read (intena)")	\
  o(0xdff01e, "R-", INTREQR, "Interrupt request bits read (intreq)")	\
  o(0xdff020, "W-", DSKPTH,  "Disk track buffer pointer (high 5 bits)") \
  o(0xdff022, "W-", DSKPTL,  "Disk track buffer pointer (low 15 bits)") \
  o(0xdff024, "W-", DSKLEN,  "Disk track buffer length")		\
  o(0xdff026, "W-", DSKDAT,  "Disk DMA data write")			\
  o(0xdff028, "W-", REFPTR,  "AGA: Refresh pointer")			\
  o(0xdff02a, "W-", VPOSW,   "Write vert most sig. bits (and frame flop)") \
  o(0xdff02c, "W-", VHPOSW,  "Write vert and horiz pos of beam")	\
  o(0xdff02e, "W-", COPCON,  "Coprocessor control register (CDANG)")	\
  o(0xdff030, "W-", SERDAT,  "Serial port data and stop bits write")	\
  o(0xdff032, "W-", SERPER,  "Serial port period and control")		\
  o(0xdff034, "W-", POTGO,   "Pot count start, pot pin drive enable data") \
  o(0xdff036, "W-", JOYTEST, "Write to all 4 joystick/mouse counters at once") \
  o(0xdff038, "S-", STREQU,  "Strobe for horiz sync with VBLANK and EQU") \
  o(0xdff03a, "S-", STRVBL,  "Strobe for horiz sync with VBLANK")	\
  o(0xdff03c, "S-", STRHOR,  "Strobe for horiz sync")			\
  o(0xdff03e, "S-", STRLONG, "Strobe for identification of long/short horiz line") \
  o(0xdff040, "W-", BLTCON0, "Blitter control reg 0")			\
  o(0xdff042, "W-", BLTCON1, "Blitter control reg 1")			\
  o(0xdff044, "W-", BLTAFWM, "Blitter first word mask for source A")	\
  o(0xdff046, "W-", BLTALWM, "Blitter last word mask for source A")	\
  o(0xdff048, "W-", BLTCPTH, "Blitter pointer to source C (high 5 bits)") \
  o(0xdff04a, "W-", BLTCPTL, "Blitter pointer to source C (low 15 bits)") \
  o(0xdff04c, "W-", BLTBPTH, "Blitter pointer to source B (high 5 bits)") \
  o(0xdff04e, "W-", BLTBPTL, "Blitter pointer to source B (low 15 bits)") \
  o(0xdff050, "W-", BLTAPTH, "Blitter pointer to source A (high 5 bits)") \
  o(0xdff052, "W-", BLTAPTL, "Blitter pointer to source A (low 15 bits)") \
  o(0xdff054, "W-", BLTDPTH, "Blitter pointer to destination D (high 5 bits)") \
  o(0xdff056, "W-", BLTDPTL, "Blitter pointer to destination D (low 15 bits)") \
  o(0xdff058, "W-", BLTSIZE, "Blitter start and size (win/width, height)") \
  o(0xdff05a, "W-", BLTCON0L, "Blitter control 0 lower 8 bits (minterms)") \
  o(0xdff05c, "W-", BLTSIZV, "Blitter V size (for 15 bit vert size)")	\
  o(0xdff05e, "W-", BLTSIZH, "ECS: Blitter H size & start (for 11 bit H size)") \
  o(0xdff060, "W-", BLTCMOD, "Blitter modulo for source C")		\
  o(0xdff062, "W-", BLTBMOD, "Blitter modulo for source B")		\
  o(0xdff064, "W-", BLTAMOD, "Blitter modulo for source A")		\
  o(0xdff066, "W-", BLTDMOD, "Blitter modulo for destination D")	\
  o(0xdff070, "W-", BLTCDAT, "Blitter source C data reg")		\
  o(0xdff072, "W-", BLTBDAT, "Blitter source B data reg")		\
  o(0xdff074, "W-", BLTADAT, "Blitter source A data reg")		\
  o(0xdff078, "W-", SPRHDAT, "AGA: Ext logic UHRES sprite pointer and data identifier") \
  o(0xdff07a, "W-", BPLHDAT, "AGA: Ext logic UHRES bit plane identifier") \
  o(0xdff07c, "W-", LISAID, "AGA: Chip revision level for Denise/Lisa") \
  o(0xdff07e, "W-", DSKSYNC, "Disk sync pattern")			\
  o(0xdff080, "W-", COP1LCH, "Write Copper pointer 1 (high 5 bits)")	\
  o(0xdff082, "W-", COP1LCL, "Write Copper pointer 1 (low 15 bits)")	\
  o(0xdff084, "W-", COP2LCH, "Write Copper pointer 2 (high 5 bits)")	\
  o(0xdff086, "W-", COP2LCL, "Write Copper pointer 2 (low 15 bits)")	\
  o(0xdff088, "S-", COPJMP1, "Trigger Copper 1 (any value)")		\
  o(0xdff08a, "S-", COPJMP2, "Trigger Copper 2 (any value)")		\
  o(0xdff08c, "W-", COPINS, "Coprocessor inst fetch identify")		\
  o(0xdff08e, "W-", DIWSTRT, "Display window start (upper left vert-hor pos)") \
  o(0xdff090, "W-", DIWSTOP, "Display window stop (lower right vert-hor pos)") \
  o(0xdff092, "W-", DDFSTRT, "Display bitplane data fetch start.hor pos") \
  o(0xdff094, "W-", DDFSTOP, "Display bitplane data fetch stop.hor pos") \
  o(0xdff096, "W-", DMACON, "DMA control write (clear or set)")		\
  o(0xdff098, "W-", CLXCON, "Write Sprite collision control bits")	\
  o(0xdff09a, "W-", INTENA, "Interrupt enable bits (clear or set bits)") \
  o(0xdff09c, "W-", INTREQ, "Interrupt request bits (clear or set bits)") \
  o(0xdff09e, "W-", ADKCON, "Audio, disk and UART control")		\
  o(0xdff0a0, "W-", AUD0LCH, "Audio channel 0 pointer (high 5 bits)")	\
  o(0xdff0a2, "W-", AUD0LCL, "Audio channel 0 pointer (low 15 bits)")	\
  o(0xdff0a4, "W-", AUD0LEN, "Audio channel 0 length")			\
  o(0xdff0a6, "W-", AUD0PER, "Audio channel 0 period")			\
  o(0xdff0a8, "W-", AUD0VOL, "Audio channel 0 volume")			\
  o(0xdff0aa, "W-", AUD0DAT, "Audio channel 0 data")			\
  o(0xdff0b0, "W-", AUD1LCH, "Audio channel 1 pointer (high 5 bits)")	\
  o(0xdff0b2, "W-", AUD1LCL, "Audio channel 1 pointer (low 15 bits)")	\
  o(0xdff0b4, "W-", AUD1LEN, "Audio channel 1 length")			\
  o(0xdff0b6, "W-", AUD1PER, "Audio channel 1 period")			\
  o(0xdff0b8, "W-", AUD1VOL, "Audio channel 1 volume")			\
  o(0xdff0ba, "W-", AUD1DAT, "Audio channel 1 data")			\
  o(0xdff0c0, "W-", AUD2LCH, "Audio channel 2 pointer (high 5 bits)")	\
  o(0xdff0c2, "W-", AUD2LCL, "Audio channel 2 pointer (low 15 bits)")	\
  o(0xdff0c4, "W-", AUD2LEN, "Audio channel 2 length")			\
  o(0xdff0c6, "W-", AUD2PER, "Audio channel 2 period")			\
  o(0xdff0c8, "W-", AUD2VOL, "Audio channel 2 volume")			\
  o(0xdff0ca, "W-", AUD2DAT, "Audio channel 2 data")			\
  o(0xdff0d0, "W-", AUD3LCH, "Audio channel 3 pointer (high 5 bits)")	\
  o(0xdff0d2, "W-", AUD3LCL, "Audio channel 3 pointer (low 15 bits)")	\
  o(0xdff0d4, "W-", AUD3LEN, "Audio channel 3 length")			\
  o(0xdff0d6, "W-", AUD3PER, "Audio channel 3 period")			\
  o(0xdff0d8, "W-", AUD3VOL, "Audio channel 3 volume")			\
  o(0xdff0da, "W-", AUD3DAT, "Audio channel 3 data")			\
  o(0xdff0e0, "W-", BPL1PTH, "Bitplane pointer 1 (high 5 bits)")	\
  o(0xdff0e2, "W-", BPL1PTL, "Bitplane pointer 1 (low 15 bits)")	\
  o(0xdff0e4, "W-", BPL2PTH, "Bitplane pointer 2 (high 5 bits)")	\
  o(0xdff0e6, "W-", BPL2PTL, "Bitplane pointer 2 (low 15 bits)")	\
  o(0xdff0e8, "W-", BPL3PTH, "Bitplane pointer 3 (high 5 bits)")	\
  o(0xdff0ea, "W-", BPL3PTL, "Bitplane pointer 3 (low 15 bits)")	\
  o(0xdff0ec, "W-", BPL4PTH, "Bitplane pointer 4 (high 5 bits)")	\
  o(0xdff0ee, "W-", BPL4PTL, "Bitplane pointer 4 (low 15 bits)")	\
  o(0xdff0f0, "W-", BPL5PTH, "Bitplane pointer 5 (high 5 bits)")	\
  o(0xdff0f2, "W-", BPL5PTL, "Bitplane pointer 5 (low 15 bits)")	\
  o(0xdff0f4, "W-", BPL6PTH, "Bitplane pointer 6 (high 5 bits)")	\
  o(0xdff0f6, "W-", BPL6PTL, "Bitplane pointer 6 (low 15 bits)")	\
  o(0xdff0f8, "W-", BPL7PTH, "AGA: Bitplane pointer 7 (high 5 bits)")	\
  o(0xdff0fa, "W-", BPL7PTL, "AGA: Bitplane pointer 7 (low 15 bits)")	\
  o(0xdff0fc, "W-", BPL8PTH, "AGA: Bitplane pointer 8 (high 5 bits)")	\
  o(0xdff0fe, "W-", BPL8PTL, "AGA: Bitplane pointer 8 (low 15 bits)")	\
  o(0xdff100, "W-", BPLCON0, "Bitplane depth and screen mode)")		\
  o(0xdff102, "W-", BPLCON1, "Bitplane/playfield horizontal scroll values") \
  o(0xdff104, "W-", BPLCON2, "Sprites vs. Playfields priority")		\
  o(0xdff106, "W-", BPLCON3, "AGA: Bitplane control reg (enhanced features)") \
  o(0xdff108, "W-", BPL1MOD, "Bitplane modulo (odd planes)")		\
  o(0xdff10a, "W-", BPL2MOD, "Bitplane modulo (even planes)")		\
  o(0xdff10c, "W-", BPLCON4, "AGA: Bitplane control reg (bitplane & sprite masks)") \
  o(0xdff10e, "W-", CLXCON2, "AGA: Write Extended sprite collision control bits") \
  o(0xdff110, "W-", BPL1DAT, "Bitplane 1 data (parallel to serial convert)") \
  o(0xdff112, "W-", BPL2DAT, "Bitplane 2 data (parallel to serial convert)") \
  o(0xdff114, "W-", BPL3DAT, "Bitplane 3 data (parallel to serial convert)") \
  o(0xdff116, "W-", BPL4DAT, "Bitplane 4 data (parallel to serial convert)") \
  o(0xdff118, "W-", BPL5DAT, "Bitplane 5 data (parallel to serial convert)") \
  o(0xdff11a, "W-", BPL6DAT, "Bitplane 6 data (parallel to serial convert)") \
  o(0xdff11c, "W-", BPL7DAT, "AGA: Bitplane 7 data (parallel to serial convert)") \
  o(0xdff11e, "W-", BPL8DAT, "AGA: Bitplane 8 data (parallel to serial convert)") \
  o(0xdff120, "W-", SPR0PTH, "Sprite 0 pointer (high 5 bits)")		\
  o(0xdff122, "W-", SPR0PTL, "Sprite 0 pointer (low 15 bits)")		\
  o(0xdff124, "W-", SPR1PTH, "Sprite 1 pointer (high 5 bits)")		\
  o(0xdff126, "W-", SPR1PTL, "Sprite 1 pointer (low 15 bits)")		\
  o(0xdff128, "W-", SPR2PTH, "Sprite 2 pointer (high 5 bits)")		\
  o(0xdff12a, "W-", SPR2PTL, "Sprite 2 pointer (low 15 bits)")		\
  o(0xdff12c, "W-", SPR3PTH, "Sprite 3 pointer (high 5 bits)")		\
  o(0xdff12e, "W-", SPR3PTL, "Sprite 3 pointer (low 15 bits)")		\
  o(0xdff130, "W-", SPR4PTH, "Sprite 4 pointer (high 5 bits)")		\
  o(0xdff132, "W-", SPR4PTL, "Sprite 4 pointer (low 15 bits)")		\
  o(0xdff134, "W-", SPR5PTH, "Sprite 5 pointer (high 5 bits)")		\
  o(0xdff136, "W-", SPR5PTL, "Sprite 5 pointer (low 15 bits)")		\
  o(0xdff138, "W-", SPR6PTH, "Sprite 6 pointer (high 5 bits)")		\
  o(0xdff13a, "W-", SPR6PTL, "Sprite 6 pointer (low 15 bits)")		\
  o(0xdff13c, "W-", SPR7PTH, "Sprite 7 pointer (high 5 bits)")		\
  o(0xdff13e, "W-", SPR7PTL, "Sprite 7 pointer (low 15 bits)")		\
  o(0xdff140, "W-", SPR0POS, "Sprite 0 vert-horiz start pos data")	\
  o(0xdff142, "W-", SPR0CTL, "Sprite 0 position and control data")	\
  o(0xdff144, "W-", SPR0DATA, "Sprite 0 low bitplane data")		\
  o(0xdff146, "W-", SPR0DATB, "Sprite 0 high bitplane data")		\
  o(0xdff148, "W-", SPR1POS, "Sprite 1 vert-horiz start pos data")	\
  o(0xdff14a, "W-", SPR1CTL, "Sprite 1 position and control data")	\
  o(0xdff14c, "W-", SPR1DATA, "Sprite 1 low bitplane data")		\
  o(0xdff14e, "W-", SPR1DATB, "Sprite 1 high bitplane data")		\
  o(0xdff150, "W-", SPR2POS, "Sprite 2 vert-horiz start pos data")	\
  o(0xdff152, "W-", SPR2CTL, "Sprite 2 position and control data")	\
  o(0xdff154, "W-", SPR2DATA, "Sprite 2 low bitplane data")		\
  o(0xdff156, "W-", SPR2DATB, "Sprite 2 high bitplane data")		\
  o(0xdff158, "W-", SPR3POS, "Sprite 3 vert-horiz start pos data")	\
  o(0xdff15a, "W-", SPR3CTL, "Sprite 3 position and control data")	\
  o(0xdff15c, "W-", SPR3DATA, "Sprite 3 low bitplane data")		\
  o(0xdff15e, "W-", SPR3DATB, "Sprite 3 high bitplane data")		\
  o(0xdff160, "W-", SPR4POS, "Sprite 4 vert-horiz start pos data")	\
  o(0xdff162, "W-", SPR4CTL, "Sprite 4 position and control data")	\
  o(0xdff164, "W-", SPR4DATA, "Sprite 4 low bitplane data")		\
  o(0xdff166, "W-", SPR4DATB, "Sprite 4 high bitplane data")		\
  o(0xdff168, "W-", SPR5POS, "Sprite 5 vert-horiz start pos data")	\
  o(0xdff16a, "W-", SPR5CTL, "Sprite 5 position and control data")	\
  o(0xdff16c, "W-", SPR5DATA, "Sprite 5 low bitplane data")		\
  o(0xdff16e, "W-", SPR5DATB, "Sprite 5 high bitplane data")		\
  o(0xdff170, "W-", SPR6POS, "Sprite 6 vert-horiz start pos data")	\
  o(0xdff172, "W-", SPR6CTL, "Sprite 6 position and control data")	\
  o(0xdff174, "W-", SPR6DATA, "Sprite 6 low bitplane data")		\
  o(0xdff176, "W-", SPR6DATB, "Sprite 6 high bitplane data")		\
  o(0xdff178, "W-", SPR7POS, "Sprite 7 vert-horiz start pos data")	\
  o(0xdff17a, "W-", SPR7CTL, "Sprite 7 position and control data")	\
  o(0xdff17c, "W-", SPR7DATA, "Sprite 7 low bitplane data")		\
  o(0xdff17e, "W-", SPR7DATB, "Sprite 7 high bitplane data")		\
  o(0xdff180, "W-", COLOR00, "Palette color 00")			\
  o(0xdff182, "W-", COLOR01, "Palette color 1")				\
  o(0xdff184, "W-", COLOR02, "Palette color 2")				\
  o(0xdff186, "W-", COLOR03, "Palette color 3")				\
  o(0xdff188, "W-", COLOR04, "Palette color 4")				\
  o(0xdff18a, "W-", COLOR05, "Palette color 5")				\
  o(0xdff18c, "W-", COLOR06, "Palette color 6")				\
  o(0xdff18e, "W-", COLOR07, "Palette color 7")				\
  o(0xdff190, "W-", COLOR08, "Palette color 8")				\
  o(0xdff192, "W-", COLOR09, "Palette color 9")				\
  o(0xdff194, "W-", COLOR10, "Palette color 10")			\
  o(0xdff196, "W-", COLOR11, "Palette color 11")			\
  o(0xdff198, "W-", COLOR12, "Palette color 12")			\
  o(0xdff19a, "W-", COLOR13, "Palette color 13")			\
  o(0xdff19c, "W-", COLOR14, "Palette color 14")			\
  o(0xdff19e, "W-", COLOR15, "Palette color 15")			\
  o(0xdff1a0, "W-", COLOR16, "Palette color 16")			\
  o(0xdff1a2, "W-", COLOR17, "Palette color 17")			\
  o(0xdff1a4, "W-", COLOR18, "Palette color 18")			\
  o(0xdff1a6, "W-", COLOR19, "Palette color 19")			\
  o(0xdff1a8, "W-", COLOR20, "Palette color 20")			\
  o(0xdff1aa, "W-", COLOR21, "Palette color 21")			\
  o(0xdff1ac, "W-", COLOR22, "Palette color 22")			\
  o(0xdff1ae, "W-", COLOR23, "Palette color 23")			\
  o(0xdff1b0, "W-", COLOR24, "Palette color 24")			\
  o(0xdff1b2, "W-", COLOR25, "Palette color 25")			\
  o(0xdff1b4, "W-", COLOR26, "Palette color 26")			\
  o(0xdff1b6, "W-", COLOR27, "Palette color 27")			\
  o(0xdff1b8, "W-", COLOR28, "Palette color 28")			\
  o(0xdff1ba, "W-", COLOR29, "Palette color 29")			\
  o(0xdff1bc, "W-", COLOR30, "Palette color 30")			\
  o(0xdff1be, "W-", COLOR31, "Palette color 31")			\
  o(0xdff1c0, "W-", HTOTAL, "AGA: Highest number count in horiz line (VARBEAMEN = 1)") \
  o(0xdff1c2, "W-", HSSTOP, "AGA: Horiz line pos for HSYNC stop")	\
  o(0xdff1c4, "W-", HBSTRT, "AGA: Horiz line pos for HBLANK start")	\
  o(0xdff1c6, "W-", HBSTOP, "AGA: Horiz line pos for HBLANK stop")	\
  o(0xdff1c8, "W-", VTOTAL, "AGA: Highest numbered vertical line (VARBEAMEN = 1)") \
  o(0xdff1ca, "W-", VSSTOP, "AGA: Vert line for Vsync stop")		\
  o(0xdff1cc, "W-", VBSTRT, "AGA: Vert line for VBLANK start")		\
  o(0xdff1ce, "W-", VBSTOP, "AGA: Vert line for VBLANK stop")		\
  o(0xdff1d0, "W-", SPRHSTRT, "AGA: UHRES sprite vertical start")	\
  o(0xdff1d2, "W-", SPRHSTOP, "AGA: UHRES sprite vertical stop")	\
  o(0xdff1d4, "W-", BPLHSTRT, "AGA: UHRES bit plane vertical start")	\
  o(0xdff1d6, "W-", BPLHSTOP, "AGA: UHRES bit plane vertical stop")	\
  o(0xdff1d8, "W-", HHPOSW, "AGA: DUAL mode hires H beam counter write") \
  o(0xdff1da, "W-", HHPOSR, "AGA: DUAL mode hires H beam counter read") \
  o(0xdff1dc, "W-", BEAMCON0, "Beam counter control register")		\
  o(0xdff1de, "W-", HSSTRT, "AGA: Horizontal sync start (VARHSY)")	\
  o(0xdff1e0, "W-", VSSTRT, "AGA: Vertical sync start (VARVSY)")	\
  o(0xdff1e2, "W-", HCENTER, "AGA: Horizontal pos for vsync on interlace") \
  o(0xdff1e4, "W-", DIWHIGH, "AGA: Display window upper bits for start/stop") \
  o(0xdff1e6, "W-", BPLHMOD, "AGA: UHRES bit plane modulo")		\
  o(0xdff1e8, "W-", SPRHPTH, "AGA: UHRES sprite pointer (high 5 bits)") \
  o(0xdff1ea, "W-", SPRHPTL, "AGA: UHRES sprite pointer (low 15 bits)") \
  o(0xdff1ec, "W-", BPLHPTH, "AGA: VRam (UHRES) bitplane pointer (high 5 bits)") \
  o(0xdff1ee, "W-", BPLHPTL, "AGA: VRam (UHRES) bitplane pointer (low 15 bits)") \
  o(0xdff1fc, "W-", FMODE, "AGA: Write Fetch mode (0=OCS compatible)")	\
  o(0xdff1fe, "W-", CUSTOM_NOOP, "No operation/NULL (Copper NOP instruction)") 

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

#endif
