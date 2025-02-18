#ifndef __mac_h__
#define __mac_h__

/* VIA Chipset:
 * orb = rtc, sound[7], adb[4,5,6]
 * ora = overlay[4], iwm[5], video[6], sound[0,1,2,3]
 *
 * acr & 20 == 0, = free running t2
 * acr & 1c : shift enabled
 *
 *   rd              wr
 * 00 0 orb          orb
 * 02 1 ora.1        ora
 * 04 2 ddrb         ddrb
 * 06 3 ddra         ddra
 * 08 4 t1ctr_l      t1latch_l
 * 0a 5 t1ctr_h      t1ctr_h
 * 0c 6 t1latch_l    t1latch_l
 * 0e 7 t1latch_h    t1latch_h
 * 10 8 t2ctr_l      t2latch_lo
 * 12 9 t2ctr_h      t2ctr_h
 * 14 a shift        shift
 * 16 b acr          acr
 * 18 c pcr          pcr
 * 1a d ifr          ifr
 * 1c e ier          ier
 * 1e f ora.0        ora
 *
 * +---+---+---+---+---+---+---+---+
 * |SET|T1 |T2 |CB1|CB2|SR |CA1|CA2| ier/ifr
 * +---+---+---+---+---+---+---+---+
 *
 */
enum {
  /* VIA IFR/IER status */
  IFR_SET   = 0x80,
  IFR_TMR1  = 0x40,    // timer 1 reset (PB7, sound reset)
  IFR_TMR2  = 0x20,    // timer 2         
  IFR_CB1   = 0x10,    // kbd.sclk        
  IFR_CB2   = 0x08,    // kbd.data        
  IFR_SR    = 0x04,    // kbd.data        
  IFR_CA1   = 0x02,    // vsync (vblank)
  IFR_CA2   = 0x01,    // 1 sec interrupt
  IFR_VSYNC = IFR_CA1, // VSYNC

  /* Port A */
  PA7       = 0x80, // i  scc channel a=b
  PA6       = 0x40, // o  video page 2
  PA5       = 0x20, // o  disk head selector
  PA4       = 0x10, // o  OVERLAY
  PA3       = 0x08, // o  sound buffer page 2
  PA2       = 0x04, // o  sound vol2
  PA1       = 0x02, // o  sound vol1
  PA0       = 0x01, // o  sound vol0
  OVL       = PA4,
  
  /* Port B */
  PB7       = 0x80, // o  sound reset
  PB6       = 0x40, // i  horiz blank            **
  PB5       = 0x20, // i  mouse x2
  PB4       = 0x10, // i  mouse y2
  PB3       = 0x08, // i  mouse sw
  PB2       = 0x04, // o  rtc enable
  PB1       = 0x02, // o  rtc clock
  PB0       = 0x01, // io rtc data [shift register]
  RTC_DATA  = PB0,
  RTC_CLOCK = PB1,
  RTC_EN    = PB2,

  /* CB1 */
  PCR7      = 0x80, // io cb2 kbd.data
  PCR6      = 0x40, // io cb2 kbd.data
  PCR5      = 0x20, // io cb2 kbd.data
  PCR4      = 0x10, // io cb1 kbd.sclk
  PCR3      = 0x08, // io ca2 1sec clkout
  PCR2      = 0x04, // io ca2 1sec clkout
  PCR1      = 0x02, // io ca2 1sec clkout
  PCR0      = 0x01, // i  ca1 vsync            **
};

/* Global variables (System communication area)
 * http://www.mac.linux-m68k.org/devel/macalmanac.php                                                                                                                */
#define CHIPREG(o)				    \
  o(0xEFE1FE, "--", VIA_IORB, "VIA I/O register B") \
  o(0xEFE3FE, "--", VIA_IORAH,"VIA I/O register A handshake") \
  o(0xEFE5FE, "--", VIA_DDRB, "VIA DDRB") \
  o(0xEFE7FE, "--", VIA_DDRA, "VIA DDRA") \
  o(0xEFE9FE, "--", VIA_TALO, "VIA Timer1 Counter Lo") \
  o(0xEFEBFE, "--", VIA_TAHI, "VIA Timer1 Counter Hi") \
  o(0xEFF1FE, "--", VIA_TBLO, "VIA Timer1 Counter Lo") \
  o(0xEFF3FE, "--", VIA_TBHI, "VIA Timer1 Counter Hi") \
  o(0xEFF5FE, "--", VIA_SHIFT,"VIA Shift") \
  o(0xEFF7FE, "--", VIA_AUX,  "VIA AUX Control") \
  o(0xEFF9FE, "--", VIA_PCI,  "VIA Peripheral Control") \
  o(0xEFFBFE, "--", VIA_IFR,  "VIA Interrupt Status Flag") \
  o(0xEFFDFE, "--", VIA_IER,  "VIA Interrupt Enable Flag") \
  o(0xEFFFFE, "--", VIA_IORA, "VIA I/O register A") \
						    \
  o(0xBFFFFF, "w-", SCC_DATAWA, "SCC Data Write Channel A") \
  o(0xBFFFFD, "w-", SCC_DATAWB, "SCC Data Write Channel B") \
  o(0xBFFFFB, "w-", SCC_CTRLWA, "SCC Write Control A") \
  o(0xBFFFF9, "w-", SCC_CTRLWB, "SCC Write Control B")			\
  o(0x9FFFFE, "r-", SCC_DATARA, "SCC Data Read Channel A") \
  o(0x9FFFFC, "r-", SCC_DATARB, "SCC Data Read Channel B") \
  o(0x9FFFFA, "r-", SCC_CTRLRA, "SCC Read Control A")			\
  o(0x9FFFF8, "r-", SCC_CTRLRB, "SCC Read Control B")			\
  o(0x9FFFFF, "w-", SCC_RESET,  "SCC Reset") \
  o(0xDFE1FF, "--", IWM_PHASE0_LO, "IWM Phase0 Low")	\
  o(0xDFE3FF, "--", IWM_PHASE0_HI, "IWM Phase0 Hi")	\
  o(0xDFE5FF, "--", IWM_PHASE1_LO, "IWM Phase1 Low")	\
  o(0xDFE7FF, "--", IWM_PHASE1_HI, "IWM Phase1 Hi")	\
  o(0xDFE9FF, "--", IWM_PHASE2_LO, "IWM Phase2 Low")	\
  o(0xDFEBFF, "--", IWM_PHASE2_HI, "IWM Phase2 Hi")	\
  o(0xDFEDFF, "--", IWM_PHASE3_LO, "IWM Phase3 Low")	\
  o(0xDFEFFF, "--", IWM_PHASE3_HI, "IWM Phase3 Hi")	\
  o(0xDFF1FF, "--", IWM_MOTOR_OFF, "IWM Motor Off")	\
  o(0xDFF3FF, "--", IWM_MOTOR_ON,  "IWM Motor On")	\
  o(0xDFF5FF, "--", IWM_SEL1, "IWM Sel 1")	\
  o(0xDFF7FF, "--", IWM_SEL2, "IWM Sel 2")	\
  o(0xDFF9FF, "--", IWM_Q6_LO, "IWM Q6 Low")	\
  o(0xDFFBFF, "--", IWM_Q6_HI, "IWM Q6 High")	\
  o(0xDFFDFF, "--", IWM_Q7_LO, "IWM Q7 Low")	\
  o(0xDFFFFF, "--", IWM_Q7_HI, "IWM Q7 High")	\
  									\
  o(0x100, "--", z_SysCom, "start of System communication area")	\
  o(0x102, "--", z_ScrVRes, "screen vertical dots/inch") \
  o(0x104, "--", z_ScrHRes, "screen horizontal dots/inch") \
  o(0x106, "--", z_ScreenRow, "rowBytes of screen") \
  o(0x108, "--", z_MemTop, "ptr to end of RAM") \
  o(0x10c, "--", z_BufPtr, "ptr to end of jump table") \
  o(0x110, "--", z_StkLowPt, "lowest stack pointer value as measured in VBL task") \
  o(0x114, "--", z_HeapEnd, "ptr to end of application heap") \
  o(0x118, "--", z_TheZone, "ptr to current heap zone") \
  o(0x11c, "--", z_UTableBase, "ptr to unit I/O table") \
  o(0x120, "--", z_MacJmp, "ptr to jump vector table used by MacsBug") \
  o(0x124, "--", z_DskRtnAdr, "temporary pointer used by Disk Driver") \
  o(0x128, "--", z_TwiggyVars, "ptr to 'other' driver variables (Lisa 5.25 drive)") \
  o(0x12c, "--", z_DskVerify, "used by Mac 3.5 Disk Driver for read/verify") \
  o(0x12d, "--", z_LoadTrap, "trap before launch?") \
  o(0x12e, "--", z_MmInOK, "Initial Memory Manager checks ok?") \
  o(0x12f, "--", z_DskWr11, "try 1-1 disk writes?") \
  o(0x130, "--", z_ApplLimit, "address of application heap limit") \
  o(0x134, "--", z_SonyVars, "ptr to Mac 3.5 Disk Driver variables") \
  o(0x138, "--", z_PWMValue, "current PWM value") \
  o(0x13a, "--", z_PollStack, "address of SCC poll data start stack location") \
  o(0x13e, "--", z_PollProc, "ptr to SCC poll data procedure") \
  o(0x142, "--", z_DskErr, "disk routine result code") \
  o(0x144, "--", z_SysEvtMask, "system event mask") \
  o(0x146, "--", z_SysEvtBuf, "ptr to system event queue element buffer") \
  o(0x14a, "--", z_EventQueue, "event queue header") \
  o(0x154, "--", z_EvtBufCnt, "maximum #of events in SysEvtBuf minus 1") \
  o(0x156, "--", z_RndSeed, "random number seed") \
  o(0x15a, "--", z_SysVersion, "System file version number (e.g. System 4.1=$0410)") \
  o(0x15c, "--", z_SEvtEnb, "0 = SysEvent always returns FALSE") \
  o(0x15d, "--", z_DSWndUpdate, "GetNextEvent not to paint behind System error dialog?") \
  o(0x15e, "--", z_FontFlag, "font manager loop flag") \
  o(0x15f, "--", z_Filler3, "1 byte of filler") \
  o(0x160, "--", z_VBLQueue, "VBL queue header") \
  o(0x16a, "--", z_Ticks, "Tick count: time since system startup (tick=1/60 sec)") \
  o(0x16e, "--", z_MBTicks, "tick count when mouse button was last pressed") \
  o(0x172, "--", z_MBState, "current mouse button state") \
  o(0x173, "--", z_Tocks, "Lisa sub-tick count") \
  o(0x174, "--", z_KeyMap, "bitmap of the keyboard") \
  o(0x17c, "--", z_KeypadMap, "bitmap for numeric keypad (uses 18 bits)") \
  o(0x184, "--", z_KeyLast, "ASCII code for last valid keycode") \
  o(0x186, "--", z_KeyTime, "tickcount when KEYLAST was received") \
  o(0x18a, "--", z_KeyRepTime, "tick count when key was last repeated") \
  o(0x18e, "--", z_KeyThresh, "threshold for key repeat") \
  o(0x190, "--", z_KeyRepThresh, "key repeat speed") \
  o(0x192, "--", z_Lv11DT, "Level-1 secondary interrupt vector table") \
  o(0x1b2, "--", z_Lv12DT, "Level-2 secondary interrupt vector table") \
  o(0x1d2, "--", z_UnitNtryCnt, "count of entries in unit table") \
  o(0x1d4, "--", z_VIA, "base address of 6522 VIA chip ") \
  o(0x1d8, "--", z_SCCRd, "addr of Z8530 SCC chip (used when reading the chip)") \
  o(0x1dc, "--", z_SCCWr, "address of Z8530 SCC chip (used when writing the chip)") \
  o(0x1e0, "--", z_IWM, "base address of IWM chip (floppy drive controller)") \
  o(0x1e4, "--", z_scratch20, "general scratch area") \
  o(0x1f8, "--", z_SysParam, "System parameter RAM vars (PRAM info)") \
  o(0x1f9, "--", z_SPATalkA, "AppleTalk node ID for modem port") \
  o(0x1fa, "--", z_SPATalkB, "AppleTalk node ID for printer port") \
  o(0x1fb, "--", z_SPConfig, "serial-port-in-use flags for both ports") \
  o(0x1fc, "--", z_SPPortA, "modem port configuration (baud, parity, bits)") \
  o(0x1fe, "--", z_SPPortB, "printer port configuration (baud, parity, bits)") \
  o(0x200, "--", z_SPAlarm, "alarm clock setting") \
  o(0x204, "--", z_SPFont, "font number of application font minus 1") \
  o(0x206, "--", z_SPKbdPrint, "auto-key threshold/rate and printer connection") \
  o(0x207, "--", z_SPPrint, "printer connection") \
  o(0x208, "--", z_SPVolClik, "speaker volume; double click and caret flash times") \
  o(0x209, "--", z_SPClikCaret, "double click and caret flash times") \
  o(0x20a, "--", z_SPMisc, "reserved for future use") \
  o(0x20b, "--", z_SPMisc2, "mouse tracking, startup floppy drive, menu blink") \
  o(0x20c, "--", z_Time, "current date/time (seconds since midnight 1 JAN 1904)") \
  o(0x210, "--", z_BootDrive, "drive number of boot drive") \
  o(0x212, "--", z_JShell, "journaling shell state") \
  o(0x214, "--", z_Filler3A, "negative of vRefNum last seen by Standard File Package") \
  o(0x216, "--", z_KbdVars, "Keyboard manager variables") \
  o(0x218, "--", z_KbdLast, "ADB address of keyboard last used") \
  o(0x21a, "--", z_JKybdTask, "ptr to keyboard VBL task hook") \
  o(0x21e, "--", z_KbdType, "keyboard model number") \
  o(0x21f, "--", z_AlarmState, "alarm clock: Bit7=parity, Bit6=beeped, Bit0=enable") \
  o(0x220, "--", z_CurIOTrap, "current I/O trap being executed") \
  o(0x222, "--", z_DiskVars, "Disk driver variables (60 bytes)") \
  o(0x25e, "--", z_FlEvtMask, "mask of flushable events (FlushEvents)") \
  o(0x260, "--", z_SdVolume, "Current speaker volume (bits 0 through 2 only)") \
  o(0x261, "--", z_SdEnable, "Sound enabled?") \
  o(0x262, "--", z_SoundVars, "Sound driver variables (32 bytes)") \
  o(0x266, "--", z_SoundBase, "ptr to free-form sound definition (SynthRec)") \
  o(0x26a, "--", z_SoundVBL, "vertical retrace control element") \
  o(0x27a, "--", z_SoundDCE, "pointer to Sound Driver's device control entry") \
  o(0x27e, "--", z_SoundActive, "sound is active?") \
  o(0x27f, "--", z_SoundLevel, "current amplitude in 740-byte sound buffer") \
  o(0x280, "--", z_CurPitch, "current value of COUNT in square-wave SynthRec") \
  o(0x282, "--", z_SoundLast, "address past last sound variable") \
  o(0x28e, "--", z_ROM85, "holds a positive value if 128K or later ROM in Mac") \
  o(0x290, "--", z_PortAUse, "Port A usage: if zero, port available") \
  o(0x291, "--", z_PortBUse, "Port B usage: if zero, port available") \
  o(0x292, "--", z_ScreenVars, "Screen driver variables (8 bytes)") \
  o(0x29a, "--", z_JGNEFilter, "ptr to GetNextEvent filter procedure") \
  o(0x29e, "--", z_Key1Trans, "ptr to keyboard translator procedure") \
  o(0x2a2, "--", z_Key2Trans, "ptr to numeric keypad translator procedure") \
  o(0x2a6, "--", z_SysZone, "starting address of system heap zone") \
  o(0x2aa, "--", z_ApplZone, "starting address of application heap zone") \
  o(0x2ae, "--", z_ROMBase, "base address of ROM (Trap Dispatcher)") \
  o(0x2b2, "--", z_RAMBase, "base address of RAM (Trap Dispatcher)") \
  o(0x2b6, "--", z_BasicGlob, "ptr to BASIC globals") \
  o(0x2ba, "--", z_DSAlertTab, "ptr to system error alert table in use") \
  o(0x2be, "--", z_ExtStsDT, "External/status interrupt vector table") \
  o(0x2ce, "--", z_SCCASts, "SCC read register 0 last external/status interrupt - A") \
  o(0x2cf, "--", z_SCCBSts, "SCC read register 0 last external/status interrupt - B") \
  o(0x2d0, "--", z_SerialVars, "async driver variables (16 bytes) ") \
  o(0x2d8, "--", z_ABusVars, "ptr to AppleTalk variables") \
  o(0x2e0, "--", z_FinderName, "name of the shell, usually Finder (STRING[15])") \
  o(0x2f0, "--", z_DoubleTime, "double click interval in ticks") \
  o(0x2f4, "--", z_CaretTime, "caret blink interval in ticks") \
  o(0x2f8, "--", z_ScrDmpEnb, "screen dump enable - zero disables FKEY processing") \
  o(0x2f9, "--", z_ScrDmpType, "$FF dumps screen, $FE dumps front window (FKEY 4)") \
  o(0x2fa, "--", z_TagData, "sector tag info for disk drivers (14 bytes)") \
  o(0x2fc, "--", z_BufTgFNum, "File tags buffer: file number") \
  o(0x300, "--", z_BufTgFFlg, "File tags buffer: flags (bit1=1 if resource fork)") \
  o(0x302, "--", z_BufTgFBkNum, "File tags buffer: logical block number") \
  o(0x304, "--", z_BufTgDate, "File tags buffer: last modification date/time") \
  o(0x308, "--", z_DrvQHdr, "queue header of drives in system") \
  o(0x312, "--", z_PWMBuf2, "ptr to PWM buffer 1 (or 2 if sound)") \
  o(0x316, "--", z_HpChk, "heap check RAM code") \
  o(0x31a, "--", z_MaskBC, "Memory Manager byte count mask") \
  o(0x31e, "--", z_MinStack, "minimum stack size used in InitApplZone") \
  o(0x322, "--", z_DefltStack, "default size of stack") \
  o(0x326, "--", z_MMDefFlags, "default zone flags") \
  o(0x328, "--", z_GZRootHnd, "root handle for GrowZone") \
  o(0x32c, "--", z_GZRootPtr, "root pointer for GrowZone") \
  o(0x330, "--", z_GZMoveHnd, "moving handle for GrowZone") \
  o(0x334, "--", z_DSDrawProc, "ptr to alternate system error draw procedure") \
  o(0x338, "--", z_EjectNotify, "ptr to eject notify procedure") \
  o(0x33c, "--", z_IAZNotify, "ptr to world swaps notify procedure") \
  o(0x340, "--", z_FileVars, "file system vars (184 bytes)") \
  o(0x342, "--", z_FSCallAsync, "One byte free") \
  o(0x344, "--", z_MaxDB, "") \
  o(0x346, "--", z_FlushOnly, "flag used by UnMountVol and FlushVol") \
  o(0x347, "--", z_RegRsrc, "flag used by OpenRF and FileOpen") \
  o(0x348, "--", z_FLckUnlck, "flag used by SetFilLock and RstFilLock") \
  o(0x349, "--", z_FrcSync, "when set, all file system calls are synchronized") \
  o(0x34a, "--", z_NewMount, "used by MountVol to flag new mounts") \
  o(0x34b, "--", z_NoEject, "used by Eject and Offline") \
  o(0x34c, "--", z_DrMstrBlk, "master directory block in a volume") \
  o(0x34e, "--", z_FCBSPtr, "ptr to file control block buffer") \
  o(0x352, "--", z_DefVCBPtr, "ptr to default volume control block") \
  o(0x356, "--", z_VCBQHdr, "volume control block queue header") \
  o(0x360, "--", z_FSQHdr, "file I/O queue header") \
  o(0x36a, "--", z_HFSVars, "Start of TFS variables (RAM version)") \
  o(0x36e, "--", z_HFSStkPtr, "Temporary location of HFS stack ptr") \
  o(0x372, "--", z_WDCBsPtr, "Working Directory queue header") \
  o(0x376, "--", z_HFSFlags, "Internal HFS flags") \
  o(0x377, "--", z_SysCRefCnt, "system cache usage count (#of vols)") \
  o(0x378, "--", z_SysBMCPtr, "System-wide bitmap cache pointer") \
  o(0x37c, "--", z_SysVolCPtr, "System-wide volume cache pointer") \
  o(0x380, "--", z_SysCtlCPtr, "System-wide control cache pointer") \
  o(0x384, "--", z_DefVRefNum, "Default volume's VRefNum/WDRefNum") \
  o(0x386, "--", z_PMSPPtr, "ptr to list of directories on PMSP") \
  o(0x392, "--", z_HFSDSErr, "Final gasp - error that caused IOErr") \
  o(0x394, "--", z_HFSVarEnd, "End of HFS variable area") \
  o(0x398, "--", z_CurDirStore, "ID of last directory opened") \
  o(0x39c, "--", z_CacheCom, "") \
  o(0x3a2, "--", z_ErCode, "report errors here during async routines") \
  o(0x3a4, "--", z_Params, "File Mgr I/O ParamBlock (50 bytes)") \
  o(0x3d6, "--", z_FSTemp8, "used by Rename") \
  o(0x3de, "--", z_FSTemp4, "used by Rename and CkFilMod") \
  o(0x3e2, "--", z_FSQueueHook, "ptr to hook to capture all FS calls") \
  o(0x3e6, "--", z_ExtFSHook, "ptr to command done hook") \
  o(0x3ea, "--", z_DskSwtchHook, "ptr to hook for disk-switch dialog") \
  o(0x3ee, "--", z_ReqstVol, "ptr to offline or external file system volume VCB") \
  o(0x3f2, "--", z_ToExtFS, "ptr to external file system") \
  o(0x3f6, "--", z_FSVarEnd, "end of file system variables") \
  o(0x3f8, "--", z_DSAlertRect, "rectangle for system error and disk-switch alerts") \
									\
  o(0x0800, "--", z_JHideCursor, "") \
  o(0x0804, "--", z_JShowCursor, "") \
  o(0x0808, "--", z_JShieldCursor, "") \
  o(0x080C, "--", z_JScrnAddr, "") \
  o(0x0810, "--", z_JScrnSize, "") \
  o(0x0814, "--", z_JInitCrsr, "") \
  o(0x0818, "--", z_JSetCrsr, "") \
  o(0x081C, "--", z_JCrsrObscure, "") \
  o(0x0820, "--", z_JUpdateProc, "") \
  o(0x0824, "--", z_LGrafJump, "") \
  o(0x0824, "--", z_GrafVar, "QuickDraw variables") \
  o(0x0824, "--", z_ScrnBase, "base address of main screen") \
  o(0x0828, "--", z_MTemp, "low-level interrupt mouse location") \
  o(0x082C, "--", z_RawMouse, "un-jerked mouse coordinates") \
  o(0x0830, "--", z_NMouse, "processed mouse coordinate") \
  o(0x0834, "--", z_CrsrPin, "cursor pinning rectangle") \
  o(0x083C, "--", z_CrsrRect, "cursor hit rectangle") \
  o(0x0844, "--", z_TheCrsr, "cursor data, mask & hotspot") \
  o(0x0888, "--", z_CrsrAddr, "address of data under cursor") \
  o(0x088C, "--", z_CrsrSave, "data under the cursor [Editor's note: 64K ROM only]") \
  o(0x08A4, "--", z_MainDevice, "handle to current main device") \
  o(0x08A8, "--", z_DeviceList, "handle to first element in device list") \
  o(0x08B0, "--", z_QDColors, "default QuickDraw colors") \
  o(0x08CC, "--", z_CrsrVis, "cursor visible?") \
  o(0x08CD, "--", z_CrsrBusy, "cursor locked out?") \
  o(0x08CE, "--", z_CrsrNew, "cursor changed?") \
  o(0x08CF, "--", z_CrsrCouple, "cursor coupled to mouse?") \
  o(0x08D0, "--", z_CrsrState, "cursor nesting level") \
  o(0x08D2, "--", z_CrsrObscure, "Cursor obscure semaphore") \
  o(0x08D3, "--", z_CrsrScale, "cursor scaled?") \
  o(0x08D6, "--", z_MouseMask, "V-H mask for ANDing with mouse") \
  o(0x08DA, "--", z_MouseOffset, "V-H offset for adding after ANDing") \
  o(0x08DE, "--", z_JournalFlag, "journaling state") \
  o(0x08E0, "--", z_JSwapFont, "jump entry for FMSwapFont") \
  o(0x08E4, "--", z_JFontInfo, "jump entry for FMFontMetrics") \
  o(0x08E4, "--", z_WidthListHand, "handle to a list of handles of recently-used width") \
  o(0x08E8, "--", z_JournalRef, "Journalling driver's refnum") \
  o(0x08EC, "--", z_CrsrThresh, "delta threshold for mouse scaling") \
  o(0x08EE, "--", z_JCrsrTask, "address of CrsrVBLTask") \
  o(0x08F2, "--", z_GRAFEND, "End of graphics globals") \
  o(0x08F2, "--", z_WWExist, "window manager initialized?") \
  o(0x08F3, "--", z_DExist, "QuickDraw is initialized") \
  o(0x08F4, "--", z_JFetch, "ptr to fetch-a-byte routine for drivers") \
  o(0x08F8, "--", z_JStash, "ptr to stash-a-byte routine for drivers") \
  o(0x08FC, "--", z_JIODone, "ptr to IODone routine for drivers") \
  o(0x0900, "--", z_LoadVars, "Segment Loader variables (68 bytes)") \
  o(0x0900, "--", z_CurApRefNum, "refNum of current application's resFile") \
  o(0x0902, "--", z_LaunchFlag, "Tells whether Launch or Chain was last called") \
  o(0x0904, "--", z_CurrentA5, "current value of register A5") \
  o(0x0908, "--", z_CurStackBase, "ptr to the base (beginning) of the stack") \
  o(0x0910, "--", z_CurApName, "name of current application (STRING[31])") \
  o(0x0930, "--", z_SaveSegHandle, "handle to segment 0 (CODE 0)") \
  o(0x0934, "--", z_CurJTOffset, "current jump table offset from register A5") \
  o(0x0936, "--", z_CurPageOption, "current page 2 configuration (screen/sound buffers)") \
  o(0x0938, "--", z_HiliteMode, "set to -1 if hilighting mode is on, 0 otherwise") \
  o(0x093A, "--", z_LoaderPBlock, "param block for ExitToShell") \
  o(0x0944, "--", z_PrintVars, "print code variables") \
  o(0x0944, "--", z_LastLGlobal, "address past last loader global") \
  o(0x0944, "--", z_PrintErr, "Print Manager error code") \
  o(0x0954, "--", z_CoreEditVars, "core edit variables") \
  o(0x0954, "--", z_LastPGlobal, "address of last printer global") \
  o(0x0960, "--", z_scrapVars, "Scrap Manager variables (32 bytes)") \
  o(0x0960, "--", z_scrapInfo, "scrap length") \
  o(0x0960, "--", z_scrapSize, "scrap length") \
  o(0x0964, "--", z_scrapHandle, "handle to RAM scrap") \
  o(0x0968, "--", z_scrapCount, "count changed by ZeroScrap") \
  o(0x096A, "--", z_scrapState, "scrap state: tells if scrap exists in RAM or on disk") \
  o(0x096C, "--", z_scrapName, "pointer to scrap file name (normally Clipboard File)") \
  o(0x0970, "--", z_scrapTag, "scrap file name (STRING[15])") \
  o(0x0980, "--", z_scrapEnd, "End of scrap vars") \
  o(0x0980, "--", z_ToolGBase, "base address of toolbox globals") \
  o(0x0980, "--", z_ToolVars, "toolbox variables") \
  o(0x0980, "--", z_RomFont0, "handle to system font") \
  o(0x0984, "--", z_ApFontID, "font number of application font") \
  o(0x0986, "--", z_GotStrike, "Do we have the strike?") \
  o(0x0987, "--", z_FMDefaultSize, "default size") \
  o(0x0988, "--", z_CurFMInput, "ptr to QuickDraw FMInput record") \
  o(0x0988, "--", z_CurFMFamily, "current font family") \
  o(0x098A, "--", z_CurFMSize, "current font size") \
  o(0x098C, "--", z_CurFMFace, "current font face") \
  o(0x098D, "--", z_CurFMNeedBits, "boolean telling whether it needs strike") \
  o(0x098E, "--", z_CurFMDevice, "current font device") \
  o(0x0990, "--", z_CurFMNumer, "current numerator of scale factor") \
  o(0x0994, "--", z_CurFMDenom, "current denominator of scale factor") \
  o(0x0998, "--", z_FMgrOutRec, "ptr to QuickDraw FontOutput record") \
  o(0x0998, "--", z_FOutError, "Font Manager error code") \
  o(0x099E, "--", z_FOutBold, "bolding factor") \
  o(0x099F, "--", z_FOutItalic, "italic factor") \
  o(0x09A0, "--", z_FOutULOffset, "underline offset") \
  o(0x09A1, "--", z_FOutULShadow, "underline halo") \
  o(0x09A2, "--", z_FOutULThick, "underline thickness") \
  o(0x09A3, "--", z_FOutShadow, "shadow factor") \
  o(0x09A4, "--", z_FOutExtra, "extra horizontal width") \
  o(0x09A5, "--", z_FOutAscent, "height above baseline") \
  o(0x09A6, "--", z_FOutDescent, "height below baseline") \
  o(0x09A7, "--", z_FOutWidMax, "maximum width of character") \
  o(0x09A8, "--", z_FOutLeading, "space between lines") \
  o(0x09A9, "--", z_FOutUnused, "unused (padding) byte -must have even number") \
  o(0x09AA, "--", z_FOutNumer, "point for numerators of scale factor") \
  o(0x09AE, "--", z_FOutDenom, "point for denominators of scale factor") \
  o(0x09B2, "--", z_FMDotsPerInch, "h,v dotsPerInch (resolution) of current device") \
  o(0x09B6, "--", z_FMStyleTab, "style heuristic table given by device") \
  o(0x09CE, "--", z_ToolScratch, "scratch area") \
  o(0x09D6, "--", z_WindowList, "ptr to Z-ordered linked list of windows") \
  o(0x09DA, "--", z_SaveUpdate, "Enable update events?") \
  o(0x09DC, "--", z_PaintWhite, "erase windows before update event?") \
  o(0x09DE, "--", z_WMgrPort, "ptr to window manager's grafport") \
  o(0x09E2, "--", z_DeskPort, "ptr to Desk grafPort (Whole screen)") \
  o(0x09E6, "--", z_OldStructure, "handle to saved structure region") \
  o(0x09EA, "--", z_OldContent, "handle to saved content region") \
  o(0x09EE, "--", z_GrayRgn, "handle to rounded-corner region drawn as the desktop") \
  o(0x09F2, "--", z_SaveVisRgn, "handle to temporarily saved visRegion") \
  o(0x09F6, "--", z_DragHook, "ptr to user hook called during dragging") \
  o(0x09FA, "--", z_scratch8, "general scratch area") \
  o(0x09FA, "--", z_TempRect, "scratch rectangle") \
  o(0x0A02, "--", z_OneOne, "holds the constant $00010001") \
  o(0x0A06, "--", z_MinusOne, "holds the constant $FFFFFFFF") \
  o(0x0A0A, "--", z_TopMenuItem, "pixel value of top of scrollable menu") \
  o(0x0A0C, "--", z_AtMenuBottom, "flag for menu scrolling") \
  o(0x0A0E, "--", z_IconBitmap, "scratch bitmap used for plotting things") \
  o(0x0A1C, "--", z_MenuList, "handle to current menuBar list structure") \
  o(0x0A20, "--", z_MBarEnable, "menuBar enable for desk acc's that own the menu bar") \
  o(0x0A22, "--", z_CurDeKind, "window kind of deactivated window") \
  o(0x0A24, "--", z_MenuFlash, "flash feedback count") \
  o(0x0A26, "--", z_TheMenu, "resource ID of hilited menu") \
  o(0x0A28, "--", z_SavedHandle, "handle to data under a menu") \
  o(0x0A2C, "--", z_MrMacHook, "Mr. Macintosh hook") \
  o(0x0A2C, "--", z_MBarHook, "ptr to MenuSelect hook called before menu is drawn") \
  o(0x0A30, "--", z_MenuHook, "ptr to user hook called during MenuSelect") \
  o(0x0A34, "--", z_DragPattern, "pattern used to draw outlines of dragged regions") \
  o(0x0A3C, "--", z_DeskPattern, "pattern used for the desktop") \
  o(0x0A44, "--", z_DragFlag, "implicit parameter to DragControl") \
  o(0x0A46, "--", z_CurDragAction, "ptr to implicit actionProc for dragControl") \
  o(0x0A4A, "--", z_FPState, "floating point state") \
  o(0x0A50, "--", z_TopMapHndl, "handle to map of most recently opened resource file") \
  o(0x0A54, "--", z_SysMapHndl, "handle to map of System resourc file") \
  o(0x0A58, "--", z_SysMap, "reference number of System resource file") \
  o(0x0A5A, "--", z_CurMap, "reference number of current resource file") \
  o(0x0A5C, "--", z_ResReadOnly, "Read-only flag") \
  o(0x0A5E, "--", z_ResLoad, "Auto-load feature") \
  o(0x0A60, "--", z_ResErr, "Resource Manager error code") \
  o(0x0A62, "--", z_TaskLock, "re-entering SystemTask") \
  o(0x0A63, "--", z_FScaleDisable, "disable font scaling?") \
  o(0x0A64, "--", z_CurActivate, "ptr to window slated for activate event") \
  o(0x0A68, "--", z_CurDeactive, "ptr to window slated for deactivate event") \
  o(0x0A6C, "--", z_DeskHook, "ptr to hook for painting the desk") \
  o(0x0A70, "--", z_TEDoText, "ptr to textEdit doText proc hook") \
  o(0x0A74, "--", z_TERecal, "ptr to textEdit recalText proc hook") \
  o(0x0A78, "--", z_MicroSoft, "ApplScratch - for Seattle font") \
  o(0x0A78, "--", z_ApplScratch, "application scratch area") \
  o(0x0A84, "--", z_GhostWindow, "ptr to window never to be considered frontmost") \
  o(0x0A88, "--", z_CloseOrnHook, "ptr to hook for closing desk ornaments") \
  o(0x0A8C, "--", z_ResumeProc, "ptr to Resume procedure (System error dialog)") \
  o(0x0A90, "--", z_SaveProc, "address of Save failsafe procedure") \
  o(0x0A94, "--", z_SaveSP, "Safe stack ptr for restart or save") \
  o(0x0A98, "--", z_ANumber, "resID of last alert") \
  o(0x0A9A, "--", z_ACount, "number of times last alert was called (0 through 3)") \
  o(0x0A9C, "--", z_DABeeper, "ptr to current beep routine") \
  o(0x0AA0, "--", z_DAStrings, "paramText substitution strings (4 handles)") \
  o(0x0AB0, "--", z_TEScrpLengt, "textEdit Scrap Length") \
  o(0x0AB4, "--", z_TEScrpHandl, "handle to textEdit Scrap") \
  o(0x0AB8, "--", z_AppPacks, "Handles to PACK resources (ID's from 0 to 7)") \
  o(0x0AD8, "--", z_SysResName, "name of system resource file (STRING[19])") \
  o(0x0AEC, "--", z_AppParmHandle, "handle to hold application parameters") \
  o(0x0AF0, "--", z_DSErrCode, "last (or current) system error alert ID") \
  o(0x0AF2, "--", z_ResErrProc, "ptr to Resource Manager error procedure") \
  o(0x0AF6, "--", z_TEWdBreak, "ptr to default word break routine") \
  o(0x0AFA, "--", z_DlgFont, "current font number for dialogs and alerts") \
  o(0x0AFC, "--", z_LastTGLobal, "address of last global") \
  

enum {
#define o(addr, xx, name, str) name = addr,
  CHIPREG(o)
#undef o
};

/* a71e: NewPtrSysClear(d0:size) -> a0:ptr
 * a002: Read(a0:IOParamBlock
 *
 *
 *
 *
 */

/* ADB Commands:
 *   +---+---+---+---+---+---+---+---+
 *   |  address      |  cmd  |  reg  |
 *   +---+---+---+---+---+---+---+---+
 *                     0   0   0   0   SendReset
 *                     0   0   0   1   Flush
 *                     0   0   1   0   reserved
 *                     0   0   1   1   reserved
 *                     0   1   x   x   reserved
 *                     1   0   r0  r1  listen
 *                     1   1   r0  r1  talk
 *
 */
struct adbdev_t {
  uint16_t reg[4];
};

void adb_reset(adbdev_t *a) {
  a->reg[0] = 0;
  a->reg[1] = 0;
  a->reg[2] = 0;
  a->reg[3] = 0;
}

#endif
