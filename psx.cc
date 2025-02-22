#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <inttypes.h>
#include <fcntl.h>
#include <unistd.h>
#include <assert.h>
#include "dstk.h"
#define HAVE_CPU16
#define HAVE_CPU32
#include "cpu.h"
#include "util.h"
#include "bus.h"
#include "gr.h"

#include "gte.h"

extern int trace, SPC;
int psxlogfd;
int frame;

Screen *screen;

#include "mips.cc"

void _drawgpu();

//https://stackoverflow.com/questions/69447778/fastest-way-to-draw-filled-quad-triangle-with-the-sdl2-renderer
#ifndef OPENGL
#define glColor3ub(a,b,c)
#define glVertex2f(a,b)
#define glBegin(x)
#define glEnd()
#define glFlush()
#define glMatrixMode(x)
#define glLoadIdentity()
#endif

/* I/O Register Space
 * 1F801000 ... 1F803FFF
 *     1000 ...     1020 : Memory Control
 *     1040 ...     105E : Peripherals
 *     1060              : Memory Control
 *     1070 ...     1074 : Interrupt Control
 *     1080 ...     10FC : DMA Control
 *     1100 ...     112F : Timer
 *     1800 ...     1803 : CDROM
 *     1810 ...     1814 : GPU
 *     1820 ...     1024 : MDEC
 *     1C00 ...     1DFF : Sound
 *     2041              : Trace Port
 */
uint8_t ioreg[0x4000];
static uint8_t *ram, *scr, *rom;

enum {
  DMABASE = 0x1080,
  DPCR    = 0x10F0,
  DICR    = 0x10F4,

  SPUBASE = 0x1C00,
};

struct spuvoice_t {
  uint16_t volLeft;
  uint16_t volRight;
  uint16_t sampRate;  // 0x1000 = 44100KHz
  uint16_t sampStart;
  uint32_t adsr;
  uint16_t adsrVol;
  uint16_t repeatAddr;
};

spuvoice_t *spu_voices = (spuvoice_t *)&ioreg[0x1C00];

/*=============================================*
 * DMA Engine
 *=============================================*/
struct dmachan_t {
  uint32_t madr;
  uint32_t bcr;
  uint32_t chcr;
  uint32_t rsvd;
};

struct dmaxfer_t {
  uint32_t enabled;
  uint32_t addr[2];
  uint32_t incr[2];
  uint32_t pos;
  uint32_t len;
};

uint32_t& dicr  = *(uint32_t *)&ioreg[DICR];
uint32_t& dpcr  = *(uint32_t *)&ioreg[DPCR];
dmachan_t *dmas = (dmachan_t *)&ioreg[DMABASE];

int run_dma(int chan) {
  uint32_t src, incr, mode, dir;

  /* Check if we are active */
  if (((dpcr >> (4 * chan)) & 0x8) == 0)
    return 0;
  if ((dmas[chan].chcr & D24) == 0)
    return 0;
  
  src  = dmas[chan].madr;
  dir  = dmas[chan].chcr & D0;
  incr = (dmas[chan].chcr & D1) ? -4 : 4;
  mode = (dmas[chan].chcr >> 9) & 3;

  printf("Launch DMA: %d %d %6s %d 0x%.8x\n", chan, mode, dir ? "<<mem" : ">>mem", incr, src);
  dmas[chan].chcr &= ~(0x01000000);
  if (dicr & (1L << (chan + 16))) {
    dicr |= (1L << (chan + 24));
  }
  return 0;
};

void dumpspu()
{
  int i;

  for (i = 0; i < 24; i++) {
    printf("  Voice%2d: %.4x %.4x %.4x %.4x %.8x %.4x %.4x\n", i,
	   spu_voices[i].volLeft, spu_voices[i].volRight,
	   spu_voices[i].sampRate,spu_voices[i].sampStart,
	   spu_voices[i].adsr, spu_voices[i].adsrVol,
	   spu_voices[i].repeatAddr);
  }
}

void dumpdma()
{
  int i;

  printf("DICR:%.8x %c[%c%c%c%c%c%c%c] ",
	 dicr,
	 dicr & D23 ? 'y' : 'n',
	 dicr & D16 ? 'y' : '-',
	 dicr & D17 ? 'y' : '-',
	 dicr & D18 ? 'y' : '-',
	 dicr & D19 ? 'y' : '-',
	 dicr & D20 ? 'y' : '-',
	 dicr & D21 ? 'y' : '-',
	 dicr & D22 ? 'y' : '-');
  printf(" DPCR:%.8x %d%c %d%c %d%c %d%c %d%c %d%c %d%c\n",
	 dpcr,
	 (dpcr >> 0) & 3,  (dpcr >> 3) & 1 ? '+' : '-',
	 (dpcr >> 4) & 3,  (dpcr >> 7) & 1 ? '+' : '-',
	 (dpcr >> 8) & 3,  (dpcr >> 11) & 1 ? '+' : '-',
	 (dpcr >> 12) & 3, (dpcr >> 15) & 1 ? '+' : '-',
	 (dpcr >> 16) & 3, (dpcr >> 19) & 1 ? '+' : '-',
	 (dpcr >> 20) & 3, (dpcr >> 23) & 1 ? '+' : '-',
	 (dpcr >> 24) & 3, (dpcr >> 27) & 1 ? '+' : '-');
  for (i = 0; i < 7; i++) {
    printf("  DMA%2d: %.8x %.8x %.8x %.8x %s,%s,%d,start.busy=%d start.trigger=%d\n", i,
	   dmas[i].madr, dmas[i].bcr, dmas[i].chcr,
	   dmas[i].rsvd,
	   dmas[i].chcr & D0 ? "fm" : "to",
	   dmas[i].chcr & D1 ? "fw" : "bk",
	   (dmas[i].chcr >> 9) & 3,
	   !!(dmas[i].chcr & D24),
	   !!(dmas[i].chcr & D28));
  }
}

/* DMA Channels
 *  0: MDECIN : RAM -> MDEC
 *  1: MDECOUT: MDEC -> RAM
 *  2: GPU
 *  3: CDROM
 *  4: SPU
 *  5: PIO
 *  6: OTC
 */

  /* Mirrors 0x00, 0x80, 0xA0
 * 00.000000 2048k Main RAM
 * 1F.000000 8192k Expansion Region 1 [ROM/RAM]
 * 1F.800000 1K    Scratchpad RAM
 * 1F.801000 8k    I/O Ports
 * 1F.802000 8k    Expansion Region 2 [I/O Ports]
 * 1F.A00000 2048k Expansion Region 3
 * 1F.C00000 512k  ROM
 */
enum {
  RAM_START  = 0x00000000,
  RAM_END    = 0x007FFFFF,
  RAM_SIZE   = 0x00200000,

  SCR_START  = 0x00800000,
  SCR_END    = 0x008003FF,
  SCR_SIZE   = 0x00000400,

  IO_START   = 0x00801000,
  IO_END     = 0x00803FFF,
  IO_SIZE    = 0x00003000,
  
  ROM_START  = 0x00C00000,
  ROM_END    = 0x00C7FFFF,
  ROM_SIZE   = 0x00080000,
  
  MMUMASK    = 0x00FFFFFF,
};

struct psxheader_t {
  uint8_t  sig[8];  // PS-X.EXE
  uint32_t text;       // SCE
  uint32_t data;       // SCE
  uint32_t ip;
  uint32_t gp;         // SCE
  uint32_t text_addr;
  uint32_t text_size;
  uint32_t data_addr;  // SCE
  uint32_t data_size;  // SCE
  uint32_t bss_addr;   // SCE
  uint32_t bss_size;   // SCE
  uint32_t stack_addr;
  uint32_t stack_size;
  uint32_t SavedSP;
  uint32_t SavedFP;
  uint32_t SavedGP;
  uint32_t SavedRA;
  uint32_t SavedS0;
};

/*======================================================*
 * MMU Handler
 *======================================================*/
// 24-bit address space
bus_t mmu(0x00FFFFFF);

void cpu_write8(uint32_t addr, uint8_t data, int type) {
  mmu.write(addr, data);
}
void cpu_write16(uint32_t addr, uint16_t data, int type) {
  mmu.write(addr, data, _SZ16);
}
void cpu_write32(uint32_t addr, uint32_t data, int type) {
  mmu.write(addr, data, _SZ32);
}
uint8_t cpu_read8(uint32_t addr, int type) {
  iodata_t data;
  mmu.read(addr, data);
  return data;
}
uint16_t cpu_read16(uint32_t addr, int type) {
  iodata_t data;
  mmu.read(addr, data, _SZ16);
  return data;
}
uint32_t cpu_read32(uint32_t addr, int type) {
  iodata_t data;
  mmu.read(addr, data, _SZ32);
  return data;
}

#include <string>
#include <map>
std::map<uint32_t, std::string> fnmap;

const char *fnname(uint32_t addr)
{
  return fnmap[addr & MMUMASK].c_str();
}

void loadsyms()
{
  FILE *fp;
  char line[256], name[256];
  uint32_t fn;
  
  if ((fp = fopen("pcxsyms.txt", "r")) != NULL) {
    while (fgets(line, sizeof(line), fp) != NULL) {
      sscanf(line, "%x %s", &fn, name);
      fnmap[fn & MMUMASK] = name;
    }
    fclose(fp);
  }
}

/* Gernerate Coverage Map */
void dumpcfg(mips_cpu *c, uint8_t *buf, size_t sz, int pos, int base)
{
  dstk stk(sz, printf);
  uint32_t op, opcode, func;
  int nxt[2];

  stk.push(pos, 1, dstk::PENDING);
  for(auto h : fnmap) {
    printf("%x %s\n", h.first, h.second.c_str());
    stk.push(h.first & 0xfffff, 1, dstk::PENDING);
  }
  while ((pos = stk.pop()) != -1) {
    printf("\n------------------------------ %.8x [%s]\n", pos, fnname(pos+base));
    do { 
      op = get32(buf + pos);
      stk.push(pos, 4, dstk::CODE);

      nxt[0] = pos+4;
      nxt[1] = -1;
      func = (op & 0x3F);
      opcode = (op >> 26) & 0x3F;
      printf("@@ %.8x: op=%.8x [%.2x,%.2x] ", pos, op, opcode, func);
      disasm(c, pos+base, op);
      
      if (opcode == 0x00 && (func == 0x0d || func == 0x08 || func == 0x09 || func == 0x0c)) {
	nxt[0] = -1;
      }
      else if (opcode >= 0x02 && opcode <= 0x03) {
	if (opcode == 2)
	  nxt[0] = -1;
	nxt[1] = signex(op, 26)*4;
      }
      else if (opcode >= 0x4 && opcode <= 0x07) {
	nxt[1] = pos + 4 + signex(op, 16)*4;
      }
      for (int i = 0; i < 2; i++) {
	if (nxt[i] != -1)
	  nxt[i] &= 0x000fffff;
	stk.push(nxt[i], 1, dstk::PENDING);
      }
      pos = nxt[0];
    } while (nxt[0] != -1 && nxt[1] == -1);
  }
  stk.showstk(128);
  exit(0);
}

/* GTE are in coproc 2 registers */
struct gte_t {
  vec3s16   &V0   = *(vec3s16 *)&copr[2][0];    // |000|vz0|vy0|vx0|
  vec3s16   &V1   = *(vec3s16 *)&copr[2][2];    // |000|vz1|vy1|vx1|
  vec3s16   &V2   = *(vec3s16 *)&copr[2][4];    // |000|vz2|vy2|vx2|
  uint8_t   *RGBC = (uint8_t*)&copr[2][6];      
  uint16_t  &OTZ  = *(uint16_t *)&copr[2][7];   // |000|otz|
  int16_t   &IR0  = *(int16_t *)&copr[2][8];    // |000|ir0|
  int16_t   &IR1  = *(int16_t *)&copr[2][9];    // |000|ir1|
  int16_t   &IR2  = *(int16_t *)&copr[2][10];   // |000|ir2|
  int16_t   &IR3  = *(int16_t *)&copr[2][11];   // |000|ir3|
  vec2s16   &S0   = *(vec2s16 *)&copr[2][12];   // |sy0|sx0|
  vec2s16   &S1   = *(vec2s16 *)&copr[2][13];   // |sy1|sx1|
  vec2s16   &S2   = *(vec2s16 *)&copr[2][14];   // |sy2|sx2|
  uint16_t  &SZ0  = *(uint16_t *)&copr[2][16];  // |000|sz0|
  uint16_t  &SZ1  = *(uint16_t *)&copr[2][17];  // |000|sz1|
  uint16_t  &SZ2  = *(uint16_t *)&copr[2][18];  // |000|sz2|
  uint16_t  &SZ3  = *(uint16_t *)&copr[2][19];  // |000|sz3|
  uint8_t   *RGB0 = (uint8_t *)&copr[2][20];
  uint8_t   *RGB1 = (uint8_t *)&copr[2][21];
  uint8_t   *RGB2 = (uint8_t *)&copr[2][22];
  int32_t   &MAC0 = *(int32_t *)&copr[2][24];
  int32_t   &MAC1 = *(int32_t *)&copr[2][25];
  int32_t   &MAC2 = *(int32_t *)&copr[2][26];
  int32_t   &MAC3 = *(int32_t *)&copr[2][27];
  uint16_t  &IRGB = *(uint16_t *)&copr[2][28];
  uint16_t  &ORGB = *(uint16_t *)&copr[2][29];
  int32_t   &LZCS = *(int32_t *)&copr[2][30];
  int32_t   &LZCR = *(int32_t *)&copr[2][31];

  mtx3x3    &ROT   = *(mtx3x3 *)&copr[2][32];  // ||000|xxx||xxx|xxx||xxx|xxx||xxx|xxx||xxx|xxx||
  vec3s32   &TRANS = *(vec3s32 *)&copr[2][37];
  mtx3x3    &LIGHT = *(mtx3x3 *)&copr[2][40];  // ||000|xxx||xxx|xxx||xxx|xxx||xxx|xxx||xxx|xxx||
  mtx3x3    &COLOR = *(mtx3x3 *)&copr[2][48];  // ||000|xxx||xxx|xxx||xxx|xxx||xxx|xxx||xxx|xxx||
};

gte_t gte;

void dumpcopr(int cop)
{
  int i;

  if (cop != 2)
    return;
  printf("=============== copr2:\n");
  showvec(gte.V0,"V0");
  showvec(gte.V1,"V1");
  showvec(gte.V2,"V2");
  showmtx(gte.ROT, "ROT");
  showmtx(gte.LIGHT, "LIGHT");
  showmtx(gte.COLOR, "COLOR");
  for (i = 0; i < 64; i++) {
    printf("%.8x ", copr[2][i]);
    if ((i & 0xF) == 0xF)
      printf("\n");
  }
}

struct kv gtecmds[] = {
  { 0x01, "RTPS" },
  { 0x06, "NCLIP" },
  { 0x0C, "OP" },
  { 0x10, "DPCS" },
  { 0x11, "INTPL" },
  { 0x12, "MVMVA" },
  { 0x13, "NCDS" },
  { 0x14, "CDP" },
  { 0x16, "NCDT" },
  { 0x1B, "NCCS" },
  { 0x1C, "CC" },
  { 0x1E, "NCS" },
  { 0x20, "NCT" },
  { 0x28, "SQR" },
  { 0x29, "DCPL" },
  { 0x2A, "DCPT" },
  { 0x2D, "AVSZ3" },
  { 0x2E, "AVSZ4" },
  { 0x30, "RTPT" },
  { 0x3D, "GPF" },
  { 0x3E, "GPL" },
  { 0x3F, "NCCT" },
  { -1 },
};

int gtecmd(gte_t *g, int cmd)
{
  int16_t &sx0 = g->S0.v[0];
  int16_t &sy0 = g->S0.v[1];
  int16_t &sx1 = g->S1.v[0];
  int16_t &sy1 = g->S1.v[1];
  int16_t &sx2 = g->S2.v[0];
  int16_t &sy2 = g->S2.v[1];
  
  switch(cmd) {
  case 0x01: // rtps
    break;
  case 0x06: // nclip
    g->MAC0 = sx0*sy1 + sx1*sy2 + sx2*sy0 - sx0*sy2 - sx1*sy0 - sx2*sy1;
    break;
  }
  return 0;
}

/* COPR2
 *   r0,r1 = VXY0, VZ0
 *   r2,r3 = VXY1, VZ1
 *   r4,r5 = VXY2, VZ2
 */
void mips_copr(mips_cpu *c, uint32_t op, int id)
{
  int cop = (op >> 26) & 3;
  int reg = (op >> 11) & 0x1F;

  switch ((op >> 21) & 0x1F) {
  case 0x00: // mfc Rt = cop#Rd(0-31)
    Rt = copr[cop][reg];
    printf("COPRMFC: R%2d <= %d.%d [%x]\n", rt, cop, reg, Rt);
    break;
  case 0x02: // cfc Rt = cop#Rd(32-63)
    Rt = copr[cop][32+reg];
    printf("COPRCFC: R%2d <= %d.%d [%x]\n", rt, cop, reg+32, Rt);
    break;
  case 0x04: // mtc
    copr[cop][reg] = Rt;
    printf("COPRMTC: R%2d => %d.%d [%x]\n", rt, cop, reg, Rt);
    break;
  case 0x06: // ctc cop#Rd(32-63) = Rt
    copr[cop][32+reg] = Rt;
    printf("COPRCTC: R%2d => %d.%d [%x]\n", rt, cop, reg+32, Rt);
    break;
  default:
    if (cop == 2 && (op & 0xFE000000) == 0x4A000000) {
      printf("GTECMD: fake:%x sf:%x mm:%x mv:%x tv:%x lm:%x cmd:%x : %s\n",
	     (op >> 20) & 0xF,
	     !!(op & D19),
	     (op >> 17) & 3,
	     (op >> 15) & 3,
	     (op >> 13) & 3,
	     !!(op & D10),
	     (op & 0x1F),
	     kvlookup(gtecmds, (op & 0x1F), "???"));
    }
    else {
      printf("unknown coproc : %.8x %d.%d\n", op, cop, reg);
    }
    break;
  }
}

void mips_syscall(uint32_t call)
{
  printf("@@ SysCall(0x%x)\n", call);
}

const char *ms(uint32_t addr)
{
  switch (addr) {
  case ROM_START ... ROM_END:
    return (const char *)&rom[addr % ROM_SIZE];
  case RAM_START ... RAM_END:
    return (const char *)&ram[addr % RAM_SIZE];
  }
  return "xxx";
}

struct kv bioscalls[] = {
  { 0xA000, "FileOpen(filename,accessmode)" },
  { 0xA001, "FileSeek(fd,offset,seektype)" },
  { 0xA002, "FileRead(fd,dst,length)" },
  { 0xA003, "FileWrite(fd,src,length)" },
  { 0xA004, "FileClose(fd)" },
  { 0xA005, "FileIoctl(fd,cmd,arg)" },
  { 0xA006, "exit(exitcode)" },
  { 0xA007, "FileGetDeviceFlag(fd)" },
  { 0xA008, "FileGetc(fd)" },
  { 0xA009, "FilePutc(char,fd)" },
  { 0xA00A, "todigit(char)" },
  { 0xA00B, "atof(src)     ;Does NOT work - uses (ABSENT) cop1 !!!" },
  { 0xA00C, "strtoul(src,src_end,base)" },
  { 0xA00D, "strtol(src,src_end,base)" },
  { 0xA00E, "abs(val)" },
  { 0xA00F, "labs(val)" },
  { 0xA010, "atoi(src)" },
  { 0xA011, "atol(src)" },
  { 0xA012, "atob(src,num_dst)" },
  { 0xA013, "SaveState(buf)" },
  { 0xA014, "RestoreState(buf,param)" },
  { 0xA015, "strcat(dst,src)" },
  { 0xA016, "strncat(dst,src,maxlen)" },
  { 0xA017, "strcmp(str1,str2)" },
  { 0xA018, "strncmp(str1,str2,maxlen)" },
  { 0xA019, "strcpy(dst,src)" },
  { 0xA01A, "strncpy(dst,src,maxlen)" },
  { 0xA01B, "strlen(src)" },
  { 0xA01C, "index(src,char)" },
  { 0xA01D, "rindex(src,char)" },
  { 0xA01E, "strchr(src,char)  ;exactly the same as 'index'" },
  { 0xA01F, "strrchr(src,char) ;exactly the same as 'rindex'" },
  { 0xA020, "strpbrk(src,list)" },
  { 0xA021, "strspn(src,list)" },
  { 0xA022, "strcspn(src,list)" },
  { 0xA023, "strtok(src,list)  ;use strtok(0,list) in further calls" },
  { 0xA024, "strstr(str,substr) - buggy" },
  { 0xA025, "toupper(char)" },
  { 0xA026, "tolower(char)" },
  { 0xA027, "bcopy(src,dst,len)" },
  { 0xA028, "bzero(dst,len)" },
  { 0xA029, "bcmp(ptr1,ptr2,len)      ;Bugged" },
  { 0xA02A, "memcpy(0x%x,0x%x,0x%x)" },
  { 0xA02B, "memset(0x%x,0x%x,0x%x)" },
  { 0xA02C, "memmove(0x%x,0x%x,0x%x)     ;Bugged" },
  { 0xA02D, "memcmp(0x%x,0x%x,0x%x)    ;Bugged" },
  { 0xA02E, "memchr(0x%x,0x%x,0x%x)" },
  { 0xA02F, "rand()" },
  { 0xA030, "srand(seed)" },
  { 0xA031, "qsort(base,nel,width,callback)" },
  { 0xA032, "strtod(src,src_end) ;Does NOT work - uses (ABSENT) cop1 !!!" },
  { 0xA033, "malloc(size)" },
  { 0xA034, "free(buf)" },
  { 0xA035, "lsearch(key,base,nel,width,callback)" },
  { 0xA036, "bsearch(key,base,nel,width,callback)" },
  { 0xA037, "calloc(sizx,sizy)            ;SLOW!" },
  { 0xA038, "realloc(old_buf,new_siz)     ;SLOW!" },
  { 0xA039, "InitHeap(addr,size)" },
  { 0xA03A, "SystemErrorExit(exitcode)" },
  { 0xA03B, "std_in_getchar()" },
  { 0xA03C, "std_out_putchar('%c')" },
  { 0xA03D, "std_in_gets(dst)" },
  { 0xA03E, "std_out_puts(src)" },
  { 0xA03F, "printf(txt,param1,param2,etc.)" },
  { 0xA040, "SystemErrorUnresolvedException()" },
  { 0xA041, "LoadExeHeader(filename,headerbuf)" },
  { 0xA042, "LoadExeFile(filename,headerbuf)" },
  { 0xA043, "DoExecute(headerbuf,param1,param2)" },
  { 0xA044, "FlushCache()" },
  { 0xA045, "init_a0_b0_c0_vectors" },
  { 0xA046, "GPU_dw(Xdst,Ydst,Xsiz,Ysiz,src)" },
  { 0xA047, "gpu_send_dma(Xdst,Ydst,Xsiz,Ysiz,src)" },
  { 0xA048, "SendGP1Command(gp1cmd)" },
  { 0xA049, "GPU_cw(gp0cmd)   ;send GP0 command word" },
  { 0xA04A, "GPU_cwp(src,num) ;send GP0 command word and parameter words" },
  { 0xA04B, "send_gpu_linked_list(src)" },
  { 0xA04C, "gpu_abort_dma()" },
  { 0xA04D, "GetGPUStatus()" },
  { 0xA04E, "gpu_sync()" },
  { 0xA04F, "SystemError" },
  { 0xA050, "SystemError" },
  { 0xA051, "LoadAndExecute(filename,stackbase,stackoffset)" },
  { 0xA052, "SystemError ----OR---- 'GetSysSp()' ?" },
  { 0xA053, "SystemError           ;PS2: set_ioabort_handler(src)" },
  { 0xA054, "CdInit()" },
  { 0xA055, "bu_init()" },
  { 0xA056, "CdRemove()  ;does NOT work due to SysDeqIntRP bug" },
  { 0xA057, "return 0" },
  { 0xA058, "return 0" },
  { 0xA059, "return 0" },
  { 0xA05A, "return 0" },
  { 0xA05B, "dev_tty_init()                                      ;PS2: SystemError" },
  { 0xA05C, "dev_tty_open(fcb,and unused:'path\name',accessmode) ;PS2: SystemError" },
  { 0xA05D, "dev_tty_in_out(fcb,cmd)                             ;PS2: SystemError" },
  { 0xA05E, "dev_tty_ioctl(fcb,cmd,arg)                          ;PS2: SystemError" },
  { 0xA05F, "dev_cd_open(fcb,'path\name',accessmode)" },
  { 0xA060, "dev_cd_read(fcb,dst,len)" },
  { 0xA061, "dev_cd_close(fcb)" },
  { 0xA062, "dev_cd_firstfile(fcb,'path\name',direntry)" },
  { 0xA063, "dev_cd_nextfile(fcb,direntry)" },
  { 0xA064, "dev_cd_chdir(fcb,'path')" },
  { 0xA065, "dev_card_open(fcb,'path\name',accessmode)" },
  { 0xA066, "dev_card_read(fcb,dst,len)" },
  { 0xA067, "dev_card_write(fcb,src,len)" },
  { 0xA068, "dev_card_close(fcb)" },
  { 0xA069, "dev_card_firstfile(fcb,'path\name',direntry)" },
  { 0xA06A, "dev_card_nextfile(fcb,direntry)" },
  { 0xA06B, "dev_card_erase(fcb,'path\name')" },
  { 0xA06C, "dev_card_undelete(fcb,'path\name')" },
  { 0xA06D, "dev_card_format(fcb)" },
  { 0xA06E, "dev_card_rename(fcb1,'path\name1',fcb2,'path\name2')" },
  { 0xA06F, "?   ;card ;[r4+18h]=00000000h  ;card_clear_error(fcb) or so" },
  { 0xA070, "_bu_init()" },
  { 0xA071, "CdInit()" },
  { 0xA072, "CdRemove()   ;does NOT work due to SysDeqIntRP bug" },
  { 0xA073, "return 0" },
  { 0xA074, "return 0" },
  { 0xA075, "return 0" },
  { 0xA076, "return 0" },
  { 0xA077, "return 0" },
  { 0xA078, "CdAsyncSeekL(src)" },
  { 0xA079, "return 0               ;DTL-H: Unknown?" },
  { 0xA07A, "return 0               ;DTL-H: Unknown?" },
  { 0xA07B, "return 0               ;DTL-H: Unknown?" },
  { 0xA07C, "CdAsyncGetStatus(dst)" },
  { 0xA07D, "return 0               ;DTL-H: Unknown?" },
  { 0xA07E, "CdAsyncReadSector(count,dst,mode)" },
  { 0xA07F, "return 0               ;DTL-H: Unknown?" },
  { 0xA080, "return 0               ;DTL-H: Unknown?" },
  { 0xA081, "CdAsyncSetMode(mode)" },
  { 0xA082, "return 0               ;DTL-H: Unknown?" },
  { 0xA083, "return 0               ;DTL-H: Unknown?" },
  { 0xA084, "return 0               ;DTL-H: Unknown?" },
  { 0xA085, "return 0               ;DTL-H: Unknown?, or reportedly, CdStop (?)" },
  { 0xA086, "return 0               ;DTL-H: Unknown?" },
  { 0xA087, "return 0               ;DTL-H: Unknown?" },
  { 0xA088, "return 0               ;DTL-H: Unknown?" },
  { 0xA089, "return 0               ;DTL-H: Unknown?" },
  { 0xA08A, "return 0               ;DTL-H: Unknown?" },
  { 0xA08B, "return 0               ;DTL-H: Unknown?" },
  { 0xA08C, "return 0               ;DTL-H: Unknown?" },
  { 0xA08D, "return 0               ;DTL-H: Unknown?" },
  { 0xA08E, "return 0               ;DTL-H: Unknown?" },
  { 0xA08F, "return 0               ;DTL-H: Unknown?" },
  { 0xA090, "CdromIoIrqFunc1()" },
  { 0xA091, "CdromDmaIrqFunc1()" },
  { 0xA092, "CdromIoIrqFunc2()" },
  { 0xA093, "CdromDmaIrqFunc2()" },
  { 0xA094, "CdromGetInt5errCode(dst1,dst2)" },
  { 0xA095, "CdInitSubFunc()" },
  { 0xA096, "AddCDROMDevice()" },
  { 0xA097, "AddMemCardDevice()     ;DTL-H: SystemError" },
  { 0xA098, "AddDuartTtyDevice()    ;DTL-H: AddAdconsTtyDevice ;PS2: SystemError" },
  { 0xA099, "AddDummyTtyDevice()" },
  { 0xA09A, "SystemError            ;DTL-H: AddMessageWindowDevice" },
  { 0xA09B, "SystemError            ;DTL-H: AddCdromSimDevice" },
  { 0xA09C, "SetConf(num_EvCB,num_TCB,stacktop)" },
  { 0xA09D, "GetConf(num_EvCB_dst,num_TCB_dst,stacktop_>dst)" },
  { 0xA09E, "SetCdromIrqAutoAbort(type,flag)" },
  { 0xA09F, "SetMemSize(megabytes)" },
  { 0xA0A0, "WarmBoot()" },
  { 0xA0A1, "SystemErrorBootOrDiskFailure(type,errorcode)" },
  { 0xA0A2, "EnqueueCdIntr()  ;with prio=0 (fixed)" },
  { 0xA0A3, "DequeueCdIntr()  ;does NOT work due to SysDeqIntRP bug" },
  { 0xA0A4, "CdGetLbn(filename) ;get 1st sector number (or garbage when not found)" },
  { 0xA0A5, "CdReadSector(count,sector,buffer)" },
  { 0xA0A6, "CdGetStatus()" },
  { 0xA0A7, "bu_callback_okay()" },
  { 0xA0A8, "bu_callback_err_write()" },
  { 0xA0A9, "bu_callback_err_busy()" },
  { 0xA0AA, "bu_callback_err_eject()" },
  { 0xA0AB, "_card_info(port)" },
  { 0xA0AC, "_card_async_load_directory(port)" },
  { 0xA0AD, "set_card_auto_format(flag)" },
  { 0xA0AE, "bu_callback_err_prev_write()" },
  { 0xA0AF, "card_write_test(port)  ;CEX-1000: jump_to_00000000h" },
  { 0xA0B0, "return 0               ;CEX-1000: jump_to_00000000h" },
  { 0xA0B1, "return 0               ;CEX-1000: jump_to_00000000h" },
  { 0xA0B2, "ioabort_raw(param)     ;CEX-1000: jump_to_00000000h" },
  { 0xA0B3, "return 0               ;CEX-1000: jump_to_00000000h" },
  { 0xA0B4, "GetSystemInfo(index)   ;CEX-1000: jump_to_00000000h" },
  { 0xB000, "alloc_kernel_memory(size)" },
  { 0xB001, "free_kernel_memory(buf)" },
  { 0xB002, "init_timer(t,reload,flags)" },
  { 0xB003, "get_timer(t)" },
  { 0xB004, "enable_timer_irq(t)" },
  { 0xB005, "disable_timer_irq(t)" },
  { 0xB006, "restart_timer(t)" },
  { 0xB007, "DeliverEvent(class, spec)" },
  { 0xB008, "OpenEvent(class,spec,mode,func)" },
  { 0xB009, "CloseEvent(event)" },
  { 0xB00A, "WaitEvent(event)" },
  { 0xB00B, "TestEvent(event)" },
  { 0xB00C, "EnableEvent(event)" },
  { 0xB00D, "DisableEvent(event)" },
  { 0xB00E, "OpenThread(reg_PC,reg_SP_FP,reg_GP)" },
  { 0xB00F, "CloseThread(handle)" },
  { 0xB010, "ChangeThread(handle)" },
  { 0xB011, "jump_to_00000000h" },
  { 0xB012, "InitPad(buf1,siz1,buf2,siz2)" },
  { 0xB013, "StartPad()" },
  { 0xB014, "StopPad()" },
  { 0xB015, "OutdatedPadInitAndStart(type,button_dest,unused,unused)" },
  { 0xB016, "OutdatedPadGetButtons()" },
  { 0xB017, "ReturnFromException()" },
  { 0xB018, "SetDefaultExitFromException()" },
  { 0xB019, "SetCustomExitFromException(addr)" },
  { 0xB01A, "SystemError  ;PS2: return 0" },
  { 0xB01B, "SystemError  ;PS2: return 0" },
  { 0xB01C, "SystemError  ;PS2: return 0" },
  { 0xB01D, "SystemError  ;PS2: return 0" },
  { 0xB01E, "SystemError  ;PS2: return 0" },
  { 0xB01F, "SystemError  ;PS2: return 0" },
  { 0xB020, "UnDeliverEvent(class,spec)" },
  { 0xB021, "SystemError  ;PS2: return 0" },
  { 0xB022, "SystemError  ;PS2: return 0" },
  { 0xB023, "SystemError  ;PS2: return 0" },
  { 0xB024, "jump_to_00000000h" },
  { 0xB025, "jump_to_00000000h" },
  { 0xB026, "jump_to_00000000h" },
  { 0xB027, "jump_to_00000000h" },
  { 0xB028, "jump_to_00000000h" },
  { 0xB029, "jump_to_00000000h" },
  { 0xB02A, "SystemError  ;PS2: return 0" },
  { 0xB02B, "SystemError  ;PS2: return 0" },
  { 0xB02C, "jump_to_00000000h" },
  { 0xB02D, "jump_to_00000000h" },
  { 0xB02E, "jump_to_00000000h" },
  { 0xB02F, "jump_to_00000000h" },
  { 0xB030, "jump_to_00000000h" },
  { 0xB031, "jump_to_00000000h" },
  { 0xB032, "FileOpen(filename,accessmode)" },
  { 0xB033, "FileSeek(fd,offset,seektype)" },
  { 0xB034, "FileRead(fd,dst,length)" },
  { 0xB035, "FileWrite(fd,src,length)" },
  { 0xB036, "FileClose(fd)" },
  { 0xB037, "FileIoctl(fd,cmd,arg)" },
  { 0xB038, "exit(exitcode)" },
  { 0xB039, "FileGetDeviceFlag(fd)" },
  { 0xB03A, "FileGetc(fd)" },
  { 0xB03B, "FilePutc(char,fd)" },
  { 0xB03C, "std_in_getchar()" },
  { 0xB03D, "std_out_putchar(char)" },
  { 0xB03E, "std_in_gets(dst)" },
  { 0xB03F, "std_out_puts(src)" },
  { 0xB040, "chdir(name)" },
  { 0xB041, "FormatDevice(devicename)" },
  { 0xB042, "firstfile(filename,direntry)" },
  { 0xB043, "nextfile(direntry)" },
  { 0xB044, "FileRename(old_filename,new_filename)" },
  { 0xB045, "FileDelete(filename)" },
  { 0xB046, "FileUndelete(filename)" },
  { 0xB047, "AddDevice(device_info)  ;subfunction for AddXxxDevice functions" },
  { 0xB048, "RemoveDevice(device_name_lowercase)" },
  { 0xB049, "PrintInstalledDevices()" },
  { 0xB04A, "InitCard(pad_enable)  ;uses/destroys k0/k1 !!!" },
  { 0xB04B, "StartCard()" },
  { 0xB04C, "StopCard()" },
  { 0xB04D, "_card_info_subfunc(port)  ;subfunction for '_card_info'" },
  { 0xB04E, "write_card_sector(port,sector,src)" },
  { 0xB04F, "read_card_sector(port,sector,dst)" },
  { 0xB050, "allow_new_card()" },
  { 0xB051, "Krom2RawAdd(shiftjis_code)" },
  { 0xB052, "SystemError  ;PS2: return 0" },
  { 0xB053, "Krom2Offset(shiftjis_code)" },
  { 0xB054, "GetLastError()" },
  { 0xB055, "GetLastFileError(fd)" },
  { 0xB056, "GetC0Table" },
  { 0xB057, "GetB0Table" },
  { 0xB058, "get_bu_callback_port()" },
  { 0xB059, "testdevice(devicename)" },
  { 0xB05A, "SystemError  ;PS2: return 0" },
  { 0xB05B, "ChangeClearPad(int)" },
  { 0xB05C, "get_card_status(slot)" },
  { 0xB05D, "wait_card_status(slot)" },
  { 0xC000, "EnqueueTimerAndVblankIrqs(priority) ;used with prio=1" },
  { 0xC001, "EnqueueSyscallHandler(priority)     ;used with prio=0" },
  { 0xC002, "SysEnqIntRP(priority,struc)  ;bugged, use with care" },
  { 0xC003, "SysDeqIntRP(priority,struc)  ;bugged, use with care" },
  { 0xC004, "get_free_EvCB_slot()" },
  { 0xC005, "get_free_TCB_slot()" },
  { 0xC006, "ExceptionHandler()" },
  { 0xC007, "InstallExceptionHandlers()  ;destroys/uses k0/k1" },
  { 0xC008, "SysInitMemory(addr,size)" },
  { 0xC009, "SysInitKernelVariables()" },
  { 0xC00A, "ChangeClearRCnt(t,flag)" },
  { 0xC00B, "SystemError  ;PS2: return 0" },
  { 0xC00C, "InitDefInt(priority) ;used with prio=3" },
  { 0xC00D, "SetIrqAutoAck(irq,flag)" },
  { 0xC00E, "return 0               ;DTL-H2000: dev_sio_init" },
  { 0xC00F, "return 0               ;DTL-H2000: dev_sio_open" },
  { 0xC010, "return 0               ;DTL-H2000: dev_sio_in_out" },
  { 0xC011, "return 0               ;DTL-H2000: dev_sio_ioctl" },
  { 0xC012, "InstallDevices(ttyflag)" },
  { 0xC013, "FlushStdInOutPut()" },
  { 0xC014, "return 0               ;DTL-H2000: SystemError" },
  { 0xC015, "tty_cdevinput(circ,char)" },
  { 0xC016, "tty_cdevscan()" },
  { 0xC017, "tty_circgetc(circ)    ;uses r5 as garbage txt for ioabort" },
  { 0xC018, "tty_circputc(char,circ)" },
  { 0xC019, "ioabort(txt1,txt2)" },
  { 0xC01A, "set_card_find_mode(mode)  ;0=normal, 1=find deleted files" },
  { 0xC01B, "KernelRedirect(ttyflag)   ;PS2: ttyflag=1 causes SystemError" },
  { 0xC01C, "AdjustA0Table()" },
  { 0xC01D, "get_card_find_mode()" },
  { -1 },
}; 

void bios_call(mips_cpu *c)
{
  int cmd;
  char str[128];
  uint32_t *regs = c->regs;
  uint32_t *jmpslot = c->jmpslot;
  const char *sfx;
  
  cmd = (c->jmpslot[0] * 0x100) + c->regs[9];
  printf("Bios Call %.2x%.2x(%.8x,%.8x,%.8x,%.8x): ",
	 jmpslot[0], regs[9], regs[4], regs[5], regs[6], regs[7]);
  switch (cmd) {
  case 0xa03f:
    // printf
    printf(" printf('%s',0x%x,0x%x,0x%x)\n", ms(regs[4]), regs[5], regs[6], regs[7]);
    return;
  case 0xb015: // outdatedpadinitandstart
    break;
  case 0xa03C: // std_out_putchar
  case 0xb03d: // std_out_putchar
    if (regs[4] == '\n')
      sfx = "'\\n'";
    else {
      snprintf(str, sizeof(str), "'%c'", regs[4]);
      char c = regs[4];
      if (c == '\r')
	c = '\n';
      write(psxlogfd, &c, 1);
      sfx = str;
    }
    printf(": std_out_putchar(%s);\n", sfx);
    fprintf(stderr, "%c", regs[4]);
    return;
  }
  sfx = kvlookup(bioscalls, cmd, "zzz");
  if (sfx != NULL) {
    printf(": %s\n", sfx);
    return;
  }
  printf(": zzz\n");
}

extern void _cpu_step(mips_cpu *c);

/* 2 cycles per instruction, 16.9 million per sec , ~281666 cycles per frame */
int cpu_step(mips_cpu *c)
{
  uint32_t *regs = c->regs;
  uint32_t *jmpslot = c->jmpslot;
  
  static uint32_t ctr;

  if ((++ctr % 28000) == 0 && screen) {
    ctr = 0;
    _drawgpu();
    screen->draw();
  }

  /* Reset zero */
  regs[0] = 0;
  
  /* Sliding jumpslot */
  if (jmpslot[0] != 0xFFFFFFFF) {
    if (jmpslot[0] == 0xA0 || jmpslot[0] == 0xB0 || jmpslot[0] == 0xc0) {
      hexdump(&ram[0x00], 256);
      // BIOS Call
      bios_call(c);
    }
    c->PC = jmpslot[0];
  }
  jmpslot[0] = jmpslot[1];
  jmpslot[1] = 0xFFFFFFFF;

  _cpu_step(c);
  return 0;
}
mips_cpu c;

void cpu_reset(uint32_t addr) {
  memset(copr, 0, sizeof(copr));
  memset(c.regs, 0, sizeof(c.regs));

  copr[0][12] = 0x10900000;
  copr[0][15] = 0x2;
  
  c.jmpslot[0] = addr;
  c.jmpslot[1] = 0xFFFFFFFF;
}

int psxio(void *arg, uint32_t addr, int mode, iodata_t& data)
{
  uint8_t *buf;

  buf = (uint8_t *)arg + addr;
  if (mode == _RD8)
    data = get8(buf);
  else if (mode == _RD16)
    data = get16(buf);
  else if (mode == _RD32)
    data = get32(buf);
  else if (mode == _WR32)
    put32(buf, data);
  else if (mode == _WR16)
    put16(buf, data);
  else
    put8(buf, data);
  return 0;
}

/*==================================================*
 * GPU
 * 
 * Data register  : 1010 wr
 *  Read/Write 1 word from GPU
 *
 * Command        : 1014 wr
 *
 * Status register: 1014 rd
 *  31  30  29  28  27  26  25  24  23  22  21  20  19  18  17  16  15  14  13  12  11  10  9   8   7   6   5   4   3   2   1   0
 * +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
 * |odd|dma    |rfc|rfv|bsy|??     |den|int|dcd|pal|hgt|width      |?          |me |md |dfe|dtd|tp     |abr    |ty |tx             |
 * +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
 *==================================================*/
enum {
  GP0 = 0x1810,
  GP1 = 0x1814,

  GPUDATA   = 0x1810,
  GPUSTATUS = 0x1814,
};

#define GPU_COMMAND(x) (((x) >> 24) & 0xFF)

/* GP0 commands */
struct kv gp0[] = {
  // 0010.0000 mono3
  // 0010.0010 mono3
  // 0010.1000 mono4
  // 0010.1010 mono4
  { 0x20, "Monochrome three-point polygon, opaque" },
  { 0x22, "Monochrome three-point polygon, semi-transparent" },
  { 0x28, "Monochrome four-point polygon, opaque" },
  { 0x2A, "Monochrome four-point polygon, semi-transparent" },

  // 0010.0100 tex3
  // 0010.0101 tex3
  // 0010.0110 tex3
  // 0010.0111 tex3
  { 0x24, "Textured three-point polygon, opaque, texture-blending" },
  { 0x25, "Textured three-point polygon, opaque, raw-texture" },
  { 0x26, "Textured three-point polygon, semi-transparent, texture-blending" },
  { 0x27, "Textured three-point polygon, semi-transparent, raw-texture" },

  // 0010.1100 tex4
  // 0010.1101 tex4
  // 0010.1110 tex4
  // 0010.1111 tex4
  { 0x2C, "Textured four-point polygon, opaque, texture-blending" },
  { 0x2D, "Textured four-point polygon, opaque, raw-texture" },
  { 0x2E, "Textured four-point polygon, semi-transparent, texture-blending" },
  { 0x2F, "Textured four-point polygon, semi-transparent, raw-texture" },

  // 0011.0000 shade3
  // 0011.0010 shade3
  // 0011.1000 shade4
  // 0011.1010 shade4
  { 0x30, "Shaded three-point polygon, opaque" },
  { 0x32, "Shaded three-point polygon, semi-transparent" },
  { 0x38, "Shaded four-point polygon, opaque" },
  { 0x3A, "Shaded four-point polygon, semi-transparent" },

  { 0x34, "Shaded Textured three-point polygon, opaque, texture-blending" },
  { 0x36, "Shaded Textured three-point polygon, semi-transparent, tex-blend" },
  { 0x3C, "Shaded Textured four-point polygon, opaque, texture-blending" },
  { 0x3E, "Shaded Textured four-point polygon, semi-transparent, tex-blend" },

  { 0x40, "Monochrome line, opaque" },
  { 0x42, "Monochrome line, semi-transparent" },
  { 0x48, "Monochrome Poly-line, opaque" },
  { 0x4A, "Monochrome Poly-line, semi-transparent" },

  { 0x50, "Shaded line, opaque" },
  { 0x52, "Shaded line, semi-transparent" },
  { 0x58, "Shaded Poly-line, opaque" },
  { 0x5A, "Shaded Poly-line, semi-transparent" },

  { 0x60, "Monochrome Rectangle (variable size) (opaque)" },
  { 0x62, "Monochrome Rectangle (variable size) (semi-transparent)" },
  { 0x68, "Monochrome Rectangle (1x1) (Dot) (opaque)" },
  { 0x6A, "Monochrome Rectangle (1x1) (Dot) (semi-transparent)" },
  { 0x70, "Monochrome Rectangle (8x8) (opaque)" },
  { 0x72, "Monochrome Rectangle (8x8) (semi-transparent)" },
  { 0x78, "Monochrome Rectangle (16x16) (opaque)" },
  { 0x7A, "Monochrome Rectangle (16x16) (semi-transparent)" },

  { 0x64, "Textured Rectangle, variable size, opaque, texture-blending" },
  { 0x65, "Textured Rectangle, variable size, opaque, raw-texture" },
  { 0x66, "Textured Rectangle, variable size, semi-transp, texture-blending" },
  { 0x67, "Textured Rectangle, variable size, semi-transp, raw-texture" },
  { 0x6C, "Textured Rectangle, 1x1 (nonsense), opaque, texture-blending" },
  { 0x6D, "Textured Rectangle, 1x1 (nonsense), opaque, raw-texture" },
  { 0x6E, "Textured Rectangle, 1x1 (nonsense), semi-transp, texture-blending" },
  { 0x6F, "Textured Rectangle, 1x1 (nonsense), semi-transp, raw-texture" },
  { 0x74, "Textured Rectangle, 8x8, opaque, texture-blending" },
  { 0x75, "Textured Rectangle, 8x8, opaque, raw-texture" },
  { 0x76, "Textured Rectangle, 8x8, semi-transparent, texture-blending" },
  { 0x77, "Textured Rectangle, 8x8, semi-transparent, raw-texture" },
  { 0x7C, "Textured Rectangle, 16x16, opaque, texture-blending" },
  { 0x7D, "Textured Rectangle, 16x16, opaque, raw-texture" },
  { 0x7E, "Textured Rectangle, 16x16, semi-transparent, texture-blending" },
  { 0x7F, "Textured Rectangle, 16x16, semi-transparent, raw-texture" },

  { 0xe1, "DrawMode" },
  { 0xe2, "Texture Window" },
  { 0xe3, "Set Drawing Area UL" },
  { 0xe4, "Set Drawing Area LR" },
  { 0xe5, "Set Drawing Offset" },

  { 0x02, "FillRectVRAM" },
  { 0x80, "CopyVRAM2VRAM" },
  { 0xa0, "CopyCPU2VRAM" },
  { 0xc0, "CopyVRAM2CPU" },
  { -1 },
};

/* GP1 commands */
struct kv gp1[] = {
  { 0x00, "reset" },
  { 0x01, "reset cmd buffer" },
  { 0x02, "ack gpu interrupt" },
  { 0x03, "display enable" },
  { 0x04, "dma direction" },
  { 0x05, "start of display area" },
  { 0x06, "horiz display range" },
  { 0x07, "vert display range" },
  { 0x08, "display mode" },
  { -1 },
};

/* decode coordinate:
 * yyyyyyyy.yyyyyyyy.xxxxxxxx.xxxxxxxx 16-bit (default)
 * --------.----yyyy.yyyyyyxx.xxxxxxxx 10-bit
 */
struct Point {
  uint16_t x, y;
  static Point shift(uint32_t xy, int shift) {
    const uint32_t mask = (1L << shift) - 1;
    return Point (xy & mask, (xy >> shift) & mask);
  };
  Point(uint32_t xy = 0) {
    x = (xy & 0xffff);
    y = (xy >> 16) & 0xffff;
  };
  Point(uint32_t _x, uint32_t _y) {
    x = _x;
    y = _y;
  };
  Point operator+(const Point& rhs) const {
    Point p;
    p.x = x + rhs.x;
    p.y = y + rhs.y;
    return p;
  };
  Point operator-(const Point& rhs) const {
    Point p;
    p.x = x - rhs.x;
    p.y = y - rhs.y;
    return p;
  };
  // cross product.... used for EDGE
  float cross(const Point& rhs) const {
    return (float)(x * rhs.y) - (y * rhs.x);
  };
};

struct Rect {
  uint16_t x, y, w, h;
  Rect(uint32_t xy = 0, uint32_t wh = 0, bool abs = false) {
    x = (xy & 0xffff);
    y = (xy >> 16);
    w = (wh & 0xffff);
    h = (wh >> 16);
    if (abs) {
      w -= x;
      h -= y;
    };
  }
};

/* RGB Color */
struct Color {
  uint8_t r, g, b;
  Color(const uint32_t bgr = 0) {
    r = (bgr >> 0) & 0xFF;
    g = (bgr >> 8) & 0xFF;
    b = (bgr >> 16) & 0xFF;
  };
  color getcolor() {
    return ((r << 16) | (g << 8) | b);
  };
};

void setColor(const Color& c)
{
  glColor3ub(c.r, c.g, c.b);
}

void setColor(const uint32_t bgr)
{
  setColor(Color(bgr));
}

/* Rasterize triangle
 * sort by y coords:
 *     ----  top equal      /\   bottom equal
 *     \  /                /  \
 *      \/                 ----
 *
 *     /|  left   |\     right
 *    / |         | \
 *   /  |         |  \
 *   \  |         |  /
 *    \ |         | /
 *     \|         |/
 *
 *
 */
struct gpu_t {
  enum {
    GP0_NONE = 0xFFFF,
    GP0_COPY = 0xdeadcafe,
    VARLEN   = 0x100,
  };
  /* Resolution/Mode */
  uint32_t xres = 256;
  uint32_t yres = 240;
  uint8_t  vmode;

  /* DMA mode */
  uint8_t dma;
  
  /* Start of Display */
  Point sod;

  /* Horiz/Vert display range */
  uint32_t x1, x2;
  uint32_t y1, y2;
  
  /* Texture page */
  uint8_t tx;          // D0-3
  uint8_t ty;          // D4
  uint8_t trans;       // D5-6
  uint8_t tpc;         // D7-8

  bool dither = false; // D09                                  GP0.E1
  bool pal    = false; // D20 NTSC[0] PAL[1]                   GP1.03
  bool dcd    = false; // D21 Color depth 15[0] 24[1           GP1.08                      
  bool vint   = false; // D22 Vertical Interlace off[0] on[1]  GP1.00
  bool den    = false; // D23 display enable[0] disable[1]     GP1.03
  bool rfc    = true;  // D26 ready for command
  bool rfv    = true;  // D27 ready for vram->cpu
  bool rfd    = true;  // D28 ready for dma
  bool odd    = false; // D31

  /* Upper Left/Lower Right clipping area */
  Point clip_ul, clip_lr;

  /* Command buffer */
  uint32_t cmd = GP0_NONE;
  uint32_t cmd_pos;
  uint32_t cmd_len;
  uint32_t cmd_data[16];
  Rect     bltRect, bltXY;
  uint32_t ret_data;

  Color mergeclr(Color &c, int x, int y, int mode) {
    uint32_t old = vram_getpix(x, y);
    Color nc = c;
    int r = (old >> 16) & 0xff;
    int g = (old >> 8) & 0xff;
    int b = (old >> 0) & 0xff;
    switch(mode) {
    case 0x0:
      nc.r = (c.r * 0.5) + (r * 0.5);
      nc.g = (c.g * 0.5) + (g * 0.5);
      nc.b = (c.b * 0.5) + (b * 0.5);
      break;
    }
    return nc;
  }
  void copyvram(Rect src, Point dest) {
    for (int y = 0; y < src.h; y++) {
      for (int x = 0; x < src.w; x++) {
	vram_setpix(dest.x + x, dest.y + y,
		    vram_getpix(src.x + x, src.y + y));
      }
    }
  }
  void fillrect(Rect dst, Color c) {
    for (int y = 0; y < dst.h; y++) {
      for (int x = 0; x < dst.w; x++) {
	vram_setpix(x, y, c.getcolor());
      }
    }
  };
  void fillrect(Point xy1, Point xy2, Color c) {
    for (int y = xy1.y; y < xy2.y; y++) {
      for (int x = xy1.x; x < xy2.x; x++) {
	Color mc;
	mc.r = 0x55;
	mc.g = 0x00;
	mc.b = 0x00;
	vram_setpix(xy1.x + x, xy1.y + y, mc.getcolor());
      }
    }
  };
  void filltri(int P0,int P1,int P2,int C0,int C1,int C2) {
    Point p0(cmd_data[P0]);
    Point p1(cmd_data[P1]);
    Point p2(cmd_data[P2]);
    Color c0(cmd_data[C0]);
    Color c1(cmd_data[C1]);
    Color c2(cmd_data[C2]);

    printf("filltri.. : %d %d/%d %d/%d %d\n", p0.x, p0.y, p1.x, p1.y, p2.x, p2.y);
    auto edges = [](const Point& p0, const Point& p1, const Point& p2) {
      return (float)(p1.x - p0.x) * (p2.y - p0.y) - (p1.y - p0.y) * (p2.x - p0.x);
    };
    int min_x = std::min({p0.x, p1.x, p2.x});
    int min_y = std::min({p0.y, p1.y, p2.y});
    int max_x = std::max({p0.x, p1.x, p2.x});
    int max_y = std::max({p0.y, p1.y, p2.y});

    Point p;
    float area = edges(p0, p1, p2);
    for (p.y = min_y; p.y <= max_y; p.y++) {
      for (p.x = min_x; p.x <= max_x; p.x++) {
	float w0 = edges(p1, p2, p) / area;
	float w1 = edges(p2, p0, p) / area;
	float w2 = edges(p0, p1, p) / area;
	if (w0 >= 0 && w1 >= 0 && w2 >= 0) {
	  Color crgb;
	  
	  crgb.r = (int)(c0.r * w0 + c1.r * w1 + c2.r * w2);
	  crgb.g = (int)(c0.g * w0 + c1.g * w1 + c2.g * w2);
	  crgb.b = (int)(c0.b * w0 + c1.b * w1 + c2.b * w2);
	  if (cmd & 0x2) {
	    crgb = mergeclr(crgb, p.x, p.y, 0);
	  }
	  vram_setpix(p.x, p.y, crgb.getcolor());
	}
      }
    }
  };
  void setblt(Rect r) {
    bltRect = r;
    bltXY.x = 0;
    bltXY.y = 0;
  }
  color vram[1024*1024];
  int vramincr(int n) {
    bltXY.x += n;
    if (bltXY.x < bltRect.w) {
      return 0;
    }
    bltXY.x = 0;
    if (++bltXY.y >= bltRect.h) {
      return -1;
    }
    return 1;
  }
  void polyfill(int n, Point *xy, Color c);

  // read/write pixel taking into account clipping
  void vram_setpix(int x, int y, color c, bool clip=true) {
    vram[(y * 1024) + x] = c;
  }
  color vram_getpix(int x, int y, bool clip=true) {
    return vram[(y * 1024) + x];
  };
  double mym;
  double my;
  void drawline(int x1, int x2, int y, Color c) {
    color cx = c.getcolor();
    while(x1 < x2) {
      Color cx = c;
      if (cmd & 0x2) {
	cx = mergeclr(c, x1, y, 0);
      }
      vram_setpix(x1++, y, cx.getcolor());
    }
  };

  /* Make vertex */
  void mkvtx(const Point &p, uint32_t clr, uint32_t tex = -1) {
    Color c(clr);
    float px = (p.x * 2.0 / xres) - 1.0;
    float py = ((yres - p.y) * 2.0 / yres) - 1.0;
    printf(" vertex: %f %f [%d,%d] : %.8x %.8x\n", px, py, p.x, p.y, c.getcolor(), tex);
    glColor3ub(c.r, c.g, c.b);
    glVertex2f(px, py);
  };
  
  void mkvtx(uint32_t v, uint32_t clr, uint32_t tex = -1) {
    mkvtx(Point(v), clr, tex);
  };
  
  /* Gets GP0 command length */
  int get_cmdlen(int cmd) {
    switch (cmd) {
    case 0x00: return (0);
    case 0x01: return (0);
    case 0x02: return (3); // fillrect
      /* Draw Polygon */
    case 0x20: case 0x22: return (4);
    case 0x28: case 0x2a: return (5);

    case 0x24 ... 0x27:   return (7);
    case 0x2c ... 0x2f:   return (9);

    case 0x30: case 0x32: return (6);
    case 0x38 ... 0x3a:   return (8);

    case 0x34: case 0x36: return (9);
    case 0x3c: case 0x3e: return (12);

      /* Draw Line */
    case 0x40: case 0x42: return (3);
    case 0x48: case 0x4a: return (3 | VARLEN);

    case 0x50: case 0x52: return (4);
    case 0x58: case 0x5a: return (4 | VARLEN);

      /* Draw Rectangles */
    case 0x60: case 0x62: return (3);
    case 0x68: case 0x6a: return (2);
    case 0x70: case 0x72: return (2);
    case 0x78: case 0x7a: return (2);

    case 0x64 ... 0x67:   return (4);
    case 0x6c ... 0x6f:   return (3);
    case 0x74 ... 0x77:   return (3);
    case 0x7c ... 0x7f:   return (3);

    case 0x80 ... 0x8f: return (4); // copy rect vram->vram
    case 0xa0 ... 0xbf: return (3); // copy rect cpu->vram
    case 0xc0 ... 0xdf: return (3); // copy rect vram->cpu

    case 0xe1 ... 0xe6: return (0);

    }
    printf("unknown command: %x\n", cmd);
    assert(0);
    /* Polylines keep using same data */
  };
  void drawLine() {
    glBegin(GL_LINE_STRIP);
    mkvtx(cmd_data[1], cmd_data[0]);
    switch (cmd) {
    case 0x40: case 0x42:
      mkvtx(cmd_data[2], cmd_data[0]);
      break;
    case 0x48: case 0x4a:
      for (int i = 1; i < cmd_pos; i++) {
	mkvtx(cmd_data[i], cmd_data[0]);
      }
      break;
    case 0x50: case 0x52:
    case 0x58: case 0x5a:
      for (int i = 1; i < cmd_pos; i+=2) {
	mkvtx(cmd_data[i], cmd_data[i-1]);
      }
      break;
    }
    glEnd();
    glFlush();
  };
  void drawRect() {
    int cp = 0, ci = 0;
    Point p[4];

    p[0] = Point(cmd_data[1]);
    switch (cmd) {
    case 0x60: case 0x62:
      //monochrome rectangle, opaque
      //monochrome, semi transparent
      p[2] = p[0] + Point(cmd_data[2]);
      break;
    case 0x64 ... 0x67:
      p[2] = p[0] + Point(cmd_data[3]);
      break;
    case 0x68: case 0x6a:
    case 0x6c ... 0x6f:
      // x1
      p[2] = p[0] + Point(1, 1);
      break;
    case 0x70: case 0x72:
    case 0x74 ... 0x77:
      // x8
      p[2] = p[0] + Point(8, 8);
      break;
    case 0x78: case 0x7a:
    case 0x7c ... 0x7f:
      // x16
      p[2] = p[0] + Point(16, 16);
      break;
    };
    fillrect(p[0], p[2], Color(cmd_data[0]));
  };
  void drawTriangle() {
    switch (cmd_len) {
    case 4: // color,v1,v2,v3 mono
      filltri(1,2,3,0,0,0);
      break;
    case 7: // color,v1,t1,v2,t2,v3,t3 textured
      filltri(1,3,5,0,0,0);
      break;
    case 6: // c1,v1,c2,v2,c3,v3 shaded
      filltri(1,3,5,0,2,4);
      break;
    case 9: // c1,v1,t1,c2,v2,t2,c3,v3,t3 shaded textured
      filltri(1,4,7,0,3,6);
      break;
    }
  }
  void drawQuad() {
    int ti[] = { -1, -1, -1, -1 };
    int pi[] = { 1, 2, 3, 4 };
    int ci[] = { 0, 0, 0, 0 };

    switch (cmd_len) {
    case 5: // c1,v1,v2,v3,v4 mono
      break;
    case 9: // c1,v1,t1,v2,t2,v3,t3,v4,t4 textured
      ti[0] = 2;
      pi[1] = 3; ti[1] = 4;
      pi[2] = 5; ti[2] = 6;
      pi[3] = 7; ti[3] = 8;
      break;
    case 8: // c1,v1,c2,v2,c3,v3,c4,v4 shaded
      ci[1] = 2; pi[1] = 3;
      ci[2] = 4; pi[2] = 5;
      ci[3] = 6; pi[3] = 7;
      break;
    case 12: // c1,v1,t1|c2,v2,t2|c3,v3,t3|c4,v4,t4 shaded textured
      ti[2] = 2;
      ci[1] = 3; pi[1] = 4;   ti[1] = 5;
      ci[2] = 6; pi[2] = 7;   ti[2] = 8;
      ci[3] = 9; pi[3] = 10;  ti[3] = 11;
      break;
    }
    filltri(pi[0],pi[1],pi[2],ci[0],ci[1],ci[2]);
    filltri(pi[1],pi[2],pi[3],ci[1],ci[2],ci[3]);
  }
  void reset() {
    vmode  = 0;
    dither = false;
    pal    = false;
    dcd    = false;
    den    = false;
    rfd    = true;
    rfv    = true;
    rfc    = true;
    odd    = false;
    vint   = false;
    x1 = x2 = y1 = y2 = 0;

    cmd = GP0_NONE;
    cmd_len = 0;
    cmd_pos = 0;
  };
  uint32_t get_status() {
    return ((tx << 0) | (ty << 4) | (trans << 5) | (tpc << 7) | (dither << 9) | (vmode << 16) | (pal << 20) |
	    (dcd << 21) | (vint << 22) | (den << 23) | (rfc << 26) | (rfv << 27) | (rfd << 28) | (dma << 29) |
	    (odd << 31) | 0x2000);
  };
  void SetGP0(uint32_t data) {
    if (cmd == GP0_NONE) {
      cmd = (data >> 24) & 0xFF;
      cmd_len = get_cmdlen(cmd);
      cmd_pos = 0;
      
      data &= 0xFFFFFF;
      printf(" GP0_CMD(%.2x.%.6x) : len=%d %s\n", cmd, data, cmd_len, kvlookup(gp0, cmd, "???"));
    }
    else if (cmd == GP0_COPY) {
      vram_setpix(bltRect.x + bltXY.x + 0, bltRect.y + bltXY.y, BGRRGB(data & 0xFFFF));
      vram_setpix(bltRect.x + bltXY.x + 1, bltRect.y + bltXY.y, BGRRGB(data >> 16));
      if (vramincr(2) < 0) {
	cmd = GP0_NONE;
      }
      return;
    }
    printf("   GPU_DATA(%.8x) pos:%.8x/%.4x\n", data, cmd_pos, cmd_len);

    /* Add data to vector, return if we don't yet have enough */
    cmd_data[cmd_pos++] = data;
    if ((cmd_len & VARLEN) && (data == 0x55555555)) {
      printf("varlen data...\n");
      cmd_pos--;
    }
    else if (cmd_pos < cmd_len) {
      return;
    }

    /* commands:
     * 1F = irq
     * 20..3F : render polygons
     * 40..5F : render lines
     * 60..7F : renter rectangles
     * E1..E6 : render attribs
     */
    switch (cmd) {
    case 0x20: case 0x22: // 4 mono
    case 0x24 ... 0x27:   // 7 textured
    case 0x30: case 0x32: // 6 shaded
    case 0x34: case 0x36: // 9 shaded textured
      drawTriangle();
      break;
    case 0x28: case 0x2a:
    case 0x2c ... 0x2f:  
    case 0x38: case 0x3a:
    case 0x3c: case 0x3e:
      drawQuad();
      break;
    case 0x40 ... 0x5f:
      drawLine();
      break;
    case 0x60 ... 0x7f:
      drawRect();
      break;
    case 0xE1: // DrawMode
      // cccccccc.--------.------d-.p-ty--xx
      tx     = (data & 3);
      ty     = (data >> 4) & 1;
      trans  = (data >> 5) & 3;
      tpc    = (data >> 7) & 3;
      dither = (data >> 9) & 1;
      break;
    case 0xE3: // set Drawing Area Upper Left
      clip_ul = Point::shift(data, 10);
      //glViewport(dul.x, dul.y, dlr.x, dlr.y);
      //glOrtho(0, xres, yres, 0, 0, 1);
      break;
    case 0xE4: // set Drawing Area Lower Right
      clip_lr = Point::shift(data, 10);
      //glViewport(dul.x, dul.y, dlr.x, dlr.y);
      //glOrtho(0, xres, yres, 0, 0, 1);
     break;
    case 0xa0 ... 0xbf: // copy to vram
      setblt(Rect(cmd_data[1], cmd_data[2]));
      cmd = GP0_COPY;
      return;
    case 0x80 ... 0x8f: // copy vram2vram
      copyvram(Rect(cmd_data[1], cmd_data[3]), Point(cmd_data[2]));
      break;
    case 0x02:
      fillrect(Rect(cmd_data[1], cmd_data[2]), Color(cmd_data[0]));
      break;
    }
    cmd = GP0_NONE;
  };

  /* Handle GP1 command */
  void SetGP1(uint32_t data) {
    int cmd  = (data >> 24) & 0xFF;

    data &= 0xFFFFFF;
    printf(" GP1_CMD(%.2x.%.6x) : %s\n", cmd, data, kvlookup(gp1, cmd, "???"));
    switch (cmd) {
    case 0x00: // gpu reset (14802000)
      reset();
      printf("pre\n");
      glMatrixMode(GL_PROJECTION);
      glLoadIdentity();
      printf("post\n");
      break;
    case 0x01: // reset command buffer;
      cmd_pos = 0;
      break;
    case 0x02: // ack GPU interrupt
      break;
    case 0x03: // display enable
      den = data & 0x1;
      break;
    case 0x04: // DMA direction
      dma = data & 0x3;
      break;
    case 0x05: // start of display area
      sod = Point(data, 10);
      printf("  sx:%4d sy:%4d\n", sod.x, sod.y);
      break;
    case 0x06: // horiz display range
      // cccccccc.XXXXXXXX.XXXXxxxx.xxxxxxxx
      x1 = data & 0xFFF;
      x2 = (data >> 12) & 0xFFF;
      printf("  X1:%4d X2:%4d\n", x1, x2);
      break;
    case 0x07: // vert display range
      // cccccccc.----YYYY.YYYYYYyy.yyyyyyyy
      y1 = data & 0x3FF;
      y2 = (data >> 10) & 0x3FF;
      printf("  Y1:%4d Y2:%4d\n", y1, y2);
      break;
    case 0x08: // display mode
      // cccccccc.--------.--------.-v--dvvv
      dcd = !!(data & D4);
      vmode = ((data & 7) << 1) | ((data >> 6) & 1);
      yres  = (vmode & 8) ? 480 : 240;
      switch (vmode & 7) {
      case 0b000: xres = 256; break;
      case 0b001: xres = 384; break;
      case 0b010: xres = 320; break;
      case 0b100: xres = 512; break;
      case 0b110: xres = 640; break;
      }
      printf("  vmode: %3dx%3d\n", xres, yres);
      xres = 1024;
      yres = 512;
      screen = new Screen(xres, yres, 20, 30, 0, NULL);
      screen->xs = 2;
      screen->ys = 2;
      screen->init(1);
      break;
    case 0x10: // get gpu info
      ret_data = 2;
      break;
    }
  };
};

struct edge_t {
  double x0, y0, y1, m;
  int flag;
};

static void swap(int *a, int *b)
{
  int tmp = *a;
  *a = *b;
  *b = tmp;
}

int edgecmp(const void *a, const void *b)
{
  const edge_t *ea = (edge_t *)a;
  const edge_t *eb = (edge_t *)b;
  int rc;

  if (eb->flag || (ea->y0 < eb->y0))
    return -1;
  if (ea->flag || (eb->y0 < ea->y0))
    return 1;
  return ea->x0 - eb->x0;
}

void gpu_t::polyfill(int nvertex, Point *xy, Color c)
{
  edge_t *global;
  int ng = 0, x0, y0, x1, y1;

  my = 0;
  global = (edge_t *)alloca(sizeof(edge_t) * nvertex);
  for (int n = 0; n < nvertex; n++) {
    x0 = xy[n].x;
    y0 = xy[n].y;
    x1 = xy[(n+1) % nvertex].x;
    y1 = xy[(n+1) % nvertex].y;
    if (y0 > y1) {
      // increasing y
      swap(&y0, &y1);
      swap(&x0, &x1);
    }
    if (y0 != y1) {
      // skip horizontal lines
      global[ng].x0 = x0;
      global[ng].y0 = y0;
      global[ng].y1 = y1;
      global[ng].flag = 0;
      global[ng].m = ((double)(x1-x0)) / (y1-y0);
      ng++;
    }
  }
  printf("haz: %d\n", ng);
#if 1
  while (ng) {
    qsort(global, ng, sizeof(edge_t), edgecmp);
    if (global[0].flag) {
      /* all segments are done */
      break;
    }
    y0 = global[0].y0;
    /* draw lines increasing x coords */
    for (int n = 0; n < ng && global[n].y0 == y0 && global[n].flag == 0; n += 2) {
      drawline(global[n].x0, global[(n+1)%nvertex].x0, y0, c);
    }
    for (int n = 0; n < ng && global[n].y0 == y0; n++) {
      /* increase x slope */
      global[n].x0 += global[n].m;
      /* if at max y, flag as done */
      if (++global[n].y0 >= global[n].y1) {
	global[n].flag = 1;
      }
    }
  }
  printf("done\n");
#endif
}

gpu_t gpu;

uint32_t kp = 0;
void setkeystate(int vk, int mask)
{
  if (screen->key(vk, true)) {
    kp |= mask;
  }
  else {
    kp &= ~mask;
  }
}

/*
JOY_L2     equ 0x0001 ; Joypad Input: L2       (Bit 0)
JOY_R2     equ 0x0002 ; Joypad Input: R2       (Bit 1)
JOY_L1     equ 0x0004 ; Joypad Input: L1       (Bit 2)
JOY_R1     equ 0x0008 ; Joypad Input: R1       (Bit 3)
JOY_T      equ 0x0010 ; Joypad Input: Triangle (Bit 4)
JOY_C      equ 0x0020 ; Joypad Input: Circle   (Bit 5)
JOY_X      equ 0x0040 ; Joypad Input: X        (Bit 6)
JOY_S      equ 0x0080 ; Joypad Input: Square   (Bit 7)
JOY_SELECT equ 0x0100 ; Joypad Input: Select   (Bit 8)
JOY_L3     equ 0x0200 ; Joypad Input: L3       (Bit 9)  (Analog Mode Only)
JOY_R3     equ 0x0400 ; Joypad Input: R3       (Bit 10) (Analog Mode Only)
JOY_START  equ 0x0800 ; Joypad Input: Start    (Bit 11)
JOY_UP     equ 0x1000 ; Joypad Input: Up       (Bit 12)
JOY_RIGHT  equ 0x2000 ; Joypad Input: Right    (Bit 13)
JOY_DOWN   equ 0x4000 ; Joypad Input: Down     (Bit 14)
JOY_LEFT   equ 0x8000 ; Joypad Input: Left     (Bit 15)
*/

enum {
  THPAD_UP = (1L << 12),
  THPAD_DOWN = (1L << 14),
  THPAD_LEFT = (1L << 15),
  THPAD_RIGHT = (1L << 13),
  THPAD_S = (1L << 7),
  THPAD_A = (1L << 11),
  THPAD_B = (1L << 8),
  THPAD_C = (1L << 5),
};

void _drawgpu() {
  static time_t fpstime = time(NULL);
  time_t now;
  float fps;

  setkeystate('i', THPAD_UP);
  setkeystate('k', THPAD_DOWN);
  setkeystate('j', THPAD_LEFT);
  setkeystate('l', THPAD_RIGHT);
  setkeystate('n', THPAD_S);
  setkeystate('m', THPAD_A);
  setkeystate(',', THPAD_B);
  setkeystate('.', THPAD_C);

  cpu_write32(0x80010bdc, kp);
  
  now = time(NULL);
  fps = (float)frame / (now - fpstime);
  for (int y = 0; y < 1024; y++) {
    for (int x = 0; x < 1024; x++) {
      screen->setpixel(x, y, gpu.vram_getpix(x, y));
    }
  }
  screen->scrtext(0, screen->height + 5, MKRGB(255,255,0),
		  "frame:%d fps:%.2f PC:%.8x",
		  frame, fps, SPC);
  frame++;
}

/* Packet commands:
 * GPU command:
 *   01: clear cache
 *   02: frame buffer rectangle
 *   80: move image in frame buffer
 *   a0: send image to frame buffer
 *       0: a0|BGR
 *       1: y | x
 *       2: h | w
 *   c0: copy image from frame buffer
 * Primitives:
 *   20: 4  mono    poly3
 *   24: 7  tex     poly3
 *   28: 5  mono    poly4
 *   2c: 9  tex     poly4
 *   30: 6  grad    poly3
 *   34: 9  gradtex poly3
 *   38: 8  grad    poly4
 *   3c: 12 gradtex poly4
 *   40: 3  mono line
 *   48: xx mono polyline [55555555 = term]
 *   50: 4  grad line
 *   58: xx grad polyline [55555555 = term]
 *   60: 3  rectangle
 *   64: 4  sprite
 *   68: 2  dot
 *   70: 2  8x8 rectangle
 *   74: 3  8x8 sprite
 *   78: 2  16x16 rectangle
 *   7c: 3  16x16 sprite
 * Mode:
 *   e1: draw mode setting
 *   e2: texture window setting
 *   e3: set drawing top left
 *   e4: set drawing bottom right
 *   e5: drawing offset
 *   e6: mask
 */
int gpuio(void *arg, uint32_t addr, int mode, iodata_t& data)
{
  int mm = (mode & 0xFF);
  
  switch (addr) {
  case GPUDATA:   // GP0
    if (mm == 'w') {
      gpu.SetGP0(data);
    }
    else {
      // should only be for GP0(C0) or GP1(10)
      printf("   %c.GPU_DATA(%.8x) pos:%.8x len:%.8x\n", mm, data, gpu.cmd_pos, 0);
      data = gpu.ret_data;
    }
    break;
  case GPUSTATUS: // GP1
    if (mm == 'w') {
      gpu.SetGP1(data);
    }
    else {
      data = gpu.get_status();
      printf(" GPU_STATUS: %.8x %3dx%3d rfc:%d rfv:%d dma:%d den:%d\n", data,
	     gpu.xres, gpu.yres, gpu.rfc, gpu.rfv, gpu.rfd, gpu.den);
    }
    break;
  }
  return 0;
}

void dmaio(void *arg, uint32_t addr, int mode, iodata_t& data)
{
  dumpdma();
  mode &= 0xFF;
  if ((addr & 0xf) == 8) {
    /* Start XFER */
    run_dma((addr >> 4) & 7);
  }
}

/* 1070            w
 * 1080 .. 10e8    w
 * 10f4            w
 * 1104 .. 1124    w rootc
 * 1810 .. 1814    w vs
 */
int psxreg(void *arg, uint32_t addr, int mode, iodata_t& data)
{
  psxio(arg, addr & 0xFFFF, mode, data);

  switch (addr & 0xFFFF) {
  case 0x1000 ... 0x1020: printf("%.8x.%c: I/O: MEMORY CONTROL    %.8x\n", addr, mode & 0xFF, data); break;
  case 0x1040 ... 0x105e: printf("%.8x.%c: I/O: PERIPHERAL        %.8x\n", addr, mode & 0xFF, data); break;
  case 0x1060:            printf("%.8x.%c: I/O: MEMORY CONTROL2   %.8x\n", addr, mode & 0xFF, data); break;
  case 0x1070 ... 0x1074: printf("%.8x.%c: I/O: INTERRUPT CONTROL %.8x\n", addr, mode & 0xFF, data); break;
  case 0x1080 ... 0x10FC: printf("%.8x.%c: I/O: DMA CONTROL       %.8x\n", addr, mode & 0xFF, data); dmaio(arg, addr & 0xFFFF, mode, data); break;
  case 0x1100 ... 0x112F: printf("%.8x.%c: I/O: TIMER             %.8x\n", addr, mode & 0xFF, data); break;
  case 0x1800 ... 0x1803: printf("%.8x.%c: I/O: CDROM             %.8x\n", addr, mode & 0xFF, data); break;
  case 0x1810 ... 0x1814: /*printf("%.8x.%c: I/O: GPU               %.8x\n", addr, mode & 0xFF, data);*/ gpuio(arg, addr & 0xFFFF, mode, data); break;
  case 0x1820 ... 0x1824: printf("%.8x.%c: I/O: MDEC              %.8x\n", addr, mode & 0xFF, data); break;
  case 0x1c00 ... 0x1DFF: printf("%.8x.%c: I/O: SPU               %.8x\n", addr, mode & 0xFF, data); break;
  case 0x2041:            printf("%.8x.%c: I/O: TRACE             %.8x\n", addr, mode & 0xFF, data); break;
  default:                printf("%.8x.%c: I/O: ???               %.8x\n", addr, mode & 0xFF, data); break;
  }
  return 0;
}

int psxram(void *arg, uint32_t addr, int mode, iodata_t& data)
{
  if ((mode & 0xFF) == 'w' && (copr[0][12] & 0x10000) != 0) {
    return 0;
  }
  return psxio(arg, addr, mode, data);
}

int psxscr(void *arg, uint32_t addr, int mode, iodata_t& data)
{
  return psxio(arg, addr, mode, data);
}

int psxrom(void *arg, uint32_t addr, int mode, iodata_t& data)
{
  return psxio(arg, addr, mode, data);
}

void flogger(int lvl, const char *fmt, ...) {
  va_list ap;

  va_start(ap, fmt);
  vprintf(fmt, ap);
}

int main(int argc, char *argv[])
{
  uint8_t *cart;
  psxheader_t *hdr;
  size_t sz;

  loadsyms();
  psxlogfd = open("psx.log", O_CREAT|O_TRUNC|O_WRONLY, 0660);
  
  /* load BIOS */
  rom = loadrom("SCPH1001.BIN", sz);
  assert(rom && sz == ROM_SIZE);

  /* read ROM */
  setbuf(stdout, NULL);
  cart = loadrom(argv[1], sz);
  hdr = (psxheader_t *)cart;

  // verify that data looks sane
  assert(sz >= 0x800 + hdr->text_size);
  assert(hdr->ip >= hdr->text_addr && hdr->ip < (hdr->text_addr + hdr->text_size));
  printf("size:  %.8zx\n", sz);

  ram = new uint8_t[RAM_SIZE]{0};
  scr = new uint8_t[SCR_SIZE]{0};

  if (argc > 2) {
    dumpcfg(&c, rom, sz, 0x0, hdr->text_addr);
    exit(0);
  }
  mmu.init(MMUMASK);
  /* Memory map
   * exp1 1f000000 1f080000
   * mc1  1f801000 1f801023
   * mc2  1f801060 1F801063
   * gpu  1f801810 1f801814
   * mdec 1f801820 1f801827
   * ic   1f801070 1F801077
   * exp2 1f802000 1f9fffff
   * bios 1fc00000 1fc7ffff
   */
  mmu.register_handler(RAM_START, RAM_END, 0x7FFFFF, psxram, ram,  _RW, "RAM");
  mmu.register_handler(SCR_START, SCR_END, 0x3FF,    psxscr, scr,  _RW, "SCRATCH");
  mmu.register_handler(IO_START,  IO_END,  0x3FFF,   psxreg, ioreg,_RW, "I/O");
  mmu.register_handler(ROM_START, ROM_END, 0x7FFFF,  psxrom, rom,  _RD, "bios.ROM");

  // print header
#define cp(x) printf("%-12s : %.8x [%12u]\n", #x, hdr->x, hdr->x);
  cp(text);
  cp(data);
  cp(ip);
  cp(gp);
  cp(text_addr);
  cp(text_size);
  cp(data_addr);
  cp(data_size);
  cp(bss_addr);
  cp(bss_size);
  cp(stack_addr);
  cp(stack_size);
  cp(SavedSP);
  cp(SavedFP);
  cp(SavedGP);
  cp(SavedRA);

  printf("%.8x %x\n", hdr->text_addr % RAM_SIZE, get32(ram + (hdr->ip % RAM_SIZE)));

  cpu_reset(0xBFC00000); // hdr.ip & (RAM_SIZE-1));
  //cpu_reset(hdr->ip & (RAM_SIZE-1));

  /* Run until 'LoadShell' */
  while (c.jmpslot[0] != 0x80030000) {
    cpu_step(&c);
  }
  printf("================================== Completed bootstrap!!!\n\n");
  printf("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ Start ROM: '%s'\n", argv[1]);

  /* Read in ROM */
  memcpy(&ram[hdr->text_addr & (RAM_SIZE-1)], &cart[0x800], hdr->text_size);

  if (argc > 2) {
    dumpcfg(&c, &cart[0x800], hdr->text_size, 0x0, 0x0);
    exit(0);
  }
  c.jmpslot[0] = (hdr->ip % RAM_SIZE);
  //jmpslot[0] = 0xc00460;
  for(;;)  {
    cpu_step(&c);
  }
}
