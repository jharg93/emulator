LXCPP = clang++ -std=c++20
SDLFLAGS = -DGR -D_SDL `sdl2-config --cflags --libs`
CFLAGS = -std=c++20 -Wall -ggdb3 -fomit-frame-pointer -O3 -Icpu -Icommon -I. ${SDLFLAGS}

STDC = common/bus.cc common/gr.cc common/util.cc common/cpu.cc
STDH = $(STDC) common/bus.h common/gr.h common/util.h Makefile

all: gboy snes c64 ppc 8086 mac amiga genesis appleii n64 atarixl

# Targets
gboy: gboy.cc cpu/cpu_gboy.cc gboy_apu.cc $(STDC)
	$(LXCPP) $(CFLAGS) -o gboy gboy.cc cpu/cpu_gboy.cc $(STDC)

gba: gba.cc $(STDH) $(ARMCPU) armcpu.h $(STDC)
	$(LXCPP) $(CFLAGS) -o gba gba.cc $(ARMCPU) -DGBA $(STDC)

psx: psx.cc cpu/mips.cc $(STDH) $(STDC)
	$(LXCPP) $(CFLAGS) -o psx psx.cc cpu/mips.cc -DOPENGL -Wall -lGL $(STDC)

snes: snes.cc cpu/cpu_65816.h $(STDH) $(STDC)
	$(LXCPP) $(CFLAGS) -o snes snes.cc $(STDC)

c64: c64.cc $(STDH) cpu/cpu_6502.cc $(STDC)
	$(LXCPP) $(CFLAGS) -o c64 c64.cc cpu/cpu_6502.cc $(STDC)

ppc: cpu/cpu_ppc.cc common/bus.cc common/cpu.cc
	$(LXCPP) $(CFLAGS) -o ppc cpu/cpu_ppc.cc common/bus.cc common/cpu.cc common/util.cc

8086: 8086.cc $(STDH) emit.cc $(STDC)
	$(LXCPP) $(CFLAGS) -o 8086 8086.cc -mavx2 $(STDC)

mac: mac.cc $(STDC)
	$(LXCPP) $(CFLAGS) -o mac mac.cc $(STDC)

amiga: amiga.cc $(STDC)
	$(LXCPP) $(CFLAGS) -o amiga amiga.cc $(STDC)

genesis: genesis.cc genesis.h $(STDC)
	$(LXCPP) $(CFLAGS) -o genesis genesis.cc -DM68K $(STDC)

appleii: appleii.cc cpu/cpu_6502.cc $(STDC)
	$(LXCPP) $(CFLAGS) -o appleii appleii.cc cpu/cpu_6502.cc $(STDC)

n64: n64.cc $(STDC)
	$(LXCPP) $(CFLAGS) -o n64 n64.cc -DN64 $(STDC)

atarixl: atarixl.cc cpu/cpu_6502.cc $(STDC)
	$(LXCPP) $(CFLAGS) -o atarixl atarixl.cc cpu/cpu_6502.cc $(STDC)

.PHONY: clean
clean:
	rm -f gboy gba psx snes c64 ppc 8086 mac amiga genesis appleii n64 atarixl
