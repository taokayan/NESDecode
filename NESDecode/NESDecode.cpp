// NESDecode.cpp : Defines the entry point for the console application.
//

#include "stdafx.h"
#include <stdlib.h>
#include <memory.h>
#include <afxwin.h>
#include "resource.h"
#include "EasyBitmap.h"

#define _CRT_SECURE_NO_WARNINGS
#pragma warning(push)
#pragma warning(disable: 4996)

#include <mmsystem.h>
#pragma comment(lib, "winmm.lib")

const static unsigned short AutoTraceOff[] = { 0,
	// mario
	//0x800a, 0x800d, 0x8057

	// test1
//	0xc129, 0xc12b, // loop
	//0xcbb3, 0xcbb4, // count down
  //  0xc135, 0xc138, 0xc13b, 0xc13c, 0xc13d, 0xc13e, 0xc13f // set y=f0 to pre-sprite memory

  // test3
  //0xc653, 0xc656, 0xc658, // initial wait 2002
  //0xc683, 0xc680, 0xee78, 0xee7a, 0xee7c,
  //0xee7e, 0xee80, 0xee82, 0xee84, 0xee85, 0xee88,
  //0xee8a, 0xee8c, // wait for VSYNC

  // test7
  // 0xc057
};

const static int NES_COLOR[] = {
  0x7C7C7C,0x0000FC,0x0000BC,0x4428BC,0x940084,0xA80020,0xA81000,0x881400,
  0x503000,0x007800,0x006800,0x005800,0x004058,0x000000,0x000000,0x000000,
  0xBCBCBC,0x0078F8,0x0058F8,0x6844FC,0xD800CC,0xE40058,0xF83800,0xE45C10,
  0xAC7C00,0x00B800,0x00A800,0x00A844,0x008888,0x000000,0x000000,0x000000,
  0xF8F8F8,0x3CBCFC,0x6888FC,0x9878F8,0xF878F8,0xF85898,0xF87858,0xFCA044,
  0xF8B800,0xB8F818,0x58D854,0x58F898,0x00E8D8,0x787878,0x000000,0x000000,
  0xFCFCFC,0xA4E4FC,0xB8B8F8,0xD8B8F8,0xF8B8F8,0xF8A4C0,0xF0D0B0,0xFCE0A8,
  0xF8D878,0xD8F878,0xB8F8B8,0xB8F8D8,0x00FCFC,0xF8D8F8,0x000000,0x000000
};

#pragma pack(push, 1)
struct NESFlag {
	enum _ { 
		vectical =   0x0001,
		batteryRAM = 0x0002,
		trainer512 = 0x0004,
		fourScreen = 0x0008,
		vsSystem   = 0x0100
	};
};
typedef struct NESHeader_
{
	unsigned char  m_nesstr[4];
	unsigned char  m_nROM;
	unsigned char  m_nVROM;
	unsigned short m_flag;
	unsigned char  m_nRAM;
	unsigned char  m_isPAL;
	unsigned char  m_pad[6];
} NESHeader;
#pragma pack(pop)

#define NES_STACKBASE  0x0100
#define NSE_ROMBASE    0x8000
#define NES_SR_INT     0x04;

struct NESMirroring {
	enum _ { 
		single = 0, vertical, horizontal, fourscreen
	};
};

typedef struct NESMem_ {
	// savable data...
	//
	unsigned char  m_cmem[65536]; // cpu-mem
	unsigned char  m_pmem[16384]; // ppu-mem
	unsigned char  m_smem[256];   // ppu-sprite-mem
	unsigned short m_pc;
	unsigned char  m_sp, m_x, m_y, m_a, m_sr;

	// ppu related
	unsigned char  m_mirroring; // value from NESMirroring
	unsigned short m_paddr; // ppu addr to write
	unsigned char  m_saddr; // sprite addr to write
	unsigned char  m_ppubuffer; // last ppu byte read
	unsigned char  m_scrx, m_scry; // scroll x, y

	// mapper
	unsigned char  m_mapper;
	unsigned char  m_mapperBuf[4]; // for mapper used

	unsigned char  m_oldsp, m_oldx, m_oldy, m_olda, m_oldsr; // for print only
	unsigned char  m_joystickCount; // index of button to next read
	unsigned char  m_joystickStatus; // current button states...
	unsigned char  m_breaked, m_trace, m_autoTraceCount;
	int            m_lastVerPos;
	int            m_delayms;
	unsigned long long      m_insCount;
	int            m_vsyncCount;

	// non-saving datas
	//
	// rendering data
	DWORD          m_lastFrameTime;
	unsigned int   m_display[256][256];
	unsigned char  m_debugText[256];
	unsigned long long m_opFreq[256];

	// file
	FILE           *m_fp;
	char           m_savePath[1024];
	unsigned int   m_vromFileOffset;
	unsigned short m_nPRGROM, m_nCHRROM;
	unsigned char  m_signalSave;
	unsigned char  m_signalLoad;
} NESMem;

static const int NES_SAVING_SIZE = offsetof(NESMem, m_lastFrameTime);

static NESHeader hdr;
static NESMem mem;
static EasyBitmap *hBitmap = 0;
static HWND hwnd = 0;

void writeCMem(NESMem *m, unsigned short addr_, unsigned char c);

inline void printInfo(NESMem *mem, const char *msg) {
	if (!mem->m_trace) return ;
	//HANDLE hStdout = GetStdHandle(STD_OUTPUT_HANDLE);
	//SetConsoleTextAttribute(hStdout, FOREGROUND_RED | FOREGROUND_GREEN | FOREGROUND_INTENSITY);
	printf("%s", msg);
	//SetConsoleTextAttribute(hStdout, FOREGROUND_RED | FOREGROUND_GREEN | FOREGROUND_BLUE);
}

void pputick(NESMem *m) 
{
	const unsigned int frameCycle = 16384;
	const unsigned int scanLineTotal = 260;

	unsigned char ctl = m->m_cmem[0x2000];
	unsigned short sppataddr = (ctl & (1<<3)) ? 0x1000 : 0x0000;
	unsigned short bkpataddr = (ctl & (1<<4)) ? 0x1000 : 0x0000;
	unsigned char spriteSize = (ctl & (1<<5)) ? 16 : 8;
	unsigned short ntaddr[4] = {0x2000, 0x2400, 0x2800, 0x2c00}; // name table addr
	unsigned short highcoladdr[4] = {0x23c0, 0x27c0, 0x2bc0, 0x2fc0}; // attribute table addr
	if (spriteSize == 16) sppataddr = 0x0000;
	unsigned int cycle = m->m_insCount % frameCycle;
	int oldVerPos = m->m_lastVerPos;
	if (oldVerPos < 0) {
		printf("ERROR oldVerPos < 0, pc = %04x", (int)m->m_pc);
		oldVerPos = 0;
	}
	m->m_lastVerPos = cycle * scanLineTotal / frameCycle;

	unsigned char drawed = 0;
	unsigned char spriteHit = 0;
	unsigned char bgpalette[16], sppalette[16];
	int bgcolor[16], spcolor[16];
	for (int y = oldVerPos; y < 240 && y < m->m_lastVerPos; y++) {
		unsigned char tindex = (ctl & 3); // which name table to use
		if (m->m_mirroring == NESMirroring::vertical) tindex &= 1;
		if (m->m_mirroring == NESMirroring::horizontal) tindex &= 2;
		if (!drawed) {
			drawed = 1;
			memcpy(bgpalette, &(m->m_pmem[0x3f00]), sizeof(bgpalette));
			memcpy(sppalette, &(m->m_pmem[0x3f10]), sizeof(sppalette));
			bgpalette[4] = bgpalette[8] = bgpalette[12] = bgpalette[0] = m->m_pmem[0x3f00];
			sppalette[4] = sppalette[8] = sppalette[12] = sppalette[0] = m->m_pmem[0x3f00];
			for (int i = 0; i < 16; i++) {
				bgcolor[i] = NES_COLOR[bgpalette[i] & 63];
				spcolor[i] = NES_COLOR[sppalette[i] & 63];
			}
		}
		// draw background
		short nty = (y + m->m_scry) / 8; // nty: name table y-offset
		if (nty >= 30) {
			nty -= 30; 
			if (m->m_mirroring == NESMirroring::horizontal ||
				m->m_mirroring == NESMirroring::fourscreen) tindex ^= 2; // flip nametable
		}
		short ntxstart = m->m_scrx / 8;
		short ntxend = ntxstart + 32 + (m->m_scrx % 8 ? 1 : 0);
		for (short ntx = ntxstart; ntx < ntxend; ntx++) { // ntx: name table x-offset
			int hcolindex = (nty / 4) * 8 + (ntx & 31) / 4;
			int hcolshift = ((nty / 2) & 1) * 4 + (((ntx & 31) / 2) & 1) * 2;
			if (ntx == 32) {
				if (m->m_mirroring == NESMirroring::vertical ||
				    m->m_mirroring == NESMirroring::fourscreen)
					tindex ^= 1; // flip nametable
			}
			unsigned char colhh = m->m_pmem[highcoladdr[tindex] + hcolindex];
			colhh >>= (hcolshift);
			colhh &= 3;
			colhh <<= 2;
			for (short dx = 0; dx < 8; dx++) {
				unsigned char mask = (1<<(8-dx-1));
				unsigned char pindex = m->m_pmem[ntaddr[tindex]+nty*32+(ntx & 31)];
				unsigned char coll = m->m_pmem[bkpataddr + pindex * 16 + (y + m->m_scry) % 8];
				unsigned char colh = m->m_pmem[bkpataddr + pindex * 16 + (y + m->m_scry) % 8 + 8];
				coll = (coll & mask) ? 1 : 0;
				colh = (colh & mask) ? 2 : 0;
				//unsigned char colindex = bgpalette[(colhh + coll + colh) & 15];
				//m->m_display[y][ntx * 8 + dx] = NES_COLOR[colindex & (64-1)];
				short screenx = ntx * 8 + dx - (short)m->m_scrx;
				if (screenx >= 0 && screenx < 256)
					m->m_display[y][screenx] = bgcolor[(colhh + coll + colh) & 15];
			}
		}
		// draw sprites
		for (short sp = 252; sp >= 0; sp -= 4) {
			unsigned short spy = (short)y - ((short)m->m_smem[sp] + 1);
			if (spy >= spriteSize) continue;
			unsigned char overbg = (m->m_smem[sp+2] & (1<<5)) ? 0 : 1; // 1: priority over background
			unsigned char flipx = (m->m_smem[sp+2] & (1<<6));
			unsigned char flipy = (m->m_smem[sp+2] & (1<<7)) ? 1 : 0;
			unsigned short spindex = m->m_smem[sp+1];
			if (spriteSize == 16) {
				if (spindex & 1) spindex = spindex - 1 + 256;
				if (spy >= 8) {
					spy -= 8; 
					spindex++;
				}
			}
			unsigned char colhh = ((m->m_smem[sp+2] & 0x3) << 2);
			unsigned short srcx = m->m_smem[sp+3];
			for (short dx = 0; dx < 8; dx++, srcx++) {
				if (srcx >= 256) break;
				unsigned char mask = flipx ? (1<<dx) : (1<<(8-dx-1));
				unsigned char coll = m->m_pmem[sppataddr + (spindex ^ flipy) * 16 + 
					                           (flipy ? 7-spy : spy)];
				unsigned char colh = m->m_pmem[sppataddr + (spindex ^ flipy) * 16 + 
					                           (flipy ? 7-spy : spy) + 8];
				coll = (coll & mask) ? 1 : 0;
				colh = (colh & mask) ? 2 : 0;
				//unsigned char colindex = sppalette[(colhh + coll + colh) & 15];
				if ((colhh + coll + colh) % 4) {
					//if (overbg) 
					//	m->m_display[y][srcx] = NES_COLOR[colindex & (64-1)];
					m->m_display[y][srcx] = spcolor[(colhh + coll + colh) & 15];
					if (sp == 0) spriteHit = 1;
				}
			}
		}
	}

	if (drawed) {
		if (spriteHit) {
			m->m_cmem[0x2002] |= (1<<6);
			if (m->m_trace) printf("Sprite Hit!!! y=%d", oldVerPos);
		} else
			m->m_cmem[0x2002] &= ~(1<<6); // reset sprite 0 hit
	}

	unsigned char updateUI = 0;
	if (hwnd && m->m_trace && oldVerPos < 239 && oldVerPos != m->m_lastVerPos) 
		updateUI = 1;

	// vsync signal
	if (oldVerPos < 240 && m->m_lastVerPos >= 240) {
		m->m_vsyncCount++;
		updateUI = 1;
		DWORD t = ::timeGetTime();
		if (t - m->m_lastFrameTime < 16) {
			::Sleep(16 - (t-m->m_lastFrameTime));
		}
		m->m_lastFrameTime = t;
		m->m_cmem[0x2002] &= ~(1<<6); // sprite 0 hit
		if (m->m_cmem[0x2000] & 0x80) {
			//m->m_trace = 1;
			printInfo(m, "VSYNC with NMI: ");
			if (m->m_trace) printf("ctl(0x2000)=%02x\n", ctl);
			writeCMem(m, NES_STACKBASE + m->m_sp, m->m_pc >> 8);
			m->m_sp--;
			writeCMem(m, NES_STACKBASE + m->m_sp, m->m_pc & 0xff);
			m->m_sp--;
			writeCMem(m, NES_STACKBASE + m->m_sp, m->m_sr);
		    m->m_sp--;

			//m->m_cmem[0x2000] &= ~0x80;
			m->m_pc = *(unsigned short *)&(m->m_cmem[0xfffa]);
			//m->m_delayms = -1;
			m->m_cmem[0x2002] |= 0x80;
		} else {
			m->m_cmem[0x2002] |= 0x80;
			if (m->m_trace) printf("vsync ctl(0x2000)=%02x\n", ctl);
		}
	}
	// else if (m->m_lastVerPos < 240) {
	//	m->m_cmem[0x2002] &= ~0x80;
	//}

	if (updateUI) {
		_snprintf((char *)m->m_debugText, sizeof(m->m_debugText)-1, 
				  "#ins %llum #vsync %u y %d scrx %u scry %u [2000]:%02x %02x %02x %02x %02x %02x",
				  m->m_insCount/1000000ull, m->m_vsyncCount, m->m_lastVerPos,
				  m->m_scrx, m->m_scry,
				  m->m_cmem[0x2000], m->m_cmem[0x2001], m->m_cmem[0x2002],
				  m->m_cmem[0x2003], m->m_cmem[0x2004], m->m_cmem[0x2005]);
		m->m_debugText[sizeof(m->m_debugText)-1] = 0;
		::SendMessage(hwnd, WM_PAINT, 0, 0);
	}
}

void draw(NESMem *m) {
	unsigned char ctl = m->m_cmem[0x2000];
	unsigned short bkpataddr = (ctl & (1<<4)) ? 0x0000 : 0x1000;
	unsigned short bkpaladdr = 0x3F00;
	unsigned short sppaladdr = 0x3F10;
	unsigned short taddr[4] = {0x2000, 0x2400, 0x2800, 0x2c00};
	printf("bkpalettes:[");
	for (int i = 0; i < 16; i++) {
		printf("%02x ", m->m_pmem[bkpaladdr + i]);
	}
	printf("]\n");
	for (int t = 0; t < 4; t++) {
		for (int y = 0; y < 30; y++) {
			printf("\n%02x ", y);
			for (int x = 0; x < 32; x++) {
				unsigned char pindex = m->m_pmem[taddr[t]+y*32+x];
				printf("%02x", pindex);
			}
		}
		printf("\n");
	}
	printf("\nsprites:[");
	for (int i = 0; i < 256; i+=4) {
		if (m->m_smem[i] >= 240) continue;
		printf("%02x%02x%02x%02x ", 
			   m->m_smem[i], m->m_smem[i+1], m->m_smem[i+2], m->m_smem[i+3]);
	}
	printf("]\n");
	printf("OpFreqs:");
	for (int i = 0; i < 256; i++) {
		if (m->m_opFreq[i])
		  printf("[%02x]:%llu\t", i, m->m_opFreq[i]);
	}
	printf("\n");
}

inline unsigned short mirror(unsigned short caddr) {
	if (caddr < 0x2000) caddr &= (0x0800-1);
	else if (caddr >= 0x2008 && caddr < 0x4000) {
		caddr = caddr - 0x2000 + (caddr & 7);
	}
	return caddr;
}

inline unsigned short ppmirror(NESMem *m, unsigned short paddr) {
	paddr = (paddr & (16384-1));
	if (paddr < 0x2400) return paddr;
	if (paddr < 0x3f00) {
		switch (m->m_mirroring) {
		case NESMirroring::vertical:
			return ((paddr - 0x2000) & (0x800 - 1)) + 0x2000; // mario
		case NESMirroring::horizontal:
			if (paddr < 0x2800) return ((paddr - 0x2000) & (0x400 - 1)) + 0x2000;
			else return ((paddr - 0x2800) & (0x400 - 1)) + 0x2800;
		case NESMirroring::fourscreen: // fourscreen
			return ((paddr - 0x2000) & (0x1000 - 1)) + 0x2000;
		default: // single
			return ((paddr - 0x2000) & (0x400 - 1)) + 0x2000;
		}
	}
	unsigned short d = ((paddr - 0x3f00) & 31);
	if (d % 4 == 0) return (d & 15) + 0x3f00;
	return d + 0x3f00;
}

void printState(NESMem *m) {
	if (!m->m_trace) return;
	HANDLE hStdout = GetStdHandle(STD_OUTPUT_HANDLE);
	const char *texts[] = {"S ", "X ", "Y ", "A "};
	const char bits[] = {'C', 'Z','I','D','B','=','V','N'};
	unsigned char vals[][2] = { 
		{m->m_oldsp, m->m_sp}, {m->m_oldx, m->m_x}, 
		{m->m_oldy, m->m_y}, {m->m_olda, m->m_a} };
	for (int i = 0; i < 4; i++) {
		SetConsoleTextAttribute(hStdout, FOREGROUND_RED | FOREGROUND_GREEN | FOREGROUND_BLUE);
		if (texts[i][0]) printf(texts[i]);
		if (vals[i][0] != vals[i][1])
			SetConsoleTextAttribute(hStdout, FOREGROUND_RED | FOREGROUND_GREEN | FOREGROUND_INTENSITY);
		printf("%02x ", vals[i][1]);
	}
	for (int i = 7; i >= 0; i--) {
		if ((m->m_oldsr & (1<<i)) != (m->m_sr & (1<<i)))
			SetConsoleTextAttribute(hStdout, FOREGROUND_RED | FOREGROUND_GREEN | FOREGROUND_INTENSITY);
		else 
			SetConsoleTextAttribute(hStdout, FOREGROUND_RED | FOREGROUND_GREEN | FOREGROUND_BLUE);
		printf("%c", (m->m_sr & (1<<i)) ? bits[i] : '-');
	}
	printf("(%02x) ", m->m_sr);
	SetConsoleTextAttribute(hStdout, FOREGROUND_RED | FOREGROUND_GREEN | FOREGROUND_BLUE);
	m->m_oldsp = m->m_sp;
	m->m_oldx = m->m_x;
	m->m_oldy = m->m_y;
	m->m_olda = m->m_a;
	m->m_oldsr = m->m_sr;
}

unsigned short vsyncCount = 0;

inline unsigned char readCMem(NESMem *m, unsigned short addr_) {
	unsigned short addr = mirror(addr_);
	unsigned char c = m->m_cmem[addr];
	HANDLE hStdout;
	if (addr == 0x4016) { // joystick 0
		if (m->m_joystickCount < 8)
			return (m->m_joystickStatus & (1<< (m->m_joystickCount++))) ? 0x41 : 0;
		else return 0;
	}
	else if (addr == 0x2002) { // read vsync
		mem.m_cmem[0x2002] &= ~0x80; // unset vsync (already read) 
	}
	else if (addr == 0x2007) { // read from PPU RAM
		unsigned paddr = ppmirror(m, m->m_paddr);
		if (paddr < 0x3f00) {
			c = m->m_ppubuffer;
			m->m_ppubuffer = m->m_pmem[paddr];
		} else {
			c = m->m_pmem[paddr];
			m->m_ppubuffer = m->m_pmem[paddr];
		}
		unsigned char pctrl = m->m_cmem[0x2000];
		m->m_paddr += (pctrl & (1<<2)) ? 32 : 1;
	}
	if (m->m_trace) {
		if (addr>=0x2000 && addr <= 0x2007) {
			hStdout = GetStdHandle(STD_OUTPUT_HANDLE);
			SetConsoleTextAttribute(hStdout, FOREGROUND_GREEN | FOREGROUND_INTENSITY);
		}
		printf("r[%04x]:%02x ", (int)addr_, (int)c);
		if (addr>=0x2000 && addr <= 0x2007) {
			hStdout = GetStdHandle(STD_OUTPUT_HANDLE);
			SetConsoleTextAttribute(hStdout, FOREGROUND_RED | FOREGROUND_GREEN | FOREGROUND_BLUE);
		}
	}
	return c;
}

unsigned short readCMem2(NESMem *m, unsigned short addr_) {
	unsigned short addr = mirror(addr_);
	unsigned short c = *(unsigned short *)&(m->m_cmem[addr]);
	if (m->m_trace) {
		printf("r16[%04x]:%04x ", (int)addr_, (int)c);
	}
	return c;
}

inline void traceCMem(NESMem *m, unsigned short addr_)
{
	if (m->m_trace) {
		HANDLE hStdout;
		DWORD color = 0;
		unsigned short addr = mirror(addr_);
		if (addr>=0x2000 && addr <= 0x2007 || addr == 0x4014) {
			hStdout = GetStdHandle(STD_OUTPUT_HANDLE);
			color = FOREGROUND_GREEN | FOREGROUND_INTENSITY;
			SetConsoleTextAttribute(hStdout, color);
		}
		printf("[%04x]:%02x ", (int)addr_, (int)m->m_cmem[addr]);
	}
}

inline void writeCMem(NESMem *m, unsigned short addr_, unsigned char c) {
	HANDLE hStdout;
	DWORD color = 0;
	unsigned short addr = mirror(addr_);
	
	if (m->m_trace) {
		if (addr>=0x2000 && addr <= 0x2007 || addr == 0x4014) {
			hStdout = GetStdHandle(STD_OUTPUT_HANDLE);
			color = FOREGROUND_GREEN | FOREGROUND_INTENSITY;
			SetConsoleTextAttribute(hStdout, color);
		}
		printf("w[%04x]:%02x ", (int)addr_, (int)c);
	}

	if (addr < 0x0800) goto write_ram; // normal RAM

	if (addr == 0x4016) { 
		if (!c && m->m_cmem[addr]) {
			m->m_joystickCount = 0; // reset joystick read index
		}
	}
	if (addr == 0x2005) {
		m->m_scrx = m->m_scry;
		m->m_scry = c;
	} else if (addr == 0x2006) {
		m->m_paddr = (m->m_paddr << 8 | c);
		m->m_scrx = m->m_scry = 0;
		if (m->m_trace) printf("paddr %02x ", m->m_paddr);
	} else if (addr == 0x2007) {
		m->m_pmem[ppmirror(m, m->m_paddr)] = c;
		if (m->m_trace) printf("pw[%04x] ", m->m_paddr);
		unsigned char pctrl = m->m_cmem[0x2000];
		m->m_paddr += (pctrl & (1<<2)) ? 32 : 1;
	} else if (addr == 0x2003) {
		m->m_saddr = c;
	} else if (addr == 0x2004) {
		if (m->m_trace) printf("sw[%02x] ", m->m_saddr);
		//m->m_delayms = -1;
		m->m_smem[m->m_saddr] = c;
	} else if (addr == 0x4014) {
		unsigned short dmaaddr = mirror(c * (unsigned short)256);
		unsigned char trace = m->m_trace;
		if (trace) 
			printf("\nsprites from [%04x]:[", dmaaddr);
		for (int i = 0; i < 256; i+=4) {
			*(unsigned int *)&(m->m_smem[i]) = *(unsigned int *)&(m->m_cmem[dmaaddr + i]);
			if (trace && m->m_smem[i] < 240) {
				printf(" %08x", *(unsigned int *)&(m->m_smem[i]));
			}
		}
		if (trace) printf("]");
	} else if (addr >= 0x8000) { // mappers
		if (m->m_mapper != 3 && m->m_mapper != 4)
			printf("mapper(%d): w[%04x]:%02x\n", m->m_mapper, (int)addr_, (int)c);
		if (m->m_mapper == 1) {
			unsigned short ppuaddr = 0;
			unsigned short ppuswapsize = 0;		
		} else if (m->m_mapper == 3) {
			const int VROMSIZE = 8192;
			fseek(m->m_fp, m->m_vromFileOffset + (c & 3) * VROMSIZE, SEEK_SET);
			fread(m->m_pmem, VROMSIZE, 1, m->m_fp);
		} else if (m->m_mapper == 4) {
			unsigned short ppuaddr = 0;
			unsigned short ppuswapsize = 0;
			if (addr >= 0x8000 && addr <= 0x9fff) addr = (addr % 2) + 0x8000;
			else if (addr >= 0xa000 && addr <= 0xbfff) addr = (addr %2 ) + 0xa000;
			switch (addr) {
			case 0xa000:
				m->m_mirroring = (c & 1) ? NESMirroring::horizontal : NESMirroring::vertical;
				break;
			case 0x8000: 
				m->m_mapperBuf[0] = c; 
				break;
			case 0x8001: 
				if ((m->m_mapperBuf[0] & 7) < 6) { // CHR-ROM
					if ((m->m_mapperBuf[0] & 7) < 2) c &= 0xfe;
					fseek(m->m_fp, m->m_vromFileOffset + c * 1024, SEEK_SET);
				} else { // PRG-ROM
					c = c & 0x3f;
					fseek(m->m_fp, sizeof(NESHeader) + c * 8192, SEEK_SET);
				}
				switch (m->m_mapperBuf[0] & 7) {
				case 0:
					ppuswapsize = 2048;
					if (m->m_mapperBuf[0] & 0x80) ppuaddr = 0x1000;
					else ppuaddr = 0x0000;
					break;
				case 1:
					ppuswapsize = 2048;
					if (m->m_mapperBuf[0] & 0x80) ppuaddr = 0x1800;
					else ppuaddr = 0x0800;
					break;
				case 2:
					ppuswapsize = 1024;
					if (m->m_mapperBuf[0] & 0x80) ppuaddr = 0x0000;
					else ppuaddr = 0x1000;
					break;
				case 3:
					ppuswapsize = 1024;
					if (m->m_mapperBuf[0] & 0x80) ppuaddr = 0x0400;
					else ppuaddr = 0x1400;
					break;
				case 4:
					ppuswapsize = 1024;
					if (m->m_mapperBuf[0] & 0x80) ppuaddr = 0x0800;
					else ppuaddr = 0x1800;
					break;
				case 5:
					ppuswapsize = 1024;
					if (m->m_mapperBuf[0] & 0x80) ppuaddr = 0x0c00;
					else ppuaddr = 0x1c00;
					break;
				case 6:
					if (m->m_mapperBuf[0] & 0x40) {
						fread(m->m_cmem + 0xc000, 8192, 1, m->m_fp);
						fseek(m->m_fp, sizeof(NESHeader) + (m->m_nPRGROM * 2 - 2) * 8192, SEEK_SET);
						fread(m->m_cmem + 0x8000, 8192, 1, m->m_fp);
					} else {
						fread(m->m_cmem + 0x8000, 8192, 1, m->m_fp);
						fseek(m->m_fp, sizeof(NESHeader) + (m->m_nPRGROM * 2 - 2) * 8192, SEEK_SET);
						fread(m->m_cmem + 0xc000, 8192, 1, m->m_fp);
					}
					break;
				case 7:
					fread(m->m_cmem + 0xa000, 8192, 1, m->m_fp);
					break;
				}
				if (ppuswapsize) {
					fread(m->m_pmem + ppuaddr, 1, ppuswapsize, m->m_fp);
				}
				break;
			}
		} else if (m->m_mapper == 33 || m->m_mapper == 48) {
			switch(addr) {
			case 0x8000:
				if (c & 0x40) m->m_mirroring = NESMirroring::horizontal;
				else m->m_mirroring = NESMirroring::vertical;
				fseek(m->m_fp, sizeof(NESHeader) + (c & 0x3f) * 8192, SEEK_SET);
				fread(m->m_cmem + 0x8000, 8192, 1, m->m_fp);
				break;
			case 0x8001:
				fseek(m->m_fp, sizeof(NESHeader) + (c & 0x3f) * 8192, SEEK_SET);
				fread(m->m_cmem + 0xa000, 8192, 1, m->m_fp);
				break;
			case 0x8002:
				fseek(m->m_fp, m->m_vromFileOffset + c * 1024, SEEK_SET);
				fread(m->m_pmem, 2048, 1, m->m_fp);
				break;
			case 0x8003:
				fseek(m->m_fp, m->m_vromFileOffset + c * 1024, SEEK_SET);
				fread(m->m_pmem + 0x0800, 2048, 1, m->m_fp);
				break;
			case 0xa000:
				fseek(m->m_fp, m->m_vromFileOffset + c * 1024, SEEK_SET);
				fread(m->m_pmem + 0x1000, 1024, 1, m->m_fp);
				break;
			case 0xa001:
				fseek(m->m_fp, m->m_vromFileOffset + c * 1024, SEEK_SET);
				fread(m->m_pmem + 0x1400, 1024, 1, m->m_fp);
				break;
			case 0xa002:
				fseek(m->m_fp, m->m_vromFileOffset + c * 1024, SEEK_SET);
				fread(m->m_pmem + 0x1800, 1024, 1, m->m_fp);
				break;
			case 0xa003:
				fseek(m->m_fp, m->m_vromFileOffset + c * 1024, SEEK_SET);
				fread(m->m_pmem + 0x1c00, 1024, 1, m->m_fp);
				break;
			}
		}
	}

	if (color) {
		hStdout = GetStdHandle(STD_OUTPUT_HANDLE);
		SetConsoleTextAttribute(hStdout, FOREGROUND_RED | FOREGROUND_GREEN | FOREGROUND_BLUE);
	}

	if (addr >= 0x8000) return; // can't write ROM

write_ram:
	m->m_cmem[addr] = c;

	//if (addr == 0x2000) {
	//	printf("Waiting for VSYNC...\n");
	//	if (c & 0x80) m->m_trace = 0;
	//}
}

void reset(NESMem *m) {
	unsigned char h = readCMem(m, 0xFFFD), l = readCMem(m, 0xFFFC);
	m->m_pc = ((unsigned short)h << 8) | l;
	//m->m_pc = 0xc000; // test purpose;
	m->m_sp = 0xff;
	m->m_x = m->m_y = m->m_a= 0;
	m->m_sr = 0x24;
}

#define NES_PC(m) ((m)->m_sr & 1)
#define NES_PZ(m) ((m)->m_sr & 2)
#define NES_PI(m) ((m)->m_sr & 4)
#define NES_PD(m) ((m)->m_sr & 8)
#define NES_PB(m) ((m)->m_sr & 0x10)
#define NES_PV(m) ((m)->m_sr & 0x40)
#define NES_PN(m) ((m)->m_sr & 0x80)

#define NES_SPC(m) ((m)->m_sr |= 1)
#define NES_SPZ(m) ((m)->m_sr |= 2)
#define NES_SPI(m) ((m)->m_sr |= 4)
#define NES_SPD(m) ((m)->m_sr |= 8)
#define NES_SPB(m) ((m)->m_sr |= 0x10)
#define NES_SPV(m) ((m)->m_sr |= 0x40)
#define NES_SPN(m) ((m)->m_sr |= 0x80)

#define NES_CPC(m) ((m)->m_sr &= ~1)
#define NES_CPZ(m) ((m)->m_sr &= ~2)
#define NES_CPI(m) ((m)->m_sr &= ~4)
#define NES_CPD(m) ((m)->m_sr &= ~8)
#define NES_CPB(m) ((m)->m_sr &= ~0x10)
#define NES_CPV(m) ((m)->m_sr &= ~0x40)
#define NES_CPN(m) ((m)->m_sr &= ~0x80)

#define NES_SetBit(dst, index, src) if (src) (dst) |= (1<<(index)); else (dst) &= ~(1<<(index))

#define NES_SetNZ(dst) \
	NES_SetBit(mem->m_sr, 7, (dst) & 0x80); \
	NES_SetBit(mem->m_sr, 1, !(dst)); \

#define Load_XY(dst, opImm, opZP, opZPXY, opABS, opABXY, m_xy) \
	case opImm:\
		mem->m_pc+=2;\
		(dst) = op[0]; NES_SetNZ(dst); break;\
	case opZP:\
		mem->m_pc+=2; tc = readCMem(mem, op[0]); \
		(dst) = tc; NES_SetNZ(dst); break;\
	case opZPXY:\
		mem->m_pc+=2; tc = readCMem(mem, op[0] + mem->m_xy); \
		(dst) = tc; NES_SetNZ(dst);break;\
	case opABS:\
		mem->m_pc+=3; tc = readCMem(mem, *(unsigned short*)&(op[0])); \
		(dst) = tc; NES_SetNZ(dst);break;\
	case opABXY:\
		mem->m_pc+=3; tc = readCMem(mem, *(unsigned short*)&(op[0]) + mem->m_xy); \
		(dst) = tc; NES_SetNZ(dst);break;

#define Gen_OPS(dst, op_, src, opImm, opZP, opZPX, opABS, opABX, opABY, opInX, opInY) \
	case opImm:\
		mem->m_pc+=2;\
		(dst) = (src) op_ op[0]; NES_SetNZ(dst); break;\
	case opZP:\
		mem->m_pc+=2; tc = readCMem(mem, op[0]); \
		(dst) = (src) op_ tc; NES_SetNZ(dst); break;\
	case opZPX:\
		mem->m_pc+=2; tc = readCMem(mem, op[0] + mem->m_x); \
		(dst) = (src) op_ tc; NES_SetNZ(dst);break;\
	case opABS:\
		mem->m_pc+=3; tc = readCMem(mem, *(unsigned short*)&(op[0])); \
		(dst) = (src) op_ tc; NES_SetNZ(dst);break;\
	case opABX:\
		mem->m_pc+=3; tc = readCMem(mem, *(unsigned short*)&(op[0]) + mem->m_x); \
		(dst) = (src) op_ tc; NES_SetNZ(dst);break;\
	case opABY:\
		mem->m_pc+=3; tc = readCMem(mem, *(unsigned short*)&(op[0]) + mem->m_y); \
		(dst) = (src) op_ tc; NES_SetNZ(dst);break;\
	case opInX:\
		mem->m_pc+=2; ts = readCMem2(mem, (unsigned char)(op[0] + mem->m_x)); \
		tc = readCMem(mem, ts); \
		(dst) = (src) op_ tc;\
		NES_SetNZ(dst);break;\
	case opInY:\
		mem->m_pc+=2;\
		ts = readCMem2(mem, op[0]); \
		(dst) = (src) op_ readCMem(mem, ts + mem->m_y);\
		NES_SetNZ(dst);break

#define NES_ASL(dst) \
		NES_SetBit(mem->m_sr, 0, (dst) & 0x80); \
		(dst) <<= 1;\
		NES_SetBit(mem->m_sr, 7, (dst) & 0x80);\
		NES_SetBit(mem->m_sr, 1, !(dst))\

#define NES_ASR(dst) \
		NES_SetBit(mem->m_sr, 0, (dst) & 0x01); \
		(dst) >>= 1;\
		NES_SetBit(mem->m_sr, 7, 0);\
		NES_SetBit(mem->m_sr, 1, !(dst))\

#define NES_ARL(dst) \
	    { unsigned char tc2_ = (dst); \
		  (dst) <<= 1; \
		  (dst) |= (NES_PC(mem)); \
		  NES_SetBit(mem->m_sr, 0, (tc2_) & 0x80); \
		  NES_SetBit(mem->m_sr, 7, (dst) & 0x80);\
		  NES_SetBit(mem->m_sr, 1, !(dst)); }

#define NES_ARR(dst) \
		{ unsigned char tc2_ = (dst); \
		  (dst) >>= 1; (dst) &= 0x7f; \
		  (dst) |= ((NES_PC(mem)) << 7); \
		  NES_SetBit(mem->m_sr, 0, (tc2_) & 0x01); \
		  NES_SetBit(mem->m_sr, 7, (dst) & 0x80);\
		  NES_SetBit(mem->m_sr, 1, !(dst)); }

#define NES_SUBC(src) \
	    { short t = (short)mem->m_a - (short)(src) - (NES_PC(mem) ? 0 : 1);\
		  short st = (short)(signed char)mem->m_a - (short)(signed char)(src) - (NES_PC(mem) ? 0 : 1);\
		  if (st < -128 || st > 127) NES_SPV(mem); else NES_CPV(mem); \
          if (t >= 0) NES_SPC(mem); else NES_CPC(mem);\
		  mem->m_a = t & 0xff; \
		  NES_SetNZ(mem->m_a); }

#define NES_ADDC(src) \
		{ short t = (short)mem->m_a + (short)(src) + (NES_PC(mem) ? 1 : 0);\
		  short st = (short)(signed char)mem->m_a + (short)(signed char)(src) + (NES_PC(mem) ? 1 : 0);\
		  if (st < -128 || st > 127) NES_SPV(mem); else NES_CPV(mem); \
		  mem->m_a = t & 0xff; \
          NES_SetNZ(mem->m_a); \
		  if (t > 255) NES_SPC(mem); else NES_CPC(mem); }

void dumpStack(NESMem *mem) {
	//if (!mem->m_trace) return;
	//printf("stacks:[");
	//for (int i = 0; i < mem->m_nestedCalls; i++) {
	//	printf ("%04x ", mem->m_retStack[i]);
	//}
	//printf("]");
	////mem->m_delayms = -1;
}

int execute(NESMem *mem) {
	unsigned char ins = mem->m_cmem[mem->m_pc];
	unsigned char op[2] = { mem->m_cmem[mem->m_pc+1], mem->m_cmem[mem->m_pc+2] };
	unsigned short ts;
	unsigned char tc;
	if (mem->m_insCount == 252) {
		ts = 0;
	}
	// auto trace
	if (sizeof(AutoTraceOff) / sizeof(AutoTraceOff[0]) > 1)
	{   // auto trace on/off
		unsigned char autoOff = 0;
		for (int i = 0; i < sizeof(AutoTraceOff) / sizeof(AutoTraceOff[0]); i++) {
			if (AutoTraceOff[i] == mem->m_pc) autoOff = 1;
		}
		if (autoOff) {
			if (!mem->m_autoTraceCount) mem->m_autoTraceCount = 32;
			if (mem->m_autoTraceCount > 1) {
				mem->m_autoTraceCount--;
				if (mem->m_autoTraceCount == 1) {
					mem->m_trace = 1;
					printInfo(mem, "AUTO TRACE OFF\n");
					mem->m_trace = 0; 
				}
			}
		} else { 
			if (mem->m_autoTraceCount) {
				mem->m_trace = 1;
				printInfo(mem, "AUTO TRACE ON\n");
			}
			mem->m_autoTraceCount = 0;
		}
	}
	// trace info
	if (mem->m_trace) {
		printf("[%04x] %02x %02x %02x  ", mem->m_pc, ins, op[0], op[1]);
		printState(mem);
	}
	mem->m_opFreq[ins]++;
	switch(ins) {
		// assign A
		// AND
		Gen_OPS(mem->m_a, &, mem->m_a, 0x29, 0x25, 0x35, 0x2d, 0x3d, 0x39, 0x21, 0x31);
		// OR
		Gen_OPS(mem->m_a, |, mem->m_a, 0x09, 0x05, 0x15, 0x0d, 0x1d, 0x19, 0x01, 0x11);
		// XOR
		Gen_OPS(mem->m_a, ^, mem->m_a, 0x49, 0x45, 0x55, 0x4d, 0x5d, 0x59, 0x41, 0x51);
		// LDA
	    Gen_OPS(mem->m_a, +, 0,        0xa9, 0xa5, 0xb5, 0xad, 0xbd, 0xb9, 0xa1, 0xb1);

		// LoadX
		Load_XY(mem->m_x, 0xa2, 0xa6, 0xb6, 0xae, 0xbe, m_y);
		// LoadY
		Load_XY(mem->m_y, 0xa0, 0xa4, 0xb4, 0xac, 0xbc, m_x);

		// transfers
	case 0xaa: // a->x
		mem->m_pc+=1;
		mem->m_x = mem->m_a;
		NES_SetNZ(mem->m_x);
		break;
	case 0xa8: // a->y
		mem->m_pc+=1;
		mem->m_y = mem->m_a;
		NES_SetNZ(mem->m_y);
		break;
	case 0xba: // sp->x
		mem->m_pc+=1;
		mem->m_x = mem->m_sp;
		NES_SetNZ(mem->m_x);
		break;
	case 0x8a: // x->a
		mem->m_pc+=1;
		mem->m_a = mem->m_x;
		NES_SetNZ(mem->m_a);
		break;
	case 0x9a: // x->sp
		mem->m_pc+=1;
		mem->m_sp = mem->m_x;
		break;
	case 0x98: // y->a
		mem->m_pc+=1;
		mem->m_a = mem->m_y;
		NES_SetNZ(mem->m_a);
		break;

		// additions
	case 0x69: // addc imm
		mem->m_pc+=2;
		tc = op[0];
		NES_ADDC(tc); break;
	case 0x65: // addc zp
		mem->m_pc+=2;
		tc = readCMem(mem, op[0]);
		NES_ADDC(tc); break;
	case 0x75: // addc zpx
		mem->m_pc+=2;
		tc = readCMem(mem, op[0] + mem->m_x);
		NES_ADDC(tc); break;
	case 0x6d: // addc abs
		mem->m_pc+=3;
		tc = readCMem(mem, *(unsigned short*)&(op[0]));
		NES_ADDC(tc); break;
	case 0x7d: // addc abx
		mem->m_pc+=3;
		tc = readCMem(mem, *(unsigned short*)&(op[0]) + mem->m_x);
		NES_ADDC(tc); break;
	case 0x79: // addc aby
		mem->m_pc+=3;
		tc = readCMem(mem, *(unsigned short*)&(op[0]) + mem->m_y);
		NES_ADDC(tc); break;
	case 0x61: // addc inx
		mem->m_pc+=2; ts = readCMem2(mem, (unsigned char)(op[0] + mem->m_x)); 
		tc = readCMem(mem, ts); 
		NES_ADDC(tc); break;
	case 0x71: // addc iny
		mem->m_pc+=2;
		ts = readCMem2(mem, op[0]); 
		tc = readCMem(mem, ts + mem->m_y);
		NES_ADDC(tc); break;

		// subtracts
	case 0xe9: // subc imm
		mem->m_pc+=2;
		tc = op[0];
		NES_SUBC(tc); break;
	case 0xe5: // subc zp
		mem->m_pc+=2;
		tc = readCMem(mem, op[0]);
		NES_SUBC(tc); break;
	case 0xf5: // subc zpx
		mem->m_pc+=2;
		tc = readCMem(mem, op[0] + mem->m_x);
		NES_SUBC(tc); break;
	case 0xed: // subc abs
		mem->m_pc+=3;
		tc = readCMem(mem, *(unsigned short*)&(op[0]));
		NES_SUBC(tc); break;
	case 0xfd: // subc abx
		mem->m_pc+=3;
		tc = readCMem(mem, *(unsigned short*)&(op[0]) + mem->m_x);
		NES_SUBC(tc); break;
	case 0xf9: // subc aby
		mem->m_pc+=3;
		tc = readCMem(mem, *(unsigned short*)&(op[0]) + mem->m_y);
		NES_SUBC(tc); break;
	case 0xe1: // subc inx
		mem->m_pc+=2; ts = readCMem2(mem, (unsigned char)(op[0] + mem->m_x)); 
		tc = readCMem(mem, ts); 
		NES_SUBC(tc); break;
	case 0xf1: // subc iny
		mem->m_pc+=2;
		ts = readCMem2(mem, op[0]); 
		tc = readCMem(mem, ts + mem->m_y);
		NES_SUBC(tc); break;

		// compares
	case 0xc9: // cmp imm
		mem->m_pc+=2;
		tc = op[0];
		if (mem->m_a >= tc) NES_SPC(mem); else NES_CPC(mem);
		NES_SetNZ(mem->m_a - tc);
		break;
	case 0xc5: // cmp zp
		mem->m_pc+=2;
		tc = readCMem(mem, op[0]);
		if (mem->m_a >= tc) NES_SPC(mem); else NES_CPC(mem);
		NES_SetNZ(mem->m_a - tc);
		break;
	case 0xd5: // cmp zpx
		mem->m_pc+=2;
		tc = readCMem(mem, op[0] + mem->m_x);
		if (mem->m_a >= tc) NES_SPC(mem); else NES_CPC(mem);
		NES_SetNZ(mem->m_a - tc);
		break;
	case 0xcd: // cmp abs
		mem->m_pc+=3;
		tc = readCMem(mem, *(unsigned short*)&(op[0]));
		if (mem->m_a >= tc) NES_SPC(mem); else NES_CPC(mem);
		NES_SetNZ(mem->m_a - tc);
		break;
	case 0xdd: // cmp abx
		mem->m_pc+=3;
		tc = readCMem(mem, *(unsigned short*)&(op[0]) + mem->m_x);
		if (mem->m_a >= tc) NES_SPC(mem); else NES_CPC(mem);
		NES_SetNZ(mem->m_a - tc);
		break;
	case 0xd9: // cmp aby
		mem->m_pc+=3;
		tc = readCMem(mem, *(unsigned short*)&(op[0]) + mem->m_y);
		if (mem->m_a >= tc) NES_SPC(mem); else NES_CPC(mem);
		NES_SetNZ(mem->m_a - tc);
		break;
	case 0xc1: // cmp inx
		mem->m_pc+=2; ts = readCMem2(mem, (unsigned char)(op[0] + mem->m_x)); 
		tc = readCMem(mem, ts); 
		if (mem->m_a >= tc) NES_SPC(mem); else NES_CPC(mem);
		NES_SetNZ(mem->m_a - tc);
		break;
	case 0xd1: // cmp iny
		mem->m_pc+=2;
		ts = readCMem2(mem, op[0]); 
		tc = readCMem(mem, ts + mem->m_y);
		if (mem->m_a >= tc) NES_SPC(mem); else NES_CPC(mem);
		NES_SetNZ(mem->m_a - tc);
		break;

	case 0xe0: // cmpx imm
		mem->m_pc+=2;
		tc = op[0];
		if (mem->m_x >= tc) NES_SPC(mem); else NES_CPC(mem);
		NES_SetNZ(mem->m_x - tc);
		break;
	case 0xe4: // cmpx zp
		mem->m_pc+=2;
		tc = readCMem(mem, op[0]);
		if (mem->m_x >= tc) NES_SPC(mem); else NES_CPC(mem);
		NES_SetNZ(mem->m_x - tc);
		break;
	case 0xec: // cmpx abs
		mem->m_pc+=3;
		tc = readCMem(mem, *(unsigned short*)&(op[0]));
		if (mem->m_x >= tc) NES_SPC(mem); else NES_CPC(mem);
		NES_SetNZ(mem->m_x - tc);
		break;

	case 0xc0: // cmpy imm
		mem->m_pc+=2;
		tc = op[0];
		if (mem->m_y >= tc) NES_SPC(mem); else NES_CPC(mem);
		NES_SetNZ(mem->m_y - tc);
		break;
	case 0xc4: // cmpy zp
		mem->m_pc+=2;
		tc = readCMem(mem, op[0]);
		if (mem->m_y >= tc) NES_SPC(mem); else NES_CPC(mem);
		NES_SetNZ(mem->m_y - tc);
		break;
	case 0xcc: // cmpy abs
		mem->m_pc+=3;
		tc = readCMem(mem, *(unsigned short*)&(op[0]));
		if (mem->m_y >= tc) NES_SPC(mem); else NES_CPC(mem);
		NES_SetNZ(mem->m_y - tc);
		break;
		
	case 0x24: // test bits zp
		mem->m_pc+=2;
		tc = readCMem(mem, op[0]);
		if (tc & 0x80) NES_SPN(mem); else NES_CPN(mem);
		if (tc & 0x40) NES_SPV(mem); else NES_CPV(mem);
		tc &= mem->m_a;
		if (!tc) NES_SPZ(mem); else NES_CPZ(mem);
		break;
	case 0x2c: // test bits abs
		mem->m_pc+=3;
		tc = readCMem(mem, *(unsigned short*)&(op[0]));
		if (tc & 0x80) NES_SPN(mem); else NES_CPN(mem);
		if (tc & 0x40) NES_SPV(mem); else NES_CPV(mem);
		tc &= mem->m_a;
		if (!tc) NES_SPZ(mem); else NES_CPZ(mem);
		break;

		// save A
	case 0x85:
		mem->m_pc+=2; writeCMem(mem, op[0], mem->m_a); break;
	case 0x95:
		mem->m_pc+=2; writeCMem(mem, op[0] + mem->m_x, mem->m_a); break;
	case 0x8d:
		mem->m_pc+=3; writeCMem(mem, *(unsigned short*)&(op[0]), mem->m_a); break;
	case 0x9d:
		mem->m_pc+=3; writeCMem(mem, *(unsigned short*)&(op[0]) + mem->m_x, mem->m_a); break; 
	case 0x99:
		mem->m_pc+=3; writeCMem(mem, *(unsigned short*)&(op[0]) + mem->m_y, mem->m_a); break;
	case 0x81:
		mem->m_pc+=2; ts = readCMem2(mem, (unsigned char)(op[0] + mem->m_x)); 
		writeCMem(mem, ts, mem->m_a); break;
	case 0x91:
		mem->m_pc+=2;
		ts = readCMem2(mem, op[0]); 
		writeCMem(mem, ts + mem->m_y, mem->m_a); break;

		// save X
	case 0x86:
		mem->m_pc+=2; writeCMem(mem, op[0], mem->m_x); break;
	case 0x96:
		mem->m_pc+=2; writeCMem(mem, op[0] + mem->m_y, mem->m_x); break;
	case 0x8e:
		mem->m_pc+=3; writeCMem(mem, *(unsigned short*)&(op[0]), mem->m_x); break;

		// save Y
	case 0x84:
		mem->m_pc+=2; writeCMem(mem, op[0], mem->m_y); break;
	case 0x94:
		mem->m_pc+=2; writeCMem(mem, op[0] + mem->m_x, mem->m_y); break;
	case 0x8c:
		mem->m_pc+=3; writeCMem(mem, *(unsigned short*)&(op[0]), mem->m_y); break;


	case 0x0a: // asl
		printInfo(mem, "ASL ");
		mem->m_pc++;
		NES_ASL(mem->m_a);
		break;
	case 0x06: // asl, zero page
		printInfo(mem, "ASLz ");
		mem->m_pc+=2;
		NES_ASL(mem->m_cmem[op[0]]);
		traceCMem(mem, op[0]);
		break;
	case 0x16: // asl, ZPX
		printInfo(mem, "ASLzpx ");
		mem->m_pc+=2;
		NES_ASL(mem->m_cmem[(unsigned char)(op[0] + mem->m_x)]);
		traceCMem(mem, (unsigned char)(op[0] + mem->m_x));
		break;
	case 0x0e: // asl, ABS
		printInfo(mem, "ASLabs ");
		mem->m_pc+=3;
		NES_ASL(mem->m_cmem[*(unsigned short*)&(op[0])]);
		traceCMem(mem, *(unsigned short*)&(op[0]));
		break;
	case 0x1e: // asl, ABX
		printInfo(mem, "ASLabx ");
		mem->m_pc+=3;
		NES_ASL(mem->m_cmem[*(unsigned short*)&(op[0]) + mem->m_x]);
		traceCMem(mem, *(unsigned short*)&(op[0]) + mem->m_x);
		break;

	case 0x4a: // asr
		printInfo(mem, "ASR ");
		mem->m_pc++;
		NES_ASR(mem->m_a);
		break;
	case 0x46: // asr, zero page
		printInfo(mem, "ASRz ");
		mem->m_pc+=2;
		NES_ASR(mem->m_cmem[op[0]]);
		traceCMem(mem, op[0]);
		break;
	case 0x56: // asr, ZPX
		printInfo(mem, "ASRzpx ");
		mem->m_pc+=2;
		NES_ASR(mem->m_cmem[(unsigned char)(op[0] + mem->m_x)]);
		traceCMem(mem, (unsigned char)(op[0] + mem->m_x));
		break;
	case 0x4e: // asr, ABS
		printInfo(mem, "ASRabs ");
		mem->m_pc+=3;
		NES_ASR(mem->m_cmem[*(unsigned short*)&(op[0])]);
		traceCMem(mem, *(unsigned short*)&(op[0]));
		break;
	case 0x5e: // asr, ABX
		printInfo(mem, "ASRabx ");
		mem->m_pc+=3;
		NES_ASR(mem->m_cmem[*(unsigned short*)&(op[0]) + mem->m_x]);
		traceCMem(mem, *(unsigned short*)&(op[0]) + mem->m_x);
		break;

	case 0x2a: // arl
		printInfo(mem, "ROL ");
		mem->m_pc++;
		NES_ARL(mem->m_a);
		break;
	case 0x26: // arl, zero page
		printInfo(mem, "ROLz ");
		mem->m_pc+=2;
		NES_ARL(mem->m_cmem[op[0]]);
		traceCMem(mem, op[0]);
		break;
	case 0x36: // arl, ZPX
		printInfo(mem, "ROLzpx ");
		mem->m_pc+=2;
		NES_ARL(mem->m_cmem[(unsigned char)(op[0] + mem->m_x)]);
		traceCMem(mem, (unsigned char)(op[0] + mem->m_x));
		break;
	case 0x2e: // arl, ABS
		printInfo(mem, "ROLabs ");
		mem->m_pc+=3;
		NES_ARL(mem->m_cmem[*(unsigned short*)&(op[0])]);
		traceCMem(mem, *(unsigned short*)&(op[0]));
		break;
	case 0x3e: // arl, ABX
		printInfo(mem, "ROLabx ");
		mem->m_pc+=3;
		NES_ARL(mem->m_cmem[*(unsigned short*)&(op[0]) + mem->m_x]);
		traceCMem(mem, *(unsigned short*)&(op[0]) + mem->m_x);
		break;

	case 0x6a: // arr
		printInfo(mem, "ROR ");
		mem->m_pc++;
		NES_ARR(mem->m_a);
		break;
	case 0x66: // arr, zero page
		printInfo(mem, "RORz ");
		mem->m_pc+=2;
		tc = readCMem(mem, op[0]);
		NES_ARR(tc);
		writeCMem(mem, op[0], tc);
		break;
	case 0x76: // arr, ZPX
		printInfo(mem, "RoRzpx ");
		mem->m_pc+=2;
		tc = readCMem(mem, (unsigned char)(op[0] + mem->m_x));
		NES_ARR(tc);
		writeCMem(mem, (unsigned char)(op[0] + mem->m_x), tc);
		break;
	case 0x6e: // arr, ABS
		printInfo(mem, "RoRab ");
		mem->m_pc+=3;
		tc = readCMem(mem, *(unsigned short*)&(op[0]));
		NES_ARR(tc);
		writeCMem(mem, *(unsigned short*)&(op[0]), tc);
		break;
	case 0x7e: // arr, ABX
		printInfo(mem, "RoRabx ");
		mem->m_pc+=3;
		tc = readCMem(mem, *(unsigned short*)&(op[0]) + mem->m_x);
		NES_ARR(tc);
		writeCMem(mem, *(unsigned short*)&(op[0]) + mem->m_x, tc);
		break;

		// branches
	case 0x90: // bcc
		printInfo(mem, "BNC ");
		if (!NES_PC(mem)) mem->m_pc += (signed char)op[0];
		mem->m_pc += 2;
		break;
	case 0xb0: // bcs
		printInfo(mem, "BC ");
		if (NES_PC(mem)) mem->m_pc += (signed char)op[0];
		mem->m_pc += 2;
		break;
	case 0xf0: // 
		printInfo(mem, "BZ ");
		if (NES_PZ(mem)) mem->m_pc += (signed char)op[0];
		mem->m_pc += 2;
		break;
	case 0x30: // bmi
		printInfo(mem, "BN ");
		if (NES_PN(mem)) mem->m_pc += (signed char)op[0];
		mem->m_pc += 2;
		break;
	case 0xd0: // bne
		printInfo(mem, "BNZ ");
		if (!NES_PZ(mem)) mem->m_pc += (signed char)op[0];
		mem->m_pc += 2;
		break;
	case 0x10: // bpl
		printInfo(mem, "BNN ");
		if (!NES_PN(mem)) mem->m_pc += (signed char)op[0];
		mem->m_pc += 2;
		break;
	case 0x50: // bmi
		printInfo(mem, "BNV ");
		if (!NES_PV(mem)) mem->m_pc += (signed char)op[0];
		mem->m_pc += 2;
		break;
	case 0x70: // bvs
		printInfo(mem, "BV ");
		if (NES_PV(mem)) mem->m_pc += (signed char)op[0];
		mem->m_pc += 2;
		break;


	case 0x18: // clc
		NES_CPC(mem); mem->m_pc++; break;
	case 0xd8: // cld
		NES_CPD(mem); mem->m_pc++; break;
	case 0x58: // cli
		NES_CPI(mem); mem->m_pc++; break;
	case 0xb8: 
		NES_CPV(mem); mem->m_pc++; break;


	case 0xca: // dex
		mem->m_x--;
		NES_SetNZ(mem->m_x);
		mem->m_pc++; break;
	case 0x88: // dey
		mem->m_y--;
		NES_SetNZ(mem->m_y);
		mem->m_pc++; break;
	case 0xe8: // inx
		mem->m_x++;
		NES_SetNZ(mem->m_x);
		mem->m_pc++; break;
	case 0xc8: // iny
		mem->m_y++;
		NES_SetNZ(mem->m_y);
		mem->m_pc++; break;
	case 0xe6: // inc mem
		printInfo(mem, "INCM ");
		mem->m_pc+=2;
		tc = readCMem(mem, op[0]);
		tc++;
		writeCMem(mem, op[0], tc);
		NES_SetNZ(tc);
		break;
	case 0xf6:// inc mem
		printInfo(mem, "INCM ");
		mem->m_pc+=2;
		tc = readCMem(mem, op[0] + mem->m_x);
		tc++;
		writeCMem(mem, op[0] + mem->m_x, tc);
		NES_SetNZ(tc);
		break;
	case 0xee:// inc mem
		printInfo(mem, "INCM ");
		mem->m_pc+=3;
		tc = readCMem(mem, *(unsigned short*)&(op[0]));
		tc++;
		writeCMem(mem, *(unsigned short*)&(op[0]), tc);
		NES_SetNZ(tc);
		break;
	case 0xfe:// inc mem
		printInfo(mem, "INCM ");
		mem->m_pc+=3;
		tc = readCMem(mem, *(unsigned short*)&(op[0]) + mem->m_x);
		tc++;
		writeCMem(mem, *(unsigned short*)&(op[0]) + mem->m_x, tc);
		NES_SetNZ(tc);
		break;
	case 0xc6: // dec mem
		printInfo(mem, "DECM ");
		mem->m_pc+=2;
		tc = readCMem(mem, op[0]);
		tc--;
		writeCMem(mem, op[0], tc);
		NES_SetNZ(tc);
		break;
	case 0xd6:// dec mem
		printInfo(mem, "DECM ");
		mem->m_pc+=2;
		tc = readCMem(mem, op[0] + mem->m_x);
		tc--;
		writeCMem(mem, op[0] + mem->m_x, tc);
		NES_SetNZ(tc);
		break;
	case 0xce:// dec mem
		printInfo(mem, "DECM ");
		mem->m_pc+=3;
		tc = readCMem(mem, *(unsigned short*)&(op[0]));
		tc--;
		writeCMem(mem, *(unsigned short*)&(op[0]), tc);
		NES_SetNZ(tc);
		break;
	case 0xde:// dec mem
		printInfo(mem, "DECM ");
		mem->m_pc+=3;
		tc = readCMem(mem, *(unsigned short*)&(op[0]) + mem->m_x);
		tc--;
		writeCMem(mem, *(unsigned short*)&(op[0]) + mem->m_x, tc);
		NES_SetNZ(tc);
		break;

	case 0x4c: // jmp abs
		printInfo(mem, "JMPABS ");
		mem->m_pc = *(unsigned short *)&(op[0]);
		break;
	case 0x6c: // jmp indirect
		printInfo(mem, "JMPIND ");
		mem->m_pc = readCMem2(mem, *(unsigned short*)&(op[0]));
		break;
	case 0x00: // brk
		mem->m_trace = 1;
		printInfo(mem, "BRK ");
		mem->m_pc++;
		if (!(mem->m_sr & 0x04)) {
			mem->m_cmem[NES_STACKBASE + mem->m_sp] = ((mem->m_pc) >> 8);
			mem->m_sp--;
			mem->m_cmem[NES_STACKBASE + mem->m_sp] = (unsigned char)(mem->m_pc);
			mem->m_sp--;
			mem->m_cmem[NES_STACKBASE + mem->m_sp] = (mem->m_sr | 0x10); // break Flag
			mem->m_sp--;
			mem->m_sr |= 0x04;
			mem->m_pc = *(unsigned short *)&(mem->m_cmem[0xfffe]);
			mem->m_breaked = 1;
		}
		break;
	case 0x20: // call
		printInfo(mem, "CALL ");
		mem->m_pc+=3;
		ts = mem->m_pc - 1;
		writeCMem(mem, NES_STACKBASE + mem->m_sp, ts >> 8);
		mem->m_sp--;
		writeCMem(mem, NES_STACKBASE + mem->m_sp, ts & 0xff);
		mem->m_sp--;
		mem->m_pc = *(unsigned short *)&(op[0]);
		if (mem->m_sp >= 0xfe) {
			printf("ERROR: stack underflow\n");
			return 0;
		}
		break;
	case 0x60: // return
		printInfo(mem, "RET ");
		mem->m_sp++;
		ts = readCMem(mem, NES_STACKBASE + mem->m_sp);
		mem->m_sp++;
		ts |= (((unsigned short)readCMem(mem, NES_STACKBASE + mem->m_sp)) << 8);
		mem->m_pc = ts + 1;
		if (mem->m_sp < 2) {
			printf("ERROR: stack overflow\n");
			return 0;
		}
		
		break;
	case 0x40: // return from interrupt
		printInfo(mem, "RTI ");
		mem->m_sp++;
		mem->m_sr = readCMem(mem, NES_STACKBASE + mem->m_sp);
		NES_CPB(mem);
		mem->m_sr |= 0x20;
		mem->m_sp++;
		ts = readCMem(mem, NES_STACKBASE + mem->m_sp);
		mem->m_sp++;
		ts |= (((unsigned short)readCMem(mem, NES_STACKBASE + mem->m_sp))<<8);
		mem->m_pc = ts;
		break;

	case 0x38:
		mem->m_pc++; NES_SPC(mem); break;
	case 0xf8:
		mem->m_pc++; NES_SPD(mem); break;
	case 0x78: // SEI, set interrupt disable
		mem->m_pc++; NES_SPI(mem); break;

	case 0xea: // NOP
		mem->m_pc++; break;

	case 0x48: // push A
		mem->m_pc++;
		writeCMem(mem, NES_STACKBASE + mem->m_sp, mem->m_a);
		mem->m_sp--;
		break;
	case 0x08: // push P
		mem->m_pc++;
		writeCMem(mem, NES_STACKBASE + mem->m_sp, mem->m_sr | 0x10);
		mem->m_sp--;
		break;
	case 0x68: // pull A
		mem->m_pc++;
		mem->m_sp++;
		mem->m_a = readCMem(mem, NES_STACKBASE + mem->m_sp);
		NES_SetNZ(mem->m_a);
		break;
	case 0x28: // pull P
		mem->m_pc++;
		mem->m_sp++;
		mem->m_sr = readCMem(mem, NES_STACKBASE + mem->m_sp);
		NES_CPB(mem);
		mem->m_sr |= 0x20;
		break;

	default:
		printf("unknown op code %02x\n", ins);
	    mem->m_pc++;
		mem->m_delayms = -1;
		break;
	}

	// validates...
	if (mem->m_lastVerPos < 0 || mem->m_autoTraceCount >= 32) {
		printf("ERROR lastVerPos < 0, %02x %02x %02x", ins, op[0], op[1]);
		mem->m_delayms = -1;
	}
	return 1;
}

void printVROM(unsigned char *data, int len, int nrow = 8) 
{
	unsigned char *end = data + len;
	while (data < end) {
		int step = (end - data) / (nrow * 2);
		if (step > 8) step = 8;
		for (int y = 0; y < nrow; y++) {
			for (int s = 0; s < step; s++) {
				unsigned char *p = data + s * (nrow * 2);
				for (int x = 0; x < 8; x++) {
					unsigned char col = ((p[y/8*16 + y%8]   & (1 << (8-x-1))) >> (8-x-1)) |
										((p[y/8*16 + y%8+8] & (1 << (8-x-1))) >> (8-x-2));
					if (col == 0) printf(" ");
					else if (col == 1) printf(".");
					else if (col == 2) printf("+");
					else printf("#");
				}
				if (s < step-1) printf("  ");
			}
			printf("\n");
		}
		data += (nrow * 2) * step;
		printf("\n");
		//::system("pause");
	}
}

INT_PTR CALLBACK dialogProc(
  _In_  HWND hwndDlg,
  _In_  UINT uMsg,
  _In_  WPARAM wParam,
  _In_  LPARAM lParam);

void runEngine(void *mem_);

int _tmain(int argc, _TCHAR* argv[])
{
	FILE *fp = 0;
	const int ROMBLKSIZE = 16384;
	const int VROMBLKSize = 8192;
	char path[4096] = {0};
	
	memset(&mem, 0, sizeof(mem));
	memset(&hdr, 0, sizeof(hdr));
	
	printf("NES Path:");
	scanf("%s", path);
	fp = fopen(path, "rb");
	if (!fp) {
		printf("failed to open %s\n", path);
		return 1;
	}

	strncpy((char *)(mem.m_savePath), path, sizeof(mem.m_savePath)-5);
	strcat((char *)mem.m_savePath, "sav");
	mem.m_sr |= 0x04; // interrupt is disable at startup
	mem.m_trace = 1;

	fread(&hdr, sizeof(hdr), 1, fp);

	mem.m_fp = fp;
	mem.m_mirroring = (hdr.m_flag & 1) ? NESMirroring::vertical : NESMirroring::horizontal;
	if (hdr.m_flag & (1<<3)) mem.m_mirroring = NESMirroring::fourscreen;
	mem.m_mapper = (hdr.m_flag >> 4);
	mem.m_mapper |= ((hdr.m_flag >> 12) << 4);
	mem.m_vromFileOffset = sizeof(hdr) + ROMBLKSIZE * hdr.m_nROM;
	mem.m_nPRGROM = hdr.m_nROM;
	mem.m_nCHRROM = hdr.m_nVROM;

	printf("nROM = %d, vROM = %d, flag = 0x%x, mapperNo = %d, nRAM = %d, isPAL = %d, mirroring = %d\n",
		hdr.m_nROM, hdr.m_nVROM, hdr.m_flag, mem.m_mapper, hdr.m_nRAM, hdr.m_isPAL, mem.m_mirroring);

	fread(mem.m_cmem + NSE_ROMBASE, ROMBLKSIZE, 1, fp);	
	if (hdr.m_nROM == 1) {
		memcpy(mem.m_cmem + NSE_ROMBASE + ROMBLKSIZE, mem.m_cmem + NSE_ROMBASE, ROMBLKSIZE);
	} else {
		if (hdr.m_nROM > 2)
			fseek(fp, (hdr.m_nROM - 2) * ROMBLKSIZE, SEEK_CUR);
		fread(mem.m_cmem + NSE_ROMBASE + ROMBLKSIZE, ROMBLKSIZE, 1, fp);	
	}
	fseek(fp, mem.m_vromFileOffset, SEEK_SET);
	fread(mem.m_pmem, VROMBLKSize, 1, fp);
//	printVROM(mem.m_pmem, vromSize /2, 8);
//	printVROM(mem.m_pmem + vromSize/2, vromSize /2, 16);

	reset(&mem);

	Gdiplus::GdiplusStartupInput gdiplusStartupInput;
    ULONG_PTR gdiplusToken;
    Gdiplus::GdiplusStartup(&gdiplusToken, &gdiplusStartupInput, NULL);
	hBitmap = new EasyBitmap(512, 480, PixelFormat32bppARGB);

	DWORD dwThreadId;
	CreateThread(NULL, //Choose default security
				0, //Default stack size
				(LPTHREAD_START_ROUTINE)&runEngine,
				//Routine to execute
				(LPVOID) &mem, //Thread parameter
				0, //Immediately run the thread
				&dwThreadId //Thread Id
				);

	hwnd = (HWND)CreateDialog(0, MAKEINTRESOURCE(IDD_MAINDIALOG), 0,
		                      dialogProc);
	ShowWindow(hwnd, SW_SHOW);
	int ret;
	MSG msg;
	while((ret = GetMessage(&msg, 0, 0, 0)) != 0) {
	  if(ret == -1) /* error found */
		break;

	  if (msg.message == WM_KEYDOWN) {
		  switch((char)msg.wParam) {
			  case 'L': mem.m_joystickStatus |= 0x01; break; // button A
			  case 'J': mem.m_joystickStatus |= 0x02; break; // button B
			  case 'B': mem.m_joystickStatus |= 0x04; break; // select
			  case 'N': mem.m_joystickStatus |= 0x08; break; // start
			  case 'E': mem.m_joystickStatus |= 0x10; break; // up
			  case 'D': mem.m_joystickStatus |= 0x20; break; // down
			  case 'S': mem.m_joystickStatus |= 0x40; break; // left
			  case 'F': mem.m_joystickStatus |= 0x80; break; // right
		  }
		  //printf("KeyDown %c status=%02x\n", (char)msg.wParam, mem.m_joystickStatus);
	  }
	  if (msg.message == WM_KEYUP) {
		  switch((char)msg.wParam) {
			  case 'L': mem.m_joystickStatus &= ~0x01; break; // button A
			  case 'J': mem.m_joystickStatus &= ~0x02; break; // button B
			  case 'B': mem.m_joystickStatus &= ~0x04; break; // select
			  case 'N': mem.m_joystickStatus &= ~0x08; break; // start
			  case 'E': mem.m_joystickStatus &= ~0x10; break; // up
			  case 'D': mem.m_joystickStatus &= ~0x20; break; // down
			  case 'S': mem.m_joystickStatus &= ~0x40; break; // left
			  case 'F': mem.m_joystickStatus &= ~0x80; break; // right
		  }
		  //printf("KeyUp %c status=%02x\n", (char)msg.wParam, mem.m_joystickStatus);
	  }
	 // if(!IsDialogMessage(hwnd, &msg)) {
		TranslateMessage(&msg); /* translate virtual-key messages */
		DispatchMessage(&msg); /* send it to dialog procedure */
	  //}
	}
	delete hBitmap;
	return 0;
}

void runEngine(void *) {
	
	while (1) {
		if (mem.m_trace) {
			printf("\n%lld ", (long long)mem.m_insCount);
		}
		if (!execute(&mem)) break;
		mem.m_insCount++;

		pputick(&mem);

		volatile int delayms = mem.m_delayms;
		if (delayms > 0) ::Sleep(delayms);
		else if (delayms == -1) {
			while (mem.m_delayms < 0) ::Sleep(10);
		}

		// save or load
		if (mem.m_signalSave) {
			FILE *fsave = fopen(mem.m_savePath, "wb");
			if (fsave) {
				fwrite(&mem, 1, NES_SAVING_SIZE, fsave);
				fclose(fsave);
				printf("Saving OK\n");
			} else printf("Saving Failed, can't open file\n");
			mem.m_signalSave = mem.m_signalLoad = 0;
		} else if (mem.m_signalLoad) {
			FILE *fload = fopen(mem.m_savePath, "rb");
			if (fload) {
				fread(&mem, 1, NES_SAVING_SIZE, fload);
				fclose(fload);
				printf("Loaded\n");
			} else printf("Failed to open file to load\n");
			mem.m_signalSave = mem.m_signalLoad = 0;
		}
	}
}

INT_PTR CALLBACK dialogProc(
  _In_  HWND hwndDlg,
  _In_  UINT uMsg,
  _In_  WPARAM wParam,
  _In_  LPARAM lParam) 
{
	::hwnd = hwndDlg;
	switch(uMsg) {
	case WM_CREATE:
		::SetWindowPos(hwndDlg, 0, 0, 0, 520, 520, 0);
		//::SetTimer(hwndDlg, 10, 50, NULL);
		break;
	//case WM_TIMER:
	case WM_PAINT:
		{
			HDC hdc = ::GetDC(hwndDlg);
			Gdiplus::Graphics *hgraph = Gdiplus::Graphics::FromHDC(hdc, 0);
			hBitmap->EasyLock();
			for (int y = 0; y < 240; y++) {
				for (int x = 0; x < 256; x++) {
					hBitmap->EasySetPixel(x*2, y*2, mem.m_display[y][x] | 0xff000000);
					hBitmap->EasySetPixel(x*2+1, y*2, mem.m_display[y][x] | 0xff000000);
					hBitmap->EasySetPixel(x*2, y*2+1, mem.m_display[y][x] | 0xff000000);
					hBitmap->EasySetPixel(x*2+1, y*2+1, mem.m_display[y][x] | 0xff000000);
				}
			}
			hBitmap->EasyUnlock();
			hgraph->DrawImage(hBitmap, 0, 0, hBitmap->GetWidth(), hBitmap->GetHeight());
			::ReleaseDC(hwndDlg, hdc);
			HWND hedit = GetDlgItem(hwndDlg, IDC_DEBUGMSG);
			if (hedit) {
				::SetWindowTextA(hedit, (const char*)mem.m_debugText);
			}
		}
		break;
	case WM_COMMAND: // button Click
		switch(wParam) {
		case IDC_PAUSE:
			mem.m_delayms = -1;
			break;
		case IDC_RUN:
			mem.m_delayms = 0;
			break;
		case IDC_STEP:
			mem.m_delayms = 1000;
			break;
		case IDC_DRAW:
			draw(&mem);
			break;
		case IDC_TRACE:
			mem.m_trace = !mem.m_trace;
			break;
		case IDC_SAVE:
			mem.m_signalSave = 1;
			break;
		case IDC_LOAD:
			mem.m_signalLoad = 1;
			break;
		};
		break;
	case WM_CLOSE:
		break;
	}
	return 0;
}
