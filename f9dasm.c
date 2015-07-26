/***************************************************************************
 * f9dasm -- Portable M6800..6809/H6309 Binary/OS9/FLEX disassembler       *
 * HEAVILY modified by H.Seib to adapt it to his needs.                    *
 *                                                                         *
 * Line Disassembler Engine Copyright (C) 2000  Arto Salmi                 *
 * Various additions        Copyright (c) 2001-2015 Hermann Seib           *
 * Various additions        Copyright (c) 2013 by Colin Bourassa           *
 * Various additions        Copyright (c) 2014 by Rainer Buchty            *
 *                                                                         *
 * This program is free software; you can redistribute it and/or modify    *
 * it under the terms of the GNU General Public License as published by    *
 * the Free Software Foundation; either version 2 of the License, or       *
 * (at your option) any later version.                                     *
 *                                                                         *
 * This program is distributed in the hope that it will be useful,         *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of          *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           *
 * GNU General Public License for more details.                            *
 *                                                                         *
 * You should have received a copy of the GNU General Public License       *
 * along with this program; if not, write to the Free Software             *
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.               *
 ***************************************************************************/

/*
 * There's a variant "1.61-RB" available at
 *   https://github.com/gordonjcp/f9dasm
 * (based on my V1.60) which contains some interesting changes by Rainer Buchty.
 * Unfortunately, I only saw that by chance in summer 2015, 6 versions later.
 * Rainer customized it to his own taste, so I didn't directly add his changes
 * to my code, which defaults to TSC Assembler compatible output;
 * you can, however, create a "Rainer Buchty compatible variant" by either
 *     1) setting the -DRB_VARIANT compiler flag or
 *     2) setting the following definition to anything other than 0.
*/

#ifndef RB_VARIANT
  #define RB_VARIANT 0
#endif

/* History, as far as I could retrace it:

   v0.1  2000-02-06 Arto Salmi's original dasm09 6809/6309
   V1.01 2001-??-?? Start of my changes; dates and changes for this and the
                    next 54 versions are lost in the mists of time ...
                    it grew in parallel to my reverse-engineering of the
                    PPG Wave synthesizers, the PPG PRK Keyboard and the
                    PPG Waveterm A operating system
   V1.56 2006-02-05 First appearance in my versioning system
                      (can't believe I lived without one for over 20 years!)
   V1.57 2008-11-18 Added "FILE filename [offset]" directive
   V1.58 2009-02-07 6800 mode added
   V1.59 2009-02-23 Made gcc-compatible (so yes, it DOES work on Linux)
                    Command line option -help added to get normal plus
                      info file usage
   V1.60 2009-03-13 Duplicate Flex labels changed
                    Error in PCR handling corrected
   V1.61 2010-05-05 Cured a little bug in binary file loading
                      (detected by J. Sigle)
   V1.62 2010-05-06 Little bug in 6800 indexed addressing cured
                    Documentation expanded
   V1.63 2013-06-11 Added Colin Bourassa's 6801/6803 support
   V1.64 2013-06-12 nofcc option added
                    Added fcc option
                    Added proper [no]asm and [no]fcc documentation
   V1.65 2013-06-13 Added omitzero / showzero options
   V1.66 2015-06-18 Added VS2008 project files, corrected VS6 mot2bin project
                    mot2bin still displayed hex2bin messages (it only took about
                      10 years until I noticed that...)
                    Added -6802/-6803/-6808/-6809 command line options
                    Added changes from Rainer Buchty's version at
                      https://github.com/gordonjcp/f9dasm
                    Added -[no]forced command line option
                    Added -ldchar command line option
                    Updated documentation
   V1.67 2015-06-19 Made setdp address-aware
                    Correctly default to DP 0 on 6809 / 6309
                    Added enumeration for info-file keywords to make code
                      more readable
   V1.68 2015-06-22 setdp addr dp now sets dp for all addresses starting
                      at addr up to the end of input
                    Updated documentation
   V1.69 2015-07-02 (RB) CPU-dependent system vector parsing
                    automatically inserts 
                    *   system vectors as word-data labels
                    *   entry points as code labels
   V1.70 2015-07-03 phase addr[-addr] phase  command added
   V1.71 2015-07-03 (RB) CVEC/DVEC directives and noLabelLoad option added
                    *   CVEC/DVEC declare code/data vector fields
                        if not declared already, they will be auto-inserted into label list
                    *   noLabelLoad option suppresses adding of load-address label
   V1.72 2015-07-08 Intel and Motorola Hex file parsing improved
   V1.73 2015-07-09 (RB) 6301 option added
                    *   like 6801/03 but with some 6809-specific commands (AIM/EIM/OIM/TIM) and addressing modes (BE/BI)
                    *   additional opcodes SLP and XGDX
   V1.74 2015-07-09 phase addr[-addr] {+|-}rel  command added
   V1.75 2015-07-26 Only install system vector labels if they aren't defined in an info file
                    INSERT, COMMENT, PREPCOMM without address range can be used to prepend text to the output, too
                    PREPEND without address now works like normal PREPEND - it adds BEFORE the first line
*/

#define ID  "1.75"

#if RB_VARIANT
#define VERSION ID "-RB"
#else
#define VERSION ID
#endif

/* NOTE! os9 call table is not tested. */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define MEMORY(address)  (memory[(address)&0xffff])
#define OPCODE(address)  MEMORY(address)
#define ARGBYTE(address) MEMORY(address)
#define ARGWORD(address) (word)((MEMORY(address)<<8)|MEMORY((address)+1))

#define ATTRBYTE(address) (label[(address) & 0xffff])

#define DATATYPE_BYTE   0x00
#define DATATYPE_WORD   0x08
#define DATATYPE_DEC    0x00
#define DATATYPE_BINARY 0x10
#define DATATYPE_HEX    0x20
#define DATATYPE_CHAR   (DATATYPE_BINARY | DATATYPE_HEX)
#define DATATYPE_RMB    (DATATYPE_WORD | DATATYPE_BINARY | DATATYPE_HEX)

/* RB: new vector datatypes */
#define DATATYPE_DVEC   0x0100
#define DATATYPE_CVEC   0x0200

#define AREATYPE_CLABEL 0x01
#define AREATYPE_LABEL  0x02
#define AREATYPE_ULABEL 0x04
#define AREATYPE_DATA   0x40
#define AREATYPE_CODE   0x80
#define AREATYPE_WORD   (AREATYPE_DATA | DATATYPE_WORD)
#define AREATYPE_RMB    (AREATYPE_DATA | DATATYPE_RMB)
#define AREATYPE_BINARY (AREATYPE_DATA | DATATYPE_BINARY)
#define AREATYPE_DEC    (AREATYPE_DATA | DATATYPE_DEC)
#define AREATYPE_HEX    (AREATYPE_DATA | DATATYPE_HEX)
#define AREATYPE_CHAR   (AREATYPE_DATA | DATATYPE_CHAR)
#define AREATYPE_CONST  (AREATYPE_CODE | AREATYPE_DATA)

/* RB: new vector areatypes */
#define AREATYPE_DVEC   (AREATYPE_WORD | DATATYPE_DVEC)
#define AREATYPE_CVEC   (AREATYPE_WORD | DATATYPE_CVEC)

#define IS_CODE(address) ((ATTRBYTE(address)&AREATYPE_CONST)==AREATYPE_CODE)
#define IS_DATA(address) ((ATTRBYTE(address)&AREATYPE_CONST)==AREATYPE_DATA)
#define IS_CONST(address) ((ATTRBYTE(address)&AREATYPE_CONST)==AREATYPE_CONST)

#define SET_USED(address) used[(address) / 8] |= (1 << ((address) % 8))
#define SET_UNUSED(address) used[(address) / 8] &= ~(1 << ((address) % 8))
#define IS_USED(address) (!!(used[(address) / 8] & (1 << ((address) % 8))))

#define IS_LABEL(address) (!!(ATTRBYTE(address) & AREATYPE_LABEL))
#define IS_CLABEL(address) ((ATTRBYTE(address) & (AREATYPE_CLABEL | AREATYPE_LABEL)) == (AREATYPE_CLABEL | AREATYPE_LABEL))
#define IS_DLABEL(address) ((ATTRBYTE(address) & (AREATYPE_CLABEL | AREATYPE_LABEL)) == AREATYPE_LABEL)
#define IS_ULABEL(address) ((ATTRBYTE(address) & (AREATYPE_ULABEL | AREATYPE_LABEL)) == (AREATYPE_ULABEL | AREATYPE_LABEL))

#define IS_BREAK(address)  ((ATTRBYTE(address) & (AREATYPE_ULABEL | AREATYPE_LABEL)) == AREATYPE_ULABEL)

#define IS_WORD(address) (!!(ATTRBYTE(address) & DATATYPE_WORD))
#define IS_BINARY(address) (!!(ATTRBYTE(address) & DATATYPE_BINARY))
#define IS_HEX(address) (!!(ATTRBYTE(address) & DATATYPE_HEX))
#define IS_CHAR(address)  (!IS_WORD(address) && IS_HEX(address) && IS_BINARY(address))
#define IS_RMB(address) ((ATTRBYTE(address) & DATATYPE_RMB) == DATATYPE_RMB)

/* RB: new vector checks */
#define IS_VEC(address)   (ATTRBYTE(address) & (DATATYPE_DVEC | DATATYPE_CVEC))
#define IS_DVEC(address)  (ATTRBYTE(address) & DATATYPE_DVEC)
#define IS_CVEC(address)  (ATTRBYTE(address) & DATATYPE_CVEC)

#ifndef TYPES
#define TYPES
typedef unsigned char  byte;
typedef unsigned short word;
#endif

#ifndef NULL
#define NULL 0
#endif

#ifndef FALSE
#define TRUE  (1==1)
#define FALSE (!TRUE)
#endif

#ifdef _MSC_VER
#define FNCASESENS 0
#else
#define FNCASESENS 1
#endif

#define MAXPHASES 16384                 /* should be overkill, but who knows */

typedef struct _phasedef                /* data for a phase                  */
  {
  unsigned short from;
  unsigned short to;
  unsigned short phase;
  unsigned short rel;
  } phasedef;

/*****************************************************************************/
/* Global Data                                                               */
/*****************************************************************************/

byte *memory = NULL;
int *label = NULL;                      /* RB: space for more flags          */
byte *used = NULL;
char **lblnames = NULL;
char **commentlines = NULL;
char **lcomments = NULL;
unsigned short *rels = NULL;
short *dps = NULL;
phasedef *phases = NULL;
int numphases = 0;
int *remaps = NULL;
int  dirpage = 0;                       /* direct page used (default 0)      */
char * szPrepend = NULL;                /* text to be printed before all     */
int showhex = TRUE;
int showaddr = TRUE;
#if RB_VARIANT
int showasc = FALSE;
int useFlex = FALSE;
int useConvenience = FALSE;
char cCommChar = ';';                   /* comment delimiter character       */
char *forcedirectaddr = "";
char *forceextendedaddr = "";
char cLblDelim = ':';
#else
int showasc = TRUE;
int useFlex = TRUE;
int useConvenience = TRUE;
char cCommChar = '*';                   /* comment delimiter character       */
char *forcedirectaddr = "<";
char *forceextendedaddr = ">";
char cLblDelim = ' ';
#endif
int emitComments = TRUE;
byte defaultDataType = DATATYPE_HEX;
int usefcc = TRUE;
int showIndexedModeZeroOperand = FALSE;
char *fname = NULL, *outname = NULL, *infoname = NULL;
unsigned begin = 0xffff, end = 0, offset = 0;
int load = -1;
static char *loaded[200] = {0};
char *sLoadType = "";
/* RB: get a divider after BRA/JMP/SWIx/RTS/RTI/PULx PC */
int optdelimbar = FALSE;  

/* RB: vector address */
int vaddr;

/* RB: option -- suppress auto-insertion of load-address label */
int noLoadLabel = FALSE;

/* RB: system vectors */
char *vec_6801[] = {"IRQ_SCI","IRQ_T0F","IRQ_OCF","IRQ_ICF","IRQ_EXT","SWI","NMI","RST"};
char *vec_6809[] = {"DIV0","SWI3","SWI2","FIRQ","IRQ","SWI","NMI","RST"};

enum                                    /* available options                 */
  {
  OPTION_BEGIN,                         
  OPTION_END,
  OPTION_OFFSET,
  OPTION_OUT,
  OPTION_ADDR,
  OPTION_NOADDR,
  OPTION_HEX,
  OPTION_NOHEX,
  OPTION_6309,
  OPTION_OS9,
  OPTION_INFO,
  OPTION_CCHAR,
  OPTION_ASC,
  OPTION_NOASC,
  OPTION_FLEX,
  OPTION_NOFLEX,
  OPTION_CONV,
  OPTION_NOCONV,
  OPTION_DEC,
  OPTION_NODEC,
  OPTION_COMMENT,
  OPTION_NOCOMMENT,
  OPTION_6800,
  OPTION_HELP,
  OPTION_6801,
  OPTION_FCC,
  OPTION_NOFCC,
  OPTION_OMITZERO,
  OPTION_SHOWZERO,
  OPTION_FORCED,
  OPTION_NOFORCED,
  OPTION_6809,
  OPTION_LDCHAR,
  OPTION_NOLOADLABEL,
  OPTION_6301,
  };

static struct
  {
  char *name;
  int value;
  } Options[] =
  {
  { "begin",     OPTION_BEGIN },
  { "end",       OPTION_END },
  { "offset",    OPTION_OFFSET },
  { "out",       OPTION_OUT },
  { "addr",      OPTION_ADDR },
  { "noaddr",    OPTION_NOADDR },
  { "hex",       OPTION_HEX },
  { "nohex",     OPTION_NOHEX },
  { "x",         OPTION_6309 },
  { "os9",       OPTION_OS9 },
  { "info",      OPTION_INFO },
  { "cchar",     OPTION_CCHAR },
  { "asc",       OPTION_ASC },
  { "noasc",     OPTION_NOASC },
  { "flex",      OPTION_FLEX },
  { "noflex",    OPTION_NOFLEX },
  { "conv",      OPTION_CONV },
  { "noconv",    OPTION_NOCONV },
  { "dec",       OPTION_DEC },
  { "nodec",     OPTION_NODEC },
  { "comment",   OPTION_COMMENT },
  { "nocomment", OPTION_NOCOMMENT },
  { "6301",      OPTION_6301 },
  { "6303",      OPTION_6301 },
  { "6309",      OPTION_6309 },
  { "6800",      OPTION_6800 },
  { "6801",      OPTION_6801 },
  { "6802",      OPTION_6800 },
  { "6803",      OPTION_6801 },
  { "6808",      OPTION_6800 },
  { "6809",      OPTION_6809 },
  { "help",      OPTION_HELP },
  { "fcc",       OPTION_FCC },
  { "nofcc",     OPTION_NOFCC },
  { "omitzero",  OPTION_OMITZERO },
  { "showzero",  OPTION_SHOWZERO },
  { "forced",    OPTION_FORCED },
  { "noforced",  OPTION_NOFORCED },
  { "ldchar",    OPTION_LDCHAR },
  { "noll",      OPTION_NOLOADLABEL },
 { NULL, 0 }
  };

enum addr_mode
  {
  _nom,     /* no mode                    */
  _imp,     /* inherent/implied           */
  _imb,     /* immediate byte             */
  _imw,     /* immediate word             */
  _dir,     /* direct                     */
  _ext,     /* extended                   */
  _ix8,     /* indexed for 6800 (unsigned)*/
  _ind,     /* indexed                    */
  _reb,     /* relative byte              */
  _rew,     /* relative word              */
  _r1 ,     /* tfr/exg mode               */
  _r2 ,     /* pul/psh system             */
  _r3 ,     /* pul/psh user               */
  _bd ,     /* Bit Manipulation direct    */
  _bi ,     /* Bit Manipulation index     */
  _be ,     /* Bit Manipulation extended  */
  _bt ,     /* Bit Transfers direct       */
  _t1 ,     /* Block Transfer r0+,r1+     */
  _t2 ,     /* Block Transfer r0-,r1-     */
  _t3 ,     /* Block Transfer r0+,r1      */
  _t4 ,     /* Block Transfer r0,r1+      */
  _iml,     /* immediate 32-bit           */
  };

enum opcodes
  {
  _ill=0,
  _aba,  _abx,  _adca, _adcb, _adda, _addb, _addd, _anda, _andb,
  _andcc,_asla, _aslb, _asl,  _asra, _asrb, _asr,  _bcc,  _lbcc,
  _bcs,  _lbcs, _beq,  _lbeq, _bge,  _lbge, _bgt,  _lbgt, _bhi,
  _lbhi, _bita, _bitb, _ble,  _lble, _bls,  _lbls, _blt,  _lblt,
  _bmi,  _lbmi, _bne,  _lbne, _bpl,  _lbpl, _bra,  _lbra, _brn,
  _lbrn, _bsr,  _lbsr, _bvc,  _lbvc, _bvs,  _lbvs, 
  _cba,  _cli,  _clra, _clrb, _clr,  _clc,  _clv,  
  _cmpa, _cmpb, _cmpd, _cmps, _cmpu, _cmpx, _cmpy, _coma,
  _comb, _com,  _cwai, _daa,  _deca, _decb, _dec,  _des,  _dex, 
  _eora, _eorb,
  _exg,  _inca, _incb, _inc,  _ins,  _inx,  
  _jmp,  _jsr,  _lda,  _ldb,  _ldd,
  _lds,  _ldu,  _ldx,  _ldy,  _leas, _leau, _leax, _leay, _lsra,
  _lsrb, _lsr,  _mul,  _nega, _negb, _neg,  _nop,  _ora,  _orb,
  _orcc, _psha, _pshb, _pshs, _pshu,
  _pula, _pulb, _puls, _pulu, _rola, _rolb, _rol,  _rora,
  _rorb, _ror,  _rti,  _rts,  _sba,  _sbca, _sbcb,
  _sec,  _sei,  _sev,  _sex,  _sez,  _sta,
  _stb,  _std,  _sts,  _stu,  _stx,  _sty,  _suba, _subb, _subd,
  _swi,  _swi2, _swi3, _sync, _tab,  _tap,  _tba,  
  _tfr,  _tpa,  _tsta, _tstb,
  _tst,  _tsx,  _txs,  
  _wai,  _reset,
  /* 6800 extra opcodes */
  _cpx,
  /* 6801/6803 extra opcodes */
  _pulx, _pshx,
  /* 6301/03 extra opcodes */
  _slp, _xgdx,
  /* 630x extra opcodes */
  _aim,  _eim,  _oim,  _tim,  
  /* 6309 extra opcodes */
  _band, _biand,_bor,  _bior, _beor,
  _bieor,_ldbt, _stbt, _tfm,  _adcd, _adcr, _adde, _addf, _addw,
  _addr, _andd, _andr, _asld, _asrd, _bitd, _bitmd,_clrd, _clre,
  _clrf, _clrw, _cmpe, _cmpf, _cmpw, _cmpr, _comd, _come, _comf,
  _comw, _decd, _dece, _decf, _decw, _divd, _divq, _eord, _eorr,
  _incd, _ince, _incf, _incw, _lde,  _ldf,  _ldq,  _ldw,  _ldmd,
  _lsrd, _lsrw, _muld, _negd, _ord,  _orr,  _pshsw,_pshuw,_pulsw,
  _puluw,_rold, _rolw, _rord, _rorw, _sbcd, _sbcr, _sexw, _ste,
  _stf,  _stq,  _stw,  _sube, _subf, _subw, _subr, _tstd, _tste,
  _tstf, _tstw,
  };

struct
  {
  char *mne;
  byte bCodeJump;
  } mnemo[] =
  {
    { "???",   0 },                     /* _ill                              */
    { "ABA",   0 },                     /* _aba                              */
    { "ABX",   0 },                     /* _abx                              */
    { "ADCA",  0 },                     /* _adca                             */
    { "ADCB",  0 },                     /* _adcb                             */
    { "ADDA",  0 },                     /* _adda                             */
    { "ADDB",  0 },                     /* _addb                             */
    { "ADDD",  0 },                     /* _addd                             */
    { "ANDA",  0 },                     /* _anda                             */
    { "ANDB",  0 },                     /* _andb                             */
    { "ANDCC", 0 },                     /* _andcc                            */
    { "ASLA",  0 },                     /* _asla                             */
    { "ASLB",  0 },                     /* _aslb                             */
    { "ASL",   0 },                     /* _asl                              */
    { "ASRA",  0 },                     /* _asra                             */
    { "ASRB",  0 },                     /* _asrb                             */
    { "ASR",   0 },                     /* _asr                              */
    { "BCC",   1 },                     /* _bcc                              */
    { "LBCC",  1 },                     /* _lbcc                             */
    { "BCS",   1 },                     /* _bcs                              */
    { "LBCS",  1 },                     /* _lbcs                             */
    { "BEQ",   1 },                     /* _beq                              */
    { "LBEQ",  1 },                     /* _lbeq                             */
    { "BGE",   1 },                     /* _bge                              */
    { "LBGE",  1 },                     /* _lbge                             */
    { "BGT",   1 },                     /* _bgt                              */
    { "LBGT",  1 },                     /* _lbgt                             */
    { "BHI",   1 },                     /* _bhi                              */
    { "LBHI",  1 },                     /* _lbhi                             */
    { "BITA",  0 },                     /* _bita                             */
    { "BITB",  0 },                     /* _bitb                             */
    { "BLE",   1 },                     /* _ble                              */
    { "LBLE",  1 },                     /* _lble                             */
    { "BLS",   1 },                     /* _bls                              */
    { "LBLS",  1 },                     /* _lbls                             */
    { "BLT",   1 },                     /* _blt                              */
    { "LBLT",  1 },                     /* _lblt                             */
    { "BMI",   1 },                     /* _bmi                              */
    { "LBMI",  1 },                     /* _lbmi                             */
    { "BNE",   1 },                     /* _bne                              */
    { "LBNE",  1 },                     /* _lbne                             */
    { "BPL",   1 },                     /* _bpl                              */
    { "LBPL",  1 },                     /* _lbpl                             */
    { "BRA",   1 },                     /* _bra                              */
    { "LBRA",  1 },                     /* _lbra                             */
    { "BRN",   1 },                     /* _brn                              */
    { "LBRN",  1 },                     /* _lbrn                             */
    { "BSR",   1 },                     /* _bsr                              */
    { "LBSR",  1 },                     /* _lbsr                             */
    { "BVC",   1 },                     /* _bvc                              */
    { "LBVC",  1 },                     /* _lbvc                             */
    { "BVS",   1 },                     /* _bvs                              */
    { "LBVS",  1 },                     /* _lbvs                             */
    { "CBA",   0 },                     /* _cba                              */
    { "CLI",   0 },                     /* _cli                              */
    { "CLRA",  0 },                     /* _clra                             */
    { "CLRB",  0 },                     /* _clrb                             */
    { "CLR",   0 },                     /* _clr                              */
    { "CLC",   0 },                     /* _clc                              */
    { "CLV",   0 },                     /* _clv                              */
    { "CMPA",  0 },                     /* _cmpa                             */
    { "CMPB",  0 },                     /* _cmpb                             */
    { "CMPD",  0 },                     /* _cmpd                             */
    { "CMPS",  0 },                     /* _cmps                             */
    { "CMPU",  0 },                     /* _cmpu                             */
    { "CMPX",  0 },                     /* _cmpx                             */
    { "CMPY",  0 },                     /* _cmpy                             */
    { "COMA",  0 },                     /* _coma                             */
    { "COMB",  0 },                     /* _comb                             */
    { "COM",   0 },                     /* _com                              */
    { "CWAI",  0 },                     /* _cwai                             */
    { "DAA",   0 },                     /* _daa                              */
    { "DECA",  0 },                     /* _deca                             */
    { "DECB",  0 },                     /* _decb                             */
    { "DEC",   0 },                     /* _dec                              */
    { "DES",   0 },                     /* _des                              */
    { "DEX",   0 },                     /* _dex                              */
    { "EORA",  0 },                     /* _eora                             */
    { "EORB",  0 },                     /* _eorb                             */
    { "EXG",   0 },                     /* _exg                              */
    { "INCA",  0 },                     /* _inca                             */
    { "INCB",  0 },                     /* _incb                             */
    { "INC",   0 },                     /* _inc                              */
    { "INS",   0 },                     /* _ins                              */
    { "INX",   0 },                     /* _inx                              */
    { "JMP",   1 },                     /* _jmp                              */
    { "JSR",   1 },                     /* _jsr                              */
    { "LDA",   0 },                     /* _lda                              */
    { "LDB",   0 },                     /* _ldb                              */
    { "LDD",   0 },                     /* _ldd                              */
    { "LDS",   0 },                     /* _lds                              */
    { "LDU",   0 },                     /* _ldu                              */
    { "LDX",   0 },                     /* _ldx                              */
    { "LDY",   0 },                     /* _ldy                              */
    { "LEAS",  0 },                     /* _leas                             */
    { "LEAU",  0 },                     /* _leau                             */
    { "LEAX",  0 },                     /* _leax                             */
    { "LEAY",  0 },                     /* _leay                             */
    { "LSRA",  0 },                     /* _lsra                             */
    { "LSRB",  0 },                     /* _lsrb                             */
    { "LSR",   0 },                     /* _lsr                              */
    { "MUL",   0 },                     /* _mul                              */
    { "NEGA",  0 },                     /* _nega                             */
    { "NEGB",  0 },                     /* _negb                             */
    { "NEG",   0 },                     /* _neg                              */
    { "NOP",   0 },                     /* _nop                              */
    { "ORA",   0 },                     /* _ora                              */
    { "ORB",   0 },                     /* _orb                              */
    { "ORCC",  0 },                     /* _orcc                             */
    { "PSHA",  0 },                     /* _psha                             */
    { "PSHB",  0 },                     /* _pshb                             */
    { "PSHS",  0 },                     /* _pshs                             */
    { "PSHU",  0 },                     /* _pshu                             */
    { "PULA",  0 },                     /* _pula                             */
    { "PULB",  0 },                     /* _pulb                             */
    { "PULS",  0 },                     /* _puls                             */
    { "PULU",  0 },                     /* _pulu                             */
    { "ROLA",  0 },                     /* _rola                             */
    { "ROLB",  0 },                     /* _rolb                             */
    { "ROL",   0 },                     /* _rol                              */
    { "RORA",  0 },                     /* _rora                             */
    { "RORB",  0 },                     /* _rorb                             */
    { "ROR",   0 },                     /* _ror                              */
    { "RTI",   0 },                     /* _rti                              */
    { "RTS",   0 },                     /* _rts                              */
    { "SBA",   0 },                     /* _sba                              */
    { "SBCA",  0 },                     /* _sbca                             */
    { "SBCB",  0 },                     /* _sbcb                             */
    { "SEC",   0 },                     /* _sec                              */
    { "SEI",   0 },                     /* _sei                              */
    { "SEV",   0 },                     /* _sev                              */
    { "SEX",   0 },                     /* _sex                              */
    { "SEZ",   0 },                     /* _sez                              */
    { "STA",   0 },                     /* _sta                              */
    { "STB",   0 },                     /* _stb                              */
    { "STD",   0 },                     /* _std                              */
    { "STS",   0 },                     /* _sts                              */
    { "STU",   0 },                     /* _stu                              */
    { "STX",   0 },                     /* _stx                              */
    { "STY",   0 },                     /* _sty                              */
    { "SUBA",  0 },                     /* _suba                             */
    { "SUBB",  0 },                     /* _subb                             */
    { "SUBD",  0 },                     /* _subd                             */
    { "SWI",   0 },                     /* _swi                              */
    { "SWI2",  0 },                     /* _swi2                             */
    { "SWI3",  0 },                     /* _swi3                             */
    { "SYNC",  0 },                     /* _sync                             */
    { "TAB",   0 },                     /* _tab                              */
    { "TAP",   0 },                     /* _tap                              */
    { "TBA",   0 },                     /* _tba                              */
    { "TFR",   0 },                     /* _tfr                              */
    { "TPA",   0 },                     /* _tpa                              */
    { "TSTA",  0 },                     /* _tsta                             */
    { "TSTB",  0 },                     /* _tstb                             */
    { "TST",   0 },                     /* _tst                              */
    { "TSX",   0 },                     /* _tsx                              */
    { "TXS",   0 },                     /* _txs                              */
    { "WAI",   0 },                     /* _wai                              */
    { "RESET", 0 },                     /* _reset                            */
  /* 6800 EXTRA OPCODES */
    { "CPX",   0 },                     /* _cpx                              */
  /* 6801/6803 EXTRA OPCODES */
    { "PULX",  0 },                     /* _pulx                             */
    { "PSHX",  0 },                     /* _pshx                             */
  /* 6301/03 EXTRA OPCODES */
    { "SLP",   0 },                     /* _slp                              */
    { "XGDX",  0 },                     /* _xgdx                             */
  /* 630x EXTRA OPCODES */
    { "AIM",   0 },                     /* _aim                              */
    { "EIM",   0 },                     /* _eim                              */
    { "OIM",   0 },                     /* _oim                              */
    { "TIM",   0 },                     /* _tim                              */
  /* 6309 EXTRA OPCODES */
    { "BAND",  0 },                     /* _band                             */
    { "BIAND", 0 },                     /* _biand                            */
    { "BOR",   0 },                     /* _bor                              */
    { "BIOR",  0 },                     /* _bior                             */
    { "BEOR",  0 },                     /* _beor                             */
    { "BIEOR", 0 },                     /* _bieor                            */
    { "LDBT",  0 },                     /* _ldbt                             */
    { "STBT",  0 },                     /* _stbt                             */
    { "TFM",   0 },                     /* _tfm                              */
    { "ADCD",  0 },                     /* _adcd                             */
    { "ADCR",  0 },                     /* _adcr                             */
    { "ADDE",  0 },                     /* _adde                             */
    { "ADDF",  0 },                     /* _addf                             */
    { "ADDW",  0 },                     /* _addw                             */
    { "ADDR",  0 },                     /* _addr                             */
    { "ANDD",  0 },                     /* _andd                             */
    { "ANDR",  0 },                     /* _andr                             */
    { "ASLD",  0 },                     /* _asld                             */
    { "ASRD",  0 },                     /* _asrd                             */
    { "BITD",  0 },                     /* _bitd                             */
    { "BITMD", 0 },                     /* _bitmd                            */
    { "CLRD",  0 },                     /* _clrd                             */
    { "CLRE",  0 },                     /* _clre                             */
    { "CLRF",  0 },                     /* _clrf                             */
    { "CLRW",  0 },                     /* _clrw                             */
    { "CMPE",  0 },                     /* _cmpe                             */
    { "CMPF",  0 },                     /* _cmpf                             */
    { "CMPW",  0 },                     /* _cmpw                             */
    { "CMPR",  0 },                     /* _cmpr                             */
    { "COMD",  0 },                     /* _comd                             */
    { "COME",  0 },                     /* _come                             */
    { "COMF",  0 },                     /* _comf                             */
    { "COMW",  0 },                     /* _comw                             */
    { "DECD",  0 },                     /* _dedc                             */
    { "DECE",  0 },                     /* _dece                             */
    { "DECF",  0 },                     /* _decf                             */
    { "DECW",  0 },                     /* _decw                             */
    { "DIVD",  0 },                     /* _divd                             */
    { "DIVQ",  0 },                     /* _divq                             */
    { "EORD",  0 },                     /* _eord                             */
    { "EORR",  0 },                     /* _eorr                             */
    { "INCD",  0 },                     /* _incd                             */
    { "INCE",  0 },                     /* _ince                             */
    { "INCF",  0 },                     /* _incf                             */
    { "INCW",  0 },                     /* _incw                             */
    { "LDE",   0 },                     /* _lde                              */
    { "LDF",   0 },                     /* _ldf                              */
    { "LDQ",   0 },                     /* _ldq                              */
    { "LDW",   0 },                     /* _ldw                              */
    { "LDMD",  0 },                     /* _ldmd                             */
    { "LSRD",  0 },                     /* _lsrd                             */
    { "LSRW",  0 },                     /* _lsrw                             */
    { "MULD",  0 },                     /* _muld                             */
    { "NEGD",  0 },                     /* _negd                             */
    { "ORD",   0 },                     /* _ord                              */
    { "ORR",   0 },                     /* _orr                              */
    { "PSHSW", 0 },                     /* _pshsw                            */
    { "PSHUW", 0 },                     /* _pshuw                            */
    { "PULSW", 0 },                     /* _pulsw                            */
    { "PULUW", 0 },                     /* _puluw                            */
    { "ROLD",  0 },                     /* _rold                             */
    { "ROLW",  0 },                     /* _rolw                             */
    { "RORD",  0 },                     /* _rord                             */
    { "RORW",  0 },                     /* _rorw                             */
    { "SBCD",  0 },                     /* _sbcd                             */
    { "SBCR",  0 },                     /* _sbcr                             */
    { "SEXW",  0 },                     /* _sexw                             */
    { "STE",   0 },                     /* _ste                              */
    { "STF",   0 },                     /* _stf                              */
    { "STQ",   0 },                     /* _stq                              */
    { "STW",   0 },                     /* _stw                              */
    { "SUBE",  0 },                     /* _sube                             */
    { "SUBF",  0 },                     /* _subf                             */
    { "SUBW",  0 },                     /* _subw                             */
    { "SUBR",  0 },                     /* _subr                             */
    { "TSTD",  0 },                     /* _tstd                             */
    { "TSTE",  0 },                     /* _tste                             */
    { "TSTF",  0 },                     /* _tstf                             */
    { "TSTW",  0 },                     /* _tstw                             */

  };

char *os9_codes[0x100] =
  {
  "F$Link",      "F$Load",      "F$UnLink",    "F$Fork",        /* 00..03 */
  "F$Wait",      "F$Chain",     "F$Exit",      "F$Mem",         /* 04..07 */
  "F$Send",      "F$Icpt",      "F$Sleep",     "F$SSpd",        /* 08..0B */
  "F$ID",        "F$SPrior",    "F$SSWI",      "F$Perr",        /* 0C..0F */
  "F$PrsNam",    "F$CmpNam",    "F$SchBit",    "F$AllBit",      /* 10..13 */
  "F$DelBit",    "F$Time",      "F$STime",     "F$CRC",         /* 14..17 */
  "F$GPrDsc",    "F$GBlkMp",    "F$GModDr",    "F$CpyMem",      /* 18..1B */
  "F$SUser",     "F$UnLoad",    "F$Alarm",     "F$",            /* 1C..1F */
  "F$",          "F$NMLink",    "F$NMLoad",    "F$Ctime",       /* 20..23 */
  "F$Cstime",    "F$CTswi2",    "F$",          "F$VIRQ",        /* 24..27 */
  "F$SRqMem",    "F$SRtMem",    "F$IRQ",       "F$IOQu",        /* 28..2B */
  "F$AProc",     "F$NProc",     "F$VModul",    "F$Find64",      /* 2C..2F */
  "F$All64",     "F$Ret64",     "F$SSvc",      "F$IODel",       /* 30..33 */
  "F$SLink",     "F$Boot",      "F$BtMem",     "F$GProcP",      /* 34..37 */
  "F$Move",      "F$AllRAM",    "F$AllImg",    "F$DelImg",      /* 38..3B */
  "F$SetImg",    "F$FreeLB",    "F$FreeHB",    "F$AllTsk",      /* 3C..3F */
  "F$DelTsk",    "F$SetTsk",    "F$ResTsk",    "F$RelTsk",      /* 40..43 */
  "F$DATLog",    "F$DATTmp",    "F$LDAXY",     "F$LDAXYP",      /* 44..47 */
  "F$LDDDXY",    "F$LDABX",     "F$STABX",     "F$AllPrc",      /* 48..4B */
  "F$DelPrc",    "F$ELink",     "F$FModul",    "F$MapBlk",      /* 4C..4F */
  "F$ClrBlk",    "F$DelRAM",    "F$GCMDir",    "F$AlHRam",      /* 50..53 */
  "F$",          "F$",          "F$",          "F$",            /* 54..57 */
  "F$",          "F$",          "F$",          "F$",            /* 58..5B */
  "F$",          "F$",          "F$",          "F$",            /* 5C..5F */
  "F$",          "F$",          "F$",          "F$",            /* 60..63 */
  "F$",          "F$",          "F$",          "F$",            /* 64..67 */
  "F$",          "F$",          "F$",          "F$",            /* 68..6B */
  "F$",          "F$",          "F$",          "F$",            /* 6C..6F */
  "F$",          "F$",          "F$",          "F$",            /* 70..73 */
  "F$",          "F$",          "F$",          "F$",            /* 74..77 */
  "F$",          "F$",          "F$",          "F$",            /* 78..7B */
  "F$",          "F$",          "F$",          "F$",            /* 7C..7F */
  "I$Attach",    "I$Detach",    "I$Dup",       "I$Create",      /* 80..83 */
  "I$Open",      "I$MakDir",    "I$Chgdir",    "I$Delete",      /* 84..87 */
  "I$Seek",      "I$Read",      "I$Write",     "I$ReadLn",      /* 88..8B */
  "I$WritLn",    "I$GetStt",    "I$SetStt",    "I$Close",       /* 8C..8F */
  "I$DeletX",    "F$",          "F$",          "F$",            /* 90..93 */
  "F$",          "F$",          "F$",          "F$",            /* 94..97 */
  "F$",          "F$",          "F$",          "F$",            /* 98..9B */
  "F$",          "F$",          "F$",          "F$",            /* 9C..9F */
  "F$",          "F$",          "F$",          "F$",            /* A0..A3 */
  "F$",          "F$",          "F$",          "F$",            /* A4..A7 */
  "F$",          "F$",          "F$",          "F$",            /* A8..AB */
  "F$",          "F$",          "F$",          "F$",            /* AC..AF */
  "F$",          "F$",          "F$",          "F$",            /* B0..B3 */
  "F$",          "F$",          "F$",          "F$",            /* B4..B7 */
  "F$",          "F$",          "F$",          "F$",            /* B8..BB */
  "F$",          "F$",          "F$",          "F$",            /* BC..BF */
  "F$",          "F$",          "F$",          "F$",            /* C0..C3 */
  "F$",          "F$",          "F$",          "F$",            /* C4..C7 */
  "F$",          "F$",          "F$",          "F$",            /* C8..CB */
  "F$",          "F$",          "F$",          "F$",            /* CC..CF */
  "F$",          "F$",          "F$",          "F$",            /* D0..D3 */
  "F$",          "F$",          "F$",          "F$",            /* D4..D7 */
  "F$",          "F$",          "F$",          "F$",            /* D8..DB */
  "F$",          "F$",          "F$",          "F$",            /* DC..DF */
  "F$",          "F$",          "F$",          "F$",            /* E0..E3 */
  "F$",          "F$",          "F$",          "F$",            /* E4..E7 */
  "F$",          "F$",          "F$",          "F$",            /* E8..EB */
  "F$",          "F$",          "F$",          "F$",            /* EC..EF */
  "F$",          "F$",          "F$",          "F$",            /* F0..F3 */
  "F$",          "F$",          "F$",          "F$",            /* F4..F7 */
  "F$",          "F$",          "F$",          "F$",            /* F8..FB */
  "F$",          "F$",          "F$",          "F$"             /* FC..FF */
  };

byte m6800_codes[512] =
  {
  _ill  ,_nom,   _nop  ,_imp,   _ill  ,_nom,   _ill  ,_nom,     /* 00..03 */
  _ill  ,_nom,   _ill  ,_nom,   _tap  ,_imp,   _tpa  ,_imp,     /* 04..07 */
  _inx  ,_imp,   _dex  ,_imp,   _clv  ,_imp,   _sev  ,_imp,     /* 08..0B */
  _clc  ,_imp,   _sec  ,_imp,   _cli  ,_imp,   _sei  ,_imp,     /* 0C..0F */
  _sba  ,_imp,   _cba  ,_imp,   _ill  ,_nom,   _ill  ,_nom,     /* 10..13 */
  _ill  ,_nom,   _ill  ,_nom,   _tab  ,_imp,   _tba  ,_imp,     /* 14..17 */
  _ill  ,_nom,   _daa  ,_imp,   _ill  ,_nom,   _aba  ,_imp,     /* 18..1B */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 1C..1F */
  _bra  ,_reb,   _ill  ,_nom,   _bhi  ,_reb,   _bls  ,_reb,     /* 20..23 */
  _bcc  ,_reb,   _bcs  ,_reb,   _bne  ,_reb,   _beq  ,_reb,     /* 24..27 */
  _bvc  ,_reb,   _bvs  ,_reb,   _bpl  ,_reb,   _bmi  ,_reb,     /* 28..2B */
  _bge  ,_reb,   _blt  ,_reb,   _bgt  ,_reb,   _ble  ,_reb,     /* 2C..2F */
  _tsx  ,_imp,   _ins  ,_imp,   _pula ,_imp,   _pulb ,_imp,     /* 30..33 */
  _des  ,_imp,   _txs  ,_imp,   _psha ,_imp,   _pshb ,_imp,     /* 34..37 */
  _ill  ,_nom,   _rts  ,_imp,   _ill  ,_nom,   _rti  ,_imp,     /* 38..3B */
  _ill  ,_nom,   _ill  ,_nom,   _wai  ,_imp,   _swi  ,_imp,     /* 3C..3F */
  _nega ,_imp,   _ill  ,_nom,   _ill  ,_nom,   _coma ,_imp,     /* 40..43 */
  _lsra ,_imp,   _ill  ,_nom,   _rora ,_imp,   _asra ,_imp,     /* 44..47 */
  _asla ,_imp,   _rola ,_imp,   _deca ,_imp,   _ill  ,_nom,     /* 48..4B */
  _inca ,_imp,   _tsta ,_imp,   _ill  ,_nom,   _clra ,_imp,     /* 4C..4F */
  _negb ,_imp,   _ill  ,_nom,   _ill  ,_nom,   _comb ,_imp,     /* 50..53 */
  _lsrb ,_imp,   _ill  ,_nom,   _rorb ,_imp,   _asrb ,_imp,     /* 54..57 */
  _aslb ,_imp,   _rolb ,_imp,   _decb ,_imp,   _ill  ,_nom,     /* 58..5B */
  _incb ,_imp,   _tstb ,_imp,   _ill  ,_nom,   _clrb ,_imp,     /* 5C..5F */
  _neg  ,_ix8,   _ill  ,_nom,   _ill  ,_nom,   _com  ,_ix8,     /* 60..63 */
  _lsr  ,_ix8,   _ill  ,_nom,   _ror  ,_ix8,   _asr  ,_ix8,     /* 64..67 */
  _asl  ,_ix8,   _rol  ,_ix8,   _dec  ,_ix8,   _ill  ,_nom,     /* 68..6B */
  _inc  ,_ix8,   _tst  ,_ix8,   _jmp  ,_ix8,   _clr  ,_ix8,     /* 6C..6F */
  _neg  ,_ext,   _ill  ,_nom,   _ill  ,_nom,   _com  ,_ext,     /* 70..73 */
  _lsr  ,_ext,   _ill  ,_nom,   _ror  ,_ext,   _asr  ,_ext,     /* 74..77 */
  _asl  ,_ext,   _rol  ,_ext,   _dec  ,_ext,   _ill  ,_nom,     /* 78..7B */
  _inc  ,_ext,   _tst  ,_ext,   _jmp  ,_ext,   _clr  ,_ext,     /* 7C..7F */
  _suba ,_imb,   _cmpa ,_imb,   _sbca ,_imb,   _ill  ,_nom,     /* 80..83 */
  _anda ,_imb,   _bita ,_imb,   _lda  ,_imb,   _ill  ,_nom,     /* 84..87 */
  _eora ,_imb,   _adca ,_imb,   _ora  ,_imb,   _adda ,_imb,     /* 88..8B */
  _cpx  ,_imw,   _bsr  ,_reb,   _lds  ,_imw,   _ill  ,_nom,     /* 8C..8F */
  _suba ,_dir,   _cmpa ,_dir,   _sbca ,_dir,   _ill  ,_nom,     /* 90..93 */
  _anda ,_dir,   _bita ,_dir,   _lda  ,_dir,   _sta  ,_dir,     /* 94..97 */
  _eora ,_dir,   _adca ,_dir,   _ora  ,_dir,   _adda ,_dir,     /* 98..9B */
  _cpx  ,_dir,   _ill  ,_nom,   _lds  ,_dir,   _sts  ,_dir,     /* 9C..9F */
  _suba ,_ix8,   _cmpa ,_ix8,   _sbca ,_ix8,   _ill  ,_nom,     /* A0..A3 */
  _anda ,_ix8,   _bita ,_ix8,   _lda  ,_ix8,   _sta  ,_ix8,     /* A4..A7 */
  _eora ,_ix8,   _adca ,_ix8,   _ora  ,_ix8,   _adda ,_ix8,     /* A8..AB */
  _cpx  ,_ix8,   _jsr  ,_ix8,   _lds  ,_ix8,   _sts  ,_ix8,     /* AC..AF */
  _suba ,_ext,   _cmpa ,_ext,   _sbca ,_ext,   _ill  ,_nom,     /* B0..B3 */
  _anda ,_ext,   _bita ,_ext,   _lda  ,_ext,   _sta  ,_ext,     /* B4..B7 */
  _eora ,_ext,   _adca ,_ext,   _ora  ,_ext,   _adda ,_ext,     /* B8..BB */
  _cpx  ,_ext,   _jsr  ,_ext,   _lds  ,_ext,   _sts  ,_ext,     /* BC..BF */
  _subb ,_imb,   _cmpb ,_imb,   _sbcb ,_imb,   _ill  ,_nom,     /* C0..C3 */
  _andb ,_imb,   _bitb ,_imb,   _ldb  ,_imb,   _ill  ,_nom,     /* C4..C7 */
  _eorb ,_imb,   _adcb ,_imb,   _orb  ,_imb,   _addb ,_imb,     /* C8..CB */
  _ill  ,_nom,   _ill  ,_nom,   _ldx  ,_imw,   _ill  ,_nom,     /* CC..CF */
  _subb ,_dir,   _cmpb ,_dir,   _sbcb ,_dir,   _ill  ,_nom,     /* D0..D3 */
  _andb ,_dir,   _bitb ,_dir,   _ldb  ,_dir,   _stb  ,_dir,     /* D4..D7 */
  _eorb ,_dir,   _adcb ,_dir,   _orb  ,_dir,   _addb ,_dir,     /* D8..DB */
  _ill  ,_nom,   _ill  ,_nom,   _ldx  ,_dir,   _stx  ,_dir,     /* DC..DF */
  _subb ,_ix8,   _cmpb ,_ix8,   _sbcb ,_ix8,   _ill  ,_nom,     /* E0..E3 */
  _andb ,_ix8,   _bitb ,_ix8,   _ldb  ,_ix8,   _stb  ,_ix8,     /* E4..E7 */
  _eorb ,_ix8,   _adcb ,_ix8,   _orb  ,_ix8,   _addb ,_ix8,     /* E8..EB */
  _ill  ,_nom,   _ill  ,_nom,   _ldx  ,_ix8,   _stx  ,_ix8,     /* EC..EF */
  _subb ,_ext,   _cmpb ,_ext,   _sbcb ,_ext,   _ill  ,_nom,     /* F0..F3 */
  _andb ,_ext,   _bitb ,_ext,   _ldb  ,_ext,   _stb  ,_ext,     /* F4..F7 */
  _eorb ,_ext,   _adcb ,_ext,   _orb  ,_ext,   _addb ,_ext,     /* F8..FB */
  _ill  ,_nom,   _ill  ,_nom,   _ldx  ,_ext,   _stx  ,_ext,     /* FC..FF */
  };

byte m6801_codes[512] =
  {
  _ill  ,_nom,   _nop  ,_imp,   _ill  ,_nom,   _ill  ,_nom,     /* 00..03 */
  _lsrd ,_imp,   _asld ,_imp,   _tap  ,_imp,   _tpa  ,_imp,     /* 04..07 */
  _inx  ,_imp,   _dex  ,_imp,   _clv  ,_imp,   _sev  ,_imp,     /* 08..0B */
  _clc  ,_imp,   _sec  ,_imp,   _cli  ,_imp,   _sei  ,_imp,     /* 0C..0F */
  _sba  ,_imp,   _cba  ,_imp,   _ill  ,_nom,   _ill  ,_nom,     /* 10..13 */
  _ill  ,_nom,   _ill  ,_nom,   _tab  ,_imp,   _tba  ,_imp,     /* 14..17 */
  _ill  ,_nom,   _daa  ,_imp,   _ill  ,_nom,   _aba  ,_imp,     /* 18..1B */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 1C..1F */
  _bra  ,_reb,   _brn  ,_reb,   _bhi  ,_reb,   _bls  ,_reb,     /* 20..23 */
  _bcc  ,_reb,   _bcs  ,_reb,   _bne  ,_reb,   _beq  ,_reb,     /* 24..27 */
  _bvc  ,_reb,   _bvs  ,_reb,   _bpl  ,_reb,   _bmi  ,_reb,     /* 28..2B */
  _bge  ,_reb,   _blt  ,_reb,   _bgt  ,_reb,   _ble  ,_reb,     /* 2C..2F */
  _tsx  ,_imp,   _ins  ,_imp,   _pula ,_imp,   _pulb ,_imp,     /* 30..33 */
  _des  ,_imp,   _txs  ,_imp,   _psha ,_imp,   _pshb ,_imp,     /* 34..37 */
  _pulx ,_imp,   _rts  ,_imp,   _abx  ,_imp,   _rti  ,_imp,     /* 38..3B */
  _pshx ,_imp,   _mul  ,_imp,   _wai  ,_imp,   _swi  ,_imp,     /* 3C..3F */
  _nega ,_imp,   _ill  ,_nom,   _ill  ,_nom,   _coma ,_imp,     /* 40..43 */
  _lsra ,_imp,   _ill  ,_nom,   _rora ,_imp,   _asra ,_imp,     /* 44..47 */
  _asla ,_imp,   _rola ,_imp,   _deca ,_imp,   _ill  ,_nom,     /* 48..4B */
  _inca ,_imp,   _tsta ,_imp,   _ill  ,_nom,   _clra ,_imp,     /* 4C..4F */
  _negb ,_imp,   _ill  ,_nom,   _ill  ,_nom,   _comb ,_imp,     /* 50..53 */
  _lsrb ,_imp,   _ill  ,_nom,   _rorb ,_imp,   _asrb ,_imp,     /* 54..57 */
  _aslb ,_imp,   _rolb ,_imp,   _decb ,_imp,   _ill  ,_nom,     /* 58..5B */
  _incb ,_imp,   _tstb ,_imp,   _ill  ,_nom,   _clrb ,_imp,     /* 5C..5F */
  _neg  ,_ix8,   _ill  ,_nom,   _ill  ,_nom,   _com  ,_ix8,     /* 60..63 */
  _lsr  ,_ix8,   _ill  ,_nom,   _ror  ,_ix8,   _asr  ,_ix8,     /* 64..67 */
  _asl  ,_ix8,   _rol  ,_ix8,   _dec  ,_ix8,   _ill  ,_nom,     /* 68..6B */
  _inc  ,_ix8,   _tst  ,_ix8,   _jmp  ,_ix8,   _clr  ,_ix8,     /* 6C..6F */
  _neg  ,_ext,   _ill  ,_nom,   _ill  ,_nom,   _com  ,_ext,     /* 70..73 */
  _lsr  ,_ext,   _ill  ,_nom,   _ror  ,_ext,   _asr  ,_ext,     /* 74..77 */
  _asl  ,_ext,   _rol  ,_ext,   _dec  ,_ext,   _ill  ,_nom,     /* 78..7B */
  _inc  ,_ext,   _tst  ,_ext,   _jmp  ,_ext,   _clr  ,_ext,     /* 7C..7F */
  _suba ,_imb,   _cmpa ,_imb,   _sbca ,_imb,   _subd ,_imw,     /* 80..83 */
  _anda ,_imb,   _bita ,_imb,   _lda  ,_imb,   _ill  ,_nom,     /* 84..87 */
  _eora ,_imb,   _adca ,_imb,   _ora  ,_imb,   _adda ,_imb,     /* 88..8B */
  _cpx  ,_imw,   _bsr  ,_reb,   _lds  ,_imw,   _ill  ,_nom,     /* 8C..8F */
  _suba ,_dir,   _cmpa ,_dir,   _sbca ,_dir,   _subd ,_dir,     /* 90..93 */
  _anda ,_dir,   _bita ,_dir,   _lda  ,_dir,   _sta  ,_dir,     /* 94..97 */
  _eora ,_dir,   _adca ,_dir,   _ora  ,_dir,   _adda ,_dir,     /* 98..9B */
  _cpx  ,_dir,   _jsr  ,_dir,   _lds  ,_dir,   _sts  ,_dir,     /* 9C..9F */
  _suba ,_ix8,   _cmpa ,_ix8,   _sbca ,_ix8,   _subd ,_ix8,     /* A0..A3 */
  _anda ,_ix8,   _bita ,_ix8,   _lda  ,_ix8,   _sta  ,_ix8,     /* A4..A7 */
  _eora ,_ix8,   _adca ,_ix8,   _ora  ,_ix8,   _adda ,_ix8,     /* A8..AB */
  _cpx  ,_ix8,   _jsr  ,_ix8,   _lds  ,_ix8,   _sts  ,_ix8,     /* AC..AF */
  _suba ,_ext,   _cmpa ,_ext,   _sbca ,_ext,   _subd ,_ext,     /* B0..B3 */
  _anda ,_ext,   _bita ,_ext,   _lda  ,_ext,   _sta  ,_ext,     /* B4..B7 */
  _eora ,_ext,   _adca ,_ext,   _ora  ,_ext,   _adda ,_ext,     /* B8..BB */
  _cpx  ,_ext,   _jsr  ,_ext,   _lds  ,_ext,   _sts  ,_ext,     /* BC..BF */
  _subb ,_imb,   _cmpb ,_imb,   _sbcb ,_imb,   _addd ,_imw,     /* C0..C3 */
  _andb ,_imb,   _bitb ,_imb,   _ldb  ,_imb,   _ill  ,_nom,     /* C4..C7 */
  _eorb ,_imb,   _adcb ,_imb,   _orb  ,_imb,   _addb ,_imb,     /* C8..CB */
  _ldd  ,_imw,   _ill  ,_nom,   _ldx  ,_imw,   _ill  ,_nom,     /* CC..CF */
  _subb ,_dir,   _cmpb ,_dir,   _sbcb ,_dir,   _addd ,_dir,     /* D0..D3 */
  _andb ,_dir,   _bitb ,_dir,   _ldb  ,_dir,   _stb  ,_dir,     /* D4..D7 */
  _eorb ,_dir,   _adcb ,_dir,   _orb  ,_dir,   _addb ,_dir,     /* D8..DB */
  _ldd  ,_dir,   _std  ,_dir,   _ldx  ,_dir,   _stx  ,_dir,     /* DC..DF */
  _subb ,_ix8,   _cmpb ,_ix8,   _sbcb ,_ix8,   _addd ,_ix8,     /* E0..E3 */
  _andb ,_ix8,   _bitb ,_ix8,   _ldb  ,_ix8,   _stb  ,_ix8,     /* E4..E7 */
  _eorb ,_ix8,   _adcb ,_ix8,   _orb  ,_ix8,   _addb ,_ix8,     /* E8..EB */
  _ldd  ,_ix8,   _std  ,_ix8,   _ldx  ,_ix8,   _stx  ,_ix8,     /* EC..EF */
  _subb ,_ext,   _cmpb ,_ext,   _sbcb ,_ext,   _addd ,_ext,     /* F0..F3 */
  _andb ,_ext,   _bitb ,_ext,   _ldb  ,_ext,   _stb  ,_ext,     /* F4..F7 */
  _eorb ,_ext,   _adcb ,_ext,   _orb  ,_ext,   _addb ,_ext,     /* F8..FB */
  _ldd  ,_ext,   _std  ,_ext,   _ldx  ,_ext,   _stx  ,_ext,     /* FC..FF */
  };

byte m6301_codes[512] =
  {
  _ill  ,_nom,   _nop  ,_imp,   _ill  ,_nom,   _ill  ,_nom,     /* 00..03 */
  _lsrd ,_imp,   _asld ,_imp,   _tap  ,_imp,   _tpa  ,_imp,     /* 04..07 */
  _inx  ,_imp,   _dex  ,_imp,   _clv  ,_imp,   _sev  ,_imp,     /* 08..0B */
  _clc  ,_imp,   _sec  ,_imp,   _cli  ,_imp,   _sei  ,_imp,     /* 0C..0F */
  _sba  ,_imp,   _cba  ,_imp,   _ill  ,_nom,   _ill  ,_nom,     /* 10..13 */
  _ill  ,_nom,   _ill  ,_nom,   _tab  ,_imp,   _tba  ,_imp,     /* 14..17 */
  _xgdx ,_imp,   _daa  ,_imp,   _slp  ,_imp,   _aba  ,_imp,     /* 18..1B: extra 0x18/xgdx, 0x1a/slp */  
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 1C..1F */
  _bra  ,_reb,   _brn  ,_reb,   _bhi  ,_reb,   _bls  ,_reb,     /* 20..23 */
  _bcc  ,_reb,   _bcs  ,_reb,   _bne  ,_reb,   _beq  ,_reb,     /* 24..27 */
  _bvc  ,_reb,   _bvs  ,_reb,   _bpl  ,_reb,   _bmi  ,_reb,     /* 28..2B */
  _bge  ,_reb,   _blt  ,_reb,   _bgt  ,_reb,   _ble  ,_reb,     /* 2C..2F */
  _tsx  ,_imp,   _ins  ,_imp,   _pula ,_imp,   _pulb ,_imp,     /* 30..33 */
  _des  ,_imp,   _txs  ,_imp,   _psha ,_imp,   _pshb ,_imp,     /* 34..37 */
  _pulx ,_imp,   _rts  ,_imp,   _abx  ,_imp,   _rti  ,_imp,     /* 38..3B */
  _pshx ,_imp,   _mul  ,_imp,   _wai  ,_imp,   _swi  ,_imp,     /* 3C..3F */
  _nega ,_imp,   _ill  ,_nom,   _ill  ,_nom,   _coma ,_imp,     /* 40..43 */
  _lsra ,_imp,   _ill  ,_nom,   _rora ,_imp,   _asra ,_imp,     /* 44..47 */
  _asla ,_imp,   _rola ,_imp,   _deca ,_imp,   _ill  ,_nom,     /* 48..4B */
  _inca ,_imp,   _tsta ,_imp,   _ill  ,_nom,   _clra ,_imp,     /* 4C..4F */
  _negb ,_imp,   _ill  ,_nom,   _ill  ,_nom,   _comb ,_imp,     /* 50..53 */
  _lsrb ,_imp,   _ill  ,_nom,   _rorb ,_imp,   _asrb ,_imp,     /* 54..57 */
  _aslb ,_imp,   _rolb ,_imp,   _decb ,_imp,   _ill  ,_nom,     /* 58..5B */
  _incb ,_imp,   _tstb ,_imp,   _ill  ,_nom,   _clrb ,_imp,     /* 5C..5F */
  _neg  ,_ix8,   _aim  ,_bi,    _oim  ,_bi,    _com  ,_ix8,     /* 60..63: extra 0x61/aim, 0x62/oim */
  _lsr  ,_ix8,   _eim  ,_bi,    _ror  ,_ix8,   _asr  ,_ix8,     /* 64..67: extra 0x65/eim */
  _asl  ,_ix8,   _rol  ,_ix8,   _dec  ,_ix8,   _tim  ,_bi,      /* 68..6B: extra 0x6b/tim */
  _inc  ,_ix8,   _tst  ,_ix8,   _jmp  ,_ix8,   _clr  ,_ix8,     /* 6C..6F */
  _neg  ,_ext,   _aim  ,_bd,    _oim  ,_bd,    _com  ,_ext,     /* 70..73: extra 0x71/aim, 0x72/oim */
  _lsr  ,_ext,   _eim  ,_bd,    _ror  ,_ext,   _asr  ,_ext,     /* 74..77: extra 0x75/eim */
  _asl  ,_ext,   _rol  ,_ext,   _dec  ,_ext,   _tim  ,_bd,      /* 78..7B: extra 0x7b/tim */
  _inc  ,_ext,   _tst  ,_ext,   _jmp  ,_ext,   _clr  ,_ext,     /* 7C..7F */
  _suba ,_imb,   _cmpa ,_imb,   _sbca ,_imb,   _subd ,_imw,     /* 80..83 */
  _anda ,_imb,   _bita ,_imb,   _lda  ,_imb,   _ill  ,_nom,     /* 84..87 */
  _eora ,_imb,   _adca ,_imb,   _ora  ,_imb,   _adda ,_imb,     /* 88..8B */
  _cpx  ,_imw,   _bsr  ,_reb,   _lds  ,_imw,   _ill  ,_nom,     /* 8C..8F */
  _suba ,_dir,   _cmpa ,_dir,   _sbca ,_dir,   _subd ,_dir,     /* 90..93 */
  _anda ,_dir,   _bita ,_dir,   _lda  ,_dir,   _sta  ,_dir,     /* 94..97 */
  _eora ,_dir,   _adca ,_dir,   _ora  ,_dir,   _adda ,_dir,     /* 98..9B */
  _cpx  ,_dir,   _jsr  ,_dir,   _lds  ,_dir,   _sts  ,_dir,     /* 9C..9F */
  _suba ,_ix8,   _cmpa ,_ix8,   _sbca ,_ix8,   _subd ,_ix8,     /* A0..A3 */
  _anda ,_ix8,   _bita ,_ix8,   _lda  ,_ix8,   _sta  ,_ix8,     /* A4..A7 */
  _eora ,_ix8,   _adca ,_ix8,   _ora  ,_ix8,   _adda ,_ix8,     /* A8..AB */
  _cpx  ,_ix8,   _jsr  ,_ix8,   _lds  ,_ix8,   _sts  ,_ix8,     /* AC..AF */
  _suba ,_ext,   _cmpa ,_ext,   _sbca ,_ext,   _subd ,_ext,     /* B0..B3 */
  _anda ,_ext,   _bita ,_ext,   _lda  ,_ext,   _sta  ,_ext,     /* B4..B7 */
  _eora ,_ext,   _adca ,_ext,   _ora  ,_ext,   _adda ,_ext,     /* B8..BB */
  _cpx  ,_ext,   _jsr  ,_ext,   _lds  ,_ext,   _sts  ,_ext,     /* BC..BF */
  _subb ,_imb,   _cmpb ,_imb,   _sbcb ,_imb,   _addd ,_imw,     /* C0..C3 */
  _andb ,_imb,   _bitb ,_imb,   _ldb  ,_imb,   _ill  ,_nom,     /* C4..C7 */
  _eorb ,_imb,   _adcb ,_imb,   _orb  ,_imb,   _addb ,_imb,     /* C8..CB */
  _ldd  ,_imw,   _ill  ,_nom,   _ldx  ,_imw,   _ill  ,_nom,     /* CC..CF */
  _subb ,_dir,   _cmpb ,_dir,   _sbcb ,_dir,   _addd ,_dir,     /* D0..D3 */
  _andb ,_dir,   _bitb ,_dir,   _ldb  ,_dir,   _stb  ,_dir,     /* D4..D7 */
  _eorb ,_dir,   _adcb ,_dir,   _orb  ,_dir,   _addb ,_dir,     /* D8..DB */
  _ldd  ,_dir,   _std  ,_dir,   _ldx  ,_dir,   _stx  ,_dir,     /* DC..DF */
  _subb ,_ix8,   _cmpb ,_ix8,   _sbcb ,_ix8,   _addd ,_ix8,     /* E0..E3 */
  _andb ,_ix8,   _bitb ,_ix8,   _ldb  ,_ix8,   _stb  ,_ix8,     /* E4..E7 */
  _eorb ,_ix8,   _adcb ,_ix8,   _orb  ,_ix8,   _addb ,_ix8,     /* E8..EB */
  _ldd  ,_ix8,   _std  ,_ix8,   _ldx  ,_ix8,   _stx  ,_ix8,     /* EC..EF */
  _subb ,_ext,   _cmpb ,_ext,   _sbcb ,_ext,   _addd ,_ext,     /* F0..F3 */
  _andb ,_ext,   _bitb ,_ext,   _ldb  ,_ext,   _stb  ,_ext,     /* F4..F7 */
  _eorb ,_ext,   _adcb ,_ext,   _orb  ,_ext,   _addb ,_ext,     /* F8..FB */
  _ldd  ,_ext,   _std  ,_ext,   _ldx  ,_ext,   _stx  ,_ext,     /* FC..FF */
  };

byte h6309_codes[512] =
  {
  _neg  ,_dir,   _oim  ,_bd ,   _aim  ,_bd ,   _com  ,_dir,     /* 00..03 */
  _lsr  ,_dir,   _eim  ,_bd ,   _ror  ,_dir,   _asr  ,_dir,     /* 04..07 */
  _asl  ,_dir,   _rol  ,_dir,   _dec  ,_dir,   _tim  ,_bd ,     /* 08..0B */
  _inc  ,_dir,   _tst  ,_dir,   _jmp  ,_dir,   _clr  ,_dir,     /* 0C..0F */
  _ill  ,_nom,   _ill  ,_nom,   _nop  ,_imp,   _sync ,_imp,     /* 10..13 */
  _sexw ,_imp,   _ill  ,_nom,   _lbra ,_rew,   _lbsr ,_rew,     /* 14..17 */
  _ill  ,_nom,   _daa  ,_imp,   _orcc ,_imb,   _ill  ,_nom,     /* 18..1B */
  _andcc,_imb,   _sex  ,_imp,   _exg  ,_r1 ,   _tfr  ,_r1 ,     /* 1C..1F */
  _bra  ,_reb,   _brn  ,_reb,   _bhi  ,_reb,   _bls  ,_reb,     /* 20..23 */
  _bcc  ,_reb,   _bcs  ,_reb,   _bne  ,_reb,   _beq  ,_reb,     /* 24..27 */
  _bvc  ,_reb,   _bvs  ,_reb,   _bpl  ,_reb,   _bmi  ,_reb,     /* 28..2B */
  _bge  ,_reb,   _blt  ,_reb,   _bgt  ,_reb,   _ble  ,_reb,     /* 2C..2F */
  _leax ,_ind,   _leay ,_ind,   _leas ,_ind,   _leau ,_ind,     /* 30..33 */
  _pshs ,_r2 ,   _puls ,_r2 ,   _pshu ,_r3 ,   _pulu ,_r3 ,     /* 34..37 */
  _ill  ,_nom,   _rts  ,_imp,   _abx  ,_imp,   _rti  ,_imp,     /* 38..3B */
  _cwai ,_imb,   _mul  ,_imp,   _reset,_imp,   _swi  ,_imp,     /* 3C..3F */
  _nega ,_imp,   _ill  ,_nom,   _ill  ,_nom,   _coma ,_imp,     /* 40..43 */
  _lsra ,_imp,   _ill  ,_nom,   _rora ,_imp,   _asra ,_imp,     /* 44..47 */
  _asla ,_imp,   _rola ,_imp,   _deca ,_imp,   _ill  ,_nom,     /* 48..4B */
  _inca ,_imp,   _tsta ,_imp,   _ill  ,_nom,   _clra ,_imp,     /* 4C..4F */
  _negb ,_imp,   _ill  ,_nom,   _ill  ,_nom,   _comb ,_imp,     /* 50..53 */
  _lsrb ,_imp,   _ill  ,_nom,   _rorb ,_imp,   _asrb ,_imp,     /* 54..57 */
  _aslb ,_imp,   _rolb ,_imp,   _decb ,_imp,   _ill  ,_nom,     /* 58..5B */
  _incb ,_imp,   _tstb ,_imp,   _ill  ,_nom,   _clrb ,_imp,     /* 5C..5F */
  _neg  ,_ind,   _oim  ,_bi ,   _aim  ,_bi ,   _com  ,_ind,     /* 60..63 */
  _lsr  ,_ind,   _eim  ,_bi ,   _ror  ,_ind,   _asr  ,_ind,     /* 64..67 */
  _asl  ,_ind,   _rol  ,_ind,   _dec  ,_ind,   _tim  ,_bi ,     /* 68..6B */
  _inc  ,_ind,   _tst  ,_ind,   _jmp  ,_ind,   _clr  ,_ind,     /* 6C..6F */
  _neg  ,_ext,   _oim  ,_be ,   _aim  ,_be ,   _com  ,_ext,     /* 70..73 */
  _lsr  ,_ext,   _eim  ,_be ,   _ror  ,_ext,   _asr  ,_ext,     /* 74..77 */
  _asl  ,_ext,   _rol  ,_ext,   _dec  ,_ext,   _tim  ,_be ,     /* 78..7B */
  _inc  ,_ext,   _tst  ,_ext,   _jmp  ,_ext,   _clr  ,_ext,     /* 7C..7F */
  _suba ,_imb,   _cmpa ,_imb,   _sbca ,_imb,   _subd ,_imw,     /* 80..83 */
  _anda ,_imb,   _bita ,_imb,   _lda  ,_imb,   _ill  ,_nom,     /* 84..87 */
  _eora ,_imb,   _adca ,_imb,   _ora  ,_imb,   _adda ,_imb,     /* 88..8B */
  _cmpx ,_imw,   _bsr  ,_reb,   _ldx  ,_imw,   _ill  ,_nom,     /* 8C..8F */
  _suba ,_dir,   _cmpa ,_dir,   _sbca ,_dir,   _subd ,_dir,     /* 90..93 */
  _anda ,_dir,   _bita ,_dir,   _lda  ,_dir,   _sta  ,_dir,     /* 94..97 */
  _eora ,_dir,   _adca ,_dir,   _ora  ,_dir,   _adda ,_dir,     /* 98..9B */
  _cmpx ,_dir,   _jsr  ,_dir,   _ldx  ,_dir,   _stx  ,_dir,     /* 9C..9F */
  _suba ,_ind,   _cmpa ,_ind,   _sbca ,_ind,   _subd ,_ind,     /* A0..A3 */
  _anda ,_ind,   _bita ,_ind,   _lda  ,_ind,   _sta  ,_ind,     /* A4..A7 */
  _eora ,_ind,   _adca ,_ind,   _ora  ,_ind,   _adda ,_ind,     /* A8..AB */
  _cmpx ,_ind,   _jsr  ,_ind,   _ldx  ,_ind,   _stx  ,_ind,     /* AC..AF */
  _suba ,_ext,   _cmpa ,_ext,   _sbca ,_ext,   _subd ,_ext,     /* B0..B3 */
  _anda ,_ext,   _bita ,_ext,   _lda  ,_ext,   _sta  ,_ext,     /* B4..B7 */
  _eora ,_ext,   _adca ,_ext,   _ora  ,_ext,   _adda ,_ext,     /* B8..BB */
  _cmpx ,_ext,   _jsr  ,_ext,   _ldx  ,_ext,   _stx  ,_ext,     /* BC..BF */
  _subb ,_imb,   _cmpb ,_imb,   _sbcb ,_imb,   _addd ,_imw,     /* C0..C3 */
  _andb ,_imb,   _bitb ,_imb,   _ldb  ,_imb,   _ill  ,_nom,     /* C4..C7 */
  _eorb ,_imb,   _adcb ,_imb,   _orb  ,_imb,   _addb ,_imb,     /* C8..CB */
  _ldd  ,_imw,   _ldq  ,_iml,   _ldu  ,_imw,   _ill  ,_nom,     /* CC..CF */
  _subb ,_dir,   _cmpb ,_dir,   _sbcb ,_dir,   _addd ,_dir,     /* D0..D3 */
  _andb ,_dir,   _bitb ,_dir,   _ldb  ,_dir,   _stb  ,_dir,     /* D4..D7 */
  _eorb ,_dir,   _adcb ,_dir,   _orb  ,_dir,   _addb ,_dir,     /* D8..DB */
  _ldd  ,_dir,   _std  ,_dir,   _ldu  ,_dir,   _stu  ,_dir,     /* DC..DF */
  _subb ,_ind,   _cmpb ,_ind,   _sbcb ,_ind,   _addd ,_ind,     /* E0..E3 */
  _andb ,_ind,   _bitb ,_ind,   _ldb  ,_ind,   _stb  ,_ind,     /* E4..E7 */
  _eorb ,_ind,   _adcb ,_ind,   _orb  ,_ind,   _addb ,_ind,     /* E8..EB */
  _ldd  ,_ind,   _std  ,_ind,   _ldu  ,_ind,   _stu  ,_ind,     /* EC..EF */
  _subb ,_ext,   _cmpb ,_ext,   _sbcb ,_ext,   _addd ,_ext,     /* F0..F3 */
  _andb ,_ext,   _bitb ,_ext,   _ldb  ,_ext,   _stb  ,_ext,     /* F4..F7 */
  _eorb ,_ext,   _adcb ,_ext,   _orb  ,_ext,   _addb ,_ext,     /* F8..FB */
  _ldd  ,_ext,   _std  ,_ext,   _ldu  ,_ext,   _stu  ,_ext,     /* FC..FF */
  };

byte m6809_codes[512] =
  {
  _neg  ,_dir,   _ill  ,_nom,   _ill  ,_nom,   _com  ,_dir,     /* 00..03 */
  _lsr  ,_dir,   _ill  ,_nom,   _ror  ,_dir,   _asr  ,_dir,     /* 04..07 */
  _asl  ,_dir,   _rol  ,_dir,   _dec  ,_dir,   _ill  ,_nom,     /* 08..0B */
  _inc  ,_dir,   _tst  ,_dir,   _jmp  ,_dir,   _clr  ,_dir,     /* 0C..0F */
  _ill  ,_nom,   _ill  ,_nom,   _nop  ,_imp,   _sync ,_imp,     /* 10..13 */
  _ill  ,_nom,   _ill  ,_nom,   _lbra ,_rew,   _lbsr ,_rew,     /* 14..17 */
  _ill  ,_nom,   _daa  ,_imp,   _orcc ,_imb,   _ill  ,_nom,     /* 18..1B */
  _andcc,_imb,   _sex  ,_imp,   _exg  ,_r1 ,   _tfr  ,_r1 ,     /* 1C..1F */
  _bra  ,_reb,   _brn  ,_reb,   _bhi  ,_reb,   _bls  ,_reb,     /* 20..23 */
  _bcc  ,_reb,   _bcs  ,_reb,   _bne  ,_reb,   _beq  ,_reb,     /* 24..27 */
  _bvc  ,_reb,   _bvs  ,_reb,   _bpl  ,_reb,   _bmi  ,_reb,     /* 28..2B */
  _bge  ,_reb,   _blt  ,_reb,   _bgt  ,_reb,   _ble  ,_reb,     /* 2C..2F */
  _leax ,_ind,   _leay ,_ind,   _leas ,_ind,   _leau ,_ind,     /* 30..33 */
  _pshs ,_r2 ,   _puls ,_r2 ,   _pshu ,_r3 ,   _pulu ,_r3 ,     /* 34..37 */
  _ill  ,_nom,   _rts  ,_imp,   _abx  ,_imp,   _rti  ,_imp,     /* 38..3B */
  _cwai ,_imb,   _mul  ,_imp,   _reset,_imp,   _swi  ,_imp,     /* 3C..3F */
  _nega ,_imp,   _ill  ,_nom,   _ill  ,_nom,   _coma ,_imp,     /* 40..43 */
  _lsra ,_imp,   _ill  ,_nom,   _rora ,_imp,   _asra ,_imp,     /* 44..47 */
  _asla ,_imp,   _rola ,_imp,   _deca ,_imp,   _ill  ,_nom,     /* 48..4B */
  _inca ,_imp,   _tsta ,_imp,   _ill  ,_nom,   _clra ,_imp,     /* 4C..4F */
  _negb ,_imp,   _ill  ,_nom,   _ill  ,_nom,   _comb ,_imp,     /* 50..53 */
  _lsrb ,_imp,   _ill  ,_nom,   _rorb ,_imp,   _asrb ,_imp,     /* 54..57 */
  _aslb ,_imp,   _rolb ,_imp,   _decb ,_imp,   _ill  ,_nom,     /* 58..5B */
  _incb ,_imp,   _tstb ,_imp,   _ill  ,_nom,   _clrb ,_imp,     /* 5C..5F */
  _neg  ,_ind,   _ill  ,_nom,   _ill  ,_nom,   _com  ,_ind,     /* 60..63 */
  _lsr  ,_ind,   _ill  ,_nom,   _ror  ,_ind,   _asr  ,_ind,     /* 64..67 */
  _asl  ,_ind,   _rol  ,_ind,   _dec  ,_ind,   _ill  ,_nom,     /* 68..6B */
  _inc  ,_ind,   _tst  ,_ind,   _jmp  ,_ind,   _clr  ,_ind,     /* 6C..6F */
  _neg  ,_ext,   _ill  ,_nom,   _ill  ,_nom,   _com  ,_ext,     /* 70..73 */
  _lsr  ,_ext,   _ill  ,_nom,   _ror  ,_ext,   _asr  ,_ext,     /* 74..77 */
  _asl  ,_ext,   _rol  ,_ext,   _dec  ,_ext,   _ill  ,_nom,     /* 78..7B */
  _inc  ,_ext,   _tst  ,_ext,   _jmp  ,_ext,   _clr  ,_ext,     /* 7C..7F */
  _suba ,_imb,   _cmpa ,_imb,   _sbca ,_imb,   _subd ,_imw,     /* 80..83 */
  _anda ,_imb,   _bita ,_imb,   _lda  ,_imb,   _ill  ,_nom,     /* 84..87 */
  _eora ,_imb,   _adca ,_imb,   _ora  ,_imb,   _adda ,_imb,     /* 88..8B */
  _cmpx ,_imw,   _bsr  ,_reb,   _ldx  ,_imw,   _ill  ,_nom,     /* 8C..8F */
  _suba ,_dir,   _cmpa ,_dir,   _sbca ,_dir,   _subd ,_dir,     /* 90..93 */
  _anda ,_dir,   _bita ,_dir,   _lda  ,_dir,   _sta  ,_dir,     /* 94..97 */
  _eora ,_dir,   _adca ,_dir,   _ora  ,_dir,   _adda ,_dir,     /* 98..9B */
  _cmpx ,_dir,   _jsr  ,_dir,   _ldx  ,_dir,   _stx  ,_dir,     /* 9C..9F */
  _suba ,_ind,   _cmpa ,_ind,   _sbca ,_ind,   _subd ,_ind,     /* A0..A3 */
  _anda ,_ind,   _bita ,_ind,   _lda  ,_ind,   _sta  ,_ind,     /* A4..A7 */
  _eora ,_ind,   _adca ,_ind,   _ora  ,_ind,   _adda ,_ind,     /* A8..AB */
  _cmpx ,_ind,   _jsr  ,_ind,   _ldx  ,_ind,   _stx  ,_ind,     /* AC..AF */
  _suba ,_ext,   _cmpa ,_ext,   _sbca ,_ext,   _subd ,_ext,     /* B0..B3 */
  _anda ,_ext,   _bita ,_ext,   _lda  ,_ext,   _sta  ,_ext,     /* B4..B7 */
  _eora ,_ext,   _adca ,_ext,   _ora  ,_ext,   _adda ,_ext,     /* B8..BB */
  _cmpx ,_ext,   _jsr  ,_ext,   _ldx  ,_ext,   _stx  ,_ext,     /* BC..BF */
  _subb ,_imb,   _cmpb ,_imb,   _sbcb ,_imb,   _addd ,_imw,     /* C0..C3 */
  _andb ,_imb,   _bitb ,_imb,   _ldb  ,_imb,   _ill  ,_nom,     /* C4..C7 */
  _eorb ,_imb,   _adcb ,_imb,   _orb  ,_imb,   _addb ,_imb,     /* C8..CB */
  _ldd  ,_imw,   _ill  ,_nom,   _ldu  ,_imw,   _ill  ,_nom,     /* CC..CF */
  _subb ,_dir,   _cmpb ,_dir,   _sbcb ,_dir,   _addd ,_dir,     /* D0..D3 */
  _andb ,_dir,   _bitb ,_dir,   _ldb  ,_dir,   _stb  ,_dir,     /* D4..D7 */
  _eorb ,_dir,   _adcb ,_dir,   _orb  ,_dir,   _addb ,_dir,     /* D8..DB */
  _ldd  ,_dir,   _std  ,_dir,   _ldu  ,_dir,   _stu  ,_dir,     /* DC..DF */
  _subb ,_ind,   _cmpb ,_ind,   _sbcb ,_ind,   _addd ,_ind,     /* E0..E3 */
  _andb ,_ind,   _bitb ,_ind,   _ldb  ,_ind,   _stb  ,_ind,     /* E4..E7 */
  _eorb ,_ind,   _adcb ,_ind,   _orb  ,_ind,   _addb ,_ind,     /* E8..EB */
  _ldd  ,_ind,   _std  ,_ind,   _ldu  ,_ind,   _stu  ,_ind,     /* EC..EF */
  _subb ,_ext,   _cmpb ,_ext,   _sbcb ,_ext,   _addd ,_ext,     /* F0..F3 */
  _andb ,_ext,   _bitb ,_ext,   _ldb  ,_ext,   _stb  ,_ext,     /* F4..F7 */
  _eorb ,_ext,   _adcb ,_ext,   _orb  ,_ext,   _addb ,_ext,     /* F8..FB */
  _ldd  ,_ext,   _std  ,_ext,   _ldu  ,_ext,   _stu  ,_ext,     /* FC..FF */
  };

byte h6309_codes10[512] =
  {
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 00..03 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 04..07 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 08..0B */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 0C..0F */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 10..13 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 14..17 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 18..1B */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 1C..1F */
  _ill  ,_nom,   _lbrn ,_rew,   _lbhi ,_rew,   _lbls ,_rew,     /* 20..23 */
  _lbcc ,_rew,   _lbcs ,_rew,   _lbne ,_rew,   _lbeq ,_rew,     /* 24..27 */
  _lbvc ,_rew,   _lbvs ,_rew,   _lbpl ,_rew,   _lbmi ,_rew,     /* 28..2B */
  _lbge ,_rew,   _lblt ,_rew,   _lbgt ,_rew,   _lble ,_rew,     /* 2C..2F */
  _addr ,_r1 ,   _adcr ,_r1 ,   _subr ,_r1 ,   _sbcr ,_r1 ,     /* 30..33 */
  _andr ,_r1 ,   _orr  ,_r1 ,   _eorr ,_r1 ,   _cmpr ,_r1 ,     /* 34..37 */
  _pshsw,_imp,   _pulsw,_imp,   _pshuw,_imp,   _puluw,_imp,     /* 38..3B */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _swi2 ,_imp,     /* 3C..3F */
  _negd ,_imp,   _ill  ,_nom,   _ill  ,_nom,   _comd ,_imp,     /* 40..43 */
  _lsrd ,_imp,   _ill  ,_nom,   _rord ,_imp,   _asrd ,_imp,     /* 44..47 */
  _asld ,_imp,   _rold ,_imp,   _decd ,_imp,   _ill  ,_nom,     /* 48..4B */
  _incd ,_imp,   _tstd ,_imp,   _ill  ,_nom,   _clrd ,_imp,     /* 4C..4F */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _comw ,_imp,     /* 50..53 */
  _lsrw ,_imp,   _ill  ,_nom,   _rorw ,_imp,   _ill  ,_nom,     /* 54..57 */
  _ill  ,_nom,   _rolw ,_imp,   _decw ,_imp,   _ill  ,_nom,     /* 58..5B */
  _incw ,_imp,   _tstw ,_imp,   _ill  ,_nom,   _clrw ,_imp,     /* 5C..5F */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 60..63 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 64..67 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 68..6B */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 6C..6F */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 70..73 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 74..77 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 78..7B */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 7C..7F */
  _subw ,_imw,   _cmpw ,_imw,   _sbcd ,_imw,   _cmpd ,_imw,     /* 80..83 */
  _andd ,_imw,   _bitd ,_imw,   _ldw  ,_imw,   _ill  ,_nom,     /* 84..87 */
  _eord ,_imw,   _adcd ,_imw,   _ord  ,_imw,   _addw ,_imw,     /* 88..8B */
  _cmpy ,_imw,   _ill  ,_nom,   _ldy  ,_imw,   _ill  ,_nom,     /* 8C..8F */
  _subw ,_dir,   _cmpw ,_dir,   _sbcd ,_dir,   _cmpd ,_dir,     /* 90..93 */
  _andd ,_dir,   _bitd ,_dir,   _ldw  ,_dir,   _stw  ,_dir,     /* 94..97 */
  _eord ,_dir,   _adcd ,_dir,   _ord  ,_dir,   _addw ,_dir,     /* 98..9B */
  _cmpy ,_dir,   _ill  ,_nom,   _ldy  ,_dir,   _sty  ,_dir,     /* 9C..9F */
  _subw ,_ind,   _cmpw ,_ind,   _sbcd ,_ind,   _cmpd ,_ind,     /* A0..A3 */
  _andd ,_ind,   _bitd ,_ind,   _ldw  ,_ind,   _stw  ,_ind,     /* A4..A7 */
  _eord ,_ind,   _adcd ,_ind,   _ord  ,_ind,   _addw ,_ind,     /* A8..AB */
  _cmpy ,_ind,   _ill  ,_nom,   _ldy  ,_ind,   _sty  ,_ind,     /* AC..AF */
  _subw ,_ext,   _cmpw ,_ext,   _sbcd ,_ext,   _cmpd ,_ext,     /* B0..B3 */
  _andd ,_ext,   _bitd ,_ext,   _ldw  ,_ext,   _stw  ,_ext,     /* B4..B7 */
  _eord ,_ext,   _adcd ,_ext,   _ord  ,_ext,   _addw ,_ext,     /* B8..BB */
  _cmpy ,_ext,   _ill  ,_nom,   _ldy  ,_ext,   _sty  ,_ext,     /* BC..BF */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* C0..C3 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* C4..C7 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* C8..CB */
  _ill  ,_nom,   _ill  ,_nom,   _lds  ,_imw,   _ill  ,_nom,     /* CC..CF */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* D0..D3 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* D4..D7 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* D8..DB */
  _ldq  ,_dir,   _stq  ,_dir,   _lds  ,_dir,   _sts  ,_dir,     /* DC..DF */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* E0..E3 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* E4..E7 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* E8..EB */
  _ldq  ,_ind,   _stq  ,_ind,   _lds  ,_ind,   _sts  ,_ind,     /* EC..EF */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* F0..F3 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* F4..F7 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* F8..FB */
  _ldq  ,_ext,   _stq  ,_ext,   _lds  ,_ext,   _sts  ,_ext,     /* FC..FF */
  };

byte m6809_codes10[512] =
  {
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 00..03 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 04..07 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 08..0B */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 0C..0F */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 10..13 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 14..17 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 18..1B */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 1C..1F */
  _ill  ,_nom,   _lbrn ,_rew,   _lbhi ,_rew,   _lbls ,_rew,     /* 20..23 */
  _lbcc ,_rew,   _lbcs ,_rew,   _lbne ,_rew,   _lbeq ,_rew,     /* 24..27 */
  _lbvc ,_rew,   _lbvs ,_rew,   _lbpl ,_rew,   _lbmi ,_rew,     /* 28..2B */
  _lbge ,_rew,   _lblt ,_rew,   _lbgt ,_rew,   _lble ,_rew,     /* 2C..2F */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 30..33 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 34..37 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 38..3B */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _swi2 ,_imp,     /* 3C..3F */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 40..43 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 44..47 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 48..4B */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 4C..4F */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 50..53 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 54..57 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 58..5B */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 5C..5F */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 60..63 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 64..67 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 68..6B */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 6C..6F */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 70..73 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 74..77 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 78..7B */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 7C..7F */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _cmpd ,_imw,     /* 80..83 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 84..87 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 88..8B */
  _cmpy ,_imw,   _ill  ,_nom,   _ldy  ,_imw,   _ill  ,_nom,     /* 8C..8F */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _cmpd ,_dir,     /* 90..93 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 94..97 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 98..9B */
  _cmpy ,_dir,   _ill  ,_nom,   _ldy  ,_dir,   _sty  ,_dir,     /* 9C..9F */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _cmpd ,_ind,     /* A0..A3 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* A4..A7 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* A8..AB */
  _cmpy ,_ind,   _ill  ,_nom,   _ldy  ,_ind,   _sty  ,_ind,     /* AC..AF */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _cmpd ,_ext,     /* B0..B3 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* B4..B7 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* B8..BB */
  _cmpy ,_ext,   _ill  ,_nom,   _ldy  ,_ext,   _sty  ,_ext,     /* BC..BF */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* C0..C3 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* C4..C7 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* C8..CB */
  _ill  ,_nom,   _ill  ,_nom,   _lds  ,_imw,   _ill  ,_nom,     /* CC..CF */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* D0..D3 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* D4..D7 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* D8..DB */
  _ill  ,_nom,   _ill  ,_nom,   _lds  ,_dir,   _sts  ,_dir,     /* DC..DF */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* E0..E3 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* E4..E7 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* E8..EB */
  _ill  ,_nom,   _ill  ,_nom,   _lds  ,_ind,   _sts  ,_ind,     /* EC..EF */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* F0..F3 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* F4..F7 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* F8..FB */
  _ill  ,_nom,   _ill  ,_nom,   _lds  ,_ext,   _sts  ,_ext,     /* FC..FF */
  };

byte h6309_codes11[512] =
  {
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 00..03 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 04..07 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 08..0B */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 0C..0F */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 10..13 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 14..17 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 18..1B */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 1C..1F */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 20..23 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 24..27 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 28..2B */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 2C..2F */
  _band ,_bt ,   _biand,_bt ,   _bor  ,_bt ,   _bior ,_bt ,     /* 30..33 */
  _beor ,_bt ,   _bieor,_bt ,   _ldbt ,_bt ,   _stbt ,_bt ,     /* 34..37 */
  _tfm  ,_t1 ,   _tfm  ,_t2 ,   _tfm  ,_t3 ,   _tfm  ,_t4 ,     /* 38..3B */
  _bitmd,_imb,   _ldmd ,_imb,   _ill  ,_nom,   _swi3 ,_imp,     /* 3C..3F */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _come ,_imp,     /* 40..43 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 44..47 */
  _ill  ,_nom,   _ill  ,_nom,   _dece ,_imp,   _ill  ,_nom,     /* 48..4B */
  _ince ,_imp,   _tste ,_imp,   _ill  ,_nom,   _clre ,_imp,     /* 4C..4F */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _comf ,_imp,     /* 50..53 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 54..57 */
  _ill  ,_nom,   _ill  ,_nom,   _decf ,_imp,   _ill  ,_nom,     /* 58..5B */
  _incf ,_imp,   _tstf ,_imp,   _ill  ,_nom,   _clrf ,_imp,     /* 5C..5F */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 60..63 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 64..67 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 68..6B */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 6C..6F */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 70..73 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 74..77 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 78..7B */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 7C..7F */
  _sube ,_imb,   _cmpe ,_imb,   _ill  ,_nom,   _cmpu ,_imw,     /* 80..83 */
  _ill  ,_nom,   _ill  ,_nom,   _lde  ,_imb,   _ill  ,_nom,     /* 84..87 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _adde ,_imb,     /* 88..8B */
  _cmps ,_imw,   _divd ,_imb,   _divq ,_imw,   _muld ,_imw,     /* 8C..8F */
  _sube ,_dir,   _cmpe ,_dir,   _ill  ,_nom,   _cmpu ,_dir,     /* 90..93 */
  _ill  ,_nom,   _ill  ,_nom,   _lde  ,_dir,   _ste  ,_dir,     /* 94..97 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _adde ,_dir,     /* 98..9B */
  _cmps ,_dir,   _divd ,_dir,   _divq ,_dir,   _muld ,_dir,     /* 9C..9F */
  _sube ,_ind,   _cmpe ,_ind,   _ill  ,_nom,   _cmpu ,_ind,     /* A0..A3 */
  _ill  ,_nom,   _ill  ,_nom,   _lde  ,_ind,   _ste  ,_ind,     /* A4..A7 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _adde ,_ind,     /* A8..AB */
  _cmps ,_ind,   _divd ,_ind,   _divq ,_ind,   _muld ,_ind,     /* AC..AF */
  _sube ,_ext,   _cmpe ,_ext,   _ill  ,_nom,   _cmpu ,_ext,     /* B0..B3 */
  _ill  ,_nom,   _ill  ,_nom,   _lde  ,_ext,   _ste  ,_ext,     /* B4..B7 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _adde ,_ext,     /* B8..BB */
  _cmps ,_ext,   _divd ,_ext,   _divq ,_ext,   _muld ,_ext,     /* BC..BF */
  _subf ,_imb,   _cmpf ,_imb,   _ill  ,_nom,   _ill  ,_nom,     /* C0..C3 */
  _ill  ,_nom,   _ill  ,_nom,   _ldf  ,_imb,   _ill  ,_nom,     /* C4..C7 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _addf ,_imb,     /* C8..CB */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* CC..CF */
  _subf ,_dir,   _cmpf ,_dir,   _ill  ,_nom,   _ill  ,_nom,     /* D0..D3 */
  _ill  ,_nom,   _ill  ,_nom,   _ldf  ,_dir,   _stf  ,_dir,     /* D4..D7 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _addf ,_dir,     /* D8..DB */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* DC..DF */
  _subf ,_ind,   _cmpf ,_ind,   _ill  ,_nom,   _ill  ,_nom,     /* E0..E3 */
  _ill  ,_nom,   _ill  ,_nom,   _ldf  ,_ind,   _stf  ,_ind,     /* E4..E7 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _addf ,_ind,     /* E8..EB */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* EC..EF */
  _subf ,_ext,   _cmpf ,_ext,   _ill  ,_nom,   _ill  ,_nom,     /* F0..F3 */
  _ill  ,_nom,   _ill  ,_nom,   _ldf  ,_ext,   _stf  ,_ext,     /* F4..F7 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _addf ,_ext,     /* F8..FB */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* FC..FF */
  };

byte m6809_codes11[512] =
  {
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 00..03 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 04..07 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 08..0B */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 0C..0F */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 10..13 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 14..17 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 18..1B */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 1C..1F */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 20..23 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 24..27 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 28..2B */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 2C..2F */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 30..33 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 34..37 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 38..3B */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _swi3 ,_imp,     /* 3C..3F */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 40..43 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 44..47 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 48..4B */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 4C..4F */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 50..53 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 54..57 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 58..5B */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 5C..5F */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 60..63 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 64..67 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 68..6B */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 6C..6F */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 70..73 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 74..77 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 78..7B */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 7C..7F */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _cmpu ,_imw,     /* 80..83 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 84..87 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 88..8B */
  _cmps ,_imw,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 8C..8F */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _cmpu ,_dir,     /* 90..93 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 94..97 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 98..9B */
  _cmps ,_dir,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* 9C..9F */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _cmpu ,_ind,     /* A0..A3 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* A4..A7 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* A8..AB */
  _cmps ,_ind,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* AC..AF */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _cmpu ,_ext,     /* B0..B3 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* B4..B7 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* B8..BB */
  _cmps ,_ext,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* BC..BF */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* C0..C3 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* C4..C7 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* C8..CB */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* CC..CF */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* D0..D3 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* D4..D7 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* D8..DB */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* DC..DF */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* E0..E3 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* E4..E7 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* E8..EB */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* EC..EF */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* F0..F3 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* F4..F7 */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* F8..FB */
  _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,   _ill  ,_nom,     /* FC..FF */
  };

char *h6309_exg_tfr[] =
  {
  "D", "X", "Y", "U", "S", "PC","W" ,"V",
  "A", "B", "CC","DP","0", "0", "E", "F"
  };

char *m6809_exg_tfr[] =
  {
  "D", "X", "Y", "U", "S", "PC","??","??",
  "A", "B", "CC","DP","??","??","??","??"
  };

char *bit_r[] = {"CC","A","B","??"};

char *block_r[] =
  {
  "D","X","Y","U","S","?","?","?","?","?","?","?","?","?","?","?"
  };

char reg[] = { 'X', 'Y', 'U', 'S' };

byte *codes            = m6809_codes;
byte *codes10          = m6809_codes10;
byte *codes11          = m6809_codes11;
char **exg_tfr         = m6809_exg_tfr;
int  allow_6309_codes  = FALSE;
int  os9_patch         = FALSE;

/*****************************************************************************/
/* FlexLabel : returns FLEX defines for well-known addresses                 */
/*****************************************************************************/

char *FlexLabel(word addr)
{
if (!useFlex)                           /* if no FLEX labels in here         */
  return NULL;                          /* return at once                    */

switch (addr)
  {
                                        /* FLEX DOS entries                  */
  case 0xCD00: return "COLDS";
  case 0xCD03: return "WARMS";
  case 0xCD06: return "RENTER";
  case 0xCD09: return "INCH";
  case 0xCD0C: return "INCH2";
  case 0xCD0F: return "OUTCH";
  case 0xCD12: return "OUTCH2";
  case 0xCD15: return "GETCHR";
  case 0xCD18: return "PUTCHR";
  case 0xCD1B: return "INBUFF";
  case 0xCD1E: return "PSTRNG";
  case 0xCD21: return "CLASS";
  case 0xCD24: return "PCRLF";
  case 0xCD27: return "NXTCH";
  case 0xCD2A: return "RSTRIO";
  case 0xCD2D: return "GETFIL";
  case 0xCD30: return "LOAD";
  case 0xCD33: return "SETEXT";
  case 0xCD36: return "ADDBX";
  case 0xCD39: return "OUTDEC";
  case 0xCD3C: return "OUTHEX";
  case 0xCD3F: return "RPTERR";
  case 0xCD42: return "GETHEX";
  case 0xCD45: return "OUTADR";
  case 0xCD48: return "INDEC";
  case 0xCD4B: return "DOCMND";
  case 0xCD4E: return "STAT";
                                        /* FLEX FMS entries:                 */
  case 0xD400: return "FMSINI";         /* FMS init                          */
  case 0xD403: return "FMSCLS";         /* FMS close                         */
  case 0xD406: return "FMS";
  case 0xC840: return "FCB";            /* standard system FCB               */
    
                                        /* miscellaneous:                    */
  case 0xD435: return "VFYFLG";         /* FMS verify flag                   */
  case 0xC080: return "LINBUF";         /* line buffer                       */
  case 0xCC00: return "TTYBS";          /* TTYSET backspace character        */
  case 0xCC01: return "TTYDEL";         /* TTYSET delete character           */
  case 0xCC02: return "TTYEOL";         /* TTYSET EOL character              */
  case 0xCC03: return "TTYDPT";         /* TTYSET depth count                */
  case 0xCC04: return "TTYWDT";         /* TTYSET width count                */
  case 0xCC05: return "TTYNUL";         /* TTYSETnull count                  */
  case 0xCC06: return "TTYTAB";         /* TTYSET tab character              */
  case 0xCC07: return "TTYBSE";         /* TTYSET backspace echo character   */
  case 0xCC08: return "TTYEJ";          /* TTYSET eject count                */
  case 0xCC09: return "TTYPAU";         /* TTYSET pause control              */
  case 0xCC0A: return "TTYESC";         /* TTYSET escape character           */
  case 0xCC0B: return "SYSDRV";         /* current system drive              */
  case 0xCC0C: return "WRKDRV";         /* current working drive             */
  case 0xCC0E: return "SYSDAT";         /* System date month                 */
  case 0xCC0F: return "SYSDAT+1";       /* System date day                   */
  case 0xCC10: return "SYSDAT+2";       /* System date year                  */
  case 0xCC11: return "TTYTRM"; 
  case 0xCC12: return "COMTBL";         /* user command table                */
  case 0xCC14: return "LINBFP";         /* line buffer pointer               */
  case 0xCC16: return "ESCRET";         /* escape return register            */
  case 0xCC18: return "LINCHR";         /* current char in linebuffer        */
  case 0xCC19: return "LINPCH";         /* previous char in linebuffer       */
  case 0xCC1A: return "LINENR";         /* line nr of current page           */
  case 0xCC1B: return "LODOFS";         /* loader address offset             */
  case 0xCC1D: return "TFRFLG";         /* loader  transfer flag             */
  case 0xCC1E: return "TFRADR";         /* transfer address                  */
  case 0xCC20: return "FMSERR";         /* FMS error type                    */
  case 0xCC21: return "IOFLG";          /* special I/O flag                  */
  case 0xCC22: return "OUTSWT";         /* output switch                     */
  case 0xCC23: return "INSWT";          /* input switch                      */
  case 0xCC24: return "FOPADR";         /* file output address               */
  case 0xCC26: return "FIPADR";         /* file input address                */
  case 0xCC28: return "COMFLG";         /* command flag                      */
  case 0xCC29: return "OUTCOL";         /* current output column             */
  case 0xCC2A: return "SCRATC";         /* system scratch                    */
  case 0xCC2B: return "MEMEND";         /* memory end                        */
  case 0xCC2D: return "ERRVEC";         /* error name vector                 */
  case 0xCC2F: return "INECHO";         /* file input echo flag              */
    
                                        /* Printer support                   */
  case 0xCCC0: return "PRTINI";         /* printer initialize                */
  case 0xCCD8: return "PRTCHK";         /* printer check                     */
  case 0xCCE4: return "PRTOUT";         /* printer output                    */
                                        /* Console I/O Driver Table :        */
  case 0xd3e5: return "CINCHNE";        /* input character w/o echo          */
  case 0xd3e7: return "CIHNDLR";        /* IRQ interrupt handler             */
  case 0xd3e9: return "CSWIVEC";        /* SWI3 vector location              */
  case 0xd3eb: return "CIRQVEC";        /* IRQ vector location               */
  case 0xd3ed: return "CTMOFF";         /* timer off routine                 */
  case 0xd3ef: return "CTMON";          /* timer on routine                  */
  case 0xd3f1: return "CTMINT";         /* timer initialization              */
  case 0xd3f3: return "CMONITR";        /* monitor entry address             */
  case 0xd3f5: return "CTINIT";         /* terminal initialization           */
  case 0xd3f7: return "CSTAT";          /* check terminal status             */
  case 0xd3f9: return "COUTCH";         /* output character                  */
  case 0xd3fb: return "CINCH";          /* input character w/ echo           */
                                        /* Disk Driver Jump Table :          */
  case 0xde00: return "DDJ_READ";       /* read a single sector              */
  case 0xde03: return "DDJ_WRITE";      /* write a single sector             */
  case 0xde06: return "DDJ_VERIFY";     /* verify last sector written        */
  case 0xde09: return "DDJ_RESTORE";    /* restore head to track #0          */
  case 0xde0c: return "DDJ_DRIVE";      /* select the specified drive        */
  case 0xde0f: return "DDJ_CHKRDY";     /* Check for drive ready             */
  case 0xde12: return "DDJ_QUICK";      /* Quick check for drive ready       */
  case 0xde15: return "DDJ_INIT";       /* driver initialize (cold start)    */
  case 0xde18: return "DDJ_WARM";       /* driver initialize (warm start)    */
  case 0xde1b: return "DDJ_SEEK";       /* seek to specified track           */
    
  default:
    {
    static char lbp[20];
    if ((addr >= 0xc080) && (addr <= 0xc0ff))
      {
      if (defaultDataType == DATATYPE_HEX)
        sprintf(lbp, "LINBUF+$%02X", addr - 0xc080);
      else
        sprintf(lbp, "LINBUF+%d", addr - 0xc080);
      return lbp;
      }
    if ((addr >= 0xc840) && (addr <= 0xc97f))
      {
      if (defaultDataType == DATATYPE_HEX)
        sprintf(lbp, "FCB+$%02X", addr - 0xc840);
      else
        sprintf(lbp, "FCB+%d", addr - 0xc840);
      return lbp;
      }
    }
    return NULL;
  }
return NULL;
}

/*****************************************************************************/
/* GetPhaseDef : return the phase definition for a given address             */
/*****************************************************************************/

int GetPhaseDef(word addr)
{
int i;
for (i = 0; i < numphases; i++)
  if (addr >= phases[i].from && addr <= phases[i].to)
    return i;
return -1;
}

/*****************************************************************************/
/* PhaseInner : "phase" an address if it's inside the current range          */
/*****************************************************************************/

word PhaseInner(word W, word addr)
{
int i;
for (i = numphases - 1; i >= 0; i--)
  {
  word from = phases[i].from;
  if (addr >= from && addr <= phases[i].to)
    {
    word phase = phases[i].phase;
    word phend = phase + phases[i].to - phases[i].from;
    /* if we're dealing with a relative phase, fetch outer phase's boundaries */
    if (phases[i].rel)
      {
      from = phases[phases[i].phase].from;
      phase = phases[phases[i].phase].phase;
      phend = phase + phases[phases[i].phase].to - phases[phases[i].phase].from;
      }
    if ((W >= phase && W <= phend) ||
        phases[i].rel)                  /* relative phases are ALWAYS phased */
      return W - phase + from + phases[i].rel;
    }
  }
return W;
}

/*****************************************************************************/
/* DephaseOuter : "de-phase" an address if it's outside the current range    */
/*****************************************************************************/

word DephaseOuter(word W, word addr)
{
int i;
for (i = numphases - 1; i >= 0; i--)
  {
  word from = phases[i].from;
  word to = phases[i].to;
  if (addr >= from && addr <= to &&
      (W < from || W > to))
    {
    if (phases[i].rel)
      return W + phases[i].rel;
    else
      return W - from + phases[i].phase;
    }
  }
return W;
}

/*****************************************************************************/
/* AddLabel : adds a label to the list                                       */
/*****************************************************************************/

void AddLabel(int MI, word W)
{
byte nOr = AREATYPE_ULABEL | AREATYPE_LABEL | mnemo[MI].bCodeJump;
ATTRBYTE(W) |= nOr;
}

/*****************************************************************************/
/* AddFlexLabels : adds all flex labels to the list                          */
/*****************************************************************************/

void AddFlexLabels()
{
word addr;

if (!useFlex)                           /* if not using FLEX9 labels         */
  return;                               /* do nothing.                       */

for (addr = 0xc000; addr < 0xe000; addr++)
  {
  switch (addr)
    {
                                        /* FLEX DOS entries                  */
    case 0xCD00:
    case 0xCD03:
    case 0xCD06:
    case 0xCD09:
    case 0xCD0C:
    case 0xCD0F:
    case 0xCD12:
    case 0xCD15:
    case 0xCD18:
    case 0xCD1B:
    case 0xCD1E:
    case 0xCD21:
    case 0xCD24:
    case 0xCD27:
    case 0xCD2A:
    case 0xCD2D:
    case 0xCD30:
    case 0xCD33:
    case 0xCD36:
    case 0xCD39:
    case 0xCD3C:
    case 0xCD3F:
    case 0xCD42:
    case 0xCD45:
    case 0xCD48:
    case 0xCD4B:
    case 0xCD4E:
                                        /* FLEX FMS entries:                 */
    case 0xD400:
    case 0xD403:
    case 0xD406:
    case 0xC840:
                                        /* miscellaneous:                    */
    case 0xD435:
    case 0xC080:
    case 0xCC00:
    case 0xCC01:
    case 0xCC02:
    case 0xCC03:
    case 0xCC04:
    case 0xCC11:
    case 0xCC12:
    case 0xCC14:
    case 0xCC16:
    case 0xCC18:
    case 0xCC19:
    case 0xCC1A:
    case 0xCC1B:
    case 0xCC1D:
    case 0xCC1E:
    case 0xCC20:
    case 0xCC21:
    case 0xCC22:
    case 0xCC23:
    case 0xCC24:
    case 0xCC26:
    case 0xCC28:
    case 0xCC29:
    case 0xCC2A:
    case 0xCC2B:
    case 0xCC2D:
    case 0xCC2F:
    
                                        /* Printer support                   */
    case 0xCCC0:
    case 0xCCD8:
    case 0xCCE4:
                                        /* Console I/O Driver Table :        */
    case 0xd3e5:
    case 0xd3e7:
    case 0xd3e9:
    case 0xd3eb:
    case 0xd3ed:
    case 0xd3ef:
    case 0xd3f1:
    case 0xd3f3:
    case 0xd3f5:
    case 0xd3f7:
    case 0xd3f9:
    case 0xd3fb:
                                        /* Disk Driver Jump Table :          */
    case 0xde00:
    case 0xde03:
    case 0xde06:
    case 0xde09:
    case 0xde0c:
    case 0xde0f:
    case 0xde12:
    case 0xde15:
    case 0xde18:
    case 0xde1b:

      ATTRBYTE(addr) |= AREATYPE_LABEL;
      break;

    }
  }
}

/*****************************************************************************/
/* label_at : returns string if address has a label                          */
/*****************************************************************************/

char *label_at(word W)
{
char *p = lblnames[W];

if (!p)
 p = FlexLabel(W);
return p;
}

/*****************************************************************************/
/* index_parse : parses index for labels                                     */
/*****************************************************************************/

unsigned index_parse(int MI, unsigned pc)
{
byte T;
word W;
word Wrel;
char R;
unsigned PC = pc;
byte bSetLabel = 1;

T = ARGBYTE(PC);
PC++;
R = reg[(T>>5)&0x03];

if (T & 0x80)
  {
  switch(T & 0x1F)
    {
    case 0x00:                          /* register operations               */
    case 0x01:
    case 0x02:
    case 0x03:
    case 0x04:
    case 0x05:
    case 0x06:
    case 0x0B:
    case 0x11:
    case 0x13:
    case 0x14:
    case 0x15:
    case 0x16:
    case 0x1B:
      break;
    case 0x0C:
      bSetLabel = !IS_CONST(PC);
      T = ARGBYTE(PC); PC++;
      if (bSetLabel)
        AddLabel(MI, (word)((int)((char)T) + PC));
      break;
    case 0x08:
      T = ARGBYTE(PC);
      if (rels[PC])
        {
        W = (word)((int)((char)T));
        bSetLabel = !IS_CONST(PC);
        if (bSetLabel)
          AddLabel(MI, (word)(W + rels[PC]));
        }
      PC++;
      break;
    case 0x18:
    case 0x1C:
      T = ARGBYTE(PC);
      PC++;
      break;
    case 0x0D:
      bSetLabel = !IS_CONST(PC);
      W = ARGWORD(PC); PC += 2;
      if (bSetLabel)
        AddLabel(MI, (word)(W + PC));
      break;
    case 0x09:
      bSetLabel = !IS_CONST(PC);
      W = ARGWORD(PC);
      Wrel = W + rels[PC];
      PC += 2;
                                        /* no labels in indirect addressing! */
                                        /* ...except when they are explicitly*/
                                        /* given in the info file, of course */
      if ((W != Wrel) ||                /* if it's relative, or              */
          (label_at(Wrel)))             /* if there's a label given there    */
        AddLabel(MI, Wrel);             /* mark it as used                   */
      break;
    case 0x19:
    case 0x1D:
      bSetLabel = !IS_CONST(PC);
      W = ARGWORD(PC); PC += 2;
      if (bSetLabel)
        AddLabel(MI, W);
      break;
    case 0x07:
    case 0x17:
    case 0x0A:
    case 0x1A:
    case 0x0E:
    case 0x1E:
      if (allow_6309_codes)
        break;
      break;
             
    default:
      if (T == 0x9F)
        {
        bSetLabel = !IS_CONST(PC);
        W = ARGWORD(PC); PC += 2;
        if (bSetLabel)
          AddLabel(MI, W);
        }
      else if (allow_6309_codes)
        {
        switch (T)
          {
          case 0xAF:
          case 0xB0:
            bSetLabel = !IS_CONST(PC);
            W = ARGWORD(PC); PC += 2;
            if (bSetLabel)
              AddLabel(MI, W);
          default :
            break;
          }
        }
      break;
    }
  }
else
  {
  char c = T & 0x1F;
  if (c & 0x10)
    c |= 0xf0;
  if (rels[PC - 1])
    {
    bSetLabel = !IS_CONST(PC - 1);
    W = (word)(c + rels[PC - 1]);
    if (bSetLabel)
      AddLabel(MI, W);
    }
  }
return(PC);
}

/*****************************************************************************/
/* number_string : converts a number to a string in a variety of formats     */
/*****************************************************************************/

char * number_string(word W, int nDigits, word addr)
{
static char s[18];                      /* buffer for a binary word max.     */

if ((nDigits == 2) &&                   /* if 2-digit value                  */
    (IS_CHAR(addr)))                    /* and character output requested    */
  {
  if (isprint(W))
#if RB_VARIANT
    sprintf(s, "'%c'", W);
#else
    sprintf(s, "'%c", W);
#endif
  else
    sprintf(s, "$%02x", W);
  }
else if (IS_BINARY(addr))               /* if a binary                       */
  {
  int nBit;

  nDigits *= 4;                         /* convert from digits to bits       */
  s[0] = '%';                           /* prepare a binary value            */
                                        /* now do for all bits               */
  for (nBit = nDigits - 1; nBit >= 0; nBit--) 
    s[nDigits - nBit] = '0' + (!!(W & (1 << nBit)));
  s[nDigits + 1] = '\0';
  }
else if (IS_HEX(addr))                  /* if hex                            */
  sprintf(s, "$%0*X", nDigits, W);      /* prepare a hex value               */
else                                    /* otherwise                         */
  sprintf(s, "%d", W);                  /* prepare decimal value             */
return s;                               /* pass back generated string        */
}

/*****************************************************************************/
/* signed_string : converts a signed number to a string in some formats      */
/*****************************************************************************/

char *signed_string(int W, int nDigits, word addr)
{
static char s[19];
char *sp = s;

if (W < 0)
  {
  *sp++ = '-';
  W = -W;
  }
strcpy(sp, number_string((word)W, nDigits, addr));
return s;
}

/*****************************************************************************/
/* label_string : eventually converts a word to a string                     */
/*****************************************************************************/

char *label_string(word W, int bUseLabel, word addr)
{
static char szOut[256];
word Wrel = W + rels[addr];
char *p;
                                        /* get label name                    */
p = (bUseLabel) ? label_at(Wrel) : NULL;
if ((Wrel == W) && (p))                 /* if there and absolute             */
  return p;                             /* return it                         */

if (p)
  strcpy(szOut, p);
else if (bUseLabel && IS_CLABEL(Wrel))
  sprintf(szOut, "Z%04X", Wrel);
else if (bUseLabel && IS_DLABEL(Wrel))
  sprintf(szOut, "M%04X", Wrel);
else
  strcpy(szOut, number_string(Wrel, 4, addr));

if (Wrel != W)                          /* if it's relative addressing       */
  {
  char *pchk;
  int nInvert;
  word wDiff = Wrel - W;                /* get difference                    */

  Wrel = rels[addr];
  strcat(szOut, "-");
  nInvert = 1;
  pchk = szOut + strlen(szOut);
                                        /* get base name                     */
  p = (bUseLabel) ? label_at(Wrel) : NULL;
  if (p)
    strcpy(pchk, p);
  else if (bUseLabel && IS_CLABEL(Wrel))
    sprintf(pchk, "Z%04X", Wrel);
  else if (bUseLabel && IS_DLABEL(Wrel))
    sprintf(pchk, "M%04X", Wrel);
  else
    {
    if (wDiff & 0x8000)                 /* if negative displacement          */
      {
      *(pchk - 1) = '+';                /* negative*negative is positive...  */
      nInvert = 0;                      /* so invert the sign                */
      Wrel = (word) (-((short)Wrel));   /* and make the number positive      */
      }
    strcpy(pchk, number_string(Wrel, 4, addr));
    }

  if (nInvert)                          /* if inverting necessary,           */
    {
    while (*pchk)                       /* invert eventual signs!            */
      {
      if (*pchk == '+')
        *pchk = '-';
      else if (*pchk == '-')
        *pchk = '+';
      pchk++;
      }
    }
  }

return szOut;
}

/*****************************************************************************/
/* index_string : converts index to string                                   */
/*****************************************************************************/

unsigned index_string(char *buffer, unsigned pc)
{
byte T;
word W;
word Wrel;
char R;
char buf[256];
unsigned PC = pc;
byte bGetLabel;

T = ARGBYTE(PC);
PC++;
R = reg[(T >> 5) & 0x03];

if (T & 0x80)
  {
  switch (T & 0x1F)
    {
    case 0x00:
      sprintf(buf,",%c+", R);
      break;
    case 0x01:
      sprintf(buf,",%c++", R);
      break;
    case 0x02:
      sprintf(buf,",-%c", R);
      break;
    case 0x03:
      sprintf(buf,",--%c", R);
      break;
    case 0x04:
      sprintf(buf,",%c", R);
      if (rels[PC - 1])
        {
        bGetLabel = !IS_CONST(PC - 1);
        sprintf(buf + 2, "   %c (%s)", cCommChar,
                label_string(0, bGetLabel, (word)(PC - 1)));
        }
      break;
    case 0x05:
      sprintf(buf,"B,%c", R);
      break;
    case 0x06:
      sprintf(buf,"A,%c", R);
      break;
    case 0x08:
      bGetLabel = !IS_CONST(PC);
      T = ARGBYTE(PC);
      PC++;
      if (rels[PC - 1])
        {
        W = (int)((char)T) + rels[PC - 1];
                                        /* "<" needed for forward declaration*/
        sprintf(buf,
                ((W > PC) || (rels[PC - 1] > PC)) ? "<%s,%c" : "%s,%c",
                label_string((word)((int)((char)T)), bGetLabel, (word)(PC - 1)), R);
        }
      else
        sprintf(buf,"%s,%c",
                signed_string((int)((char)T), 2, (word)(PC - 1)),
                R);
      break;
    case 0x09:
      bGetLabel = !IS_CONST(PC);
      W = ARGWORD(PC);
      Wrel = W + rels[PC];
      PC += 2;
      if ((Wrel != W) || (label_at(Wrel)))
        {
        if ((W < 0x80) || (W >= 0xff80))
          sprintf(buf, "%s%s,%c", forceextendedaddr, label_string(W, bGetLabel, (word)(PC - 2)), R);
        else
          sprintf(buf, "%s,%c", label_string(W, bGetLabel, (word)(PC - 2)), R);
        }
      else
        {
        if ((W < 0x80) || (W >= 0xff80))
          sprintf(buf, "%s%s,%c", forceextendedaddr, signed_string((int)(short)W, 4, (word)(PC - 2)), R);
        else
          sprintf(buf, "%s,%c", number_string((word)(int)(short)W, 4, (word)(PC - 2)), R);  /* RB: this was "signed_string" */
        }
      break;
    case 0x0B:
      sprintf(buf,"D,%c",R);
      break;
    case 0x0C:
      T = ARGBYTE(PC);
      bGetLabel = !IS_CONST(PC);
      PC++;
#if 0
      sprintf(buf,"$%s,PC",signed_string((int)(char)T, 2, (word)(PC - 1)));
#else
      if (bGetLabel)
        sprintf(buf,"%s,PCR",label_string((word)((int)((char)T) + PC), bGetLabel, (word)(PC - 1)));
      else
        sprintf(buf,"%s,PC",number_string((word)(int)(char)T, 2, (word)(PC - 1)));
#endif
      break;
    case 0x0D:
      bGetLabel = !IS_CONST(PC);
      W = ARGWORD(PC);
      PC += 2;
      if ((W < 0x80) || (W >= 0xff80))
        sprintf(buf, "%s%s,PCR", forceextendedaddr, label_string((word)(W + PC), bGetLabel, (word)(PC - 2)));
      else
        sprintf(buf, "%s,PCR", label_string((word)(W + PC), bGetLabel, (word)(PC - 2)));
      break;
    case 0x11:
      sprintf(buf,"[,%c++]", R);
      break;
    case 0x13:
      sprintf(buf,"[,--%c]", R);
      break;
    case 0x14:
      sprintf(buf,"[,%c]", R);
      break;
    case 0x15:
      sprintf(buf,"[B,%c]", R);
      break;
    case 0x16:
      sprintf(buf,"[A,%c]", R);
      break;
    case 0x18:
      T = ARGBYTE(PC);
      PC++;
      sprintf(buf,"[%s,%c]",
              number_string(T, 2, (word)(PC - 1)),
              R);
      break;
    case 0x19:
      bGetLabel = !IS_CONST(PC);
      W = ARGWORD(PC);
      PC += 2;
      sprintf(buf,"[%s,%c]",label_string(W, bGetLabel, (word)(PC - 2)),R);
      break;
    case 0x1B:
      sprintf(buf,"[D,%c]",R);
      break;
    case 0x1C:
      T = ARGBYTE(PC);
      PC++;
      sprintf(buf,"[%s,PC]",number_string(T, 2, (word)(PC - 1)));
      break;
    case 0x1D:
      bGetLabel = !IS_CONST(PC);
      W = ARGWORD(PC);
      PC += 2;
      sprintf(buf,"[%s,PC]", label_string(W, bGetLabel, (word)(PC - 2)));
      break;
    case 0x07:
      if(allow_6309_codes)
        {
        sprintf(buf,"E,%c",R);
        break;
        }
      else
        goto index_error;
    case 0x17:
      if (allow_6309_codes)
        {
        sprintf(buf,"[E,%c]",R);
        break;
        }
      else
        goto index_error;
    case 0x0A:
      if (allow_6309_codes)
        {
        sprintf(buf,"F,%c",R);
        break;
        }
      else
        goto index_error;
    case 0x1A:
      if (allow_6309_codes)
        {
        sprintf(buf,"[F,%c]",R);
        break;
        }
      else
        goto index_error;
    case 0x0E:
      if (allow_6309_codes)
        {
        sprintf(buf,"W,%c",R);
        break;
        }
      else
        goto index_error;
    case 0x1E:
      if (allow_6309_codes)
        {
        sprintf(buf,"[W,%c]",R);
        break;
        }
      else
        goto index_error;
      
    index_error:
      sprintf(buf,"???");
      break;
             
    default:
      if (T == 0x9F)
        {
        bGetLabel = !IS_CONST(PC);
        W = ARGWORD(PC);
        PC += 2;
        sprintf(buf,"[%s]", label_string(W, bGetLabel, (word)(PC - 2)));
        }
      else if (allow_6309_codes)
        {
        switch (T)
          {
          case 0x8F:
            sprintf(buf,",W");
            break;
          case 0x90:
            sprintf(buf,"[,W]");
            break;
          case 0xAF:
            bGetLabel = !IS_CONST(PC);
            W = ARGWORD(PC);
            PC += 2;
            sprintf(buf,"%s,W", label_string(W, bGetLabel, (word)(PC - 2)));
            break;
          case 0xB0:
            bGetLabel = !IS_CONST(PC);
            W = ARGWORD(PC);
            PC += 2;
            sprintf(buf,"[%s,W]", label_string(W, bGetLabel, (word)(PC - 2)));
            break;
          case 0xCF:
            sprintf(buf,",W++");
            break;
          case 0xD0:
            sprintf(buf,"[,W++]");
            break;
          case 0xEF:
            sprintf(buf,",--W");
            break;
          case 0xF0:
            sprintf(buf,"[,--W]");
            break;
          default:
            sprintf(buf,"???");
            break;
          }
        }
      else
        sprintf(buf,"???");
      break;
    }
    
  }
else
  {
  char c = T & 0x1F;
  if (c & 0x10)
    c |= 0xf0;
  if (rels[PC - 1])
    {
    bGetLabel = !IS_CONST(PC - 1);
    sprintf(buf, "%s,%c", label_string((word)c, bGetLabel, (word)(PC - 1)), R);
    }
  else
    sprintf(buf,"%s,%c", signed_string(c, 2, (word)(PC - 1)), R);
  }
  
strcat(buffer,buf);
return(PC);
}

/*****************************************************************************/
/* GetDirPage : returns the direct page set for a given address              */
/*****************************************************************************/

int GetDirPage(int nAddr)
{
int nDP = dps[nAddr];
if (nDP > 0xff)
  nDP = dirpage;
return nDP;
}

/*****************************************************************************/
/* Parse : parses an instruction for jump / data labels                      */
/*****************************************************************************/

unsigned Parse(unsigned pc)
{
byte O,T,M;
word W;
int MI;
char *I;
unsigned PC = pc;
byte bSetLabel = 1;
int dp = GetDirPage(pc);

O = T = OPCODE(PC);
PC++;

if ((codes10) && (T == 0x10))
  {
  T = OPCODE(PC); PC++;
  W = (word)(T*2);
  MI = T = codes10[W++];
  I = (char *)mnemo[T].mne;
  M = codes10[W];
  
  if ((T == _swi2) && (os9_patch == TRUE))
    {
    T = OPCODE(PC); PC++;
    return (PC-pc);
    }
  }
else if ((codes11) && (T == 0x11))
  {
  T = OPCODE(PC); PC++;
  W = (word)(T*2);
  MI = T = codes11[W++];
  I = (char *)mnemo[T].mne;
  M = codes11[W];
  }
else
  {
  W = (word)(T*2);
  MI = T = codes[W++];
  I = (char *)mnemo[T].mne;
  M = codes[W];
  }

switch(M)
  {
  case _nom:
    PC = pc + 1;
    break;

  case _imp:
    break;

  case _imb:
    T = ARGBYTE(PC); PC++;
    break;

  case _imw:
    bSetLabel = !IS_CONST(PC);
    W = ARGWORD(PC); PC+=2;
    if (bSetLabel)
      {
      W = PhaseInner(W, (word)(PC - 2));
      AddLabel(MI, W);
      }
    break;

  case _dir:
    bSetLabel = !IS_CONST(PC);
    T = ARGBYTE(PC);
    PC++;
    if (dp >= 0)
      {
      W = (word)((dp << 8) | T);
      if (bSetLabel)
        {
        W = PhaseInner(W, (word)(PC - 1));
        AddLabel(MI, W);
        }
      }
    break;

  case _ext:
    bSetLabel = !IS_CONST(PC);
    W = ARGWORD(PC); PC += 2;
    if (bSetLabel)
      {
      W = PhaseInner(W, (word)(PC - 2));
      AddLabel(MI, W);
      }
    break;
    
  case _ind:
    PC = index_parse(MI,PC);
    break;
    
  case _ix8:
    PC++;
    break;

  case _reb:
    bSetLabel = !IS_CONST(PC);
    T = ARGBYTE(PC); PC++;
    W = (word)(PC + (signed char)T);
    if (bSetLabel)
      {
      W = DephaseOuter(W, (word)(PC - 1));
      AddLabel(MI, W);
      }
    break;
    
  case _rew:
    bSetLabel = !IS_CONST(PC);
    W = ARGWORD(PC); PC += 2;
    W += (word)PC;
    if (bSetLabel)
      {
      W = DephaseOuter(W, (word)(PC - 2));
      AddLabel(MI, W);
      }
    break;
    
  case _r1:
    T = ARGBYTE(PC); PC++;
    break;

  case _r2:
  case _r3:
    T = ARGBYTE(PC); PC++;
    break;
    
  case _bd:
    M = ARGBYTE(PC); PC++;
    bSetLabel = !IS_CONST(PC);
    T = ARGBYTE(PC);
    PC++;
    if (dp >= 0)
      {
      W = (word)((dp << 8) | T);
      if (bSetLabel)
        {
        W = PhaseInner(W, (word)(PC - 1));
        AddLabel(MI, W);
        }
      }
    break;

  case _be:
    T = ARGBYTE(PC); PC++;
    bSetLabel = !IS_CONST(PC);
    W = ARGWORD(PC); PC+=2;
    if (bSetLabel)
      {
      W = PhaseInner(W, (word)(PC - 2));
      AddLabel(MI, W);
      }
    break;
    
  case _bt:
    M = ARGBYTE(PC); PC++;
    T = ARGBYTE(PC); PC++;
    break;

  case _t1:
    T = ARGBYTE(PC); PC++;
    break;
    
  case _t2:
    T = ARGBYTE(PC); PC++;
    break;
    
  case _t3:
    T = ARGBYTE(PC); PC++;
    break;
    
  case _t4:
    T = ARGBYTE(PC); PC++;
    break;
    
  case _iml:
    W = ARGWORD(PC); PC+=2;
    T = ARGBYTE(PC); PC++;
    M = ARGBYTE(PC); PC++;
    break;
    
  case _bi:
    T = ARGBYTE(PC); PC++;
    PC = index_parse(MI,PC);
    break;
    
  default:
    break;
  }

return (PC - pc);
}

/*****************************************************************************/
/* Dasm : disassemble an instruction                                         */
/*****************************************************************************/

unsigned Dasm (char *buffer, unsigned pc)
{
byte O,T,M;
word W;
char *I;
char buf[256];
unsigned PC = pc;
char bGetLabel;
int dp = GetDirPage(pc);

O = T = OPCODE(PC);
PC++;

if ((codes10) && (T == 0x10))
  {
  T = OPCODE(PC); PC++;
  W = (word)(T*2);
  T = codes10[W++];
  I = (char *)mnemo[T].mne;
  M = codes10[W];
  
  if( (T == _swi2) && (os9_patch == TRUE) )
    {
    T = OPCODE(PC); PC++;
    sprintf(buffer, "OS9 %s", os9_codes[T]);
    return (PC-pc);
    }
  }
else if ((codes11) && (T == 0x11))
  {
  T = OPCODE(PC); PC++;
  W = (word)(T*2);
  T = codes11[W++];
  I = (char *)mnemo[T].mne;
  M = codes11[W];
  }
else
  {
  W = (word)(T*2);
  T = codes[W++];
  I = (char *)mnemo[T].mne;
  M = codes[W];
  }

switch (M)
  {
  case _nom:
    sprintf(buffer, "%-7s %s", "FCB", number_string(O, 2, (word)PC));
    PC = pc + 1;
    break;

  case _imp:
    if (useConvenience &&
        !IS_LABEL(PC) &&
        !allow_6309_codes)              /* on a 6309, these are REAL opcodes!*/
      W = (word)(O << 8) | OPCODE(PC);
    else
      W = 0;
    switch (W)
      {
      case 0x4456 :                     /* LSRA + RORB                       */
        sprintf(buffer, "%s", mnemo[_lsrd].mne); PC++;
        break;
      case 0x5849 :                     /* ASLB + ROLA                       */
        sprintf(buffer, "%s", mnemo[_asld].mne); PC++;
        break;
      default :
        sprintf(buffer,"%s", I);
        break;
      }
    break;

  case _imb:
    T = ARGBYTE(PC);
    PC++;
    if (useConvenience)
      W = (word)(O << 8) | T;
    else
      W = 0;
    switch (W)                          /* examine for special CC settings   */
      {
      case 0x1a01 :                     /* (6809) ORCC $01                   */
        sprintf(buffer, "%s", mnemo[_sec].mne);
        break;
      case 0x1a02 :                     /* (6809) ORCC $02                   */
        sprintf(buffer, "%s", mnemo[_sev].mne);
        break;
      case 0x1a04 :                     /* (6809) ORCC $04                   */
        sprintf(buffer, "%s", mnemo[_sez].mne);
        break;
      case 0x1a10 :                     /* (6809) ORCC $10                   */
        sprintf(buffer, "%s", mnemo[_sei].mne);
        break;
      case 0x1a40 :                     /* (6809) ORCC $40                   */
        sprintf(buffer, "SEF");
        break;
      case 0x1a50 :                     /* (6809) ORCC $40+$10               */
        sprintf(buffer, "SEIF");
        break;
      case 0x1cfe :                     /* (6809) ANDCC ~$01                 */
        sprintf(buffer, "%s", mnemo[_clc].mne);
        break;
      case 0x1cfd :                     /* (6809) ANDCC ~$02                 */
        sprintf(buffer, "%s", mnemo[_clv].mne);
        break;
      case 0x1cfb :                     /* (6809) ANDCC ~$04                 */
        sprintf(buffer, "CLZ");
        break;
      case 0x1cef :                     /* (6809) ANDCC ~$10                 */
        sprintf(buffer, "%s", mnemo[_cli].mne);
        break;
      case 0x1cbf :                     /* (6809) ANDCC ~$40                 */
        sprintf(buffer, "CLF");
        break;
      case 0x1caf :                     /* (6809) ANDCC ~($40 + $10)         */
        sprintf(buffer, "CLIF");
        break;
      case 0x3cff :                     /* (6809) CWAI $FF                   */
        sprintf(buffer, mnemo[_wai].mne);
        break;
      default :
        sprintf(buffer, "%-7s #%s", I, number_string(T, 2, (word)(PC - 1)));
      }
    break;

  case _imw:
    bGetLabel = !IS_CONST(PC);
    W = ARGWORD(PC);
    PC += 2;
    if (bGetLabel)
      W = PhaseInner(W, (word)(PC - 2));
    sprintf(buffer,"%-7s #%s", I, label_string(W, bGetLabel, (word)(PC - 2)));
    break;

  case _dir:
    bGetLabel = !IS_CONST(PC);
    T = ARGBYTE(PC);
    PC++;
    if (dp >= 0)
      {
      W = (word)((dp << 8) | T);
      if (bGetLabel)
        W = PhaseInner(W, (word)(PC - 1));
      sprintf(buffer, "%-7s %s", I, label_string(W, bGetLabel, (word)(PC - 1)));
      }
    else
      sprintf(buffer, "%-7s <%s", I, number_string(T, 2, (word)(PC - 1)));
    break;

  case _ext:
    bGetLabel = !IS_CONST(PC);
    W = ARGWORD(PC);
    if (bGetLabel)
      W = PhaseInner(W, (word)PC);
    PC += 2;
    if ((dp >= 0) &&
        ((W & (word)0xff00) == ((word)dp << 8)))
      sprintf(buffer,"%-7s %s%s", I, forceextendedaddr, label_string(W, bGetLabel, (word)(PC - 2)));
    else
      sprintf(buffer,"%-7s %s", I, label_string(W, bGetLabel, (word)(PC - 2)));
    break;

  case _ind:                            /* 6809 / 6309 only                  */
    if (useConvenience)
      W = (word)(O << 8) | OPCODE(PC);
    else
      W = 0;
    switch (W)
      {
      case 0x3001 :                     /* (6809) LEAX +1                    */
        sprintf(buffer, "%s", mnemo[_inx].mne); PC++;
        break;
      case 0x301f :                     /* (6809) LEAX -1                    */
        sprintf(buffer, "%s", mnemo[_dex].mne); PC++;
        break;
      case 0x3121 :                     /* (6809) LEAY +1                    */
        sprintf(buffer, "INY"); PC++;
        break;
      case 0x313f :                     /* (6809) LEAY -1                    */
        sprintf(buffer, "DEY"); PC++;
        break;
      case 0x3261 :                     /* (6809) LEAS +1                    */
        sprintf(buffer, "%s", mnemo[_ins].mne); PC++;
        break;
      case 0x327f :                     /* (6809) LEAS -1                    */
        sprintf(buffer, "%s", mnemo[_des].mne); PC++;
        break;
      case 0x3341 :                     /* (6809) LEAU +1                    */
        sprintf(buffer, "INU"); PC++;
        break;
      case 0x335f :                     /* (6809) LEAU -1                    */
        sprintf(buffer, "DEU"); PC++;
        break;
      default :
        sprintf(buffer,"%-7s ", I);
        PC = index_string(buffer, PC);
        break;
      }
    break;
    
  case _ix8:
    bGetLabel = !IS_CONST(PC);
    T = ARGBYTE(PC);
    PC++;

    if (rels[PC - 1])
      {
      W = (int)((unsigned char)T) + rels[PC - 1];
      sprintf(buf, "%s,X",
              label_string((word)((int)((unsigned char)T)), bGetLabel, (word)(PC - 1)));
      }
     /* omit '$00', unless the user has set the 'showzero' option */
    else if (!T && !showIndexedModeZeroOperand)
      strcpy(buf, ",X");
    else
      sprintf(buf, "%s,X",
              number_string((word)((unsigned char)T), 2, (word)(PC - 1)));

    sprintf(buffer, "%-7s %s", I, buf);
    break;

  case _reb:
    bGetLabel = !IS_CONST(PC);
    T = ARGBYTE(PC);
    PC++;
    if (bGetLabel)
      {
      W = (word)(PC + (signed char)T);
      W = DephaseOuter(W, (word)(PC - 1));
      sprintf(buffer,"%-7s %s", I, label_string(W, bGetLabel, (word)(PC - 1)));
      }
    else
      {
      int nDiff = (int)(signed char)T;
      sprintf(buffer,"%-7s *%s%s",
              I,
              (nDiff >= 0) ? "+" : "",
              signed_string(nDiff + 2, 2, (word)(PC - 1)));
      }
    break;
    
  case _rew:
    bGetLabel = !IS_CONST(PC);
    W = ARGWORD(PC);
    PC += 2;
    W += (word)PC;
    if (bGetLabel)
      W = DephaseOuter(W, (word)(PC - 2));
    sprintf(buffer,"%-7s %s", I, label_string(W, bGetLabel, (word)(PC - 2)));
    break;
    
  case _r1:
    if (useConvenience)
      W = (word)(O << 8) | OPCODE(PC);
    else
      W = 0;
    switch (W)
      {
      case 0x1f14 :                     /* (6809) TFR X,S                    */
        sprintf(buffer, "%s", mnemo[_txs].mne); PC++;
        break;
      case 0x1f41 :                     /* (6809) TFR S,X                    */
        sprintf(buffer, "%s", mnemo[_tsx].mne); PC++;
        break;
/* hairy - some assemblers expand TAB to TAB + TSTA...
   but there's no guarantee.
      case 0x1f89 :
        sprintf(buffer, "TAB"); PC++;
        break; */
      case 0x1f8a :                     /* (6809) TFR A,CC                   */
        sprintf(buffer, "%s", mnemo[_tap].mne); PC++;
        break;
/* hairy - some assemblers expand TBA to TBA + TSTA...
   but there's no guarantee.
      case 0x1f98 :
        sprintf(buffer, "TBA"); PC++;
        break; */
      case 0x1fa8 :                     /* (6809) TFR CC,A                   */
        sprintf(buffer, "%s", mnemo[_tpa].mne); PC++;
        break;
      default :
        T = ARGBYTE(PC); PC++;
        sprintf(buffer,"%-7s %s,%s", I, exg_tfr[T >> 4], exg_tfr[T & 0xF]);
        break;
      }
    break;

  case _r2:                             /* 6809/6309 only                    */
  case _r3:                             /* -"-                               */
    if (useConvenience)
      W = (word)(O << 8) | OPCODE(PC);
    else
      W = 0;
    switch (W)
      {
      case 0x3404 :                     /* (6809) PSHS B                     */
        if (IS_LABEL(PC + 1))
          W = 0;
        else
          W = (word)(OPCODE(PC + 1) << 8) | OPCODE(PC + 2);
        switch (W)
          {
          case 0xa0e0 :                 /* (6809) PSHS B / SUBA ,S++         */
            sprintf(buffer, "%s", mnemo[_sba].mne); PC += 3;
            break;
          case 0xa1e0 :                 /* (6809) PSHS B / CMPA ,S++         */
            sprintf(buffer, "%s", mnemo[_cba].mne); PC += 3;
            break;
          case 0xabe0 :                 /* (6809) PSHS B / ADDA ,S++         */
            sprintf(buffer, "%s", mnemo[_aba].mne); PC += 3;
            break;
          default:                      /* (6809) PSHS B / anything else     */
            sprintf(buffer, "%s", mnemo[_pshb].mne); PC++;
            break;
          }
        break;
      case 0x3402 :                     /* (6809) PSHS A                     */
        sprintf(buffer, "%s", mnemo[_psha].mne); PC++;
        break;
      case 0x3406 :                     /* (6809) PSHS D                     */
        sprintf(buffer, "PSHD"); PC++;
        break;
      case 0x3410 :                     /* (6809) PSHS X                     */
        sprintf(buffer, "PSHX"); PC++;
        break;
      case 0x3420 :                     /* (6809) PSHS Y                     */
        sprintf(buffer, "PSHY"); PC++;
        break;
      case 0x3502 :                     /* (6809) PULS A                     */
        sprintf(buffer, "%s", mnemo[_pula].mne); PC++;
        break;
      case 0x3504 :                     /* (6809) PULS B                     */
        sprintf(buffer, "%s", mnemo[_pulb].mne); PC++;
        break;
      case 0x3506 :                     /* (6809) PULS D                     */
        sprintf(buffer, "PULD"); PC++;
        break;
      case 0x3510 :                     /* (6809) PULS X                     */
        sprintf(buffer, "PULX"); PC++;
        break;
      case 0x3520 :                     /* (6809) PULS Y                     */
        sprintf(buffer, "PULY"); PC++;
        break;
      default:
        buf[0] = '\0';
        T = ARGBYTE(PC);
        PC++;
        if (T & 0x80)
          strcat(buf,"PC,");
        if (T & 0x40)
          {
          if (M == _r2)
            strcat(buf,"U,");
          if (M == _r3)
            strcat(buf,"S,");
          }
        if (T&0x20)
          strcat(buf,"Y,");
        if (T & 0x10)
          strcat(buf,"X,");
        if (T & 0x08)
          strcat(buf,"DP,");
        if ((T & 0x06) == 0x06)
          strcat(buf, "D,");
        else
          {
          if (T & 0x04)
            strcat(buf,"B,");
          if (T & 0x02)
            strcat(buf,"A,");
          }
        if (T & 0x01)
          strcat(buf,"CC,");
        if (buf[0] != '\0')
          buf[strlen(buf)-1]='\0';
        sprintf(buffer,"%-7s %s", I, buf);
      break;
      }
    break;
    
  case _bd:
    M = ARGBYTE(PC); PC++;
    bGetLabel = !IS_CONST(PC);
    T = ARGBYTE(PC);
    PC++;
    if (dp >= 0)
      {
      char mBuf[20];
      W = (word)((dp << 8) | T);
      if (bGetLabel)
        W = PhaseInner(W, (word)(PC - 1));
      strcpy(mBuf, number_string(M, 2, (word)(PC - 2)));
      sprintf(buffer,
              "%-7s #%s,%s",
              I,
              mBuf,
              label_string(W, bGetLabel, (word)(PC - 1)));
      }
    else
      {
      char tBuf[20];
      strcpy(tBuf, number_string(T, 2, (word)(PC - 1)));
      sprintf(buffer,
              "%-7s #%s,<%s",
              I,
              number_string(M, 2, (word)(PC - 2)),
              tBuf);
      }
    break;

  case _be:
    T = ARGBYTE(PC);
    PC++;
    bGetLabel = !IS_CONST(PC);
    W = ARGWORD(PC);
    if (bGetLabel)
      W = PhaseInner(W, (word)PC);
    PC += 2;
    {
    char tBuf[20];
    strcpy(tBuf, number_string(T, 2, (word)(PC - 3)));
    if ((dp >= 0) &&
        ((W & (word)0xff00) == ((word)dp << 8)))
      sprintf(buffer,
              "%-7s #%s,%s%s",
              I,
              tBuf,
              forceextendedaddr, label_string(W, bGetLabel, (word)(PC - 2)));
    else
      sprintf(buffer,
              "%-7s #%s,%s",
              I,
              tBuf,
              label_string(W, bGetLabel, (word)(PC - 2)));
    }
    break;
    
  case _bt:
    M = ARGBYTE(PC); PC++;
    T = ARGBYTE(PC); PC++;
    sprintf(buffer,
            "%-7s %s,%d,%d,%s%s",
            I,
            bit_r[M >> 6],
            (M >> 3) & 7,
            M & 7,
            forcedirectaddr, number_string(T, 2, (word)(PC - 1)));
    break;

  case _t1:
    T = ARGBYTE(PC); PC++;
    sprintf(buffer, "%-7s %s+,%s+", I, block_r[T >> 4], block_r[T & 0xF]);
    break;
    
  case _t2:
    T = ARGBYTE(PC); PC++;
    sprintf(buffer,"%-7s %s-,%s-", I, block_r[T >> 4], block_r[T & 0xF]);
    break;
    
  case _t3:
    T = ARGBYTE(PC); PC++;
    sprintf(buffer,"%-7s %s+,%s", I, block_r[T >> 4], block_r[T & 0xF]);
    break;
    
  case _t4:
    T = ARGBYTE(PC); PC++;
    sprintf(buffer,"%-7s %s,%s+", I, block_r[T >> 4], block_r[T & 0xF]);
    break;
    
  case _iml:
    W = ARGWORD(PC); PC += 2;
    T = ARGBYTE(PC); PC++;
    M = ARGBYTE(PC); PC++;
    sprintf(buffer,"%-7s #$%04X%02X%02X", I, W, T, M);
    break;
    
  case _bi:
    T = ARGBYTE(PC); PC++;
    sprintf(buffer,"%-7s #%s,", I, number_string(T, 2, (word)(PC - 1)));
    PC = index_string(buffer,PC);
    break;
    
  default:
    sprintf(buffer,"%-7s ERROR",I);
  }

/* increased readability: divider after jumps (but not subroutine calls)     */
if (!strcmp(I,"RTI") ||
    !strcmp(I,"RTS") ||
    (!strncmp(I, "PUL", 3) && (T & 0x80)) ||  /* PULx PC */
    !strncmp(I, "JMP", 3) ||
    !strncmp(I, "BRA", 3) ||
    !strncmp(I," SWI", 3) )
  optdelimbar = TRUE;

return (PC - pc);
}

/*****************************************************************************/
/* ParseData : parses data area for labels                                   */
/*****************************************************************************/

unsigned ParseData(unsigned pc)
{
if (IS_WORD(pc))                        /* if WORD data                      */
  {
  if (!IS_CONST(pc))
    {
    word wo = ARGWORD(pc);
    if ((FlexLabel(wo)) ||              /* if predefined FLEX label          */
        (lblnames[wo]))                 /* or there's a label for the addr   */
      ATTRBYTE(wo) |= AREATYPE_ULABEL;  /* mark it as used                   */
    }
  return 2;
  }
return 1;
}

/*****************************************************************************/
/* ShowMemFlags : returns a set of flags for a given byte that's to be shown */
/*****************************************************************************/

#define SHMF_DATA    1
#define SHMF_RMB     2
#define SHMF_TXT     4
#define SHMF_WORD    8
#define SHMF_LABEL   16
#define SHMF_COMMENT 32

byte ShowMemFlags(unsigned pc)
{
byte wf = 0;

if ((IS_CODE(pc)) ||                    /* code needs no further inspection  */
    (!IS_USED(pc)))
  return wf;

wf |= SHMF_DATA;                        /* assemble flags for data byte      */

if (IS_RMB(pc))
  wf |= SHMF_RMB;
else if (IS_WORD(pc))
  wf |= SHMF_WORD;
else if (IS_BINARY(pc))
  ;
else
#if RB_VARIANT
    if (IS_CHAR(pc))
#endif
  {
  if ((memory[pc] >= 0x20) &&
      (memory[pc] <= 0x7e) &&
      (memory[pc] != '&') &&
      (memory[pc] != '\"') &&
      (!IS_CONST(pc)))
    wf |= SHMF_TXT;
  }

if (IS_ULABEL(pc) || IS_BREAK(pc))
  wf |= SHMF_LABEL;

if ((commentlines[pc]) ||
    (lcomments[pc]))
  wf |= SHMF_COMMENT;

return wf;
}

/*****************************************************************************/
/* ShowData : shows a block of data (up to 31 chars total output length)     */
/*****************************************************************************/

unsigned ShowData(FILE *out, unsigned pc, int nComment)
{
int i, j, end, max = (nComment ? 24 : 52);
byte wfCur, wfEnd;

wfCur = ShowMemFlags(pc) &              /* get flags for current byte        */
        (~(SHMF_LABEL | SHMF_COMMENT)); /* without label & comment flags     */

for (end = pc + 1; ; end++)             /* find end of block                 */
  {
  if(IS_LABEL(end))                     /* RB: don't overrun labels ...      */
    {                                   /* HS: unless they're displacements! */
    char *slabel = label_string((word)end, 1, (word)end);
    if (!strchr(slabel, '+') && !strchr(slabel, '-'))
      break;
    }
  wfEnd = ShowMemFlags(end);
#if 0
  if ((wfEnd & SHMF_TXT) &&             /* suppress ONE text character       */
      ((wfEnd & ~SHMF_TXT) == wfCur) && /* amidst non-text characters        */
      (wfCur == ShowMemFlags(end + 1)))
    wfEnd &= ~SHMF_TXT;
#endif
  if (wfEnd != wfCur)
    break;
  }

if (wfCur & SHMF_RMB)                   /* if reserved memory block          */
  {
  fprintf(out, "%-7s ", "RMB");
  i = end;
  j = fprintf(out, "%s", number_string((word)(i - pc), 4, (word)pc));
  }
else if (usefcc && (wfCur & SHMF_TXT))  /* if FCC (text)                     */
  {
  fprintf(out, "%-7s \"", "FCC");       /* start the game                    */
  for (i = pc, j = 1; i < end; i++)     /* now print out the characters      */
    {
    if (j + 2 >= max)                   /* if enough already                 */
      break;
    j += fprintf(out, "%c", memory[i]); /* print a character                 */
    }
  j += fprintf(out, "\"");              /* append delimiter                  */
  }
else if (wfCur & SHMF_WORD)             /* if word data                      */
  {
  char *s;
  if ((end - pc) & 1)                   /* if not word size                  */
    end--;                              /* don't use last byte               */
  if (!(end - pc))                      /* if only one word byte, assume 2   */
    end += 2;

  fprintf(out, "%-7s ", "FDB");         /* start the game                    */
  for (i = pc, j = 0; i < end; i += 2)  /* now print out the words           */
    {
    s = label_string(ARGWORD(i), !IS_CONST(i), (word)i);
    if (i - pc)                         /* if not on 1st one and             */
      {
      if (j + (int)strlen(s) + 1 >= max)/* and it would go too far           */
        break;                          /* terminate the loop                */
      j += fprintf(out, ",");           /* prepend a separator               */
      }
    j += fprintf(out, "%s", s);         /* now print the word/label          */
    }
  }
else                                    /* if FCB (hex or binary)            */
  {
  char *s;
  fprintf(out, "%-7s ", "FCB");         /* start the game                    */
  for (i = pc, j = 0; i < end; i++)     /* now print out the characters      */
    {
    s = number_string(memory[i], 2, (word)i);
    if (i - pc)                         /* if not on 1st one and             */
      {
      if (j + (int)strlen(s) + 1 >= max)/* and it would go too far           */
        break;                          /* terminate the loop                */
      j += fprintf(out, ",");           /* prepend a separator               */
      }
    j += fprintf(out, "%s", s);         /* append the byte's representation  */
    }
  }

if (nComment && (24 - j > 0))           /* if comment to follow and space    */
  fprintf(out, "%*s", 24 - j, "");      /* fill up space                     */
return i - pc;                          /* and stop here                     */
}

/*****************************************************************************/
/* infousage : displays help on info file                                    */
/*****************************************************************************/

void infousage(void)
{
printf("\n"
       "Info file contents:\n"
#if RB_VARIANT
      "\nLabel file comments\n"
       "\t* comment line\n"
       "\t; comment line\n"
      "\nOutput control\n"
       "\tPREPEND [addr[-addr]] text to be prepended to the output\n"
       "\tPREPCOMM [addr[-addr]] comment text to be prepended to the output\n"
      "\nMemory content definitions\n"
       "\tunused area:        UNUSED from-to\n"
       "\treserved area:      RMB from-to\n"
       "\tcode area:          CODE from[-to]\n"
       "\tdata area:          DATA from[-to] (default: hex byte data)\n"
       "\tbinary data area:   BIN[ARY] from[-to]\n"
       "\tdecimal data area:  DEC[IMAL] from[-to]\n"
       "\thex data area:      HEX[ADECIMAL] from[-to]\n"
       "\tconstants in memory:CONST from[-to] (like hex)\n"
       "\tchar data area:     CHAR from[-to] (like hex, but ASCII if possible)\n"
       "\tword data area:     WORD from[-to] (like hex, but 16 bit)\n"
       "\tcode vector area:   CVEC[TOR] from[-to]\n" 
       "\t                    (like WORD, but adds target labels if necessary)\n"
       "\tdata vector area:   DVEC[TOR] from[-to]\n"
       "\t                    (like WORD, but adds target labels if necessary)\n"
      "\nAddressing control\n"
      "\tset DP value:       SETDP [addr[-addr]] DP-content\n"
      "\t                    (without addr, last one is used)\n"
       "\tforce addressing relative to base:\n"
       "\t\t\t    REL[ATIVE] addr[-addr] baseaddr\n"
       "\tunset relative addressing:\n"
       "\t\t\t    UNREL[ATIVE] addr[-addr]\n"
       "\tmap memory addresses to different base:\n"
       "\t\t\t    REMAP addr[-addr] offset\n"
       "\tmap file contents to different base:\n"
       "\t\t\t    PHASE addr[-addr] phase\n"
      "\nLabel control\n"
       "\tdefine label:       LABEL addr name\n"
       "\tremove label:       UNLABEL addr[-addr]\n"
       "\tdon't apply label name to constant but treat it as a number\n"
       "\t\t\t    CONST from[-to]\n"
       "\tmark auto-generated label as used\n"
       "\t\t\t    USED[LABEL] addr\n"
      "\nCommenting\n"
       "\tcomment:            COMM[ENT] addr[-addr] text\n"
       "\tsuppress comments:  UNCOMM[ENT] addr[-addr] text\n"
       "\tappended comments:  LCOMM[ENT] addr[-addr] text\n"
       "\tprepended comments: PREPLCOMM[ENT] addr[-addr] text\n"
       "\tsuppress lcomments: UNLCOMM[ENT] addr[-addr]\n"
      "\nMisc control\n"
       "\tinsert byte data:   PATCH addr[-addr] data[...]\n"
       "\tinsert word data:   PATCHW addr[-addr] data[...]\n"
       "\tinsert text:        INSERT addr[-addr] embedded line\n"
       "\tinclude label file: INCLUDE filename\n"
       "\tload binary file:   FILE filename [baseaddr]\n"
       "\tstop parsing:       END\n"
#else
       "Consists of text records of one of the following formats:\n"
       "* comment line\n"
       "FILE filename [baseaddr]\n"
       "OPTION option (like command line, without leading -)\n"
       "PREPEND [addr[-addr]] text to be prepended to the output\n"
       "PREPCOMM [addr[-addr]] comment text to be prepended to the output\n"
       "DATA from[-to]\n"
       "BIN[ARY] from[-to] (forced binary data)\n"
       "DEC[IMAL] from[-to] (forced decimal data)\n"
       "HEX[ADECIMAL] from[-to] (forced hex data)\n"
       "CHAR from[-to] (forced character data, only if no WORD area)\n"
       "WORD from[-to] (forced word data)\n"
       "CVEC[TOR] from[-to] (forced code vectors)\n"
       "DVEC[TOR] from[-to] (forced data vectors)\n"
       "CODE from[-to]\n"
       "CONST from[-to]\n"
       "UNUSED from[-to]\n"
       "RMB from-to\n"
       "LABEL addr name\n"
       "USED[LABEL] addr  (forces a label to \"Used\")\n"
       "UNLABEL addr[-addr]\n"
       "COMMENT addr[-addr] embedded comment line\n"
       "UNCOMMENT addr[-addr]\n"
       "LCOMMENT addr[-addr] appended line comment\n"
       "PREPLCOMM[ENT] addr[-addr] prepended line comment\n"
       "UNLCOMMENT addr[-addr]\n"
       "INSERT addr[-addr] embedded line\n"
       "INCLUDE filename\n"
       "SETDP [addr[-addr]] DP-content (without addr, last definition is used)\n"
       "REL[ATIVE] addr[-addr] bsaddr\n"
       "UNREL[ATIVE] addr[-addr]\n"
       "REMAP addr[-addr] offset\n"
       "PHASE addr[-addr] phase\n"
       "END\n"
#endif
       );
}

/*****************************************************************************/
/* usage : displays usage                                                    */
/*****************************************************************************/

void usage(int nQuit)
{
printf("Usage: f9dasm [options] <filename>\n"
       "Available options are:\n"
       " -begin     - start disassembly address [offset]\n"
       " -end       - end disassembly address  [auto]\n"
       " -offset    - address to load program [0]\n"
       " -out       - output file [stdout]\n"
       " -noaddr    - no address dump\n"
       " -hex       - with hex dump (default)\n"
       " -nohex     - no hex dump\n"
       " -x         - use 6309 opcodes (old style)\n"
       " -6301      - use 6301/6303 opcodes\n"
       " -6309      - use 6309 opcodes\n"
       " -6800      - use 6800/6802/6808 opcodes\n"
       " -6801      - use 6801/6803 opcodes\n"
       " -os9       - patch swi2 (os9 call)\n"
       " -info      - file with additional information [empty]\n"
       "              \"-info help\" shows info file help\n"
       " -cchar     - comment delimiter character [*]\n"
       " -flex      - use FLEX9 standard labels (default)\n"
       " -noflex    - do not use FLEX9 standard labels\n"
       " -conv      - use convenience mnemonics (default)\n"
       " -noconv    - do not use convenience mnemonics\n"
       " -dec       - output values in decimal\n"
       " -nodec     - output values in hex (default)\n"
       " -comment   - output comments (default)\n"
       " -nocomment - omit output of comments\n"
       " -asc       - output ASCII rendering of code/data (default)\n"
       " -noasc     - omit output of ASCII rendering of code/data\n"
       " -fcc       - use FCC to define data (default)\n"
       " -nofcc     - do not use FCC to define data (instead using FCB or FDB)\n"
       " -omitzero  - omit indexed-mode operands of $00 (default)\n"
       " -showzero  - do not omit indexed-mode operands of $00\n"
       " -forced    - set forced direct / extended addressing markers (default)\n"
       " -noforced  - don't set forced direct / extended addressing markers\n"
       " -ldchar    - label delimiter character [ ]\n"
       " -help      - output more extensive help and quit\n"
       "All values should be entered in hexadecimal\n");

if (nQuit)
  exit(1);
}

/*****************************************************************************/
/* ParseOption : parses an option & associated value                         */
/*****************************************************************************/

int ParseOption(char *name, char *value)
{
int j;
int nAdd = 0;

for (j = 0; Options[j].name; ++j)
  if (!strcmp(name, Options[j].name))
    {
    j = Options[j].value;
    break;
    }

switch (j)
  {
  case OPTION_BEGIN :
    nAdd++;
    if (!value)
      usage(1);
    begin = strtoul(value, NULL, 16);
    break;
  case OPTION_END :
    nAdd++;
    if (!value)
      usage(1);
    end = strtoul(value, NULL, 16);
    break;
  case OPTION_OFFSET :
    nAdd++;
    if (!value)
      usage(1);
    offset = strtoul(value, NULL, 16);
    break;
  case OPTION_OUT :
    nAdd++;
    if (!value)
      usage(1);
    if (outname)
      free(outname);
    outname = strdup(value);
    break;
  case OPTION_ADDR :
    showaddr = TRUE;
    break;
  case OPTION_NOADDR :
    showaddr = FALSE;
    break;
  case OPTION_HEX :
    showhex = TRUE;
    break;
  case OPTION_NOHEX :
    showhex = FALSE;
    break;

  case OPTION_6301 :
    mnemo[_lda].mne   = "LDAA";         /* adjust slight mnemo differences   */
    mnemo[_ldb].mne   = "LDAB";
    mnemo[_sta].mne   = "STAA";
    mnemo[_stb].mne   = "STAB";
    mnemo[_ora].mne   = "ORAA";
    mnemo[_orb].mne   = "ORAB";

    codes             = m6301_codes;
    codes10           = NULL;
    codes11           = NULL;
    exg_tfr           = m6809_exg_tfr;
    dirpage           = 0;
    useConvenience    = FALSE;
    break;

  case OPTION_6309 :
    mnemo[_lda].mne   = "LDA";          /* adjust slight mnemo differences   */
    mnemo[_ldb].mne   = "LDB";
    mnemo[_sta].mne   = "STA";
    mnemo[_stb].mne   = "STB";
    mnemo[_ora].mne   = "ORA";
    mnemo[_orb].mne   = "ORB";

    codes             = h6309_codes;
    codes10           = h6309_codes10;
    codes11           = h6309_codes11;
    exg_tfr           = h6309_exg_tfr;
    allow_6309_codes  = TRUE;
    break;

  case OPTION_6800 :
    mnemo[_lda].mne   = "LDAA";         /* adjust slight mnemo differences   */
    mnemo[_ldb].mne   = "LDAB";
    mnemo[_sta].mne   = "STAA";
    mnemo[_stb].mne   = "STAB";
    mnemo[_ora].mne   = "ORAA";
    mnemo[_orb].mne   = "ORAB";

    codes             = m6800_codes;
    codes10           = NULL;
    codes11           = NULL;
    exg_tfr           = m6809_exg_tfr;
    dirpage           = 0;              /* 6800 uses DP 0, fixed             */
    useConvenience    = FALSE;          /* NO Convenience ops!               */
    break;

  case OPTION_6801 :
    mnemo[_lda].mne   = "LDAA";         /* adjust slight mnemo differences   */
    mnemo[_ldb].mne   = "LDAB";
    mnemo[_sta].mne   = "STAA";
    mnemo[_stb].mne   = "STAB";
    mnemo[_ora].mne   = "ORAA";
    mnemo[_orb].mne   = "ORAB";

    codes             = m6801_codes;
    codes10           = NULL;
    codes11           = NULL;
    exg_tfr           = m6809_exg_tfr;
    dirpage           = 0;
    useConvenience    = FALSE;
    break;

  case OPTION_6809 :
    mnemo[_lda].mne   = "LDA";          /* adjust slight mnemo differences   */
    mnemo[_ldb].mne   = "LDB";
    mnemo[_sta].mne   = "STA";
    mnemo[_stb].mne   = "STB";
    mnemo[_ora].mne   = "ORA";
    mnemo[_orb].mne   = "ORB";

    codes             = m6809_codes;
    codes10           = m6809_codes10;
    codes11           = m6809_codes11;
    exg_tfr           = m6809_exg_tfr;
    allow_6309_codes  = FALSE;
    break;
  case OPTION_OS9 :
    os9_patch = TRUE;
    break;
  case OPTION_INFO :
    nAdd++;
    if (!value)
      usage(1);
    if (infoname)
      free(infoname);
    infoname = strdup(value);
    break;
  case OPTION_CCHAR :
  case OPTION_LDCHAR :
    nAdd++;
    if ((!value) ||
      (strlen(value) > 1))
      usage(1);
    if (j == OPTION_CCHAR)
      cCommChar = value[0];
    else if (j == OPTION_LDCHAR)
      cLblDelim = value[0];
    break;
  case OPTION_ASC :
    showasc = TRUE;
    break;
  case OPTION_NOASC :
    showasc = FALSE;
    break;
  case OPTION_FLEX :
    useFlex = TRUE;
    break;
  case OPTION_NOFLEX :
    useFlex = FALSE;
    break;
  case OPTION_CONV :
                                        /* only if not in 6800 mode          */
    useConvenience = (codes != m6800_codes  && codes != m6801_codes);
    break;
  case OPTION_NOCONV :
    useConvenience = FALSE;
    break;
  case OPTION_DEC :
    defaultDataType = DATATYPE_DEC;
    break;
  case OPTION_NODEC :
    defaultDataType = DATATYPE_HEX;
    break;
  case OPTION_COMMENT :
    emitComments = TRUE;
    break;
  case OPTION_NOCOMMENT :
    emitComments = FALSE;
    break;
  case OPTION_FCC :
    usefcc = TRUE;
    break;
  case OPTION_NOFCC :
    usefcc = FALSE;
    break;
  case OPTION_OMITZERO :
    showIndexedModeZeroOperand = FALSE;
    break;
  case OPTION_SHOWZERO :
    showIndexedModeZeroOperand = TRUE;
    break;
  case OPTION_FORCED :
    forcedirectaddr = "<";
    forceextendedaddr = ">";
    break;
  case OPTION_NOFORCED :
    forcedirectaddr = "";
    forceextendedaddr = "";
    break;
  case OPTION_HELP :
    usage(0);
    infousage();
    exit(0);
  default:
    usage(1);
  }
return nAdd;
}

/*****************************************************************************/
/* optionsinfo : processes OPTIONS settings from an info file                */
/*****************************************************************************/

void optionsinfo(char *name)
{
int i;
FILE *fp = NULL;
char szBuf[256];

strcpy(szBuf, name);
#if !FNCASESENS
strupr(szBuf);
#endif
for (i = 0;                             /* look whether already loaded       */
     (i < (sizeof(loaded) / sizeof(loaded[0]))) &&
     loaded[i];
     i++)
  {
  char szLoaded[256];
  strcpy(szLoaded, loaded[i]);
#if !FNCASESENS
  strupr(szLoaded);
#endif
  if (!strcmp(szBuf, szLoaded))         /* if so, avoid recursion            */
    return;                             /* and get out of here               */
  }
                                        /* allow a maximum of 200 inclusions */
if (i >= (sizeof(loaded) / sizeof(loaded[0])))
  return;

loaded[i] = strdup(name);               /* copy name to current position     */
fp = fopen(name, "r");
if (!fp)
  return;
while (fgets(szBuf, sizeof(szBuf), fp))
  {
  char *p = szBuf, *q;
  char *ltype, *option, *value;

  while ((*p == ' ') || (*p == '\t'))
    p++;
  if ((!*p) || (*p == '\n') || (*p == '*'))
    continue;
  ltype = p;
  for (q = p; (*q) && (*q != ' ') && (*q != '\t') && (*q != '\n'); q++)
    ;
  if (*q)
    *q++ = '\0';
  while (*p)
    {
    *p = toupper(*p);
    p++;
    }
  for (p = q; (*p == ' ') || (*p == '\t'); p++)
    ;

  if (!strcmp(ltype, "INCLUDE"))        /* we need to walk INCLUDEs          */
    {
    char *fname = p;
    char delim = ' ';
    if ((*p == '\'') || (*p == '\"'))
      {
      fname++;
      delim = *p++;
      }
    for (; (*p) && (*p != '\n') && (*p != delim) && (*p != '\t'); p++) ;
    if (*p)
      *p = '\0';
    if (*fname)
      optionsinfo(fname);
    continue;
    }

  if (!strcmp(ltype, "OPTION"))         /* and we need to check OPTIONs      */
    {
    option = p;
    for (; (*p) && (*p != ' ') && (*p != '\t') && (*p != '\n'); p++) ;
    if (*p)
      *p++ = '\0';
    for (; (*p == ' ') || (*p == '\t'); p++)
      ;
    value = p;
    for (q = p;
        (*q) &&
        (*q != '\n') && 
        (*q != '*') && 
        (*q != ' ') &&
        (*q != '\t');
        q++)
      {
      if (*q == '\\')                   /* process escape character          */
        strcpy(q, q+1);
      }
    if (*q)
      *q = '\0';
    ParseOption(option, (*value) ? value : NULL);
    continue;
    }
  }

fclose(fp);
}

/*****************************************************************************/
/* ScanHex : scans 1 hex value from a string, eventually remapping           */
/*****************************************************************************/

int ScanHex(char *s, int *n1)
{
int num = sscanf(s, "%x", n1);
if ((num >= 1) && (n1) && (*n1 >= 0) && (*n1 < 0x10000))
  *n1 += remaps[*n1];
return num;
}

/*****************************************************************************/
/* Scan2Hex : scans 1 or 2 hex values from a string, eventually remapping    */
/*****************************************************************************/

int Scan2Hex(char *s, int *n1, int *n2)
{
int num = sscanf(s, "%x-%x", n1, n2);
if ((num >= 1) && (n1) && (*n1 >= 0) && (*n1 < 0x10000))
  *n1 += remaps[*n1];
if ((num >= 2) && (n2) && (*n2 >= 0) && (*n2 < 0x10000))
  *n2 += remaps[*n2];
return num;
}

/*****************************************************************************/
/* SFlexRecord : a record in a binary FLEX9 disk file                        */
/*****************************************************************************/

#pragma pack(1)
struct SFlexRecord
{
byte bSOI;                              /* start of record indicator         */
byte bLoadAddress[2];                   /* Hi/Lo byte of load address        */
byte bDataLen;                          /* length of data record             */
byte bData[255];                        /* data record                       */
};
#pragma pack()

int IsTransferAddress(struct SFlexRecord *pRec)
{ return (pRec->bSOI == 0x16); }
int IsRecord(struct SFlexRecord *pRec)
{ return (pRec->bSOI == 0x02); }
int GetSize(struct SFlexRecord *pRec)
{ return pRec->bDataLen; }
int GetLoadAddress(struct SFlexRecord *pRec)
{ return (((int)(pRec->bLoadAddress[0])) << 8) | pRec->bLoadAddress[1]; }
byte * GetData(struct SFlexRecord *pRec)
{ return pRec->bData; }

/*****************************************************************************/
/* ReadFlexRecord : read one record of a FLEX9 binary                        */
/*****************************************************************************/

int ReadFlexRecord(FILE *f, struct SFlexRecord *pRecord)
{
int nCurPos = ftell(f);
byte bCur = 0;
int i;

while (!bCur)                           /* read 1st byte, skipping 0 bytes   */
  if (!fread(&bCur, 1, 1, f))
    return FALSE;
switch (bCur)                           /* OK, so what's that?               */
  {
  case 0x02 :                           /* Start of Record Indicator ?       */
    {
    pRecord->bSOI = bCur;
    if ((!fread(pRecord->bLoadAddress, 2, 1, f)) ||
        (!fread(&pRecord->bDataLen, 1, 1, f)))
      return FALSE;
    for (i = 0; i < pRecord->bDataLen; i++)
      if (!fread(&pRecord->bData[i], 1, 1, f))
        return FALSE;
    }
    break;
  case 0x16 :                           /* Transfer Address ?                */
    pRecord->bSOI = bCur;
    if (!fread(pRecord->bLoadAddress, 2, 1, f))
      return FALSE;
    break;
  default :
    fseek(f, nCurPos, SEEK_SET);        /* seek back                         */
    return FALSE;
  }
return TRUE;
}

/*****************************************************************************/
/* IsFlex : returns whether this is a FLEX binary                            */
/*****************************************************************************/

int IsFlex(FILE *f, byte *memory, unsigned *pbegin, unsigned *pend, int *load)
{
struct SFlexRecord rec;
int nCurPos = ftell(f);
int nRecs = 0;
int bExecutable = 0;
int begin = 0xffff;
int end = 0;
int i;

while (ReadFlexRecord(f, &rec))
  {
  int nStart = GetLoadAddress(&rec);
  int nEnd = nStart + GetSize(&rec) - 1;

  nRecs++;
  if (nStart < begin)
    begin = nStart;
  if (nEnd > end)
    end = nEnd;
  if (IsRecord(&rec) && GetSize(&rec))
    {
    for (i = nStart; i <= nEnd; i++)    /* mark area as used                 */
      {
      SET_USED(i);
      ATTRBYTE(i) |= defaultDataType;
      }

    memcpy(memory + nStart,
           GetData(&rec),
           GetSize(&rec));
    }
  else if (IsTransferAddress(&rec))
    {
    bExecutable = 1;
    *load = GetLoadAddress(&rec);
    }
  }

if (fgetc(f) != EOF)                    /* if not read through the whole file*/
  {
  for (i = begin; i <= end; i++)        /* mark area as UNused               */
    SET_UNUSED(i);
  nRecs = 0;                            /* this ain't no valid FLEX file     */
  }

fseek(f, nCurPos, SEEK_SET);
if (nRecs > 0)
  {
  if (begin < (int)(*pbegin))
    *pbegin = begin;
  if (end > (int)*pend)
    *pend = end;
  sLoadType = "FLEX";
  }
return (nRecs > 0);
}

/*****************************************************************************/
/* GetHex : retrieves a hex value in given length from file                  */
/*****************************************************************************/

int GetHex(FILE *f, int nChars)
{
int out = 0;

for (; nChars; nChars--)
  {
  int c = fgetc(f);
  if (c == EOF)
    return -1;
  if ((c >= 'a') && (c <= 'f'))
    c -= ('a' - 'A');
  if ((!((c >= '0') && (c <= '9'))) &&
      (!((c >= 'A') && (c <= 'F'))))
    return -1;
  c -= '0';
  if (c > 9)
    c -= 7;
  out = out * 16 + c;
  }

return out;
}

/*****************************************************************************/
/* IsIntelHex : tries to load as an Intel HEX file                           */
/*****************************************************************************/

int IsIntelHex(FILE *f, byte *memory, unsigned *pbegin, unsigned *pend, int *pload)
{
int nCurPos = ftell(f);
int c = 0, rectype;
int done = 0;
int nBytes = 0;
int begin = 0xffff;
int end = 0;
int segment = 0;                        /* segment address                   */
int load = -1;

if ((c = fgetc(f)) == EOF)              /* look whether starting with ':'    */
  return FALSE;
fseek(f, nCurPos, SEEK_SET);
if (c != ':')
  return FALSE;

while ((!done) &&
       (nBytes >= 0) &&
       (fread(&c, 1, 1, f)))            /* while there are lines             */
  {
  int nBytesOnLine, nAddr, i;
  if (c != ':')
    break;
  nBytesOnLine = GetHex(f, 2);          /* retrieve # bytes on line          */
  if (nBytesOnLine < 0)                 /* if error                          */
    { nBytes = -1; break; }             /* return with error                 */
  else if (nBytesOnLine == 0)           /* if end of file                    */
    break;                              /* just break;                       */
  nAddr = GetHex(f,4) + segment;        /* get address for bytes             */
  if ((nAddr < 0) || (nAddr >= 0x10000)) /* if illegal address               */
    { nBytes = -1; break; }             /* return with error                 */
  if (nAddr < begin)                    /* adjust start and end values       */
    begin = nAddr;
  if (nAddr + nBytesOnLine - 1 > end)
    end = nAddr + nBytesOnLine - 1;
  nBytes += nBytesOnLine;
  rectype = GetHex(f, 2);               /* fetch record type character       */
  switch (rectype)                      /* which type of record is this?     */
    {
    case 0 :                            /* data record                       */
      for (i = 0; i<nBytesOnLine; i++)  /* now get the bytes                 */
        {
        c = GetHex(f, 2);               /* retrieve a byte                   */
        if ((c < 0) || (c > 0xff))      /* if illegal byte                   */
          { nBytes = -1; break; }       /* return with error                 */
        memory[nAddr + i] = (byte)c;    /* otherwise add memory byte         */
        SET_USED(nAddr + i);            /* mark as used byte                 */
        ATTRBYTE(nAddr + i) |= defaultDataType;
        }
      break;
    case 1 :                            /* End Of File record                */
      done = 1;
      break;
    case 2 :                            /* Extended Segment Address          */
      segment = GetHex(f, 4);           /* get segment value to use          */
      segment <<= 4;                    /* convert to linear addition value  */
      if (segment < 0 || segment >= 0x10000)
        nBytes = -1;                    /* stop processing                   */
      break;
    case 3 :                            /* Start Segment Address             */
      segment = GetHex(f, 4);           /* get segment value to use          */
      segment <<= 4;                    /* convert to linear addition value  */
      nAddr = GetHex(f, 4) + segment;   /* get start instruction pointer     */
      if ((nAddr < 0) || (nAddr >= 0x10000)) /* if illegal address           */
        nBytes = -1;                    /* return with error                 */
      else
        load = nAddr;
      break;
    case 4 :                            /* Extended Linear Address           */
      segment = GetHex(f, 4);           /* get segment value to use          */
      segment <<= 16;                   /* convert to linear addition value  */
      if (segment < 0 || segment >= 0x10000)
        nBytes = -1;                    /* stop processing                   */
      break;
    case 5 :                            /* Start Linear Address              */
      nAddr = GetHex(f, 8);             /* get start instruction pointer     */
      if ((nAddr < 0) || (nAddr >= 0x10000)) /* if illegal address           */
        nBytes = -1;                    /* return with error                 */
      else
        load = nAddr;
      break;
    default :                           /* anything else?                    */
      nBytes = -1;                      /* unknown format. stop processing   */
      break;
    }

  /* ignore checksum byte; its calculation would be:
     add up all decoded bytes after the ':',
     take 2's complement of lowest byte */

  while (((c = fgetc(f)) != EOF) &&     /* skip to newline                   */
         (c != '\r') && (c != '\n'))
    ;

  while (((c = fgetc(f)) != EOF) &&     /* skip newline itself               */
         ((c == '\r') || (c == '\n')))
    ;
  if (c == ':')
    fseek(f, ftell(f) - 1, SEEK_SET);
  }

fseek(f, nCurPos, SEEK_SET);
if (nBytes >= 0)
  {
  if (begin < (int)*pbegin)
    *pbegin = begin;
  if (end > (int)*pend)
    *pend = end;
  if (load >= 0)
    *pload = load;
  }

if (nBytes > 0)
  sLoadType = "Intel HEX";
return (nBytes > 0);                    /* pass back #bytes interpreted      */
}

/*****************************************************************************/
/* IsMotorolaHex : tries to load as a Motorola HEX file                      */
/*****************************************************************************/

int IsMotorolaHex(FILE *f, byte *memory, unsigned *pbegin, unsigned *pend, int *pload)
{
int nCurPos = ftell(f);
int c = 0;
int done = 0;
int nBytes = 0;
int begin = 0xffff;
int end = 0;
int load = -1;

if ((c = fgetc(f)) == EOF)              /* look whether starting with 'S'    */
  return FALSE;
fseek(f, nCurPos, SEEK_SET);
if (c != 'S')
  return FALSE;

while ((!done) &&
       (nBytes >= 0) &&
       (fread(&c, 1, 1, f)))            /* while there are lines             */
  {
  int nLineType = 0, nBytesOnLine, nAddr, i;
  if (c != 'S')
    break;
  fread(&nLineType, 1, 1, f);           /* retrieve line type                */
  nBytesOnLine = GetHex(f, 2);          /* retrieve # bytes on line          */
  if (nBytesOnLine < 0)                 /* if error                          */
    { nBytes = -1; break; }             /* return with error                 */
  else if (nBytesOnLine == 0)           /* if end of file                    */
    break;                              /* just break;                       */
  switch (nLineType)                    /* now examine line type             */
    {
    case '0' :
#if 0                                   /* simply ignore the rest of the line*/
      nBytesOnLine--;
      while (nBytesOnLine--)
        GetHex(f, 2);
#endif
      break;
    case '1' :                          /* record with 16bit address         */
      nBytesOnLine -= 3;
      nAddr = GetHex(f,4);              /* get address for bytes             */
    data16bit:
      /* this program only deals with 16bit data, so restrict to 0-$FFFF     */
      if ((nAddr < 0) || (nAddr >= 0x10000)) /* if illegal address           */
        { nBytes = -1; break; }         /* return with error                 */
      if (nAddr < begin)                /* adjust start and end values       */
        begin = nAddr;
      if (nAddr + nBytesOnLine - 1 > end)
        end = nAddr + nBytesOnLine - 1;
      nBytes += nBytesOnLine;
                                        /* now get the bytes                 */
      for (i = 0; i < nBytesOnLine; i++)
        {
        c = GetHex(f, 2);               /* retrieve a byte                   */
        if ((c < 0) || (c > 0xff))      /* if illegal byte                   */
          { nBytes = -1; break; }       /* return with error                 */
        memory[nAddr + i] = (byte)c;    /* otherwise add memory byte         */
        SET_USED(nAddr + i);            /* mark as used byte                 */
        ATTRBYTE(nAddr + i) |= defaultDataType;
        }
      break;
    case '2' :                          /* record with 24bit address         */
      nBytesOnLine -= 4;
      nAddr = GetHex(f,6);              /* get address for bytes             */
      goto data16bit;
    case '3' :                          /* record with 32bit address         */
      nBytesOnLine -= 5;
      nAddr = GetHex(f,8);              /* get address for bytes             */
      goto data16bit;
    /* S5/S6 records ignored; don't think they make any sense here           */
    case '5' :
    case '6' :
      break;
    case '7' :                          /* 32-bit entry point                */
      nAddr = GetHex(f, 8);             /* get address to jump to            */
      goto entry16bit;
    case '8' :                          /* 24-bit entry point                */
      nAddr = GetHex(f, 6);             /* get address to jump to            */
      goto entry16bit;
    case '9' :
      nAddr = GetHex(f, 4);             /* get address to jump to            */
    entry16bit:
      /* this program only deals with 16bit data, so restrict to 0-$FFFF     */
      if ((nAddr < 0) || (nAddr >= 0x10000)) /* if illegal address           */
        { nBytes = -1; break; }         /* return with error                 */
      /* the documentation says "if address isn't needed, use 0".
       * bad idea IMO (they should have allowed to pass NO address instead),
       * but, well ... 0 MIGHT be a valid start address, so we need to live
       * with the ambiguity. */
      load = nAddr;
      done = 1;
      break;
    default :
      done = 1;
      break;
    }

  /* ignore checksum byte; its calculation would be:
     add up all decoded bytes after the record type,
     take 1's complement of lowest byte */

  while (((c = fgetc(f)) != EOF) &&     /* skip to newline                   */
         (c != '\r') && (c != '\n'))
    ;

  while (((c = fgetc(f)) != EOF) &&     /* skip newline itself               */
         ((c == '\r') || (c == '\n')))
    ;
  if (c == 'S')
    fseek(f, ftell(f) - 1, SEEK_SET);
  }

fseek(f, nCurPos, SEEK_SET);
if (nBytes >= 0)
  {
  if (begin < (int)*pbegin)
    *pbegin = begin;
  if (end > (int)*pend)
    *pend = end;
  if (load >= 0)
    *pload = load;
  }

if (nBytes > 0)
  sLoadType = "Motorola S";
return (nBytes > 0);                    /* pass back #bytes interpreted      */
}

/*****************************************************************************/
/* loadfile : loads a binary file to be disassembled into the memory space   */
/*****************************************************************************/

int loadfile
    (
    char *fn,
    unsigned *pbegin, unsigned *pend, int *pload, unsigned offset,
    FILE *out
    )
{
FILE *f = fopen(fn,"rb");
if (!f)
  return 1;
                                        /* if not a FLEX binary              */
if ((!IsFlex(f, memory, pbegin, pend, pload)) &&
                                        /* and not an Intel HEX file         */
    (!IsIntelHex(f, memory, pbegin, pend, pload)) &&
                                        /* and not a Motorola HEX file       */
    (!IsMotorolaHex(f, memory, pbegin, pend, pload)))
  {                                     /* load as normal binary image       */
  unsigned int i, off;
  fseek(f,0,SEEK_END);
  off = ftell(f);
  rewind(f);

  /* naive tests by J. Sigle showed that the logic was not robust enough...  */
  if (offset + off > 0x10000)           /* restrict to 64K area              */
    off = 0x10000 - offset;

  if (offset < *pbegin)                 /* set begin if not specified        */
    *pbegin = offset;
  if (*pend < offset + off - 1)         /* set end if not specified          */
    *pend = offset + off -1;
                                        /* mark area as used                 */
  for (i = offset; i < offset + off; i++)
    {
    SET_USED(i);
    ATTRBYTE(i) |= defaultDataType;
    }
  fread(&memory[offset&0xFFFF],         /* read binary                       */
        sizeof(byte),
        0x10000-(offset&0xFFFF),
        f);
  sLoadType = "binary";
  }

printf("Loaded %s file %s\n", sLoadType, fn);
if (out != stdout)
  fprintf(out,
          "%c Loaded %s file %s\n", cCommChar, sLoadType, fn);

fclose(f);
return 0;
}

/*****************************************************************************/
/* processinfo : processes an information file                               */
/*****************************************************************************/

void processinfo(char *name, FILE *outfile, int *FoundVectors)
{
FILE *fp = NULL;
char szBuf[256];
int i;
byte bDataType;
byte endinfo = 0;
enum
  {
  infoCode,                             /* [+]CODE addr[-addr]               */
  infoData,                             /* [+]DATA addr[-addr]               */
  infoLabel,                            /* LABEL addr name                   */
  infoComment,                          /* COMMENT addr[-addr] comment       */
  infoInclude,                          /* INCLUDE infofilename              */
  infoBinary,                           /* [+]BINARY addr[-addr]             */
  infoWord,                             /* [+]WORD addr[-addr]               */
  infoDWord,                            /* [+]DWORD addr[-addr]              */
  infoUnused,                           /* [+]UNUSED addr[-addr]             */
  infoInsert,                           /* INSERT addr[-addr] text           */
  infoSetDP,                            /* SETDP [addr[-addr]] dp            */
  infoUnsetDP,                          /* UNSETDP [addr[-addr]]             */
  infoLComment,                         /* LCOMMENT addr[-addr] [.]lcomment  */
  infoRMB,                              /* [+]RMB addr[-addr]                */
  infoUnlabel,                          /* UNLABEL addr[-addr]               */
  infoUncomment,                        /* UNCOMMENT addr[-addr]             */
  infoUnLComment,                       /* UNLCOMMENT addr[-addr]            */
  infoConstant,                         /* [+]CONST addr[-addr]              */
  infoPatch,                            /* PATCH addr [byte]*                */
  infoPatchWord,                        /* PATCHW addr [word]*               */
  infoBreak,                            /* [+]BREAK addr[-addr]              */
  infoPrepend,                          /* PREPEND [addr[-addr]] line        */
  infoHex,                              /* [+]HEX addr[-addr]                */
  infoDec,                              /* [+]DEC addr[-addr]                */
  infoChar,                             /* [+]CHAR addr[-addr]               */
  infoUsedLabel,                        /* USEDLABEL addr[-addr]             */
  infoPrepComm,                         /* PREPCOMM [addr[-addr]] comment    */
  infoRelative,                         /* RELATIVE addr[-addr] rel          */
  infoUnRelative,                       /* UNRELATIVE addr[-addr]            */
  infoPrepLComment,                     /* PREPLCOMM addr[-addr] [.]lcomment */
  infoRemap,                            /* REMAP addr[-addr] offs            */
  infoFile,                             /* FILE filename                     */
  infoPhase,                            /* PHASE addr[-addr] phase           */

  /* RB: new vector label types */
  infoCVector,                           /* CVEC[TOR] addr[-addr]             */
  infoDVector,                           /* DVEC[TOR] addr[-addr]             */

  infoEnd,                              /* END (processing this file)        */
  };
static struct                           /* structure to convert key to type  */
  {
  const char *szName;
  int nType;
  } sKey[] =
  {
  { "CODE",         infoCode },
  { "DATA",         infoData },
  { "LABEL",        infoLabel },
  { "COMMENT",      infoComment },
  { "COMM",         infoComment },
  { "INCLUDE",      infoInclude },
  { "BIN",          infoBinary },
  { "BINARY",       infoBinary },
  { "WORD",         infoWord },
  { "DWORD",        infoDWord },
  { "UNUSED",       infoUnused },
  { "INSERT",       infoInsert },
  { "SETDP",        infoSetDP },
  { "LCOMMENT",     infoLComment },
  { "LCOMM",        infoLComment },
  { "RMB",          infoRMB },
  { "OPTION",       -1 },
  { "UNLABEL",      infoUnlabel },
  { "UNCOMMENT",    infoUncomment },
  { "UNCOMM",       infoUncomment },
  { "UNLCOMMENT",   infoUnLComment },
  { "UNLCOMM",      infoUnLComment },
  { "CONSTANT",     infoConstant },
  { "CONST",        infoConstant },
  { "PATCH",        infoPatch },
  { "PATCHW",       infoPatchWord },
  { "BREAK",        infoBreak },
  { "PREPEND",      infoPrepend },
  { "HEXADECIMAL",  infoHex },
  { "SEDECIMAL",    infoHex },
  { "HEX",          infoHex },
  { "DECIMAL",      infoDec },
  { "DEC",          infoDec },
  { "CHARACTER",    infoChar },
  { "CHAR",         infoChar },
  { "USED",         infoUsedLabel },
  { "USEDLABEL",    infoUsedLabel },
  { "PREPCOMM",     infoPrepComm },
  { "RELATIVE",     infoRelative },
  { "REL",          infoRelative },
  { "UNRELATIVE",   infoUnRelative },
  { "UNREL",        infoUnRelative },
  { "PREPLCOMMENT", infoPrepLComment },
  { "PREPLCOMM",    infoPrepLComment },
  { "REMAP",        infoRemap },
  { "FILE",         infoFile },
  { "PHASE",        infoPhase },

  /* RB: new vector label types */
  { "CVECTOR",      infoCVector },
  { "CVEC",         infoCVector },
  { "DVECTOR",      infoDVector },
  { "DVEC",         infoDVector },

  { "END",          infoEnd },
  };

strcpy(szBuf, name);
#if !FNCASESENS
strupr(szBuf);
#endif
for (i = 0;                             /* look whether already loaded       */
     (i < (sizeof(loaded) / sizeof(loaded[0]))) &&
     loaded[i];
     i++)
  {
  char szLoaded[256];
  strcpy(szLoaded, loaded[i]);
#if !FNCASESENS
  strupr(szLoaded);
#endif
  if (!strcmp(szBuf, szLoaded))         /* if so, avoid recursion            */
    return;                             /* and get out of here               */
  }
                                        /* allow a maximum of 200 inclusions */
if (i >= (sizeof(loaded) / sizeof(loaded[0])))
  return;

/* RB: we will otherwise never see an error-free help */
if (!strcmp(name, "help"))
  {
  infousage();
  exit(0);
  }

loaded[i] = strdup(name);               /* copy name to current position     */
fp = fopen(name, "r");
if (!fp)
  {
  printf("Could not open %s\n", name);
  infousage();
  return;
  }
while (fgets(szBuf, sizeof(szBuf), fp))
  {
  char *p = szBuf, *q, *r;
  char *ltype;
  int nType = -1;
  int bMod = FALSE;
  int nFrom, nTo, nScanned;

  while ((*p == ' ') || (*p == '\t'))
    p++;
  if ((!*p) || (*p == '\n') || (*p == '*'))
    continue;
  if (*p == '+')
    {
    bMod = TRUE;
    p++;
    while ((*p == ' ') || (*p == '\t'))
      p++;
    if ((!*p) || (*p == '\n') || (*p == '*'))
      continue;
    }
  ltype = p;
  for (q = p; (*q) && (*q != ' ') && (*q != '\t') && (*q != '\n'); q++)
    ;
  if (*q)
    *q++ = '\0';
  while (*p)
    {
    *p = toupper(*p);
    p++;
    }
  for (p = q; (*p == ' ') || (*p == '\t'); p++)
    ;

  for (i = 0; i < (sizeof(sKey) / sizeof(sKey[0])); i++)
    {
    if (!strcmp(ltype, sKey[i].szName))
      {
      nType = sKey[i].nType;
      break;
      }
    }

  switch (nType)
    {
    case infoCode :                     /* [+]CODE addr[-addr]               */
    case infoData :                     /* [+]DATA addr[-addr]               */
    case infoBinary :                   /* [+]BINARY addr[-addr]             */
    case infoWord :                     /* [+]WORD addr[-addr]               */
    case infoDWord :                    /* [+]DWORD addr[-addr]              */
      /* f9dasm can't really do DWORDs (only in 6309 LDQ processing),
         so this is treated like WORD+CONST */
    case infoUnused :                   /* [+]UNUSED addr[-addr]             */
    case infoRMB :                      /* [+]RMB addr[-addr]                */
    case infoConstant :                 /* [+]CONST addr[-addr]              */
    case infoBreak :                    /* [+]BREAK addr[-addr]              */
    case infoHex :                      /* [+]HEX addr[-addr]                */
    case infoDec :                      /* [+]DEC addr[-addr]                */
    case infoChar :                     /* [+]CHAR addr[-addr]               */
    case infoCVector:                   /* [+]CVEC[TOR] addr[-addr]          */
    case infoDVector:                   /* [+]DVEC[TOR] addr[-addr]          */

      /* datatype setting */
      switch(nType)
        {
        case infoHex:     bDataType = DATATYPE_HEX;     break;
        case infoDec:     bDataType = DATATYPE_DEC;     break;
        case infoChar:    bDataType = DATATYPE_CHAR;    break;
        case infoBinary:  bDataType = DATATYPE_BINARY;  break;
        default:
          if (nType != infoUnused)
            bDataType = defaultDataType;
          else
            bDataType = 0;
        }

      nScanned = Scan2Hex(p, &nFrom, &nTo);

      /* skip if not at least "from" address found */
      if (nScanned < 1)
        break;

      /* set implicit "to" address, if not specified */
      if (nScanned == 1)
        nTo = (nType == infoWord) ? nFrom + 1 : nFrom;

      /* skip in case of erratic ranges */
      if ((nFrom < 0) || (nTo < 0) || (nFrom > nTo) || (nTo >= 0x10000))
        break;

      /* RB: signal VECTOR statement occurrence */
      if (
        (nType == infoCVector)||
        (nType == infoDVector)
      )
        *FoundVectors = TRUE;

      for (; nFrom <= nTo; nFrom++)     /* work through address range        */
        if (nType == infoUnused)
          {
          ATTRBYTE(nFrom) &=            /* force byte to NOT USED            */
              (AREATYPE_LABEL | AREATYPE_ULABEL);
          SET_UNUSED(nFrom);
          }
        else
          {

          if (bMod)                     /* if '+' modifier was found         */
            {
            if ((nType == infoBinary) || 
                (nType == infoHex) ||
                (nType == infoDec) ||
                (nType == infoChar))
              ATTRBYTE(nFrom) &= ~DATATYPE_CHAR;
            }
          else
            {
            if ((nType != infoConstant) &&
                (nType != infoBreak))
              ATTRBYTE(nFrom) &= ~(AREATYPE_CODE | AREATYPE_DATA |
                                   AREATYPE_WORD |
                                   AREATYPE_CHAR | AREATYPE_BINARY | AREATYPE_HEX);
            }

          if (nType == infoRMB)
            SET_USED(nFrom);            /* force byte@address to USED        */

          if (nType == infoDec)
            ATTRBYTE(nFrom) &= ~DATATYPE_HEX;
          else
            ATTRBYTE(nFrom) |= ((nType == infoCode)     ? (AREATYPE_CODE | bDataType) : 
                                (nType == infoData)     ? (AREATYPE_DATA | bDataType) :
                                (nType == infoBinary)   ? AREATYPE_BINARY :
                                (nType == infoWord)     ? (AREATYPE_WORD | bDataType) :
                                (nType == infoDWord)    ? (AREATYPE_WORD | AREATYPE_CONST | bDataType) :
                                (nType == infoRMB)      ? AREATYPE_RMB :
                                (nType == infoConstant) ? (AREATYPE_CONST | bDataType) :
                                (nType == infoBreak)    ? AREATYPE_ULABEL :
                                (nType == infoHex)      ? AREATYPE_HEX :
                                (nType == infoChar)     ? AREATYPE_CHAR :
                                (nType == infoDVector)  ? AREATYPE_DVEC :
                                (nType == infoCVector)  ? AREATYPE_CVEC :
                                0);
          }
      break;
    case infoUnlabel :                  /* UNLABEL addr[-addr]               */
      nScanned = Scan2Hex(p, &nFrom, &nTo);
      if (nScanned == 1)
        nTo = nFrom;
      if ((nScanned < 1) ||
          (nFrom < 0) || (nFrom >= 0x10000) ||
          (nTo < 0) || (nTo >= 0x10000) || (nFrom > nTo))
        break;
      for (nScanned = nFrom; nScanned <= nTo; nScanned++)
        {
        if (lblnames[nScanned])
          {
          free(lblnames[nScanned]);
          lblnames[nScanned] = NULL;
          }
        ATTRBYTE(nScanned) &= ~AREATYPE_LABEL;
        }
      break;
    case infoUsedLabel :                /* USEDLABEL addr[-addr]             */
      nScanned = Scan2Hex(p, &nFrom, &nTo);
      if (nScanned == 1)
        nTo = nFrom;
      if ((nScanned < 1) ||
          (nFrom < 0) || (nFrom >= 0x10000) ||
          (nTo < 0) || (nTo >= 0x10000) || (nFrom > nTo))
        break;
      for (nScanned = nFrom; nScanned <= nTo; nScanned++)
        ATTRBYTE(nScanned) |= AREATYPE_ULABEL;
      break;
    case infoUncomment :                /* UNCOMMENT addr[-addr]             */
    case infoUnLComment :               /* UNLCOMMENT addr[-addr]            */
      {
      char **ct;
      nScanned = Scan2Hex(p, &nFrom, &nTo);
      if (nScanned == 1)
        nTo = nFrom;
      if ((nScanned < 1) ||
          (nFrom < 0) || (nFrom >= 0x10000) ||
          (nTo < 0) || (nTo >= 0x10000) || (nFrom > nTo))
        break;
      ct = (nType == infoUncomment) ? commentlines : lcomments;
      for (nScanned = nFrom; nScanned <= nTo; nScanned++)
        {
        if (ct[nScanned])
          {
          free(ct[nScanned]);
          ct[nScanned] = NULL;
          }
        }
      }
      break;

    case infoLabel :                    /* LABEL addr name                   */
    DoInsert:                           /* PREPCOMM, PREPEND                 */
      {
      char *laddr = p;
      for (; (*p) && (*p != ' ') && (*p != '\t'); p++) ;
      if (*p)
        *p++ = '\0';
      for (; (*p == ' ') || (*p == '\t'); p++)
        ;
      if (*p == '.')
        p++;
      nScanned = Scan2Hex(laddr, &nFrom, &nTo);
      if (nScanned < 1)
        break;
      else if (nScanned == 1)
        nTo = nFrom;
      for (q = p;
           (*q) &&
           (*q != '\n') && 
           (*q != '*') && 
           ((nType != infoLabel) || ((*q != ' ') && (*q != '\t')));
           q++)
        {
        if (*q == '\\')                 /* process escape character          */
          strcpy(q, q+1);
        }

      if (*q)
        *q = '\0';
      if (((nType == infoLabel) && (!*p)) ||
          (nFrom < 0) || (nFrom > 0xffff) ||
          (nTo < 0) || (nTo >= 0x10000) || (nFrom > nTo))
        break;
      for (nScanned = nFrom; nScanned <= nTo; nScanned++)
        {
        if (nType == infoLabel)         /* if setting a label,               */
          {
          if (lblnames[nScanned])       /* remove eventual predefinition     */
            free(lblnames[nScanned]);
          lblnames[nScanned] = strdup(p);
          ATTRBYTE(nScanned) |= AREATYPE_LABEL;
          }
        else                            /* if setting a comment or insert,   */
          {
          if (commentlines[nScanned])   /* if comment line is already there  */
            {
            int nLen = strlen(commentlines[nScanned]) + strlen(p) + 6;
            commentlines[nScanned] = realloc(commentlines[nScanned], nLen);
            if (commentlines[nScanned])
              {
              if ((nType == infoPrepend) ||
                  (nType == infoPrepComm))
                {
                int prepcomm = 0;       /* prepend comment char necessary    */
                                        /* if that was an INSERT or PREPEND  */
                if (commentlines[nScanned][0] == (char)0xff)
                                        /* remove the INSERT marker          */
                  strcpy(commentlines[nScanned], commentlines[nScanned] + 1);
                else if ((commentlines[nScanned][0]) &&
                         (commentlines[nScanned][0] != '\n'))
                  prepcomm = 2;
                                        /* position behind string            */
                q = commentlines[nScanned] + strlen(commentlines[nScanned]);
                r = q + strlen(p) + 1 + prepcomm;
                if (*p)                 /* if there's a text,                */
                  {
                  if (nType == infoPrepend)
                    r += 1;             /* add space for 0xff                */
                  }
                                        /* make space for new string         */
                while (q >= commentlines[nScanned])
                  *r-- = *q--;
                q++;                    /* advance to start of buffer        */
                if (*p)                 /* if there's a text                 */
                  {
                  if (nType == infoPrepend)
                    *q++ = (char)0xFF;  /* prepend a "No comment char" marker*/
                  }

                while (*p)              /* then copy in the new string       */
                  *q++ = *p++;
                *q++ = '\n';            /* and append a newline              */
                if (prepcomm)           /* if that was a comment and needs   */
                  {                     /* a comment character now,          */
                  *q++ = cCommChar;     /* prepend comment char              */
                  *q++ = ' ';
                  }
                }
              else                      /* if INSERT or COMMENT              */
                {
                strcat(commentlines[nScanned], "\n");
                if (strlen(p))          /* if there's data,                  */
                  {
                  if (nType == infoComment)
                    sprintf(commentlines[nScanned] + strlen(commentlines[nScanned]),
                            "%c ", cCommChar);
                  strcat(commentlines[nScanned], p);
                  }
                }
              }
            }
          else                          /* if this is fresh,                 */
            {
            /* if COMMENT or PREPCOMM, simply set the line as is */
            if ((nType == infoComment) ||
                (nType == infoPrepComm))
              commentlines[nScanned] = strdup(p);
            else                        /* if INSERT or PREPEND              */
              {                         /* add line, but with comment delim. */
              commentlines[nScanned] = malloc(strlen(p) + 2);
              if (commentlines[nScanned])
                {
                commentlines[nScanned][0] = (char)0xff;
                strcpy(commentlines[nScanned]+1, p);
                }
              }
            }
          }
        }
      }
      break;
    case infoInclude :                  /* INCLUDE infofilename              */
      {
      char *fname = p;
      char delim = ' ';
      if ((*p == '\'') || (*p == '\"'))
        {
        fname++;
        delim = *p++;
        }
      for (; (*p) && (*p != '\n') && (*p != delim) && (*p != '\t'); p++) ;
      if (*p)
        *p = '\0';
      if (*fname)
        processinfo(fname, outfile, FoundVectors);
      }
      break;
    case infoSetDP :                    /* SETDP [addr[-addr]] dp            */
      if (codes != m6800_codes &&       /* this is not for M6800!            */
          codes != m6801_codes)
        {
        char *laddr = p;
        int nDP;
        for (; (*p) && (*p != ' ') && (*p != '\t'); p++) ;
        if (*p)
          *p++ = '\0';
        for (; (*p == ' ') || (*p == '\t'); p++)
          ;
                                        /* start interpreting as range       */
        nScanned = Scan2Hex(laddr, &nFrom, &nTo);
        if (nScanned < 1)               /* at least dp MUST be there         */
          break;
        else if (nScanned == 1)         /* if only start given,              */
          nTo = 0xffff;                 /* set DP for there to end           */
        if (sscanf(p, "%x", &nDP) >= 1) /* range must be followed by DP      */
          {
          if (nDP < 0 || nDP > 0xff)
            nDP = -1;                   /* 0..FF or -1 for "off"             */
          for (nScanned = nFrom; nScanned <= nTo; nScanned++)
            dps[nScanned] = (short)nDP;
          }
        else                            /* no range, so it's global DP       */
          {
          if (sscanf(laddr, "%x", &nDP) != 1)
            break;
          if ((nDP < 0) || (nDP > 0xff))
            nDP = -1;                   /* 0..FF or -1 for "off"             */
          dirpage = nDP;
          }
        }
      break;
    case infoUnsetDP :                  /* UNSETDP [addr[-addr]]             */
      nScanned = Scan2Hex(p, &nFrom, &nTo);
      if (nScanned < 1)                 /* if global DP                      */
        dirpage = 0;                    /* reset global DP                   */
      else                              /* if for a range                    */
        {
        if (nScanned == 1)
          nTo = nFrom;
        if ((nScanned < 1) ||
            (nFrom < 0) || (nFrom >= 0x10000) ||
            (nTo < 0) || (nTo >= 0x10000) || (nFrom > nTo))
          break;
        for (nScanned = nFrom; nScanned <= nTo; nScanned++)
          dps[nScanned] = 0x0101;       /* disable range DP                  */
        }
      break;
    case infoLComment :                 /* LCOMMENT addr[-addr] [.]lcomment  */
    case infoPrepLComment :             /* PREPLCOMM addr[-addr] [.]lcomment */
      {
      char *laddr = p;
      for (; (*p) && (*p != ' ') && (*p != '\t'); p++) ;
      if (*p)
        *p++ = '\0';
      for (; (*p == ' ') || (*p == '\t'); p++)
        ;
      if (*p == '.')                    /* '.' allows leading blanks         */
        p++;
      nScanned = Scan2Hex(laddr, &nFrom, &nTo);
      if ((nScanned < 1) || (!emitComments))
        break;
      else if (nScanned == 1)
        nTo = nFrom;
      for (q = p;
           (*q) &&
           (*q != '\n') && (*q != '*');
           q++)
        {
        if (*q == '\\')                 /* process escape character          */
          strcpy(q, q+1);
        }

      if (*q)
        *q = '\0';
      if ((nFrom < 0) || (nFrom >= 0x10000) ||
          (nTo < 0) || (nTo >= 0x10000) || (nFrom > nTo))
        break;
      for (nScanned = nFrom; nScanned <= nTo; nScanned++)
        {
        if (lcomments[nScanned])
          {
          int nBlanks = 41;
          int nLen = strlen(lcomments[nScanned]) + strlen(p) + nBlanks + 4;
          lcomments[nScanned] = realloc(lcomments[nScanned], nLen);
          if (lcomments[nScanned])
            {
            if (nType == infoLComment)  /* if LCOMMENT                       */
              {
              q = lcomments[nScanned] + strlen(lcomments[nScanned]);
              *q++ = '\n';
              for (; nBlanks > 0; nBlanks--)
                *q++ = ' ';
              *q++ = cCommChar;
              *q++ = ' ';
              strcpy(q, p);
              }
            else                        /* if PREPLCOMM                      */
              {                         /* position behind string            */
              q = lcomments[nScanned] + strlen(lcomments[nScanned]);
              r = q + strlen(p) + nBlanks + 3;
                                        /* make space for new string         */
              while (q >= lcomments[nScanned])
                *r-- = *q--;
              q++;                      /* advance to start of buffer        */
              strcpy(q, p);             /* copy in the string                */
              q += strlen(p);           /* advance behind it                 */
              *q++ = '\n';
              for (; nBlanks > 0; nBlanks--)
                *q++ = ' ';
              *q++ = cCommChar;
              *q++ = ' ';
              }
            }
          }
        else
          lcomments[nScanned] = strdup(p);
        }
      }
      break;
    case infoPatch :                    /* PATCH addr [byte]*                */
    case infoPatchWord :                /* PATCHW addr [word]*               */
      {
      char *laddr = p;
      for (; (*p) && (*p != ' ') && (*p != '\t'); p++) ;
      if (*p)
        *p++ = '\0';
      if (ScanHex(laddr, &nFrom) != 1)
        break;
      do
        {
        for (; (*p == ' ') || (*p == '\t'); p++)
          ;
        if ((!*p) ||
            (*p == '*') ||
            (sscanf(p, "%x", &nTo) != 1) ||
            (nFrom < 0) || (nFrom > 0xffff) ||
            (nTo < 0) || (nTo > 0xffff))
          break;
        if (nType == infoPatch)
          MEMORY(nFrom++) = (byte)(nTo & 0xff);
        else
          {
          MEMORY(nFrom++) = (byte)((nTo >> 8) & 0xff);
          MEMORY(nFrom++) = (byte)(nTo & 0xff);
          }
        for (; (*p) && (*p != '*') && (*p != ' ') && (*p != '\t'); p++)
          ;
        } while (1);
      }
      break;
    case infoComment :                  /* COMMENT addr[-addr] comment       */
    case infoPrepComm :                 /* PREPCOMM [addr[-addr]] comment    */
      if (!emitComments)
        break;
      /* otherwise fall through to... */
    case infoInsert :                   /* INSERT [addr[-addr]] text         */
    case infoPrepend :                  /* PREPEND [addr[-addr]] line        */
      nScanned = Scan2Hex(p, &nFrom, &nTo);
      if (nScanned == 1)
        nTo = nFrom;
      if ((nScanned >= 1) &&
          (nFrom >= 0) && (nFrom < 0x10000) &&
          (nTo >= 0) && (nTo < 0x10000) && (nFrom <= nTo))
        goto DoInsert;
      if (*p == '.')
        p++;
      for (q = p;
          (*q) && (*q != '\n') && (*q != '*');
           q++)
        {
        if (*q == '\\')                 /* process escape character          */
          strcpy(q, q + 1);
        }
      if (*q)
        *q = '\0';
      if (!szPrepend)
        {
        szPrepend = malloc(strlen(p) + 3);
        if (szPrepend)
          *szPrepend = '\0';
        }
      else
        szPrepend = realloc(szPrepend, strlen(szPrepend) + strlen(p) + 4);
      if (szPrepend)
        {
        size_t len = 0;
        if (nType == infoPrepComm ||    /* if prepending line                */
            nType == infoPrepend)
          {
          char *old = strdup(szPrepend);
          if (strlen(p) && nType == infoPrepComm)
            len = sprintf(szPrepend, "%c ", cCommChar);
          len += sprintf(szPrepend + len, "%s", p);
          if (*old)
            len += sprintf(szPrepend + len, "\n%s", old);
          free(old);
          }
        else if (nType == infoComment ||/* if appending a line               */
                 nType == infoInsert)
          {
          len = strlen(szPrepend);
          if (len)
            strcpy(szPrepend + len++, "\n");
          if (strlen(p))
            {
            if (nType == infoComment)
              len += sprintf(szPrepend + len, "%c ", cCommChar);
            strcpy(szPrepend + len, p);
            }
          }
        }
      break;
    case infoRelative :                 /* RELATIVE addr[-addr] rel          */
      {
      char *laddr = p;
      int nrel;
      
      for (; (*p) && (*p != ' ') && (*p != '\t'); p++) ;
      if (*p)
        *p++ = '\0';
      for (; (*p == ' ') || (*p == '\t'); p++)
        ;
      nScanned = Scan2Hex(laddr, &nFrom, &nTo);
      if (nScanned < 1)
        break;
      else if (nScanned == 1)
        nTo = nFrom;
      nScanned = sscanf(p, "%x", &nrel);
      if (nScanned < 1)
        break;
      if ((nFrom < 0) || (nFrom >= 0x10000) ||
          (nTo < 0) || (nTo >= 0x10000) || (nFrom > nTo) ||
          (nrel < 0) || (nrel >= 0x10000))
        break;
      for (nScanned = nFrom; nScanned <= nTo; nScanned++)
        rels[nScanned] = nrel;
      }
      break;
    case infoUnRelative :               /* UNRELATIVE addr[-addr]            */
      {
      nScanned = Scan2Hex(p, &nFrom, &nTo);
      if (nScanned == 1)
        nTo = nFrom;
      if ((nScanned < 1) ||
          (nFrom < 0) || (nFrom >= 0x10000) ||
          (nTo < 0) || (nTo >= 0x10000) || (nFrom > nTo))
        break;
      for (nScanned = nFrom; nScanned <= nTo; nScanned++)
        rels[nScanned] = 0;
      }
      break;
    case infoRemap :                    /* REMAP addr[-addr] offs            */
      {
      char *laddr = p;
      int nremap;
      int minus = 1;
      
      for (; (*p) && (*p != ' ') && (*p != '\t'); p++) ;
      if (*p)
        *p++ = '\0';
      for (; (*p == ' ') || (*p == '\t'); p++)
        ;
      /* allow remapped remaps... :-) */
      nScanned = Scan2Hex(laddr, &nFrom, &nTo);
      if (nScanned < 1)
        break;
      else if (nScanned == 1)
        nTo = nFrom;
      if (*p == '-')
        {
        p++;
        minus = -1;
        }
      nScanned = sscanf(p, "%x", &nremap);
      if (nScanned < 1)
        break;
      nremap *= minus;
      if ((nFrom < 0) || (nFrom >= 0x10000) ||
          (nTo < 0) || (nTo >= 0x10000) || (nFrom > nTo) ||
          (nremap <= -0x10000) || (nremap >= 0x10000))
        break;
      for (nScanned = nFrom; nScanned <= nTo; nScanned++)
        remaps[nScanned] += nremap;
      }
      break;
    case infoFile :                     /* FILE filename                     */
      {
      char *fname = p;
      char delim = ' ';
      if ((*p == '\'') || (*p == '\"'))
        {
        fname++;
        delim = *p++;
        }
      for (; (*p) && (*p != '\n') && (*p != delim) && (*p != '\t'); p++) ;
      if (*p)
        *p++ = '\0';
      nScanned = sscanf(p, "%x", &nFrom);
      if (nScanned < 1)
        nFrom = end;
      if ((!*fname) || (nFrom < 0) || (nFrom >= 0x10000))
        break;
      loadfile(fname, &begin, &end, &load, nFrom, outfile);
      }
      break;
    case infoPhase :                    /* PHASE addr[-addr] phase           */
      {
      char *laddr = p;
      int nPhase, nRel, bSigned;
      
      for (; (*p) && (*p != ' ') && (*p != '\t'); p++) ;
      if (*p)
        *p++ = '\0';
      for (; (*p == ' ') || (*p == '\t'); p++)
        ;
      nScanned = Scan2Hex(laddr, &nFrom, &nTo);
      if (nScanned < 1)
        break;
      else if (nScanned == 1)
        nTo = nFrom;
      bSigned = (*p == '+' || *p == '-');
      nScanned = sscanf(p, "%x", &nPhase);
      if (nScanned < 1)
        break;
      if (bSigned)
        {
        /* allow relative phases - no check for overlapping areas. GIGO */
        int i;
        nRel = nPhase;
        for (i = numphases - 1; i >= 0; i--)
          if (nFrom >= phases[i].from && nFrom <= phases[i].to && !phases[i].rel)
            {
            nPhase = i;
            break;
            }
        }
      else
        nRel = 0;
      if ((nFrom < 0) || (nFrom >= 0x10000) ||
          (nTo < 0) || (nTo >= 0x10000) || (nFrom > nTo) ||
          (nPhase < 0) || (nPhase >= 0x10000))
        break;
      /* NB: if over/underflow happens in the phased area, well, too bad...  */
      if (numphases < MAXPHASES)
        {
        phases[numphases].from = nFrom;
        phases[numphases].to = nTo;
        phases[numphases].phase = nPhase;
        phases[numphases].rel = nRel;
        numphases++;
        }
      }
      break;
    }

  if (nType == infoEnd)                 /* if END directive found            */
    break;                              /* stop processing                   */
  }

fclose(fp);
}

/*****************************************************************************/
/* main : main program function                                              */
/*****************************************************************************/

int main(int argc, char *argv[])
{
unsigned pc, add;
int i, n, nComment, isautolabel, curdp, curphase = -1;
int lastwasdata = FALSE;  /* RB: get a divider between data and code */
int fvec = FALSE;         /* RB: found vector declaration in label file */
char buf[256];
FILE *out = stdout;

printf("f9dasm: M6800/1/2/3/8/9 / H6309 Binary/OS9/FLEX9 Disassembler V" VERSION "\n");

for (i = 1, n = 0; i < argc; ++i)
  {
  if (argv[i][0] != '-')
    {
    switch (++n)
      {
      case 1:
        fname = argv[i];
        break;
      default:
        usage(1);
      }
    }
  else
    i += ParseOption(argv[i]+1, argv[i+1]);
  }

memory = (byte *)malloc(0x10000);
label = (int *)malloc(0x10000 * sizeof(int));
used = (byte *)malloc(0x10000 / 8);
lblnames = (char **)malloc(0x10000 * sizeof(char *));
commentlines = (char **)malloc(0x10000 * sizeof(char *));
lcomments = (char **)malloc(0x10000 * sizeof(char *));
rels = (unsigned short *)malloc(0x10000 * sizeof(unsigned short));
dps = (short *)malloc(0x10000 * sizeof(short));
phases = (phasedef *)malloc(MAXPHASES * sizeof(phasedef));
remaps = (int *)malloc(0x10000 * sizeof(int));
if ((!memory) || (!label) || (!used) ||
    (!lblnames) || (!commentlines) || (!lcomments) ||
    (!rels) || (!phases) || (!dps) || (!remaps))
  {
  printf("no mem buffer\n");
  goto exit;
  }
memset(memory, 0x01, 0x10000);
memset(label, 0x00, 0x10000 * sizeof(int));
memset(used, 0x00, 0x10000 / 8);
memset(lblnames, 0x00, 0x10000 * sizeof(char *));
memset(commentlines, 0x00, 0x10000 * sizeof(char *));
memset(lcomments, 0x00, 0x10000 * sizeof(char *));
memset(rels, 0x00, 0x10000 * sizeof(unsigned short));
memset(dps, 0x01, 0x10000 * sizeof(short));
memset(remaps, 0x00, 0x10000 * sizeof(int));

if (outname)
  {
  out = fopen(outname,"w");
  if (!out)
    {
    printf("can't open %s \n",outname);
    return 1;
    }
  fprintf(out,
          "%c f9dasm: M6800/1/2/3/8/9 / H6309 Binary/OS9/FLEX9 Disassembler V" VERSION "\n",
          cCommChar);
  }

if (infoname)                           /* first get options from info file  */
  optionsinfo(infoname);                /* (may be needed before load)       */

for (i = 0;                             /* remove loaded information         */
     (i < (sizeof(loaded) / sizeof(loaded[0]))) &&
     loaded[i];
     i++)
  {
  free(loaded[i]);                      /* this is re-set in processinfo()   */
  loaded[i] = NULL;
  }
                                        /* set comment flag                  */
nComment = (showaddr || showhex || showasc);
AddFlexLabels();                        /* make sure all FLEX labels are OK  */

                                        /* load initial file                 */
if (fname && loadfile(fname, &begin, &end, &load, offset, out))
  {
  printf("Error loading %s\n", fname);
  return 1;
  }

/*****************************/
/* RB: insert system vectors */
/*****************************/


pc=0xfff0;                              /* base vector address               */

/* 6809 has 0xfff0/1 reserved
 * comment out #if section for proper databook behavior */
if (codes==m6809_codes)
  {
#if 0
  /* precaution fop case of funky HEX/S files
   * blank memory is filled with $010101... */
  if( ARGWORD(pc)!=0x0101 )
    {
    ATTRBYTE(pc)  |=AREATYPE_WORD;
    ATTRBYTE(pc+1)|=AREATYPE_WORD;
    lblnames[pc] = strdup("svec_RESERVED");
    }
#endif
  pc += 2;
  }

/* 6800/2/8 have only upper 4 */
else if (codes==m6800_codes)
  pc+=8;
  
/* 
 * RB: is this true?                                                         
 * Can we safely assume that the load address is a valid jump entry?         
 * 
 * HS: we have no way of knowing, but it would be a weird idea to explicitly
 * specify an entry point address that isn't. GIGO. 
 * 
 * RB: multiple ROMs (or better: ROM banks at the same logical address) disassembled individually might be a counterexample,
 * there, "spillover" from other banks might occur -- between banked and static code even within an instruction. 
 * quite a special occurrence, agreed, but I added a "noLoadLabel" option (default: FALSE) for such cases
 * 
 * HS: I've been thinking... there's another reason where it's good to have
 * "noLoadLabel". Motorola S-Files always have an entry point address;
 * default is 0 (which is ambiguous, since 0 might be the start address as well).
 *
 */
if ( (load >= 0) && (noLoadLabel == FALSE) )
  AddLabel(_jmp, (word)load);

if (infoname)                           /* now get all other settings        */
  processinfo(infoname, out, &fvec);    /* from info file                    */

/* set label names and attributes */
for(; pc<=0xfffe; pc+=2)
{
  /* precaution for case of funky HEX/S files
   * blank memory is filled with $010101... */
  if( ARGWORD(pc)!= 0x0101 &&
     !IS_CONST(pc) && /* only if not defined as constant!  */
     !IS_LABEL(pc))   /* only if not defined in info file! */
  {
    /* vectors are words and data */
    ATTRBYTE(pc)  |=AREATYPE_WORD|AREATYPE_LABEL;
    ATTRBYTE(pc+1)|=AREATYPE_WORD;

    /* add target address as type jump as we don't know 
     * if, where, and when we'll be hitting RTx */
    vaddr=ARGWORD(pc);  

    /* add handler label */
    ATTRBYTE(vaddr)|=AREATYPE_CODE|AREATYPE_CLABEL|AREATYPE_LABEL;

    /* enter system vector names and according handlers */
    lblnames[pc]=malloc(16);
    lblnames[vaddr]=malloc(16);

    if(codes==m6801_codes)
    {
      sprintf(lblnames[pc],    "svec_%s", vec_6801[(pc>>1)&7]);
      sprintf(lblnames[vaddr], "hdlr_%s", vec_6801[(pc>>1)&7]);
    }
    else
    {
      sprintf(lblnames[pc],    "svec_%s", vec_6809[(pc>>1)&7]);
      sprintf(lblnames[vaddr], "hdlr_%s", vec_6809[(pc>>1)&7]);
    }
  }
}

/* RB: any vector fields declared? */
if(fvec==TRUE)
{
  for(pc=0; pc<=0xffff; pc++)
  {

    /* vectors found? */
    if(IS_VEC(pc))
    {
      /* vectors are words and data */
      /* no label here, that needs to come from a specific LABEL statement */
      ATTRBYTE(pc)  |=AREATYPE_WORD;
      ATTRBYTE(pc+1)|=AREATYPE_WORD;

      /* get target */
      vaddr=ARGWORD(pc);

      /* target label not defined yet? */
      if(!IS_LABEL(vaddr))
      {
        lblnames[vaddr]=malloc(20*sizeof(char));

        ATTRBYTE(vaddr) |= AREATYPE_LABEL;

        /* code? set flag and jump mark */
        if(IS_CVEC(pc))
        {
          ATTRBYTE(vaddr) |= AREATYPE_CLABEL | AREATYPE_CODE;
          sprintf(lblnames[vaddr],"M%04X_via_cvec_%04x",vaddr,pc);
        }

        /* data? mark area as hex. */
        else
        {
          ATTRBYTE(vaddr) |= AREATYPE_HEX;
          sprintf(lblnames[vaddr],"M%04X_via_dvec_%04x",vaddr,pc);
        }
    }

    pc++;
    }
  }
}  

begin &= 0xFFFF;
end &= 0xFFFF;

pc = begin;                             /* pass 1 - generate labels          */
do
  {
  if (!IS_USED(pc))
    pc++;
  else if (IS_CONST(pc) || IS_DATA(pc))
    pc += ParseData(pc);
  else
    pc += Parse(pc);
  } while (pc <= end);

pc = begin;                             /* pass 2 - complete label references*/
do                                      /* (necessary for backward ref's)    */
  {
  if (!IS_USED(pc))
    pc++;
  else if (IS_CONST(pc) || IS_DATA(pc))
    pc += ParseData(pc);
  else
    pc += Parse(pc);
  } while (pc <= end);

                                        /* resolve all XXXXXXX+/-nnn labels  */
for (pc = 0x0000; pc <= 0xFFFF; pc++)
  {
  if (IS_ULABEL(pc))
    {
    char *p = label_string((word)pc, 1, (word)pc);
    char *q;
    if ((q = strchr(p, '+')) ||
        (q = strchr(p, '-')))
      {
      int nOff;
      if (q[1] == '$')
        sscanf(q + 2, "%x", &nOff);
      else if (q[1] == '%')
        {
        int i = 2;
        nOff = 0;
        while (q[i] == '0' || q[i] == '1')
          {
          nOff = (nOff << 1) | (q[i] - '0');
          i++;
          }
        }
      else if (q[1] == '\'')
        nOff = q[2];
      else
        sscanf(q + 1, "%d", &nOff);
      if (*q == '+')
        nOff = -nOff;
      ATTRBYTE(pc) &= ~(AREATYPE_LABEL | AREATYPE_ULABEL);
      nOff += (int)pc;
      ATTRBYTE(nOff) |= (AREATYPE_LABEL | AREATYPE_ULABEL);
      }
    }
  }

if (szPrepend)
  fprintf(out, "\n%s\n", szPrepend);

if (emitComments)
  fprintf(out,
          "\n"
          "%c****************************************************\n"
          "%c* Used Labels                                      *\n"
          "%c****************************************************\n"
          "\n", cCommChar, cCommChar, cCommChar);

                                        /* now print all labels that aren't  */
for (pc = 0x0000; pc <= 0xFFFF; pc++)   /* inside the used area              */
  {
  if ((!IS_USED(pc)) && (IS_ULABEL(pc)))
    {
    char *p = label_string((word)pc, 1, (word)pc);
    if ((!strchr(p, '+')) &&
        (!strchr(p, '-')))
      {
      if (commentlines[pc])
        {
        if ((byte)commentlines[pc][0] == (byte)0xff)
          fprintf(out, "%s\n", commentlines[pc] + 1);
        else
          {
          if ((*commentlines[pc]) && (*commentlines[pc] != '\n'))
            fprintf(out, "%c ", cCommChar);
          fprintf(out, "%s\n", commentlines[pc]);
          }
        }
#if RB_VARIANT
      fprintf(out, "%-24s\tEQU\t$%04X", p, pc);
#else
      fprintf(out, "%-7s EQU     $%04X", p, pc);
#endif
      if (emitComments && lcomments[pc])
        fprintf(out, "%21c %s", cCommChar, lcomments[pc]);
      fprintf(out, "\n");
      }
    }
  }

fprintf(out,
        "\n"
        "%c****************************************************\n"
        "%c* Program Code / Data Areas                        *\n"
        "%c****************************************************\n"
        "\n", cCommChar, cCommChar, cCommChar);

#if 0
if (codes == m6800_codes)
  fprintf(out, "        %-7s %s\n\n", "OPT", "M68");
#endif

curdp = dirpage;                        /* start with global direct page     */
if ((curdp != 0) && (codes != m6800_codes) && (codes != m6801_codes))
  {
  if (curdp > 0)
    fprintf(out, "        %-7s $%02X\n\n", "SETDP", curdp);
  else
    fprintf(out, "        %-7s\n\n", "SETDP");
  }

for (pc = 0x0000; pc <= 0xFFFF; pc++)
  if (IS_USED(pc))
    break;
if (pc > 0xffff)
  goto exit;
fprintf(out,"        %-7s $%04X\n\n", "ORG", pc);
do
  {
  char *slabel;
  int llen = 0;
  optdelimbar = FALSE;

  if (IS_LABEL(pc))
    {
    slabel = label_string((word)pc, 1, (word)pc);
    isautolabel = !label_at((word)pc);

#if RB_VARIANT
    /* RB:  make a stronger separation between data and code */
    if (lastwasdata && !(IS_CONST(pc) || IS_DATA(pc)) )
      fprintf(out, "%c------------------------------------------------------------------------\n",cCommChar);
    if (!isautolabel)
      fprintf(out, "\n");
#endif

    lastwasdata = FALSE;
    }

  if (codes != m6800_codes &&           /* deal with SETDP ranges            */
      codes != m6801_codes)
    {
    int newdp = GetDirPage(pc);
    if (newdp != curdp)
      {
      curdp = newdp;
      if (curdp >= 0)                   /* enable direct addressing          */
        fprintf(out, "        %-7s $%02X\n\n", "SETDP", curdp);
      else                              /* disable direct addressing         */
        fprintf(out, "        %-7s\n\n", "SETDP");
      }
    }

  {                                     /* deal with phases                  */
  int newphase = GetPhaseDef((word)pc);
  if (newphase != curphase)
    {
    curphase = newphase;
    if (curphase >= 0)
        fprintf(out, "        %-7s $%02X\n\n", "PHASE", phases[curphase].phase);
      else                              /* disable direct addressing         */
        fprintf(out, "        %-7s\n\n", "DEPHASE");
    }
  }
  
  if (commentlines[pc])
    {
    if ((byte)commentlines[pc][0] == (byte)0xff)
      fprintf(out, "%s\n", commentlines[pc] + 1);
    else
      {
      if ((*commentlines[pc]) && (*commentlines[pc] != '\n'))
        fprintf(out, "%c ", cCommChar);
      fprintf(out, "%s\n", commentlines[pc]);
      }
    }

  if (IS_LABEL(pc))                     /* if any label here                 */
    {
    if ((strchr(slabel, '+')) ||
        (strchr(slabel, '-')))
      slabel = "";
#if RB_VARIANT
    fprintf(out,
            (*slabel) ? 
                 (isautolabel) ?
                     "%s:\t" :
                     "%s:\n        " :
                 "%-7s ",
            slabel);
#else
    llen = fprintf(out, "%s", slabel);
    if (cLblDelim != ' ')
      llen += fprintf(out, "%c", cLblDelim);
    llen += fprintf(out, " ");
    if (llen < 8)
      llen += fprintf(out, "%-*s", 8 - llen, "");
#endif
    }
  else
    llen = fprintf(out, "%-*s ", 7, "");

  if (IS_CONST(pc) || IS_DATA(pc))
    {
    add = ShowData(out, pc, (nComment || lcomments[pc]));
    lastwasdata = TRUE;
    }
  else
    {
    add = Dasm(buf, pc);
  
    if (nComment || lcomments[pc])
      llen += fprintf(out,"%-32s",buf);
    else
      llen += fprintf(out,"%s",buf);
    }

  if (emitComments && (nComment || lcomments[pc]))
    llen += fprintf(out, " %c", cCommChar);
  if (showaddr)
    llen += fprintf(out,"%04X: ", pc);
  if ((showhex || showasc) && !IS_RMB(pc))
    {
    if (showhex)
      {
      for (i = 0; i < (int)add; i++)
        llen += fprintf(out,"%02X ", memory[(pc + i)&0xFFFF]);
      if (showasc || lcomments[pc])
        for (; i < 5; i++)
          llen += fprintf(out,"   ");
      }
    if (showasc)
      {
      llen += fprintf(out, "'");
      for (i = 0; i < (int)add; i++)
        {
        byte b = memory[(pc + i) & 0xFFFF];
        llen += fprintf(out, "%c", ((b >= 0x20) && (b <= 0x7e)) ? b : '.');
        }
      llen += fprintf(out, "'");
      if (emitComments && lcomments[pc])
        {
        for (; i < 5; i++)
          llen += fprintf(out, " ");
        llen += fprintf(out, " ");
        }
      }
    }
  if (emitComments && lcomments[pc])
    {
    if (!nComment)
      llen += fprintf(out, " ");
    llen += fprintf(out,"%s", lcomments[pc]);
    }

  pc += add;
  fprintf(out, "\n");
  llen = 0;

  while ((pc <= 0xffff) &&              /* skip unused bytes                 */
         (!IS_USED(pc)))
    pc++;

  if (curphase >= 0 &&                  /* phase definition change?          */
      curphase != GetPhaseDef((word)pc))
    fprintf(out, "\n        %-*s\n\n", 7, "DEPHASE");

  if ((pc < 0x10000) &&                 /* only if still in range,           */
      (!IS_USED(pc - 1)))               /* if we DID skip something set ORG  */
    fprintf(out, "\n        %-*s $%04X \n\n", 7, "ORG", pc);

#if RB_VARIANT
  /* RB: divider bar after jumps and jumpalikes */
  if (optdelimbar) 
    fprintf(out, "%c------------------------------------------------------------------------\n", cCommChar);
#endif
  } while (pc <= 0xffff);

fprintf(out, "\n");
if (load != -1)
  fprintf(out, "        %-7s %s\n", "END",
          label_string((word)load, 1, (word)load));
else
  fprintf(out, "        END\n");

exit:
if (outname)
  if(out)
    fclose(out);
if (memory)
  free(memory);
if (label)
  free(label);
if (used)
  free(used);
if (lblnames)
  free(lblnames);
if (commentlines)
  free(commentlines);
if (lcomments)
  free(lcomments);
if (rels)
  free(rels);
if (phases)
  free(phases);
if (dps)
  free(dps);
if (remaps)
  free(remaps);
if (szPrepend)
  free(szPrepend);  
return 0;
}
