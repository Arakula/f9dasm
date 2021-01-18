/*****************************************************************************/
/* cmd2mot - converts FLEX CMD file to Motorola hex                          */
/*****************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

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

/*****************************************************************************/
/* Global Data                                                               */
/*****************************************************************************/

byte *memory = NULL;
byte *label = NULL;
char **lblnames = NULL;
char **commentlines = NULL;

static char *Options[]=
  {
  "out",
  NULL
  };

#define OPCODE(address)  memory[address&0xffff]
#define ARGBYTE(address) memory[address&0xffff]
#define ARGWORD(address) (word)((memory[address&0xffff]<<8)|memory[(address+1)&0xffff])

#define ATTRBYTE(address) (label[(address) & 0xffff])
#define LABELTYPE(address) (ATTRBYTE(address) & 0x03)
#define LABELTYBE_ISLABEL (0x02)
#define LABEL_REFERENCED(address) ((ATTRBYTE(address) & 0x04) >> 2)
#define USEDBYTE(address) ((ATTRBYTE(address) & 0x80) >> 7)
#define AREATYPE(address) ((ATTRBYTE(address) & 0x78) >> 3)
#define AREATYPE_CODE 0x40
#define AREATYPE_DATA 0x20
#define AREATYPE_BINARY (AREATYPE_DATA | 0x10)
#define AREATYPE_WORD   (AREATYPE_DATA | 0x08)

/*****************************************************************************/
/* usage : displays usage                                                    */
/*****************************************************************************/

void usage(void)
{
printf("* Usage: cmd2mot [options] <filename>\n"
       "* Available options are:\n"
       "*  -out    - output file [stdout]\n");

exit(1);
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

int IsFlex(FILE *f, byte *memory, unsigned *pbegin, unsigned *pend, long *load)
{
struct SFlexRecord rec;
int nCurPos = ftell(f);
int nRecs = 0;
int bExecutable = 0;
int begin = 0x10000;
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
      ATTRBYTE(i) |= 0x80;              /* mark as used byte                 */

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
    ATTRBYTE(i) &= ~0x80;               /* mark as used byte                 */
  nRecs = 0;                            /* this ain't no valid FLEX file     */
  }

fseek(f, nCurPos, SEEK_SET);
if (nRecs > 0)
  {
  if (begin < (int)(*pbegin))
    *pbegin = begin;
  if (end > (int)*pend)
    *pend = end;
  }
return (nRecs > 0);
}

/*****************************************************************************/
/* writeS1rec : write Motorola S1 record                                     */
/*****************************************************************************/

void writeS1rec(FILE *objfile, unsigned hexaddr, int hexcount)
{
int i;
int chksum = 0;

fprintf(objfile, "S1%02X%04X", (hexcount + 3) & 0xff, hexaddr & 0xffff);
for (i = 0; i < hexcount; i++)
  {
  byte b = memory[hexaddr + i];
  chksum += b;
  fprintf(objfile, "%02X", b);
  }
chksum += (hexaddr & 0xff) + ((hexaddr >> 8) & 0xff) + hexcount + 3;
fprintf(objfile, "%02X\r\n", 0xff - (chksum & 0xff));
}

/*****************************************************************************/
/* writeS9rec : write Motorola S9 record                                     */
/*****************************************************************************/

void writeS9rec(FILE *objfile, unsigned tfraddr)
{
int chksum = (tfraddr & 0xff) + ((tfraddr >> 8) & 0xff) + 3;
fprintf(objfile, "S903%04X%02X\n", tfraddr, 0xff - (chksum & 0xff));
}

/*****************************************************************************/
/* writeS1range : writes out a memory range as Motorola S1 records           */
/*****************************************************************************/

void writeS1range(FILE *objfile, unsigned hexaddr, int hexcount)
{
while (hexcount > 16)
  {
  writeS1rec(objfile, hexaddr, 16);
  hexaddr += 16;
  hexcount -= 16;
  }
writeS1rec(objfile, hexaddr, hexcount);
}


/*****************************************************************************/
/* main : main program function                                              */
/*****************************************************************************/

int main(int argc, char *argv[])
{
unsigned begin = 0x10000,end = 0, offset = 0;
long load = -1;
char *fname = NULL, *outname = NULL;
int i, j, n;
int off;
FILE *f;
FILE *out = stdout;

printf("cmd2mot: convert FLEX Binary to Motorola .HEX\n");

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
        usage();
      }
    }
  else
    {
    for (j = 0; Options[j]; ++j)
      if (!strcmp(argv[i] + 1, Options[j]))
        break;

    switch (j)
      {
      case 0:
        ++i;
        if (i > argc)
          usage();
        outname = argv[i];
        break;
        
      default:
        usage();
      }
    }
  }

if (!fname)
  usage();
f = fopen(fname,"rb");
if (!f)
  usage();
if (!end)
  {
  fseek(f,0,SEEK_END);
  off = ftell(f);
  end = (offset+off)-1;
  rewind(f);
  }

memory = (byte *)malloc(0x10000);
label = (byte *)malloc(0x10000);
if ((!memory) || (!label))
  {
  printf("no mem buffer\n");
  goto exit;
  }
memset(memory, 0x01, 0x10000);
memset(label, 0x00, 0x10000);

if (outname)
  {
  out = fopen(outname, "wb");
  if (!out)
    {
    printf("can't open %s \n",outname);
    out = stdout;
    }
  }

if ((IsFlex(f, memory, &begin, &end, &load)) &&
    (out != stdout))
  {
  unsigned long blkstrt = begin;
  while (blkstrt <= end)
    {
    unsigned long after = blkstrt;
    while (after <= end && (ATTRBYTE(after) & 0x80))
      after++;
    writeS1range(out, blkstrt, after - blkstrt);
    blkstrt = after;
    while (blkstrt <= end && !(ATTRBYTE(blkstrt) & 0x80))
      blkstrt++;
    }
  if (load >= 0 && load <= 0xffff)
    writeS9rec(out, (unsigned int)load);
  }

exit:
if (f)
  fclose(f);
if (outname)
  if(out)
    fclose(out);
if (memory)
  free(memory);
  
return 0;
}
