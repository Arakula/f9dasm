/*****************************************************************************/
/* mot2bin - converts Motorola hex file to .bin                              */
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

#define ATTRBYTE(address) (label[address & 0xffff])
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
printf("* Usage: mot2bin [options] <filename>\n"
       "* Available options are:\n"
       "*  -out    - output file [stdout]\n");

exit(1);
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
/* IsMotorolaHex : tries to load as a Motorola HEX file                      */
/*****************************************************************************/

int IsMotorolaHex(FILE *f, byte *memory, unsigned *pbegin, unsigned *pend, unsigned *pload)
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
    case '1' :
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
        ATTRBYTE(nAddr + i) |= 0x80;    /* mark as used byte                 */
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
  *pbegin = begin;
  *pend = end;
  if (load >= 0)
    *pload = load;
  }

return (nBytes > 0);                    /* pass back #bytes interpreted      */
}

/*****************************************************************************/
/* main : main program function                                              */
/*****************************************************************************/

int main(int argc, char *argv[])
{
unsigned begin = 0,end = 0, offset = 0;
char *fname = NULL, *outname = NULL;
int load = -1;
int i, j, n;
int off;
FILE *f;
FILE *out = stdout;

printf("mot2bin: convert Motorola .HEX to Binary\n");

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

if ((IsMotorolaHex(f, memory, &begin, &end, &load)) &&
    (out != stdout))
  fwrite(memory + begin, end - begin + 1, 1, out);

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
