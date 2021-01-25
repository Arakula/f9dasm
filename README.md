# F9DASM - 6800/6801/6802/6803/6808/6809 / 6301/6303/6309 Disassembler

Copyright (c) 2000 Arto Salmi  
&#13;&#10;Parts Copyright (c) 2001-2020 Hermann Seib  
&#13;&#10;Parts Copyright (c) 2013 Colin Bourassa  
&#13;&#10;Parts Copyright (c) 2014-2015 Rainer Buchty

Based on Arto Salmi's C core that can be found somewhere on the 'net 
(last address known to me was
[http://koti.mbnet.fi/~atjs/mc6809/Disassembler/dasm09.TGZ](http://koti.mbnet.fi/~atjs/mc6809/Disassembler/dasm09.TGZ)),
I built a complete 6800/6809/6309 disassembler that can handle input files
in a variety of formats (Intel Hex / Motorola S09 / Flex9 Binary / Binary).
Since disassembly without guidance produces measly results, it can load
information files with quite a lot of directives, too.

I taylored the original to my taste by working through the source code; since 
F9DASM has reached a level of complexity that doesn't really lend itself to following the "Use 
the Source, Luke!" principle if you just want to disassemble a little 6809 
program, I've added this documentation. Have fun!

Hermann Seib, 2015

## Building

`f9dasm`, `hex2bin` and `mot2bin` are command line tools written in
fairly generic C that should compile on a wide range of systems.

For Microsoft Visual Studio, `.dsp`, `.dsw` (Visual Studio 6), `.sln`, and `.vcproj`
files are provided.

A `Makefile` is provided for Linux systems; this may also work on
other systems using GNU Make. You'll need to make sure you have the
appropriate development packages (`make`, `gcc`, etc.) installed. This
was tested on Debian 9.

## Syntax

`f9dasm [-option]* [filename]`

### Command Line Options

<dl>
  <dt><b>-offset <i>address</i></b></dt>
  <dd>When disassembling a binary file, the default load address
	  is 0, since the binary file does not contain any clues; using this
	  option forces F9DASM to load the file at the specified address.</dd>
  <dt><b>-begin <i>address</i></b></dt>
  <dd>start disassembly address (<i>address</i> has to be given in hex format)<br>
      Default is the first referenced address in the file.</dd>
  <dt><b>-end <i>address</i></b></dt>
  <dd>end disassembly address (<i>address</i> has to be given in hex format)<br>
      Normally, this is defined either through the file size or its contents,
	  if it has an embedded END addres. This option allows to override the implicit
	  end address.</dd>
  <dt><b>-out <i>filename</i></b></dt>
  <dd>normally, f9dasm streams to standard output;
      this option forces it to write to the specified file instead.</dd>
  <dt><b>-[no]addr</b></dt>
  <dd>if disabled, suppresses the address field output for clean assembler source files (default is enabled)</dd>
  <dt><b>-[no]hex</b></dt>
  <dd>disables or enables hex dump output (default is enabled).<br>
  While analyzing a file, the hex dump can be quite helpful; if you want to generate a
  clean assembler source file, you can disable it.</dd>
  <dt><b>-x</b>, <b>-6309</b></dt>
  <dd>puts disassembler in 6309 mode (default is 6809).</dd>
  <dt><b>-6800</b>, <b>-6802</b>, <b>-6808</b> </dt>
  <dd>puts disassembler in 6800/6802/6808 mode (default is 6809).</dd>
  <dt><b>-6801</b>, <b>-6803</b></dt>
  <dd>puts disassembler in 6801/6803 mode (default is 6809).</dd>
  <dt><b>-6301</b>, <b>-6303</b></dt>
  <dd>puts disassembler in 6301/6303 mode (default is 6809).</dd>
  <dt><b>-os9</b></dt>
  <dd>using this flag, the disassembler tries to convert swi2 to the
      corresponding OS/9 calls.</dd>
  <dt><b>-info <i>filename</i></b></dt>
  <dd><i>filename</i> gives an information file which contains additional
      hints for the disassembler. See the <b>Info File</b> section below.<br>
      Passing the file name <i>help</i> displays help for the information file.</dd>
  <dt><b>-cchar <i>char</i></b></dt>
  <dd><i>char</i> is the character to be used as comment delimiter.<br>
      Default is <b>;</b>, but not all assemblers can use this.<br>
	  <b>-cchar *</b> would switch the comment delimiter to <b>*</b>, for example,
	  which might be better if a TSC-compatible assembler is used.</dd>
  <dt><b>-[no]flex</b></dt>
  <dd>using this flag, the disassembler uses the standardized FLEX labels for a defined
      range of addresses.</dd>
  <dt><b>-[no]conv</b></dt>
  <dd>using this flag, the disassembler outputs various "convenience" mnemonics 
      (like, for example, <b>CLRD</b> instead of <b>CLRA</b> followed by <b>CLRB</b>).<br>
      Default is on, but not all assemblers support this, so f9dasm can be forced
	  to stick to the base set of mnemonics.<br>
    f9dasm's companion, <a href="https://github.com/Arakula/A09">A09</a>, can of course handle them :-)</dd>
  <dt><b>-[no]dec</b></dt>
  <dd>can be used to output values in decimal notation (by default it's hexadecimal).</dd>
  <dt><b>-[no]comment</b></dt>
  <dd>can be used to enable or disable the output of comments (which can be provided
     in an info file, see below).<br>
	 Normally, comments are enabled.</dd>
  <dt><b>-[no]asc</b></dt>
  <dd>can be used to enable or disable output of the ASCII equivalent to code/data.<br>
  Default is to output ASCII</dd>
  <dt><b>-[no]fcc</b></dt>
  <dd>can be used to enable or disable the use of FCC to define data (instead of FCB or FDB)<br>
  Default is to use FCC</dd>
  <dt><b>-omitzero</b></dt>
  <dd>omit indexed-mode operands of $00 (default)</dd>
  <dt><b>-showzero</b></dt>
  <dd>do not omit indexed-mode operands of $00<br>
  This only works for 6800-based code; in 6809-based code, omitting the zero is encoded differently.</dd>
  <dt><b>-[no]forced</b></dt>
  <dd>using this flag, the assembler outputs <i>forced direct (&lt;) or extended (&gt;)
  addressing markers</i> where this is necessary to ensure an exact reproduction.<br>
  This is based on the TSC Assembler's syntax, which is not necessarily universal,
   so it can be turned off.</dd>
  <dt><b>-ldchar <i>char</i></b></dt>
  <dd><i>char</i> is the character to be used as label delimiter.<br>
      Default is nothing, but not all assemblers can use this; some need a colon, for example.<br>
	  <b>-ldchar :</b> can be used to switch the label delimiter to <b>:</b> in this case.</dd>
  <dt><b>-help</b></dt>
  <dd>outputs an abbreviated version of this documentation.</dd>
</dl>

## Info File
Using the <b>-info <i>filename</i></b> option, you can give f9dasm additional
information about the layout of the processed file.

Normally, f9dasm will try to interpret everything as <i>code</i>;
it doesn't try to find out which areas contain code and which areas contain data,
or the format of the data. Using an info file, you can give it detailed instructions
how to process the file, add comments, insert additional stuff, and so on.

The info file is a simple text file, which contains one instruction per line.  
&#13;&#10;The instructions are case-insensitive.  
&#13;&#10;Addresses need to be given in <i>hexadecimal</i> notation.
Anything following an asterisk (<b>*</b>) is interpreted as a comment.

The info file can contain the following instructions:

<dl>
  <dt><b>file <i>filename [baseaddr]</i></b></dt>
  <dd>This instructs f9dasm to load the given file at the given address.<br>
      Can be used instead of the command line parameter for the file name;
	  this can, for example, be useful if you want to generate a listing for a
	  bunch of small EPROMs that cover a continuous memory area.</dd>
  <dt><b>option <i>option [value]</i></b></dt>
  <dd><i>option</i> is one of the options listed above, just without the leading
      hyphen (-).</dd>
  <dt><b>code <i>addr[-addr]</i></b></dt>
  <dd>defines the given address (range) as containing code.</dd>
  <dt><b>data <i>addr[-addr]</i></b></dt>
  <dd>defines the given address (range) as containing data instead of code.<br>
      f9dasm will try to decipher ASCII strings in the area and display them
	  in the best possible format.</dd>
  <dt><b>bin[ary] <i>addr[-addr]</i></b><br />
  <b>char <i>addr[-addr]</i></b><br />
  <b>dec[imal] <i>addr[-addr]</i></b><br />
  <b>hex[adecimal] <i>addr[-addr]</i></b></dt>
  <dd>defines the output format used for the given data range.<br>
  This can also be used for constants in the <i>code</i> area; if, for example,
  f9dasm outputs the following line of code:<br>
  <pre>        LDA     #$D6                     *C115: 86 D6</pre>
  and you know pretty well that this is a binary bit mask, you can force it to
  display the data in a nicer format by giving the instruction
  <b>bin c116</b> (note that the address of the constant byte is given,
  <i>not</i> the address of the instruction!).
  This results in the modified output
  <pre>        LDA     #%11010110               *C115: 86 D6</pre>
  which may be easier to read (depending on your mental approach to assembler
  programming :-).<br>
  Note that <b>char</b> and <b>bin</b> can not be used for word areas (see below).</dd>
  <dt><b>word <i>addr[-addr]</i></b></dt>
  <dd>defines that the area consists of <i>words</i> (i.e., 2-byte entities,
      high byte first) instead of single bytes.</dd>
  <dt><b>const <i>addr[-addr]</i></b></dt>
  <dd>defines the data in the given range as <i>constants</i>.<br>
      Normally, if f9dasm can interpret the data as addresses, it will;
	  and if there's a label defined for this address, it will display
	  the label instead of the original value.</dd>
  <dt><b>unused <i>addr[-addr]</i></b></dt>
  <dd>defines the given address range as unused.<br>
      This can be useful if an area in the loaded file is known to be empty;
	  there's no need to put it into the generated assembler source.</dd>
  <dt><b>rmb <i>addr[-addr]</i></b></dt>
  <dd>defines the given address range as reserved, but not initialized to
      defined values.<br>
  </dd><dt><b>label <i>addr name</i></b></dt>
  <dd>sets a label at the given address.<br>
      Note that f9dasm doesn't restrict the length of the label, nor does it enforce
	  a given range of characters (except for * and zero bytes - these terminate
	  the name). This may conflict with the assembler of your choice, so choose
	  the labels withg caution.</dd>
  <dt><b>used[label] <i>addr [name]</i></b></dt>
  <dd>forces the given address used.<br>
	  Normally, f9dasm would only emit a label definition in the form of an
	  EQU statement if the label is really used in the code.</dd>
  <dt><b>unlabel <i>addr[-addr]</i></b></dt>
  <dd>removes defined labels in the given address range.<br>
      This is mainly useful if you use a set of info files (see the
	  <b>include</b> instruction below) and want to remove label definitions
	  from an earlier info file.</dd>
  <dt><b>insert <i>addr[-addr] text</i></b></dt>
  <dd>This instruction adds the given text to the text lines which
      are displayed before a given (range of) address(es).<br>
	  In contrast to a comment, there's no comment character written before
	  the text, which allows to insert any assembler statement or pseudo-op.</dd>
  <dt><b>comment <i>addr[-addr] text</i></b></dt>
  <dd>appends a comment to the lines displayed before a given address (range).</dd>
  <dt><b>lcomment <i>addr[-addr] text</i></b></dt>
  <dd>appends a <i>line</i> comment to the lines displayed before a given address
      (range).<br>
	  A line comment is displayed as a comment to the right of the instruction.<br>
	  If more than one line comment is given, they are displayed on separate lines,
	  but all to the right of the instruction.</dd>
  <dt><b>prepend <i>addr[-addr] text</i></b></dt>
  <dd>This instruction <i>prepends</i> the given text to the text lines which
      are displayed before a given (range of) address(es).<br>
	  This is mainly useful if you use a set of info files (see the
	  <b>include</b> instruction below) and want to add additional text from
	  a later info file before text lines from an earlier info file.</dd>
  <dt><b>prepcomm <i>addr[-addr] text</i></b></dt>
  <dd>Same as <b>prepend</b> (see above), but it prepends a <i>comment</i> 
      instead of a normal text line.</dd>
  <dt><b>preplcom[ment] <i>addr[-addr] text</i></b></dt>
  <dd>prepends a line comment to the lines displayed before a given address
      (range).</dd>
  <dt><b>uncomment <i>addr[-addr]</i></b></dt>
  <dd>removes <b>insert</b> and <b>comment</b> lines from the given address range.<br>
      This is mainly useful if you use a set of info files (see the
	  <b>include</b> instruction below) and want to remove comments
	  from an earlier info file so that you can replace them.</dd>
  <dt><b>unlcomment <i>addr[-addr]</i></b></dt>
  <dd>removes line comments from the given address range.<br>
      This is mainly useful if you use a set of info files (see the
	  <b>include</b> instruction below) and want to remove line 
	  comments from an earlier info file.</dd>
  <dt><b>include <i>filename</i></b></dt>
  <dd>includes the given info file.</dd>
  <dt><b>setdp <i>[addr[-addr]] dp-content</i></b></dt>
  <dd>uses the specified direct page (if the specified processor can 
      do that - 6800 and its derivates implicitly uses Direct Page 0).<br>
      To allow usage with programs that repeatedly change the direct page,
      an address range can be given.<br>
      If only the start address is given, the range is assumed to start there
      and go up to the end of the address range.<br>
      If no address is given, the global direct page is changed.<br>
      Any value between 00 and FF sets the direct page (0 is the default);
      values outside this range <i>disable</i> direct page processing by forcing f9dasm to
      emit a
<pre>        SETDP</pre>
      line which 6809 assemblers normally interpret as "don't use direct page addressing".<br>
      Only the last global <b>setdp</b> statement is used.</dd>
  <dt><b>unsetdp <i>[addr[-addr]]</i></b></dt>
  <dd>removes <i>setdp</i> effects from the given range or, if no range is given,
      sets the global direct page back to the default.</dd>
  <dt><b>rel[ative] <i>addr[-addr] baseaddr</i></b></dt>
  <dd>This can be used to make instructions with indexed addressing easier
      to read.</dd>
  <dt><b>unrel[ative] <i>addr[-addr]</i></b></dt>
  <dd>cancels the effect of <b>relative</b> instructions.</dd> 
  <dt><b>remap <i>addr[-addr] offset</i></b></dt>
  <dd>This is a tricky instruction that only makes sense in very special
      situations; imagine, for example, that you already have an elaborate
	  info file for a specific EPROM - and then you get another EPROM that
	  contains <i>nearly</i> the same stuff, just with one or two instructions
	  added or missing.<br>
	  You could, of course, adapt all instructions in your info file to the new
	  address layout.<br>
	  Would you? I wouldn't.<br>
	  In this case, it's easier to prepend a <b>remap</b> instruction that tells
	  f9dasm to "shift" the following addresses in the info file some bytes.</dd>
  <dt><b>phase <i>addr[-addr] [+|-]phase</i></b></dt>
  <dd>Sometimes, an EPROM can contain data that are mapped to a different location;
      in this case, all jump targets etc. inside that area are incorrect,
      i.e., "out of phase".
      The <b>phase</b> instruction tells F9DASM to disassemble that area as if it
      was starting at the address given in <b><i>phase</i></b>.<br>
      Using this instruction embeds PHASE/DEPHASE pseudo-ops in the generated source code, so
      an Assembler that can process these pseudo-ops is required
      (the mighty <a href="http://john.ccac.rwth-aachen.de:8000/as/">AS</a> can do it,
      and my <a href="https://github.com/Arakula/A09">A09</a> can do it since V1.30, too).<br>
      This deals with one part of the problem - data inside phased areas are
      disassembled correctly. But to reference them correctly from <i>outside</i> the
      phased area requires additional, <b><i>relative</i></b> phase statements.<br>
      Relative phase statements are made by adding a + or - before <b><i>phase</i></b>
      (which in this case is a relative value).<br>
      <b>Attention:</b> a relative phase statement can only be set <i>inside</i> a phased
      area; so, if relative phasing is required, <i>all</i> areas in the disassembled module
      that use it have to be put into a phased area, even the "un-phased" ones, like:
      <pre>phase 8000-ffff 8000</pre>
      and relative phases have to be defined <i>after</i> the phase area definition.<br>
      If your Assembler of choice can't handle phasing, all you can do is to cut the binary
      in slices, use the <b>-offset</b> command line option to disassemble them,
      and merge the generated assembler source files by hand.
      </dd>
  <dt><b>cvec[tor] <i>addr[-addr]</i></b></dt>
  <dd>defines a <i>code vector</i> area (a table of code addresses).<br>
      Works like <b>word <i>addr[-addr]</i></b>, but also defines code labels for
	  the addresses if necessary</dd>
  <dt><b>dvec[tor] <i>addr[-addr]</i></b></dt>
  <dd>defines a <i>data vector</i> area (a table of data addresses).<br>
      Works like <b>word <i>addr[-addr]</i></b>, but also defines data labels for
	  the addresses if necessary</dd>
  <dt><b>patch <i>addr[-addr] hex [hex]*</i></b></dt>
  <dd>Can be used to patch bytes in a loaded file. Using this feature, modified versions
      of a file can be created automatically.<br>
      <a href="https://github.com/Arakula/A09">A09</a> can be used to emit patch
      instructions instead of listings to make it easier to develop a modified version
      of a program.</dd>
  <dt><b>patchw <i>addr[-addr] word [word]*</i></b></dt>
  <dd>Can be used to patch words (i.e., 2-byte entities) in a loaded file.</dd>
  <dt><b>end</b></dt>
  <dd>Terminates processing of this info file at once.</dd>
</dl>

<i><b>Note:</b></i> wherever <i><b>text</b></i> requires leading blanks or tabs,
it isn't possible to simply put them into the instruction, as f9dasm would discard them.
For these cases, simply prepend a '\\' (backslash) to the text, like, for example, in
```insert 0 \        OPT H63```
so that f9dasm knows where <i><b>text</b></i> really starts.
Despite the similarity, f9dasm doesn't know the range of "C" escape characters; a '\\'
simply means "Ignore this backslash, but make sure the next character is part of the text".
If you need to have a <b>\*</b> as part of the text, it is mandatory to write it as <b>\\*</b>;
f9dasm would otherwise assume the line ends at the <b>\*</b>, which starts the comment part.
