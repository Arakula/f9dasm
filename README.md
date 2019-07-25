f9dasm
======

Disassembler for Motorola 6800/6801/6809 and Hitachi 6301/6303/6309 binaries

The documentation can be found in f9dasm.htm


Building
--------

`f9dasm`, `hex2bin` and `mot2bin` are command line tools written in
fairly generic C that should compile on a wide range of systems.

For Microsoft Visual Studio, `.dsp`, `.dsw`, `.sln`, and `.vcproj`
files are provided.

A `Makefile` is provided for Linux systems; this may also work on
other systems using GNU Make. You'll need to make sure you have the
appropriate development packages (`make`, `gcc`, etc.) installed. This
was tested on Debian 9.
