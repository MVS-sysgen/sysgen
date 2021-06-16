# EZASMI - TCP/IP for MVS 3.8 Assembler Introduction

This installs the EZASMI socket API to sysgen.

TCP/IP for MVS 3.8 Assembler is built upon the original EZASOKET interface developed by Jason Winter. It has been extended to provide additional functionality, improved error detection, and compatibility with the EZASMI macros. It ships as a load module, EZASOH03 included in this package, which must be linked with your TCP/IP application.


Version 1.0.0 by Shelby Lynne Beach and Jürgen Winkelmann

## Install

:warning: To install EZASMI you need to install `SYS2.MACLIB` and `SYS2.SXMACLIB` by running `INSTALL EXTRAS` in TSO

To install in TSO issue the command `INSTALL EZASMI`.

The macros needed are installed to `SYS2.MACLIB`, the `EZASOH03` module is in `SYS2.LINKLIB`.

## Samples

This library comes with two examples located in `SYSGEN.TCPIP.SAMPLIB` (or in the SAMPLIB folder). The files `COMPNCAT.jcl` and `CNSLOOKUP.jcl` (or `SYSGEN.TCPIP.SAMPLIB(COMPNCAT)`/`SYSGEN.TCPIP.SAMPLIB(CNSLOOKUP)` ) provides an example how to assemble and link programs that use the EZASMI macros.

## Documentation

The `docs` folder contains the EZASMI documentation

