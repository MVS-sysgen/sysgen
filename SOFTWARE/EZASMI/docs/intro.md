TCP/IP for MVS 3.8 Assembler Introduction
=========================================

This document describes the assembler language application programming
interface for MVS 3.8. This API is based upon the z/OS Communications
Server EZASMI macro, and is upwardly compatible with it. TCP/IP for MVS
3.8 Assembler is a small subset of the function provided by the z/OS
Communications Server, but is sufficient for developing fully functional
TCP/IP applications in assembler language on MVS 3.8. The information
provided here concentrates on the differences between the two
implementations. Where appropriate, links to the z/OS Communications
Server API will be provided. For complete information on the z/OS EZASMI
macro API, please see www.ibm.com/support/knowledgecenter/SSLTBW_2.2.0/com.ibm.zos.v2r2.hala001/macro.htm

TCP/IP for MVS 3.8 Assembler is built upon the original EZASOKET
interface developed by Jason Winter. It has been extended to provide
additional functionality, improved error detection, and compatibility
with the EZASMI macros. It ships as a load module, EZASOH03 included in
this package, which must be linked with your TCP/IP application.

Like EZASOKET, EZASOH03 is dependent upon the TCPIP instruction
(X\'75\'), which is provided in the Hercules emulator. Note that
currently, only the Hercules emulator included with TK4- includes the
implementation of the TCPIP instruction required by EZASOH03. You should
be aware that neither Hercules nor MVS 3.8 provides a TCP/IP
implementation. The actual TCP/IP functionality is provided by the
Hercules host TCP/IP package, either Winsock (Microsoft Windows), or BSD
Sockets (Linux), and is accessed by the TCPIP instruction. The final
limiting factor on the functionality provided by the EZASMI API will be
determined by the underlying host TCP/IP package.
