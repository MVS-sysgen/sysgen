Using the EZASMI Macro
======================

This section provides information on using the EZASMI macro with TCP/IP
for MVS 3.8 Assembler. References to the corresponding z/OS API are
provided along with the limitations and differences comprising the
TCP/IP for MVS 3.8 Assembler EZASMI implementation. Note that TCP/IP for
MVS 3.8 Assembler only supports IPv4; any references to IPv6 in the z/OS
documentation (e.g. INET6) do not apply.

When any modules assembled using the EZASMI macro are subsequently
linked, the EZASOH03 module must be included in the resultant load
module. This is most easily accomplished by simply including
SYS2.LINKLIB in the SYSLIB DD concatenation for your link step. Note:
Ensure that you have not specified the NCAL linkage editor option.
