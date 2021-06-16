INITAPI
=======

TCP/IP for MVS 3.8 Assembler requires that the **INITAPI** function be
explicitly executed, or that one of the following functions which
implicitly invoke **INITAPI** be executed before performing any other
EZASMI function requests: **GETHOSTBYADDR**, **GETHOSTBYNAME**,
**SELECT**, or **SOCKET**. Note that there is no TCPIP.DATA to be read.

Parameter Considerations
------------------------

None.

Unsupported Parameters
----------------------

The only parameters supported by TCP/IP for MVS 3.8 Assembler are
**TASK** and **MF**. Any other parameters will be accepted, but will not
affect INITAPI processing.

Note that with z/OS, the **MAXSOC** parameter may be used to specify the
maximum number of sockets supported by the application, with a maximum
value of 65535. With MVS, the number of sockets supported by an
application is fixed at 1023 (socket numbers 1 through 1023).
