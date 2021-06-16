.*------------------------------------------------------------------- *
.*                                                                    *
.*                   EZASMI Macro                                     *
.*                                                                    *
.*  Function:  The EZASMI macro provides MVS 3.8 with an interface    *
.*             to the TCP/IP stack developed by Jason Winter for      *
.*             MVS 3.8. This macro is designed as a subset of the     *
.*             z/OS Communications Server ESASMI macro, and provides  *
.*             an API that is programmatically, though not            *
.*             functionally, a subset of that provided on z/OS. The   *
.*             available function types are limited to those included *
.*             in the MVS 3.8 TCP/IP stack, and not all of options    *
.*             for the available functions are supported.             *
.*             Where possible, unsupported macro parameters are       *
.*             accepted, allowing a developer to port a z/OS TCP/IP   *
.*             application down to MVS 3.8 with minimal changes to    *
.*             the EZASMI macro instructions in the application.      *
.*                                                                    *
.*  Author:    Shelby Beach, Ms. Kitty Programming Werks              *
.*                                                                    *
.*  Change Log:                                                       *
.*                                                                    *
.*    Date     Pgmr   Change                                          *
.*  12/01/2016  SLB   Initial release                                 *
.*                                                                    *
.*------------------------------------------------------------------- *
.*
.*------------------------------------------------------------------- *
.* Below are the keyword parameters accepted by the MVS EZASMI macro. *
.* Those keywords flagged with an asterisk(*) are z/OS keywords that  *
.* are accepted but not supported by MVS. This allows code to be      *
.* ported down to MVS from z/OS, but the user is responsible for      *
.* verifying that the program will still execute correctly.           *
.*------------------------------------------------------------------- *
.*
         MACRO
&LABEL   EZASMI &TYPE=,        EZASOH03 function name                  +
               &AF=,           Internet protocol                       +
               &ALET,         *Address space token                     +
               &APITYPE=,     *API type number                         +
               &ASYNC=,       *Asynchronous support option             +
               &BACKLOG=,      Number of mesgs that can be backlogged  +
               &BUF=,          RECV/SEND buffer address                +
               &COMMAND=,      IOCTL command options                   +
               &DSTADDR=,      Converted character IP address          +
               &DSTLEN=,       Len DSTADDR; returned len char string   +
               &ECB=,         *ECB to be posted upon completion        +
               &ERETMSK=,      Bit-mask returned for ESNDMSK           +
               &ERRNO=,        Error number                            +
               &ERROR=,       *Routine to handle errors loading the API+
               &ESNDMSK=,      Bit-mask for exception interrupts       +
               &FLAGS=,       *SEND/RECV flags                         +
               &HOSTADR=,      Internet host address                   +
               &HOSTENT=,      Returned address of HOSTENT             +
               &HOW=,          SHUTDOWN type                           +
               &IDENT=,       *Address space identifiers               +
               &MAXSNO=,      *Max socket number assigned to app       +
               &MAXSOC=,       Max number of sockets supported         +
               &MF=,           Macro form                              +
               &NAME=,         Socket address structure                +
               &NAMELEN=,      Length of name value                    +
               &NBYTE=,        Size of BUF                             +
               &NS=,          *New socket number                       +
               &PROTO=,        Protocol supported                      +
               &RETCODE=,      Return code/returned data               +
               &REQAREA=,     *Token passed to user exit               +
               &REQARG=,       Parameters to/from IOCTL                +
               &RETARG=,       Parameter returned by IOCTL             +
               &RRETMSK=,      Bit-mask returned for RSNDMSK           +
               &RSNDMSK=,      Bit-mask for read interrupts            +
               &S=,            Socket descriptor                       +
               &SOCTYPE=,      Socket type                             +
               &SRCADDR=,      IP address to convert character         +
               &SRCLEN=,       Len of IP address to be converted       +
               &STORAGE=,      Task storage option                     +
               &SUBTASK=,     *Subtask identity in address space       +
               &TASK=,         Location of task storage area           +
               &TIMEOUT=,      SELECT wait time interval               +
               &UEEXIT=,      *Unsolicitated event data                +
               &WRETMSK=,      Bit-mask returned for WSNDMSK           +
               &WSNDMSK=       Bit-mask for write interrupts
         LCLA  &I              Loop index
         LCLC  &MACFORM        Macro form
         LCLC  &FUNC           EZASOH03 function name
.*
         GBLC  &GPR            GPR containing addr of EZASOH03 parm
.*
.*--------------------------------------------------------------------*
.* Analyze MF parameter                                               *
.*--------------------------------------------------------------------*
&MACFORM SETC  ''                      Assume no MF parm
         AIF   (K'&MF EQ 0).SETKW      MF not coded
         AIF   ('&MF(1)' NE 'E').MFL   MF=E ?
&MACFORM SETC  'E'                     Yes, set macro form
         AGO   .SETKW
.MFL     ANOP
         AIF   ('&MF' EQ 'L').LISTMF   MF=L
         MNOTE 12,'Invalid MF parm'
         MEXIT
.SETKW   ANOP
.*
.*------------------------------------------------------------------- *
.* The following arrays are used in validating the parameters         *
.* specified for a given invocation of the EZASMI macro. The order    *
.* of the elements in each of the arrays must be maintained. If new   *
.* elements are to be added, it is best to add them to the end of     *
.* each array (alphabetic order is not required). Note that the size  *
.* of all arrays must be identical, including global references in    *
.* the EZASMIA macro. If new entries are added, be sure to adjust     *
.* the loop index limits both here and in EZASMIA.                    *
.*------------------------------------------------------------------- *
.*
.* EZASMI keyword parameters array
.*
         GBLC  &KW(31)
.*
&KW(1)   SETC  'AF'
&KW(2)   SETC  'BACKLOG'
&KW(3)   SETC  'BUF'
&KW(4)   SETC  'COMMAND'
&KW(5)   SETC  'DSTADDR'
&KW(6)   SETC  'DSTLEN'
&KW(7)   SETC  'ERETMSK'
&KW(8)   SETC  'ERRNO'
&KW(9)   SETC  'ESNDMSK'
&KW(10)  SETC  'HOSTADR'
&KW(11)  SETC  'HOSTENT'
&KW(12)  SETC  'MAXSOC'
&KW(13)  SETC  'MF'
&KW(14)  SETC  'NAME'
&KW(15)  SETC  'NAMELEN'
&KW(16)  SETC  'NBYTE'
&KW(17)  SETC  'PROTO'
&KW(18)  SETC  'REQARG'
&KW(19)  SETC  'RETARG'
&KW(20)  SETC  'RETCODE'
&KW(21)  SETC  'RRETMSK'
&KW(22)  SETC  'RSNDMSK'
&KW(23)  SETC  'S'
&KW(24)  SETC  'SOCTYPE'
&KW(25)  SETC  'SRCADDR'
&KW(26)  SETC  'SRCLEN'
&KW(27)  SETC  'STORAGE'
&KW(28)  SETC  'TASK'
&KW(29)  SETC  'TIMEOUT'
&KW(30)  SETC  'WRETMSK'
&KW(31)  SETC  'WSNDMSK'
.*
.* EZASMI keyword parameter values array
.*
         LCLC  &VAL(31)
.*
.* Copy keyword parameter values to simplify use in loops
.*
&VAL(1)  SETC  '&AF'
&VAL(2)  SETC  '&BACKLOG'
&VAL(3)  SETC  '&BUF'
&VAL(4)  SETC  '&COMMAND'
&VAL(5)  SETC  '&DSTADDR'
&VAL(6)  SETC  '&DSTLEN'
&VAL(7)  SETC  '&ERETMSK'
&VAL(8)  SETC  '&ERRNO'
&VAL(9)  SETC  '&ESNDMSK'
&VAL(10) SETC  '&HOSTADR'
&VAL(11) SETC  '&HOSTENT'
&VAL(12) SETC  '&MAXSOC'
&VAL(13) SETC  '&MF'
&VAL(14) SETC  '&NAME'
&VAL(15) SETC  '&NAMELEN'
&VAL(16) SETC  '&NBYTE'
&VAL(17) SETC  '&PROTO'
&VAL(18) SETC  '&REQARG'
&VAL(19) SETC  '&RETARG'
&VAL(20) SETC  '&RETCODE'
&VAL(21) SETC  '&RRETMSK'
&VAL(22) SETC  '&RSNDMSK'
&VAL(23) SETC  '&S'
&VAL(24) SETC  '&SOCTYPE'
&VAL(25) SETC  '&SRCADDR'
&VAL(26) SETC  '&SRCLEN'
&VAL(27) SETC  '&STORAGE'
&VAL(28) SETC  '&TASK'
&VAL(29) SETC  '&TIMEOUT'
&VAL(30) SETC  '&WRETMSK'
&VAL(31) SETC  '&WSNDMSK'
.*
.*--------------------------------------------------------------------*
.* The EZASMIA macro is utilized to set switches in each of the       *
.* following arrays indicating which parameters are required and      *
.* which parameters are optional as a function of the type specified  *
.* on the invocation of EZASMI. The size of these arrays must match   *
.* the size of the KW and VAL arrays.                                 *
.*--------------------------------------------------------------------*
.*
.* EZASMI required parameters array
         GBLB  &REQD(31)
.* EZASMI optional parameters array
         GBLB  &OPTN(31)
.*
.*--------------------------------------------------------------------*
.* Validate function type.                                            *
.*--------------------------------------------------------------------*
         AIF   (K'&TYPE NE 0).VERTYPE
         MNOTE 16,'Function TYPE keyword required'
         MEXIT
.*
.* Determine specified function name
.*
.VERTYPE ANOP
         AIF   ('&TYPE' NE 'ACCEPT').F2
.* Required ACCEPT parameters
         EZASMIA REQD,S,NAME,ERRNO,RETCODE
.* Optional ACCEPT parameters
         EZASMIA OPTN,MF,TASK
&FUNC    SETC  'ACCE'          Set EZASOH03 function name.
         AGO   .CHKPARM
.*
.F2      ANOP
         AIF   ('&TYPE' NE 'BIND').F3
.* Required BIND parameters
         EZASMIA REQD,S,NAME,ERRNO,RETCODE
.* Optional BIND parameters
         EZASMIA OPTN,MF,TASK
&FUNC    SETC  'BIND'          Set EZASOH03 function name.
         AGO   .CHKPARM
.*
.F3      ANOP
         AIF   ('&TYPE' NE 'CLOSE').F4
.* Required CLOSE parameters
         EZASMIA REQD,S,ERRNO,RETCODE
.* Optional CLOSE parameters
         EZASMIA OPTN,MF,TASK
&FUNC    SETC  'CLOS'          Set EZASOH03 function name.
         AGO   .CHKPARM
.*
.F4      ANOP
         AIF   ('&TYPE' NE 'CONNECT').F5
.* Required CONNECT parameters
         EZASMIA REQD,S,NAME,ERRNO,RETCODE
.* Optional CONNECT parameters
         EZASMIA OPTN,MF,TASK
&FUNC    SETC  'CONN'          Set EZASOH03 function name.
         AGO   .CHKPARM
.*
.F5      ANOP
         AIF   ('&TYPE' NE 'GETHOSTBYNAME').F6
.* Required GETHOSTBYNAME parameters
         EZASMIA REQD,NAMELEN,NAME,HOSTENT,RETCODE
.* Optional GETHOSTBYNAME parameters
         EZASMIA OPTN,MF,TASK
&FUNC    SETC  'GETH'          Set EZASOH03 function name.
         AGO   .CHKPARM
.*
.F6      ANOP
         AIF   ('&TYPE' NE 'GETSOCKNAME').F7
.* Required GETSOCKNAME parameters
         EZASMIA REQD,S,NAME,ERRNO,RETCODE
.* Optional GETSOCKNAME parameters
         EZASMIA OPTN,MF,TASK
&FUNC    SETC  'GETS'          Set EZASOH03 function name.
         AGO   .CHKPARM
.*
.F7      ANOP
         AIF   ('&TYPE' NE 'IOCTL').F8
.* Required IOCTL parameters
         EZASMIA REQD,S,COMMAND,REQARG,RETARG,ERRNO,RETCODE
.* Optional IOCTL parameters
         EZASMIA OPTN,MF,TASK
&FUNC    SETC  'IOCT'          Set EZASOH03 function name.
         AGO   .CHKPARM
.*
.F8      ANOP
         AIF   ('&TYPE' NE 'LISTEN').F9
.* Required LISTEN parameters
         EZASMIA REQD,S,BACKLOG,ERRNO,RETCODE
.* Optional LISTEN parameters
         EZASMIA OPTN,MF,TASK
&FUNC    SETC  'LIST'          Set EZASOH03 function name.
         AGO   .CHKPARM
.*
.F9      ANOP
         AIF   ('&TYPE' NE 'RECV').F10
.* Required RECV parameters
         EZASMIA REQD,S,NBYTE,BUF,ERRNO,RETCODE
.* Optional RECV parameters
         EZASMIA OPTN,MF,TASK
&FUNC    SETC  'RECV'          Set EZASOH03 function name.
         AGO   .CHKPARM
.*
.F10     ANOP
         AIF   ('&TYPE' NE 'SELECT').F11
.* Required SELECT parameters
         EZASMIA REQD,MAXSOC,ERRNO,RETCODE
.* Optional SELECT parameters
         EZASMIA OPTN,MF,TIMEOUT,RSNDMSK,RRETMSK,WSNDMSK,WRETMSK,      +
               ESNDMSK,ERETMSK,TASK
&FUNC    SETC  'SELE'          Set EZASOH03 function name.
         AGO   .CHKPARM
.*
.F11     ANOP
         AIF   ('&TYPE' NE 'SEND').F12
.* Required SEND parameters
         EZASMIA REQD,S,NBYTE,BUF,ERRNO,RETCODE
.* Optional SEND parameters
         EZASMIA OPTN,MF,TASK
&FUNC    SETC  'SEND'          Set EZASOH03 function name.
         AGO   .CHKPARM
.*
.F12     ANOP
         AIF   ('&TYPE' NE 'SOCKET').F13
.* Required SOCKET parameters
         EZASMIA REQD,AF,SOCTYPE,ERRNO,RETCODE
.* Optional SOCKET parameters
         EZASMIA OPTN,MF,PROTO,TASK
&FUNC    SETC  'SOCK'          Set EZASOH03 function name.
         AGO   .CHKPARM
.*
.F13     ANOP
         AIF   ('&TYPE' NE 'TASK').F14
.* Required TASK parameters
         EZASMIA REQD,STORAGE
.* Optional TASK parameters
         EZASMIA OPTN                  None
&FUNC    SETC  'TASK'          Note this is not an EZASOH03 function.
         AGO   .CHKPARM
.F14     ANOP
         AIF   ('&TYPE' NE 'NTOP').F15
.* Required NTOP parameters
         EZASMIA REQD,AF,SRCADDR,DSTADDR,DSTLEN,ERRNO,RETCODE
.* Optional NTOP parameters
         EZASMIA OPTN,MF,TASK
&FUNC    SETC  'NTOP'
         AGO   .CHKPARM
.F15     ANOP
         AIF   ('&TYPE' NE 'PTON').F16
.* Required PTON parameters
         EZASMIA REQD,AF,SRCADDR,SRCLEN,DSTADDR,ERRNO,RETCODE
.* Optional PTON parameters
         EZASMIA OPTN,MF,TASK
&FUNC    SETC  'PTON'
         AGO   .CHKPARM
.F16     ANOP
         AIF   ('&TYPE' NE 'GETPEERNAME').F17
.* Required GETPEERNAME parameters
         EZASMIA REQD,S,NAME,ERRNO,RETCODE
.* Optional GETPEERNAME parameters
         EZASMIA OPTN,MF,TASK
&FUNC    SETC  'GETP'
         AGO   .CHKPARM
.F17     ANOP
         AIF   ('&TYPE' NE 'GETHOSTBYADDR').F18
.* Required GETHOSTBYADDR parameters
         EZASMIA REQD,HOSTADR,HOSTENT,RETCODE
.* Optional GETHOSTBYADDR parameters
         EZASMIA OPTN,MF,TASK
&FUNC    SETC  'GETA'
         AGO   .CHKPARM
.F18     ANOP
         AIF   ('&TYPE' NE 'INITAPI').F19
.* Required INITAPI parameters
         EZASMIA REQD                  None
.* Optional INITAPI parameters (ERRNO and RETCODE specified to
.* prevent erroneous error mesg)
         EZASMIA OPTN,ERRNO,RETCODE,MF,TASK
&FUNC    SETC  'INIT'
         AGO   .CHKPARM
.F19     ANOP
         AIF   ('&TYPE' NE 'TERMAPI').F20
.* Required TERMAPI parameters
         EZASMIA REQD                  None
.* Optional TERMAPI parameters
         EZASMIA OPTN,MF,TASK
&FUNC    SETC  'TERM'
         AGO   .CHKPARM
.F20     ANOP
         AIF   ('&TYPE' NE 'SHUTDOWN').BADTYPE
.* Required SHUTDOWN parameters
         EZASMIA REQD,S,ERRNO,RETCODE
.* Optional SHUTDOWN parameters
         EZASMIA OPTN,MF,TASK
&FUNC    SETC  'SHUT'          Set EZASOH03 function name.
         AGO   .CHKPARM
.*
.BADTYPE ANOP
         MNOTE 16,'Invalid TYPE value, &TYPE'
         MEXIT
.*
.CHKPARM ANOP
.*
.*--------------------------------------------------------------------*
.* Validate specified keyword parameters                              *
.*--------------------------------------------------------------------*
         LCLC  &STR            Char string value
.*
.* Ensure required parameters have been coded                         *
&I       SETA  1
.RLOOP   ANOP
         AIF   (&I GT 31).CHKBAD
         AIF   (NOT &REQD(&I) OR K'&VAL(&I) NE 0).NEXTR
         MNOTE 16,'Missing required option, &KW(&I)'
.NEXTR   ANOP
&I       SETA  &I+1
         AGO   .RLOOP
.*
.CHKBAD  ANOP
.* Ensure that no unrelated parameters have been coded
&I       SETA  1
.BLOOP   ANOP
         AIF   (&I GT 31).GENER
         AIF   (&REQD(&I) OR &OPTN(&I) OR K'&VAL(&I) EQ 0).NEXTB
         MNOTE 16,'Invalid option specified, &KW(&I)'
.NEXTB   ANOP
&I       SETA  &I+1
         AGO   .BLOOP
.*
.*--------------------------------------------------------------------*
.* Generate object code for specified function                        *
.*--------------------------------------------------------------------*
.GENER   ANOP
         AIF   ('&FUNC' EQ 'TASK').TASK
         AIF  (K'&LABEL EQ 0).NOLABEL
&LABEL   DS    0H
.NOLABEL ANOP
.*
.* Begin by addressing task storage
.*
         AIF   (K'&TASK NE 0).GEN1     Test for TASK parm
         LA    15,EZASMTIE             R15 = A(Task storage area)
         AGO   .GEN2
.GEN1    ANOP
.*
.* Use task storage pointed to by TASK parm
.*
         EZASMIP &TASK,15              Handle address parm
.*
.* Check for MF=E
.*
.GEN2    ANOP
         AIF   ('&MACFORM' NE 'E').GEN4
         AIF   (K'&MF(2) NE 0).GEN3
         MNOTE 12,'No LIST specified for E form of macro'
         MEXIT
.*
.* Use parm list pointed to by MF=E
.*
.GEN3    ANOP
         EZASMIP &MF(2),1              R1 = A(Parm list)
         AGO   .GEN5
.*
.* Use parm list in task storage
.*
.GEN4    ANOP
         LA    1,T#PARMS-EZASMTIE(15)  R1 = A(EZASOH03 parm list)
.GEN5    ANOP
.*
.*--------------------------------------------------------------------*
.*                             TYPE=SOCKET                            *
.*--------------------------------------------------------------------*
.*
         AIF   ('&FUNC' NE 'SOCK').BIND
.*
.* Setup EZASOH03 SOCKET function call
.*
         LA    0,=C'SOCK'              R0 = A(SOCKET function ID)
         ST    0,0(,1)                 Pt to SOCKET function ID
.*
.* Generate ERRNO parm code
.*
         EZASMIP &ERRNO                Handle address parm
         ST    &GPR,4(,1)              Pt to ERRNO parm value
.*
.* Generate RETCODE parm code
.*
         EZASMIP &RETCODE              Handle address parm
         ST    &GPR,8(,1)              Pt to RETCODE parm value
.*
.* Generate AF parm code
.*
         AIF   ('&AF'(1,1) NE '''').SOCK1 Not quoted string value
&STR     SETC  '&AF'(2,K'&AF-2)
         EZASMIC &STR,INET             AF quoted string parm
         AGO   .SOCK2
.SOCK1   ANOP
         EZASMIP &AF                   Handle address parm
.SOCK2   ANOP
         ST    &GPR,12(,1)             Pt to AF parm value
.*
.* Generate SOCTYPE parm code
.*
         AIF   ('&SOCTYPE'(1,1) NE '''').SOCK3 Not quoted string value
&STR     SETC  '&SOCTYPE'(2,K'&SOCTYPE-2)
         EZASMIC &STR,STREAM,DATAGRAM,RAW SOCTYPE quoted string parm
         AGO   .SOCK4
.SOCK3   ANOP
         EZASMIP &SOCTYPE              Handle address parm
.SOCK4   ANOP
         ST    &GPR,16(,1)             Pt to SOCTYPE parm value
.*
.* Generate PROTO parm code
.*
         AIF   (K'&PROTO NE 0).SOCK5   Check for PROTO
         LA    0,=F'00'                Default PROTO
         AGO   .SOCK6
.SOCK5   ANOP
         EZASMIP &PROTO                Handle address parm
.SOCK6   ANOP
         ST    &GPR,20(,1)             Pt to PROTO parm value
         AGO   .CALL
.*
.*--------------------------------------------------------------------*
.*                             TYPE=BIND                              *
.*--------------------------------------------------------------------*
.*
.BIND    ANOP
         AIF   ('&FUNC' NE 'BIND').CONN
.*
.* Setup EZASOH03 BIND function call
.*
         LA    0,=C'BIND'              R0 = A(BIND function ID)
         ST    0,0(,1)                 Pt to BIND function ID
.*
.* Generate ERRNO parm code
.*
         EZASMIP &ERRNO                Handle address parm
         ST    &GPR,4(,1)              Pt to ERRNO parm value
.*
.* Generate RETCODE parm code
.*
         EZASMIP &RETCODE              Handle address parm
         ST    &GPR,8(,1)              Pt to RETCODE parm value
.*
.* Generate S parm code
.*
         EZASMIP &S,0,AL2,65535        Handle address parm and literal
         ST    &GPR,12(,1)             Pt to S parm value
.*
.* Generate NAME parm code
.*
         EZASMIP &NAME                 Handle address parm
         ST    &GPR,16(,1)             Pt to NAME parm value
         AGO   .CALL
.*
.*--------------------------------------------------------------------*
.*                             TYPE=CONNECT                           *
.*--------------------------------------------------------------------*
.*
.CONN    ANOP
         AIF   ('&FUNC' NE 'CONN').LIST
.*
.* Setup EZASOH03 CONN function call
.*
         LA    0,=C'CONN'              R0 = A(CONN function ID)
         ST    0,0(,1)                 Pt to CONN function ID
.*
.* Generate ERRNO parm code
.*
         EZASMIP &ERRNO                Handle address parm
         ST    &GPR,4(,1)              Pt to ERRNO parm value
.*
.* Generate RETCODE parm code
.*
         EZASMIP &RETCODE              Handle address parm
         ST    &GPR,8(,1)              Pt to RETCODE parm value
.*
.* Generate S parm code
.*
         EZASMIP &S,0,AL2,65535        Handle address parm and literal
         ST    &GPR,12(,1)             Pt to S parm value
.*
.* Generate NAME parm code
.*
         EZASMIP &NAME                 Handle address parm
         ST    &GPR,16(,1)             Pt to NAME parm value
         AGO   .CALL
.*
.*--------------------------------------------------------------------*
.*                             TYPE=LISTEN                            *
.*--------------------------------------------------------------------*
.*
.LIST    ANOP
         AIF   ('&FUNC' NE 'LIST').ACCE
.*
.* Setup EZASOH03 LIST function call
.*
         LA    0,=C'LIST'              R0 = A(LIST function ID)
         ST    0,0(,1)                 Pt to LIST function ID
.*
.* Generate ERRNO parm code
.*
         EZASMIP &ERRNO                Handle address parm
         ST    &GPR,4(,1)              Pt to ERRNO parm value
.*
.* Generate RETCODE parm code
.*
         EZASMIP &RETCODE              Handle address parm
         ST    &GPR,8(,1)              Pt to RETCODE parm value
.*
.* Generate S parm code
.*
         EZASMIP &S,0,AL2,65535        Handle address parm and literal
         ST    &GPR,12(,1)             Pt to S parm value
.*
.* Generate BACKLOG parm code
.*
         EZASMIP &BACKLOG,0,F,4095     Handle address parm and literal
         ST    &GPR,16(,1)             Pt to BACKLOG parm value
         AGO   .CALL
.*
.*--------------------------------------------------------------------*
.*                             TYPE=ACCEPT                            *
.*--------------------------------------------------------------------*
.*
.ACCE    ANOP
         AIF   ('&FUNC' NE 'ACCE').RECV
.*
.* Setup EZASOH03 ACCE function call
.*
         LA    0,=C'ACCE'              R0 = A(ACCE function ID)
         ST    0,0(,1)                 Pt to ACCE function ID
.*
.* Generate ERRNO parm code
.*
         EZASMIP &ERRNO                Handle address parm
         ST    &GPR,4(,1)              Pt to ERRNO parm value
.*
.* Generate RETCODE parm code
.*
         EZASMIP &RETCODE              Handle address parm
         ST    &GPR,8(,1)              Pt to RETCODE parm value
.*
.* Generate S parm code
.*
         EZASMIP &S,0,AL2,65535        Handle address parm and literal
         ST    &GPR,12(,1)             Pt to S parm value
.*
.* Generate NAME parm code
.*
         EZASMIP &NAME                 Handle address parm
         ST    &GPR,16(,1)             Pt to NAME parm value
         AGO   .CALL
.*
.*--------------------------------------------------------------------*
.*                             TYPE=RECV                              *
.*--------------------------------------------------------------------*
.*
.RECV    ANOP
         AIF   ('&FUNC' NE 'RECV').SEND
.*
.* Setup EZASOH03 RECV function call
.*
         LA    0,=C'RECV'              R0 = A(RECV function ID)
         ST    0,0(,1)                 Pt to RECV function ID
.*
.* Generate ERRNO parm code
.*
         EZASMIP &ERRNO                Handle address parm
         ST    &GPR,4(,1)              Pt to ERRNO parm value
.*
.* Generate RETCODE parm code
.*
         EZASMIP &RETCODE              Handle address parm
         ST    &GPR,8(,1)              Pt to RETCODE parm value
.*
.* Generate S parm code
.*
         EZASMIP &S,0,AL2,65535        Handle address parm and literal
         ST    &GPR,12(,1)             Pt to S parm value
.*
.* Generate NBYTE parm code
.*
         EZASMIP &NBYTE,0,F,1048576    Handle address parm and literal
         ST    &GPR,16(,1)             Pt to NBYTE parm value
.*
.* Generate BUF parm code
.*
         EZASMIP &BUF                  Handle address parm
         ST    &GPR,20(,1)             Pt to BUF parm value
         AGO   .CALL
.*
.*--------------------------------------------------------------------*
.*                             TYPE=SEND                              *
.*--------------------------------------------------------------------*
.*
.SEND    ANOP
         AIF   ('&FUNC' NE 'SEND').CLOS
.*
.* Setup EZASOH03 SEND function call
.*
         LA    0,=C'SEND'              R0 = A(SEND function ID)
         ST    0,0(,1)                 Pt to SEND function ID
.*
.* Generate ERRNO parm code
.*
         EZASMIP &ERRNO                Handle address parm
         ST    &GPR,4(,1)             Pt to ERRNO parm value
.*
.* Generate RETCODE parm code
.*
         EZASMIP &RETCODE              Handle address parm
         ST    &GPR,8(,1)             Pt to RETCODE parm value
.*
.* Generate S parm code
.*
         EZASMIP &S,0,AL2,65535        Handle address parm and literal
         ST    &GPR,12(,1)              Pt to S parm value
.*
.* Generate NBYTE parm code
.*
         EZASMIP &NBYTE,0,F,1048576    Handle address parm and literal
         ST    &GPR,16(,1)             Pt to NBYTE parm value
.*
.* Generate BUF parm code
.*
         EZASMIP &BUF                  Handle address parm
         ST    &GPR,20(,1)             Pt to BUF parm value
         AGO   .CALL
.*
.*--------------------------------------------------------------------*
.*                             TYPE=CLOSE                             *
.*--------------------------------------------------------------------*
.*
.CLOS    ANOP
         AIF   ('&FUNC' NE 'CLOS').NTOP
.*
.* Setup EZASOH03 CLOS function call
.*
         LA    0,=C'CLOS'              R0 = A(CLOS function ID)
         ST    0,0(,1)                 Pt to CLOS function ID
.*
.* Generate ERRNO parm code
.*
         EZASMIP &ERRNO                Handle address parm
         ST    &GPR,4(,1)              Pt to ERRNO parm value
.*
.* Generate RETCODE parm code
.*
         EZASMIP &RETCODE              Handle address parm
         ST    &GPR,8(,1)              Pt to RETCODE parm value
.*
.* Generate S parm code
.*
         EZASMIP &S,0,AL2,65535        Handle address parm and literal
         ST    &GPR,12(,1)              Pt to S parm value
         AGO   .CALL
.*
.*--------------------------------------------------------------------*
.*                             TYPE=NTOP                              *
.*--------------------------------------------------------------------*
.*
.NTOP    ANOP
         AIF   ('&FUNC' NE 'NTOP').PTON
.*
.* Setup EZASOH03 NTOP function call
.*
         LA    0,=C'NTOP'              R0 = A(NTOP function ID)
         ST    0,0(,1)                 Pt to NTOP function ID
.*
.* Generate ERRNO parm code
.*
         EZASMIP &ERRNO                Handle address parm
         ST    &GPR,4(,1)              Pt to ERRNO parm value
.*
.* Generate RETCODE parm code
.*
         EZASMIP &RETCODE              Handle address parm
         ST    &GPR,8(,1)              Pt to RETCODE parm value
.*
.* Generate AF parm code
.*
         AIF   ('&AF'(1,1) NE '''').NTOP1 Not quoted string value
&STR     SETC  '&AF'(2,K'&AF-2)
         EZASMIC &STR,INET             AF quoted string parm
         AGO   .NTOP2
.NTOP1   ANOP
         EZASMIP &AF                   Handle address parm
.NTOP2   ANOP
         ST    &GPR,12(,1)             Pt to AF parm value
.*
.* Generate SRCADDR parm code
.*
         EZASMIP &SRCADDR              Handle address parm
         ST    &GPR,16(,1)             Pt to SRCADDR parm value
.*
.* Generate DSTADDR parm code
.*
         EZASMIP &DSTADDR              Handle address parm
         ST    &GPR,20(,1)             Pt to DSTADDR parm value
.*
.* Generate DSTLEN parm code
.*
         EZASMIP &DSTLEN               Handle address parm
         ST    &GPR,24(,1)             Pt to DSTLEN parm value
         AGO   .CALL
.*
.*--------------------------------------------------------------------*
.*                             TYPE=PTON                              *
.*--------------------------------------------------------------------*
.*
.PTON    ANOP
         AIF   ('&FUNC' NE 'PTON').IOCT
.*
.* Setup EZASOH03 PTON function call
.*
         LA    0,=C'PTON'              R0 = A(PTON function ID)
         ST    0,0(,1)                 Pt to PTON function ID
.*
.* Generate ERRNO parm code
.*
         EZASMIP &ERRNO                Handle address parm
         ST    &GPR,4(,1)              Pt to ERRNO parm value
.*
.* Generate RETCODE parm code
.*
         EZASMIP &RETCODE              Handle address parm
         ST    &GPR,8(,1)              Pt to RETCODE parm value
.*
.* Generate AF parm code
.*
         AIF   ('&AF'(1,1) NE '''').PTON1 Not quoted string value
&STR     SETC  '&AF'(2,K'&AF-2)
         EZASMIC &STR,INET             AF quoted string parm
         AGO   .PTON2
.PTON1   ANOP
         EZASMIP &AF                   Handle address parm
.PTON2   ANOP
         ST    &GPR,12(,1)             Pt to AF parm value
.*
.* Generate SRCADDR parm code
.*
         EZASMIP &SRCADDR              Handle address parm
         ST    &GPR,16(,1)             Pt to SRCADDR parm value
.*
.* Generate DSTADDR parm code
.*
         EZASMIP &DSTADDR              Handle address parm
         ST    &GPR,20(,1)             Pt to DSTADDR parm value
.*
.* Generate SRCLEN parm code
.*
         EZASMIP &SRCLEN               Handle address parm
         ST    &GPR,24(,1)             Pt to SRCLEN parm value
         AGO   .CALL
.*
.*--------------------------------------------------------------------*
.*                             TYPE=IOCTL                             *
.*--------------------------------------------------------------------*
.*
.IOCT    ANOP
         AIF   ('&FUNC' NE 'IOCT').GETS
.*
.* Setup EZASOH03 IOCTL function call
.*
         LA    0,=C'IOCT'              R0 = A(IOCT function ID)
         ST    0,0(,1)                 Pt to IOCT function ID
.*
.* Generate ERRNO parm code
.*
         EZASMIP &ERRNO                Handle address parm
         ST    &GPR,4(,1)              Pt to ERRNO parm value
.*
.* Generate RETCODE parm code
.*
         EZASMIP &RETCODE              Handle address parm
         ST    &GPR,8(,1)              Pt to RETCODE parm value
.*
.* Generate S parm code
.*
         EZASMIP &S,0,AL2,65535        Handle address parm and literal
         ST    &GPR,12(,1)             Pt to S parm value
.*
.* Generate COMMAND parm code
.*
         AIF   ('&COMMAND'(1,1) NE '''').IOCT1 Not quoted string value
&STR     SETC  '&COMMAND'(2,K'&COMMAND-2)
         EZASMIC &STR,FIONBIO,FIONREAD,SIOCATMARK,SIOCGIFADDR,         +
               SIOCGIFBRDADDR,SIOCGIFCONF,SIOCGIFDSTADDR,SIOCGIFMTU,   +
               SIOCGIFNAMEINDEX,SIOCGIPMSFILTER,SIOCGMONDATA,          +
               SIOCGMSFILTER,SIOCGPARTNERINFO,SIOCGSPLXFQDN,           +
               SIOCSAPPLDATA,SIOCSIPMSFILTER,SIOCSMSFILTER,            +
               SIOCSPARTNERINFO,SIOCTTLSCTL
         AGO   .IOCT2
.IOCT1   ANOP
         EZASMIP &COMMAND              Handle address parm
.IOCT2   ANOP
         ST    &GPR,16(,1)             Pt to COMMAND parm value
.*
.* Generate REQARG parm code
.*
         EZASMIP &REQARG               Handle address parm
         ST    &GPR,20(,1)             Pt to REQARG parm value
.*
.* Generate RETARG parm code
.*
         EZASMIP &RETARG               Handle address parm
         ST    &GPR,24(,1)             Pt to RETARG parm value
         AGO   .CALL
.*
.*--------------------------------------------------------------------*
.*                             TYPE=GETSOCKNAME                       *
.*--------------------------------------------------------------------*
.*
.GETS    ANOP
         AIF   ('&FUNC' NE 'GETS').SELE
.*
.* Setup EZASOH03 GETSOCKNAME function call
.*
         LA    0,=C'GETS'              R0 = A(GETS function ID)
         ST    0,0(,1)                 Pt to GETS function ID
.*
.* Generate ERRNO parm code
.*
         EZASMIP &ERRNO                Handle address parm
         ST    &GPR,4(,1)              Pt to ERRNO parm value
.*
.* Generate RETCODE parm code
.*
         EZASMIP &RETCODE              Handle address parm
         ST    &GPR,8(,1)              Pt to RETCODE parm value
.*
.* Generate S parm code
.*
         EZASMIP &S,0,AL2,65535        Handle address parm and literal
         ST    &GPR,12(,1)             Pt to S parm value
.*
.* Generate NAME parm code
.*
         EZASMIP &NAME                 Handle address parm
         ST    &GPR,16(,1)             Pt to NAME parm value
         AGO   .CALL
.*
.*--------------------------------------------------------------------*
.*                             TYPE=SELECT                            *
.*--------------------------------------------------------------------*
.*
.SELE    ANOP
         LCLB  &R0ZERO
&R0ZERO  SETB  0                       True if XR can be skipped
         AIF   ('&FUNC' NE 'SELE').GETH
.*
.* Setup EZASOH03 SELECT function call
.*
         LA    0,=C'SELE'              R0 = A(SELE function ID)
         ST    0,0(,1)                 Pt to SELE function ID
.*
.* Generate ERRNO parm code
.*
         EZASMIP &ERRNO                Handle address parm
         ST    &GPR,4(,1)              Pt to ERRNO parm value
.*
.* Generate RETCODE parm code
.*
         EZASMIP &RETCODE              Handle address parm
         ST    &GPR,8(,1)              Pt to RETCODE parm value
.*
.* Generate MAXSOC parm code
.*
         EZASMIP &MAXSOC,0,AL2,65535   Handle address parm and literal
         ST    &GPR,12(,1)             Pt to MAXSOC parm value
.*
.* Generate TIMEOUT parm code
.*
         AIF (K'&TIMEOUT NE 0).SELE0
&GPR     SETC  '0'                     Ensure ST uses correct reg
         AIF   (&R0ZERO).SELET
         XR    0,0                     Null parm value
&R0ZERO  SETB  1
         AGO   .SELET
.SELE0   ANOP
         EZASMIP &TIMEOUT              Handle address parm
         AIF   ('&GPR' NE '0').SELET
&R0ZERO  SETB  0
.SELET   ANOP
         ST    &GPR,16(,1)             Pt to TIMEOUT parm value
.*
.* Generate RSNDMSK parm code
.*
         AIF (K'&RSNDMSK NE 0).SELE1
&GPR     SETC  '0'                     Ensure ST uses correct reg
         AIF   (&R0ZERO).SELE2
         XR    0,0                     Null parm value
&R0ZERO  SETB  1
         AGO   .SELE2
.SELE1   ANOP
         EZASMIP &RSNDMSK              Handle address parm
         AIF   ('&GPR' NE '0').SELE2
&R0ZERO  SETB  0
.SELE2   ANOP
         ST    &GPR,20(,1)             Pt to RSNDMSK parm value
.*
.* Generate WSNDMSK parm code
.*
         AIF (K'&WSNDMSK NE 0).SELE3
&GPR     SETC  '0'                     Ensure ST uses correct reg
         AIF   (&R0ZERO).SELE4
         XR    0,0                     Null parm value
&R0ZERO  SETB  1
         AGO   .SELE4
.SELE3   ANOP
         EZASMIP &WSNDMSK              Handle address parm
         AIF   ('&GPR' NE '0').SELE4
&R0ZERO  SETB  0
.SELE4   ANOP
         ST    &GPR,24(,1)             Pt to WSNDMSK parm value
.*
.* Generate ESNDMSK parm code
.*
         AIF (K'&ESNDMSK NE 0).SELE5
&GPR     SETC  '0'                     Ensure ST uses correct reg
         AIF   (&R0ZERO).SELE6
         XR    0,0                     Null parm value
&R0ZERO  SETB  1
         AGO   .SELE6
.SELE5   ANOP
         EZASMIP &ESNDMSK              Handle address parm
         AIF   ('&GPR' NE '0').SELE6
&R0ZERO  SETB  0
.SELE6   ANOP
         ST    &GPR,28(,1)             Pt to ESNDMSK parm value
.*
.* Generate RRETMSK parm code
.*
         AIF (K'&RRETMSK NE 0).SELE7
&GPR     SETC  '0'                     Ensure ST uses correct reg
         AIF   (&R0ZERO).SELE8
         XR    0,0                     Null parm value
&R0ZERO  SETB  1
         AIF   (K'&RSNDMSK EQ 0).SELE8
         MNOTE 12,'RRETMSK is required when RSNDMSK is specified'
         AGO   .SELE8
.SELE7   ANOP
         EZASMIP &RRETMSK              Handle address parm
         AIF   ('&GPR' NE '0').SELE8
&R0ZERO  SETB  0
.SELE8   ANOP
         ST    &GPR,32(,1)             Pt to RRETMSK parm value
.*
.* Generate WRETMSK parm code
.*
         AIF (K'&WRETMSK NE 0).SELE9
&GPR     SETC  '0'                     Ensure ST uses correct reg
         AIF   (&R0ZERO).SELEA
         XR    0,0                     Null parm value
&R0ZERO  SETB  1
         AIF   (K'&WSNDMSK EQ 0).SELEA
         MNOTE 12,'WRETMSK is required when WSNDMSK is specified'
         AGO   .SELEA
.SELE9   ANOP
         EZASMIP &WRETMSK              Handle address parm
         AIF   ('&GPR' NE '0').SELEA
&R0ZERO  SETB  0
.SELEA   ANOP
         ST    &GPR,36(,1)             Pt to WRETMSK parm value
.*
.* Generate ERETMSK parm code
.*
         AIF (K'&ERETMSK NE 0).SELEB
&GPR     SETC  '0'                     Ensure ST uses correct reg
         AIF   (&R0ZERO).SELEC
         XR    0,0                     Null parm value
&R0ZERO  SETB  1
         AIF   (K'&ESNDMSK EQ 0).SELEC
         MNOTE 12,'ERETMSK is required when ESNDMSK is specified'
         AGO   .SELEC
.SELEB   ANOP
         EZASMIP &ERETMSK              Handle address parm
         AIF   ('&GPR' NE '0').SELEC
&R0ZERO  SETB  0
.SELEC   ANOP
         ST    &GPR,40(,1)             Pt to ERETMSK parm value
         AGO   .CALL
.*
.*--------------------------------------------------------------------*
.*                             TYPE=GETHOSTBYNAME                     *
.*--------------------------------------------------------------------*
.*
.GETH    ANOP
         AIF   ('&FUNC' NE 'GETH').GETP
.*
.* Setup EZASOH03 GETHOSTBYNAME function call
.*
         LA    0,=C'GETH'              R0 = A(GETH function ID)
         ST    0,0(,1)                 Pt to GETH function ID
.*
.* Generate ERRNO parm code
.*
         XR    0,0                     Null parm value
         ST    0,4(,1)                 Pt to ERRNO parm value
.*
.* Generate RETCODE parm code
.*
         EZASMIP &RETCODE              Handle address parm
         ST    &GPR,8(,1)              Pt to RETCODE parm value
.*
.* Generate NAMELEN parm code
.*
         EZASMIP &NAMELEN,0,F,255      Handle address parm and literal
         ST    &GPR,12(,1)             Pt to NAMELEN parm value
.*
.* Generate NAME parm code
.*
         EZASMIP &NAME                 Handle address parm
         ST    &GPR,16(,1)             Pt to NAME parm value
.*
.* Generate HOSTENT parm code
.*
         EZASMIP &HOSTENT              Handle address parm
         ST    &GPR,20(,1)             Pt to HOSTENT parm value
.*
.* Generate A(Working storage)
.*
         LA    0,T#WORK-EZASMTIE(15)
         ST    0,24(,1)                Pt to working storage
         AGO   .CALL
.*
.*--------------------------------------------------------------------*
.*                             TYPE=GETPEERNAME                       *
.*--------------------------------------------------------------------*
.*
.GETP    ANOP
         AIF   ('&FUNC' NE 'GETP').GETA
.*
.* Setup EZASOH03 GETPEERNAME function call
.*
         LA    0,=C'GETP'              R0 = A(GETP function ID)
         ST    0,0(,1)                 Pt to GETP function ID
.*
.* Generate ERRNO parm code
.*
         EZASMIP &ERRNO                Handle address parm
         ST    &GPR,4(,1)              Pt to ERRNO parm value
.*
.* Generate RETCODE parm code
.*
         EZASMIP &RETCODE              Handle address parm
         ST    &GPR,8(,1)              Pt to RETCODE parm value
.*
.* Generate S parm code
.*
         EZASMIP &S,0,AL2,65535        Handle address parm and literal
         ST    &GPR,12(,1)             Pt to S parm value
.*
.* Generate NAME parm code
.*
         EZASMIP &NAME                 Handle address parm
         ST    &GPR,16(,1)             Pt to NAME parm value
         AGO   .CALL
.*
.*--------------------------------------------------------------------*
.*                             TYPE=GETHOSTBYADDR                     *
.*--------------------------------------------------------------------*
.*
.GETA    ANOP
         AIF   ('&FUNC' NE 'GETA').INIT
.*
.* Setup EZASOH03 GETHOSTBYADDR function call
.*
         LA    0,=C'GETA'              R0 = A(GETA function ID)
         ST    0,0(,1)                 Pt to GETA function ID
.*
.* Generate ERRNO parm code
.*
         XR    0,0                     Null parm value
         ST    0,4(,1)                 Pt to ERRNO parm value
.*
.* Generate RETCODE parm code
.*
         EZASMIP &RETCODE              Handle address parm
         ST    &GPR,8(,1)              Pt to RETCODE parm value
.*
.* Generate HOSTADR parm code
.*
         EZASMIP &HOSTADR              Handle address parm
         ST    &GPR,12(,1)             Pt to HOSTADR parm value
.*
.* Generate HOSTENT parm code
.*
         EZASMIP &HOSTENT              Handle address parm
         ST    &GPR,16(,1)             Pt to HOSTENT parm value
.*
.* Generate A(working storage)
.*
         LA    0,T#WORK-EZASMTIE(15)
         ST    0,20(,1)                Pt to working storage
         AGO   .CALL
.*
.*--------------------------------------------------------------------*
.*                             TYPE=INITAPI                           *
.*--------------------------------------------------------------------*
.*
.INIT    ANOP
         AIF   ('&FUNC' NE 'INIT').TERM
.*
.* Setup INITAPI load of EZASOH03
.*
         LA    0,=C'INIT'              R0 = A(INIT function ID)
         ST    0,0(,1)                 Pt to INIT function ID
         AGO   .CALL
.*
.*--------------------------------------------------------------------*
.*                             TYPE=TERMAPI                           *
.*--------------------------------------------------------------------*
.*
.TERM    ANOP
         AIF   ('&FUNC' NE 'TERM').SHUT
.*
.* Setup EZASOH03 TERMAPI function call
.*
         LA    0,=C'TERM'              R0 = A(TERM function ID)
         ST    0,0(,1)                 Pt to TERM function ID
.*
.* Generate A(working storage)
.*
         LA    0,T#WORK-EZASMTIE(15)
         ST    0,4(,1)                 Pt to working storage
         AGO   .CALL
.*
.*--------------------------------------------------------------------*
.*                             TYPE=SHUTDOWN                          *
.*--------------------------------------------------------------------*
.*
.SHUT    ANOP
         AIF   ('&FUNC' NE 'SHUT').DONE
.*
.* Setup EZASOH03 CLOS function call (MVS doesn't support SHUTDOWN)
.*
         LA    0,=C'CLOS'              R0 = A(CLOS function ID)
         ST    0,0(,1)                 Pt to CLOS function ID
.*
.* Generate ERRNO parm code
.*
         EZASMIP &ERRNO                Handle address parm
         ST    &GPR,4(,1)              Pt to ERRNO parm value
.*
.* Generate RETCODE parm code
.*
         EZASMIP &RETCODE              Handle address parm
         ST    &GPR,8(,1)              Pt to RETCODE parm value
.*
.* Generate S parm code
.*
         EZASMIP &S,0,AL2,65535        Handle address parm and literal
         ST    &GPR,12(,1)              Pt to S parm value
         AGO   .CALL
.*
.*--------------------------------------------------------------------*
.*                    Generate call to EZASOH03                       *
.*--------------------------------------------------------------------*
.*
.CALL    ANOP
         L     15,=V(EZASOH03)         Invoke EZASOH03
         BALR  14,15
.DONE    ANOP
         MEXIT
.*
.*--------------------------------------------------------------------*
.*                             MF=L                                   *
.*--------------------------------------------------------------------*
.LISTMF  ANOP
         AIF  (K'&LABEL NE 0).LISTMF1  Ensure label on line
         MNOTE 12,'Missing label for LIST form'
         MEXIT
.LISTMF1 ANOP
&LABEL   DS    0D
         DS    16F                     Parm list data
         MEXIT
.*
.*--------------------------------------------------------------------*
.*                             TYPE=TASK                              *
.*--------------------------------------------------------------------*
.*
.* Handle any label on EZASMI macro invocation
.*
.TASK    ANOP
         LCLC  &TASKSTG
&TASKSTG SETC  'EZASMTIE'      Default task storage label
         AIF   ('&LABEL' EQ '').TASK1
&TASKSTG SETC  '&LABEL'
.TASK1   ANOP
.*
***********************************************************************
*          TCP/IP Task Storage                                        *
***********************************************************************
.*
.* Determine if STORAGE=DSECT|CSECT
.*
         AIF   ('&STORAGE' EQ 'DSECT').TASK2
&TASKSTG DS    0D
         AGO   .TASK3
.TASK2   ANOP
&TASKSTG DSECT
.TASK3   ANOP
         AIF   ('&LABEL' EQ '').TASK4
EZASMTIE EQU   *
.TASK4   ANOP
T#PARMS  DS    16F             EZASOH03 parm list data
.*
.* EZASOH03 work area
.*
.* The contents of this work area are mapped by the HOSTWORK
.* DSECT in EZASOH03
.*
.* Working storage
T#WORK   DS    0D              EZASOH03 Working storage
         DS    F
T#END    DS    0D
TIELENTH EQU   T#END-EZASMTIE  Length of task storage
         MEND
