//EZASMI JOB (JOB),
//             'INSTALL EZA',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             USER=IBMUSER,
//             PASSWORD=SYS1
//*
//* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//* DO NOT EDIT THIS JCL IT IS GENERATED FROM
//* 01_build_jobstream.sh edit that file instead
//* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//*
//CLEANUP EXEC PGM=IDCAMS
//SYSIN    DD *
  DELETE SYSGEN.TCPIP.SAMPLIB
  SET MAXCC=0
  SET LASTCC=0
//SYSPRINT DD  SYSOUT=*
//*
//* Installs EZASMI
//*
//* Part 1 copy macros
//STEP1   EXEC PGM=PDSLOAD
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DSN=SYS2.MACLIB,DISP=SHR
//SYSUT1   DD  DATA,DLM=@@
./ ADD NAME=EZASMIA
.*------------------------------------------------------------------- *
.*                                                                    *
.*                   EZASMIA Macro                                    *
.*                                                                    *
.*  Function:  The EZASMIA macro processes lists of required and      *
.*             optional keyword parameter names producing a bit map   *
.*             for each parameter type which EZASMI (the caller) can  *
.*             use to validate what the user has coded.               *
.*                                                                    *
.*             The first parameter (&LIST) must specify:              *
.*             'OPTN' or 'REQD'.                                      *
.*                                                                    *
.*  Author:    Shelby Beach, Ms. Kitty Programming Werks              *
.*                                                                    *
.*  Change Log:                                                       *
.*                                                                    *
.*    Date     Pgmr   Change                                          *
.*  12/01/2016  SLB   Initial release                                 *
.*                                                                    *
.*------------------------------------------------------------------- *
         MACRO
         EZASMIA &LIST,&P1,&P2,&P3,&P4,&P5,&P6,&P7,&P8,&P9,&P10,       +
               &P11,&P12,&P13,&P14,&P15,&P16,&P17,&P18,&P19,&P20,      +
               &P21,&P22,&P23,&P24,&P25,&P26,&P27,&P28,&P29,&P30
.*
.*------------------------------------------------------------------- *
.* The size of the following arrays must be consistent with those     *
.* specified in EZASMI.                                               *
.*------------------------------------------------------------------- *
.*
         GBLB  &REQD(31)
         GBLB  &OPTN(31)
         GBLC  &KW(31)
.*
.* Loop indices
         LCLA  &I,&J
.*
.* Clear residual array bits
.*
&I       SETA  1
.CLOOP   ANOP
         AIF   ('&LIST' NE 'REQD').CLROPTN
&REQD(&I) SETB 0
         AGO   .NEXTC
.CLROPTN ANOP
         AIF   ('&LIST' NE 'OPTN').BADLIST
&OPTN(&I) SETB 0
.NEXTC   ANOP
&I       SETA  &I+1
         AIF   (&I LE 31).CLOOP
.*
.* Process keyword names parameter list
.*
&I       SETA  2
.PLOOP   ANOP
         AIF   (&I GT N'&SYSLIST).DONE
&J       SETA  1
.KLOOP   ANOP
         AIF   ('&SYSLIST(&I)' EQ '&KW(&J)').SETP
&J       SETA  &J+1
         AIF   (&J LE 31).KLOOP
         MNOTE 16,'Unknown keyword &SYSLIST(&I)'
         AGO   .NEXTP
.*
.SETP    ANOP
         AIF   ('&LIST' NE 'REQD').OPTLIST
.*
.* Set required parameter
.*
&REQD(&J) SETB 1
         AGO   .NEXTP
.OPTLIST ANOP
.*
.* Set optional parameter
.*
&OPTN(&J) SETB 1
.NEXTP   ANOP
&I       SETA  &I+1
         AGO   .PLOOP
.*
.BADLIST ANOP
         MNOTE 16,'Invalid list name, &LIST'
.*
.DONE    ANOP
         MEND
./ ADD NAME=EZASMIC
.*------------------------------------------------------------------- *
.*                                                                    *
.*                   EZASMIC Macro                                    *
.*                                                                    *
.*  Function:  The EZASMIC macro validates character constant         *
.*             keyword values, generating a load into R0 of the       *
.*             the address of the numeric constant literal.           *
.*                                                                    *
.*  Parameters:                                                       *
.*             &KWVAL - The keyword value to be processed             *
.*             &Pn    - The valid values for &KWVAL                   *
.*                                                                    *
.*  Author:    Shelby Beach, Ms. Kitty Programming Werks              *
.*                                                                    *
.*  Change Log:                                                       *
.*                                                                    *
.*    Date     Pgmr   Change                                          *
.*  12/01/2016  SLB   Initial release                                 *
.*                                                                    *
.*------------------------------------------------------------------- *
         MACRO
         EZASMIC &KWVAL,&P1,&P2,&P3,&P4,&P5,&P6,&P7,&P8,&P9,&P10,      +
               &P11,&P12,&P13,&P14,&P15,&P16,&P17,&P18,&P19,&P20
.*
         GBLC  &GPR            GPR containing addr of EZASOKET parm
.*
&GPR     SETC  '0'             Set to register to store in EZASMI
.*
.* Character string value/numeric constant array
.*
         LCLC  &STR(46)
.*
.* Initialize character string constant array
.*   Entry n is the character string value
.*   Entry n+1 is the numeric equivalent value
.*
&STR(1)  SETC 'INET'
&STR(2)  SETC 'F''02'''
&STR(3)  SETC 'STREAM'
&STR(4)  SETC 'F''01'''
&STR(5)  SETC 'DATAGRAM'
&STR(6)  SETC 'F''02'''
&STR(7)  SETC 'RAW'
&STR(8)  SETC 'F''03'''
&STR(9)  SETC 'FIONBIO'
&STR(10) SETC 'X''00000001'''          Changed from X'8004A77E' for MVS
&STR(11) SETC 'FIONREAD'
&STR(12) SETC 'X''4004A77F'''
&STR(13) SETC 'SIOCATMARK'
&STR(14) SETC 'X''4004A707'''
&STR(15) SETC 'SIOCGIFADDR'
&STR(16) SETC 'X''C020A70D'''
&STR(17) SETC 'SIOCGIFBRDADDR'
&STR(18) SETC 'X''C020A712'''
&STR(19) SETC 'SIOCGIFCONF'
&STR(20) SETC 'X''C008A714'''
&STR(21) SETC 'SIOCGIFDSTADDR'
&STR(22) SETC 'X''C020A70F'''
&STR(23) SETC 'SIOCGIFMTU'
&STR(24) SETC 'X''C020A726'''
&STR(25) SETC 'SIOCGIFNAMEINDEX'
&STR(26) SETC 'X''4000F603'''
&STR(27) SETC 'SIOCGIPMSFILTER'
&STR(28) SETC 'X''C000A724'''
&STR(29) SETC 'SIOCGMONDATA'
&STR(30) SETC 'X''C018D902'''
&STR(31) SETC 'SIOCGMSFILTER'
&STR(32) SETC 'X''C000F610'''
&STR(33) SETC 'SIOCGPARTNERINFO'
&STR(34) SETC 'X''C000F612'''
&STR(35) SETC 'SIOCGSPLXFQDN'
&STR(36) SETC 'X''C018D905'''
&STR(37) SETC 'SIOCSAPPLDATA'
&STR(38) SETC 'X''8018D90C'''
&STR(39) SETC 'SIOCSIPMSFILTER'
&STR(40) SETC 'X''8000A725'''
&STR(41) SETC 'SIOCSMSFILTER'
&STR(42) SETC 'X''8000F611'''
&STR(43) SETC 'SIOCSPARTNERINFO'
&STR(44) SETC 'X''8004F613'''
&STR(45) SETC 'SIOCTTLSCTL'
&STR(46) SETC 'X''C038D90B'''
.*
         LCLA  &I              Loop index
.*
.* Validate input keyword value
.*
&I       SETA  2
.VLOOP   ANOP
         AIF   (&I GT N'&SYSLIST).ERROR
         AIF   ('&SYSLIST(&I)' EQ '&KWVAL').LOOKUP
&I       SETA  &I+1
         AGO   .VLOOP
.*
.LOOKUP  ANOP
.*
.* Lookup keyword value numeric equivalent
.*
&I       SETA  1
.LLOOP   ANOP
         AIF   (&I GT 46).MISS
         AIF   ('&STR(&I)' EQ '&KWVAL').GENER
&I       SETA  &I+2
         AGO   .LLOOP
.*
.GENER   ANOP
.*
.* Load R0 with ptr to string's numeric equivalent
.*
         LA    0,=&STR(&I+1)
         MEXIT
.*
.ERROR   ANOP
         MNOTE 16,'Invalid parameter value, &KWVAL'
         MEXIT
.*
.MISS    ANOP
         MNOTE 16,'Missing value string, &KWVAL'
         MEND
./ ADD NAME=EZASMI
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
./ ADD NAME=EZASMIP
.*------------------------------------------------------------------- *
.*                                                                    *
.*                   EZASMIP Macro                                    *
.*                                                                    *
.*  Function:  The EZASMIP macro analyzes EZASMI keyword parameters   *
.*             to determine their type and load the address of the    *
.*             parameter into &REG. The following types are supported:*
.*             Char constants - Character string enclosed in single   *
.*                              quotes (calls EZASMIC)                *
.*             Address label: - Assembler label referencing value     *
.*             Indirect addr: - Assembler label preceded by an        *
.*                              asterisk(*) pointing to a full word   *
.*                              which contains the address of the     *
.*                              value                                 *
.*             GPR            - General purpose register number       *
.*                              enclosed in parentheses of the        *
.*                              register containing the address of    *
.*                              the value.                            *
.*             Literal value  - Literal numeric value                 *
.*                                                                    *
.*  Parameters:                                                       *
.*             &PARM - The keyword value to be analyzed               *
.*             &REG  - The register to be loaded (default R0)         *
.*             &TYPE - Type of numeric literal to generate. Numeric   *
.*                     literal not valid if this parm not specified   *
.*             &MAX  - Maximum value for numeric literal; if zero,    *
.*                     no limit check is performed                    *
.*                                                                    *
.*  Author:    Shelby Beach, Ms. Kitty Programming Werks              *
.*                                                                    *
.*  Change Log:                                                       *
.*                                                                    *
.*    Date     Pgmr   Change                                          *
.*  12/01/2016  SLB   Initial release                                 *
.*                                                                    *
.*------------------------------------------------------------------- *
         MACRO
         EZASMIP &PARM,&REG,&TYPE,&MAX
         LCLC  &R1,&R2,&IADDR,&VAL
         LCLA  &NUM,&MNUM
.*
         GBLC  &GPR            GPR containing addr of EZASOKET parm
.*
&R1      SETC  '0'             Default register to load
&GPR     SETC  '0'             Set to register to store in EZASMI
         AIF   (K'&REG EQ 0).P1
&R1      SETC  '&REG'
.P1      ANOP
         AIF   ('&PARM'(1,1) NE '(').P2
.*
.* Register points to value
.*
.* There is no need to load a register. The parm address is already in
.* a register, we just need to tell the mainline what register needs
.* to be stored into the EZASOKET parm list.
.*
&GPR     SETC  '&PARM'(2,K'&PARM-2)
         MEXIT
.P2      ANOP
         AIF   ('&PARM'(1,1) NE '*').P3
.*
.* Indirect address points to value
.*
&IADDR   SETC  '&PARM'(2,K'&PARM-1)
         L     &R1,&IADDR
         MEXIT
.P3      ANOP
.*
.* Remove quotes if specified
.*
&VAL     SETC  '&PARM'
         AIF   ('&PARM'(1,1) NE '''').P4
&VAL     SETC  '&PARM'(2,K'&PARM-2)
.P4      ANOP
.*
.* Direct address points to value
.*
         AIF   ('&VAL'(1,1) GE '0').P5
         LA    &R1,&VAL
         MEXIT
.*
.* Literal value coded
.*
.P5      ANOP
         AIF   (K'&TYPE NE 0).P6
.*
.* Numeric literal not allowed
.*
         MNOTE 16,'Invalid keyword value, &PARM'
         MEXIT
.P6      ANOP
&MNUM    SETA  &MAX
&NUM     SETA  &VAL
         AIF   (&MNUM EQ 0).P7
         AIF   (&NUM LE &MNUM).P7
.*
.* Numeric literal exceeds max allowed
.*
         MNOTE 16,'Keyword value &VAL greater than &MAX'
         MEXIT
.*
.* Generate load of ptr to literal value
.*
.P7      ANOP
         AIF   ('&TYPE' EQ 'F').P8
         LA    &R1,=&TYPE.(&VAL.)
         MEXIT
.P8      ANOP
         LA    &R1,=F'&VAL'
         MEND
./ ADD NAME=TPIMASK
.**********************************************************************
.*                                                                    *
.* This macro provides the capability to set and test bits in the     *
.* read, write, and exception bit masks used with the EZASMI          *
.* TYPE=SELECT macro call.                                            *
.*                                                                    *
.* Invocation:                                                        *
.*       TPIMASK {SET | TEST},MASK=maskaddr,SD=socket                 *
.*                                                                    *
.*             SET      - Sets the socket descriptor bit in the mask. *
.*             TEST     - Tests the socket descriptor bit in the mask.*
.*                        The TEST function sets the condition code   *
.*                        such that you may follow the TPIMASK macro  *
.*                        call with a BC instruction (BE branches if  *
.*                        bit was on, BNE branches if bit was off).   *
.*                                                                    *
.*             maskaddr - Address of the read, write, or execution    *
.*                        mask to be acted on. May be specified as a  *
.*                        storage location label or a register        *
.*                        number enclosed in parentheses.             *
.*                                                                    *
.*             socket   - Address of the socket descriptor which      *
.*                        determines the bit to be tested or set. May *
.*                        be specified as a storage location label or *
.*                        the register number enclosed in parentheses *
.*                        that contains the socket descriptor.        *
.*                                                                    *
.* Change history:                                                    *
.*                                                                    *
.* 03/04/2017 - Initial release                 Shelby Beach          *
.*                                                                    *
.**********************************************************************
.*
         MACRO
         TPIMASK &FUNC,        Function to perform (TEST or SET)       +
               &MASK=,         Mask address                            +
               &SD=            Socket descriptor
         AIF ('&FUNC' EQ 'TEST' OR '&FUNC' EQ 'SET').FUNCOK
         MNOTE 16,'Invalid TPIMASK function'
         MEXIT

.FUNCOK  ANOP
         XR    14,14           Prepare for divide.
         AIF   ('&SD'(1,1) EQ '(').SDREG
         LH    15,&SD          R15 = Socket Descriptor
         AGO   .SDOK
.SDREG   ANOP
         LR    15,&SD          R15 = Socket Descriptor
.SDOK    ANOP
         D     14,=F'32'       R15 = Word containing bit
         SLL   15,2            R15 = Offset of word
         AIF   ('&MASK'(1,1) EQ '(').MASKREG
         LA    1,&MASK         R1 = A(Mask)
         AGO   .MASKOK
.MASKREG ANOP
         LR    1,&MASK         R1 = A(Mask)
.MASKOK  ANOP
         AR    15,1            R15 = A(Word bit is in)
         LA    1,1             Need a bit in R1.
         SLL   1,0(14)         Position bit for test/set.
         O     1,0(,15)        Combine bit with others in mask.
         AIF   ('&FUNC' EQ 'SET').SETBIT
         C     1,0(,15)        Will be equal if bit was set.
         MEXIT
.SETBIT  ANOP
         ST    1,0(,15)        Store updated mask word.
         MEND
@@
//STEP1   EXEC PGM=PDSLOAD
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DSN=SYSGEN.TCPIP.SAMPLIB,DISP=(NEW,CATLG),
//             VOL=SER=MVS000,
//             UNIT=3350,SPACE=(CYL,(1,1,5)),
//             DCB=(BLKSIZE=3120,RECFM=FB,LRECL=80)
//SYSUT1   DD  DATA,DLM=@@
./ ADD NAME=CNSLOOKUP
//NSLOOKUP JOB (JOB),'NCAT',CLASS=A,MSGCLASS=A,NOTIFY=&SYSUID,
//     REGION=4096K,MSGLEVEL=(1,1)
//ASSEM        EXEC ASMFCL,MAC='SYS2.MACLIB',MAC1='SYS2.SXMACLIB',
//             MAC2='SYS1.AMODGEN',
//             PARM.LKED='(XREF,LET,LIST,CAL)'
//ASM.SYSIN    DD DISP=SHR,DSN=SYSGEN.TCPIP.SAMPLIB(NSLOOKUP)
//LKED.SYSLMOD DD DISP=SHR,DSN=SYS2.CMDLIB(NSLOOKUP)
//LKED.SYSLIB   DD  DSN=SYS2.LINKLIB,DISP=SHR
./ ADD NAME=COMPNCAT
//NCAT JOB (JOB),'NCAT',CLASS=A,MSGCLASS=A,NOTIFY=&SYSUID,
//     REGION=4096K,MSGLEVEL=(1,1)
//ASSEM        EXEC ASMFCL,MAC='SYS2.MACLIB',MAC1='SYS2.SXMACLIB',
//             MAC2='SYS1.AMODGEN',
//             PARM.LKED='(XREF,LET,LIST,CAL)'
//ASM.SYSIN    DD DISP=SHR,DSN=SYSGEN.TCPIP.SAMPLIB(NCAT)
//LKED.SYSLMOD DD DISP=SHR,DSN=SYS2.CMDLIB(NCAT)
//LKED.SYSLIB   DD  DSN=SYS2.LINKLIB,DISP=SHR
./ ADD NAME=NCAT
NCAT     TITLE 'TCP/IP EZASMI -- Assembler XF Interface Demo'
***********************************************************************
***                                                                 ***
*** Program:  NCAT                                                  ***
***                                                                 ***
*** Purpose:  Demonstrate using the EZASMI API to interface to      ***
***           the Hercules host's IP stack via the TCPIP (X'75')    ***
***           instruction.                                          ***
***                                                                 ***
***           NCAT communicates interactively with a network-cat    ***
***           tool like ncat, netcat, socat, or another NCAT        ***
***           instance. This can be used to perform interactive     ***
***           chats over the the internet, or even to run an        ***
***           interactive "telnet like" shell session.              ***
***                                                                 ***
***           A "telnet like" session requires a suitable shell to  ***
***           be used on the remote side, like for example bash     ***
***           on *i*x or powershell.exe on Windows systems. For     ***
***           example, on a Linux host, issuing the following       ***
***           command will provide a "telnet like" bash session:    ***
***                                                                 ***
***           ncat --exec "/bin/bash" -l 4466                       ***
***                                                                 ***
***           Issuing the NCAT command without arguments on TSO     ***
***           will then connect to this bash session, allowing to   ***
***           execute commands on the host directly from the TSO    ***
***           terminal.                                             ***
***                                                                 ***
*** Usage:    NCAT <options> <hostname> <port>                      ***
***                                                                 ***
***           NCAT operates in one of two modes: Connect mode and   ***
***           listen mode. In connect mode, NCAT works as a client. ***
***           In listen mode it is a server.                        ***
***                                                                 ***
***           - In connect mode, the <hostname> and <port>          ***
***             arguments tell where to connect to. <hostname> is   ***
***             required, and may be a hostname or an IP address.   ***
***             If <port> is supplied, it must be a decimal port    ***
***             number. If omitted, it defaults to 4466.            ***
***                                                                 ***
***           - In listen mode, <hostname> and <port> control the   ***
***             address the server will bind to. Both arguments are ***
***             optional. If <hostname> is omitted, it defaults to  ***
***             listening on all available addresses. If <port> is  ***
***             omitted, it defaults to 4466.                       ***
***                                                                 ***
***           Options                                               ***
***                                                                 ***
***           -l Bind and listen for incoming connections.          ***
***           -j Improved handling and readability for interactive  ***
***              chats. This option shouldn't be used with sessions ***
***              where the remote session partner is a shell style  ***
***              program.                                           ***
***                                                                 ***
***           Once the connection is established, data entered in   ***
***           the TSO session running NCAT will be transferred to   ***
***           the remote peer and data received from the remote     ***
***           peer will be displayed on the TSO terminal.           ***
***                                                                 ***
***           To terminate enter /* on the NCAT session, or send    ***
***           EOT from the remote peer, or close the connection at  ***
***           the remote peer. When NCAT is running as a server     ***
***           (i.e. the -l flag was specified) it is recommended    ***
***           to have the remote peer close its connection first,   ***
***           because otherwise the server port might enter a       ***
***           FIN_WAIT_x or a TIME_WAIT state, making it            ***
***           unavailable until the respective timeout(s)           ***
***           expire(s).                                            ***
***                                                                 ***
*** Note:     The NCAT program isn't designed to perform receive    ***
***           operations in quick succession, as, after each        ***
***           receive operation, the data received is written to    ***
***           the terminal before the next recv() call is           ***
***           performed. This is no (significant) restriction for   ***
***           interactive chatting or shell sessions. A peer        ***
***           program sending bulks of data in quick succession,    ***
***           however, can easily overrun NCAT.                     ***
***                                                                 ***
*** Updates:  2017/01/10 original implementation.                   ***
***           2017/01/22 command line processing using IKJPARSE.    ***
***                                                                 ***
*** Author:   Juergen Winkelmann, winkelmann@id.ethz.ch             ***
***                                                                 ***
*** Credits:  Thanks to Shelby Beach for providing an MVS 3.8j      ***
***           (Assembler XF) version of the EZASMI API.             ***
***                                                                 ***
***********************************************************************
TERMSIZE EQU   4096             size of terminal input buffer
RECVSIZE EQU   81920            size of network receive buffer
NCAT     CSECT ,                start of program
         STM   R14,R12,12(R13)  save registers
         LR    R12,R15          establish module addressability
         LA    R11,1(,R12)      second base ..
         LA    R11,4095(,R11)                 .. is base plus 4096
         USING NCAT,R12,R11     tell assembler of base
         ST    R13,REGSMAIN+4   chain ..
         LA    R2,REGSMAIN        .. the ..
         ST    R2,8(R13)            .. save ..
         LR    R13,R2                 .. areas
*
* Parse command line
*
         USING CPPL,R1          establish CPPL addressability
         MVC   PPLUPT,CPPLUPT   put in the UPT address from CPPL
         MVC   PPLECT,CPPLECT   put in the ECT address from CPPL
         MVC   PPLCBUF,CPPLCBUF put in the command buffer address
         DROP  R1               don't use CPPL any more
         CALLTSSR EP=IKJPARS,MF=(E,PPLUPT) invoke parse
         L     R2,ANSWER        get answer address
         USING OPERANDS,R2      establish answer addressability
         USING OPERPDE,R3       establish PDE addressability
         SR    R9,R9            exit indicator for storage release
         LA    R3,O1            get address of first operand PDE
         LA    R10,4            operand count
PROCESS  TM    OPERFLGS,X'80'   operand present?
         BZ    NEXT             no -> process next operand
         L     R4,OPER          yes -> get operand address
         LH    R5,OPERLNG       get operand length
         CHI   R5,2             at least two characters?
         BL    NOTFLAG          no -> can't be a flag
         CLC   0(2,R4),=CL2'-j' chat mode requested?
         BNE   *+12             no -> default to shell mode
         MVI   CHAT,X'01'       yes -> enable chat mode
         B     NEXT             process next operand
         CLC   0(2,R4),=CL2'-l' listen mode requested?
         BNE   *+12             no -> default to connect mode
         MVI   LISTEN,X'01'     yes -> enable listen mode
         B     NEXT             process next operand
         CLI   0(R4),C'-'       any other flag specified?
         BNE   NOTFLAG          no -> done with flags
         LA    R9,1             signal exit after storage release
         STRING 'Invalid flag specified: ',((R4),(R5)),                +
               INTO=PRTDATA     yes -> format error message ..
         TPUT  PRTDATA,L'PRTDATA       .. and tell user
         B     PARSERLS         release parse storage and exit
NOTFLAG  LR    R6,R4            operand address
         LR    R7,R5            operand length
ISDIGIT  CLI   0(R6),C'0'       EBCDIC zero or greater?
         BL    NOTPORT          no -> can't be a port number
         CLI   0(R6),C'9'       EBCDIC nine or lower?
         BH    NOTPORT          no -> can't be a port number
         LA    R6,1(,R6)        increment and ..
         BCT   R7,ISDIGIT                       .. check next character
         LR    R7,R5            number of digits entered as port number
         CHI   R7,5             more than five digits?
         BNH   *+8              no -> use it
         LA    R7,5             yes -> truncate to 5 digits
         ST    R7,PORTLEN       remember number of digits
         LA    R6,PORTR+5       right justify ..
         SR    R6,R7              .. to 5 digits
         BCTR  R7,0             decrement for EXecute
         EX    R7,MOVEPRTR      get port number right justified
         PACK  PORTD(8),PORTRPCK(10) pack port number and ..
         CVB   R6,PORTD             .. convert to binary
         STH   R6,PORT          remember port number
         B     NEXT             process next operand
NOTPORT  LR    R7,R5            operand length
         BCTR  R7,0             decrement for EXecute
         EX    R7,MOVEHOST      get hostname or IP address
         ST    R5,NAMELEN       remember length
NEXT     LA    R3,O2-O1(,R3)    address next operand
         BCT   R10,PROCESS      process next operand
         DROP  R2,R3            don't need PDEs any more
PARSERLS IKJRLSA (R2)           free storage that parse allocated
         LTR   R9,R9            exit signaled?
         BNZ   ALLDONE          yes -> exit
*
* Initialize EZASMI interface
*
         XC    EZASMTIE(TIELENTH),EZASMTIE clear EZASMI storage
         EZASMI TYPE=INITAPI,MAXSNO=MAXSNO,ERRNO=ERRCD,RETCODE=RETCD
*
* Try converting hostname/IP addr to network binary format using PTON
*
         EZASMI TYPE=PTON,AF='INET',SRCADDR=HOSTIN,SRCLEN=NAMELEN+2,   +
               DSTADDR=IPADDR,ERRNO=ERRCD,RETCODE=RETCD
         CLC   RETCD,=X'FFFFFFFF' conversion successful?
         BE    GETHBN           no, go treat input as a hostname
         LH    R6,NAMELEN+2     get length
         STH   R6,ADDRLEN       remember for display
         BCTR  R6,0             decrement for EXecute
         LA    R3,HOSTIN        address of IP address
         EX    R6,GETIP         copy IP address
*
* Resolve IP address to hostname using GETHOSTBYADDR
*
         EZASMI TYPE=GETHOSTBYADDR,HOSTADR=IPADDR,HOSTENT=HOSTENT,     +
               RETCODE=RETCD
         L     R3,HOSTENT       get hostname address ..
         L     R3,0(,R3)            .. from HOSTENT
         LR    R6,R3            \
CHKLEN   CLI   0(R6),X'00'       \
         BE    HOSTEND            \ compute length
         LA    R6,1(,R6)          / of hostname
         B     CHKLEN            /
HOSTEND  SR    R6,R3            /
         ST    R6,NAMELEN       remember length
         BCTR  R6,0             decrement for EXecute
         EX    R6,GETHOST       copy hostname
         B     SOCKET           create socket
*
* Resolve hostname to IP address using GETHOSTBYNAME
*
GETHBN   L     R6,NAMELEN       get length
         BCTR  R6,0             decrement for EXecute
         LA    R3,HOSTIN        hostname address
         EX    R6,GETHOST       copy hostname
         EZASMI TYPE=GETHOSTBYNAME,NAMELEN=NAMELEN,NAME=HOSTNAME,      +
               HOSTENT=HOSTENT,RETCODE=RETCD
         L     R3,HOSTENT       \
         L     R3,16(,R3)        \
         L     R3,0(,R3)          > get first IP address from HOSTENT
         L     R3,0(,R3)         /
         ST    R3,IPADDR        /
*
* Convert IP address to dotted decimal format using NTOP
*
         EZASMI TYPE=NTOP,AF='INET',SRCADDR=IPADDR,DSTADDR=ADDR,       +
               DSTLEN=ADDRLEN,ERRNO=ERRCD,RETCODE=RETCD
*
* Create socket
*
SOCKET   EZASMI TYPE=SOCKET,AF='INET',SOCTYPE='STREAM',                +
               ERRNO=ERRCD,RETCODE=SOCKDESC
         LT    R3,SOCKDESC      socket created?
         BNL   MODE             yes -> continue
         TPUT  MNOSOCK,L'MNOSOCK \ no -> tell user..
         B     RETNSOCK          /                  .. and exit
*
* listen or connect
*
MODE     CLI   LISTEN,X'01'     listen mode requested?
         BNE   CONNECT          no -> go connect
*
* Listen
*
         LH    R6,ADDRLEN       length of dotted decimal IP address
         ICM   R5,B'1111',NAMELEN length of hostname
         BNZ   *+16             not zero -> continue
         MVI   HOSTNAME,C'*'    \
         LA    R5,1              > else -> set hostname to asterisk
         ST    R5,NAMELEN       /
         STRING 'Listening at ',(HOSTNAME,(R5)),' (',(ADDR,(R6)),')',  +
               ' port ',(PORT,H,L),INTO=PRTDATA format message
BIND     EZASMI TYPE=BIND,S=SOCKDESC+2,NAME=SOCKADDR,                  +
               ERRNO=ERRCD,RETCODE=RETCD
         LT    R3,RETCD         socket bound?
         BNL   BOUND            yes -> continue
         CLFHSI ERRCD,EADDINUS  no -> address already in use?
         BE    RETRYBND               yes -> wait and retry
         TPUT  MNOBIND,L'MNOBIND      no -> tell user..
         B     RETURN           exit
RETRYBND TPUT  MINUSE,L'MINUSE  tell user
         STIMER WAIT,BINTVL=WAIT1000 wait ten seconds
         B     BIND             retry
BOUND    EZASMI TYPE=LISTEN,S=SOCKDESC+2,BACKLOG='1',                  +
               ERRNO=ERRCD,RETCODE=RETCD
         LT    R3,RETCD         listening?
         BNL   LISTNING         yes -> continue
         TPUT  MNOLIST,L'MNOLIST no -> tell user..
         B     RETURN           exit
LISTNING TPUT  PRTDATA,L'PRTDATA listening, tell user
         EZASMI TYPE=ACCEPT,S=SOCKDESC+2,NAME=SOCKADDR,                +
               ERRNO=ERRCD,RETCODE=NEWSOCK
         LT    R3,NEWSOCK       socket connected?
         BNL   ACCEPTED         yes -> continue
         TPUT  MNOACPT,L'MNOACPT no -> tell user..
         B     RETURN           exit
ACCEPTED EZASMI TYPE=CLOSE,S=SOCKDESC+2,ERRNO=ERRCD,RETCODE=RETCD
         MVC   SOCKDESC,NEWSOCK
         B     CONECTED         connection established
*
* Connect
*
CONNECT  CLC   IPADDR,=XL4'00000000' address zero?
         BNE   ADDROK           no -> continue
         MVC   IPADDR,=XL4'7F000001'  \
         MVC   ADDR,=CL9'127.0.0.1'    \
         MVC   HOSTNAME,=CL9'localhost' \ yes -> connect to
         LA    R6,9                     /        localhost (127.0.0.1)
         STH   R6,ADDRLEN              /
         ST    R6,NAMELEN             /
ADDROK   EZASMI TYPE=CONNECT,S=SOCKDESC+2,NAME=SOCKADDR,               +
               ERRNO=ERRCD,RETCODE=RETCD
         LT    R3,RETCD         socket connected?
         BNL   CONECTED         yes -> continue
         TPUT  MNOCONN,L'MNOCONN no -> tell user..
         B     RETURN           exit
*
* Identify peer
*
CONECTED EZASMI TYPE=GETPEERNAME,S=SOCKDESC+2,NAME=SOCKADDR,           +
               ERRNO=ERRCD,RETCODE=RETCD
         LT    R3,RETCD         GETPEERNAME successful?
         BNL   GOTPEER          yes -> continue
         TPUT  MNOPEER,L'MNOPEER no -> tell user..
         B     RETURN           exit
*
* Resolve peer IP address to hostname using GETHOSTBYADDR
*
GOTPEER  EZASMI TYPE=GETHOSTBYADDR,HOSTADR=IPADDR,HOSTENT=HOSTENT,     +
               RETCODE=RETCD
         L     R3,HOSTENT       get hostname address ..
         L     R3,0(,R3)            .. from HOSTENT
         LR    R6,R3            \
CHKLENP  CLI   0(R6),X'00'       \
         BE    HOSTENDP           \ compute length
         LA    R6,1(,R6)          / of hostname
         B     CHKLENP           /
HOSTENDP SR    R6,R3            /
         ST    R6,NAMELEN       remember length
         BCTR  R6,0             decrement for EXecute
         EX    R6,GETHOST       copy hostname
*
* Convert peer IP address to dotted decimal format using NTOP
*
         LA    R6,L'ADDR        reinitialize length ..
         STH    R6,ADDRLEN                            .. of IP address
         EZASMI TYPE=NTOP,AF='INET',SRCADDR=IPADDR,DSTADDR=ADDR,       +
               DSTLEN=ADDRLEN,ERRNO=ERRCD,RETCODE=RETCD
*
* Display connection message
*
         LH    R6,ADDRLEN       length of dotted decimal IP address
         L     R5,NAMELEN       length of hostname
         LA    R4,MFROM         indicate server mode
         LA    R3,L'MFROM       length of server mode message
         LT    R2,NEWSOCK       did we get a new socket?
         BNL   *+12             yes -> continue
         LA    R4,MTO           indicate client mode
         LA    R3,L'MTO         length of client mode message
         STRING ((R4),(R3)),(HOSTNAME,(R5)),' (',(ADDR,(R6)),')',      +
               ' port ',(PORT,H,L),INTO=PRTDATA format message
         TPUT  PRTDATA,L'PRTDATA display message
*
* Get storage for SEND/RECV buffers
*
         GETMAIN R,LV=TERMSIZE  get terminal input buffer
         ST    R1,TERMBUFA      remember address
         GETMAIN R,LV=RECVSIZE  get network input buffer
         ST    R1,NETIN         remember address
*
* Use linefeed character matching the installation's conversion tables
*
         LA    R3,1             convert ASCII linefeed ..
         ST    R3,NETLENR                .. to EBCDIC (LF or NL) and ..
         CALL  EZACIC05,(LF1+4,NETLENR),VL .. put it into ..
         MVC   LF2+1(1),LF1+4                .. immediate instructions
*
* Start terminal input task
*
         MVI   RUNNING,X'01'    indicate dialog is running
         IDENTIFY EP=NCATRMIN,ENTRY=GETLINE define entry point
         ATTACH EP=NCATRMIN     attach task
         ST    R1,TERMTCB       remember TCB
*
* While RUNNING is X'01' and no RECV error occurs:
* Read network input from peer and display it on terminal
*
         TPUT  UNLOCK,5,CONTROL unlock keyboard
READNET  CLI   RUNNING,X'01'    dialog running?
         BNE   RETURN           no -> exit
         MVC   NETLENR,NETINL   yes -> get length of network buffer
         EZASMI TYPE=RECV,S=SOCKDESC+2,NBYTE=NETLENR,BUF=*NETIN,       +
               ERRNO=ERRCD,RETCODE=RETCD
         LT    R3,RETCD         successfully received?
         BNL   RECVOK           yes -> continue
         CLFHSI ERRCD,ENOTSOCK  no -> socket already closed?
         BE    RETNSOCK               yes -> exit without error msg
         CLFHSI ERRCD,ECANCELD        no -> RECV cancelled?
         BE    RETNSOCK                     yes -> exit without msg
         TPUT  MNORECV,L'MNORECV            no -> tell user..
         B     RETURN           exit
RECVOK   BH    CONVERT          convert if at least one byte received
         TPUT  EOD,L'EOD        nothing received: Tell user ..
         B     RETURN              .. and exit
CONVERT  ST    R3,NETLENR       store length for use by EZACIC05
         L     R4,NETIN         address data
         CALL  EZACIC05,((R4),NETLENR),VL convert to EBCDIC
         CLI   0(R4),55         EOT character received?
         BNE   CHKCHATR         no -> check for dialog type
         TPUT  EOT,L'EOT        yes -> Tell user ..
         B     RETURN              .. and exit
CHKCHATR CLI   CHAT,X'01'       chat mode?
         BNE   DISPLAY          no -> display received data
         TPUT  NEWLINE,1,ASIS   yes -> avoid overtyping of input
*
* Split data at LF or CRLF and display line by line on terminal
*
DISPLAY  LR    R5,R4            initialize compare pointer
         SR    R8,R8            initialize to zero for CLIJ
NEXTCHAR IC    R8,0(,R5)        load character to check
LF1      CLIJE R8,X'0A',DISPLINE if linefeed -> display line
         LA    R5,1(,R5)                else -> point to next character
         B     NEXTCHAR         check next character
DISPLINE MVI   0(R5),X'15'      replace linefeed with newline
         LR    R7,R4            remember begin of current output line
         LA    R6,1(,R5)        remember begin of next output line
         BCTR  R5,0             last character of output line
         IC    R8,0(,R5)        load character to check
         CLIJNE R8,X'0D',LASTCHAR if not CR -> go ahead
         BCTR  R5,0                    else -> decrement
LASTCHAR LA    R5,1(,R5)        compute ...
         SR    R5,R4                       ... length of output line
         TPUT  (R7),(R5)        display on terminal
         LR    R5,R6            compute ..
         SR    R5,R7                      .. residual ..
         SR    R3,R5                                    .. count
         LR    R4,R6            initialize ..
         LR    R5,R6                         .. next line
         BNZ   NEXTCHAR         more data -> check it
         TPUT  NEWLINE,1,ASIS        else -> newline ..
         TPUT  PROMPT,1,ASIS                       .. prompt
         TPUT  UNLOCK,5,CONTROL                  .. and unlock keyboard
         B     READNET          continue reading socket
*
* Exit processing
*
RETURN   EZASMI TYPE=CLOSE,S=SOCKDESC+2,ERRNO=ERRCD,RETCODE=RETCD
RETNSOCK MVI   RUNNING,X'02'    terminate terminal task
         STIMER WAIT,BINTVL=WAIT50 wait half a second
         EZASMI TYPE=TERMAPI    terminate API
         CLI   RUNNING,X'00'    never running?
         BE    DONE             yes -> just exit
         STIMER WAIT,BINTVL=WAIT50 wait half a second
         CLI   RUNNING,X'FF'    terminal subtask already terminated?
         BE    DONE             yes -> we are done
         STIMER WAIT,BINTVL=WAIT50 no -> wait half a second and exit
*
* Release SEND/RECV buffers
*
DONE     ICM   R1,B'1111',TERMBUFA send buffer address
         BZ    RELNETIN         skip release if never allocated
         FREEMAIN R,LV=TERMSIZE,A=(R1) release send buffer
RELNETIN ICM   R1,B'1111',NETIN receive buffer address
         BZ    ALLDONE          skip release if never allocated
         FREEMAIN R,LV=RECVSIZE,A=(R1) release receive buffer
*
* That's all folks!
*
ALLDONE  L     R13,4(,R13)      caller's save area pointer
         RETURN (14,12),RC=0    restore registers and return
*
* Read terminal input and send it to peer
*
         DROP  R11,R12          avoid confusion
MAINLEN  DC    Y(GETLINE-NCAT)  length of mainline code
GETLINE  STM   R14,R12,12(R13)  save registers
         USING GETLINE,R15      tell assembler of temporary base
         ST    R13,REGSTGET+4   chain ..
         LA    R2,REGSTGET        .. the ..
         ST    R2,8(R13)            .. save ..
         LR    R13,R2                 .. areas
         LA    R5,GETLINE       address ..
         BCTR  R5,0                      .. length of ..
         BCTR  R5,0                                    .. mainline code
         DROP  R15              temporary base no longer needed
         LR    R12,R15          entry address minus mainline ..
         SH    R12,0(,R5)                    .. length is mainline base
         LA    R11,1(,R12)      second base ..
         LA    R11,4095(,R11)                 .. is base plus 4096
         USING NCAT,R12,R11     tell assembler to use mainline bases
*
* Loop while RUNNING is X'01' and no SEND error occurs
*
         TPUT  NEWLINE,1,ASIS   initial newline and ..
         TPUT  PROMPT,1,ASIS                          .. prompt
         B     READNEXT         unlock keyboard and read from terminal
READTERM CLI   RUNNING,X'01'    dialog running?
         BNE   STOPTRMI         no -> stop this task
         L     R9,TERMBUFA      address of terminal buffer
         L     R8,TERMBUFS      size of terminal buffer
         TGET  (R9),(R8),,NOWAIT yes -> read input from terminal
         CFI   R15,4            input available?
         BNE   GOTINPUT         yes -> check for EOD
         STIMER WAIT,BINTVL=WAIT25 no -> wait quarter second ..
         B     READTERM                  .. and try again
GOTINPUT CLHHSI 0(R9),C'/*'     EOD?
         BNE   SENDIT           no -> send it
         TPUT  EOD,L'EOD        yes -> Tell user ..
         B     STOPTRMI                .. and stop this task
SENDIT   LA    R3,1(,R1)        increment length for newline
         ST    R3,NETLENS       store length for use by EZACIC04/SEND
         LA    R2,0(R1,R9)      terminal input
LF2      MVI   0(R2),*-*        insert linefeed character
         L     R4,TERMBUFA      address data
         CALL  EZACIC04,((R4),NETLENS),VL convert to ASCII
         EZASMI TYPE=SEND,S=SOCKDESC+2,NBYTE=NETLENS,BUF=*TERMBUFA,    +
               ERRNO=ERRCDS,RETCODE=RETCDS,MF=(E,TRMPLIST)
         LT    R3,RETCDS        successfully sent?
         BNL   CHKCHATS         yes -> check for dialog type
         TPUT  MNOSEND,L'MNOSEND no -> tell user..
         B     STOPTRMI                .. and stop this task
CHKCHATS CLI   CHAT,X'01'       chat mode?
         BNE   READNEXT         no -> read next line
         TPUT  NEWLINE,1,ASIS   yes -> newline and ..
         TPUT  PROMPT,1,ASIS                         .. prompt
READNEXT TPUT  UNLOCK,5,CONTROL unlock keyboard
         B     READTERM         start over
*
* Close connection and exit terminal input handler
*
STOPTRMI CLI   RUNNING,X'02'    stopped by main task?
         BE    OUT              yes -> exit, no -> close, then exit
         EZASMI TYPE=CLOSE,S=SOCKDESC+2,ERRNO=ERRCDS,RETCODE=RETCDS,   +
               MF=(E,TRMPLIST)
         STIMER WAIT,BINTVL=WAIT25 wait quarter second
OUT      MVI   RUNNING,X'FF'    indicate we are out
         L     R13,4(,R13)      caller's save area pointer
         RETURN (14,12),RC=0    restore registers and return
*
* EXecuted instructions
*
GETHOST  MVC   HOSTNAME(*-*),0(R3) copy hostname
GETIP    MVC   ADDR(*-*),0(R3)  copy IP address
MOVEPRTR MVC   0(*-*,R6),0(R4)  right justify port number
MOVEHOST MVC   HOSTIN(*-*),0(R4) get hostname/IP addr from command line
*
* Data areas
*
REGSMAIN DS    18F              main task save area
REGSTGET DS    18F              terminal input task save area
ERRCD    DC    A(*-*)           error code main task
ERRCDS   DC    A(*-*)           error code terminal input task
RETCD    DC    A(*-*)           return code main task
RETCDS   DC    A(*-*)           return code terminal input task
MAXSNO   DC    A(*-*)           highest socket number assigned
SOCKDESC DC    F'-1'            socket descriptor
SOCKADDR DS    0F               sockaddr structure
AF@INET  DC    H'2'             address family
PORT     DC    H'4466'          port to connect to or listen at
IPADDR   DC    F'0'             INET address (netid)
         DC    XL8'00'          Reserved area not used
NEWSOCK  DC    F'-1'            new socket descriptor from ACCEPT
HOSTENT  DC    F'0'             address of HOSTENT structure goes here
PORTD    DC    D'0'             decimal port number
MNOSOCK  DC    C'SOCKET failed'
MNOCLOS  DC    C'CLOSE failed'
MNOCONN  DC    C'CONNECT failed'
MNOBIND  DC    C'BIND failed'
MINUSE   DC    C'BIND: Address in use, retrying in 10 seconds...'
MNOLIST  DC    C'LISTEN failed'
MNOACPT  DC    C'ACCEPT failed'
MNOPEER  DC    C'GETPEERNAME failed'
MNOSEND  DC    C'SEND failed'
MNORECV  DC    C'RECV failed'
MFROM    DC    C'Connection from '
MTO      DC    C'Connected to '
EOD      DC    C'Terminating at EOD'
EOT      DC    C'Terminating at EOT'
NEWLINE  DC    X'15'            EBCDIC newline character
PROMPT   DC    C'$'             prompt character
CHAT     DC    X'00'            dialog type: chat = 1, shell = 0
LISTEN   DC    X'00'            mode: listen = 1, connect = 0
RUNNING  DC    X'00'            status indicator
UNLOCK   DC    X'27F1C30013'    control datastream to unlock keyboard
ADDR     DC    CL15' '          dotted decimal IP address
ADDRLEN  DC    Y(L'ADDR)        length of dotted decimal IP address
HOSTNAME DC    CL256' '         Hostname
TERMTCB  DS    F                TCB of terminal input subtask
WAIT25   DC    F'25'            quarter of a second
WAIT50   DC    F'50'            half a second
WAIT1000 DC    F'1000'          ten seconds
NAMELEN  DC    F'0'             length of hostname
PORTLEN  DC    F'0'             length of port number
NETLENR  DS    F                number of bytes for RECV/EZACIC05
NETLENS  DS    F                number of bytes for SEND/EZACIC04
PORTRPCK DC    CL5' '           port right justified for PACK
PORTR    DC    CL5' '           port right justified
PRTDATA  DC    CL78' '          formatted result
HOSTIN   DC    CL50' '          hostname or IP addr from command line
TERMBUFA DC    F'0'             address of terminal input buffer
TERMBUFS DC    A(TERMSIZE)      size of terminal input buffer
NETIN    DC    F'0'             address of network input buffer
NETINL   DC    A(RECVSIZE)      size of network input buffer
ANSWER   DS    F                IKJPARSE answer place
PPLUPT   DS    A                address of UPT
PPLECT   DS    A                address of ECT
PPLECB   DC    A(ECB)           address of CP's ECB
PPLPCL   DC    A(PCLDEFS)       address of PCL
PPLANS   DC    A(ANSWER)        address of answer place
PPLCBUF  DS    A                address of command buffer
PPLUWA   DC    A(0)             address of user work area
ECB      DC    F'0'             CP's event control block
PCLDEFS  IKJPARM  DSECT=OPERANDS begin of operand descriptions
O1       IKJIDENT 'Operand 1',ASIS,CHAR
O2       IKJIDENT 'Operand 2',ASIS,CHAR
O3       IKJIDENT 'Operand 3',ASIS,CHAR
O4       IKJIDENT 'Operand 4',ASIS,CHAR
         IKJENDP ,              end of operand descriptions
TRMPLIST EZASMI MF=L            EZASMI parameter list for terminal task
         EZASMI TYPE=TASK,STORAGE=CSECT EZASMI task storage
         LTORG                  literals go here
         STRING GENERATE        STRING storage and code goes here
ENOTSOCK EQU   38               socket already closed on RECV
EADDINUS EQU   48               address already in use
ECANCELD EQU   1009             RECV cancelled, EIBMCANCELLED
OPERPDE  DSECT ,                PDE mapping for a positional operand
OPER     DS    F                address of operand value
OPERLNG  DS    H                length of operand value
OPERFLGS DS    CL1              flags byte
         DS    CL1              reserved
         IKJCPPL ,              command processor parameter list
         CVT   DSECT=YES        CVT mapping needed for CALLTSSR
         YREGS ,                register equates
         END   NCAT             end of program
./ ADD NAME=NSLOOKUP
NSLOOKUP TITLE 'TCP/IP EZASMI -- Assembler XF Interface Demo'
***********************************************************************
***                                                                 ***
*** Program:  NSLOOKUP                                              ***
***                                                                 ***
*** Purpose:  Demonstrate using the EZASMI API to interface to      ***
***           the Hercules host's IP stack via the TCPIP (X'75')    ***
***           instruction.                                          ***
***                                                                 ***
*** Usage:    Run from the TSO READY prompt.                        ***
***                                                                 ***
*** Function: - read hostname or IP address from terminal.          ***
***                                                                 ***
***           - resolve depending on type of input:                 ***
***                                                                 ***
***             o Hostname:   Call GETHOSTBYNAME and NTOP           ***
***             o IP address: Call PTON and GETHOSTBYADDR           ***
***                                                                 ***
***           - display result on terminal.                         ***
***                                                                 ***
*** Updates:  2016/12/31 original implementation.                   ***
***                                                                 ***
*** Author:   Juergen Winkelmann, winkelmann@id.ethz.ch             ***
***                                                                 ***
*** Credits:  Thanks to Shelby Beach for providing an MVS 3.8j      ***
***           (Assembler XF) version of the EZASMI API.             ***
***                                                                 ***
***********************************************************************
NSLOOKUP CSECT ,                start of program
         STM   R14,R12,12(R13)  save registers
         LR    R12,R15          establish module addressability
         USING NSLOOKUP,R12     tell assembler of base
         ST    R13,REGSAVE+4    chain ..
         LA    R2,REGSAVE         .. the ..
         ST    R2,8(R13)            .. save ..
         LR    R13,R2                 .. areas
*
* Get input
*
         TPUT  BANNER,BANNERL,ASIS prompt for hostname or IP address
         TGET  INPUT,L'INPUT    read input from terminal
         LR    R5,R1            remember length ..
         ST    R5,NAMELEN                          .. of input
*
* Initialize EZASMI interface
*
         EZASMI TYPE=INITAPI,MAXSNO=MAXSNO,ERRNO=ERRCD,RETCODE=RETCD
*
* Try converting input to network binary format using PTON
*
         EZASMI TYPE=PTON,AF='INET',SRCADDR=INPUT,SRCLEN=NAMELEN+2,    +
               DSTADDR=IPADDR,ERRNO=ERRCD,RETCODE=RETCD
         CLC   RETCD,=X'FFFFFFFF' conversion successful?
         BE    GETHBN           no, go treat input as a hostname
*
* Resolve IP address to hostname using GETHOSTBYADDR
*
         EZASMI TYPE=GETHOSTBYADDR,HOSTADR=IPADDR,HOSTENT=HOSTENT,     +
               RETCODE=RETCD
         L     R3,HOSTENT       get hostname address ..
         L     R3,0(,R3)            .. from HOSTENT
         LR    R6,R3            \
CHKLEN   CLI   0(R6),X'00'       \
         BE    HOSTEND            \ compute length
         LA    R6,1(,R6)          / of hostname
         B     CHKLEN            /
HOSTEND  SR    R6,R3            /
         STRING 'IP address: ',(INPUT,(R5)),', Hostname: ',((R3),(R6)),+
               INTO=PRTDATA     format result
         B     DISPLAY          display result on terminal
*
* Resolve hostname to IP address using GETHOSTBYNAME
*
GETHBN   DS    0H               come here if input isn't an IP address
         EZASMI TYPE=GETHOSTBYNAME,NAMELEN=NAMELEN,NAME=INPUT,         +
               HOSTENT=HOSTENT,RETCODE=RETCD
         L     R3,HOSTENT       \
         L     R3,16(,R3)        \
         L     R3,0(,R3)          > get first IP address from HOSTENT
         L     R3,0(,R3)         /
         ST    R3,IPADDR        /
*
* Convert IP address to dotted decimal format using NTOP
*
         EZASMI TYPE=NTOP,AF='INET',SRCADDR=IPADDR,DSTADDR=ADDR,       +
               DSTLEN=ADDRLEN,ERRNO=ERRCD,RETCODE=RETCD
         LH    R6,ADDRLEN       length of converted address
         STRING 'Hostname: ',(INPUT,(R5)),', IP address: ',(ADDR,(R6)),+
               INTO=PRTDATA     format result
*
* Display result, terminate EZASMI interface and return
*
DISPLAY  TPUT  PRTDATA,L'PRTDATA display result
         EZASMI TYPE=TERMAPI    terminate API
         L     R13,4(,R13)      caller's save area pointer
         RETURN (14,12),RC=0    restore registers and return
*
* Data areas
*
REGSAVE  DS    18F              save area
ERRCD    DC    A(*-*)           error code
RETCD    DC    A(*-*)           return code
MAXSNO   DC    A(*-*)           highest socket number assigned
NAMELEN  DS    F                length of hostname
IPADDR   DS    F                IP address in network binary format
HOSTENT  DS    F                address of HOSTENT structure goes here
BANNER   DC    C'MVS 3.8j NSLOOKUP - enter hostname or IP address:'
BANNERL  EQU   *-BANNER         length of banner
ADDR     DC    CL15' '          dotted decimal IP address
ADDRLEN  DC    AL2(L'ADDR)      length of dotted decimal IP address
INPUT    DC    CL60' '          input from terminal goes here
PRTDATA  DC    CL78' '          formated result
         EZASMI TYPE=TASK,STORAGE=CSECT EZASMI storage goes here
         STRING GENERATE        STRING storage and code goes here
         YREGS ,                register equates
         END   NSLOOKUP         end of program
@@
//*
//* PROC TO ASSEMBLE EZASMI
//*
//ASSEM    PROC M=DUMMY
//ASM      EXEC PGM=IFOX00,REGION=4096K,
// PARM=('XREF(FULL),OBJ,SYSPARM((ON,GEN,NODATA,YES,YES))',
//       'NODECK')
//SYSLIB   DD    DSN=SYS2.SXMACLIB,DISP=SHR
//         DD    DSN=SYS2.MACLIB,DISP=SHR
//         DD    DSN=SYS1.AMODGEN,DISP=SHR
//         DD    DSN=SYS1.MACLIB,DISP=SHR
//SYSUT1   DD    DSN=&&SYSUT1,UNIT=SYSDA,SPACE=(1700,(5600,500))
//SYSUT2   DD    DSN=&&SYSUT2,UNIT=SYSDA,SPACE=(1700,(1300,500))
//SYSUT3   DD    DSN=&&SYSUT3,UNIT=SYSDA,SPACE=(1700,(1300,500))
//SYSPRINT DD SYSOUT=*
//SYSPUNCH DD SYSOUT=B
//SYSGO    DD DSN=&&OBJ(&M),DISP=(MOD,PASS),
//            SPACE=(800,(2000,1000,10)),UNIT=SYSDA
//SYSIN    DD DUMMY
// PEND
//*
//*
//*
//EZACIC04 EXEC ASSEM,M=EZACIC04
//ASM.SYSIN DD *
EZACIC04 TITLE 'TCP/IP for MVS 3.8 EBCDIC to ASCII Translation'

EZACIC04 CSECT
***********************************************************************
* This routine provides an interface which is compatible with the     *
* the z/OS Communications Server routine to perform an EBCDIC to      *
* ASCII translation. Translation is accomplished by invoking EZASOH03 *
* which then executes the TCPIP instruction to perform the translation*
* on the Hercules host.                                               *
*                                                                     *
* Invocation:                                                         *
*                                                                     *
*  CALL   EZACIC04,(buffer,length),VL                                 *
*          buffer - Address of the EBCDIC string to translate to ASCII*
*          length - Length of the string to be translated             *
*                                                                     *
* Change history:                                                     *
*                                                                     *
* 02/28/2017 - Initial release                 Shelby Beach           *
*                                                                     *
***********************************************************************

***********************************************************************
*                 Register usage                                      *
***********************************************************************
R0       EQU   0
R1       EQU   1
R2       EQU   2
R3       EQU   3
R4       EQU   4
R5       EQU   5
R6       EQU   6
R7       EQU   7
R8       EQU   8                       A(Buffer)
R9       EQU   9                       A(L'Buffer)
R10      EQU   10                      A(EZACIC04 parameter list)
R11      EQU   11                      Intermediate return code value
R12      EQU   12                      Module base
R13      EQU   13                      A(Dynamic storage area)
R14      EQU   14
R15      EQU   15

***********************************************************************
*                 Entry Processing                                    *
***********************************************************************
         STM   R14,R12,12(R13)         Save caller's regs.
         LR    R12,R15                 R12 = Base addr
         USING EZACIC04,R12

         LR    R10,R1                  R10 = A(EZACIC04 parm list)

*  Allocate dynamic storage area
         LA    R0,DSALEN               R0 = L'Dynamic storage area
         GETMAIN R,LV=(0)              Get DSA.

         ST    R13,4(,R1)              Chain save
         ST    R1,8(,R13)              areas.
         LR    R13,R1                  R13 = A(DSA)
         USING DSA,R13

***********************************************************************
*                 Request Translation                                 *
***********************************************************************
         LM    R8,R9,0(R10)            R8 = A(Buffer); R9 = A(L'Buffer)

*  Perform required error checks
         LA    R11,8                   R11 = RC (Too many parms)
         LTR   R8,R8                   Invalid parm count ?
         BM    LEAVE                   Yes.
         LTR   R9,R9                   Invalid parm count ?
         BNM   LEAVE                   Yes.
         LA    R11,12                  R11 = RC (Zero buffer length)
         ICM   R0,B'1111',0(R9)        R0 = L'Buffer
         BZ    LEAVE                   Invalid buffer length parm.
         LA    R11,16                  R11 = RC (Zero buffer address)
         LTR   R8,R8                   Zero buffer address ?
         BZ    LEAVE                   Yes.

*  Build and execute translation request
         XR    R11,R11                 R11 = RC (No errors)
         LA    R0,=C'EBCD'             We are requesting EBCDIC->ASCII.
         ST    R0,TRANPRMS             Set translation type.
         ST    R9,TRANPRMS+4           Set A(L'Buffer)
         ST    R8,TRANPRMS+8           Set A(Buffer)
         LA    R1,TRANPRMS             R1 = A(Translation parm list)
         L     R15,=V(EZASOH03)        R15 = A(EZASOH03 TCP/IP routine)
         BALR  R14,R15                 Perform translation.

***********************************************************************
*                 Exit Processing                                     *
***********************************************************************
LEAVE    DS    0H
         LR    R1,R13                  R1 = A(DSA)
         LA    R0,DSALEN               R0 = L'Dynamic storage area
         L     R13,4(,R13)             Pt to caller's save area.
         FREEMAIN R,A=(1),LV=(0)       Release dynamic storage area.

         ST    R11,16(,R13)            Set to pass RC to caller in R15.
         LM    R14,R12,12(R13)         Restore caller's regs.
         BR    R14                     Return.

***********************************************************************
*                 Dynamic Storage Area                                *
***********************************************************************
DSA      DSECT
SAVEAREA DS    18F

TRANPRMS DS    3A                      EZASOH03 translation parm list

         DS    0D
DSALEN   EQU   *-DSA
         END
//EZACIC05 EXEC ASSEM,M=EZACIC05
//ASM.SYSIN DD *
EZACIC05 TITLE 'TCP/IP for MVS 3.8 ASCII to EBCDIC Translation'

EZACIC05 CSECT
***********************************************************************
* This routine provides an interface which is compatible with the     *
* the z/OS Communications Server routine to perform an ASCII TO       *
* EBCDIC translation. Translation is accomplished by invoking EZASOH03*
* which then executes the TCPIP instruction to perform the translation*
* on the Hercules host.                                               *
*                                                                     *
* Invocation:                                                         *
*                                                                     *
*  CALL   EZACIC05,(buffer,length),VL                                 *
*          buffer - Address of the EBCDIC string to translate to ASCII*
*          length - Length of the string to be translated             *
*                                                                     *
* Change history:                                                     *
*                                                                     *
* 02/28/2017 - Initial release                 Shelby Beach           *
*                                                                     *
***********************************************************************

***********************************************************************
*                 Register usage                                      *
***********************************************************************
R0       EQU   0
R1       EQU   1
R2       EQU   2
R3       EQU   3
R4       EQU   4
R5       EQU   5
R6       EQU   6
R7       EQU   7
R8       EQU   8                       A(Buffer)
R9       EQU   9                       A(L'Buffer)
R10      EQU   10                      A(EZACIC05 parameter list)
R11      EQU   11                      Intermediate return code value
R12      EQU   12                      Module base
R13      EQU   13                      A(Dynamic storage area)
R14      EQU   14
R15      EQU   15

***********************************************************************
*                 Entry Processing                                    *
***********************************************************************
         STM   R14,R12,12(R13)         Save caller's regs.
         LR    R12,R15                 R12 = Base addr
         USING EZACIC05,R12

         LR    R10,R1                  R10 = A(EZACIC05 parm list)

*  Allocate dynamic storage area
         LA    R0,DSALEN               R0 = L'Dynamic storage area
         GETMAIN R,LV=(0)              Get DSA.

         ST    R13,4(,R1)              Chain save
         ST    R1,8(,R13)              areas.
         LR    R13,R1                  R13 = A(DSA)
         USING DSA,R13

***********************************************************************
*                 Request Translation                                 *
***********************************************************************
         LM    R8,R9,0(R10)            R8 = A(Buffer); R9 = A(L'Buffer)

*  Perform required error checks
         LA    R11,8                   R11 = RC (Too many parms)
         LTR   R8,R8                   Invalid parm count ?
         BM    LEAVE                   Yes.
         LTR   R9,R9                   Invalid parm count ?
         BNM   LEAVE                   Yes.
         LA    R11,12                  R11 = RC (Zero buffer length)
         ICM   R0,B'1111',0(R9)        R0 = L'Buffer
         BZ    LEAVE                   Invalid buffer length parm.
         LA    R11,16                  R11 = RC (Zero buffer address)
         LTR   R8,R8                   Zero buffer address ?
         BZ    LEAVE                   Yes.

*  Build and execute translation request
         XR    R11,R11                 R11 = RC (No errors)
         LA    R0,=C'ASCI'             We are requesting ASCII->EBCDIC.
         ST    R0,TRANPRMS             Set translation type.
         ST    R9,TRANPRMS+4           Set A(L'Buffer)
         ST    R8,TRANPRMS+8           Set A(Buffer)
         LA    R1,TRANPRMS             R1 = A(Translation parm list)
         L     R15,=V(EZASOH03)        R15 = A(EZASOH03 TCP/IP routine)
         BALR  R14,R15                 Perform translation.

***********************************************************************
*                 Exit Processing                                     *
***********************************************************************
LEAVE    DS    0H
         LR    R1,R13                  R1 = A(DSA)
         LA    R0,DSALEN               R0 = L'Dynamic storage area
         L     R13,4(,R13)             Pt to caller's save area.
         FREEMAIN R,A=(1),LV=(0)       Release dynamic storage area.

         ST    R11,16(,R13)            Set to pass RC to caller in R15.
         LM    R14,R12,12(R13)         Restore caller's regs.
         BR    R14                     Return.

***********************************************************************
*                 Dynamic Storage Area                                *
***********************************************************************
DSA      DSECT
SAVEAREA DS    18F

TRANPRMS DS    3A                      EZASOH03 translation parm list

         DS    0D
DSALEN   EQU   *-DSA
         END
//EZASOH03 EXEC ASSEM,M=EZASOH03
//ASM.SYSIN DD *
EZASOH03 TITLE 'TCP/IP for MVS 3.8 TCPIP Instruction Interface'

EZASOH03 CSECT
***********************************************************************
* This is a major rewrite of Jason Winter's EZASOKET so that it is    *
* fully reentrant and compatible with the EZASMI macro set provided   *
* by the TCP/IP for MVS 3.8 Assembler product.                        *
*                                                                     *
* EZASOH03 utilizes the TCPIP instruction and must be run on an MVS   *
* system running under control of the Hercules emulator provided by   *
* Juergen Winkelmann's TK4-.                                          *
*                                                                     *
* The EZASMI macro provides the interface to this module.             *
*                                                                     *
*  On Entry:                                                          *
*           R1  = Pointer to EZASOH03 parameter list                  *
*           R14 = Return address                                      *
*  On Return:                                                         *
*           R15 = Return code (Always zero - actual error info is     *
*                              stored in the locations pointed to     *
*                              by the RETCODE and ERRNO parameters.)  *
*                                                                     *
* Copyright (c)2003 Jason Paul Winter, All Rights Reserved.           *
*                                                                     *
* Change history:                                                     *
*                                                                     *
* 06/25/2004 - Enhanced connect() timing       Volker Bandke          *
* 10/12/2016 - Assembler XF compatibility      Juergen Winkelmann     *
* 11/08/2016 - Add NTOP and PTON functions     Shelby Beach           *
* 11/19/2016 - Check NBYTE for SEND/RECV       Shelby Beach           *
* 11/28/2016 - Modify GETHOSTBYNAME to use     Shelby Beach           *
*              EZASOH03 storage                Shelby Beach           *
* 11/30/2016 - Rename to EZASOH03              Shelby Beach           *
* 12/23/2016 - Add gethostbyaddr and           Juergen Winkelmann     *
*              getpeername                                            *
* 12/30/2016   Fix gethostbyaddr plist         Juergen Winkelmann     *
* 01/18/2017   Rewrite for EZASOH03 compatability Shelby Beach        *
***********************************************************************

***********************************************************************
*                 Register usage                                      *
***********************************************************************
R0       EQU   0
R1       EQU   1
R2       EQU   2
R3       EQU   3
R4       EQU   4
R5       EQU   5
R6       EQU   6
R7       EQU   7
R8       EQU   8
R9       EQU   9
R10      EQU   10                      A(EZASMI parameter list)
R11      EQU   11                      CALLPC link reg
R12      EQU   12                      Module base
R13      EQU   13                      A(Dynamic storage area)
R14      EQU   14
R15      EQU   15

AF@INET  EQU   2

***********************************************************************
*                 Entry Processing                                    *
***********************************************************************
         STM   R14,R12,12(R13)         Save caller's regs.
         LR    R12,R15                 R12 = Base addr
         USING EZASOH03,R12

         LR    R10,R1                  R10 = A(EZASMI parm list)

*  Allocate dynamic storage area
         LA    R0,DSALEN               R0 = L'Dynamic storage area
         GETMAIN R,LV=(0)              Get DSA.

         ST    R13,4(,R1)              Chain save
         ST    R1,8(,R13)              areas.
         LR    R13,R1                  R13 = A(DSA)
         USING DSA,R13

         L     R2,0(,R10)              R2 = A(Function name)
         L     R2,0(,R2)               R2 = Function name

         C     R2,INITAPI4             Function = INITAPI ?
         BNE   NOTINIT                 No.

***********************************************************************
*                 TYPE=INITAPI                                        *
***********************************************************************
         XR    R1,R1                   No bytes to send.
         LA    R7,1                    R7 = INITAPI function(1)
         BAL   R11,CALLPC              Perform INITAPI on PC.
         B     EZARETRN                Return to caller.

NOTINIT  DS 0H
         C     R2,TERMAPI4             Function = TERMAPI ?
         BNE   NOTTERM                 No.

***********************************************************************
*                 TYPE=TERMAPI                                        *
*                                                                     *
* The TERMAPI function exists solely to cleanup HOSTENT storage. No   *
* processing is performed by the host PC system.                      *
*                                                                     *
***********************************************************************
         STM   R2,R15,SAVEAREA+12      Save mainline regs.
         L     R11,4(,R10)             R11 = A(Task storage area)
         USING EZAWORK,R11

*  We begin by isolating the entire HOSTENT list, and then freeing
*  the HOSTENT storage areas in that list. In general, since we are
*  suppossedly done with IP processing, no new entries should be added
*  to the list during TERMAPI processing; however, we will serialize
*  access to the head of the list to ensure the integrity of the list.
*  It is possible that we may delete a HOSTENT that some other task
*  has just allocated, but this is essentially a user error.
LISTMOD  DS    0H
         L     R3,HEHEAD               R3 = Head of HOSTENT list
         XR    R1,R1                   We will replace with null list.
         CS    R3,R1,HEHEAD            List still the same ?
         BNZ   LISTMOD                 No, try again.

*  The list as seen by the caller is now empty. Now we will free
*  the storage for all the HOSTENT entries at TERMAPI time.
         DO WHILE=(LTR,R3,R3,NZ)       Continue while not end of list.
           LR    R1,R3                 R1 = A(HOSTENT to free)
           L     R3,0(,R3)             R3 = A(Next HOSTENT entry)
           LA    R0,HSECTLEN           R0 = L'HOSTSECT storage area
           FREEMAIN R,A=(1),LV=(0)     Free HOSTENT storage.
         ENDDO

         LM    R2,R15,SAVEAREA+12      Restore mainline regs.
         B     EZARETRN                Return to caller.

NOTTERM  DS 0H
         C     R2,GETHOST4             Function = GETHOSTBYNAME ?
         BNE   NOTGETH                 No.

***********************************************************************
*                 TYPE=GETHOSTBYNAME                                  *
***********************************************************************
         L     R1,12(,R10)             R1 = A(NAMELEN parm)
         L     R1,0(,R1)               R1 = NAMELEN
         L     R5,16(,R10)             R5 = A(NAME parm)

         LA    R7,4                    R7 = GETHOSTBYNAME function(4)
         BAL   R11,CALLPC              Perform GETHOSTBYNAME on PC.

*  Obtain storage for the HOSTENT
         L     R1,24(,R10)             R1 = A(Task storage work area)
         BAL   R14,GETHOST             Get HOSTENT storage.
         LR    R5,R1                   R5 = A(HOSTENT storage)
         USING HOSTENT,R5

         L     R1,20(,R10)             R1 = A(HOSTENT parm)
         ST    R5,0(,R1)               Return A(HOSTENT) to caller.

*  Initialize HOSTENT storage                                       SLB
         XC    HOSTENT(256),HOSTENT                                 SLB
         XC    HOSTENT+256(HOSTLEN-256),HOSTENT+256                 SLB
         LA    R0,GNAME                                             JW
         ST    R0,HOSTENT                                           JW
         LA    R0,GNULL                                             SLB
         ST    R0,HOSTENT+4            A(h_aliases)                 SLB
         LA    R0,AF@INET                                           SLB
         ST    R0,HOSTENT+8            A(h_addrtype)                SLB
         LA    R0,4                                                 SLB
         ST    R0,HOSTENT+12           h_length                     SLB
         LA    R0,GADD2                                             SLB
         ST    R0,HOSTENT+16           A(h_addr_list)               SLB
         LA    R0,GADDR                                             SLB
         ST    R0,GADD2                                             SLB

         ST    R4,GADDR                Store returned addr in HOSTENT

         LTR   R15,R4                  Null addr returned ?         SLB
         BNZ   *+6                     No                           SLB
         BCTR  R15,0                   Set RC = -1                  SLB

         L     R1,8(,R10)              R1 = A(RETCODE parm)
         ST    R15,0(,R1)              Return RC value to caller.
         B     EZARETRN                Return to caller.
         DROP  R5

NOTGETH  DS 0H
         C     R2,SOCKET4              Function = SOCKET ?
         BNE   NOTSOCK                 No.

***********************************************************************
*                 TYPE=SOCKET                                         *
***********************************************************************
         XR    R1,R1                   No bytes to send.

         L     R8,12(,R10)             R8 = A(AF parm)
         L     R8,0(,R8)               R8 = AF
         SLL   R8,16                   Make room for SOCTYPE.

         L     R9,16(,R10)             R9 = A(SOCTYPE parm)
         L     R9,0(,R9)               R9 = SOCTYPE
         OR    R8,R9                   R8 = AF | SOCTYPE

         L     R9,20(,R10)             R9 = A(PROTO)
         L     R9,0(,R9)               R9 = PROTP

         LA    R7,5                    R7 = SOCKET function(5)
         BAL   R11,CALLPC              Perform SOCKET on PC.

         L     R1,8(,R10)              R1 = A(RETCODE)
         ST    R4,0(,R1)               Return socket descriptor.

         C     R4,MINUS1               SOCKET call successful ?
         BNE   EZARETRN                Yes, return.

         XR    R1,R1                   No bytes to send.

         LA    R7,3                    R7 = Get error function(3)
         BAL   R11,CALLPC              Get error code from PC.

         L     R1,16(,R10)             R1 = A(ERRNO)
         ST    R4,0(,R1)               Pass error code to caller.

         B     EZARETRN                Return to caller.

NOTSOCK  DS 0H
         C     R2,BIND4
         BNE   NOTBIND

***********************************************************************
*                 TYPE=BIND                                           *
***********************************************************************
         XR    R1,R1                   No bytes to send.

         LA    R7,6                    R7 = BIND function(6)
         L     R8,12(,R10)             R8 = A(S parm)
         LH    R9,0(,R8)               R9 = Socket descriptor
         SLL   R9,16                   Shift to combine with function.
         OR    R7,R9                   R7 = S | Function

         L     R9,16(,R10)             R9 = A(NAME)
         L     R8,4(,R9)               R8 = IP address
         L     R9,0(,R9)               R9 = Family & Port number
         SLL   R9,8
         SRL   R9,8                    Remove Length (if present).

         BAL   R11,CALLPC              Perform BIND on PC.

         L     R1,8(,R10)              R1 = A(RETCODE)
         ST    R4,0(,R1)               Return error indication.

         C     R4,MINUS1               BIND error ?
         BNE   EZARETRN                No, return.

         XR    R1,R1                   No bytes to send.

         LR    R8,R7                   R7 = S | Function
         SRL   R8,16                   R8 = Socket descriptor

         LA    R7,2                    R7 = Get error function(2)
         BAL   R11,CALLPC              Get error code from PC.

         L     R1,4(,R10)              R1 = A(ERRNO)
         ST    R4,0(,R1)               Pass error code to caller.

         B     EZARETRN                Return to caller.

NOTBIND  DS 0H
         C     R2,CONNECT4             Function = CONNECT ?
         BNE   NOTCONN                 No.

***********************************************************************
*                 TYPE=CONNECT                                        *
***********************************************************************
         LA    R7,2                    Loop 3 times (wait twice).
CONNECTL DS    0H
         XR    R1,R1                   No bytes to send.

         SLL   R7,24                   Clear 16 'left'.
         SRL   R7,16                   Clear 8  'right' = 0000xx00.
         LA    R7,7(,R7)               R7 = CONNECT function(7)
         L     R8,12(,R10)             R8 = A(S parm)
         LH    R9,0(,R8)               R9 = Socket descriptor
         SLL   R9,16                   Make room for function & count.
         OR    R7,R9                   R7 = Count | function | socket

         L     R9,16(,R10)             R9 = A(NAME parm)
         L     R8,4(,R9)               R8 = IP address
         L     R9,0(,R9)               R9 = Family & Port number
         SLL   R9,8
         SRL   R9,8                    Remove Length (if present).

         BAL   R11,CALLPC              Perfrm CONNECT on PC.

         C     R4,MINUS2               Need to WAIT ?
         BNE   CNOWAIT                 No.

         SLL   R7,16                   Isolate wait ind.
         SRL   R7,24                   R7 = Wait indicator
         LTR   R7,R7                   Long wait ?
         BZ    CWAITLNG                Yes.

         BCT   R7,SHRTWAIT             Do short wait.

         BAL   R11,TASKWAIS            Wait 1 sec if init wait failed.
         B     CONNECTL                Try again.

SHRTWAIT DS    0H
         BAL   R11,TASKWAIT            Initially only wait .08 secs.
         B     CONNECTL                Try again.

CWAITLNG DS    0H
         L     R4,MINUS1               R4 = CONNECT failed ind
         L     R1,8(,R10)              R1 = A(RETCODE parm)
         ST    R4,0(,R1)               Pass RC to caller.

         LA    R4,61                   R4 = hECONNREFUSED ERRNO
         L     R1,4(,R10)              R1 = A(ERRNO parm)
         ST    R4,0(,R1)               Pass error number to caller.

         B     EZARETRN                Return.

CNOWAIT  DS    0H
         L     R1,8(,R10)              R1 = A(RETCODE parm)
         ST    R4,0(,R1)               Pass RC to caller.

         C     R4,MINUS1               Did an error occur ?
         BNE   EZARETRN                No, return.

         XR    R1,R1                   No bytes to send.

         L     R8,12(,R10)             R8 = A(S parm)
         LH    R8,0(,R8)               R8 = Socket descriptor

         LA    R7,2                    R7 = Get error function(2)
         BAL   R11,CALLPC              Get error code from PC.

         L     R1,4(,R10)              R1 = A(ERRNO parm)
         ST    R4,0(,R1)               Pass error number to caller.
         B     EZARETRN                Return.

NOTCONN  DS 0H
         C     R2,LISTEN4              Function = LISTEN ?
         BNE   NOTLIST                 No.

***********************************************************************
*                 TYPE=LISTEN                                         *
***********************************************************************
         XR    R1,R1                   No bytes to send.

         L     R8,12(,R10)             R8 = A(S parm)
         LH    R8,0(,R8)               R8 = Socket descriptor

         L     R9,16(,R10)             R9 = A(BACKLOG parm)
         L     R9,0(,R9)               R9 = Backlog value

         LA    R7,8                    R7 = LISTEN function(8)
         BAL   R11,CALLPC              Perform LISTEN on PC.

         L     R1,8(,R10)              R1 = A(RETCODE parm)
         ST    R4,0(,R1)               Pass RC to caller.

         C     R4,MINUS1               Did error occur ?
         BNE   EZARETRN                No, return.

         XR    R1,R1                   No bytes to send.

         LA    R7,2                    R7 = Get error function(2)
         BAL   R11,CALLPC              Get error code from PC.

         L     R1,4(,R10)              R1 = A(ERRNO parm)
         ST    R4,0(,R1)               Pass error number to caller.

         B     EZARETRN                Return.

NOTLIST  DS 0H
         C     R2,ACCEPT4              Function = ACCEPT ?
         BNE   NOTACCE                 No.

***********************************************************************
*                 TYPE=ACCEPT                                         *
***********************************************************************
ACCEPTL  DS 0H
         XR    R1,R1                   No bytes to send.

         L     R8,12(,R10)             R8 = A(S parm)
         LH    R8,0(,R8)               R8 = Socket descriptor

         L     R6,16(,R10)             R6 = A(NAME parm)

         LA    R7,9                    R7 = ACCEPT function(9)
         BAL   R11,CALLPC              Perform ACCEPT on PC.

         C     R4,MINUS2               Need to wait ?
         BNE   ANOWAIT                 No.

         BAL   R11,TASKWAIT            Wait .08 secs.
         B     ACCEPTL                 Try again.

ANOWAIT  DS 0H
         L     R1,8(,R10)              R1 = A(RETCODE parm)
         ST    R4,0(,R1)               Pass RC to caller.

         C     R4,MINUS1               Did error occur ?
         BNE   EZARETRN                No, return.

         XR    R1,R1                   No bytes to send.

         LA    R7,2                    R7 = Get error function(2)
         BAL   R11,CALLPC              Get error code from PC.

         L     R1,4(,R10)              R1 = A(ERRNO parm)
         ST    R4,0(,R1)               Pass error number to caller.

         B     EZARETRN                Return.

NOTACCE  DS 0H
         C     R2,SEND4                Function = SEND ?
         BNE   NOTSEND                 No.

***********************************************************************
*                 TYPE=SEND                                           *
***********************************************************************
         L     R1,16(,R10)             R1 = A(NBYTE parm)
         L     R1,0(,R1)               R1 = Number of bytes

         C     R1,=A(1024*1024)        NBYTE > 1024K ?              SLB
         BH    NBYTEERR                Yes.                         SLB

         L     R5,20(,R10)             R5 = A(BUF parm)

         L     R8,12(,R10)             R8 = A(S parm)
         LH    R8,0(,R8)               R8 = Socket descriptor

         LA    R7,10                   R7 = SEND function(10)
         BAL   R11,CALLPC              Perform SEND on PC.

         L     R1,8(,R10)              R1 = A(RETCODE parm)
         ST    R4,0(,R1)               Pass RC to caller.

         C     R4,MINUS1               Did error occur ?
         BNE   EZARETRN                No, return.

         XR    R1,R1                   No bytes to send.

         LA    R7,2                    R7 = Get error function(2)
         BAL   R11,CALLPC              Get error code from PC.

         L     R1,4(,R10)              R1 = A(ERRNO parm)
         ST    R4,0(,R1)               Pass error number to caller.

         B     EZARETRN                Return.

NOTSEND  DS 0H
         C     R2,RECV4                Function = RECV ?
         BNE   NOTRECV                 No.

***********************************************************************
*                 TYPE=RECV                                           *
***********************************************************************
RECVL    DS 0H
         XR    R1,R1                   No bytes to send.

         L     R8,12(,R10)             R8 = A(S parm)
         LH    R8,0(,R8)               R8 = Socket descriptor

         L     R9,16(,R10)             R9 = A(NBYTE parm)
         L     R9,0(,R9)               R9 = Number of bytes

         C     R9,=A(1024*1024)        NBYTE > 1024K ?              SLB
         BH    NBYTEERR                Yes.                         SLB

         L     R6,20(,R10)             R6 = A(BUF parm)

         LA    R7,11                   R7 = RECV function(11)
         BAL   R11,CALLPC              Perform RECV on PC.

         C     R4,MINUS2               Need to wait ?
         BNE   RNOWAIT                 No.

         BAL   R11,TASKWAIT            Wait .08 secs.
         B     RECVL                   Try again.

RNOWAIT  DS 0H
         L     R1,8(,R10)              R1 = A(RETCODE parm)
         ST    R4,0(,R1)               Pass RC to caller.

         C     R4,MINUS1               Did error occur ?
         BNE   EZARETRN                No, return.

         XR    R1,R1                   No bytes to send.

         LA    R7,2                    R7 = Get error function(2)
         BAL   R11,CALLPC              Get error code from PC.

         L     R1,4(,R10)              R1 = A(ERRNO parm)
         ST    R4,0(,R1)               Pass error number to caller.

         B     EZARETRN                Return.

*  SEND/RECV NBYTE value exceeds 1024K                              SLB
NBYTEERR DS    0H                                                   SLB
         LA    R4,40                   R4 = Error code (EMSGSIZE)   SLB
         L     R1,4(,R10)              R1 = A(ERRNO parm)           SLB
         ST    R4,0(,R1)               Set error code.              SLB
         L     R4,MINUS1               R4 = Error return code       SLB
         L     R1,8(,R10)              R1 = A(RETCODE parm)         SLB
         ST    R4,0(,R1)               Set return code.             SLB
         B     EZARETRN                Return.                      SLB

NOTRECV  DS 0H
         C     R2,CLOSE4               Function = CLOSE ?
         BNE   NOTCLOS                 No.

***********************************************************************
*                 TYPE=CLOSE                                          *
***********************************************************************
         XR    R1,R1                   No bytes to send.

         L     R8,12(,R10)             R8 = A(S parm)
         LH    R8,0(,R8)               R8 = Socket descriptor

         LA    R7,12                   R7 = CLOSE function(12)
         BAL   R11,CALLPC              Perform CLOSE on PC.

         L     R1,8(,R10)              R1 = A(RETCODE parm)
         XR    R15,R15                 Apparently always OK.
         ST    R15,0(,R1)              EZASMI expects an RC.

         B     EZARETRN                Return.

NOTCLOS  DS 0H
         C     R2,EBCDIC24             Function = EBCDIC->ASCII ?
         BNE   NOTEBCD                 No.

***********************************************************************
*                 TYPE=ETOA                                           *
***********************************************************************
         L     R1,4(,R10)              R1 = A(NBYTE parm)
         L     R1,0(,R1)               R1 = Number of bytes

         L     R5,8(,R10)              R5 = A(BUF parm)
         LR    R6,R5                   R6 = A(BUF parm)

         LA    R7,13                   R7 = ETOA function(13)
         BAL   R11,CALLPC              Perform E->A translation on PC.

         B     EZARETRN                Return.

NOTEBCD  DS 0H
         C     R2,ASCII24              Function = ASCII->EBCDIC ?
         BNE   NOTASCI                 No.

***********************************************************************
*                 TYPE=ATOE                                           *
***********************************************************************
         L     R1,4(,R10)              R1 = A(NBYTE parm)
         L     R1,0(,R1)               R1 = Number of bytes

         L     R5,8(,R10)              R5 = A(BUF parm)
         LR    R6,R5                   R6 = A(BUF parm)

         LA    R7,14                   R7 = ATOE function(14)
         BAL   R11,CALLPC              Perform A->E translation on PC.

         B     EZARETRN                Return.

NOTASCI  DS 0H
         C     R2,IOCTL4               Function = IOCTL ?
         BNE   NOTIOCT                 No.

***********************************************************************
*                 TYPE=IOCTL                                          *
***********************************************************************
         XR    R1,R1                   No bytes to send.

         LA    R7,15                   R7 = IOCTL function(15)
         L     R8,12(,R10)             R8 = A(S parm)
         LH    R8,0(,R8)               R8 = Socket descriptor
         SLL   R8,16                   Shift to combine with function.
         OR    R7,R8                   R7 = S | Function

         L     R8,16(,R10)             R8 = A(COMMAND parm)
         L     R8,0(,R8)               R8 = Command
         L     R9,20(,R10)             R9 = A(REQARG parm)
         L     R9,0(,R9)               R9 = Argument value

         BAL   R11,CALLPC              Perform IOCTL on PC.

         C     R8,PLUS1                Command = FIONBIO ?
         BNE   FIONREAD                No.

         XR    R4,R4                   FIONBIO can't produce an error.
         L     R1,8(,R10)              R1 = A(RETCODE parm)
         ST    R4,0(,R1)               Pass RC to caller.
         B     EZARETRN                Return.

FIONREAD DS    0H
         C     R4,MINUS1               IOCTL error ?
         BE    IOCTLERR                Yes.

         L     R1,24(,R10)             R1 = A(RETARG parm)
         ST    R4,0(,R1)               Pass return value to caller.

         XR    R4,R4                   R4 = No error
         L     R1,8(,R10)              R1 = A(RETCODE parm)
         ST    R4,0(,R1)               Pass RC to caller.
         B     EZARETRN                Return.

IOCTLERR DS    0H
         L     R1,8(,R10)              R1 = A(RETCODE parm)
         ST    R4,0(,R1)               Pass RC to caller.

         XR    R1,R1                   No bytes to send.

         L     R8,12(,R10)             R8 = A(S parm)
         LH    R8,0(,R8)               R8 = Socket descriptor

         LA    R7,2                    R7 = Get error function(2)
         BAL   R11,CALLPC              Get error code from PC.

         L     R1,4(,R10)              R1 = A(ERRNO parm)
         ST    R4,0(,R1)               Pass error number to caller.

         B     EZARETRN                Return.

NOTIOCT  DS 0H
         C     R2,GETSOCK4             Function = GETSOCKNAME ?
         BNE   NOTGETS                 No.

***********************************************************************
*                 TYPE=GETSOCKNAME                                    *
***********************************************************************
         XR    R1,R1                   No bytes to send.

         L     R8,12(,R10)             R8 = A(S parm)
         LH    R8,0(,R8)               R8 = Socket number

         L     R6,16(,R10)             R8 = A(NAME parm)

         LA    R7,16                   R7 = GETSOCKNAME function(16)
         BAL   R11,CALLPC              Perform GETSOCKNAME on PC.

         L     R1,8(,R10)              R1 = A(RETCODE parm)
         ST    R4,0(,R1)               Pass RC to caller.

         C     R4,MINUS1               Did error occur ?
         BNE   EZARETRN                No, return.

         XR    R1,R1                   No bytes to send.

         LA    R7,2                    R7 = Get error function(2)
         BAL   R11,CALLPC              Get error code from PC.

         L     R1,4(,R10)              R1 = A(ERRNO parm)
         ST    R4,0(,R1)               Pass error number to caller.

         B     EZARETRN                Return.

NOTGETS  DS 0H                         Function = SELECT ?
         C     R2,SELECT4
         BNE   NOTSELCT                No.                          JW

***********************************************************************
*                 TYPE=SELECT                                         *
***********************************************************************
         XR    R1,R1                   No bytes to send.
         LA    R7,17                   R7 = SELECT function(17)
         L     R8,12(,R10)             R8 = A(MAXSOC parm)
         ICM   R8,B'1111',0(R8)        R8 = Max socket number + 1
         BZ    EZARETRN                Nothing to do.
         BM    EZARETRN                Should allow sleep, but no!
         C     R8,PLUS1                MAXSOC = 1 ?
         BE    EZARETRN                Yes, return.

         LR    R9,R8                   R9 = MaxSock+1
         BCTR  R8,0                    R8 = 'Owner' socket
         SLL   R8,16                   Make room for function number.
         OR    R7,R8                   R7 = Socket | Function

         LA    R8,0                    R8 = Subcode = Init(0)
         BAL   R11,CALLPC              Perform SELECT on PC.

         ICM   R5,B'1111',20(R10)      R5 = A(RSNDMSK parm)
         BZ    NOSETRD                 Skip if null.
         LR    R1,R9                   R1 = MaxSock+1
         LA    R1,30(,R1)              R1 = MaxSock + 31 (MS+1+30)
         SRL   R1,5                    R1 = Words in struct
         SLL   R1,2                    R1 = Bytes in struct
         LA    R8,1                    R8 = Subcode = Set Read(1)
         BAL   R11,CALLPC              Perform SELECT on PC.

NOSETRD  DS    0H
         ICM   R5,B'1111',24(R10)      R5 = A(WSNDMASK parm)
         BZ    NOSETWR                 Skip if null.
         LR    R1,R9                   R1 = MaxSock+1
         LA    R1,30(,R1)              R1 = MaxSock + 31 (MS+1+30)
         SRL   R1,5                    R1 = Words in struct
         SLL   R1,2                    R1 = Bytes in struct
         LA    R8,2                    R8 = Subcode = Set Write(2)
         BAL   R11,CALLPC              Perform SELECT on PC.

NOSETWR  DS    0H
         ICM   R5,B'1111',28(R10)      R5 = A(ESNDMASK parm)
         BZ    NOSETEX                 Skip if null.
         LR    R1,R9                   R1 = MaxSock+1
         LA    R1,30(,R1)              R1 = MaxSock + 31 (MS+1+30)
         SRL   R1,5                    R1 = Words in struct
         SLL   R1,2                    R1 = Bytes in struct
         LA    R8,3                    R8 = Subcode = Set Exception(3)
         BAL   R11,CALLPC              Perform SELECT on PC.

NOSETEX  DS    0H
         ICM   R1,B'1111',16(R10)      R1 = A(TIMEOUT parm)
         BZ    SRERUN                  If null, wait forever.

         L     R8,0(,R1)               R8 = Seconds to wait
         SLL   R8,5                    R8 = Seconds * 32
         LR    R15,R8                  R15 = Seconds * 32
         SLL   R8,1                    R8 = Seconds * 64
         AR    R8,R15                  R8 = Secs * ~100(96) = #.01 secs
         SRL   R8,3                    R8 = ~#.08 sec waits

         ICM   R1,B'1111',4(R1)        R1 = ms to wait
         BZ    SRERUN                  Must be sec-to-wait or no-wait.

         SRL   R1,6                    R1 = ms/~10(8) = # .08 secs
         LTR   R1,R1                   Is it zero ?
         BNZ   SNOROUND                No, no need to round it up.
         LA    R1,1                    Ensure we wait at least .08 secs

SNOROUND DS    0H
         AR    R8,R1                   R8 = ~#.08 sec waits

SRERUN   DS    0H
         XR    R1,R1                   No bytes to send.

         SLL   R8,8                    Make room for subcode.
         O     R8,PLUS4                R8 = Wait time | Subcode(4)
         BAL   R11,CALLPC              Perform SELECT on PC.

         C     R4,MINUS2               Wait required ?
         BNE   SNOWAIT                 No.

         ICM   R1,B'1111',16(R10)      R1 = A(TIMEOUT parm)
         BZ    SDOWAIT                 If null, wait forever.

         XR    R4,R4                   Assume we were triggered.

         SRL   R8,8                    R8 = Timeout value
         LTR   R8,R8                   Time left ?
         BZ    SNOWAIT                 No.
         BCTR  R8,0                    Decr .08 secs.

SDOWAIT  DS    0H
         BAL   R11,TASKWAIT            Perform wait.
         B     SRERUN                  Try again.

SNOWAIT  DS    0H
         L     R1,8(,R10)              R1 = A(RETCODE parm)
         ST    R4,0(,R1)               Pass RC to caller.

         C     R4,MINUS1               Did error occur ?
         BNE   SNOERR                  No.

         XR    R1,R1                   No bytes to send.

         LR    R8,R7                   R7 = Socket | Function
         SRL   R8,16                   R8 = Socket descriptor

         LA    R7,2                    R7 = Get error function(2)
         BAL   R11,CALLPC              Get error code from PC.

         L     R1,4(,R10)              R1 = A(ERRNO parm)
         ST    R4,0(,R1)               Pass error number to caller.

SNOERR   DS    0H
         XR    R1,R1                   No bytes to send.
         ICM   R6,B'1111',32(R10)      R6 = A(RRETMSK parm)
         BZ    NOGETRD                 Skip if null.
         LA    R8,5                    R8 = Get Read subcode(5)
         BAL   R11,CALLPC              Perform SELECT on PC.

NOGETRD  DS    0H
         XR    R1,R1                   No bytes to send.
         ICM   R6,B'1111',36(R10)      R6 = A(WRETMSK parm)
         BZ    NOGETWR                 Skip if null.
         LA    R8,6                    R8 = Get Write subcode(6)
         BAL   R11,CALLPC              Perform SELECT on PC.

NOGETWR  DS    0H
         XR    R1,R1                   No bytes to send.
         ICM   R6,B'1111',40(R10)      R6 = A(ERETMSK parm)
         BZ    NOGETEX                 Skip if null.
         LA    R8,7                    R8 = Get Exception subcode(7)
         BAL   R11,CALLPC              Perform SELECT on PC.

NOGETEX  DS    0H
         LA    R8,8                    R8 = Cleanup subcode(8)
         BAL   R11,CALLPC              Perform SELECT on PC.

         B     EZARETRN                Return.

NOTSELCT DS 0H                                                      JW
         C     R2,GETHBA4              Function = GETHOSTBYADDR ?   JW
         BNE   NOTGETA                 No.                          JW

***********************************************************************
*                 TYPE=GETHOSTBYADDR                                  *
***********************************************************************
         L     R8,12(,R10)             R8 = A(HOSTADR parm)         JW
         L     R8,0(,R8)               R8 = IP address              JW

*  Obtain storage for the HOSTENT
         L     R1,20(,R10)             R1 = A(Task storage work area)
         BAL   R14,GETHOST             Get HOSTENT storage.
         LR    R5,R1                   R5 = A(HOSTENT storage)
         USING HOSTENT,R5

         L     R1,16(,R10)             R1 = A(HOSTENT parm)         JW
         ST    R5,0(,R1)               Return A(HOSTENT) to caller. JW

*  Initialize HOSTENT storage                                       SLB
         XC    HOSTENT(256),HOSTENT                                 SLB
         XC    HOSTENT+256(HOSTLEN-256),HOSTENT+256                 SLB
         LA    R0,GNAME                                             JW
         ST    R0,HOSTENT                                           JW
         LA    R0,GNULL                                             SLB
         ST    R0,HOSTENT+4            A(h_aliases)                 SLB
         LA    R0,AF@INET                                           SLB
         ST    R0,HOSTENT+8            A(h_addrtype)                SLB
         LA    R0,4                                                 SLB
         ST    R0,HOSTENT+12           h_length                     SLB
         LA    R0,GADD2                                             SLB
         ST    R0,HOSTENT+16           A(h_addr_list)               SLB
         LA    R0,GADDR                                             SLB
         ST    R0,GADD2                                             SLB

         XR    R1,R1                   No bytes to send.
         LA    R6,GNAME                                             JW

         LA    R7,18                   R7 = GETHOSTBYADDR function(18)
         BAL   R11,CALLPC              Perform CLOSE on PC.

         LTR   R15,R4                  Zero length name returned ?  JW
         BNZ   *+6                     No.                          SLB
         BCTR  R15,0                   R15 = RC = -1                SLB

         L     R1,8(,R10)              R1 = A(RETCODE parm)         JW
         ST    R15,0(,R1)              Return RC to caller.         JW

         B     EZARETRN                Return.                      JW
         DROP  R5

NOTGETA  DS 0H                                                      JW
         C     R2,GETPEER4             Function = GETPEERNAME ?     JW
         BNE   NONTOP                  No.                          JW

***********************************************************************
*                 TYPE=GETPEERNAME                                    *
***********************************************************************
         XR    R1,R1                   No bytes to send.

         L     R8,12(,R10)             R8 = A(S parm)               JW
         LH    R8,0(,R8)               R8 = Socket descriptor       JW

         L     R6,16(,R10)             R6 = A(NAME parm)            JW

         LA    R7,19                   R7 = GETPEERNAME function(19)
         BAL   R11,CALLPC              Perform CLOSE on PC.

         L     R1,8(,R10)              R1 = A(RETCODE parm)         JW
         ST    R4,0(,R1)               Pass RC to caller.           JW

         C     R4,MINUS1               Did an error occur ?         JW
         BNE   EZARETRN                No, return.                  JW

         XR    R1,R1                   No bytes to send.            JW

         LA    R7,2                    R7 = Get error function(2)   JW
         BAL   R11,CALLPC              Get error code from PC.      JW

         L     R1,4(,R10)              R1 = A(ERRNO parm)           JW
         ST    R4,0(,R1)               Pass error number to caller. JW

         B     EZARETRN                Return.                      SLB

NONTOP   DS 0H                                                      SLB
         C     R2,NTOP4                Function = NTOP ?            SLB
         BNE   NOPTON                  No.                          SLB

***********************************************************************
*                 TYPE=NTOP                                           *
***********************************************************************

*  Convert network addr to presentation form

*  Verify AF parm value                                             SLB
         L     R1,12(,R10)             R1 = A(AF parm)              SLB
         L     R1,0(,R1)               R1 = AF value                SLB
         LA    R0,AF@INET              R0 = Only acceptable AF valueSLB
         CR    R1,R0                   Valid AF ?                   SLB
         BE    NTOP1                   Yes.                         SLB
         LA    R0,97                   R0 = Bad AF (EAFNOSUPPORT)   SLB
         B     NTOP8                   Return error to caller.      SLB

NTOP1    DS    0H                                                   SLB
         L     R1,16(,R10)             R1 = A(SRCADDR parm)         SLB
         L     R9,0(,R1)               R9 = IP network addr         SLB
         LA    R14,4                   R14 = # of IP addr bytes     SLB
         LA    R15,WORK2               R15 = Formatted addr output  SLB

*  Format IP address network bytes                                  SLB
NTOP3    DS    0H                                                   SLB
         XR    R8,R8                   Clear for next network byte. SLB
         SLDL  R8,8                    R8 = Next IP addr net byte   SLB
         CVD   R8,DWRD                 Make decimal.                SLB
         MVC   WORK1(5),=X'402021204B' Set edit mask.               SLB
         LA    R1,WORK1+3              Set R1 in case single digit. SLB
         EDMK  WORK1(4),DWRD+6         Format network byte.         SLB
         MVC   0(4,R15),0(R1)          Move in next formatted byte. SLB
         LA    R0,WORK1+5              R0 = A(End formatted seg+1)  SLB
         SR    R0,R1                   R0 = L'Formatted network byteSLB
         AR    R15,R0                  R15 = A(Next formatted byte) SLB
         BCT   R14,NTOP3               Continue with next net byte. SLB

*  Copy formatted IP address to DSTADDR location                    SLB
         BCTR  R15,0                   Backup to last dot.          SLB
         LA    R14,WORK2               R14 = Formatted addr output  SLB
         SR    R15,R14                 R15 = L' Formatted IP addr   SLB
         L     R1,24(,R10)             R1 = A(DSTLEN)               SLB
         LH    R0,0(,R1)               R0 = DSTLEN                  SLB
         STH   R15,0(,R1)              Return length to caller.     SLB
         CR    R0,R15                  Output field large enough ?  SLB
         BL    NTOP7                   No, error.                   SLB
         L     R1,20(,R10)             R1 = A(DSTADDR location)     SLB
         BCTR  R15,0                   Decr length for EX.          SLB
         EX    R15,NTOPMVC             Formatted addr to DSTADDR.   SLB

*  Indicate no errors                                               SLB
         XR    R0,R0                   R0 = Error code = Return codeSLB
         L     R1,4(,R10)              R1 = A(ERRNO parm)           SLB
         ST    R0,0(,R1)               Set ERRNO value.             SLB
         L     R1,8(,R10)              R1 = A(RETCODE parm)         SLB
         ST    R0,0(,R1)               Set RETCODE value.           SLB
         B     EZARETRN                Return.                      SLB

*  Return error info                                                SLB
NTOP7    DS    0H                                                   SLB
         LA    R0,28                   R0 = Value won't fit (ENOSPC)SLB

NTOP8    DS    0H                                                   SLB
         L     R1,4(,R10)              R1 = A(ERRNO parm)           SLB
         ST    R0,0(,R1)               Set ERRNO value.             SLB
         L     R0,MINUS1               R0 = Error RC                SLB
         L     R1,8(,R10)              R1 = A(RETCODE parm)         SLB
         ST    R0,0(,R1)               Set RETCODE value.           SLB
         B     EZARETRN                Return.                      SLB

NTOPMVC  MVC   0(*-*,R1),0(R14)                                     SLB

NOPTON   DS 0H                                                      SLB
         C     R2,PTON4                Function = PTON ?            SLB
         BNE   EZARETRN                No.                          SLB

***********************************************************************
*                 TYPE=PTON                                           *
***********************************************************************

*  Convert presentation form to network addr (PTON)

*  Verify AF parm value                                             SLB
         L     R1,12(,R10)             R1 = A(AF parm)              SLB
         L     R1,0(,R1)               R1 = AF value                SLB
         LA    R0,AF@INET              R0 = Only acceptable AF valueSLB
         CR    R1,R0                   Valid AF ?                   SLB
         BE    PTON1                   Yes.                         SLB
         LA    R0,97                   R0 = Bad AF (EAFNOSUPPORT)   SLB
         B     PTON8                   Return error to caller.      SLB

PTON1    DS    0H                                                   SLB
         L     R8,16(,R10)             R8 = A(SRCADDR parm)         SLB
         LA    R14,4                   R14 = # of char string segs  SLB
         L     R1,24(,R10)             R1 = A(SRCLEN parm)          SLB
         XR    R0,R0                                                SLB
         ICM   R0,B'0011',0(R1)        R0 = SRCLEN value            SLB
         BZ    PTON7                   Error, null string.          SLB
         XR    R9,R9                   R9 will hold converted addr. SLB

*  Obtain next address segment
PTON2    DS    0H                                                   SLB
         LTR   R0,R0                   String chars remaining ?     SLB
         BZ    PTON7                   No, error.                   SLB
         SLL   R9,8                    Shift for next segment byte. SLB
         XR    R1,R1                   R1 holds addr seg digits.    SLB

*  Get next address segment digit
PTON3    DS    0H                                                   SLB
         CLI   0(R8),C'0'              Check                        SLB
         BL    PTON7                   for                          SLB
         CLI   0(R8),C'9'              non-digit.                   SLB
         BH    PTON7                                                SLB
         IC    R1,0(,R8)               R1 = Addr digit              SLB
         BCT   R0,PTON5                Decr string length.          SLB
         B     PTON6                   End of IP addr value.        SLB
PTON5    DS    0H                                                   SLB
         LA    R8,1(,R8)               Pt to next digit.            SLB
         CLI   0(R8),C'.'              End of char string segment ? SLB
         BE    PTON5B                  Yes.                         SLB
         LTR   R0,R0                   String chars remaining ?     SLB
         BZ    PTON6                   No.                          SLB
         SLL   R1,8                    Shift for next digit.        SLB
         B     PTON3                   Continue.                    SLB
PTON5B   DS    0H                                                   SLB
         BCTR  R0,0                    Decr string length for dot.  SLB

*  Convert next address segment                                     SLB
PTON6    DS    0H                                                   SLB
         ST    R1,WORK1                R1 = Unpacked seg value      SLB
         PACK  DWRD,WORK1(4)           Pack for CVB.                SLB
         CVB   R1,DWRD                 R1 = Binary seg value        SLB
         CH    R1,=H'255'              Valid seg value ?            SLB
         BH    PTON7                   No.                          SLB
         OR    R9,R1                   Combine with prior segments  SLB
         LA    R8,1(,R8)               Pt past dot.                 SLB
         BCT   R14,PTON2               Continue with next seg.      SLB
         LTR   R0,R0                   String chars remaining ?     SLB
         BNZ   PTON7                   Yes, error.                  SLB

*  Return converted addr to caller                                  SLB
         L     R1,20(,R10)             R1 = A(DSTADDR)              SLB
         ST    R9,0(,R1)               Store at DSTADDR.            SLB

*  Indicate no errors                                               SLB
         XR    R0,R0                   R0 = Error code = Return codeSLB
         L     R1,4(,R10)              R1 = A(ERRNO parm)           SLB
         ST    R0,0(,R1)               Set ERRNO value.             SLB
         L     R1,8(,R10)              R1 = A(RETCODE parm)         SLB
         ST    R0,0(,R1)               Set RETCODE value.           SLB
         B     EZARETRN                Return.                      SLB

*  Return error info                                                SLB
PTON7    DS    0H                                                   SLB
         LA    R0,99                   R0 = Bad IP address          SLB
PTON8    DS    0H                                                   SLB
         L     R1,4(,R10)              R1 = A(ERRNO parm)           SLB
         ST    R0,0(,R1)               Set ERRNO value.             SLB
         L     R0,MINUS1               R0 = Error RC                SLB
         L     R1,8(,R10)              R1 = A(RETCODE parm)         SLB
         ST    R0,0(,R1)               Set RETCODE value.           SLB
         B     EZARETRN                Return.                      SLB

***********************************************************************
*                 Obtain HOSTENT Storage                              *
*  This routine allocates or returns previously allocated storage in  *
*  which HOSTENT data will be stored. If the currently executing task *
*  has previously required a HOSTENT entry, that same storage will be *
*  returned from this call. If not, storage will be allocated for a   *
*  new HOSTENT entry. This entry will be saved in a LIFO list for use *
*  in subsequent requests for HOSTENT storage by this task.           *
*                                                                     *
*  On Entry:                                                          *
*           R1  = A(Task storage area)                                *
*           R14 = Return address                                      *
*  On Return:                                                         *
*           R1  = A(HOSTENT storage)                                  *
***********************************************************************
GETHOST  DS 0H
         STM   R2,R15,SAVEAREA+12      Save mainline regs.
         LR    R11,R1                  R11 = A(Task storage area)
         USING EZAWORK,R11

*  Determine address of current TCB
         L     R1,16                   R1 = A(CVT)
         L     R1,0(,R1)               R1 = A(TCB words)
         L     R2,0(,R1)               R2 = A(Our TCB)

*  Scan existing HOSTENT list for prior entry allocated for our task.
*  Note that we can do this without serialization because scanning
*  does not affect, nor will it be affected, by the addition of any
*  new entry (which would not be for our task).
         LA    R3,HEHEAD               R3 = A(Head of HOSTENT list)
         USING HOSTSECT,R3

         DO WHILE=(ICM,R3,B'1111',HOSTLINK,NZ),AND,                    +
               (C,R2,NE,HOSTTCB)
         ENDDO

         IF (LTR,R3,R3,Z)              If new HOSTENT needed...
           LA    R0,HSECTLEN           R0 = L'HOSTSECT storage area
           GETMAIN R,LV=(0)            Get HOSTENT storage.
           LR    R3,R1                 R3 = A(HOSTENT storage)
           ST    R2,HOSTTCB            Set requesting task.

*  Add new HOSTENT to top of list
           L     R1,HEHEAD
RETRYLNK   DS    0H
           ST    R1,HOSTLINK           New HOSTENT -> Old 1st HOSTENT
           CS    R1,R3,HEHEAD          Try head -> new HOSTENT
           BNE   RETRYLNK              Try again if other new HOSTENT.
         ENDIF

         LA    R1,HOSTENT              R1 = A(HOSTENT)

         LM    R2,R15,SAVEAREA+12      Restore mainline regs.
         BR    R14                     Return.
         DROP  R3

***********************************************************************
*                 Exit Processing                                     *
***********************************************************************
EZARETRN DS 0H
         LR    R1,R13                  R1 = A(DSA)
         LA    R0,DSALEN               R0 = L'Dynamic storage area
         L     R13,4(,R13)             Pt to caller's save area.
         FREEMAIN R,A=(1),LV=(0)       Release dynamic storage area.

         LM    R14,R12,12(R13)         Restore caller's regs.
         BR    R14                     Return.

* CALLPC: (** REPRESENTS REQUIRED VALUE ON ENTRY)
*  R0  = 0 (Initially, but turns to > 0 after the native call.
** R1  = Byte Counter to send
*  R2  = Source/Destination of PC buffer.  32bits.
*  R3  = Direction (0 = to Host PC, 1 = from Host PC)
*  R4  = Returned Bytes/Code/Socket etc.
** R5  = Source Buffer
** R6  = Destination Buffer (If required)
** R7  = Function Code (+ Socket for 'Connect', 'ioctl' & 'select')
** R8  = Aux. Data (Socket, or Port for 'Connect'...)
** R9  = Aux. Data (Port, or IP Address for 'Connect'...)
*  R10 = <Preserved>
** R11 = Return Address
*  R12 = <Preserved Base>
*  R13 = <Preserved Save>
*  R14 = Identifier (returned & passed back for conversations.)
*  R15 = Work Variable / Return Code

CALLPC   DS 0H
         LA    R3,0              To Host PC
         XR    R0,R0             Restart = No
         DC    X'75005000'       TCPIP 0,000(,R5)
         LTR   R15,R15
         BNZ   ERROR

         LA    R3,1              From Host PC
         XR    R0,R0             Restart = No
         DC    X'75006000'       TCPIP 0,000(,R6)

ERROR    DS 0H
         BR    R11

TASKWAIT DS 0H
         STIMER WAIT,BINTVL=WAITTIME        MVS38j
*        STIMER7 WAIT,BINTVL=WAITTIME        OS/390 for MVS38j
         BR    R11

TASKWAIS DS 0H
         STIMER WAIT,BINTVL=WAITTIMS        MVS38j
*        STIMER7 WAIT,BINTVL=WAITTIMS        OS/390 for MVS38j
         BR    R11

* *******************************************************************
         LTORG
         DS    0F                                                   SLB
INITAPI4 DC    C'INIT'       function 1, func 2&3: error(sock)/error
GETHOST4 DC    C'GETH'       function 4
SOCKET4  DC    C'SOCK'       function 5
BIND4    DC    C'BIND'       function 6
CONNECT4 DC    C'CONN'       function 7
LISTEN4  DC    C'LIST'       function 8
ACCEPT4  DC    C'ACCE'       function 9
SEND4    DC    C'SEND'       function 10
RECV4    DC    C'RECV'       function 11
CLOSE4   DC    C'CLOS'       function 12
EBCDIC24 DC    C'EBCD'       function 13
ASCII24  DC    C'ASCI'       function 14
IOCTL4   DC    C'IOCT'       function 15
GETSOCK4 DC    C'GETS'       function 16
SELECT4  DC    C'SELE'       function 17
GETHBA4  DC    C'GETA'       function 18    gethostbyaddr           JW
GETPEER4 DC    C'GETP'       function 19    getpeername             JW
TERMAPI4 DC    C'TERM'       function 20    TERMAPI                 SLB
NTOP4    DC    C'NTOP'       EZASMI function
PTON4    DC    C'PTON'       EZASMI function

MINUS1   DC    F'-1'
MINUS2   DC    F'-2'
PLUS1    DC    F'1'          code for FIONBIO
PLUS4    DC    F'4'          call *select* function

WAITTIME DC    F'8'          shorter timer interval = 0.08 seconds
WAITTIMS DC    F'100'        slower  timer interval = 1.00 second

***********************************************************************
*                 Dynamic Storage Area                                *
***********************************************************************
DSA      DSECT
*  EZASOH03 does not currently call any MVS routines. This save area
*  is provided to allow standard linkage conventions with the caller
*  to be implemented. The save area beginning at offset 12 is used
*  locally to isolate internal subroutines from mainline register
*  settings required by the TCPIP instruction.
SAVEAREA DS    18F

* NTOP/PTON work areas                                              SLB
DWRD     DS    D             Packed decimal value                   SLB
WORK1    DS    XL8                                                  SLB
WORK2    DS    XL16                                                 SLB

         DS    0D
DSALEN   EQU   *-DSA

***********************************************************************
*                 EZASMI Task Storage Work Area                       *
*                                                                     *
* Provides EZASOH03 with access to the task storage work area passed  *
* as a pointer in some of the EZASMI parameter lists.                 *
*                                                                     *
***********************************************************************
EZAWORK  DSECT
HEHEAD   DS    A                       Head ptr for HOSTENT list

***********************************************************************
*                 HOSTENT Storage                                     *
***********************************************************************
HOSTSECT DSECT
HOSTLINK DC    A(0)                    Link to next HOSTENT
HOSTTCB  DC    A(0)                    A(Allocating TCB)

HOSTENT  DC    A(GNAME)      h_name                                 JW
         DC    A(GNULL)      h_aliases
         DC    A(AF@INET)    h_addrtype
         DC    F'4'          h_length
         DC    A(GADD2)      h_addr_list
GNULL    DC    F'0'          NULL
GADD2    DC    A(GADDR)      Array Ý2¨ addresses
         DC    F'0'          NULL Terminator word

GADDR    DC    F'0'          Address
GNAME    DS    CL256         Name                                   SLB
HOSTLEN  EQU   *-HOSTENT                                            SLB
         DS    0D
HSECTLEN EQU   *-HOSTLINK
         END
//LINK     EXEC PGM=IEWL,PARM='LIST,XREF'
//SYSLMOD  DD   DSN=SYS2.LINKLIB,DISP=SHR
//OBJECT   DD   DSN=&&OBJ,DISP=SHR
//SYSPRINT DD   SYSOUT=*
//SYSLIN   DD   *
 INCLUDE OBJECT(EZASOH03)
 INCLUDE OBJECT(EZACIC04)
 INCLUDE OBJECT(EZACIC05)
 ALIAS EZACIC04,EZACIC05
 NAME EZASOH03(R)
