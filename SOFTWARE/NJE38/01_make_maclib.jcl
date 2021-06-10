//NJE38MAC  JOB (TSO),
//             'Install NJE38 MACLIB',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             USER=IBMUSER,
//             PASSWORD=SYS1
//*
//*  Installs SYSGEN.NJE38.MACLIB
//*
//STEP1   EXEC PGM=PDSLOAD
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DSN=SYSGEN.NJE38.MACLIB,DISP=(NEW,CATLG),
//             VOL=SER=MVS000,
//             UNIT=3350,SPACE=(TRK,(44,14,17)),
//             DCB=(RECFM=FB,LRECL=80,BLKSIZE=19040)
//SYSUT1   DD  DATA,DLM=@@
./ ADD NAME=AUTHLIST
AUTHLIST DSECT
AUTHPTR  DS    A                   -> next AUTHLIST entry or 0
         DS    A                   Reserved
AUTHUSER DS    CL8                 Authorized userid
AUTHNODE DS    CL8                 Authorized node of above userid
AUTHSIZE EQU   *-AUTHLIST          Length of an authlist entry
./ ADD NAME=LINKTABL
LINKTABL DSECT
*
***                     LINKTABL  -  LINK TABLE ENTRY
*
*          0   +-----------------------------------------------+
*              |                   LINKID                      |
*          8   +-----------------------+-----------------------+
*              |       LDEFTNME        |     LACTTNME          |
*         10   +-----------------------+-----------------------+
*              |                  LDEFDRVR                     |
*         18   +-----------------------------------------------+
*              |                  LACTDRVR                     |
*         20   +-----------+-----------+-----------------------+
*              |  LDEFLINE | LACTLINE  |     LDRVRVAR          |
*         28   +-----+-----+-----+-----+-----+-----+-----+-----+
*              | L*1 | L*2 | L*3 | L*4 | L*5 | L*6 | L*7 | L*8 |
*         30   +-----+-----+-----+-----+-----+-----+-----+-----+
*              | L*9 |LFLAG| LBUFF     | LPENDING  |  LTAKEN   |
*         38   +-----+-----+-----------+-----------+-----------+
*              |       LPOINTER        |         LMSGQ         |
*         40   +-----------+-----------+-----------+-----------+
*              | LTRNSCNT  |  LERRCNT  |   LTOCNT  |
*         48   +-----------+-----------+-----------+-----------+
*              |                   LNKCLOCK                    |
*         50   +-----------------------------------------------+
*
*
***                     LINKTABL  -  LINK TABLE ENTRY
*
LINKID   DS    CL8                 EBCDIC LINK ID
LDEFTNME DS    CL4                 DEFAULT TASK NAME
LACTTNME DS    CL4                 ACTIVE TASK NAME
LDEFUSER DS    0CL8                DEFAULT USERID IF NO SECURITY   v130
LDEFDRVR DS    CL8                 DEFAULT DRIVER ID
LACTDRVR DS    CL8                 ACTIVE DRIVER ID
LDEFLINE DS    XL2                 DEFAULT VIRTUAL LINE ADDRESS   *XJE
LACTLINE DS    XL2                 ACTIVE VIRTUAL LINE ADDRESS    *XJE
LDRVRVAR DS    1F                  LINE DRIVER VARIABLE INFO
LDEFCLS1 DS    CL1             L*1 DEFAULT  SPOOL FILE CLS 1
LDEFCLS2 DS    CL1             L*2 DEFAULT  SPOOL FILE CLS 2
LDEFCLS3 DS    CL1             L*3 DEFAULT  SPOOL FILE CLS 3
LDEFCLS4 DS    CL1             L*4 DEFAULT  SPOOL FILE CLS 4
LACTCLS1 DS    CL1             L*5 ACTIVE   SPOOL FILE CLS 1
LACTCLS2 DS    CL1             L*6 ACTIVE   SPOOL FILE CLS 2
LACTCLS3 DS    CL1             L*7 ACTIVE   SPOOL FILE CLS 3
LACTCLS4 DS    CL1             L*8 ACTIVE   SPOOL FILE CLS 4
LTIMEZON DS    1X              L*9 2 COMP TIME ZONE DISP FROM GMT
LFLAG    DS    1X                  LINK FLAG BYTE
LACTIVE  EQU   X'80'                LINK ACTIVE
*LALERT   EQU   X'40'   ************AXS ALERT EXIT SET-not used in XJE
LAUTO    EQU   X'40'                LINK TO BE AUTOSTARTED        *XJE
LHOLD    EQU   X'20'                LINK HOLD SET
LDRAIN   EQU   X'10'                LINK DRAIN IN PROGRESS
LTRALL   EQU   X'08'                LINK TRANSACTION TRACING (ALL)
LTRERR   EQU   X'04'                LINK TRANSACTION TRACING (ERROR)
LCONNECT EQU   X'02'                Link successfully signed onHRC031DT
LHALT    EQU   X'01'                LINK TO BE FORCED INACTIVE
LBUFF    DS    1H                  Max buffer size for line       *XJE
LNEGO    DS    1H                  Negotiated actual buffer size  *XJE
LTAKEN   DS    1H                  COUNT OF TAG SLOTS IN USE
LPOINTER DS    1F                  LINK QUEUE ADDR
LMSGQ    DS    1F                  MSG QUEUE POINTER
LTRNSCNT DS    1H                  LINK TRANSACTION COUNT
LERRCNT  DS    1H                  ERROR COUNT
LTOCNT   DS    1H                  TIMEOUT COUNT
LSPARE   DS    1H                  SPARE HALF WORD
LNKCLOCK DS    8X             CLOCK COMP VALUE FOR THIS LINK   @VA03349
*
*- New fields for NJE/MVS use; below                              *XJE
*
LNEXT    DS    A                   -> next LINKTABL entry or 0
LTCBA    DS    A                   -> TCB for this link
LTRMECB  DS    F                   Link subtask termination ECB
LECB     DS    F                   ECB for main task notific'n to link
LNJEW    DS    A                   -> local work area for this link
         DS    F                   Available
LWRESWAP DS    0D                  CDS swap doubleword
LWREQIN  DS    A                   Incoming WREs Q chain anchor
LWREQCT  DS    F                   Incoming synchronization count
LINKLEN  EQU   *-LINKTABL          LENGTH OF LINK TABLE ENTRY
         SPACE
./ ADD NAME=MSGX
         MACRO
&LABEL   MSGX  &NUM,&VAR
.* REENTERABLE FORM OF MSG MACRO
         LCLA  &TOFF,&TVARS
         LCLC  &COFF
&LABEL   MVC   MSGXNUM,=AL2(&NUM)
         AIF   (N'&SYSLIST(2) EQ 0).NOVAR
&TOFF    SETA  N'&SYSLIST(2)
&COFF    SETC  '&TOFF'
.NOVAR   ANOP
         AIF   (N'&SYSLIST(2) EQ 0).NOVAR1
&TOFF    SETA  0
&TVARS   SETA  1
.MLOP    ANOP
&COFF    SETC  '&TOFF'
         MVC   MSGXVAL+&COFF.(8),&SYSLIST(2,&TVARS)
&TOFF    SETA  &TOFF+8
&TVARS   SETA  &TVARS+1
         AIF   (&TVARS LE N'&SYSLIST(2)).MLOP
.NOVAR1  ANOP
         LA    1,MSGXNUM
         LA    0,&TOFF+4
         BAL   14,MSG
         SPACE 1
         MEND
./ ADD NAME=NETSPOOL
*
* Change log:
*
* 23 Jul 20 - Add NCBPCT to return spool file percentage           v200
* 02 Jul 20 - Default userid to CSA in support of TRANSMIT/RECEOVE v200
* 21 May 20 - Add update directory entry funcation                 v120
* 04 May 20 - Show CONFIG assembly date and time on start up.      v102
*
*
NCB      DSECT                     NETSPOOL CONTROL BLOCK
NCBEYE   DS    CL4'NCB'            NCB id
NCBTKN   DS    F                   Token identifier (caller unique)
NCBFL1   DS    X                   Flag bits
NCBPRT   EQU   X'40'                PRT type data
NCBPUN   EQU   X'80'                PUN type data
NCBREQ   DS    X                   Request type
NCBOPEN  EQU   X'01'                Open NETSPOOL dataset
NCBCLOSE EQU   X'02'                Close NETSPOOL dataset
NCBPUT   EQU   X'03'                Write a logical record
NCBGET   EQU   X'04'                Read a logical record
NCBPURGE EQU   X'05'                Delete a file
NCBLOC   EQU   X'06'                Locate a file
NCBCON   EQU   X'07'                Get directory contents
NCBUDIR  EQU   X'08'                Update directory entry         v120
NCBRTNCD DS    X                   RC from VSAM macro (same as R15)
NCBERRCD DS    X                   Error code from VSAM macro
NCBMACAD DS    A                   Addr of failing VSAM macro
NCBTAG   DS    A                   Addr of associated TAG block
NCBEODAD DS    A                   Addr of End of Data routine
NCBAREAL DS    F                   Length of record area
NCBAREA  DS    A                   Addr of record area
NCBRECLN DS    AL2                 Length of record
NCBRECCT DS    AL2                 Record count
NCBPCT   DS    0AL2                Spool percentage full (NCBCON)  v200
NCBFID   DS    AL2                 File id # (avail on new file CLOSE)
NCBRESV1 DS    AL2                 Available bytes
NCBRESV2 DS    A                   Available bytes
         DS    0D                  Force doubleword boundary
NCBSZ    EQU   *-NCB               Size of NCB
*
*
NSDIR    DSECT                     NETSPOOL directory entry
NSLEN    DS    AL2(NSDIRLN)        Length of this record incl len
NSRESV1  DS    AL2                 Resv
NSBLK    DS    AL4                 Block number of file's ptr block
NSINLOC  DS    CL8                 Originating location
NSLINK   DS    CL8                 Next location for transmission
NSINTOD  DS    CL8                 Time of file origin
NSINVM   DS    CL8                 Originating virtual machine
NSRECNM  DS    1F                  Number of records in file
NSRECLN  DS    1H                  Maximum file data record length
NSINDEV  DS    1X                  Device code of originating dev
NSCLASS  DS    CL1                 File output class
NSID     DS    1H                  File number at origin location
NSCOPY   DS    1H                  Number of copies requested
NSFLAG   DS    1X                  VM/370 SFBLOK control flags
NSFLAG2  DS    1X                  VM/370 SFBLOK control flags
NSSPARE  DS    1H                  Spare
NSNAME   DS    CL12                File name
NSTYPE   DS    CL12                File type
NSDIST   DS    CL8                 File distribution code
NSTOLOC  DS    CL8                 Destination location id
NSTOVM   DS    CL8                 Destination virtual machine id
NSPRIOR  DS    1H                  Transmission priority
NSDEV    DS    2X                  Active file's virt dev addr
NSRESV2  DS    AL4                 Resv
NSDIRLN  EQU   *-NSDIR
*
NJ38CSA  DSECT                     NJE38 CSA STORAGE BLOCK
NJ38NODE DS    CL8                 Node name of this NJE38
NJ38ASCB DS    A                   ASCB address of NJE38 addr space
NJ38ECB  DS    F                   NJE38 ECB for cross memory post
NJ38SWAP DS    0D                  CDS swap doubleword
NJ38WRIN DS    A                   Incoming WREs Q chain anchor
NJ38WRCT DS    F                   Incoming synchronization count  v200
NJ38DUSR DS    CL8                 Default 'no security' userid    v200
NJ38CSAZ EQU   *-NJ38CSA           Size of CSA area
*
CMDBLOK  DSECT                     Map cmd area used by DMTXJE
CMDBLEN  DS    AL1                 CMDBLOK length
CMDBTYP  DS    AL1(0)              Type 0 = CMDBLOK request
         DS    AL1
         DS    AL1
CMDLINK  DS    CL8                 LINKID
CMDVMID  DS    CL8                 VIRTUAL MACHINE ID
CMDTEXT  DS    CL120' '            text of command
CMDBLOKL EQU   *-CMDBLOK           Size of dsect
*
STACKMSG DSECT                     Stacked message format
STKOWN   DS    A                   RQE owner
STKNEXT  DS    A                   -> next STACKMSG or zero
STKLEN   DS    AL1                 Stacked msg length
STKZERO  DS    AL1(0)              Must be 0
STKNODE  DS    CL8                 Node of receiver of this msg
STKID    DS    CL8                 userid of receiver of this msg
STKMSG   DS    CL238               Area for msg text
STKSZ    EQU   *-STACKMSG          Total size should be 264=RQESZ
*
*
*
RQE      DSECT
RQEOWN   DS    A                    ->LINKTABL entry of owner (0=free)
RQEDATA  DS    XL260                TANK or MSG data as used by DMTXJE
RQESZ    EQU   *-RQE                Size of RQE area
*
*
./ ADD NAME=NJEPARMS
         MACRO
&X       NJEPARMS
.*
.* Change log:
.*
.*
.* 04 Dec 20 - Expanded internal trace table support               v212
.* 29 Nov 20 - Use text-based configuration; alternate routes      v211
.* 29 Nov 20 - Initial creation.                                   v211
.*
*--this area mapped as INITPARM; passed to NJEDRV/NJECMX/NJESCN    v211
         DS    0D                                                  v211
INITPARM DS    0XL72                                               v220
*                  Offset  Owner   Area to be passed               v211
*                  ------ -------  --------------------------------v211
LCLNODE  DS    CL8    0   NJEINIT  Local node name                 v211
CPUID    DS    D      8   NJEINIT  CPUID of this system            v211
ANJECMX  DS    A     10   NJEINIT  -> entry of NJECMX cmd processorv211
ANJESPL  DS    A     14   NJEINIT  -> NJESPOOL interface           v211
RQENUM   DS    F     18   NJEINIT  # RQEs in stg area              v211
ARQESTG  DS    A     1C   NJEINIT  -> RQE stg area                 v211
CSABLK   DS    A     20   NJEINIT  -> CSA communication area       v211
ALINKS   DS    A     24   NJEINIT  -> LINKS  (LINKTABL anchor)     v211
AROUTES  DS    A     28   NJEINIT  -> ROUTES (RTE list anchor)     v211
AAUTHS   DS    A     2C   NJEINIT  -> AUTHS  (AUTHLIST anchor)     v211
ACMDBLOK DS    A     30   NJEINIT  -> CMDBLOK dsect (CMNDBLOK)     v211
MSGQ     DS    A     34   NJEDRV   Stacked msg Q anchor            v211
XJELINK  DS    A     38   NJEDRV   -> task's LINKTABL              v211
ATRACE   DS    A     3C   NJEINIT  -> Trace table control          v212
AREGUSER DS    A     40   NJEINIT  -> REGUSER (REGUSER anchor)     v220
RESV1    DS    F     44            Available word                  v220
*                    48            Total length                    v220
INITPRML EQU   *-INITPARM          Length of this parm list        v211
*--end of passed area                                              v211
         MEND
./ ADD NAME=NJEQUMSG
         MACRO
&X       NJEQUMSG
.*
.* Change log:
.*
.* 11 Dec 20 - Initial creation.                                   v220
.*
QUMSG    DSECT                     Queued user message
QUMNEXT  DS    A                   -> next QUMSG or 0
QUMOWNER DS    A                   -> REGUSER that owns this msg
QUMSGTXT DS    CL120               Message text
QUMSIZE  EQU   *-QUMSG             Size of dsect
         MEND
./ ADD NAME=NJERUSER
         MACRO
&X       NJERUSER
.*
.* Change log:
.*
.* 10 Dec 20 - Initial creation.                                   v220
.*
*
REGUSERB DSECT                     Registered userid block
REGNEXT  DS    A                   -> next REGUSER or 0
REGEYE   DS    CL4'REGU'           Eyecatcher
REGWRE   DS    A                   -> user's registration WRE in CSA
REGMSGQ  DS    A                   -> user's queued msgs WRE chain
REGUSRID DS    CL8                 Userid
REGSIZE  EQU   *-REGUSERB          Size of dsect
         MEND
./ ADD NAME=NJETRACE
         MACRO
&X       NJETRACE &TYPE=
.*
.* Change log:
.*
.* 10 Dec 20 - Support for registered users and message queuing    v220
.* 10 Dec 20 - Create NJETRACE macro from old in-line TRACE macro  v220
.*
         AIF   ('&TYPE' EQ 'DSECT').DSECT
.*
&X       STM   R15,R2,16(R13)          R0-R2 restored by trace rtn
         L     R2,ATRACE               -> trace table
         L     R15,TRCRTN-TRCCTL(,R2)  -> trace routine
         BALR  R14,R15                 Go get a new trace entry
         L     R15,16(,R13)            Restore R15
         MVI   0(R14),&TYPE            Move in trace type code
         MEXIT
.*
.DSECT   ANOP
TRCCTL   DSECT
TRCEYE   DS    CL8'TRACETAB'       Eyecatcher
TRCRTN   DS    A                   -> Trace routine
         DS    A                   Reserved
TRCSTRT  DS    A                   -> Start of trace table
TRCCURR  DS    A                   -> Current trace entry
TRCEND   DS    A                   -> End of trace table
         DS    A                   Reserved
TRCSZ    EQU   32                  Size of each trace entry
*
*-- TRACE TABLE TYPES
*
TRCEXCP  EQU   X'01'                    EXCP operation
TRCWAIT  EQU   X'02'                    Wait completed
TRCDYNA  EQU   X'03'                    Dynamic Allocation
TRCMSG   EQU   X'04'                    Message
TRCRCMD  EQU   X'05'                    remote command
TRCGET   EQU   X'06'                    Getmain
TRCFREE  EQU   X'07'                    Freemain
TRCOPNO  EQU   X'08'                    Open output request
TRCCLSO  EQU   X'09'                    Close output request
TRCOPNI  EQU   X'0A'                    Open input request
TRCCONT  EQU   X'0B'                    Spool contents request
TRCCLSI  EQU   X'0C'                    Close input request
TRCPURG  EQU   X'0D'                    File Purge request
TRC0E    EQU   X'0E'                   Available
TRCGLQ   EQU   X'0F'                    GLINKREQ
TRCGRQ   EQU   X'10'                    GROUTREQ
TRCALQ   EQU   X'11'                    ALERTREQ
TRCGMQM  EQU   X'12'                    GMSGREQ from MSGQ
TRCGMQR  EQU   X'13'                    GMSGREQ from RQE
TRCIWRE  EQU   X'14'                    Incoming WRE
TRCOWRE  EQU   X'15'                    Outgoing WRE
TRCGWRE  EQU   X'16'                    Getmain WRE
TRCFWRE  EQU   X'17'                    Freemain WRE
*
         MEND
./ ADD NAME=NJE
*
*        DSECTs defining NJE headers
*
*        Prefix section common to all headers
*
NJEPDSEC DSECT                     NJE header prefix
NJEPLEN  DS    AL2                 NJE header segment length
NJEPFLGS DS    XL1                 NJE header segment flags
NJEPSEQ  DS    XL1                 NJE header segment sequence
NJEPSIZE EQU   *-NJEPDSEC          NJE header prefix size
*
*        NJE job header general section
*
NJHGDSEC DSECT                     NJE job hdr general section
NJHGLEN  DS    AL2                 NJE job gen. sect. length
NJHGTYPE DS    XL1                 NJE job gen. sect. type
NJHGMOD  DS    XL1                 NJE job gen. sect. modifier
NJHGJID  DS    AL2                 NJE job gen. sect. identif.
NJHGJCLS DS    CL1                 NJE job gen. sect. class
NJHGMCLS DS    CL1                 NJE job gen. sect. msg cls
NJHGFLG1 DS    XL1                 NJE job gen. sect. flags
NJHGPRIO DS    XL1                 NJE job gen. sect. priority
NJHGORGQ DS    XL1                 NJE job gen. sect. qualifier
NJHGJCPY DS    XL1                 NJE job gen. sect. copy
NJHGLNCT DS    XL1                 NJE job gen. sect. lpp
         DS    XL1                 NJE job gen. sect. reserved
NJHGHOPS DS    AL2                 NJE job gen. sect. hop count
NJHGACCT DS    CL8                 NJE job gen. sect. acct
NJHGJNAM DS    CL8                 NJE job gen. sect. name
NJHGUSID DS    CL8                 NJE job gen. sect. userid
NJHGPASS DS    XL8                 NJE job gen. sect. password
NJHGNPAS DS    XL8                 NJE job gen. sect. new pass
NJHGETS  DS    XL8                 NJE job gen. sect. TOD time
NJHGORGN DS    CL8                 NJE job gen. sect. org node
NJHGORGR DS    CL8                 NJE job gen. sect. org user
NJHGXEQN DS    CL8                 NJE job gen. sect. exe node
NJHGXEQU DS    CL8                 NJE job gen. sect. exe user
NJHGPRTN DS    CL8                 NJE job gen. sect. prt dest
NJHGPRTR DS    CL8                 NJE job gen. sect. prt user
NJHGPUNN DS    CL8                 NJE job gen. sect. pun dest
NJHGPUNR DS    CL8                 NJE job gen. sect. pun user
NJHGFORM DS    CL8                 NJE job gen. sect. form
NJHGICRD DS    XL4                 NJE job gen. sect. inp cards
NJHGETIM DS    XL4                 NJE job gen. sect. job time
NJHGELIN DS    XL4                 NJE job gen. sect. prt lines
NJHGECRD DS    XL4                 NJE job gen. sect. pun cards
NJHGPRGN DS    CL20                NJE job gen. sect. programmr
NJHGROOM DS    CL8                 NJE job gen. sect. room no
NJHGDEPT DS    CL8                 NJE job gen. sect. dept
NJHGBLDG DS    CL8                 NJE job gen. sect. building
NJHGNREC DS    XL4                 NJE job gen. sect. rec. cnt
NJHGSIZE EQU   *-NJHGDSEC          NJE job gen. sect. size
NJHSIZE  EQU   NJEPSIZE+NJHGSIZE   NJE job header total size
*
*        NJE data set header general section
*
NDHGDSEC DSECT                     NJE data set general sect.
NDHGLEN  DS    AL2                 NJE ds gen sect. length
NDHGTYPE DS    XL1                 NJE ds gen sect. type
NDHGMOD  DS    XL1                 NJE ds gen sect. type modif
NDHGNODE DS    CL8                 NJE ds gen sect. dest node
NDHGRMT  DS    CL8                 NJE ds gen sect. dest user
NDHGPROC DS    CL8                 NJE ds gen sect. proc name
NDHGSTEP DS    CL8                 NJE ds gen sect. step type
NDHGDD   DS    CL8                 NJE ds gen sect. ddname
NDHGDSNO DS    AL2                 NJE ds gen sect. count
         DS    XL1                 Reserved
NDHGCLAS DS    CL1                 NJE ds gen sect. class
NDHGNREC DS    XL4                 NJE ds gen sect. Record cnt
NDHGFLG1 DS    XL1                 NJE ds gen sect. flags
NDHGRCFM DS    XL1                 NJE ds gen sect. record fmt
NDHGLREC DS    AL2                 NJE ds gen sect. record len
NDHGDSCT DS    XL1                 NJE ds gen sect. copy count
NDHGFCBI DS    XL1                 NJE ds gen sect. print index
NDHGLNCT DS    XL1                 NJE ds gen sect. lpp
         DS    XL1                 Reserved
NDHGFORM DS    CL8                 NJE ds gen sect. form
NDHGFCB  DS    CL8                 NJE ds gen sect. FCB
NDHGUCS  DS    CL8                 Universal char set name
NDHGXWTR DS    CL8                 Data set external writer
NDHGNAME DS    CL8                 Data set name qualifier
NDHGFLG2 DS    XL1                 Second flag byte
NDHGUCSO DS    XL1                 NJE ds gen sect. UCS options
         DS    XL2                 Reserved
NDHGPMDE DS    CL8                 NJE ds gen sect. proc mode
NDHGSIZE EQU   *-NDHGDSEC          Ds hdr general section size
*
*        NJE data set header RSCS section
*
NDHVDSEC DSECT                     Data set header RSCS sect.
NDHVLEN  DS    AL2                 Ds header RSCS sect. length
NDHVTYPE DS    AL1                 Ds header RSCS sect. type
NDHVMOD  DS    AL1                 Ds header RSCS sec modifier
NDHVFLG1 DS    AL1                 Ds header RSCS sect flags
NDHVCLAS DS    CL1                 Ds header RSCS sect class
NDHVIDEV DS    AL1                 Ds header RSCS sect dev typ
NDHVPGLE DS    AL1                 Ds header RSCS 3800 page ln
NDHVDIST DS    CL8                 Ds header RSCS dist code
NDHVFNAM DS    CL12                Ds header RSCS filename
NDHVFTYP DS    CL12                Ds header RSCS filetype
NDHVPRIO DS    AL2                 Ds header RSCS trn priority
NDHVVRSN DS    AL1                 Ds header RSCS version no
NDHVRELN DS    AL1                 Ds header RSCS release no
NDHVSIZE EQU   *-NDHVDSEC          Ds header RSCS section size
NDHSIZE  EQU   NJEPSIZE+NDHGSIZE+NDHVSIZE Total ds header size
*
*        NJE job trailer general section
*
NJTGDSEC DSECT                     Job trailer general section
NJTGLEN  DS    AL2                 Job trailer gen sect length
NJTGTYPE DS    AL1                 Job trailer gen sect type
NJTGMOD  DS    AL1                 Job trailer gen sc modifier
NJTGFLG1 DS    AL1                 Job trailer gen sect flags
NJTGXCLS DS    CL1                 Job trailer execution class
         DS    XL2                 Reserved
NJTGSTRT DS    XL8                 Job trailer job start TOD
NJTGSTOP DS    XL8                 Job trailer job stop TOD
         DS    XL4                 Reserved
NJTGALIN DS    XL4                 Job trailer print lines
NJTGACRD DS    XL4                 Job trailer card images
         DS    XL4                 Reserved
NJTGIXPR DS    XL1                 Job trailer init exec prior
NJTGAXPR DS    XL1                 Job trailer actul exe prior
NJTGIOPR DS    XL1                 Job trailer init job prior
NJTGAOPR DS    XL1                 Job trailer actual job prio
NJTGSIZE EQU   *-NJTGDSEC          Job trailer gen. sect. size
NJTSIZE  EQU   NJEPSIZE+NJTGSIZE   Job trailer total size
*
* NMR record
*
NMRDSECT DSECT
NMRFLAG  DS    XL1                 NMR flags
NMRLVPR  DS    XL1                 NMR level / priority
NMRTYPE  DS    XL1                 NMR type
NMRML    DS    XL1                 Length of contents of NMRMSG
NMRTO    DS    0XL9                Destination system
NMRTONOD DS    CL8                 NMR destination node
NMRTOQUL DS    XL1                 Destination node system identifier
NMROUT   DS    CL8                 Userid / remote id / console id
NMRFM    DS    0XL9                NMR originating system
NMRFMNOD DS    CL8                 NMR originating node
NMRFMQUL DS    XL1                 Originating node system identifier
NMRHSIZE EQU   *-NMRDSECT          Size of NMR header only
NMRECSID DS    0CL8                Message origination node
NMRMSG   DS    CL148               NMR message / command
NMRSIZE  EQU   *-NMRDSECT          NMR size including message / command
*
*        Fields in NMRFLAG
*
NMRFLAGC EQU   X'80'               NMR is a command
NMRFLAGW EQU   X'40'               NMROUT has remote workstation id
NMRFLAGT EQU   X'20'               NMROUT contains a userid
NMRFLAGU EQU   X'10'               NMROUT contains console identifier
NMRFLAGR EQU   X'08'               Console is remote-authorized only
NMRFLAGJ EQU   X'04'               Console is not job-authorized
NMRFLAGD EQU   X'02'               Console is not device-authorized
NMRFLAGS EQU   X'01'               Console is not system-authorized
*
*        Fields in NMRTYPE
*
NMRTYPE4 EQU   X'08'               Source userid embedded in NMRMSG
NMRTYPET EQU   X'04'               Timestamp is not embedded in NMRMSG
NMRTYPEF EQU   X'02'               NMR comtains a formatted command
NMRTYPED EQU   X'02'               Contains a delete operator message
*
*        SYSIN RCBs
*
RRCB1    EQU   X'98'               Stream 1 sysin records
RRCB2    EQU   X'A8'               Stream 2 sysin records
RRCB3    EQU   X'B8'               Stream 3 sysin records
RRCB4    EQU   X'C8'               Stream 4 sysin records
RRCB5    EQU   X'D8'               Stream 5 sysin records
RRCB6    EQU   X'E8'               Stream 6 sysin records
RRCB7    EQU   X'F8'               Stream 7 sysin records
*
*        SYSOUT RCBs
*
PRCB1    EQU   X'99'               Stream 1 sysout records
PRCB2    EQU   X'A9'               Stream 2 sysout records
PRCB3    EQU   X'B9'               Stream 3 sysout records
PRCB4    EQU   X'C9'               Stream 4 sysout records
PRCB5    EQU   X'D9'               Stream 5 sysout records
PRCB6    EQU   X'E9'               Stream 6 sysout records
PRCB7    EQU   X'F9'               Stream 7 sysout records
./ ADD NAME=NJEVER
         MACRO
         NJEVER
         GBLC  &VERS
&VERS    SETC  'v2.2.1'               -> Current version
         B     34(,R15)
         DC    AL1(29)
         DC    CL9'&SYSECT'
         DC    CL6'&VERS'
         DC    CL9'&SYSDATE'
         DC    CL5'&SYSTIME'
         MEND
./ ADD NAME=NJEWRE
         MACRO
&X       NJEWRE
.*
.* Change log:
.*
.* 10 Dec 20 - Support for registered users and message queuing    v220
.*
WRE      DSECT
WRENEXT  DS    A                    -> next WRE or 0
WRETYPE  DS    X                    WRE type
WRENEW   EQU   X'04'                 New file added to NETSPOOL
WRECMD   EQU   X'08'                 CMD type
WREMSG   EQU   X'0C'                 MSG type
WRESTAR  EQU   X'10'                 START type
WREREG   EQU   X'14'                 Registration request          v220
WREDREG  EQU   X'18'                 Deregistration request        v220
WREQRM   EQU   X'1C'                 Queue registered user msg     v220
WREDRM   EQU   X'20'                 Dequeue registered user msg   v220
WRECODE  DS    X                    Command code for link driver
WRETXTLN DS    X                    CMD or MSG text length
WRESP    DS    X                    Getmained subpool number       v220
WRELINK  DS    CL8                  Target link name for this WRE
WREUSER  DS    CL8                  Target user name for this WRE
WREORIG  DS    0CL8                 Originating userid of MSG      v220
WREASCB  DS    A                    Originating ASCB addr          v220
WREECB   DS    F                    Originator ECB for CM POST     v220
WRETXT   DS    CL120                Command or message text
WRESIZE  EQU   *-WRE                Size of WRE                    v220
*
*- Error codes for registered user services (POST code in WREECB)  v220
ERNOERR  EQU   0                    No errors                      v220
ERNOMSG  EQU   4                    No more messages               v220
ERSTOP   EQU   8                    STOP command issued            v220
ERINVREQ EQU   12                   Invalid request                v220
ERINACT  EQU   16                   NJE38 is not active            v220
ERPOST   EQU   20                   CM POST to NJE38 failure       v220
ERDUPUSR EQU   24                   User already registered        v220
ERUSERNF EQU   28                   Userid is not registered       v220
ERECBPST EQU   32                   User ECB was posted            v220
         MEND
./ ADD NAME=NSIO
         MACRO                                                          MAC00010
&L       NSIO  &TYPE=,                                                 XMAC00020
               &NCB=NCB,                                               XMAC00030
               &TAG=,                                                  XMAC00040
               &EODAD=,                                                XMAC00050
               &AREALEN=,                                              XMAC00060
               &AREA=,                                                 XMAC00070
               &RECLEN=,                                           v210XMAC00080
               &ENTRY=                                             v210 MAC00080
.*
.* Change log:
.*
.* 10 AUG 20 - Add alternate entry point via ENTRY=                v210
.* 21 May 20 - Add update directory entry functionality            v120
.*
.*                                                                      MAC00100
         LCLA  &OFFREQ                                                  MAC00110
         LCLA  &OFFTAG                                                  MAC00120
         LCLA  &OFFEOD                                                  MAC00130
         LCLA  &OFFARL                                                  MAC00140
         LCLA  &OFFARA                                                  MAC00150
         LCLA  &OFFRCL                                                  MAC00160
         LCLA  &NSIZE                                                   MAC00180
         LCLA  &REQ                                                     MAC00190
         LCLC  &W                                                       MAC00200
.*                                                                      MAC00210
.* Offsets within NCB block                                             MAC00220
&OFFREQ  SETA  9                       Offset of NCBREQ                 MAC00230
&OFFTAG  SETA  16                      Offset of NCBTAG                 MAC00240
&OFFEOD  SETA  20                      Offset of NCBEODAD               MAC00250
&OFFARL  SETA  24                      Offset of NCBAREAL               MAC00260
&OFFARA  SETA  28                      Offset of NCBAREA                MAC00270
&OFFRCL  SETA  32                      Offset of NCBRECLN               MAC00280
*                                                                       MAC00300
.* Assembled size of NCB DSECT                                          MAC00310
&NSIZE   SETA  48                      Size of an NCB                   MAC00320
.*                                                                      MAC00330
         AIF   (T'&NCB NE 'O').NCB1                                     MAC00340
         MNOTE 8,'NCB= PARAMETER REQUIRED'                              MAC00350
         AGO   .TYPE                                                    MAC00360
.*                                                                      MAC00370
.NCB1    ANOP                                                           MAC00380
         AIF   ('&NCB'(1,1) EQ '(').NCB1R                               MAC00390
&L       LA    1,&NCB                  -> NCB                           MAC00400
         AGO   .TYPE                                                    MAC00410
.NCB1R   ANOP                                                           MAC00420
&W       SETC  '&NCB'(2,K'&NCB-2)                                       MAC00430
&L       LR    1,&W                    -> NCB                           MAC00440
.*                                                                      MAC00450
.ISTYPE  ANOP                                                           MAC00460
         AIF   (T'&TYPE NE 'O').TYPE                                    MAC00470
         MNOTE 8,'TYPE= PARAMETER REQUIRED'                             MAC00480
         MEXIT                                                          MAC00490
.*                                                                      MAC00500
.TYPE    ANOP                                                           MAC00510
         AIF   ('&TYPE' EQ 'OPEN').OPEN                                 MAC00520
         AIF   ('&TYPE' EQ 'CLOSE').CLOSE                               MAC00530
         AIF   ('&TYPE' EQ 'PUT').PUT                                   MAC00540
         AIF   ('&TYPE' EQ 'GET').GET                                   MAC00550
         AIF   ('&TYPE' EQ 'PURGE').PURGE                               MAC00560
         AIF   ('&TYPE' EQ 'FIND').FIND                                 MAC00570
         AIF   ('&TYPE' EQ 'CONTENTS').CONTENT                          MAC00580
         AIF   ('&TYPE' EQ 'UDIR').UDIR                            v120 MAC00570
         MNOTE 8,'TYPE=&TYPE IS NOT A VALID FUNCTION TYPE'              MAC00590
         MEXIT                                                          MAC00600
.*                                                                      MAC00610
.OPEN    ANOP                                                           MAC00620
&REQ     SETA  1                                                        MAC00630
         XC    0(&NSIZE,1),0(1)        Initialize NCB                   MAC00640
         MVC   0(4,1),=CL4'NCB'        Set NCB identifier               MAC00650
         AGO   .SETREQ                                                  MAC00660
.*                                                                      MAC00670
.CLOSE   ANOP                                                           MAC00680
&REQ     SETA  2                                                        MAC00690
         AGO   .SETREQ                                                  MAC00700
.*                                                                      MAC00710
.PUT     ANOP                                                           MAC00720
&REQ     SETA  3                                                        MAC00730
         AGO   .SETREQ                                                  MAC00740
.*                                                                      MAC00750
.GET     ANOP                                                           MAC00760
&REQ     SETA  4                                                        MAC00770
         AGO   .SETREQ                                                  MAC00780
.*                                                                      MAC00790
.PURGE   ANOP                                                           MAC00800
&REQ     SETA  5                                                        MAC00810
         AGO   .SETREQ                                                  MAC00820
.*                                                                      MAC00830
.FIND    ANOP                                                           MAC00840
&REQ     SETA  6                                                        MAC00850
         AGO   .SETREQ                                                  MAC00860
.*                                                                      MAC00870
.CONTENT ANOP                                                           MAC00880
&REQ     SETA  7                                                        MAC00890
         AGO   .SETREQ                                             v120 MAC00860
.*                                                                      MAC00830
.UDIR    ANOP                                                      v120 MAC00840
&REQ     SETA  8                                                   v120 MAC00850
.*                                                                      MAC00900
.SETREQ  ANOP                                                           MAC00910
         MVI   &OFFREQ.(1),&REQ        Set NCBREQ type                  MAC00920
.*                                                                      MAC00930
.TAG     ANOP                                                           MAC00940
         AIF   (T'&TAG EQ 'O').EODAD                                    MAC00950
         AIF   ('&TAG'(1,1) EQ '(').TAG1R                               MAC00960
         LA    0,&TAG                  -> TAG data                      MAC00970
         ST    0,&OFFTAG.(,1)          Store in NCB                     MAC00980
         AGO   .EODAD                                                   MAC00990
.TAG1R   ANOP                                                           MAC01000
&W       SETC  '&TAG'(2,K'&TAG-2)                                       MAC01010
         ST    &W,&OFFTAG.(,1)         Store tag ptr in NCB             MAC01020
.*                                                                      MAC01030
.EODAD   ANOP                                                           MAC01040
         AIF   (T'&EODAD EQ 'O').AREALEN                                MAC01050
         AIF   ('&EODAD'(1,1) EQ '(').EODAD1R                           MAC01060
         LA    0,&EODAD                -> End of data routine           MAC01070
         ST    0,&OFFEOD.(,1)          Store in NCB                     MAC01080
         AGO   .AREALEN                                                 MAC01090
.EODAD1R ANOP                                                           MAC01100
&W       SETC  '&EODAD'(2,K'&EODAD-2)                                   MAC01110
         ST    &W,&OFFEOD.(,1)         Set EODAD address in NCB         MAC01120
.*                                                                      MAC01130
.AREALEN ANOP                                                           MAC01140
         AIF   (T'&AREALEN EQ 'O').AREA                                 MAC01150
         AIF   ('&AREALEN'(1,1) EQ '(').AREAL1R                         MAC01160
         MVC   &OFFARL.(4,1),=A(&AREALEN) Set area length value in NCB  MAC01170
         AGO   .AREA                                                    MAC01180
.AREAL1R ANOP                                                           MAC01190
&W       SETC  '&AREALEN'(2,K'&AREALEN-2)                               MAC01200
         ST    &W,&OFFARL.(,1)         Set area length in NCB           MAC01210
.*                                                                      MAC01220
.AREA    ANOP                                                           MAC01230
         AIF   (T'&AREA EQ 'O').RECLEN                                  MAC01240
         AIF   ('&AREA'(1,1) EQ '(').AREA1R                             MAC01250
         LA    0,&AREA                 -> Record buffer area            MAC01260
         ST    0,&OFFARA.(,1)          Store in NCB                     MAC01270
         AGO   .RECLEN                                                  MAC01280
.AREA1R  ANOP                                                           MAC01290
&W       SETC  '&AREA'(2,K'&AREA-2)                                     MAC01300
         ST    &W,&OFFARA.(,1)         Set area address in NCB          MAC01310
.*                                                                      MAC01320
.RECLEN  ANOP                                                           MAC01330
         AIF   (T'&RECLEN EQ 'O').ENTRY                            v210 MAC01340
         AIF   ('&RECLEN'(1,1) EQ '(').REC1R                            MAC01350
         MVC   &OFFRCL.(2,1),=Y(&RECLEN) Set record length in NCB       MAC01360
         AGO   .ENTRY                                              v210 MAC01370
.REC1R   ANOP                                                           MAC01380
&W       SETC  '&RECLEN'(2,K'&RECLEN-2)                                 MAC01390
         STH   &W,&OFFRCL.(,1)         Set record length in NCB         MAC01400
.*                                                                      MAC01500
.ENTRY   ANOP                                                           MAC01510
         AIF   (T'&ENTRY EQ 'O').VCON                              v210
         AIF   ('&ENTRY'(1,1) EQ '(').ENT1R                        v210 MAC01350
         L     15,&ENTRY               Load NJESPOOL entry addr    v210
         AGO   .LAUNCH                                             v210
.*                                                                      MAC01500
.ENT1R   ANOP                                                      v210 MAC01510
&W       SETC  '&ENTRY'(2,K'&ENTRY-2)                              v210 MAC01390
         AIF   ('&W' EQ '15').LAUNCH                               v210 MAC01350
         LR    15,&W                   Entry addr to R15           v210 MAC01400
         AGO   .LAUNCH                                             v210
.*
.VCON    ANOP                                                      v210
         L     15,=V(NJESPOOL)
.*
.LAUNCH  ANOP                                                      v210
         BALR  14,15
.*
.MEND    ANOP                                                      v210 MAC01510
         MEND                                                           MAC01520
./ ADD NAME=REGEQU
         MACRO                                                          REG00010
&X       REGEQU                                                         REG00020
*                                      DEFINES GENERAL REGISTERS        REG00030
R0       EQU   0                                                        REG00040
R1       EQU   1                                                        REG00050
R2       EQU   2                                                        REG00060
R3       EQU   3                                                        REG00070
R4       EQU   4                                                        REG00080
R5       EQU   5                                                        REG00090
R6       EQU   6                                                        REG00100
R7       EQU   7                                                        REG00110
R8       EQU   8                                                        REG00120
R9       EQU   9                                                        REG00130
R10      EQU   10                                                       REG00140
R11      EQU   11                                                       REG00150
R12      EQU   12                                                       REG00160
R13      EQU   13                                                       REG00170
R14      EQU   14                                                       REG00180
R15      EQU   15                                                       REG00190
*                                      DEFINES CONTROL REGISTERS        REG00200
C0       EQU   0                                                        REG00210
C1       EQU   1                                                        REG00220
C2       EQU   2                                                        REG00230
C3       EQU   3                                                        REG00240
C4       EQU   4                                                        REG00250
C5       EQU   5                                                        REG00260
C6       EQU   6                                                        REG00270
C7       EQU   7                                                        REG00280
C8       EQU   8                                                        REG00290
C9       EQU   9                                                        REG00300
C10      EQU   10                                                       REG00310
C11      EQU   11                                                       REG00320
C12      EQU   12                                                       REG00330
C13      EQU   13                                                       REG00340
C14      EQU   14                                                       REG00350
C15      EQU   15                                                       REG00360
*                                      DEFINES FLOATING PT REGISTERS    REG00370
F0       EQU   0                                                        REG00380
F2       EQU   2                                                        REG00390
F4       EQU   4                                                        REG00400
F6       EQU   6                                                        REG00410
         MEND                                                           REG00420
./ ADD NAME=ROUTE
         MACRO
&LABEL   ROUTE &PARM1,&PARM2,                                          X
               &TYPE=ENTRY
         GBLA  &RTETOT
         AIF   ('&TYPE' EQ 'FINAL').FINAL
         LCLC  &DEST,&NEXT
&RTETOT  SETA  &RTETOT+1
         AIF   (&RTETOT NE 1).NOT1
ROUTES   DS    0D
.NOT1    ANOP
&DEST    SETC  ' '
&NEXT    SETC  ' '
         AIF   (T'&PARM1 EQ 'O').NOID
&DEST    SETC  '&PARM1'
         AIF   (T'&PARM2 EQ 'O').NOID
&NEXT    SETC  '&PARM2'
.NOID    ANOP
&LABEL   DC    CL8'&DEST',CL8'&NEXT' DESTINATION, NEXT LINK
         MEXIT
.FINAL   ANOP
NUMRTES  EQU   &RTETOT
         AIF   (&RTETOT NE 0).MEND
ROUTES   DS    0D
.MEND    ANOP
         MEND
./ ADD NAME=RSSEQU
         PUSH  PRINT
         AIF   ('&SYSPARM' NE 'SUP').RSS01
         PRINT OFF,NOGEN
.RSS01   ANOP
*
***      RSS EQUATE SYMBOLS - MACHINE USAGE
*
          SPACE 1
*        BITS DEFINED IN STANDARD/EXTENDED PSW
EXTMODE  EQU   X'08'          BIT 12 - EXTENDED MODE
MCHEK    EQU   X'04'          BIT 13 - MACHINE CHECK ENABLED
WAIT     EQU   X'02'          BIT 14 - WAIT STATE
PROBMODE EQU   X'01'          BIT 15 - PROBLEM STATE
          SPACE 1
*        BITS DEFINED IN CHANNEL STATUS WORD - CSW
ATTN     EQU   X'80'          BIT 32 - ATTENTION
SM       EQU   X'40'          BIT 33 - STATUS MODIFIER
CUE      EQU   X'20'          BIT 34 - CONTROL UNIT END
BUSY     EQU   X'10'          BIT 35 - BUSY
CE       EQU   X'08'          BIT 36 - CHANNEL END
DE       EQU   X'04'          BIT 37 - DEVICE END
UC       EQU   X'02'          BIT 38 - UNIT CHECK
UE       EQU   X'01'          BIT 39 - UNIT EXCEPTION
*
PCI      EQU   X'80'          BIT 40 - PROGRAM-CONTROL INTERRUPT
IL       EQU   X'40'          BIT 41 - INCORRECT LENGTH
PRGC     EQU   X'20'          BIT 42 - PROGRAM CHECK
PRTC     EQU   X'10'          BIT 43 - PROTECTION CHECK
CDC      EQU   X'08'          BIT 44 - CHANNEL DATA CHECK
CCC      EQU   X'04'          BIT 45 - CHANNEL CONTROL CHECK
IFCC     EQU   X'02'          BIT 46 - INTERFACE CONTROL CHECK
CHC      EQU   X'01'          BIT 47 - CHAINING CHECK
          SPACE 1
*        BITS DEFINED IN CHANNEL COMMAND WORD - CCW
CD       EQU   X'80'          BIT 32 - CHAIN DATA
CC       EQU   X'40'          BIT 33 - COMMAND CHAIN
SILI     EQU   X'20'          BIT 34 - SUPPRESS INCORRECT LENGTH IND.
SKIP     EQU   X'10'          BIT 35 - SUPPRESS DATA TRANSFER
PCIF     EQU   X'08'          BIT 36 - PROGRAM-CONTROL INTERRUPT FETCH
IDA      EQU   X'04'          BIT 37 - INDIRECT DATA ADDRESS
          SPACE 1
*        BITS DEFINED IN SENSE BYTE 0 -- COMMON TO MOST DEVICES
CMDREJ   EQU   X'80'          BIT 0 - COMMAND REJECT
INTREQ   EQU   X'40'          BIT 1 - INTERVENTION REQUIRED
BUSOUT   EQU   X'20'          BIT 2 - BUS OUT
EQCHK    EQU   X'10'          BIT 3 - EQUIPMENT CHECK
DATACHK  EQU   X'08'          BIT 4 - DATA CHECK
         EJECT
*
***      CP370 EQUATE SYMBOLS - CP USAGE
*
*        SYMBOLIC REGISTER EQUATES
R0       EQU   0
R1       EQU   1
R2       EQU   2
R3       EQU   3
R4       EQU   4
R5       EQU   5
R6       EQU   6
R7       EQU   7              GENERAL
R8       EQU   8              REGISTER
R9       EQU   9              DEFINITIONS
R10      EQU   10
R11      EQU   11
R12      EQU   12
R13      EQU   13
R14      EQU   14
R15      EQU   15
*
Y0       EQU   0              FLOATING
Y2       EQU   2              POINT
Y4       EQU   4              REGISTER
Y6       EQU   6              DEFINITIONS
         EJECT
         POP   PRINT
         SPACE
./ ADD NAME=RTE
RTE      DSECT
ROUTPTR  DS    A                   -> next RTE entry or 0
         DS    A                   Reserved
ROUTNAME DS    CL8                 Route destination node
ROUTNEXT DS    CL8                 Link id for indirect routing
ROUTALT1 DS    CL8                 Alternate link id for indirect rt'g
ROUTALT2 DS    CL8                 Alternate link id for indirect rt'g
ROUTALT3 DS    CL8                 Alternate link id for indirect rt'g
ROUTSIZE EQU   *-RTE               Length of a routing table entry
./ ADD NAME=TAG
         PUSH  PRINT
         AIF   ('&SYSPARM' NE 'SUP').TAG01
         PRINT OFF,NOGEN
.TAG01   ANOP
TAG      DSECT
         SPACE 1
***                          TAG  -  FILE TAG
*
*          0   +-----------------------+-----------------------+
*              |      TAGNEXT          |     TAGBLOCK          |
*          8   +-----------------------+-----------------------+
*              |                   TAGINLOC                    |
*         10   +-----------------------------------------------+
*              |                   TAGLINK                     |
*         18   +-----------------------------------------------+
*              |                   TAGINTOD                    |
*         20   +-----------------------------------------------+
*              |                   TAGINVM                     |
*         28   +-----------------------+-----------+-----+-----+
*              |      TAGRECNM         | TAGRECLN  | T*1 | T*2 |
*         30   +-----------+-----------+-----------+-----+-----+
*              |   TAGID   |  TAGCOPY  | T*3 | T*4 |   SPARE   |
*         38   +-----------+-----------+-----------------------+
*              |                   TAGNAME                     |
*         40   |                       +-----------------------+
*              |                       |                       |
*         48   +-----------------------+                       |
*              |                   TAGTYPE                     |
*         50   +-----------------------------------------------+
*              |                   TAGDIST                     |
*         58   +-----------------------------------------------+
*              |                   TAGTOLOC                    |
*         60   +-----------------------------------------------+
*              |                   TAGTOVM                     |
*         68   +-----------------------------------------------+
*              | TAGPRIOR  |  TAGDEV   |
*         70   +-----------+-----------+
*
***                          TAG  -  FILE TAG
         SPACE 1
TAGNEXT  DS    1F                  ADDR OF NEXT ACTIVE QUEUE ENTRY
TAGBLOCK DS    1F                  ADDR OF ASSOCIATED I/O AREA
         SPACE
TAGINLOC DS    CL8                 ORIGINATING LOCATION
TAGLINK  DS    CL8                 NEXT LOCATION FOR TRANSMISSION
TAGINTOD DS    CL8                 TIME OF FILE ORIGIN
TAGINVM  DS    CL8                 ORIGINATING VIRTUAL MACHINE
TAGRECNM DS    1F                  NUMBER OF RECORDS IN FILE
TAGRECLN DS    1H                  MAXIMUM FILE DATA RECORD LENGTH
TAGINDEV DS    1X              T*1 DEVICE CODE OF ORIGINATING DEV
TAGCLASS DS    CL1             T*2 FILE OUTPUT CLASS
TAGID    DS    1H                  FILE NUMBER AT ORIGIN LOCATION
TAGCOPY  DS    1H                  NUMBER OF COPIES REQUESTED
TAGFLAG  DS    1X              T*3 VM/370 SFBLOK CONTROL FLAGS
TAGFLAG2 DS    1X              T*4 VM/370 SFBLOK CONTROL FLAGS
         DS    1H                  SPARE
TAGNAME  DS    CL12                FILE NAME
TAGTYPE  DS    CL12                FILE TYPE
TAGDIST  DS    CL8                 FILE DISTRIBUTION CODE
TAGTOLOC DS    CL8                 DESTINATION LOCATION ID
TAGTOVM  DS    CL8                 DESTINATION VIRTUAL MACHINE ID
TAGPRIOR DS    1H                  TRANSMISSION PRIORITY
TAGDEV   DS    2X                  ACTIVE FILE'S VIRT DEV ADDR
         SPACE
TAGUSELN EQU   *-TAGINLOC          USABLE TAG INFO LEN            *XJE
TAGLEN   EQU   *-TAGNEXT           LENGTH OF THE FILE TAG
         EJECT
         POP   PRINT
         SPACE
@@
//
