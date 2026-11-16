//TRANSMIT  JOB (TSO),
//             'Install TRANSMIT',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1)
/*JOBPARM   LINES=1000
//* Vendored from MVS-sysgen/MVP packages/TRANSMIT (MVP af59943):
//* TSO TRANSMIT/RECEIVE from NJE38. sysgen step 7 needs RECEIVE for
//* the BREXX/370 SMP install, before RAKF and MVP exist. The job card
//* has no USER/PASSWORD (no RAKF yet). Needs an IPL with CLPA after.
/*MESSAGE *********************************************
/*MESSAGE *                                           *
/*MESSAGE * TRANSMIT REQUIRES CLPA RE-IPL BEFORE USE  *
/*MESSAGE *                                           *
/*MESSAGE *********************************************
//*
//* This JCL does 4 things:
//* 1) Creates &&MACLIB and adds the NJE38 maclibs to it
//* 2) Creates &&ASMSRC and adds the needed source files
//* 3) Assembles the required and adds them to SYS2.CMDLIB
//* 4) Updates and adds TRANSMIT/RECEIVE to SYS1.UMODSRC(IKJEFTE2)
//* 5) Installs the IKJEFTE2 changes with SMP
//*
//*  ********
//*  **
//*  ** You must Re-IPL with CLPA or you will get a TSO error
//*  **
//*  ** This JCL is for MVS/CE ONLY
//*  **
//*  ********
//*
//*  Type HELP TRANSMIT or HELP RECEIVE for information how to use 
//*  these commands.
//*
//* *******************************************************************
//*
//*  Installs &&MACLIB
//*
//NJE38MAC EXEC PGM=PDSLOAD
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DSN=&&MACLIB,DISP=(NEW,CATLG),
//             VOL=SER=PUB001,
//             UNIT=3390,SPACE=(CYL,(1,1,5)),
//             DCB=(BLKSIZE=3120,RECFM=FB,LRECL=80)
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
./ ADD NAME=NJEVER
         MACRO
         NJEVER
         GBLC  &VERS
&VERS    SETC  'v2.3.0'               -> Current version
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
//*
//*  Installs &&ASMSRC
//*
//ASMSRC   EXEC PGM=PDSLOAD
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DSN=&&ASMSRC,DISP=(NEW,CATLG),
//             VOL=SER=PUB001,
//             UNIT=3390,SPACE=(CYL,(2,1,10)),
//             DCB=(BLKSIZE=6160,LRECL=80,RECFM=FB)
//SYSUT1   DD  DATA,DLM=@@
./ ADD NAME=NJESYS
*
*
*-- NJE38 - Locate NJE38 information from an ENQ resource
*
*
*   Called by NJEINIT,NJERCV,NJETRN,NJE38,NJ38XMIT,NJ38RECV
*
*
* Change log:
*
* 01 Oct 20 - Initial creation                                     v210
*
*
         GBLC  &VERS
         REGEQU
NJESYS   CSECT
         NJEVER
         STM   R14,R12,12(R13) Save regs
         LR    R12,R15
         USING NJESYS,R12
*
*-- Determine if NJE38 is already active in another address space
*
CHK000   EQU   *
         L     R2,16           Get CVT ptr
         USING CVT,R2
         LA    R2,CVTFQCB      -> ENQ QCB chain anchor
         USING QCB,R2
*
CHK010   EQU   *
         ICM   R2,15,MAJNMAJ   -> next major QCB
         BZ    CHK080          Our guy not found
         CLC   MAJNAME,NJE38Q  Look for our QNAME "NJE38"
         BNE   CHK010          Nope, go to next QCB
*
         L     R3,MAJFMIN      -> first minor QCB
         USING MIN,R3
*
CHK020   EQU   *
         LA    R4,MINNAME      -> minor name
         CLC   NJERCON,0(R4)   Does minor name match?
         BE    CHK030          Yes. NJE38 is active
         C     R3,MAJLMIN      Is this the last minor QCB?
         BE    CHK080          Yes, we're done. NJE38 is not active
         ICM   R3,15,MINNMIN   -> next minor name
         BZR   R14             Just in case no address
         B     CHK020          Spin through the minor QCBs
*
CHK030   EQU   *
         LTR   R1,R1           Store spool DSN?
         BZ    CHK040          No
         MVC   0(44,R1),12(R4) Save off NETSPOOL dsname
*
CHK040   EQU   *
         L     R1,8(,R4)       Get CSABLK ptr from QCB minor
         SR    R15,R15         RC=0, ENQ data was found
         B     CHK090
*
CHK080   EQU   *
         LA    R15,4           RC=4, no ENQ located
*
CHK090   EQU   *
         ST    R1,24(,R13)     Return R1 value
         ST    R15,16(,R13)    Return R15 RC
*
         LM    R14,R12,12(R13) Reload regs
         BR    R14             Return
*
         DS    0D
NJE38Q   DC    CL8'NJE38'
NJERCON  DC    CL8'NJEINIT'
*
         LTORG ,
*
         CVT   DSECT=YES,PREFIX=NO
         IHAQCB
*
         END
./ ADD NAME=NJESPOOL
*
*
*-- NJE38 - "Spool" Services
*
*
*   Called by NJEINIT and NJEDRV for spool-like services
*
*
*
* Change log:
*
* 23 Jul 20 - Make CONTENTS return spool full percentage           v200
* 21 Jul 20 - Only part of record buffer area was FREEMAINed       v200
* 01 Jun 20 - Exclusive control error because ENDREQ not issued    v130
*              on CONTENTS function against an empty spool.        v130
* 21 May 20 - Add update directory entry functionality             v120
* 08 May 20 - RC 12 errors need error addr in NCBMACAD             v110
*
*
* NJESPOOL - Provide a spooling mechanism "access method" for use by
*            NJE38 to hold data files queued for transmission, or to
*            hold data files that have been received via transmission
*            but not yet retrieved by the destination user.
*
* The main goal of NJESPOOL is to provide a simple way to read and
* write files by the NJE line driver without the line driver having
* to know the vagaries of i/o, record formats, directories, and so on.
* NJESPOOL does the heavier lifting and spool management under the
* covers and unknown to the line driver.
*
* The spool dataset, "NETSPOOL", is a VSAM RRDS-type dataset.  All
* blocks in the dataset are one control interval in size.  The CI size
* must be 4096, which gives a usable record size of 4089 bytes.  The
* NETSPOOL internal format is based on these sizes.
*
* NETSPOOL contains a directory which describes the data files
* present within.  There are two directories; one is the current
* directory which describes the true state of NETSPOOL, the other is
* the current-minus-1 diectory, which is the state of NETSPOOL just
* prior to the very last directory update.  When new data files are
* added or removed from NETSPOOL, the current directory is copied onto
* current-minus-1 and then the addition or deletion is applied.  This
* then becomes the current directory and the directory that was most
* recently current becomes current-minus-1.  Thus the directories
* alternate back and forth.  The first block of each directory are
* blocks 2 and 3, respectively.  If the directory size expands to
* additional blocks, they can be anywhere in the dataset, but the
* very first block of either directory is ALWAYS 2 or 3.
*
* Block #1 contains a fullword pointer that contains the block number
* of whichever directory is current.  Thus, it will contain a 2 or 3.
* Alternating directories ensures that in the event of a failure while
* adding or deleting a data file, the changes do not clobber the
* current directory.  Only when those updates complete successfully
* is the block 1 pointer to the new current directory updated.
*
*
* The format of the NETSPOOL dataset is very simple.
*  Block 1    - contains the block # of the current directory block and
*               a few other items.
*  Blocks 2-3 - contain the 1st directory block for the current
*               and current-minus-1 directories.
*  Blocks 4-7 - contains the free space bit map.
*  Blocks 8-n - data blocks available for data files or directory blks.
*
* The free space bitmap is simply a 4-block long (4089 * 4 = 16356
* bytes) string of bits that represent whether a given CI in the
* dataset is used or available.  Upon initial formatting, the blocks
* 1-7 are marked as used.  The rest of the data blocks are free until
* the last block number that is physically present in the VSAM RRDS
* dataset.  The maximum number of blocks supported by this scheme is
* 130,848.  This is 873 cylinders of 3380 DASD space, for example.
* For VSAM RRDS NETSPOOL sizes of fewer cylinders, blocks higher than
* the highest available physical block number are marked as used out
* to the end of the bitmap so they will never be allocated.
*
*
* ACCESSING NETSPOOL VIA PROGRAMMING
*
* You may access the NETSPOOL dataset via programming the same way
* that the NJE line driver and NJE38 utilities do:  via a NETSPOOL
* CONTROL BLOCK (NCB) and the NSIO macro.
*
* The NCB is a small control block that is something akin to a VSAM
* RPL.  It simply contains information about the file being read or
* written and contains pointers to the user buffer, and file
* attributes.
*
* The NSIO macro is used to open or close the NETSPOOL dataset.  It is
* also used to read or write data records, and obtain directory
* information.
*
* The NCB and the NSIO macro are used together and provide the
* functions for spool access:
*
*  NSIO  TYPE=OPEN      - Opens the NETSPOOL dataset for i/o
*             CLOSE     - Closes NETSPOOL and updates directory
*             PUT       - Writes a single record to the spool
*             GET       - Reads a single record from the spool
*             PURGE     - Deletes a data file from the spool
*             FIND      - Locates a data file by file number
*             CONTENTS  - Returns the current directory contents
*             UDIR      - Update a directory entry                 v120
*
*  All NSIO macros must specify the NCB that it is associated with.
*  The spool is not opened for "input" or for "output" in the
*  traditional sense.  Rather, the first TYPE=GET or TYPE=PUT
*  issued establishes the mode.  Once the mode is established you
*  may not change from PUT to GET, or GET to PUT, without first
*  closing the spool and re-opening.  The PURGE, FIND, and CONTENTS
*  functions do not establish any mode, and can be used any time
*  the spool is open.
*
*  If you need to open the spool file by two or more tasks or modes
*  simultaneously, use multiple NCBs.
*
*  VSAM errors are returned via the NCBRTNCD and NCBERRCD fields which
*  are analagous to the VSAM RPLRTNCD and RPLERRCD fields. If an
*  actual VSAM error occurs, NCBRTNCD will be set to 8 and the NCBERRCD
*  field contains the actual VSAM RPLERRCD value.  If NCBRTNCD is 12,
*  the error code value is an internal value used by NJESPOOL.  These
*  are:
*
*  NCBRTNCD=X'0C'  Internal NJESPOOL error
*  NCBERRCD=X'01'  Invalid function code (not open, close, get, etc).
*           X'02'  VSAM RRDS ACB is not open
*           X'03'  NETSPOOL dataset is full
*           X'04'  File # not found in directory (TYPE=FIND/PURGE)
*           X'05'  GET attempted in PUT mode, or,
*                    PUT attempted in GET mode
*           X'06'  No files in directory  (TYPE=CONTENTS)
*
* Refer to the utilities NJ38XMIT and NJ38RECV for examples using
* NCB and NSIO to access the spool.
*
         PRINT GEN                                                      NJE00030
         REGEQU                        REGISTER EQUATES                 NJE00040
*
* NETSPOOL Internal values
*
ALLOCBLK EQU   4                       Starting BLK# of allocation map
ALLOCNUM EQU   4                       Number of allocation map blocks
*
*
NJESPOOL CSECT                                                          NJE00020
         NJEVER
         STM   R14,R12,12(R13)         SAVE CMS REGS                    NJE00050
         LR    R12,R15                 BASE                             NJE00060
         USING NJESPOOL,R12            ADDRESS IT                       NJE00070
         LTR   R9,R1                   NCB ptr to R9
         BZ    EXIT16                  Exit if no ptr
         USING NCB,R9
         CLC   NCBEYE,=CL4'NCB'        Is it an NCB?
         BNE   EXIT16                  Exit if not
         XC    NCBRTNCD(2),NCBRTNCD    Clear prior error codes
         CLI   NCBREQ,NCBOPEN          Is this an OPEN function?
         BE    INIT000                 Yes, ignore token
         L     R10,NCBTKN              Get caller token
         CLC   0(4,R10),=CL4'NSPL'     Token point to NSPL work area?
         BE    INIT010                 Yes, looks good
         B     EXIT16                  Exit if token invalid
*
*
INIT000  EQU   *
         GETMAIN RU,                   Get local stg area              X
               LV=4096,                                                X
               BNDRY=PAGE
         LR    R10,R1
         ST    R10,NCBTKN              Set area addr as token
         LR    R1,R0                   Copy length
         LR    R2,R0                   Copy length
         LR    R0,R10                  -> new stg area
         SR    R15,R15                 set pad
         MVCL  R0,R14                  Clear the page
*
         USING NJEWK,R10
         MVC   NJEEYE,=CL4'NSPL'       Work area eyecatcher
         ST    R2,NJEWKLEN             Save size of area in area
*
INIT010  EQU   *
         USING NJEWK,R10
         ST    R13,NJESA+4             SAVE prv S.A. ADDR               NJE00080
         LA    R1,NJESA                -> my save area
         ST    R1,8(,R13)              Plug it into prior SA
         LR    R13,R1
*
         L     R11,=A(NJECMN)          -> common csect
         ST    R11,ANJECMN             Save addr
         USING NJECMN,R11
*
*
INIT100  EQU   *
         LA    R14,*                   -> location of error source v110
         SR    R1,R1                   Clear for IC
         IC    R1,NCBREQ               Get request type
         SLL   R1,2                    Multiply by 4 to make index
         C     R1,=A(INIT120-INIT110)  Size of branch table
         BH    ERR1201                 Exit if req type invalid
         B     INIT110(R1)             Branch to requested function
*
INIT110  B     ERR1201              00 Invalid function
         B     OPN000               01 Open NETSPOOL dataset
         B     CLS000               02 Close NETSPOOL dataset
         B     PUT000               03 Write a logical record
         B     GET000               04 Read a logical record
         B     PUR000               05 Purge a file from NETSPOOL
         B     FID000               06 Locate a file by file id
         B     CON000               07 Get a list of files in NETSPOOL
         B     UDR000               08 Update directory entry      v120
*
INIT120  EQU   *                       Must mark end of branch table
*
*                                                                       NJE00920
********************                                                    NJE00920
*                  *                                                    NJE00920
*  OPEN DATASET    *                                                    NJE00920
*  NCBREQ = X'01'  *                                                    NJE00920
*                  *                                                    NJE00920
********************                                                    NJE00920
*                                                                       NJE00920
*
*- Get storage for NETSPOOL block
*
OPN000   EQU   *
         GETMAIN RU,                   Get stg for NETSPOOL blocks     X
               LV=3*4096,                                              X
               BNDRY=PAGE
         ST    R1,BLOCK                This is the VSAM AREA
         LR    R3,R1                   R3 for now
         LA    R2,4089(,R1)            -> end of BLOCK record size
         ST    R2,BLOCKEND             Save it
         A     R1,=F'4096'             -> 2nd page
         ST    R1,PTRBUF               This is an internal rec'd buffer
         ST    R1,PTRPOS               Save also as internal write pos
         LA    R2,4084(,R1)            -> end of ptr part of PTRBUF
         ST    R2,PTRBUFEN             Save it (bytes 4084-4089 special
         A     R1,=F'4096'             -> 2nd page
         ST    R1,BUFF                 This is an internal rec'd buffer
         ST    R1,PUTPOS               Save also as internal write pos
         LA    R1,4089(,R1)            -> end of BUFF record size
         ST    R1,BUFFEND              Save it
         XC    PTRBLK,PTRBLK           Initialize
         XC    NEWBLK,NEWBLK           Initialize
         XC    PUTCNT,PUTCNT           Initialize (to be placed in TAG)
         XC    GETCNT,GETCNT           Initialize (only used for debug)
*
         GENCB BLK=ACB,                                                x
               DDNAME=NETSPOOL,                                        x
               MACRF=(OUT,KEY,DIR),                                    x
               MF=(G,MACLIST)
         STM   R0,R1,ACBL              Save len, addr
*
         LA    R4,KEY                  -> block number argument
         GENCB BLK=RPL,                                                x
               ACB=(*,ACB),                                            x
               AREA=(R3),              -> block area                   x
               AREALEN=4089,                                           x
               RECLEN=4089,                                            x
               ARG=(R4),                                               x
               OPTCD=(KEY,DIR,MVE,UPD),                                x
               MF=(G,MACLIST)
         STM   R0,R1,RPLL              Save len, addr
*
         BAL   R14,ENQ000              Get exclusive control
*
         L     R7,ACB                  -> ACB
         MVC   MACLIST(OPENL),OPEN     Move macro model
         OPEN  ((R7)),                 Open NETSPOOL                   x
               MF=(E,MACLIST)
*
         BAL   R14,CHKOC               Check open/close result
         BNZ   EXIT08                  Exit with VSAM error
         OI    NJFL1,NJF1OACB          Indic ACB open
*
*-- Get NETSPOOL directory block ptr from block 1; determine if
*-- NETSPOOL has been formatted.
*
OPN040   EQU   *
         MVC   KEY,=F'1'
         L     R7,RPL
         GET   RPL=(R7)
         BAL   R14,CHKRPL              Check RPL result
         BNZ   EXIT08                  Exit with VSAM error
*
         ENDREQ RPL=(R7)               Cancel the update request
         BAL   R14,CHKRPL              Check RPL result
         BNZ   EXIT08                  Exit with VSAM error
*
         BAL   R14,DEQ000              Release control
         B     EXIT00                  Otherwise OPEN is complete
*                                                                       NJE00920
*                                                                       NJE00920
********************                                                    NJE00920
*                  *                                                    NJE00920
*  CLOSE DATASET   *                                                    NJE00920
*  NCBREQ = X'02'  *                                                    NJE00920
*                  *                                                    NJE00920
********************                                                    NJE00920
*                                                                       NJE00920
CLS000   EQU   *
         SR    R5,R5                   Clear possible RC
         TM    NJFL1,NJF1OACB          Is ACB open?
         BZ    CLS090                  No
         BAL   R14,ENQ000              Get exclusive control
*
         TM    NJFL1,NJF1PUT           Processing PUTs against file?
         BZ    CLS050                  N, skip close related PUT funcs.
*
         CLC   NCBTAG,=A(0)            Is tag data present?
         BE    CLS050                  0, Cant write a directory
*
         TM    NJFL1,NJF1WPND          Is physical write pending?
         BZ    CLS030                  No
         NI    NJFL1,255-NJF1WPND      No physical write pending
*
         MVC   KEY,NEWBLK              Prep for update of blk to write
         L     R7,RPL                  -> RPL
         GET   RPL=(R7)                Get the block for update
         BAL   R14,CHKRPL              Deal with errors
         BNZ   EXIT08                  Exit with VSAM error
*
         L     R3,PUTPOS               -> logical record position
         LA    R3,2(,R3)               Account for FFFF EOF marker
         L     R0,BLOCK                -> VSAM i/o area
         LA    R1,4089                 Size of physical block
         L     R2,BUFF                 -> buffer to write out
         SR    R3,R2                   Compute length to write out
         MVCL  R0,R2                   Move data and pad remaining
*
         PUT   RPL=(R7)                Update the physical block
         BAL   R14,CHKRPL              Deal with errors
         BNZ   EXIT08                  Exit with VSAM error
*
*
CLS030   EQU   *
         NC    PTRBLK,PTRBLK           Is ptr block write pending?
         BZ    CLS040
         MVC   KEY,PTRBLK              Prep for update of blk to write
         XC    PTRBLK,PTRBLK           Clear block number for recursion
         OI    NJFL1,NJF1DPND          Indic directory add pending
*
         L     R7,RPL                  -> RPL
         GET   RPL=(R7)                Get the block for update
         BAL   R14,CHKRPL              Deal with errors
         BNZ   EXIT08                  Exit with VSAM error
*
         L     R3,PTRPOS               -> ptr record position
         L     R0,BLOCK                -> VSAM i/o area
         LA    R1,4089                 Size of physical block
         L     R2,PTRBUF               -> buffer to write out
         SR    R3,R2                   Compute length to write out
         MVCL  R0,R2                   Move data and pad remaining
*
         PUT   RPL=(R7)                Update the physical block
         BAL   R14,CHKRPL              Deal with errors
         BNZ   EXIT08                  Exit with VSAM error
*
*
CLS040   EQU   *
         TM    NJFL1,NJF1DPND          Directory add pending?
         BZ    CLS050                  No
         NI    NJFL1,255-NJF1DPND      Remove directory add pending
*
         L     R1,NCBTAG               -> tag data
         USING TAG,R1
         MVC   TAGRECNM,PUTCNT         Save # records actually written
         DROP  R1
*
         LA    R0,DIRADD               Add directory entry function
         L     R15,=A(NJEDIR)          Call directory mgmt
         BALR  R14,R15                 File to add is in NCB
         LR    R5,R15                  Any RC to R5
*
CLS050   EQU   *
         L     R7,ACB                  -> ACB
         MVC   MACLIST(CLOSEL),CLOSE   Move close list
         CLOSE ((R7)),                 Close the ACB                   x
               MF=(E,MACLIST)
*
         NI    NJFL1,255-NJF1OACB      ACB now closed
         BAL   R14,DEQ000              Release control
*
CLS090   EQU   *
         L     R1,BLOCK                -> NETSPOOL record areas
         FREEMAIN RU,LV=3*4096,A=(1)   Release it                  v200
*
         LM    R0,R1,RPLL
         FREEMAIN RU,LV=(0),A=(1)
*
         LM    R0,R1,ACBL
         FREEMAIN RU,LV=(0),A=(1)
*
         XC    NCBTKN,NCBTKN           Clear token
         B     QUIT000                 Exit with RC in R5
*                                                                       NJE00920
*                                                                       NJE00920
********************                                                    NJE00920
*                  *   Write a logical record (not a physical block)    NJE00920
*  PUT             *                                                    NJE00920
*  NCBREQ = X'03'  *   No ENQ is required when writing the physical     NJE00920
*                  *   blocks as these blocks are allocated exclusively NJE00920
********************   to the calling task.                             NJE00920
*                                                                       NJE00920
PUT000   EQU   *
         LA    R14,*                   -> location of error source v110
         TM    NJFL1,NJF1OACB          Is ACB open?
         BZ    ERR1202                 No
         TM    NJFL1,NJF1GET           Processing GETs against file?
         BO    ERR1205                 Yes, cant do PUT now
         OI    NJFL1,NJF1PUT           Indicate PUT in progress
*
         NC    PTRBLK,PTRBLK           Do we have a ptr block?
         BNZ   PUT020                  Yes
         BAL   R14,GETBLK              Allocate a new physical block
         BNZ   EXIT08                  Exit with VSAM error
         LTR   R0,R0                   Is there a block number?
         BZ    ERR1203                 NETSPOOL dataset full
         ST    R0,PTRBLK               Save block number of ptr blk
         ST    R0,INITBLK              Save first block # used in PUT
         L     R0,PTRBUF               -> ptr block area
         LA    R1,4089                 Size of physical block
         LR    R3,R1                   Compute length to write out
         MVCL  R0,R2                   Clear the ptr block
         MVC   PTRPOS,PTRBUF           Set write position in block
*
         BAL   R14,GETBLK              Allocate a new physical block
         BNZ   EXIT08                  Exit with VSAM error
         LTR   R0,R0                   Is there a block number?
         BZ    ERR1203                 NETSPOOL dataset full
         ST    R0,NEWBLK               Save allocated blk #
         MVC   PUTPOS,BUFF             Set write position in block
         L     R1,PTRPOS               Get current ptr block position
         ST    R0,0(,R1)               Save new blk# in ptr block
         LA    R1,4(,R1)               Next ptr block slot
         ST    R1,PTRPOS               Update position
*
PUT020   EQU   *
         L     R3,PUTPOS               Get current position
         L     R1,BUFFEND              -> end of buffer
         SR    R1,R3                   Determine remaining space in blk
         LH    R4,NCBRECLN             Get size of record to write
         LA    R2,2+2(,R4)             Add in overhead
*                                       +2 for length halfword
*                                       +2 for next block marker
         CR    R1,R2                   Is there room to add record?
         BL    PUT100                  No, better get another block
*
         L     R15,NCBAREA              -> to logical record
         BCT   R4,*+10                 Adjust len for execute
PUTREC   MVC   2(0,R3),0(R15)
         EX    R4,PUTREC               Move record to block
         LA    R4,1+2(,R4)             Get record len + overhead
*                                       +1 to get back true length
*                                       +2 for length halfword itself
         STCM  R4,3,0(R3)              Store the length
*
         TM    NCBFL1,NCBPUN           Is this PUN type data?
         BO    PUT050                  Y, no special action
         TM    2(R3),X'03'             Is carriage ctl an immediate?
         BO    PUT060                  Y, Don't count these records
*
PUT050   EQU   *
         L     R1,PUTCNT               Get count of records written
         LA    R1,1(,R1)               Bump it
         ST    R1,PUTCNT               Update count
*
PUT060   EQU   *
         AR    R3,R4                   Compute next avail byte in blk
         MVC   0(2,R3),=X'FFFF'        Set current EOF marker in case
*                                       we write no more records
         ST    R3,PUTPOS               Save write position for next
*                                       record; would overwrite the
*                                       FFFF marker on next PUT.
         OI    NJFL1,NJF1WPND          Indicate physical write req'd
         B     EXIT00
*
PUT100   EQU   *
         L     R5,NEWBLK               Get current blk # we need to wrt
         BAL   R14,GETBLK              Allocate a new physical block
         BNZ   EXIT08                  Exit with VSAM error
         LTR   R0,R0                   Is there a block number?
         BZ    ERR1203                 NETSPOOL dataset full
         ST    R0,NEWBLK               Save newly allocated blk #
         MVC   0(2,R3),=X'FFFE'        Insert ptr indic for next blk
         LA    R3,2(,R3)               -> next write position
*
         ST    R5,KEY                  Prep for update of blk to write
         L     R7,RPL                  -> RPL
         GET   RPL=(R7)                Get the block for update
         BAL   R14,CHKRPL              Deal with errors
         BNZ   EXIT08                  Exit with VSAM error
*
         L     R0,BLOCK                -> VSAM i/o area
         LA    R1,4089                 Size of physical block
         L     R2,BUFF                 -> buffer to write out
         SR    R3,R2                   Compute length to write out
         MVCL  R0,R2                   Move data and pad remaining
*
         PUT   RPL=(R7)                Update the physical block
         BAL   R14,CHKRPL              Deal with errors
         BNZ   EXIT08                  Exit with VSAM error
*
         MVC   PUTPOS,BUFF             Reset write position in new blk
         NI    NJFL1,255-NJF1WPND      No physical write pending
*
*-- Now ensure newly allocated block is also pointed to by ptr block
*
         L     R3,PTRPOS               Get current ptr block position
         MVC   0(4,R3),NEWBLK          Save new blk# in ptr block
         LA    R3,4(,R3)               Next ptr block slot
         C     R3,PTRBUFEN             Is ptr block full?
         BNL   PUT200                  Yes
         ST    R3,PTRPOS               Update position
         B     PUT020                  Now retry to add next logical
*
*-- Here if we need another ptr block  (chain them together)
*
PUT200   EQU   *
         L     R5,PTRBLK               Get current blk # we need to wrt
         BAL   R14,GETBLK              Allocate a new phys ptr block
         BNZ   EXIT08                  Exit with VSAM error
         LTR   R0,R0                   Is there a block number?
         BZ    ERR1203                 NETSPOOL dataset full
         ST    R0,PTRBLK               Save newly allocated blk #
         ST    R0,0(,R3)               Insert ptr to next ptr blk in
*                                       full ptr block
         MVI   0(R3),X'FE'             Indic "ptr to next ptr blk" and
*                                       not ptr to a data block
         LA    R3,4(,R3)               -> next write position
*
         ST    R5,KEY                  Prep for update of blk to write
         L     R7,RPL                  -> RPL
         GET   RPL=(R7)                Get the block for update
         BAL   R14,CHKRPL              Deal with errors
         BNZ   EXIT08                  Exit with VSAM error
*
         L     R0,BLOCK                -> VSAM i/o area
         LA    R1,4089                 Size of physical block
         L     R2,PTRBUF               -> buffer to write out
         SR    R3,R2                   Compute length to write out
         MVCL  R0,R2                   Move data and pad remaining
*
         PUT   RPL=(R7)                Update the physical block
         BAL   R14,CHKRPL              Deal with errors
         BNZ   EXIT08                  Exit with VSAM error
*
         L     R0,PTRBUF               -> ptr block area
         LA    R1,4089                 Size of physical block
         LR    R3,R1                   Compute length to write out
         MVCL  R0,R2                   Clear the ptr block
         MVC   PTRPOS,PTRBUF           Reset ptr position in new blk
         B     PUT020                  Now retry to add next logical
*                                                                       NJE00200
*
*                                                                       NJE00920
********************                                                    NJE00920
*                  *   Read a logical record (not a physical block)     NJE00920
*  GET             *                                                    NJE00920
*  NCBREQ = X'04'  *   No ENQ is required when reading the physical     NJE00920
*                  *   blocks as these blocks are allocated exclusively NJE00920
********************   to the calling task.  The file id to read must   NJE00920
*                      be in NSID in the tag data pointed to by NCBTAG
*                                                                       NJE00920
GET000   EQU   *
         LA    R14,*                   -> location of error source v110
         TM    NJFL1,NJF1OACB          Is ACB open?
         BZ    ERR1202                 No
         TM    NJFL1,NJF1PUT           Processing PUTs against file?
         BO    ERR1205                 Yes, cant do GET now
         OI    NJFL1,NJF1GET           Indicate GET in progress
*
         L     R7,RPL                  -> RPL
         NC    PTRBLK,PTRBLK           Do we have a ptr block in prog?
         BNZ   GET060                  Yes, read next logical rec
*
         LA    R0,DIRLOC               Locate file function
         L     R15,=A(NJEDIR)          Call directory mgmt
         BALR  R14,R15                 File id is in tag field TAGID
*
         LTR   R15,R15                 Was file found?
         BZ    GET010                  Yes
         C     R15,=F'12'              Errors processing directory?
         BL    EXIT08                  Exit here if 4 or 8=VSAM errors
         B     EXIT12                  All others Exit12
*
GET010   EQU   *
         MODCB RPL=(R7),                                               x
               OPTCD=(KEY,DIR,MVE,NUP), No update needed on GETs       x
               MF=(G,MACLIST)
*
         L     R3,NCBTAG               -> tag data
         USING TAG,R3
         MVC   GETLIM,TAGRECNM         Save off # of records in file
         DROP  R3
*
         L     R3,INITBLK              Get 1st block # of file
*
GET020   EQU   *                    ** Get a ptr block
         ST    R3,KEY                  Set block retrieval key
         GET   RPL=(R7)                Get the ptr block
         BAL   R14,CHKRPL              Deal with errors
         BNZ   EXIT08                  Exit with VSAM error
*
         ST    R3,PTRBLK               Save ptr blk #
         L     R0,BLOCK                -> VSAM i/o area
         LA    R1,4089                 Size of physical block
         L     R14,PTRBUF              -> buffer containing repl dir
         LR    R15,R1                  Copy length
         MVCL  R14,R0                  Put ptr data in ptrbuf
*
         L     R4,PTRBUF               -> ptr block ptrs
         ST    R4,PTRPOS               Maintain ptr position
*
GET030   EQU   *
         C     R4,PTRBUFEN             Out of ptrs this block?
         BL    GET040                  No
*
*                                   ** Here if ptr block chains to
*                                       another ptr block
         CLI   0(R4),X'FE'             ptr to ptrblk indicator?
         BNE   GET200         EOF      No, done with ptrs
         SR    R3,R3                   Clear for IC
         ICM   R3,7,1(R4)              Get ptr to next ptr block
         ST    R3,KEY                  Set up for retrieval
         B     GET020                  Go get it
*
GET040   EQU   *
         ICM   R2,15,0(R4)             Get a data block #
         BZ    GET200         EOF      Done with ptrs
*
         ST    R2,KEY                  Set block retrieval key
         GET   RPL=(R7)                Get the ptr block
         BAL   R14,CHKRPL              Deal with errors
         BNZ   EXIT08                  Exit with VSAM error
*
         L     R5,BLOCK                -> VSAM i/o area
         ST    R5,GETPOS               Maintain read position
*
GET060   EQU   *
         L     R5,GETPOS               -> next logical record to read
         CLC   0(2,R5),=X'FFFF'        Is this end of file?
         BE    GET200                  Yes
         CLC   0(2,R5),=X'FFFE'        Skip to next ptr indication?
         BE    GET100                  Yes
*
         SR    R14,R14                 Clear for IC
         ICM   R14,3,0(R5)             Get the record length
         BCTR  R14,0                   Reduce length of length
         BCTR  R14,0                   Reduce length of length
         STH   R14,NCBRECLN            Return length to caller
*
         L     R15,NCBAREA              -> to caller's record buffer
         BCT   R14,*+10                Adjust len for execute
GETREC   MVC   0(0,R15),2(R5)
         EX    R14,GETREC              Move record to user area
         LA    R5,1+2(R14,R5)          Get record len + overhead
*                                       +1 to get back true length
*                                       +2 for length halfword itself
         ST    R5,GETPOS               Save read position
         L     R1,GETCNT               Get count of records read
         LA    R1,1(,R1)               Bump it
         ST    R1,GETCNT               Update count for debug purposes
         B     EXIT00                  Exit with record in NCBAREA
*
GET100   EQU   *
         L     R4,PTRPOS               Get ptr position
         LA    R4,4(,R4)               -> next ptr field
         ST    R4,PTRPOS               Maintain ptr position
         B     GET030                  Go process next ptr
*
GET200   EQU   *
         MVI   NCBERRCD,X'04'          Indicate EOF
         B     EXIT08
*                                                                       NJE00920
*                                                                       NJE00920
********************                                                    NJE00920
*                  *   Delete a file from the NETSPOOL dataset          NJE00920
*  PURGE           *                                                    NJE00920
*  NCBREQ = X'05'  *                                                    NJE00920
*                  *                                                    NJE00920
********************                                                    NJE00920
*                                                                       NJE00920
PUR000   EQU   *
         LA    R14,*                   -> location of error source v110
         TM    NJFL1,NJF1OACB          Is ACB open?
         BZ    ERR1202                 No
*
         LA    R0,DIRDEL               Del file function
         L     R15,=A(NJEDIR)          Call directory mgmt
         BALR  R14,R15                 File to del is in NCB  ???
         LR    R5,R15                  Any RC to R5
         B     QUIT000
*
*                                                                       NJE00920
********************                                                    NJE00920
*                  *   Locate a file in the directory by file id        NJE00920
*  LOCATE          *                                                    NJE00920
*  NCBREQ = X'06'  *                                                    NJE00920
*                  *                                                    NJE00920
********************                                                    NJE00920
*                                                                       NJE00920
FID000   EQU   *
         LA    R14,*                   -> location of error source v110
         TM    NJFL1,NJF1OACB          Is ACB open?
         BZ    ERR1202                 No
*
         LA    R0,DIRLOC               Locate file function
         L     R15,=A(NJEDIR)          Call directory mgmt
         BALR  R14,R15                 File id is in tag field TAGID
         LR    R5,R15                  Any RC to R5
         B     QUIT000
*
*                                                                       NJE00920
********************                                                    NJE00920
*                  *   Return a list of files in NETSPOOL dataset       NJE00920
*  CONTENTS        *                                                    NJE00920
*  NCBREQ = X'07'  *                                                    NJE00920
*                  *                                                    NJE00920
********************                                                    NJE00920
*                                                                       NJE00920
CON000   EQU   *
         LA    R14,*                   -> location of error source v110
         TM    NJFL1,NJF1OACB          Is ACB open?
         BZ    ERR1202                 No
*
         LA    R0,DIRLST               List files function
         L     R15,=A(NJEDIR)          Call directory mgmt
         BALR  R14,R15
         LR    R5,R15                  Any RC to R5
         B     QUIT000
*
*                                                                       NJE00920
********************                                                    NJE00920
*                  *   Update a directory entry by file id         v120 NJE00920
*  UDIR            *                                                    NJE00920
*  NCBREQ = X'08'  *                                                    NJE00920
*                  *                                                    NJE00920
********************                                                    NJE00920
*                                                                       NJE00920
UDR000   EQU   *                                                   v120
         LA    R14,*                   -> location of error source v120
         TM    NJFL1,NJF1OACB          Is ACB open?                v120
         BZ    ERR1202                 No                          v120
*                                                                  v120
         LA    R0,DIRUPD               Update dir  function        v120
         L     R15,=A(NJEDIR)          Call directory mgmt         v120
         BALR  R14,R15                                             v120
         LR    R5,R15                  Any RC to R5                v120
         B     QUIT000                                             v120
*
*
ERR1201  EQU   *                       Invalid NCBREQ function code
         MVI   NCBERRCD,X'01'          Set error code
         B     EXIT12
*
ERR1202  EQU   *                       ACB is not open
         MVI   NCBERRCD,X'02'          Set error code
         B     EXIT12
*
ERR1203  EQU   *                       NETSPOOL dataset is full
         MVI   NCBERRCD,X'03'          Set error code
         B     EXIT12
*
ERR1204  EQU   *                       File # not found in directory
         MVI   NCBERRCD,X'04'          Set error code
         B     EXIT12
*
ERR1205  EQU   *                       GET attempted in PUT mode, or,
*                                      PUT attempted in GET mode
         MVI   NCBERRCD,X'05'          Set error code
         B     EXIT12
*
ERR1206  EQU   *                       No files in directory  (NCBCON)
         MVI   NCBERRCD,X'06'          Set error code
         B     EXIT12
*
*                                                                       NJE00200
* Exit points                                                           NJE00200
*                                                                       NJE00200
*                                                                       NJE00200
*                                                                       NJE00200
EXIT00   EQU   *                                                        NJE00210
         SR    R5,R5                   Set RC=0
         B     QUIT000
*
* Exit04 reasons:
*   All VSAM OPEN/CLOSE and RPL errors.
*
EXIT04   EQU   *                                                        NJE00210
         LA    R5,4                    Set RC=4
         B     QUIT000
*
* Exit08 reasons:
*   All VSAM OPEN/CLOSE and RPL errors.
*
EXIT08   EQU   *                                                        NJE00210
         C     R15,=F'4'               Is is really RC 4?
         BE    EXIT04                  Reflect the truth
         LA    R5,8                    Set RC=8
         B     QUIT000
*
* Exit12 reasons:
*   NETSPOOL dataset is full (no available blocks)
*   NCBREQ contains invalid/unsupported function code
*   File is not open
*   File # is not found in directory
*   GET issued during PUT activity
*   PUT issued during GET activity
*
EXIT12   EQU   *                                                        NJE00210
         ST    R14,NCBMACAD            Save error address          v110
         LA    R5,12                   Set RC=12
         B     QUIT000
*
* Exit16 reasons:
*   R1 = zero on entry
*   R1 doesnt point to NCB  ('NCB ' in 1st four bytes)
*   NCBTKN is zero but NCBREQ is not NCBOPEN
*   NCBTKN doesnt point to area containing 'NSPL'
*
EXIT16   EQU   *                                                        NJE00210
         L     R13,4(,R13)             -> caller's sa                   NJE00210
         LA    R5,16                   Set RC=16
         B     QUIT090
*
QUIT000  EQU   *
         STC   R5,NCBRTNCD             Set R15 return code
         BAL   R14,DEQ000              Remove any ENQ
         L     R13,4(,R13)             -> caller's sa                   NJE00210
         CLC   NCBREQ(3),=AL1(NCBGET,8,4)   EOF on a NCBGET function?
         BNE   QUIT020                 No
         ICM   R15,15,NCBEODAD         Get EODAD address
         BZ    QUIT020                 If none, let 8,4 rtn cd pass
         ST    R15,12(,R13)            Set R14 return to EODAD address
         XC    NCBRTNCD(2),NCBRTNCD    Remove EOF error indicators
         SR    R5,R5                   Set RC=0
*
QUIT020  EQU   *
         CLI   NCBREQ,NCBCLOSE         Is this a close request?
         BNE   QUIT090                 No. Exit without free stgs
*
         LR    R1,R10                  -> NJEWK main work area page
         FREEMAIN RU,                                                  x
               LV=4096,                                                x
               A=(1)
*
QUIT090  EQU   *
         ST    R5,16(,R13)             Set RC in R15
         LM    R14,R12,12(R13)         Reload callers's regs            NJE00220
         BR    R14                     Return                           NJE00240
*                                                                       NJE00250
         LTORG                                                          NJE00280
*
*
OPEN     OPEN  0,MF=L
OPENL    EQU   *-OPEN
CLOSE    CLOSE 0,MF=L
CLOSEL   EQU   *-CLOSE
*
*
         DROP  R12
*
*                                                                       NJE00920
*********************                                                   NJE00920
*  N J E C M N      *               NJECMN hosts small routines and     NJE00920
*                   *               frequently used constants           NJE00920
*  Common routines  *                                                   NJE00920
*  and constants    *               via base register 11                NJE00920
*                   *                                                   NJE00920
*********************                                                   NJE00920
*                                                                       NJE00920
NJECMN   CSECT                                                          NJE00020
         DC    A(0)                 No branch around constants
         DC    AL1(23)                LENGTH OF EYECATCHERS
         DC    CL9'NJECMN'
         DC    CL9'&SYSDATE'
         DC    CL5'&SYSTIME'
         USING NJECMN,R11
         USING NJEWK,R10
*
*-- Check result of VSAM OPEN or CLOSE macro
*
CHKOC    EQU   *
         LTR   R15,R15                 Did request succeed?
         BZR   R14                     Yes return
         ST    R14,NCBMACAD            Save addr of failing macro
         STC   R15,NCBRTNCD            Set return code
         MVC   NCBERRCD,ACBERFLG-IFGACB(R7) error code
         BR    R14                     Return with VSAM error
*
*-- Check result of VSAM RPL macros
*
CHKRPL   EQU   *
         LTR   R15,R15                 Did request succeed?
         BZR   R14                     Yes return
         ST    R14,NCBMACAD            Save addr of failing macro
         STC   R15,NCBRTNCD            Set return code
         MVC   NCBERRCD,RPLERRCD-IFGRPL(R7) error code
         BR    R14                     Return with VSAM error
*
*
ENQ000   EQU   *
         TM    NJFL1,NJF1ENQ           Is ENQ active?
         BOR   R14                     Return if so
*
         ST    R14,SV14                Save return addr
         ENQ   (NJE38Q,NJEDSN,E,44,SYSTEM),                            X
               RET=NONE
*
         OI    NJFL1,NJF1ENQ           ENQ active
         L     R14,SV14                Reload return addr
         BR    R14                     Return
*
*
DEQ000   EQU   *
         TM    NJFL1,NJF1ENQ           Is ENQ active?
         BZR   R14                     Return if not
*
         ST    R14,SV14                Save return addr
         DEQ   (NJE38Q,NJEDSN,44,SYSTEM),                              X
               RET=NONE
         NI    NJFL1,255-NJF1ENQ       ENQ off
         L     R14,SV14                Reload return addr
         BR    R14                     Return
*                                                                       NJE00200
*                                                                       NJE00200
*-- ADDBLK / GETBLK routines                                            NJE00200
*                                                                       NJE00200
*-- Allocate a new physical block.  Scan the allocation map for a free  NJE00200
*-- block and mark it as taken, and return the new block number to the  NJE00200
*-- caller.
*
*-- ADDBLK and GETBLK are functionally identical except that ADDBLK
*-- does not ENQ or DEQ on NETSPOOL; it is assumed that the caller
*-- already has done that (the DIR functions).
*
*-- Uses R14-R4,R7.   R1-R4 are preserved across call
*                                                                       NJE00200
*-- Entry: None                                                         NJE00200
*                                                                       NJE00200
*-- Exit:  R15 = 0 if ok, else RC from VSAM macro.                      NJE00200
*          R0  = block # of new block.  If R0=0, no blocks available.   NJE00200
*                                                                       NJE00200
ADDBLK   EQU   *
         ST    R14,SV14GB              Save return addr
         STM   R1,R4,SVGB              Save caller's regs
         BAL   R14,GETB000             Go allocate the block
         LTR   R15,R15                 VSAM RC in R15, set CC
         LR    R0,R4                   Return block # in R0
         LM    R1,R4,SVGB              Load caller's regs
         L     R14,SV14GB              Load return addr
         BR    R14                     Return
*                                                                       NJE00200
GETBLK   EQU   *
         ST    R14,SV14GB              Save return addr
         STM   R1,R4,SVGB              Save caller's regs
         BAL   R14,ENQ000              Get exclusive control
         BAL   R14,GETB000             Go allocate the block
         LR    R3,R15                  Save R15 across DEQ
         BAL   R14,DEQ000              Release control
         LTR   R15,R3                  Return VSAM RC in R15, set CC
         LR    R0,R4                   Return block # in R0
         LM    R1,R4,SVGB              Load caller's regs
         L     R14,SV14GB              Load return addr
         BR    R14                     Return
*
GETB000  EQU   *
         ST    R14,SV14B0              Save return addr
         LA    R2,ALLOCNUM             Get # of alloc map blocks
         LA    R3,ALLOCBLK             Get 1st alloc map block #
         LA    R4,1                    Starting relative block #
*
GETB010  EQU   *
         ST    R3,KEY                  Set retrieval key
         L     R7,RPL                  -> RPL
         GET   RPL=(R7)                Get a map block
         BAL   R14,CHKRPL              Deal with errors
         BNZ   GETB090                 Exit with VSAM error
*
         L     R14,BLOCK               -> allocation map
         LA    R15,4089                # of entries in map
         L     R1,=X'FF000000'         Set pad char=X'FF'
         CLCL  R14,R0                  Look for a non-FF entry
         BE    GETB030       all FFs:  We're full up in this block
*
         LR    R1,R14                  Copy ptr to map byte
         S     R1,BLOCK                Compute offset from start
         SLL   R1,3                    Each map byte is 8 records
         AR    R4,R1                   Adjust relative block number for
*                                       byte position we located
         ICM   R1,8,0(R14)             Get map byte with the free bit
         LA    R2,X'80'                Create possible opposing bit
*
GETB020  EQU   *
         SR    R0,R0                   Clear for shift use
         SLDL  R0,1                    Shift off one bit into R0
         LTR   R0,R0                   Is this the zero bit?
         BZ    GETB040                 Yes
         SRL   R2,1                    Next opposing bit position
         LA    R4,1(,R4)               Compute next rel blk #
         B     GETB020                 Find that 0 bit
*
GETB030  EQU   *
         LA    R4,4089(,R4)            Incr starting relative block #
         LA    R3,1(,R3)               Next map block key
         BCT   R2,GETB010              Read next map block
*
         ENDREQ RPL=(R7)               No update
         SR    R4,R4                   Return no block #: ALL FULL
         SR    R15,R15                 No VSAM errors
         B     GETB090                 Done
*
SETMAP   OI    0(R14),X'00'            Executed instr
*
GETB040  EQU   *
         EX    R2,SETMAP               Set the bit in allocation map
*
         PUT   RPL=(R7)                Update the allocation map
         BAL   R14,CHKRPL              Deal with errors
*
GETB090  EQU   *
         L     R14,SV14B0              Load return addr
         BR    R14                     Return
*
*
         LTORG
*
WTOMSG   WTO   '                                                       x
                                             ',MF=L
WTOMSGL  EQU   *-WTOMSG
*
ENQ      ENQ   (0),MF=L
ENQL     EQU   *-ENQ
*
DEQ      DEQ   (0),MF=L
DEQL     EQU   *-DEQ
*
         DS    0D
NJE38Q   DC    CL8'NJE38'
NJEDSN   DC    CL44'NJE38.NETSPOOL'
*
BLANKS   DC    CL120' '
NONBLANK DC    64X'FF',X'00',191X'FF'   TR Table to locate nonblank
BLANK    DC    64X'00',X'FF',100X'00'   TR Table to locate blanks
TRTAB$   DC    91X'00',X'FF',164X'00'   TR Table to locate '$'
HEXTRAN  DC    CL16'0123456789ABCDEF'  Translate table
*                                                                       NJE00920
*                                                                       NJE00920
*********************                                                   NJE00920
*                   *                                                   NJE00920
*  N J E D I R      *                                                   NJE00920
*                   *                                                   NJE00920
*  Directory        *                                                   NJE00920
*  Management       *                                                   NJE00920
*                   *                                                   NJE00920
*********************                                                   NJE00920
*                                                                       NJE00920
*
NJEDIR   CSECT                                                          NJE00020
         B     28(,R15)               BRANCH AROUND EYECATCHERS
         DC    AL1(23)                LENGTH OF EYECATCHERS
         DC    CL9'NJEDIR'
         DC    CL9'&SYSDATE'
         DC    CL5'&SYSTIME'
         STM   R14,R12,12(R13)         SAVE CMS REGS                    NJE00050
         LR    R12,R15                 BASE                             NJE00060
         USING NJEDIR,R12              ADDRESS IT                       NJE00070
         USING NJEWK,R10
         USING NCB,R9
*
         ST    R13,NJEDIRSA+4          SAVE prv S.A. ADDR               NJE00080
         LA    R1,NJEDIRSA             -> my save area
         ST    R1,8(,R13)              Plug it into prior SA
         LR    R13,R1
*
         L     R11,=A(NJECMN)          -> common csect
         ST    R11,ANJECMN             Save addr
         USING NJECMN,R11
*
DIRADD   EQU   0                       Add new file to directory
DIRDEL   EQU   4                       Purge a file from directory
DIRLOC   EQU   8                       Locate a file by ID
DIRLST   EQU   12                      List directory contents
DIRUPD   EQU   16                      Update directory entry      v120
*
         LR    R2,R0                   Copy entry code
         B     *+4(R2)                 Branch into branch table
         B     ADD000               0  Add a new directory entry
         B     DEL000               4  Delete a directory entry
         B     LOC000               8  Locate a file by ID
         B     LST000               C  List directory contents
         B     UPD000              10  Update directory entry      v120
*
ADD000   EQU   *
         LA    R0,(10000/8)+1          Byte size of 10,000 bits
         ST    R0,SPLIDLEN             Save the length
         GETMAIN RU,                   Get stg for spool id bitmap     x
               LV=(0)
         ST    R1,SPLIDMAP             Save stg addr
         LR    R0,R1                   Copy starting addr
         L     R1,SPLIDLEN             Get the length
         SR    R15,R15                 Set pad char
         MVCL  R0,R14                  Initialize the map
*
         BAL   R14,ENQ000              Get exclusivity
*
         MVC   KEY,=F'1'               Get the first block
         L     R7,RPL                  -> RPL
         GET   RPL=(R7)                Get a map block
         BAL   R14,CHKRPL              Deal with errors
         BNZ   ADD900                  Exit with VSAM error
*
         L     R2,BLOCK                -> blk #1 in stg
         USING BLKONE,R2
         MVC   SPLID,SPLNUM            Save the last assigned id #
         L     R2,DIRBLK               Get blk# of current directory
         DROP  R2
         LA    R3,1                    Load XOR counterpart
         XR    R3,R2                   Compute alternate directry blk#
*
*-- R2 = starting block number of current directory
*-- R3 = starting block number of replacement directory
*
*
         ST    R2,KEY                  Get a current dir block
         L     R7,RPL                  -> RPL
         GET   RPL=(R7)                Get a block
         BAL   R14,CHKRPL              Deal with errors
         BNZ   ADD900                  Exit with VSAM error
*
         L     R0,BLOCK                -> VSAM i/o area
         LA    R1,4089                 Size of physical block
         L     R14,BUFF                -> buffer to place block
         LR    R15,R1                  Copy length
         MVCL  R14,R0                  Move data
*
         ST    R3,KEY                  Get a replacement dir block
         L     R7,RPL                  -> RPL
         GET   RPL=(R7)                Get a block
         BAL   R14,CHKRPL              Deal with errors
         BNZ   ADD900                  Exit with VSAM error
*
*-- Copy all of the directory entries in the current directory over
*-- to the (new) replacement directory (where we will eventually add
*-- a new directory entry).  Along the way, build a bit map of all
*-- of the spool file numbers that are in use (they're in the
*-- directory entries) so that we can assign a new unique file # to
*-- the new file in its new directory entry.
*
         L     R4,BUFF                 -> current directory
         L     R5,BLOCK                -> replacement directory
         L     R8,NSRECNM-NSDIR(,R4)   Get # directory entries current
         LA    R1,1(,R8)               +1 for new dir ent to be added
         ST    R1,NSRECNM-NSDIR(,R4)   Store (will get copied to repl)
         ST    R3,NSBLK-NSDIR(,R4)     Store starting blk of dir (will
*                                       get copied to replacement dir)
*
ADD050   EQU   *
         CLC   NSLEN-NSDIR(,R4),=X'FFFE'   Ptr to next block?
         BE    ADD100                  yes
         MVC   0(NSDIRLN,R5),0(R4)     Copy existing dir entry to repl
*
         LH    R7,NSID-NSDIR(,R4)      Get file id # for this file
         SR    R6,R6                   Clear for divide
         D     R6,=F'8'                Get byte offset remainder bits
*
         A     R7,SPLIDMAP             -> byte containing bit for
*                                       this file #
         LA    R1,X'80'                Create a bit
         SRL   R1,0(R6)                Adjust to bit for this file #
         EX    R1,SPLSET               Set the bit in the spool id map
*
         LA    R4,NSDIRLN(,R4)         -> next current dir entry
         LA    R5,NSDIRLN(,R5)         -> next replacement dir entry
         BCT   R8,ADD050               Keep copying dir entries
         B     ADD200                  Go add the new dir entry
*
SPLSET   OI    0(R7),X'00'             Executed instr
*
*
*-- Here if the directory continues onto another block.  Get these
*-- blocks, and continue processing individual entries.
*
ADD100   EQU   *
         L     R7,RPL                  -> RPL
         PUT   RPL=(R7)                Update the replacement block
         BAL   R14,CHKRPL              Deal with errors
         BNZ   ADD900                  Exit with VSAM error
*
         CLC   NSLEN-NSDIR(,R5),=X'FFFE' Repl dir ptr to next block?
         BNE   ADD190                  No; we need to add a block
*
ADD120   EQU   *
         ICM   R2,15,2(R4)             Get ptr to next current dir blk
         ICM   R3,15,2(R5)             Get ptr to next repl dir blk
*
         ST    R2,KEY                  Get next current dir block
         L     R7,RPL                  -> RPL
         GET   RPL=(R7)                Get a block
         BAL   R14,CHKRPL              Deal with errors
         BNZ   ADD900                  Exit with VSAM error
*
         L     R0,BLOCK                -> VSAM i/o area
         LA    R1,4089                 Size of physical block
         L     R14,BUFF                -> buffer to place block
         LR    R15,R1                  Copy length
         MVCL  R14,R0                  Move data
*
         ST    R3,KEY                  Get next replacement dir block
         L     R7,RPL                  -> RPL
         GET   RPL=(R7)                Get a block
         BAL   R14,CHKRPL              Deal with errors
         BNZ   ADD900                  Exit with VSAM error
*
         L     R4,BUFF                 -> current directory
         L     R5,BLOCK                -> replacement directory
         B     ADD050                  Continue processing
*
ADD190   EQU   *
         L     R3,KEY                  Get current blk # we just wrote
*
         BAL   R14,ADDBLK              Allocate a new physical block
         BNZ   ADD900                  Exit with VSAM error
         LTR   R6,R0                   Is there a block number?
         BZ    ADD910                  No, NETSPOOL dataset full   v130
*
         ST    R3,KEY                  Gotta update blk again with ptr
         GET   RPL=(R7)                Get the physical block
         BAL   R14,CHKRPL              Deal with errors
         BNZ   ADD900                  Exit with VSAM error
*
         MVC   0(2,R5),=X'FFFE'        Insert ptr indic for next blk
         STCM  R6,15,2(R5)             Insert next block #
         B     ADD100                  Now go jump to next dir blks
*
*-- Here when all current directory entries have been copied to the
*-- new (replacement) directory.  Add the new directory entry for
*-- the file just written out via PUT actions.
*
ADD200   EQU   *
         L     R1,BLOCKEND             -> end of buffer
         SR    R1,R5                   Determine remaining space in blk
         LA    R4,NSDIRLN              Get size of directory entry
         LA    R4,2+4(,R4)             Add in overhead
*                                       +2 for n block marker
*                                       +4 for next block ptr
         CR    R1,R4                   Is there room to add entry?
         BL    ADD300                  No, better get another block
*
         USING NSDIR,R5
         XC    NSDIR(NSDIRLN),NSDIR    Init new entry
         MVC   NSLEN,=Y(NSDIRLN)       Set entry length
         MVC   NSBLK,INITBLK           Set starting blk# of the file
         L     R6,NCBTAG               -> TAG block for file
         USING TAG,R6
         MVC   NSINLOC(TAGUSELN),TAGINLOC  Tag data to dir entry
*
         L     R1,SPLID                Get last assigned file id #
         L     R0,=F'10000'            10,000 possible spool ids
*
ADD250   EQU   *
         LA    R15,1(,R1)              Choose next number
         C     R15,=F'10000'           At the limit?
         BL    *+8                     No
         LA    R15,1                   Reset to 1
         LR    R1,R15                  Save next possible number
*
         SR    R14,R14                 Clear for divide
         D     R14,=F'8'               Get byte offset remainder bits
*
         A     R15,SPLIDMAP            -> byte containing bit for
*                                       this spool id #
         LA    R7,X'80'                Create a bit
         SRL   R7,0(R14)               Adjust to bit for this id #
         EX    R7,TMBIT                Check bit status in the bitmap
         BZ    ADD260                  Spool id not in use. take it
         BCT   R0,ADD250               Else try next number
         SR    R1,R1                   Otherwise use id=0000
         B     ADD260
*
TMBIT    TM    0(R15),X'00'            Executed instr
*
*
*
ADD260   EQU   *
         ST    R1,SPLID                Save newly assigned spool id
         STCM  R1,3,NSID               Assign the file id # to file
         STCM  R1,3,NCBFID             Also put it in the NCB
         STCM  R1,3,TAGID              Also, put it in the tag data
         DROP  R5,R6                   NSDIR,TAG
*
         LA    R4,NSDIRLN(,R5)         Skip past entry just added
         L     R5,BLOCKEND             -> end of block
         SR    R5,R4                   Compute length remaining in blk
         SR    R15,R15                 Set pad
         MVCL  R4,R14                  Clear to end of block
*
         L     R7,RPL                  -> RPL
         PUT   RPL=(R7)                Update final replacement block
         BAL   R14,CHKRPL              Deal with errors
         BNZ   ADD900                  Exit with VSAM error
*
*-- Now update block 1 to activate the replacement directory
*
         MVC   KEY,=F'1'               Get the first block
         L     R7,RPL                  -> RPL
         GET   RPL=(R7)                Get a map block
         BAL   R14,CHKRPL              Deal with errors
         BNZ   ADD900                  Exit with VSAM error
*
         L     R1,BLOCK                -> blk #1 in stg
         USING BLKONE,R1
         L     R2,DIRBLK               Get blk# of current directory
         LA    R3,1                    Load XOR counterpart
         XR    R3,R2                   Compute alternate directry blk#
         ST    R3,DIRBLK               Plug in alternate
         MVC   SPLNUM,SPLID            Save last assigned spool id
         DROP  R1
*
         L     R7,RPL                  -> RPL
         PUT   RPL=(R7)                Update block 1
         BAL   R14,CHKRPL              Deal with errors
         BNZ   ADD900                  Exit with VSAM error
         B     XITDIR                  Exit with RC=0
*
*-- Here if there is no room in a directory block to add the new
*-- file's directory entry.  An additional block will be allocated and
*-- chained to the directory entries.
*
ADD300   EQU   *
         L     R7,RPL                  -> RPL
         PUT   RPL=(R7)                Write back the dir block
         BAL   R14,CHKRPL              Deal with errors
         BNZ   ADD900                  Exit with VSAM error
         L     R4,KEY                  Get current blk # we just wrote
*
         BAL   R14,ADDBLK              Allocate a new physical block
         BNZ   ADD900                  Exit with VSAM error
         LTR   R6,R0                   Is there a block number?
         BZ    ADD910                  No, NETSPOOL dataset full   v130
*
         ST    R4,KEY                  Gotta update blk again with ptr
         GET   RPL=(R7)                Get the physical block
         BAL   R14,CHKRPL              Deal with errors
         BNZ   ADD900                  Exit with VSAM error
*
         MVC   0(2,R5),=X'FFFE'        Insert ptr indic for next blk
         STCM  R6,15,2(R5)             Insert next block #
*
         L     R7,RPL                  -> RPL
         PUT   RPL=(R7)                Write back the dir block
         BAL   R14,CHKRPL              Deal with errors
         BNZ   ADD900                  Exit with VSAM error
*
         ST    R6,KEY                  Now point to newly obtained blk
         GET   RPL=(R7)                Get the physical block
         BAL   R14,CHKRPL              Deal with errors
         BNZ   ADD900                  Exit with VSAM error
*
         L     R0,BLOCK                -> VSAM i/o area
         LA    R1,4089                 Size of physical block
         SR    R3,R3                   Pad
         MVCL  R0,R2                   Clear it
*
         L     R5,BLOCK                -> new block stg
         B     ADD200                  Try again to add new dir entry
*
ADD900   EQU   *                       VSAM Error return
*                                       Error codes in NCB already
         B     XITDIR                  Exit with RC in R15
*
ADD910   EQU   *                       No space in NETSPOOL
         MVC   NCBRTNCD(2),=X'0C03'    Set to 12,3 code
         LA    R14,*                   -> location of error source v110
         ST    R14,NCBMACAD            Store into NCB              v110
         LA    R15,12                  Set RC
         B     XITDIR                  Return that notice
*
*
*
*
*
DEL000   EQU   *
         GETMAIN RU,                   Get stg for alloc bitmap        x
               LV=16384
         STM   R0,R1,SPLIDLEN          Save len,addr
*
         L     R7,RPL                  -> RPL
         MODCB RPL=(R7),                                               x
               OPTCD=(KEY,DIR,MVE,UPD), Update mode                    x
               MF=(G,MACLIST)
*
         BAL   R14,ENQ000              Get exclusivity
*
         LA    R2,ALLOCNUM             Get # of alloc map blocks
         LA    R3,ALLOCBLK             Get 1st alloc map block #
         L     R4,SPLIDMAP             -> receiving stg area
*
DEL020   EQU   *
         ST    R3,KEY                  Set retrieval key
         L     R7,RPL                  -> RPL
         GET   RPL=(R7)                Get a map block
         BAL   R14,CHKRPL              Deal with errors
         BNZ   DEL900                  Exit with VSAM error
*
         L     R14,BLOCK               -> block just read
         LA    R15,4089                # of bytes in block
         LR    R5,R15                  Copy len
         MVCL  R4,R14                  Move alloc bitmap to stg area
*
         LA    R3,1(,R3)               Next block number of alloc map
         BCT   R2,DEL020               Go read them all
*
         MVC   KEY,=F'1'               Get the first block
         L     R7,RPL                  -> RPL
         GET   RPL=(R7)                Get a map block
         BAL   R14,CHKRPL              Deal with errors
         BNZ   DEL900                  Exit with VSAM error
*
         L     R2,BLOCK                -> blk #1 in stg
         USING BLKONE,R2
         L     R2,DIRBLK               Get blk# of current directory
         LA    R3,1                    Load XOR counterpart
         XR    R3,R2                   Compute alternate directry blk#
         DROP  R2
*
*-- R2 = starting block number of current directory
*-- R3 = starting block number of replacement directory
*
*
         ST    R2,KEY                  Get a current dir block
         L     R7,RPL                  -> RPL
         GET   RPL=(R7)                Get a block
         BAL   R14,CHKRPL              Deal with errors
         BNZ   DEL900                  Exit with VSAM error
*
         L     R0,BLOCK                -> VSAM i/o area
         LA    R1,4089                 Size of physical block
         L     R14,BUFF                -> buffer to place block
         LR    R15,R1                  Copy length
         MVCL  R14,R0                  Move data
*
         ST    R3,KEY                  Get a replacement dir block
         L     R7,RPL                  -> RPL
         GET   RPL=(R7)                Get a block
         BAL   R14,CHKRPL              Deal with errors
         BNZ   DEL900                  Exit with VSAM error
*
         L     R0,BLOCK                -> VSAM i/o area
         LA    R1,4089                 Size of physical block
         L     R14,PTRBUF              -> buffer to place block
         LR    R15,R1                  Copy length
         MVCL  R14,R0                  Move data
*
*-- Current directory is in BUFF
*-- Replacement directory will be in PTRBUF
*
*
*-- Copy all of the directory entries in the current directory over
*-- to the (new) replacement directory (where we will eventually delete
*-- a directory entry).  Along the way, look for the entry to be
*-- purged.
*
         L     R4,BUFF                 -> current directory
         L     R5,PTRBUF               -> replacement directory
         L     R8,NSRECNM-NSDIR(,R4)   Get # directory entries current
         LR    R1,R8                   Copy count
         BCTR  R1,0                    Reduce for to-be-deleted file
         ST    R1,NSRECNM-NSDIR(,R4)   Store (will get copied to repl)
         ST    R3,NSBLK-NSDIR(,R4)     Store starting blk of dir (will
*                                       get copied to replacement dir)
         L     R6,NCBTAG               -> TAG data
         LH    R6,TAGID-TAG(,R6)       Get file id number
         XC    INITBLK,INITBLK         Clear file's starting blk #
*
DEL050   EQU   *
         CLC   NSLEN-NSDIR(,R4),=X'FFFE'   Ptr to next block?
         BE    DEL100                  yes
         CH    R6,NSID-NSDIR(,R4)      Is this the file to be purged?
         BE    DEL070
         CLC   NSLEN-NSDIR(,R5),=X'FFFE'   Ptr to next block?
         BE    DEL120                  yes
         MVC   0(NSDIRLN,R5),0(R4)     Copy existing dir entry to repl
         LA    R5,NSDIRLN(,R5)         -> next replacement dir entry
*
DEL060   EQU   *
         LA    R4,NSDIRLN(,R4)         -> next current dir entry
         BCT   R8,DEL050               Keep copying dir entries
         B     DEL200                  Done with copy
*
DEL070   EQU   *
         MVC   INITBLK,NSBLK-NSDIR(R4) Save starting block # of file
         B     DEL060                  Continue copy
*
*
*-- Get next current dir block (move it to BUFF)
*
DEL100   EQU   *
         ICM   R2,15,2(R4)             Get ptr to next current dir blk
*
         ST    R2,KEY                  Get next current dir block
         L     R7,RPL                  -> RPL
         GET   RPL=(R7)                Get a block
         BAL   R14,CHKRPL              Deal with errors
         BNZ   DEL900                  Exit with VSAM error
*
         L     R0,BLOCK                -> VSAM i/o area
         LA    R1,4089                 Size of physical block
         L     R14,BUFF                -> buffer to place block
         LR    R15,R1                  Copy length
         MVCL  R14,R0                  Move data
         L     R4,BUFF                 -> current directory
         B     DEL050                  Continue with copy
*
*-- Get next replacement dir block
*--  1. Write back the replacement we've been copying to (from PTRBUF)
*--  2. Get next block
*--  3. Move it to PTFBUF
*
DEL120   EQU   *
         ST    R3,KEY                  Set blk# of repl dir block
         L     R7,RPL                  -> RPL
         GET   RPL=(R7)                Get the block for update
         BAL   R14,CHKRPL              Deal with errors
         BNZ   DEL900                  Exit with VSAM error
*
         L     R0,BLOCK                -> VSAM i/o area
         LA    R1,4089                 Size of physical block
         L     R14,PTRBUF              -> buffer containing repl dir
         LR    R15,R1                  Copy length
         MVCL  R0,R14                  Move data to i/o buffer
*
         L     R7,RPL                  -> RPL
         PUT   RPL=(R7)                Update the replacement block
         BAL   R14,CHKRPL              Deal with errors
         BNZ   DEL900                  Exit with VSAM error
*
         ICM   R3,15,2(R5)             Get ptr to next current dir blk
*
         ST    R3,KEY                  Get next current dir block
         L     R7,RPL                  -> RPL
         GET   RPL=(R7)                Get a block
         BAL   R14,CHKRPL              Deal with errors
         BNZ   DEL900                  Exit with VSAM error
*
         L     R0,BLOCK                -> VSAM i/o area
         LA    R1,4089                 Size of physical block
         L     R14,PTRBUF              -> buffer to place block
         LR    R15,R1                  Copy length
         MVCL  R14,R0                  Move data
         L     R5,PTRBUF               -> replacement directory
         B     DEL050                  Continue with copy
*
*-- Fix up the last replacement dir block
*
DEL200   EQU   *
         L     R1,PTRBUF               -> start of buffer
         LA    R15,4088(,R1)           -> end of that buffer - 1
*
DEL210   EQU   *
         CR    R1,R15                  Past end of buffer?
         BH    DEL230                  Y, done searching
         CLC   0(2,R1),=X'FFFE'        Left over pointer indicator?
         BE    DEL220                  Yes
         LA    R1,NSDIRLN(,R1)         Next dir entry position
         B     DEL210
*
DEL220   EQU   *
         ICM   R7,15,2(R1)             Pick up the left over block #
         BAL   R14,FREBLK              Go free the block in R7
*
DEL230   EQU   *
         LR    R0,R5                   -> end of used part of ptrbuf
         L     R1,PTRBUF               -> start of buffer
         LA    R1,4089(,R1)            -> end of that buffer
         SR    R1,R5                   Compute length to clear
         SR    R15,R15                 Compute length to write out
         MVCL  R0,R14                  Clear to end of block
*
         ST    R3,KEY                  Set blk# of repl dir block
         L     R7,RPL                  -> RPL
         GET   RPL=(R7)                Re-get for update
         BAL   R14,CHKRPL              Deal with errors
         BNZ   DEL900                  Exit with VSAM error
*
         L     R0,BLOCK                -> VSAM i/o area
         LA    R1,4089                 Size of physical block
         L     R14,PTRBUF              -> buffer containing repl dir
         LR    R15,R1                  Copy length
         MVCL  R0,R14                  Move repl data to i/o buffer
*
         PUT   RPL=(R7)                Update the last repl block
         BAL   R14,CHKRPL              Deal with errors
         BNZ   DEL900                  Exit with VSAM error
*
*-- DEL300 is used to free all of the blocks used by the file itself
*
DEL300   EQU   *
         ICM   R7,15,INITBLK           Get 1st block # of deleted file
         BZ    DEL910                  If 0, file # wasn't found
*
DEL310   EQU   *
         ST    R7,KEY                  Set block retreival key
         BAL   R14,FREBLK              Mark the block as free in bitmap
*
         L     R7,RPL                  -> RPL
         GET   RPL=(R7)                Get the ptr block
         BAL   R14,CHKRPL              Deal with errors
         BNZ   DEL900                  Exit with VSAM error
*
         L     R4,BLOCK                -> ptr block ptrs
         LA    R5,4084(,R4)            -> end of ptr block ptrs
*
DEL330   EQU   *
         ICM   R7,15,0(R4)             Get a block #
         BZ    DEL350                  Done with ptrs
         BAL   R14,FREBLK              Free the block
         LA    R4,4(,R4)               -> next ptr field
         CR    R4,R5                   At end of ptr block?
         BL    DEL330
*                                   ** Here if ptr block chains to
*                                       another ptr block
         CLI   0(R4),X'FE'             Ptr to ptr blk indicator?
         BNE   DEL350                  No, we've processed last ptr
         SR    R7,R7                   Clear for IC
         ICM   R7,7,1(R4)              Get ptr to next ptr block
         B     DEL310
*
*-- Write back the allocation map
*
DEL350   EQU   *
         LA    R2,ALLOCNUM             Get # of alloc map blocks
         LA    R3,ALLOCBLK             Get 1st alloc map block #
         L     R4,SPLIDMAP             -> map stg area
*
DEL360   EQU   *
         ST    R3,KEY                  Set retrieval key
         L     R7,RPL                  -> RPL
         GET   RPL=(R7)                Get a map block
         BAL   R14,CHKRPL              Deal with errors
         BNZ   DEL900                  Exit with VSAM error
*
         L     R14,BLOCK               -> block just read
         LA    R15,4089                # of bytes in block
         LR    R5,R15                  Copy len
         MVCL  R14,R4                  Move alloc bitmap to i/o buffer
*
         PUT   RPL=(R7)                Put the map block
         BAL   R14,CHKRPL              Deal with errors
         BNZ   DEL900                  Exit with VSAM error
*
         LA    R3,1(,R3)               Next block number of alloc map
         BCT   R2,DEL360               Go read them all
*
*-- Now update block 1 to activate the replacement directory
*
DEL400   EQU   *
         MVC   KEY,=F'1'               Get the first block
         L     R7,RPL                  -> RPL
         GET   RPL=(R7)                Get a map block
         BAL   R14,CHKRPL              Deal with errors
         BNZ   DEL900                  Exit with VSAM error
*
         L     R1,BLOCK                -> blk #1 in stg
         USING BLKONE,R1
         L     R2,DIRBLK               Get blk# of current directory
         LA    R3,1                    Load XOR counterpart
         XR    R3,R2                   Compute alternate directry blk#
         ST    R3,DIRBLK               Plug in alternate
         DROP  R1
*
         L     R7,RPL                  -> RPL
         PUT   RPL=(R7)                Update block 1
         BAL   R14,CHKRPL              Deal with errors
         BNZ   DEL900                  Exit with VSAM error
         B     XITDIR                  Exit with RC=0
*
DEL900   EQU   *                       VSAM Error return
*                                       Error codes in NCB already
         B     XITDIR                  Exit with RC in R15
*
DEL910   EQU   *                    ** Here if directry entry not found
         MVC   NCBRTNCD(2),=X'0C04'    Set to 12,4 code
         LA    R14,*                   -> location of error source v110
         ST    R14,NCBMACAD            Store into NCB              v110
         LA    R15,12                  Set RC
         B     XITDIR                  Exit with RC in R15
*
*-- Free a block (mark it available in the allocation bitmap)
*
*-- Entry: R7 = block #
*
FREBLK   EQU   *
         BCTR  R7,0                    Make blk # relative to 0
         SR    R6,R6                   Clear for divide
         D     R6,=F'8'                Get byte offset remainder bits
*
         A     R7,SPLIDMAP             -> byte containing bit for
*                                       this block
         LA    R1,X'80'                Create a bit
         SRL   R1,0(R6)                Adjust to bit for this blk #
         LA    R0,X'FF'                Create AND mask
         XR    R1,R0                   Compute mask to turn a bit off
         EX    R1,FREBIT               Turn off the bit in the bitmap
         BR    R14                     Return
*
FREBIT   NI    0(R7),X'00'             Executed instr
*
*
*
* LOC000 - FIND a file by id in the directory.                     v120
* UPD000 - UDIR update a directory entry for a specific file.      v120
*
*
*-- UDIR functionality only updates the destination node id and    v120
*--      destination user id within the directory entry from       v120
*--      the TAG data supplied by the caller.  No other directory  v120
*--      fields are altered.                                       v120
*
*
LOC000   EQU   *
UPD000   EQU   *                                                   v120
         BAL   R14,ENQ000              Get exclusivity
*
         MVC   KEY,=F'1'               Get the first block
         L     R7,RPL                  -> RPL
         GET   RPL=(R7)                Get a map block
         BAL   R14,CHKRPL              Deal with errors
         BNZ   LOC900                  Exit with VSAM error
*
         L     R2,BLOCK                -> blk #1 in stg
         USING BLKONE,R2
         L     R2,DIRBLK               Get blk# of current directory
         DROP  R2
*
*
         ST    R2,KEY                  Get a current dir block
         GET   RPL=(R7)                Get a block
         BAL   R14,CHKRPL              Deal with errors
         BNZ   LOC900                  Exit with VSAM error
*
*
         L     R4,BLOCK                -> current directory
         USING NSDIR,R4
         L     R8,NSRECNM              Get # directory entries current
*
         L     R6,NCBTAG               -> TAG data
         USING TAG,R6
         XC    INITBLK,INITBLK         Clear file's starting blk #
*
LOC050   EQU   *
         CLC   NSLEN,=X'FFFE'          Ptr to next block?
         BNE   LOC060                  No
*
         ICM   R2,15,2(R4)             Get ptr to next current dir blk
         ST    R2,KEY                  Get next current dir block
*
         GET   RPL=(R7)                Get a block
         BAL   R14,CHKRPL              Deal with errors
         BNZ   LOC900                  Exit with VSAM error
         L     R4,BLOCK                -> next directory block
*
LOC060   EQU   *
         CLC   TAGID,NSID              Is this the file we need?
         BE    LOC070
*
         LA    R4,NSDIRLN(,R4)         -> next current dir entry
         BCT   R8,LOC050               Keep looking
         B     LOC100                  Done with search
*
LOC070   EQU   *
         CLI   NCBREQ,NCBUDIR          Is this UDIR function?      v120
         BE    UPD100                  Yes                         v120
*
         MVC   INITBLK,NSBLK           Save starting block # of file
         MVC   TAGINLOC(TAGUSELN),NSINLOC  Return the tag data to callr
*
*
LOC100   EQU   *
         ENDREQ RPL=(R7)               Release the get-for-update
*
         NC    INITBLK,INITBLK         Did we find a file?
         BZ    LOC910                  No, exit with not found error
         SR    R15,R15                 Set RC to 0
         B     XITDIR
*
*
UPD100   EQU   *                                                   v120
         MVC   NSTOLOC,TAGTOLOC        Update destination node id  v120
         MVC   NSTOVM,TAGTOVM          Update destination user id  v120
         MVC   TAGINLOC(TAGUSELN),NSINLOC  Rtrn tag data to caller v120
         MVC   INITBLK,NSBLK           Save file's startinblock #  v120
*
         PUT   RPL=(R7)                Update the directory        v120
         BAL   R14,CHKRPL              Deal with errors            v120
         BNZ   LOC900                  Exit if VSAM error          v120
         B     XITDIR
*
         DROP  R6                      TAG                         v120
         DROP  R4                      NSDIR                       v120
*
*
LOC900   EQU   *                       VSAM Error return
*                                       Error codes in NCB already
         B     XITDIR                  Exit with RC in R15
*
LOC910   EQU   *                    ** Here if directry entry not found
         MVC   NCBRTNCD(2),=X'0C04'    Set to 12,4 code
         LA    R14,*                   -> location of error source v110
         ST    R14,NCBMACAD            Store into NCB              v110
         LA    R15,12                  Set RC
         B     XITDIR                  Exit with RC in R15
*
*
*
*
*
LST000   EQU   *
         XC    LISTLEN,LISTLEN         Ensure no stray len
         XC    LISTADDR,LISTADDR       Ensure no stray address
         BAL   R14,ENQ000              Get exclusivity
*
         MVC   KEY,=F'1'               Get the first block
         L     R7,RPL                  -> RPL
         GET   RPL=(R7)                Get a map block
         BAL   R14,CHKRPL              Deal with errors
         BNZ   LST900                  Exit with VSAM error
*
         L     R2,BLOCK                -> blk #1 in stg
         USING BLKONE,R2
         L     R3,ALMBLK               Get blk# of alloc map       v200
         L     R8,MAXBLK               Get blk# in dataset         v200
         L     R2,DIRBLK               Get blk# of current directory
         DROP  R2
*
*-- Compute spool percentage full from alloc map                   v200
*
         SR    R5,R5                   Init blks used counter      v200
         LR    R6,R8                   Copy max blocks in dataset  v200
         SRL   R6,3     divide by 8    # map bytes represent'g blksv200
*
LST010   EQU   *                                                   v200
         ST    R3,KEY                  Get a block of map          v200
         L     R7,RPL                  -> RPL                      v200
         GET   RPL=(R7)                Get a map block             v200
         BAL   R14,CHKRPL              Deal with errors            v200
         BNZ   LST900                  Exit with VSAM error        v200
*                                                                  v200
         SR    R0,R0                   Clear for IC work           v200
         L     R15,BLOCK               -> record                   v200
         LA    R14,4089                # bytes to process          v200
*
LST020   EQU   *                                                   v200
         CLI   0(R15),X'00'            Map byte unallocated?       v200
         BE    LST050                  Dont count any              v200
         CLI   0(R15),X'FF'            Map byte fully allocated?   v200
         BE    LST060                  Yes, count 8 blocks         v200
         LA    R4,8                    # bits in a byte            v200
         IC    R0,0(,R15)              Get a map byte              v200
*
LST030   EQU   *                                                   v200
         SR    R1,R1                   Clear for shift             v200
         SRDL  R0,1                    Move a bit into R1          v200
         LTR   R1,R1                   Was the bit=1?              v200
         BZ    LST040                  No, dont count it           v200
         LA    R5,1(,R5)               Count the block bit         v200
*
LST040   EQU   *                                                   v200
         BCT   R4,LST030               Scan whole byte             v200
*
LST050   EQU   *                                                   v200
         BCT   R6,LST070               # map bytes remaining to scnv200
         B     LST080                  Done counting               v200
*
LST060   EQU   *                                                   v200
         LA    R5,8(,R5)               All 8 blocks allocated      v200
         B     LST050                  Decr remaining and continue v200
*
LST070   EQU   *                                                   v200
         LA    R15,1(,R15)             -> next map byte            v200
         BCT   R14,LST020              Keep scanning               v200
         LA    R3,1(,R3)               Bump alloc map block number v200
         B     LST010                  Get another map block       v200
*
LST080   EQU   *                                                   v200
         MH    R5,=Y(100)              Blocks used: prep for % calcv200
         SR    R4,R4                   Clear for divide            v200
         DR    R4,R8                   Compute % full              v200
         AR    R4,R4                   Double remainder            v200
         CR    R4,R8                   Do we need to round up?     v200
         BL    LST090                  No                          v200
         LA    R5,1(,R5)               Round up percent full       v200
*
LST090   EQU   *                                                   v200
         STH   R5,NCBPCT               Return % full in NCB        v200
*
*-- Retrieve directory contents                                    v200
*
LST100   EQU   *
         ST    R2,KEY                  Get a current dir block
         GET   RPL=(R7)                Get a block
         BAL   R14,CHKRPL              Deal with errors
         BNZ   LST900                  Exit with VSAM error
*
*
         L     R4,BLOCK                -> current directory
         USING NSDIR,R4
         L     R8,NSRECNM              Get # directory entries
         BCTR  R8,0                    Less 1 for directory itself
         STCM  R8,3,NCBRECCT           Set entries count in NCB
         LTR   R8,R8                   Were there any entries?
         BZ    LST910                  No
         SR    R0,R0                   Clear for multiply
         LA    R1,NSDIRLN              Length of directory entry
         MR    R0,R8                   Compute size of area needed
         LR    R0,R1                   Copy size to r0
         GETMAIN RU,                   Get stg area to hold entries    x
               LV=(0)
         STM   R0,R1,LISTLEN
         LR    R5,R1                   -> where to place entries
         LA    R4,NSDIRLN(,R4)         Skip over directory's own entry
*
*
LST150   EQU   *                                                   v200
         CLC   NSLEN,=X'FFFE'          Ptr to next block?
         BNE   LST160                  No                          v200
*
         ICM   R2,15,2(R4)             Get ptr to next current dir blk
         ST    R2,KEY                  Get next current dir block
*
         GET   RPL=(R7)                Get a block
         BAL   R14,CHKRPL              Deal with errors
         BNZ   LST900                  Exit with VSAM error
         L     R4,BLOCK                -> next directory block
*
LST160   EQU   *                                                   v200
         MVC   0(NSDIRLN,R5),0(R4)     Move directory entry to stg area
         LA    R4,NSDIRLN(,R4)         -> next dir entry
         LA    R5,NSDIRLN(,R5)         -> next stg area slot
         BCT   R8,LST150               Keep loading                v200
*
         DROP  R4                      NSDIR
*
*
LST200   EQU   *                                                   v200
         ENDREQ RPL=(R7)               Release the get-for-update
*
         MVC   NCBAREA,LISTADDR        Return list stg addr
         MVC   NCBAREAL,LISTLEN        Return list stg len
         MVC   NCBRECLN,=Y(NSDIRLN)    Return size of each dir entry
         SR    R15,R15                 Set RC to 0
         B     XITDIR
*
*
LST900   EQU   *                       VSAM Error return
*                                       Error codes in NCB already
         LM    R0,R1,LISTLEN           Get stg area len, addr
         LTR   R0,R0                   Is there an area?
         BZ    XITDIR                  No
         FREEMAIN RU,LV=(0),A=(1)      Else free it
         SR    R15,R15                 Clear for RC
         IC    R15,NCBRTNCD            Reinsert RC
         B     XITDIR                  Exit with RC in R15
*
LST910   EQU   *                    ** Here if no files queued
         ENDREQ RPL=(R7)               Release the get-for-update  v130
         XC    NCBAREA,NCBAREA         No directory list obtained  v110
         MVC   NCBRTNCD(2),=X'0C06'    Set to 12,6 code
         LA    R15,12                  Set RC
         LA    R14,*                   -> location of error source v110
         ST    R14,NCBMACAD            Store into NCB              v110
         B     XITDIR                  Exit with RC in R15
*
*
XITDIR   EQU   *
         LR    R5,R15                  Any RC value to R5
         BAL   R14,DEQ000              Release the ENQ
*
         ICM   R1,15,SPLIDMAP          Get spool id bitmap stg addr
         BZ    XITDIR10                Don't have a map
         L     R0,SPLIDLEN             Size of bitmap
         FREEMAIN RU,LV=(0),A=(1)      Free the bitmap
         XC    SPLIDMAP,SPLIDMAP       Clear unsed ptr
*
XITDIR10 EQU   *
         L     R13,4(,R13)             -> caller's sa                   NJE00210
*
         ST    R5,16(,R13)             Set RC in R15
         LM    R14,R12,12(R13)         Reload callers's regs            NJE00220
         BR    R14                     Return                           NJE00240
*                                                                       NJE00290
         LTORG
         DROP  R12
*                                                                       NJE00290
****  Main work area common                                             NJE00290
****  to all NJExxx CSECTs.                                             NJE00290
*                                                                       NJE00290
NJEWK    DSECT
NJEEYE   DS    CL4'NSPL'           Eyecatcher
NJEWKLEN DS    F                   Getmain size of this area
NSOWN    DS    A                   -> TCB of caller
ANJECMN  DS    A                   -> NJECNM common csect               NJE00320
*
DBLE     DS    D                   Work area                            NJE00310
TWRK     DS    2D                  Work area
*
MACLIST  DS    XL160               Macro expansion area
*
SV14     DS    A                   R14 save area
SV14GB   DS    A                   R14 save area
SV14B0   DS    A                   R14 save area
SVGB     DS    4F                  R1-R4 save area
SPLIDLEN DS    F                   Length of spool id bitmap stg
SPLIDMAP DS    A                   -> Spool file id bitmap
SPLID    DS    F                   Last assigned spool id number
LISTLEN  DS    F                   Length of contents stg area
LISTADDR DS    A                   -> directory contents stg area
*
BLOCK    DS    A                   -> buffer for NETSPOOL VSAM i/o
BLOCKEND DS    A                   -> end of BLOCK (BLOCK+4089)
PTRBUF   DS    A                   -> buffer for NJESPOOL ptr use
PTRBUFEN DS    A                   -> end of PTRBUF (PTRBUF+4089)
BUFF     DS    A                   -> buffer for NJESPOOL use
BUFFEND  DS    A                   -> end of BUFF (BUFF+4089)
*
*
INITBLK  DS    F                   Blk # of first block to be written
*                                   for a new file
PTRBLK   DS    F                   Blk # of current phys record for
*                                   pointer block (NCBGET/NCTPUT)
NEWBLK   DS    F                   Blk # of current phys record for
*                                   logical i/o (NCBGET/NCTPUT)
PUTPOS   DS    A                   Current write position in BUFF (next
*                                   available write position)
GETPOS   DS    A                   Current read position in BLOCK (next
*                                   available read position)
PTRPOS   DS    A                   Current write position in PTRBUF
*                                   (next available write position)
PUTCNT   DS    F                   Number of logical records written
GETCNT   DS    F                   Number of logical records read
GETLIM   DS    F                   Max logical records in GET file
*
KEY      DS    F                   Relative block number key
ACBL     DS    F                   ACB length
ACB      DS    A                   -> ACB
RPLL     DS    F                   RPL length
RPL      DS    A                   -> RPL
*
NJFL1    DS    X                   Flag bits
NJF1OACB EQU   X'80'  1... ....     NETSPOOL ACB is open
NJF1ENQ  EQU   X'40'  .1.. ....     Exclusive control of NETSPOOL
NJF1WPND EQU   X'20'  ..1. ....     Physical write is pending
NJF1DPND EQU   X'10'  ...1 ....     Directory add is pending
NJF1PUT  EQU   X'02'  .... ..1.     Processing PUTs to file
NJF1GET  EQU   X'01'  .... ...1     Processing GETs from file
*                     .... xx..     Available
*
NJFL2    DS    X                   Flag bits
NJFL3    DS    X                   Flag bits
NJFL4    DS    X                   Flag bits
*
*
*
*
NJESA    DS    18F                     NJESPOOL OS save area            NJE00300
NJEDIRSA DS    18F                     NJEDIR   OS save area            NJE00300
*
         DS    0D                      Force doubleword size
NJEWKSZ  EQU   *-NJEWK
*                                                                       NJE00930
*
BLKONE   DSECT                      ** Maps block #1 in NETSPOOL
DIRBLK   DS    F                       Block number of current directry
ALMBLK   DS    F                       Block number of allocation map
MAXBLK   DS    F                       Highest block number in NETSPOOL
SPLNUM   DS    F                       Last assigned spool file #
BLKONESZ EQU   *-BLKONE                Size of dsect
*                                                                       NJE00930
*
TYPPRT   EQU   X'40'                PRT dev
TYPPUN   EQU   X'80'                PUN dev
         COPY  NETSPOOL
         COPY  TAG
*
         IFGACB
         IFGRPL
*
         END   NJESPOOL                                                 NJE01000
./ ADD NAME=NJEINIT
*
*
*-- NJE38 - Initialization and start up
*
*
*
* Change log:
*
*
* 03 Mar 22 - Avoid 0C4 if no links in CONFIG, APF check, F NJE.   v230
* 10 Dec 20 - Support for registered users and message queuing     v220
* 04 Dec 20 - Expanded internal trace table support                v212
* 29 Nov 20 - Use text-based configuration; alternate routes       v211
* 02 Oct 20 - Use actual length for MGCR SEND cmds                 v210
* 01 Oct 20 - Put ENQ existence check in common module             v210
* 10 Aug 20 - Use single NJESPOOL load for all STC NJE38 modules.  v210
* 22 Jul 20 - Make non-swappable to eliminate long-wait delays     v200
* 21 Jul 20 - Slightly delay auto-start of links on start-up.      v200
* 02 Jul 20 - Default userid to CSA in support of TRANSMIT/RECEOVE v200
* 20 May 20 - Dont pass new file WREs for local node to cmd proc'g v120
* 05 May 20 - Abend SD23 if SVC 34 parmlist >=130 bytes.           v102
* 04 May 20 - Show CONFIG assembly date and time on start up.      v102
*
*
*
*
*
*
         PRINT GEN
         REGEQU                        REGISTER EQUATES
         GBLC  &VERS
*
* User abend codes
*  U0038 - Unsupported/unrecognized CIB
*  U0039 - VSAM error on NETSPOOL
*
* MSG numbers used:
*
*     0-34      used
*     35 - 39   available
*     42-79     used
*     163       used
*
*-- Program limits
*
TRACESZ  EQU   64                      Size in K of trace table    v212
RQELIM   EQU   256                     # of preallocated RQEs
*
*
NJEINIT  CSECT
         NJEVER
         STM   R14,R12,12(R13)         SAVE CMS REGS
         LR    R12,R15                 BASE
         USING NJEINIT,R12             ADDRESS IT
*
         GETMAIN RU,                   Get local stg area              X
               LV=4096,                                                X
               BNDRY=PAGE
         LR    R10,R1
         LR    R1,R0                   Copy length
         LR    R2,R0                   Copy length
         LR    R0,R10                  -> new stg area
         SR    R15,R15                 set pad
         MVCL  R0,R14                  Clear the page
*
         USING NJEMWK,R10
         ST    R13,NJESA+4             SAVE prv S.A. ADDR
         LA    R1,NJESA                -> my save area
         ST    R1,8(,R13)              Plug it into prior SA
         LR    R13,R1
*
         MVC   NJEEYE,=CL4'NJEM'       Work area eyecatcher
         ST    R2,NJEWKLEN             Save size of area in area
*
         L     R11,=A(NJECOM)          -> common csect
         USING NJECOM,R11
         ST    R11,ANJECOM             Save in main work area
         MVC   CMDBLNK,BLANKS          Init field
         MVC   RELAYID,=CL8'RELAY'     Set RELAY entity id         v220
         LA    R1,LINKS                -> LINKTABL anchor word     v211
         ST    R1,ALINKS               Plug it into param list     v211
         LA    R1,ROUTES               -> RTE anchor word          v211
         ST    R1,AROUTES              Plug it into param list     v211
         LA    R1,AUTHS                -> AUTHLIST anchor word     v211
         ST    R1,AAUTHS               Plug it into param list     v211
         LA    R1,REGUSER              -> REGUSER anchor word      v220
         ST    R1,AREGUSER             Plug it into param list     v220
*
INIT000  EQU   *                                                   v200
         SR    R1,R1                   Dont return spool DSN       v210
         L     R15,=V(NJESYS)          -> ENQ finder               v210
         BALR  R14,R15                 Check if NJE38 already act  v210
         LTR   R15,R15                 Look for RC=0=ENQ was found v210
         BZ    ERR999                  Branch if NJE38 active      v210
*
         MVC   MACLIST(WTOMSGL),WTOMSG
         MVC   MACLIST+4(L'NJE000I),NJE000I   NJE38 v xx.xx
         WTO   ,MF=(E,MACLIST)
*
         TESTAUTH FCTN=1               Are we authorized on entry? v230
         LTR   R15,R15                 Check result                v230
         BZ    INIT005                 Branch if authorized        v230
         WTO   'NJE034I NJE38 is not APF-authorized'               v230
         B     QUIT000                                             v230
*
INIT005  EQU   *                                                   v230
         SR    R1,R1                                               v200
         SYSEVENT TRANSWAP                                         v200
         CLM   R1,1,=X'00'             SYSEVENT RC=0?              v200
         BE    INIT010                 Yes                         v200
         WTO   'NJE032I NJE38 could not enter non-swappable state' v200
         B     INIT020                                             v200
*
INIT010  EQU   *                                                   v200
         WTO   'NJE031I NJE38 is non-swappable'                    v200
*
INIT020  EQU   *                                                   v200
         MVC   MACLIST(ESTAEL),ESTAE   Move ESTAE parm list
         L     R6,=A(NJEDMP)           Point to local ESTAE rtn
         ESTAE (R6),                   Issue ESTAE                     X
               CT,                                                     X
               TERM=YES,                                               X
               PARAM=(R10),            PARAM is work area address      X
               MF=(E,MACLIST)
*
*-- Scan the configuration and build control blocks
*
         MODESET MODE=SUP
         SR    R0,R0                   R0=0 scan entire configuration
         LA    R1,INITPARM             -> parm list to pass to NJESCN
         L     R15,=V(NJESCN)
         BALR  R14,R15
         LTR   R15,R15
         BNZ   QUIT000
*
         L     R1,LINKS                Get LINKTABL anchor         v210
         USING LINKTABL,R1
         MVC   LCLNODE,LINKID          Set LCLNODE in param list   v210
         DROP  R1
*
*-- Issue STIMER for keep alive to avoid S 522 abends
*
         L     R0,=A(NJETMR)           -> Timer expiration exit
         L     R1,=A(INTVL)            -> interval
         STIMER REAL,                  Set timer                       X
               (0),                                                    X
               DINTVL=(1)
*
         LOAD  EP=NJESPOOL             Load spool interface        v210
         ST    R0,ANJESPL              Store entry addr            v210
*
         LOAD  EP=NJECMX               Load command processor
         ST    R0,ANJECMX              Store entry addr of processor
*
         BAL   R14,NET000              Check NETSPOOL status
         BNZ   QUIT000                 Exit if NETSPOOL is not ready
*
INIT030  EQU   *
         MODESET MODE=SUP,KEY=ZERO
         L     R1,PSATOLD-PSA(0)                                   v230
         L     R1,TCBJSCB-TCB(,R1)                                 v230
         L     R1,JSCBCSCB-IEZJSCB(,R1)                            v230
         USING CSCB,R1                                             v230
         MVC   CHUNIT(3),=C'NJE'                                   v230
         DROP  R1                                                  v230
*
         STIDP CPUID                   Get the CPU ID
*
         GETMAIN RU,                   Get CSA communication area      x
               LV=NJ38CSAZ,                                            x
               SP=241
*
         ST    R1,CSABLK               Save addr of CSA stg area
         USING NJ38CSA,R1
         XC    0(NJ38CSAZ,R1),0(R1)    Clear area
         MVC   NJ38NODE,LCLNODE        Local node name to CSA
         MVC   NJ38DUSR,DEFUSER        Default userid to CSA       v200
         MVC   NJ38ASCB,PSAAOLD-PSA(0) Move ASCB addr of this space
         LA    R2,NJ38ECB              -> cross memory ECB
         ST    R2,CSAECBAD             Save address locally
         DROP  R1                      NJ38CSA
*
         SPKA  X'80'                    Back to user key
*
         MVC   NJERNAME(8),NJERCON     Set rname constant
         MVC   NJERNAME+8(4),CSABLK    CSA stg addr to Rname
*                                      JFCB DSN should already be here
         LA    R5,NJERNAME
         MVC   MACLIST(ENQL),ENQ       Move macro model
*
         ENQ   (NJE38Q,(5),E,56,SYSTEM),                               x
               RET=NONE,                                               x
               MF=(E,MACLIST)
         OI    NJFL1,NJF1ENQ           Set NJE38 ENQ active
*
         GETMAIN RU,                   Preallocate RQE storage         x
               LV=RQESZ*RQELIM
         ST    R1,ARQESTG              Save the address
         LR    R2,R1                   Copy length
         LR    R1,R0                   Copy length
         LR    R0,R2                   -> new stg area
         SR    R15,R15                 set pad
         MVCL  R0,R14                  Clear the stg
         LA    R0,RQELIM               Get RQE limit
         ST    R0,RQENUM               Save the value
*
*
*- Build trace table                                               v212
*
         GETMAIN RU,                   Get stg for trace table     v212X
               LV=TRACESZ*1024,                                    v212X
               BNDRY=PAGE                                          v212
         ST    R1,ATRACE               Save ptr to trace table     v212
         MVC   0(5,R1),=CL5'TRACE'                                 v212
         MVI   5(R1),C'T'              So eyecatcher TRACETAB      v212
         MVI   6(R1),C'A'               wont show in a dump        v212
         MVI   7(R1),C'B'                in this load module       v212
         USING TRCCTL,R1                                           v212
         ST    R1,TRCSTRT              Set start                   v212
         ST    R1,TRCCURR              Set current                 v212
         AR    R0,R1                   -> end                      v212
         ST    R0,TRCEND               Set end                     v212
         L     R15,=A(NJETRC)          -> Trace CSECT              v212
         ST    R15,TRCRTN              Set trace routine EPA       v212
         DROP  R1                                                  v212
*
*
*-- Initialize console processing to allow MVS modify and stop
*-- commands to control this address space
*
INIT040  EQU   *
         MVC   MACLIST(EXTRACTL),EXTRACT Move macro model
         LA    R3,COMMAREA             -> area to place comm area addr
         EXTRACT (3),                  Get ptr to comm area            X
               FIELDS=COMM,                                            X
               MF=(E,MACLIST)
*
         L     R3,COMMAREA             -> ptrs to COMM CIB and ECB
         USING IEZCOM,R3               Map the communication area
         MVC   COMMECBA,COMECBPT       Save off addr of COMM ECB
         ICM   R4,15,COMCIBPT          Get addr of CIB ptr
         BZ    INIT060                 No CIB, go get one
         USING CIBNEXT,R4              Map the CIB
*
         CLI   CIBVERB,CIBSTART        Is this a START CIB?
         BNE   INIT060                 No, set up CIB count
*
         QEDIT ORIGIN=COMCIBPT,        Free the CIB from the START cmd X
               BLOCK=(4)                that started this space
*
INIT060  EQU   *
         QEDIT ORIGIN=COMCIBPT,        Set CIB limit to 1              X
               CIBCTR=1
         DROP  R4                      IEZCIB
         DROP  R3                      IEZCOM
*
*
*
*- Initialization Completed
*
INIT090  EQU   *
         MVC   MACLIST(WTOMSGL),WTOMSG
         MVC   MACLIST+4(L'NJE001I),NJE001I   Move msg text
         MVC   MACLIST+51(8),LCLNODE
         WTO   ,MF=(E,MACLIST)
*
*- Start any auto-startable links
*
*
         L     R2,LINKS                -> 1st entry (LOCAL entry)  v211
         USING LINKTABL,R2
         ICM   R2,15,LNEXT             -> first remote link        v22x
         BZ    MAIN000                 No auto if no links         v22x
*
AUTO000  EQU   *
         TM    LFLAG,LAUTO             Is link autostartable?
         BZ    AUTO010                 No
         BAL   R14,SLNK000             Try to start the link
*
         STIMER WAIT,DINTVL=ATTDLY     Pause briefly               v200
*
AUTO010  EQU   *
         ICM   R2,15,LNEXT             -> next LINKTABL entry
         BNZ   AUTO000                 Look for another link
         DROP  R2                      LINKTABL
*
*
*
MAIN000  EQU   *
         BAL   R14,BLDL000             Go build the ECB list
         BZ    QUIT000                 No ECBS in list; terminate
*
         SPKA  0                       Use key 0 for CSA ECB
         WAIT  1,ECBLIST=ECBLIST
*
*-- Identify the ECB that was posted
*
MAIN010  EQU   *
         LA    R1,ECBLIST              -> our ECBLIST
*
MAIN050  EQU   *
         ICM   R2,15,0(R1)             -> ECB                      v211
         BZ    MAIN055                 Skip ECB if empty slot      v211
         TM    0(R2),X'40'             Was this ECB posted?
         BO    MAIN060                 Yes
*
MAIN055  EQU   *                                                   v211
         TM    0(R1),X'80'             Last ECB addr in list?
         BO    MAIN000                 Nothing to do, go WAIT
         LA    R1,4(,R1)               -> next ECB addr
         B     MAIN050                 Keep looking
*
*
MAIN060  EQU   *
         CLM   R2,7,CSAECBAD+1         Was the WRE work ECB posted?
         BE    WRK000                  Hey!  We have something to do
*
         SPKA  X'80'                   Back to user key for the rest
         CLM   R2,7,COMMECBA+1         Was the COMM ECB posted?
         BE    COMM000                 Yes
*
***      L     R3,0(,R2)               Load the ECB content        v211
         XC    0(4,R2),0(R2)           Clear the ECB
         LA    R0,LTRMECB-LINKTABL     Offset of ECB in LINKTABL   v211
         SR    R2,R0                   -> LINKTABL entry           v211
         USING LINKTABL,R2
***      CLM   R3,7,=AL3(255)          ECB post code 255?          v211
***      BE    MAIN080                 Yes, LINKTABL entry delete  v211
*
         DETACH LTCBA                  Detach the subtask
         XC    LTCBA,LTCBA             Mark task terminated
         MVI   LFLAG,X'00'             Clear status flags
*
         MVC   MACLIST(WTOMSGL),WTOMSG
         MVC   MACLIST+4(L'NJE010I),NJE010I   Line is drained
         UNPK  DBLE(4),LACTLINE(3)     Convert CUU of line
         TR    DBLE(3),HEXTRAN-240
         MVC   MACLIST+17(3),DBLE
         WTO   ,MF=(E,MACLIST)         Line xxx is drained
         B     MAIN010                 Look for more work
*
*-- Here to delete a LINKTABL entry (from LINK OFF command)        v211
*-- We arrive here from POST code 255. NJESCN LOFF000 does the POSTv211
*
         DROP  R2                      LINKTABL                    v211
*
*-- Build a new ECBLIST before the wait
*
BLDL000  EQU   *
         SR    R1,R1                   Init: no ECBs in list
         LA    R15,ECBLIST-4           -> 0th ECB list entry
         TM    NJFL1,NJF1STOP          Is main task termination set?
         BO    BLDL010                 Yes, dont add COMM ECBs to list
         LA    R15,4(,R15)             -> next available ECB list slot
         L     R1,COMMECBA             -> COMM ECB
         ST    R1,0(,R15)              Set addr in ECB list
         LA    R15,4(,R15)             -> next available ECB list slot
         L     R1,CSAECBAD             -> WRE work ECB
         ST    R1,0(,R15)              Set addr in ECB list
*
BLDL010  EQU   *
         L     R2,LINKS                -> 1st entry (LOCAL entry)  v211
         USING LINKTABL,R2
         L     R2,LNEXT                -> first remote link        v211
*
BLDL020  EQU   *
         CLC   LTCBA,=A(0)             Is task active for link?
         BE    BLDL030                 Zero, skip this one
         LA    R15,4(,R15)             -> next available ECB list slot
         LA    R1,LTRMECB              -> task's termination ECB
         ST    R1,0(,R15)              Set ECB addr in ECB list
*
BLDL030  EQU   *
         ICM   R2,15,LNEXT             -> next LINKTABL entry
         BNZ   BLDL020                 Scan them all
         DROP  R2                      LINKTABL
         LTR   R1,R1                   Any ECB in the list?
         BZR   R14                     No, return with CC=0 set
         OI    0(R15),X'80'            Mark end of list
         BR    R14                     Return with ECB list built
*
**********************************************************************
*                                                                    *
*                           WRE FLOWS                                *
*                                                                    *
**********************************************************************
*
* When WREs are created by out-of-address space tasks (such as by
* modules NJE38 by TSO users, or NJ38XMIT by jobs) they are
* created in CSA and chained off the NJE38 CSA block NJ38CSA.  The
* WRE ECB is posted via cross memory post.  Any WRE posted in this
* manner will first end up here, at WRK000 below.
*
* WRK000 will pull the entire chain of WREs and get it off that queue
* so that these can be processed one at a time while outside tasks may
* continue to add new WREs to the CSA chain.
*
* Each WRE is examined for its destination.  If the WRE has a
* destination link id in the LINKs table, or via a route that can be
* forwarded via a destination link, the WRE will be requeued to that
* particular link task at WRK120.
*
*  When the link task gets the WRE, it will be processed by NJEDRV
*  label COMM000, which will dequeue it and flow continues to
*  label WRK000 in that same module.  After processing the WRE stg
*  is freed.
*
* Back in NJEINIT, if the WRE is destined for the local link (at
* WRK030) flow proceeds to WRK200 where the command processor NJECMD
* is called to examine and process the action.  Upon return, the
* WRE storage is freed and the next WRE on the chain is examined,
* if any.
*
* Notes:
*  1. WREs are created in subpool 2 which is shared by other TCBs.
*       (Except for out-of-address-space WREs, which are in CSA).
*  2. WREs are sometimes created internally:
*     a). in NJEINIT STOP000 to queue a WRE to each active link task
*         in order to stop the link.
*     b). in NJEINIT CCD000 in order to queue a command that was
*         input from the system console to a remote link task.
*  3. Whether the WRE is created from an outside address space or
*     internally, they all flow the same way, via the post to the
*     ECB in NJ38CSA and being placed on the queue anchor in NJ38CSA.
*
*
*
* Summary:
*
*  1. WRE gets created and posted to CSA anchor
*  2. NJEINIT WRK000 sees the WRE first
*  3. WRE is requeued to a link or handled by NJEINIT/NJECMD
*  4. WRE is freed.
*
*
*
*
*
*
*
*-- WRE work ECB was posted
*
WRK000   EQU   *
         SPKA  0                       This routine must run key=0
         XC    0(4,R2),0(R2)           Reinit WRE work ECB
         L     R2,CSABLK               -> CSA communications area
         USING NJ38CSA,R2
*
         LM    R6,R7,NJ38SWAP          Get WRE anchor, sync count
*
WRK010   EQU   *
         LTR   R6,R6                   Was WRE Q empty?
         BZ    MAIN010                 Yes, nothing else to do
         SR    R14,R14                 Zero out the WRE Q anchor
         LR    R15,R7                  Copy same sync count
         CDS   R6,R14,NJ38SWAP         Try to empty the WRE Q
         BC    7,WRK010                Can't yet, try again
         DROP  R2                      NJ38CSA
*
*-- Distribute the WREs to the various links
*
*-- R6 -> start of WRE chain we dequeued from WRE Q
*
         USING WRE,R6
*
*
WRK030   EQU   *
         NJETRACE TYPE=TRCIWRE     Trace incoming WRE
         STCM  R10,7,1(R14)            Identify trace entry        v220
         LA    R15,*               -> here                         v220
         ST    R15,4(,R14)         Save addr of trace request      v220
         ST    R6,8(,R14)          Trace WRE addr                  v220
         MVC   12(4,R14),WRETYPE   Trace type code,len,subpool     v220
         MVC   16(8,R14),WRELINK   link dest                       v220
         MVC   24(8,R14),WREUSER   userid dest                     v220
         NJETRACE TYPE=TRCIWRE     Trace incoming WRE follow on    v220
         OI    0(R14),X'80'        Indicate follow on              v220
         STCM  R10,7,1(R14)            Identify trace entry        v220
         MVC   4(8,R14),WREORIG    Originator userid               v220
         MVC   12(20,R14),WRETXT   Trace WRE content               v220
*
         CLC   WRELINK,LCLNODE         Is this WRE for the local node?
         BE    WRK200                  Yes, don't queue it to a link
*
WRK040   EQU   *
         LA    R1,WRELINK              -> destination link of WRE
         BAL   R14,FLNK000             Locate the LINKTABL entry
         BZ    WRK050                  No link found, check routes
*
         USING LINKTABL,R2
         TM    LFLAG,LCONNECT          Is link connected?
         BO    WRK120                  Yes, post the link task
*
*-- Otherwise, look at routes.  R1-> WRELINK
*
WRK050   EQU   *
         BAL   R14,RLNK000             Find matching route
         BZ    WRK150                  No matching routes
         BAL   R14,FLNK000             Locate the LINKTABL entry
         BZ    WRK150                  No link found for this WRE
         TM    LFLAG,LCONNECT          Is link connected?
         BZ    WRK150                  No, skip this WRE
*
*
*-- Here to requeue the WRE to the link WRE chain
*
WRK120   EQU   *
         NJETRACE TYPE=TRCOWRE     Trace outgoing WRE
         STCM  R10,7,1(R14)            Identify trace entry        v220
         LA    R15,*               -> here                         v220
         ST    R15,4(,R14)         Save addr of trace request      v220
         ST    R6,8(,R14)          Trace WRE addr                  v220
         MVC   12(4,R14),WRETYPE   Trace type code,len,subpool     v220
         MVC   16(8,R14),WRELINK   link dest                       v220
         MVC   24(8,R14),WREUSER   userid dest                     v220
         NJETRACE TYPE=TRCOWRE     Trace outgoing WRE follow on    v220
         OI    0(R14),X'80'        Indicate follow on              v220
         STCM  R10,7,1(R14)            Identify trace entry        v220
         MVC   4(8,R14),WREORIG    Originator userid               v220
         MVC   12(20,R14),WRETXT   Trace WRE content               v220
*
         L     R8,WRENEXT              -> next WRE in CSA chain
*
         LM    R0,R1,LWRESWAP          Get first WRE ptr, sync count
WRK130   EQU   *
         ST    R0,WRENEXT              First WRE becomes next
         LR    R4,R6                   -> WRE to be added as first
         LA    R5,1(,R1)               Incr synchronization count
         CDS   R0,R4,LWRESWAP          Update LINK WRE anchor, sync
         BC    7,WRK130                Gotta try again
*
         LA    R1,LECB                 -> link task notification ECB
         POST  (1)                     Tell task
         B     WRK290                  Go get another WRE
*
*-- Release WRE that we cant distribute to a link
*
WRK150   EQU   *
         B     WRK290
         DROP  R2                      LINKTABL
*
*-- Here if WRE is intended for the local node
*
WRK200   EQU   *
         SR    R15,R15                 Clear for IC                v220
         IC    R15,WRETYPE             Get WRE type code           v220
         CLM   R15,1,=AL1(WRK210HI)    Check against highest code  v220
         BH    WRK280                  Dispose of invalid WRE      v220
         B     WRK210(R15)             Branch into table           v220
*
WRK210   EQU   *                                                   v220
         B     WRK280            X'00'  Invalid; just delete WRE   v220
         B     WRK280            X'04'  WRENEW; ignore for LCL nodev220
         B     WRK215            X'08'  WRECMD                     v220
         B     WRK220            X'0C'  WREMSG                     v220
         B     WRK240            X'10'  WRESTAR                    v220
         B     WRK300            X'14'  WREREG                     v220
         B     WRK350            X'18'  WREDREG                    v220
         B     WRK400            X'1C'  WREQRM                     v220
         B     WRK450            X'20'  WREDRM                     v220
WRK210HI EQU   (*-WRK210-4)      Highest code supported            v220
*
*
WRK215   EQU   *
         SPKA  X'80'
         MVC   CMDAREA,BLANKS          Init receiving area
         SR    R2,R2                   Clear for IC
         IC    R2,WRETXTLN             Get cmd image length
         EX    R2,MVTXT1               Move cmd image
         STC   R2,CMNDBLEN             IBM length of image to CMDBLOK
         MVC   CMNDLINK,LCLNODE        This node is the issuer
         MVC   CMNDUSER,WREUSER        Copy TSO id of issuer
*
         L     R15,=A(NJECMD)          -> command processor
         BALR  R14,R15                 Go there
         SPKA  X'00'
         B     WRK280
*
MVTXT1   MVC   CMDAREA(0),WRETXT       Executed instr
*
*-- Send the msg response to a local TSO user
*
WRK220   EQU   *
         CLC   WREUSER,=CL8'OP'        Message destined for operator?
         BE    WRK230                  Yes
         LA    R15,WREUSER              -> userid to locate
         BAL   R14,REG000               See if user registered     v220
         BNZ   WRK280                   Yes it was; we queued it   v220
         BAL   R14,USR800               See if TSO user logged on
         BZ    WRK280                   Skip msg if not
         MVC   MACLIST(80),BLANKS       Init first part
         MVC   MACLIST+4(9),=C'SE ''From '
         MVC   MACLIST+13(8),WREORIG
         TRT   MACLIST+13(9),BLANK     Look for end of orig userid
         MVI   0(R1),C':'
         LA    R1,2(,R1)               -> area for msg
         MVC   0(104,R1),WRETXT        Move msg text               v102
         LA    R2,MACLIST+111      -> last byte from MTEXT area    v210
         LA    R0,32               # char to check backwards       v210
*
WRK223   EQU   *                   Only look backwards to col 80   v210
         CLI   0(R2),C' '          Try to find last non-blank      v210
         BNE   WRK226              Found it                        v210
         BCTR  R2,0                -> prev char                    v210
         BCT   R0,WRK223           Keep scanning                   v210
*
WRK226   EQU   *                                                   v210
         LA    R2,1(,R2)           -> first blank after last char  v210
         MVC   0(8,R2),=C''',USER=('                               v210
         MVC   8(12,R2),BLANKS      Ensure trailer initted         v210
         MVC   8(7,R2),WREUSER      Max for TSO userid is 7        v210
         LA    R1,8+7(,R2)          -> max end of trt              v210
         TRT   8(7,R2),BLANK        Look for end of userid         v210
         MVI   0(R1),C')'           Move closing                   v210
         MVI   1(R1),C' '           Plus 1 blank                   v210
         LA    R0,MACLIST           -> start of msg area           v210
         SR    R1,R0                Compute length of msg          v210
         LA    R1,1(,R1)            Account for blank at end       v210
         XC    MACLIST(4),MACLIST   Clear len, flags               v210
         STH   R1,MACLIST           Insert the msg length          v210
*
         LA    R1,MACLIST
         SR    R0,R0
         SVC   34                      Issue MGCR SVC
         B     WRK280
*
*-- Send the msg response to the system operator
*
WRK230   EQU   *
         MVC   MACLIST(WTOMSGL),WTOMSG
         MVC   MACLIST+4(4),=C'From'
         MVC   MACLIST+9(8),WREORIG    Move originating userid
         TRT   MACLIST+9(9),BLANK      Look for end of orig userid
         MVI   0(R1),C':'
         LA    R1,2(,R1)               -> area for msg
         MVC   0(104,R1),WRETXT        Move msg text               v102
         WTO   ,MF=(E,MACLIST)
         B     WRK280
*
*-- Start a link (via a local or remote command)
*
WRK240   EQU   *
         L     R2,WREUSER              -> LINKTABL entry of START cmd
         BAL   R14,SLNK000             Attach the link driver
         B     WRK280
*
*-- Clean up spent WRE
*
WRK280   EQU   *
         SPKA  0                       In case WRE isin CSA        v220
         L     R8,WRENEXT              -> next WRE in chain
         SR    R15,R15                 Clear for IC                v220
         IC    R15,WRESP               Get subpool number          v220
         LA    R0,WRESIZE              Size of this WRE            v220
*
         NJETRACE TYPE=TRCFWRE                                     v220
         STCM  R10,7,1(R14)            Identify trace entry        v220
         LA    R2,*                                                v220
         STCM  R2,7,5(R14)             Addr of Freemain to trace   v220
         ST    R0,8(,R14)              Len to trace                v220
         ST    R6,12(,R14)             addr to trace               v220
         STC   R15,8(,R14)             Trace subspool              v220
         MVI   WRESP,X'FF'             Mark stg as freed           v220
*
         FREEMAIN RU,                  Free the  WRE                   x
               LV=(0),                                                 x
               A=(6),                                                  x
               SP=(15)                                             v220
         SPKA  X'80'                                               v220
*
*-- Done processing a WRE; get another
*
WRK290   EQU   *
         LTR   R6,R8                   Get next WRE to distribute
         BNZ   WRK030                  Yes have an addr            v220
         B     MAIN010                 All done with WREs
*
*
* Registered User Service Support Notes                            v220
*
* The registered user service allows an outside address space
* operating in the same MVS system as NJE38, to 'register' or
* establish a relationship with NJE38 where messages that would
* ordinarily be sent to a user terminal are instead queued in
* storage and presented to the outside address space upon request.
*
* Users wishing to use this service call the NJERLY interface which
* is responsible for establishing the relationship with NJE38. This
* is done using WREs and cross-memory POST.  In this way, a batch,
* TSO, or STC address space can capture message traffic destined
* for it before it would arrive at a terminal, and thereby process
* this message or display it in the manner of their choosing.
*
* WREs created by NJERLY are always in CSA.  When they are used to
* request service of NJE38, they place the WRE on the NJ38SWAP
* compare and swap chain just like any other outside requester and
* post NJEINIT's CSA ECB.  NJEINIT then acts on the request.
*
* NJEINIT never frees the WRE created by NJERLY.  That is NJERLY's
* responsibility.
*
* For some functions of the service, the request is ignored if
* important information is missing (unlikely) such as ASCB address
* of NJERLY, or the WRE address.  Ignoring the request is all that
* can be done since without either of those pieces, NJEINIT cannot
* issue CM POST back to the NJERLY space to let it know of the error.
*
* When a user joins the service, he registers.  NJEINIT will create
* a REGUSERB control block to establish the registration and hold
* the NJERLY requester'e WRE and ASCB address.
*
* Once a user (userid) has registered, any message traffic inbound
* destined for that user will be queued in NJE38 storage and chained
* from REGUSERB,  The user can then request a message be returned
* one per request.  A post code of 4 (ERNOMSG) is used to indicate
* no messages are queued.
*
* When the user wants to stop using the service, it 'deregisters',
* causing NJEINIT to freemain any queued messages for the user and
* releasing the REGUSERB.  Message traffic destined for that user
* resumes being presented to the terminal as before.
*
* In the comments below, the 'registered user WRE' refers to the
* WRE created by NJERLY in CSA by the user address space.
*
*
*
*- WREREG
*- Register a user for queued message services
*
*- Who requests this service: user address space via NJERLY
*
*- Steps:
*    1. Ensure userid is not already registered on REGUSERB chain.
*    2. Create a new REGUSERB for this user
*    3. Issue CM POST to registered user space, function complete.
*
*
*  Notes: - On entry, registered user WRE is in R6.
*         - Registered users WREs are not freemained; we are not the
*            owner.
*         - If the registered user WRE has no ASCB addr, we have no
*            choice but to ignore the request.
*
WRK300   EQU   *
         L     R8,WRENEXT              -> next WRE                 v220
         XC    WRENEXT,WRENEXT         Clear next next ptr because v220
*                                       this is a registration WRE v220
*                                       and wont be freemained herev220
         CLC   WREASCB,=A(0)           Is ASCB present?            v220
         BE    WRK810                  No, invalid. Can't respond  v220
*
         ICM   R1,15,REGUSER           -> first REGUSER            v220
         BZ    WRK320                  None, let's start a chain   v220
         USING REGUSERB,R1                                         v220
         LA    R0,ERDUPUSR             Assume duplicate user error v220
*
WRK310   EQU   *                                                   v220
         CLC   REGUSRID,WREUSER        Is this user already reg?   v220
         BE    WRK800                  Yes, post the error in R0   v220
         ICM   R1,15,REGNEXT           Keep looking                v220
         BNZ   WRK310                                              v220
*
WRK320   EQU   *                                                   v220
         GETMAIN RU,                   Get storage for a REGUSER   v220x
               LV=REGSIZE,                                         v220x
               SP=2                                                v220
         XC    0(REGSIZE,R1),0(R1)     Init stg                    v220
         MVC   REGEYE,=CL4'REGU'       Set eye                     v220
         MVC   REGUSRID,WREUSER        Userid to be registered     v220
         ST    R6,REGWRE               Save ptr to registration WREv220
         MVC   REGNEXT,REGUSER         Chain other REGUSERs to thisv220
         ST    R1,REGUSER              This REGUSER is first       v220
         DROP  R1                      REGUSERB                    v220
         SR    R0,R0                   Set RC=0 success            v220
         B     WRK800                  User successfully registeredv220
*
*- WREDREG
*- Deregister a user from queued message services
*
*- Who requests this service: user address space via NJERLY
*
*- Steps:
*    1. Locate the REGUSERB for the userid
*    2. Get the chain anchor for queued message WREs, if any
*    3. Freemain the REGUSERB.
*    4. Freemain each queued message WRE
*    5. Issue CM POST to registered user space, function complete.
*
*  Notes: - On entry, registered user WRE is in R6.
*         - Registered users WREs are not freemained; we are not the
*            owner.
*         - If the registered user WRE has no ASCB addr, we have no
*            choice but to ignore the request.
*
WRK350   EQU   *
         L     R8,WRENEXT              -> next WRE                 v220
         XC    WRENEXT,WRENEXT         Clear next next ptr because v220
*                                       this is a registration WRE v220
*                                       and wont be freemained herev220
         CLC   WREASCB,=A(0)           Is ASCB present?            v220
         BE    WRK810                  No, invalid. Can't respond  v220
*
         LA    R0,ERUSERNF             Assume user not found       v220
         LA    R2,REGUSER              -> 0th REGUSER entry        v220
         ICM   R1,15,REGUSER           -> first REGUSER            v220
         BZ    WRK800                  None, user indeed isnt foundv220
         USING REGUSERB,R1                                         v220
*
WRK360   EQU   *                                                   v220
         CLC   REGUSRID,WREUSER        Is this user we want?       v220
         BE    WRK370                  Yes                         v220
         LR    R2,R1                   Save this REGUSER ptr       v220
         ICM   R1,15,REGNEXT           Get next REGUSER and continuv220
         BNZ   WRK360                                              v220
         B     WRK800                  Exit with user not found    v220
*
WRK370   EQU   *                                                   v220
         MVC   REGNEXT-REGUSERB(,R2),REGNEXT unchain R1 REGUSER    v220
         L     R2,REGMSGQ              -> MSG WRE chain for user   v220
         DROP  R1                      REGUSERB                    v220
*
         FREEMAIN RU,                  Free storage for a REGUSERB v220x
               LV=REGSIZE,                                         v220x
               A=(1),                                              v220x
               SP=2                                                v220
*
WRK380   EQU   *                                                   v220
         LTR   R1,R2                   Were any WREs chained?      v220
         BZ    WRK390                  No, we're done              v220
         L     R2,WRENEXT-WRE(,R2)     -> next WRE                 v220
         LA    R0,WRESIZE              Get size of WRE             v220
*
         NJETRACE TYPE=TRCFWRE                                     v220
         STCM  R10,7,1(R14)            Identify trace entry        v220
         LA    R15,*                                               v220
         STCM  R15,7,5(R14)            Addr of Freemain to trace   v220
         STM   R0,R1,8(R14)            Len, stg addr to trace      v220
         MVI   8(R14),2                Trace subspool              v220
         MVI   WRESP-WRE(R1),X'FF'     Mark stg as freed           v220
*
         FREEMAIN RU,                  Free storage for a WRE      v220x
               LV=(0),                                             v220x
               A=(1),                                              v220x
               SP=2                                                v220
         B     WRK380                  Free entire chain           v220
*
WRK390   EQU   *                                                   v220
         SR    R0,R0                   Set RC=0 success            v220
         B     WRK800                  User successfully deregisterv220
*
*
*- WREQRM
*- Queue a message destined for a registered user
*
*- Who requests this service: Internal by NJEINIT, NJECMX, NJEDRV
*   as message traffic arrives and needs to be queued.
*
*- Steps:
*    1. Locate the REGUSERB for the userid
*    2. If REGUSERB is not found, userid is not registered. Exit
*        with CC=0 and allow the message to go to the user terminal.
*    3. Get the registration WRE address from REGUSERB, exit if none.
*    4. Add this queued message WRE (in R6) to the queued message
*        chain REGMSGQ (in REGUSERB). Do not freemain this WRE!
*    5. Issue CM POST to registered user space that message is avail.
*
*  Notes: - On entry, a queued message WRE is in R6.
*         - The WREs are added to the start of the chain (REGMSGQ)
*           because they come to us in reverse order of issuance.
*           This puts them back in the right order
*
WRK400   EQU   *
         L     R8,WRENEXT              -> next WRE                 v220
         ICM   R3,15,REGUSER           -> first REGUSER            v220
         BZ    WRK810                  No one registered           v220
         USING REGUSERB,R3                                         v220
*
WRK410   EQU   *                                                   v220
         CLC   REGUSRID,WREUSER        Is this user the one?       v220
         BE    WRK420                  Yes                         v220
         ICM   R3,15,REGNEXT           Keep looking                v220
         BNZ   WRK410                                              v220
         B     WRK810                  Can't find REGUSER          v220
*
WRK420   EQU   *                                                   v220
         ICM   R4,15,REGWRE            -> user's registration WRE  v220
         BZ    WRK810                  Ignore if not there         v220
*
         MVC   WRENEXT,REGMSGQ         Add chain to new WRE        v220
         ST    R6,REGMSGQ              Add WRE to anchor           v220
         LR    R6,R4                   User registration WRE to R6 v220
         SR    R0,R0                   Indicate success            v220
         B     WRK800                  Tell user msg pending       v220
*                                                                  v220
*                                                                  v220
*- WREDRM
*- Dequeue message for a registered user when they request it
*
*- Who requests this service: user address space via NJERLY
*
*- Steps:
*    1. Locate the REGUSERB for the userid
*    2. If REGUSERB is not found, userid is not registered. Issue
*        error to requester.
*    3. Get the first queued message WRE from REGUSERB, issue
*        ERNOMSG error if nothing queued.
*    4. Copy the message text from the queued message WRE into the
*        registered user WRE.
*    5. Issue CM POST to registered user space, function complete.
*
*  Notes: - On entry, the registered user WRE is in R6.
*
*
WRK450   EQU   *
         L     R8,WRENEXT              -> next WRE                 v220
         XC    WRENEXT,WRENEXT         Clear next next ptr because v220
*                                       this is a registration WRE v220
*                                       and wont be freemained herev220
         ICM   R3,15,REGUSER           -> first REGUSER            v220
         BZ    WRK810                  No one registered           v220
         USING REGUSERB,R3                                         v220
*
WRK460   EQU   *                                                   v220
         CLC   REGUSRID,WREUSER        Is this user the one?       v220
         BE    WRK470                  Yes                         v220
         ICM   R3,15,REGNEXT           Keep looking                v220
         BNZ   WRK460                                              v220
         B     WRK810                  Can't find REGUSER          v220
*
WRK470   EQU   *                                                   v220
         LA    R0,ERNOMSG              Assume no msgs queued       v220
         ICM   R5,15,REGMSGQ           -> first queued msg WRE     v220
         BZ    WRK800                  No msgs available           v220
*
         MVC   REGMSGQ,WRENEXT-WRE(R5) Remove 1st queued from chainv220
         DROP  R3                      REGUSERB                    v220
*
         MVC   WRETXT,WRETXT-WRE(R5)   Copy queued msg text to     v220
*                                       registered user WRE        v220
*
         LA    R0,WRESIZE              Get size of WRE             v220
         NJETRACE TYPE=TRCFWRE                                     v220
         STCM  R10,7,1(R14)            Identify trace entry        v220
         LA    R15,*                                               v220
         STCM  R15,7,5(R14)            Addr of Freemain to trace   v220
         ST    R0,8(,R14)              Len to trace                v220
         MVI   8(R14),2                Trace subspool              v220
         ST    R5,12(,R14)             Addr to trace               v220
         MVI   WRESP-WRE(R5),X'FF'     Mark stg as freed           v220
*
         FREEMAIN RU,                  Free Queued msg WRE         v220x
               LV=(0),                                             v220x
               A=(5),                                              v220x
               SP=2                                                v220
*
         SR    R0,R0                   Indicate success            v220
         B     WRK800                  Tell user msg pending       v220
*
*
WRK800   EQU   *                       USING WRE,R6                v220
         L     R7,WREASCB              -> ASCB of requestor        v220
         LA    R1,WREECB               -> WRE's ECB                v220
*
         MVC   MACLIST(POSTL),POST     Move macro model            v220
         POST  (1),(0),                Post requestor's ECB        v220x
               ASCB=(7),                                           v220x
               ERRET=WRK810,                                       v220x
               ECBKEY=0,                                           v220x
               MF=(E,MACLIST)                                      v220
*
WRK810   EQU   *                                                   v220
         B     WRK290                  All done with WRE           v220
         DROP  R6                      WRE                         v220
*
*-- Address space Communications ECB was posted
*
COMM000  EQU   *
         L     R4,COMMAREA             -> Communications area
         USING IEZCOM,R4
         L     R5,COMCIBPT             -> CIB
         USING CIBNEXT,R5
         CLI   CIBVERB,CIBMODFY        Modify cmd?
         BE    MOD000                  Yes
         CLI   CIBVERB,CIBSTOP         Stop cmd?
         BE    STOP000                 Yes, let subtasks know
U0038    ABEND 38,DUMP,STEP            Shouldnt happen
*
MOD000   EQU   *
         MVC   CMDAREA,BLANKS          Init receiving area
         LH    R2,CIBDATLN             Get cmd image length
         BCTR  R2,0                    Adjust for execute
         EX    R2,MVMOD1               Move cmd image
         STC   R2,CMNDBLEN             IBM length of image to CMDBLOK
*
         QEDIT ORIGIN=COMCIBPT,BLOCK=(5)     Purge the CIB
*
         MVC   CMNDLINK,LCLNODE        Console operator
         MVC   CMNDUSER,=CL8'OP'        should get any responses
         L     R15,=A(NJECMD)          -> command processor
         BALR  R14,R15                 Go there
         B     MAIN010
*
MVMOD1   MVC   CMDAREA(0),CIBDATA      Executed instr
*
*
*
STOP000  EQU   *
         QEDIT ORIGIN=COMCIBPT,BLOCK=(5)     Purge the CIB
         DROP  R4                      IEZCOM
         DROP  R5                      IEZCIB
*
STOP010  EQU   *
         OI    NJFL1,NJF1STOP          Indicate STOP ordered
         L     R2,LINKS                -> 1st entry (LOCAL entry)  v211
         USING LINKTABL,R2
         L     R2,LNEXT                -> first remote link        v211
*
STOP020  EQU   *
         CLC   LTCBA,=A(0)             Is task active for link?
         BE    STOP030                 Zero, skip this one
*
         BAL   R14,GTW000              Get a WRE
         LR    R4,R1                   -> WRE
         USING WRE,R4
         MVI   WRECODE,X'81'           Code for drain link
         DROP  R4
         BAL   R14,PST000              Queue the WRE to link
*
STOP030  EQU   *
         ICM   R2,15,LNEXT             -> next LINKTABL entry
         BNZ   STOP020                 Scan them all
         DROP  R2                      LINKTABL
*
         B     MAIN010
*
*
*-- Open then Close NETSPOOL dataset to determine status
*
* NCBRTNCD/ERRCD after call to NCBOPEN
*    0474 = dataset not closed properly (do verify)
*    0874 = dataset not formatted
*
NET000   EQU   *
         ST    R14,SV14                Save return
*
         MVC   JFCBDCB(NSPOOLN),NSPOOL Move DCB for RDJFCB use
         LA    R1,JFCB                 -> JFCB return area
         ST    R1,JEXLST               Set addr in exit list
         MVI   JEXLST,X'87'            Set exlst for JFCB return
         LA    R1,JFCBDCB              -> DCB
         USING IHADCB,R1
         LA    R0,JEXLST               -> exit list
         STCM  R0,7,DCBEXLSA           Store it into DCB
         DROP  R1
*
         MVC   MACLIST(RDJFCBL),RDJFCB Move model
         RDJFCB JFCBDCB,MF=(E,MACLIST) Get NETSPOOL DSN
*
         LA    R3,NCB1
         USING NCB,R3
*
         NSIO  TYPE=OPEN,              Open NETSPOOL                   x
               NCB=(R3),                                           v210x
               ENTRY=ANJESPL                                       v210
         LTR   R15,R15
         BZ    NET040
         BAL   R14,FMT000
*
NET040   EQU   *
         NSIO  TYPE=CLOSE,                                             x
               NCB=(R3),                                           v210x
               ENTRY=ANJESPL                                       v210
         DROP  R3
         TM    NJFL1,NJF1VSER          Did VSAM error occur?
         BZ    NET090                  No
         CLC   LASTRC(2),=X'0474'      NETSPOOL needs verify?
         BE    NET080
         CLC   LASTRC(2),=X'0874'      NETSPOOL not formatted?
         BNE   NET070
         MVC   MACLIST(WTOMSGL),WTOMSG Move macro model
         MVC   MACLIST+4(L'NJE007I),NJE007I  Not formatted msg
         WTO   ,MF=(E,MACLIST)
         B     NET090
*
NET070   EQU   *
         MVC   MACLIST(WTOMSGL),WTOMSG Move macro model
         MVC   MACLIST+4(L'NJE006I),NJE006I  Open failed
         WTO   ,MF=(E,MACLIST)
         B     NET090
*
NET080   EQU   *
         MVC   MACLIST(WTOMSGL),WTOMSG Move macro model
         MVC   MACLIST+4(L'NJE008I),NJE008I  Do verify
         WTO   ,MF=(E,MACLIST)
         MVC   MACLIST(WTOMSGL),WTOMSG Move macro model
         MVC   MACLIST+4(L'NJE009I),NJE009I  verify complete
         WTO   ,MF=(E,MACLIST)
*
NET090   EQU   *
         TM    NJFL1,NJF1VSER          Set CC: Did VSAM error occur?
         L     R14,SV14                Reload return
         BR    R14                     Return
*
ERR999   EQU   *
         WTO   'NJE999I NJE38 is already active'
*
QUIT000  EQU   *
         ESTAE 0                       Turn off ESTAE
*
         TTIMER CANCEL                 Cancel the timer
*
         FREEMAIN RU,SP=1              Free all CONFIG related stg
         FREEMAIN RU,SP=2              Free all WRE related stg
*
QUIT020  EQU   *
         DELETE EP=NJECMX              Delete command processor
         DELETE EP=NJESPOOL            Delete spool interface      v210
*
         ICM   R1,15,ARQESTG           -> RQE stg area
         BZ    QUIT030                 Skip free if none           v212
         FREEMAIN RU,                  Free it                         x
               LV=RQESZ*RQELIM,                                        x
               A=(1)
*
QUIT030  EQU   *                                                   v212
         ICM   R1,15,ATRACE            -> Trace table stg          v212
         BZ    QUIT070                 Skip free if none           v212
         FREEMAIN RU,                  Free it                     v212x
               LV=TRACESZ*1024,                                    v212x
               A=(1)                                               v212
*
QUIT070  EQU   *
         TM    NJFL1,NJF1ENQ           Is NJE38 ENQ active?
         BZ    QUIT080                 No
         LA    R5,NJERNAME             -> RNAME
         MVC   MACLIST(ENQL),ENQ       Move macro model
         DEQ   (NJE38Q,(5),56,SYSTEM),                                 x
               RET=NONE,                                               x
               MF=(E,MACLIST)
*
QUIT080  EQU   *
         ICM   R5,15,CSABLK            -> CSA stg area
         BZ    QUIT090                 Not present
*
         SPKA  0
*
         FREEMAIN RU,LV=NJ38CSAZ,A=(5),SP=241  Free CSA area
         XC    CSABLK,CSABLK
*
         SPKA  X'80'
*
QUIT090  EQU   *
         LR    R1,R10                  -> NJEWK main work area page
         L     R13,4(,R13)             -> caller's sa
         FREEMAIN RU,                                                  x
               LV=4096,                                                x
               A=(1)
         LM    R14,R12,12(R13)         Reload system's regs
         XR    R15,R15                 RC=0
         BR    R14                     Return
*
U0039    EQU   *
         STM   R0,R1,DBLE              Save regs across abend SVC
         ABEND 39,DUMP,STEP
*
         LTORG
*                  HHMMSSTH
         DS    0D                                                  v200
ATTDLY   DC    CL8'00000050'           1/2 sec
*
EXTRACT  EXTRACT MF=L
EXTRACTL EQU   *-EXTRACT
ESTAE    ESTAE 0,MF=L
ESTAEL   EQU   *-ESTAE
*
ENQ      ENQ   (0),MF=L
ENQL     EQU   *-ENQ
*
DEQ      DEQ   (0),MF=L
DEQL     EQU   *-DEQ
*
RDJFCB   RDJFCB 0,MF=L
RDJFCBL  EQU   *-RDJFCB
*
NJE38Q   DC    CL8'NJE38'
NJERCON  DC    CL8'NJEINIT'
*
NSPOOL   DCB   DDNAME=NETSPOOL,DSORG=PS,MACRF=GL,EXLST=0
NSPOOLN  EQU   *-NSPOOL
*
*                456789012345678901234567890123456789012345678901
NJE000I  DC    C'NJE000I NJE38  &VERS'
NJE001I  DC    C'NJE001I Initialization complete for local node'
NJE006I  DC    C'NJE006I Open failed for DD NETSPOOL'
NJE007I  DC    C'NJE007I NETSPOOL dataset has not been formatted'
NJE008I  DC    C'NJE008I The NETSPOOL dataset required verification befx
               ore start-up'
NJE009I  DC    C'NJE009I Verification complete.  Please restart NJE38'
NJE010I  DC    C'NJE010I Line xxx is drained'
*
         DROP  R12
*
*********************
*  N J E C O M      *               NJECOM hosts small routines and
*                   *               frequently used constants that
*  Common routines  *               are available to all NJExxx csects
*  and constants    *               via base register 11
*                   *
*********************
*
NJECOM   CSECT
         DC    A(0)                 No branch around constants
         DC    AL1(23)                LENGTH OF EYECATCHERS
         DC    CL9'NJECOM'
         DC    CL9'&SYSDATE'
         DC    CL5'&SYSTIME'
         USING NJECOM,R11
         USING NJEMWK,R10
*
* FLNK000 - Locate a link table entry by link name
*
*  Entry:  R1 -> Link name to find (CL8 field padded with blanks)
*  Exit:   CC=0 link was not found
*          CC<>0 link table entry address is in R2
*
*
*
FLNK000  EQU   *
         L     R2,LINKS                -> 1st entry (LOCAL entry)  v211
         USING LINKTABL,R2
         L     R2,LNEXT                -> first remote link        v211
*
FLNK010  EQU   *
         CLC   LINKID,0(R1)            Find the link entry by name
         BE    FLNK020                 Got it
         ICM   R2,15,LNEXT             -> next LINKTABL entry
         BZR   R14                     Exit CC=0 if not found
         B     FLNK010                 Keep searching
         DROP  R2                      LINKTABL
*
FLNK020  EQU   *
         LTR   R2,R2                   Set CC non-zero
         BR    R14                     Return w/LINKTABL entry -> R2
*
* RLNK000 - Locate a name in the route table
*
*  Entry:  R1 -> Routed name to find (CL8 field padded with blanks)
*  Exit:   CC=0 link was not found
*          CC<>0 Associated link name address is in R1
*          CC<>0 Named route address is in R15
*
*-- First determine if the route name we are looking up is actually
*-- a link name.
*
RLNK000  EQU   *
         ICM   R15,15,ROUTES       -> RTE list                     v211
         BZR   R14                 Exit CC=0 if no RTE list        v211
         USING RTE,R15                                             v211
*
         L     R2,LINKS            1st entry (LOCAL entry)         v211
         USING LINKTABL,R2
         ICM   R2,15,LNEXT         Skip over local entry           v211
         BZR   R14                 Fail the request if none        v211
         SR    R0,R0               R0=0 assume name not a link     v211
*
RLNK010  EQU   *                                                   v211
         CLC   LINKID,0(R1)        Find the link entry by name     v211
         BE    RLNK020             Got it                          v211
         ICM   R2,15,LNEXT         -> next LINKTABL entry          v211
         BNZ   RLNK010             Keep looking                    v211
         B     RLNK030             Didn't find a matching link     v211
         DROP  R2                  LINKTABL                        v211
*
*-- Here if route we want is a link name too (dont use wildcards)  v211
*
RLNK020  EQU   *                                                   v211
         BCTR  R0,0                Indic route is explicit link nm v211
*                                                                  v211
*-- Search the RTEs for the route name                             v211
*                                                                  v211
RLNK030  EQU   *
         STM   R4,R7,12(R13)       Save work regs                  v211
*
RLNK040  EQU   *                                                   v211
         LA    R4,ROUTNAME         -> name from route list         v211
         LA    R5,8                max length                      v211
         LR    R6,R1               -> selected name to locate      v211
         LR    R7,R5               copy length                     v211
         CLCL  R4,R6               Did we locate the name?         v211
         BE    RLNK400             Yes, exact match                v211
         LTR   R0,R0               Must be explicit link name?     v211
         BNZ   RLNK050             Yes, no wildcard checking       v211
         CLI   0(R4),C'*'          Wildcard was in the name?       v211
         BE    RLNK400             Then we matched to that point   v211
*
RLNK050  EQU   *
         ICM   R15,15,ROUTPTR      -> Next route entry             v211
         BNZ   RLNK040             Keep looking                    v211
         LM    R4,R7,12(R13)       Restore work regs               v211
         BR    R14                 No matching route               v211
*
*-- Found the RTE with a matching name, now determine what link    v211
*-- to route to.                                                   v211
*
RLNK400  EQU   *                                                   v211
         LM    R4,R7,12(R13)       Restore work regs               v211
         LA    R0,4                # possible routed-to names      v211
         LA    R1,ROUTNEXT         -> first possible name          v211
*
RLNK410  EQU   *                                                   v211
         L     R2,LINKS            -> first LINKTABL entry         v211
         USING LINKTABL,R2                                         v211
         ICM   R2,15,LNEXT         Skip over local entry           v211
         BZR   R14                 Fail the request if none        v211
*
RLNK420  EQU   *                                                   v211
         CLC   0(8,R1),BLANKS      No route-to name?               v211
         BE    RLNK499             Fail the request                v211
         CLC   0(8,R1),LINKID      Look for destination link       v211
         BE    RLNK440             Found it                        v211
         ICM   R2,15,LNEXT         -> next LINKTABL entry          v211
         BNZ   RLNK420             Keep searching                  v211
*
RLNK430  EQU   *                                                   v211
         LA    R1,8(,R1)           Next alternate route-to         v211
         BCT   R0,RLNK410          Rescan for matching link        v211
         B     RLNK499             None found, fail the request    v211
*
RLNK440  EQU   *                                                   v211
         TM    LFLAG,LCONNECT      Is the link active?             v211
         BZ    RLNK430             N, try next route-to link       v211
         DROP  R2,R15              LINKTABL, RTE                   v211
*
RLNK490  EQU   *                                                   v211
         CLI   *,0                 Set CC to non-zero              v211
         BR    R14                 Return with link name -> R1     v211
*
RLNK499  EQU   *                                                   v211
         CLI   *+1,0               Set CC to 0                     v211
         BR    R14                 No matching route/act link foundv211
*
* SLNK000 - Start a link
*
*  Entry:  R2 -> LINKTABL entry to be started
*  Exit:   CC=0 link was started
*          CC<>0 link was already started
*
*
*
         USING LINKTABL,R2
SLNK000  EQU   *
         STM   R14,R9,BALRSAVE         Save regs used
         CLC   LTCBA,=A(0)             Is link already started?
         BNE   SLNK090                 Exit w/ CC<>0 if addr present
*
         XC    LTRMECB,LTRMECB         Clear from any prior use
         LA    R1,INITPARM             -> INITPARM mapping area
         ST    R1,LPOINTER             Pass addr of area to subtask
         L     R5,=A(NJEDMP)           -> ESTAI exit
         LA    R9,LTRMECB
         LR    R1,R2                   LINKTABL entry is parameter
*
         MVC   MACLIST(ATTACHL),ATTACH Move macro model
         ATTACH EP=NJEDRV,             Attach                          X
               SZERO=YES,              Ok to share SP 0                X
               SHSPL=SPLIST,           Shared subpool list         v220X
               DPMOD=0,                Run task same prty              X
               SM=SUPV,                Run task in Supervisor state    X
               KEY=PROP,               Run task in key 8               X
               ECB=(R9),               Subtask termination ECB         X
               ESTAI=((5),(10)),       ESTAI exit, work area is param  X
               SF=(E,MACLIST),         Attach macro plist              X
               MF=(E,(1))              Param plist area
*
         ST    R1,LTCBA                Save attached TCB address
         SR    R15,R15                 Set CC=0
         B     SLNK090                 Exit with task attached
         DROP  R2                      LINKTABL
*
SLNK090  EQU   *
         LM    R14,R9,BALRSAVE         Restore caller regs
         BR    R14                     Exit with CC set
*
SPLIST   DC    X'02'                   Number of shared subpools   v220
         DC    X'01'                    Share SP 1                 v220
         DC    X'02'                    Share SP 2                 v220
         DS    X                        Reserved                   v220
*
*-- Get a new command type WRE
*
*-- Entry:  None
*   Exit:   R1 -> WRE
*
*
GTW000   EQU   *
         ST    R14,SV14            Save return addr
         GETMAIN RU,               Get CSA for WRE TYPE=WRECMD         x
               LV=WRESIZE,                                         v220x
               SP=2                                                v220
         XC    0(WRESIZE,R1),0(R1)    Clear stg area               v220
         USING WRE,R1
         MVI   WRESP,2             Save subpool                    v220
         MVI   WRETYPE,WRECMD      CMD/MSG WRE
*
         NJETRACE TYPE=TRCGWRE
         STCM  R10,7,1(R14)        Identify trace entry            v220
         MVC   5(3,R14),SV14+1     Addr of GTW000 caller           v220
         STM   R0,R1,8(R14)        Len, stg addr to trace          v220
         MVI   8(R14),2            Trace subpool #                 v220
         DROP  R1
         L     R14,SV14            Load return addr
         BR    R14
*
*-- Queue the WRE on the Link and post link's ECB
*-- Caller must be PSW key 0
*
*-- Entry:  R2 -> LINKTABL entry
*--         R4 -> WRE
*-- Exit:   None
*
PST000   EQU   *
         USING LINKTABL,R2
         USING WRE,R4
         ST    R14,SV14            Save return addr
         LM    R0,R1,LWRESWAP      Get first WRE ptr, sync count
*
PST020   EQU   *
         ST    R0,WRENEXT          First WRE becomes next
         LA    R5,1(,R1)           Incr synchronization count
         CDS   R0,R4,LWRESWAP      Update LINK WRE anchor, sync
         BC    7,PST020            Gotta try again
*
         LA    R1,LECB             -> link task notification ECB
         POST  (1)                 Tell subtask WRE is queued
         L     R14,SV14            Load return addr
         BR    R14
*
         DROP  R2                  LINKTABL
         DROP  R4                  WRE
*
*
*-- Message response to console or local TSO user
*
*=== NOTE ===
*=== At present this routine (RSP000) is not called or used, but
*=== is retained here for possible future use.
*
*
*-- Entry:  Area "MACLIST" contains a WTO format msg
*           Area CMNDUSER=BLANKS send to console
*           Area CMNDUSER=userid send to that userid
*-- Exit:   None
*
*           Area "CMDAREA" is used by this call.
*
*
RSP000   EQU   *
         ST    R14,SV14                 Save return addr
         CLC   CMNDUSER,BLANKS          Is there a userid?
         BE    RSP010                   No, respond to console
         CLC   CMNDUSER,=CL8'OP'        Respond to operator
         BE    RSP010                   Y
*
         LA    R15,CMNDUSER             -> userid to locate
         BAL   R14,USR800               See if TSO user logged on
         BZ    RSP090                   Skip msg if not
         MVC   CMDAREA,MACLIST+4        Save message text
         MVC   MACLIST+4(4),=C'SE '''
         MVC   MACLIST+8(104),CMDAREA                              v102
         MVC   MACLIST+112(8),=C''',USER=('                        v102
         MVC   MACLIST+120(12),BLANKS     Ensure trailer initted   v102
         MVC   MACLIST+120(7),CMNDUSER    Max for TSO userid is 7  v102
         LA    R1,MACLIST+127                                      v102
         TRT   MACLIST+120(7),BLANK                                v102
         MVI   0(R1),C')'
         MVI   1(R1),C' '
         MVC   MACLIST(4),=AL2(129,0)   max len + 4 overhead       v102
*
         SPKA  0
         LA    R1,MACLIST
         SR    R0,R0
         SVC   34                       Issue MGCR SVC
         SPKA  X'80'
         B     RSP090
*
RSP010   EQU   *
         WTO   ,MF=(E,MACLIST)
*
RSP090   EQU   *
         L     R14,SV14                 Reload return addr
         BR    R14
*
*-- Search CSCB chain to see if TSO user is logged on
*-- Entry:  R15->8-byte padded field containing TSO userid to find
*-- Exit:  CC=0  user was not logged on
*--        CC<>0 user is logged on
*
USR800   EQU   *
         CLC   =CL8'OP',0(R15)     Is the userid the operator?
         BE    USR890              Yes, let it thru
         L     R1,16               Get CVT ptr
         USING CVT,R1
         L     R1,CVTASCBH         -> highest prty ASCB
         USING ASCB,R1
*
USR810   EQU   *
         L     R2,ASCBCSCB         -> CSCB
         USING CSCB,R2
         LTR   R2,R2               Is there a CSCB?
         BZ    USR840              No, get next ASCB
*
USR820   EQU   *
         CLC   CHKEY,=XL8'00'      Jobname zeroed?
         BE    USR830              Y, skip this CSCB
         CLC   CHKEY,=CL8' '       Jobname is blank?
         BE    USR830              Y, skip this CSCB
         CLC   CHKEY,0(R15)        Is this the userid?
         BE    USR890              Yes
USR830   EQU   *
         L     R2,CHPTR            -> next CSCB
         LA    R2,0(,R2)           Clear high order
         LTR   R2,R2               Last CSCB?
         BNZ   USR820              No
         BR    R14                 Return with CC=0 (not found)
*
USR840   EQU   *
         L     R1,ASCBFWDP         -> next ASCB
         LTR   R1,R1               last one?
         BNZ   USR810              No
         BR    R14                 Return with CC=0 (not found)
*
USR890   EQU   *
         LTR   R14,R14             Set CC=non zero (userid found)
         BR    R14                 Return to caller
*
         DROP  R1                  ASCB
         DROP  R2                  CSCB
*
*-- Special code to intercept messages destined for                v220
*-- registered users                                               v220
*
*
REG000   EQU   *                                                   v220
         L     R2,AREGUSER         -> registered user anchor word  v220
         ICM   R2,15,0(R2)         -> registered user queue        v220
         BZR   R14                 No registered users             v220
*
         USING REGUSERB,R2                                         v220
REG010   EQU   *                                                   v220
         CLC   REGUSRID,0(R15)     Find a matching registered user v220
         BE    REG020              Found it                        v220
         ICM   R2,15,REGNEXT       -> next REGUSER entry           v220
         BNZ   REG010              Keep looking                    v220
         BR    R14                 Userid was not registered       v220
*
REG020   EQU   *                                                   v220
         ST    R14,SVR14R          Save return addr                v220
         BAL   R14,GTW000          Get a WRE                       v220
         LR    R4,R1                                               v220
         USING WRE,R4                                              v220
         MVI   WRETYPE,WREQRM      Queue registered msg WRE        v220
*
         MVC   WRELINK,LCLNODE     Target WRE to local node task   v220
         MVC   WREUSER,REGUSRID    Dest= registered user id        v220
         MVC   WREORIG,BLANKS      No originating node             v220
         MVC   WRETXT,BLANKS       Init first part                 v220
         MVC   WRETXT(5),=C'From '                                 v220
         MVC   WRETXT+5(8),WREORIG-WRE(R6)  From original msg      v220
         TRT   WRETXT+5(9),BLANK   Look for end of orig userid     v220
         MVI   0(R1),C':'                                          v220
         LA    R1,2(,R1)           -> area for msg                 v220
         MVC   0(104,R1),WRETXT-WRE(R6)  Copy msg text             v220
         MVI   WRETXTLN,L'WRETXT   Set the max possible len        v220
*
         SPKA  0                                                   v220
         L     R15,CSABLK          -> NJE38 CSA block              v220
         USING NJ38CSA,R15                                         v220
         LM    R0,R1,NJ38SWAP      Get first WRE ptr, sync count   v220
*
REG030   EQU   *                                                   v220
         ST    R0,WRENEXT          First WRE becomes next          v220
         LA    R5,1(,R1)           Incr synchronization count      v220
         CDS   R0,R4,NJ38SWAP      Update LINK WRE anchor, sync    v220
         BC    7,REG030            Gotta try again                 v220
*
         LA    R1,NJ38ECB          -> main task notification ECB   v220
         POST  (1)                 Wake him up                     v220
*
         SPKA  X'80'                                               v220
*
         DROP  R2,R4,R15           REGUSERB,WRE,NJ38CSA            v220
*                                                                  v220
REG090   EQU   *                                                   v220
         L     R14,SVR14R          Load return addr                v220
         LTR   R14,R14             Set non-zero CC                 v220
         BR    R14                 Ret w/CC non-zero (msg queued)  v220
*
*
*-- Format and display VSAM errors
*
FMT000   EQU   *
         STM   R14,R2,BALRSAVE         Save regs used
         MVC   MACLIST(WTOMSGL),WTOMSG
         MVC   MACLIST+4(L'NJE079I),NJE079I   Move msg text
         MVC   MACLIST+55(8),5(R12)    Move csect name
         TRT   MACLIST+55(9),BLANK     Look for end of csect name
         MVI   0(R1),C'+'
*
         LA    R15,0(,R14)  Clear high, Get addr of call to this rtn
         LA    R12,0(,R12)             Clear high byte
         SR    R15,R12                 Compute offset of call
         ST    R15,DBLE                Save to work area
         UNPK  TWRK(5),DBLE+2(3)       Add zones
         TR    TWRK(4),HEXTRAN-240     Display hex
         MVC   1(4,R1),TWRK            Move call offset to msg
*
         LA    R15,NCB1
         UNPK  TWRK(5),NCBRTNCD-NCB(3,R15)  Add zones
         TR    TWRK(4),HEXTRAN-240
         MVC   MACLIST+35(4),TWRK      Move rtncd/errcd
*
         UNPK  TWRK(3),NCBREQ-NCB(2,R15)  Add zones
         TR    TWRK(2),HEXTRAN-240
         MVC   MACLIST+45(2),TWRK      Move req code
*
         L     R1,NCBMACAD-NCB(,R15)   Get failing VSAM macro addr
         LA    R1,0(,R1)               Clear high byte
         S     R1,ANJESPL              offset into NJESPOOL rtn v210
         ST    R1,DBLE
         UNPK  TWRK(5),DBLE+2(3)       Add zones
         TR    TWRK(4),HEXTRAN-240     Display hex
         MVC   MACLIST+50(4),TWRK      Move NJESPOOL offset to msg
*
         MVC   LASTRC(2),NCBRTNCD-NCB(R15)  Save off rtncd/errcd
         OI    NJFL1,NJF1VSER          Indicate VSAM error occurred
*
         WTO   ,MF=(E,MACLIST)
*
FMT090   EQU   *
         LM    R14,R2,BALRSAVE         Restore caller regs
         BR    R14                     Exit with CC set
*
*
*
ATTACH   ATTACH SF=L
ATTACHL  EQU   *-ATTACH
POST     POST  0,ASCB=0,ERRET=0,MF=L                               v220
POSTL    EQU   *-POST                                              v220
WTOMSG   WTO   '                                                       x
                                             ',MF=L
WTOMSGL  EQU   *-WTOMSG
*
BLANKS   DC    CL120' '
NONBLANK DC    64X'FF',X'00',191X'FF'   TR Table to locate nonblank
BLANK    DC    64X'00',X'FF',191X'00'   TR Table to locate blanks
ASTER    DC    92X'00',X'FF',163X'00'   TR Table to locate asteriskv211
HEXTRAN  DC    CL16'0123456789ABCDEF'  Translate table
*                      1         2         3           4         5
*                456789012345678901234567890123 45678 90123456789012345
NJE079I  DC    C'NJE079I NETSPOOL RTNCD/ERRCD=X''0000'',REQ=01,O=1234,Mx
               MMMMMMM     '
*
         LTORG
*
*                                                                     *
***********************************************************************
**                                                                   **
**                        TASK ESTAI EXIT                            **
**                                                                   **
** This csect handles all abends trapped by ESTAE during the normal  **
** execution of the subtask.          This exit does not attempt     **
** any recovery other than to terminate processing.                  **
** An SVC dump is taken on abends.                                   **
**                                                                   **
** On entry:  R0=ESTAE provide entry code                            **
**            R1=SDWA address                                        **
**            R2=parameter passed on ESTAE macro                     **
**                                                                   **
**                                                                   **
** On exit: If SDWACLUP is 1, then no retry is allowed and this      **
**             exit will allow percolation back to system routines   **
**             to terminate the task.                                **
**                                                                   **
**          If SDWACLUP is 0, then retry is allowed.                 **
**                                                                   **
** Security:  N/A.                                                   **
**                                                                   **
** Register usage:                                                   **
**                                                                   **
**   R1  = SDWA address                                              **
**   R3  = SDWA address                                              **
**   R10 = Dynamic storage area base                                 **
**   R12 = This program base                                         **
**                                                                   **
**                                                                   **
**                                                                   **
***********************************************************************
*
NJEDMP   CSECT
         B     28(,R15)               BRANCH AROUND EYECATCHERS
         DC    AL1(23)                LENGTH OF EYECATCHERS
         DC    CL9'NJEDMP'
         DC    CL9'&SYSDATE'
         DC    CL5'&SYSTIME'
*
         LR    R12,R15                SET UP BASE REG
         USING NJEDMP,R12             ESTABLISH ADDRESSABILITY
         LR    R8,R14                 SAVE RETURN ADDRESS TO SYSTEM
*
         L     R10,0(,R1)             GET VALUE PASSED TO US (WORKA)
         USING NJEMWK,R10
         L     R11,ANJECOM            -> common code and constants
         USING NJECOM,R11
*
         LR    R3,R1                  SAVE R1 ENTRY CONTENTS
         USING SDWA,R3
         LR    R5,R0                  Save R0 entry code
*
         LTR   R3,R3                  Do we have an SDWA?
         BZ    NOSDWA                 Exit if no SDWA
         LA    R13,MVSSAVE            Save area
*
         MODESET MODE=SUP,            Run this ESTAI exit privileged   x
               KEY=ZERO                to access PSW -> storage
*
         MVC   MACLIST(WTOMSGL),WTOMSG
         L     R6,PSATOLD-PSA(0)      -> my TCB
         L     R5,TCBTIO-TCB(,R6)     -> TIOT
         MVC   MACLIST+4(8),0(R5)         Plug in job name
         MVC   MACLIST+14(5),=C'LINK '
         MVC   LKNAME,=CL8' '           Init receiving field
*
         L     R2,LINKS                -> 1st entry (LOCAL entry)  v211
         USING LINKTABL,R2
         ICM   R2,15,LNEXT             -> 1st non-lcl LINKTABL     v211
         BZ    LNK005                  Skip if not there           v211
*
LNK000   EQU   *
         CLM   R6,7,LTCBA+1             Look for TCB of failing link
         BE    LNK010                   Found it
         ICM   R2,15,LNEXT              -> next LINKTABL entry
         BNZ   LNK000                   Keep searching
*
LNK005   EQU   *                                                   v211
         MVC   MACLIST+14(5),=C'LMOD '
         MVC   MACLIST+19(8),=CL8'NJEINIT' Else it is main task
         OI    NJFL1,NJF1INIT           This is the NJEINIT task
         B     LNK020                   No TCB/link found
*
LNK010   EQU   *
         MVC   MACLIST+19(8),LINKID     Move link name
         MVC   LKNAME,LINKID            Save copy of link name
         DROP  R2
*
LNK020   EQU   *
         MVC   MACLIST+29(5),=C'ABEND'
         L     R5,SDWAABCC              GET ABEND CODE INFO WORD
         N     R5,=X'00FFF000'          KEEP ONLY THE SYSTEM CODE
         BZ    USERCDE                  NONE THERE, MUST BE A USER CODE
         C     R5,=X'00222000'          Operator cancel, no dump?
         BE    SDUMP040                 no
         C     R5,=X'00013000'          013-OPEN abend?            v211
         BE    SDUMP040                 no dump                    v211
*
         MVI   MACLIST+35,C'S'          INDICATE SYSTEM CODE
         UNPK  FWORK(5),SDWACMPC(3)     GET SYSTEM CMP CODE
         TR    FWORK(3),HEXTRAN-240
         MVC   FWORK+3(5),=CL5' '       CLEAR REST OF ABEND CODE
         B     NOREAS
*
USERCDE  EQU   *
         MVI   MACLIST+35,C'U'         INDICATE USER ABEND CODE
         L     R5,SDWAABCC             GET ABEND CODE
         N     R5,=X'00000FFF'         KEEP USER ABEND CODE
         CVD   R5,FSAVE                CONVERT CODE TO DECIMAL
         UNPK  FWORK(4),FSAVE          UNPK THE CODE
         OI    FWORK+3,X'F0'           FIX SIGN
         MVC   FWORK+4(2),=CL2' '      BLANKS AT END OF ABEND CODE
*
NOREAS   EQU   *
         MVC   MACLIST+36(6),FWORK     MOVE ABEND-REASON TO LINE
         MVC   ABCODE,MACLIST+36       Save a copy of formatted abcode
*
         WTO   ,MF=(E,MACLIST)
*
         MVC   MACLIST(WTOMSGL),WTOMSG
         MVC   MACLIST+4(3),=C'PSW'
         UNPK  FSAVE(9),SDWAEC1(5)    Add zones to PSW word 1
         TR    FSAVE(8),HEXTRAN-240
         MVC   MACLIST+10(8),FSAVE
         UNPK  FSAVE(9),SDWAEC1+4(5)  Add zones to PSW word 2
         TR    FSAVE(8),HEXTRAN-240
         MVC   MACLIST+19(8),FSAVE
*
         SR    R5,R5                   CLEAR FOR IC
         IC    R5,SDWAILC1             GET THE ILC
         CVD   R5,FWORK                MAKE DECIMAL
         MVC   MACLIST+29(3),=C'ILC'
         UNPK  MACLIST+33(2),FWORK     UNPK
         OI    MACLIST+34,X'F0'        FIX THE SIGN
*
         MVC   MACLIST+37(4),=C'INTC'
         UNPK  FWORK(5),SDWAINC1(3)    MAKE INTC DISPLAYABLE
         TR    FWORK(4),HEXTRAN-240
         MVC   MACLIST+42(4),FWORK     MOVE INTC TO LINE
*
         WTO   ,MF=(E,MACLIST)
*
         MVC   MACLIST(WTOMSGL),WTOMSG
         MVC   MACLIST+4(13),=C'DATA NEAR PSW'
         MVC   MACLIST+19(8),=CL8'UNAVAIL'  ASSUME WE CANT GET DATA
         L     R4,SDWAEC1+4            Get PSW IA
         LA    R4,0(,R4)               Clear high bit
         C     R4,=F'8'                1st 8 bytes of storage?
         BH    LOC010                  No, its higher than that
         SR    R4,R4                   Yes, just use 0
         B     LOC020
*
LOC010   EQU   *
         S     R4,=F'8'                BACK UP BEFORE INTERRUPT ADDR
*
LOC020   EQU   *
         LRA   R0,0(,R4)               Do we have access?
         BNZ   UNAVAIL                 No translation, better not
         LRA   R0,14(,R4)              Do we have access?
         BNZ   UNAVAIL                 No translation, better not
*
         ST    R4,FWORK                SAVE FOR CONVERSION
         UNPK  FSAVE(9),FWORK(5)       ADD ZONES TO ADDRESS
         TR    FSAVE(8),HEXTRAN-240    MAKE DISPLAYABLE HEX
         MVC   MACLIST+19(8),FSAVE     MOVE DISPLAYABLE
*
         MVC   FWORK(4),0(R4)          MOVE 4 WORDS AT PSW
         UNPK  FSAVE(9),FWORK(5)       ADD ZONES
         TR    FSAVE(8),HEXTRAN-240    MAKE DISPLAYABLE HEX
         MVC   MACLIST+29(8),FSAVE     MOVE TO LINE
*
         MVC   FWORK(4),4(R4)          MOVE 4 WORDS AT PSW
         UNPK  FSAVE(9),FWORK(5)       ADD ZONES
         TR    FSAVE(8),HEXTRAN-240    MAKE DISPLAYABLE HEX
         MVC   MACLIST+38(8),FSAVE     MOVE TO LINE
*
         MVC   FWORK(4),8(R4)          MOVE 4 WORDS AT PSW
         UNPK  FSAVE(9),FWORK(5)       ADD ZONES
         TR    FSAVE(8),HEXTRAN-240    MAKE DISPLAYABLE HEX
         MVC   MACLIST+47(8),FSAVE     MOVE TO LINE
*
         MVC   FWORK(4),12(R4)         MOVE 4 WORDS AT PSW
         UNPK  FSAVE(9),FWORK(5)       ADD ZONES
         TR    FSAVE(8),HEXTRAN-240    MAKE DISPLAYABLE HEX
         MVC   MACLIST+56(8),FSAVE     MOVE TO LINE
*
UNAVAIL  EQU   *
         WTO   ,MF=(E,MACLIST)
*----
         LA    R4,4                    4 ROWS OF REGISTERS
         LA    R5,SDWAGR00             POINT TO ABEND REGS
         LA    R6,REGLIST              POINT TO REGISTER ID LITERALS
*
GPR000   EQU   *                                                   v220
         MVC   MACLIST(WTOMSGL),WTOMSG
         MVC   MACLIST+4(8),0(R6)      MOVE REGISTERS ID
         LA    R15,MACLIST+13          WHERE 1ST REG GOES ON LINE
         LA    R14,4                   4 REGS PER LINE
*
GPR010   EQU   *                                                   v220
         UNPK  FSAVE(9),0(5,R5)        UNPK A REGISTER
         TR    FSAVE(8),HEXTRAN-240    MAKE DISPLAYABLE HEX
         MVC   0(8,R15),FSAVE          MOVE TO THE LINE
         LA    R15,10(,R15)            NEXT SPOT ON PRINT LINE
         LA    R5,4(,R5)               NEXT REGISTER
         BCT   R14,GPR010              KEEP DOING REGS             v220
         WTO   ,MF=(E,MACLIST)
         LA    R6,8(,R6)               NEXT REGISTER ID
         BCT   R4,GPR000               GO DISPLAY THE NEXT ROW     v220
*
*
SDUMP000 EQU   *
         MVI   DHDR,C' '
         MVC   DHDR+1(29),DHDR
         MVI   DHDR,29                IBM length of header
         L     R5,PSATOLD-PSA(0)      -> my TCB
         L     R5,TCBTIO-TCB(,R5)     -> TIOT
         MVC   DHDR+1(8),0(R5)        Use jobname in description
         MVC   DHDR+11(8),LKNAME      Use link name
         MVC   DHDR+21(7),ABCODE
*
         MVC   MACLIST(SDUMPL),SDUMP    MOVE SDUMP LIST TO WORK
         LA    R1,MACLIST
         SDUMP HDRAD=DHDR,              ISSUE SDUMP TO RECORD STATUS   x
               BUFFER=NO,                                              x
               QUIESCE=NO,                                             x
               SDATA=(RGN,CSA,LPA,SUM),                                x
               MF=(E,(1))
*
*
SDUMP040 EQU   *
         TM    NJFL1,NJF1INIT          Is this the NJEINIT task?
         BZ    SDUMP090                No
         ICM   R5,15,CSABLK            -> CSA stg area
         BZ    SDUMP090                Not present
*
         FREEMAIN RU,LV=16,A=(5),SP=241  Free CSA area
         XC    CSABLK,CSABLK
*
SDUMP090 EQU   *
         LR    R1,R3                  SDWA BACK TO R1
*                                 **  SDWA ADDR MUST BE IN R1 FOR SETRP
         SETRP RC=0,                  No retry                         X
               DUMP=NO                Suppress any further dumps
*
NOSDWA   EQU   *                  **  NO RETRY AVAILABLE (OR DESIRED)
         SR    R15,R15                REQUEST PERCOLATION
         LR    R14,R8                 RESTORE RETURN ADDRESS
         BR    R14                    RETURN TO SYSTEM
*
         LTORG
*
SDUMP    SDUMP MF=L
SDUMPL   EQU   *-SDUMP
*
REGLIST  DC    CL8'GR 0-3'
         DC    CL8'GR 4-7'
         DC    CL8'GR 8-11'
         DC    CL8'GR 12-15'
*
         LTORG
*
*
*
*********************
*  N J E C M D      *       Commands issued by TSO users via command
*                   *       module NJE38 also arrive here
*  MVS Modify cmd   *
*  processing       *
*                   *
*********************
*
NJECMD   CSECT
         B     28(,R15)               BRANCH AROUND EYECATCHERS
         DC    AL1(23)                LENGTH OF EYECATCHERS
         DC    CL9'NJECMD'
         DC    CL9'&SYSDATE'
         DC    CL5'&SYSTIME'
         STM   R14,R12,12(R13)
         LR    R12,R15                 Base
         USING NJECMD,R12              ADDRESS IT
         USING NJECOM,R11
         USING NJEMWK,R10
*
         ST    R13,NJECMDSA+4
         LA    R13,NJECMDSA
*
CMD000   EQU   *
         BAL   R14,LOC000          Announce command being executed
*
CMD010   EQU   *
         LA    R0,TGTCONS          Console gets response
         LA    R1,CMNDBLOK         -> local CMDBLOK area
         ST    R1,ACMDBLOK         Set addr in cmd parm list
         LA    R1,INITPARM         -> parm list
         L     R15,ANJECMX         -> Command processor
         BALR  R14,R15
         B     XITCMD00
*
*
LOC000   EQU   *
         CLC   CMNDUSER,=CL8'OP'       Command from operator?
         BER   R14                     Yes, skip location msg
*
         ST    R14,SV14                 Save return addr
         MVC   MACLIST(WTOMSGL),WTOMSG
         MVC   MACLIST+4(L'NJE005I),NJE005I
         LA    R1,MACLIST+4+L'NJE005I   -> next byte
         MVC   0(8,R1),LCLNODE         Local node
         TRT   0(9,R1),BLANK           Look for end
         MVI   0(R1),C'('
         MVC   1(8,R1),CMNDUSER        Local userid
         TRT   1(9,R1),BLANK           Look for end
         MVC   0(12,R1),=CL12') executing:'
         LA    R1,13(,R1)              -> area for msg
         SR    R15,R15                 Clear for IC
         IC    R15,CMNDBLEN            Len of cmd text
         C     R15,=F'50'              Allow 50 char max
         BL    *+8                     We're ok
         LA    R15,50                  Use 50
         EX    R15,MVCMTXT1            Move command text to msg
*
         WTO   ,MF=(E,MACLIST)         Issue location executing msg
*
LOC090   EQU   *
         L     R14,SV14                Reload return
         BR    R14                     Return
*
MVCMTXT1 MVC   0(0,R1),CMDAREA         executed instr
*
*
*
*
*
XITCMD00 EQU   *
         L     R13,4(,R13)         -> NJEREQ save area
         LM    R14,R12,12(R13)     Reload callers regs
         SR    R15,R15
         BR    R14                 Return to NJEREQ
*
         LTORG
*
*                456789012345678901234567890123456789012345678901
NJE005I  DC    C'NJE005I Location '             Location executing
*
*
*
*
***************
* TIMER       *                        THIS EXIT WILL KEEP THE JOB
* EXPIRATION  *                        ACTIVE EVERY 20 MINUTES, AND
* EXIT        *                        WILL KEEP THE JOB FROM ABENDING
***************                        WITH AN S 522 ABEND (WAIT LIMIT)
*
NJETMR   CSECT
         B     28(,R15)                BRANCH AROUND EYECATCHERS
         DC    AL1(23)                 LENGTH OF EYECATCHERS
         DC    CL9'NJETMR'
         DC    CL9'&SYSDATE'
         DC    CL5'&SYSTIME'
         STM   R14,R12,12(R13)
         LR    R12,R15
         USING NJETMR,R12
*
         STIMER REAL,                  RESET THE TIMER AGAIN           X
               (12),                   POINT TO THE EXIT ROUTINE       X
               DINTVL=INTVL            INTERVAL
*
         LM    R14,R12,12(R13)         RELOAD REGS
         SR    R15,R15
         BR    R14                     RETURN TO SYSTEM
*
         DS    0D
*                  HHMMSSTH
INTVL    DC    CL8'00200000'           20 MINUTE TIMER
*
         DROP  R12
         LTORG
*
*
***************                                                    v212
* GET         *                                                    v212
* TRACE       *                                                    v212
* ENTRY       *                                                    v212
***************                                                    v212
*
NJETRC   CSECT                                                     v212
         B     28(,R15)                BRANCH AROUND EYECATCHERS   v212
         DC    AL1(23)                 LENGTH OF EYECATCHERS       v212
         DC    CL9'NJETRC'                                         v212
         DC    CL9'&SYSDATE'                                       v212
         DC    CL5'&SYSTIME'                                       v212
         USING NJETRC,R15                                          v212
         LR    R0,R14                  Save return addr            v212
*
TRC000   EQU   *                                                   v212
         USING TRCCTL,R2                                           v212
         L     R1,TRCCURR              -> current trace slot       v212
         LA    R14,TRCSZ(,R1)          -> next slot                v212
         C     R14,TRCEND              At end of table?            v212
         BL    TRC010                  No                          v212
         L     R14,TRCSTRT             Y, wrap to beginning        v212
         LA    R14,TRCSZ(,R14)         -> Skip over first slot     v212
*
TRC010   EQU   *                                                   v212
         CS    R1,R14,TRCCURR          Set new current             v212
         BC    4,TRC000                CC=1; no match; try again   v212
*
         XC    0(TRCSZ,R14),0(R14)     Clear slot                  v212
         DROP  R2,R15                                              v212
*
         LR    R15,R0                  Load return addr to.. R15  !v212
         LM    R0,R2,20(R13)           Reload the rest             v212
         BR    R15                     Return via R15;             v212
*                                       New trace entry -> R14     v212
*
*
*
****  Main work area common
****  to all NJExxx CSECTs.
*
NJEMWK   DSECT
NJEEYE   DS    CL4'NJEM'           Eyecatcher; main task work area
NJEWKLEN DS    F                   Getmain size of this area
*
DEFUSER  DS    CL8                 Default userid from CONFIG      v200
RELAYID  DS    CL8                 Relay entity id                 v220
DBLE     DS    D                   Work area
TWRK     DS    2D                  Work area
NCB1     DS    XL48                NETSPOOL CB
*
         NJEPARMS                  Define passed parameter list    v220
*
MACLIST  DS    XL160               Macro expansion area
ANJECOM  DS    A                   -> NJECOM csect
COMMAREA DS    A                   -> Console communications area
COMMECBA DS    A                   -> Console communications ECB
REGUSER  DS    A                   -> REGUSER chain anchor         v220
LINKS    DS    A                   -> LINKTABL chain anchor        v211
ROUTES   DS    A                   -> RTE chain anchor             v211
AUTHS    DS    A                   -> AUTHLIST chain anchor        v211
CSAECBAD DS    A                   -> WRE ECB in CSA (same as NJ38ECB)
*
ECBLIST  DS    66A                 ECB list, 64 links + 2 COMM ECBs
*
NJFL1    DS    X                   Flag byte
NJF1STOP EQU   X'80'   1... ....    Console STOP issued
NJF1ENQ  EQU   X'40'   .1.. ....    NJE38 system ENQ issued
NJF1VSER EQU   X'02'   .... ..1.    NETSPOOL VSAM error occurred
NJF1INIT EQU   X'01'   .... ...1    NJEINIT task in RTM
*                      ..xx xx..    Available
*
NJFL2    DS    X                   Flag byte
*                      xxxx xxxx    Available
*
LASTRC   DS    X                   Last RC from NCBRTNCD
LASTERRC DS    X                   Last errcd from NCBERRCD
*
FSAVE    DS    2D
FWORK    DS    D
DHDR     DS    CL30
ABCODE   DS    CL7
FLAGS    DS    X
LKNAME   DS    CL8                 Name of failing link
*
*
*                                  Command response target
TGTUSER  EQU   0                    remote user
TGTCONS  EQU   4                    MVS system console
CMNDBLOK DS    0XL140              CMDBLOK
CMNDBLEN DS    AL1                 Command image ibm length
CMNDDMY  DS    XL3                 Rest of CMDBLOK (unused here)
CMNDLINK DS    CL8                 Node of issuer
CMNDUSER DS    CL8                 yserid of issuer
CMDAREA  DS    CL120               Modify command image
*
CMDBLNK  DS    CL120               For TRT overflow, all blanks
*
JFCBDCB  DS    (NSPOOLN)X          Space for DCB
JEXLST   DS    A                   DCB EXLST
*
NJERNAME DS    CL12                12 ENQ RNAME,+44 for DSN in JFCB
JFCB     DS    XL176               Space for JFCB
*
SV14     DS    A                   General use R14 save
SVR14R   DS    A                   General use R14 save
NJESA    DS    18F                 NJEINIT OS save area
NJECMDSA DS    18F                 NJECMD OS save area
MVSSAVE  DS    18F                 ESTAE exit OS save
BALRSAVE DS    16F                 Local register save area
*
         DS    0D                      Force doubleword size
NJEWKSZ  EQU   *-NJEMWK
*
*
*-- System DSECTs
*
*
IEZCOM   DSECT
         IEZCOM
IEZCIB   IEZCIB
         IHAPSA
         IHASDWA
         IKJTCB
         IHAASCB
         IEZJSCB
*
CSCB     DSECT
         IEECHAIN                      MAP FOR A CSCB
         CVT   DSECT=YES,LIST=YES
         DCBD  DEVD=DA,DSORG=PS
*
         COPY  LINKTABL
         COPY  RTE
         COPY  AUTHLIST
         COPY  NETSPOOL
*
*-- NJE38 DSECTs
*
         NJEWRE                                                    v220
         NJERUSER                                                  v220
         NJETRACE TYPE=DSECT                                       v220
*
         END   NJEINIT
./ ADD NAME=NJEFMT
*
*
*-- NJE38 - NETSPOOL Formatter
*
*
*   This program formats the NETSPOOL dataset.
*
*
*
*
         REGEQU
NJEFMT   CSECT
         NJEVER
         STM   R14,R12,12(R13)         SAVE CMS REGS
         LR    R12,R15                 BASE
         USING NJEFMT,R12              ADDRESS IT
*
         GETMAIN RU,                   GET LOCAL STG AREA              X
               LV=NJEFSZ
         LR    R10,R1
         LR    R1,R0                   COPY LENGTH
         LR    R2,R0                   COPY LENGTH
         LR    R0,R10                  -> NEW STG AREA
         SR    R15,R15                 SET PAD
         MVCL  R0,R14                  CLEAR THE PAGE
*
         USING NJEFWK,R10
         ST    R13,NJESA+4             SAVE PRV S.A. ADDR
         LA    R1,NJESA                -> MY SAVE AREA
         ST    R1,8(,R13)              PLUG IT INTO PRIOR SA
         LR    R13,R1
*
         MVC   NJEEYE,=CL4'NJEF'       Work area eyecatcher
         ST    R2,NJEWKLEN             Save size of area
         MVC   SYSPRINT(DMYPRTL),DMYPRT Set up DCB
         MVC   LIST,BLANKS             Init print line
*
         MVC   MACLIST(OPENL),OPEN     Move OPEN list
         OPEN  (SYSPRINT,OUTPUT),      Open the print dataset          X
               MF=(E,MACLIST)
*
         MVC   LIST(L'MSG001),MSG001
         BAL   R14,PUT                 Write the line
         BAL   R14,PUT                 Write blank line
*
         GETMAIN RU,                   Get storage for NETSPOOL block  x
               LV=4089,                                                x
               BNDRY=PAGE
         ST    R1,BLK                  Save address
         LR    R8,R1                   Keep in R8
*
         LR    R1,R0                   Copy length
         LR    R0,R8                   Copy address
         SR    R15,R15                 Clear pad
         MVCL  R0,R14                  Clear the stg
*
         GENCB BLK=ACB,                                                x
               DDNAME=NETSPOOL,                                        x
               MACRF=(OUT,KEY,SEQ),                                    x
               MF=(G,MACLIST)
         STM   R0,R1,ACBL              Save len, addr
*
         LA    R9,KEY                  -> block number argument
         GENCB BLK=RPL,                                                x
               ACB=(*,ACB),                                            x
               AREA=(R8),              -> block area                   x
               AREALEN=4089,                                           x
               RECLEN=4089,                                            x
               ARG=(R9),                                               x
               OPTCD=(KEY,SEQ,MVE),                                    x
               MF=(G,MACLIST)
         STM   R0,R1,RPLL              Save len, addr
*
         L     R7,ACB                  -> ACB
         MVC   MACLIST(OPENL),OPEN     Move macro model
         OPEN  ((R7)),                 Open NETSPOOL                   x
               MF=(E,MACLIST)
         LTR   R15,R15                 Did open succeed?
         BNZ   OPENFAIL                No
         OI    FLAGS1,FL1OPEN          Indic ACB open
*
         LA    R5,HIRBA                -> SHOWCB receipt fields
         SHOWCB ACB=(R7),                                              x
               AREA=(R5),                                              x
               LENGTH=8,                                               x
               FIELDS=(HALCRBA,CINV),  Hi alloc RBA + CISZ             x
               MF=(G,MACLIST)
*
         CLC   CISZ,=F'4096'           Ensure CISZ is 4096
         BNE   BADCISZ                 It is not
         L     R5,HIRBA                Get high allocated RBA
         SRL   R5,12                   Divide by 4096
         ST    R5,BLKS                 Save number of blocks in d.set
*
         L     R6,RPL
         USING IFGRPL,R6
         LA    R4,1                    Init block counter
*
FMT000   EQU   *
         PUT   RPL=(R6)                Write a block
*
         LTR   R15,R15                 Any errors?
         BZ    FMT010                  No
         CLI   RPLRTNCD,X'08'          Logical error?
         BNE   PUTFAIL                 No, display error
         CLI   RPLERRCD,X'08'          Duplicate block?
         BE    FMT100                  Cluster is already formatted
         B     PUTFAIL                 Display all other errors
*
FMT010   EQU   *
         LA    R4,1(,R4)               Count blocks
         BCT   R5,FMT000               Format exact amount
         B     FMT200                  Now go write images
*
*-- Here if NETSPOOL was previously formatted
*
FMT100   EQU   *
         WTO   'NJEFMT - NETSPOOL dataset is already formatted'
         WTO   'NJEFMT - Reformatting will cause loss of all data'
*
FMT110   EQU   *
         XC    OPECB,OPECB             Reinit ECB
         LA    R2,DBLE                 -> reply area
         LA    R3,OPECB                -> WTOR ECB
         MVC   MACLIST(WTORDMYL),WTORDMY  Move model WTOR
         WTOR  ,(R2),6,(R3),MF=(E,MACLIST)
*
         WAIT  1,ECB=OPECB
         CLC   DBLE(6),=C'CANCEL'      Was cancel chosen?
         BE    OPERCAN                 Yes
         CLI   DBLE,C'U'               Was U chosen
         BNE   FMT110                  Reissue msg
*
*-- Switch to direct processing and rewrite fresh initial images
*-- to the directory and allocation map to be a newly formatted file.
*
FMT200   EQU   *
         MVC   MACLIST(CLOSEL),CLOSE   Move close list
         CLOSE ((R7)),                 Close ACB                       X
               MF=(E,MACLIST)
         NI    FLAGS1,255-FL1OPEN      Indic ACB closed
*
         MODCB ACB=(R7),               Switch to direct                x
               MACRF=(KEY,DIR,OUT),                                    x
               MF=(G,MACLIST)
*
         MODCB RPL=(R6),                                               x
               OPTCD=(KEY,DIR,UPD),    Switch to direct update         x
               MF=(G,MACLIST)
*
         MVC   MACLIST(OPENL),OPEN     Move macro model
         OPEN  ((R7)),                 Open NETSPOOL                   x
               MF=(E,MACLIST)
         LTR   R15,R15                 Did open succeed?
         BNZ   OPENFAIL                No
         OI    FLAGS1,FL1OPEN          Indic ACB open
*
         L     R2,BLK                  -> VSAM area
*
FMT210   EQU   *
         MVC   KEY,=F'1'               Set block # argument
         GET   RPL=(6)
         LTR   R15,R15                 Any errors?
         BNZ   GETFAIL                 YES
*
         LR    R0,R2                   -> block area
         LA    R1,4089                 Size of block
         LM    R14,R15,BLK1            Get block data addr, pad+len
         MVCL  R0,R14                  Init the block, SPL ID=0
         MVC   8(4,R2),BLKS            Set max # blocks in dataset
*
         PUT   RPL=(6)                 Update the block
         LTR   R15,R15                 Any errors?
         BNZ   PUTFAIL2                YES
*
FMT220   EQU   *
         MVC   KEY,=F'2'               Set block # argument
         GET   RPL=(6)
         LTR   R15,R15                 Any errors?
         BNZ   GETFAIL                 YES
*
         LR    R0,R2                   -> block area
         LA    R1,4089                 Size of block
         LM    R14,R15,BLK2            Get block data addr, pad+len
         MVCL  R0,R14                  Init the block
*
         PUT   RPL=(6)                 Update the block
         LTR   R15,R15                 Any errors?
         BNZ   PUTFAIL2                YES
*
FMT230   EQU   *
         MVC   KEY,=F'3'               Set block # argument
         GET   RPL=(6)
         LTR   R15,R15                 Any errors?
         BNZ   GETFAIL                 YES
*
         LR    R0,R2                   -> block area
         LA    R1,4089                 Size of block
         LM    R14,R15,BLK3            Get block data addr, pad+len
         MVCL  R0,R14                  Init the block
*
         PUT   RPL=(6)                 Update the block
         LTR   R15,R15                 Any errors?
         BNZ   PUTFAIL2                YES
*
*-- Set up allocation map
*
* The allocation map is a bit map, 1 bit for each block in the
* NETSPOOL dataset.  A "1" bit means the block is in use.  Initially,
* blocks 1-7 will be marked in use as they contain upon formatting:
*   block 1 - pointer to directory (A or B) and allocation blocks
*   block 2 - initial directory block A
*   block 3 - initial directory block B
*   blocks 4-7 - allocation bit map
*
* The bitmap contains 4 * 4096 bytes * 8 bits = 130,848 bits.  Thus,
* the largest supported NETSPOOL size is about 874 cylinders on a
* 3380 DASD.
*
* The size of the NETSPOOL dataset can of course be smaller and all
* bits past the end of the file should be marked as "in-use" in the
* bitmap so they would never be allocated.
*
* The calculation for this is at FMT250.
* Example:  assume 10 cylinder file on 3380 = 150 blocks per cyl,
* or 1500 total blocks in file.
*
* Starting from block 1501 (the first block past the end of the
* dataset) divide by 8 to compute the byte number in the bitmap
* representing block 1501:
*
* 1. 1501 / 8 = 187 remainder 5
* 2. Make a byte image of X'FF' (all records unavail in byte).
* 3. Shift it to the right by the remainder (adding 0's on the left):
*       X'FF' shifted right by 5 = X'07'
* 4. Store the X'07' computed value into byte 187 of the bitmap.
* 5. All subsequent bytes 188 through the end of four blocks are X'FF'
* 6. Write the four blocks to disk.
*
FMT240   EQU   *
         GETMAIN RU,LV=16384           4 blocks of size
         LR    R4,R1
         LR    R1,R0                   COPY LENGTH
         LR    R0,R4                   -> NEW STG AREA
         SR    R15,R15                 SET PAD
         MVCL  R0,R14                  CLEAR THE PAGES
*
         MVC   0(1,R4),DATA4           Set up allocation; blocks 1-7
*                                       are initially in use
FMT250   EQU   *
         L     R7,BLKS                 Get # blocks in dataset
         LA    R7,1(,R7)               block # of first unavail blk
         SR    R6,R6                   Clear for divide
         D     R6,=F'8'                Get byte offset remainder bits
*
         AR    R7,R4                   -> byte containing bit for
*                                       first record beyond file size
         ICM   R1,8,=X'FF'             Assume all recs in byte unavail
         SRL   R1,0(R6)                Adjust for actual blocks that
*                                       do exist in same byte
         STCM  R1,8,0(R7)              Store it in map
*
         LA    R0,1(,R7)               -> next byte in map
         L     R1,=F'16384'            Stg size
         AR    R1,R4                   Point to end of it
         SR    R1,R0                   Compute length to end
         L     R15,=X'FF000000'        Set all FFs pad char
         MVCL  R0,R14                  All FFs to the end
*
*
         LA    R3,4                    Blk # of allocation map
         LA    R7,4                    # of blocks to process
         L     R6,RPL                  -> RPL
*
FMT270   EQU   *               Write map blocks 4 through 7
         ST    R3,KEY                  Set block # argument
         GET   RPL=(6)
         LTR   R15,R15                 Any errors?
         BNZ   GETFAIL                 YES
*
         LR    R0,R2                   -> block area
         LA    R1,4089                 Size of block
         LA    R5,4089                 Size of block
         MVCL  R0,R4                   Init the block
*
         PUT   RPL=(6)                 Update the block
         LTR   R15,R15                 Any errors?
         BNZ   PUTFAIL2                YES
*
         LA    R3,1(,R3)               next blk #
         BCT   R7,FMT270
         B     EXIT0                   Format success
*
*-- Error routines
*
OPENFAIL EQU   *
         MVC   LIST(L'MSG002),MSG002   Open failed
         CVD   R15,DBLE                Convert RC
         UNPK  LIST+29(2),DBLE
         OI    LIST+30,X'F0'
         USING IFGACB,R7
         UNPK  DBLE(3),ACBERFLG(2)
         TR    DBLE(2),HEXTRAN-240
         MVC   LIST+43(2),DBLE         Move error value to line
         DROP  R7
         BAL   R14,PUT                 Write open fail msg
         B     EXIT8
*
BADCISZ  EQU   *
         MVC   LIST(L'MSG004),MSG004   NETSPOOL dataset definition err
         BAL   R14,PUT                 Write msg
         MVC   LIST(L'MSG005),MSG005   CISZ must be 4096
         BAL   R14,PUT                 Write msg
         B     EXIT8
*
OPERCAN  EQU   *
         MVC   LIST(L'MSG006),MSG006   Formatting terminated by oper
         BAL   R14,PUT                 Write msg
         B     EXIT8
*
PUTFAIL  EQU   *
         MVC   LIST(L'MSG003),MSG003   PUT failed
         CVD   R5,DBLE                 Convert block number
         MVC   LIST+25(8),=X'4020202020202120'  Move edit mask
         ED    LIST+25(8),DBLE+4       Edit block count
         USING IFGRPL,R6
         UNPK  TWRK(9),RPLFDBWD(5)
         TR    TWRK(8),HEXTRAN-240
         MVC   LIST+48(2),TWRK+2       Move RTNCD value to line
         MVC   LIST+50(2),TWRK+6       Move FDBK value to line
         DROP  R6
         BAL   R14,PUT                 Write open fail msg
         B     EXIT8
*
GETFAIL  EQU   *
         MVC   LIST(L'MSG007),MSG007   Get failed
         CVD   R3,DBLE                 Convert block number
         MVC   LIST+25(8),=X'4020202020202120'  Move edit mask
         ED    LIST+25(8),DBLE+4       Edit block count
         USING IFGRPL,R6
         UNPK  TWRK(9),RPLFDBWD(5)
         TR    TWRK(8),HEXTRAN-240
         MVC   LIST+48(2),TWRK+2       Move RTNCD value to line
         MVC   LIST+50(2),TWRK+6       Move FDBK value to line
         DROP  R6
         BAL   R14,PUT                 Write open fail msg
         B     EXIT8
*
PUTFAIL2 EQU   *
         MVC   LIST(L'MSG008),MSG008   PUT failed
         CVD   R5,DBLE                 Convert block number
         MVC   LIST+26(8),=X'4020202020202120'  Move edit mask
         ED    LIST+26(8),DBLE+4       Edit block count
         USING IFGRPL,R6
         UNPK  TWRK(9),RPLFDBWD(5)
         TR    TWRK(8),HEXTRAN-240
         MVC   LIST+49(2),TWRK+2       Move RTNCD value to line
         MVC   LIST+51(2),TWRK+6       Move FDBK value to line
         DROP  R6
         BAL   R14,PUT                 Write open fail msg
         B     EXIT8
*
PUT      EQU   *
         ST    R14,SV14                Save return addr
         PUT   SYSPRINT,LIST
         MVC   LIST,BLANKS
         L     R14,SV14                Load return addr
         BR    R14                     Return
*
EXIT8    EQU   *
         BAL   R14,PUT                 Write blank
         MVC   LIST(L'MSG999),MSG999   Exited with errors
         BAL   R14,PUT                 Write msg
*
         LA    R15,8
         B     QUIT000
*
EXIT0    EQU   *
         BAL   R14,PUT                 Write blank
         MVC   LIST(L'MSG900),MSG900   Exited with success
         BAL   R14,PUT                 Write msg
*
         SR    R15,R15
*
QUIT000  EQU   *
         LR    R5,R15                  Copy exit RC
*
         TM    FLAGS1,FL1OPEN          Is ACB open?
         BZ    QUIT010                 No, skip close
         MVC   MACLIST(CLOSEL),CLOSE   Move close list
         L     R7,ACB                  -> ACB
         CLOSE ((R7)),                 Close ACB                       X
               MF=(E,MACLIST)
*
QUIT010  EQU   *
         MVC   MACLIST(CLOSEL),CLOSE   Move close list
         CLOSE (SYSPRINT),                                             X
               MF=(E,MACLIST)
*
         LM    R0,R1,RPLL
         FREEMAIN RU,LV=(0),A=(1)
*
         LM    R0,R1,ACBL
         FREEMAIN RU,LV=(0),A=(1)
*
         L     R1,BLK
         FREEMAIN RU,LV=4089,A=(1)
*
         LR    R1,R10                  -> NJEFWK work area
         L     R13,4(,R13)             -> CALLER'S SA
         FREEMAIN RU,                  Free the work area              X
               LV=NJEFSZ,                                              X
               A=(1)
*
         ST    R5,16(,R13)             Save R15 RC
         LM    R14,R12,12(R13)         RELOAD SYSTEM'S REGS
         BR    R14                     Return
*
         LTORG
*
DMYPRT   DCB   DDNAME=SYSPRINT,                                        X
               MACRF=(PM),                                             X
               DSORG=PS,                                               X
               LRECL=80,                                               X
               RECFM=FB,                                               X
               BLKSIZE=800
DMYPRTL  EQU   *-DMYPRT
*
OPEN     OPEN  0,MF=L
OPENL    EQU   *-OPEN
CLOSE    CLOSE 0,MF=L
CLOSEL   EQU   *-CLOSE
*
WTORDMY  WTOR  'NJEFMT - Reply U to proceed with format, or CANCEL',   x
               MF=L
WTORDMYL EQU   *-WTORDMY
*
MSG001   DC    C'NJEFMT - NJE38 NETSPOOL FORMAT UTILITY'
*                0123456789012345678901234567890123456789012 345 6789
MSG002   DC    C'Open failed for NETSPOOL, RC=xx,ACBERFLG=X''xx'''
*                012345678901234567890123456789012345678901234567 8901
MSG003   DC    C'PUT failed writing record xxxxxxx, RTNCD-FDBK=X''xxxx'x
               ''
MSG004   DC    C'NETSPOOL dataset definition error:'
MSG005   DC    C'  CONTROLINTERVALSIZE must be exactly 4096 bytes'
MSG006   DC    C'Formatting terminated by system operator'
MSG007   DC    C'GET failed reading record xxxxxxx, RTNCD-FDBK=X''xxxx'x
               ''
MSG008   DC    C'PUT failed updating record xxxxxxx, RTNCD-FDBK=X''xxxxX
               '''
MSG900   DC    C'Format utility completed successfully'
MSG999   DC    C'Format utility terminated with errors'
*
BLANKS   DC    CL80' '
HEXTRAN  DC    CL16'0123456789ABCDEF'
*
*
*
*
BLK1     DC    A(DATA1),A(DATA1L)      Addr and length
DATA1    DC    F'2'                    Blk # of current directory
         DC    F'4'                    Blk # of allocation map
         DC    F'0'                    # blks in dataset
         DC    F'0'                    Last assigned spool file id #
DATA1L   EQU   *-DATA1
*
BLK2     DC    A(DATA2),A(DATA2L)      Addr and length
DATA2    EQU   *
         DC    AL2(NSDIRLN)     LEN    Length of record
         DC    AL2(0)           RESV1  reserved
         DC    F'2'             BLK    blk # of 1st block of file
         DC    CL8'NETSPOOL'    INLOC
         DC    CL16' '          LINK/INTOD
         DC    CL8'DIR'         INVM
         DC    AL4(1)           RECNM  No. records in the file
*                                      Remainder of block is zeros
*
DATA2L   EQU   *-DATA2
*
BLK3     DC    A(DATA3),A(DATA3L)      Addr and length
DATA3    EQU   *
         DC    AL2(NSDIRLN)     LEN    Length of record
         DC    AL2(0)           RESV1  reserved
         DC    F'3'             BLK    blk # of 1st block of file
         DC    CL8'NETSPOOL'    INLOC
         DC    CL16' '          LINK/INTOD
         DC    CL8'DIR'         INVM
         DC    AL4(1)           RECNM  No. records in the file
*                                      Remainder of block is zeros
DATA3L   EQU   *-DATA3
*
DATA4    DC    B'11111110'             Blocks initially allocated are
*                                        blocks 1-7
*
* The rest of blocks 4 and 5,6,7 are computed at FMT250 and written
* at FMT270.
*
*
*
NJEFWK   DSECT
NJEEYE   DS    CL4'NJEF'               EYECATCHER
NJEWKLEN DS    F                       SIZE OF WORK AREA
*
NJESA    DS    18F
DBLE     DS    D
TWRK     DS    XL16
MACLIST  DS    XL128
LIST     DS    CL80                    PRINT LINE
SV14     DS    F                       R14 save area
OPECB    DS    F                       Operator reply ECB
BLK      DS    A                       -> NETSPOOL block stg area
KEY      DS    F                       Relative block number key
ACBL     DS    F                       ACB length
ACB      DS    A                       -> ACB
RPLL     DS    F                       RPL length
RPL      DS    A                       -> RPL
HIRBA    DS    F                       High allocated RBA
CISZ     DS    F                       CI Size
BLKS     DS    F                       Number of relative blocks
*
FLAGS1   DS    X
FL1OPEN  EQU   X'80'      1... ....    ACB is open
*
SYSPRINT DS    (DMYPRTL)X              SYSPRINT DCB
         DS    0D                      Force doubleword boundary
NJEFSZ   EQU   *-NJEFWK                Size of work area
*
         COPY  NETSPOOL
         IFGACB
         IFGRPL
         END
./ ADD NAME=NJESCN
*
*
*-- NJE38 - Configuration scan and create
*
*
*   Called by NJEINIT (on start up)
*   Called by NJECMX (for commands entered by users and operators)
*
*
* Change log:
*
* 04 Dec 20 - Expanded internal trace table support                v212
* 29 Nov 20 - Initial creation                                     v211
*
*
*
* notes for doc:
*
* -keywords must start in col 1
* -keywords and values 1-8 bytes only
* -last keyword or value on line must have a trailing blank (e.g,
*    column 80 must be blank)
* -CUU must be 3-digit
* -ROUTE names are not validated for existence, or character makeup
*
*
*
         GBLC  &VERS
         REGEQU
NJESCN   CSECT                                                          NJE00020
         NJEVER
         STM   R14,R12,12(R13)         Save Regs                        NJE00050
         LR    R12,R15                 Base                             NJE00060
         LA    R11,2048
         LA    R11,2048(R11,R12)       2nd Base
         USING NJESCN,R12,R11                                           NJE00070
         LR    R7,R0                   Save input code
         LR    R8,R1                   Save input parm list addr
         LR    R9,R2                   Save input parm list addr
*
         LA    R0,NJEWKSZ              Size of work area
         GETMAIN RU,                   Get local stg area              X
               LV=(0),                                                 X
               BNDRY=PAGE
         LR    R10,R1
         LR    R1,R0                   Copy length
         LR    R2,R0                   Copy length
         LR    R0,R10                  -> new stg area
         SR    R15,R15                 set pad
         MVCL  R0,R14                  Clear the stg
*
         USING NJEWK,R10
         ST    R13,NJESA+4             SAVE prv S.A. ADDR               NJE00080
         LA    R1,NJESA                -> my save area
         ST    R1,8(,R13)              Plug it into prior SA
         LR    R13,R1
*
         MVC   NJEEYE,=CL4'NJES'       Work area eyecatcher
         ST    R2,NJEWKLEN             Save size of area in area
         STM   R8,R9,INITPLST          Save entry parm list addrs
         MVC   INITPARM,0(R8)          Copy passed parameters
*
INIT000  EQU   *
         B     INIT010(R7)             Branch into request table
*
INIT010  EQU   *
         B     SCN000               00 Scan and create configuration
         B     CMD000               04 Scan a configuration command
*
SCN000   EQU   *
         MVC   CONFIG,DMYDCB           Set up DCB
         LA    R1,JFCBL                -> JFCB area
         ST    R1,EXLST                Plug into exit list
         MVI   EXLST,X'87'             Set up for JFCB retrieve
         LA    R4,CONFIG               -> DCB
         USING IHADCB,R4
         LA    R1,EXLST                -> exit list
         STCM  R1,7,DCBEXLST+1         Plug it into DCB
*
         MVC   MACLIST(RDJFCBL),RDJFCB Move macro model
         RDJFCB CONFIG,MF=(E,MACLIST)
*
         MVC   MACLIST(OPENL),OPEN     Move macro model
         OPEN  (CONFIG,INPUT),         Open dataset                    x
               MF=(E,MACLIST)
         TM    DCBOFLGS,DCBOFOPN       Did DCB open ok?
         BZ    EXIT08                  No
         DROP  R4                      IHADCB
         OI    NJFL1,F1OPEN            Indicate DCB is open
*
         LA    R3,JFCBL
         USING JFCB,R3
         MVC   WTOMSG,WTO              Move WTO model
         MVC   WTOTXT(L'NJE049I),NJE049I  Using configuration DSN
         MVC   WTOTXT+28(44),JFCBDSNM  Move DSN
         TM    JFCBIND1,JFCPDS         Using PDS member?
         BZ    SCN030                  No
         TRT   WTOTXT+28(45),BLANK     Look for end of DSN
         MVI   0(R1),C'('
         MVC   1(8,R1),JFCBELNM        Move member name
         TRT   0(10,R1),BLANK          Look for end of member name
         MVI   0(R1),C')'
         DROP  R3                      JFCB
*
SCN030   EQU   *
         WTO   ,MF=(E,WTOMSG)          Write the using config msg
         SR    R9,R9                   Init record counter
*
SCN040   EQU   *
         GET   CONFIG,REC              Get a record from CONFIG DD
         LA    R1,REC                  -> RECORD
         LA    R9,1(,R9)               Bump record count
         CLI   0(R1),C'*'              Comment line?
         BE    SCN040                  Yes, skip it
         CLI   0(R1),C' '              Blank leading off the line?
         BE    ERR073                  Yes, this is invalid
*
         OC    REC,BLANKS              Upper case the record
         LA    R15,80                  Scan length of input line
         B     CTK000                  Join common code
*
SCN100   EQU   *                       End of scan of one record
         B     SCN040                  Scan next
*
*
*
CMD000   EQU   *
         L     R2,ACMDBLOK             -> CMDBLOK
         USING CMDBLOK,R2
         MVC   REC,BLANKS              Init receiving field
         IC    R1,CMDBLEN              Len of command text
         EX    R1,OCCMD                Move and uppercase cmd image
         DROP  R2                      CMDBLOK
         LA    R15,120                 Max len of command image
         LA    R1,REC                  -> Command image
*
CTK000   EQU   *
         BAL   R14,TKN000              Parse and tokenize the cmd
         LA    R15,TOKENS-L'TOKENS     -> 0th length/token in list
         BAL   R14,GETTKN              Get 1st token
*
         CLC   =CL8'LOCAL',1(R15)      Local?
         BE    LCL000                  Yes
         CLC   =CL8'LINK',1(R15)       Link?
         BE    LNK000                  Yes
         CLC   =CL8'ROUTE',1(R15)      Route?
         BE    RTE000                  Yes
         CLC   =CL8'AUTH',1(R15)       Auth?
         BE    AUTH000                 Yes
         B     ERR076                  Unknown configuration statement
*
OCCMD    OC    REC(0),CMDTEXT-CMDBLOK(R2)  Executed instr
*
RETURN   EQU   *                       R7 branch table index
         B     SCN040               00 Read another config record
         B     EXIT00               04 End of command processing
*
*-- LOCAL
*
LCL000   EQU   *
         L     R1,ALINKS               -> LINKS anchor word
         NC    0(4,R1),0(R1)           Was LOCAL processed?
         BNZ   ERR052                  Y, only one LOCAL allowed
         LA    R0,LINKLEN              Length of LINKTABL entry
         BAL   R14,GETSTG              Get stg for entry
         LR    R8,R1
         XC    0(LINKLEN,R8),0(R8)     Initialize entry
         USING LINKTABL,R8
*
         BAL   R14,GETTKN              Get next token
         BZ    ERR075                  No local name
         MVC   LINKID,1(R15)           Local node name to entry
         TRT   LINKID,VALDNAME         Valid node name?
         BNZ   ERR043                  No
*
         MVC   LDEFUSER,DEFUSER        Set default userid
         BAL   R14,GETTKN              Get next tkn (should be DEFUSER)
         BZ    LCL090                  No other tokens
         CLC   =CL8'DEFUSER',1(R15)    Was this the DEFUSER keyword?
         BNE   ERR055                  No, error
         BAL   R14,GETTKN              Get next tkn (should be userid)
         BZ    ERR075                  Missing userid
         MVC   LDEFUSER,1(R15)         Set default userid of choice
         DROP  R8                      LINKTABL
*
LCL090   EQU   *
         L     R15,ALINKS              -> LINKS anchor word
         ST    R8,0(,R15)              Start LINKS chain
         B     RETURN(R7)              Resume scan
*
*
*-- LINK
*
LNK000   EQU   *
         L     R1,ALINKS               -> LINKS anchor word
         NC    0(4,R1),0(R1)           Was LOCAL processed?
         BZ    ERR053                  No; it is required
         LA    R8,BLDBUF               Temp area to build entry
         XC    0(LINKLEN,R8),0(R8)     Initialize entry
         USING LINKTABL,R8
         MVC   LBUFF,=H'1012'          Set default buffer size
*
         BAL   R14,GETTKN              Get next token
         BZ    ERR075                  No link name
         MVC   LINKID,1(R15)           Link node name to entry
         TRT   LINKID,VALDNAME         Valid node name?
         BNZ   ERR043                  No
*
LNK010   EQU   *
         BAL   R14,GETTKN              Get next tkn
         BZ    LNK050                  None
*
         CLC   =CL8'LINE',1(R15)       Was it a LINE keyword?
         BE    LNE000                  Yes
         CLC   =CL8'BUFF',1(R15)       Was it a BUFF keyword?
         BE    BUF000                  Yes
         CLC   =CL8'AUTO',1(R15)       Was it a AUTO keyword?
         BE    ATO000                  Yes
         CLC   =CL8'OFF',1(R15)        Was it the OFF keyword?
         BE    LOFF000                 Yes
         B     ERR055                  Unrecognized keyword
*
LNK050   EQU   *
         TM    NJFL2,F2LINE            Was line CUU found?
         BZ    ERR077                  No, its required
*
*-- LINK successfully scanned.  Now add the LINKTABL entry to chain.
*
         L     R2,ALINKS               -> LINKS anchor word (0th entry)
         L     R2,0(,R2)               -> First LINKTABL e.g. LOCAL
*
LNK060   EQU   *
         ICM   R3,15,LNEXT-LINKTABL(R2) -> next LINKTABL entry
         BZ    LNK080                  Found the end
         CLC   LINKID,LINKID-LINKTABL(R3)   Match on link name?
         BE    LNK120                  Yes, trying to add duplicate
*
LNK070   EQU   *
         LR    R2,R3                   Copy next entry ptr
         B     LNK060                  Keep scanning for end
*
LNK080   EQU   *
         LA    R0,LINKLEN              Size of LINKTABL entry
         BAL   R14,GETSTG              Get an actual entry
         MVC   0(LINKLEN,R1),0(R8)     Make build entry a permanent one
         ST    R1,LNEXT-LINKTABL(,R2)  Add R1 LINKTABL to chain end
*
LNK090   EQU   *
         LTR   R7,R7                   Doing CONFIG scan?
         BZ    SCN100                  Yes, Resume scan
*
*--Issue LINK cmd success msg
         MVC   WTOMSG,WTO              Move WTO model
         MVC   WTOTXT(L'NJE066I),NJE066I
         MVC   WTOTXT+13(8),LINKID     Move link name to msg
         TRT   WTOTXT+13(9),BLANK      Look for end
         MVC   1(7,R1),=CL7'defined'
         LA    R0,22(,R1)              -> end of msg
         LA    R1,WTOTXT               -> start of msg
         SR    R0,R1                   Compute length to display
         B     SUCCMSG                 Issue success msg and exit
*
LNK120   EQU   *                    ** Here for duplicate entry
*--Issue LINK cmd duplicate msg
         MVC   WTOMSG,WTO              Move WTO model
         MVC   WTOTXT(L'NJE048E),NJE048E
         MVC   WTOTXT+13(8),LINKID     Move link name to msg
         TRT   WTOTXT+13(9),BLANK      Look for end
         MVC   1(20,R1),=CL20'duplicate definition'
         LTR   R7,R7                   Doing CONFIG scan?
         BZ    LNK130                  Yes, issue duplicate msg
         MVC   1(20,R1),=CL20'is already defined'
*
LNK130   EQU   *
         LA    R0,34(,R1)              -> end of msg
         LA    R1,WTOTXT               -> start of msg
         SR    R0,R1                   Compute length to display
         B     ERRTYPE(R7)             Issue dup msg
*
LNE000   EQU   *
         BAL   R14,GETTKN              Get next tkn
         BZ    ERR075                  Missing CUU
         CLI   0(R15),X'02'            Check keyword length
         BNE   ERR078                  Not valid CUU
         LR    R3,R1                   Save R1 across TRT
         TRT   1(3,R15),INVALHEX       Valid hex chars?
         BNZ   ERR078                  Not valid cuu
*
         LR    R1,R3                   Restore R1
         MVC   TWRK(3),1(R15)          Move the character CUU
         TR    TWRK(3),TRANHEX-192     Make all A-F chars = xFA-XFF
*
         PACK  DBLE(3),TWRK(4)         Strip the zones
         MVC   LDEFLINE,DBLE           Move to LINKTABL entry
         OI    NJFL2,F2LINE            Indicate valid LINE CUU found
         B     LNK010                  Continue LINK token eval
*
BUF000   EQU   *
         BAL   R14,GETTKN              Get next tkn
         BZ    ERR075                  Missing buffersize
         SR    R3,R3                   Clear for IC
         IC    R3,0(,R15)              Length of value characters
         EX    R3,BFMVC                Make a copy of value
         EX    R3,BFOC                 Force copy to be numeric
         EX    R3,BFCLC                Was original numeric?
         BNE   ERR054                  Invalid BUFF value
         EX    R3,BFPACK               Pack the value
         CVB   R0,DBLE                 Get binary value
         CH    R0,=H'300'              Too small?
         BL    ERR054                  Yes
         CH    R0,=H'4020'             Too large?
         BH    ERR054                  Yes
         STH   R0,LBUFF                Else set specified BUFF size
         B     LNK010                  Continue LINK token eval
*
BFMVC    MVC   DBLE(0),1(R15)          executed instr
BFOC     OC    DBLE(0),=8C'0'          executed instr
BFCLC    CLC   DBLE(0),1(R15)          executed instr
BFPACK   PACK  DBLE(8),1(0,R15)        executed instr
*
ATO000   EQU   *
         BAL   R14,GETTKN              Get next tkn
         BZ    ERR075                  Missing YES/NO
         CLC   =CL8'YES',1(R15)        Was it yes?
         BE    ATO020
         CLC   =CL8'NO',1(R15)         Was it no?
         BE    LNK010                  Line will not be autostartable
         B     ERR078                  Unrecognized value
*
ATO020   EQU   *
         OI    LFLAG,LAUTO             Set line auto-startable
         B     LNK010                  Continue LINK token eval
*
LOFF000  EQU   *
         LTR   R7,R7                   Doing config scan?
         BZ    ERR055                  OFF not recognized in CONFIG
         L     R2,ALINKS               -> LINKS anchor word
         L     R2,0(,R2)               -> 1st entry (LOCAL entry)
*
LOFF010  EQU   *
         ICM   R3,15,LNEXT-LINKTABL(R2) -> next link entry
         BZ    LOFF050                 Found the end, linkid not fnd
         CLC   LINKID,LINKID-LINKTABL(R3) Match on name?
         BNE   LOFF020                 No, next
*
         TM    LFLAG-LINKTABL(R3),LCONNECT+LACTIVE+LDRAIN  Link busy?
         BNZ   ERR046                  Yes, can't remove it
         CLC   LTCBA-LINKTABL(,R3),=A(0)  Task active on link?
         BNE   ERR046                  Nonzero, can't remove it
*
* Remove the matching entry from the LINKTABL chain:
         MVC   LNEXT-LINKTABL(,R2),LNEXT-LINKTABL(R3)
*
         MVC   WTOMSG,WTO              Move WTO model
         MVC   WTOTXT(L'NJE046I),NJE046I
         MVC   WTOTXT+13(8),LINKID-LINKTABL(R3) Move link name to msg
         TRT   WTOTXT+13(9),BLANK      Look for end
         MVC   1(7,R1),=CL7'deleted'
         LA    R2,13+8(,R1)            -> end of msg
         LA    R1,WTOTXT               -> start of msg
         SR    R2,R1                   Compute length to display
         FREEMAIN RU,                  Free entry that was removed v211x
               LV=LINKLEN,                                         v211x
               A=(3),                                              v211x
               SP=1                                                v211
*
***      LA    R3,LTRMECB-LINKTABL(,R3) -> task termination ECB
***      POST  (3),255                 Signal NJEINIT to delete link
*
         LR    R0,R2                   Msg length to R0
         B     SUCCMSG                 Issue success msg
*
LOFF020  EQU   *
         LR    R2,R3                   Copy next entry
         B     LOFF010                 Keep scanning for end
*
LOFF050  EQU   *
         MVC   WTOMSG,WTO              Move WTO model
         MVC   WTOTXT(L'NJE045I),NJE045I
         MVC   WTOTXT+13(8),LINKID     Move name to msg
         TRT   WTOTXT+13(9),BLANK       Look for end
         MVC   1(14,R1),=CL14'is not defined'
         LA    R0,13+15(,R1)           -> end of msg
         LA    R1,WTOTXT               -> start of msg
         SR    R0,R1                   Compute length to display
         B     SUCCMSG                 Issue success msg
         DROP  R8                      LINKTABL
*
*-- ROUTE
*
RTE000   EQU   *
         L     R1,ALINKS               -> LINKS anchor word
         NC    0(4,R1),0(R1)           Was LOCAL processed?
         BZ    ERR053                  No; it is required
         LA    R8,BLDBUF               Temp area to build entry
         XC    0(ROUTSIZE,R8),0(R8)    Initialize entry
         USING RTE,R8
         MVC   ROUTALT1,BLANKS         Init
         MVC   ROUTALT2,BLANKS         Init
         MVC   ROUTALT3,BLANKS         Init
*
         BAL   R14,GETTKN              Get next token
         BZ    ERR075                  No link name
         MVC   ROUTNAME,1(R15)         Route node name destination
         TRT   ROUTNAME,VALDNAME       Valid node name?
         BZ    RTE010                  Yes
         CLM   R2,1,=C'*'              Was wildcard in use?
         BNE   ERR043                  No. The name contains inv char
*
RTE010   EQU   *
         BAL   R14,GETTKN              Get next tkn
         BZ    RTE050                  None
*
         CLC   =CL8'TO',1(R15)         Was it the TO keyword?
         BE    TO000                   Yes
         CLC   =CL8'ALT',1(R15)        Was it a ALT keyword?
         BE    ALT000                  Yes
         CLC   =CL8'OFF',1(R15)        Was it the OFF keyword?
         BE    ROFF000                 Yes
         B     ERR055                  Unrecognized keyword
*
RTE050   EQU   *
         TM    NJFL2,F2RTO             Was ROUTE TO processed?
         BZ    ERR051                  No, its required
*
*-- ROUTE successfully scanned.
*
*  1. first check for existing name; if so, update existing.
*  2. else add new route in collating sequence, except that the
*     wildcard character (if present) is treated as a X'FF' character
*     in order to force wildcard routes after all explicity named
*     routes.
*
         L     R2,AROUTES              -> ROUTES anchor word (0th RTE)
*
RTE060   EQU   *
         ICM   R3,15,ROUTPTR-RTE(R2)   -> next RTE entry
         BZ    RTE080                  Found the end
         CLC   ROUTNAME,ROUTNAME-RTE(R3)   Match on name?
         BE    RTE070                  Yes, update duplicate
         LR    R2,R3                   Copy next entry
         B     RTE060                  Keep scanning for end
*
*-- Update existing route
RTE070   EQU   *
         L     R0,ROUTPTR-RTE(,R3)     Save RTE next ptr
         MVC   0(ROUTSIZE,R3),0(R8)    Update&replace existing route
         ST    R0,ROUTPTR-RTE(,R3)     Restore the next ptr
         B     RTE200                  Issue success msg
*
*-- Add new route to chain in collating seq based on route name
RTE080   EQU   *
         L     R2,AROUTES              -> ROUTES anchor word (0th RTE)
         MVC   DBLE,ROUTNAME           Copy name we want to add
         TR    DBLE,ASTER              Set any * char high
*
RTE090   EQU   *
         ICM   R3,15,ROUTPTR-RTE(R2)   -> next RTE entry
         BZ    RTE100                  Found the end; add to end
         MVC   TWRK,ROUTNAME-RTE(R3)   Copy name in chained RTE
         TR    TWRK,ASTER              Set any * char high
         CLC   DBLE,TWRK               Locate place to insert RTE
         BL    RTE100
         LR    R2,R3                   Copy next entry
         B     RTE090                  Keep scanning for end
*
RTE100   EQU   *
         LA    R0,ROUTSIZE             Length of RTE entry
         BAL   R14,GETSTG              Get stg for entry
         MVC   0(ROUTSIZE,R1),0(R8)    Make build entry a permanent one
         ST    R1,ROUTPTR-RTE(,R2)     Insert R1 RTE into chain
         ST    R3,ROUTPTR-RTE(,R1)     R1 RTE now points to next RTE
*
RTE200   EQU   *
         LTR   R7,R7                   Doing CONFIG scan?
         BZ    SCN100                  Yes, Resume scan
*
*--Issue ROUTE cmd success msg
         MVC   WTOMSG,WTO              Move WTO model
         MVC   WTOTXT(L'NJE065I),NJE065I
         MVC   WTOTXT+13(8),ROUTNAME   Move route name to msg
         TRT   WTOTXT+13(9),BLANK      Look for end
         MVC   1(19,R1),=CL19'routed through link'
         MVC   21(8,R1),ROUTNEXT       Move link name to msg
         LA    R0,21+8(,R1)            -> end of msg
         LA    R1,WTOTXT               -> start of msg
         SR    R0,R1                   Compute length to display
         B     SUCCMSG                 Issue success msg and exit
*
TO000    EQU   *
         BAL   R14,GETTKN              Get next tkn
         BZ    ERR075                  Missing node id
         MVC   ROUTNEXT,1(R15)         Move the route-to link id
         TRT   ROUTNEXT,VALDNAME       Valid node name?
         BNZ   ERR043                  Invalid node name if not
         OI    NJFL2,F2RTO             Indicate ROUTE TO processed
         B     RTE010                  Continue route scan
*
ALT000   EQU   *
         LA    R3,ROUTALT1             -> first alternate node id slot
         LA    R4,3                    Max number of alternates
*
ALT010   EQU   *
         BAL   R14,GETTKN              Get next tkn
         BZ    RTE050                  Done with route scan
         MVC   0(8,R3),1(R15)          Move the route alternate id
         LA    R3,8(,R3)               -> next alternate slot
         BCT   R4,ALT010               Continue route scan
         B     RTE050                  Done with route scan
*
ROFF000  EQU   *
         LTR   R7,R7                   Doing config scan?
         BZ    ERR055                  OFF not recognized in CONFIG
         L     R2,AROUTES              -> ROUTES anchor  (0th entry)
*
ROFF010  EQU   *
         ICM   R3,15,ROUTPTR-RTE(R2)   -> next RTE entry
         BZ    ROFF050                 Found the end, user/node not fnd
         CLC   ROUTNAME,ROUTNAME-RTE(R3)   Match on name?
         BNE   ROFF020                 No, next
*
* Remove the matching entry from the RTE chain:
         MVC   ROUTPTR-RTE(,R2),ROUTPTR-RTE(R3)
*
         MVC   WTOMSG,WTO              Move WTO model
         MVC   WTOTXT(L'NJE064I),NJE064I
         MVC   WTOTXT+23(8),ROUTNAME-RTE(R3) Move route name to msg
         TRT   WTOTXT+23(9),BLANK      Look for end
         MVC   1(7,R1),=CL7'deleted'
         LA    R2,23+8(,R1)            -> end of msg
         LA    R1,WTOTXT               -> start of msg
         SR    R2,R1                   Compute length to display
         FREEMAIN RU,                  Free entry that was removed     x
               LV=ROUTSIZE,                                            x
               A=(3),                                                  x
               SP=1
         LR    R0,R2                   Msg length to R0
         B     SUCCMSG                 Issue success msg
*
ROFF020  EQU   *
         LR    R2,R3                   Copy next entry
         B     ROFF010                 Keep scanning for end
*
ROFF050  EQU   *
         MVC   WTOMSG,WTO              Move WTO model
         MVC   WTOTXT(L'NJE063I),NJE063I
         MVC   WTOTXT+13(8),ROUTNAME   Move name to msg
         TRT   WTOTXT+13(9),BLANK       Look for end
         MVC   1(13,R1),=CL13'is not routed'
         LA    R0,13+14(,R1)           -> end of msg
         LA    R1,WTOTXT               -> start of msg
         SR    R0,R1                   Compute length to display
         B     SUCCMSG                 Issue success msg
         DROP  R8                      RTE
*
*-- AUTH
*
AUTH000  EQU   *
         L     R1,ALINKS               -> LINKS anchor word
         NC    0(4,R1),0(R1)           Was LOCAL processed?
         BZ    ERR053                  No; it is required
         LA    R8,BLDBUF               Temp area to build entry
         XC    0(AUTHSIZE,R8),0(R8)    Initialize entry
         USING AUTHLIST,R8
*
         BAL   R14,GETTKN              Get next token
         BZ    ERR075                  No userid
         MVC   AUTHUSER,1(R15)         Authorized userid
*
AUTH010  EQU   *
         BAL   R14,GETTKN              Get next tkn
         BZ    AUTH050                 None
*
         CLC   =CL8'AT',1(R15)         Was it the AT keyword?
         BE    AT000                   Yes
         CLC   =CL8'OFF',1(R15)        Was it the OFF keyword?
         BE    AOFF000                 Yes
         B     ERR055                  Unrecognized keyword
*
AUTH050  EQU   *
         TM    NJFL2,F2AAT             Was AUTH AT processed?
         BZ    ERR050                  No, its required
*
*-- AUTH successfully scanned.  Now add the AUTH entry to chain.
*
         L     R2,AAUTHS               -> AUTHS anchor word (0th entry)
*
AUTH060  EQU   *
         ICM   R3,15,AUTHPTR-AUTHLIST(R2) -> next AUTHLIST entry
         BZ    AUTH080                 Found the end
         CLC   AUTHUSER,AUTHUSER-AUTHLIST(R3)   Match on userid?
         BNE   AUTH070                 No, next
         CLC   AUTHNODE,AUTHNODE-AUTHLIST(R3)   Match on node?
         BE    AUTH120                 Yes, trying to add duplicate
*
AUTH070  EQU   *
         LR    R2,R3                   Copy next entry ptr
         B     AUTH060                 Keep scanning for end
*
AUTH080  EQU   *
         LA    R0,AUTHSIZE             Size of AUTHLIST entry
         BAL   R14,GETSTG              Get an actual entry
         MVC   0(AUTHSIZE,R1),0(R8)    Make build entry a permanent one
         ST    R1,AUTHPTR-AUTHLIST(,R2) Add R1 AUTHLIST to chain end
*
AUTH090  EQU   *
         LTR   R7,R7                   Doing CONFIG scan?
         BZ    SCN100                  Yes, Resume scan
*
*--Issue AUTH cmd success msg
         MVC   WTOMSG,WTO              Move WTO model
         MVC   WTOTXT(L'NJE068I),NJE068I
         MVC   WTOTXT+13(8),AUTHUSER   Move auth name to msg
         TRT   WTOTXT+13(9),BLANK      Look for end
         MVC   1(2,R1),=CL2'at'
         MVC   4(8,R1),AUTHNODE        Move link name to msg
         TRT   4(9,R1),BLANK           Look for end
         MVC   1(17,R1),=CL17'is now authorized'
         LA    R0,18(,R1)              -> end of msg
         LA    R1,WTOTXT               -> start of msg
         SR    R0,R1                   Compute length to display
         B     SUCCMSG                 Issue success msg and exit
*
AUTH120  EQU   *                    ** Here for duplicate entry
         LTR   R7,R7                   Doing CONFIG scan?
         BZ    SCN100                  Yes, skip duplicate msg
*--Issue AUTH cmd duplicate msg
         MVC   WTOMSG,WTO              Move WTO model
         MVC   WTOTXT(L'NJE067I),NJE067I
         MVC   WTOTXT+13(8),AUTHUSER   Move auth name to msg
         TRT   WTOTXT+13(9),BLANK      Look for end
         MVC   1(2,R1),=CL2'at'
         MVC   4(8,R1),AUTHNODE        Move link name to msg
         TRT   4(9,R1),BLANK           Look for end
         MVC   1(21,R1),=CL21'is already authorized'
         LA    R0,22(,R1)              -> end of msg
         LA    R1,WTOTXT               -> start of msg
         SR    R0,R1                   Compute length to display
         B     ERRMSG                  Issue dup msg to cmd issuer
*
AT000    EQU   *
         BAL   R14,GETTKN              Get next tkn
         BZ    ERR075                  Missing node id
         MVC   AUTHNODE,1(R15)         Move the auth node id to list
         TRT   AUTHNODE,VALDNAME       Valid node name?
         BNZ   ERR043                  No
         OI    NJFL2,F2AAT             Indicate AUTH AT processed
         B     AUTH010                 Continue AUTH scan
*
AOFF000  EQU   *
         LTR   R7,R7                   Doing config scan?
         BZ    ERR055                  OFF not recognized in CONFIG
         L     R2,AAUTHS               -> AUTHS anchor word (0th entry)
*
AOFF010  EQU   *
         ICM   R3,15,AUTHPTR-AUTHLIST(R2) -> next AUTHLIST entry
         BZ    AOFF050                 Found the end, user/node not fnd
         CLC   AUTHUSER,AUTHUSER-AUTHLIST(R3)   Match on userid?
         BNE   AOFF020                 No, next
         CLC   AUTHNODE,AUTHNODE-AUTHLIST(R3)   Match on node?
         BNE   AOFF020                 No, next
*
* Remove the matching entry from the AUTHLIST chain:
         MVC   AUTHPTR-AUTHLIST(,R2),AUTHPTR-AUTHLIST(R3)
*
         MVC   WTOMSG,WTO              Move WTO model
         MVC   WTOTXT(L'NJE070I),NJE070I
         MVC   WTOTXT+13(8),AUTHUSER-AUTHLIST(R3) Move auth name to msg
         TRT   WTOTXT+13(9),BLANK      Look for end
         MVC   1(2,R1),=CL2'at'
         MVC   4(8,R1),AUTHNODE-AUTHLIST(R3) Move node name to msg
         TRT   4(9,R1),BLANK           Look for end
         MVC   1(23,R1),=CL23'is no longer authorized'
         LA    R2,24(,R1)              -> end of msg
         LA    R1,WTOTXT               -> start of msg
         SR    R2,R1                   Compute length to display
         FREEMAIN RU,                  Free entry that was removed     x
               LV=AUTHSIZE,                                            x
               A=(3),                                                  x
               SP=1
         LR    R0,R2                   Msg length to R0
         B     SUCCMSG                 Issue success msg
*
AOFF020  EQU   *
         LR    R2,R3                   Copy next entry
         B     AOFF010                 Keep scanning for end
*
AOFF050  EQU   *
         MVC   WTOMSG,WTO              Move WTO model
         MVC   WTOTXT(L'NJE069I),NJE069I
         MVC   WTOTXT+13(8),AUTHUSER   Move auth name to msg
         TRT   WTOTXT+13(9),BLANK       Look for end
         MVC   1(2,R1),=CL2'at'
         MVC   4(8,R1),AUTHNODE        Move link name to msg
         TRT   4(9,R1),BLANK           Look for end
         MVC   1(36,R1),=CL36'was not found in authorization table'
         LA    R0,37(,R1)              -> end of msg
         LA    R1,WTOTXT               -> start of msg
         SR    R0,R1                   Compute length to display
         DROP  R8                      AUTHLIST
         B     SUCCMSG                 Issue success msg
*
*
*-- Get next tokenized length/value pair
*   Entry:  R15 -> Current pair
*   Exit:  CC  =0, R15 unchanged, no more tokens
*          CC ¬=0, R15 -> next pair
*
GETTKN   EQU   *
         LA    R15,L'TOKENS(,R15)      -> next length/token pair
         CLI   0(R15),X'FF'            No length available?
         BNER  R14                     Exit with pair -> R15
         S     R15,=A(L'TOKENS)        Back to previous token
         CLI   *+1,0                   Set CC=0
         BR    R14                     Exit with tkn ptr not changed
*
*-- Get a storage area
*   Entry:  R0 =  length to obtain
*   Exit:   R1 -> new stg area
*
GETSTG   EQU   *
         STM   R14,R15,SV14GT          Save regs
         GETMAIN RU,                   Get requested stg for block     x
               LV=(0),                                                 x
               SP=1                    All configuration elements SP=1
         LM    R14,R15,SV14GT          Reload regs
         BR    R14                     Return with stg addr in R1
*
TKN000   EQU   *
         MVI   NJFL2,0                 Clear tokenization ctl flags
         LR    R5,R1                   Save start of parse position
         LR    R3,R1                   Start position to R3
         BCTR  R15,0                   Make scan length IBM length
         MVC   TOKENS(12*8),TKNINIT    Init receiving fields
         LA    R6,TOKENS               -> token area
*
TKN040   EQU   *
         EX    R15,SCANBL              Look for blank at end of token
*SCANBL  TRT   0(0,R3),BLANK
         BZ    ERR074                  Syntax error
         SR    R1,R3                   Compute token length
         C     R1,=F'8'                Max length of token is 8
         BH    ERR074                  Syntax error
         BCTR  R1,0
         EX    R1,MVTKN                Save the token
*MVTKN   MVC   1(0,R6),0(R3)
         STC   R1,0(,R6)               Save its length
         LA    R6,9(,R6)               -> next token area
         LA    R1,1(,R1)               Restore length relative to 1
         AR    R3,R1                   -> next byte in line
         SR    R15,R1                  Reduce remaining length
         BNPR  R14                     Done with line
*
         EX    R15,SCANNBL             Look for next token
*SCANNBL TRT   0(0,R3),NONBLANK
         BZR   R14                     Nothing else on line
         SR    R1,R3                   Compute length to that token
         AR    R3,R1                   -> next byte in line
         SR    R15,R1                  Reduce remaining length
         BNPR  R14                     Done with line
*
TKN090   EQU   *
         B     TKN040                  Continue scanning
*
SCANBL   TRT   0(0,R3),BLANK           executed instr
SCANNBL  TRT   0(0,R3),NONBLANK        executed instr
MVTKN    MVC   1(0,R6),0(R3)           executed instr
*
EOD000   EQU   *
         B     EXIT00
*
*
ERR043   EQU   *
         LA    R3,1(,R15)              -> failing keyword token
         BAL   R14,CFGERR              Show failing stmt
         MVC   WTOMSG,WTO              Move WTO model
         MVC   WTOTXT(L'NJE042E),NJE042E  Node name invalid chars
         LA    R0,L'NJE042E            Length of msg
         B     ERRTYPE(R7)
*
ERR046   EQU   *
         LA    R3,1(,R15)              -> failing keyword token
         BAL   R14,CFGERR              Show failing stmt
         MVC   WTOMSG,WTO              Move WTO model
         MVC   WTOTXT(L'NJE044E),NJE044E  LINK is not inactive
         MVC   WTOTXT+13(8),LINKID-LINKTABL(R8)  LINK name
         TRT   WTOTXT+13(9),BLANK       Look for end
         MVC   1(15,R1),=CL15'is still active'
         LA    R0,13+16(,R1)           -> end of msg
         LA    R1,WTOTXT               -> start of msg
         SR    R0,R1                   Compute length to display
         B     ERRTYPE(R7)
*
ERR050   EQU   *
         LA    R3,1(,R15)              -> failing keyword token
         BAL   R14,CFGERR              Show failing stmt
         MVC   WTOMSG,WTO              Move WTO model
         MVC   WTOTXT(L'NJE050E),NJE050E  AUTH AT required
         LA    R0,L'NJE050E            Length of msg
         B     ERRTYPE(R7)
*
ERR051   EQU   *
         LA    R3,1(,R15)              -> failing keyword token
         BAL   R14,CFGERR              Show failing stmt
         MVC   WTOMSG,WTO              Move WTO model
         MVC   WTOTXT(L'NJE051E),NJE051E  ROUTE TO required
         LA    R0,L'NJE051E            Length of msg
         B     ERRTYPE(R7)
*
ERR052   EQU   *
         LA    R3,1(,R15)              -> failing keyword token
         BAL   R14,CFGERR              Show failing stmt
         MVC   WTOMSG,WTO              Move WTO model
         MVC   WTOTXT(L'NJE052E),NJE052E  More than one LOCAL stmt
         LA    R0,L'NJE052E            Length of msg
         B     ERRTYPE(R7)
*
ERR053   EQU   *
         LA    R3,1(,R15)              -> failing keyword token
         BAL   R14,CFGERR              Show failing stmt
         MVC   WTOMSG,WTO              Move WTO model
         MVC   WTOTXT(L'NJE053E),NJE053E  No LOCAL stmt
         LA    R0,L'NJE053E            Length of msg
         B     ERRTYPE(R7)
*
ERR054   EQU   *
         S     R15,=A(L'TOKENS)        Back to previous token
         LA    R3,1(,R15)              -> failing keyword token
         BAL   R14,CFGERR              Show failing stmt
         MVC   WTOMSG,WTO              Move WTO model
         MVC   WTOTXT(L'NJE054E),NJE054E  invalid BUFF value
         LA    R0,L'NJE054E            Length of msg
         B     ERRTYPE(R7)
*
ERR055   EQU   *
         LA    R3,1(,R15)              -> failing keyword token
         BAL   R14,CFGERR              Show failing stmt
         MVC   WTOMSG,WTO              Move WTO model
         MVC   WTOTXT(L'NJE055E),NJE055E  invalid value after keywd
         MVC   WTOTXT+29(8),0(R3)      Show failed keyword
         LA    R0,L'NJE055E            Length of msg
         B     ERRTYPE(R7)
*
ERR073   EQU   *
         BAL   R14,CFGERR              Show failing stmt
         MVC   WTOMSG,WTO              Move WTO model
         MVC   WTOTXT(L'NJE073E),NJE073E  keywd not in col 1
         LA    R0,L'NJE073E            Length of msg
         B     ERRTYPE(R7)
*
ERR074   EQU   *
         BAL   R14,CFGERR              Show failing stmt
         MVC   WTOMSG,WTO              Move WTO model
         MVC   WTOTXT(L'NJE074E),NJE074E  syntax error
         SR    R3,R5                   Compute column number
         LA    R3,1(,R3)               Make rel to 1
         CVD   R3,DBLE
         UNPK  WTOTXT+53(2),DBLE
         OI    WTOTXT+54,X'F0'         Fix sign
         LA    R0,L'NJE074E            Length of msg
         B     ERRTYPE(R7)
*
ERR075   EQU   *
         LA    R3,1(,R15)              -> failing keyword token
         BAL   R14,CFGERR              Show failing stmt
         MVC   WTOMSG,WTO              Move WTO model
         MVC   WTOTXT(L'NJE075E),NJE075E  syntax error
         MVC   WTOTXT+36(8),0(R3)      Show failed keyword
         LA    R0,L'NJE075E            Length of msg
         B     ERRTYPE(R7)
*
ERR076   EQU   *
         BAL   R14,CFGERR              Show failing stmt
         MVC   WTOMSG,WTO              Move WTO model
         MVC   WTOTXT(L'NJE076E),NJE076E  unknown config stmt type
         LA    R0,L'NJE076E            Length of msg
         B     ERRTYPE(R7)
*
ERR077   EQU   *
         BAL   R14,CFGERR              Show failing stmt
         MVC   WTOMSG,WTO              Move WTO model
         MVC   WTOTXT(L'NJE077E),NJE077E  line addr required
         LA    R0,L'NJE077E            Length of msg
         B     ERRTYPE(R7)
*
ERR078   EQU   *
         S     R15,=A(L'TOKENS)        Back to previous token
         LA    R3,1(,R15)              -> failing keyword token
         BAL   R14,CFGERR              Show failing stmt
         MVC   WTOMSG,WTO              Move WTO model
         MVC   WTOTXT(L'NJE078E),NJE078E  invalid value after keywd
         MVC   WTOTXT+46(8),0(R3)      Show failed keyword
         LA    R0,L'NJE078E+8          Length of msg
         B     ERRTYPE(R7)
*
ERRTYPE  EQU   *                       R7 branch table index
         B     ERRWTO               00 Issue WTO to console
         B     ERRMSG               04 Return msg len/text to caller
*
ERRWTO   EQU   *
         WTO   ,MF=(E,WTOMSG)
         B     EXIT08
*
ERRMSG   EQU   *
         L     R1,AMTEXT               -> callers MTEXT area
         MVC   0(120,R1),WTOTXT        Pass back the msg text
         B     EXIT08                  and exit to caller with error
*
SUCCMSG  EQU   *
         L     R1,AMTEXT               -> callers MTEXT area
         MVC   0(120,R1),WTOTXT        Pass back the msg text
         B     EXIT00                  and exit to caller with success
*
CFGERR   EQU   *
         LTR   R7,R7                   Processing CONFIG member?
         BNZR  R14                     No; skip config msgs
         ST    R14,SV14CF              Save return
         MVC   WTOMSG,WTO              Move WTO model
         MVC   WTOTXT(L'NJE072E),NJE072E
         CVD   R9,DBLE                 convert record #
         MVC   WTOTXT+45(3),=X'202120' Move edit mask
         ED    WTOTXT+44(4),DBLE+6     Edit record number
         WTO   ,MF=(E,WTOMSG)
         MVC   WTOMSG,WTO              Move WTO model
         MVC   WTOTXT(7),NJE072E       Move just msg number
         MVI   WTOTXT+8,C''''          Move apost
         MVC   WTOTXT+09(52),REC       Move first part of record image
         MVI   WTOTXT+61,C''''         Move apost
         WTO   ,MF=(E,WTOMSG)
         L     R14,SV14CF              Load return
         BR    R14
*
EXIT08   EQU   *
         LR    R6,R0                   Msg length to R6 for now
         LA    R5,8                    RC=8
         B     XIT000
*
EXIT00   EQU   *
         LR    R6,R0                   Possible msg len to R6 for now
         SR    R5,R5                   RC=0
*                                                                       NJE00200
XIT000   EQU   *                                                        NJE00210
         TM    NJFL1,F1OPEN            Is DCB open?
         BZ    XIT010                  No
         MVC   MACLIST(CLOSEL),CLOSE   Move macro model
         CLOSE (CONFIG),                                               x
               MF=(E,MACLIST)
         FREEPOOL CONFIG
*
XIT010   EQU   *
*
XIT090   EQU   *
         LR    R1,R10                  -> NJEWK main work area page
         L     R13,4(,R13)             -> caller's sa                   NJE00210
         STM   R5,R6,16(R13)           Set RC, msg len in SA R15, R0
*
         FREEMAIN RU,                                                  x
               LV=NJEWKSZ,                                             x
               A=(1)
         LM    R14,R12,12(R13)         Reload system's regs             NJE00220
         BR    R14                     Return                           NJE00240
         DROP  R12
         LTORG ,
*
DMYDCB   DCB   DDNAME=CONFIG,                                          x
               MACRF=GM,                                               x
               DSORG=PS,                                               x
               LRECL=80,                                               x
               RECFM=FB,                                               x
               EODAD=EOD000
DMYDCBL  EQU   *-DMYDCB
*
*               1234567890123456789012345678901234567890123456789012345
WTO      WTO   '                                                       x
                                                                       x
                        ',MF=L
*              67890123456789012345678901234567890123456789012345678901
WTOL     EQU   *-WTO
*
*
         DS    0D
BLANKS   DC    CL120' '
NONBLANK DC    64X'FF',X'00'           TR Table to locate nonblank
INVALHEX DC    193X'FF'                TR table to locate invalid hex
         DC    6X'00'     A-F
         DC    41X'FF'
         DC    10X'00'    0-9
         DC    6X'FF'
BLANK    DC    64X'00',X'FF',191X'00'  TR Table to locate blanks
*
ASTER    DC    256AL1(*-ASTER)         TR table to set asterisk high
         ORG   ASTER+C'*'               Set * high, only
         DC    X'FF'
         ORG   ,
*
HEXTRAN  DC    CL16'0123456789ABCDEF'  Translate table
*                 0 1 2 3 4 5 6 7 8 9 A B C D E F
TRANHEX  DC    X'00FAFBFCFDFEFF000000000000000000' C
         DC    X'00000000000000000000000000000000' D
         DC    X'00000000000000000000000000000000' E
         DC    X'F0F1F2F3F4F5F6F7F8F9000000000000' F
*
*                 0 1 2 3 4 5 6 7 8 9 A B C D E F
VALDNAME DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 0 Invalid node name
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 1 characters
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 2
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 3
         DC    X'00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 4  Blank=ok/delim
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFF5CFFFFFF' 5  * indicator
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 6
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 7
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 8
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 9
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' A
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' B  valid =
         DC    X'FF000000000000000000FFFFFFFFFFFF' C  C1-C9
         DC    X'FF000000000000000000FFFFFFFFFFFF' D  D1-D9
         DC    X'FFFF0000000000000000FFFFFFFFFFFF' E  E2-E9
         DC    X'00000000000000000000FFFFFFFFFFFF' F  F0-F9
*
OPEN     OPEN  0,MF=L
OPENL    EQU   *-OPEN
CLOSE    CLOSE 0,MF=L
CLOSEL   EQU   *-CLOSE
RDJFCB   RDJFCB 0,MF=L
RDJFCBL  EQU   *-RDJFCB
*
DEFUSER  DC    CL8'HERC01'         Default userid for LOCAL
*
TKNINIT  DC    12X'FF4040404040404040'  byte 0=IBM len, bytes 1-8 blank
*                          1         2         3         4         5    NJE00250
*                01234567890123456789012345678901234567890123456789012
NJE042E  DC    C'NJE042E Node names must contain A-Z, 0-9 only'
NJE044E  DC    C'NJE044E Link ' is not inactive'
NJE045I  DC    C'NJE045I Link ' is not defined
NJE046I  DC    C'NJE046I Link ' xxx deleted
NJE048E  DC    C'NJE048I Link ' xxx duplicate definition
NJE049I  DC    C'NJE049I Using configuration '
NJE050E  DC    C'NJE050E ''AT'' keyword is required with AUTH'
NJE051E  DC    C'NJE051E ''TO'' keyword is required with ROUTE'
NJE052E  DC    C'NJE052E Only one LOCAL statement allowed'
NJE053E  DC    C'NJE053E LOCAL statement must preceed LINK, ROUTE, or A*
               UTH'
NJE054E  DC    C'NJE054E Invalid BUFF value; range is 300 to 4020'
NJE055E  DC    C'NJE055E Unrecognized keyword x'
NJE063I  DC    C'NJE063I Node xxxxxxxx ' is not routed
NJE064I  DC    C'NJE064I Route for node ' xxxxxxxx deleted
NJE065I  DC    C'NJE065I Node xxxxxxxx ' routed through link xxxx
NJE066I  DC    C'NJE066I Link '     xxx defined'
NJE067I  DC    C'NJE067I User '     xxx at yyy is already authorized'
NJE068I  DC    C'NJE068I User '     xxx at yyy is now authorized
NJE069I  DC    C'NJE069I User '     xxx at yyy not in authorization lst
NJE070I  DC    C'NJE070I User '     xxx at yyy removed from auth list
NJE072E  DC    C'NJE072E Configuration syntax error in record xxx'
NJE073E  DC    C'NJE073E Keyword is not in column 1'
NJE074E  DC    C'NJE074E Syntax, keyword, or value error after column x*
               x'
NJE075E  DC    C'NJE075E Missing value after keyword x'
NJE076E  DC    C'NJE076E Unrecognized configuration statement type'
NJE077E  DC    C'NJE077E LINE address required on LINK statement'
NJE078E  DC    C'NJE078E Invalid/incorrect value after keyword x'
*                                                                       NJE00930
* NJE043E                                                               NJE00930
* NJE047E                                                               NJE00930
* NJE056I                                                               NJE00930
* NJE057I                                                               NJE00930
* NJE058I                                                               NJE00930
* NJE059I all NJECMX msgs                                               NJE00930
* NJE060I                                                               NJE00930
* NJE061I                                                               NJE00930
* NJE062I                                                               NJE00930
* NJE071I                                                               NJE00930
*                                                                       NJE00930
         COPY  LINKTABL                                                 NJE00940
         COPY  RTE                                                      NJE00940
         COPY  AUTHLIST                                                 NJE00940
         COPY  NETSPOOL                                                 NJE00940
*
****  Main work area                                                    NJE00290
*                                                                       NJE00290
NJEWK    DSECT
NJEEYE   DS    CL4'NJES'           Eyecatcher
NJEWKLEN DS    F                   Getmain size of this area
*
DBLE     DS    D                   Work area                            NJE00310
TWRK     DS    2D                  Work area
MACLIST  DS    XL64                Macro expansion area
REC      DS    CL120               Input record or command area
TOKENS   DS    12CL9               Parsed keyword tokens (1 len,8 tkn)
*
         NJEPARMS                  Passed parameter list           v220
*
*
INITPLST DS    A                   -> entry parm list in R1
AMTEXT   DS    A                   -> MTEXT field in NJECMX
SV14CF   DS    A                   R14 save
SV14GT   DS    A,A                 R14-15 save
*
NJFL1    DS    X                   Flag bits
F1OPEN   EQU   X'80'   1... ....    CONFIG DCB is open
*
NJFL2    DS    X                   token scan and ctl flags
F2LINE   EQU   X'80'   1... ....    LINE keyword found
F2RTO    EQU   X'40'   .1.. ....    ROUTE TO keyword found
F2AAT    EQU   X'20'   ..1. ....    AUTH AT keyword found
*
         DS    0F
EXLST    DS    A                   X'87'+AL3(JFCBL)
JFCBL    DS    XL176
CONFIG   DS    XL(DMYDCBL)         CONFIG DCB
WTOMSG   DS    0XL(WTOL)           WTO header
         DS    F                   Header area
WTOTXT   DS    XL(WTOL-4)          WTO text area
*
BLDBUF   DS    XL(LINKLEN)         Build area for LINK,RTE,AUTH entries
*
NJESA    DS    18F                 NJESCN OS save area                  NJE00300
BALRSAVE DS    16F                 Local rtns register save             NJE00300
*
         DS    0D                  Force doubleword size
NJEWKSZ  EQU   *-NJEWK
*
         DCBD  DSORG=PS,DEVD=DA
JFCB     DSECT
         IEFJFCBN LIST=YES
*
         END   NJESCN                                                   NJE01000
./ ADD NAME=NJESYS
*
*
*-- NJE38 - Locate NJE38 information from an ENQ resource
*
*
*   Called by NJEINIT,NJERCV,NJETRN,NJE38,NJ38XMIT,NJ38RECV
*
*
* Change log:
*
* 01 Oct 20 - Initial creation                                     v210
*
*
         GBLC  &VERS
         REGEQU
NJESYS   CSECT
         NJEVER
         STM   R14,R12,12(R13) Save regs
         LR    R12,R15
         USING NJESYS,R12
*
*-- Determine if NJE38 is already active in another address space
*
CHK000   EQU   *
         L     R2,16           Get CVT ptr
         USING CVT,R2
         LA    R2,CVTFQCB      -> ENQ QCB chain anchor
         USING QCB,R2
*
CHK010   EQU   *
         ICM   R2,15,MAJNMAJ   -> next major QCB
         BZ    CHK080          Our guy not found
         CLC   MAJNAME,NJE38Q  Look for our QNAME "NJE38"
         BNE   CHK010          Nope, go to next QCB
*
         L     R3,MAJFMIN      -> first minor QCB
         USING MIN,R3
*
CHK020   EQU   *
         LA    R4,MINNAME      -> minor name
         CLC   NJERCON,0(R4)   Does minor name match?
         BE    CHK030          Yes. NJE38 is active
         C     R3,MAJLMIN      Is this the last minor QCB?
         BE    CHK080          Yes, we're done. NJE38 is not active
         ICM   R3,15,MINNMIN   -> next minor name
         BZR   R14             Just in case no address
         B     CHK020          Spin through the minor QCBs
*
CHK030   EQU   *
         LTR   R1,R1           Store spool DSN?
         BZ    CHK040          No
         MVC   0(44,R1),12(R4) Save off NETSPOOL dsname
*
CHK040   EQU   *
         L     R1,8(,R4)       Get CSABLK ptr from QCB minor
         SR    R15,R15         RC=0, ENQ data was found
         B     CHK090
*
CHK080   EQU   *
         LA    R15,4           RC=4, no ENQ located
*
CHK090   EQU   *
         ST    R1,24(,R13)     Return R1 value
         ST    R15,16(,R13)    Return R15 RC
*
         LM    R14,R12,12(R13) Reload regs
         BR    R14             Return
*
         DS    0D
NJE38Q   DC    CL8'NJE38'
NJERCON  DC    CL8'NJEINIT'
*
         LTORG ,
*
         CVT   DSECT=YES,PREFIX=NO
         IHAQCB
*
         END
./ ADD NAME=NJETRN
*
*-- NJE38 - TSO TRANSMIT
*
*   Command line format:
*
*   TRANSMIT node.userid
*            DATASET( )
*            OUTDATASET( )
*            VOLSER( )
*            UNIT( )
*            PDS | SEQUENTIAL
*            QUIET
*
*   where:
*
*    node.userid    - specifies the destination of the transmission
*
*    DATASET( )     - specifies the dsname of the dataset to be
*                     transmitted.  May optionally specify a member.
*
*    OUTDATASET( )  - optional. Specifies the encoded file is to be
*                     written to this dataset instead of being
*                     transmitted.  'node.userid' may be omitted if
*                     OUTDATASET is specified, but if it is present
*                     then the specified node and userid will be part
*                     of the encoded data instead of meaningless
*                     defaults. If OUTDATASET is specified, the
*                     named dataset will be used if it exists, other-
*                     wise it will be created.
*                     The contents of OUTDATASET can be input to a
*                     RECEIVE command by the use of RECEIVE INDATASET.
*
*    VOLSER( )      - optional. Specifies a volume where OUTDATASET
*                     should be created.  If not specified, a PUBLIC
*                     volume will be selected.
*
*    UNIT( )        - optional. Specifies a unit name where OUTDATASET
*                     should be created.  If not specified, SYSDA is
*                     the default unit name.
*
*    PDS            - If specified, indicates that the member name
*                     specified with DATASET is to be transmitted
*                     with IEBCOPY unload, thereby preserving the
*                     user directory data in the source PDS.
*
*    SEQUENTIAL     - DEFAULT. Indicates that any member name specified
*                     with DATASET is to be transmitted as a sequential
*                     file; no directory information is part of the
*                     transmission. SEQL must be specified or defaulted
*                     if the destination host is a VM system.
*
*    QUIET          - If specified, indicates that all informational
*                     messages from TRANSMIT are suppressed.  Error
*                     messages will always be displayed.
*
*
*    Examples (a user is logged on to TSO with userid FRED:
*
*    1. Send member COBSRC from FRED.MY.PDS to user HERC01 at
*       node MVSA.  The directory information associated with COBSRC
*       is to be part of the transmission:
*
*       TRANSMIT mvsa.herc01 da(my.pds(cobsrc)) pds
*
*    2. Encode dataset HERC02.COBOL.LISTING into FRED.NETLIB:
*
*       TRANSMIT da('herc02.cobol.listing') out(netlib)
*
*    3. Send macro GETQ from FRED.MACLIB to CMSUSER at VMSYS1.
*
*       TRANSMIT vmsys1.cmsuser da(maclib(getq))
*
*
* Change log:
*
* 24 Apr 21 - Use TSO userid as default user if no security and    v222
*              NJE38 is not active.                                v222
* 15 Feb 21 - Not picking up jobname when run as an STC.           v221
* 10 Dec 20 - Support for registered users and message queuing     v220
* 01 Oct 20 - Put ENQ existence check in common module             v210
* 09 Aug 20 - Improve TSO attention key handling                   v201
* 24 Jul 20 - Fix S013-18 if DATASET member not found              v200
* 15 Jul 20 - Don't display final record count.                    v200
* 12 Jul 20 - Add support for the UNIT parameter.                  v200
* 21 Jun 20 - Initial creation
*
*
         GBLC  &VERS
         REGEQU
NJETRN   CSECT                                                          NJE00020
         NJEVER
         STM   R14,R12,12(R13)         Save Regs                        NJE00050
         LR    R12,R15                 Base                             NJE00060
         USING NJETRN,R12                                               NJE00070
         LR    R8,R1                   Copy input parm addr
*
         GETMAIN RU,                   Get local stg area              X
               LV=4096,                                                X
               BNDRY=PAGE
         LR    R10,R1
         LR    R1,R0                   Copy length
         LR    R2,R0                   Copy length
         LR    R0,R10                  -> new stg area
         SR    R15,R15                 set pad
         MVCL  R0,R14                  Clear the page
*
         USING NJEWK,R10
         ST    R13,NJESA+4             SAVE prv S.A. ADDR               NJE00080
         LA    R1,NJESA                -> my save area
         ST    R1,8(,R13)              Plug it into prior SA
         LR    R13,R1
*
         MVC   NJEEYE,=CL4'NJET'       Work area eyecatcher
         ST    R2,NJEWKLEN             Save size of area in area
         L     R11,=A(NJECOM)          -> common csect
         USING NJECOM,R11
         ST    R8,CPARMS               Save ptr to input parms
         MVC   LCLNODE,=CL8'ORIGNODE'  Set default local node
         MVC   DESTNODE,=CL8'DESTNODE'  Set default
         MVC   DESTUSER,=CL8'DESTUSER'  Set default
         MVC   PBREM,=F'80'            Initialize
         LA    R1,REC                  -> output record area
         ST    R1,PBRPS                Initialize
*
INIT000  EQU   *
         MVC   MACLIST(ESTAEL),ESTAE   Move ESTAE parm list
         L     R6,=A(NJEDMP)           Point to local ESTAE rtn
         ESTAE (R6),                   Issue ESTAE                     X
               CT,                                                     X
               TERM=YES,                                               X
               PARAM=(R10),            PARAM is work area address      X
               MF=(E,MACLIST)
*
*-- Establish TSO userid issuing this command
*
         TESTAUTH FCTN=1               Are we authorized on entry?
         LTR   R15,R15                 Check result
         BNZ   INIT010                 Branch if not authorized
         OI    FLAGS1,F1APF            Indicate authorized on entry
*
INIT010  EQU   *
         L     R2,PSATOLD-PSA(0)       -> my TCB
         L     R2,TCBTIO-TCB(R2)       -> my TIOT
         LA    R4,TIOCNJOB-IEFTIOT(R2) -> TIOT jobname             v222
         LR    R3,R4                   Assume will use jobname     v222
*
         L     R2,PSAAOLD-PSA(0)       -> my ASCB
         L     R6,ASCBTSB-ASCB(,R2)    -> TSB (or 0)
         L     R2,ASCBASXB-ASCB(,R2)   -> my ASXB
         ICM   R2,15,ASXBSENV-ASXB(R2) -> my ACEE
         BZ    INIT015                 Exit if no ACEE
*
         USING ACEE,R2
         CLI   ACEEUSRL,X'00'          No userid available?
         BE    INIT015                 Exit if unavail
         CLI   ACEEUSR,X'00'           Userid not formed correctly?
         BE    INIT015                 Exit if unavail
         LA    R3,ACEEUSR              -> Userid
         OI    FLAGS1,F1ACEE           Valid ACEE found
         CLC   ACEEUSR,=CL8'STC'       Is this a started task?     v221
         BNE   INIT015                 No, use ACEEUSR id          v221
         LR    R3,R4                   Make the TIOT jobname the idv221
         DROP  R2                      ACEE
*
INIT015  EQU   *
         MVC   USERID,0(R3)            Set the userid
         TM    FLAGS1,F1APF            Authorized at entry?
         BO    INIT040                 yes.
         CLC   USERID,=CL8'HERC01'     Special access id?
         BE    INIT020                 Yes
         CLC   USERID,=CL8'HERC02'     Special access id?
         BNE   INIT030                 No
*
INIT020  EQU   *
         OI    FLAGS1,F1AUSR           Indicate special authorized user
         SR    0,0                     Use authorization SVC
         LA    1,1                      For TK4- HERC01/HERC02 only
         SVC   244                     Get authorized
         B     INIT040
*
INIT030  EQU   *
         TM    FLAGS1,F1APF            Authorized at entry?
         BZ    ERR006                  No, issue error
*
INIT040  EQU   *
         LA    R6,0(,R6)               Clear high order byte
         LTR   R6,R6                   Was there a TSB address
         BNZ   INIT050                 There was. Running in TSO userid
         OI    FLAGS1,F1BATCH          Indicate batch TSO
         TM    FLAGS1,F1ACEE           Valid ACEE found?
         BO    INIT050                 Yes, go with ACEE userid
         BAL   R2,CHK000               See if NJE38 is active      v210
         BNZ   INIT050                 NJE38 not act; use jobname  v222
         MVC   USERID,DEFUSER          Use default userid
*
INIT050  EQU   *
         L     R2,4(,R8)               -> UPT from input parms
         USING UPT,R2
         MVC   PREFIX,BLANKS           Init receiving field
         SR    R1,R1                   Clear for IC
         ICM   R1,1,UPTPREFL           Get prefix length
         BZ    INIT060                 No prefix value in use
         BCT   R1,*+10                 Adjust for execute
         MVC   PREFIX(0),UPTPREFX      executed instr
         EX    R1,*-6                  Copy the prefix value
         DROP  R2                      UPT
*
INIT060  EQU   *
         MVC   STAXLIST(STAXL),STAX    Move STAX parm list
         LA    R5,LIST                 -> input buffer from attn
         LA    R6,STAXXIT              Point to local exit
         STAX  (R6),                   Set exit for attention          X
               OBUF=(ATTNMSG,L'ATTNMSG),                               x
               IBUF=((5),80),                                          x
               USADDR=(10),            Parameter is our work area      x
               MF=(E,MACLIST)
*
*-- Parse command line
*
         SR    R0,R0                   Code 0: parse command line
         L     R15,=A(NJEPAR)          -> parse routine
         BALR  R14,R15
*
         TM    FLAGS1,F1ATTN           Was ATTN pressed?           v201
         BO    EXIT08                  Y, immediate exit           v201
*
         B     INIT070(R15)            Branch into table on RC
INIT070  B     INIT080                 Continue
         B     ERR004                  No parameters entered
         B     ERR005                  Invalid node.user entered
         B     ERR001                  Display IJKPARS RC
*
INIT080  EQU   *
         LA    R2,MSG000               Issue hello msg
         BAL   R14,PUTLINE
         LA    R2,MSGBLNK              Issue blank line
         BAL   R14,PUTLINE
*
*-- Check if we have the required parameters:
*
*-- 1. DATASET is required.  No exceptions.
*-- 2. Either one of:
*--    a.  OUTDATASET, or,
*--    b.  node.userid
*-- 3. If node.user specified, we need NJE38 to be active.
*
         TM    FLAGS3,F3INDS           Was DATASET specified?
         BZ    ERR002                  N, it is required
         TM    FLAGS3,F3OUTDS          Was OUTDATASET specified?
         BO    INIT090                 Y, we don't need NJE38
         TM    FLAGS3,F3DEST           Do we have a node.user?
         BZ    ERR011                  No, bail out.
*
         BAL   R2,CHK000               Determine NJE38 status      v210
         BNZ   ERR013                  NJE38 is not active         v210
*
*-- Set up user selected input dataset
*
*-- 1. Dynamically allocate it (also return DSORG and VOLSER).
*-- 2. OBTAIN the DSCB for the dataset to get DCB attributes.
*-- 3. Use DEVTYPE and TRKCALC along with the DSCB last used TTR to
*--    determine how many blocks were used in the dataset.
*-- 4. If it was a PDS, count the number of directory blocks.
*
INIT090  EQU   *
         MVC   TDSNAME,INPUTDS         Set DSNAME of user dataset
         MVC   TMEMBER,INMEM           Set member name (or null)
*
         LA    R0,DYNINDS         24   allocate input dataset
         L     R15,=A(NJEDYN)          -> dynamic allocation rtns
         BALR  R14,R15
         LTR   R15,R15                 Any errors?
         BNZ   EXIT08                  Exit if allocation error
         TM    TDSORG,X'42'            Is it DSORG=PO/PS ?
         BZ    ERR003                  No, can't support it
*
         MVC   DDSYSUT1,TDDNAME        Save off the DDNAME returned
         TM    FLAGS3,F3PDS            Was PDS forced?
         BO    INIT100                 Y, use IEBCOPY instead of PS mbr
         TM    FLAGS3,F3INMEM          Was a member name specified?
         BZ    INIT100                 No, DSORG is what it is
         MVC   TDSORG,=X'4000'         Member makes it DSORG=PS
*
INIT100  EQU   *
         XC    CAMWORK,CAMWORK         Init CAMLST work area
         MVC   CAMLST,DMYLST           Move dummy CAMLST to area
         LA    R1,CAMLST               -> CAMLST
         LA    R4,TDSNAME              -> DATASET NAME
         ST    R4,4(,R1)               Put in CAMLST
         LA    R4,TVOLSER              -> VOLSER
         ST    R4,8(,R1)               Put in CAMLST
         LA    R4,CAMWORK              -> AT WORK AREA
         ST    R4,12(,R1)              Put in CAMLST
*
         OBTAIN (1)                    Get the format 1 DSCB
         LA    R4,CAMWORK-44           -> DSCB we obtained (less DSN)
         USING DSCBF1,R4
         LTR   R15,R15                 SUCCESSFUL?
         BNZ   ABEND101                No
*
INIT110  EQU   *
         LA    R7,INMF02A              -> first INMR02 data items
         USING INMFIELD,R7
         MVC   DSORG,TDSORG            Set DSORG
         SR    R0,R0                   Clear for IC
         ICM   R0,3,DS1BLKL
         ST    R0,BLKSIZE              Set BLKSIZE
         ICM   R0,3,DS1LRECL
         ST    R0,LRECL                Set LRECL
         MVC   RECFM,DS1RECFM          Set RECFM
         MVC   DSNAME,INPUTDS          Set DSNAME
         MVC   UTLNAME,=CL8'INMCOPY'   Assume utility is sequential cpy
         TM    DSORG,X'40'             Is DSORG=PS?
         BO    INIT120                 Yes
         MVC   UTLNAME,=CL8'IEBCOPY'   Utility is for partitioned
*
*
INIT120  EQU   *
         DEVTYPE TDDNAME,DEVINFO,DEVTAB  Get device info
         LTR   R15,R15                 Success?
         BNZ   ABEND102                No
*
INIT130  EQU   *
         MVC   MACLIST(TRKCALCL),TRKCALC  Move macro model
         TRKCALC FUNCTN=TRKCAP,      Calc track capacity for this blkszX
               REGSAVE=YES,          Save all regs                     X
               TYPE=DEVUCBTY+3,      Point to device type byte         X
               R=1,                  Record 1 = calc for entire track  X
               K=0,                  No Keys                           X
               DD=DS1BLKL,           Use the BLKSIZE from DSCB         X
               MF=(E,MACLIST)        R0 = # blks per track on exit
*
*
         SR    R1,R1                   Clear
         L     R3,BLKSIZE              Get current block size
         MR    R2,R0                   Compute bytes per track
         SR    R1,R1                   Clear
         ICM   R1,3,DS1LSTAR           Get TT of last used TTR
         LA    R1,1(,R1)               One extra for partial last track
         MR    R2,R1                   Compute approx bytes in file
         ST    R3,FILESIZE             Set approx file size in bytes
         DROP  R4                      DSCBF1
*
*-- If input dataset is a PDS, count the number of directory blocks.
*-- Then, use IEBCOPY to unload the PDS into a sequential file.
*
         TM    DSORG,X'40'             Is DSORG=PS?
         BO    OUT000                  Y, done with input dataset
*
         OI    FLAGS1,F1INPDS          INDS is a PDS dataset
         MVC   INDS(DMYINDSL),DMYINDS  Set up DCB
         LA    R6,INDS                 -> DCB
         USING IHADCB,R6
         MVC   DCBDDNAM,DDSYSUT1       Set DCB DDNAME
         MVC   DCBBLKSI,=Y(256)        Set up to read dir blocks
         MVC   DCBLRECL,=Y(256)        Set up to read dir blocks
         MVI   DCBRECFM,DCBRECF        RECFM=F
         LA    R1,INIT150              -> temporary EOF addr
         ST    R1,DCBEODAD             Set it
         DROP  R6
*
         MVC   MACLIST(OPENL),OPEN     Move OPEN list
         OPEN  (INDS,INPUT),           Open the input dataset          X
               MF=(E,MACLIST)
         OI    FLAGS2,F2INOPN          Indicate DCB is open
         SR    R2,R2                   Init directory blocks counter
*
INIT140  EQU   *
         GET   INDS                    Get a dir block
         LA    R2,1(,R2)               Count it
         B     INIT140
*
INIT150  EQU   *
         ST    R2,DIRBLKS              Set DIRBLKS value
         TM    FLAGS2,F2INOPN          Is INDS DCB open?
         BZ    UNLD000                 No
         MVC   MACLIST(CLOSEL),CLOSE   Move close list
         CLOSE (INDS),                 Close it                        X
               MF=(E,MACLIST)
         NI    FLAGS2,255-F2INOPN      Indicate DCB is closed
*
*-- If DATASET is a PDS, prepare to call IEBCOPY to unload it.
*
*-- 1. Create sequential dataset for IEBCOPY to unload into.
*-- 2. Allocate other required IEBCOPY datasets.
*-- 3. If user specified a member name in DATASET, build IEBCOPY
*--    control statements.
*-- 4. Invoke IEBCOPY to unload the entire PDS or single member.
*
UNLD000  EQU   *
         LA    R6,INMF02A              -> 1st INMR02 record
         LA    R7,INMF02B              -> 2nd INMR02 record
         USING INMFIELD,R7
*
*-- Filling dynamic allocation text units for unload PS dataset
*
         LA    R1,3120                 Use 3120 for IEBCOPY SYSUT2
         STH   R1,TBLKSIZE             Set dynalloc block size
         STCM  R1,7,TBLKLEN            Set dynalloc space blk len
*
         L     R3,FILESIZE-INMFIELD(R6) Get INDS size
         ST    R3,FILESIZE             Use as temporary DS size
         SR    R2,R2                   Clear for divide
         DR    R2,R1                   Compute # blocks needed
         LA    R3,1(,R3)               Always round up
         LR    R1,R3                   Return primary blocks in R1
         SRL   R3,2                    Compute 1/4th of needed amt
         LA    R2,1(,R3)               Round up = secondary blks needed
*
         STCM  R1,7,TPRIME             Set primary space in blocks
         STCM  R2,7,TSECND             Set secondary space in blocks
*
         MVC   TDSORG,=X'4000'         Always PS
*
*
*-- Call NJEDYN to allocate the unload output dataset as "SYSUT2"
*
         LA    R0,DYNUNLD         10   allocate unload dataset
         L     R15,=A(NJEDYN)          -> dynamic allocation rtns
         BALR  R14,R15
*
         B     UNLD020(R15)            Branch on RC
UNLD020  B     UNLD040             00  Normal, proceed
         B     EXIT08              04  Dataset exists, shouldnt happen
         B     EXIT08              08  All other errors
*
*-- Prepare to launch IEBCOPY
*
UNLD040  EQU   *
         MVC   DDSYSUT2,TDDNAME        Set replacement SYSUT2 DD
*
*-- Call NJEDYN to allocate the SYSIN dataset needed by IEBCOPY
*
         LA    R0,DYNSYSIN        08   allocate SYSIN for IEBCOPY
         L     R15,=A(NJEDYN)          -> dynamic allocation rtns
         BALR  R14,R15
         LTR   R15,R15
         BNZ   EXIT08                  Exit with dynalloc error
         MVC   DDSYSIN,TDDNAME         Save generated DDNAME
*
*-- Call NJEDYN to allocate the SYSPRINT dataset needed by IEBCOPY
*
         LA    R0,DYNSYSPR        12   allocate SYSPRINT for IEBCOPY
         L     R15,=A(NJEDYN)          -> dynamic allocation rtns
         BALR  R14,R15
         LTR   R15,R15
         BNZ   EXIT08                  Exit with dynalloc error
         MVC   DDSYSPR,TDDNAME         Save generated DDNAME
*
*-- Call NJEDYN to allocate the SYSUT4 dataset needed by IEBCOPY
*
         LA    R0,DYNSYSU4        14   allocate SYSUT4 temporary
         L     R15,=A(NJEDYN)          -> dynamic allocation rtns
         BALR  R14,R15
         LTR   R15,R15
         BNZ   EXIT08                  Exit with dynalloc error
         MVC   DDSYSUT4,TDDNAME        Set replacement SYSUT4 DD
*
         TM    FLAGS3,F3INMEM          Was a member name specified?
         BZ    UNLD080                 No, skip ctl card build
         BAL   R14,CTL000              Build IEBCOPY control statements
*
*-- Invoke IEBCOPY
*
UNLD080  EQU   *
         MVC   CPYPLIST,COPYPARM       Move IEBCOPY parms to 24-bit stg
         MVC   DDLISTL,=AL2(DDLISTSZ)  Set IEBCOPY DD list length
         LA    R2,CPYPLIST
         LA    R3,DDLISTL
         MVC   MACLIST(LINKL),LINK     Move macro model
         LINK  EP=IEBCOPY,                                             x
               PARAM=((R2),(R3)),                                      x
               VL=1,                                                   x
               MF=(E,MACLIST)
         LTR   R5,R15                  Copy RC to R5
         BNZ   ERR018                  Exit on error
*
*-- Find out what we can about the IEBCOPY unloaded dataset
*
*-- 1. OBTAIN the DSCB for the dataset to get DCB attributes.
*-- 2. Use DEVTYPE and TRKCALC along with the DSCB last used TTR to
*--    determine how many blocks were used in the dataset.
*
UNLD100  EQU   *
         XC    CAMWORK,CAMWORK         Init CAMLST work area
         MVC   CAMLST,DMYLST           Move dummy CAMLST to area
         LA    R1,CAMLST               -> CAMLST
         LA    R4,TDSNAME              -> DATASET NAME
         ST    R4,4(,R1)               Put in CAMLST
         LA    R4,TVOLSER              -> VOLSER
         ST    R4,8(,R1)               Put in CAMLST
         LA    R4,CAMWORK              -> AT WORK AREA
         ST    R4,12(,R1)              Put in CAMLST
*
         OBTAIN (1)                    Get the format 1 DSCB
         LA    R4,CAMWORK-44           -> DSCB we obtained (less DSN)
         USING DSCBF1,R4
         LTR   R15,R15                 SUCCESSFUL?
         BNZ   ABEND103                No
*
UNLD110  EQU   *
         LA    R7,INMF02B              -> 2nd INMR02 data items
         USING INMFIELD,R7
         MVC   DSORG,TDSORG            Set DSORG
         SR    R0,R0                   Clear for IC
         ICM   R0,3,DS1BLKL
         ST    R0,BLKSIZE              Set BLKSIZE
         ICM   R0,3,DS1LRECL
         ST    R0,LRECL                Set LRECL
         MVC   RECFM(1),DS1RECFM       Set RECFM
         MVI   RECFM+1,X'02'           Indicate shortened variable fmt
         XC    DSNAME,DSNAME           No DSNAME in INMR02B
         MVC   UTLNAME,=CL8'INMCOPY'   Utility is sequential cpy
*
*
UNLD120  EQU   *
         DEVTYPE TDDNAME,DEVINFO,DEVTAB  Get device info
         LTR   R15,R15                 Success?
         BNZ   ABEND104                No
*
UNLD130  EQU   *
         MVC   MACLIST(TRKCALCL),TRKCALC  Move macro model
         TRKCALC FUNCTN=TRKCAP,      Calc track capacity for this blkszX
               REGSAVE=YES,          Save all regs                     X
               TYPE=DEVUCBTY+3,      Point to device type byte         X
               R=1,                  Record 1 = calc for entire track  X
               K=0,                  No Keys                           X
               DD=DS1BLKL,           Use the BLKSIZE from DSCB         X
               MF=(E,MACLIST)        R0 = # blks per track on exit
*
*
         SR    R1,R1                   Clear
         L     R3,BLKSIZE              Get current block size
         MR    R2,R0                   Compute bytes per track
         SR    R1,R1                   Clear
         ICM   R1,3,DS1LSTAR           Get TT of last used TTR
         LA    R1,1(,R1)               One extra for partial last track
         MR    R2,R1                   Compute approx bytes in file
         ST    R3,FILESIZE             Set approx file size in bytes
         DROP  R4                      DSCBF1
*
*-- Prep OUTDATASET if specified
*
*--  Determine if it exists,
*--   If yes, DSORG must be PS unless OUTDS member coded.
*--   If no, create it, 3120/80/FB,
*--      and create as PDS if user specified a OUTDS member, else SEQL,
*--      using estimated file size from input dataset.
*
OUT000   EQU   *
         TM    FLAGS3,F3OUTDS          Did user specify OUTDATASET?
         BZ    OPN000                  No, transmit to NETSPOOL
*
OUT200   EQU   *
         MVC   CAMLST,LOCATLST         Move modem CAMLST
         XC    BUFF,BUFF               Clear sufficient camlst workarea
         XC    REC,REC                 Clear more
         LA    R1,CAMLST               -> CAMLST
         LA    R2,OUTPUTDS             -> DATASET name
         ST    R2,4(,R1)               Place in CAMLST
         LA    R2,CAMWORK              -> CAMLST work area
         ST    R2,12(,R1)              Place in CAMLST
*
         LOCATE (1)                    Does dataset exist?
         LTR   R15,R15                 Any errors?
         BNZ   OUT240                  Yes, dataset doesn't exist
         OI    FLAGS2,F2EXIST          Indicate OUTDATASET exists
*
*-- Find out about this existing OUTDATASET
*
OUT210   EQU   *
         LA    R4,CAMWORK              -> CAMLST work area
         USING VOLLIST,R4              Address the volume list
         MVC   TVOLSER,VOLSER          Save off the volume
         DROP  R4                      VOLLIST
*
         XC    CAMWORK,CAMWORK         Init CAMLST work area
         MVC   CAMLST,DMYLST           Move dummy CAMLST to area
         LA    R1,CAMLST               -> CAMLST
         LA    R4,OUTPUTDS             -> DATASET NAME
         ST    R4,4(,R1)               Put in CAMLST
         LA    R4,TVOLSER              -> VOLSER
         ST    R4,8(,R1)               Put in CAMLST
         LA    R4,CAMWORK              -> AT WORK AREA
         ST    R4,12(,R1)              Put in CAMLST
*
         OBTAIN (1)                    Get the format 1 DSCB
         LA    R4,CAMWORK-44           -> DSCB we obtained (less DSN)
         USING DSCBF1,R4
         LTR   R15,R15                 SUCCESSFUL?
         BNZ   ABEND105                No
*
OUT220   EQU   *
         TM    DS1DSORG,X'40'          Is it a Seql dataset?
         BO    OUT230                  Yes
         TM    DS1DSORG,X'02'          Is it a PDS dataset?
         BZ    ERR007                  No, error; must be PS or PO
         TM    FLAGS3,F3OUTMEM         Did user also code member name?
         BZ    ERR008                  N, mem req'd if PO
         B     OUT250                  Y, proceed with allocation
*
*-- Whether OUTDATASET existed or not, ignore any member name
*-- coded on OUTDATASET if the dataset is PS.
*
OUT230   EQU   *
         NI    FLAGS3,255-F3OUTMEM     Ignore any user member name
         B     OUT250                  And go allocate it
*
*-- OUTDATASET didn't exist, prepare to create it
*
OUT240   EQU   *
         LA    R2,3120                 3120 = NETDATA output blksize
         STH   R2,TBLKSIZE             Set per NETDATA std
         STCM  R2,7,TBLKLEN            Set per NETDATA std
         MVC   TLRECL,=H'80'           Set per NETDATA std
         MVI   TRECFM,X'90'            Set FB per NETDATA std
         MVC   TDSORG,=X'4000'         Set PS per NETDATA std
         SR    R0,R0                   Clear for divide
         L     R1,FILESIZE             Get # bytes in input file
         DR    R0,R2                   Compute # of 3120 blks needed
*
         LR    R2,R1                   Copy # blks needed
         SRA   R1,3         div by 8   Compute 12% for NETDATA overhead
         AR    R1,R2                   Get # blks + 12%
         STCM  R1,7,TPRIME             Set # primary space blocks
         SRA   R2,2         div by 4   Compute 25% for secondary
         STCM  R2,7,TSECND             Set # secondary space blocks
*
OUT250   EQU   *
         MVC   TDSNAME,OUTPUTDS        Set DSNAME for allocation
         MVC   TMEMBER,OUTMEM          Set possible member name
         MVC   TVOLSER,OUTVOL          Set possible volser override
*
         LA    R0,DYNOUTDS        32   allocate OUTDATASET
         L     R15,=A(NJEDYN)          -> dynamic allocation rtns
         BALR  R14,R15
         LTR   R15,R15                 Any errors?
         BNZ   EXIT08                  Exit if allocation error
*
         MVC   DDOUTDS,TDDNAME         Save DD returned
         MVC   OUTDS(DMYOUTDL),DMYOUTDS Set up DCB
         LA    R6,OUTDS                -> DCB
         USING IHADCB,R6
         MVC   DCBDDNAM,DDOUTDS        Set DCB DDNAME
         DROP  R6                      IHADCB
*
         MVC   MACLIST(OPENL),OPEN     Move OPEN list
         OPEN  (OUTDS,OUTPUT),         Open the OUTDATASET             X
               MF=(E,MACLIST)
         OI    FLAGS2,F2OUTOPN         Indicate DCB is open
         B     TRN000                  Start transmitting to OUTDATASET
*
*-- Open NETSPOOL if not using OUTDATASET
*
OPN000   EQU   *
         BAL   R2,CHK000               Determine NJE38 status      v210
         BNZ   ERR013                  NJE38 is not active         v210
*
         MVC   DDNETSPL,=CL8'NETSPOOL' Set NETSPOOL DDN (for unalloc)
         MVC   TDDNAME,DDNETSPL        NETSPOOL DD
         MVC   TDSNAME,SPLDSN          Set spool DSN
         LA    R0,DYNETSPL        28   allocate NETSPOOL
         L     R15,=A(NJEDYN)          -> dynamic allocation rtns
         BALR  R14,R15
         LTR   R15,R15
         BNZ   EXIT08                  Exit with dynalloc error
*
         LA    R8,NCB1                 -> NCB
         USING NCB,R8
         MVI   NCBFL1,TYPPUN           Only outputting punch type recs
*
         NSIO  TYPE=OPEN,                                              x
               NCB=(R8)
         C     R15,=F'4'               NETSPOOL needs verify?
         BE    ERR025                  Yes
         BL    OPN010                  Everything is good
         BAL   R14,FMT000              Display Open error
         CLC   NCBRTNCD(2),=AL1(8,152) X'0898' Security denied access?
         BE    ERR014                  Yes, special message
         B     EXIT08                  Exit on VSAM error
*
OPN010   EQU   *
         OI    FLAGS2,F2NCBOPN         Indicate NETSPOOL is open
*
*-- Create the NETDATA and transmit the results to the destination
*-- node, or store it in the OUTDATASET.
*
*-- DDNAME setup below at TRN000 may look confusing.  To explain:
*-- The NETDATA is always built from a sequential dataset.  So the
*-- INDS DCB here represents either the original user specified
*-- input DATASET -or - the IEBCOPY unloaded sequential dataset from
*-- the original PDS.
*
*-- If the original was sequential, it is already allocated at the
*-- DDSYSUT1 ddname.
*-- If the original was a PDS, then the IEBCOPY unload dataset is
*-- allocated at the DDSYSUT2 ddname.
*
*
TRN000   EQU   *
         MVC   INDS(DMYINDSL),DMYINDS  Set up DCB
         LA    R6,INDS                 -> DCB
         USING IHADCB,R6
         MVC   DCBDDNAM,DDSYSUT2  PDS: Set DCB DDNAME (iebcopy UNLD DD)
*
         TM    FLAGS1,F1INPDS          Is input dataset a PDS?
         BO    TRN210                  Yes
         MVC   DCBDDNAM,DDSYSUT1  SEQ: Set DCB DDNAME (input DS DD)
*
TRN210   EQU   *
         MVC   MACLIST(OPENL),OPEN     Move OPEN list
         OPEN  (INDS,INPUT),           Open the input dataset          X
               MF=(E,MACLIST)
         OI    FLAGS2,F2INOPN          Indicate DCB is open
         DROP  R6                      IHADCB
*
TRN220   EQU   *
         L     R15,=A(NJENET)          -> NETDATA build and write
         BALR  R14,R15                 Go write NETDATA
         LTR   R15,R15                 Any errors?
         BNZ   ERR010                  Write i/o error
*
TRN300   EQU   *
         TM    FLAGS2,F2NCBOPN         Was spool open?
         BZ    TRN350                  No
*
         L     R5,16                   -> CVT
         L     R5,CVTSMCA-CVT(,R5)     -> SMCA
         LA    R5,SMCASID-SMCABASE(,R5) -> system id
*
*-- Fill in the tag data to satisfy the DMTXJE RSCS line driver used
*-- by NJE38.
*
TRN310   EQU   *
         LA    R6,TAGDATA              -> tag data area
         USING TAG,R6
*
         STCK  TAGINTOD                Time of spool file creation
*
         MVC   TAGDEV,=X'000C'         Pseudo card rdr CUU
         MVC   TAGINLOC,LCLNODE        Local node name of origin
         MVC   TAGINVM,USERID          Userid of origin
         MVC   TAGRECNM,OUTRECS        # of records written
         MVC   TAGRECLN,=Y(80)         Move record length
         MVI   TAGINDEV,TYPPUN         data type (PRT/PUN)
         MVC   TAGCLASS,=C'A'          Spool class
         MVC   TAGCOPY,=H'1'           # copies
         MVC   TAGNAME,BLANKS          Init receiving field
         MVC   TAGNAME(8),USERID       Insert userid
         MVC   TAGTYPE,=CL12'OUTPUT'
         MVC   TAGDIST,BLANKS          Init receiving field
         MVC   TAGDIST(4),0(R5)        Insert system id
         MVC   TAGTOLOC,DESTNODE       destination node
         MVC   TAGTOVM,DESTUSER        destination userid
         MVC   TAGPRIOR,=H'1'          priority
         DROP  R6                      TAG
*
TRN320   EQU   *
         NSIO  TYPE=CLOSE,             Close NETSPOOL                  x
               NCB=(R8),                                               x
               TAG=(R6)                Pass TAG data
         NI    FLAGS2,255-F2NCBOPN     Indicate NETSPOOL is closed
*
*-- Let NJE38 know that a new file was just placed into the spool so
*-- it can be queued for transmission.
*
         BAL   R2,CHK000               Determine NJE38 status      v210
         BNZ   TRN350                  NJE38 is not active         v210
         CLC   DESTNODE,LCLNODE        Trying to send file locally?
         BE    TRN350                  Y, but skip the POST
*
         LR    R7,R1                   -> NJE38 CSA ptr to R7      v210
         USING NJ38CSA,R7
*
         MODESET MODE=SUP,KEY=ZERO
*
         GETMAIN RU,                   Get CSA for WRE TYPE=WRENEW     x
               LV=WRESIZE,                                         v220x
               SP=241
         XC    0(WRESIZE,R1),0(R1)     Clear stg area              v220
         USING WRE,R1
         MVI   WRESP,241               Save subpool                v220
         MVI   WRETYPE,WRENEW          "New file in spool" WRE
         MVC   WRELINK,DESTNODE        Set destination node
         MVC   WREUSER,DESTUSER        Set destination userid
*
         LM    R2,R3,NJ38SWAP          Get first WRE ptr, sync count
TRN340   EQU   *
         ST    R2,WRENEXT              First WRE becomes next
         LR    R4,R1                   -> WRE to be added as first
         LA    R5,1(,R3)               Incr synchronization count
         CDS   R2,R4,NJ38SWAP          Update CSA WRE anchor, sync
         BC    7,TRN340                Gotta try again
*
         LA    R6,NJ38ECB              -> NJE38 external WRE ECB
         L     R7,NJ38ASCB             -> NJE38 ASCB
         DROP  R7                      NJ38CSA
*
         MVC   MACLIST(POSTL),POST     Move macro model
         POST  (6),                    Wake up NJE38 to new spool file x
               ASCB=(7),                                               x
               ERRET=TRN350,           Exit if can't do the post       x
               ECBKEY=0,                                               x
               MF=(E,MACLIST)
*
         MODESET MODE=PROB,KEY=NZERO
*
TRN350   EQU   *
*
*
*-- Transmission complete.  Issue # records sent and terminate.
*
*
TRN900   EQU   *
         LA    R2,MSGBLNK
         BAL   R14,PUTLINE             Write blank line
*
         MVC   LIST,BLANKS
*
*-- Record count not displayed until discrepancy with NJE counts   v200
*-- can be resolved.                                               v200
*                                                                  v200
*        L     R15,OUTRECS             # of output records written v200
*        CVD   R15,DBLE                unpk count                  v200
*        MVC   LIST+4(11),=X'2020206B2020206B202120'               v200
*        LA    R1,LIST+14              In case no significance     v200
*        EDMK  LIST+3(12),DBLE+3       Edit result                 v200
*        MVC   LIST+4(12),0(R1)        left justify displayed digitv200
*        TRT   LIST+4(12),BLANK        Look for end                v200
*        LA    R1,1(,R1)               Skip the blank              v200
         MVC   LIST+4(L'MSG009T),MSG009T Move 'file successfully'  v200
         LA    R1,LIST+L'MSG009T+4     -> next available byte      v200
*
         TM    FLAGS3,F3OUTDS          Using OUTDATASET?
         BO    TRN910
         MVC   0(10,R1),=C'queued to '
         MVC   10(8,R1),DESTNODE
         TRT   10(9,R1),BLANK          Look for end of node id
         MVI   0(R1),C'.'              Set dot
         MVC   1(8,R1),DESTUSER        Move userid
         LA    R1,9(,R1)               -> next available byte
         B     TRN920
*
TRN910   EQU   *
         MVC   0(12,R1),=C'written to '''
         MVC   12(44,R1),OUTPUTDS      Move OUTDATASET name
         TRT   12(45,R1),BLANK         Look for end of DSN
         MVI   0(R1),C''''             Set dot
         LA    R1,1(,R1)               -> next available byte
*
TRN920   EQU   *
         LA    R2,LIST                 -> msg
         MVC   0(4,R2),MSG009          Move RDW and flags
         SR    R1,R2
         STH   R1,LIST                 Set updated RDW
         BAL   R14,PUTLINE
         B     EXIT00
*
*
*--Error routines
*
ERR001   EQU   *
         MVC   LIST(4+L'MSG001T),MSG001  Move msg to work area
         CVD   R15,DBLE                unpk IKJPARS RC
         UNPK  LIST+57(2),DBLE
         OI    LIST+58,X'F0'           Fix sign
         LA    R2,LIST                 -> msg
         B     ERRPUT
*
ERR002   EQU   *
         LA    R2,MSG002               Input dataset is required
         B     ERRPUT                  Write it
*
ERR003   EQU   *
         LA    R2,MSG003               Input dataset not PS or PO
         B     ERRPUT                  Write it
*
ERR004   EQU   *
         LA    R2,MSG004               No parameters entered on cmd lin
         B     ERRPUT                  Write it
*
ERR005   EQU   *
         LA    R2,MSG005               Invalid node.user specified
         B     ERRPUT                  Write it
*
ERR006   EQU   *
         LA    R2,MSG006               Not APF authorized
         B     ERRPUT
*
ERR007   EQU   *
         LA    R2,MSG008               OUTDATASET not SEQ or PDS
         B     ERRPUT
*
ERR008   EQU   *
         LA    R2,MSG008               OUTDATASET is PDS but no mem
         B     ERRPUT
*
ERR010   EQU   *
         MVC   LIST(4+L'MSG010T),MSG010  Move msg text
         LA    R1,=CL10'OUTDATASET'    Assume writing to OUTDATASET
         TM    FLAGS3,F3OUTDS          Using OUTDATASET?
         BO    *+8                     We are
         LA    R1,=CL10'NETSPOOL'      NO, its NETSPOOL
         MVC   LIST+4+L'MSG010T(9),0(R1)  Move source of error
         LH    R1,LIST                 Get current msg length
         LA    R1,10(,R1)              Add on the source length
         STH   R1,LIST                 Put back
         LA    R2,LIST                 Write i/o error on OUTDS/SPOOL
         B     ERRPUT
*
ERR011   EQU   *
         LA    R2,MSG011               No destination node.user
         B     ERRPUT
*
ERR013   EQU   *
         LA    R2,MSG013               NJE38 is not active
         B     ERRPUT
*
ERR014   EQU   *
         LA    R2,MSG014               Security denied NETSPOOL access
         B     ERRPUT
*
*-- Member not found in DATASET (come here from ESTAE 013-18)      v200
*
ERR015   EQU   *                                                   v200
         LA    R13,NJESA               Ensure using proper SA sincev200
*                                       we came here from ESTAE    v200
*
         MVC   LIST(4+L'MSG015T),MSG015 Member not found msg       v200
         MVC   LIST+11(8),INMEM        Plug missing member name    v200
*                                                                  v200
         LA    R2,LIST                 -> start of msg             v200
         B     ERRPUT                                              v200
*
*-- TRANSMIT ended because IEBCOPY failed
*
ERR018   EQU   *
         LA    R2,MSGBLNK              -> blank line msg
         BAL   R14,PUTLINE
*
         MVC   LIST(4+L'MSG018T),MSG018 IEBCOPY fail msg
         CVD   R5,DBLE                 Convert IEBCOPY RC
         UNPK  LIST+38(2),DBLE
         OI    LIST+39,X'F0'           Fix sign
*
         LA    R2,LIST                 -> start of msg
         B     ERRPUT                  Display failure
*
ERR025   EQU   *
         LA    R2,MSG025               Need to run VERIFY
         B     ERRPUT
*
ERRPUT   EQU   *
         BAL   R14,PUTLINE
         B     EXIT08
*
*****************
* EXIT FROM     *
* TRANSMIT      *
*****************
*
*
*
EXIT00   EQU   *
         SR    R15,R15                 Set RC=0
         B     XIT000                  Clean up and exit
*
EXIT08   EQU   *
         LA    R15,8                   Set RC=8
         B     XIT000                  Clean up and exit
*
XIT000   EQU   *
         LA    R13,NJESA               Ensure using proper SA in case
*                                       we've come here due to ESTAE
         LR    R5,R15                  Save RC across shutdown
         ESTAE 0                       Disable ESTAE
*
         TM    FLAGS2,F2INOPN          Is input dataset open?
         BZ    XIT010                  No
         MVC   MACLIST(CLOSEL),CLOSE   Move close list
         CLOSE (INDS),                 Close it                        X
               MF=(E,MACLIST)
*
XIT010   EQU   *
         TM    FLAGS2,F2OUTOPN         Is OUTDATASET open?
         BZ    XIT020                  No
         MVC   MACLIST(CLOSEL),CLOSE   Move close list
         CLOSE (OUTDS),                Close it                        X
               MF=(E,MACLIST)
*
XIT020   EQU   *
         TM    FLAGS2,F2NCBOPN         Is NETSPOOL open?
         BZ    XIT030                  No
         SR    R6,R6                   Ensure no tag data
         LA    R1,NCB1                 -> NCB
         NSIO  TYPE=CLOSE,             Close the spool                 x
               NCB=(R1),TAG=(R6)
*
XIT030   EQU   *
*                                                                       NJE00200
XIT040   EQU   *                                                        NJE00210
         LA    R3,DDLIST               -> list of DD's we allocated
         LA    R4,UNLISTSZ/8           # of DD list entries
*
XIT050   EQU   *
         CLC   =XL8'00',0(R3)          Unassigned DD?
         BE    XIT060                  Skip to next
*
         MVC   UDDNAME,0(R3)
         LA    R0,UNDYN           00   unalloc
         L     R15,=A(NJEDYN)          -> dynamic allocation rtns
         BALR  R14,R15
*
XIT060   EQU   *                                                        NJE00210
         LA    R3,8(,R3)               -> next DD entry
         BCT   R4,XIT050               Continue unallocation scan
*
XIT070   EQU   *                                                        NJE00210
         TM    FLAGS1,F1AUSR           Special authorized user?
         BZ    XIT080                  Y, Don't need Auth SVC
         SR    0,0                     Use authorization SVC
         SR    1,1                      For HERC01/HERC02 only
         SVC   244                     Get un-authorized
*
XIT080   EQU   *                                                        NJE00210
*
QUIT     EQU   *                                                        NJE00210
         LR    R1,R10                  -> NJEWK main work area page
         L     R13,4(,R13)             -> caller's sa                   NJE00210
         ST    R5,16(,R13)             Set exit RC
         FREEMAIN RU,                                                  x
               LV=4096,                                                x
               A=(1)
         LM    R14,R12,12(R13)         Reload system's regs             NJE00220
         BR    R14                     Return                           NJE00240
*
*-- User ABENDs issued
*
ABEND101 EQU   *
         LA    R1,101                  OBTAIN failed for input DATASET
         B     ABEND
*
ABEND102 EQU   *
         LA    R1,102                  DEVTYPE failed for input DATASET
         B     ABEND
*
ABEND103 EQU   *
         LA    R1,103                  OBTAIN failed for IEBCOPY UNLD
         B     ABEND
*
ABEND104 EQU   *
         LA    R1,104                  DEVTYPE failed for IEBCOPY UNLD
         B     ABEND
*
ABEND105 EQU   *
         LA    R1,105                  OBTAIN failed for OUTDATASET
         B     ABEND
*
*ABEND106 EQU   *                      DSNAME build failure, See
*                                       label B2DSN020 in CSECT NJENET
*
ABEND    ABEND (1),DUMP,STEP
         DROP  R12
*
*-- STAX attention exit
*
*-- Doesn't do anything, but allows us to deallocate and get un-
*-- authorized rather than a native TSO abort.
*
STAXXIT  EQU   *
         STM   R14,R12,12(R13)         Save
         LR    R12,R15                 Get base
         USING STAXXIT,R12
         L     R10,8(,R1)              -> NJEWK area
         USING NJEWK,R10
         OI    FLAGS1,F1ATTN           Indicate ATTN pressed       v201
         LM    R14,R12,12(R13)         Load
         DROP  R12
         BR    R14                     Return
*
         LTORG ,
*
DMYINDS  DCB   DDNAME=INDS,                                            X
               MACRF=(GL),                                             X
               DSORG=PS,                                               X
               BFTEK=A,                                                X
               EODAD=EOD000
DMYINDSL EQU   *-DMYINDS
*
DMYOUTDS DCB   DDNAME=OUTDS,                                           X
               MACRF=(PM),                                             X
               DSORG=PS,                                               X
               BLKSIZE=3120,                                           X
               LRECL=80,                                               X
               RECFM=FB
DMYOUTDL EQU   *-DMYOUTDS
*
*
*
OPEN     OPEN  0,MF=L
OPENL    EQU   *-OPEN
CLOSE    CLOSE 0,MF=L
CLOSEL   EQU   *-CLOSE
LINK     LINK  EP=0,SF=L
LINKL    EQU   *-LINK
ESTAE    ESTAE 0,MF=L
ESTAEL   EQU   *-ESTAE
STAX     STAX  0,OBUF=(0,0),IBUF=(0,0),USADDR=0,MF=L
STAXL    EQU   *-STAX
DMYLST   CAMLST SEARCH,0,0,0
DMYLSTL  EQU   *-DMYLST
LOCATLST CAMLST NAME,0,,0
TRKCALC  TRKCALC MF=L
TRKCALCL EQU   *-TRKCALC
POST     POST  0,ASCB=0,ERRET=0,MF=L
POSTL    EQU   *-POST
*
COPYPARM DC    AL2(L'COPYOPT)
COPYOPT  DC    C'WORK=0512K'
COPYPRML EQU   *-COPYPARM                 TOTAL LENGTH OF PARM OPTION
*
ATTNMSG  DC  C'COMMAND TERMINATED DUE TO ATTENTION; PRESS ENTER TWICE'
*                                                                  v201
*********************
*  N J E C O M      *               NJECOM hosts small routines and
*                   *               frequently used constants that
*  Common routines  *               are available to all NJERxx csects
*  and constants    *               via base register 11
*                   *
*********************
*
NJECOM   CSECT
         DC    A(0)                 No branch around constants
         DC    AL1(23)                LENGTH OF EYECATCHERS
         DC    CL9'NJECOM'
         DC    CL9'&SYSDATE'
         DC    CL5'&SYSTIME'
         USING NJECOM,R11
*
*-- Write a record to the NJE38 spool, or to OUTDATASET
*
*-- Entry: None
*-- Exit:  RC=0 if write ok, RC=8 if write error.
*
PUT000   EQU   *
         ST    R14,SV14PUT             Save return reg
*
         TM    FLAGS3,F3OUTDS          Did user specify OUTDATASET?
         BZ    PUT050                  No, transmit to NETSPOOL
*
         PUT   OUTDS,REC               Write the record
         SR    R15,R15                 Set RC=0
         B     PUT090
*
PUT050   EQU   *
         LA    R1,NCB1
         NSIO  TYPE=PUT,               Write the record to spool       x
               NCB=(R1),                                               x
               AREA=REC,                                               x
               RECLEN=80
         LTR   R15,R15                 Any errors?
         BZ    PUT090                  No
         BAL   R14,FMT000              Display error
         LA    R15,8                   Set RC=8
*
PUT090   EQU   *
         LA    R1,1                    Get 1
         A     R1,OUTRECS              Bump record count
         ST    R1,OUTRECS              Update output counter
         L     R14,SV14PUT             Load return reg
         BR    R14                     Return with RC in R15
*
*-- Build IEBCOPY control statements
*
*-- Used if a member name was specified on DATASET and the PDS option
*-- was specified.
*
*-- Entry: None
*-- Exit:  None; card images written to the SYSIN dataset.
*
CTL000   EQU   *
         ST    R14,SV14SI              Save return reg
*
         L     R15,=A(DMYOUTDS)        -> DCB to use as model
         MVC   SYSINDS(DMYOUTDL),0(R15) Set up DCB model
         LA    R6,SYSINDS              -> DCB
         USING IHADCB,R6
         MVC   DCBDDNAM,DDSYSIN        Set DCB DDNAME
         DROP  R6                      IHADCB
*
         L     R15,=A(OPEN)            -> model list
         MVC   MACLIST(OPENL),0(R15)   Move OPEN list
         OPEN  (SYSINDS,OUTPUT),       Open the SYSIN dataset          X
               MF=(E,MACLIST)
         OI    FLAGS2,F2SYSOPN         Indicate DCB is open
*
*-- Build IEBCOPY control statements to select one member
*
         MVC   REC,BLANKS              Init record image
         MVC   REC+1(10),=C'COPY INDD='
         MVC   REC+11(8),DDSYSUT1      Set SYSUT1 DD name
         TRT   REC+11(9),BLANK         Look for end
         MVC   0(7,R1),=C',OUTDD='
         MVC   7(8,R1),DDSYSUT2        Set SYSUT2 DD name
         PUT   SYSINDS,REC             Write the COPY ctrl stmt
*
         MVC   REC,BLANKS              Init image
         MVC   REC+1(5),=C'S M=('
         MVC   REC+6(8),INMEM          User specified member name
         TRT   REC+6(9),BLANK          Look for end
         MVI   0(R1),C')'              End selection list
         PUT   SYSINDS,REC
*
         TM    FLAGS2,F2SYSOPN         Is SYSINDS open?
         BZ    CTL090                  No
         L     R15,=A(CLOSE)           -> model list
         MVC   MACLIST(CLOSEL),0(R15)  Move close list
         CLOSE (SYSINDS),              Close it                        X
               MF=(E,MACLIST)
*
CTL090   EQU   *
         L     R14,SV14SI              Load return reg
         BR    R14                     Return
*
*
*-- Format VSAM NETSPOOL errors
*
*
FMT000   EQU   *
         STM   R14,R2,PARSA+12         Borrow NJEPAR save area
         LA    R15,0(,R14)  Clear high, Get addr of call to this rtn
         L     R2,NJESA+4              -> system provided FSA
         L     R2,16(,R2)              Get R15's entry point addr
         LA    R2,0(,R2)               Ensure high byte clear
         SR    R15,R2                  Compute offset of call
         MVC   LIST+0(4+L'MSG024T),MSG024 Move msg text
         MVC   LIST+55(8),5(R2)        Move csect name
         TRT   LIST+55(9),BLANK        Look for end of csect name
         MVI   0(R1),C'+'
*
         ST    R15,DBLE                Save call offset to work area
         UNPK  TWRK(5),DBLE+2(3)       Add zones
         TR    TWRK(4),HEXTRAN-240     Display hex
         MVC   1(4,R1),TWRK            Move call offset to msg
*
         LA    R15,NCB1
         UNPK  TWRK(5),NCBRTNCD-NCB(3,R15)  Add zones
         TR    TWRK(4),HEXTRAN-240
         MVC   LIST+35(4),TWRK         Move rtncd/errcd
*
         UNPK  TWRK(3),NCBREQ-NCB(2,R15)  Add zones
         TR    TWRK(2),HEXTRAN-240
         MVC   LIST+45(2),TWRK         Move req code
*
         L     R1,NCBMACAD-NCB(,R15)   Get failing VSAM macro addr
         LA    R1,0(,R1)               Clear high byte
         S     R1,=V(NJESPOOL)         Compute offset into NJESPOOL rtn
         ST    R1,DBLE
         UNPK  TWRK(5),DBLE+2(3)       Add zones
         TR    TWRK(4),HEXTRAN-240     Display hex
         MVC   LIST+50(4),TWRK         Move NJESPOOL offset to msg
*
         LA    R2,LIST
         BAL   R14,PUTLINE
*
FMT090   EQU   *
         LM    R14,R2,PARSA+12         Restore caller regs
         BR    R14                     Return
*
*-- Write a single line to terminal
*
*-- Entry: R2 -> output msg (RDW+msg text)
*-- Exit:  R15 = RC from PUTLINE
*
PUTLINE  EQU   *
         TM    FLAGS3,F3QUIET          QUIET mode enabled?
         BZ    PUT010                  No, proceed
         CLI   3(R2),1                 Suppress this msg in QUIET mode?
         BER   R14                     Yes
*
PUT010   EQU   *
         ST    R14,SV14LN              Save return
         XC    PUTECB,PUTECB           Clear PUTLINE ECB
         L     R15,CPARMS              -> command input CPPL
         USING CPPL,R15
         LA    R1,IOPLAREA             -> IOPL
         USING IOPL,R1
         MVC   IOPLUPT,CPPLUPT         Set UPT ptr
         MVC   IOPLECT,CPPLECT         Set ECT ptr
         DROP  R15                     CPPL
*
         MVC   TWRK(PBL),PB            Move macro model
         PUTLINE PARM=TWRK,            Write a line                    x
               ECB=PUTECB,                                             x
               OUTPUT=((R2),TERM,SINGLE,DATA),                         x
               MF=(E,(1))
         DROP  R1                      IOPL
         L     R14,SV14LN              Load return
         BR    R14
*
*-- Get status of NJE38
*
*-- Entry: R1=0 (no spool dsn needed), or, R1-> 44-char spool DSN area
*-- Exit:  RC=0  NJE38 is active; R1-> NJE38 CSA block
*--        RC<>0 NJE is not active.
*
*
CHK000   EQU   *
         LA    R1,SPLDSN               => where to place spool DSN v210
         L     R15,=V(NJESYS)          -> ENQ finder               v210
         BALR  R14,R15                 Check if NJE38 already act  v210
         LTR   R15,R15                 Set CC (RC=0 NJE38 active)  v210
         BNZR  R2                      Return if NJE38 inactive    v210
         MVC   LCLNODE,NJ38NODE-NJ38CSA(R1)  Save off lcl node namev210
         MVC   DEFUSER,NJ38DUSR-NJ38CSA(R1)  Save off default user v210
         BR    R2                      Return; NJE38 active        v210
*
         LTORG
*
PB       PUTLINE MF=L
PBL      EQU   *-PB
*
NJE38Q   DC    CL8'NJE38'              QNAME
NJERCON  DC    CL8'NJEINIT'            RNAME (first 8 bytes)
*
*
*
BLANKS   DC    CL120' '
NONBLANK DC    64X'FF',X'00',191X'FF'  TR Table to locate nonblank
BLANK    DC    64X'00',X'FF',191X'00'  TR Table to locate blanks
DOTS     DC    75X'00',X'FF',180X'00'  TR Table to locate '.' char
HEXTRAN  DC    CL16'0123456789ABCDEF'  Translate table
*
NONALNUM EQU   *  0 1 2 3 4 5 6 7 8 9 A B C D E F
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'  0
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'  1   Allow alpha-
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'  2   numeric only
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'  3   and '.'
         DC    X'FFFFFFFFFFFFFFFFFFFFFF00FFFFFFFF'  4
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'  5
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'  6
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'  7
         DC    X'FF000000000000000000FFFFFFFFFFFF'  8
         DC    X'FF000000000000000000FFFFFFFFFFFF'  9
         DC    X'FFFF0000000000000000FFFFFFFFFFFF'  A
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'  B
         DC    X'FF000000000000000000FFFFFFFFFFFF'  C
         DC    X'FF000000000000000000FFFFFFFFFFFF'  D
         DC    X'FFFF0000000000000000FFFFFFFFFFFF'  E
         DC    X'00000000000000000000FFFFFFFFFFFF'  F
*
*-- TRANSMIT messages
*
*-- Note:  a '1' after the length indicates suppress this msg if QUIET
*
MSGBLNK  DC    Y(4+L'MSGBLNKT,1)
MSGBLNKT DC    C' '
*
MSG000   DC    Y(4+L'MSG000T,1)
MSG000T  DC    C'NJE38 TRANSMIT &VERS'
*
MSG001   DC    Y(4+L'MSG001T,0)
MSG001T  DC    C'Error parsing TRANSMIT command parameters. IKJPARS RC=x
               yy (dec)'
*                456789012345678901234567890123456789012345678901234567
*
MSG002   DC    Y(4+L'MSG002T,0)
MSG002T  DC    C'DATASET(dsname) parameter is missing; it is required'
*
MSG003   DC    Y(4+L'MSG003T,0)
MSG003T  DC    C'Input dataset must be sequential or partitioned (DSORGx
               =PS/PO)'
*                                                                       NJE00250
*
MSG004   DC    Y(4+L'MSG004T,0)
MSG004T  DC    C'No transmit parameters were specified'
*
MSG005   DC    Y(4+L'MSG005T,0)
MSG005T  DC    C'Invalid node.user specification'
*
MSG006   DC    Y(4+L'MSG006T,0)
MSG006T  DC    C'The TRANSMIT command is not APF-authorized'
*
MSG007   DC    Y(4+L'MSG007T,0)
MSG007T  DC    C'OUTDATASET must specify a sequential dataset or PDS wix
               th a member name'
*
MSG008   DC    Y(4+L'MSG008T,0)
MSG008T  DC    C'OUTDATASET specifies a PDS; a member name is required'
*
MSG009   DC    Y(4+L'MSG009T,1)
MSG009T  DC    C'File successfully '         queued to/written to  v200
*
MSG010   DC    Y(4+L'MSG010T,0)
MSG010T  DC    C'I/O error writing '
*
*
MSG011   DC    Y(4+L'MSG011T,0)
MSG011T  DC    C'A destination node.userid was not specified'
*
MSG012   DC    Y(4+44+L'MSG012T,0)
MSG012T  DC    C'Allocation error xxxxxxxx, DSN='
*
MSG013   DC    Y(4+L'MSG013T,0)
MSG013T  DC    C'NJE38 is not active'
*
*
MSG014   DC    Y(4+L'MSG014T,0)
MSG014T  DC    C'Access to the NETSPOOL dataset denied due to security x
               settings'
*
MSG015   DC    Y(4+L'MSG015T,0)                                    v200
MSG015T  DC    C'Member xxxxxxxx was not found'                    v200
*
MSG018   DC    Y(4+L'MSG018T,0)
MSG018T  DC    C'Transmit failed due to IEBCOPY RC=xx'
*                456789012345678901234567890123456789012345678901234567
*
MSG024   DC    Y(4+L'MSG024T,0)
MSG024T  DC    C'ERROR:  NETSPOOL RTNCD/ERRCD=X''0000'',REQ=01,O=1234,Mx
               MMMMMMM     '
*
MSG025   DC    Y(4+L'MSG025T,0)
MSG025T  DC    C'Unable to open NETSPOOL. Run IDCAMS VERIFY against thex
                NETSPOOL dataset'
*
MSG027   DC    Y(4+L'MSG027T,0)
MSG027T  DC    C' exists'
*
MSG031   DC    Y(4+L'MSG031T,0)
MSG031T  DC    C' does not exist'
*
*                                                                       NJE00250
*                                                                       NJE00250
*********************
*  N J E D Y N      *               NJEDYN handles the various
*                   *               dynamic allocations required
*  Handle DYNALLOC  *               and their unallocations as well.
*                   *
*********************
*
*        USING INMFIELD,R7          -> R7 at entry
*
NJEDYN   CSECT
         B     28(,R15)               BRANCH AROUND EYECATCHERS
         DC    AL1(23)                LENGTH OF EYECATCHERS
         DC    CL9'NJEDYN'
         DC    CL9'&SYSDATE'
         DC    CL5'&SYSTIME'
*
         STM   R14,R12,12(R13)         Save Regs                        NJE00050
         LR    R12,R15                 Base                             NJE00060
         USING NJEDYN,R12                                               NJE00070
         USING NJEWK,R10
         ST    R13,DYNSA+4             SAVE prv S.A. ADDR               NJE00080
         LA    R1,DYNSA                -> my save area
         ST    R1,8(,R13)              Plug it into prior SA
         LR    R13,R1
*
         MVC   LS99RB,CPS99RB          init THE S99RB
         LA    R1,LS99RB               -> S99RB
         USING S99RB,R1
         ST    R1,LS99PTR              Set parameter word
         OI    LS99PTR,X'80'           Set VL
         LA    R6,TXTPTRS              -> start of text unit list
         ST    R6,S99TXTPP             Put in S99RB
         DROP  R1                      S99RB
*
UNDYN    EQU   0                   00  unallocate DDNAME
DYNUNDEF EQU   4                   04  unused, undefined
DYNSYSIN EQU   8                   08  Allocate SYSIN for IEBCOPY
DYNSYSPR EQU   12                  0C  Allocate SYSPRINT for IEBCOPY
DYNUNLD  EQU   16                  10  Allocate unload dataset IEBCOPY
DYNSYSU4 EQU   20                  14  Allocate SYSUT4 IEBCOPY
DYNINDS  EQU   24                  18  Allocate user input dataset
DYNETSPL EQU   28                  1C  Allocate NETSPOOL
DYNOUTDS EQU   32                  20  Allocate OUTDATASET
*
         LR    R5,R0                   Copy action code
         B     DYN000(R5)              Branch into table
*
DYN000   B     DYN010              00  Perform DDNAME Unallocation
         DC    AL4(0)              04  undefined
         B     DYN200              08  Allocate SYSIN for IEBCOPY
         B     DYN300              0C  Allocate SYSPRINT for IEBCOPY
         B     DYN400              10  Allocate unload dataset IEBCOPY
         B     DYN500              14  Allocate SYSUT4 IEBCOPY
         B     DYN600              18  Allocate user input dataset
         B     DYN700              1C  Allocate NETSPOOL
         B     DYN800              20  Allocate OUTDATASET
*
DYN010   EQU   *
         MVC   UTXT,UTXTD              Init text unit
         LA    R1,LS99RB               -> S99RB
         USING S99RB,R1
         MVI   S99VERB,S99VRBUN        Set verb code to unallocation
         DROP  R1                      S99RB
*
         LA    R0,UTXT                 -> UNALLOC DD text unit
         ST    R0,0(,R6)               Plug into ptr list
         OI    0(R6),X'80'             End the parameter list
         B     DYN900                  Deallocate the DD
*
*-- SYSIN for IEBCOPY
*
*   Equivalent JCL  (if command line SEQL specified or defaulted):
*   //SYS00000 DD DUMMY
*
*
*   Equivalent JCL  (if command line PDS specified and a member name
*                    was coded in DATASET):
*   //SYS00000 DD DISP=(NEW,DELETE),UNIT=SYSDA,
*   //            SPACE=(CYL,1)
*
*
DYN200   EQU   *
         TM    FLAGS3,F3PDS            PDS copy forced?
         BO    DYN220                  Y, we need to set up for
*                                        IEBCOPY control statements
*
         MVC   TXT01,TXT01D            Init from the models
         MVC   TXT16,TXT16D
         LA    R0,TXT01                -> return DDNAME
         ST    R0,0(,R6)               Plug into ptr list
         LA    R6,4(,R6)               -> next ptr list slot
         LA    R0,TXT16                -> DUMMY
         ST    R0,0(,R6)               Plug into ptr list
         OI    0(R6),X'80'             End the parameter list
         B     DYN900                  Go allocate
*
DYN220   EQU   *
         MVC   TXT01,TXT01D            Init from the models
         MVC   TXT03,TXT03D            DISP 1
         MVC   TXT04,TXT04D            DISP 2
         MVC   TXT06,TXT06D            PRIME
         MVC   TXT10,TXT10D            UNIT
         MVC   TXT19,TXT19D            CYL
*
         MVI   TXT04+6,X'04'           Adjust to DISP=,DELETE
         MVC   TXT06+6(3),=XL3'01'     1 cylinders
*
         LA    R0,TXT01                -> return DDNAME
         ST    R0,0(,R6)               Plug into ptr list
         LA    R6,4(,R6)               -> next ptr list slot
         LA    R0,TXT03                -> DISP=NEW
         ST    R0,0(,R6)               Plug into ptr list
         LA    R6,4(,R6)               -> next ptr list slot
         LA    R0,TXT04                -> DISP=,DELETE
         ST    R0,0(,R6)               Plug into ptr list
         LA    R6,4(,R6)               -> next ptr list slot
         LA    R0,TXT06                -> Primary space
         ST    R0,0(,R6)               Plug into ptr list
         LA    R6,4(,R6)               -> next ptr list slot
         LA    R0,TXT10                -> UNIT
         ST    R0,0(,R6)               Plug into ptr list
         LA    R6,4(,R6)               -> next ptr list slot
         LA    R0,TXT19                -> SPACE CYL
         ST    R0,0(,R6)               Plug into ptr list
         OI    0(R6),X'80'             End the parameter list
         B     DYN900                  Go allocate
*
*-- SYSPRINT for IEBCOPY
*
*   Equivalent JCL:
*   //SYS00000 DD SYSOUT=*,TERM=TS
*
DYN300   EQU   *
         MVC   TXT01,TXT01D            Init from the models
         MVC   TXT16,TXT16D
         MVC   TXT17,TXT17D
         MVC   TXT18,TXT18D
         LA    R0,TXT01                -> return DDNAME
         ST    R0,0(,R6)               Plug into ptr list
*
         TM    FLAGS3,F3QUIET          QUIET mode enabled?
         BO    DYN310                  Yes, use DUMMY
         LA    R6,4(,R6)               -> next ptr list slot
         LA    R0,TXT17                -> SYSOUT=*
         ST    R0,0(,R6)               Plug into ptr list
         LA    R6,4(,R6)               -> next ptr list slot
         LA    R0,TXT18                -> TERM=TS
         ST    R0,0(,R6)               Plug into ptr list
         B     DYN320
*
DYN310   EQU   *
         LA    R6,4(,R6)               -> next ptr list slot
         LA    R0,TXT16                -> DUMMY
         ST    R0,0(,R6)               Plug into ptr list
*
DYN320   EQU   *
         OI    0(R6),X'80'             End the parameter list
         B     DYN900                  Go allocate
*
*-- Temporary dataset that IEBCOPY will unload into
*
*   Equivalent JCL:
*   //SYS00000 DD DISP=(NEW,DELETE),UNIT=SYSDA,
*   //            SPACE=(4096,(pri,sec)),
*   //            DCB=(BLKSIZE=4096,DSORG=PS)
*
DYN400   EQU   *
         MVC   TXT01,TXT01D            Init from the models
         MVC   TXT02,TXT02D            DSN
         MVC   TXT03,TXT03D            DISP1
         MVC   TXT04,TXT04D            DISP2
         MVC   TXT05,TXT05D            Blklen
         MVC   TXT06,TXT06D            Prime
         MVC   TXT07,TXT07D            Second
         MVC   TXT09,TXT09D            volume
         MVC   TXT10,TXT10D            unit
         MVC   TXT12,TXT12D            BLKSIZE
         MVC   TXT13,TXT13D            DSORG
*
         LA    R0,TXT01                -> Return DDNAME text unit
         ST    R0,0(,R6)               Plug into ptr list
         LA    R6,4(,R6)               -> next ptr list slot
         LA    R0,TXT03                -> DISP text unit 1
         ST    R0,0(,R6)               Plug into ptr list
         LA    R6,4(,R6)               -> next ptr list slot
         LA    R0,TXT04                -> DISP text unit 2
         ST    R0,0(,R6)               Plug into ptr list
         LA    R6,4(,R6)               -> next ptr list slot
         LA    R0,TXT05                -> BLKLEN text unit 2
         ST    R0,0(,R6)               Plug into ptr list
         LA    R6,4(,R6)               -> next ptr list slot
         LA    R0,TXT06                -> PRIMARY text unit
         ST    R0,0(,R6)               Plug into ptr list
         LA    R6,4(,R6)               -> next ptr list slot
         LA    R0,TXT07                -> SECONDARY text unit
         ST    R0,0(,R6)               Plug into ptr list
         LA    R6,4(,R6)               -> next ptr list slot
         LA    R0,TXT12                -> BLKSIZE text unit
         ST    R0,0(,R6)               Plug into ptr list
         LA    R6,4(,R6)               -> next ptr list slot
         LA    R0,TXT13                -> DSORG text unit
         ST    R0,0(,R6)               Plug into ptr list
         LA    R6,4(,R6)               -> next ptr list slot
         LA    R0,TXT09                -> VOLSER text unit
         ST    R0,0(,R6)               Plug into ptr list
         LA    R6,4(,R6)               -> next ptr list slot
         LA    R0,TXT10                -> UNIT text unit
         ST    R0,0(,R6)               Plug into ptr list
         LA    R6,4(,R6)               -> next ptr list slot
         LA    R0,TXT02                -> DSN text unit
         ST    R0,0(,R6)               Plug into ptr list
         OI    0(R6),X'80'             End the parameter list
*
         MVI   TXT04+6,X'04'           Set DISP=,DELETE
         MVC   TXT09(2),=Y(DALRTVOL)   Set to return VOLSER
         MVC   TXT02(2),=Y(DALRTDSN)   Set to return DSN
         B     DYN900                  Go allocate
*
*-- SYSUT4 for IEBCOPY
*
*   Equivalent JCL:
*   //SYS00000 DD DISP=(NEW,DELETE),UNIT=SYSDA,
*   //            SPACE=(CYL,5)
*
DYN500   EQU   *
         MVC   TXT01,TXT01D            Init from the models
         MVC   TXT03,TXT03D            DISP 1
         MVC   TXT04,TXT04D            DISP 2
         MVC   TXT06,TXT06D            PRIME
         MVC   TXT10,TXT10D            UNIT
         MVC   TXT19,TXT19D            CYL
*
         MVI   TXT04+6,X'04'           Adjust to DISP=,DELETE
         MVC   TXT06+6(3),=XL3'05'     5 cylinders
*
         LA    R0,TXT01                -> return DDNAME
         ST    R0,0(,R6)               Plug into ptr list
         LA    R6,4(,R6)               -> next ptr list slot
         LA    R0,TXT03                -> DISP=NEW
         ST    R0,0(,R6)               Plug into ptr list
         LA    R6,4(,R6)               -> next ptr list slot
         LA    R0,TXT04                -> DISP=,DELETE
         ST    R0,0(,R6)               Plug into ptr list
         LA    R6,4(,R6)               -> next ptr list slot
         LA    R0,TXT06                -> Primary space
         ST    R0,0(,R6)               Plug into ptr list
         LA    R6,4(,R6)               -> next ptr list slot
         LA    R0,TXT10                -> UNIT
         ST    R0,0(,R6)               Plug into ptr list
         LA    R6,4(,R6)               -> next ptr list slot
         LA    R0,TXT19                -> SPACE CYL
         ST    R0,0(,R6)               Plug into ptr list
         OI    0(R6),X'80'             End the parameter list
         B     DYN900                  Go allocate
*
*-- Dataset user.input.dataset from command line
*
*   Equivalent JCL:
*   //SYS00000 DD DISP=SHR,DSNAME=user.input.dataset(mem)
*
DYN600   EQU   *
         MVC   TXT01,TXT01D            Init from the models
         MVC   TXT02,TXT02D
         MVC   TXT03,TXT03D
         MVC   TXT09,TXT09D
         MVC   TXT13,TXT13D
         MVC   TXT21,TXT21D
*
         MVI   TXT03+6,X'08'           set DISP=SHR
         MVC   TXT09(2),=Y(DALRTVOL)   Set to return VOLSER
         MVC   TXT13(2),=Y(DALRTORG)   Set to return DSORG
*
         LA    R0,TXT01                -> return DDNAME
         ST    R0,0(,R6)               Plug into ptr list
         LA    R6,4(,R6)               -> next ptr list slot
         LA    R0,TXT03                -> DISP=SHR
         ST    R0,0(,R6)               Plug into ptr list
         LA    R6,4(,R6)               -> next ptr list slot
         LA    R0,TXT09                -> RETURN VOLSER
         ST    R0,0(,R6)               Plug into ptr list
         LA    R6,4(,R6)               -> next ptr list slot
         LA    R0,TXT13                -> RETURN DSORG
         ST    R0,0(,R6)               Plug into ptr list
         LA    R6,4(,R6)               -> next ptr list slot
         LA    R0,TXT02                -> DSNAME
         ST    R0,0(,R6)               Plug into ptr list
*
         TM    FLAGS3,F3PDS            Was PDS specified?
         BO    DYN610                  Yes, we'll use IEBCOPY, no mbr
         TM    FLAGS3,F3INMEM          Was a member specified?
         BZ    DYN610                  No
         LA    R6,4(,R6)               -> next ptr list slot
         LA    R0,TXT21                -> MEMBER
         ST    R0,0(,R6)               Plug into ptr list
*
DYN610   EQU   *
         OI    0(R6),X'80'             End the parameter list
         B     DYN900                  Go allocate
*
*-- Dataset NETSPOOL
*
*   Equivalent JCL:
*   //NETSPOOL DD DISP=SHR,DSNAME=NJE38.NETSPOOL
*
*
DYN700   EQU   *
         MVC   TXT01,TXT01D            Init from the models
         MVC   TXT02,TXT02D
         MVC   TXT03,TXT03D
*
         MVC   TXT01(2),=Y(DALDDNAM)   Use fixed DD
         MVI   TXT03+6,X'08'           set DISP=SHR
*
         LA    R0,TXT01                -> DDNAME
         ST    R0,0(,R6)               Plug into ptr list
         LA    R6,4(,R6)               -> next ptr list slot
         LA    R0,TXT03                -> DISP=SHR
         ST    R0,0(,R6)               Plug into ptr list
         LA    R6,4(,R6)               -> next ptr list slot
         LA    R0,TXT02                -> DSNAME
         ST    R0,0(,R6)               Plug into ptr list
*
         OI    0(R6),X'80'             End the parameter list
         B     DYN900                  Go allocate
*
*-- Dataset created for OUTDATASET
*
*   Equivalent JCL:
*   //SYS00000 DD DISP=(NEW,CATLG),UNIT=unitname,
*   //            SPACE=(3120,(pri,sec)),
*   //            DCB=(BLKSIZE=3120,LRECL=80,RECFM=FB,DSORG=PS),
*   //            DSN=dsname,VOL=SER=volser
*
DYN800   EQU   *
         TM    FLAGS2,F2EXIST          Does OUTDATASET exist?
         BO    DYN850                  Yes, don't create it
*
         MVC   TXT01,TXT01D            Init from the models
         MVC   TXT02,TXT02D
         MVC   TXT03,TXT03D
         MVC   TXT04,TXT04D
         MVC   TXT05,TXT05D
         MVC   TXT06,TXT06D
         MVC   TXT07,TXT07D
         MVC   TXT08,TXT08D
         MVC   TXT09,TXT09D
         MVC   TXT10,TXT10D
         MVC   TXT12,TXT12D
         MVC   TXT13,TXT13D
         MVC   TXT14,TXT14D
         MVC   TXT15,TXT15D
         MVC   TXT21,TXT21D
*
         LA    R0,TXT01                -> Return DDNAME text unit
         ST    R0,0(,R6)               Plug into ptr list
         LA    R6,4(,R6)               -> next ptr list slot
         LA    R0,TXT03                -> DISP text unit 1
         ST    R0,0(,R6)               Plug into ptr list
         LA    R6,4(,R6)               -> next ptr list slot
         LA    R0,TXT04                -> DISP text unit 2
         ST    R0,0(,R6)               Plug into ptr list
         LA    R6,4(,R6)               -> next ptr list slot
         LA    R0,TXT12                -> BLKSIZE text unit
         ST    R0,0(,R6)               Plug into ptr list
         LA    R6,4(,R6)               -> next ptr list slot
         LA    R0,TXT05                -> BLKLEN text unit
         ST    R0,0(,R6)               Plug into ptr list
         LA    R6,4(,R6)               -> next ptr list slot
         LA    R0,TXT14                -> LRECL text unit
         ST    R0,0(,R6)               Plug into ptr list
         LA    R6,4(,R6)               -> next ptr list slot
         LA    R0,TXT15                -> RECFM text unit
         ST    R0,0(,R6)               Plug into ptr list
         LA    R6,4(,R6)               -> next ptr list slot
         LA    R0,TXT06                -> PRIMARY text unit
         ST    R0,0(,R6)               Plug into ptr list
         LA    R6,4(,R6)               -> next ptr list slot
         LA    R0,TXT07                -> SECONDARY text unit
         ST    R0,0(,R6)               Plug into ptr list
         LA    R6,4(,R6)               -> next ptr list slot
         LA    R0,TXT13                -> DSORG text unit
         ST    R0,0(,R6)               Plug into ptr list
         LA    R6,4(,R6)               -> next ptr list slot
         LA    R0,TXT10                -> UNIT text unit
         ST    R0,0(,R6)               Plug into ptr list
         LA    R6,4(,R6)               -> next ptr list slot
         LA    R0,TXT02                -> DSN text unit
         ST    R0,0(,R6)               Plug into ptr list
         TM    FLAGS3,F3VOLSER         Was there a volser?
         BZ    DYN810                  No
*
         LA    R6,4(,R6)               -> next ptr list slot
         LA    R0,TXT09                -> VOLSER text unit
         ST    R0,0(,R6)               Plug into ptr list
*
DYN810   EQU   *
         TM    FLAGS3,F3OUTMEM         Was a member specified?
         BZ    DYN820                  No
         MVC   TDSORG,=X'0200'         Force DSORG to PO if member
         MVC   TDIRBLKS,=AL3(5)        Set 5 directory blocks
         LA    R6,4(,R6)               -> next ptr list slot
         LA    R0,TXT08                -> DIRBLKS
         ST    R0,0(,R6)               Plug into ptr list
         LA    R6,4(,R6)               -> next ptr list slot
         LA    R0,TXT21                -> MEMBER
         ST    R0,0(,R6)               Plug into ptr list
*
DYN820   EQU   *
         TM    FLAGS2,F2UNIT           User specified unit?        v200
         BZ    DYN890                  No                          v200
         MVC   TUNIT,OUTUNIT           Use user specified unit namev200
*
DYN890   EQU   *                                                   v200
         OI    0(R6),X'80'             End the parameter list
         B     DYN900                  Go allocate
*
*-- Allocate existing OUTDATASET (with optional member)
*
*   Equivalent JCL:
*   //SYS00000 DD DISP=SHR,DSNAME=out.data.set(mem)
*
DYN850   EQU   *
         MVC   TXT01,TXT01D            Init from the models
         MVC   TXT02,TXT02D
         MVC   TXT03,TXT03D
         MVC   TXT21,TXT21D
*
         MVI   TXT03+6,X'08'           set DISP=SHR
*
         LA    R0,TXT01                -> return DDNAME
         ST    R0,0(,R6)               Plug into ptr list
         LA    R6,4(,R6)               -> next ptr list slot
         LA    R0,TXT03                -> DISP=SHR
         ST    R0,0(,R6)               Plug into ptr list
         LA    R6,4(,R6)               -> next ptr list slot
         LA    R0,TXT02                -> DSNAME
         ST    R0,0(,R6)               Plug into ptr list
*
         TM    FLAGS3,F3OUTMEM         Was a member specified?
         BZ    DYN860                  No
         LA    R6,4(,R6)               -> next ptr list slot
         LA    R0,TXT21                -> MEMBER
         ST    R0,0(,R6)               Plug into ptr list
*
DYN860   EQU   *
         OI    0(R6),X'80'             End the parameter list
         B     DYN900                  Go allocate
*
*-- Allocate the dataset
*
DYN900   EQU   *
         LA    R1,LS99RB               -> S99RB
         USING S99RB,R1
         OI    S99FLAG1,S99NOCNV        FORCE NEW ALLOCATION
         DROP  R1
         LA    R1,LS99PTR               POINTER TO S99 PTR
         SVC   99                       ISSUE DYNALLOC
         LTR   R15,R15                  Any errors?
         BZ    XITDYN00                 No
*
         LA    R1,LS99RB
         USING S99RB,R1
         UNPK  TWRK(9),S99ERROR(5)      Add zones to error code
         DROP  R1
         TR    TWRK(8),HEXTRAN-240
*
         CLI   TWRK+1,C'7'              Class 7 error code?
         BNE   ERR012                   No
         LA    R1,DYNINDS               Code for the input dataset?
         CR    R1,R5                    Was alloc for DYNINDS?
         BE    ERR031                   Yes, dataset does not exist
*
ERR012   EQU   *
         MVC   LIST(4+L'MSG012T),MSG012 Dyn alloc failure msg
         MVC   LIST+21(8),TWRK          Error codes to line
         MVC   LIST+35(44),TDSNAME      Move DSNAME
         LA    R2,LIST                  -> msg
         BAL   R14,PUTLINE              Display it
         B     XITDYN08
*
ERR031   EQU   *
         MVC   LIST,BLANKS
         MVC   LIST+4(9),=C'Dataset '''
         MVC   LIST+13(44),TDSNAME Move name
         TRT   LIST+13(45),BLANK   Look for end of name
         MVI   0(R1),C''''         Close apost
         LA    R1,1(,R1)           Skip apost
         MVC   0(L'MSG031T,R1),MSG031T  Move rest of msg
         LA    R1,L'MSG031T(,R1)   point to end
         XC    LIST(4),LIST        Clear RDW area
         LA    R2,LIST             -> start of RDW+msg
         SR    R1,R2               Compute total length
         STH   R1,LIST             Plug RDW
         BAL   R14,PUTLINE         Inform user
         B     XITDYN04            And exit with dataset doesnt exist
*
*
*-- Exit
*
XITDYN00 EQU   *
         SR    R15,R15             Set RC=0;  alloc/dealloc ok
         B     XITDYN
*
XITDYN04 EQU   *
         LA    R15,4               Set RC=4;  Exit for special action
         B     XITDYN
*
XITDYN08 EQU   *
         LA    R15,8               Set RC=8;  allocation error
*
XITDYN   EQU   *
         L     R13,4(,R13)         -> prev s.a.
         ST    R15,16(,R13)        Set RC
         LM    R14,R12,12(R13)     Reload callers regs
         BR    R14                 Return with RC
*
         LTORG
*        DROP  R7                  INMFIELD
*
*
*
*-- Text unit skeletons
*
*-- Note:  EXPDT is included for completeness but is not used.
*
*
*
TXT01D   DC    Y(DALRTDDN),AL2(1),AL2(8)          RETURN DDNAME
TXT02D   DC    Y(DALDSNAM),AL2(1),AL2(44)         DSNAME
TXT03D   DC    Y(DALSTATS),AL2(1),AL2(1),X'04'    DISP=(NEW,)
TXT04D   DC    Y(DALNDISP),AL2(1),AL2(1),X'02'    DISP=(,CATLG)
TXT05D   DC    Y(DALBLKLN),AL2(1),AL2(3)          BLK TEXT KEY, BLKLEN
TXT06D   DC    Y(DALPRIME),AL2(1),AL2(3)          PRIMARY SPACE UNITS
TXT07D   DC    Y(DALSECND),AL2(1),AL2(3)          SECONDARY SPACE UNITS
TXT08D   DC    Y(DALDIR),AL2(1),AL2(3)            DIRECTORY BLOCKS
TXT09D   DC    Y(DALVLSER),AL2(1),AL2(6)          VOLSER
TXT10D   DC    Y(DALUNIT),AL2(1),AL2(8),CL8'SYSDA' UNIT default    v200
TXT11D   DC    Y(DALEXPDT),AL2(1),AL2(5)          EXPDT C'YYDDD'
TXT12D   DC    Y(DALBLKSZ),AL2(1),AL2(2)          BLKSIZE
TXT13D   DC    Y(DALDSORG),AL2(1),AL2(2)          DSORG
TXT14D   DC    Y(DALLRECL),AL2(1),AL2(2)          LRECL
TXT15D   DC    Y(DALRECFM),AL2(1),AL2(1)          RECFM
TXT16D   DC    Y(DALDUMMY),AL2(0)                 DUMMY
TXT17D   DC    Y(DALSYSOU),AL2(0)                 SYSOUT
TXT18D   DC    Y(DALTERM),AL2(0)                  TERM
TXT19D   DC    Y(DALCYL),AL2(0)                   CYLINDER
TXT20D   DC    Y(DALCLOSE),AL2(0)                 FREE=CLOSE
TXT21D   DC    Y(DALMEMBR),AL2(1),AL2(8)          MEMBER
*
UTXTD    DC    Y(DUNDDNAM),AL2(1),AL2(8)          DD for deallocation
*
         DS    0F
CPS99RB  DS    0XL20                   DEFINE INITIAL S99RB
         DC    AL1(20)                 LENGTH OF REQ BLOCK
         DC    AL1(1)                  VERB CODE:  ALLOCATION
         DC    X'20'                   FLAGS:  NO MOUNTS,OFFLINE VOLS
         DC    X'00'                   FLAGS
         DC    AL2(0)                  ERROR REASON CODE
         DC    AL2(0)                  INFO REASON CODE
         DC    A(0)                    ADDR OF TEXT PTRS
         DC    A(0)                    ADDR OF RBX
         DC    AL4(0)                  MORE FLAGS
*                                                                       NJE00250
*
*
*********************
*  N J E N E T      *               NJENET converts the incoming
*                   *               files into NETDATA format and
*  Output NETDATA   *               writes 80-byte records to the spool
*                   *               or OUTDATASET destination.
*********************
*
NJENET   CSECT
         B     28(,R15)               BRANCH AROUND EYECATCHERS
         DC    AL1(23)                LENGTH OF EYECATCHERS
         DC    CL9'NJENET'
         DC    CL9'&SYSDATE'
         DC    CL5'&SYSTIME'
*
         STM   R14,R12,12(R13)         Save Regs
         LR    R12,R15                 Base
         USING NJENET,R12
         USING NJEWK,R10
         ST    R13,NETSA+4             SAVE prv S.A. ADDR
         LA    R1,NETSA                -> my save area
         ST    R1,8(,R13)              Plug it into prior SA
         LR    R13,R1
*
*
****************
* BUILD INMR01 *
****************
*
         USING INMFIELD,R7
         LA    R7,INMF01               -> INMR01 fields
         LA    R3,BUFF                 -> build buffer
         MVI   0(R3),0                 Init control record length
         MVI   1(R3),X'E0'             Indicate 1 segment, ctl record
         MVC   2(6,R3),INMR01          Create INMR01
         LA    R3,8(,R3)               -> next available byte
*
         BAL   R14,B1LRECL             Build the LRECL
         BAL   R14,B1FNODE             Build the FNODE
         BAL   R14,B1FUID              Build the FUID
         BAL   R14,B1TNODE             Build the TNODE
         BAL   R14,B1TUID              Build the TUID
         BAL   R14,B1FTIME             Build the time stamp
         BAL   R14,B1NUMF              Build the number of files
*
         LA    R1,BUFF                 -> start of build buffer
         SR    R3,R1                   Compute INMR01 total length
         STC   R3,0(,R1)               Plug into length byte
*
         LR    R0,R3                   Length to R0
         BAL   R14,PUTBYTES            Write the entire segment
*
****************
* BUILD INMR02 *
****************
*
         LA    R7,INMF02A              -> 1st INMR02 fields
         LA    R3,BUFF                 -> build buffer
         MVI   0(R3),0                 Init control record length
         MVI   1(R3),X'E0'             Indicate 1 segment, ctl record
         MVC   2(6,R3),INMR02          Create INMR02
         MVC   8(4,R3),=F'1'           Set file number to 1
         LA    R3,12(,R3)              -> next available byte
*
         BAL   R14,B2UTIL              Build the Utility name
         BAL   R14,B2FSIZE             Build the file size
         BAL   R14,B2DIRBLK            Build the dir blocks
         BAL   R14,B2LRECL             Build the LRECL
         BAL   R14,B2DSORG             Build the DSORG
         BAL   R14,B2BLKSI             Build the BLKSIZE
         BAL   R14,B2RECFM             Build the RECFM
         BAL   R14,B2DSN               Build the DSNAME
*
         LA    R1,BUFF                 -> start of build buffer
         SR    R3,R1                   Compute INMR02 total length
         STC   R3,0(,R1)               Plug into length byte
*
         LR    R0,R3                   Length to R0
         BAL   R14,PUTBYTES            Write the entire segment
*
****************
* BUILD INMR02 *  Second INMR02 is build if input DS was a PDS
****************
*
         TM    FLAGS1,F1INPDS          Was input DS a PDS?
         BZ    INM3                    No, dont need 2nd INMR02
         LA    R7,INMF02B              -> 2nd INMR02 fields
         LA    R3,BUFF                 -> build buffer
         MVI   0(R3),0                 Init control record length
         MVI   1(R3),X'E0'             Indicate 1 segment, ctl record
         MVC   2(6,R3),INMR02          Create INMR02
         MVC   8(4,R3),=F'1'           Set file number to 1
         LA    R3,12(,R3)              -> next available byte
*
         BAL   R14,B2UTIL              Build the Utility name
         BAL   R14,B2FSIZE             Build the file size
         BAL   R14,B2LRECL             Build the LRECL
         BAL   R14,B2DSORG             Build the DSORG
         BAL   R14,B2BLKSI             Build the BLKSIZE
         BAL   R14,B2RECFM             Build the RECFM
*
         LA    R1,BUFF                 -> start of build buffer
         SR    R3,R1                   Compute INMR02 total length
         STC   R3,0(,R1)               Plug into length byte
*
         LR    R0,R3                   Length to R0
         BAL   R14,PUTBYTES            Write the entire segment
*
****************
* BUILD INMR03 *
****************
*
INM3     EQU   *
         LA    R7,INMF02A              -> 1st INMR02 fields
         L     R0,FILESIZE             Get size from prev INMR02 buffer
         LA    R7,INMF03               -> INMR03 fields
         ST    R0,FILESIZE             Plug it into INMR03 buffer
         LA    R3,BUFF                 -> build buffer
         MVI   0(R3),0                 Init control record length
         MVI   1(R3),X'E0'             Indicate 1 segment, ctl record
         MVC   2(6,R3),INMR03          Create INMR02
         LA    R3,8(,R3)               -> next available byte
*
         BAL   R14,B3FSIZE             Build the file size
         BAL   R14,B3LRECL             Build the LRECL
         BAL   R14,B3DSORG             Build the DSORG
         BAL   R14,B3RECFM             Build the RECFM
*
         LA    R1,BUFF                 -> start of build buffer
         SR    R3,R1                   Compute INMR03 total length
         STC   R3,0(,R1)               Plug into length byte
*
         LR    R0,R3                   Length to R0
         BAL   R14,PUTBYTES            Write the entire segment
         DROP  R7                      INMFIELD
*
****************
* PERFORM      *
* "INMCOPY"    *
* FUNCTION     *
****************
*
CPY000   EQU   *
         LA    R4,INDS                 -> INDS DCB
         USING IHADCB,R4
         LA    R9,253                  Segment size (less len,ctl bytes
*
CPY020   EQU   *
         GET   INDS                    Get input record
         LR    R2,R1                   -> record to R2
         LH    R3,DCBLRECL             Get record length
         MVI   CTL,X'80'               Assume starting new segment
*
         TM    DCBRECFM,DCBRECF        RECFM=F (or U) records?
         BO    CPY060                  Handle them same way
         LH    R3,0(,R2)               Get length from RDW
         S     R3,=F'4'                Remove length of RDW
         LA    R2,4(,R1)               Skip over RDW
*
CPY060   EQU   *
         LR    R5,R3                   Working length to R5
         CR    R3,R9                   LRECL <= max segment size?
         BNH   CPY070                  Yes
         LR    R5,R9                   Else limit to max segment
*
CPY070   EQU   *
         SR    R3,R5                   Compute remaining length
         BCTR  R5,0                    Adjust working len for execute
         EX    R5,MVCREC               Move record to build buffer
         LA    R2,1(R5,R2)             -> next avail byte in record
         LA    R0,3(,R5)               Account for ex, len & ctl bytes
         STC   R0,BUFF                 Set the segment length
         LTR   R3,R3                   Is length remaining?
         BNZ   CPY080                  Yes
         OI    CTL,X'40'               Indicate this is last segment
*
CPY080   EQU   *
         MVC   BUFF+1(1),CTL           Set segment control
         BAL   R14,PUTBYTES            Write the netdata
         TM    CTL,X'40'               Did we process the final seg?
         BO    CPY020                  Yes, time for a new record
         MVI   CTL,X'00'               Clear segment ctl
         B     CPY060                  Go get another
*
MVCREC   MVC   BUFF+2(0),0(R2)         executed instr
*
EOD000   EQU   *
         LA    R3,BUFF                 -> build buffer
         MVI   0(R3),8                 Init control record length
         MVI   1(R3),X'E0'             Indicate 1 segment, ctl record
         MVC   2(6,R3),INMR06          Create INMR06
*
         LA    R0,8                    Write the INMR06 record
         BAL   R14,PUTBYTES
*
         XC    BUFF,BUFF
         L     R1,PBREM                Get # bytes remaining in REC
         LA    R0,1(,R1)               +1 more to force record write
         BAL   R14,PUTBYTES            Write a last full record
         DROP  R4                      IHADCB
         B     XITNET00                NETDATA build complete
*
*
*
*-- NETDATA text unit key build routines
*
*
         USING INMFIELD,R7
B1LRECL  EQU   *
         MVC   0(2,R3),INMLRECL        Set key
         MVC   2(2,R3),=Y(1)           Set #
         MVC   4(2,R3),=Y(4)           Set length
         MVC   6(4,R3),=A(80)          LRECL always 80 for INMR01
         LA    R3,10(,R3)              -> next available byte
         BR    R14                     Return
*
B1FNODE  EQU   *
         MVC   0(2,R3),INMFNODE        Set key
         MVC   2(2,R3),=Y(1)           Set #
         MVC   6(8,R3),LCLNODE         Use local node name
         LA    R1,6+8(,R3)             If TRT fails
         TRT   6(8,R3),BLANK           Look for end of name
         LA    R2,6(,R3)               -> start of name
         SR    R1,R2                   Compute length of name
         STCM  R1,3,4(R3)              Set length of name
         LA    R3,6(R1,R3)             -> next available byte
         BR    R14                     Return
*
B1FUID   EQU   *
         MVC   0(2,R3),INMFUID         Set key
         MVC   2(2,R3),=Y(1)           Set #
         MVC   6(8,R3),USERID          Use userid
         LA    R1,6+8(,R3)             If TRT fails
         TRT   6(8,R3),BLANK           Look for end of name
         LA    R2,6(,R3)               -> start of name
         SR    R1,R2                   Compute length of name
         STCM  R1,3,4(R3)              Set length of name
         LA    R3,6(R1,R3)             -> next available byte
         BR    R14                     Return
*
B1TNODE  EQU   *
         MVC   0(2,R3),INMTNODE        Set key
         MVC   2(2,R3),=Y(1)           Set #
         MVC   6(8,R3),DESTNODE        Use destination node name
         LA    R1,6+8(,R3)             If TRT fails
         TRT   6(8,R3),BLANK           Look for end of name
         LA    R2,6(,R3)               -> start of name
         SR    R1,R2                   Compute length of name
         STCM  R1,3,4(R3)              Set length of name
         LA    R3,6(R1,R3)             -> next available byte
         BR    R14                     Return
*
B1TUID   EQU   *
         MVC   0(2,R3),INMTUID         Set key
         MVC   2(2,R3),=Y(1)           Set #
         MVC   6(8,R3),DESTUSER        Use destination userid
         LA    R1,6+8(,R3)             If TRT fails
         TRT   6(8,R3),BLANK           Look for end of name
         LA    R2,6(,R3)               -> start of name
         SR    R1,R2                   Compute length of name
         STCM  R1,3,4(R3)              Set length of name
         LA    R3,6(R1,R3)             -> next available byte
         BR    R14                     Return
*
B1FTIME  EQU   *
         MVC   0(2,R3),INMFTIME        Set key
         MVC   2(2,R3),=Y(1)           Set #
         MVC   4(2,R3),=Y(16)          Set length
         TIME  DEC                     Get the date and time
*                                       R1 = 0yyydddF
*                                       R0 = hhmmssth
         LR    R2,R1                   Copy the date info
         SRL   R2,12                   Put year here: 0000yyyx
         ST    R2,DBLE                 Plug into work area
         OI    DBLE+3,X'0F'            Insert sign
         AP    DBLE(4),=P'1900'        Add base century
         UNPK  6(4,R3),DBLE(4)         Unpk the year
         OI    9(R3),X'F0'             Fix sign
         DP    DBLE(4),=P'4'           Check for leap year
         LA    R15,LEAP                Assume leap year
         CP    DBLE+3(1),=P'0'         Did it divide evenly?
         BE    B1FTME10                Yes, it is a leap year
         LA    R15,NONLEAP             Use non leap year table
*
B1FTME10 EQU   *
         N     R1,=X'0000FFFF'         Keep only the day and sign
         ST    R1,DBLE                 Save into work area
         LA    R2,1                    Init month counter
*
B1FTME20 EQU   *
         CP    DBLE(4),0(2,R15)        Check against days table
         BNH   B1FTME30                Found the right month
         LA    R15,2(,R15)             -> next days entry
         LA    R2,1(,R2)               Next month number
         B     B1FTME20                Continue
*
B1FTME30 EQU   *
         C     R2,=F'1'                Was it found in month 1?
         BE    B1FTME40                Yes, use day as is
         BCTR  R15,0                   Back up
         BCTR  R15,0                    to prior month's entry
         SP    DBLE(4),0(2,R15)        Compute the day number
*
B1FTME40 EQU   *
         UNPK  12(2,R3),DBLE(4)        unpk day number
         OI    13(R3),X'F0'            Fix sign
         CVD   R2,DBLE                 Convert month number
         UNPK  10(2,R3),DBLE           unpk month number
         OI    11(R3),X'F0'            Fix sign
*
         ST    R0,DBLE                 Save the time value
         UNPK  TWRK(9),DBLE(5)         Add zones
         MVC   14(8,R3),TWRK           Mov the time HHMMSSTH
*
         LA    R3,22(,R3)              -> next available byte
         BR    R14                     Return
*
B1NUMF   EQU   *
         MVC   0(2,R3),INMNUMF         Set key
         MVC   2(2,R3),=Y(1)           Set #
         MVC   4(2,R3),=Y(1)           Set length
         MVI   6(R3),1                 Only 1 file supported
         LA    R3,7(,R3)               -> next available byte
         BR    R14                     Return
*
B2UTIL   EQU   *
         MVC   0(2,R3),INMUTILN        Set key
         MVC   2(2,R3),=Y(1)           Set #
         MVC   6(8,R3),UTLNAME         Use utility name
         LA    R1,6+8(,R3)             If TRT fails
         TRT   6(8,R3),BLANK           Look for end of name
         LA    R2,6(,R3)               -> start of name
         SR    R1,R2                   Compute length of name
         STCM  R1,3,4(R3)              Set length of name
         LA    R3,6(R1,R3)             -> next available byte
         BR    R14                     Return
*
B2FSIZE  EQU   *
         MVC   0(2,R3),INMSIZE         Set key
         MVC   2(2,R3),=Y(1)           Set #
         MVC   4(2,R3),=Y(4)           Set length
         MVC   6(4,R3),FILESIZE        File size in bytes
         LA    R3,10(,R3)              -> next available byte
         BR    R14                     Return
*
B2DIRBLK EQU   *
         ICM   R0,15,DIRBLKS           Get # of dir blocks needed
         BZR   R14                     This key is not needed
         MVC   0(2,R3),INMDIR          Set key
         MVC   2(2,R3),=Y(1)           Set #
         MVC   4(2,R3),=Y(3)           Set length
         STCM  R0,7,6(R3)              Set directory blocks
         LA    R3,9(,R3)               -> next available byte
         BR    R14                     Return
*
B2LRECL  EQU   *
         MVC   0(2,R3),INMLRECL        Set key
         MVC   2(2,R3),=Y(1)           Set #
         MVC   4(2,R3),=Y(4)           Set length
         MVC   6(4,R3),LRECL           Set LRECL
         LA    R3,10(,R3)              -> next available byte
         BR    R14                     Return
*
B2DSORG  EQU   *
         MVC   0(2,R3),INMDSORG        Set key
         MVC   2(2,R3),=Y(1)           Set #
         MVC   4(2,R3),=Y(2)           Set length
         MVC   6(2,R3),DSORG           Set DSORG
         LA    R3,8(,R3)               -> next available byte
         BR    R14                     Return
*
B2BLKSI  EQU   *
         ICM   R0,15,BLKSIZE           Get block size
         BZR   R14                     This key is not needed
         MVC   0(2,R3),INMBLKSZ        Set key
         MVC   2(2,R3),=Y(1)           Set #
         MVC   4(2,R3),=Y(4)           Set length
         STCM  R0,15,6(R3)             Set blocksize
         LA    R3,10(,R3)              -> next available byte
         BR    R14                     Return
*
B2RECFM  EQU   *
         TM    RECFM,X'40'             Variable (or U) records?
         BZ    *+8                     No
         MVI   RECFM+1,X'02'           Y, indicate varying w/o RDW fmt
*
         MVC   0(2,R3),INMRECFM        Set key
         MVC   2(2,R3),=Y(1)           Set #
         MVC   4(2,R3),=Y(2)           Set length
         MVC   6(2,R3),RECFM           Set RECFM
         LA    R3,8(,R3)               -> next available byte
         BR    R14                     Return
*
B2DSN    EQU   *
         CLI   DSNAME,X'00'            DSNAME field filled?
         BER   R14                     Exit if no DSNAME avail
         MVC   0(2,R3),INMDSNAM        Set key
         LA    R1,DSNAME+44            In case TRT fails
         TRT   DSNAME,BLANK            Find end of DSNAME
         LA    R2,DSNAME               -> start
         SR    R1,R2                   Compute DSN length
         LR    R0,R1                   Keep length in R0
         LA    R1,1                    Set # qualifiers to start
         LA    R4,4(,R3)               -> where 1st length fld goes
*
B2DSN010 EQU   *
         LA    R5,2(,R4)               -> DSN qualifier goes
         SR    R6,R6                   Init qualifier length
*
B2DSN020 EQU   *
         CLI   0(R2),C'.'              Look for qualification delim
         BNE   B2DSN040                No, just a regular character
         STCM  R6,3,0(R4)              Fill in length field
         LA    R4,2(R6,R4)             -> next length field area
         LA    R1,1(,R1)               Bump qualifier count
         LA    R2,1(,R2)               -> next DSN character (skip '.')
         BCT   R0,B2DSN010             Keep building
ABEND106 ABEND 106,DUMP                Shouldn't happen
*
B2DSN040 EQU   *
         MVC   0(1,R5),0(R2)           Move a DSN char
         LA    R5,1(,R5)               Next available byte in BUFF
         LA    R6,1(,R6)               Count qualifier length
         LA    R2,1(,R2)               -> next DSN character
         BCT   R0,B2DSN020             Keep building
*
         STCM  R1,3,2(R3)              Set the # field (# qualifiers)
         STCM  R6,3,0(R4)              Fill in length field
         LA    R3,2(R6,R4)             -> next length field area
         BR    R14                     Return
*
B3FSIZE  EQU   *
         MVC   0(2,R3),INMSIZE         Set key
         MVC   2(2,R3),=Y(1)           Set #
         MVC   4(2,R3),=Y(4)           Set length
         MVC   6(4,R3),FILESIZE        File size in bytes
         LA    R3,10(,R3)              -> next available byte
         BR    R14                     Return
*
B3DSORG  EQU   *
         MVC   0(2,R3),INMDSORG        Set key
         MVC   2(2,R3),=Y(1)           Set #
         MVC   4(2,R3),=Y(2)           Set length
         MVC   6(2,R3),=X'4000'        Set DSORG to PS in INMR03
         LA    R3,8(,R3)               -> next available byte
         BR    R14                     Return
*
B3LRECL  EQU   *
         MVC   0(2,R3),INMLRECL        Set key
         MVC   2(2,R3),=Y(1)           Set #
         MVC   4(2,R3),=Y(4)           Set length
         MVC   6(4,R3),=A(80)          LRECL always 80 for INMR03
         LA    R3,10(,R3)              -> next available byte
         BR    R14                     Return
*
B3RECFM  EQU   *
         MVC   0(2,R3),INMRECFM        Set key
         MVC   2(2,R3),=Y(1)           Set #
         MVC   4(2,R3),=Y(2)           Set length
         MVC   6(2,R3),=X'0001'        Shortened transmission var fmt
         LA    R3,8(,R3)               -> next available byte
         BR    R14                     Return
*
*
*-- Request some more bytes of NETDATA formatted data
*
*-- Entry:  R0 = # of bytes to write   (1-255)
*--         BUFF contains the data
*
*-- Exit:   None
*
*-- Uses R0-R1,R5-R8,R14-R15; the caller's values in these
*--      registers are not preserved across this call.
*
PUTBYTES EQU   *
         ST    R14,SV14PB              Save return addr
         L     R5,PBREM                Get # bytes remaining in rec buf
         LA    R1,BUFF                 Point to putbytes (PB) buffer
         ST    R1,PBPOS                Set starting position
*
         LR    R8,R0                   Requested amount to R8
*
*
PB010    EQU   *
         LTR   R5,R5                   Any bytes left in phys record?
         BP    PB040                   Yes, use them first
*
         BAL   R14,PUT000              Write the record
         LTR   R15,R15                 Any errors?
         BNZ   XITNET08                Exit if yes
*
         LA    R5,80                   Reset record to 80 remaining
         LA    R1,REC                  -> physical record
         ST    R1,PBRPS                Reset start of record position
*
PB040    EQU   *
         LR    R7,R8                   Assume requested amt avail
         LR    R15,R8                  Same
*
         CR    R5,R8                   Have more than we need?
         BH    PB050                   Yes, just move requested
         LR    R7,R5                   Else move only what we have avai
         LR    R15,R5                  Same
*
PB050    EQU   *
         LR    R0,R7                   Save copy of length to move
         L     R14,PBPOS               -> PB buffer position
         L     R6,PBRPS                -> output record curr position
         MVCL  R6,R14                  Move
*
         ST    R14,PBPOS               New PB position
         ST    R6,PBRPS                New phys record curr position
*
         SR    R5,R0                   Reduce bytes left in phy record
         SR    R8,R0                   Reduce requested amt
         BP    PB010                   We need more, go get it
*
         ST    R5,PBREM                Remember whats left in phy rec
*
         L     R14,SV14PB              Load  return addr
         BR    R14                     Return from getbytes
*
*
*
*-- Exits from NJENET
*
XITNET00 EQU   *
         SR    R15,R15
         B     XITNET
*
XITNET08 EQU   *
         LA    R15,8                   I/O writing records
         B     XITNET
*
XITNET   EQU   *
         L     R13,4(,R13)             -> prev s.a.
         ST    R15,16(,R13)            Set RC
         LM    R14,R12,12(R13)         Reload callers regs
         BR    R14                     Return with RC
*
*
         LTORG
*
NONLEAP  DC    PL2'31,59,90,120,151,181,212,243,273,304,334,365'
LEAP     DC    PL2'31,60,91,121,152,182,213,244,274,305,335,366'
*
*-- Find INMR01 record
*                                                                       NET02190
*                                                                       NET02190
*- Control records that we look for and process (others ignored).       NET02190
INMR01   DC    C'INMR01'               Header Control record            NET02200
INMR02   DC    C'INMR02'               File Utility Control record      NET02210
INMR03   DC    C'INMR03'               Data Control record              NET02210
INMR06   DC    C'INMR06'               Trailer Control record           NET02210
*                                                                       NET02220
*- Keys we are supporting                                               NET02230
INMKEYS  DS    0H
INMBLKSZ DC    X'0030'                 Block size
INMDIR   DC    X'000C'                 Number of directory blocks
INMDSNAM DC    X'0002'                 Name of the file
INMDSORG DC    X'003C'                 File organization
INMFNODE DC    X'1011'                 Origin node name or node number
INMFTIME DC    X'1024'                 Origin timestamp
INMFUID  DC    X'1012'                 Origin user ID
INMLRECL DC    X'0042'                 Logical record length
INMRECFM DC    X'0049'                 Record format
INMSIZE  DC    X'102C'                 File size in bytes
INMTNODE DC    X'1001'                 Target node name or node number
INMTUID  DC    X'1002'                 Target user ID
INMUTILN DC    X'1028'                 Name of utility program
INMNUMF  DC    X'102F'                 Number of files transmitted = 1
         DC    X'FFFF'                 End of table
*                                                                       NET02220
*- Keys we are NOT supporting; for reference                            NET02230
INMCREAT EQU   X'1022'                 Creation date
INMDDNAM EQU   X'0001'                 DDNAME for the file
INMEATTR EQU   X'8028'                 Extended attribute status
INMERRCD EQU   X'1027'                 RECEIVE command error code
INMEXPDT EQU   X'0022'                 Expiration date
INMFACK  EQU   X'1026'                 Originator requested notificat'n
INMFFM   EQU   X'102D'                 Filemode number
INMFVERS EQU   X'1023'                 Origin version num of the data
INMLCHG  EQU   X'1021'                 Date last changed
INMLREF  EQU   X'1020'                 Date last referenced
INMLSIZE EQU   X'8018'                 Data set size in megabytes.
INMMEMBR EQU   X'0003'                 Member name list
INMRECCT EQU   X'102A'                 Transmitted record count
INMSECND EQU   X'000B'                 Secondary space quantity
INMTERM  EQU   X'0028'                 Data transmitted as a message
INMTYPE  EQU   X'8012'                 Data set type
INMTTIME EQU   X'1025'                 Destination timestamp
INMUSERP EQU   X'1029'                 User parameter string
*
*-- Target fields from INMRxx control records that we issue:
*
*
*                       INMR0x    R=required to be sent
*                       1 2 3 6   X=may optionally be sent
INMFIELD DSECT          - - - -
UTLNAME  DS    CL8        R           Utility name                      NET02490
FNODE    DS    CL8      R             Origin node                       NET02580
FUSER    DS    CL8      R             Origin userid                     NET02580
TNODE    DS    CL8      R             Dest node                         NET02580
TUSER    DS    CL8      R             Dest userid                       NET02580
FILESIZE DS    XL4        R R         File size in bytes                NET02500
DIRBLKS  DS    XL4        X           #directory blocks                 NET02500
BLKSIZE  DS    XL4        X           BLKSIZE                           NET02510
LRECL    DS    XL4      R R R         LRECL                             NET02520
DSORG    DS    XL2        R R         DSORG                             NET02540
RECFM    DS    XL2        R R         RECFM                             NET02530
DSNAME   DS    CL44       X           DSNAME                            NET02580
FTIME    DS    CL20     R             Origin time stamp                 NET02580
         DS    0F                     Force to halfword size
INMFSZ   EQU   *-INMFIELD             Size of DSECT
*
*                                                                       NJE00250
*********************
*  N J E P A R      *               NJEPAR calls IKJPARS to parse
*                   *               the TSO command line parameters.
*  TSO Command Line *
*  Parse            *
*                   *
*********************
*
*  Entry:  None.
*
*
*  Exit:   R15 = IKJPARS RC
*
NJEPAR   CSECT
         B     28(,R15)               BRANCH AROUND EYECATCHERS
         DC    AL1(23)                LENGTH OF EYECATCHERS
         DC    CL9'NJEPAR'
         DC    CL9'&SYSDATE'
         DC    CL5'&SYSTIME'
*
         STM   R14,R12,12(R13)         Save Regs                        NJE00050
         LR    R12,R15                 Base                             NJE00060
         USING NJEPAR,R12                                               NJE00070
         USING NJEWK,R10
         ST    R13,PARSA+4             SAVE prv S.A. ADDR               NJE00080
         LA    R2,PARSA                -> my save area
         ST    R2,8(,R13)              Plug it into prior SA
         LR    R13,R2
*
*
         LR    R7,R0                   Copy entry action code
         LR    R6,R1                   Copy any passed ptr
*
*-- Identify and parse out the nodeid.userid if present
*
NOD000   EQU   *
         L     R1,CPARMS               -> CPPL entry parms
         L     R2,0(,R1)               -> Command buffer
         LH    R3,0(,R2)               Get length of command buffer
         C     R3,=F'257'              Is buffer length within 256?
         BL    NOD010                  Yes
         LA    R3,256                  Set to max of 256
*
NOD010   EQU   *
         LR    R4,R3                   Copy final length
         ICM   R3,8,BLANKS             Set pad character
         LA    R0,BUFF                 -> internal 256 byte work buffer
         LA    R1,256                  Max length
         MVCL  R0,R2                   Move CBUF to our stg area
*
         STH   R4,BUFF                 Set adjusted buffer length
         MVC   REC,BLANKS              Use as temporary TRT over-
         MVC   LIST,BLANKS              flow areas
*
NOD020   EQU   *
         SR    R1,R1                   Clear
         LA    R3,BUFF+4               -> copy of cmd buffer (past RDW)
         AH    R3,BUFF+2               -> first parameter
         SH    R4,BUFF+2               Reduce remaining length
         S     R4,=F'4'                Back out length of buffer RDW
         BZ    XITPAR04                No parameters were entered
*
         EX    R4,TRTBLK               Look for end of first param
         BZ    PARS000                 Something wrong, give to parse
         LR    R5,R1                   Save end of param addr
         SR    R1,R3                   Compute length we traversed
         LR    R6,R1                   Save copy of length to R6
         C     R1,=F'3'                Length < 3?
         BL    PARS000                 Can't be node.user
         C     R1,=F'17'               Length > 17?
         BH    PARS000                 Can't be node.user
*
         BCTR  R1,0                    Adjust for execute
         EX    R1,TRTNAN               Look for any non-alphanumeric
         BNZ   PARS000                 Found something, not node.user
*
         EX    R6,TRTDLM               Look for '.' delimiter
         BZ    PARS000                 Didn't find it, not node.user
         LA    R4,1(,R1)               Save addr of userid start
         SR    R1,R3                   Compute length from start to dot
         BZ    PARS000                 Not valid node name
         C     R1,=F'8'                More than 8 char in node name?
         BH    XITPAR08                Not valid node name
         MVC   DESTNODE,BLANKS         Init receiving field
         BCTR  R1,0                    Adjust for execute
         EX    R1,OCNODE               Save off node name and uppercase
         LA    R1,1(,R1)               Restore length
         SR    R6,R1                   Reduce length by node name size
         BCTR  R6,0                    Reduce length of '.'
         LTR   R6,R6                   is len=0? (. in last character)
         BZ    XITPAR08                Not valid node.user combo
         C     R6,=F'8'                Userid > 8?
         BH    XITPAR08                Not valid user name
         MVC   DESTUSER,BLANKS         Init receiving field
         BCTR  R6,0                    Adjust for execute
         EX    R6,OCUSER               Save off user name and uppercase
*
         SR    R5,R3                   Compute area size of node.user
         BCTR  R5,0                    Adjust for execute
         EX    R5,MVCREM               Remove node.user from cmd buffer
         OI    FLAGS3,F3DEST           Valid node.user specified
         B     PARS000                 Turn the rest over to parser
*
MVCREM   MVC   0(0,R3),BLANKS          executed instr
OCNODE   OC    DESTNODE(0),0(R3)       executed instr
OCUSER   OC    DESTUSER(0),0(R4)       executed instr
TRTBLK   TRT   0(0,R3),BLANK           executed instr
TRTDLM   TRT   0(0,R3),DOTS            executed instr
TRTNAN   TRT   0(0,R3),NONALNUM        executed instr
*
*
PARS000  EQU   *
         L     R1,CPARMS               -> CPPL entry parms
         LM    R2,R5,0(R1)             Get TSO command entry parameters
*                                       R2 -> Command buffer
*                                       R3 -> UPT
*                                       R4 -> PSCB
*                                       R5 -> ECT
*
         LA    R8,PPLSTG               -> PPL
         USING PPL,R8
         ST    R3,PPLUPT               Set UPT addr
         ST    R5,PPLECT               Set ECT addr
         LA    R3,PARSECB              -> parse ECB
         ST    R3,PPLECB               Set it
         LA    R3,ANSWER               -> IKJPARS "answer area"
         ST    R3,PPLANS               Set it
         ST    R10,PPLUWA              Set user work area addr
*
*                                   ** Process command line
         LA    R2,BUFF                 -> local copy of TSO cmd buff
         ST    R2,PPLCBUF              Set TSO command buffer addr
         L     R3,=A(PCLDEFS)          -> command parms definitions
         ST    R3,PPLPCL               Set it
         B     PARS020
*
PARS020  EQU   *
         CALLTSSR EP=IKJPARS,MF=(E,PPLSTG)   Parse command line
         LTR   R0,R15                  Any parse errors?
         BNZ   XITPAR12                Yes
         DROP  R8                      PPL
*
*- Examine command line results
         L     R4,ANSWER               -> IKJPARS built PCEs
         USING PRDSECT,R4
*
PARS030  EQU   *
         LA    R2,QTPCE                -> QUIET PCE
         CLC   0(2,R2),=AL2(1)         Was QUIET specified?
         BNE   PARS035                 No
         OI    FLAGS3,F3QUIET          Indicate QUIET
*
PARS035  EQU   *
         LA    R2,PDSPCE               -> PDS/SEQL PCE
         CLC   0(2,R2),=AL2(2)         Was PDS specified?
         BNE   PARS040                 No
         OI    FLAGS3,F3PDS            Indic PDS copy and not SEQL copy
*
PARS040  EQU   *
         LA    R2,VOLPCE               -> VOLSER PCE
         TM    6(R2),X'80'             Was VOLSER specified?
         BZ    PARS050                 No
         L     R3,0(,R2)               -> VOLSER string
         LH    R1,4(,R2)               Length of volser
         MVC   OUTVOL,BLANKS           Init receiving field
         BCTR  R1,0                    Adjust for execute
         EX    R1,MVVOL                Move the volser
         OI    FLAGS3,F3VOLSER         Indicate volser valid
*
PARS050  EQU   *
         LA    R2,OTDAPCE              -> OUTDATASET PCE
         TM    6(R2),X'80'             Was OUTDATASET specified?
         BZ    PARS080                 No
         MVC   OUTPUTDS,BLANKS         Init receiving field
         LA    R5,OUTPUTDS             -> where to place DSN
*
         TM    6(R2),X'40'             Was dataset name in quotes?
         BO    PARS060                 Y, don't insert prefix
         CLC   PREFIX,BLANKS           Is a prefix available?
         BE    PARS060                 All blank, dont use prefix
*
         MVC   OUTPUTDS(8),PREFIX      Add the prefix
         TRT   OUTPUTDS,BLANK          Look for end of prefix
         MVI   0(R1),C'.'              Set delim after prefix
         LA    R5,1(,R1)               -> place to put rest of dsn
         LA    R2,OTDAPCE              -> OUTDATASET PCE
*
PARS060  EQU   *
         L     R3,0(,R2)               -> OUTDATASET string
         LH    R1,4(,R2)               Length of DSN
         BCTR  R1,0                    Adjust for execute
         EX    R1,MVDS                 Move the DSN
         OI    FLAGS3,F3OUTDS          Indicate OUTDATASET valid
*
PARS070  EQU   *
         TM    14(R2),X'80'            Was OUTDATASET member specified?
         BZ    PARS080                 No
         L     R3,8(,R2)               -> OUTDATASET member name
         LH    R1,12(,R2)              Length of member name
         MVC   OUTMEM,BLANKS           Init receiving field
         BCTR  R1,0                    Adjust for execute
         EX    R1,MVOUTMEM             Move the member name
         OI    FLAGS3,F3OUTMEM         Indicate OUTDATASET member valid
*
PARS080  EQU   *
         LA    R2,FDAPCE                -> DATASET PCE
         TM    6(R2),X'80'             Was DATASET specified?
         BZ    PARS130                 No                          v200
         MVC   INPUTDS,BLANKS          Init receiving field
         LA    R5,INPUTDS              -> where to place DSN
*
         TM    6(R2),X'40'             Was dataset name in quotes?
         BO    PARS090                 Y, don't insert prefix
         CLC   PREFIX,BLANKS           Is a prefix available?
         BE    PARS090                 All blank, dont use prefix
*
         MVC   INPUTDS(8),PREFIX       Add the prefix
         TRT   INPUTDS,BLANK           Look for end of prefix
         MVI   0(R1),C'.'              Set delim after prefix
         LA    R5,1(,R1)               -> place to put rest of dsn
         LA    R2,FDAPCE               -> DATASET PCE
*
PARS090  EQU   *
         L     R3,0(,R2)               -> DATASET string
         LH    R1,4(,R2)               Length of DSN
         BCTR  R1,0                    Adjust for execute
         EX    R1,MVDS                 Move the DSN
         OI    FLAGS3,F3INDS           Indicate DATASET valid
*
PARS100  EQU   *
         TM    14(R2),X'80'            Was DATASET member specified?
         BZ    PARS120                 No
         L     R3,8(,R2)               -> DATASET member name
         LH    R1,12(,R2)              Length of member name
         MVC   INMEM,BLANKS            Init receiving field
         BCTR  R1,0                    Adjust for execute
         EX    R1,MVINMEM              Move the member name
         OI    FLAGS3,F3INMEM          Indicate SEQL MEMBER specified
         B     PARS130                 We're done                  v200
*
PARS120  EQU   *
         NI    FLAGS3,255-F3PDS        Turn off;we'll do what DSORG say
*
PARS130  EQU   *                                                   v200
         LA    R2,UNIPCE               -> UNIT PCE                 v200
         TM    6(R2),X'80'             Was UNIT specified?         v200
         BZ    PARS190                 No                          v200
         L     R3,0(,R2)               -> UNIT string              v200
         LH    R1,4(,R2)               Length of unit name         v200
         MVC   OUTUNIT,BLANKS          Init receiving field        v200
         BCTR  R1,0                    Adjust for execute          v200
         EX    R1,MVUNIT               Move the unit               v200
         OI    FLAGS2,F2UNIT           Indicate unit valid         v200
*
PARS190  EQU   *
         B     XITPAR00                All done
         DROP  R4                      PRDSECT
*
MVVOL    MVC   OUTVOL(0),0(R3)         executed instr
MVDS     MVC   0(0,R5),0(R3)           executed instr
MVINMEM  MVC   INMEM(0),0(R3)          executed instr
MVOUTMEM MVC   OUTMEM(0),0(R3)         executed instr
MVUNIT   MVC   OUTUNIT(0),0(R3)        executed instr              v200
*
*
*-- Exit
*
XITPAR00 EQU   *
         LA    R1,ANSWER               -> IKJPARS "answer place"
         IKJRLSA (1)                   Release parsing storage
*
         SR    R0,R0               Set secondary RC=0
         SR    R15,R15             Set RC=0;
         B     XITPAR
*
XITPAR04 EQU   *
         SR    R0,R0               Set secondary RC=0
         LA    R15,4               Set RC=4; no parameters entered
         B     XITPAR
*
XITPAR08 EQU   *
         SR    R0,R0               Set secondary RC=0
         LA    R15,8               Set RC=8; invalid node.user combo
         B     XITPAR
*
XITPAR12 EQU   *
         LA    R15,12              Set RC=12; R0 already set by IKJPARS
         B     XITPAR
*
XITPAR   EQU   *
         L     R13,4(,R13)         -> prev s.a.
         L     R14,12(,R13)        Load r14
         LM    R1,R12,24(R13)      Reload callers regs
         BR    R14                 Return with RCs in R0/R15
*
         LTORG
*
*-- IKJPARS Description Macros
*
*-- TRANSMIT command parms:
*
*    TRANSMIT  node.userid DATASET(ddd) OUTDATASET(ooo) VOLSER(vvvvv)
*                          PDS | SEQUENTIAL
*                          QUIET
*
*   Where:
*
*     node.user is the node and userid destination for the file.
*     ddd is the dataset(+member) to be transmitted.
*     ooo is the optional output dataset to write the NETDATA encoded
*         transmission into in lieu of actually sending it.
*     vvv is an optional VOLSER of where to allocate the OUTDATASET.
*
*
PCLDEFS  IKJPARM DSECT=PRDSECT
*
*
QTPCE    IKJKEYWD
         IKJNAME  QUIET            PCE value = 1
*
PDSPCE   IKJKEYWD DEFAULT='SEQUENTIAL'
         IKJNAME  SEQUENTIAL       PCE value = 1
         IKJNAME  PDS              PCE value = 2
*
OTDSPCE  IKJKEYWD
         IKJNAME  'OUTDATASET',SUBFLD=OTDSFLD,ALIAS='OUTDSNAME'
*
FDSPCE   IKJKEYWD
         IKJNAME  'DATASET',SUBFLD=FDSFLD,ALIAS='DSNAME'
*
VSRPCE   IKJKEYWD
         IKJNAME  'VOLSER',SUBFLD=VOLSFLD,ALIAS='VOLUME'
*
USRPCE   IKJKEYWD ,                                                v200
         IKJNAME  'UNIT',SUBFLD=UNISFLD,ALIAS=('U')                v200
*
OTDSFLD  IKJSUBF
OTDAPCE  IKJPOSIT DSNAME,                                              x
               PROMPT='THE NAME OF THE DATA SET YOU WANT TO CONTAIN THEx
                ENCODED FILE'
*
FDSFLD   IKJSUBF
FDAPCE   IKJPOSIT DSNAME,                                              x
               PROMPT='THE NAME OF THE DATA SET YOU WANT TO TRANSMIT'
*
VOLSFLD  IKJSUBF
VOLPCE   IKJPOSIT DSTHING,VOLSER,                                      x
               PROMPT='THE VOLUME SERIAL OF THE VOLUME WHERE YOU WANT Tx
               HE OUTDATASET ALLOCATED'
*
UNISFLD  IKJSUBF ,                                                 v200
UNIPCE   IKJIDENT 'UNIT NAME',MAXLNTH=8,FIRST=ALPHANUM,            v200x
               OTHER=ALPHANUM                                      v200
*
         IKJENDP
*
*
*
         IKJPPL
IKJPPLSZ EQU   (*-PPL)/4           # words in PPL
*
         LTORG
*
*                                                                     *
***********************************************************************
**                                                                   **
**                        TASK ESTAE EXIT                            **
**                                                                   **
** This csect handles all abends trapped by ESTAE during the normal  **
** execution of the subtask.          This exit does not attempt     **
** any recovery other than to terminate processing.                  **
** An SVC dump is taken on abends.                                   **
**                                                                   **
** On entry:  R0=ESTAE provide entry code                            **
**            R1=SDWA address                                        **
**            R2=parameter passed on ESTAE macro                     **
**                                                                   **
**                                                                   **
** On exit: If SDWACLUP is 1, then no retry is allowed and this      **
**             exit will allow percolation back to system routines   **
**             to terminate the task.                                **
**                                                                   **
**          If SDWACLUP is 0, then retry is allowed.                 **
**                                                                   **
** Security:  N/A.                                                   **
**                                                                   **
** Register usage:                                                   **
**                                                                   **
**   R1  = SDWA address                                              **
**   R3  = SDWA address                                              **
**   R10 = Dynamic storage area base                                 **
**   R12 = This program base                                         **
**                                                                   **
**                                                                   **
**                                                                   **
***********************************************************************
*
NJEDMP   CSECT
         B     28(,R15)               BRANCH AROUND EYECATCHERS
         DC    AL1(23)                LENGTH OF EYECATCHERS
         DC    CL9'NJEDMP'
         DC    CL9'&SYSDATE'
         DC    CL5'&SYSTIME'
*
         LR    R12,R15                SET UP BASE REG
         USING NJEDMP,R12             ESTABLISH ADDRESSABILITY
         LR    R8,R14                 SAVE RETURN ADDRESS TO SYSTEM
*
         L     R10,0(,R1)             GET VALUE PASSED TO US (WORKA)
         USING NJEWK,R10
         L     R11,=A(NJECOM)          -> common csect
         USING NJECOM,R11
*
         LR    R3,R1                  SAVE R1 ENTRY CONTENTS
         USING SDWA,R3
         LR    R5,R0                  Save R0 entry code
*
         LTR   R3,R3                  Do we have an SDWA?
         BZ    NOSDWA                 Exit if no SDWA
         LA    R13,MVSSAVE            Save area
         ESTAE 0
*
         MODESET MODE=SUP,            Run this ESTAI exit privileged   x
               KEY=ZERO                to access PSW -> storage
*
         MVC   MACLIST(WTOMSGL),WTOMSG
         L     R6,PSATOLD-PSA(0)      -> my TCB
         L     R5,TCBTIO-TCB(,R6)     -> TIOT
         MVC   MACLIST+9(8),0(R5)         Plug in job name
         MVC   MACLIST+4(4),=C'USER'
         MVC   MACLIST+19(8),=C'TRANSMIT' Plug in command name
*
*
LNK020   EQU   *
         MVC   MACLIST+29(5),=C'ABEND'
         L     R5,SDWAABCC              GET ABEND CODE INFO WORD
         N     R5,=X'00FFF000'          KEEP ONLY THE SYSTEM CODE
         BZ    USERCDE                  NONE THERE, MUST BE A USER CODE
         SRL   R5,12                    Put sys code in low order  v201
         C     R5,=X'00000222'          Operator cancel, no dump?  v201
         BE    SDUMP040                 Yes, suppress dump
         CLM   R5,1,=X'3E'              Was it an x3E (DETACH) ?   v201
         BE    SDUMP040                 Yes, suppress dump         v201
         C     R5,=X'00000013'          Open 013 abend?            v201
         BNE   ACCPT                    no, do the dump            v200
         CLC   SDWAGR15,=X'00000018'    Was it 013-18?             v200
         BE    SDUMP060                 Yes, suppress dump         v200
*
ACCPT    EQU   *                                                   v200
         MVI   MACLIST+35,C'S'          INDICATE SYSTEM CODE
         UNPK  FWORK(5),SDWACMPC(3)     GET SYSTEM CMP CODE
         TR    FWORK(3),HEXTRAN-240
         MVC   FWORK+3(5),=CL5' '       CLEAR REST OF ABEND CODE
         B     NOREAS
*
USERCDE  EQU   *
         MVI   MACLIST+35,C'U'         INDICATE USER ABEND CODE
         L     R5,SDWAABCC             GET ABEND CODE
         N     R5,=X'00000FFF'         KEEP USER ABEND CODE
         CVD   R5,FSAVE                CONVERT CODE TO DECIMAL
         UNPK  FWORK(4),FSAVE          UNPK THE CODE
         OI    FWORK+3,X'F0'           FIX SIGN
         MVC   FWORK+4(2),=CL2' '      BLANKS AT END OF ABEND CODE
*
NOREAS   EQU   *
         MVC   MACLIST+36(6),FWORK     MOVE ABEND-REASON TO LINE
         MVC   ABCODE,MACLIST+36       Save a copy of formatted abcode
*
         WTO   ,MF=(E,MACLIST)        Write to console
         LA    R2,MACLIST
         BAL   14,PUTLINE             Echo to TSO terminal
*
         MVC   MACLIST(WTOMSGL),WTOMSG
         MVC   MACLIST+4(3),=C'PSW'
         UNPK  FSAVE(9),SDWAEC1(5)    Add zones to PSW word 1
         TR    FSAVE(8),HEXTRAN-240
         MVC   MACLIST+10(8),FSAVE
         UNPK  FSAVE(9),SDWAEC1+4(5)  Add zones to PSW word 2
         TR    FSAVE(8),HEXTRAN-240
         MVC   MACLIST+19(8),FSAVE
*
         SR    R5,R5                   CLEAR FOR IC
         IC    R5,SDWAILC1             GET THE ILC
         CVD   R5,FWORK                MAKE DECIMAL
         MVC   MACLIST+29(3),=C'ILC'
         UNPK  MACLIST+33(2),FWORK     UNPK
         OI    MACLIST+34,X'F0'        FIX THE SIGN
*
         MVC   MACLIST+37(4),=C'INTC'
         UNPK  FWORK(5),SDWAINC1(3)    MAKE INTC DISPLAYABLE
         TR    FWORK(4),HEXTRAN-240
         MVC   MACLIST+42(4),FWORK     MOVE INTC TO LINE
*
         WTO   ,MF=(E,MACLIST)
         LA    R2,MACLIST
         BAL   14,PUTLINE             Echo to TSO terminal
*
         MVC   MACLIST(WTOMSGL),WTOMSG
         MVC   MACLIST+4(13),=C'DATA NEAR PSW'
         MVC   MACLIST+19(8),=CL8'UNAVAIL'  ASSUME WE CANT GET DATA
         L     R4,SDWAEC1+4            Get PSW IA
         LA    R4,0(,R4)               Clear high bit
         C     R4,=F'8'                1st 8 bytes of storage?
         BH    LOC010                  No, its higher than that
         SR    R4,R4                   Yes, just use 0
         B     LOC020
*
LOC010   EQU   *
         S     R4,=F'8'                BACK UP BEFORE INTERRUPT ADDR
*
LOC020   EQU   *
         LRA   R0,0(,R4)               Do we have access?
         BNZ   UNAVAIL                 No translation, better not
         LRA   R0,14(,R4)              Do we have access?
         BNZ   UNAVAIL                 No translation, better not
*
         ST    R4,FWORK                SAVE FOR CONVERSION
         UNPK  FSAVE(9),FWORK(5)       ADD ZONES TO ADDRESS
         TR    FSAVE(8),HEXTRAN-240    MAKE DISPLAYABLE HEX
         MVC   MACLIST+19(8),FSAVE     MOVE DISPLAYABLE
*
         MVC   FWORK(4),0(R4)          MOVE 4 WORDS AT PSW
         UNPK  FSAVE(9),FWORK(5)       ADD ZONES
         TR    FSAVE(8),HEXTRAN-240    MAKE DISPLAYABLE HEX
         MVC   MACLIST+29(8),FSAVE     MOVE TO LINE
*
         MVC   FWORK(4),4(R4)          MOVE 4 WORDS AT PSW
         UNPK  FSAVE(9),FWORK(5)       ADD ZONES
         TR    FSAVE(8),HEXTRAN-240    MAKE DISPLAYABLE HEX
         MVC   MACLIST+38(8),FSAVE     MOVE TO LINE
*
         MVC   FWORK(4),8(R4)          MOVE 4 WORDS AT PSW
         UNPK  FSAVE(9),FWORK(5)       ADD ZONES
         TR    FSAVE(8),HEXTRAN-240    MAKE DISPLAYABLE HEX
         MVC   MACLIST+47(8),FSAVE     MOVE TO LINE
*
         MVC   FWORK(4),12(R4)         MOVE 4 WORDS AT PSW
         UNPK  FSAVE(9),FWORK(5)       ADD ZONES
         TR    FSAVE(8),HEXTRAN-240    MAKE DISPLAYABLE HEX
         MVC   MACLIST+56(8),FSAVE     MOVE TO LINE
*
UNAVAIL  EQU   *
         WTO   ,MF=(E,MACLIST)
         LA    R2,MACLIST
         BAL   14,PUTLINE              Echo to TSO terminal
*----
         LA    R4,4                    4 ROWS OF REGISTERS
         LA    R5,SDWAGR00             POINT TO ABEND REGS
         LA    R6,REGLIST              POINT TO REGISTER ID LITERALS
*
REG000   EQU   *
         MVC   MACLIST(WTOMSGL),WTOMSG
         MVC   MACLIST+4(8),0(R6)      MOVE REGISTERS ID
         LA    R15,MACLIST+13          WHERE 1ST REG GOES ON LINE
         LA    R14,4                   4 REGS PER LINE
*
REG010   EQU   *
         UNPK  FSAVE(9),0(5,R5)        UNPK A REGISTER
         TR    FSAVE(8),HEXTRAN-240    MAKE DISPLAYABLE HEX
         MVC   0(8,R15),FSAVE          MOVE TO THE LINE
         LA    R15,10(,R15)            NEXT SPOT ON PRINT LINE
         LA    R5,4(,R5)               NEXT REGISTER
         BCT   R14,REG010              KEEP DOING REGS
         WTO   ,MF=(E,MACLIST)
         LA    R2,MACLIST
         BAL   14,PUTLINE              Echo to TSO terminal
         LA    R6,8(,R6)               NEXT REGISTER ID
         BCT   R4,REG000               GO DISPLAY THE NEXT ROW
*
*
SDUMP000 EQU   *
         L     R5,SDWAABCC             Get abend code info word
         N     R5,=X'00FFF000'         Keep only the system code
         SRL   R5,12                   Right justify the code
         C     R5,=X'00000222'         Operator cancel, no dump?
         BE    SDUMP040                Yes, skip dump
         CLM   R5,1,=X'37'             x37 abend code?
         BE    SDUMP040                Skip the dump
*
         MVI   DHDR,C' '
         MVC   DHDR+1(29),DHDR
         MVI   DHDR,29                IBM length of header
         L     R5,PSATOLD-PSA(0)      -> my TCB
         L     R5,TCBTIO-TCB(,R5)     -> TIOT
         MVC   DHDR+1(8),0(R5)        Use jobname in description
         MVC   DHDR+11(8),=C'TRANSMIT' Use command name
         MVC   DHDR+21(7),ABCODE
*
         MVC   MACLIST(SDUMPL),SDUMP    MOVE SDUMP LIST TO WORK
         LA    R1,MACLIST
         SDUMP HDRAD=DHDR,              ISSUE SDUMP TO RECORD STATUS   x
               BUFFER=NO,                                              x
               QUIESCE=NO,                                             x
               SDATA=(RGN,CSA,LPA,SUM),                                x
               MF=(E,(1))
*
*
SDUMP040 EQU   *
         LR    R1,R3                  SDWA BACK TO R1
         L     R15,=A(NJETRN)         Main csect addr
         ST    R15,SDWASRSV+4*R12     Plug it to R12
         L     R15,=A(EXIT08)         -> TRANSMIT exit point
         B     SDUMP090                                            v200
*
SDUMP060 EQU   *                  **  Here for S013-18 abend only  v200
         LR    R1,R3                  SDWA BACK TO R1              v200
         L     R15,=A(NJETRN)         Main csect addr              v200
         ST    R15,SDWASRSV+4*R12     Plug it to R12               v200
         L     R15,=A(ERR015)         -> TRANSMIT ERRMSG           v200
*
SDUMP090 EQU   *
         SETRP RC=4,                  Retry - try to shut down TRANSMITx
               DUMP=NO,               Suppress any further dumps       x
               FRESDWA=YES,           Free the SDWA                    x
               RETREGS=YES,           Restore original regs            x
               RETADDR=(15)           Return to Transmit exit point
*
NOSDWA   EQU   *                  **  NO RETRY AVAILABLE (OR DESIRED)
         SR    R15,R15                REQUEST PERCOLATION
         LR    R14,R8                 RESTORE RETURN ADDRESS
         BR    R14                    RETURN TO SYSTEM
*
         LTORG
*
SDUMP    SDUMP MF=L
SDUMPL   EQU   *-SDUMP
*
REGLIST  DC    CL8'GR 0-3'
         DC    CL8'GR 4-7'
         DC    CL8'GR 8-11'
         DC    CL8'GR 12-15'
*
WTOMSG   WTO   '                                                       x
                                             ',MF=L
WTOMSGL  EQU   *-WTOMSG
*
         LTORG
*
*
****  Main work area common                                             NJE00290
****  to all NJExxx CSECTs.                                             NJE00290
*                                                                       NJE00290
NJEWK    DSECT
NJEEYE   DS    CL4'NJET'           Eyecatcher
NJEWKLEN DS    F                   Getmain size of this area
*
DBLE     DS    D                   Work area                            NJE00310
TWRK     DS    2D                  Work area
LCLNODE  DS    CL8                 Local node id
DEFUSER  DS    CL8                 Default 'no security' userid
USERID   DS    CL8                 TSO Userid
PREFIX   DS    CL8                 TSO PREFIX
DESTNODE DS    CL8                 Destination node
DESTUSER DS    CL8                 Destination userid
SPLDSN   DS    CL44                NETSPOOL dataset name
*
*
MACLIST  DS    CL96                   Macro expansion area
STAXLIST DS    CL20                   STAX parameter list
*                                                                       NET02360
CPARMS   DS    A                      -> input CPPL (entry parms)
PUTECB   DS    F                      ECB for PUTLINE
IOPLAREA DS    4A                     IOPL for PUTLINE
SV14PUT  DS    A                      R14 save area
SV14LN   DS    A                      R14 save area                     NET02370
SV14PB   DS    A                      R14 save area                     NET02370
SV14SI   DS    A                      R14 save area                     NET02370
*
PBREM    DS    F                      # bytes remaining in phys rec
PBPOS    DS    A                      -> current position in BUFF
PBRPS    DS    A                      -> current position in phys rec
OUTRECS  DS    F                      Count of output records written
*
BLOCKLEN DS    F                      Length of block buffer
BLOCK    DS    A                      -> Block of physical records
*
DEVINFO  DS    0XL20                  5 WORDS OF DEVTYPE INFO
DEVUCBTY DS    F                       DEV TYPE: VALUE OF UCBTYP FIELD
DEVMAXBK DS    F                       MAXIMUM BLKSIZE ON DEVICE
DEVCYLS  DS    XL2                     NUMBER OF CYLINDERS ON DEVICE
DEVTRKS  DS    XL2                     NUMBER OF HEADS ON DEVICE
DEVNUSED DS    0XL8                    2 WORDS NOT USED HERE
*
OLD      DS    F                      For PUTGET, # segments
OLDMSGAD DS    A                      -> msg len/text
*
PARSECB  DS    F                      IKJPARS ECB
ANSWER   DS    F                      IKJPARS Answer area
PPLSTG   DS    (IKJPPLSZ)A            Space for PPL
OUTVOL   DS    CL6                    User specified output volser
OUTPUTDS DS    CL44                   User specified OUTDATASET DSN
OUTMEM   DS    CL8                    User specified OUTDATASET member
OUTUNIT  DS    CL8                    User specified UNIT name     v200
INPUTDS  DS    CL44                   Input dataset name
INMEM    DS    CL8                    User specified input member
*
*
FLAGS1   DS    X                      Flag bits
F1INPDS  EQU   X'80'  1... ....        Input dataset is a PDS, 0=SEQL
F1ATTN   EQU   X'40'  .1.. ....        User pressed ATTN key       v201
F1BATCH  EQU   X'08'  .... 1...        Running in BATCH TSO
F1ACEE   EQU   X'04'  .... .1..        Security is available on system
F1AUSR   EQU   X'02'  .... ..1.        Special user
F1APF    EQU   X'01'  .... ...1        Authorized at invocation
*                     ..xx ....        available bits
*
FLAGS2   DS    X                      Flag bits
F2INOPN  EQU   X'80'  1... ....        INDS DCB open
F2NCBOPN EQU   X'40'  .1.. ....        NETSPOOL NCB open
F2OUTOPN EQU   X'20'  ..1. ....        OUTDS DCB open
F2NJE38  EQU   X'10'  ...1 ....        NJE38 is active (LCLNODE valid)
F2SYSOPN EQU   X'08'  .... 1...        SYSINDS DCB open
F2EXIST  EQU   X'04'  .... .1..        OUTDATASET previously existed
F2UNIT   EQU   X'02'  .... ..1.        UNIT specified              v200
*                     .... ...x        available bits
*
FLAGS3   DS    X                      Flag bits from CMD line parse
F3DEST   EQU   X'80'  1... ....        Valid node.user destination spec
F3PDS    EQU   X'40'  .1.. ....        1=PDS,0=SEQL specified
F3VOLSER EQU   X'20'  ..1. ....        VOLSER specified
F3OUTDS  EQU   X'10'  ...1 ....        OUTDATASET specified
F3OUTMEM EQU   X'08'  .... 1...        OUTDATASET MEMBER specified
F3INDS   EQU   X'04'  .... .1..        DATASET specified
F3INMEM  EQU   X'02'  .... ..1.        DATASET member specified
F3QUIET  EQU   X'01'  .... ...1        1=QUIET suppress info msgs
*
FLAGS4   DS    X                      Flag bits
*                     xxxx xxxx        available bits
*                                                                       NET02470
         DS    0F
INMF01   DS    (INMFSZ)X              Fields for INMR01 record
INMF02A  DS    (INMFSZ)X              Fields for 1st INMR02 record
INMF02B  DS    (INMFSZ)X              Fields for 2nd INMR02 record
INMF03   DS    (INMFSZ)X              Fields for INMR03 record
*                                                                       NET02590
         DS    0F
CAMWORK  DS    0XL140                 CAMLST work area
BUFF     DS    CL256                  GB buffer containing request data NET02600
LIST     DS    CL80                   Print line
REC      DS    CL133                  Physical record from spool
*
*----
LS99PTR  DS    A                       PTR TO S99RB
LS99RB   DS    XL20                    SPACE FOR S99RB
*
TXTPTRS  DS    15A                     -> Text unit ptr list
*
         DS    0H
UTXT     DS    0XL06,Y,AL2,AL2         DDNAME Unallocation
UDDNAME  DS    CL8                      DDNAME
*
         DS    0H
TXT01    DS    0XL06,Y,AL2,AL2         Return DDNAME
TDDNAME  DS    CL8                      DDNAME
*
         DS    0H
TXT02    DS    0XL06,Y,AL2,AL2         DSN=
TDSNAME  DS    CL44                     DSNAME
*
         DS    0H
TXT03    DS    0XL07,Y,AL2,AL2,X       DISP=(NEW,
*
         DS    0H
TXT04    DS    0XL07,Y,AL2,AL2,X       DISP=(,CATLG)
*
         DS    0H
TXT05    DS    0XL06,Y,AL2,AL2         SPACE BLOCK LEN
TBLKLEN  DS    XL3                      BLKLEN
*
         DS    0H
TXT06    DS    0XL06,Y,AL2,AL2         SPACE PRIMARY
TPRIME   DS    XL3                      Primary
*
         DS    0H
TXT07    DS    0XL06,Y,AL2,AL2         SPACE SECONDARY
TSECND   DS    XL3                      Secondary
*
         DS    0H
TXT08    DS    0XL06,Y,AL2,AL2         SPACE DIRECTORY BLOCKS
TDIRBLKS DS    XL3                      DIR BLKS
*
         DS    0H
TXT09    DS    0XL06,Y,AL2,AL2         VOLUME
TVOLSER  DS    CL6                      VOLSER
*
         DS    0H
TXT10    DS    0XL14,Y,AL2,AL2         UNIT                        v200
TUNIT    DS    CL8                      UNITNAME                   v200
*
         DS    0H
TXT11    DS    0XL06,Y,AL2,AL2         EXPDT
TEXPDT   DS    CL5                      EXPDT=yyddd
*
         DS    0H
TXT12    DS    0XL06,Y,AL2,AL2         BLKSIZE
TBLKSIZE DS    XL2                      BLKSIZE
*
         DS    0H
TXT13    DS    0XL06,Y,AL2,AL2         DSORG
TDSORG   DS    XL2                      DSORG
*
         DS    0H
TXT14    DS    0XL06,Y,AL2,AL2         LRECL
TLRECL   DS    XL2                      LRECL
*
         DS    0H
TXT15    DS    0XL06,Y,AL2,AL2         RECFM
TRECFM   DS    XL1                      RECFM
*
         DS    0H
TXT16    DS    0XL04,Y,AL2             DUMMY
*
         DS    0H
TXT17    DS    0XL04,Y,AL2             SYSOUT
*
         DS    0H
TXT18    DS    0XL04,Y,AL2             TERM
*
         DS    0H
TXT19    DS    0XL04,Y,AL2             CYLINDER
*
         DS    0H
TXT20    DS    0XL04,Y,AL2             FREE=CLOSE
*
         DS    0H
TXT21    DS    0XL06,Y,AL2,AL2         MEMBER
TMEMBER  DS    CL8
*---
*
CTL      DS    X                      Segment descriptor byte
*
*
         DS    0F
TAGDATA  DS    XL108                  TAG data area
TYPPRT   EQU   X'40'                   PRT dev
TYPPUN   EQU   X'80'                   PUN dev
*
NCB1     DS    XL48                   NCB for Spool Access
SYSINDS  DS    0X                     SYSIN DCB for IEBCOPY ctl cards
INDS     DS    (DMYINDSL)X            Input dataset DCB
OUTDS    DS    (DMYOUTDL)X            OUTDATASET DCB
CAMLST   DS    (DMYLSTL)X             Space to hold a CAMLST
*
CPYPLIST DS    XL(COPYPRML)            IEBCOPY PARM FIELD
*
         DS    0H
DDLISTL  DS    AL2(DDLISTSZ)           DDNAME LIST LENGTH
DDLIST   DS    4XL8'00'                    FOUR DDNAMES UNDEFINED
DDSYSIN  DS    CL8    DDNAME representing IEBCOPY's SYSIN
DDSYSPR  DS    CL8    DDNAME representing IEBCOPY's SYSPRINT
         DS    XL8'00'                     UNDEFINED DD
DDSYSUT1 DS    CL8    DDNAME of the dataset to be transmitted (SYSUT1)
DDSYSUT2 DS    CL8    DDNAME representing IEBCOPY's SYSUT2
         DS    XL8'00'                    SYSUT3 unused
DDSYSUT4 DS    CL8    DDNAME representing IEBCOPY's SYSUT4
DDLISTSZ EQU   *-DDLIST                LENGTH OF DDLIST for IEBCOPY
DDOUTDS  DS    XL8'00'                 OUTDATASET DDNAME
DDNETSPL DS    XL8'00'                 NETSPOOL DDNAME
UNLISTSZ EQU   *-DDLIST                TOTAL of all DDs in list
*
*-- ESTAE exit used areas
*
FSAVE    DS    2D
FWORK    DS    D
DHDR     DS    CL30
ABCODE   DS    CL7
MVSSAVE  DS    18F                 ESTAE exit OS save
*-- End of ESTAE area
*
*
NJESA    DS    18F                    NJERCV OS save area               NJE00300
DYNSA    DS    18F                    NJEDYN OS save area               NJE00300
NETSA    DS    18F                    NJENET OS save area               NJE00300
PARSA    DS    18F                    NJEPAR OS save area               NJE00300
*
         DS    0D                     Force doubleword size
NJEWKSZ  EQU   *-NJEWK
*                                                                       NJE00930
*
*-- System DSECTs
*
         CVT   DSECT=YES,PREFIX=NO
         IEFZB4D0
         IEFZB4D2
         DCBD  DSORG=PS,DEVD=DA
*
IEFUCBOB DSECT
         IEFUCBOB LIST=YES
         IHAPSA
         IEESMCA
         IKJTCB
         IHASDWA
IEFTIOT  DSECT
         IEFTIOT1
         IHAASCB
         IHAASXB
         IKJUPT
         IKJCPPL
         IKJPGPB
         IKJIOPL
DSCBF1   DSECT
         IECSDSL1 (1)
*
VOLLIST  DSECT                         Volume list returned by LOCATE
VOLCOUNT DS    H                       Volume count
VOLDEV   DS    CL4                     UCB dev type
VOLSER   DS    CL6                     Volser
VOLSTAT  DS    H                       Status bytes
*
ACEE     DSECT                         Maps a portion of ACEE in MVS3.8
ACEEEYE  DS    CL4'ACEE'
         DS    16X
ACEEUSRL DS    X                       Length of userid
ACEEUSR  DS    CL8                     Userid
*
         COPY  NETSPOOL                                                 NJE00940
         COPY  TAG
*
*-- NJE38 DSECTs
*
         NJEWRE                                                    v220
*
         END   NJETRN                                                   NJE01000
./ ADD NAME=NJERCV
*
*-- NJE38 - TSO RECEIVE
*
*   Command line format (all parameters are optional):
*
*   RECEIVE  filenum
*            DATASET( )
*            VOLSER( )
*            UNIT( )
*            DIR( )
*            INDATASET( )
*            PURGE  | NOPURGE
*            PROMPT | NOPROMPT
*            QUIET
*
*   where:
*
*    filenum        - specifies a specific NJE38 spool file number
*                     to be received.  If not specified, the next
*                     available spool file is received.  Ignored if
*                     INDATASET is specified.
*
*    DATASET( )     - specifies the dsname of the dataset to be
*                     created; the received data will be placed within.
*                     If not specified, the dataset name will be
*                     derived from the incoming dataset name, with
*                     the first qualifer being replaced by the
*                     receiver's TSO userid.
*
*    VOLSER( )      - specifies a volume where DATASET should be
*                     created.  If not specified, a PUBLIC volume will
*                     be chosen based on the receiving dataset's
*                     attributes.
*
*    UNIT( )        - specifies a unit name where DATASET should be
*                     created.  If not specified, SYSDA is the default
*                     unit name.
*
*    DIR( )         - specifies a number of directory blocks if
*                     incoming file was a PDSE.
*
*    INDATASET( )  -  optional. Specifies that the encoded named
*                     dataset is to be received.  The encoded dataset
*                     was previously created by TRANSMIT using
*                     OUTDATASET.  May optionally specify a membername.
*
*    PURGE          - DEFAULT.  Indicates that RECEIVE is to purge
*                     the spool file after successful retrieval. Has
*                     no meaning if INDATASET is specified.
*
*    NOPURGE        - Indicates that RECEIVE is to retain the spool
*                     file.  The file can be received again or must be
*                     removed from the spool by other means.  Has
*                     no meaning if INDATASET is specified.
*
*    PROMPT         - DEFAULT.  Indicates that RECEIVE is to prompt
*                     the TSO user to respecify DATASET or VOLSER
*                     after learning the incoming dataset name. The
*                     user can then choose to change the name or
*                     volume.
*
*    NOPROMPT       - Indicates that no prompts are to be issued.  If
*                     errors are encountered, such as the incoming
*                     dataset name already existing, then RECEIVE is
*                     terminated without any opportunity to change
*                     the parameters.
*
*    QUIET          - If specified, indicates that all informational
*                     messages from  RECEIVE are suppressed.  Error
*                     messages will always be displayed. QUIET also
*                     forces on NOPROMPT.
*
*
* Change log:
*
*
* 21 Oct 21 - Temp dataset on IEBCOPY type receive not using vol   v230
*              identified by GETVOL (instead uses hi-cuu PUBLIC)   v230
* 22 Jul 21 - Typo could cause alloc error with user coded volume  v230
* 24 Apr 21 - Use TSO userid as default user if no security and    v222
*              NJE38 is not active.                                v222
* 15 Feb 21 - Not picking up jobname when run as an STC.           v221
* 01 Oct 20 - Put ENQ existence check in common module             v210
* 09 Aug 20 - Improve TSO attention key handling                   v201
* 13 Jul 20 - Flat file with JCL sneaks by NETDATA checks, causing v200
*             loss of first record in result.                      v200
* 12 Jul 20 - Add support for DIR( ) command line parameter        v200
* 10 Jul 20 - Add support for UNIT( ) command line parameter       v200
* 08 Jul 20 - IEBCOPY failures if netdata records shorter than 80  v200
* 15 May 20 - Initial creation
*
*
         GBLC  &VERS
         REGEQU
NJERCV   CSECT                                                          NJE00020
         NJEVER
         STM   R14,R12,12(R13)         Save Regs                        NJE00050
         LR    R12,R15                 Base                             NJE00060
         USING NJERCV,R12                                               NJE00070
         LR    R8,R1                   Copy input parm addr
*
         GETMAIN RU,                   Get local stg area              X
               LV=4096,                                                X
               BNDRY=PAGE
         LR    R10,R1
         LR    R1,R0                   Copy length
         LR    R2,R0                   Copy length
         LR    R0,R10                  -> new stg area
         SR    R15,R15                 set pad
         MVCL  R0,R14                  Clear the page
*
         USING NJEWK,R10
         ST    R13,NJESA+4             SAVE prv S.A. ADDR               NJE00080
         LA    R1,NJESA                -> my save area
         ST    R1,8(,R13)              Plug it into prior SA
         LR    R13,R1
*
         MVC   NJEEYE,=CL4'NJER'       Work area eyecatcher
         ST    R2,NJEWKLEN             Save size of area in area
         L     R11,=A(NJECOM)          -> common csect
         USING NJECOM,R11
         ST    R8,CPARMS               Save ptr to input parms
         MVC   OLD,=F'1'               Set number of PUTGET segments
         OI    FLAGS3,F3PURGE          Set default: PURGE
*
INIT000  EQU   *
         MVC   MACLIST(ESTAEL),ESTAE   Move ESTAE parm list
         L     R6,=A(NJEDMP)           Point to local ESTAE rtn
         ESTAE (R6),                   Issue ESTAE                     X
               CT,                                                     X
               TERM=YES,                                               X
               PARAM=(R10),            PARAM is work area address      X
               MF=(E,MACLIST)
*
*-- Establish TSO userid issuing this command
*
         TESTAUTH FCTN=1               Are we authorized on entry?
         LTR   R15,R15                 Check result
         BNZ   INIT010                 Branch if not authorized
         OI    FLAGS1,F1APF            Indicate authorized on entry
*
INIT010  EQU   *
         L     R2,PSATOLD-PSA(0)       -> my TCB
         L     R2,TCBTIO-TCB(R2)       -> my TIOT
         LA    R4,TIOCNJOB-IEFTIOT(R2) -> TIOT jobname             v221
         LR    R3,R4                   Assume will use jobname     v222
*
         L     R2,PSAAOLD-PSA(0)       -> my ASCB
         L     R6,ASCBTSB-ASCB(,R2)    -> TSB (or 0)
         L     R2,ASCBASXB-ASCB(,R2)   -> my ASXB
         ICM   R2,15,ASXBSENV-ASXB(R2) -> my ACEE
         BZ    INIT015                 Exit if no ACEE
*
         USING ACEE,R2
         CLI   ACEEUSRL,X'00'          No userid available?
         BE    INIT015                 Exit if unavail
         CLI   ACEEUSR,X'00'           Userid not formed correctly?
         BE    INIT015                 Exit if unavail
         LA    R3,ACEEUSR              -> Userid
         OI    FLAGS1,F1ACEE           Valid ACEE found
         CLC   ACEEUSR,=CL8'STC'       Is this a started task?     v221
         BNE   INIT015                 No, use ACEEUSR id          v221
         LR    R3,R4                   Make the TIOT jobname the idv221
         DROP  R2                      ACEE
*
INIT015  EQU   *
         MVC   USERID,0(R3)            Set the userid
         TM    FLAGS1,F1APF            Authorized at entry?
         BO    INIT040                 yes.
         CLC   USERID,=CL8'HERC01'     Special access id?
         BE    INIT020                 Yes
         CLC   USERID,=CL8'HERC02'     Special access id?
         BNE   INIT030                 No
*
INIT020  EQU   *
         OI    FLAGS1,F1AUSR           Indicate special authorized user
         SR    0,0                     Use authorization SVC
         LA    1,1                      For TK4- HERC01/HERC02 only
         SVC   244                     Get authorized
         B     INIT040
*
INIT030  EQU   *
         TM    FLAGS1,F1APF            Authorized at entry?
         BZ    ERR006                  No, issue error
*
INIT040  EQU   *
         LA    R6,0(,R6)               Clear high order byte
         LTR   R6,R6                   Was there a TSB address
         BNZ   INIT045                 There was. Running in TSO userid
         OI    FLAGS1,F1BATCH          Indicate batch TSO
         TM    FLAGS1,F1ACEE           Valid ACEE found?
         BO    INIT045                 Yes, go with ACEE userid
         BAL   R2,CHK000               See if NJE38 is active      v210
         BNZ   INIT045                 NJE38 not active; use jobnamv222
         MVC   USERID,DEFUSER          Use default userid
*
INIT045  EQU   *
         L     R2,4(,R8)               -> UPT from input parms
         USING UPT,R2
         MVC   PREFIX,BLANKS           Init receiving field
         SR    R1,R1                   Clear for IC
         ICM   R1,1,UPTPREFL           Get prefix length
         BZ    INIT050                 No prefix value in use
         BCT   R1,*+10                 Adjust for execute
         MVC   PREFIX(0),UPTPREFX      executed instr
         EX    R1,*-6                  Copy the prefix value
         DROP  R2                      UPT
*
INIT050  EQU   *
         MVC   STAXLIST(STAXL),STAX    Move STAX parm list
         LA    R5,LIST                 -> input buffer from attn
         LA    R6,STAXXIT              Point to local exit
         STAX  (R6),                   Set exit for attention          X
               OBUF=(ATTNMSG,L'ATTNMSG),                               x
               IBUF=((5),80),                                          x
               USADDR=(10),            Parameter is our work area      x
               MF=(E,MACLIST)
*
*-- Parse command line
*
         SR    R0,R0                   Code 0: parse command line
         L     R15,=A(NJEPAR)          -> parse routine
         BALR  R14,R15
*
         TM    FLAGS4,F4ATTN           Was ATTN pressed?           v201
         BO    EXIT08                  Y, immediate exit           v201
         LTR   R15,R15                 Any errors?
         BNZ   ERR001                  Display IJKPARS RC
*
*-- Issue hello msgs
*
INIT060  EQU   *
         LA    R2,MSG000               Issue hello msg
         BAL   R14,PUTLINE
         LA    R2,MSGBLNK              Issue blank line
         BAL   R14,PUTLINE
*
*-- Are we reading from the NJE38 spool or an INDATASET?
*
         TM    FLAGS3,F3INDS           INDATASET specified?
         BZ    OPN000                  No, use NETSPOOL
*
*-- Set up INDATASET
*
INIT080  EQU   *
         MVC   TDSNAME,USRINDS         Set DSNAME of INDATASET
*
         LA    R0,DYNINDS         24   allocate INDATASET
         L     R15,=A(NJEDYN)          -> dynamic allocation rtns
         BALR  R14,R15
         LTR   R15,R15                 Any errors?
         BNZ   EXIT08                  Exit if allocation error
*
         MVC   NETDATA(DMYNPOL),DMYNPO Set up DCB for PDS
         CLI   TDSORG,X'02'            Was DSORG=PO ?
         BE    *+10                    Yes
         MVC   NETDATA(DMYNPSL),DMYNPS Set up DCB for SEQL
*
         MVC   DDNETDAT,TDDNAME        Save off the DDNAME returned
         MVC   DECB(READL),READ        Set up DECB
         LA    R6,NETDATA              -> DCB
         USING IHADCB,R6
         MVC   DCBDDNAM,DDNETDAT       Set DCB DDNAME
*
         MVC   MACLIST(OPENL),OPEN     Move OPEN list
         OPEN  (NETDATA,INPUT),        Open the NETDATA dataset        X
               MF=(E,MACLIST)
         OI    FLAGS2,F2NETOPN         Indicate DCB is open
*
         CLC   DCBLRECL,=Y(80)         Is LRECL 80?
         BNE   ERR009                  No, cant be netdata file
         TM    DCBRECFM,DCBRECF        Fixed length records?
         BZ    ERR009                  No, cant be netdata file
*
         LH    R0,DCBBLKSI             Get physical blksize
         ST    R0,BLOCKLEN             Save it
         GETMAIN RU,LV=(0)             Get buffer to read blocks
         ST    R1,BLOCK                Save buffer addr
         DROP  R6
*
         CLI   TDSORG,X'40'            Was DSORG=PS ?
         BE    INIT100                 Yes, don't do the FIND
*
         FIND  NETDATA,USRMEM,D        Point to the member
         LTR   R15,R15                 Any errors?
         BNZ   ERR004                  Exit if member not found
*
*-- Process the initial NETDATA control records from INDATASET
*
INIT100  EQU   *
         SR    R0,R0                   Code 0, process initial NETDATA
         L     R15,=A(NJENET)          -> NETDATA parsing routines
         BALR  R14,R15                 Process the control records
*
         B     INIT110(R15)            Branch based on error
INIT110  B     USR000              00  Normal, proceed.
         B     ERR008              04  File is not NETDATA
         B     EXIT08              08  Invalid NETDATA encountered
         B     ERR005              0C  Unexpected EOF on INDATASET
         B     ERR007              10  READ i/o error on INDATASET
         B     ERR030              14  INMTEXT detected, not supported
         B     ERR038              18  Record segments exceed LRECLv222
*
*-- Open NETSPOOL
*
OPN000   EQU   *
         BAL   R2,CHK000               Get NJE38 Spool DSN         v210
         BNZ   ERR013                  NJE38 is not active         v210
*
         MVC   DDNETSPL,=CL8'NETSPOOL' Set NETSPOOL DDN (for unalloc)
         MVC   TDDNAME,DDNETSPL        NETSPOOL DD
         LA    R0,DYNETSPL        28   allocate NETSPOOL
         L     R15,=A(NJEDYN)          -> dynamic allocation rtns
         BALR  R14,R15
         LTR   R15,R15
         BNZ   EXIT08                  Exit with dynalloc error
*
         LA    R8,NCB1                 -> NCB
         USING NCB,R8
*
         LA    R6,TAGDATA              -> area to hold tag data
         USING TAG,R6
*
         NSIO  TYPE=OPEN,                                              x
               NCB=(R8),                                               x
               TAG=(R6),               -> Where tag data will be       x
               EODAD=EOD000
         C     R15,=F'4'               NETSPOOL needs verify?
         BE    ERR025                  Yes
         BL    OPN010                  Everything is good
         BAL   R14,FMT000              Display Open error
         CLC   NCBRTNCD(2),=AL1(8,152) X'0898' security denied access?
         BE    ERR032                  Yes, special msg
         B     EXIT08                  Exit on VSAM error
*
OPN010   EQU   *
         OI    FLAGS2,F2NCBOPN         Indicate NETSPOOL is open
         TM    FLAGS3,F3FILEID         Specific file # specified?
         BO    OPN020                  Yes
*
*-- Here for 'next' available spool file
*
         NSIO  TYPE=CONTENTS,          Get list of files               x
               NCB=(R8)
         LTR   R15,R15                 Any errors?
         BZ    OPN030                  No
         CLC   NCBRTNCD(2),=AL1(12,6)  No files in directory?
         BE    ERR010                  Close up and indicate no files
         BAL   R14,FMT000              Display error
         B     EXIT08                  Exit on VSAM error
*
*-- Here for 'specific' spool file number
*
OPN020   EQU   *
         MVC   TAGID,FILEID+2          Set file # to find
*
         NSIO  TYPE=FIND,              get directory entry             x
               NCB=(R8),                                               x
               TAG=(R6)                Where to place tag data
         LTR   R15,R15                 Any errors?
         BZ    OPN200                  No, process file
         CLC   NCBRTNCD(2),=AL1(12,4)  Was file id not found?
         BE    ERR011                  Yes
         BAL   R14,FMT000              Otherwise, display error
         B     EXIT08                  Exit on VSAM error
*
*-- Look for next available in contents directory
*
OPN030   EQU   *
         L     R2,NCBAREA              Get a list of spool content
         USING NSDIR,R2
         SR    R5,R5
         ICM   R5,3,NCBRECCT           # of returned entries
         SR    R6,R6                   Indicate nothing found yet
*
OPN040   EQU   *
         CLC   LCLNODE,NSTOLOC         Is this file for this link?
         BNE   OPN160                  no, skip this file
         TM    FLAGS1,F1ACEE           Was security available?
         BZ    OPN150                  No; do not enforce selection
         CLC   USERID,NSTOVM           Is this file for this userid?
         BNE   OPN160                  no, skip this file
*
OPN150   EQU   *
         LA    R6,TAGDATA              -> tag data area for file
         USING TAG,R6
         XC    TAGDATA(TAGLEN),TAGDATA
         MVC   TAGINLOC(TAGUSELN),NSINLOC  Copy tag datq
         B     OPN170                  Go process the file
*
OPN160   EQU   *
         LA    R2,NSDIRLN(,R2)         Next NETSPOOL dir entry
         BCT   R5,OPN040               Continue thru the contents
         DROP  R2                      NSDIR
*
*
OPN170   EQU   *
         LM    R0,R1,NCBAREAL          Get list length and address
         XC    NCBAREA,NCBAREA         Clear obsolete ptr
         FREEMAIN RU,LV=(0),A=(1)
*
         LTR   R6,R6                   Did we obtain tag data?
         BZ    ERR010                  No, no files available
         B     OPN300
*
*-- validate specific file owner
*
OPN200   EQU   *
         CLC   LCLNODE,TAGTOLOC        Is this file for this link?
         BNE   ERR016                  no, skip this file
         TM    FLAGS1,F1ACEE           Was security available?
         BZ    OPN300                  No; do not enforce selection
         CLC   USERID,TAGTOVM          Is file for this userid?
         BNE   ERR016                  no, skip this file
         DROP  R6                      TAG
         DROP  R8                      NCB
*
*-- Process the initial NETDATA control records from NETSPOOL
*
OPN300   EQU   *
         SR    R0,R0                   Code 0, process initial NETDATA
         L     R15,=A(NJENET)          -> NETDATA parsing routines
         BALR  R14,R15                 Process the control records
*
         B     OPN310(R15)             Branch based on error
OPN310   B     USR000              00  Normal, proceed.
         B     OPN400              04  File is not NETDATA
         B     EXIT08              08  Invalid NETDATA encountered
         B     ERR005              0C  Unexpected EOF on NETSPOOL
         B     ERR007              10  READ i/o error on NETSPOOL
         B     ERR030              14  INMTEXT detected, not supported
         B     ERR038              18  Record segments exceed LRECLv222
*
OPN400   EQU   *
         OI    FLAGS2,F2FLAT           Indicate file is a flat file
*
*-- Notify user of dataset and prompt for changes
*
*-- This routine will:
*   1. Obtain or make the dataset name that came from the Tag/NETDATA
*   2. Tell user that name and prompt for changes
*   3. Parse the changes
*
USR000   EQU   *
         LA    R7,INMF02A              -> 1st INMR02 record        v200
         USING INMFIELD,R7                                         v200
         NC    DSTYPE(2),DSTYPE        Was a DSTYPE key detected?  v200
         BZ    USR020                  No, we're good              v200
         CLI   DSTYPE+2,X'40'          PDSE program library?       v200
         BE    ERR035                  Can't support it            v200
         DROP  R7                      INMFIELD                    v200
*
USR020   EQU   *                                                   v200
         MVI   FLAGS4,X'00'            Reinit parse results flags
         L     R15,=A(NJENOT)          -> Notify user and parse rtn
         BALR  R14,R15
*
         LR    R1,R15                  RC to R1
         LR    R15,R0                  Any secondary RC to R15
         B     USR080(R1)              Branch based on error in R1
USR080   B     USR100              00  Normal, proceed.
         B     RCV920              04  User specified "END"
         B     RCV910              08  User specified "PURGE"
         B     ERR001              0C  IKJPARS err, RC in R15
         B     ERR026              10  PUTGET errr, RC in R15
*
*-- Did user enter a dataset name -and- member name on the prompt?
*-- If so, warn him that we are ignoring the member name.
*
USR100   EQU   *
         TM    FLAGS4,F4MEMINV         Was a member name specified?
         BZ    USR110                  No
*
         LA    R2,MSG021               msg: member name ignored
         BAL   R14,PUTLINE             Inform user
*
USR110   EQU   *
         TM    FLAGS2,F2FLAT           Flat non-NETDATA type file?
         BZ    RCV000                  No, process NETDATA
*
*-- Prepare attributes for a flat file
*
FLT000   EQU   *
         LA    R7,INMF02A              -> 1st INMR02 record
         USING INMFIELD,R7
         MVC   TDSNAME,FINALDS         Set up DSNAME to build
         MVI   DSNAME+1,44             Set DSNAME length for dynalloc
         MVC   DSORG+2(2),=X'4000'     Set DSORG=PS
         MVC   BLKSIZE+6(4),=F'0'      Set BLKSIZE to 0 to be computed
         MVI   RECFM+2,DCBRECF+DCBRECBR  Indicate RECFM=FB
         LA    R6,TAGDATA              -> TAG data
         USING TAG,R6
         LA    R1,80                   Assume punch data length
         TM    TAGINDEV,TYPPUN         Is this punch data?
         BO    FLT010                  Yes
         LA    R1,133                  Assign print data length
         OI    RECFM+2,DCBRECCA        Use ASA ctl char
*
FLT010   EQU   *
         STCM  R1,15,LRECL+6           Set LRECL
         SR    R0,R0                   Clear for multiply
         M     R0,TAGRECNM             Compute size of file
         ST    R1,FILESIZE+6           Set size in bytes for space calc
         DROP  R6,R7                   TAG,INMFIELD
*
*-- Prepare to receive the data
*
RCV000   EQU   *
         LA    R7,INMF02A              -> 1st INMR02 record
         USING INMFIELD,R7                                         v222
         ICM   R0,15,BLKSIZE+6         Get blocksize to use in srchv222
         C     R0,=F'32760'            BLKSIZE > 32760 MVS limit?  v222
         BH    ERR023                  Exit if invalid blksize     v222
         CLC   LRECL+6(4),=F'32760'    LRECL > 32760 MVS limit?    v222
         BH    ERR023                  Exit if invalid LRECL       v222
*
         TM    FLAGS1,F1INMR2B         Was there a second INMR02?
         BZ    RCV030                  No
         LA    R7,INMF02B              -> 2nd INMR02 record
*
*-- Locate a suitable volume to hold the new dataset
*
RCV030   EQU   *
         MVC   TVOLSER,USRVOL          Assume user specified volser
         TM    FLAGS3,F3VOLSER         Did user specify a volser?
         BO    RCV040                  Yes, we'll use that
         TM    FLAGS4,F4VOLSER         user specify a volser at prompt?
         BO    RCV040                  Yes, we'll use that
         TM    FLAGS2,F2UNIT           user specify a unit?        v200
         BO    RCV040                  Y, dont select a volume     v200
*
*                                      R0 must contain BLKSIZE or 0
         BAL   R14,GETVOL              Find a volume for allocation
         BZ    ERR022                  No volume found
*
*-- Start computing values and filling dynamic allocation text units
*
RCV040   EQU   *
         LA    R1,TVOLSER              -> selected volser          v200
         BAL   R14,FNDVOL              Get track sz of selected volv200
         BZ    ERR036                  Volume not online           v200
         LA    R1,INMF02A              -> 1st INMR02 record        v200
         L     R1,BLKSIZE+6-INMFIELD(,R1) Get target DSN blksize   v200
         CR    R1,R15                  Will block fit on track?    v200
         BH    ERR037                  No; were done here          v200
*
         BAL   R14,GETBSZ              Obtain final sizes, format
         STH   R1,TBLKSIZE             Set dynalloc block size
         STCM  R1,7,TBLKLEN            Set dynalloc space blk len
         STH   R2,TLRECL               Set dynalloc lrecl
         STC   R3,TRECFM               Set dynalloc recfm
*
         BAL   R14,GETSPACE            Compute space parameters
         STCM  R1,7,TPRIME             Set primary space in blocks
         STCM  R2,7,TSECND             Set secondary space in blocks
*
         MVC   TDSORG,DSORG+2          NETDATA DSORG to text unit
         MVC   TDSNAME,FINALDS         Set DSNAME to allocate
*
*
*
*-- Call NJEDYN to allocate the dataset
*
         LA    R0,DYNINMCP        04   allocate dataset for SEQL file
         L     R15,=A(NJEDYN)          -> dynamic allocation rtns
         BALR  R14,R15
*
         B     RCV060(R15)             Branch on RC
RCV060   B     RCV200              00  Normal, proceed
         B     USR000              04  Dataset exists, reprompt
         B     EXIT08              08  All other errors
*
*
*-- Open the dataset
*
RCV200   EQU   *
         MVC   DDSYSUT1,TDDNAME        Save off the DDNAME returned
         MVC   NEWDS(DMYSEQL),DMYSEQ   Set up DCB
         LA    R6,NEWDS                -> DCB
         USING IHADCB,R6
         MVC   DCBBLKSI,TBLKSIZE       Set block size
         MVC   DCBLRECL,TLRECL         Set length
         MVC   DCBRECFM,TRECFM         Set format
         MVC   DCBDDNAM,DDSYSUT1       Set Dynamic DD name
         TM    DCBRECFM,DCBRECU        Using undefined records?
         BNO   RCV210                  No
*
         LH    R0,DCBBLKSI             Get dataset block size
         GETMAIN RU,LV=(0)             Get recd build buffer for RECFMU
         STM   R0,R1,NEWLEN            Save length and addr
         DROP  R6                      IHADCB
*
RCV210   EQU   *
         MVC   MACLIST(OPENL),OPEN     Move OPEN list
         OPEN  (NEWDS,OUTPUT),         Open the NEWDS dataset          X
               MF=(E,MACLIST)
         OI    FLAGS2,F2NEWOPN         Indicate DCB is open
*
         LA    R0,4                    Code 4, process NETDATA
         TM    FLAGS2,F2FLAT           Flat non-NETDATA type file?
         BZ    RCV220                  No, proceed with NETDATA
         LA    R0,8                    Code 8, process PRT/PUN file
*
RCV220   EQU   *
         L     R15,=A(NJENET)          -> data retreival routines
         BALR  R14,R15                 Process the records
*
         B     RCV230(R15)             Branch based on result RC
RCV230   B     RCV240              00  Normal, proceed.
         DC    AL4(0)              04  Not used
         B     EXIT08              08  Invalid NETDATA encountered
         B     ERR005              0C  Unexpected EOF on INDATASET
         B     ERR007              10  READ i/o error on INDATASET
         B     ERR030              14  INMTEXT detected, not supported
         B     ERR038              18  Record segments exceed LRECLv222
*
RCV240   EQU   *
         MVC   MACLIST(CLOSEL),CLOSE   Move close list
         CLOSE (NEWDS),                Close it                        X
               MF=(E,MACLIST)
         NI    FLAGS2,255-F2NEWOPN     Indicate file closed
*
         TM    FLAGS3,F3INDS           INDATASET specified?
         BZ    RCV250                  No, skip close
*
         MVC   MACLIST(CLOSEL),CLOSE   Move close list
         CLOSE (NETDATA),              Close it                        X
               MF=(E,MACLIST)
         NI    FLAGS2,255-F2NETOPN     Indicate NETDATA file closed
*
*
*
*-- If two INMR02 control records were found, then we need to run
*-- IEBCOPY to load a PDS from the unloaded file just processed above.
*
RCV250   EQU   *
         TM    FLAGS1,F1INMR2B         Was there a second INMR02?
         BZ    RCV950                  No. We're done
*
         LA    R7,INMF02A              -> 1st INMR02 record
         USING INMFIELD,R7
*
*-- Filling dynamic allocation text units for final dataset
*
         ICM   R1,15,BLKSIZE+6         Get the NETDATA blksize
         STH   R1,TBLKSIZE             Set dynalloc block size
         STCM  R1,7,TBLKLEN            Set dynalloc space blk len
         MVC   TLRECL,LRECL+8          Set dynalloc lrecl
         MVC   TRECFM,RECFM+2          Set dynalloc recfm
*
         BAL   R14,GETSPACE            Compute space parameters
         STCM  R1,7,TPRIME             Set primary space in blocks
         STCM  R2,7,TSECND             Set secondary space in blocks
*
         MVC   TDIRBLKS,DIRBLKS+7      Set directory blocks required
         TM    FLAGS2,F2DIR            Did user override with DIR? v200
         BZ    RCV255                  No                          v200
         MVC   TDIRBLKS,USRDIR+1       Set directory blocks req'd  v200
*
RCV255   EQU   *                                                   v200
         MVC   TDSORG,DSORG+2          NETDATA DSORG to text unit
         MVC   TDSNAME,FINALDS         Set DSNAME to allocate
*
*
*-- Call NJEDYN to allocate the final output dataset as "SYSUT2"
*
         LA    R0,DYNFINAL        10   allocate final dataset
         L     R15,=A(NJEDYN)          -> dynamic allocation rtns
         BALR  R14,R15
*
         B     RCV260(R15)             Branch on RC
RCV260   B     RCV400              00  Normal, proceed
         B     RCV300              04  Dataset exists, reprompt
         B     EXIT08              08  All other errors
*
*-- Notify user of existing dataset and prompt for changes
*
*-- This routine will:
*   1. Obtain or make the dataset name that came from the Tag/NETDATA
*   2. Tell user that name and prompt for changes
*   3. Parse the changes
*
RCV300   EQU   *
         MVI   FLAGS4,X'00'            Reinit parse results flags
         L     R15,=A(NJENOT)          -> Notify user and parse rtn
         BALR  R14,R15
*
         LR    R1,R15                  RC to R1
         LR    R15,R0                  Any secondary RC to R15
         B     RCV310(R1)              Branch based on error in R1
RCV310   B     RCV320              00  Normal, proceed.
         B     RCV920              04  User specified "END"
         B     RCV910              08  User specified "PURGE"
         B     ERR001              12  IKJPARS err, RC in R15
         B     ERR026              16  PUTGET errr, RC in R15
*
*-- Did user enter a dataset name -and- and member name on the prompt?
*-- If so, warn him that we are ignoring the member name.
*
RCV320   EQU   *
         TM    FLAGS4,F4VOLSER         Was a new volser specified?
         BZ    RCV330                  No
         MVC   TVOLSER,USRVOL          Grab new volser
*
RCV330   EQU   *
         TM    FLAGS4,F4MEMINV         Was a member name specified?
         BZ    RCV250                  No, try to allocate again
*
         LA    R2,MSG021               msg: member name ignored
         BAL   R14,PUTLINE             Inform user
         B     RCV250                  Try to allocate again
*
*
*
*-- Prepare to launch IEBCOPY
*
RCV400   EQU   *
         MVC   DDSYSUT2,TDDNAME        Set replacement SYSUT2 DD
*
*-- Call NJEDYN to allocate the SYSIN dataset needed by IEBCOPY
*
         LA    R0,DYNSYSIN        08   allocate SYSIN for IEBCOPY
         L     R15,=A(NJEDYN)          -> dynamic allocation rtns
         BALR  R14,R15
         LTR   R15,R15
         BNZ   EXIT08                  Exit with dynalloc error
         MVC   DDSYSIN,TDDNAME         Save generated DDNAME
*
*-- Call NJEDYN to allocate the SYSPRINT dataset needed by IEBCOPY
*
         LA    R0,DYNSYSPR        12   allocate SYSPRINT for IEBCOPY
         L     R15,=A(NJEDYN)          -> dynamic allocation rtns
         BALR  R14,R15
         LTR   R15,R15
         BNZ   EXIT08                  Exit with dynalloc error
         MVC   DDSYSPR,TDDNAME         Save generated DDNAME
*
*-- Call NJEDYN to allocate the SYSUT3 dataset needed by IEBCOPY
*
         LA    R0,DYNSYSU3        14   allocate SYSUT3 temporary
         L     R15,=A(NJEDYN)          -> dynamic allocation rtns
         BALR  R14,R15
         LTR   R15,R15
         BNZ   EXIT08                  Exit with dynalloc error
         MVC   DDSYSUT3,TDDNAME        Set replacement SYSUT3 DD
*
*-- Invoke IEBCOPY
*
         MVC   CPYPLIST,COPYPARM       Move IEBCOPY parms to 24-bit stg
         MVC   DDLISTL,=AL2(DDLISTSZ)  Set IEBCOPY DD list length
         LA    R2,CPYPLIST
         LA    R3,DDLISTL
         MVC   MACLIST(LINKL),LINK     Move macro model
         LINK  EP=IEBCOPY,                                             x
               PARAM=((R2),(R3)),                                      x
               VL=1,                                                   x
               MF=(E,MACLIST)
         LTR   R5,R15                  Copy RC to R5
         BZ    RCV950                  Exit on success
*
*-- RECEIVE ended because IEBCOPY failed
*
RCV900   EQU   *
         LA    R2,MSGBLNK              -> blank line msg
         BAL   R14,PUTLINE
*
         MVC   LIST(4+L'MSG018T),MSG018 IEBCOPY fail msg
         CVD   R5,DBLE                 Convert IEBCOPY RC
         UNPK  LIST+37(2),DBLE
         OI    LIST+38,X'F0'           Fix sign
*
         LA    R2,LIST                 -> start of msg
         BAL   R14,PUTLINE             Display failure
         B     EXIT08
*
*-- User chose PURGE on the action prompt; purge the spool file
*-- (if not using INDATASET) and then exit.
*
RCV910   EQU   *
         TM    FLAGS3,F3INDS           Was INDATASET specified?
         BO    RCV920                  Y, exit with no action
         BAL   R14,PUR000              Purge spool file as requested
         LA    R2,MSGBLNK              -> blank line msg
         BAL   R14,PUTLINE
         LA    R2,MSG029               -> ended with nothing recv'd
         BAL   R14,PUTLINE
         B     EXIT00                  And we're done
*
*-- RECEIVE ended with no action taken
*
*-- Here if 'END' specified or attention received
*
RCV920   EQU   *
         LA    R2,MSGBLNK              -> blank line msg
         BAL   R14,PUTLINE
         LA    R2,MSG019               -> ended with no action
         BAL   R14,PUTLINE
         TM    FLAGS2,F2FEND           Was END forced in BATCH mode?
         BO    EXIT08                  Yes, force RC=8
         B     EXIT00
*
*-- RECEIVE ended successfully with dataset created and filled
*
*-- If the user at any time specified the PURGE option, remove
*-- the spool file that was received.
*
RCV950   EQU   *
*
RCV990   EQU   *
         LA    R2,MSGBLNK              -> blank line msg
         BAL   R14,PUTLINE
         MVC   LIST,BLANKS
         MVC   LIST(4+L'MSG017T),MSG017 Success msg
         LA    R1,LIST+4+L'MSG017T     -> next available byte
         MVI   0(R1),C''''             Move apost
         MVC   1(44,R1),FINALDS        Move final DSN
         TRT   1(45,R1),BLANK          Look for end of DSN
         MVI   0(R1),C''''             Move apost
         LA    R1,2(,R1)               -> skip over apost + 1 blank
         MVC   0(10,R1),=C'successful'
         LA    R1,10(,R1)              -> skip to end
         LA    R2,LIST                 -> start of msg
         SR    R1,R2                   Compute msg length
         STH   R1,LIST                 Set RDW
         BAL   R14,PUTLINE             Display success
*
         BAL   R14,PUR000              Purge the spool file if needed
         B     EXIT00
*
*
*-- Return the BLKSIZE value from the NETDATA, and adjust the
*-- RECFM and LRECL based on the NETDATA-unique variable formats when
*-- applicable.
*
*-- General guidelines and manipulations by this routine:
*
*-- 1. If the NETDATA LRECL is zero, this is unusual but don't alter
*--    any other DCB parameters; this covers the RECFM=U case.
*
*-- 2. If the NETDATA RECFM specifies the variable spanned records,
*--    leave all other parameters as is.  This file came from MVS.
*
*-- 3. If the NETDATA RECFM specifies the shortened variable format,
*--    e.g., RECFM=xx01 or xx02, then the LRECL must be increased
*--    by 4 bytes to account for a RDW to be inserted.
*
*-- 4. If the NETDATA RECFM specifies the shortened variable format,
*--    e.g., RECFM=xx01 or xx02, then the RECFM value used for
*--    dynamic allocation of the dataset must be modified to specify
*--    variable length records, as the variable X'40' bit may not
*--    be set in the NETDATA RECFM.
*
*-- 5. If the BLKSIZE is 0, the file probably came from VM; then
*--    do the following:
*--    a. Manufacture a suitable blksize as close to 4K as possible.
*--    b. If the LRECL > 4K, then make BLKSIZE=LRECL.
*--    c. For Fixed length records, force the RECFM X'10' bit to
*--       indicate blocked records, if BLKSIZE is not equal to LRECL.
*
*-- Entry: Fields BLKSIZE, LRECL, RECFM as decoded from NETDATA
*-- Exit:  R1 = BLKSIZE for use in dynamic allocation and DCB
*--        R2 = LRECL for use in dynamic allocation and DCB
*--        R3 = RECFM for use in dynamic allocation and DCB
*
GETBSZ   EQU   *
         ICM   R1,15,BLKSIZE+6         Get blocksize
         IC    R3,RECFM+2              Get DCB portion of NETDATA RECFM
         ICM   R2,15,LRECL+6           Get lrecl
         BZR   R14                     No LRECL?  leave everything be
         TM    RECFM+2,X'48'           Spanned variable records?
         BOR   R14                     Yes, use as specified
         TM    RECFM+3,X'03'           Shortened variable format?
         BZ    GETB010                 No
         LA    R2,4(,R2)               Add length to LRECL for RDW
         O     R3,=A(DCBRECV+DCBRECBR) Ensure DCB RECFM is VB
*
GETB010  EQU   *
         LTR   R1,R1                   Was there a blksize?
         BNZR  R14                     Use it if we have it
         TM    RECFM+2,X'40'           Variable format data?
         BO    GETB030                 Yes
         TM    RECFM+3,X'03'           Compressed variable format?
         BNZ   GETB030                 Yes, treat as variable
*fixed
         L     R1,=F'4096'             Get possible block size
         DR    R0,R2                   Compute # recs in 4096 block
         LTR   R1,R1                   Do any recs fit?
         BZ    GETB020                 No, so make blksize=lrecl
         SR    R0,R0                   Dispose of remainder
         MR    R0,R2                   Compute nearest block size
         O     R3,=A(DCBRECF+DCBRECBR) Set RECFM to FB
         BR    R14                     Return with BLKSIZE in R1
*
GETB020  EQU   *
         LR    R1,R2                   Make BLKSIZE=LRECL if LRECL>4096
         N     R3,=A(-1-DCBRECBR)      Turn off blocking
         BR    R14                     Return with BLKSIZE in R1
*
*variable
GETB030  EQU   *
         L     R1,=F'4096'             Get possible block size
         LA    R0,4092                 Possible size - 4 (for RDW)
         CR    R2,R0                   Will LRECL fit in possible size?
         BNHR  R14                     Yes, use the 4K blksize
*
GETB040  EQU   *
         LA    R1,4(,R2)               Mk BLKSIZE=LRECL+4 if LRECL>4092
         N     R3,=A(-1-DCBRECBR)      Turn off blocking
         BR    R14                     Return with BLKSIZE in R1
*
*-- Compute primary and secondary space values in # blocks
*
*-- Entry: R1 = blksize
*--        Field FILESIZE contains NETDATA estimated file size in bytes
*
*-- Exit:  R1 = # of primary blocks
*--        R2 = # of secondary blocks (always 10% of primary)
*
GETSPACE EQU   *
         ICM   R3,15,FILESIZE+6        Get approx size of file
         SR    R2,R2                   Clear for divide
         DR    R2,R1                   Compute # blocks needed
         LA    R3,1(,R3)               Always round up
         LR    R1,R3                   Return primary blocks in R1
         SR    R2,R2                   Clear for divide
         D     R2,=F'10'               Compute 1/10th of needed amt
         LA    R2,1(,R3)               Round up = secondary blks needed
         BR    R14                     Return with R1 & R2 values
         DROP  R7
*
*-- PURGE the spool file
*
*-- Conditions:
*-- 1.  If INDATASET was specified then there is no spool file to purge
*-- 2.  If PURGE was specified when the user was prompted for
*--     additional parameters, then purge the spool file and exit
*--     without receiving the file.
*-- 3.  If NOPURGE was specified on the command line (if not overridden
*--     by (2) above), then receive the file but do not purge it from
*--     the spool.
*-- 4.  Otherwise, PURGE is defaulted or explicity specified on the
*--     command line, receive the file and then purge it from spool.
*
PUR000   EQU   *
         TM    FLAGS3,F3INDS           INDATASET specified?
         BOR   R14                     Yes, PURGE has no meaning
         TM    FLAGS4,F4PURGE          PURGE specified on prompt?
         BO    PUR010                  Yes, do it
         TM    FLAGS3,F3PURGE          PURGE specified or defaulted?
         BZR   R14                     No; do not purge spool file
*
PUR010   EQU   *
         ST    R14,SV14PUR             Save return addr
         LA    R6,TAGDATA              -> area containing tag data
         USING TAG,R6
         LA    R8,NCB1                 -> NCB
         NSIO  TYPE=PURGE,             Purge the file                  x
               NCB=(R8),                                               x
               TAG=(R6)                -> Where tag data is
         LTR   R15,R15                 Any errors?
         BZ    PUR040                  No
         BAL   R14,FMT000              Display error
         B     EXIT08                  Exit on VSAM error
*
PUR030   EQU   *
         NSIO  TYPE=CLOSE,             Close the spool                 x
               NCB=(R8)
         NI    FLAGS2,255-F2NCBOPN     Indicate NETSPOOL closed
*
PUR040   EQU   *
         MVC   LIST(4+L'MSG028T),MSG028  Move file purged msg
         LH    R1,TAGID                Get the file ID
         DROP  R6                      TAG
         CVD   R1,DBLE
         UNPK  LIST+9(4),DBLE
         OI    LIST+12,X'F0'           Fix sign
         LA    R2,LIST                 -> msg text
         BAL   R14,PUTLINE             Inform user
         L     R14,SV14PUR             Reload return addr
         BR    R14                     Return
*
*
ERR001   EQU   *
         MVC   LIST(4+L'MSG001T),MSG001  Move msg to work area
         CVD   R15,DBLE                unpk IKJPARS RC
         UNPK  LIST+57(2),DBLE
         OI    LIST+58,X'F0'           Fix sign
         LA    R2,LIST                 -> msg
         B     ERRPUT                  Write msg                   v200
*
ERR004   EQU   *
         MVC   LIST(4+L'MSG004T),MSG004  Move msg to work area
         MVC   LIST+11(8),USRMEM       Plug in member name
         LA    R2,LIST                 -> msg
         B     ERRPUT                  Write msg                   v200
*
ERR005   EQU   *
         MVC   LIST(4+L'MSG005T),MSG005  Move msg text
         LA    R1,=CL9'INDATASET'      Assume reading from INDATASET
         TM    FLAGS3,F3INDS           Using INDATASET?
         BO    *+8                     We are
         LA    R1,=CL9'NETSPOOL'       NO, its NETSPOOL
         MVC   LIST+4+L'MSG005T(9),0(R1)  Move source of error
         LH    R1,LIST                 Get current msg length
         LA    R1,9(,R1)               Add on the source length
         STH   R1,LIST                 Put back
         LA    R2,LIST                 Unexpected EOF on xxxxxxxxx
         B     ERRPUT                  Write msg                   v200
*
ERR006   EQU   *
         LA    R2,MSG006               Not APF authorized
         B     ERRPUT                  Write msg                   v200
*
ERR007   EQU   *
         MVC   LIST(4+L'MSG007T),MSG007  Move msg text
         LA    R1,=CL9'INDATASET'      Assume reading from INDATASET
         TM    FLAGS3,F3INDS           Using INDATASET?
         BO    *+8                     We are
         LA    R1,=CL9'NETSPOOL'       NO, its NETSPOOL
         MVC   LIST+4+L'MSG007T(9),0(R1)  Move source of error
         LH    R1,LIST                 Get current msg length
         LA    R1,9(,R1)               Add on the source length
         STH   R1,LIST                 Put back
         LA    R2,LIST                 Read i/o error on INDATASET
         B     ERRPUT                  Write msg                   v200
*
ERR008   EQU   *
         LA    R2,MSG008               INDATASET is not NETDATA fmt
         B     ERRPUT                  Write msg                   v200
*
ERR009   EQU   *
         LA    R2,MSG009               INDATASET is not 80/F
         B     ERRPUT                  Write msg                   v200
*
ERR010   EQU   *
         LA    R2,MSG010               No files available to receive
         B     ERRPUT                  Write msg                   v200
*
ERR011   EQU   *
         LA    R2,MSG011               Specific file number not exis
         B     ERRPUT                  Write msg                   v200
*
ERR013   EQU   *
         LA    R2,MSG013               NJE38 is not active
         B     ERRPUT                  Write msg                   v200
*
ERR016   EQU   *
         LA    R2,MSG016               Cant receive another users file
         B     ERRPUT                  Write msg                   v200
*
ERR022   EQU   *
         LA    R2,MSG022               No suitable PUBLIC volume
         B     ERRPUT                  Write msg                   v200
*
ERR023   EQU   *
         LA    R2,MSG023               BLKSIZE/LRECL to large
         B     ERRPUT                  Write msg                   v200
*
ERR025   EQU   *
         LA    R2,MSG025               Need to run VERIFY
         B     ERRPUT                  Write msg                   v200
*
ERR026   EQU   *
         MVC   LIST(4+L'MSG026T),MSG026  Move msg to work area
         CVD   R15,DBLE                unpk PUTGET RC
         UNPK  LIST+49(2),DBLE
         OI    LIST+50,X'F0'           Fix sign
         LA    R2,LIST                 -> msg PUTGET failed
         B     ERRPUT                  Write msg                   v200
*
ERR030   EQU   *
         LA    R2,MSG030               INMTEXT detected not supported
         B     ERRPUT                  Write msg                   v200
*
ERR032   EQU   *
         LA    R2,MSG032               Security denied access NETSPOOL
         B     ERRPUT                  Write msg                   v200
*
ERR035   EQU   *                                                   v200
         LA    R2,MSG035               Incoming is a PDSE Prog Lib v200
         B     ERRPUT                  Write msg                   v200
*
ERR036   EQU   *                                                   v200
         LA    R2,MSG036               Volume not online           v200
         B     ERRPUT                  Write msg                   v200
*
ERR037   EQU   *                                                   v200
         LA    R2,MSG037               BLKSIZE to large for volume v200
         B     ERRPUT                  Write msg                   v200
*
ERR038   EQU   *                                                   v222
         LA    R2,MSG038               Input file recs exceed LRECLv222
         B     ERRPUT                  Write msg                   v222
*
ERRPUT   EQU   *                                                   v200
         BAL   R14,PUTLINE             Write error msg in R2       v200
         B     EXIT08                  Exit w RC=08                v200
*
EXIT00   EQU   *
         SR    R15,R15                 Set RC=0
         B     XIT000                  Clean up and exit
*
EXIT08   EQU   *
         LA    R15,8                   Set RC=8
         B     XIT000                  Clean up and exit
*
XIT000   EQU   *
         LA    R13,NJESA               Ensure using proper SA in case
*                                       we've come here due to ESTAE
         LR    R5,R15                  Save RC across shutdown
         ESTAE 0                       Disable ESTAE
*
         TM    FLAGS2,F2NETOPN         Is NETDATA open?
         BZ    XIT010                  No
         MVC   MACLIST(CLOSEL),CLOSE   Move close list
         CLOSE (NETDATA),              Close it                        X
               MF=(E,MACLIST)
*
XIT010   EQU   *
         TM    FLAGS2,F2NEWOPN         Is NEWDS open?
         BZ    XIT020                  No
         MVC   MACLIST(CLOSEL),CLOSE   Move close list
         CLOSE (NEWDS),                Close it                        X
               MF=(E,MACLIST)
*
XIT020   EQU   *
         TM    FLAGS2,F2NCBOPN         Is NETSPOOL open?
         BZ    XIT030                  No
         SR    R6,R6                   Ensure no tag data
         LA    R8,NCB1                 -> NCB
         NSIO  TYPE=CLOSE,             Close the spool                 x
               NCB=(R8)
*
XIT030   EQU   *
         L     R0,BLOCKLEN             Size of stg area
         ICM   R1,15,BLOCK             -> stg area
         BZ    XIT040                  Skip if never allocated
         FREEMAIN RU,LV=(0),A=(1)      Release it
*
XIT040   EQU   *
         L     R0,NEWLEN               Size of stg area
         ICM   R1,15,NEWBLK            -> stg area
         BZ    XIT050                  Skip if never allocated
         FREEMAIN RU,LV=(0),A=(1)      Release it
*                                                                       NJE00200
XIT050   EQU   *                                                        NJE00210
         LA    R3,DDLIST               -> list of DD's we allocated
         LA    R4,UNLISTSZ/8           # of DD list entries
*
XIT060   EQU   *
         CLC   =XL8'00',0(R3)          Unassigned DD?
         BE    XIT070                  Skip to next
*
         MVC   UDDNAME,0(R3)
         LA    R0,UNDYN           00   unalloc
         L     R15,=A(NJEDYN)          -> dynamic allocation rtns
         BALR  R14,R15
*
XIT070   EQU   *                                                        NJE00210
         LA    R3,8(,R3)               -> next DD entry
         BCT   R4,XIT060               Continue unallocation scan
*
XIT080   EQU   *                                                        NJE00210
         TM    FLAGS1,F1AUSR           Special authorized user?
         BZ    QUIT                    Y, Don't need Auth SVC
         SR    0,0                     Use authorization SVC
         SR    1,1                      For HERC01/HERC02 only
         SVC   244                     Get un-authorized
*
QUIT     EQU   *                                                        NJE00210
         LR    R1,R10                  -> NJEWK main work area page
         L     R13,4(,R13)             -> caller's sa                   NJE00210
         ST    R5,16(,R13)             Set exit RC
         FREEMAIN RU,                                                  x
               LV=4096,                                                x
               A=(1)
         LM    R14,R12,12(R13)         Reload system's regs             NJE00220
         BR    R14                     Return                           NJE00240
         DROP  R12
*
*-- STAX attention exit
*
*-- Just post the PUTGET ECB and return.  PUTGET will fail with RC=8.
*
STAXXIT  EQU   *
         STM   R14,R12,12(R13)         Save
         LR    R12,R15                 Get base
         USING STAXXIT,R12
         L     R10,8(,R1)              -> NJEWK area
         USING NJEWK,R10
         POST  PUTECB,16               Post the PUTGET ECB
         OI    FLAGS4,F4ATTN           Indicate ATTN pressed       v201
         LM    R14,R12,12(R13)         Load
         DROP  R12
         BR    R14                     Return
*
         LTORG ,
*
DMYNPO   DCB   DDNAME=NETDATA,                                         X
               MACRF=(R),                                              X
               DSORG=PO,                                               X
               EODAD=EOD000
DMYNPOL  EQU   *-DMYNPO
*
DMYNPS   DCB   DDNAME=NETDATA,                                         X
               MACRF=(R),                                              X
               DSORG=PS,                                               X
               EODAD=EOD000
DMYNPSL  EQU   *-DMYNPS
*
*
DMYSEQ   DCB   DDNAME=0,                                               X
               MACRF=(PL),                                             X
               DSORG=PS,                                               X
               BFTEK=A
DMYSEQL  EQU   *-DMYSEQ
*
*
OPEN     OPEN  0,MF=L
OPENL    EQU   *-OPEN
CLOSE    CLOSE 0,MF=L
CLOSEL   EQU   *-CLOSE
LINK     LINK  EP=0,SF=L
LINKL    EQU   *-LINK
READ     READ  DMYDECB,SF,DMYNPO,MF=L
READL    EQU   *-READ
ESTAE    ESTAE 0,MF=L
ESTAEL   EQU   *-ESTAE
STAX     STAX  0,OBUF=(0,0),IBUF=(0,0),USADDR=0,MF=L
STAXL    EQU   *-STAX
*
COPYPARM DC    AL2(L'COPYOPT)
COPYOPT  DC    C'WORK=0512K'
COPYPRML EQU   *-COPYPARM                 TOTAL LENGTH OF PARM OPTION
*
ATTNMSG  DC   C'COMMAND TERMINATED DUE TO ATTENTION; PRESS ENTER TWICE'
*                                                                  v201
*********************
*  N J E C O M      *               NJECOM hosts small routines and
*                   *               frequently used constants that
*  Common routines  *               are available to all NJERxx csects
*  and constants    *               via base register 11
*                   *
*********************
*
NJECOM   CSECT
         DC    A(0)                 No branch around constants
         DC    AL1(23)                LENGTH OF EYECATCHERS
         DC    CL9'NJECOM'
         DC    CL9'&SYSDATE'
         DC    CL5'&SYSTIME'
         USING NJECOM,R11
*
*-- GET000 is used to read a block from the INDATASET or to read
*-- a record from NETSPOOL.
*
*-- Entry:  INDATASET or NETSPOOL must be OPEN.
*-- Exit:  R0 = length of data read
*--        R1 -> data read
*--        R15= RC.  0=OK
*--                  4=Unexpected end of file
*--                  8=Read i/o error
*
GET000   EQU   *
         ST    R14,SV14GET
         TM    FLAGS3,F3INDS        INDATASET specified?
         BZ    GET030               No, use NETSPOOL
*
         LA    R2,NETDATA           -> DCB
         L     R0,BLOCK             -> read buffer
*
         READ  DECB,SF,(R2),(R0),'S',MF=E   Read block
         CHECK DECB
*
         CLI   DECB,X'7F'           Was read successful?
         BNE   GET090               No, read failed
*
         L     R0,BLOCKLEN          Get current block size
         L     R15,DECB+16          -> IOB addr
         SH    R0,14(,R15)          Compute size of block read
         L     R1,BLOCK             Return buffer addr
         SR    R15,R15              Set RC=0
         L     R14,SV14GET
         BR    R14                  Return w/len & addr in R0,R1
*
GET030   EQU   *
         LA    R1,NCB1              -> NCB
         NSIO  TYPE=GET,            TAG data contains file #           x
               NCB=(1),                                                x
               AREA=LIST                                           v200
         LTR   R15,R15              Any errors?
         BZ    GET040               No
         BAL   R14,FMT000           Display error
         B     GET090
*
GET040   EQU   *
         MVC   REC(133),BLANKS      Init receiving field           v200
         LH    R2,NCBRECLN-NCB(,R1) Get the record length          v200
         BCTR  R2,0                 Adjust for execute             v200
         EX    R2,MVSPL             Mv spool record to phy rec areav200
         LA    R1,REC               -> record
         LA    R0,80                Always 80
         SR    R15,R15              Set RC=0
         L     R14,SV14GET
         BR    R14                  Return w/len & addr in R0,R1
*
MVSPL    MVC   REC(0),LIST          executed instr                 v200
*
GET090   EQU   *
         LA    R15,8                Set RC=8 = Read error
         L     R14,SV14GET
         BR    R14                  Return
*
EOD000   EQU   *
         LA    R15,4                Set RC=4 = unexpected EOF
         L     R14,SV14GET
         BR    R14                  Return w/len & addr in R0,R1
*
*-- Find a PUBLIC volume for use in allocations
*
*-- Entry:  R0 = blksize of dataset to be allocated
*-- Exit:   CC=0 if no volume selected
*--         CC<>0 if volume selected, and,
*--           TVOLSER,DEVINFO fields are filled in.
*
*-- Uses R15-R3
*
GETVOL   EQU   *
         LA    R1,DISKS            -> dasd characteristics table
         USING DASDTAB,R1
*
GETV010  EQU   *
         SR    R3,R3               Clear for ICM
         L     R2,16               -> CVT
         USING CVT,R2
         L     R2,CVTILK2          -> UCB Lookup table
*
GETV020  EQU   *
         LA    R2,2(,R2)           -> first table entry
*
         CLC   0(2,R2),=X'FFFF'    End of UCBs?
         BE    GETV030             Y
         ICM   R3,3,0(R2)          -> UCB
         BZ    GETV020             Skip empty table slot
         USING IEFUCBOB,R3
         TM    UCBSTAT,UCBONLI     Is device online?
         BZ    GETV020             N, next UCB
         TM    UCBTBYT3,UCB3DACC   Direct access device?
         BZ    GETV020             N, next UCB
         CLC   UCBTBYT4,DASDTYPE   Preferred device type?
         BNE   GETV020             N, next UCB
         TM    UCBSTAB,UCBBPUB     PUBLIC volume?
         BZ    GETV020             N
         LA    R15,DASDSIZE        -> full track size for device
         CLC   DASDHTRK,=AL2(0)    Is a half-track blksize avail?
         BE    *+8                 No
         LA    R15,DASDHTRK        Yes, use 1/2 track for device
         CLM   R0,3,0(R15)         Will file blksize fit?
         BH    GETV030             Too large, get another dasd type
*
         ST    R1,DEVINFO          Save ptr to selected dev type
*                                  UCBNAME contains C'cuu'
         MVC   TVOLSER,UCBVOLI     Save selected volser to text unit
         CLI   *,1                 Set CC to non zero
         BR    R14
*
GETV030  EQU   *
         LA    R1,DASDLEN(,R1)     Next DASD device preference
         CLI   0(R1),X'FF'         End of DASD table?
         BER   R14                 Y, no suitable unit found, cc=0
         B     GETV010             Search again
         DROP  R3                  IEFUCBOB
         DROP  R2                  CVT                             v200
         DROP  R1                  DASDTAB
*
*-- Find a volser in the UCBs so we can get its devtype (cant use  v200
*-- DEVTYPE because it is not allocated yet) and determine its     v200
*-- maximum track size.                                            v200
*
*-- Entry:  R1 -> CL'volser' to be located                         v200
*-- Exit:   CC=0 if the volser was not found                       v200
*--         CC<>0 if volume found; and R15 = track size in bytes   v200
*
*-- Uses R15-R3                                                    v200
*
FNDVOL   EQU   *                                                   v200
         SR    R3,R3               Clear for ICM                   v200
         L     R2,16               -> CVT                          v200
         USING CVT,R2                                              v200
         L     R2,CVTILK2          -> UCB Lookup table             v200
         DROP  R2                  CVT                             v200
*
FNDV020  EQU   *                                                   v200
         LA    R2,2(,R2)           -> first table entry            v200
*                                                                  v200
         CLC   0(2,R2),=X'FFFF'    End of UCBs?                    v200
         BE    FNDV090             Y                               v200
         ICM   R3,3,0(R2)          -> UCB                          v200
         BZ    FNDV020             Skip empty table slot           v200
         USING IEFUCBOB,R3                                         v200
         TM    UCBSTAT,UCBONLI     Is device online?               v200
         BZ    FNDV020             N, next UCB                     v200
         TM    UCBTBYT3,UCB3DACC   Direct access device?           v200
         BZ    FNDV020             N, next UCB                     v200
         CLC   UCBVOLI,0(R1)       Selected volser?                v200
         BNE   FNDV020             No, next UCB                    v200
*
         LA    R1,DISKS            -> dasd characteristics table   v200
         USING DASDTAB,R1                                          v200
*
FNDV030  EQU   *                                                   v200
         CLI   0(R1),X'FF'         End of DASD types?              v200
         BE    FNDV090             Cant match volser vs devtype    v200
*
         CLC   UCBTBYT4,DASDTYPE   Match the device type?          v200
         BE    FNDV080             Yes                             v200
         LA    R1,DASDLEN(,R1)     Next DASD device in table       v200
         B     FNDV030             Look again                      v200
*
FNDV080  EQU   *                                                   v200
         SR    R15,R15             Clear for IC                    v200
         ICM   R15,3,DASDSIZE      Get full track size for device  v200
         CLI   *,1                 Set CC to non zero              v200
*
FNDV090  EQU   *                                                   v200
         BR    R14                 Return w CC=0 or CC<>0          v200
         DROP  R3                  IEFUCBOB                        v200
         DROP  R1                  DASDTAB                         v200
*
*
*-- Format VSAM NETSPOOL errors
*
*
FMT000   EQU   *
         STM   R14,R2,PARSA+12         Borrow NJEPAR save area
         LA    R15,0(,R14)  Clear high, Get addr of call to this rtn
         L     R2,NJESA+4              -> system provided FSA
         L     R2,16(,R2)              Get R15's entry point addr
         LA    R2,0(,R2)               Ensure high byte clear
         SR    R15,R2                  Compute offset of call
         MVC   LIST+0(4+L'MSG024T),MSG024 Move msg text
         MVC   LIST+55(8),5(R2)        Move csect name
         TRT   LIST+55(9),BLANK        Look for end of csect name
         MVI   0(R1),C'+'
*
         ST    R15,DBLE                Save call offset to work area
         UNPK  TWRK(5),DBLE+2(3)       Add zones
         TR    TWRK(4),HEXTRAN-240     Display hex
         MVC   1(4,R1),TWRK            Move call offset to msg
*
         LA    R15,NCB1
         UNPK  TWRK(5),NCBRTNCD-NCB(3,R15)  Add zones
         TR    TWRK(4),HEXTRAN-240
         MVC   LIST+35(4),TWRK         Move rtncd/errcd
*
         UNPK  TWRK(3),NCBREQ-NCB(2,R15)  Add zones
         TR    TWRK(2),HEXTRAN-240
         MVC   LIST+45(2),TWRK         Move req code
*
         L     R1,NCBMACAD-NCB(,R15)   Get failing VSAM macro addr
         LA    R1,0(,R1)               Clear high byte
         S     R1,=V(NJESPOOL)         Compute offset into NJESPOOL rtn
         ST    R1,DBLE
         UNPK  TWRK(5),DBLE+2(3)       Add zones
         TR    TWRK(4),HEXTRAN-240     Display hex
         MVC   LIST+50(4),TWRK         Move NJESPOOL offset to msg
*
         LA    R2,LIST
         BAL   R14,PUTLINE
*
FMT090   EQU   *
         LM    R14,R2,PARSA+12         Restore caller regs
         BR    R14                     Return
*
*-- Write a single line to terminal
*
*-- Entry: R2 -> output msg (RDW+msg text)
*-- Exit:  R15 = RC from PUTLINE
*
PUTLINE  EQU   *
         TM    FLAGS3,F3QUIET          QUIET mode enabled?
         BZ    PUT010                  No, proceed
         CLI   3(R2),1                 Suppress this msg in QUIET mode?
         BER   R14                     Yes
*
PUT010   EQU   *
         ST    R14,SV14LN              Save return
         XC    PUTECB,PUTECB           Clear PUTLINE ECB
         L     R15,CPARMS              -> command input CPPL
         USING CPPL,R15
         LA    R1,IOPLAREA             -> IOPL
         USING IOPL,R1
         MVC   IOPLUPT,CPPLUPT         Set UPT ptr
         MVC   IOPLECT,CPPLECT         Set ECT ptr
         DROP  R15                     CPPL
*
         MVC   TWRK(PBL),PB            Move macro model
         PUTLINE PARM=TWRK,            Write a line                    x
               ECB=PUTECB,                                             x
               OUTPUT=((R2),TERM,SINGLE,DATA),                         x
               MF=(E,(1))
         DROP  R1                      IOPL
         L     R14,SV14LN              Load return
         BR    R14
*
*
*-- Write a single line to terminal and prompt for response
*
*-- Entry: OLDMSGAD points to output message
*-- Exit:  R15 = RC from PUTGET
*--        PGPBIBUF -> input data (if any)
*
PUTGET   EQU   *
         ST    R14,SV14LN              Save return
         XC    PUTECB,PUTECB
         L     R15,CPARMS              -> command input CPPL
         USING CPPL,R15
         LA    R1,IOPLAREA             -> IOPL
         USING IOPL,R1
         MVC   IOPLUPT,CPPLUPT         Set UPT ptr
         MVC   IOPLECT,CPPLECT         Set ECT ptr
         DROP  R15                     CPPL
*
         MVC   MACLIST(PGTL),PGT       move macro model
         PUTGET PARM=MACLIST,                                          x
               ECB=PUTECB,                                             x
               OUTPUT=(OLD,SINGLE,PROMPT),                             x
               TERMPUT=(EDIT,WAIT,NOHOLD,NOBREAK),                     x
               TERMGET=(EDIT,WAIT),                                    x
               MF=(E,(1))
         DROP  R1                      IOPL
*
         LA    R5,MACLIST
         USING PGPB,R5
         L     R1,PGPBIBUF             -> input buffer acquired
         DROP  R5
         L     R14,SV14LN              Load return
         BR    R14
*
*-- Get status of NJE38
*
*-- Entry: R1=0 (no spool dsn needed), or, R1-> 44-char spool DSN area
*-- Exit:  RC=0  NJE38 is active; R1-> NJE38 CSA block
*--        RC<>0 NJE is not active.
*
CHK000   EQU   *
         LA    R1,TDSNAME              => where to place spool DSN v210
         L     R15,=V(NJESYS)          -> ENQ finder               v210
         BALR  R14,R15                 Check if NJE38 already act  v210
         LTR   R15,R15                 Set CC (RC=0 NJE38 active)  v210
         BNZR  R2                      Return if NJE38 inactive    v210
         MVC   LCLNODE,NJ38NODE-NJ38CSA(R1)  Save off lcl node namev210
         MVC   DEFUSER,NJ38DUSR-NJ38CSA(R1)  Save off default user v210
         BR    R2                      Return; NJE38 active        v210
*
         LTORG
*
PB       PUTLINE MF=L
PBL      EQU   *-PB
PGT      PUTGET MF=L
PGTL     EQU   *-PGT
*
NJE38Q   DC    CL8'NJE38'              QNAME
NJERCON  DC    CL8'NJEINIT'            RNAME (first 8 bytes)
*
*
*
BLANKS   DC    CL136' '                                            v200
NONBLANK DC    64X'FF',X'00',191X'FF'  TR Table to locate nonblank
BLANK    DC    64X'00',X'FF',191X'00'  TR Table to locate blanks
DOTS     DC    75X'00',X'FF',180X'00'  TR Table to locate '.' char
HEXTRAN  DC    CL16'0123456789ABCDEF'  Translate table
*
*-- RECEIVE messages
*
*-- Note:  a '1' after the length indicates suppress this msg if QUIET
*
MSGBLNK  DC    Y(4+L'MSGBLNKT,1)
MSGBLNKT DC    C' '
*
MSG000   DC    Y(4+L'MSG000T,1)
MSG000T  DC    C'NJE38 RECEIVE &VERS'
*
MSG001   DC    Y(4+L'MSG001T,0)
MSG001T  DC    C'Error parsing RECEIVE command parameters. IKJPARS RC=yx
               y (dec)'
*                456789012345678901234567890123456789012345678901234567
MSG002   DC    Y(4+L'MSG002T,0)
MSG002T  DC    C' '                   UNUSED - AVAILABLE
*
MSG003   DC    Y(4+L'MSG003T,0)
MSG003T  DC    C'Invalid or unsupported NETDATA detected; error code x,*
                record '
*
MSG004   DC    Y(4+L'MSG004T,0)
MSG004T  DC    C'Member xxxxxxxx was not found'
*                456789012345678901234567890123456789012345678901234567
*
MSG005   DC    Y(4+L'MSG005T,0)
MSG005T  DC    C'Unexpected end of file encountered reading '
*
MSG006   DC    Y(4+L'MSG006T,0)
MSG006T  DC    C'The RECEIVE command is not APF-authorized'
*
MSG007   DC    Y(4+L'MSG007T,0)
MSG007T  DC    C'I/O error reading '
*
MSG008   DC    Y(4+L'MSG008T,0)
MSG008T  DC    C'Specified INDATASET does not contain NETDATA formattedx
                records'
*
MSG009   DC    Y(4+L'MSG009T,0)
MSG009T  DC    C'Specified INDATASET must be LRECL=80, RECFM=F or FB'
*
MSG010   DC    Y(4+L'MSG010T,0)
MSG010T  DC    C'No files are available to receive'
*
MSG011   DC    Y(4+L'MSG011T,0)
MSG011T  DC    C'Specified file number does not exist'
*
MSG012   DC    Y(4+44+L'MSG012T,0)
MSG012T  DC    C'Allocation error xxxxxxxx, DSN='
*
MSG013   DC    Y(4+L'MSG013T,0)
MSG013T  DC    C'NJE38 is not active'
*
MSG014   DC    Y(4+L'MSG014T,1)
MSG014T  DC    C'Receiving '
*
MSG015   DC    Y(4+L'MSG015T,1)
MSG015T  DC    C' Enter receive parameters or ''PURGE'' or ''END'' +'
*
MSG016   DC    Y(4+L'MSG016T,0)
MSG016T  DC    C'Cannot receive file destined for another user'
*
MSG017   DC    Y(4+L'MSG017T,1)
MSG017T  DC    C'Receive into '
*
MSG018   DC    Y(4+L'MSG018T,0)
MSG018T  DC    C'Receive failed due to IEBCOPY RC=xx'
*                456789012345678901234567890123456789012345678901234567
*
MSG019   DC    Y(4+L'MSG019T,0)
MSG019T  DC    C'RECEIVE ended with no action taken'
*
MSG020   DC    Y(4+L'MSG020T,1)
MSG020T  DC    C' Enter receive parameters or ''END'' +'
*
MSG021   DC    Y(4+L'MSG021T,0)
MSG021T  DC    C'Member name ignored'
*
MSG022   DC    Y(4+L'MSG022T,0)
MSG022T  DC    C'No suitable PUBLIC volume found that can contain this x
               dataset'
*
MSG023   DC    Y(4+L'MSG023T,0)
MSG023T  DC    C'The BLKSIZE or LRECL of the received file exceeds the x
               32760-byte MVS limit'
*
MSG024   DC    Y(4+L'MSG024T,0)
MSG024T  DC    C'ERROR:  NETSPOOL RTNCD/ERRCD=X''0000'',REQ=01,O=1234,Mx
               MMMMMMM     '
*
MSG025   DC    Y(4+L'MSG025T,0)
MSG025T  DC    C'Unable to open NETSPOOL. Run IDCAMS VERIFY against thex
                NETSPOOL dataset'
*
MSG026   DC    Y(4+L'MSG026T,0)
MSG026T  DC    C'Error in terminal prompt message.  PUTGET RC=yy (dec)'
*                456789012345678901234567890123456789012345678901234567
*
MSG027   DC    Y(4+L'MSG027T,0)
MSG027T  DC    C' exists'
*
MSG028   DC    Y(4+L'MSG028T,1)
MSG028T  DC    C'File(xxxx) purged from NJE38 spool'
*
MSG029   DC    Y(4+L'MSG029T,0)
MSG029T  DC    C'RECEIVE ended without receiving anything'
*
MSG030   DC    Y(4+L'MSG030T,0)
MSG030T  DC    C'RECEIVE halted; unsupported message text (INMTEXT) detx
               ected'
*
MSG031   DC    Y(4+L'MSG031T,0)
MSG031T  DC    C' does not exist'
*
MSG032   DC    Y(4+L'MSG032T,0)
MSG032T  DC    C'Access to the NETSPOOL dataset denied due to security x
               settings'
*
MSG033   DC    Y(4+L'MSG033T,0)                                    v200
MSG033T  DC    C'Volume unavailable or conflicting with specified UNIT x
               name'                                               v200
*
MSG034   DC    Y(4+L'MSG034T,0)                                    v200
MSG034T  DC    C'The specified UNIT name is not defined in the system' x
                                                                   v200
*
MSG035   DC    Y(4+L'MSG035T,0)                                    v200
MSG035T  DC    C'Incoming file is a PDSE Program Library which cannot bx
               e supported'                                        v200
*
MSG036   DC    Y(4+L'MSG036T,0)                                    v200
MSG036T  DC    C'The selected volume is not online'                v200
*
MSG037   DC    Y(4+L'MSG037T,0)                                    v200
MSG037T  DC    C'The incoming file block size is too large to fit on thx
               e selected volume'                                  v200
*
MSG038   DC    Y(4+L'MSG038T,0)                                    v222
MSG038T  DC    C'The incoming file contains logical records that exceedx
                the LRECL of the dataset'                          v222
*                                                                       NJE00250
*                                                                       NJE00250
* DASD Characteristics in order of selection preference                 NJE00250
*                                                                       NJE00250
* NOTE:  3380 DASD exist in this table twice.  The reason for           NJE00250
*        this is to allow a better identification of the DASD           NJE00250
*        type required by "GETVOL" based on the received file's         NJE00250
*        BLKSIZE.  The file could be half-track blocked, so we          NJE00250
*        need to account for that in selecting a device type.           NJE00250
*        However, the file could be using a BLKSIZE that is             NJE00250
*        larger than the half track size despite the inefficiency.      NJE00250
*        For these cases, the last 3380 entry does not have             NJE00250
*        a half-track size value, allowing the GETVOL search to
*        succeed using the full track size which would accomodate
*        any MVS BLKSIZE that could be received.
*
*                                                                       NJE00250
DISKS    EQU   * TYP    CYLS    TRKS     BYTES    1/2-TRK
         DC    X'0E',AL2(885),AL1(15),AL2(47476),AL2(23476)  3380 A/D/J
         DC    X'0B',AL2(555),AL1(30),AL2(19069),AL2(0)      3350
         DC    X'0C',AL2(959),AL1(12),AL2(35616),AL2(17600)  3375
         DC    X'0F',AL2(1113),AL1(15),AL2(56664),AL2(27998) 3390-1
         DC    X'0D',AL2(808),AL1(19),AL2(13030),AL2(0)      3330-11
         DC    X'09',AL2(404),AL1(19),AL2(13030),AL2(0)      3330-1
         DC    X'0A',AL2(696),AL1(12),AL2(8368),AL2(0)       3340-70
         DC    X'08',AL2(200),AL1(20),AL2(7294),AL2(0)       2314
         DC    X'0E',AL2(885),AL1(15),AL2(47476),AL2(0)      3380 A/D/J
         DC    X'FF'                End of table
*                                                                       NJE00250
DASDTAB  DSECT
DASDTYPE DS    X                    Dasd UCB device type code
DASDCYLS DS    AL2                  Number of cylinders
DASDTRKS DS    AL1                  Number of tracks
DASDSIZE DS    AL2                  Bytes per track
DASDHTRK DS    AL2                  Bytes per half-track block or 0
DASDLEN  EQU   *-DASDTAB            Size of one DASDTAB entry
*
*                                                                       NJE00250
*********************
*  N J E N O T      *               NJENOT tells the user the chosen
*                   *               DSN of the file and prompts for
*  User notify and  *               changes
*  prompt           *
*                   *
*********************
*
*
NJENOT   CSECT
         B     28(,R15)               BRANCH AROUND EYECATCHERS
         DC    AL1(23)                LENGTH OF EYECATCHERS
         DC    CL9'NJENOT'
         DC    CL9'&SYSDATE'
         DC    CL5'&SYSTIME'
*
         STM   R14,R12,12(R13)         Save Regs                        NJE00050
         LR    R12,R15                 Base                             NJE00060
         USING NJENOT,R12                                               NJE00070
         USING NJEWK,R10
         ST    R13,NOTSA+4             SAVE prv S.A. ADDR               NJE00080
         LA    R2,NOTSA                -> my save area
         ST    R2,8(,R13)              Plug it into prior SA
         LR    R13,R2
*
NOT000   EQU   *
         BAL   R14,BDS000              Build final dataset name
         BAL   R14,NTF000              Build notification msg
*
         TM    FLAGS2,F2FEND           END forced previously?
         BO    XITNOT04                Force 'END' again
*
         LA    R2,LIST                 -> MSG014 receiving dataset...
         BAL   R14,PUTLINE             Notify user
*
         TM    FLAGS1,F1BATCH          Are we in BATCH mode?
         BO    NOT090                  Yes, special handling
         TM    FLAGS3,F3NPRMPT         Are we in NOPROMPT mode?
         BO    NOT090                  Yes, special handling
*
         LA    R1,MSG015               -> enter parameters prompt
         TM    FLAGS3,F3INDS           Was INDATASET specified?
         BZ    NOT010                  No
         LA    R1,MSG020               Use MSG020 if INDS in use
*
NOT010   EQU   *
         ST    R1,OLDMSGAD             Set it in the OLD
         BAL   R14,PUTGET              Prompt the user
         C     R15,=F'8'               PUTGET ECB posted (attn recv'd)?
         BE    XITNOT04                Yes, treat as 'END' specified
         LTR   R0,R15                  PUTGET RC to R0
         BNZ   XITNOT16                Exit if putget error
*
         ST    R1,OLDMSGAD             Temp save of PUTGET input ptr
         LA    R0,4                    Code 4: use prompt parameters
*                                      R1 -> PUTGET input buffer
         L     R15,=A(NJEPAR)          -> parse routine
         BALR  R14,R15
         LR    R5,R15                  Any errors to R5
*
         TM    FLAGS4,F4ATTN           Was ATTN pressed?           v201
         BO    XITNOT04                Y, immediate exit           v201
*
         L     R1,OLDMSGAD             -> PUTGET input buffer
         LH    R0,0(,R1)               Get length of area
         O     R0,=X'01000000'         Set SP=1
         FREEMAIN R,LV=(0),A=(1)       Free the PUTGET msg buffer
*
         LTR   R0,R5                   Now put IKJPARS RC in R0
         BNZ   XITNOT12                Display IJKPARS RC
         TM    FLAGS4,F4END            Was END specified?
         BO    XITNOT04                Exit if END
         TM    FLAGS4,F4PURGE          Was PURGE specified?
         BO    XITNOT08                Exit if PURGE
         B     XITNOT00
*
*-- If running BATCH, allow one trip through here to exit cleanly
*-- to simulate pressing "enter" with no parameters.  On all
*-- subsquent calls to NJENOT, F2END will be set (if BATCH) so
*-- we can force END in order to prevent looping in batch.
*
NOT090   EQU   *
         OI    FLAGS2,F2FEND           Indic force END from now on
         B     XITNOT00                Allow null prompt this time
*
*-- Build DSN
*
*-- DSN Strategy:  The DSN from the NETDATA will be extracted and
*-- the first qualifer eliminated, unless the DSN is only one
*-- qualifier.  Then, the remaining part of that DSN will be appended
*-- to the receiving user's userid  (the userid will be the new
*-- first qualifier.
*
*-- If the incoming file is a flat file (not NETDATA) the DSNAME is
*-- manufactured from the filename and filetype fields of the TAG data.
*
*-- If DATASET was specified on the command line (F3DS=1) then we
*-- will attempt to use that as is and exit the build DS routine.
*
BDS000   EQU   *
         TM    FLAGS3,F3DS             Is final dataset already set?
         BOR   R14                     Exit if we already have it
         MVC   FINALDS,BLANKS          Init
         MVC   FINALDS(8),USERID       Move userid
         TM    FLAGS2,F2FLAT           Is incoming file a flat file?
         BO    BDS020                  Yes, use tag data
*
         LA    R7,INMF02A              -> first INMR02 results
         USING INMFIELD,R7
*
         SR    R4,R4                   Clear for IC
         ICM   R4,3,DSNAME             Get NETDATA DSN length
         LA    R1,DSNAME+2             Assume DSN has 1 qualifier
         TRT   DSNAME+2(10),DOTS       Look for end of 1st qualifier
         BZ    BDS010                  Branch if only 1 qualifier
         LA    R1,1(,R1)               Skip the delim after 1st qualifr
         LR    R0,R1                   Copy position
         LA    R2,DSNAME+2             Start of DSN
         SR    R0,R2                   Compute length we are skipping
         SR    R4,R0                   Reduce remaining DSN length
*
BDS010   EQU   *
         LR    R3,R1                   Save start of NETDATA DSN
         TRT   FINALDS(9),BLANK        Look for end of userid
         MVI   0(R1),C'.'              Add delimiter
         LA    R1,1(,R1)               -> next available byte
         LA    R2,FINALDS              -> start of userid
         LR    R0,R1                   Copy next available byte addr
         SR    R0,R2                   Compute userid. length
         LA    R15,43                  Total DSN length -1 for execute
         SR    R15,R0                  Compute remaining available
         CR    R15,R4                  Use lesser remaining length
         BL    *+6                     Br if TDSNAME length is less
         LR    R15,R4                  No, NETDATA DSN len is less
         EX    R15,MVCDSN              Move the rest of it
*MVCDSN  MVC   0(0,R1),0(R3)
         CLI   FINALDS+43,C'.'         Last char of DSN is a delim?
         BNER  R14                     No
         MVI   FINALDS+43,C' '         Blank it out
         DROP  R7                      INMFIELD
         BR    R14                     Return
*
BDS020   EQU   *
         LA    R4,TAGDATA              -> Spool file's tag data
         USING TAG,R4
*
         LA    R1,FINALDS              -> final dsname area
         TRT   0(9,R1),BLANK           Look for end of userid
         MVI   0(R1),C'.'              Add delimiter
         LA    R1,1(,R1)               -> next available byte
         MVC   0(8,R1),TAGNAME         Insert tag's file name
         TRT   0(9,R1),BLANK           Find the end of it
         MVI   0(R1),C'.'              Add delimiter
         LA    R1,1(,R1)               -> next available byte
         MVC   0(8,R1),TAGTYPE         Insert tag's file name
         DROP  R4                      TAG
         BR    R14                     Return
*
*
MVCDSN   MVC   0(0,R1),0(R3)           executed instr
*
*
*-- Build msg containing incoming dataset name from the netdata
*
NTF000   EQU   *
         MVC   LIST,BLANKS
         MVC   LIST(4+L'MSG014T),MSG014  'Receiving ...'
         LA    R1,LIST+4+L'MSG014T     -> next available byte
         MVI   0(R1),C''''             Move apost
         LA    R1,1(,R1)               Next byte
*
         TM    FLAGS2,F2FLAT           Is incoming file a flat file?
         BO    NTF020                  Yes, use tag data
*
         LA    R7,INMF02A              -> First INMR02 data
         USING INMFIELD,R7
         MVC   0(44,R1),DSNAME+2       Move incoming DSN
         TRT   0(45,R1),BLANK          Look for end of DSN
         MVI   0(R1),C''''             Move apost
         LA    R1,2(,R1)               -> skip over apost + 1 blank
         MVC   0(4,R1),=C'from'
         LA    R1,5(,R1)               -> where to put node id
         LA    R7,INMF01               -> INMR01 data
         USING INMFIELD,R7
         MVC   0(8,R1),FNODE+2         Move from node name
         TRT   0(9,R1),BLANK           Look for end of nodeid
         MVI   0(R1),C'('              Insert (
         MVC   1(8,R1),FUSER+2         Move from user name
         DROP  R7                      INMFIELD
         TRT   1(9,R1),BLANK           Look for end of userid
         MVI   0(R1),C')'              Insert )
         LA    R1,1(,R1)               -> end of msg
         LA    R0,LIST                 -> start of msg
         ST    R0,OLDMSGAD             Set msg ptr
         SR    R1,R0                   Compute length of msg
         STH   R1,LIST                 Set length of msg for PUTGET
         BR    R14                     Return
*
*-- Build msg containing incoming dataset name from the tag data
*
NTF020   EQU   *
         LA    R4,TAGDATA              -> Spool file's tag data
         USING TAG,R4
*
         MVC   0(8,R1),TAGNAME         Insert tag's file name
         TRT   0(9,R1),BLANK           Find the end of it
         LA    R1,1(,R1)               -> next available byte
         MVC   0(8,R1),TAGTYPE         Insert tag's file name
         TRT   0(9,R1),BLANK           Find the end of it
         MVI   0(R1),C''''             Move apost
         LA    R1,2(,R1)               -> skip over apost + 1 blank
         MVC   0(4,R1),=C'from'
         LA    R1,5(,R1)               -> where to put node id
         MVC   0(8,R1),TAGINLOC        Move from node name
         TRT   0(9,R1),BLANK           Look for end of nodeid
         MVI   0(R1),C'('              Insert (
         MVC   1(8,R1),TAGINVM         Move from user name
         DROP  R4                      TAG
         TRT   1(9,R1),BLANK           Look for end of userid
         MVI   0(R1),C')'              Insert )
         LA    R1,1(,R1)               -> end of msg
         LA    R0,LIST                 -> start of msg
         ST    R0,OLDMSGAD             Set msg ptr
         SR    R1,R0                   Compute length of msg
         STH   R1,LIST                 Set length of msg for PUTGET
         BR    R14                     Return
*
*-- Exit
*
XITNOT00 EQU   *
         SR    R0,R0               Set secondary RC=0;
         SR    R15,R15             Set RC=0; normal
         B     XITNOT
*
XITNOT04 EQU   *
         SR    R0,R0               Set secondary RC=0;
         LA    R15,4               Set RC=4;  END specified
         B     XITNOT
*
XITNOT08 EQU   *
         SR    R0,R0               Set secondary RC=0;
         LA    R15,8               Set RC=8;  PURGE specified
         B     XITNOT
*
XITNOT12 EQU   *
         LA    R15,12              Set RC=12; secondary is IKJPARS RC
         B     XITNOT
*
XITNOT16 EQU   *
         LA    R15,16              Set RC=16; secondary is PUTGET RC
*
XITNOT   EQU   *
         L     R13,4(,R13)         -> prev s.a.
         L     R14,12(,R13)        Load r14
         LM    R1,R12,24(R13)      Reload callers regs
         BR    R14                 Return with RCs in R0/R15
*
         LTORG
*                                                                       NJE00250
*                                                                       NJE00250
*********************
*  N J E D Y N      *               NJEDYN handles the various
*                   *               dynamic allocations required
*  Handle DYNALLOC  *               and their unallocations as well.
*                   *
*********************
*
         USING INMFIELD,R7          -> R7 at entry
*
NJEDYN   CSECT
         B     28(,R15)               BRANCH AROUND EYECATCHERS
         DC    AL1(23)                LENGTH OF EYECATCHERS
         DC    CL9'NJEDYN'
         DC    CL9'&SYSDATE'
         DC    CL5'&SYSTIME'
*
         STM   R14,R12,12(R13)         Save Regs                        NJE00050
         LR    R12,R15                 Base                             NJE00060
         USING NJEDYN,R12                                               NJE00070
         USING NJEWK,R10
         ST    R13,DYNSA+4             SAVE prv S.A. ADDR               NJE00080
         LA    R1,DYNSA                -> my save area
         ST    R1,8(,R13)              Plug it into prior SA
         LR    R13,R1
*
         MVC   LS99RB,CPS99RB          init THE S99RB
         LA    R1,LS99RB               -> S99RB
         USING S99RB,R1
         ST    R1,LS99PTR              Set parameter word
         OI    LS99PTR,X'80'           Set VL
         LA    R6,TXTPTRS              -> start of text unit list
         ST    R6,S99TXTPP             Put in S99RB
         DROP  R1                      S99RB
*
UNDYN    EQU   0                   00  unallocate DDNAME
DYNINMCP EQU   4                   04  Allocate INMCOPY dataset
DYNSYSIN EQU   8                   08  Allocate SYSIN for IEBCOPY
DYNSYSPR EQU   12                  0C  Allocate SYSPRINT for IEBCOPY
DYNFINAL EQU   16                  10  Allocate final dataset IEBCOPY
DYNSYSU3 EQU   20                  14  Allocate SYSUT3 IEBCOPY
DYNINDS  EQU   24                  18  Allocate INDATASET
DYNETSPL EQU   28                  1C  Allocate NETSPOOL
*
         LR    R5,R0                   Copy action code
         B     DYN000(R5)              Branch into table
*
DYN000   B     DYN010              00  Perform DDNAME Unallocation
         B     DYN100              04  Allocate INMCOPY dataset
         B     DYN200              08  Allocate SYSIN for IEBCOPY
         B     DYN300              0C  Allocate SYSPRINT for IEBCOPY
         B     DYN400              10  Allocate final dataset IEBCOPY
         B     DYN500              14  Allocate SYSUT3 IEBCOPY
         B     DYN600              18  Allocate INDATASET
         B     DYN700              1C  Allocate NETSPOOL
*
DYN010   EQU   *
         MVC   UTXT,UTXTD              Init text unit
         LA    R1,LS99RB               -> S99RB
         USING S99RB,R1
         MVI   S99VERB,S99VRBUN        Set verb code to unallocation
         DROP  R1                      S99RB
*
         LA    R0,UTXT                 -> UNALLOC DD text unit
         ST    R0,0(,R6)               Plug into ptr list
         OI    0(R6),X'80'             End the parameter list
         B     DYN900                  Deallocate the DD
*
*-- Dataset created for INMCOPY INMR02 control record
*
*-- If there is no DSN, this is a temporary 'unloaded pds' dataset and
*-- no volser is used and can be allocated on a storage volume.
*
*   Equivalent JCL:
*   //SYS00000 DD DISP=(NEW,DELETE),UNIT=SYSDA,
*   //            SPACE=(blk,(pri,sec)),
*   //            DCB=(BLKSIZE=blk,LRECL=l,RECFM=f,DSORG=PS)
*
*-- If there is a DSN, then this is a final dataset, so use the
*-- dsname text unit and place it on the volser of choise.
*
*   Equivalent JCL:
*   //SYS00000 DD DISP=(NEW,CATLG),UNIT=SYSDA,
*   //            SPACE=(blk,(pri,sec)),
*   //            DCB=(BLKSIZE=blk,LRECL=l,RECFM=f,DSORG=PS),
*   //            DSN=dsname,VOL=SER=volser
*
DYN100   EQU   *
         MVC   TXT01,TXT01D            Init from the models
         MVC   TXT02,TXT02D
         MVC   TXT03,TXT03D
         MVC   TXT04,TXT04D
         MVC   TXT05,TXT05D
         MVC   TXT06,TXT06D
         MVC   TXT07,TXT07D
         MVC   TXT09,TXT09D
         MVC   TXT10,TXT10D
         MVC   TXT12,TXT12D
         MVC   TXT13,TXT13D
         MVC   TXT14,TXT14D
         MVC   TXT15,TXT15D
*
         LA    R0,TXT01                -> Return DDNAME text unit
         ST    R0,0(,R6)               Plug into ptr list
         LA    R6,4(,R6)               -> next ptr list slot
         LA    R0,TXT03                -> DISP text unit 1
         ST    R0,0(,R6)               Plug into ptr list
         LA    R6,4(,R6)               -> next ptr list slot
         LA    R0,TXT04                -> DISP text unit 2
         ST    R0,0(,R6)               Plug into ptr list
         LA    R6,4(,R6)               -> next ptr list slot
         LA    R0,TXT12                -> BLKSIZE text unit
         ST    R0,0(,R6)               Plug into ptr list
         LA    R6,4(,R6)               -> next ptr list slot
         LA    R0,TXT05                -> BLKLEN text unit
         ST    R0,0(,R6)               Plug into ptr list
         LA    R6,4(,R6)               -> next ptr list slot
         LA    R0,TXT14                -> LRECL text unit
         ST    R0,0(,R6)               Plug into ptr list
         LA    R6,4(,R6)               -> next ptr list slot
         LA    R0,TXT15                -> RECFM text unit
         ST    R0,0(,R6)               Plug into ptr list
         LA    R6,4(,R6)               -> next ptr list slot
         LA    R0,TXT06                -> PRIMARY text unit
         ST    R0,0(,R6)               Plug into ptr list
         LA    R6,4(,R6)               -> next ptr list slot
         LA    R0,TXT07                -> SECONDARY text unit
         ST    R0,0(,R6)               Plug into ptr list
         LA    R6,4(,R6)               -> next ptr list slot
         LA    R0,TXT13                -> DSORG text unit
         ST    R0,0(,R6)               Plug into ptr list
         LA    R6,4(,R6)               -> next ptr list slot
         LA    R0,TXT10                -> UNIT text unit
         ST    R0,0(,R6)               Plug into ptr list
*
         CLC   DSNAME(2),=AL2(0)       Was there a DSN?
         BNE   DYN120                  Yes, plug DSN & VOL text unit
         MVI   TXT04+6,X'04'           No, its a temp; set DISP=,DELETE
*v223    B     DYN190                  Then skip DSN text unit     v200
         B     DYN130                  Process remaining txt units v223
*
DYN120   EQU   *
         LA    R6,4(,R6)               -> next ptr list slot
         LA    R0,TXT02                -> DSN text unit
         ST    R0,0(,R6)               Plug into ptr list
*
DYN130   EQU   *                                                   v223
         TM    FLAGS2,F2UNIT           Was UNIT specified?         v200
         BZ    DYN170                  No, leave default           v200
         MVC   TUNIT,USRUNIT           Set user's unit name        v200
         TM    FLAGS3,F3VOLSER         Did user specify VOLSER?    v200
         BO    DYN170                  Yes, use what he coded      v200
         TM    FLAGS4,F4VOLSER         Did user specify VOLSER?    v200
         BO    DYN170                  Yes, use what he coded      v200
         B     DYN190                  UNIT without VOLSER specif'dv200
*
DYN170   EQU   *                       Use specified VOL or GETVOL v200
         LA    R6,4(,R6)               -> next ptr list slot
         LA    R0,TXT09                -> VOLSER text unit
         ST    R0,0(,R6)               Plug into ptr list
*
DYN190   EQU   *                                                   v200
         OI    0(R6),X'80'             End the parameter list
         B     DYN900                  Go allocate
*
*-- SYSIN for IEBCOPY
*
*   Equivalent JCL:
*   //SYS00000 DD DUMMY
*
*
DYN200   EQU   *
         MVC   TXT01,TXT01D            Init from the models
         MVC   TXT16,TXT16D
         LA    R0,TXT01                -> return DDNAME
         ST    R0,0(,R6)               Plug into ptr list
         LA    R6,4(,R6)               -> next ptr list slot
         LA    R0,TXT16                -> DUMMY
         ST    R0,0(,R6)               Plug into ptr list
         OI    0(R6),X'80'             End the parameter list
         B     DYN900                  Go allocate
*
*-- SYSPRINT for IEBCOPY
*
*   Equivalent JCL:
*   //SYS00000 DD SYSOUT=*,TERM=TS
*
DYN300   EQU   *
         MVC   TXT01,TXT01D            Init from the models
         MVC   TXT16,TXT16D
         MVC   TXT17,TXT17D
         MVC   TXT18,TXT18D
         LA    R0,TXT01                -> return DDNAME
         ST    R0,0(,R6)               Plug into ptr list
*
         TM    FLAGS3,F3QUIET          QUIET mode enabled?
         BO    DYN310                  Yes, use DUMMY
         LA    R6,4(,R6)               -> next ptr list slot
         LA    R0,TXT17                -> SYSOUT=*
         ST    R0,0(,R6)               Plug into ptr list
         LA    R6,4(,R6)               -> next ptr list slot
         LA    R0,TXT18                -> TERM=TS
         ST    R0,0(,R6)               Plug into ptr list
         B     DYN320
*
DYN310   EQU   *
         LA    R6,4(,R6)               -> next ptr list slot
         LA    R0,TXT16                -> DUMMY
         ST    R0,0(,R6)               Plug into ptr list
*
DYN320   EQU   *
         OI    0(R6),X'80'             End the parameter list
         B     DYN900                  Go allocate
*
*-- Final dataset that IEBCOPY will load
*
*   Equivalent JCL:
*   //SYS00000 DD DISP=(NEW,CATLG),UNIT=SYSDA,
*   //            SPACE=(blk,(pri,sec)),
*   //            DCB=(BLKSIZE=blk,LRECL=l,RECFM=f,DSORG=PO),
*   //            DSN=dsname,VOL=SER=volser
*
DYN400   EQU   *
         MVC   TXT01,TXT01D            Init from the models
         MVC   TXT02,TXT02D
         MVC   TXT03,TXT03D
         MVC   TXT04,TXT04D
         MVC   TXT05,TXT05D
         MVC   TXT06,TXT06D
         MVC   TXT07,TXT07D
         MVC   TXT08,TXT08D
         MVC   TXT09,TXT09D
         MVC   TXT10,TXT10D
         MVC   TXT12,TXT12D
         MVC   TXT13,TXT13D
         MVC   TXT14,TXT14D
         MVC   TXT15,TXT15D
*
         LA    R0,TXT01                -> Return DDNAME text unit
         ST    R0,0(,R6)               Plug into ptr list
         LA    R6,4(,R6)               -> next ptr list slot
         LA    R0,TXT03                -> DISP text unit 1
         ST    R0,0(,R6)               Plug into ptr list
         LA    R6,4(,R6)               -> next ptr list slot
         LA    R0,TXT04                -> DISP text unit 2
         ST    R0,0(,R6)               Plug into ptr list
         LA    R6,4(,R6)               -> next ptr list slot
         LA    R0,TXT12                -> BLKSIZE text unit
         ST    R0,0(,R6)               Plug into ptr list
         LA    R6,4(,R6)               -> next ptr list slot
         LA    R0,TXT05                -> BLKLEN text unit
         ST    R0,0(,R6)               Plug into ptr list
         LA    R6,4(,R6)               -> next ptr list slot
         LA    R0,TXT14                -> LRECL text unit
         ST    R0,0(,R6)               Plug into ptr list
         LA    R6,4(,R6)               -> next ptr list slot
         LA    R0,TXT15                -> RECFM text unit
         ST    R0,0(,R6)               Plug into ptr list
         LA    R6,4(,R6)               -> next ptr list slot
         LA    R0,TXT06                -> PRIMARY text unit
         ST    R0,0(,R6)               Plug into ptr list
         LA    R6,4(,R6)               -> next ptr list slot
         LA    R0,TXT07                -> SECONDARY text unit
         ST    R0,0(,R6)               Plug into ptr list
         LA    R6,4(,R6)               -> next ptr list slot
         LA    R0,TXT08                -> DIRECTORY BLOCKS text unit
         ST    R0,0(,R6)               Plug into ptr list
         LA    R6,4(,R6)               -> next ptr list slot
         LA    R0,TXT13                -> DSORG text unit
         ST    R0,0(,R6)               Plug into ptr list
         LA    R6,4(,R6)               -> next ptr list slot
         LA    R0,TXT02                -> DSN text unit
         ST    R0,0(,R6)               Plug into ptr list
         LA    R6,4(,R6)               -> next ptr list slot       v200
         LA    R0,TXT10                -> UNIT text unit           v200
         ST    R0,0(,R6)               Plug into ptr list          v200
*
         TM    FLAGS2,F2UNIT           Was UNIT specified?         v200
         BZ    DYN470                  No, leave default           v200
         MVC   TUNIT,USRUNIT           Set user's unit name        v200
         TM    FLAGS3,F3VOLSER         Did user specify VOLSER?    v200
         BO    DYN470                  Yes, use what he coded      v200
         TM    FLAGS4,F4VOLSER         Did user specify VOLSER?    v200
         BO    DYN470                  Yes, use what he coded      v223
         B     DYN490                  UNIT without VOLSER specif'dv200
*
DYN470   EQU   *                       Use specified VOL or GETVOL v200
         LA    R6,4(,R6)               -> next ptr list slot
         LA    R0,TXT09                -> VOLSER text unit
         ST    R0,0(,R6)               Plug into ptr list
*
DYN490   EQU   *                                                   v200
         OI    0(R6),X'80'             End the parameter list
         B     DYN900                  Go allocate
*
*-- SYSUT3 for IEBCOPY
*
*   Equivalent JCL:
*   //SYS00000 DD DISP=(NEW,DELETE),UNIT=SYSDA,
*   //            SPACE=(CYL,5)
*
DYN500   EQU   *
         MVC   TXT01,TXT01D            Init from the models
         MVC   TXT03,TXT03D            DISP 1
         MVC   TXT04,TXT04D            DISP 2
         MVC   TXT06,TXT06D            PRIME
         MVC   TXT10,TXT10D            UNIT
         MVC   TXT19,TXT19D            CYL
*
         MVI   TXT04+6,X'04'           Adjust to DISP=,DELETE
         MVC   TXT06+6(3),=XL3'05'     5 cylinders
*
         LA    R0,TXT01                -> return DDNAME
         ST    R0,0(,R6)               Plug into ptr list
         LA    R6,4(,R6)               -> next ptr list slot
         LA    R0,TXT03                -> DISP=NEW
         ST    R0,0(,R6)               Plug into ptr list
         LA    R6,4(,R6)               -> next ptr list slot
         LA    R0,TXT04                -> DISP=,DELETE
         ST    R0,0(,R6)               Plug into ptr list
         LA    R6,4(,R6)               -> next ptr list slot
         LA    R0,TXT06                -> Primary space
         ST    R0,0(,R6)               Plug into ptr list
         LA    R6,4(,R6)               -> next ptr list slot
         LA    R0,TXT10                -> UNIT
         ST    R0,0(,R6)               Plug into ptr list
         LA    R6,4(,R6)               -> next ptr list slot
         LA    R0,TXT19                -> SPACE CYL
         ST    R0,0(,R6)               Plug into ptr list
         OI    0(R6),X'80'             End the parameter list
         B     DYN900                  Go allocate
*
*-- Dataset INDATASET
*
*   Equivalent JCL:
*   //SYS00000 DD DISP=SHR,DSNAME=indataset
*
DYN600   EQU   *
         MVC   TXT01,TXT01D            Init from the models
         MVC   TXT02,TXT02D
         MVC   TXT03,TXT03D
         MVC   TXT13,TXT13D
*
         MVI   TXT03+6,X'08'           set DISP=SHR
         MVC   TXT13(2),=Y(DALRTORG)   set RETURN DSORG
*
         LA    R0,TXT01                -> return DDNAME
         ST    R0,0(,R6)               Plug into ptr list
         LA    R6,4(,R6)               -> next ptr list slot
         LA    R0,TXT03                -> DISP=SHR
         ST    R0,0(,R6)               Plug into ptr list
         LA    R6,4(,R6)               -> next ptr list slot
         LA    R0,TXT13                -> DSORG
         ST    R0,0(,R6)               Plug into ptr list
         LA    R6,4(,R6)               -> next ptr list slot
         LA    R0,TXT02                -> DSNAME
         ST    R0,0(,R6)               Plug into ptr list
*
DYN610   EQU   *
         OI    0(R6),X'80'             End the parameter list
         B     DYN900                  Go allocate
*
*-- Dataset NETSPOOL
*
*   Equivalent JCL:
*   //NETSPOOL DD DISP=SHR,DSNAME=NJE38.NETSPOOL
*
*
DYN700   EQU   *
         MVC   TXT01,TXT01D            Init from the models
         MVC   TXT02,TXT02D
         MVC   TXT03,TXT03D
*
         MVC   TXT01(2),=Y(DALDDNAM)   Use fixed DD
         MVI   TXT03+6,X'08'           set DISP=SHR
*
         LA    R0,TXT01                -> DDNAME
         ST    R0,0(,R6)               Plug into ptr list
         LA    R6,4(,R6)               -> next ptr list slot
         LA    R0,TXT03                -> DISP=SHR
         ST    R0,0(,R6)               Plug into ptr list
         LA    R6,4(,R6)               -> next ptr list slot
         LA    R0,TXT02                -> DSNAME
         ST    R0,0(,R6)               Plug into ptr list
*
         OI    0(R6),X'80'             End the parameter list
         B     DYN900                  Go allocate
*
*-- Allocate the dataset
*
DYN900   EQU   *
         LA    R1,LS99RB               -> S99RB
         USING S99RB,R1
         OI    S99FLAG1,S99NOCNV        FORCE NEW ALLOCATION
         DROP  R1
         LA    R1,LS99PTR               POINTER TO S99 PTR
         SVC   99                       ISSUE DYNALLOC
         LTR   R15,R15                  Any errors?
         BZ    XITDYN00                 No
*
         LA    R1,LS99RB
         USING S99RB,R1
         UNPK  TWRK(9),S99ERROR(5)      Add zones to error code
         L     R4,S99ERROR              Error code in R4 for later v200
         DROP  R1
         TR    TWRK(8),HEXTRAN-240
         CLI   TWRK+1,C'7'              Class 7 error code?
         BNE   ERR012                   No
         LA    R1,DYNINMCP              Code for the INMCOPY dataset?
         CR    R1,R5                    Was alloc for DYNINMCP?
         BE    ERR027                   Yes, dataset exists
         LA    R1,DYNFINAL              Code for the final dataset?
         CR    R1,R5                    Was alloc for DYNFINAL?
         BE    ERR027                   Yes, dataset exists
         LA    R1,DYNINDS               Code for the INDATASET?
         CR    R1,R5                    Was alloc for DYNINDS?
         BE    ERR031                   Yes, dataset does not exist
*
ERR012   EQU   *
         MVC   LIST(4+L'MSG012T),MSG012 Dyn alloc failure msg
         MVC   LIST+21(8),TWRK          Error codes to line
         MVC   LIST+35(44),TDSNAME      Move DSNAME
         LA    R2,LIST                  -> msg
         BAL   R14,PUTLINE              Display it
         CLM   R4,12,=X'0218'           volume conflict/invalid?   v200
         BE    ERR033                   Yes                        v200
         CLM   R4,12,=X'021C'           Unit undefined?            v200
         BE    ERR034                   Yes                        v200
         B     XITDYN08                 General allocation failure
*
ERR027   EQU   *
         MVC   LIST,BLANKS
         MVC   LIST+4(9),=C'Dataset '''
         MVC   LIST+13(44),TDSNAME Move name
         TRT   LIST+13(45),BLANK   Look for end of name
         MVI   0(R1),C''''         Close apost
         LA    R1,1(,R1)           Skip apost
         MVC   0(L'MSG027T,R1),MSG027T  Move rest of msg
         LA    R1,L'MSG027T(,R1)   point to end
         XC    LIST(4),LIST        Clear RDW area
         LA    R2,LIST             -> start of RDW+msg
         SR    R1,R2               Compute total length
         STH   R1,LIST             Plug RDW
         BAL   R14,PUTLINE         Inform user
         B     XITDYN04            And exit with dataset exists
*
ERR031   EQU   *
         MVC   LIST,BLANKS
         MVC   LIST+4(9),=C'Dataset '''
         MVC   LIST+13(44),TDSNAME Move name
         TRT   LIST+13(45),BLANK   Look for end of name
         MVI   0(R1),C''''         Close apost
         LA    R1,1(,R1)           Skip apost
         MVC   0(L'MSG031T,R1),MSG031T  Move rest of msg
         LA    R1,L'MSG031T(,R1)   point to end
         XC    LIST(4),LIST        Clear RDW area
         LA    R2,LIST             -> start of RDW+msg
         SR    R1,R2               Compute total length
         STH   R1,LIST             Plug RDW
         BAL   R14,PUTLINE         Inform user
         B     XITDYN04            And exit with dataset doesnt exist
*
ERR033   EQU   *                                                   v200
         LA    R2,MSG033           -> msg (inv unit/volser combo)  v200
         BAL   R14,PUTLINE              Display it                 v200
         B     XITDYN08                                            v200
*
ERR034   EQU   *                                                   v200
         LA    R2,MSG034           -> msg (undefined unit)         v200
         BAL   R14,PUTLINE              Display it                 v200
         B     XITDYN08                                            v200
*
*
*-- Exit
*
XITDYN00 EQU   *
         SR    R15,R15             Set RC=0;  alloc/dealloc ok
         B     XITDYN
*
XITDYN04 EQU   *
         LA    R15,4               Set RC=4;  Exit for special action
         B     XITDYN
*
XITDYN08 EQU   *
         LA    R15,8               Set RC=8;  allocation error
*
XITDYN   EQU   *
         L     R13,4(,R13)         -> prev s.a.
         ST    R15,16(,R13)        Set RC
         LM    R14,R12,12(R13)     Reload callers regs
         BR    R14                 Return with RC
*
         LTORG
         DROP  R7                  INMFIELD
*
*
*
*-- Text unit skeletons
*
*-- Note:  EXPDT is included for completeness but is not used.
*
*
*
TXT01D   DC    Y(DALRTDDN),AL2(1),AL2(8)          RETURN DDNAME
TXT02D   DC    Y(DALDSNAM),AL2(1),AL2(44)         DSNAME
TXT03D   DC    Y(DALSTATS),AL2(1),AL2(1),X'04'    DISP=(NEW,)
TXT04D   DC    Y(DALNDISP),AL2(1),AL2(1),X'02'    DISP=(,CATLG)
TXT05D   DC    Y(DALBLKLN),AL2(1),AL2(3)          BLK TEXT KEY, BLKLEN
TXT06D   DC    Y(DALPRIME),AL2(1),AL2(3)          PRIMARY SPACE UNITS
TXT07D   DC    Y(DALSECND),AL2(1),AL2(3)          SECONDARY SPACE UNITS
TXT08D   DC    Y(DALDIR),AL2(1),AL2(3)            DIRECTORY BLOCKS
TXT09D   DC    Y(DALVLSER),AL2(1),AL2(6)          VOLSER
TXT10D   DC    Y(DALUNIT),AL2(1),AL2(8),CL8'SYSDA'  UNIT default   v200
TXT11D   DC    Y(DALEXPDT),AL2(1),AL2(5)          EXPDT C'YYDDD'
TXT12D   DC    Y(DALBLKSZ),AL2(1),AL2(2)          BLKSIZE
TXT13D   DC    Y(DALDSORG),AL2(1),AL2(2)          DSORG
TXT14D   DC    Y(DALLRECL),AL2(1),AL2(2)          LRECL
TXT15D   DC    Y(DALRECFM),AL2(1),AL2(1)          RECFM
TXT16D   DC    Y(DALDUMMY),AL2(0)                 DUMMY
TXT17D   DC    Y(DALSYSOU),AL2(0)                 SYSOUT
TXT18D   DC    Y(DALTERM),AL2(0)                  TERM
TXT19D   DC    Y(DALCYL),AL2(0)                   CYLINDER
TXT20D   DC    Y(DALCLOSE),AL2(0)                 FREE=CLOSE
*
UTXTD    DC    Y(DUNDDNAM),AL2(1),AL2(8)          DD for deallocation
*
         DS    0F
CPS99RB  DS    0XL20                   DEFINE INITIAL S99RB
         DC    AL1(20)                 LENGTH OF REQ BLOCK
         DC    AL1(1)                  VERB CODE:  ALLOCATION
         DC    X'20'                   FLAGS:  NO MOUNTS,OFFLINE VOLS
         DC    X'00'                   FLAGS
         DC    AL2(0)                  ERROR REASON CODE
         DC    AL2(0)                  INFO REASON CODE
         DC    A(0)                    ADDR OF TEXT PTRS
         DC    A(0)                    ADDR OF RBX
         DC    AL4(0)                  MORE FLAGS
*                                                                       NJE00250
*                                                                       NJE00250
*********************
*  N J E N E T      *               NJENET determines if NETDATA
*                   *               exists in a spool file and
*  Examine NETDATA  *               examines the INMR02 control
*                   *               record for attributes
*********************
*
NJENET   CSECT
         B     28(,R15)               BRANCH AROUND EYECATCHERS
         DC    AL1(23)                LENGTH OF EYECATCHERS
         DC    CL9'NJENET'
         DC    CL9'&SYSDATE'
         DC    CL5'&SYSTIME'
*
         STM   R14,R12,12(R13)         Save Regs                        NJE00050
         LR    R12,R15                 Base                             NJE00060
         USING NJENET,R12                                               NJE00070
         USING NJEWK,R10
         ST    R13,NETSA+4             SAVE prv S.A. ADDR               NJE00080
         LA    R1,NETSA                -> my save area
         ST    R1,8(,R13)              Plug it into prior SA
         LR    R13,R1
*
         LR    R15,R0                  Copy action code
         B     FUNC000(R15)            Branch into table
*
FUNC000  B     PINIT000            00  Process initial ctl records
         B     PDATA000            04  Process netdata records
         B     PFLAT000            08  Process flat records
*
*-- Find INMR01 record                                                  NET00060
*                                                                       NET00060
PINIT000 EQU   *
         LA    R0,2                    # of bytes to get                NET00070
         BAL   R14,GETBYTES            Get length and desc of segment   NET00080
         MVC   CTL,1(R1)               Save copy of descriptor byte
*                                                                       NET00090
         CLI   CTL,X'E0'               Valid 1st control rec indic?v200 NET00100
         BE    PINIT010                Could be ok                 v200 NET00110
         CLI   CTL,X'A0'               Valid 1st control rec indic?v200 NET00100
         BNE   XITNET04                No, its not NETDATA         v200 NET00110
*                                                                       NET00120
PINIT010 EQU   *                                                   v200
         L     R2,GBRPS                -> phys record position     v200
         CLC   0(6,R2),INMR01          Peek ahead. INMR01?         v200
         BNE   XITNET04                No, its not NETDATA         v200
*
         SR    R0,R0                                                    NET00130
         IC    R0,0(,R1)               Get segment length byte          NET00140
         S     R0,=F'2'                Less 2 we already retrieved      NET00150
         LR    R3,R0                   Copy length of control record    NET00300
         BAL   R14,GETBYTES            Get control record               NET00160
*                                                                       NET00170
         CLC   0(6,R1),INMR01          NETDATA?                         NET00180
         BNE   XITNET04                Not NETDATA                      NET00190
*
         LA    R15,6                   Len of "INMR01"                  NET00360
         AR    R1,R15                  Skip over that field             NET00370
         LA    R9,INMF01               -> area to hold INMR01 fields
         USING INMFIELD,R9
         BAL   R14,CTL000              Go parse control record
         OI    FLAGS1,F1INMR01         Indicate fields processed
*
*-- Find INMR02 record                                                  NET00060
*                                                                       NET00200
FINMR02  EQU   *
         LA    R0,2                    # of bytes to get                NET00210
         BAL   R14,GETBYTES            Get length and desc of segment   NET00220
         MVC   CTL,1(R1)               Save copy of descriptor byte
*                                                                       NET00090
         LA    R0,CODE1                Control record not indicated
         TM    CTL,X'20'               Is this a control record?        NET00100
         BZ    XITNET08                No, invalid NETDATA              NET00110
*                                                                       NET00260
         SR    R0,R0                                                    NET00270
         IC    R0,0(,R1)               Get segment length byte          NET00280
         S     R0,=F'2'                Less 2 we already retrieved      NET00290
         LR    R3,R0                   Copy length of control record    NET00300
         BAL   R14,GETBYTES            Get control record               NET00310
*                                                                       NET00320
         LA    R0,CODE2                INMR02 record not detected
         CLC   0(6,R1),INMR02          NETDATA?                         NET00330
         BNE   XITNET08                invalid NETDATA                  NET00190
*                                                                       NET00350
         LA    R15,10                  Len of "INMR02"+file number word NET00360
         AR    R1,R15                  Skip over those fields           NET00370
         LA    R9,INMF02A              -> area to hold INMR02 fields
         BAL   R14,CTL000              Go parse control record
         TM    FLAGS2,F2TERM           Was a TERM text unit key found?
         BO    XITNET20                Y, unsupported
         OI    FLAGS1,F1INMR2A         Indicate fields processed
*
*-- Find INMR02 or INMR03 record                                        NET00060
*                                                                       NET00200
         LA    R0,2                    # of bytes to get                NET00210
         BAL   R14,GETBYTES            Get length and desc of segment   NET00220
         MVC   CTL,1(R1)               Save copy of descriptor byte
*                                                                       NET00090
         LA    R0,CODE3                Control record not indicated
         TM    CTL,X'20'               Is this a control record?        NET00100
         BZ    XITNET08                No, invalid NETDATA              NET00110
*                                                                       NET00260
         SR    R0,R0                                                    NET00270
         IC    R0,0(,R1)               Get segment length byte          NET00280
         S     R0,=F'2'                Less 2 we already retrieved      NET00290
         LR    R3,R0                   Copy length of control record    NET00300
         BAL   R14,GETBYTES            Get control record               NET00310
*                                                                       NET00320
         CLC   0(6,R1),INMR03          Is it INMR03?                    NET00330
         BE    IR03                    Yes, go there                    NET00190
         LA    R0,CODE4                INMR02 record not detected
         CLC   0(6,R1),INMR02          Is it INMR02?                    NET00330
         BNE   XITNET08                Something wrong; inval NETDATA   NET00190
*                                                                       NET00350
         LA    R15,10                  Len of "INMR02"+file number word NET00360
         AR    R1,R15                  Skip over those fields           NET00370
         LA    R9,INMF02B              -> area to hold INMR02 fields
         BAL   R14,CTL000              Go parse control record
         TM    FLAGS2,F2TERM           Was a TERM text unit key found?
         BO    XITNET20                Y, unsupported
         OI    FLAGS1,F1INMR2B         Indicate fields processed
*
*-- Find INMR03 record                                                  NET00060
*                                                                       NET00200
FINMR03  EQU   *
         LA    R0,2                    # of bytes to get                NET00210
         BAL   R14,GETBYTES            Get length and desc of segment   NET00220
         MVC   CTL,1(R1)               Save copy of descriptor byte
*                                                                       NET00090
         LA    R0,CODE5                INMR03 ctl rec not indicated
         TM    CTL,X'20'               Is this a control record?        NET00100
         BZ    XITNET08                No, invalid NETDATA              NET00110
*                                                                       NET00260
         SR    R0,R0                                                    NET00270
         IC    R0,0(,R1)               Get segment length byte          NET00280
         S     R0,=F'2'                Less 2 we already retrieved      NET00290
         LR    R3,R0                   Copy length of control record    NET00300
         BAL   R14,GETBYTES            Get control record               NET00310
*                                                                       NET00320
         LA    R0,CODE6                INMR03 record not detected
         CLC   0(6,R1),INMR03          Is it INMR03?                    NET00330
         BNE   XITNET08                Something wrong; invalid NETDATA NET00190
*                                                                       NET00350
IR03     EQU   *
         LA    R15,6                   Len of "INMR03"                  NET00360
         AR    R1,R15                  Skip over those fields           NET00370
         LA    R9,INMF03               -> area to hold INMR02 fields
         BAL   R14,CTL000              Go parse control record
         OI    FLAGS1,F1INMR03         Indicate fields processed
         B     XITNET00                Done
*                                                                       NET00380
*-- Parse the text unit keys from a control record                      NET00380
*                                                                       NET00380
*-- Entry:  R3 = length of entire control record                        NET00380
*--         R15= length of INMRxx header fields to skip over
*-- Exit:   Keys identified are parsed an in their respective fields    NET00380
*                                                                       NET00380
CTL000   EQU   *                                                        NET00390
         ST    R14,SV14CTL             Save return addr
*
CTL010   EQU   *                                                        NET00390
         LA    R4,CTL010               Where to return with new segmnt
         SR    R3,R15                  Reduce remaining length          NET00400
         BNP   CTL070                  Done with control record segmnt  NET00410
         LA    R7,INMKEYS              -> text unit keys table
*                                                                       NET00420
*-- Look for supported keys                                             NET00430
*                                                                       NET00440
CTL020   EQU   *                                                        NET00390
         LA    R0,CODE7                Inv/unrecognized NETDATA key
         CLI   0(R7),X'FF'             End of table?
         BE    XITNET08                Invalid NETDATA key
*
         CLC   0(2,R1),0(R7)           Look for matching key            NET00450
         BE    CTL030                  Got one                          NET00460
         LA    R7,KEYLEN(,R7)          Bump to next in table
         B     CTL020                  Keep searching
*
CTL030   EQU   *                                                        NET00390
         ICM   R15,15,2(R7)            -> supporting rtn for key
         BNZR  R15                     Go there if rtn available
*                                                                       NET00610
*-- Skip over and ignore unsupported/unrecognized keys                  NET00620
*                                                                       NET00630
CTL050   EQU   *
         LA    R1,2(,R1)               Skip over unrecognized key       NET00640
         LA    R15,2                   Remaining length adjust          NET00650
         SR    R0,R0                   Clear for IC                     NET00660
         ICM   R0,3,0(R1)              Get # value                      NET00670
         LA    R1,2(,R1)               Skip over # value                NET00680
         LA    R15,2(,R15)             Remaining length adjust          NET00690
         BZ    CTL010                  # was 0; no lengths              NET00700
*
         LA    R4,CTL060               Where to return with new segmnt
         SR    R3,R15                  Reduce remaining length          NET00400
         BNP   CTL070                  Done with control record segmnt  NET00410
*                                                                       NET00720
CTL060   EQU   *                                                        NET00730
         SR    R14,R14                 Clear for ICM                    NET00710
         ICM   R14,3,0(R1)             Get length field                 NET00740
         LA    R1,2(R14,R1)            Skip over length and data        NET00750
         LA    R15,2(R14,R15)          Remaining length adjust          NET00760
         BCT   R0,CTL060               Do next len/data field pair      NET00770
         B     CTL010                  Resume                           NET00780
*                                                                       NET00720
*-- Here at end of segment or entire control record.                    NET00720
*-- We could also be here in the middle of a key (like INMMEMBR) and    NET00720
*-- we need to return to the right place after getting the next
*-- segment to continue on.
*                                                                       NET00720
CTL070   EQU   *                                                        NET00730
         TM    CTL,X'40'               Was that the final segment?
         BO    CTL090                  Yes, done with control record
*
*-- We need another control record segment
*
         ST    R0,SVR0CTL              Save # value for keys in progres
         LA    R0,2                    # of bytes to get                NET00070
         BAL   R14,GETBYTES            Get length and desc of segment   NET00080
         MVC   CTL,1(R1)               Save copy of descriptor byte
*                                                                       NET00090
         LA    R0,CODE8                Ctl rec segment not detected
         TM    CTL,X'20'               Is this a control record?        NET00100
         BZ    XITNET08                Bad...something wrong            NET00110
*                                                                       NET00120
         SR    R0,R0                                                    NET00130
         IC    R0,0(,R1)               Get segment length byte          NET00140
         S     R0,=F'2'                Less 2 we already retrieved      NET00150
         LR    R3,R0                   Copy length of ctl segment       NET00300
         BAL   R14,GETBYTES            Get control record segment       NET00160
         L     R0,SVR0CTL              Restore # value of the key
         SR    R15,R15                 Clear length adjustment
         BR    R4                      Return to CTL010 or CTL060
*                                                                       NET00720
CTL090   EQU   *                                                        NET00730
         L     R14,SV14CTL             Load return addr
         BR    R14                     Done with control record
*                                                                       NET00790
*-- Handle keys we dont support.                                        NET00800
*-- We look for INMTERM in order to bail out if present.
*-- This is generated by modern TRANSMIT with MSG.
*
TRM000   EQU   *
         OI    FLAGS2,F2TERM           Indicate INMTERM discovered
         B     CTL050                  Ignore the text unit key
*
*-- Handle keys we support, as well as those that we will capture       NET00800
*-- a value for but not do anything with it (example: creation date).
*                                                                       NET00810
*- Utility name                                                         NET00820
UTL000   EQU   *                       Get utility name                 NET00830
         MVC   UTLNAME+2,BLANKS         Init receiving field            NET00840
         LA    R6,UTLNAME              -> receiving field               NET00850
         BAL   R14,KEY000              Go handle the key                NET00860
         B     CTL010                  Scan for next key
*                                                                       NET00870
*- File size                                                            NET00880
FSZ000   EQU   *                       File size                        NET00890
         LA    R6,FILESIZE             -> receiving field               NET00900
         BAL   R14,KEY000              Go handle the key                NET00910
         BAL   R14,ADJ000              Right justify the value          NET00910
         B     CTL010                  Scan for next key
*                                                                       NET00920
*- DSORG                                                                NET00930
DSG000   EQU   *                       DSORG                            NET00940
         LA    R6,DSORG                -> receiving field               NET00950
         BAL   R14,KEY000              Go handle the key                NET00960
         B     CTL010                  Scan for next key
*- BLKSIZE                                                              NET00970
BLK000   EQU   *                       BLKSIZE                          NET00980
         LA    R6,BLKSIZE              -> receiving field               NET00990
         BAL   R14,KEY000              Go handle the key                NET01000
         BAL   R14,ADJ000              Right justify the value          NET00910
         B     CTL010                  Scan for next key
*                                                                       NET01010
*- LRECL                                                                NET01020
LRL000   EQU   *                       LRECL                            NET01030
         LA    R6,LRECL                -> receiving field               NET01040
         BAL   R14,KEY000              Go handle the key                NET01050
         BAL   R14,ADJ000              Right justify the value          NET00910
         B     CTL010                  Scan for next key
*                                                                       NET01060
*- RECFM                                                                NET01070
RFM000   EQU   *                       RECFM                            NET01080
         LA    R6,RECFM                -> receiving field               NET01090
         BAL   R14,KEY000              Go handle the key                NET01100
         B     CTL010                  Scan for next key
*                                                                       NET00870
*- # directory blocks                                                   NET00880
DIR000   EQU   *                       File size                        NET00890
         LA    R6,DIRBLKS              -> receiving field               NET00900
         BAL   R14,KEY000              Go handle the key                NET00910
         BAL   R14,ADJ000              Right justify the value          NET00910
         B     CTL010                  Scan for next key
*                                                                       NET01110
*- FFM                                                                  NET01120
FFM000   EQU   *                       File mode number                 NET01130
         LA    R6,FFM                  -> receiving field               NET01140
         BAL   R14,KEY000              Go handle the key                NET01150
         B     CTL010                  Scan for next key
*
*- Origin timestamp                                                     NET00820
FTM000   EQU   *                                                        NET00830
         MVC   FTIME+2,BLANKS          Init receiving field             NET00840
         LA    R6,FTIME                -> receiving field               NET00850
         BAL   R14,KEY000              Go handle the key                NET00860
         B     CTL010                  Scan for next key
*
*- Origin node                                                          NET00820
FND000   EQU   *                       Get origin node                  NET00830
         MVC   FNODE+2,BLANKS          Init receiving field             NET00840
         LA    R6,FNODE                -> receiving field               NET00850
         BAL   R14,KEY000              Go handle the key                NET00860
         B     CTL010                  Scan for next key
*
*- Origin userid                                                        NET00820
FUS000   EQU   *                       Get origin userid                NET00830
         MVC   FUSER+2,BLANKS          Init receiving field             NET00840
         LA    R6,FUSER                -> receiving field               NET00850
         BAL   R14,KEY000              Go handle the key                NET00860
         B     CTL010                  Scan for next key
*
*- To node                                                              NET00820
TND000   EQU   *                       Get destination node             NET00830
         MVC   TNODE+2,BLANKS          Init receiving field             NET00840
         LA    R6,TNODE                -> receiving field               NET00850
         BAL   R14,KEY000              Go handle the key                NET00860
         B     CTL010                  Scan for next key
*
*- To userid                                                            NET00820
TUS000   EQU   *                       Get destination userid           NET00830
         MVC   TUSER+2,BLANKS          Init receiving field             NET00840
         LA    R6,TUSER                -> receiving field               NET00850
         BAL   R14,KEY000              Go handle the key                NET00860
         B     CTL010                  Scan for next key
*
*- Version                                                              NET00820
VER000   EQU   *                       Get Version info                 NET00830
         MVC   FVERS+2,BLANKS          Init receiving field             NET00840
         LA    R6,FVERS                -> receiving field               NET00850
         BAL   R14,KEY000              Go handle the key                NET00860
         BAL   R14,ADJ000              Right justify the value          NET00910
         B     CTL010                  Scan for next key
*
*- Creation date                                                        NET00820
CRE000   EQU   *                                                        NET00830
         MVC   CREATE+2,BLANKS         Init receiving field             NET00840
         LA    R6,CREATE               -> receiving field               NET00850
         BAL   R14,KEY000              Go handle the key                NET00860
         B     CTL010                  Scan for next key
*
*- DDNAME                                                               NET00820
DDN000   EQU   *                                                        NET00830
         MVC   DDNAME+2,BLANKS         Init receiving field             NET00840
         LA    R6,DDNAME               -> receiving field               NET00850
         BAL   R14,KEY000              Go handle the key                NET00860
         B     CTL010                  Scan for next key
*
*- Extended attribute                                                   NET00820
ATR000   EQU   *                                                        NET00830
         LA    R6,EATTR                -> receiving field               NET00850
         BAL   R14,KEY000              Go handle the key                NET00860
         B     CTL010                  Scan for next key
*
*- RECEIVE error code                                                   NET00820
ECD000   EQU   *                                                        NET00830
         LA    R6,ERRCD                -> receiving field               NET00850
         BAL   R14,KEY000              Go handle the key                NET00860
         B     CTL010                  Scan for next key
*
*- Expiration date                                                      NET00820
EXP000   EQU   *                                                        NET00830
         MVC   EXPDT+2,BLANKS          Init receiving field             NET00840
         LA    R6,EXPDT                -> receiving field               NET00850
         BAL   R14,KEY000              Go handle the key                NET00860
         B     CTL010                  Scan for next key
*
*- Last changed date                                                    NET00820
LCH000   EQU   *                                                        NET00830
         MVC   LCHG+2,BLANKS           Init receiving field             NET00840
         LA    R6,LCHG                 -> receiving field               NET00850
         BAL   R14,KEY000              Go handle the key                NET00860
         B     CTL010                  Scan for next key
*
*- Last referenced date                                                 NET00820
LRF000   EQU   *                                                        NET00830
         MVC   LREF+2,BLANKS           Init receiving field             NET00840
         LA    R6,LREF                 -> receiving field               NET00850
         BAL   R14,KEY000              Go handle the key                NET00860
         B     CTL010                  Scan for next key
*                                                                       NET00870
*- Size in megabytes                                                    NET00880
LSZ000   EQU   *                       File size in MB                  NET00890
         LA    R6,LSIZE                -> receiving field               NET00900
         BAL   R14,KEY000              Go handle the key                NET00910
         B     CTL010                  Scan for next key
*                                                                       NET00870
*- Number of files                                                      NET00880
NMF000   EQU   *                       File size in MB                  NET00890
         LA    R6,NUMF                 -> receiving field               NET00900
         BAL   R14,KEY000              Go handle the key                NET00910
         BAL   R14,ADJ000              Right justify the value          NET00910
         B     CTL010                  Scan for next key
*                                                                       NET00870
*- Record count                                                         NET00880
RCT000   EQU   *                                                        NET00890
         LA    R6,RECCT                -> receiving field               NET00900
         BAL   R14,KEY000              Go handle the key                NET00910
         BAL   R14,ADJ000              Right justify the value          NET00910
         B     CTL010                  Scan for next key
*                                                                       NET00870
*- Secondary space                                                      NET00880
SEC000   EQU   *                                                        NET00890
         LA    R6,SECND                -> receiving field               NET00900
         BAL   R14,KEY000              Go handle the key                NET00910
         B     CTL010                  Scan for next key
*
*- Destination timestamp                                                NET00820
TTM000   EQU   *                                                        NET00830
         MVC   TTIME+2,BLANKS          Init receiving field             NET00840
         LA    R6,TTIME                -> receiving field               NET00850
         BAL   R14,KEY000              Go handle the key                NET00860
         B     CTL010                  Scan for next key
*                                                                       NET00870
*- Dataset Type                                                         NET00880
TYP000   EQU   *                       Data set type                    NET00890
         LA    R6,DSTYPE               -> receiving field               NET00900
         BAL   R14,KEY000              Go handle the key                NET00910
         B     CTL010                  Scan for next key
*                                                                       NET01160
*                                                                       NET01160
*- DSNAME                                                               NET01170
DSN000   EQU   *                       DSNAME                           NET01180
         MVC   DSNAME+2,BLANKS         Init receiving field             NET01190
         LA    R6,DSNAME+2             -> receiving field               NET01200
         LA    R1,2(,R1)               Skip over key                    NET01210
         LA    R15,2                   Remaining length adjust          NET01220
         SR    R0,R0                   Clear for IC                     NET01230
         ICM   R0,3,0(R1)              Get # value                      NET01240
         LA    R1,2(,R1)               Skip over # value                NET01250
         LA    R15,2(,R15)             Remaining length adjust          NET01260
         BZ    CTL010                  # was 0; no lengths              NET01270
         SR    R14,R14                 Clear for ICM                    NET01280
*                                                                       NET01290
DSN020   EQU   *                                                        NET01300
         ICM   R14,3,0(R1)             Get length field                 NET01310
         BCT   R14,DSN030              Adjust for execute               NET01320
         MVC   0(0,R6),2(R1)           executed instr                   NET01330
DSN030   EX    R14,*-6                 Move name to receiving field     NET01340
         LA    R1,3(R14,R1)            Skip over length and data        NET01350
         LA    R15,3(R14,R15)          Remaining length adjust          NET01360
         LA    R6,1(R14,R6)            Bump to next qualifier area      NET01370
         MVI   0(R6),C'.'              Add qualifier dot
         LA    R6,1(,R6)               -> next qualifier area
         BCT   R0,DSN020               Do next len/data field pair      NET01380
         BCTR  R6,0                    -> last byte of DSNAME           NET01390
         MVI   0(R6),C' '              Remove trailing dot
         BCTR  R6,0                    -> prior to trailing '.'         NET01390
         LA    R0,DSNAME+2             -> start of DSNAME               NET01400
         SR    R6,R0                   Compute final DSN length         NET01410
         STCM  R6,3,DSNAME             Save it                          NET01420
         B     CTL010                  get next key                     NET01430
*                                                                       NET01440
*-- Common routine to break part key/#/len/data elements that have #=1  NET01450
*                                                                       NET01460
KEY000   EQU   *                                                        NET01470
         LA    R1,4(,R1)               Skip over key, #                 NET01480
         LA    R15,4                   Remaining length accum           NET01490
         SR    R5,R5                   Clear for IC                     NET01500
         ICM   R5,3,0(R1)              Get length of name               NET01510
         STCM  R5,3,0(R6)              Store actual len in result fld
         BZR   R14                     If no length, done with key
         BCT   R5,KEY010               Adjust for execute               NET01520
         MVC   2(0,R6),2(R1)           executed instr                   NET01530
KEY010   EX    R5,*-6                  Move name to receiving field     NET01540
         LA    R1,3(R5,R1)             -> next text unit key            NET01550
         LA    R15,3(R5,R15)           Accum length adjustment          NET01560
         BR    R14                     Return                           NET01570
*
*-- Common routine right justify numeric fields of numeric text units   NET01450
*                                                                       NET01460
ADJ000   EQU   *
         LA    R8,8                    Max length of value
         LH    R0,0(,R6)               Get length from NETDATA key
         SR    R8,R0                   Compute # bytes of shift
         BZR   R14                     No justification required
         SLA   R8,3                    Turn # bytes into # bits
         LM    R4,R5,2(R6)             Get numeric field
         SRDL  R4,0(R8)                Right justify the number
         STM   R4,R5,2(R6)             Put back justified numeric value
         BR    R14
*
         DROP  R9                      INMFIELD
*
*
*-- Process data records
*
*-- NOTE!  We are using PUT LOCATE mode here, which offers the
*--        flexibility to accomodate RECFM=VS and VBS records when
*--        combined with DCB=BFTEK=A.  This can be confusing looking
*--        as it seems the PUT is issued and then the record is built.
*--        However, the PUT is actually writing the previous record
*--        and the last record is written by CLOSE, all per the IBM
*--        data management specification.
*
*-- NOTE!  RECFM=U processing requires the length of the record
*--        be stored in DCBLRECL prior to the PUT LOCATE being issued.
*--        But we don't know the length of the record yet, because it
*--        is coming in from the NETDATA in segments.  So, we have to
*--        unfortunately use a separate buffer to accumulate the
*--        segments and when complete obtain the total length to
*--        store into DCBLRECL.  Then we can issue the PUT.  Then
*--        we have to move the data from our segment accumulation
*--        buffer into the PUT LOCATE buffer. Note though, that
*--        if we used PUT MOVE for RECFM=U, the system would
*--        move our data out of the segment buffer; so either way
*--        the data is moved an extra time.
*
PDATA000 EQU   *
         LA    R0,2                    # of bytes to get                NET00210
         BAL   R14,GETBYTES            Get length and desc of segment   NET00220
*                                                                       NET00230
         TM    1(R1),X'20'             Is this a control record?        NET00240
         BO    PDATA100                Yes                              NET00110
*
         MVC   CTL,1(R1)               Save copy of descriptor byte
         SR    R0,R0                                                    NET00130
         IC    R0,0(,R1)               Get segment length byte          NET00140
         S     R0,=F'2'                Less 2 we already retrieved      NET00150
         LR    R3,R0                   Copy length of segment           NET00300
         BAL   R14,GETBYTES            Get a segment                    NET00160
         LR    R4,R1                   Copy segment ptr
*
*-- Determine type of record segment
*
         TM    CTL,X'C0'           C0  Complete record?
         BO    CMP000                  Yes
         TM    CTL,X'80'           80  1st record of segment?
         BO    FST000                  Yes
         TM    CTL,X'40'           40  last record of segment?
         BO    LST000                  Yes
*
*-- Middle segment
*
MID000   EQU   *                   00  Handle middle segment
         L     R1,RBPOS                -> next available buffer byte
         LR    R0,R1                   Copy next available ptr     v222
         S     R0,RBUFF                Compute len used so far     v222
         AR    R0,R3                   Add len of next segment     v222
         CH    R0,NEWDS+(DCBLRECL-IHADCB) will segment fit in buff?v222
         BH    XITNET24                No. record too large        v222
         BCTR  R3,0                    Adjust for execute
         EX    R3,MVCSEG               Move segement data
         LA    R1,1(R3,R1)             -> next available byte
         ST    R1,RBPOS                Save record position
         B     PDATA000                Go get some more
*
*-- Complete segment (an entire record)
*
CMP000   EQU   *
         TM    TRECFM,X'C0'            Using undefined format?
         BO    CMPU100                 Yes
*
         PUT   NEWDS                   Write R1 buffer and get new one
*
         TM    TRECFM,X'40'            Using variable format?
         BO    CMP010                  Yes
         BCTR  R3,0                    Adjust for execute
         EX    R3,MVCSEG               Move segment data
         B     PDATA000                Go get some more
*
CMP010   EQU   *
         LA    R0,4(,R3)               Copy record length + 4
         SLL   R0,16                   Make RDW = LLZZ
         STCM  R0,15,0(R1)             Plug in RDW
         LA    R1,4(,R1)               Skip over RDW
         BCTR  R3,0                    Adjust for execute
         EX    R3,MVCSEG               Move segment data
         B     PDATA000                Go get some more
*
CMPU100  EQU   *                    ** Here if RECFM=U only
         STH   R3,NEWDS+(DCBLRECL-IHADCB) Set len of output recrd
         PUT   NEWDS                   Write R1 buffer and get new one
         BCTR  R3,0                    Adjust for execute
         EX    R3,MVCSEG               Move segment data
         B     PDATA000                Go get some more
*
MVCSEG   MVC   0(0,R1),0(R4)           executed instr
*
*-- First segment of a record
*
FST000   EQU   *                       Handle first segment
         TM    TRECFM,X'C0'            Using undefined format?
         BO    FSTU100                 Yes
*
         PUT   NEWDS                   Write R1 buffer and get new one
         ST    R1,RBUFF                Save start addr of buffer
*
         TM    TRECFM,X'40'            Variable records?
         BZ    FST010                  No
         LA    R1,4(,R1)               Leave space for RDW
*
FST010   EQU   *
         BCTR  R3,0                    Adjust for execute
         EX    R3,MVCSEG               Move segment data
         LA    R1,1(R3,R1)             -> next available byte
         ST    R1,RBPOS                Save record position
         B     PDATA000                Go get some more
*
FSTU100  EQU   *                    ** Here if RECFM=U only
         L     R1,NEWBLK               -> RECFM=U build buffer
         BCTR  R3,0                    Adjust for execute
         EX    R3,MVCSEG               Move segment data
         LA    R1,1(R3,R1)             -> next available byte
         ST    R1,RBPOS                Save record position
         B     PDATA000                Go get some more
*
*-- Last segment of a record
*
LST000   EQU   *                       Handle last segment
         L     R1,RBPOS                -> next available buffer byte
         LR    R0,R1                   Copy next available ptr     v222
         S     R0,RBUFF                Compute len used so far     v222
         AR    R0,R3                   Add len of next segment     v222
         CH    R0,NEWDS+(DCBLRECL-IHADCB) will segment fit in buff?v222
         BH    XITNET24                No. record too large        v222
         BCTR  R3,0                    Adjust for execute
         EX    R3,MVCSEG               Move segment data
         LA    R1,1(R3,R1)             -> next available byte
*
         TM    TRECFM,X'C0'            Using undefined format?
         BO    LSTU100                 Yes
*
         L     R3,RBUFF                -> record start
         TM    TRECFM,X'40'            Variable records?
         BZ    LST010                  No
*
         SR    R1,R3                   Compute record length
         LA    R0,CODE9                Assume bad segment length   v200
         CH    R1,NEWDS+(DCBLRECL-IHADCB) Chk RDW against LRECL    v200
         BH    XITNET08                It was                      v200
*
         SLL   R1,16                   Make RDW = LLZZ
         STCM  R1,15,0(R3)             Plug in RDW
         SRL   R1,16                   Make length
*
LST010   EQU   *
         B     PDATA000                Go get some more
*
LSTU100  EQU   *                    ** Here if RECFM=U only
         L     R2,NEWBLK               -> RECFM=U record build area
         SR    R1,R2                   Compute record length
         STH   R1,NEWDS+(DCBLRECL-IHADCB) Set len of output recrd
         LR    R3,R1                   Copy length to write
*
         PUT   NEWDS                   Write prv buffer and get new one
         LR    R0,R1                   -> PUT buffer to R0
         LR    R1,R3                   Length of record
         MVCL  R0,R2                   Move to PUT LOCATE buffer
         B     PDATA000                Go get some more
*
*-- Control record encountered in data stream
*
PDATA100 EQU   *
         SR    R0,R0                                                    NET00130
         IC    R0,0(,R1)               Get segment length byte          NET00140
         S     R0,=F'2'                Less 2 we already retrieved      NET00150
         LR    R3,R0                   Copy length of segment           NET00300
         BAL   R14,GETBYTES            Get a segment                    NET00160
*
         CLC   0(6,R1),INMR06          Is it INMR06?                    NET00330
         BNE   PDATA000                Ignore other control records
         B     XITNET00                Done
*
*-- Process FLAT FILE not in NETDATA format
*
*-- We've already read the first spool record, in REC.
*-- PUN files:  just write 80 byte records.
*-- PRT files:  write 133 bytes, always convert to ASA carriage ctl;
*--             the raw data over NJE is always M carriage ctl for PRT.
*
*-- Writing out to DCB NEWDS using MACRF=PL
*
*
*
PFLAT000 EQU   *
         LA    R6,TAGDATA
         USING TAG,R6
         LA    R4,X'40'                Assume CC of space 1
         B     PFLAT030                1st record is already in REC
*
*
*-- Retrieve the spool file records
*
PFLAT010 EQU   *
         BAL   R14,GET000              Get a record
         C     R15,=F'4'               EOF?
         BE    XITNET00                Yes, were done
         LTR   R15,R15                 Any errors?
         BNZ   XITNET16                Yes, deal with them
*
PFLAT030 EQU   *
         TM    TAGINDEV,TYPPUN         Is this punch data?
         BO    PFLAT180                Yes
*
*-- Output PRT records with RECFM=A carriage control
*
         TM    REC,X'03'               Immediate cmd CC in record?
         BNO   PFLAT070                No, this one is the data
*
         LA    R4,C'0'                 Space 2 lines
         CLI   REC,X'13'               Is CC character space 2 immed?
         BE    PFLAT010                Yes
         LA    R4,C'1'                 Skip to channel 1
         CLI   REC,X'8B'               Is CC character ch 1 immed?
         BE    PFLAT010                Yes
         LA    R4,C'-'                 Space 3 lines
         CLI   REC,X'1B'               Is CC character space 3 immed?
         BE    PFLAT010                Yes
         LA    R4,C'+'                 Suppress space
         CLI   REC,X'01'               Is CC character write sup imd?
         BE    PFLAT010                Yes
         LA    R4,X'40'                Otherwise use space 1
         B     PFLAT010
*
PFLAT070 EQU   *
         PUT   NEWDS                   Write a line
         LR    R5,R1                   Get new buffer addr
*
         STC   R4,0(,R5)               Set the CC byte
         LH    R1,NCB1+(NCBRECLN-NCB)  Get length of spool record
         BCTR  R1,0                    Less one to skip CC byte
         ICM   R1,8,BLANKS             Set pad char
         LA    R0,REC+1                -> spool input record skipping
*                                       the M carriage control
         LA    R14,1(,R5)              Where to build output record
         LH    R15,NEWDS+(DCBLRECL-IHADCB) get len of output recrd area
         BCTR  R15,0                   Less one to skip CC byte
         MVCL  R14,R0                  Move record and pad excess
         B     PFLAT010                Process another line
*
*-- PUN records
*
PFLAT180 EQU   *
         PUT   NEWDS                   Write a line
         LR    R5,R1                   Get new buffer addr
*
         LH    R1,NCB1+(NCBRECLN-NCB)  Get length of spool record
         ICM   R1,8,BLANKS             Set pad char
         LA    R0,REC                  -> spool input record
         LR    R14,R5                  Where to build output record
         LH    R15,NEWDS+(DCBLRECL-IHADCB) get len of output recrd area
         MVCL  R14,R0                  Move record and pad excess
         B     PFLAT010                Process another line
*
*
*                                                                       NET01580
*                                                                       NET01580
*                                                                       NET01580
*-- Request some more bytes of NETDATA formatted data                   NET01590
*                                                                       NET01590
*-- Entry:  R0 = # of bytes requested  (1-255)                          NET01590
*-- Exit:   R1 -> string of bytes obtained                              NET01590
*                                                                       NET01660
*-- Uses R0-R1,R5-R8,R14-R15; the caller's values in these              NET01660
*--      registers are not preserved across this call.
*                                                                       NET01660
GETBYTES EQU   *                                                        NET01670
         ST    R14,SV14GB              Save return addr                 NET01680
         L     R5,GBREM                Get # bytes remaining in rec buf NET01690
         LA    R1,BUFF                 Point to getbytes buffer         NET01700
         ST    R1,GBPOS                Set starting position            NET01710
*
         L     R8,GBRBA                Get RBA of current position
         ST    R8,GBPBA                Save prior RBA
         AR    R8,R0                   Compute next RBA
         ST    R8,GBRBA                Update RBA if successful
*
         LR    R8,R0                   Requested amount to R8           NET01720
*                                                                       NET01730
*                                                                       NET01740
GB010    EQU   *                                                        NET01750
         LTR   R5,R5                   Any bytes left in phy record?    NET01760
         BP    GB040                   Yes, use them first              NET01770
*                                                                       NET01780
         BAL   R14,GET000              Get a NETDATA record
         LTR   R15,R15                 Any errors?
         BNZ   GB090                   Yes, deal with them
*                                      R0-> length of record read       NET01850
         LR    R5,R0                   Num bytes read                   NET01840
         ST    R1,GBRPS                Reset start of record position   NET01880
*                                                                       NET01890
GB040    EQU   *                                                        NET01900
         LR    R7,R8                   Assume requested amt avail       NET01910
         LR    R15,R8                  Same                             NET01920
*                                                                       NET01930
         CR    R5,R8                   Have more than we need?          NET01940
         BH    GB050                   Yes, just move requested         NET01950
         LR    R7,R5                   Else move what we have           NET01960
         LR    R15,R5                  Same                             NET01970
*                                                                       NET01980
GB050    EQU   *                                                        NET01990
         LR    R0,R7                   Save copy of length to move      NET02000
         L     R14,GBPOS               -> GB buffer position            NET02010
         L     R6,GBRPS                -> input record curr position    NET02020
         MVCL  R14,R6                  Move                             NET02030
*                                                                       NET02040
         ST    R14,GBPOS               New GB position                  NET02050
         ST    R6,GBRPS                New phys record curr position    NET02060
*                                                                       NET02070
         SR    R5,R0                   Reduce bytes left in phy record  NET02080
         SR    R8,R0                   Reduce requested amt             NET02090
         BP    GB010                   We need more, go get it          NET02100
*                                                                       NET02110
         ST    R5,GBREM                Remember whats left in phy rec   NET02120
*                                                                       NET02130
         LA    R1,BUFF                 Point to the requested bytes     NET02140
         L     R14,SV14GB              Load  return addr                NET02150
         BR    R14                     Return from getbytes             NET02160
*                                                                       NET01980
GB090    EQU   *                                                        NET01990
         C     R15,=F'4'               End of file?
         BE    XITNET12                Yes
         B     XITNET16                I/O error
*
*-- Exit NETDATA processing
*
XITNET00 EQU   *
         SR    R15,R15             RC=0; NETDATA info filled
         B     XITNET
*
XITNET04 EQU   *
         LA    R15,4               RC=4; File doesnt lead off w/NETDATA
         B     XITNET
*
*-- Here if unexpected or unrecognized NETDATA sequences are found
*
*-- There are 8 places that could branch here; they are numbered 1-8
*-- in R0 to indicate how we arrived here "detection code".  Used
*-- with the input NETDATA record and byte number this code can
*-- help to locate the offending error.
*
*-- The detection CODEx equates below describe the 8 tests
*
CODE1    EQU   1                       Control record not indicated
CODE2    EQU   2                       INMR02 record not detected
CODE3    EQU   3                       Control record not indicated
CODE4    EQU   4                       INMR02 record not detected
CODE5    EQU   5                       INMR03 ctl rec not indicated
CODE6    EQU   6                       INMR03 record not detected
CODE7    EQU   7                       Inv/unrecognized NETDATA key
CODE8    EQU   8                       Ctl rec segment not detected
CODE9    EQU   9                       Incorrect segment lengths   v200
*
*-- Format error msg (MSG003):
*
*Invalid or unsupported NETDATA detected; error code x, record y byte z
*
*-- Note for debugging:  the record and byte number displayed point
*-- to the position in the original input at the point of the GETBYTES
*-- call.  The error may be at that exact byte or following it for
*-- some reasonable amount (up to 255 bytes).  For error codes 1-6 and
*-- code 8, the rec/byte shown is very close and usually exact. For
*-- code 7 errors there is an invalid or unknown text unit key and
*-- the invalid key is somewhere after the rec/byte shown in the
*-- next 255 bytes.
*
*-- For code 9, the record segment lengths exceeded the LRECL      v200
*--  (variable length records only).                               v200
*
XITNET08 EQU   *
         LR    R6,R0               Detection code to R6
*
         L     R1,GBPBA            Get RBA of prior GETBYTES call
         SR    R0,R0               Clear for divide
         D     R0,=F'80'           Compute input record number
         LA    R2,1                Load 1
         AR    R1,R2               Make record number relative to 1
         AR    R0,R2               Make byte number relative to 1
         CVD   R1,DBLE             Convert
         MVC   LIST(4+L'MSG003T),MSG003  Build msg
         MVC   TWRK(12),=X'402020206B2020206B202120' Edit mask
         LA    R1,TWRK+11          Start of significance
         EDMK  TWRK(12),DBLE+3     Edit record count
         LA    R2,TWRK+11          -> last digit of edited number
         SR    R2,R1               Compute display length
         EX    R2,MVREC            Move edited number to line
         LA    R1,LIST+67(R2)      -> next available byte in line
         MVC   0(8,R1),=C' byte xx'
*
         CVD   R0,DBLE             Convert byte position
         UNPK  6(2,R1),DBLE        Fill in byte #
         OI    7(R1),X'F0'         Fix sign
*
         STC   R6,LIST+56          Store detection code
         OI    LIST+56,X'F0'       Add a sign to make display
*
         LA    R1,8(,R1)           Compute end of msg text
         LA    R2,LIST             -> start of msg
         SR    R1,R2               Compute msg length RDW
         STH   R1,LIST             Set RDW
*
         BAL   R14,PUTLINE         Notify user
         LA    R15,8               RC=8; Invalid NETDATA detected
         B     XITNET
*
*-- EOF on NETDATA
XITNET12 EQU   *
         LA    R15,12              RC=12 unexpected EOF
         B     XITNET
*
*-- Read error on NETDATA
XITNET16 EQU   *
         LA    R15,16              RC=16 Read i/o error
         B     XITNET
*
*-- INMTERM text unit key detected and it is unsupported
XITNET20 EQU   *
         LA    R15,20              RC=20 INMTERM detected
         B     XITNET
*
*-- Segmented record pieces are too large for LRECL and exceed     v222
*--  the PUT record buffer                                         v222
XITNET24 EQU   *                                                   v222
         LA    R15,24              RC=24 record too large          v222
         B     XITNET                                              v222
*
XITNET   EQU   *
         L     R13,4(,R13)         -> prev s.a.
         ST    R15,16(,R13)        Set RC
         LM    R14,R12,12(R13)     Reload callers regs
         BR    R14                 Return with RC
*
MVREC    MVC   LIST+66(0),0(R1)    executed instr
*
         LTORG
*                                                                       NET02190
*                                                                       NET02190
*- Control records that we look for and process (others ignored).       NET02190
INMR01   DC    C'INMR01'               Header Control record            NET02200
INMR02   DC    C'INMR02'               File Utility Control record      NET02210
INMR03   DC    C'INMR03'               Data Control record              NET02210
INMR06   DC    C'INMR06'               Trailer Control record           NET02210
*                                                                       NET02220
*- Keys                                                                 NET02230
INMKEYS  DS    0H
INMBLKSZ DC    X'0030',AL4(BLK000)     Block size
INMCREAT DC    X'1022',AL4(CRE000)     Creation date
INMDDNAM DC    X'0001',AL4(DDN000)     DDNAME for the file
INMDIR   DC    X'000C',AL4(DIR000)     Number of directory blocks
INMDSNAM DC    X'0002',AL4(DSN000)     Name of the file
INMDSORG DC    X'003C',AL4(DSG000)     File organization
INMEATTR DC    X'8028',AL4(ATR000)     Extended attribute status
INMERRCD DC    X'1027',AL4(ECD000)     RECEIVE command error code
INMEXPDT DC    X'0022',AL4(EXP000)     Expiration date
INMFACK  DC    X'1026',AL4(0)  NO SPT--Originator requested notificat'n
INMFFM   DC    X'102D',AL4(FFM000)     Filemode number
INMFNODE DC    X'1011',AL4(FND000)     Origin node name or node number
INMFTIME DC    X'1024',AL4(FTM000)     Origin timestamp
INMFUID  DC    X'1012',AL4(FUS000)     Origin user ID
INMFVERS DC    X'1023',AL4(VER000)     Origin version num of the data
INMLCHG  DC    X'1021',AL4(LCH000)     Date last changed
INMLRECL DC    X'0042',AL4(LRL000)     Logical record length
INMLREF  DC    X'1020',AL4(LRF000)     Date last referenced
INMLSIZE DC    X'8018',AL4(LSZ000)     Data set size in megabytes.
INMMEMBR DC    X'0003',AL4(0)  NO SPT--Member name list
INMNUMF  DC    X'102F',AL4(NMF000)     Number of files transmitted
INMRECCT DC    X'102A',AL4(RCT000)     Transmitted record count
INMRECFM DC    X'0049',AL4(RFM000)     Record format
INMSECND DC    X'000B',AL4(SEC000)     Secondary space quantity
INMSIZE  DC    X'102C',AL4(FSZ000)     File size in bytes
INMTERM  DC    X'0028',AL4(TRM000)     Data transmitted as a message
INMTNODE DC    X'1001',AL4(TND000)     Target node name or node number
INMTTIME DC    X'1025',AL4(TTM000)     Destination timestamp
INMTUID  DC    X'1002',AL4(TUS000)     Target user ID
INMTYPE  DC    X'8012',AL4(TYP000)     Data set type
INMUSERP DC    X'1029',AL4(0)  NO SPT--User parameter string
INMUTILN DC    X'1028',AL4(UTL000)     Name of utility program
         DC    X'FFFF'                 End of table
KEYLEN   EQU   6                       Length of key/adcon pair
*
*-- Target fields from INMRxx control records that we recognize:
*
*-- Missing from the list and unsupported:
*--  INMFACK  1-64 bytes, notification string from transmit
*--  INMTERM  0 bytes, data was transmitted as a message
*--  INMUSERP 1-251 bytes, user PARM field string from TRANSMIT/RECEIVE
*
INMFIELD DSECT
UTLNAME  DS    0XL8,XL2,CL8           Utility name                      NET02490
FILESIZE DS    0XL8,XL2,XL8           File size in bytes                NET02500
DIRBLKS  DS    0XL8,XL2,XL8           #directory blocks                 NET02500
BLKSIZE  DS    0XL8,XL2,XL8           BLKSIZE                           NET02510
LRECL    DS    0XL8,XL2,XL8           LRECL                             NET02520
RECFM    DS    0XL2,XL2,XL2           RECFM                             NET02530
DSORG    DS    0XL2,XL2,XL2           DSORG                             NET02540
FFM      DS    0XL1,XL2,CL1           File mode number                  NET02550
DSNAME   DS    0XL44,XL2,CL44         DSNAME                            NET02580
FTIME    DS    0XL20,XL2,CL20         Origin time stamp                 NET02580
FNODE    DS    0XL8,XL2,CL8           Origin node                       NET02580
FUSER    DS    0XL8,XL2,CL8           Origin userid                     NET02580
TNODE    DS    0XL8,XL2,CL8           Dest node                         NET02580
TUSER    DS    0XL8,XL2,CL8           Dest userid                       NET02580
TTIME    DS    0XL16,XL2,CL16         Destination time stamp            NET02580
FVERS    DS    0XL8,XL2,XL8           Version                           NET02580
DDNAME   DS    0XL8,XL2,CL8           DDNAME                            NET02580
CREATE   DS    0XL16,XL2,CL16         Creation date                     NET02580
EATTR    DS    0XL1,XL2,CL1           Extended attributes               NET02550
ERRCD    DS    0XL1,XL2,CL1           Receive error code                NET02550
EXPDT    DS    0XL16,XL2,CL16         Expiration date                   NET02580
LCHG     DS    0XL16,XL2,CL16         Last Changed date                 NET02580
LREF     DS    0XL16,XL2,CL16         Last Referenced date              NET02580
LSIZE    DS    0XL4,XL2,XL4           Size of file in MB                NET02580
MEMBR    DS    0XL8,XL2,CL8           Member name list (1 supported)    NET02580
NUMF     DS    0XL8,XL2,XL8           Number of files in transmission   NET02520
RECCT    DS    0XL8,XL2,XL8           Number of records transmitted     NET02520
SECND    DS    0XL3,XL2,XL3           secondary space qty               NET02520
DSTYPE   DS    0XL1,XL2,XL1           Data set type                     NET02520
         DS    0H                     Force to halfword size
INMFSZ   EQU   *-INMFIELD             Size of DSECT
*
*                                                                       NJE00250
*********************
*  N J E P A R      *               NJEPAR calls IKJPARS to parse
*                   *               the TSO command line parameters.
*  TSO Command Line *
*  Parse            *
*                   *
*********************
*
*  Entry:  R0=0  Parse the command line parameters
*
*          R0=4  Parse the prompt parameters (change dsname, etc)
*          R1 -> Prompt input buffer from PUTGET if R0 = 4.
*
*  Exit:   R15 = IKJPARS RC
*
NJEPAR   CSECT
         B     28(,R15)               BRANCH AROUND EYECATCHERS
         DC    AL1(23)                LENGTH OF EYECATCHERS
         DC    CL9'NJEPAR'
         DC    CL9'&SYSDATE'
         DC    CL5'&SYSTIME'
*
         STM   R14,R12,12(R13)         Save Regs                        NJE00050
         LR    R12,R15                 Base                             NJE00060
         USING NJEPAR,R12                                               NJE00070
         USING NJEWK,R10
         ST    R13,PARSA+4             SAVE prv S.A. ADDR               NJE00080
         LA    R2,PARSA                -> my save area
         ST    R2,8(,R13)              Plug it into prior SA
         LR    R13,R2
*
*
         LR    R7,R0                   Copy entry action code
         LR    R6,R1                   Copy any passed ptr
*
         L     R1,CPARMS               -> CPPL entry parms
         LM    R2,R5,0(R1)             Get TSO command entry parameters
*                                       R2 -> Command buffer
*                                       R3 -> UPT
*                                       R4 -> PSCB
*                                       R5 -> ECT
*
*-- Build the IKJPARS PPL
*
PARS000  EQU   *
         LA    R8,PPLSTG               -> PPL
         USING PPL,R8
         ST    R3,PPLUPT               Set UPT addr
         ST    R5,PPLECT               Set ECT addr
         LA    R3,PARSECB              -> parse ECB
         ST    R3,PPLECB               Set it
         LA    R3,ANSWER               -> IKJPARS "answer area"
         ST    R3,PPLANS               Set it
         ST    R10,PPLUWA              Set user work area addr
         C     R7,=F'4'                Process prompt parameters?
         BE    PARS010                 Yes
*
*                                   ** Process command line
         ST    R2,PPLCBUF              Set TSO command buffer addr
         L     R3,=A(PCLDEFS)          -> command parms definitions
         ST    R3,PPLPCL               Set it
         B     PARS020
*
PARS010  EQU   *                    ** Process prompt parameters
         ST    R6,PPLCBUF              Set PUTGET input buffer addr
         L     R3,=A(PRMTOPS)          -> prompt parms definitions
         ST    R3,PPLPCL               Set it
*
PARS020  EQU   *
         CALLTSSR EP=IKJPARS,MF=(E,PPLSTG)   Parse command line
         LTR   R0,R15                  Any parse errors?
         BNZ   XITPAR12                Yes
         DROP  R8                      PPL
*
         C     R7,=F'4'                Did we parse prompt parms?
         BE    PARS200                 Yes, examine those
*
*- Examine command line results
         L     R4,ANSWER               -> IKJPARS built PCEs
         USING PRDSECT,R4
*
         LA    R2,FILEPCE              -> File #### PCE data
         TM    6(R2),X'80'             Was file #### specified?
         BZ    PARS025                 No
*
         L     R3,0(,R2)               -> word containing file #
         MVC   FILEID,0(R3)            Save specified spool id #
         OI    FLAGS3,F3FILEID         Indicate file id valid
*
PARS025  EQU   *
         LA    R2,QTPCE                -> QUIET PCE
         CLC   0(2,R2),=AL2(1)         Was QUIET specified?
         BNE   PARS030                 No
         OI    FLAGS3,F3QUIET+F3NPRMPT Indicate QUIET+NOPROMPT
*
PARS030  EQU   *
         LA    R2,PURPCE               -> PURGE/NOPURGE PCE
         CLC   0(2,R2),=AL2(2)         Was NOPURGE specified?
         BNE   PARS035                 No
         NI    FLAGS3,255-F3PURGE      Indicate no purge
*
PARS035  EQU   *
         LA    R2,PRMTPCE              -> PROMPT/NOPROMPT PCE
         CLC   0(2,R2),=AL2(2)         Was NOPROMPT specified?
         BNE   PARS040                 No
         OI    FLAGS3,F3NPRMPT         Indicate no prompts
*
PARS040  EQU   *
         LA    R2,VOLPCE               -> VOLSER PCE
         TM    6(R2),X'80'             Was VOLSER specified?
         BZ    PARS050                 No
         L     R3,0(,R2)               -> VOLSER string
         LH    R1,4(,R2)               Length of volser
         MVC   USRVOL,BLANKS           Init receiving field
         BCTR  R1,0                    Adjust for execute
         EX    R1,MVVOL                Move the volser
         OI    FLAGS3,F3VOLSER         Indicate volser valid
*
PARS050  EQU   *
         LA    R2,INDAPCE              -> INDATASET PCE
         TM    6(R2),X'80'             Was INDATASET specified?
         BZ    PARS080                 No
         MVC   USRINDS,BLANKS          Init receiving field
         LA    R5,USRINDS              -> where to place DSN
*
         TM    6(R2),X'40'             Was dataset name in quotes?
         BO    PARS060                 Y, don't insert prefix
         CLC   PREFIX,BLANKS           Is a prefix available?
         BE    PARS060                 All blank, dont use prefix
*
         MVC   USRINDS(8),PREFIX       Add the prefix
         TRT   USRINDS,BLANK           Look for end of prefix
         MVI   0(R1),C'.'              Set delim after prefix
         LA    R5,1(,R1)               -> place to put rest of dsn
         LA    R2,INDAPCE              -> INDATASET PCE
*
PARS060  EQU   *
         L     R3,0(,R2)               -> INDATASET string
         LH    R1,4(,R2)               Length of DSN
         BCTR  R1,0                    Adjust for execute
         EX    R1,MVINDS               Move the DSN
         OI    FLAGS3,F3INDS           Indicate INDATASET valid
*
PARS070  EQU   *
         TM    14(R2),X'80'            Was INDATASET member specified?
         BZ    PARS080                 No
         L     R3,8(,R2)               -> INDATASET member name
         LH    R1,12(,R2)              Length of member name
         MVC   USRMEM,BLANKS           Init receiving field
         BCTR  R1,0                    Adjust for execute
         EX    R1,MVINMEM              Move the member name
         OI    FLAGS3,F3INMEM          Indicate INDATASET member valid
*
PARS080  EQU   *
         LA    R2,FDAPCE                -> DATASET PCE
         TM    6(R2),X'80'             Was DATASET specified?
         BZ    PARS110                 No
         MVC   FINALDS,BLANKS          Init receiving field
         LA    R5,FINALDS              -> where to place DSN
*
         TM    6(R2),X'40'             Was dataset name in quotes?
         BO    PARS090                 Y, don't insert prefix
         CLC   PREFIX,BLANKS           Is a prefix available?
         BE    PARS090                 All blank, dont use prefix
*
         MVC   FINALDS(8),PREFIX       Add the prefix
         TRT   FINALDS,BLANK           Look for end of prefix
         MVI   0(R1),C'.'              Set delim after prefix
         LA    R5,1(,R1)               -> place to put rest of dsn
         LA    R2,FDAPCE               -> DATASET PCE
*
PARS090  EQU   *
         L     R3,0(,R2)               -> DATASET string
         LH    R1,4(,R2)               Length of DSN
         BCTR  R1,0                    Adjust for execute
         EX    R1,MVINDS               Move the DSN
         OI    FLAGS3,F3DS             Indicate DATASET valid
*
PARS100  EQU   *
         TM    14(R2),X'80'            Was DATASET member specified?
         BZ    PARS110                 No
         OI    FLAGS4,F4MEMINV         Indicate MEMBER specified
*
PARS110  EQU   *                                                   v200
         LA    R2,UNIPCE               -> UNIT PCE                 v200
         TM    6(R2),X'80'             Was UNIT specified?         v200
         BZ    PARS120                 No                          v200
         L     R3,0(,R2)               -> UNIT string              v200
         LH    R1,4(,R2)               Length of unit name         v200
         MVC   USRUNIT,BLANKS          Init receiving field        v200
         BCTR  R1,0                    Adjust for execute          v200
         EX    R1,MVUNIT               Move the unit               v200
         OI    FLAGS2,F2UNIT           Indicate unit valid         v200
*
PARS120  EQU   *                                                   v200
         LA    R2,DIRPCE               -> # dir blocks PCE         v200
         TM    6(R2),X'80'             Was DIR specified?          v200
         BZ    PARS130                 No                          v200
*
         L     R3,0(,R2)               -> word containing # blks   v200
         MVC   USRDIR,0(R3)            Save specified #            v200
         OI    FLAGS2,F2DIR            Indicate DIR valid          v200
*
PARS130  EQU   *                                                   v200
         B     XITPAR00                All done
         DROP  R4                      PRDSECT
*
MVVOL    MVC   USRVOL(0),0(R3)         executed instr
MVUNIT   MVC   USRUNIT(0),0(R3)        executed instr              v200
MVINDS   MVC   0(0,R5),0(R3)           executed instr
MVINMEM  MVC   USRMEM(0),0(R3)         executed instr
*
*
*- Examine prompt parameter results
PARS200  EQU   *
         L     R4,ANSWER               -> IKJPARS built PCEs
         USING PRMSECT,R4
*
PARS220  EQU   *
         LA    R2,ACTPCE               -> PURGE/END PCE
         CLC   0(2,R2),=AL2(1)         Was PURGE specified?
         BNE   PARS230                 No
         OI    FLAGS4,F4PURGE          Indicate purge
*
PARS230  EQU   *
         CLC   0(2,R2),=AL2(2)         Was END specified?
         BNE   PARS240                 No
         OI    FLAGS4,F4END            Indicate END
*
PARS240  EQU   *
         LA    R2,VLPCE                -> VOLSER PCE
         TM    6(R2),X'80'             Was VOLSER specified?
         BZ    PARS250                 No
         L     R3,0(,R2)               -> VOLSER string
         LH    R1,4(,R2)               Length of volser
         MVC   USRVOL,BLANKS           Init receiving field
         BCTR  R1,0                    Adjust for execute
         EX    R1,MVVOL                Move the volser
         OI    FLAGS4,F4VOLSER         Indicate volser valid
*
PARS250  EQU   *
         LA    R2,DAPCE                -> DATASET PCE
         TM    6(R2),X'80'             Was DATASET specified?
         BZ    PARS280                 No
         MVC   FINALDS,BLANKS          Init receiving field
         LA    R5,FINALDS              -> where to place DSN
*
         TM    6(R2),X'40'             Was dataset name in quotes?
         BO    PARS260                 Y, don't insert prefix
         CLC   PREFIX,BLANKS           Is a prefix available?
         BE    PARS260                 All blank, dont use prefix
*
         MVC   FINALDS(8),PREFIX       Add the prefix
         TRT   FINALDS,BLANK           Look for end of prefix
         MVI   0(R1),C'.'              Set delim after prefix
         LA    R5,1(,R1)               -> place to put rest of dsn
         LA    R2,DAPCE                -> DATASET PCE
*
PARS260  EQU   *
         L     R3,0(,R2)               -> DATASET string
         LH    R1,4(,R2)               Length of DSN
         BCTR  R1,0                    Adjust for execute
         EX    R1,MVINDS               Move the DSN
         OI    FLAGS4,F4DS             Indicate DATASET valid
         NI    FLAGS3,255-F3DS         DATASET from cmd line not valid
*
PARS270  EQU   *
         TM    14(R2),X'80'            Was DATASET member specified?
         BZ    PARS280                 No
         OI    FLAGS4,F4MEMINV         Indicate MEMBER specified
*
PARS280  EQU   *
         LA    R2,UNPCE                -> UNIT PCE                 v200
         TM    6(R2),X'80'             Was UNIT specified?         v200
         BZ    PARS290                 No                          v200
         L     R3,0(,R2)               -> UNIT string              v200
         LH    R1,4(,R2)               Length of unit name         v200
         MVC   USRUNIT,BLANKS          Init receiving field        v200
         BCTR  R1,0                    Adjust for execute          v200
         EX    R1,MVUNIT               Move the unit               v200
         OI    FLAGS2,F2UNIT           Indicate unit valid         v200
*
PARS290  EQU   *                                                   v200
         LA    R2,DRPCE                -> # dir blocks PCE         v200
         TM    6(R2),X'80'             Was DIR specified?          v200
         BZ    PARS300                 No                          v200
*
         L     R3,0(,R2)               -> word containing # blks   v200
         MVC   USRDIR,0(R3)            Save specified #            v200
         OI    FLAGS2,F2DIR            Indicate DIR valid          v200
*
PARS300  EQU   *                                                   v200
         B     XITPAR00                All done
         DROP  R4                      PRMSECT
*
*-- Exit
*
XITPAR00 EQU   *
         LA    R1,ANSWER               -> IKJPARS "answer place"
         IKJRLSA (1)                   Release parsing storage
*
         SR    R0,R0               Set secondary RC=0;
         SR    R15,R15             Set RC=0;
         B     XITPAR
*
XITPAR12 EQU   *
         LA    R15,12              Set RC=12; R0 already set by IKJPARS
         B     XITPAR
*
XITPAR   EQU   *
         L     R13,4(,R13)         -> prev s.a.
         L     R14,12(,R13)        Load r14
         LM    R1,R12,24(R13)      Reload callers regs
         BR    R14                 Return with RCs in R0/R15
*
         LTORG
*
*-- IKJPARS Description Macros
*
*-- RECEIVE command parms:
*
*     RECEIVE  #### INDATASET(ddd) VOLSER(vvv) UNIT(uuu) DATASET(iii)
*                   DIR(nnn)
*                   PURGE | NOPURGE
*                   PROMPT | NOPROMPT
*                   QUIET
*
*   Where:
*
*     #### if specified must be the first parm, all numeric spool id
*     ddd is an optional dataset name to RECEIVE from
*     vvv is an optional VOLSER of where to allocate the RECEIVEd data
*     uuu is an optional UNIT of where to allocate the RECEIVEd data
*     iii is an optional DSNAME to RECEIVE into.
*     nnn is an optional number of directory blocks to assign
*     PURGE indicates the spool file is purged after RECEIVE  (DEFAULT)
*     NOPURGE indicates the spool file is retained after RECEIVE
*     PROMPT indicates to prompt user for parameters          (DEFAULT)
*     NOPROMPT no user prompts are issued
*     QUIET  suppress all informational msgs
*
*
PCLDEFS  IKJPARM DSECT=PRDSECT
*
FILEPCE  IKJIDENT 'FILE NUMBER',                                       x
               MAXLNTH=4,FIRST=NUMERIC,OTHER=NUMERIC,                  x
               INTEG
*
PURPCE   IKJKEYWD DEFAULT='PURGE'
         IKJNAME  PURGE            PCE value = 1
         IKJNAME  NOPURGE          PCE value = 2
*
PRMTPCE  IKJKEYWD DEFAULT='PROMPT'
         IKJNAME  PROMPT           PCE value = 1
         IKJNAME  NOPROMPT         PCE value = 2
*
QTPCE    IKJKEYWD
         IKJNAME  QUIET            PCE value = 1
*
INDSPCE  IKJKEYWD
         IKJNAME  'INDATASET',SUBFLD=INDSFLD,ALIAS='INDSNAME'
*
FDSPCE   IKJKEYWD
         IKJNAME  'DATASET',SUBFLD=FDSFLD,ALIAS='DSNAME'
*
VSRPCE   IKJKEYWD
         IKJNAME  'VOLSER',SUBFLD=VOLSFLD,ALIAS='VOLUME'
*
USRPCE   IKJKEYWD ,                                                v200
         IKJNAME  'UNIT',SUBFLD=UNISFLD,ALIAS=('U')                v200
*
DRBPCE   IKJKEYWD ,                                                v200
         IKJNAME  'DIR',SUBFLD=DBSFLD                              v200
*
INDSFLD  IKJSUBF
INDAPCE  IKJPOSIT DSNAME,                                              x
               PROMPT='THE NAME OF THE DATA SET YOU WANT TO RECEIVE FROx
               M'
*
FDSFLD   IKJSUBF
FDAPCE   IKJPOSIT DSNAME,                                              x
               PROMPT='THE NAME OF THE DATA SET YOU WANT TO RECEIVE INTx
               O'
*
VOLSFLD  IKJSUBF
VOLPCE   IKJPOSIT DSTHING,VOLSER,                                      x
               PROMPT='THE VOLUME SERIAL OF THE VOLUME WHERE YOU WANT Tx
               HE DATASET ALLOCATED'
*
UNISFLD  IKJSUBF ,                                                 v200
UNIPCE   IKJIDENT 'UNIT NAME',MAXLNTH=8,FIRST=ALPHANUM,            v200x
               OTHER=ALPHANUM                                      v200
*
DBSFLD   IKJSUBF ,                                                 v200
DIRPCE   IKJIDENT 'DIRECTORY BLOCKS',                              v200x
               MAXLNTH=5,FIRST=NUMERIC,OTHER=NUMERIC,              v200x
               INTEG                                               v200
*
         IKJENDP
*
*-- RECEIVE parameters from prompt:
*
*     DATASET(ddd) VOLSER(vvv) UNIT(uuu) DIR(nnn) PURGE/END
*
*   Where:
*
*     ddd is an alternate dataset name to RECEIVE intp
*     vvv is an optional VOLSER of where to allocate the RECEIVEd data
*     uuu is an optional UNIT of where to allocate the RECEIVEd data
*     nnn is an optional number of directory blocks to assign
*     PURGE indicates the spool file is purged immediately and the
*      RECEIVE operation is aborted.
*     END indicates the RECEIVE operation is aborted with no action.
*
PRMTOPS  IKJPARM DSECT=PRMSECT
*
ACTPCE   IKJKEYWD DEFAULT=
         IKJNAME  PURGE            PCE value = 1
         IKJNAME  END              PCE value = 2
*
DSPCE    IKJKEYWD
         IKJNAME  'DATASET',SUBFLD=DAFLD,ALIAS='DSNAME'
*
VSPCE    IKJKEYWD
         IKJNAME  'VOLSER',SUBFLD=VLFLD,ALIAS='VOLUME'
*
USPCE    IKJKEYWD ,                                                v200
         IKJNAME  'UNIT',SUBFLD=UNFLD,ALIAS=('U')                  v200
*
DBPCE    IKJKEYWD ,                                                v200
         IKJNAME  'DIR',SUBFLD=DRFLD                               v200
*
DAFLD    IKJSUBF
DAPCE    IKJPOSIT DSNAME,                                              x
               PROMPT='THE NAME OF THE DATA SET YOU WANT TO RECEIVE INTx
               O'
*
VLFLD    IKJSUBF
VLPCE    IKJPOSIT DSTHING,VOLSER,                                      x
               PROMPT='THE VOLUME SERIAL OF THE VOLUME WHERE YOU WANT Tx
               HE DATASET ALLOCATED'
*
UNFLD    IKJSUBF ,                                                 v200
UNPCE    IKJIDENT 'UNIT NAME',MAXLNTH=8,FIRST=ALPHANUM,            v200x
               OTHER=ALPHANUM                                      v200
*
DRFLD    IKJSUBF ,                                                 v200
DRPCE    IKJIDENT 'DIRECTORY BLOCKS',                              v200x
               MAXLNTH=5,FIRST=NUMERIC,OTHER=NUMERIC,              v200x
               INTEG                                               v200
*
         IKJENDP
*
*
         IKJPPL
IKJPPLSZ EQU   (*-PPL)/4           # words in PPL
*
         LTORG
*
*                                                                     *
***********************************************************************
**                                                                   **
**                        TASK ESTAE EXIT                            **
**                                                                   **
** This csect handles all abends trapped by ESTAE during the normal  **
** execution of the subtask.          This exit does not attempt     **
** any recovery other than to terminate processing.                  **
** An SVC dump is taken on abends.                                   **
**                                                                   **
** On entry:  R0=ESTAE provide entry code                            **
**            R1=SDWA address                                        **
**            R2=parameter passed on ESTAE macro                     **
**                                                                   **
**                                                                   **
** On exit: If SDWACLUP is 1, then no retry is allowed and this      **
**             exit will allow percolation back to system routines   **
**             to terminate the task.                                **
**                                                                   **
**          If SDWACLUP is 0, then retry is allowed.                 **
**                                                                   **
** Security:  N/A.                                                   **
**                                                                   **
** Register usage:                                                   **
**                                                                   **
**   R1  = SDWA address                                              **
**   R3  = SDWA address                                              **
**   R10 = Dynamic storage area base                                 **
**   R12 = This program base                                         **
**                                                                   **
**                                                                   **
**                                                                   **
***********************************************************************
*
NJEDMP   CSECT
         B     28(,R15)               BRANCH AROUND EYECATCHERS
         DC    AL1(23)                LENGTH OF EYECATCHERS
         DC    CL9'NJEDMP'
         DC    CL9'&SYSDATE'
         DC    CL5'&SYSTIME'
*
         LR    R12,R15                SET UP BASE REG
         USING NJEDMP,R12             ESTABLISH ADDRESSABILITY
         LR    R8,R14                 SAVE RETURN ADDRESS TO SYSTEM
*
         L     R10,0(,R1)             GET VALUE PASSED TO US (WORKA)
         USING NJEWK,R10
         L     R11,=A(NJECOM)          -> common csect
         USING NJECOM,R11
*
         LR    R3,R1                  SAVE R1 ENTRY CONTENTS
         USING SDWA,R3
         LR    R5,R0                  Save R0 entry code
*
         LTR   R3,R3                  Do we have an SDWA?
         BZ    NOSDWA                 Exit if no SDWA
         LA    R13,MVSSAVE            Save area
         ESTAE 0
*
         MODESET MODE=SUP,            Run this ESTAI exit privileged   x
               KEY=ZERO                to access PSW -> storage
*
         MVC   MACLIST(WTOMSGL),WTOMSG
         L     R6,PSATOLD-PSA(0)      -> my TCB
         L     R5,TCBTIO-TCB(,R6)     -> TIOT
         MVC   MACLIST+9(8),0(R5)         Plug in job name
         MVC   MACLIST+4(4),=C'USER'
         MVC   MACLIST+19(7),=C'RECEIVE'  Plug in command name
*
*
LNK020   EQU   *
         MVC   MACLIST+29(5),=C'ABEND'
         L     R5,SDWAABCC              GET ABEND CODE INFO WORD
         N     R5,=X'00FFF000'          KEEP ONLY THE SYSTEM CODE
         BZ    USERCDE                  NONE THERE, MUST BE A USER CODE
         SRL   R5,12                    Put sys code in low order  v201
         C     R5,=X'00000222'          Operator cancel, no dump?  v201
         BE    SDUMP040                 Yes, suppress dump
         CLM   R5,1,=X'3E'              Was it an x3E (DETACH) ?   v201
         BE    SDUMP040                 Yes, suppress dump         v201
*
         MVI   MACLIST+35,C'S'          INDICATE SYSTEM CODE
         UNPK  FWORK(5),SDWACMPC(3)     GET SYSTEM CMP CODE
         TR    FWORK(3),HEXTRAN-240
         MVC   FWORK+3(5),=CL5' '       CLEAR REST OF ABEND CODE
         B     NOREAS
*
USERCDE  EQU   *
         MVI   MACLIST+35,C'U'         INDICATE USER ABEND CODE
         L     R5,SDWAABCC             GET ABEND CODE
         N     R5,=X'00000FFF'         KEEP USER ABEND CODE
         CVD   R5,FSAVE                CONVERT CODE TO DECIMAL
         UNPK  FWORK(4),FSAVE          UNPK THE CODE
         OI    FWORK+3,X'F0'           FIX SIGN
         MVC   FWORK+4(2),=CL2' '      BLANKS AT END OF ABEND CODE
*
NOREAS   EQU   *
         MVC   MACLIST+36(6),FWORK     MOVE ABEND-REASON TO LINE
         MVC   ABCODE,MACLIST+36       Save a copy of formatted abcode
*
         WTO   ,MF=(E,MACLIST)        Write to console
         LA    R2,MACLIST
         BAL   14,PUTLINE             Echo to TSO terminal
*
         MVC   MACLIST(WTOMSGL),WTOMSG
         MVC   MACLIST+4(3),=C'PSW'
         UNPK  FSAVE(9),SDWAEC1(5)    Add zones to PSW word 1
         TR    FSAVE(8),HEXTRAN-240
         MVC   MACLIST+10(8),FSAVE
         UNPK  FSAVE(9),SDWAEC1+4(5)  Add zones to PSW word 2
         TR    FSAVE(8),HEXTRAN-240
         MVC   MACLIST+19(8),FSAVE
*
         SR    R5,R5                   CLEAR FOR IC
         IC    R5,SDWAILC1             GET THE ILC
         CVD   R5,FWORK                MAKE DECIMAL
         MVC   MACLIST+29(3),=C'ILC'
         UNPK  MACLIST+33(2),FWORK     UNPK
         OI    MACLIST+34,X'F0'        FIX THE SIGN
*
         MVC   MACLIST+37(4),=C'INTC'
         UNPK  FWORK(5),SDWAINC1(3)    MAKE INTC DISPLAYABLE
         TR    FWORK(4),HEXTRAN-240
         MVC   MACLIST+42(4),FWORK     MOVE INTC TO LINE
*
         WTO   ,MF=(E,MACLIST)
         LA    R2,MACLIST
         BAL   14,PUTLINE             Echo to TSO terminal
*
         MVC   MACLIST(WTOMSGL),WTOMSG
         MVC   MACLIST+4(13),=C'DATA NEAR PSW'
         MVC   MACLIST+19(8),=CL8'UNAVAIL'  ASSUME WE CANT GET DATA
         L     R4,SDWAEC1+4            Get PSW IA
         LA    R4,0(,R4)               Clear high bit
         C     R4,=F'8'                1st 8 bytes of storage?
         BH    LOC010                  No, its higher than that
         SR    R4,R4                   Yes, just use 0
         B     LOC020
*
LOC010   EQU   *
         S     R4,=F'8'                BACK UP BEFORE INTERRUPT ADDR
*
LOC020   EQU   *
         LRA   R0,0(,R4)               Do we have access?
         BNZ   UNAVAIL                 No translation, better not
         LRA   R0,14(,R4)              Do we have access?
         BNZ   UNAVAIL                 No translation, better not
*
         ST    R4,FWORK                SAVE FOR CONVERSION
         UNPK  FSAVE(9),FWORK(5)       ADD ZONES TO ADDRESS
         TR    FSAVE(8),HEXTRAN-240    MAKE DISPLAYABLE HEX
         MVC   MACLIST+19(8),FSAVE     MOVE DISPLAYABLE
*
         MVC   FWORK(4),0(R4)          MOVE 4 WORDS AT PSW
         UNPK  FSAVE(9),FWORK(5)       ADD ZONES
         TR    FSAVE(8),HEXTRAN-240    MAKE DISPLAYABLE HEX
         MVC   MACLIST+29(8),FSAVE     MOVE TO LINE
*
         MVC   FWORK(4),4(R4)          MOVE 4 WORDS AT PSW
         UNPK  FSAVE(9),FWORK(5)       ADD ZONES
         TR    FSAVE(8),HEXTRAN-240    MAKE DISPLAYABLE HEX
         MVC   MACLIST+38(8),FSAVE     MOVE TO LINE
*
         MVC   FWORK(4),8(R4)          MOVE 4 WORDS AT PSW
         UNPK  FSAVE(9),FWORK(5)       ADD ZONES
         TR    FSAVE(8),HEXTRAN-240    MAKE DISPLAYABLE HEX
         MVC   MACLIST+47(8),FSAVE     MOVE TO LINE
*
         MVC   FWORK(4),12(R4)         MOVE 4 WORDS AT PSW
         UNPK  FSAVE(9),FWORK(5)       ADD ZONES
         TR    FSAVE(8),HEXTRAN-240    MAKE DISPLAYABLE HEX
         MVC   MACLIST+56(8),FSAVE     MOVE TO LINE
*
UNAVAIL  EQU   *
         WTO   ,MF=(E,MACLIST)
         LA    R2,MACLIST
         BAL   14,PUTLINE              Echo to TSO terminal
*----
         LA    R4,4                    4 ROWS OF REGISTERS
         LA    R5,SDWAGR00             POINT TO ABEND REGS
         LA    R6,REGLIST              POINT TO REGISTER ID LITERALS
*
REG000   EQU   *
         MVC   MACLIST(WTOMSGL),WTOMSG
         MVC   MACLIST+4(8),0(R6)      MOVE REGISTERS ID
         LA    R15,MACLIST+13          WHERE 1ST REG GOES ON LINE
         LA    R14,4                   4 REGS PER LINE
*
REG010   EQU   *
         UNPK  FSAVE(9),0(5,R5)        UNPK A REGISTER
         TR    FSAVE(8),HEXTRAN-240    MAKE DISPLAYABLE HEX
         MVC   0(8,R15),FSAVE          MOVE TO THE LINE
         LA    R15,10(,R15)            NEXT SPOT ON PRINT LINE
         LA    R5,4(,R5)               NEXT REGISTER
         BCT   R14,REG010              KEEP DOING REGS
         WTO   ,MF=(E,MACLIST)
         LA    R2,MACLIST
         BAL   14,PUTLINE              Echo to TSO terminal
         LA    R6,8(,R6)               NEXT REGISTER ID
         BCT   R4,REG000               GO DISPLAY THE NEXT ROW
*
*
SDUMP000 EQU   *
         L     R5,SDWAABCC             Get abend code info word
         N     R5,=X'00FFF000'         Keep only the system code
         SRL   R5,12                   Right justify the code
         C     R5,=X'00000222'         Operator cancel, no dump?
         BE    SDUMP040                Yes, skip dump
         CLM   R5,1,=X'37'             x37 abend code?
         BE    SDUMP040                Skip the dump
*
         MVI   DHDR,C' '
         MVC   DHDR+1(29),DHDR
         MVI   DHDR,29                IBM length of header
         L     R5,PSATOLD-PSA(0)      -> my TCB
         L     R5,TCBTIO-TCB(,R5)     -> TIOT
         MVC   DHDR+1(8),0(R5)        Use jobname in description
         MVC   DHDR+11(7),=C'RECEIVE' Use command name
         MVC   DHDR+21(7),ABCODE
*
         MVC   MACLIST(SDUMPL),SDUMP    MOVE SDUMP LIST TO WORK
         LA    R1,MACLIST
         SDUMP HDRAD=DHDR,              ISSUE SDUMP TO RECORD STATUS   x
               BUFFER=NO,                                              x
               QUIESCE=NO,                                             x
               SDATA=(RGN,CSA,LPA,SUM),                                x
               MF=(E,(1))
*
*
SDUMP040 EQU   *
*
SDUMP090 EQU   *
         LR    R1,R3                  SDWA BACK TO R1
         L     R15,=A(NJERCV)         Main csect addr
         ST    R15,SDWASRSV+4*R12     Plug it to R12
         L     R15,=A(EXIT08)         -> RECEIVE exit point
*
         SETRP RC=4,                  Retry - try to shut down RECEIVE x
               DUMP=NO,               Suppress any further dumps       x
               FRESDWA=YES,           Free the SDWA                    x
               RETREGS=YES,           Restore original regs            x
               RETADDR=(15)           Return to Receive exit point
*
NOSDWA   EQU   *                  **  NO RETRY AVAILABLE (OR DESIRED)
         SR    R15,R15                REQUEST PERCOLATION
         LR    R14,R8                 RESTORE RETURN ADDRESS
         BR    R14                    RETURN TO SYSTEM
*
         LTORG
*
SDUMP    SDUMP MF=L
SDUMPL   EQU   *-SDUMP
*
REGLIST  DC    CL8'GR 0-3'
         DC    CL8'GR 4-7'
         DC    CL8'GR 8-11'
         DC    CL8'GR 12-15'
*
WTOMSG   WTO   '                                                       x
                                             ',MF=L
WTOMSGL  EQU   *-WTOMSG
*
         LTORG
*
*
****  Main work area common                                             NJE00290
****  to all NJExxx CSECTs.                                             NJE00290
*                                                                       NJE00290
NJEWK    DSECT
NJEEYE   DS    CL4'NJER'           Eyecatcher
NJEWKLEN DS    F                   Getmain size of this area
*
DBLE     DS    D                   Work area                            NJE00310
TWRK     DS    2D                  Work area
LCLNODE  DS    CL8                 Local node id
DEFUSER  DS    CL8                 Default 'no security' userid
USERID   DS    CL8                 TSO Userid
PREFIX   DS    CL8                 TSO PREFIX
*
*
MACLIST  DS    CL96                   Macro expansion area
STAXLIST DS    CL20                   STAX parameter list
*                                                                       NET02360
CPARMS   DS    A                      -> input CPPL (entry parms)
PUTECB   DS    F                      ECB for PUTLINE/PUTGET
IOPLAREA DS    4A                     IOPL for PUTLINE/PUTGET
DEVINFO  DS    A                      -> Entry selected from disks tbl
SV14CTL  DS    A                      R14 save area                     NET02370
SV14GB   DS    A                      R14 save area                     NET02370
SV14GET  DS    A                      R14 save area                     NET02370
SV14LN   DS    A                      R14 save area                     NET02370
SV14PUR  DS    A                      R14 save area                     NET02370
SVR0CTL  DS    F                      R0 save of # value for a key      NET02370
*
GBREM    DS    F                      # bytes remaining in phys rec     NET02380
GBPOS    DS    A                      -> current position in BUFF       NET02390
GBRPS    DS    A                      -> current position in phys rec   NET02400
GBRBA    DS    F        for debug     RBA of last GETBYTES call         NET02400
GBPBA    DS    F        for debug     RBA of prior GETBYTES call        NET02400
RBUFF    DS    A                      -> Record build area
RBPOS    DS    A                      -> current position in RBUFF      NET02390
*
BLOCKLEN DS    F                      Length of block buffer
BLOCK    DS    A                      -> Block of physical records
NEWLEN   DS    F                      Length of NEWDS RECFM=U buffer
NEWBLK   DS    A                      -> NEWDS RECFM=U build buffer
*
OLD      DS    F                      For PUTGET, # segments
OLDMSGAD DS    A                      -> msg len/text
*
PARSECB  DS    F                      IKJPARS ECB
ANSWER   DS    F                      IKJPARS Answer area
PPLSTG   DS    (IKJPPLSZ)A            Space for PPL
FILEID   DS    F                      User specified spool id #
USRDIR   DS    F                      User specified # of dir blks v200
USRVOL   DS    CL6                    User specified VOLSER
USRUNIT  DS    CL8                    User specified UNIT name     v200
USRINDS  DS    CL44                   User specified INDATASET
USRMEM   DS    CL8                    User specified INDATASET member
FINALDS  DS    CL44                   Final dataset name
*
*
FLAGS1   DS    X                      Flag bits
F1INMR01 EQU   X'80'  1... ....        INMR01 fields processed
F1INMR2A EQU   X'40'  .1.. ....        1st INMR02 fields processed
F1INMR2B EQU   X'20'  ..1. ....        2nd INMR02 fields processed
F1INMR03 EQU   X'10'  ...1 ....        INMR03 fields processed
F1BATCH  EQU   X'08'  .... 1...        Running in BATCH TSO
F1ACEE   EQU   X'04'  .... .1..        Security is available on system
F1AUSR   EQU   X'02'  .... ..1.        Special user
F1APF    EQU   X'01'  .... ...1        Authorized at invocation
*
FLAGS2   DS    X                      Flag bits
F2NETOPN EQU   X'80'  1... ....        NETDATA DCB open
F2NCBOPN EQU   X'40'  .1.. ....        NETSPOOL NCB open
F2NEWOPN EQU   X'20'  ..1. ....        NEWDS DCB open
F2TERM   EQU   X'10'  ...1 ....        INMTERM text unit detected
F2DIR    EQU   X'08'  .... 1...        DIR (CMD -OR- PROMPT) spec. v200
F2FLAT   EQU   X'04'  .... .1..        Incoming file is a flat file
F2UNIT   EQU   X'02'  .... ..1.        UNIT (CMD -OR- PROMPT) spec.v200
F2FEND   EQU   X'01'  .... ...1        Force END in batch after 1st pmt
*                     .... ....        available bits
*
FLAGS3   DS    X                      Flag bits from CMD line parse
F3FILEID EQU   X'80'  1... ....        Spool file id specified
F3PURGE  EQU   X'40'  .1.. ....        1=PURGE, 0=NOPURGE
F3VOLSER EQU   X'20'  ..1. ....        VOLSER specified
F3INDS   EQU   X'10'  ...1 ....        INDATASET specified
F3INMEM  EQU   X'08'  .... 1...        INDATASET MEMBER specified
F3DS     EQU   X'04'  .... .1..        DATASET specified
F3NPRMPT EQU   X'02'  .... ..1.        NOPROMPT was specified
F3QUIET  EQU   X'01'  .... ...1        QUIET was specified
*
FLAGS4   DS    X                      Flag bits from prompt parse
F4MEMINV EQU   X'80'  1... ....        DATASET MEMBER specified (inval)
F4PURGE  EQU   X'40'  .1.. ....        PURGE (delete spool file & exit)
F4VOLSER EQU   X'20'  ..1. ....        VOLSER specified
F4DS     EQU   X'10'  ...1 ....        DATASET specified
F4END    EQU   X'08'  .... 1...        END (take no action and exit)
F4ATTN   EQU   X'01'  .... ...1        User pressed ATTN key       v201
*                     .... .xx.        available bits
*                                                                       NET02470
INMF01   DS    (INMFSZ)X              Fields from INMR01 record
INMF02A  DS    (INMFSZ)X              Fields from 1st INMR02 record
INMF02B  DS    (INMFSZ)X              Fields from 2nd INMR02 record
INMF03   DS    (INMFSZ)X              Fields from INMR03 record
*                                                                       NET02590
         DS    0F
BUFF     DS    CL256                  GB buffer containing request data NET02600
LIST     DS    CL133                  Print line                   v200
REC      DS    CL133                  Physical record from spool
*
*----
LS99PTR  DS    A                       PTR TO S99RB
LS99RB   DS    XL20                    SPACE FOR S99RB
*
TXTPTRS  DS    15A                     -> Text unit ptr list
*
         DS    0H
UTXT     DS    0XL06,Y,AL2,AL2         DDNAME Unallocation
UDDNAME  DS    CL8                      DDNAME
*
         DS    0H
TXT01    DS    0XL06,Y,AL2,AL2         Return DDNAME
TDDNAME  DS    CL8                      DDNAME
*
         DS    0H
TXT02    DS    0XL06,Y,AL2,AL2         DSN=
TDSNAME  DS    CL44                     DSNAME
*
         DS    0H
TXT03    DS    0XL07,Y,AL2,AL2,X       DISP=(NEW,
*
         DS    0H
TXT04    DS    0XL07,Y,AL2,AL2,X       DISP=(,CATLG)
*
         DS    0H
TXT05    DS    0XL06,Y,AL2,AL2         SPACE BLOCK LEN
TBLKLEN  DS    XL3                      BLKLEN
*
         DS    0H
TXT06    DS    0XL06,Y,AL2,AL2         SPACE PRIMARY
TPRIME   DS    XL3                      Primary
*
         DS    0H
TXT07    DS    0XL06,Y,AL2,AL2         SPACE SECONDARY
TSECND   DS    XL3                      Secondary
*
         DS    0H
TXT08    DS    0XL06,Y,AL2,AL2         SPACE DIRECTORY BLOCKS
TDIRBLKS DS    XL3                      DIR BLKS
*
         DS    0H
TXT09    DS    0XL06,Y,AL2,AL2         VOLUME
TVOLSER  DS    CL6                      VOLSER
*
         DS    0H
TXT10    DS    0XL14,Y,AL2,AL2         UNIT                        v200
TUNIT    DS    CL8                      UNITNAME                   v200
*
         DS    0H
TXT11    DS    0XL06,Y,AL2,AL2         EXPDT
TEXPDT   DS    CL5                      EXPDT=yyddd
*
         DS    0H
TXT12    DS    0XL06,Y,AL2,AL2         BLKSIZE
TBLKSIZE DS    XL2                      BLKSIZE
*
         DS    0H
TXT13    DS    0XL06,Y,AL2,AL2         DSORG
TDSORG   DS    XL2                      DSORG
*
         DS    0H
TXT14    DS    0XL06,Y,AL2,AL2         LRECL
TLRECL   DS    XL2                      LRECL
*
         DS    0H
TXT15    DS    0XL06,Y,AL2,AL2         RECFM
TRECFM   DS    XL1                      RECFM
*
         DS    0H
TXT16    DS    0XL04,Y,AL2             DUMMY
*
         DS    0H
TXT17    DS    0XL04,Y,AL2             SYSOUT
*
         DS    0H
TXT18    DS    0XL04,Y,AL2             TERM
*
         DS    0H
TXT19    DS    0XL04,Y,AL2             CYLINDER
*
         DS    0H
TXT20    DS    0XL04,Y,AL2             FREE=CLOSE
*---
*
CTL      DS    X                      Segment descriptor byte
*
*
         DS    0F
TAGDATA  DS    XL108                  TAG data area
TYPPRT   EQU   X'40'                   PRT dev
TYPPUN   EQU   X'80'                   PUN dev
*
NCB1     DS    XL48                   NCB for Spool Access
NETDATA  DS    (DMYNPSL)X             NETDATA DCB
NEWDS    DS    (DMYSEQL)X             New dataset DCB
DECB     DS    (READL)X               DECB for NETDATA
*
CPYPLIST DS    XL(COPYPRML)            IEBCOPY PARM FIELD
*
         DS    0H
DDLISTL  DS    AL2(DDLISTSZ)           DDNAME LIST LENGTH
DDLIST   DS    4XL8'00'                    FOUR DDNAMES UNDEFINED
DDSYSIN  DS    CL8    DDNAME representing IEBCOPY's SYSIN
DDSYSPR  DS    CL8    DDNAME representing IEBCOPY's SYSPRINT
         DS    XL8'00'                     UNDEFINED DD
DDSYSUT1 DS    CL8    DDNAME of ds created by INMRCOPY INMR02 (SYSUT1)
DDSYSUT2 DS    CL8    DDNAME representing IEBCOPY's SYSUT2
DDSYSUT3 DS    CL8    DDNAME representing IEBCOPY's SYSUT3
         DS    XL8'00'                     SYSUT4 UNUSED
DDLISTSZ EQU   *-DDLIST                LENGTH OF DDLIST for IEBCOPY
DDNETDAT DS    XL8'00'                 INDATASET DDNAME
DDNETSPL DS    XL8'00'                 NETSPOOL DDNAME
UNLISTSZ EQU   *-DDLIST                TOTAL of all DDs in list
*
*-- ESTAE exit used areas
*
FSAVE    DS    2D
FWORK    DS    D
DHDR     DS    CL30
ABCODE   DS    CL7
MVSSAVE  DS    18F                 ESTAE exit OS save
*-- End of ESTAE area
*
*
NJESA    DS    18F                    NJERCV OS save area               NJE00300
NETSA    DS    18F                    NJENET OS save area               NJE00300
DYNSA    DS    18F                    NJEDYN OS save area               NJE00300
PARSA    DS    18F                    NJEPAR OS save area               NJE00300
NOTSA    DS    18F                    NJENOT OS save area               NJE00300
*
         DS    0D                     Force doubleword size
NJEWKSZ  EQU   *-NJEWK
*                                                                       NJE00930
         CVT   DSECT=YES,PREFIX=NO
         IEFZB4D0
         IEFZB4D2
         DCBD  DSORG=PS,DEVD=DA
*
IEFUCBOB DSECT
         IEFUCBOB LIST=YES
         IHAPSA
         IKJTCB
         IHASDWA
IEFTIOT  DSECT
         IEFTIOT1
         IHAASCB
         IHAASXB
         IKJUPT
         IKJCPPL
         IKJPGPB
         IKJIOPL
*
ACEE     DSECT                         Maps a portion of ACEE in MVS3.8
ACEEEYE  DS    CL4'ACEE'
         DS    16X
ACEEUSRL DS    X                       Length of userid
ACEEUSR  DS    CL8                     Userid
*
         COPY  NETSPOOL                                                 NJE00940
         COPY  TAG
*
         END   NJERCV                                                   NJE01000
@@
//*
//* All steps should receive COND CODE 0
//*
//ASSEM    PROC R=RENT,M=
//ASSEMBLE EXEC  PGM=IFOX00,REGION=4096K,
// PARM=('XREF(FULL),OBJ,SYSPARM((ON,GEN,NODATA,YES,YES))',
//       'NODECK,&R')
//SYSLIB   DD    DSN=&&MACLIB,DISP=SHR,DCB=BLKSIZE=32720
//         DD    DSN=SYS1.SMPMTS,DISP=SHR
//         DD    DSN=SYS1.SMPSTS,DISP=SHR
//         DD    DSN=SYS1.MACLIB,DISP=SHR
//         DD    DSN=SYS1.AMODGEN,DISP=SHR
//SYSUT1   DD    DSN=&&SYSUT1,UNIT=SYSDA,SPACE=(1700,(5600,500))
//SYSUT2   DD    DSN=&&SYSUT2,UNIT=SYSDA,SPACE=(1700,(1300,500))
//SYSUT3   DD    DSN=&&SYSUT3,UNIT=SYSDA,SPACE=(1700,(1300,500))
//SYSPRINT DD SYSOUT=*
//SYSPUNCH DD SYSOUT=B
//SYSGO    DD DSN=&&NJE38OBJ(&M),DISP=(MOD,PASS),
//            SPACE=(800,(2000,1000,10)),UNIT=SYSDA
//SYSIN    DD DSN=&&ASMSRC(&M),DISP=SHR
// PEND
//* ***********************************
//         EXEC ASSEM,M=NJERCV,R=RENT
//         EXEC ASSEM,M=NJESPOOL,R=RENT
//         EXEC ASSEM,M=NJESYS,R=RENT
//         EXEC ASSEM,M=NJETRN,R=RENT
//* ***********************************
//*
//LKCMDLIB EXEC PGM=IEWL,PARM='XREF,LET,LIST,NCAL,RENT',COND=(4,LT)
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  DSN=&&SYSUT1,UNIT=SYSDA,SPACE=(1024,(50,20))
//SYSLMOD  DD  DSN=SYS2.CMDLIB,DISP=SHR
//NJEOBJ   DD  DSN=&&NJE38OBJ,DISP=(OLD,PASS)
//SYSLIN   DD *
        ORDER NJERCV(P)
        INCLUDE NJEOBJ(NJERCV)
        INCLUDE NJEOBJ(NJESYS)
        INCLUDE NJEOBJ(NJESPOOL)
        ENTRY NJERCV
        SETCODE AC(1)
        ALIAS RECV
   NAME RECEIVE(R)
        ORDER NJETRN(P)
        INCLUDE NJEOBJ(NJETRN)
        INCLUDE NJEOBJ(NJESYS)
        INCLUDE NJEOBJ(NJESPOOL)
        ENTRY NJETRN
        SETCODE AC(1)
        ALIAS XMIT
   NAME TRANSMIT(R)
//*
//* Edit SYS1.UMODSRC(IKJEFTE2) Adding NJE38 programs
//* that need auth
//*
//EDITUMOD EXEC PGM=IKJEFT01,REGION=1024K,DYNAMNBR=50
//SYSPRINT DD  SYSOUT=*
//SYSTSPRT DD  SYSOUT=*
//SYSTERM  DD  SYSOUT=*
//SYSTSIN  DD  *
 EDIT 'SYS1.UMODSRC(IKJEFTE2)' DATA 
 LIST
 TOP
 FIND /TERMINATOR/
 UP
 INSERT          DC    C'RECEIVE '             NJE38 RECEIVE
 INSERT          DC    C'RECV    '             NJE38 RECEIVE Alias
 INSERT          DC    C'TRANSMIT'             NJE38 TRANSMIT
 INSERT          DC    C'XMIT    '             NJE38 TRANSMIT Alias
 LIST
 END SAVE
/*
//*
//* Add Help files
//* 
//HELP     EXEC PGM=PDSLOAD
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DSN=SYS2.HELP,DISP=SHR
//SYSUT1   DD  DATA,DLM=@@
./ ADD NAME=TRANSMIT
)F FUNCTION - NJE38 - TSO TRANSMIT used to create XMIT files

)X SYNTAX  -
         TRANSMIT  DATASET('DSN') OUTDATASET('DSN') 
            [VOLSER(PUB000)] [UNIT(3390)] [PDS]|[SEQ] [QUIET]
                                  
  REQUIRED - DATASET() OUTDATASET()
  DEFAULTS - VOLSER(PUBLIC) UNIT(SYSDA) SEQ
  ALIAS    - XMIT
  EXAMPLE - a user is logged on to TSO with userid FRED:

    Encode dataset HERC02.COBOL.LISTING into FRED.NETLIB:

       TRANSMIT da('herc02.cobol.listing') out(netlib)

  
)O OPERANDS -
))node.userid      - optional. specifies the destination of the 
                     transmission

    DATASET( )     - specifies the dsname of the dataset to be
                     transmitted.  May optionally specify a member.

    OUTDATASET( )  - required. Specifies the encoded file is to be
                     written to this dataset instead of being
                     transmitted.  'node.userid' may be omitted if
                     OUTDATASET is specified, but if it is present
                     then the specified node and userid will be part
                     of the encoded data instead of meaningless
                     defaults. If OUTDATASET is specified, the
                     named dataset will be used if it exists, other-
                     wise it will be created.
                     The contents of OUTDATASET can be input to a
                     RECEIVE command by the use of RECEIVE INDATASET.

))VOLSER( )        - optional. Specifies a volume where OUTDATASET
                     should be created.  If not specified, a PUBLIC
                     volume will be selected.

))UNIT( )          - optional. Specifies a unit name where OUTDATASET
                     should be created.  If not specified, SYSDA is
                     the default unit name.

))PDS              - If specified, indicates that the member name
                     specified with DATASET is to be transmitted
                     with IEBCOPY unload, thereby preserving the
                     user directory data in the source PDS.

))SEQUENTIAL       - DEFAULT. Indicates that any member name specified
                     with DATASET is to be transmitted as a sequential
                     file; no directory information is part of the
                     transmission. SEQL must be specified or defaulted
                     if the destination host is a VM system.

))QUIET            - If specified, indicates that all informational
                     messages from TRANSMIT are suppressed.  Error
                     messages will always be displayed.
./ ADD NAME=RECEIVE
)F FUNCTION - NJE38 - TSO RECEIVE
)X SYNTAX  -
         RECEIVE  DATASET('DSN') INDATASET('DSN') 
            [VOLSER(PUB000)] [UNIT(3390)] 
            [PURGE]|[NOPURGE] [PROMPT]|[NOPROMPT] [QUIET]
                                  
  REQUIRED - DATASET() INDATASET()
  DEFAULTS - VOLSER(PUBLIC) UNIT(SYSDA) PURGE PROMPT
  ALIAS    - RECV
  EXAMPLE - a user is logged on to TSO with userid FRED:

    Decode dataset FRED.NETLIB to HERC02.COBOL.LISTING:

       RECEIVE INDATASET('herc02.cobol.listing') DATASET(netlib)


)O OPERANDS -

    DATASET( )     - specifies the dsname of the dataset to be
                     created; the received data will be placed within.
                     If not specified, the dataset name will be
                     derived from the incoming dataset name, with
                     the first qualifer being replaced by the
                     receiver's TSO userid.

    VOLSER( )      - specifies a volume where DATASET should be
                     created.  If not specified, a PUBLIC volume will
                     be chosen based on the receiving dataset's
                     attributes.

    UNIT( )        - specifies a unit name where DATASET should be
                     created.  If not specified, SYSDA is the default
                     unit name.

    DIR( )         - specifies a number of directory blocks if
                     incoming file was a PDSE.

    INDATASET( )  -  optional. Specifies that the encoded named
                     dataset is to be received.  The encoded dataset
                     was previously created by TRANSMIT using
                     OUTDATASET.  May optionally specify a membername.

    PURGE          - DEFAULT.  Indicates that RECEIVE is to purge
                     the spool file after successful retrieval. Has
                     no meaning if INDATASET is specified.

    NOPURGE        - Indicates that RECEIVE is to retain the spool
                     file.  The file can be received again or must be
                     removed from the spool by other means.  Has
                     no meaning if INDATASET is specified.

    PROMPT         - DEFAULT.  Indicates that RECEIVE is to prompt
                     the TSO user to respecify DATASET or VOLSER
                     after learning the incoming dataset name. The
                     user can then choose to change the name or
                     volume.

    NOPROMPT       - Indicates that no prompts are to be issued.  If
                     errors are encountered, such as the incoming
                     dataset name already existing, then RECEIVE is
                     terminated without any opportunity to change
                     the parameters.

    QUIET          - If specified, indicates that all informational
                     messages from  RECEIVE are suppressed.  Error
                     messages will always be displayed. QUIET also
                     forces on NOPROMPT.
@@
/*
//*
//* Install NJE001 Usermod to IKJEFTE2
//*
//NJSMPASM EXEC SMPASML,M=IKJEFTE2,COND=(0,NE)
//*
//NJERECV  EXEC SMPAPP,COND=(0,NE),WORK=SYSALLDA
//SMPPTFIN DD  *
++USERMOD(NJE0001)
  .
++VER(Z038)
  FMID(EBB1102)
  PRE(JLM0003)
  .
++MOD(IKJEFTE2)
  DISTLIB(AOST4)
  LKLIB(UMODLIB)
  .
/*
//SMPCNTL  DD  *
 RECEIVE
         SELECT(NJE0001)
         .
  APPLY
        SELECT(NJE0001)
        DIS(WRITE)
        .
/*
//*
//