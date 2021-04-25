//SYSGEN05 JOB (SYSGEN),'SETUP VTAM/TSO',
//             CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1)
/*JOBPARM LINES=100
//JOBCAT   DD  DSN=SYS1.VSAM.MASTER.CATALOG,DISP=SHR
//*
//*        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//*        !!                                !!
//*        !! DO NOT RENUMBER THIS JOBSTREAM !!
//*        !!                                !!
//*        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//*
//* ***************************************************************** *
//* VTAM/TSO CONFIGURATION USED IS ALMOST ENTIRELY WITHOUT CHANGES AS *
//* IT WAS INCLUDED BY JIM MORRISON WITH HIS 3375/3380/3390 USERMODS. *
//* ***************************************************************** *
//*
//*--------------------------------------------------------------------
//ASMLKED  PROC U=3350,                  UNIT FOR WORK DATSETS
//             M=MISSING                MEMBER NAME TO BE ASSEMBLED
//ASM      EXEC PGM=ASMBLR,REGION=1024K,
//             PARM='TERM,LIST,NOXREF,NODECK,OBJ'
//STEPLIB  DD  DSN=SYS1.LINKLIB,UNIT=3350,VOL=SER=MVSRES,DISP=SHR
//SYSLIB   DD  DISP=SHR,DSN=SYS1.MACLIB
//         DD  DISP=SHR,DSN=SYS1.AMODGEN
//SYSUT1   DD  UNIT=&U,SPACE=(1700,(600,100))
//SYSUT2   DD  UNIT=&U,SPACE=(1700,(600,100))
//SYSUT3   DD  UNIT=&U,SPACE=(1700,(600,100))
//SYSPRINT DD  SYSOUT=*
//SYSTERM  DD  SYSOUT=*
//SYSGO    DD  DISP=(,PASS),UNIT=&U,SPACE=(80,(200,50))
//SYSPUNCH DD  DUMMY,DCB=BLKSIZE=80
//SYSIN    DD  DISP=SHR,DSN=SYS1.VTAMSRC(&M)
//LKED     EXEC PGM=IEWL,REGION=512K,
//             PARM='XREF,MAP,LET,LIST,NCAL',
//             COND=(8,LT,ASM)
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  UNIT=&U,SPACE=(1024,(50,20))
//SYSLIN   DD  DSN=*.ASM.SYSGO,DISP=(OLD,DELETE)
//         DD  DDNAME=SYSIN
//SYSLMOD  DD  DISP=SHR,DSN=SYS1.VTAMLIB(&M)
//         PEND
//*
//*--------------------------------------------------------------------
//UPDATE01 EXEC PGM=IEBUPDTE,PARM=NEW
//*
//* ***************************************************************** *
//* SYS1.PARMLIB: CREATE IKJTSO00 (TSO PARAMETERS)                    *
//* ***************************************************************** *
//*
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DSN=SYS1.PARMLIB,DISP=MOD
//SYSIN    DD  *
./ ADD NAME=IKJTSO00,LIST=ALL
./ NUMBER NEW1=10,INCR=10
USERMAX=8,                       /* MAXIMUM USERS                    */
RECONLIM=120,                    /* MAXIMUM DISCONNECT MINUTES       */
BUFRSIZE=132,                    /* VTIOC BUFFER SIZE                */
HIBFREXT=13200,                  /* MAX BUFFERS BEFORE SWAP OUT      */
LOBFREXT=6600,                   /* MIMIMUM BUFFERS BEFORE SWAP IN   */
MODE=NOBREAK,                    /* KEYBOARD LOCK OPTION             */
MODESW=NO,                       /* MODESWITCH FROM TERMINAL OPTION  */
CHNLEN=4,                        /* NO. OF RU'S PER CHAIN            */
SCRSIZE=1920                     /* MAXIMUM SCREEN SIZE              */
./ ENDUP
//* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -UPDATE01
//*
//UPDATE02 EXEC PGM=IEBUPDTE,PARM=NEW
//*
//* ***************************************************************** *
//* SYS1.PROCLIB: CREATE IKJACCNT (TSO USER PROCEDURE)                *
//*                      NET (VTAM PROCEDURE)                         *
//*                      TSO (TSO PROCEDURE)                          *
//* ***************************************************************** *
//*
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DSN=SYS1.PROCLIB,DISP=MOD
//SYSIN    DD  DATA,DLM='><'
./ ADD NAME=IKJACCNT,LIST=ALL
./ NUMBER NEW1=10,INCR=10
//IKJACCNT PROC
//IKJACCNT EXEC PGM=IKJEFT01,DYNAMNBR=20,
//             PARM='EX ''SYS1.CMDPROC(TSOLOGON)'''
//SYSHELP  DD  DISP=SHR,DSN=SYS1.HELP
//         DD  DISP=SHR,DSN=SYS2.HELP
//SYSPROC  DD  DISP=SHR,DSN=SYS1.CMDPROC
//DD1      DD  DYNAM
//DD2      DD  DYNAM
//DD3      DD  DYNAM
//DD4      DD  DYNAM
//DD5      DD  DYNAM
//DD6      DD  DYNAM
//DD7      DD  DYNAM
//DD8      DD  DYNAM
//DD9      DD  DYNAM
//DDA      DD  DYNAM
//DDB      DD  DYNAM
//DDC      DD  DYNAM
//DDD      DD  DYNAM
//DDE      DD  DYNAM
//DDF      DD  DYNAM
./ ADD NAME=NET,LIST=ALL
./ NUMBER NEW1=10,INCR=10
//NET      PROC
//IEFPROC  EXEC PGM=ISTINM01,TIME=1440,REGION=4096K,DPRTY=(14,15)
//VTAMLST  DD  DSN=SYS1.VTAMLST,DISP=SHR
//VTAMLIB  DD  DSN=SYS1.VTAMLIB,DISP=SHR
//VTAMOBJ  DD  DSN=&&NETOBJ,DISP=(NEW,DELETE,DELETE),
//             UNIT=SYSDA,SPACE=(CYL,(5,5,10)),
//             DCB=(RECFM=FB,LRECL=80,BLKSIZE=3120)
./ ADD NAME=TSO,LIST=ALL
./ NUMBER NEW1=10,INCR=10
//TSO      PROC MEM=00
//STEP1    EXEC PGM=IKTCAS00,TIME=1440
//PARMLIB  DD  DISP=SHR,DSN=SYS1.PARMLIB(IKJTSO&MEM),FREE=CLOSE
//PRINTOUT DD  SYSOUT=*,DCB=(LRECL=133,RECFM=FB)
./ ENDUP
><
//* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -UPDATE02
//*
//UPDATE03 EXEC PGM=IEBUPDTE,PARM=NEW
//*
//* ***************************************************************** *
//* SYS1.VTAMLST: ALLOCATE/CATALOG ON MVS000
//*               CREATE ATCSTR00                                     *
//*               CREATE ATCCON00                                     *
//*               CREATE APPLTSO                                      *
//*               CREATE LCL400 (DEFINE LOCAL 3277 TERMINALS)         *
//* ***************************************************************** *
//*
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DSN=SYS1.VTAMLST,DISP=(,CATLG),
//             UNIT=3350,VOL=SER=MVS000,SPACE=(CYL,(5,5,10)),
//             DCB=(RECFM=FB,LRECL=80,BLKSIZE=3120)
//SYSIN    DD  *
./ ADD NAME=ATCSTR00,LIST=ALL
./ NUMBER NEW1=10,INCR=10
CONFIG=00,                         /*CONFIG LIST SUFFIX              */+
SSCPID=01,                         /*THIS VTAMS ID IN NETWORK        */+
NETSOL=YES,                        /*NETWORK SOLICITOR OPTION        */+
MAXSUBA=31,                        /*MAXIMUM SUBAREAS IN NETWORK     */+
NOPROMPT,                          /*OPERATOR PROMPT OPTION          */+
SUPP=NOSUP,                        /*MESSAGE SUPPRESSION OPTION      */+
COLD,                              /*RESTART OPTION   - COLD/WARM    */+
APBUF=(128,,064),                  /*ACE STORAGE POOL                */+
CRPLBUF=(256,,44),                 /*RPL COPY POOL                   */+
IOBUF=(20,3992,10,F),              /*FIXED IO (GP-5/2009)            */+
LFBUF=(016,,16,F),                 /*LARGE FIXED BUFFER POOL         */+
LPBUF=(032,,32,F),                 /*LARGE PAGEBLE BUFFER POOL       */+
NPBUF=(032,,08,F),                 /*NON WS FMCB                     */+
PPBUF=(20,3992,10,F),              /*PAGEBLE IO (GP-5/2009)          */+
SFBUF=(032,,32,F),                 /*SMALL FIXED BUFFER POOL         */+
SPBUF=(032,,32,F),                 /*SMALL PGBL BUFFER POOL          */+
UECBUF=(32,,16,F),                 /*USER EXIT CB                    */+
WPBUF=(64,,64,F)                   /*MESSAGE CONTROL BUFFER POOL     */
./ ADD NAME=ATCCON00,LIST=ALL
./ NUMBER NEW1=10,INCR=10
APPLTSO,                                             TSO APPLS         X
LCL400                                               LOCAL 3270S
./ ADD NAME=APPLTSO,LIST=ALL
./ NUMBER NEW1=10,INCR=10
TSO      APPL AUTH=(PASS,NVPACE,TSO),BUFFACT=5
TSO0001  APPL AUTH=(PASS,NVPACE,TSO),BUFFACT=5
TSO0002  APPL AUTH=(PASS,NVPACE,TSO),BUFFACT=5
TSO0003  APPL AUTH=(PASS,NVPACE,TSO),BUFFACT=5
TSO0004  APPL AUTH=(PASS,NVPACE,TSO),BUFFACT=5
TSO0005  APPL AUTH=(PASS,NVPACE,TSO),BUFFACT=5
TSO0006  APPL AUTH=(PASS,NVPACE,TSO),BUFFACT=5
TSO0007  APPL AUTH=(PASS,NVPACE,TSO),BUFFACT=5
TSO0008  APPL AUTH=(PASS,NVPACE,TSO),BUFFACT=5
./ ADD NAME=LCL400,LIST=ALL
./ NUMBER NEW1=10,INCR=10
LCL400   LBUILD SUBAREA=2
CUU400   LOCAL TERM=3277,CUADDR=400,ISTATUS=ACTIVE,                    +
               LOGTAB=LOGTAB01,LOGAPPL=NETSOL,                         +
               FEATUR2=(MODEL2,PFK)
CUU401   LOCAL TERM=3277,CUADDR=401,ISTATUS=ACTIVE,                    +
               LOGTAB=LOGTAB01,LOGAPPL=NETSOL,                         +
               FEATUR2=(MODEL2,PFK)
CUU402   LOCAL TERM=3277,CUADDR=402,ISTATUS=ACTIVE,                    +
               LOGTAB=LOGTAB01,LOGAPPL=NETSOL,                         +
               FEATUR2=(MODEL2,PFK)
CUU403   LOCAL TERM=3277,CUADDR=403,ISTATUS=INACTIVE,                  +
               LOGTAB=LOGTAB01,LOGAPPL=NETSOL,                         +
               FEATUR2=(MODEL2,PFK)
CUU404   LOCAL TERM=3277,CUADDR=404,ISTATUS=INACTIVE,                  +
               LOGTAB=LOGTAB01,LOGAPPL=NETSOL,                         +
               FEATUR2=(MODEL2,PFK)
CUU405   LOCAL TERM=3277,CUADDR=405,ISTATUS=INACTIVE,                  +
               LOGTAB=LOGTAB01,LOGAPPL=NETSOL,                         +
               FEATUR2=(MODEL2,PFK)
CUU406   LOCAL TERM=3277,CUADDR=406,ISTATUS=INACTIVE,                  +
               LOGTAB=LOGTAB01,LOGAPPL=NETSOL,                         +
               FEATUR2=(MODEL2,PFK)
CUU407   LOCAL TERM=3277,CUADDR=407,ISTATUS=INACTIVE,                  +
               LOGTAB=LOGTAB01,LOGAPPL=NETSOL,                         +
               FEATUR2=(MODEL2,PFK)
./ ENDUP
//* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -UPDATE03
//*
//UPDATE04 EXEC PGM=IEBUPDTE,PARM=NEW
//*
//* ***************************************************************** *
//* SYS1.VTAMSRC: ALLOCATE/CATALOG ON MVS000
//*               CREATE LOGTAB01 (VTAM LOGON INTERPRET TABLE)        *
//*               CREATE LOGMOD01 (VTAM LOGMODE TABLE)                *
//* ***************************************************************** *
//*
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DSN=SYS1.VTAMSRC,DISP=(,CATLG),
//             UNIT=3350,VOL=SER=MVS000,SPACE=(CYL,(1,1,10)),
//             DCB=(RECFM=FB,LRECL=80,BLKSIZE=3120)
//SYSIN    DD  *
./ ADD NAME=LOGTAB01,LIST=ALL
./ NUMBER NEW1=10,INCR=10
***********************************************************************
*  VTAM LOGON INTERPRET TABLE                                         *
***********************************************************************
LOGTAB01 INTAB
         LOGCHAR APPLID=(APPLICID,TSO),SEQNCE='LOGON'
         LOGCHAR APPLID=(APPLICID,TSO),SEQNCE='logon'
         LOGCHAR APPLID=(APPLICID,TSO),SEQNCE='TSO'
         LOGCHAR APPLID=(APPLICID,TSO),SEQNCE='tso'
         ENDINTAB
         END
./ ADD NAME=LOGMOD01,LIST=ALL
./ NUMBER NEW1=10,INCR=10
***********************************************************************
*  VTAM LOGMODE TABLE                                                 *
***********************************************************************
LOGMOD01 MODETAB
*****************************************************************
* NON-SNA 3270 LOCAL TERMINALS                                  *
*      PRIMARY SCREEN   : MODEL 2                               *
*      SECONDARY SCREEN : NON                                   *
*****************************************************************
S3270    MODEENT LOGMODE=S3270,                                        X
               FMPROF=X'02',                                           X
               TSPROF=X'02',                                           X
               PRIPROT=X'71',                                          X
               SECPROT=X'40',                                          X
               COMPROT=X'2000',                                        X
               PSERVIC=X'000000000000000000000200'
*****************************************************************
* NON-SNA 3270 LOCAL TERMINALS                                  *
*      PRIMARY SCREEN   : MODEL 5                               *
*      SECONDARY SCREEN : NON                                   *
*****************************************************************
S32785   MODEENT LOGMODE=S32785,                                       X
               FMPROF=X'02',                                           X
               TSPROF=X'02',                                           X
               PRIPROT=X'71',                                          X
               SECPROT=X'40',                                          X
               COMPROT=X'2000',                                        X
               PSERVIC=X'00000000000018501B847F00'
*****************************************************************
* 3274 MODEL 1C WITH MODEL 2 SCREEN (REMOTE SNA)                *
*      PRIMARY SCREEN   : MODEL 2                               *
*      SECONDARY SCREEN : NON                                   *
*****************************************************************
D4C32782 MODEENT LOGMODE=D4C32782,                                     X
               FMPROF=X'03',                                           X
               TSPROF=X'03',                                           X
               PRIPROT=X'B1',                                          X
               SECPROT=X'90',                                          X
               COMPROT=X'3080',                                        X
               RUSIZES=X'87F8',                                        X
               PSERVIC=X'020000000000185020507F00'
*****************************************************************
*      3276 SNA WITH MODEL 2 SCREEN (REMOTE SNA)                *
*      PRIMARY SCREEN   : MODEL 2                               *
*      SECONDARY SCREEN : NON                                   *
*****************************************************************
D6327802 MODEENT LOGMODE=D6327802,                                     X
               FMPROF=X'03',                                           X
               TSPROF=X'03',                                           X
               PRIPROT=X'B1',                                          X
               SECPROT=X'90',                                          X
               COMPROT=X'3080',                                        X
               RUSIZES=X'88F8',                                        X
               PSERVIC=X'020000000000185000007E00'
*****************************************************************
*      3274 1C SNA WITH MODEL 5 SCREEN (REMOTE SNA)             *
*      PRIMARY SCREEN   : MODEL 5                               *
*      SECONDARY SCREEN : NONE                                  *
*****************************************************************
D4C32785 MODEENT LOGMODE=D4C32785,                                     X
               FMPROF=X'03',                                           X
               TSPROF=X'03',                                           X
               PRIPROT=X'B1',                                          X
               SECPROT=X'90',                                          X
               COMPROT=X'3080',                                        X
               RUSIZES=X'87F8',                                        X
               PSERVIC=X'0200000000001B8400007E00'
*****************************************************************
*      3276 SNA WITH MODEL 2 SCREEN (REMOTE SNA) (T.S.O)        *
*      PRIMARY SCREEN   : MODEL 2                               *
*      SECONDARY SCREEN : NON                                   *
*****************************************************************
D63278TS MODEENT LOGMODE=D63278TS,                                     X
               FMPROF=X'03',                                           X
               TSPROF=X'03',                                           X
               PRIPROT=X'B1',                                          X
               SECPROT=X'90',                                          X
               COMPROT=X'3080',                                        X
               RUSIZES=X'8587',                                        X
               PSERVIC=X'020000000000000000000200'
*****************************************************************
*      3276 SNA WITH 3289 MODEL 2 PRINTER                       *
*****************************************************************
D6328902 MODEENT LOGMODE=D6328902,                                     X
               FMPROF=X'03',                                           X
               TSPROF=X'03',                                           X
               PRIPROT=X'B1',                                          X
               SECPROT=X'90',                                          X
               COMPROT=X'3080',                                        X
               RUSIZES=X'8787',                                        X
               PSERVIC=X'030000000000185018507F00'
*****************************************************************
*      3274 NON-SNA  MODEL 2 SCREEN (LOCAL)                     *
*      PRIMARY SCREEN   : MODEL 2                               *
*      SECONDARY SCREEN : NON                                   *
*****************************************************************
D4B32782 MODEENT LOGMODE=D4B32782,                                     X
               FMPROF=X'02',                                           X
               TSPROF=X'02',                                           X
               PRIPROT=X'71',                                          X
               SECPROT=X'40',                                          X
               COMPROT=X'2000',                                        X
               RUSIZES=X'0000',                                        X
               PSERVIC=X'000000000000185000007E00'
*****************************************************************
*     S C S   P R I N T E R                                     *
*****************************************************************
SCS      MODEENT LOGMODE=SCS,                                          X
               FMPROF=X'03',                                           X
               TSPROF=X'03',                                           X
               PRIPROT=X'B1',                                          X
               SECPROT=X'90',                                          X
               COMPROT=X'3080',                                        X
               RUSIZES=X'87C6',                                        X
               PSNDPAC=X'01',                                          X
               SRCVPAC=X'01',                                          X
               PSERVIC=X'01000000E100000000000000'
*****************************************************************
*        N C C F                                                *
*****************************************************************
DSILGMOD MODEENT LOGMODE=DSILGMOD,                                     X
               FMPROF=X'02',                                           X
               TSPROF=X'02',                                           X
               PRIPROT=X'71',                                          X
               SECPROT=X'40',                                          X
               COMPROT=X'2000',                                        X
               RUSIZES=X'0000',                                        X
               PSERVIC=X'000000000000000000000200'
*****************************************************************
*        N C C F                                                *
*****************************************************************
DSIXDMN  MODEENT LOGMODE=DSIXDMN,                                      X
               FMPROF=X'03',                                           X
               TSPROF=X'03',                                           X
               PRIPROT=X'20',                                          X
               SECPROT=X'20',                                          X
               COMPROT=X'4000',                                        X
               RUSIZES=X'0000',                                        X
               PSERVIC=X'000000000000000000000000'
*****************************************************************
*      3276 SNA WITH MODEL 2 SCREEN (MAGNETIC STRIPE READER)    *
*      PRIMARY SCREEN   : MODEL 2                               *
*      SECONDARY SCREEN : NON                                   *
*      TEST TEST TEST TEST TEST TEST                            *
*****************************************************************
SCSLRDR  MODEENT LOGMODE=SCSLRDR,                                      X
               FMPROF=X'03',                                           X
               TSPROF=X'03',                                           X
               PRIPROT=X'B1',                                          X
               SECPROT=X'90',                                          X
               COMPROT=X'3080',                                        X
               RUSIZES=X'87C6',                                        X
               PSNDPAC=X'01',                                          X
               SRCVPAC=X'01',                                          X
               PSERVIC=X'04000000E100000000000000'
         MODEEND
         END
./ ENDUP
//* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -UPDATE04
//*
//UPDATE05 EXEC PGM=IEBUPDTE,PARM=NEW
//*
//* ***************************************************************** *
//* SYS1.CMDPROC: ALLOCATE/CATALOG ON MVS000
//*               CREATE STDLOGON (TSO LOGON GREETING)                *
//*               CREATE USRLOGON (TSO LOGON PROCEDURE)               *
//* ***************************************************************** *
//*
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DSN=SYS1.CMDPROC,DISP=(,CATLG),
//             UNIT=3350,VOL=SER=MVS000,SPACE=(CYL,(20,,100)),
//             DCB=SYS1.MACLIB
//SYSIN    DD  DATA,DLM='><'
./ ADD NAME=STDLOGON
        PROC 0
CONTROL NOMSG,NOLIST,NOSYMLIST,NOCONLIST,NOFLUSH
CLS
WRITE ******************************************-
*************************************
WRITE *                                         -
                                    *
WRITE *                                         -
                                    *
WRITE *                    Welcome to the TSO sy-
stem                                *
WRITE *                    =====================-
====                                *
WRITE *                                         -
                                    *
WRITE *                                         -
                                    *
WRITE ******************************************-
*************************************
REVINIT
./ ADD NAME=TSOLOGON
        PROC 0
CONTROL NOMSG,NOLIST,NOSYMLIST,NOCONLIST,NOFLUSH
FREE FILE(SYSHELP)
WRITE Logging on to TSO at &SYSTIME using &SYSPROC
ALLOC FILE(SYSHELP) DSN('SYS1.HELP','SYS2.HELP') SHR
ALLOC FILE(X1) DSN('&SYSUID..CLIST(STDLOGON)') SHR
IF &LASTCC = 0 THEN +
   DO
      WRITE Logging on using private logon procedure
      FREE FILE(SYSPROC)
      FREE FILE(X1)
      ALLOC FILE(SYSPROC) DSN('&SYSUID..CLIST','SYS1.CMDPROC') SHR
   END
ELSE +
   DO
      WRITE Logging on using public logon procedure
      FREE FILE(X1)
   END
/* ENDIF */
%STDLOGON
EXIT
./ ENDUP
><
//* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -UPDATE05
//*
//UPDATE06 EXEC PGM=IEBUPDTE,PARM=NEW
//*
//* ***************************************************************** *
//* SYS2.PROCLIB: CREATE TSONUSER (USED TO ADD NEW TSO USERS)         *
//*               CREATE TSODUSER (USED TO DELETE TSO USERS)          *
//* ***************************************************************** *
//*
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DSN=SYS2.PROCLIB,DISP=MOD
//SYSIN    DD  DATA,DLM='><'
./ ADD NAME=TSONUSER,LIST=ALL
./ NUMBER NEW1=10,INCR=10
//*                                                                     00000100
//* PROCEDURE TO ADD NEW TSO USER ID TO SYS1.UADS AND CREATE STANDARD   00000200
//* SET OF USER FILES FOR THE NEW USER ID                               00000300
//*                                                                     00000400
//TSONUSER PROC ID=,              TSO USER ID                          +00000500
//             PW='*',            TSO USER PASSWORD                    +00000600
//             AN='*',            ACCOUNT NUMBER                       +00000700
//             PR='IKJACCNT',     LOGON PROCEDURE                      +00000800
//             MS='NOLIM',        REGION LIMIT: MAXSIZE(N)/NOLIM       +00000900
//             OP='NOOPER',       OPERATOR CMNDS: OPER/NOOPER          +00001000
//             AC='NOACCT',       ACCOUNT CMD: ACCT/NOACCT             +00001100
//             JC='JCL',          JOB CONTROL: JCL/NOJCL               +00001200
//             MT='NOMOUNT',      TAPE MOUNT: MOUNT/NOMOUNT            +00001300
//             SZ='SIZE(4096)',   DEFAULT REGION SIZE                  +00001400
//             UN='UNIT(SYSDA)'   DYNAMIC ALLOCATION UNIT               00001500
//*                                                                     00001600
//* IF ANY FILES WE WILL CREATE FOR NEW USER'S ID EXIST, DELETE THEM    00001700
//*                                                                     00001800
//DEL01    EXEC PGM=IEFBR14                                             00001900
//CLIST    DD  DSN=&ID..CLIST,DISP=(MOD,DELETE,DELETE),                 00002000
//             UNIT=SYSDA,VOL=SER=PUB000,                               00002100
//             SPACE=(CYL,0),DCB=SYS1.CMDPROC                           00002200
//CNTL     DD  DSN=&ID..CNTL,DISP=(MOD,DELETE,DELETE),                  00002300
//             UNIT=SYSDA,VOL=SER=PUB000,                               00002400
//             SPACE=(CYL,0),DCB=SYS1.MACLIB                            00002500
//SOURCE   DD  DSN=&ID..SOURCE,DISP=(MOD,DELETE,DELETE),                00002600
//             UNIT=SYSDA,VOL=SER=PUB000,                               00002700
//             SPACE=(CYL,0),DCB=SYS1.MACLIB                            00002800
//LOAD     DD  DSN=&ID..LOAD,DISP=(MOD,DELETE,DELETE),                  00002900
//             UNIT=SYSDA,VOL=SER=PUB000,                               00003000
//             SPACE=(CYL,0),DCB=SYS1.LINKLIB                           00003100
//*                                                                     00003200
//* ALLOCATE STANDARD FILES FOR NEW USER'S ID                           00003300
//*                                                                     00003400
//AL02     EXEC PGM=IEFBR14                                             00003500
//CLIST    DD  DSN=&ID..CLIST,DISP=(,KEEP,DELETE),                      00003600
//             UNIT=SYSDA,VOL=SER=PUB000,                               00003700
//             SPACE=(CYL,(1,1,20)),DCB=SYS1.CMDPROC                    00003800
//CNTL     DD  DSN=&ID..CNTL,DISP=(,KEEP,DELETE),                       00003900
//             UNIT=SYSDA,VOL=SER=PUB000,                               00004000
//             SPACE=(CYL,(1,1,20)),DCB=SYS1.MACLIB                     00004100
//SOURCE   DD  DSN=&ID..SOURCE,DISP=(,KEEP,DELETE),                     00004200
//             UNIT=SYSDA,VOL=SER=PUB000,                               00004300
//             SPACE=(CYL,(1,1,20)),DCB=SYS1.MACLIB                     00004400
//LOAD     DD  DSN=&ID..LOAD,DISP=(,KEEP,DELETE),                       00004500
//             UNIT=SYSDA,VOL=SER=PUB000,                               00004600
//             SPACE=(CYL,(1,1,20)),DCB=SYS1.LINKLIB                    00004700
//*                                                                     00004800
//* GENERATE COMMAND FILE TO CREATE NEW USER'S PROFILE                  00004900
//*                                                                     00005000
//PW03     EXEC PGM=PSU002,PARM=('\',                                   00005100
//             ' PROFILE NOPREFIX\',                                    00005200
//             ' ACCOUNT\',                                             00005300
//             '   SYNC')                                               00005400
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR                                00005500
//SYSOUT   DD  DSN=&&ACCOUNT,DISP=(,PASS),                              00005600
//             UNIT=SYSDA,SPACE=(TRK,(15)),                             00005700
//             DCB=(RECFM=FB,LRECL=80,BLKSIZE=3120)                     00005800
//PW04     EXEC PGM=PSU002,PARM=('\',                                   00005900
//             '   DELETE (&ID.)\',                                     00006000
//             '   ADD (&ID. &PW. &AN. &PR.) +\',                       00006100
//             '       &MS. +')                                         00006200
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR                                00006300
//SYSOUT   DD  DSN=&&ACCOUNT,DISP=(MOD,PASS)                            00006400
//PW05     EXEC PGM=PSU002,PARM=('\',                                   00006500
//             '       &OP. +\',                                        00006600
//             '       &AC. +\',                                        00006700
//             '       &JC. +\',                                        00006800
//             '       &MT. +')                                         00006900
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR                                00007000
//SYSOUT   DD  DSN=&&ACCOUNT,DISP=(MOD,PASS)                            00007100
//PW06     EXEC PGM=PSU002,PARM=('\',                                   00007200
//             '       &SZ. +\',                                        00007300
//             '       &UN.\',                                          00007400
//             '   LIST (&ID.)\',                                       00007500
//             '   SYNC\',                                              00007600
//             '   END')                                                00007700
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR                                00007800
//SYSOUT   DD  DSN=&&ACCOUNT,DISP=(MOD,PASS)                            00007900
//*                                                                     00008000
//* CREATE NEW USER'S PROFILE                                           00008100
//*                                                                     00008200
//TSO07    EXEC PGM=IKJEFT01,REGION=4096K                               00008300
//SYSUADS   DD DSN=SYS1.UADS,DISP=SHR                                   00008400
//SYSLBC    DD DSN=SYS1.BRODCAST,DISP=SHR                               00008500
//SYSTSPRT  DD SYSOUT=*                                                 00008600
//SYSTSIN   DD DSN=&&ACCOUNT,DISP=(OLD,DELETE)                          00008700
//*                                                                     00008800
//* GENERATE COMMAND FILE TO 1) DEFINE ALIAS FOR NEW USER'S ID          00008900
//* AND 2) DEFINE CATALOG ENTRIES FOR FILES FOR NEW USER                00009000
//*                                                                     00009100
//PW08     EXEC PGM=PSU002,PARM=('\',                                   00009200
//             ' DELETE &ID..* NOSCRATCH \',                            00009300
//             ' DELETE &ID. ALIAS \',                                  00009400
//             ' SET LASTCC = 0')                                       00009500
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR                                00009600
//SYSOUT   DD  DSN=&&IDCAMS,DISP=(,PASS),                               00009700
//             UNIT=SYSDA,SPACE=(TRK,(15)),                             00009800
//             DCB=(RECFM=FB,LRECL=80,BLKSIZE=3120)                     00009900
//PW09     EXEC PGM=PSU002,PARM=('\',                                   00010000
//             ' SET MAXCC = 0\',                                       00010100
//             ' DEFINE ALIAS (NAME(&ID.) RELATE(UCPUB000))')           00010200
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR                                00010300
//SYSOUT   DD  DSN=&&IDCAMS,DISP=(MOD,PASS)                             00010400
//PW10     EXEC PGM=PSU002,PARM=('\',                                   00010500
//             ' DEFINE NONVSAM (NAME(&ID..CLIST) DEVT(3380) ',         00010600
//             'VOL(PUB000))')                                          00010700
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR                                00010800
//SYSOUT   DD  DSN=&&IDCAMS,DISP=(MOD,PASS)                             00010900
//PW11     EXEC PGM=PSU002,PARM=('\',                                   00011000
//             ' DEFINE NONVSAM (NAME(&ID..CNTL) DEVT(3380) ',          00011100
//             'VOL(PUB000))')                                          00011200
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR                                00011300
//SYSOUT   DD  DSN=&&IDCAMS,DISP=(MOD,PASS)                             00011400
//*                                                                     00011500
//PW12     EXEC PGM=PSU002,PARM=('\',                                   00011600
//             ' DEFINE NONVSAM (NAME(&ID..SOURCE) DEVT(3380) ',        00011700
//             'VOL(PUB000))')                                          00011800
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR                                00011900
//SYSOUT   DD  DSN=&&IDCAMS,DISP=(MOD,PASS)                             00012000
//*                                                                     00012100
//PW13     EXEC PGM=PSU002,PARM=('\',                                   00012200
//             ' DEFINE NONVSAM (NAME(&ID..LOAD) DEVT(3380) ',          00012300
//             'VOL(PUB000))')                                          00012400
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR                                00012500
//SYSOUT   DD  DSN=&&IDCAMS,DISP=(MOD,PASS)                             00012600
//*                                                                     00012700
//PW14     EXEC PGM=PSU002,PARM=('\',                                   00012800
//             ' REPRO INFILE(SYSPROC) ',                               00012900
//             'OUTDATASET(&ID..CLIST(STDLOGON))')                      00013000
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR                                00013100
//SYSOUT   DD  DSN=&&IDCAMS,DISP=(MOD,PASS)                             00013200
//*                                                                     00013300
//* DEFINE ALIAS FOR NEW USER'S ID AND CATALOG ENTRIES FOR STANDARD     00013400
//* FILES FOR NEW USER                                                  00013500
//*                                                                     00013600
//IDC15    EXEC PGM=IDCAMS,REGION=1024K                                 00013700
//SYSPRINT DD  SYSOUT=*                                                 00013800
//SYSPROC  DD  DSN=SYS1.CMDPROC(STDLOGON),DISP=SHR                      00013900
//SYSIN    DD  DSN=&&IDCAMS,DISP=(OLD,DELETE)                           00014000
./ ADD NAME=TSODUSER,LIST=ALL
./ NUMBER NEW1=10,INCR=10
//*                                                                     00000100
//* PROCEDURE TO DELETE TSO USER ID TO SYS1.UADS AND (OPTIONALLY)       00000200
//* DELETE ALL FILES ASSOCIATED WITH THE USER ID                        00000300
//*                                                                     00000400
//TSODUSER PROC ID=,              TSO USER ID                          +00000500
//             KEEP='YES'         RETAIN USER'S FILES?                  00000600
//*                                                                     00000700
//* EVALUATE KEEPFILES PARAMETER                                        00000800
//*                                                                     00000900
//EVAL01   EXEC PGM=PSU001,PARM=('&KEEP,EQ,YES')                        00001000
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR                                00001100
//*                                                                     00001200
//* GENERATE COMMAND FILE TO DELETE TSO USER'S PROFILE                  00001300
//*                                                                     00001400
//PW02     EXEC PGM=PSU002,PARM=('\',                                   00001500
//             ' PROFILE NOPREFIX\',                                    00001600
//             ' ACCOUNT\',                                             00001700
//             '   SYNC\',                                              00001800
//             '   DELETE (&ID.)\',                                     00001900
//             '   SYNC\',                                              00002000
//             '   END')                                                00002100
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR                                00002200
//SYSOUT   DD  DSN=&&ACCOUNT,DISP=(,PASS),                              00002300
//             UNIT=SYSDA,SPACE=(TRK,(15)),                             00002400
//             DCB=(RECFM=FB,LRECL=80,BLKSIZE=3120)                     00002500
//*                                                                     00002600
//* DELETE TSO USER'S PROFILE                                           00002700
//*                                                                     00002800
//TSO03    EXEC PGM=IKJEFT01,REGION=4096K                               00002900
//SYSUADS   DD DSN=SYS1.UADS,DISP=SHR                                   00003000
//SYSLBC    DD DSN=SYS1.BRODCAST,DISP=SHR                               00003100
//SYSTSPRT  DD SYSOUT=*                                                 00003200
//SYSTSIN   DD DSN=&&ACCOUNT,DISP=(OLD,DELETE)                          00003300
//*                                                                     00003400
//* GENERATE COMMAND FILE TO DELETE TSO USER'S FILES AND ALIAS          00003500
//*                                                                     00003600
//PW04     EXEC PGM=PSU002,COND=(0,NE,EVAL01),PARM=('\',                00003700
//             ' DELETE &ID..* NONVSAM SCRATCH PURGE\',                 00003800
//             ' DELETE &ID. ALIAS \',                                  00003900
//             ' SET LASTCC = 0\',                                      00004000
//             ' SET MAXCC = 0')                                        00004100
//SYSOUT   DD  DSN=&&IDCAMS,DISP=(,PASS),                               00004200
//             UNIT=SYSDA,SPACE=(TRK,(15)),                             00004300
//             DCB=(RECFM=FB,LRECL=80,BLKSIZE=3120)                     00004400
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR                                00004500
//*                                                                     00004600
//* DELETE TSO USER'S FILES AND ALIAS                                   00004700
//*                                                                     00004800
//IDC05    EXEC PGM=IDCAMS,COND=(0,NE,EVAL01),REGION=1024K              00004900
//SYSPRINT DD  SYSOUT=*                                                 00005000
//SYSIN    DD  DSN=&&IDCAMS,DISP=(OLD,DELETE)                           00005100
./ ENDUP
><
//* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -UPDATE06
//*
//LOGTAB07 EXEC PROC=ASMLKED,M=LOGTAB01
//* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -LOGTAB07
//*
//LOGMODE8 EXEC PROC=ASMLKED,M=LOGMOD01
//* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -LOGTAB08
//*
//* ----------------------------------------------------------------- *
//* REPLACE NETWORK SOLICITOR SCREEN, Step 1: restore default source  *
//* ----------------------------------------------------------------- *
//UPDATE09 EXEC PGM=IEBUPDTE,PARM=NEW
//SYSPRINT DD  DUMMY
//SYSUT2   DD  DISP=SHR,DSN=SYS1.MACLIB
//SYSIN    DD  *
./ ADD NAME=NETSOL
//         DD  DSN=SYS1.AMACLIB(NETSOL),DISP=SHR
//         DD  *
./ ENDUP
//* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -UPDATE09
//*
//* ----------------------------------------------------------------- *
//* Step 2, update with changes to screen appearance                  *
//* ----------------------------------------------------------------- *
//UPDATE10 EXEC PGM=IEBUPDTE
//SYSPRINT DD  DUMMY
//SYSUT1   DD  DISP=SHR,DSN=SYS1.MACLIB
//SYSUT2   DD  DISP=SHR,DSN=SYS1.MACLIB
//SYSIN    DD  *
./ CHANGE NAME=NETSOL
         CLI   MSGINDEX,X'0C'                                           23164802
         BNE   EGSKIP                                                   23164804
         MVC   EGDATA(8),NAMEDEV                                        23164806
         MVC   EGDATA+9(8),APPLID                                       23164807
         LA    R3,EGMSGLN                                               23164808
         L     R4,=A(EGMSG)                                             23164810
*                                                                       23164812
         WRITE RPL=(PTRRPL),                                           X23164814
               OPTCD=(LBT,ERASE),                                      X23164816
               AREA=(R4),                                              X23164818
               RECLEN=(R3),                                            X23164820
               EXIT=WRITEND                                             23164822
*                                                                       23164824
         B     EGOK                                                     23164826
*                                                                       23164828
*                                                                       23164830
EGSKIP   DS    0H                                 EGSKIP                23164832
EGOK     DS    0H                                 EGOK                  23166010
EGMSG    DS    0C                                 EGMSG                 66810010
         DC    X'C3'                                                    66810020
*                                                                       66810030
 DC X'11',X'C5C3',X'1D',X'E8',X'2842F5'                                 66810040
 DC C'MM     MM VV     VV  SSSSSS           //    TTTTTTTT  SSSSSS   '  66810050
 DC C'OOOOOOO '                                                         66810060
*                                                                       66810070
 DC X'11',X'C6D3',X'1D',X'E8'                                           66810080
 DC C'MMM   MMM VV     VV SS    SS         //        TT    SS    SS O'  66810090
 DC C'O     OO'                                                         66810100
*                                                                       66810110
 DC X'11',X'C7E3',X'1D',X'E8'                                           66810120
 DC C'MMMM MMMM VV     VV SS              //         TT    SS       O'  66810130
 DC C'O     OO'                                                         66810140
*                                                                       66810150
 DC X'11',X'C8F3',X'1D',X'E8'                                           66810160
 DC C'MM MMM MM VV     VV  SSSSSS        //          TT     SSSSSS  O'  66810170
 DC C'O     OO'                                                         66810180
*                                                                       66810190
 DC X'11',X'4AC3',X'1D',X'E8'                                           66810200
 DC C'MM  M  MM  VV   VV     SSSS       //           TT       SSSS  O'  66810210
 DC C'O     OO'                                                         66810220
*                                                                       66810230
 DC X'11',X'4BD3',X'1D',X'E8'                                           66810240
 DC C'MM     MM  VV   VV        SS     //            TT          SS O'  66810250
 DC C'O     OO'                                                         66810260
*                                                                       66810270
 DC X'11',X'4CE3',X'1D',X'E8'                                           66810280
 DC C'MM     MM   VV VV   SS    SS    //             TT    SS    SS O'  66810290
 DC C'O     OO'                                                         66810300
*                                                                       66810310
 DC X'11',X'4DF3',X'1D',X'E8'                                           66810320
 DC C'MM     MM    VVV     SSSSSS    //              TT     SSSSSS   '  66810330
 DC C'OOOOOOO '                                                         66810340
*                                                                       66810350
 DC X'11',X'D27B',X'1DF0',X'284100',X'2842F4'                           66810360
 DC C'Welcome to MVS3.8j, running under the Hercules emulator'          66810370
 DC X'1DF0',X'284200'                                                   66810371
*                                                                       66810380
*                                                                       66810390
*                                                                       66810400
*                                                                       66810401
*                                                                       66810410
 DC X'11',X'5B60',X'1D',X'E8'                                           66810420
 DC C'TSO Logon ===>'                                                   66810430
 DC X'1D',X'C1'                                                         66810440
 DC X'13'                                                               66810450
EGDATA DC CL8' ',C' ',CL8' '                                            66810460
EGMSGLN  EQU   *-EGMSG                                                  66810470
//* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -UPDATE10
//*
//* ----------------------------------------------------------------- *
//* Step 3: assemble and link-edit new NETSOL                         *
//* ----------------------------------------------------------------- *
//*
//ASM11    EXEC PGM=IFOX00,REGION=1024K,
//             PARM='LINECOUNT(49),TERM'
//SYSLIB   DD  DISP=SHR,DSN=SYS1.MACLIB
//         DD  DISP=SHR,DSN=SYS1.AMODGEN
//SYSUT1   DD  UNIT=SYSDA,SPACE=(1700,(600,100))
//SYSUT2   DD  UNIT=SYSDA,SPACE=(1700,(300,50))
//SYSUT3   DD  UNIT=SYSDA,SPACE=(1700,(300,50))
//SYSTERM  DD  SYSOUT=*
//SYSPRINT DD  SYSOUT=*,DCB=BLKSIZE=1089
//SYSPUNCH DD  DSN=&&A,DISP=(NEW,PASS,DELETE),
//             UNIT=3350,SPACE=(TRK,(2,2)),
//             DCB=(BLKSIZE=80,LRECL=80,RECFM=F)
//SYSIN    DD  *
ISTNSC00 CSECT ,
         NETSOL SYSTEM=VS2
         END   ,
//* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ASM11
//*
//LKED12   EXEC PGM=IEWL,PARM='XREF,LIST,LET,NCAL',REGION=1024K
//SYSPRINT DD  SYSOUT=*
//SYSLIN   DD  DSN=&&A,DISP=(OLD,DELETE,DELETE)
//SYSLMOD  DD  DISP=SHR,DSN=SYS1.VTAMLIB(ISTNSC00)
//SYSUT1   DD  UNIT=SYSDA,SPACE=(1024,(200,20))
//* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -LKED12
//
