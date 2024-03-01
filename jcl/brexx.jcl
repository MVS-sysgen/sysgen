//BRXXINST JOB (TSO),
//             'Install BREXX',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             REGION=8192K
//* ,USER=IBMUSER,PASSWORD=SYS1
//* First step is to make an alias for BREXX
//CLEANUP EXEC PGM=IDCAMS
//SYSIN    DD *
  DEFINE ALIAS(NAME(BREXX) RELATE(UCPUB001))
  SET MAXCC=00
//SYSPRINT DD  SYSOUT=*
//* RECV370 DDNAMEs:
//* ----------------
//*
//*     RECVLOG    RECV370 output messages (required)
//*
//*     RECVDBUG   Optional, specifies debugging options.
//*
//*     XMITIN     input XMIT file to be received (required)
//*
//*     SYSPRINT   IEBCOPY output messages (required for DSORG=PO
//*                input datasets on SYSUT1)
//*
//*     SYSUT1     Work dataset for IEBCOPY (not needed for sequential
//*                XMITs; required for partitioned XMITs)
//*
//*     SYSUT2     Output dataset - sequential or partitioned
//*
//*     SYSIN      IEBCOPY input dataset (required for DSORG=PO XMITs)
//*                A DUMMY dataset.
//*
//RECV370 EXEC PGM=RECV370
//STEPLIB  DD  DISP=SHR,DSN=SYSC.LINKLIB
//XMITIN   DD  UNIT=01C,DCB=(RECFM=FB,LRECL=80,BLKSIZE=80)
//RECVLOG  DD  SYSOUT=*
//RECVDBUG  DD *
DEFAULT  1
U370DBUG 1     TURN ON DEBUG MSGS RIGHT AWAY
RECV370  9
RECVBLK  0
RECVCTL  5
RECVGCTL 3
RECVGSEG 0
RECVMSG  9
RECVRCPY 2
RECVUTIL 9
U370ABND 0
U370AP00 1
U370AP99 1
U370CHAR 0
U370HERC 9
U370HEX  0
U370LOG  0
U370PSR  0
U370QENV 2
U370QTCB 2     3=DUMP TCB, 8=ASCB, 9=ASXB
U370QSS  2     3=DUMP SSCVT 4=DUMP SSVT
U370SDWA 4
U370SNAP 0
U370STAE 9 4 6 8
U370TIOT 0
U370VADR 0
U370WTO  0
/*
//SYSPRINT DD  SYSOUT=*
//SYSIN    DD  DUMMY
//* Work temp dataset
//SYSUT1   DD  DSN=&&SYSUT1,
//             UNIT=3390,VOL=SER=PUB001,
//             SPACE=(CYL,(10,10)),
//             DISP=(NEW,DELETE,DELETE)
//* Output dataset
//SYSUT2   DD  DSN=BREXX.V2R5M3.INSTALL,
//             UNIT=3390,VOL=SER=PUB001,
//             SPACE=(CYL,(10,10,20),RLSE),
//             DISP=(NEW,CATLG,DELETE)
//* -------------------------------------------------------------------
//XMITLOAD PROC XMITLIB='BREXX.V2R5M3.INSTALL',
//         HLQ='BREXX.V2R5M3',     <-- DO NOT CHANGE HLQ ----
//         MEMBER=
//* RECEIVE XMIT FILE AND CREATE TARGET FILE
//XMILOADP EXEC PGM=RECV370,REGION=4096K
//STEPLIB  DD  DISP=SHR,DSN=SYSC.LINKLIB
//RECVLOG  DD SYSOUT=*
//XMITIN   DD DSN=&XMITLIB(&MEMBER),DISP=SHR
//SYSPRINT DD SYSOUT=*
//SYSUT1   DD DSN=&&SYSUT1,
//         UNIT=3390,VOL=SER=PUB001,
//         SPACE=(TRK,(250,250)),
//         DISP=(NEW,DELETE,DELETE)
//SYSUT2   DD DSN=&HLQ..&MEMBER,
//         UNIT=3390,VOL=SER=PUB001,
//         SPACE=(TRK,(250,250,25),RLSE),
//         DISP=(NEW,CATLG,DELETE)
//SYSIN    DD DUMMY
// PEND
//* ------------------------------------------------------------------
//* UNPACK XMIT FILES INTO INSTALL LIBRARIES
//*        SAVE XMIT IN BREXX.$RELEASE.XMIT(AS-MEMBER)
//* ------------------------------------------------------------------
//UNPK10   EXEC XMITLOAD,MEMBER=LINKLIB
//UNPK20   EXEC XMITLOAD,MEMBER=PROCLIB
//UNPK30   EXEC XMITLOAD,MEMBER=JCL
//UNPK40   EXEC XMITLOAD,MEMBER=SAMPLES
//UNPK50   EXEC XMITLOAD,MEMBER=RXLIB
//UNPK50   EXEC XMITLOAD,MEMBER=CMDLIB
//UNPK60   EXEC XMITLOAD,MEMBER=APFLLIB
//* ------------------------------------------------------------------
//* COPY BREXX MODULE(S) INTO SYS2.LINKLIB
//* ------------------------------------------------------------------
//STEP10  EXEC  PGM=IEBCOPY
//SYSPRINT  DD  SYSOUT=*
//DDIN      DD  DSN=BREXX.V2R5M3.APFLLIB,DISP=SHR
//DDOUT     DD  DSN=SYS2.LINKLIB,DISP=SHR
//SYSIN     DD  *
  COPY INDD=((DDIN,R)),OUTDD=DDOUT
/*
//* ------------------------------------------------------------------
//* COPY BREXX MODULE(S) INTO SYS2.PROCLIB
//* ------------------------------------------------------------------
//STEP20  EXEC  PGM=IEBCOPY
//SYSPRINT  DD  SYSOUT=*
//DDIN      DD  DSN=BREXX.V2R5M3.PROCLIB,DISP=SHR
//DDOUT     DD  DSN=SYS2.PROCLIB,DISP=SHR
//SYSIN     DD  *
  COPY INDD=((DDIN,R)),OUTDD=DDOUT
/*
//* ------------------------------------------------------------------
//STEP30   EXEC PGM=IKJEFT01,DYNAMNBR=64,REGION=4096K
//SYSTSPRT DD SYSOUT=*
//SYSTSIN  DD *
 /* -----------------------------------------------  */
 /* REMOVE CLIST MEMBERS, THEY ARE NO LONGER NEEDED  */
 /*   IF ALLOC/DELETE FAIL THE CLISTS HAVE ALREADY   */
 /*   BEEN REMOVED EARLIER. THIS IS NOT AN ERROR!    */
 /* -----------------------------------------------  */
 /* -----------------------------------------------  */
 /* COMPRESS UPDATE LIBRARIES                        */
 /* -----------------------------------------------  */
  COMPRESS 'SYS2.LINKLIB'
  COMPRESS 'SYS2.PROCLIB'
 /* -----------------------------------------------  */
 /* CHECK EXISTENCE OF NEW BREXX LOAD MEMBER AND     */
 /* SET APPROPRIATE RC                               */
 /* -----------------------------------------------  */
  ALLOC DSN('SYS2.LINKLIB(BREXX)') SHR  FILE(BREXX)
/*
//*
//* Add SYS2.EXEC
//*
//SYS2EXEC EXEC PGM=IEFBR14
//SYSEXEC  DD  DSN=SYS2.EXEC,DISP=(NEW,CATLG),
//             VOL=SER=MVS000,
//             UNIT=3350,SPACE=(TRK,(44,14,17)),
//             DCB=(RECFM=FB,LRECL=80,BLKSIZE=19040)
//* ------------------------------------------------------------------
//* EDIT LOGON PROC
//* ------------------------------------------------------------------
//ALIASRXL EXEC PGM=IDCAMS
//SYSIN    DD *
  DEFINE ALIAS -
          (NAME(BREXX.CURRENT.RXLIB) -
          RELATE(BREXX.V2R5M3.RXLIB))-
          CATALOG(UCPUB001)
//SYSPRINT DD  SYSOUT=*
