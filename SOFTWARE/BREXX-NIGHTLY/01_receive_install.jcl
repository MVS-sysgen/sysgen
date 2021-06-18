//BREXXRCV JOB (TSO),
//             'Recieve XMI',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             USER=IBMUSER,PASSWORD=SYS1
//* Do not modify this JCL file, modify the .template file
//* First step is to make an alias for BREXX
//CLEANUP EXEC PGM=IDCAMS
//SYSIN    DD *
  DELETE BREXX.NIGHTLY.INSTALL
  DEFINE ALIAS(NAME(BREXX) RELATE(UCPUB001))
  SET MAXCC=0
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
//SYSPRINT DD  SYSOUT=*
//SYSIN    DD  DUMMY
//* Work temp dataset
//SYSUT1   DD  DSN=&&SYSUT1,
//             UNIT=3390,VOL=SER=PUB001,
//             SPACE=(CYL,(10,10)),
//             DISP=(NEW,DELETE,DELETE)
//* Output dataset
//SYSUT2   DD  DSN=SYSGEN.BREXX.NIGHTLY.INSTALL,
//             UNIT=3390,VOL=SER=PUB001,
//             SPACE=(CYL,(10,10,20),RLSE),
//             DISP=(NEW,CATLG,DELETE)
