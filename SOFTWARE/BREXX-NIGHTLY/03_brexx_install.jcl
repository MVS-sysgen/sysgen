//BREXXINS JOB (TSO),
//             'Recieve XMI',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             USER=IBMUSER,PASSWORD=SYS1
//* Do not modify this JCL file, modify the .template file
//* ------------------------------------------------------------------
//* COPY BREXX MODULE(S) INTO SYS2.LINKLIB
//* ------------------------------------------------------------------
//STEP10  EXEC  PGM=IEBCOPY
//SYSPRINT  DD  SYSOUT=*
//DDIN      DD  DSN=BREXX.NIGHTLY.LINKLIB,DISP=SHR
//DDOUT     DD  DSN=SYS2.LINKLIB,DISP=SHR
//SYSIN     DD  *
  COPY INDD=((DDIN,R)),OUTDD=DDOUT
/*
//* ------------------------------------------------------------------
//* COPY BREXX MODULE(S) INTO SYS2.PROCLIB
//* ------------------------------------------------------------------
//STEP20  EXEC  PGM=IEBCOPY
//SYSPRINT  DD  SYSOUT=*
//DDIN      DD  DSN=BREXX.NIGHTLY.PROCLIB,DISP=SHR
//DDOUT     DD  DSN=SYS2.PROCLIB,DISP=SHR
//SYSIN     DD  *
  COPY INDD=((DDIN,R)),OUTDD=DDOUT
/*
//PDSUPDTE EXEC PGM=PDSUPDTE,PARM=UPDATE
//STEPLIB DD DSN=SYSC.LINKLIB,DISP=SHR
//SYSIN DD *
BREXX.V2R4M0<BREXX.NIGHTLY<
/*
//SYSTERM DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//@RXLIB   DD DSN=SYS2.CMDPROC