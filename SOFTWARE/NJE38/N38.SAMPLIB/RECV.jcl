//HERC01X  JOB  CLASS=A,MSGCLASS=X
//*
//* EXAMPLE RETRIEVAL JOB FOR A PLAIN "FLAT" FILE
//*
//* SPECIFY THE DCB ATTRIBUTES BASED ON WHAT WAS TRANSMITTED
//*
//RETR     EXEC PGM=NJ38RECV,PARM=''
//*
//* Optional PARM parameters (specify one or both separated by comma):
//*   NOPURGE    - Leave the file in the NJE38 spool after retrieval.
//*   FILE=##    - Specifies the exact spool file id of the file to
//*                receive.
//*
//STEPLIB  DD DSN=NJE38.AUTHLIB,DISP=SHR
//SYSPRINT DD SYSOUT=*
//*
//SYSUT2  DD DSN=HERC01.LIST,
//           DISP=(NEW,CATLG),UNIT=SYSDA,
//           SPACE=(CYL,(2,1)),
//           DCB=(BLKSIZE=3200,
//           LRECL=80,
//           RECFM=FB)
//
