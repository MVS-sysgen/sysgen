//IND$FILE  JOB (TSO),
//             'UNLOAD',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             USER=IBMUSER,PASSWORD=SYS1
//* Installs IND$FILE to SYS2.CMDLIB
//IND$FILE EXEC PGM=IEBCOPY
//SYSPRINT DD  SYSOUT=*
//SYSUT3   DD  UNIT=SYSDA,SPACE=(TRK,(1,1))
//SYSUT4   DD  UNIT=SYSDA,SPACE=(TRK,(1,1))
//TAPE     DD  DSN=IND$FILE.LOAD,
//             UNIT=TAPE,
//             LABEL=(1,SL,,,EXPDT=98000),
//             VOL=(,RETAIN,,,SER=INDFIL),
//             DISP=(OLD,PASS)
//DISK     DD  DSN=SYS2.CMDLIB,DISP=SHR
//SYSIN    DD  *
 COPY INDD=((TAPE,R)),OUTDD=DISK
/*

