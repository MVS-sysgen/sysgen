//HERC01X  JOB  CLASS=A,MSGCLASS=X
//*
//* EXAMPLE JOB TO TRANSMIT A SIMPLE 'FLAT' FILE
//*
//* 1. SPECIFY THE NODEID AND USERID OF THE TARGET USER.
//*
//* 2. SPECIFY THE SEQUENTIAL DATASET (OR MEMBER) OF THE DATA TO
//*    BE SENT.
//*
//*
//*
//XMIT     EXEC PGM=NJ38XMIT,PARM='MVSB.HERC01'       <= TARGET USER
//STEPLIB  DD DSN=NJE38.AUTHLIB,DISP=SHR
//SYSPRINT DD SYSOUT=*
//*
//SYSUT1  DD DISP=SHR,DSN=HERC01.COBOL.SOURCE(COPYBK1)
//