//RFEINST5 JOB (TSO),
//             'Install Review',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             USER=IBMUSER,PASSWORD=SYS1
//*
//*  THIS STEP COPIES THE REVIEW/RFE CLIST MEMBERS INTO SYS1.CMDPROC
//*
//STEP1CLS EXEC PGM=IEBCOPY
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  DSN=SYSGEN.REVIEW.CLIST,DISP=SHR
//SYSUT2   DD  DSN=SYS1.CMDPROC,DISP=SHR
//SYSIN    DD  *
  COPY INDD=((SYSUT1,R)),OUTDD=SYSUT2
  COPY INDD=((SYSUT2,R)),OUTDD=SYSUT2
//*
//*  THIS STEP COPIES THE REVIEW/RFE TSO HELP MEMBERS INTO SYS2.HELP
//*
//STEP2HLP EXEC PGM=IEBCOPY
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  DSN=SYSGEN.REVIEW.HELP,DISP=SHR
//SYSUT2   DD  DSN=SYS2.HELP,DISP=SHR
//SYSIN    DD  *
  COPY INDD=((SYSUT1,R)),OUTDD=SYSUT2
  COPY INDD=((SYSUT2,R)),OUTDD=SYSUT2
//*
//*  THIS JOB COPIES THE REVIEW/RFE TSO PROGRAMS INTO SYS2.CMDLIB
//*
//STEP3LD  EXEC PGM=IEBCOPY
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  DSN=SYSGEN.REVIEW.LOAD,DISP=SHR
//SYSUT2   DD  DSN=SYS2.CMDLIB,DISP=SHR
//SYSIN    DD  *
  COPY INDD=((SYSUT1,R)),OUTDD=SYSUT2
  COPY INDD=((SYSUT2,R)),OUTDD=SYSUT2
//*
//* From Jay Moseley SYSGEN
//* ***************************************************************** *
//* As installed, the REVINIT CLIST is lacking a VOLUME parameter     *
//* which, if present, will allocate the profile dataset on the same  *
//* DASD volume as the other user's datasets.  This step simply       *
//* modifies the REVINIT member of SYS1.CMDPROC to add a VOLUME       *
//* parameter when the profile dataset is initially created. This     *
//* change does not take affect until the TSO user logs on the first  *
//* time after this job is executed.                                  *
//* ***************************************************************** *
//*
//STEP4TSO EXEC PGM=IKJEFT01,REGION=1024K,DYNAMNBR=50
//SYSPRINT DD  SYSOUT=*
//SYSTSPRT DD  SYSOUT=*
//SYSTERM  DD  SYSOUT=*
//SYSTSIN  DD  DATA
EDIT 'SYS1.CMDPROC(REVINIT)' CNTL NONUM
LIST
TOP
F =ALLOC F(REVPROF) DA('&PROFDSN') NEW USING=
C * =TR  =TR +=
INSERT       VOLUME(PUB000)
LIST
END SAVE
/*
//