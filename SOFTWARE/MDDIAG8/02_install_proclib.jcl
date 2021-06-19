//INSTPROC  JOB (TSO),
//             'Install SYS2.PROCLIB',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             USER=IBMUSER,
//             PASSWORD=SYS1
//STEP1 EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  GENERATE  MAXFLDS=3,MAXLITS=11
  RECORD  FIELD=(79,2,,1),FIELD=(1,' ',,80)
//*
//SYSUT1   DD *,DLM=@@
 //INSTALL  PROC SOFT='EXAMPLE'
 //********************************************************************       
 //INSTALL  EXEC PGM=MDDIAG8,
 //  PARM='script ../SOFTWARE/&SOFT/INSTALL.RC'       
@@
//SYSUT2   DD  DSN=SYS2.PROCLIB(INSTALL),DISP=SHR