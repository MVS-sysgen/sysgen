//*
//* JOBCARD REPLACED BY sysgen.py
//*
//*MVPISPF JOB (SYSGEN),'MVP INSTALL',      
//*             CLASS=A,                     
//*             MSGCLASS=A,                  
//*             MSGLEVEL=(1,1),
//*             USER=IBMUSER,
//*             PASSWORD=SYS1,
//*             REGION=8192K
//* This JCL installs ISPF and REVIEW Front End
//MVPINST EXEC MVP,INSTALL='ISPF -D'   
//MVPINST EXEC MVP,INSTALL='ISPTHEME -D'   
//EDIT  EXEC PGM=IKJEFT01,REGION=1024K,DYNAMNBR=50
//SYSPRINT DD  SYSOUT=*
//SYSTSPRT DD  SYSOUT=*
//SYSTERM  DD  SYSOUT=*
//SYSTSIN  DD  *
EDIT 'SYS1.CMDPROC(STDLOGON)' TEXT
LIST
INSERT WRITE Use command ISPF to access ISPF
SAVE
END       