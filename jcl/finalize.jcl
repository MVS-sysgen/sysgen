//*
//* JOBCARD REPLACED BY sysgen.py
//*
//*FINALIZE JOB (TSO),
//*             'Finalize MVSCE',
//*             CLASS=A,
//*             MSGCLASS=A,
//*             MSGLEVEL=(1,1),
//*             USER=IBMUSER,
//*             PASSWORD=SYS1
/*JOBPARM   LINES=100
//EDIT  EXEC PGM=IKJEFT01,REGION=1024K,DYNAMNBR=50
//SYSPRINT DD  SYSOUT=*
//SYSTSPRT DD  SYSOUT=*
//SYSTERM  DD  SYSOUT=*
//SYSTSIN  DD  *
EDIT 'SYS1.PARMLIB(COMMND00)' TEXT
FIND JES2
C * /JES2/JES2,,,PARM='WARM,NOREQ'/
LIST
INSERT COM='START NET'
SAVE
END
//* --------------------- Add shutdown JCL and scripts
//*
//*
//* SYS2.EXEC: CREATE SHUTDOWN (USED TO SHUTDOWN MVSCE)       
//*
//SHTDWNRX EXEC PGM=IEBUPDTE,PARM=NEW
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DSN=SYS2.EXEC,DISP=MOD
//SYSIN    DD  DATA,DLM='><'
./ ADD NAME=SHUTFAST,LIST=ALL
/* REXX SCRIPT TO INITIATE MVS/CE SHUTDOWN */                       

CALL ANNOUNCE "WARNING! MVS/CE WILL BE SHUTTING DOWN IN 10 SECONDS"   
CALL WAIT(10000)   

CALL WTO("SHUT002 - BEGINING SHUTDOWN SEQUENCE")                    

ADDRESS COMMAND "CP SCRIPT SCRIPTS/SHUTDOWN.RC"                     

EXIT                                                                

ANNOUNCE:                                                           
  PARSE ARG WHAT_TO_SAY                                             
  CALL WTO("SHUT001 - "|| WHAT_TO_SAY)                              
  CALL CONSOLE("SEND 'SHUT001 - "||WHAT_TO_SAY||"'")                
  RETURN     
./ ADD NAME=SHUTDOWN,LIST=ALL
/* REXX SCRIPT TO INITIATE MVS/CE SHUTDOWN */                       

DO I = 5 TO 2 BY -1                                                 
 CALL ANNOUNCE "WARNING! MVS/CE WILL BE SHUTTING DOWN IN" I "MINUTES"
 CALL WAIT(60000)                                                    
END                                                                 

CALL ANNOUNCE "WARNING! MVS/CE WILL BE SHUTTING DOWN IN 1 MINUTE"   
CALL WAIT(60000)   

CALL WTO("SHUT002 - BEGINING SHUTDOWN SEQUENCE")                    

ADDRESS COMMAND "CP SCRIPT SCRIPTS/SHUTDOWN.RC"                     

EXIT                                                                

ANNOUNCE:                                                           
  PARSE ARG WHAT_TO_SAY                                             
  CALL WTO("SHUT001 - "|| WHAT_TO_SAY)                              
  CALL CONSOLE("SEND 'SHUT001 - "||WHAT_TO_SAY||"'")                
  RETURN                                                            
><
//*
//* SYS2.JCLLIB: CREATE SHUTDOWN (USED TO SHUTDOWN MVSCE)       
//*
//SHTDWNJC EXEC PGM=IEBUPDTE,PARM=NEW
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DSN=SYS2.JCLLIB,DISP=MOD
//SYSIN    DD  DATA,DLM='><'
./ ADD NAME=SHUTDOWN,LIST=ALL
//SHUTDOWN JOB (JOB),'SHUTDOWN',CLASS=A,MSGCLASS=A
//SHUTDOWN EXEC SHUTDOWN
./ ADD NAME=SHUTFAST,LIST=ALL
//SHUTFAST JOB (JOB),'SHUTFAST',CLASS=A,MSGCLASS=A
//SHUTFAST EXEC SHUTDOWN,TYPE='SHUTFAST'
><
//*
//* SYS1.CMDPROC: CREATE SHUTDOWN (USED TO SHUTDOWN MVSCE)       
//*
//SHTDWNJC EXEC PGM=IEBUPDTE,PARM=NEW
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DSN=SYS1.CMDPROC,DISP=MOD
//SYSIN    DD  DATA,DLM='><'
./ ADD NAME=SHUTDOWN,LIST=ALL
PROC 0                           
CONTROL NOFLUSH                  
  SUBMIT 'SYS2.JCLLIB(SHUTDOWN)' 
./ ADD NAME=SHUTFAST,LIST=ALL
PROC 0                           
CONTROL NOFLUSH                  
  SUBMIT 'SYS2.JCLLIB(SHUTFAST)'
><
//* --------------------- Add shutdown JCL and scripts