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
//* SYS2.STCEXEC: CREATE SHUTDOWN AND SHUTFAST (USED TO SHUTDOWN MVSCE)
//*
//* Execs run only by procs (EXECLIB= of SYS2.PROCLIB(SHUTDOWN)). Not
//* SYS2.EXEC: that is SYSEXEC at logon, and with ZMG0001 a member there
//* wins over the SYS1.CMDPROC members of the same names below, so TSO
//* SHUTDOWN ran the countdown in the user's session.
//*
//STCEXEC  EXEC PGM=IEFBR14
//STCEXEC  DD  DSN=SYS2.STCEXEC,DISP=(NEW,CATLG),
//             VOL=SER=MVS000,
//             UNIT=3350,SPACE=(TRK,(2,2,2)),
//             DCB=(RECFM=FB,LRECL=80,BLKSIZE=19040)
//*
//SHTDWNRX EXEC PGM=IEBUPDTE,PARM=NEW,COND=(0,NE)
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DSN=SYS2.STCEXEC,DISP=MOD
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
//* SYS2.PROCLIB: CREATE SHUTDOWN (RUNS THE EXECS ABOVE)
//*
//SHTDWNPR EXEC PGM=IEBGENER
//SYSPRINT DD  SYSOUT=*
//SYSIN    DD  DUMMY
//SYSUT2   DD  DSN=SYS2.PROCLIB(SHUTDOWN),DISP=SHR
//SYSUT1   DD  DATA,DLM='><'
//*
//* ------------------------------------------------------------------*
//* SHUTDOWN JCL Procedure
//* Usage:
//*    S SHUTDOWN
//*    S SHUTDOWN,TYPE='SHUTFAST'
//*    //SHUTDOWN EXEC SHUTDOWN
//*    //SHUTDOWN EXEC SHUTDOWN,TYPE='SHUTFAST'
//* ------------------------------------------------------------------*
//*
//SHUTDOWN PROC TYPE='SHUTDOWN',
//         LIB='BREXX.CURRENT.RXLIB',
//         EXECLIB='SYS2.STCEXEC'
//EXEC     EXEC PGM=IKJEFT01,
//       PARM='BREXX &TYPE',
//       REGION=8192K
//TSOLIB   DD   DSN=&LIB,DISP=SHR
//RXLIB    DD   DSN=&LIB,DISP=SHR
//SYSEXEC  DD   DSN=&EXECLIB,DISP=SHR
//SYSPRINT DD   SYSOUT=*
//SYSTSPRT DD   SYSOUT=*
//SYSTSIN  DD   DUMMY
//STDOUT   DD   SYSOUT=*,DCB=(RECFM=FB,LRECL=140,BLKSIZE=5600)
//STDERR   DD   SYSOUT=*,DCB=(RECFM=FB,LRECL=140,BLKSIZE=5600)
//STDIN    DD   DUMMY
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
//* Needs USER=/PASSWORD= of a RAKF ADMIN on the job card (the exec
//* uses CONSOLE and CP). From TSO use the command SHUTDOWN instead.
//SHUTDOWN EXEC SHUTDOWN
./ ADD NAME=SHUTFAST,LIST=ALL
//SHUTFAST JOB (JOB),'SHUTFAST',CLASS=A,MSGCLASS=A
//* Needs USER=/PASSWORD= of a RAKF ADMIN on the job card (the exec
//* uses CONSOLE and CP). From TSO use the command SHUTFAST instead.
//SHUTFAST EXEC SHUTDOWN,TYPE='SHUTFAST'
><
//*
//* SYS1.CMDPROC: CREATE SHUTDOWN AND SHUTFAST (TSO COMMANDS)
//*
//* REXX, not CLISTs: a job SUBMITted from TSO has no RAKF user, so
//* BREXX/370 3.1 gives it no CONSOLE and no CP command (FACILITY SVC244
//* and DIAG8CMD). These start the SHUTDOWN proc as a started task
//* instead, which runs under STCGROUP. Needs SVC244 READ (ADMIN).
//*
//SHTDWNCL EXEC PGM=IEBUPDTE,PARM=NEW
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DSN=SYS1.CMDPROC,DISP=MOD
//SYSIN    DD  DATA,DLM='><'
./ ADD NAME=SHUTDOWN,LIST=ALL
/* REXX: TSO SHUTDOWN - START SYS2.PROCLIB(SHUTDOWN) (5 MINUTES) */
IF RACCHECK('FACILITY','SVC244','READ') = 0 THEN DO
  SAY 'SHUTDOWN: NOT AUTHORIZED (RAKF FACILITY SVC244)'
  EXIT 8
END
CALL CONSOLE 'S SHUTDOWN'
SAY 'SHUTDOWN: STARTED, MVS/CE SHUTS DOWN IN 5 MINUTES'
./ ADD NAME=SHUTFAST,LIST=ALL
/* REXX: TSO SHUTFAST - START SYS2.PROCLIB(SHUTDOWN) (10 SECONDS) */
IF RACCHECK('FACILITY','SVC244','READ') = 0 THEN DO
  SAY 'SHUTFAST: NOT AUTHORIZED (RAKF FACILITY SVC244)'
  EXIT 8
END
CALL CONSOLE "S SHUTDOWN,TYPE='SHUTFAST'"
SAY 'SHUTFAST: STARTED, MVS/CE SHUTS DOWN IN 10 SECONDS'
><
//* --------------------- Add shutdown JCL and scripts