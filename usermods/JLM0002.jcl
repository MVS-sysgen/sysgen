//JLM0002  JOB (SYSGEN),'J03 M03: JLM0002',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             REGION=4096K
/*JOBPARM LINES=100
//JOBCAT   DD  DSN=SYS1.VSAM.MASTER.CATALOG,DISP=SHR
//*
//*********************************************************************
//* Install USERMOD JLM0002 - IEFU29 exit to automatically switch     *
//* SMF datasets when active one fills /and/                          *
//* Install Procedure, control cards, and define Generation Data      *
//* Group used to manage SMF datasets.                                *
//*********************************************************************
//* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//UPDATE01 EXEC PGM=IEBUPDTE,PARM=NEW
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DISP=SHR,DSN=SYS1.UMODSRC
//SYSIN    DD  *
./ ADD NAME=IEFU29
IEFU29   TITLE 'SMF SWITCH EXIT ROUTINE                        '
*---------------------------------------------------------------------*
*                                                                     *
*             MODULE NAME = IEFU29                                    *
*                                                                     *
*                SMF EXIT ROUTINE TO START DUMP OF SMF WHEN SMF       *
*                DATASET NEEDS TO BE DUMPED.                          *
*                                                                     *
*                                                                     *
*             FUNCTION =                                              *
*                ISSUES START COMMAND FOR SYS1.PROCLIB(SMFDUMP)       *
*                PROCEDURE.                                           *
*                                                                     *
*                OPERATION =                                          *
*                   ISSUES COMMAND 'START SMFDUMP,DSN=SMFDSN'         *
*                   WHERE SMFDSN WILL BE REPLACED BY THE NAME OF      *
*                   THE SMF DATASET TO BE DUMPED (SYS1.MANX OR        *
*                   SYS1.MANY) AND WILL ISSUE A WTO THAT THE          *
*                   START COMMAND HAS BEEN ISSUED.                    *
*                                                                     *
*                 REGISTER CONVENTIONS = STANDARD CONVENTIONS.        *
*                    REGISTERS 0 TO 1  = WORK REGISTERS               *
*                    REGISTERS 2 TO 11 = UNUSED                       *
*                    REGISTER  12      = ADDRESSABILITY TO IEFU29     *
*                                        CSECT                        *
*                    REGISTER  13      = ADDRESSIBILITY TO DATA DSECT *
*                    REGISTERS 14,15   = WORK REGISTERS               *
*                                                                     *
*             INPUT = REG1 POINTS TO FULLWORD ADDRESS OF DATA SET     *
*                     NAME (SYS1.MANX/SYS1.MANY) TO BE DUMPED.        *
*                                                                     *
*             OUTPUT = NONE                                           *
*                                                                     *
*             RETURN CODE = 0004 TO PREVENT SMF FROM ISSUING          *
*             MESSAGE IEE362A OR IEE362I                              *
*                                                                     *
*             MACROS = SAVE, WTO, RETURN, GETMAIN, FREEMAIN           *
*                                                                     *
*---------------------------------------------------------------------*
         EJECT
IEFU29   CSECT ,
*
@IDENT01 B     @IDENT04(R15)       BRANCH AROUND IDENT CONSTANTS
         DC    AL1(@IDENT03-@IDENT02)
@IDENT02 DC    C'IEFU29 '
         DC    C'&SYSDATE &SYSTIME - '
         DC    C'SMF DATASET SWITCH USER EXIT'
@IDENT03 DS    0H
@IDENT04 EQU   *-@IDENT01
*
         SAVE  (14,12)             SAVE REGISTERS
         USING IEFU29,R12          SET UP BASE ADDRESSABILITY
         USING DATA,R13            SET UP DATA AREA ADDRESSABILITY
         LR    R12,R15             LOAD BASE REG WITH ENTRY POINT
         L     R8,0(R1)            SAVE INPUT PARM(DSNAME)
         GETMAIN R,LV=LENDATA      GET STORAGE
         ST    R13,4(R1)           SAVE CALLER'S SAVE AREA ADDR
         ST    R1,8(R13)           SAVE MY SAVE AREA ADDRESS
         LR    R13,R1              LOAD SAVE AREA ADDRESS
*
*---------------------------------------------------------------------*
* CHECK FOR JES RUNNING ... IF NOT, EXIT WITH NO FURTHER ACTION       *
*---------------------------------------------------------------------*
         L     R5,16               ADDRESS CVT                    JLM
         L     R5,296(R5)          ADDRESS JEST                   JLM
         L     R5,24(R5)           ADDRESS SSCT                   JLM
         L     R5,16(R5)           ADDRESS SSCT+10                JLM
         LTR   R5,R5               IS JES AVAILABLE?              JLM
         BZ    EXITNOW               NO, EXIT WITHOUT DUMP        JLM
*---------------------------------------------------------08/2004 JLM-*
*
         MVC   ENQLIST(LENQLIST),ENQLSTX LOAD IN MODEL PARM LIST
         ENQ   MF=(E,ENQLIST)      TEST IF RESOURCE IN USE?
         LTR   R15,R15             WAS THE RESOURCE AVAILABLE?
         BNZ   EXITNOW             NO, EXIT W/O STARTING DUMP
*
         MVC   WTOAREA(WTOLEN),WTOL    MOVE IN WTO MESSAGE
         MVC   WTOAREA+DSNOFF(1),8(R8)  MOVE DSN ID TO MSG
         MVC   CMDAREA(CMDLEN),CMDL    MOVE IN START COMMAND
         MVC   CMDAREA+DSNCOFF(1),8(R8)  MOVE DSN ID TO START CMD
         SLR   R0,R0               CLEAR REG ZERO FOR SVC 34
         LA    R1,CMDAREA          POINT TO START COMMAND
         SVC   34                  ISSUE START COMMAND
         WTO   MF=(E,WTOAREA)      ISSUE MESSAGE TO OPERATOR
*
EXITNOW  LR    R1,R13              LOAD ADDRESS OF GETMAINED AREA
         L     R13,4(R13)          RESTORE CALLER'S SAVE AREA
         FREEMAIN R,LV=LENDATA,A=(R1)  FREE ACQUIRED STORAGE
         LM    14,12,12(13)        RESTORE CALLER'S REGISTERS
         LA    15,4                SET RETURN CODE TO 4
         BR    R14                 RETURN
*
         EJECT
*---------------------------------------------------------------------*
*                      CONSTANT DATA AND EQUATES                      *
*---------------------------------------------------------------------*
SMFQNAME DC    CL8'SMFQUEUE'
SMFRNAME DC    CL7'DATASET'
*
CMDL     DS    0F                  START COMMAND FORMAT FOR SVC 34
         DC    AL2(CMDLEN),AL2(00) LENGTH OF STRING
         DC    C'START SMFDUMP,ID=X'  COMMAND
*                456789012345678901
CMDLEN   EQU   *-CMDL              LENGTH OF COMMAND
DSNCOFF  EQU   21                  OFFSET OF DSN FIELD IN COMMAND
*
WTOL     WTO   'IEFU29 HAS ISSUED COMMAND ''START SMFDUMP,ID=X ''',    C
               ROUTCDE=(1,2,11),MF=L
*               456789012345678901234567890 123456789012345678
WTOLEN   EQU   *-WTOL              LENGTH OF WTO STRING
DSNOFF   EQU   48                  OFFSET OF DSN FIELD IN MESSAGE
ENQLSTX  ENQ   (SMFQNAME,SMFRNAME,E,,SYSTEM),RET=TEST,MF=L
*
R0       EQU   0
R1       EQU   1
R2       EQU   2
R3       EQU   3
R4       EQU   4
R5       EQU   5
R6       EQU   6
R7       EQU   7
R8       EQU   8
R9       EQU   9
R10      EQU   10
R11      EQU   11
R12      EQU   12
R13      EQU   13
R14      EQU   14
R15      EQU   15
         EJECT
*---------------------------------------------------------------------*
*                   DATA DSECT (ACQUIRED BY GETMAIN)                  *
*---------------------------------------------------------------------*
DATA     DSECT
SAVE     DS    18F                REGISTER SAVE AREA
CMDAREA  DS    0F,XL(CMDLEN)      AREA FOR COMMAND
WTOAREA  DS    0F,XL(WTOLEN)      AREA FOR WTO PARM LIST
ENQLIST  ENQ   (SMFQNAME,SMFRNAME,E,,SYSTEM),RET=TEST,MF=L
LENQLIST EQU   *-ENQLIST          LENGTH OF WTO STRING
RESERVED DS    4D                 RESERVED
LENDATA  EQU   *-DATA             EQUATE FOR LENGTH OF DATA DSECT
*
         END   IEFU29
/*
//*
//SMPASM02 EXEC SMPASM,M=IEFU29
//*
//RECV03   EXEC SMPAPP,WORK=SYSALLDA
//SMPPTFIN DD  *
++USERMOD (JLM0002)
  .
++VER (Z038)
  FMID(EBB1102)
  .
++MOD(IEFU29)
  TXLIB(UMODOBJ)
  .
//SMPCNTL  DD  *
 RECEIVE
         SELECT(JLM0002)
         .
  APPLY
        SELECT(JLM0002)
        DIS(WRITE)
        .
//*
//UPDATE04 EXEC PGM=IEBUPDTE,REGION=1024K,PARM=NEW,COND=(0,NE)
//*
//* ***************************************************************** *
//* Create SMF Dump/Clear Procedure in SYS1.PROCLIB                   *
//* Note: Model DSCB and GDG definition will be set up in job MVS01   *
//* ***************************************************************** *
//*
//SYSUT2   DD  DSN=SYS1.PROCLIB,DISP=MOD
//SYSPRINT DD  SYSOUT=*
//SYSIN    DD  DATA,DLM='><'
./ ADD LIST=ALL,NAME=SMFDUMP
./ NUMBER NEW1=10,INCR=10
//*-------------------------------------------------------------------*
//*                 SMF DATASET DUMP/CLEAR PROCEDURE                  *
//*-------------------------------------------------------------------*
//SMFDUMP   PROC CLASS=X,ID=
//DUMP      EXEC PGM=IFASMFDP,REGION=4096K
//SYSPRINT  DD  SYSOUT=&CLASS
//DUMPIN    DD  DSN=SYS1.MAN&ID,DISP=SHR
//DUMPOUT   DD  DSN=SYSO.SMF.DATA(+1),DISP=(NEW,CATLG),
//             UNIT=SYSDA,VOL=SER=MVS000,SPACE=(CYL,(5,1),RLSE)
//SYSIN     DD  DSN=SYS2.CONTROL(SMFDUMP),DISP=SHR
//*  INDD(DUMPIN,OPTIONS(DUMP))
//*-------------------------------------------------------------------*
//CLEAR     EXEC PGM=IFASMFDP,REGION=4096K,COND=(0,NE,DUMP)
//SYSPRINT  DD  SYSOUT=&CLASS
//DUMPIN    DD  DSN=SYS1.MAN&ID,DISP=SHR
//DUMPOUT   DD  DUMMY
//SYSIN     DD  DSN=SYS2.CONTROL(SMFCLEAR),DISP=SHR
//*  INDD(DUMPIN,OPTIONS(CLEAR))
//*-------------------------------------------------------------------*
./ ENDUP
><
//*
//UPDATE05 EXEC PGM=IEBUPDTE,REGION=1024K,PARM=NEW,COND=(0,NE)
//*
//* ***************************************************************** *
//* Create SMF Dump and Clear control cards in SYS2.CONTROL           *
//* ***************************************************************** *
//*
//SYSUT2   DD  DSN=SYS2.CONTROL,DISP=SHR
//SYSPRINT DD  SYSOUT=*
//SYSIN    DD  *
./ ADD    LIST=ALL,NAME=SMFDUMP
./ NUMBER NEW1=10,INCR=10
     INDD(DUMPIN,OPTIONS(DUMP))
./ ADD    LIST=ALL,NAME=SMFCLEAR
./ NUMBER NEW1=10,INCR=10
     INDD(DUMPIN,OPTIONS(CLEAR))
./ ENDUP
//*
//
