//*
//*        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//*        !!                                     !!
//*        !! DO NOT RENUMBER THIS JOBSTREAM FILE !!
//*        !!                                     !!
//*        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//*
//*
//AY12275  JOB (SYSGEN),'J01 M01: AY12275',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             REGION=4096K
/*JOBPARM LINES=100
//JOBCAT   DD  DSN=SYS1.VSAM.MASTER.CATALOG,DISP=SHR
//*
//* 2002/02/25 @kl
//*
//*        OY12275 - TCTRSZ INCORRECTLY REFLECTS SIZE OF PRIVATE AREA,
//*                  RATHER THAN REGION SIZE REQUESTED.
//*
//RECEIVE  EXEC SMPREC,WORK='SYSALLDA'
//SMPPTFIN   DD DATA,DLM='??'
++USERMOD(AY12275).
++VER(Z038) FMID(EBB1102) PRE(UZ35042)
 /*
  PROBLEM DESCRIPTION(S):
           OY12275 - TCTRSZ INCORRECTLY REFLECTS SIZE OF PRIVATE AREA,
                     RATHER THAN REGION SIZE REQUESTED.
  COMPONENT: 5752-SC1CH-EBB1102
  APARS FIXED: OY12275
  SPECIAL CONDITIONS:
  COMMENTS:          NONE
 */.
++ZAP(IEAVPRT0) DISTLIB(AOSC5).
NAME IEAVPRT0
VER 032E 58D0,7014     L  RWORK3,PQESIZE    GET REGION SIZE FROM PQE
REP 032E 58D0,45BC     L  RWORK3,VVREGSZ    GET REGION SIZE FROM LDA
IDRDATA AY12275
??
//SMPCNTL  DD  *
  RECEIVE
          SELECT(AY12275)
          .
/*
//*
//APPLYCK  EXEC SMPAPP,WORK='SYSALLDA'
//NUCLEUS  DD  DISP=SHR,DSN=SYS1.NUCLEUS
//SMPCNTL  DD  *
  APPLY
        SELECT(AY12275)
        BYPASS(ID)
        CHECK
        .
/*
//*
//APPLY    EXEC SMPAPP,COND=(0,NE),WORK='SYSALLDA'
//NUCLEUS  DD  DISP=SHR,DSN=SYS1.NUCLEUS
//SMPCNTL  DD  *
  APPLY
        SELECT(AY12275)
        DIS(WRITE)
        .
/*
//
//JLM0001  JOB (SYSGEN),'J02 M02: JLM0001',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             REGION=4096K
/*JOBPARM LINES=100
//JOBCAT   DD  DSN=SYS1.VSAM.MASTER.CATALOG,DISP=SHR
//*
//*********************************************************************
//* Install USERMOD JLM0001 - IEFACTRT exit to provide job/step       *
//* accounting information (source: Brian Westerman/                  *
//*********************************************************************
//* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//UPDATE01 EXEC PGM=IEBUPDTE,PARM=NEW
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DISP=SHR,DSN=SYS1.UMODSRC
//SYSIN    DD  *
./ ADD NAME=IEFACTRT
*********************************************************************** 
* LOCAL MACROS DEFINED BELOW:                                         * 
*    FILL ----- INITIALIZE A DATA AREA WITH SINGLE CHARACTER. IF NO   * 
*               CHARACTER SUPPLIED WITH CALL, DEFAULT IS SPACE.       * 
*    STEPWTO -- WRITE A MESSAGE ON CONSOLE. IF NO FIELD SUPPLIED,     * 
*               TEXT IS USED FROM AREA CONTAINING STEP COMPLETION     * 
*               FIELDS. (MESSAGE LENGTH LIMITED TO SIZE DEFINED FOR   * 
*               STEP COMPLETION FIELDS)                               * 
*    WRSYSOUT - CALL IEFYS TO WRITE LINE BUFFER TO SYSTEM LOG.        * 
*********************************************************************** 
*                                                                       
         MACRO                                                          
&LABEL   FILL  &AREA,&CHAR                                              
         LCLC  &CHAR$                                                   
&CHAR$   SETC  'C'' '''                DEFAULT FILL CHARACTER           
         AIF   ('&CHAR' EQ '').NOFILL  USE DEFAULT FILL CHAR            
&CHAR$   SETC  '&CHAR'                 USER FILL CHARACTER              
.NOFILL  ANOP                                                           
&LABEL   MVI   &AREA,&CHAR$            SET FILL BYTE                    
         MVC   &AREA+1(L'&AREA-1),&AREA  FILL FIELD                     
         MEND                                                           
*                                                                       
         MACRO                                                          
&NAME    STEPWTO &MSG                                                   
         MVC   WTOBUF,WTODEF           MOVE LIST MACRO TO DYN. STORAGE  
         AIF   (T'&MSG NE 'O').MSG                                      
         MVC   WTOBUF+4(WTOMSGL),ACTRMSG COPY STANDARD MESSAGE TEXT     
         AGO   .WTO                                                     
.MSG     ANOP                                                           
         MVC   WTOBUF+4(WTOMSGL),&MSG  INSERT PASSED MESSAGE TEXT       
.WTO     ANOP                                                           
         LA    R1,WTOBUF               POINT TO LIST MACRO              
         WTO   MF=(E,(1))                AND ISSUE WTO SVC              
         MEXIT                                                          
         MEND                                                           
*                                                                       
         MACRO                                                          
&NAME    WRSYSOUT ,                    CALL IEFYS TO WRITE SYSOUT       
         MVC   36(4,R12),SYSOUT@       MOVE ADDR TO IEFYS COMM AREA     
         MVC   42(2,R12),SYSOUT#       MOVE LENGTH TO IEFSY COMM AREA   
         L     R15,=V(IEFYS)           LOAD IEFYS ENTRY ADDRESS         
         BALR  R14,R15                 CALL IEFYS                       
         MEND                                                           
*                                                                       
*********************************************************************** 
* END OF LOCAL MACRO DEFINITIONS                                      * 
*********************************************************************** 
*                                                                       
         WXTRN IEFYS                   ENTRY POINT OF SYSTEM LOG WRITE  
*                                                                       
         TITLE 'JOB/STEP PROCESSING EXIT'                               
IEFACTRT CSECT                                                          
*                                                                       
*********************************************************************** 
*                                                                     * 
*                                                                     * 
*    IIII   EEEEEE  FFFFFFF    A     CCCCC TTTTTTTT RRRRRR TTTTTTTT   * 
*     II    EE      FF        AAA   CC   CC   TT    RR   RR   TT      * 
*     II    EE      FF       AA AA  CC   CC   TT    RR   RR   TT      * 
*     II    EEEE    FFFFF   AA   AA CC        TT    RR   RR   TT      * 
*     II    EE      FF      AA   AA CC        TT    RRRRRR    TT      * 
*     II    EE      FF      AAAAAAA CC   CC   TT    RR RR     TT      * 
*     II    EE      FF      AA   AA CC   CC   TT    RR  RR    TT      * 
*    IIII   EEEEEEE FF      AA   AA  CCCCC    TT    RR   RR   TT      * 
*                                                                     * 
*                                                                     * 
*********************************************************************** 
*                                                                     * 
* FUNCTION                                                            * 
*    THIS MODULE IS ENTERED AT JOB/STEP TERMINATION. IF THE           * 
*    MODULE WAS ENTERED FOR STEP TERMINATION, INFORMATION IS          * 
*    EXTRACTED AND WRITTEN TO THE SYSOUT DATASET USING THE IEFYS      * 
*    ROUTINE, AND A MESSAGE IS WRITTEN TO THE CONSOLE INDICATING      * 
*    THE STEP COMPLETION INFORMATION.                                 * 
*                                                                     * 
* ENTRY POINT - IEFACTRT                                              * 
*                                                                     * 
* INPUT                                                               * 
*    REGISTER 0 INDICATES REASON FOR ENTRY:                           * 
*      12 - STEP TERMINATION                                          * 
*      16 - JOB TERMINATION                                           * 
*                                                                     * 
*    REGISTER 1 POINTS TO A LIST OF 4 BYTE ADDRESSES FOR THE          * 
*    FOLLOWING TEN PARAMETERS:                                        * 
*      1 - POINTER TO COMMON EXIT PARAMETER BLOCK (FIELDS MOVED TO    * 
*          LOCAL WORK AREA).  FIELDS ARE: JOB NAME, DATE/TIME, SMF    * 
*          SYSTEM ID, USERID, STEP NUMBER, SMF OPTION, RESTART        * 
*          INDICATOR, JOB CLASS, USER COMMUNICATION AREA (4 BYTES).   * 
*      2 - STEP NAME (ADDRESS IS ZERO FOR JOB TERM ENTRY)             * 
*      3 - PROGRAMMERS NAME                                           * 
*      4 - JOB EXECUTION TIME                                         * 
*      5 - JOB ACCOUNTING FIELDS                                      * 
*      6 - STEP EXECUTION TIME                                        * 
*      7 - STEP ACCOUNTING FIELDS                                     * 
*      8 - FLAGS AND STEP NUMBER                                      * 
*      9 - TERMINATION STATUS                                         * 
*     10 - SMF TERMINATION RECORD                                     * 
*                                                                     * 
* OUTPUT - NONE                                                       * 
*                                                                     * 
* EXIT - NORMAL = AT PROGRAM END VIA BRANCH REGISTER 14               * 
*                                                                     * 
* EXTERNAL REFERENCES                                                 * 
*    IEFYS - WRITES LINE TO SYSTEM LOG                                * 
*                                                                     * 
* DSECTS                                                              * 
*    IEFJMR - JOB MANAGEMENT                                          * 
*    IFASMFR - SMF RECORD FORMAT (RECORD TYPE 4 USED)                 * 
*    IHASDWA - SDWA FOR ESTAE/SETRP MACRO                             * 
*    WORKAREA - INTERNAL FIELDS STORED IN MEMORY EXTERNAL TO PROGRAM  * 
*                                                                     * 
* ATTRIBUTES - KEY 0, REENTRANT                                       * 
*                                                                     * 
* CHARACTER CODE DEPENDENCY - NONE                                    * 
*                                                                     * 
* EXTERNAL MACROS USED: GETMAIN, FREEMAIN                             * 
*                                                                     * 
* REGISTER USAGE:                                                     * 
*    R8 - USED FOR SUBROUTINE LINKAGE                                 * 
*    R10 - SMF RECORD                                                 * 
*    R11 - BASE REGISTER                                              * 
*    R12 - RESERVED FOR IEFYS COMMUNICATION                           * 
*    R13 - WORK AREA OBTAINED BY GETMAIN, INCLUDES MY SAVE AREA       * 
*                                                                     * 
* WRITTEN BY JAY MOSELEY IN JULY, 2020                                * 
*                                                                     * 
* MODIFICATIONS                                                       * 
*                                                                     * 
*   2002/08/29 JLM: ERROR IN COMPUTING LENGTH OF WTO MESSAGE AREA-    * 
*                   AS ORIGINALLY CODED, MVC OF STEP END MESSAGE TO   * 
*                   WTO MESSAGE AREA INCLUDED MCS FLAGS, WHICH COULD  * 
*                   RANDOMLY AFFECT MESSAGE HIGHLIGHTING.             * 
*                                                                     * 
*********************************************************************** 
         EJECT                                                          
SUBPOOL  EQU   241                     SUBPOOL FOR WORK AREA            
R0       EQU   0                       REGISTER 0                       
R1       EQU   1                       REGISTER 1                       
R2       EQU   2                       REGISTER 2                       
R3       EQU   3                       REGISTER 3                       
R4       EQU   4                       REGISTER 4                       
R5       EQU   5                       REGISTER 5                       
R6       EQU   6                       REGISTER 6                       
R7       EQU   7                       REGISTER 7                       
R8       EQU   8                       REGISTER 8                       
R9       EQU   9                       REGISTER 9                       
R10      EQU   10                      REGISTER 10                      
R11      EQU   11                      REGISTER 11                      
R12      EQU   12                      REGISTER 12                      
R13      EQU   13                      REGISTER 13                      
R14      EQU   14                      REGISTER 14                      
R15      EQU   15                      REGISTER 15                      
         USING IEFACTRT,R15            TEMPORARY ADDRESSABILITY         
         B     BEGIN                   BRANCH AROUND IDENT FIELDS       
IDENTITY DS    0H                      BEGIN IDENTIFICATION FIELDS      
         DC    AL1(BEGIN-IDENTITY)     LENGTH OF IDENTIFICATION FIELDS  
         DC    CL8'IEFACTRT'           CSECT NAME                       
         DC    C'&SYSDATE. @ &SYSTIME' ASSEMBLY DATE AND TIME           
BEGIN    DS    0H                      END OF IDENTIFICATION FIELDS     
         STM   R14,R12,12(R13)         ENTRY LINKAGE                    
         L     R7,0(,R1)               CEP LIST                         
         USING JMR,R7                  SET UP ADDRESSABILITY            
         TM    JMRINDC,JMRFIND         IS THIS TSO USER?                
         BO    QUIKEXIT                  YES, EXIT NOW                  
         DROP  R7                      DROP CEPLIST ADDRESSABILITY      
         DROP  R15                     DROP TEMPORARY BASE              
         BALR  R11,R0                  REGISTER 11 AS BASE REGISTER     
         USING *,R11                   SET UP ADDRESSABILITY            
         L     R0,WALNGTH              LOAD GETMAIN INFORMATION         
         GETMAIN R,LV=(0)              ACQUIRE MEMORY FOR WORK AREA     
         LR    R15,R13                 SAVE OLD SAVE AREA POINTER       
         LR    R13,R1                  R13 POINTS TO NEW SAVE AREA      
         USING WORKAREA,R13            SET UP ADDRESSABILITY TO WORK    
         LM    R0,R1,20(R15)           RESTORE ORIGINAL R0/R1           
         ST    R15,4(R13)              OLD SAVE AREA ADDRESS INTO NEW   
         ST    R13,8(R15)              NEW SAVE AREA ADDRES INTO OLD    
         LR    R3,R0                   ORIGINAL R0 TO R3                
*                                                                       
* SAVE ADDRESSES PASSED IN R1 LIST                                      
*                                                                       
         MVC   PLCEPADR(40),0(R1)      COPY 10 ADDRESSES TO WORK AREA   
*                                                                       
* SAVE COMMON EXIT PARAMETER LIST FIELDS                                
*                                                                       
         L     R1,PLCEPADR             GET ADDRESS OF CEP LIST          
         MVC   CEPJOBN(36),0(R1)       COPY 11 FIELDS TO WORK AREA      
*                                                                       
* SET UP RECOVERY ENVIRONMENT                                           
*                                                                       
         MVC   ESTAEW(LESTAEL),ESTAEL  MOVE IN ESTAE PARAMETER LIST     
         LA    R0,RTRYRTN1             RETRY ROUTINE - NO SDWA          
         ST    R0,ESTAPARM             STORE IN PARAMETER LIST          
         LA    R0,RTRYRTN2             RETRY ROUTINE - WITH SDWA        
         ST    R0,ESTAPARM+4           STORE IN PARAMETER LIST          
         STM   R11,R13,ESTAPARM+8      BASE/IEFYS WORK/DATA REGS        
         ESTAE RECOVERY,CT,PARAM=ESTAPARM,MF=(E,ESTAEW) SETUP RCVRY     
*                                                                       
         CH    R3,=H'16'               IS THIS JOB TERMINATION?         
         BE    GOBACK                    YES, RETURN TO SYSTEM          
         CH    R3,=H'12'               IS THIS STEP TERMINATION?        
         BNE   GOBACK                    NO, RETURN TO SYSTEM           
*                                                                       
         LA    R2,SYSOUTLN             GET ADDRESS OF SYSOUT BUFFER     
         ST    R2,SYSOUT@              STORE ADDRESS FOR MOVES LATER    
         LA    R2,132                  LENGTH OF SYSOUT BUFFER          
         STH   R2,SYSOUT#              STORE LENGTH FOR MOVES LATER     
*                                                                       
         L     R10,PLSMFADR            GET ADDRESS OF SMF RECORD        
         USING SMF4,R10                SET UP ADDRESSABILITY            
*                                                                       
* INITIALIZE CONSOLE MESSAGE LINE                                       
*                                                                       
         MVC   ACTRHDR,KWTOID          CONSTANT HEADER                  
         MVI   ACTRSEP1,KSLASH         CONSTANT FIELD SEPARATOR         
         MVI   ACTRSEP2,KSLASH         CONSTANT FIELD SEPARATOR         
         MVI   ACTRSEP3,KSLASH         CONSTANT FIELD SEPARATOR         
         MVI   ACTRSEP4,KSLASH         CONSTANT FIELD SEPARATOR         
         MVI   ACTRSEP5,KSLASH         CONSTANT FIELD SEPARATOR         
         MVC   ACTRJOBN(8),CEPJOBN     JOB NAME TO CONSOLE MESSAGE      
*                                                                       
* SYSOUT LINE #1 (INCLUDES JOB INFORMATION ON FIRST STEP)               
*                                                                       
         FILL  SYSOUTLN,KSTAR          INITIALIZE SYSOUT LINE 1         
*                                                                       
         CLI   CEPSNUM,X'01'           IS THIS FIRST STEP?              
         BNE   NOJOBHD                   NO, SKIP JOB HEADING           
*                                                                       
* JOB NAME                                                              
*                                                                       
         MVC   SYSOUTLN+4(11),KSJOBNAM CONSTANT FIELD LABEL             
         MVC   SYSOUTLN+15(8),CEPJOBN  JOB NAME                         
*                                                                       
* JOB CARD READ DATE/TIME (FROM SMF4 RECORD)                            
*                                                                       
         MVC   SYSOUTLN+23(14),KSJOBCRD CONSTANT FIELD LABEL            
         MVC   DATEPACK(4),SMF4RSD     JOB CARD READ DATE               
         BAL   R8,DATEFORM             CONFORM DATE TO Y2K AND EDIT     
         MVC   SYSOUTLN+37(4),DATECHAR YEAR TO PRINT AREA               
         MVI   SYSOUTLN+41,KSLASH        / TO PRINT AREA                
         MVC   SYSOUTLN+42(3),DATECHAR+4   DAY TO PRINT AREA            
         MVI   SYSOUTLN+45,KSPACE      SPACE AFTER DATE                 
         ICM   R1,B'1111',SMF4RST      JOB CARD READ TIME               
         LA    R2,KCLOCKM              MASK TO PRINT CLOCK TIME         
         BAL   R8,TIMEFORM             CONVERT TIME TO PRINTABLE        
         MVC   SYSOUTLN+46(8),TIMECHAR START TIME TO PRINT AREA         
*                                                                       
* CPU MODEL 370/???                                                     
*                                                                       
         MVC   SYSOUTLN+54(5),KSCPUMAJ COMPUTER MODEL MAJOR TO PRINT    
         L     R5,16                   ADDRESS OF CVT                   
         LA    R5,0(R5)                CLEAR HIGH ORDER BYTE            
         S     R5,=F'8'                GET TO CVT PREFIX                
         XC    DECW,DECW               CLEAR WORK FIELD                 
         MVC   DECW+2(2),2(R5)         MOVE CPU MODEL NUMBER (EG. 0145) 
         MVI   DECW+4,X'0C'            ADD SIGN (EG. 01450C)            
         DP    DECW(5),=P'10'          DIVIDE BY 10 (EG. 00145C)        
         MVC   CPUMODEL,=X'40202120'   EDIT MASK FOR CPU MODEL          
         ED    CPUMODEL,DECW+1         EDIT CPU MODEL                   
         MVC   SYSOUTLN+59(3),CPUMODEL+1 COMPUTER MODEL MINOR TO PRINT  
*                                                                       
* MVS RELEASE LEVEL (R03.8)                                             
*                                                                       
         MVC   SYSOUTLN+62(6),KSVS2    RELEASE NUMBER FIELD LABEL       
         MVC   SYSOUTLN+68(2),4(R5)    MOVE RELEASE NUMBER TO PRINT     
         MVI   SYSOUTLN+70,KPERIOD       DECIMAL POINT                  
         MVC   SYSOUTLN+71(2),6(R5)        SUB RELEASE NUMBER TO PRINT  
*                                                                       
* SYSTEM IDENTIFIER (FROM SMF PARM)                                     
*                                                                       
         MVC   SYSOUTLN+73(4),CEPSYSID SYSTEM IDENTIFIER TO PRINT       
         MVI   SYSOUTLN+77,KSPACE      SPACE AFTER SYSTEM ID            
*                                                                       
NOJOBHD  DS 0H                                                          
         WRSYSOUT ,                    CALL IEFYS TO WRITE SYSOUT LINE  
*                                                                       
* SYSOUT LINE #2 (JUST A SEPARATOR LINE)                                
*                                                                       
         FILL  SYSOUTLN,KSPACE         INITIALIZE SYSOUT LINE 2         
         MVI   SYSOUTLN,KSTAR            ..                             
         MVI   SYSOUTLN+131,KSTAR          ..                           
*                                                                       
         WRSYSOUT ,                    CALL IEFYS TO WRITE SYSOUT LINE  
*                                                                       
* SYSOUT LINE #3                                                        
*                                                                       
         FILL  SYSOUTLN,KSPACE         INITIALIZE SYSOUT LINE 3         
         MVI   SYSOUTLN,KSTAR            ..                             
         MVI   SYSOUTLN+131,KSTAR          ..                           
*                                                                       
* STEP NUMBER (FROM SMF4 RECORD)                                        
*                                                                       
         MVC   SYSOUTLN+3(12),KSTEPNUM CONSTANT FIELD LABEL             
         XR    R2,R2                   CLEAR R2                         
         IC    R2,SMF4STN              INSERT STEP # FROM SMF4 RECORD   
         CVD   R2,DECW                 CONVERT TO DECIMAL               
         MVC   SYSOUTLN+22(4),=X'40202120' EDIT MASK FOR STEP NUMBER    
         ED    SYSOUTLN+22(4),DECW+6   EDIT STEP NUMBER                 
         MVC   ACTRSTPN,SMF4STMN       STEP NAME TO CONSOLE MSG         
         CLI   SMF4STMN,KSPACE         WAS A STEP NAME SUPPLIED?        
         BNE   STPNDONE                  YES, GO TO NEXT FIELD          
         MVC   ACTRSTPN,KACTRSTP       CONSTANT (JS#999)                
         MVC   ACTRSNNN(3),SYSOUTLN+23 STEP NUMBER TO CONSOLE MSG       
*                                                                       
STPNDONE DS    0H                                                       
*                                                                       
* USER CORE (FROM SMF4 RECORD)                                          
*                                                                       
         MVC   SYSOUTLN+28(10),KUSERCOR CONSTANT FIELD LABEL            
         XR    R2,R2                   CLEAR R2                         
         LH    R2,SMF4H0ST             GET PROBLEM PROGRAM CORE USED    
         CVD   R2,DECW                 CONVERT TO DECIMAL               
         MVC   SYSOUTLN+43(6),=X'402020202120' EDIT MASK FOR USER CORE  
         ED    SYSOUTLN+43(6),DECW+5   EDIT USER CORE                   
         MVI   SYSOUTLN+49,KK          CONSTANT 'K'                     
*                                                                       
* STEP START TIME HH:MM:SS.TT (FROM SMF4 RECORD)                        
*                                                                       
         MVC   SYSOUTLN+52(11),KSTRTTIM CONSTANT FIELD LABEL            
         ICM   R1,B'1111',SMF4SIT      STEP INITIATION TIME             
         LA    R2,KCLOCKM              MASK TO PRINT CLOCK TIME         
         BAL   R8,TIMEFORM             CONVERT TIME TO PRINTABLE        
         MVC   SYSOUTLN+66(11),TIMECHAR START TIME TO PRINT AREA        
*                                                                       
*        COPY FIELDS FROM SMF4 RELOCATE SECTION FOR USE LATER           
*                                                                       
         XR    R5,R5                   CLEAR R5                         
         ICM   R5,B'0011',SMF4RLCT     GET OFFSET TO RELOCATE SECTION   
         LA    R5,SMFRCD4+4(R5)        GET ADDR OF RELOCATE SECTION     
         USING SMF4PGIN,R5             SET UP ADDRESSABILITY            
         MVC   REL4PGIN(4),SMF4PGIN    NUMBER OF PAGE-INS               
         MVC   REL4PGOT(4),SMF4PGOT    NUMBER OF PAGE-OUTS              
         MVC   REL4NSW(4),SMF4NSW      NUMBER OF SWAPS                  
         MVC   REL4PSI(4),SMF4PSI      PAGES SWAPPED IN                 
         MVC   REL4PSO(4),SMF4PSO      PAGES SWAPPED OUT                
         MVC   REL4VPI(4),SMF4VPI      VAM PAGE INS                     
         MVC   REL4VPO(4),SMF4VPO      VAM PAGE OUTS                    
         MVC   REL4SST(4),SMF4SST      STEP SERVICE TIME                
         MVC   REL4ACT(4),SMF4ACT      STEP ACTIVE TIME                 
         MVC   REL4PGNO(4),SMF4PGNO    PERFORMANCE GROUP NUMBER         
         DROP  R5                      NO LONGER NEED ADDRESSABILITY    
*                                                                       
*        COPY FIELDS FROM SMF4 ACCOUNTING SECTION FOR USE LATER         
*                                                                       
         LA    R5,SMF4LENN             GET ADDRESS OF LENGTH OF THE    C
                                         EXCP SECTION                   
         AH    R5,SMF4LENN             GET ADDRESS OF CPU/ACCT SECTION  
         XC    REL4SETM,REL4SETM       CLEAR FULLWORD                   
         MVC   REL4SETM+1(3),1(R5)     STEP CPU TIME UNDER TCB (.01SEC) 
*                                                                       
* CPU TIME HH:MM:SS.TT (FROM PASSED PARAMETER LIST AND SMF4 RECORD)     
*                                                                       
         MVC   SYSOUTLN+79(9),KCPUTIM  CONSTANT FIELD LABEL             
         L     R2,PLSETADR             GET ADDR OF STEP CPU TIME (TCB)  
         XR    R1,R1                   CLEAR R1                         
         ICM   R1,B'0111',0(R2)        GET TCB TIME                     
         XR    R2,R2                   CLEAR R2                         
         ICM   R2,B'0111',SMF4SRBT     GET STEP SRB TIME                
         AR    R1,R2                   CPU+SRB = TCB TIME               
         LA    R2,KPERIODM             MASK TO PRINT ELAPSED TIME       
         BAL   R8,TIMEFORM             CONVERT TIME TO PRINTABLE        
         MVC   SYSOUTLN+92(11),TIMECHAR STEP CPU TIME TO PRINT AREA     
         MVC   ACTRCPUT(11),TIMECHAR   STEP CPU TIME TO CONSOLE MSG     
*                                                                       
* ACTIVE TIME  (FROM SMF4 RECORD)                                       
*                                                                       
         MVC   SYSOUTLN+105(12),KACTVTIM CONSTANT FIELD LABEL           
         ICM   R7,B'1111',REL4ACT      GET STEP ACTIVE TIME            C
                                         (UNIT IS 1024 MICROSECONDS)    
         XR    R6,R6                   CLEAR R6 FOR MULTIPLY            
         SLDL  R6,10                   MULTIPLY BY 1024 TO GET         C
                                         TO GET MICROSECONDS            
         AL    R7,=A(5000)             ROUND TO NEAREST HUNDREDTH      C
                                         OF A SECOND                    
         BC    12,BCSACTIM             BRANCH IF NO CARRY               
         LA    R6,1(R6)                INCREMENT R6 ON OVERFLOW FROM R7 
*                                                                       
BCSACTIM D     R6,=A(10000)            REDUCE TO HUNDREDTHS OF SECONDS  
         LR    R1,R7                   COPY R7 TO R1                    
         LA    R2,KPERIODM             MASK TO PRINT ELAPSED TIME       
         BAL   R8,TIMEFORM             CONVERT TIME TO PRINTABLE        
         MVC   SYSOUTLN+119(11),TIMECHAR STEP ACTIVE TIME TO PRINT AREA 
*                                                                       
         WRSYSOUT ,                    CALL IEFYS TO WRITE SYSOUT LINE  
*                                                                       
* SYSOUT LINE #4                                                        
*                                                                       
         FILL  SYSOUTLN,KSPACE         INITIALIZE SYSOUT LINE 4         
         MVI   SYSOUTLN,KSTAR            ..                             
         MVI   SYSOUTLN+131,KSTAR          ..                           
*                                                                       
* STEP NAME (FROM SMF4 RECORD)                                          
*                                                                       
         MVC   SYSOUTLN+3(10),KSTEPNAM CONSTANT FIELD LABEL             
         MVC   SYSOUTLN+18(8),SMF4STMN STEP NAME FROM SMF4 RECORD       
*                                                                       
* SYSTEM CORE USED ON USER'S BEHALF (FROM SMF4 RECORD)                  
*                                                                       
         MVC   SYSOUTLN+28(12),KSYSCOR CONSTANT FIELD LABEL             
         LH    R1,SMF4SYST             SYS CORE USED ON USER'S BEHALF   
         CVD   R1,DECW                 CONVERT TO DECIMAL               
         MVC   SYSOUTLN+43(6),=X'402020202120' EDIT MASK FOR SYS CORE   
         ED    SYSOUTLN+43(6),DECW+5   EDIT SYSTEM CORE                 
         MVI   SYSOUTLN+49,KK          CONSTANT 'K'                     
*                                                                       
* STEP STOP TIME  (FROM SMF4 RECORD)                                    
*                                                                       
         MVC   SYSOUTLN+52(10),KSTOPTIM CONSTANT FIELD LABEL            
         ICM   R1,B'1111',SMF4TME      STEP TERMINATION TIME            
         LA    R2,KCLOCKM              MASK TO PRINT CLOCK TIME         
         BAL   R8,TIMEFORM             CONVERT TIME TO PRINTABLE        
         MVC   SYSOUTLN+66(11),TIMECHAR STOP TIME TO PRINT AREA         
*                                                                       
* SRB TIME HH:MM:SS.TT (FROM SMF4 RECORD)                               
*                                                                       
         MVC   SYSOUTLN+79(9),KSRBTIM  CONSTANT FIELD LABEL             
         XR    R1,R1                   CLEAR R2                         
         ICM   R1,B'0111',SMF4SRBT     GET STEP SRB TIME                
         LA    R2,KPERIODM             MASK TO PRINT ELAPSED TIME       
         BAL   R8,TIMEFORM             CONVERT TIME TO PRINTABLE        
         MVC   SYSOUTLN+92(11),TIMECHAR STEP SRB TIME TO PRINT AREA     
*                                                                       
* ALLOCATION TIME (ALLOCATION START TIME, FROM SMF4 RECORD)             
*                                                                       
         MVC   SYSOUTLN+105(11),KALLOCTI CONSTANT FIELD LABEL           
         XR    R1,R1                   CLEAR R1 FOR TIME                
         ICM   R1,B'1111',SMF4AST      GET SMF4 ALLOCATION START TIME   
         LA    R2,KCLOCKM              MASK TO PRINT CLOCK TIME         
         BAL   R8,TIMEFORM             CONVERT TIME TO PRINTABLE        
         MVC   SYSOUTLN+119(11),TIMECHAR START TIME TO PRINT AREA       
*                                                                       
         WRSYSOUT ,                    CALL IEFYS TO WRITE SYSOUT LINE  
*                                                                       
* SYSOUT LINE #5                                                        
*                                                                       
         FILL  SYSOUTLN,KSPACE         INITIALIZE SYSOUT LINE 5         
         MVI   SYSOUTLN,KSTAR            ..                             
         MVI   SYSOUTLN+131,KSTAR          ..                           
*                                                                       
* PROGRAM NAME (FROM SMF4 RECORD)                                       
*                                                                       
         MVC   SYSOUTLN+3(13),KPGMNAME CONSTANT FIELD LABEL             
         MVC   SYSOUTLN+18(8),SMF4PGMN PROGRAM NAME FROM SMF4 RECORD    
         MVC   ACTRPGMN(8),SMF4PGMN    PROGRAM NAME TO CONSOLE MSG      
*                                                                       
* REGION SIZE (FROM SMF4 RECORD)                                        
*                                                                       
         MVC   SYSOUTLN+28(12),KRGNSIZE CONSTANT FIELD LABEL            
         LH    R2,SMF4RSH0              PRIVATE AREA SIZE               
         CVD   R2,DECW                  CONVERT TO DECIMAL              
         MVC   SYSOUTLN+43(6),=X'402020202120' EDIT MASK FOR REGION     
         ED    SYSOUTLN+43(6),DECW+5    EDIT REGION SIZE                
         MVI   SYSOUTLN+49,KK           CONSTANT 'K'                    
         TM    SMF4RIN,B'00000001'      V=R JOBSTEP?                    
         BZ    REGVRNO                    NO                            
         MVC   SYSOUTLN+41(2),=C'VR'    INDICATE V=R                    
*                                                                       
REGVRNO  DS    0H                                                       
*                                                                       
* ELAPSED TIME HH:MM:SS.TT (FROM SMF4 RECORD)                           
*                                                                       
         MVC   SYSOUTLN+52(13),KELAPSED CONSTANT FIELD LABEL            
         MVC   DECW+4(4),SMF4DTE       STEP END DATE                    
         MVC   SMF4STID(1),SMF4DTE     COPY CENTURY BYTE TO START DATE  
         SP    DECW+4(4),SMF4STID(4)   MINUS STEP START DATE            
         XC    DECW(4),DECW            EQUALS NUMBER OF DAYS            
         CVB   R1,DECW                 CONVERT TO BINARY                
         XR    R0,R0                   CLEAR R0 FOR MULTIPLY            
         M     R0,=A(24*60*60*100)     TIMES 100THS OF SECONDS PER DAY  
         ICM   R3,B'1111',SMF4TME      GET STEP END TIME                
         AR    R1,R3                   ADD TO DAYS ADJUST               
         ICM   R3,B'1111',SMF4SIT      GET STEP START TIME              
         SR    R1,R3                   SUBTRACT GIVING ELAPSED TIME     
         LA    R2,KPERIODM             MASK TO PRINT ELAPSED TIME       
         BAL   R8,TIMEFORM             CONVERT TIME TO PRINTABLE        
         MVC   SYSOUTLN+66(11),TIMECHAR ELAPSED TIME TO PRINT AREA      
         MVC   ACTRELAP(11),TIMECHAR   ELAPSED TIME TO CONSOLE MSG      
*                                                                       
* TCB TIME (FROM PARAMETER FIELD)                                       
*                                                                       
         MVC   SYSOUTLN+79(9),KTCBTIM  CONSTANT FIELD LABEL             
         L     R2,PLSETADR             GET ADDR OF STEP CPU TIME (TCB)  
         XR    R1,R1                   CLEAR R1                         
         ICM   R1,B'0111',0(R2)        GET TCB TIME                     
         LA    R2,KPERIODM             MASK TO PRINT ELAPSED TIME       
         BAL   R8,TIMEFORM             CONVERT TIME TO PRINTABLE        
         MVC   SYSOUTLN+92(11),TIMECHAR STEP TCB TIME TO PRINT AREA     
*                                                                       
* PROGRAM LOAD TIME  (FROM SMF4 RECORD)                                 
*                                                                       
         MVC   SYSOUTLN+105(13),KPGLDTIM CONSTANT FIELD LABEL           
         ICM   R1,B'1111',SMF4PPST     GET PROBLEM PROGRAM START TIME   
         LA    R2,KCLOCKM              MASK TO PRINT CLOCK TIME         
         BAL   R8,TIMEFORM             CONVERT TIME TO PRINTABLE        
         MVC   SYSOUTLN+119(11),TIMECHAR STEP SRB TIME TO PRINT AREA    
*                                                                       
         WRSYSOUT ,                    CALL IEFYS TO WRITE SYSOUT LINE  
*                                                                       
* SYSOUT LINE #6                                                        
*                                                                       
         FILL  SYSOUTLN,KSPACE         INITIALIZE SYSOUT LINE 6         
         MVI   SYSOUTLN,KSTAR            ..                             
         MVI   SYSOUTLN+131,KSTAR          ..                           
*                                                                       
* STEP COMPLETION CODE (FROM SMF4 RECORD)                               
*                                                                       
         TM    SMF4STI,B'00000001'     WAS STEP FLUSHED?                
         BO    CCNOEXEC                  YES, GO HANDLE                 
         MVC   SYSOUTLN+3(15),KCONDCDE CONSTANT FIELD LABEL             
         TM    SMF4STI,B'00000010'     ABEND?                           
         BO    CCABEND                   YES, GO HANDLE                 
         XR    R1,R1                   CLEAR R1                         
         ICM   R1,3,SMF4SCC            GET STEP CC                      
         N     R1,=X'00000FFF'         CLEAR UNUSED PORTION             
         CVD   R1,DECW                 CONVERT TO DECIMAL               
         UNPK  UNPACKW(5),DECW+5(3)    UNPACK                           
         OI    UNPACKW+4,X'F0'         CORRECT SIGN FOR PRINT           
         MVC   SYSOUTLN+21(5),UNPACKW  MOVE TO PRINT                    
         MVC   ACTRSCC(5),UNPACKW        AND CONSOLE MSG                
         B     CCDONE                  CONTINUE WITH NEXT FIELD         
*                                                                       
CCNOEXEC DS    0H                                                       
         MVC   SYSOUTLN+3(19),KFLUSHED INDICATE STEP NOT RUN            
         MVC   ACTRSCC(5),=C'NOXEC'      AND ON CONSOLE MSG             
         B     LN6DONE                 LINE COMPLETE, GO PRINT          
*                                                                       
CCABEND  DS    0H                                                       
         TM    SMF4SCC,X'80'           USER ABEND?                      
         BO    CCUSERAB                  YES, GO HANDLE                 
         MVC   SYSOUTLN+21(2),=C'S-'   INDICATE SYSTEM ABEND CODE       
         MVC   ACTRSCC(2),=C'S-'         AND ON CONSOLE MSG             
         UNPK  DECW(4),SMF4SCC(3)      UNPACK                           
         TR    DECW(4),HEXTRANS-X'F0'  TRANSLATE TO PRINTABLE           
         MVC   SYSOUTLN+23(3),DECW     MOVE TO PRINT                    
         MVC   ACTRSCC+2(3),DECW         AND CONSOLE MSG                
         B     CCDONE                  CONTINUE WITH NEXT FIELD         
*                                                                       
CCUSERAB DS    0H                                                       
         MVI   SYSOUTLN+21,C'U'        INDICATE USER ABEND CODE         
         MVI   ACTRSCC,C'U'              AND ON CONSOLE MSG             
         XR    R1,R1                   CLEAR R1                         
         ICM   R1,3,SMF4SCC            GET STEP CC                      
         N     R1,=X'00000FFF'         CLEAR UNUSED PORTION             
         CVD   R1,DECW                 CONVERT TO DECIMAL               
         UNPK  UNPACKW(5),DECW+5(3)    UNPACK                           
         OI    UNPACKW+4,X'F0'         CORRECT SIGN FOR PRINT           
         MVC   SYSOUTLN+22(4),UNPACKW+1 MOVE TO PRINT                   
         MVC   ACTRSCC+1(4),UNPACKW+1   AND CONSOLE MSG                 
*                                                                       
CCDONE   DS    0H                                                       
*                                                                       
* PERFORMANCE GROUP NUMBER (FROM SMF4 RECORD)                           
*                                                                       
         MVC   SYSOUTLN+28(18),KPFMGRP CONSTANT FIELD LABEL             
         XR    R1,R1                   CLEAR R1                         
         ICM   R1,B'0011',REL4PGNO     GET PERFORMANCE GROUP NUMBER     
         CVD   R1,DECW                 CONVERT TO DECIMAL               
         UNPK  SYSOUTLN+47(3),DECW+6(2)                                 
         OI    SYSOUTLN+49,X'F0'                                        
*                                                                       
LN6DONE  DS    0H                                                       
         WRSYSOUT ,                    CALL IEFYS TO WRITE SYSOUT LINE  
*                                                                       
* SYSOUT LINE #7                                                        
*                                                                       
         FILL  SYSOUTLN,KSPACE         INITIALIZE SYSOUT LINE 7         
         MVI   SYSOUTLN,KSTAR            ..                             
         MVI   SYSOUTLN+131,KSTAR          ..                           
         MVC   SYSOUTLN+57(38),KSUHEADL      ..                         
         MVC   SYSOUTLN+95(35),KSUHEADR        ..                       
*                                                                       
* JES2 CARD IMAGES READ (FROM SMF4 RECORD)                              
*                                                                       
         MVC   SYSOUTLN+28(11),KJES2NCI CONSTANT FIELD LABEL            
         XR    R1,R1                   CLEAR R1                         
         ICM   R1,B'1111',SMF4NCI      GET JES2 CARD IMAGES READ        
         CVD   R1,DECW                 CONVERT TO DECIMAL               
         MVC   SYSOUTLN+39(11),=X'4020206B2020206B202120' EDIT MASK     
         ED    SYSOUTLN+39(11),DECW+3  EDIT FOR PRINT                   
*                                                                       
         WRSYSOUT ,                    CALL IEFYS TO WRITE SYSOUT LINE  
*                                                                       
* SYSOUT LINE #8                                                        
*                                                                       
         FILL  SYSOUTLN,KSPACE         INITIALIZE SYSOUT LINE 8         
         MVI   SYSOUTLN,KSTAR            ..                             
         MVI   SYSOUTLN+131,KSTAR          ..                           
*                                                                       
* SERVICE UNITS (FROM SMF4 RECORD)                                      
*                                                                       
         ICM   R1,B'1111',REL4SST      GET # SERVICE UNITS USED BY STEP 
         CVD   R1,DECW                 CONVERT TO DECIMAL               
         MVC   SYSOUTLN+58(12),=X'402020206B2020206B202120' MASK        
         ED    SYSOUTLN+58(12),DECW+3  EDIT TO PRINT                    
*                                                                       
* NUMBER OF PAGE-INS (FROM SMF4 RECORD)                                 
*                                                                       
         ICM   R1,B'1111',REL4PGIN     GET # OF PAGE-INS                
         CVD   R1,DECW                 CONVERT TO DECIMAL               
         MVC   SYSOUTLN+72(5),=X'4020202120' MASK                       
         ED    SYSOUTLN+72(5),DECW+5   EDIT TO PRINT                    
         MVI   SYSOUTLN+78,KSLASH      CONSTANT FIELD SEPARATOR         
*                                                                       
* NUMBER OF PAGE-OUTS (FROM SMF4 RECORD)                                
*                                                                       
         ICM   R1,B'1111',REL4PGOT     GET # OF PAGE-OUTS               
         CVD   R1,DECW                 CONVERT TO DECIMAL               
         MVC   SYSOUTLN+79(5),=X'4020202120' MASK                       
         ED    SYSOUTLN+79(5),DECW+5   EDIT TO PRINT                    
*                                                                       
* NUMBER OF TIMES SWAPPED (FROM SMF4 RECORD)                            
*                                                                       
         ICM   R1,B'1111',REL4NSW      GET # TIMES SWAPPED              
         CVD   R1,DECW                 CONVERT TO DECIMAL               
         MVC   SYSOUTLN+88(5),=X'4020202120' MASK                       
         ED    SYSOUTLN+88(5),DECW+5   EDIT TO PRINT                    
*                                                                       
* NUMBER OF PAGES SWAPPED IN (FROM SMF4 RECORD)                         
*                                                                       
         ICM   R1,B'1111',REL4PSI      GET # OF PAGES SWAPPED IN        
         CVD   R1,DECW                 CONVERT TO DECIMAL               
         MVC   SYSOUTLN+100(5),=X'4020202120' MASK                      
         ED    SYSOUTLN+100(5),DECW+5   EDIT TO PRINT                   
         MVI   SYSOUTLN+106,KSLASH      CONSTANT FIELD SEPARATOR        
*                                                                       
* NUMBER OF PAGES SWAPPED OUT (FROM SMF4 RECORD)                        
*                                                                       
         ICM   R1,B'1111',REL4PSO      GET # OF PAGES SWAPPED OUT       
         CVD   R1,DECW                 CONVERT TO DECIMAL               
         MVC   SYSOUTLN+107(5),=X'4020202120' MASK                      
         ED    SYSOUTLN+107(5),DECW+5  EDIT TO PRINT                    
*                                                                       
* NUMBER OF VIO PAGES SWAPPED IN (FROM SMF4 RECORD)                     
*                                                                       
         ICM   R1,B'1111',REL4VPI      GET # VIO PAGES SWAPPED IN       
         CVD   R1,DECW                 CONVERT TO DECIMAL               
         MVC   SYSOUTLN+118(5),=X'4020202120' MASK                      
         ED    SYSOUTLN+118(5),DECW+5  EDIT TO PRINT                    
         MVI   SYSOUTLN+124,KSLASH     CONSTANT FIELD SEPARATOR         
*                                                                       
* NUMBER OF VIO PAGES SWAPPED OUT (FROM SMF4 RECORD)                    
*                                                                       
         ICM   R1,B'1111',REL4VPO      GET # VIO PAGES SWAPPED OUT      
         CVD   R1,DECW                 CONVERT TO DECIMAL               
         MVC   SYSOUTLN+125(5),=X'4020202120' MASK                      
         ED    SYSOUTLN+125(5),DECW+5   EDIT TO PRINT                   
*                                                                       
         WRSYSOUT ,                    CALL IEFYS TO WRITE SYSOUT LINE  
*                                                                       
* SYSOUT LINE #9 (JUST A SEPARATOR LINE)                                
*                                                                       
         FILL  SYSOUTLN,KSPACE         INITIALIZE SYSOUT LINE 9         
         MVI   SYSOUTLN,KSTAR            ..                             
         MVI   SYSOUTLN+131,KSTAR          ..                           
*                                                                       
         WRSYSOUT ,                    CALL IEFYS TO WRITE SYSOUT LINE  
*                                                                       
* SYSOUT LINE #10 (HEADING FOR I/O COUNTS FOR DEVICES)                  
*                                                                       
         LH    R5,SMF4LENN             GET DEVICE ENTRY PORTION LENGTH  
         SH    R5,=H'2'                MINUS 2 FOR LENGTH FIELD         
         SRL   R5,3                    DIVIDED BY 8                     
         LTR   R5,R5                   EQUALS NUMBER OF DEVICE ENTRIES  
         BZ    CLOSEBOX                IF ZERO - SKIP WRITING SECTION   
         LA    R6,SMF4LENN+2           ADDRESS OF FIRST DEVICE ENTRY    
         FILL  SYSOUTLN,KSPACE         INITIALIZE SYSOUT LINE 10        
         MVI   SYSOUTLN,KSTAR            ..                             
         MVI   SYSOUTLN+131,KSTAR          ..                           
         MVC   SYSOUTLN+4(19),KSEXCPHD       ..                         
         MVC   SYSOUTLN+25(19),KSEXCPHD       ..                        
         MVC   SYSOUTLN+46(19),KSEXCPHD         ..                      
         MVC   SYSOUTLN+67(19),KSEXCPHD           ..                    
         MVC   SYSOUTLN+88(19),KSEXCPHD             ..                  
         MVC   SYSOUTLN+109(19),KSEXCPHD              ..                
         XR    R3,R3                   CLEAR R3 (OFFSET ON LINE)        
*                                                                       
         WRSYSOUT ,                    CALL IEFYS TO WRITE SYSOUT LINE  
*                                                                       
* SYSOUT LINE #11-?? (EXCP I/O COUNTS FOR DEVICES)                      
*                                                                       
         FILL  SYSOUTLN,KSPACE         INITIALIZE SYSOUT LINE 10        
         MVI   SYSOUTLN,KSTAR            ..                             
         MVI   SYSOUTLN+131,KSTAR          ..                           
*                                                                       
*   LOOP THROUGH ALL SMF4 DEVICE ENTRIES                                
*                                                                       
NEXTDEVC DS    0H                                                       
         MVC   EXCPBLOK,KEXCP          INITIALIZE OUTPUT FORMAT AREA    
         MVC   DECW+5(2),2(R6)         GET UNIT ADDRESS                 
         MVI   DECW+7,X'0C'            APPEND JUNK CHARACTER FO UNPK    
         UNPK  UNPACKW(5),DECW+5(3)    UNPACK ADDRESS                   
         TR    UNPACKW(4),HEXTRANS-C'0' TRANSLATE X'F1-FF' TO C'A-F'    
         CLC   UNPACKW+1(3),=C'000'    IS ADDRESS ZERO?                 
         BNE   ISITVIO                   NO, GO CHECK FOR VIO           
         L     R0,4(R6)                GET EXCP COUNT FOR DEVICE        
         LTR   R0,R0                   IS IT ZERO?                      
         BE    EXCPNEXT                  YES, DON'T PRINT ANYTHING      
         B     CONVEXCP                PRINT COUNT FOR DUMMY DEVICE     
*                                                                       
ISITVIO  DS    0H                                                       
         MVC   EXCPADDR,UNPACKW+1      MOVE ADDRESS TO FORMAT AREA      
         CLC   UNPACKW+1(3),=C'FFF'    DOES ADDRESS INDICATE VIO?       
         BNE   DEVCNAME                  NO, GO DETERMINE DEVICE        
         MVI   EXCPDEVC,C'D'           INDICATE CLASS IS DISK           
         MVC   EXCPDEVN,=C' VIO'         AND NAME IS VIO                
         B     CONVEXCP                AND REPORT COUNT                 
*                                                                       
DEVCNAME DS    0H                                                       
         MVI   EXCPDEVC,C' '           DEFAULT DEVICE CLASS             
         MVC   EXCPDEVN,=C'MISC'         AND NAME OF MISC               
         CLI   0(R6),X'20'             IS IT A DASD DEVICE?             
         BE    DASDDEVC                  YES, GO PROCESS                
         CLI   0(R6),X'80'             IS IT A TAPE DEVICE?             
         BNE   CONVEXCP                  NO, GO REPORT COUNT AS MISC    
*                                                                       
TAPEDEVC MVI   EXCPDEVC,C'T'           INDICATE TAPE DEVICE CLASS       
         XR    R1,R1                   CLEAR R1                         
         IC    R1,1(R6)                GET DEVICE TYPE                  
         IC    R1,TAPEOFST(R1)         GET OFFSET TO TAPE DEVICE NAME   
         LA    R1,TAPETABL(R1)         GET ADDRESS OF TAPE DEVICE NAME  
         MVC   EXCPDEVN,0(R1)          MOVE DEVICE NAME TO FORMAT AREA  
         B     CONVEXCP                                                 
*                                                                       
DASDDEVC MVI   EXCPDEVC,C'D'           INDICATE DASD DEVICE CLASS       
         XR    R1,R1                   CLEAR R1                         
         IC    R1,1(R6)                GET DEVICE TYPE                  
         IC    R1,DASDOFST(R1)         GET OFFSET TO DASD DEVICE NAME   
         LA    R1,DASDTABL(R1)         GET ADDRESS OF DEVICE NAME       
         MVC   EXCPDEVN,0(R1)          MOVE DEVICE NAME TO FORMAT AREA  
*                                                                       
CONVEXCP L     R0,4(R6)                EXCP COUNT                       
         CVD   R0,DECW                 CONVERT TO PACKED                
         MVC   EXCPCNT,=X'40202020202020202120' SET UP EDIT MASK        
         ED    EXCPCNT,DECW+3          EDIT TO FORMAT AREA              
*                                                                       
         XR    R0,R0                   CLEAR R0 FOR MULTIPLY            
         LR    R1,R3                   GET THIS ENTRY NUMBER ON LINE    
         M     R0,=F'21'               MULTIPLY BY LENGTH OF ENTRY      
         LA    R2,SYSOUTLN+4           ADDRESS OF FIRST ENTRY           
         AR    R2,R1                   OFFSET TO THIS ENTRY             
         MVC   0(21,R2),EXCPBLOK       MOVE ENTRY TO SYSOUT BUFFER      
         LA    R3,1(,R3)               INCREMENT ENTRY COUNT            
         C     R3,=F'6'                IS THIS 6TH ENTRY ON LINE        
         BL    EXCPNEXT                  NO, CONTINUE                   
*                                                                       
         WRSYSOUT ,                    CALL IEFYS TO WRITE SYSOUT LINE  
         FILL  SYSOUTLN,KSPACE         RE-INITIALIZE SYSOUT LINE        
         MVI   SYSOUTLN,KSTAR            ..                             
         MVI   SYSOUTLN+131,KSTAR          ..                           
         XR    R3,R3                   CLEAR R3 (OFFSET ON LINE)        
*                                                                       
EXCPNEXT DS    0H                                                       
         LA    R6,8(R6)                BUMP TO NEXT SMF4 DEVICE ENTRY   
         BCT   R5,NEXTDEVC             GO AGAIN IF MORE DEVICES         
*                                                                       
         LTR   R3,R3                   ARE THERE ENTRIES IN BUFFER?     
         BZ    EXCPERRS                  NO, GO CHECK FOR EXCEPTION     
         WRSYSOUT ,                    CALL IEFYS TO WRITE SYSOUT LINE  
*                                                                       
EXCPERRS DS    0H                                                       
         TM    SMF4RIN,X'02'           WERE THER EXCP COUNT ERRORS?     
         BZ    CLOSEBOX                  NO - SKIP WARNING LINE         
         FILL  SYSOUTLN,KSPACE         RE-INITIALIZE SYSOUT LINE        
         MVI   SYSOUTLN,KSTAR            ..                             
         MVI   SYSOUTLN+131,KSTAR          ..                           
         MVC   SYSOUTLN+30(42),KWARN   WARNING MESSAGE TO SYSOUT LINE   
         WRSYSOUT ,                    CALL IEFYS TO WRITE SYSOUT LINE  
*                                                                       
CLOSEBOX DS    0H                                                       
*                                                                       
* FINAL SYSOUT LINE (ALL ASTERISKS)                                     
*                                                                       
         FILL  SYSOUTLN,KSTAR          INITIALIZE FINAL SYSOUT LINE     
*                                                                       
         WRSYSOUT ,                    CALL IEFYS TO WRITE SYSOUT LINE  
*                                                                       
* WRITE IEFACTRT MESSAGE TO CONSOLE                                     
*                                                                       
         STEPWTO  ,                    WRITE MESSAGE TO CONSOLE         
*                                                                       
*********************************************************************** 
* EXIT BACK TO SYSTEM                                                 * 
*********************************************************************** 
GOBACK   DS    0H                                                       
         ESTAE 0                       CANCEL ESTAE EXIT                
RTRYRTN2 DS    0H                      ESTAE RETRY ROUTINE WITH SDWA    
*                                        JUST FREE STORAGE AND EXIT     
         L     R3,4(R13)               RETRIEVE SYSTEM SAVE AREA ADDR   
         L     R0,WALNGTH              LOAD FREEMAIN INFORMATION        
         LR    R1,R13                  LOAD ADDRESS OF WORKAREA MEMORY  
         FREEMAIN R,LV=(0),A=(1)       FREE ACQUIRED MEMORY             
         LR    R13,R3                  RESTORE CALLER'S SAVE AREA       
QUIKEXIT DS    0H                                                       
         LM    R14,R12,12(R13)         RESTORE CALLER'S REGISTERS       
         BR    R14                     RETURN TO SYSTEM                 
*********************************************************************** 
*                                                                     * 
*  ESTAE EXIT ROUTINE                                                 * 
*                                                                     * 
*********************************************************************** 
RECOVERY DS    0H                                                       
         USING *,R15                   SET UP ADDRESSABILITY            
         LA    R4,4                    PUT 4 IN REGISTER FOR COMPARE    
         CR    R0,R4                   IS SDWA PRESENT?                 
         BNE   HAVESDWA                YES, BR TO PROCESS WITH SDWA     
         L     R0,0(R2)                LOAD RETRY ADDR FROM PARM LIST   
         LA    R15,4                   SET RETCODE TO RETRY ADDR IN R0  
         BR    R14                     RETURN WITH RETRY ADDR           
HAVESDWA DS    0H                      ENTER HERE IF SDWA PRESENT       
         ST    R14,12(R13)             SAVE RETURN ADDRESS              
         L     R2,0(R1)                LOAD PARM LIST ADDR FROM SDWA    
         L     R2,4(R2)                LOAD RETRY ADDRESS               
         SETRP RC=4,,RETADDR=(2),RETREGS=YES,FRESDWA=YES,REGS=(14)      
         DROP  R15                                                      
*********************************************************************** 
*                                                                     * 
*  ESTAE RETRY ROUTINE WHEN NO SDWA WAS PRESENT                       * 
*                                                                     * 
*********************************************************************** 
RTRYRTN1 DS    0H                      ROUTINE WITH NO SDWA PRESENT     
         LM    R11,R13,8(R1)           LOAD REGS FOR ESTAE PARM LIST    
         B     RTRYRTN2                  AND GET OUT                    
*********************************************************************** 
* CONVERT JULIAN DATE IN 4 CHARACTER PACKED FIELD (0YYDDDS) PLACED IN * 
* DATEPACK TO VALID Y2K FORMAT (YYYYDDDS); UNPACKED VERSION WILL BE   * 
* AVAILABLE IN DATECHAR.                                              * 
*********************************************************************** 
DATEFORM DS    0H                                                       
         UNPK  DATECHAR(7),DATEPACK    UNPACK 4 BYTE JULIAN DATE        
         OI    DATECHAR+6,X'F0'        CORRECT SIGN FIELD               
         CLC   DATECHAR(2),=C'00'      SMF DATE (00YYDDD)?              
         BNE   DATEFO01                  BRANCH IF NOT                  
         MVC   DATECHAR(2),=C'19'      CHANGE YEAR TO 1900              
DATEFO01 CLC   DATECHAR(2),=C'01'      SMF DATE (01YYDDD)?              
         BNE   DATEFO02                  BRANCH IF NOT                  
         MVC   DATECHAR(2),=C'20'      CHANGE YEAR TO 2000              
DATEFO02 PACK  DATEPACK,DATECHAR(7)    REPACK MODIFIED DATE             
         BR    R8                      RETURN                           
         EJECT                                                          
*********************************************************************** 
* CONVERT TIME IN 100THS OF SECONDS (IN R1) TO A PRINTABLE            * 
* FIELD IN THE FORMAT: HH.MM.SS.HH (WILL BE IN TIMECHAR)              * 
*********************************************************************** 
TIMEFORM DS    0H                                                       
         XR    R0,R0                   CLEAR R0 FOR DIVISION            
         D     R0,=A(60*60*100)        COMPUTE HOURS IN R1              
         LR    R7,R1                   SAVE MINUTES                     
         SRDL  R0,32                   REMAINDER TO R1 (CLEAR R0)       
         D     R0,=A(60*100)           COMPUTE MINUTES IN R1            
         M     R6,=F'100'              SHIFT LEFT TWO DIGITS            
         AR    R7,R1                   ADD MINUTES TO HOURS             
         SRDL  R0,32                   REMAINDER TO R1 (CLEAR R0)       
         D     R0,=A(100)              COMPUTE SECONDS IN R1            
*                                      AND 100THS OF SECONDS IN R0      
         M     R6,=F'100'              SHIFT LEFT TWO DIGITS            
         AR    R7,R1                   ADD SECONDS TO MINUTES AND HOURS 
         M     R6,=F'100'              SHIFT LEFT TWO DIGITS            
         AR    R7,R0                   ADD 100THS OF SECONDS            
         CVD   R7,DECW                 CONVERT TO DECIMAL               
         MVC   TIMECHAR-2(13),0(R2)    MOVE MASK TO PRINT AREA          
         ED    TIMECHAR-2(13),DECW+3   EDIT                             
         BR    R8                      AND RETURN                       
         EJECT                                                          
*********************************************************************** 
* CONSTANTS                                                           * 
*********************************************************************** 
KSPACE   EQU   X'40'                   SPACE                            
KSTAR    EQU   C'*'                    ASTERISK                         
KCOMMA   EQU   C','                    COMMA                            
KPERIOD  EQU   C'.'                    PERIOD                           
KRPAREN  EQU   C')'                    RIGHT PARENTHESIS                
KLPAREN  EQU   C'('                    LEFT PARENTHESIS                 
KSLASH   EQU   C'/'                    SLASH                            
KCOLON   EQU   C':'                    COLON                            
KK       EQU   C'K'                    K                                
KCLOCKM  DC    XL13'402120207A20207A2020404040'                         
KPERIODM DC    XL13'402120207A20207A20204B2020'                         
KWTOID   DC    CL9'IEFACTRT '                                           
KSJOBNAM DC    CL11' JOB NAME: '                                        
KSJOBCRD DC    CL14' JOBCARD READ '                                     
KSCPUMAJ DC    CL5' 370/'                                               
KSVS2    DC    CL6' VS2 R'                                              
KSTEPNUM DC    CL12'STEP NUMBER:'                                       
KACTRSTP DC    CL8'(JS#000)'                                            
KUSERCOR DC    CL10'USER CORE:'                                         
KSTEPNAM DC    CL10'STEP NAME:'                                         
KSTRTTIM DC    CL11'START TIME:'                                        
KCPUTIM  DC    CL9'CPU TIME:'                                           
KSRBTIM  DC    CL9'SRB TIME:'                                           
KSYSCOR  DC    CL12'SYSTEM CORE:'                                       
KSTOPTIM DC    CL10'STOP TIME:'                                         
KACTVTIM DC    CL12'ACTIVE TIME:'                                       
KPGLDTIM DC    CL13'PROGRAM LOAD:'                                      
KELAPSED DC    CL13'ELAPSED TIME:'                                      
KTCBTIM  DC    CL9'TCB TIME:'                                           
KALLOCTI DC    CL11'ALLOC TIME:'                                        
KPGMNAME DC    CL13'PROGRAM NAME:'                                      
KRGNSIZE DC    CL12'REGION SIZE:'                                       
KCONDCDE DC    CL15'CONDITION CODE:'                                    
KFLUSHED DC    CL19'-STEP NOT EXECUTED-'                                
KPFMGRP  DC    CL18'PERFORMANCE GROUP:'                                 
KJES2NCI DC    CL11'JES2 CARDS:'                                        
KSUHEADL DC    CL38'SERVICE UNITS  PAGES IN/OUT  # SWAPS  '             
KSUHEADR DC    CL35'PAGES SWAP IN/OUT  VIO PAGES IN/OUT'                
KSEXCPHD DC    CL19'ADDR/UNIT I/O COUNT'                                
KEXCP    DC    CL21'JES/DUMMY 999999999  '                              
KWARN    DC    CL42'*** WARNING: EXCP COUNTS MAY BE WRONG ***'          
WALNGTH  DC    0F'0',AL1(SUBPOOL),AL3(WORKAEND-WORKAREA)                
WTODEF   WTO   '.........1.........2.........3.........4.........5.....C
               ....6.....',ROUTCDE=2,DESC=(6),MF=L                      
WTODEFL  EQU   *-WTODEF                LENGTH OF MODEL WTO              
WTOMSGL  EQU   (WTODEFL-8)             LENGTH OF WTO MESSAGE AREA   JLM 
ESTAEL   ESTAE MF=L                    MODEL ESTAE PARM LIST            
LESTAEL  EQU   *-ESTAEL                LENGTH OF MODEL ESTAE            
HEXTRANS DC    C'0123456789ABCDEF'                                      
*********************************************************************** 
*                                                                     * 
*   DASD DEVICE NAME OFFSETS AND TABLE                                * 
*                                                                     * 
*********************************************************************** 
DASDOFST DC    AL1(DADASD-DASDTABL)    00    GENERIC 'DASD'             
         DC    AL1(DA2311-DASDTABL)    01    '2311'                     
         DC    AL1(DADASD-DASDTABL)    02    GENERIC 'DASD'             
         DC    AL1(DADASD-DASDTABL)    03    GENERIC 'DASD'             
         DC    AL1(DADASD-DASDTABL)    04    GENERIC 'DASD'             
         DC    AL1(DADASD-DASDTABL)    05    GENERIC 'DASD'             
         DC    AL1(DADASD-DASDTABL)    06    GENERIC 'DASD'             
         DC    AL1(DADASD-DASDTABL)    07    GENERIC 'DASD'             
         DC    AL1(DA2314-DASDTABL)    08    '2314'                     
         DC    AL1(DA3330-DASDTABL)    09    '3330'                     
         DC    AL1(DA3340-DASDTABL)    0A    '3340'                     
         DC    AL1(DA3350-DASDTABL)    0B    '3350'                     
         DC    AL1(DA3375-DASDTABL)    0C    '3375'                     
         DC    AL1(DA3330-DASDTABL)    0D    '3330'                     
         DC    AL1(DA3380-DASDTABL)    0E    '3380'                     
         DC    AL1(DA3390-DASDTABL)    0F    '3390'                     
DASDTABL DS    0D                                                       
DADASD   DC    CL4'DASD'                                                
DA2311   DC    CL6'2311'                                                
DA2314   DC    CL6'2314'                                                
DA3330   DC    CL6'3330'                                                
DA3340   DC    CL6'3340'                                                
DA3350   DC    CL6'3350'                                                
DA3375   DC    CL6'3375'                                                
DA3380   DC    CL6'3380'                                                
DA3390   DC    CL6'3390'                                                
*********************************************************************** 
*                                                                     * 
*   TAPE DEVICE NAME OFFSETS AND TABLE                                * 
*                                                                     * 
*********************************************************************** 
TAPEOFST DC    AL1(TATAPE-TAPETABL)    00    GENERIC 'TAPE'             
         DC    AL1(TA2400-TAPETABL)    01    '2400'                     
         DC    AL1(TATAPE-TAPETABL)    02    GENERIC 'TAPE'             
         DC    AL1(TA3400-TAPETABL)    03    '3400'                     
TAPETABL DS    0D                                                       
TATAPE   DC    CL6'TAPE'                                                
TA2400   DC    CL6'2400'                                                
TA3400   DC    CL6'3400'                                                
         EJECT                                                          
*********************************************************************** 
* WORK AREA ALLOCATED VIA GETMAIN                                     * 
*********************************************************************** 
WORKAREA DSECT                                                          
         DS    18F                     STANDARD REGISTER SAVE AREA      
ESTAEW   DS    XL(LESTAEL)             ESTAE PARM LIST AREA             
ESTAPARM DS    5F                      PARM LIST PASSED TO RETRY RTN    
*                                        RETRY ROUTINE ADDRESS (NO SWDA 
*                                        RETRY ROUTINE ADDRESS (SWDA    
*                                        BASE REGISTER R11              
*                                        IEFYS PARMS   R12              
*                                        DATA REGISTER R13              
* * * * * * * *  PARAMETERS PASSED ON ENTRY SAVED BELOW * * * * * * * * 
PLCEPADR DS    F                       ADDR OF COMMON EXIT PARM AREA    
PLSTPADR DS    F                       ADDR OF STEPNAME                 
PLPGNADR DS    F                       ADDR OF PROGRAMMER'S NAME        
PLJETADR DS    F                       ADDR OF JOB EXECUTION TIME       
PLJAFADR DS    F                       ADDR OF JOB ACCOUNTING INFO      
PLSETADR DS    F                       ADDR OF STEP EXECUTION TIME      
PLSAFADR DS    F                       ADDR OF STEP ACCOUNTING INFO     
PLFSNADR DS    F                       ADDR OF FLAG AND STEP NUM        
PLTSTADR DS    F                       ADDR OF TERMINATION STATUS       
PLSMFADR DS    F                       ADDR OF TERMINATION RECORD       
* * * * * * * *  PARAMETERS PASSED ON ENTRY SAVED ABOVE * * * * * * * * 
* * * *  * * COMMON EXIT PARAMETER LIST FIELDS SAVED BELOW  * * * * * * 
CEPJOBN  DS    CL8                      JOBNAME                         
CEPTIME  DS    CL8                      TIME STAMP                      
CEPSYSID DS    CL4                      SMF SYSTEM ID                   
CEPUID   DS    CL8                      USER ID                         
CEPSNUM  DS    CL1                      STEP NUMBER                     
CEPSMFOP DS    CL1                      SMF OPTION                      
CEPRSTID DS    CL1                      RESTART INDICATOR               
CEPJCLAS DS    CL1                      JOB CLASS                       
CEPUCOMM DS    0F                       USER COMMUNICATION AREA         
CEPUCOMH DS    C                        USER COMMUNICATION AREA HEADER  
CEPUCOMT DS    CL3                      USER COMMUNICATION AREA TRAILER 
* * * *  * * COMMON EXIT PARAMETER LIST FIELDS SAVED ABOVE  * * * * * * 
         DS    0F                                                       
WTOBUF   DS    CL(WTODEFL)             WTO BUFFER                       
ACTRMSG  DS    0CL(WTOMSGL)            CONSOLE MESSAGE                  
ACTRHDR  DS    CL9                       MESSAGE HEADER 'IEFACTRT '     
ACTRSTPN DS    CL8                       STEP NUMBER    '(JS#999)'      
ACTRSNNN EQU   ACTRSTPN+4,3                                  999        
ACTRSEP1 DS    CL1                       SEPARATOR      '/'             
ACTRPGMN DS    CL8                       PROGRAM NAME                   
ACTRSEP2 DS    CL1                       SEPARATOR      '/'             
ACTRCPUT DS    CL11                      CPU TIME                       
ACTRSEP3 DS    CL1                       SEPARATOR      '/'             
ACTRELAP DS    CL11                      CLOCK TIME                     
ACTRSEP4 DS    CL1                       SEPARATOR      '/'             
ACTRSCC  DS    CL5                       STEP COMPLETION CODE           
ACTRSEP5 DS    CL1                       SEPARATOR      '/'             
ACTRJOBN DS    CL8                       JOB NAME                       
SYSOUT@  DS    F                       ADDRESS OF SYSOUT LINE           
SYSOUT#  DS    H                       LENGTH OF SYSOUT LINE            
SYSOUTLN DS    CL132                   AREA TO BUILD SYSOUT LINE        
DATEPACK DS    PL4                     DATE FOR CORRECTION/CONVERSION   
DATECHAR DS    CL8                     JULIAN DATE YYYYDDD              
DECW     DS    D                       DECIMAL CONVERSION WORK FIELD    
PACKW    DS    PL4                     PACKED WORK AREA                 
UNPACKW  DS    CL8                     UNPACKED WORK AREA               
CPUMODEL DS    CL4                     EDIT FIELD FOR CPU MODEL         
         DS    CL2                     TIME CONVERSION REQUIRED FILLER  
TIMECHAR DS    CL11                    TIME CONVERSION FIELD            
*        FIELDS BELOW COPIED FROM SMF4 RELOCATE SECTION                 
REL4PGIN DS    BL4                     NUMBER OF PAGE-INS               
REL4PGOT DS    BL4                     NUMBER OF PAGE-OUTS              
REL4NSW  DS    BL4                     NUMBER OF SWAPS                  
REL4PSI  DS    BL4                     PAGES SWAPPED IN                 
REL4PSO  DS    BL4                     PAGES SWAPPED OUT                
REL4VPI  DS    BL4                     VAM PAGE INS                     
REL4VPO  DS    BL4                     VAM PAGE OUTS                    
*        FIELDS BELOW COPIED FROM SMF4 PERFORMANCE SECTION              
REL4SST  DS    BL4                     STEP SERVICE TIME                
REL4ACT  DS    BL4                     STEP ACTIVE TIME                 
REL4PGNO DS    BL2                     PERFORMANCE NUMBER               
*        FIELDS BELOW COPIED FROM SMF4 ACCOUNTING SECTION               
         DS 0H                                                          
REL4SETM DS    BL4                     STEP CPU TIME UNDER TCB (.01SEC) 
*        FIELDS ABOVE COPIED FROM SMF4 RELOCATABLE SECTIONS             
EXCPBLOK DS    0CL21                   OUTPUT BUILD AREA FOR EXCP ENTRY 
         DS    CL3                       CUU ADDRESS                    
EXCPADDR EQU   EXCPBLOK,3                 " "                           
         DS    CL1                       /                              
         DS    CL5                       DEVICE CLASS/NAME              
EXCPDEVC EQU   EXCPBLOK+4,1               " "                           
EXCPDEVN EQU   EXCPBLOK+5,4               " "                           
         DS    CL10                      I/O COUNT                      
EXCPCNT  EQU   EXCPBLOK+9,10              " "                           
         DS    CL2                                                      
WORKAEND DS    0F                      END OF GETMAIN WORK AREA         
*                                                                       
*********************************************************************** 
* SYSTEM RECORD DSECTS                                                * 
*********************************************************************** 
         PUSH PRINT                                                     
         PRINT NOGEN                                                    
SMF4     DSECT                                                          
         IFASMFR 4                      SMF 4 RECORD                    
         IHASDWA DSECT=YES              SDWA FOR ESTAE/SETRP MACRO      
         IEFJMR                         JOB MGMT                        
         POP PRINT                                                      
         END   IEFACTRT                                                 
                                                                        
./ ENDUP
/*
//*
//SMPASM02 EXEC SMPASM,M=IEFACTRT,COND=(0,NE)
//*
//RECV03   EXEC SMPAPP,COND=(0,NE),WORK=SYSALLDA
//SMPPTFIN DD  *
++USERMOD (JLM0001)
  .
++VER (Z038)
  FMID(EBB1102)
  .
++MOD(IEFACTRT)
  TXLIB(UMODOBJ)
  .
/*
//SMPCNTL  DD  *
 RECEIVE
         SELECT(JLM0001)
         .
  APPLY
        SELECT(JLM0001)
        DIS(WRITE)
        .
//
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
//JLM0003  JOB (SYSGEN),'J04 M04: JLM0003',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             REGION=4096K
/*JOBPARM LINES=100
//JOBCAT   DD  DSN=SYS1.VSAM.MASTER.CATALOG,DISP=SHR
//*
//*********************************************************************
//* Install USERMOD JLM0003 - replace TSO Command Authorization Table *
//* (IKJEFTE2)                                                        *
//*********************************************************************
//* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//UPDATE01 EXEC PGM=IEBUPDTE,PARM=NEW
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DISP=SHR,DSN=SYS1.UMODSRC
//SYSIN    DD  *
./ ADD NAME=IKJEFTE2
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
* SOURCE -- MVS 3.8 OPTIONAL MATERIALS; SYM1-3                        *
*                                                                     *
* STATUS --                                                           *
*                                                                     *
*    CHANGE LEVEL  040  000                                           *
*                                                                     *
* FUNCTION --                                                         *
*                                                                     *
*    THIS MODULE COMPRISES THE CSECT FOR THE COMMAND NAMES WHICH ARE  *
*    TO BE ATTACHED BY THE TMP AS ELIGIBLE FOR AUTHORIZATION          *
*                                                                     *
* ENTRY POINTS --                                                     *
*                                                                     *
*    NONE                                                             *
*                                                                     *
* INPUT --                                                            *
*                                                                     *
*    NONE                                                             *
*                                                                     *
* OUTPUT --                                                           *
*                                                                     *
*    NONE                                                             *
*                                                                     *
* EXTERNAL REFERENCES --                                              *
*                                                                     *
*     NONE                                                            *
*                                                                     *
* EXITS, NORMAL --                                                    *
*                                                                     *
*    NONE                                                             *
*                                                                     *
* EXITS, ERROR --                                                     *
*                                                                     *
*    NONE                                                             *
*                                                                     *
* TABLES/WORK AREA --                                                 *
*                                                                     *
*    NONE                                                             *
*                                                                     *
* ATTRIBUTE --                                                        *
*                                                                     *
*    REFRESHABLE                                                      *
*                                                                     *
* NOTES --                                                            *
*                                                                     *
*    THIS MODULE IS CHARACTER CODE DEPENDENT UPON AN INTERNAL REPRE-  *
*    SENTATION OF THE EXTERNAL CHARACTER SET EQUIVALENT TO THE ONE IN *
*    USE AT ASSEMBLY TIME                                             *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         EJECT
* /* START OF SPECIFICATIONS ****
*
*1MODULE-NAME = IKJEFTE2
*
*1DESCRIPTIVE-NAME = APF-ELIGIBLE-COMMAND-NAME TABLE
*
*1DESCRIPTION = CONTAINS THE COMMAND-NAMES WHICH ARE TO BE ATTACHED
*           BY THE TMP AS ELIGIBLE FOR AUTHORIZATION
*
*1MODULE TYPE = TABLE
*  2FIELD = BLANKS
*    3TYPE = CHARACTER STRING
*  2FIELD = BLANKS
*    3TYPE = CHARACTER STRING
*  2FIELD = BLANKS
*    3TYPE = CHARACTER STRING
*  2FIELD = BLANKS
*    3TYPE = CHARACTER STRING
*  2FIELD = BLANKS
*    3TYPE = CHARACTER STRING
*  2FIELD = BLANKS
*    3TYPE = CHARACTER STRING
*  2FIELD = BLANKS
*    3TYPE = CHARACTER STRING
*  2FIELD = BLANKS
*    3TYPE = CHARACTER STRING
*  2FIELD = BLANKS
*    3TYPE = CHARACTER STRING
*  2FIELD = BLANKS
*    3TYPE = CHARACTER STRING
*
**** END OF SPECIFICATIONS ***/
         EJECT
         SPACE 3
         ENTRY APFCTABL
         SPACE 1
IKJEFTE2 CSECT
         DC    C'IKJEFTE2'             MODULE NAME
         DC    C'&SYSDATE'             RELEASE LEVEL
APFCTABL DS    0D                      ALIGNMENT
         SPACE 2
         DC    C'#       '             CMDSBSYS
         DC    C'CMDSBSYS'             CMDSBSYS
         DC    C'IM      '             IMON/370
         DC    C'LISTD   '             TSO LIST DATASET
         DC    C'LISTDS  '             TSO LIST DATASET
         DC    C'IKJEHDS1'             TSO LIST DATASET
         DC    C'ISPF    '             ISPF
         DC    C'ISPFLINK'             ISPF
         DC    C'ISPFMAIN'             ISPF
         DC    C'PDS     '             PDS UTILITY
         DC    C'PDS86   '             PDS UTILITY
         DC    C'QUEUE   '             QUEUE
         DC    C'QUE     '             QUEUE
         DC    C'Q       '             QUEUE
         DC    C'RFE     '             REVIEW FRONT END
         DC    C'RPF     '             RPF
         DC    C'RPFMAIN '             RPF
         DC    C'SPY     '             SPY CONSOLE MONITOR
         DC    C'        '             TERMINATOR
         SPACE 2
               END
./ ENDUP
/*
//*
//SMPASM02 EXEC SMPASML,M=IKJEFTE2,COND=(0,NE)
//*
//RECV03   EXEC SMPAPP,COND=(0,NE),WORK=SYSALLDA
//SMPPTFIN DD  *
++USERMOD(JLM0003)
  .
++VER(Z038)
  FMID(EBB1102)
  .
++MOD(IKJEFTE2)
  DISTLIB(AOST4)
  LKLIB(UMODLIB)
  .
/*
//SMPCNTL  DD  *
 RECEIVE
         SELECT(JLM0003)
         .
  APPLY
        SELECT(JLM0003)
        DIS(WRITE)
        .
//
//JLM0004  JOB (SYSGEN),'J05 M05: JLM0004',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             REGION=4096K
/*JOBPARM LINES=100
//JOBCAT   DD  DSN=SYS1.VSAM.MASTER.CATALOG,DISP=SHR
//*
//*********************************************************************
//* Install USERMOD JLM0004 - replace TSO Command Authorization Table *
//* (IKJEFTE8)                                                        *
//*********************************************************************
//* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//UPDATE01 EXEC PGM=IEBUPDTE,PARM=NEW
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DISP=SHR,DSN=SYS1.UMODSRC
//SYSIN    DD  *
./ ADD NAME=IKJEFTE8
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
* SOURCE -- MVS 3.8 OPTIONAL MATERIALS; SYM1-3                        *
*                                                                     *
* STATUS --                                                           *
*                                                                     *
*    CHANGE LEVEL  040  000                                           *
*                                                                     *
* FUNCTION --                                                         *
*                                                                     *
*    THIS MODULE COMPRISES THE CSECT FOR THE PROGRAM NAMES WHICH ARE  *
*    TO BE ATTACHED BY TMP-CALL AS ELIGIBLE FOR AUTHORIZATION         *
*                                                                     *
* ENTRY POINTS --                                                     *
*                                                                     *
*    NONE                                                             *
*                                                                     *
* INPUT --                                                            *
*                                                                     *
*    NONE                                                             *
*                                                                     *
* OUTPUT --                                                           *
*                                                                     *
*    NONE                                                             *
*                                                                     *
* EXTERNAL REFERENCES --                                              *
*                                                                     *
*     NONE                                                            *
*                                                                     *
* EXITS, NORMAL --                                                    *
*                                                                     *
*    NONE                                                             *
*                                                                     *
* EXITS, ERROR --                                                     *
*                                                                     *
*    NONE                                                             *
*                                                                     *
* TABLES/WORK AREA --                                                 *
*                                                                     *
*    NONE                                                             *
*                                                                     *
* ATTRIBUTE --                                                        *
*                                                                     *
*    REFRESHABLE                                                      *
*                                                                     *
* NOTES --                                                            *
*                                                                     *
*    THIS MODULE IS CHARACTER CODE DEPENDENT UPON AN INTERNAL REPRE-  *
*    SENTATION OF THE EXTERNAL CHARACTER SET EQUIVALENT TO THE ONE IN *
*    USE AT ASSEMBLY TIME                                             *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         EJECT
* /* START OF SPECIFICATIONS ****
*
*1MODULE-NAME = IKJEFTE8
*
*1DESCRIPTIVE-NAME = APF-ELIGIBLE-PROGRAM-NAME TABLE
*
*1DESCRIPTION = CONTAINS THE PROGRAM-NAMES WHICH ARE TO BE ATTACHED
*           BY TMP-CALL AS ELIGIBLE FOR AUTHORIZATION
*
*1MODULE TYPE = TABLE
*  2FIELD = BLANKS
*    3TYPE = CHARACTER STRING
*  2FIELD = BLANKS
*    3TYPE = CHARACTER STRING
*  2FIELD = BLANKS
*    3TYPE = CHARACTER STRING
*  2FIELD = BLANKS
*    3TYPE = CHARACTER STRING
*  2FIELD = BLANKS
*    3TYPE = CHARACTER STRING
*  2FIELD = BLANKS
*    3TYPE = CHARACTER STRING
*  2FIELD = BLANKS
*    3TYPE = CHARACTER STRING
*  2FIELD = BLANKS
*    3TYPE = CHARACTER STRING
*  2FIELD = BLANKS
*    3TYPE = CHARACTER STRING
*  2FIELD = BLANKS
*    3TYPE = CHARACTER STRING
*
**** END OF SPECIFICATIONS ***/
         EJECT
         SPACE 3
         ENTRY APFPTABL
         SPACE 1
IKJEFTE8 CSECT
         DC    C'IKJEFTE8'             MODULE NAME
         DC    C'&SYSDATE'             RELEASE LEVEL
APFPTABL DS    0D                      ALIGNMENT
         SPACE 2
         DC    C'IEBCOPY '
         DC    C'IKJEFF76'
         DC    C'ISPF    '
         DC    C'ISPLINK '
         DC    C'ISPMAIN '
         DC    C'RECV370 '
         DC    C'XMIT370 '
         DC    C'        '             TERMINATOR
         END
./ ENDUP
/*
//*
//SMPASM02 EXEC SMPASML,M=IKJEFTE8,COND=(0,NE)
//*
//RECV03   EXEC SMPAPP,COND=(0,NE),WORK=SYSALLDA
//SMPPTFIN DD  *
++USERMOD(JLM0004)
  .
++VER(Z038)
  FMID(EBB1102)
  PRE(UZ78841)
  .
++MOD(IKJEFTE8)
  DISTLIB(AOST4)
  LKLIB(UMODLIB)
  .
/*
//SMPCNTL  DD  *
 RECEIVE
         SELECT(JLM0004)
         .
  APPLY
        SELECT(JLM0004)
        DIS(WRITE)
        .
//
//SLB0002  JOB (SYSGEN),'J06 M06: SLB0002',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             REGION=4096K
/*JOBPARM LINES=100
//JOBCAT   DD  DSN=SYS1.VSAM.MASTER.CATALOG,DISP=SHR
//*
//*********************************************************************
//* Install USERMOD SLB0002 (source: Shelby Beach) allows use of      *
//* 'M' as the megabyte indicator for REGION in JCL                   *
//*********************************************************************
//*
//RECEIVE  EXEC SMPREC,WORK='SYSALLDA'
//SMPPTFIN DD  *
++USERMOD(SLB0002)       /* CONVERTER/INTERPRETER */ .
++VER(Z038) FMID(EBB1102) PRE(UZ36835,UZ39248)
 /*
   PROBLEM DESCRIPTION:
     THE JOB AND EXEC REGION PARAMETERS ONLY ALLOW REGION SIZE
       TO BE SPECIFIED AS KILOBYTES(K). THIS CHANGE, ALLOWS
       THOSE OF US USED TO CODING AN 'M' FOR THE REGION SIZE
       VALUE TO CONTINUE TO DO SO.

   SPECIAL CONDITIONS:
     ACTION:
       A "CLPA" MUST BE PERFORMED AT IPL TIME FOR THIS SYSMOD TO
       BECOME ACTIVE.

   COMMENTS:
     THIS USERMOD WAS WRITTEN AND PUBLISHED BY SHELBY BEACH.

     THE FOLLOWING MODULES AND/OR MACROS ARE AFFECTED BY THIS USERMOD:
     MODULES:
       IEFVEA, IEFVJA
 */.
++ZAP(IEFVEA) DISTLIB(AOSB3).
 NAME IEFVEA
 IDRDATA SLB0002
*
*  VALIDATE EXEC REGION VALUE
*
 VER 0AEE 4770B56E           BNE   EAPGM10         NO. -FORMAT ERROR
 REP 0AEE 477084C6           BNE   PATCH1          NO CHECK FOR 'M'
 VER 14C8 00000000           PATCH AREA 1
 REP 14C8 95D46000           CLI   0(R6),C'M'      NOT 'K' & NOT 'M',
 REP 14CC 4770B56E           BNE   EAPGM10         SO IT'S AN ERROR
 REP 14D0 47F0BAF0           B     BACK            BACK TO MAIN
*
*  ADJUST STORAGE IF MB
*
 VER 0AFC 9180A0B4           TM    EATEMP2,EAREGION FIRST PASS ?
 REP 0AFC 47F084D2           B PATCH2              GO TO PATCH AREA
 VER 14D4 00000000           PATCH AREA 2
 REP 14D4 95D26000           CLI   0(R6),C'K'      REGION IN KB ?
 REP 14D8 478084E6           BE    *+16            YES, SKIP MB CONV
 REP 14DC 4800A0B0           LH    R0,EATEMP1      R0 =3D MEGABYTES
 REP 14E0 8900000A           SLL   R0,10           CONVERT TO KB
 REP 14E4 4000A0B0           STH   R0,EATEMP1      AND SAVE IT
 REP 14E8 9180A0B4           TM    EATEMP2,EAREGION FIRST PASS ?
 REP 14EC 47F0BAFE           B     BACK            BACK TO MAIN
++ZAP(IEFVJA) DISTLIB(AOSB3).
 NAME IEFVJA
 IDRDATA SLB0002
*
*  VALIDATE JOB REGION VALUE
*
 VER 0310 4770B4FE           BNE   JAER1           NOT 'K' - ERROR
 REP 0310 4770B67E           BNE   PATCH1          NO CHECK FOR 'M'
 VER 0680 00000000           PATCH AREA 1
 REP 0680 95D46000           CLI   0(R6),C'M'      NOT 'K' & NOT 'M',
 REP 0684 4770B4FE           BNE   JAER1           SO IT'S AN ERROR
 REP 0688 47F0B312           B     BACK            BACK TO MAIN
*
*  ADJUST NORMAL STORAGE IF MB
*
 VER 02C6 9640C22A           OI    SWD,JOBREGNS    SET SW - JOB REGION
 REP 02C6 47F0B68A           B PATCH2              GO TO PATCH AREA
 VER 068C 00000000           PATCH AREA 2
 REP 068C 9640C22A           OI    SWD,JOBREGNS    SET SW - JOB REGION
 REP 0690 95D26000           CLI   0(R6),C'K'      REGION IN KB ?
 REP 0694 4780B2C8           BE    BACK            YES, BACK TO MAIN
 REP 0698 4808005C           LH    R0,JAREGOFF(R8) R0 =3D MEGABYTES
 REP 069C 8900000A           SLL   R0,10           CONVERT TO KB
 REP 06A0 4008005C           STH   R0,JAREGOFF(R8) SAVE REQUESTED VALUE
 REP 06A4 47F0B2C8           B     BACK            BACK TO MAIN
*
*  ADJUST BULK STORAGE IF MB
*
 VER 02F2 4A48005C           AH    R4,JAREGOFF(R8) ADD H0 VALUE TO H1
 REP 02F2 47F0B6A6           B     PATCH3          GO TO PATCH AREA
 VER 06A8 00000000           PATCH AREA 3
 REP 06A8 95D26000           CLI   0(R6),C'K'      REGION IN KB ?
 REP 06AC 4780B6B2           BE    *+8             YES
 REP 06B0 8940000A           SLL   R4,10           CONVERT MB TO KB
 REP 06B4 4A48005C           AH    R4,JAREGOFF(R8) ADD H0 VALUE TO H1
 REP 06B8 47F0B2F4           B     BACK            BACK TO MAIN
/*
//*
//SMPCNTL  DD  *
  RECEIVE
          SELECT(SLB0002)
          .
/*
//*
//APPLYCK  EXEC SMPAPP,WORK='SYSALLDA'
//SMPCNTL  DD *
  APPLY
        SELECT(SLB0002)
        CHECK
        .
/*
//*
//APPLY    EXEC SMPAPP,COND=(0,NE),WORK='SYSALLDA'
//SMPCNTL  DD *
  APPLY
        SELECT(SLB0002)
        DIS(WRITE)
        .
/*
//
//SYZM001  JOB (SYSGEN),'J07 M07: SYZM001',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             REGION=4096K
/*JOBPARM LINES=100
//JOBCAT   DD  DSN=SYS1.VSAM.MASTER.CATALOG,DISP=SHR
//*
//*********************************************************************
//* Install USERMOD SYZM001 (source: Brian Westerman) suppress CN(00) *
//* being appended to SEND operator command                           *
//*********************************************************************
//*
//RECEIVE  EXEC SMPREC,WORK='SYSALLDA'
//SMPPTFIN  DD *
++USERMOD (SYZM001) /* SEND COMMAND IMPROVEMENT */.
++VER (Z038) FMID(EBB1102) /*
  THIS USER MOD FIXES THE SEND OPERATOR COMMAND TO NOT APPEND
  CN(00) TO THE MESSAGE WHEN SEND IS ISSUED BY A SYSTEM TASK.
  THIS IMPROVES THE APPEARANCE OF JES2 NOTIFY MESSAGES. */.
++ZAP (IEEVSND6).
 NAME IEEVSND6
 VER 0078 4350,A01D
 VER 058C 0000,0000
 REP 0078 47F0,B586
 REP 058C BF51,A01D,4770,B076
 REP 0594 D205,C16D,C16C,47F0,B098
/*
//*
//SMPCNTL  DD  *
  RECEIVE
          SELECT(SYZM001)
          .
/*
//*
//APPLYCK  EXEC SMPAPP,WORK='SYSALLDA'
//SMPCNTL  DD *
  APPLY
        SELECT(SYZM001)
        CHECK
        .
/*
//*
//APPLY    EXEC SMPAPP,COND=(0,NE),WORK='SYSALLDA'
//SMPCNTL  DD *
  APPLY
        SELECT(SYZM001)
        DIS(WRITE)
        .
/*
//
//TIST801  JOB (SYSGEN),'J08 M08: TIST801',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             REGION=4096K
/*JOBPARM LINES=100
//JOBCAT   DD  DSN=SYS1.VSAM.MASTER.CATALOG,DISP=SHR
//*
//* 2002/04/12 @KL TIST801 SKIP READING VTAMOBJ
//*
//RECEIVE  EXEC SMPREC,WORK='SYSALLDA'
//SMPPTFIN   DD DATA,DLM='??'
++ USERMOD(TIST801) /* REWORK(20020624) */                 .
++ VER(Z038)
   FMID(EVT0108)
 /*
  PROBLEM DESCRIPTION(S):
    TIST801
      Bypass reading VTAMOBJ and always fetch resource
        definition from VTAMLST.

  COMPONENT:  5752-SC123-EVT0108

  SPECIAL CONDITIONS:
    ACTION:  VTAM must be restarted after this user modification
      is installed.

  COMMENTS:
    LAST CHANGE:  2002/06/24

     The following modules are affected by this usermod:
       ISTSDCRC
 */.
++ ZAP      (ISTSDCRC) DISTLIB(AOS26   ).
NAME ISTSDCRC
VER 0282 58F0,C6A0    L     @15,@CV03189     Point to VTAMOBJ read rtn
VER 0286 05EF         BALR  @14,@15          Go to it
REP 0282 41F0,0004    LA    @15,4            Simulate "VTAMOBJ member
REP 0286 0700         NOPR  0                 not found" error
IDRDATA TIST801
??
/*
//SMPCNTL  DD  *
  RECEIVE
          SELECT(TIST801)
          .
/*
//*
//APPLYCK  EXEC SMPAPP,WORK='SYSALLDA'
//SMPCNTL  DD  *
  APPLY
        SELECT(TIST801)
        BYPASS(ID)
        CHECK
        .
/*
//*
//APPLY    EXEC SMPAPP,COND=(0,NE),WORK='SYSALLDA'
//SMPCNTL  DD  *
  APPLY
        SELECT(TIST801)
        DIS(WRITE)
        .
/*
//
//TJES801  JOB (SYSGEN),'J09 M09: TJES801',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             REGION=4096K
/*JOBPARM LINES=100
//JOBCAT   DD  DSN=SYS1.VSAM.MASTER.CATALOG,DISP=SHR
//*
//* 2005/08/11 @kl TJES801 set SSCTSSID and SSIBSSID to X'02' for JES2
//*
//RECEIVE  EXEC SMPREC,WORK='SYSALLDA'
//SMPPTFIN   DD DATA,DLM='??'
++ USERMOD(TJES801)     /* REWORK(20050811) */             .
++ VER (Z038)
   FMID(EJE1103)
   PRE  (UZ31176,UZ33158,UZ35334,UZ37263,UZ52543,UZ54837,
         UZ57911,UZ60375,UZ63374,UZ65742,UZ68537,UZ71437,
         UZ76165,UZ77164,UY02859)

 /*
   PROBLEM DESCRIPTION(S):
     TJES801 -
       (1) Set SSCTSSID to SSCTJES2 (X'02') during JES2 initialization
           to indicate that the SSCVT is for a JES2 subsystem.
       (2) Set SSIBSSID to SSIBJES2 (X'02') when a job is selected
           to indicate that the selecting subsystem is JES2.

   COMPONENT:  5752-SC1BH-EJE1103

   APARS FIXED: TJES801

   SPECIAL CONDITIONS:
     ACTION:  An IPL with CLPA is required after installation of
       this user modification.

   COMMENTS:
     LAST CHANGE:  2005/08/11

     THE FOLLOWING MODULES AND/OR MACROS ARE AFFECTED BY THIS USERMOD:

     MACROS
       HASPINIT
       HASPSSSM
 */.
++ SRCUPD   (HASPINIT) DISTLIB(HASPSRC ).
./ CHANGE NAME=HASPINIT
         MVI   SSCTSSID-SSCT(WA),SSCTJES2  SHOW JES2 SSCVT     @TJES801 M4892700
++ SRCUPD   (HASPSSSM) DISTLIB(HASPSRC ).
./ CHANGE NAME=HASPSSSM
         MVI   SSIBSSID,SSIBJES2   SET SUBSYSTEM ID TO JES2    @TJES801 T1802050
         MVI   SSIBSSID,SSIBJES2   SET SUBSYSTEM ID TO JES2    @TJES801 T1834700
??
/*
//SMPCNTL  DD  *
  RECEIVE
          SELECT(TJES801)
          .
/*
//*
//APPLYCK  EXEC SMPAPP,WORK='SYSALLDA'
//SMPCNTL  DD  *
  APPLY
        SELECT(TJES801)
        CHECK
        BYPASS(ID)
        .
/*
//*
//APPLY    EXEC SMPAPP,COND=(0,NE),WORK='SYSALLDA'
//SYSUT1   DD  SPACE=(1700,(3000,100))
//SYSUT2   DD  SPACE=(1700,(3000,100))
//SYSUT3   DD  SPACE=(1700,(3000,100))
//SMPWRK3  DD  SPACE=(CYL,(30,10,84))
//SMPCNTL  DD  *
  APPLY
        SELECT(TJES801)
        DIS(WRITE)
        COMPRESS(ALL)
        .
/*
//
