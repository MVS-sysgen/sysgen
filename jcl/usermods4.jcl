//*
//*        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//*        !!                                     !!
//*        !! DO NOT RENUMBER THIS JOBSTREAM FILE !!
//*        !!                                     !!
//*        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//*
//*
//ZP60015  JOB (SYSGEN),'J01 M29: ZP60015',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             REGION=4096K
/*JOBPARM LINES=100
//JOBCAT   DD  DSN=SYS1.VSAM.MASTER.CATALOG,DISP=SHR
//*
//*  EXTEND THE JOB SEARCH BY JES2 FOR TSO STATUS WITHOUT OPERANDS.
//*
//RECEIVE EXEC SMPREC,WORK='SYSALLDA'
//SMPPTFIN DD  *
++USERMOD(ZP60015)          /* EXTEND JES2 TSO STATUS SEARCH */  .
++VER(Z038) FMID(EJE1103)
  PRE(UZ31176,UZ33158,UZ35334,UZ37263,UZ52543,UZ54837,UZ57911,
      UZ63374,UZ65742,UZ68537,UZ71437,UZ76165)
 /*
   PROBLEM DESCRIPTION:
     TSO STATUS ONLY LOOKS FOR JOB NAMES OF USERID PLUS ONE CHARACTER.
       WHEN THE TSO STATUS COMMAND IS ISSUED WITHOUT ANY OPERAND
       THE SYSTEM LOOKS FOR ALL JOBS WITH NAMES BEGINNING WITH THE
       USERID PLUS ONE CHARACTER.  IF THE USERID IS SHORTER THAN
       SEVEN CHARACTERS THEN OTHER JOBS WITH NAMES BEGINNING WITH
       THE USERID BUT HAVING MORE THAN ONE EXTRA CHARACTER ARE NOT
       REPORTED BY THE STATUS COMMAND.

       THIS USERMOD ALTERS JES2 SO THAT ANY JOB WITH A NAME WHICH
       STARTS WITH THE REQUESTING USERID IS REPORTED.

   SPECIAL CONDITIONS:
     ACTION:
       JES2 MUST BE RESTARTED FOR THIS ZAP TO BECOME ACTIVE.
       A HOT START IS SUFFICIENT.

   COMMENTS:
     PRYCROFT SIX P/L PUBLIC DOMAIN USERMOD FOR MVS 3.8 NO. 15.

     USERMODS ZP60015 AND ZP60016 ARE INTENDED TO BE OPERATIONAL
     CONCURRENTLY.  NOTE THE FOLLOWING BEHAVIOUR TABLE:

       WHICH USERMODS ACTIVE      I  STATUS DEFAULT SEARCH
       ==========================================================
       NEITHER 15 NOR 16 APPLIED  I  FIND USERID+1 ONLY
       ----------------------------------------------------------
       15 APPLIED BUT NOT 16      I  FIND USERID+0,1,2,3 BUT
                                  I  NAME REPORTED AS USERID+1
       ----------------------------------------------------------
       16 APPLIED BUT NOT 15      I  FIND USERID+1 ONLY BUT
                                  I  MESSAGE HAS NULLS AFTER NAME
       ----------------------------------------------------------
       BOTH 15 AND 16 APPLIED     I  FIND USERID+0,1,2,3
       ----------------------------------------------------------

     THE FOLLOWING MODULES AND/OR MACROS ARE AFFECTED BY THIS USERMOD:
     MODULES:
       HASPXEQ
 */.
++ SRCUPD   (HASPXEQ)  DISTLIB(HASPSRC ).
./ CHANGE NAME=HASPXEQ
         CLM   WD,1,=AL1(L'JQEJNAME-2) TEST FOR FULL NAME SCAN  ZP60015 U5596000
         SLR   R15,R15             CLEAR FOR INSERT             ZP60015 U5599000
         IC    R15,SJBTULEN        GET USER NAME LENGTH         ZP60015 U5599500
         LA    R15,JQEJNAME(R15)   POINT TO LAST CHARACTER + 1  ZP60015 U5600000
         CLM   WD,1,=AL1(L'JQEJNAME-5)  NEED TRAILER CHECK?     ZP60015 U5602000
         BH    XTJSCNA             NO, NAME LONG ENOUGH TO NOT  ZP60015 U5603000
         CLI   3(R15),C' '         INSURE NAME NOT >3 LONGER    ZP60015 U5604000
         NOP   XTJSCN     (BE)     LOOP IF NOT (TSO ID OKAY)    ZP60015 U5612000
         ICM   WD,14,0(R15)        PICK UP CHARACTER(S)         ZP60015 U5614000
         STCM  WD,14,SSCSUJOB      SET LAST CHARS IF PRESENT    ZP60015 U5684000
/*
//SMPCNTL  DD  *
  RECEIVE
          SELECT(ZP60015)
          .
/*
//*
//APPLYCK EXEC SMPAPP,WORK='SYSALLDA'
//SYSUT1   DD  SPACE=(1700,(1200,100))
//SYSUT2   DD  SPACE=(1700,(1200,100))
//SYSUT3   DD  SPACE=(1700,(1200,100))
//SMPWRK3  DD  UNIT=&WORK,SPACE=(CYL,(80,20,84)),DCB=(BLKSIZE=3120,
//             LRECL=80)
//SMPCNTL  DD  *
  APPLY
        SELECT(ZP60015)
        CHECK
        .
/*
//*
//APPLY   EXEC SMPAPP,COND=(0,NE),WORK='SYSALLDA'
//SYSUT1   DD  SPACE=(1700,(1200,100))
//SYSUT2   DD  SPACE=(1700,(1200,100))
//SYSUT3   DD  SPACE=(1700,(1200,100))
//SMPWRK3  DD  UNIT=&WORK,SPACE=(CYL,(80,20,84)),DCB=(BLKSIZE=3120,
//             LRECL=80)
//SMPCNTL  DD  *
  APPLY
        SELECT(ZP60015)
        .
/*
//
//ZP60016  JOB (SYSGEN),'J02 M30: ZP60016',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             REGION=4096K
/*JOBPARM LINES=100
//JOBCAT   DD  DSN=SYS1.VSAM.MASTER.CATALOG,DISP=SHR
//*
//*  REPORT EXTENDED RESULTS FROM JES2 TSO STATUS DEFAULT SEARCH.
//*
//RECEIVE EXEC SMPREC,WORK='SYSALLDA'
//SMPPTFIN DD  *
++USERMOD(ZP60016)     /* REPORT JES2 STATUS SEARCH RESULTS */  .
++VER(Z038) FMID(EBB1102) PRE(UZ48744)
 /*
   PROBLEM DESCRIPTION:
     TSO STATUS ONLY LOOKS FOR JOB NAMES OF USERID PLUS ONE CHARACTER.
       WHEN THE TSO STATUS COMMAND IS ISSUED WITHOUT ANY OPERAND
       THE SYSTEM LOOKS FOR ALL JOBS WITH NAMES BEGINNING WITH THE
       USERID PLUS ONE CHARACTER.  IF THE USERID IS SHORTER THAN
       SEVEN CHARACTERS THEN OTHER JOBS WITH NAMES BEGINNING WITH
       THE USERID BUT HAVING MORE THAN ONE EXTRA CHARACTER ARE NOT
       REPORTED BY THE STATUS COMMAND.

       THIS USERMOD ALTERS THE STATUS COMMAND TO REPORT UP TO
       THREE EXTRA CHARACTERS AFTER THE USERID IN THE JOB NAME.
       THESE JOBS WILL HAVE BEEN RETURNED BY JES2 PROCESSING AFTER
       THE ACTIVATION OF ZP60015.  WITHOUT THIS SYSMOD SUCH JOBS
       WILL BE REPORTED WITHOUT THE UP TO TWO EXTRA CHARACTERS
       IN THE JOB NAME.

   SPECIAL CONDITIONS:
     NONE.

   COMMENTS:
     PRYCROFT SIX P/L PUBLIC DOMAIN USERMOD FOR MVS 3.8 NO. 16.

     USERMODS ZP60015 AND ZP60016 ARE INTENDED TO BE OPERATIONAL
     CONCURRENTLY.  NOTE THE FOLLOWING BEHAVIOUR TABLE:

       WHICH USERMODS ACTIVE      I  STATUS DEFAULT SEARCH
       ==========================================================
       NEITHER 15 NOR 16 APPLIED  I  FIND USERID+1 ONLY
       ----------------------------------------------------------
       15 APPLIED BUT NOT 16      I  FIND USERID+0,1,2,3 BUT
                                  I  NAME REPORTED AS USERID+1
       ----------------------------------------------------------
       16 APPLIED BUT NOT 15      I  FIND USERID+1 ONLY BUT
                                  I  MESSAGE HAS NULLS AFTER NAME
       ----------------------------------------------------------
       BOTH 15 AND 16 APPLIED     I  FIND USERID+0,1,2,3
       ----------------------------------------------------------

     THE FOLLOWING MODULES AND/OR MACROS ARE AFFECTED BY THIS USERMOD:
     MODULES:
       IKJEFF52
 */.
++ZAP(IKJEFF52) DISTLIB(AOST4).
 NAME IKJEFF52
 IDRDATA ZP60016
VER 0348 9180,3002             TM    SSCSFLGS,SSCSUSID
VER 034C 47E0,C344             BO    JBNAMEOK
VER 0350 1F22                  SLR   R2,R2
VER 0352 4320,3003             IC    R2,SSCSULEN
VER 0356 4132,B320             LA    R3,JOBNINST(R2)
VER 035A D200,3000,4021        MVC   0(1,R3),SSCSUJOB
VER 068A E9C1,D7E9,C1D7        DC    C'ZAPZAP'
REP 0356 41E2,B320             LA    R14,JOBNINST(R2)
REP 035A 47F0,C66E,0700        B     PATCH
REP 068A D200,E000,4021  PATCH MVC   0(1,R14),SSCSUJOB
REP 0690 9506,3003             CLI   SSCSULEN,6
REP 0694 4720,C344             BH    JBNAMEOK
REP 0698 D200,E001,4022        MVC   1(1,R14),SSCSUJOB+1
REP 069E 4780,C344             BE    JBNAMEOK
REP 06A2 D200,E002,4023        MVC   2(1,R14),SSCSUJOB+2
REP 06A8 47F0,C344             B     JBNAMEOK
/*
//SMPCNTL  DD  *
  RECEIVE
          SELECT(ZP60016)
          .
/*
//*
//APPLYCK EXEC SMPAPP,WORK='SYSALLDA'
//SMPCNTL  DD  *
  APPLY
        SELECT(ZP60016)
        CHECK
        .
/*
//*
//APPLY   EXEC SMPAPP,COND=(0,NE),WORK='SYSALLDA'
//SMPCNTL  DD  *
  APPLY
        SELECT(ZP60016)
        DIS(WRITE)
        .
/*
//
//ZP60017  JOB (SYSGEN),'J03 M31: ZP60017',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             REGION=4096K
/*JOBPARM LINES=100
//JOBCAT   DD  DSN=SYS1.VSAM.MASTER.CATALOG,DISP=SHR
//*
//*  ZAP TO MOVE THE MASTER TRACE TABLE TO CSA.
//*
//RECEIVE EXEC SMPREC,WORK='SYSALLDA'
//SMPPTFIN DD  *
++USERMOD(ZP60017)       /* MASTER TRACE TABLE SUBPOOL */  .
++VER(Z038) FMID(FBB1221)
 /*
   PROBLEM DESCRIPTION:
     IT IS DIFFICULT TO ACCESS THE MASTER TRACE TABLE CONTENTS.
       THE MASTER TRACE TABLE IS A WRAP-AROUND BUFFER CONTAINING
       THE MOST RECENT WTO MESSAGES PROCESSED BY COMMTASK, EVEN
       INCLUDING MESSAGES WHICH WERE NOT LOGGED OR DISPLAYED
       ANYWHERE ELSE.  BEING ABLE TO BROWSE THE MASTER TRACE TABLE
       CONTENTS WITHOUT TAKING A DUMP INCLUDING ASID 1 AND WITHOUT
       THE NEED FOR ASYNCHRONOUS CROSS-MEMORY COMMUNICATION IS A
       USEFUL DIAGNOSIS TOOL AND OPERATIONAL AID.

       THIS USERMOD UPDATES THE NIP INITIALIZATION ROUTINE WHICH
       SETS THE MASTER TRACE TABLE SUBPOOL VALUE INTO THE MASTER
       SCHEDULER RESIDENT DATA AREA.  THE ORIGINAL SUBPOOL OF
       229 IS CHANGED TO 231 WHICH IS A CSA SUBPOOL WITH SIMILAR
       ATTRIBUTES.

   SPECIAL CONDITIONS:
     ACTION:
       AN IPL MUST BE PERFORMED FOR THIS SYSMOD TO BECOME ACTIVE.

   COMMENTS:
     PRYCROFT SIX P/L PUBLIC DOMAIN USERMOD FOR MVS 3.8 NO. 17.

     THE FOLLOWING MODULES AND/OR MACROS ARE AFFECTED BY THIS USERMOD:
     MODULES:
       IEAVNP13
 */.
++ZAP(IEAVNP13) DISTLIB(AOSC5).
 NAME IEAVNP13
 IDRDATA ZP60017
VER 0076 92E5,A028             MVI   BAMTINSP,229
REP 0076 92E7,A028             MVI   BAMTINSP,231
/*
//SMPCNTL  DD  *
  RECEIVE
          SELECT(ZP60017)
          .
/*
//*
//APPLYCK  EXEC SMPAPP,WORK='SYSALLDA'
//SMPCNTL  DD  *
  APPLY
        SELECT(ZP60017)
        CHECK
        .
/*
//*
//APPLY   EXEC SMPAPP,COND=(0,NE),WORK='SYSALLDA'
//SMPCNTL  DD  *
  APPLY
        SELECT(ZP60017)
        DIS(WRITE)
        .
/*
//
//ZP60018  JOB (SYSGEN),'J04 M32: ZP60018',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             REGION=4096K
/*JOBPARM LINES=100
//JOBCAT   DD  DSN=SYS1.VSAM.MASTER.CATALOG,DISP=SHR
//*
//*  REPORT MODULE NAME/ENTRY POINT IF PSW ADDRESS IN PLPA.
//*
//STEP1   EXEC PGM=IEBGENER
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  *
++USERMOD(ZP60018)           /* REPORT SOME PLPA MODULE NAMES */  .
++VER(Z038) FMID(EBB1102)
 /*
   PROBLEM DESCRIPTION:
     SYSTEM TRACE DOES NOT CAPTURE THE MODULE NAME OF THE EVENT.
       PROGRAM INTERRUPTS, INCLUDING THOSE CAUSED BY MONITOR CALL
       (MC) INSTRUCTIONS HAVE THE PSW CONTENTS REPORTED BUT FURTHER
       DATA ANALYSIS IS NEEDED TO OBTAIN THE MODULE NAME WHERE THE
       EVENT TOOK PLACE.

       THIS USERMOD OVERLAYS THE R0 CONTENTS IN THE SYSTEM TRACE
       DATA FORMATTED IN A DUMP IN CASES WHERE
         (1) THE TRACE EVENT IS A PROGRAM INTERRUPT, AND
         (2) THE PSW ADDRESS CORRESPONDS TO A PLPA MODULE.

       THE 'R0' LABEL IS OVERLAID WITH 'EP' WHEN THE ADDRESS
       CORRESPONDS TO THE DISPLAYED ENTRY POINT, AND 'R0' IS
       OVERLAID WITH 'NM' WHEN THE ADDRESS HAS ONLY BEEN RESOLVED
       TO RESIDE IN THE EXTENT OF THE NAMED MODULE.

       SVC INTERRUPT TRACE ENTRIES ARE NOW FORMATTED TO SHOW THE
       DECIMAL SVC NUMBER.

       PER PROGRAM CHECKS ARE FLAGGED WITH THE LITERAL 'PER'.  MC
       INSTRUCTION PROGRAM CHECKS ARE FLAGGED WITH THE LITERAL 'MC'.
       OTHER PROGRAM CHECKS (EXCEPT PAGE FAULTS) ARE FLAGGED WITH
       THE LITERAL '**'.

   SPECIAL CONDITIONS:
     ACTION:
       A "CLPA" MUST BE PERFORMED AT IPL TIME FOR THIS SYSMOD TO
       BECOME ACTIVE.

   COMMENTS:
     PRYCROFT SIX P/L PUBLIC DOMAIN USERMOD FOR MVS 3.8 NUMBER 18.

     THE FOLLOWING MODULES AND/OR MACROS ARE AFFECTED BY THIS USERMOD:
     MODULES:
       IEAVAD0C
 */.
++MOD(IEAVAD0C) DISTLIB(AOSC5).
/*
//SYSUT2   DD  DSN=&&SMPMCS,DISP=(NEW,PASS),UNIT=SYSALLDA,
//             SPACE=(CYL,3),
//             DCB=(DSORG=PS,RECFM=FB,LRECL=80,BLKSIZE=4080)
//SYSIN    DD  DUMMY
//*
//STEP2   EXEC PGM=IFOX00,PARM='OBJECT,NODECK,NOTERM,XREF(SHORT)'
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  UNIT=SYSALLDA,SPACE=(CYL,10)
//SYSUT2   DD  UNIT=SYSALLDA,SPACE=(CYL,10)
//SYSUT3   DD  UNIT=SYSALLDA,SPACE=(CYL,10)
//SYSLIB   DD  DSN=SYS1.MACLIB,DISP=SHR
//         DD  DSN=SYS1.SMPMTS,DISP=SHR
//         DD  DSN=SYS1.AMODGEN,DISP=SHR
//SYSGO    DD  DSN=&&SMPMCS,DISP=(MOD,PASS)
//SYSIN    DD  *
         TITLE 'IEAVAD0C - SUPERVISOR TRACE                            *
                        '
IEAVAD0C CSECT ,                                                   0001
@MAINENT DS    0H                                                  0001
         USING *,@15                                               0001
         B     @PROLOG                                             0001
         DC    AL1(16)                                             0001
         DC    C'IEAVAD0C  77.049'                                 0001
         DC    C' ZP60018'
         DROP  @15
@PROLOG  ST    @14,12(,@13)                                        0001
         STM   @00,@12,20(@13)                                     0001
         BALR  @11,0                                               0001
@PSTART  DS    0H                                                  0001
         USING @PSTART,@11                                         0001
*                                                                  0059
*   /*****************************************************************/
*   /*                                                               */
*   /* INITIALIZATION ON ENTRY FIRST THE ADDRESSING REGISTERS ARE    */
*   /* INITIALIZED, THEN A SAVE AREA FOR THE REGISTER SAVE AREA IS   */
*   /* OBTAINED FROM LSQA. THE ADDRESS OF THE AREA IS RETURNED IN    */
*   /* ABDGMA, AND THE ADDRESS OF THE GETMAIN PARAMETER LIST IS      */
*   /* PASSED IN REGISTER 1. NEXT THE SAVE AREAS ARE CHAINED         */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0059
*   ABDARPTR=PARMPTR;               /* THE ABDAREA POINTER IS      0059
*                                      INITIALIZED FROM THE VALUE IN
*                                      REGISTER 1                    */
         LR    ABDARPTR,PARMPTR                                    0059
*   PARMPTR=ADDR(ABDASIZE);         /* REGISTER 1 IS SET TO POINT TO
*                                      THE GETMAIN PARAMETER LIST    */
         LA    PARMPTR,ABDASIZE(,ABDARPTR)                         0060
*   GENERATE;                                                      0061
         GETMAIN EC,LV=72,                                             *
               SP=253,                                                 *
               A=ABDGMA(ABDARPTR),                                     *
               MF=(E,(1))
*   IF RETCODE^=0                   /* IF GETMAIN IS UNSUCCESSFUL    */
*     THEN                          /* RETURN IMMEDIATELY.           */
         LTR   RETCODE,RETCODE                                     0062
         BZ    @RF00062                                            0062
*     DO;                                                          0063
*       RETCODE=8;                                                 0064
         LA    RETCODE,8                                           0064
*       RETURN;                     /* BACK TO CALLER                */
@EL00001 DS    0H                                                  0065
@EF00001 DS    0H                                                  0065
@ER00001 L     @14,12(,@13)                                        0065
         LM    @00,@12,20(@13)                                     0065
         BR    @14                                                 0065
*     END;                          /* OTHERWISE CONTINUE            */
*   PARMPTR=ABDAAREA;               /* USE PARMPTR AS TEMPORARY    0067
*                                      POINTER TO SAVE AREA          */
@RF00062 L     PARMPTR,ABDAAREA(,ABDARPTR)                         0067
*   NEXSAPTR=PARMPTR;               /* INITIALIZE NEXT SAVE AREA   0068
*                                      POINTER IN OLD SAVE AREA      */
         ST    PARMPTR,NEXSAPTR(,SAVEPTR)                          0068
*   PRESAPTR=SAVEPTR;               /* INITIALIZE PREVIOUS SAVE AREA
*                                      POINTER IN NEW AREA           */
         ST    SAVEPTR,PRESAPTR(,PARMPTR)                          0069
*   SAVEPTR=PARMPTR;                /* SET SAVE REGISTER TO NEW SAVE
*                                      AREA ADDRESS                  */
         LR    SAVEPTR,PARMPTR                                     0070
*                                                                  0071
*   /*****************************************************************/
*   /*                                                               */
*   /* THIS SECTION IS ENTERED TO PRODUCE THE SUPERVISOR TRACE, IF   */
*   /* GTF IS NOT IN USE. A CHECK IS MADE TO DETERMINE IF THE        */
*   /* MAINLINE OBTAINED A TRACE BUFFER. IF THERE WAS NO SPACE       */
*   /* AVAILABLE FOR THE TRACE BUFFER, NO TRACE WAS SAVED TO BE      */
*   /* PRINTED, SO A MESSAGE IS ISSUED AND THIS MODULE EXITS. IF     */
*   /* THERE IS A BUFFER, IT IS PRINTED. ALL OF THE ENTRIES ARE      */
*   /* FORMATTED AND PRINTED, BEGINNING WITH THE OLDEST ENTRY IN THE */
*   /* TABLE, THAT IS, THE ONE AFTER THE NEW CURRENT ENTRY. THE TRACE*/
*   /* ENDS AT THE OLD CURRENT TRACE ENTRY, THE ONE AT THE TIME THE  */
*   /* DUMP WAS TAKEN. THE ENTRIES BETWEEN THE OLD CURRENT AND NEW   */
*   /* CURRENT ENTRIES ARE CONSIDERED INVALID, THAT IS THEY OCCUR    */
*   /* DURING THE TIME THE TRACE TABLE WAS BEING MOVED. BEFORE EACH  */
*   /* ENTRY IS FORMATTED, IT IS CHECKED FOR THE BIT WHICH INDICATED */
*   /* THAT GTF HAD SUSPENDED THE TRACE. IF THE BIT IS ON IT MEANS   */
*   /* THERE WAS A TIME PERIOD BETWEEN THE PREVIOUS ENTRY AND THE    */
*   /* ENTRY BEING FORMATTED IN WHICH TRACE ENTRIES WERE BYPASSED BY */
*   /* GTF, ALTHOUGH THE NUMBER IS UNCERTAIN, AND MIGHT BE ZERO. A   */
*   /* MESSAGE IS PRINTED TO INDICATE THAT THERE INDEED MIGHT BE     */
*   /* TRACE ENTRIES WHICH MAY NOT EXIST DUE TO GTF SUSPENSION OF    */
*   /* TRACE.                                                        */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0071
*   PARMPTR=ABDARPTR;               /* RESTORE ADDR OF WORKAREA      */
         LR    PARMPTR,ABDARPTR                                    0071
*   IF ABDTRBIT='0'B                /* IF BUFFER WAS NOT OBTAINED    */
*     THEN                          /* PRINT MESSAGE AND LEAVE       */
         TM    ABDTRBIT(ABDARPTR),B'00100000'                      0072
         BNZ   @RF00072                                            0072
*     DO;                                                          0073
*       ABDLINEA(1:24)=NOBUFMSG;    /* SET UP MESSAGE                */
         MVC   ABDLINEA(24,ABDARPTR),NOBUFMSG                      0074
*       CALL IEAVAD11;              /* OUTPUT MSG                    */
         L     @15,@CV00118                                        0075
         BALR  @14,@15                                             0075
*     END;                                                         0076
*   ELSE                                                           0077
*     DO;                                                          0077
*                                                                  0077
         B     @RC00072                                            0077
@RF00072 DS    0H                                                  0078
*       /*************************************************************/
*       /*                                                           */
*       /* FORMAT AND OUTPUT UNFOLDED TRACE TABLE ENTRIES            */
*       /*                                                           */
*       /*************************************************************/
*                                                                  0078
*       TRACESRT=ABDFP;             /* START OF TRACE TABLE          */
         L     TRACESRT,ABDFP(,ABDARPTR)                           0078
*       TRACEND=ABDLP;              /* END OF TRACE TABLE            */
         L     TRACEND,ABDLP(,ABDARPTR)                            0079
*       OLDCUR=ABDCP1;              /* OLD POINTER TO CURRENT TRACE
*                                      TABLE ENTRY                   */
         L     OLDCUR,ABDCP1(,ABDARPTR)                            0080
*       NEWCUR=ABDCP;               /* POINTER TO CURRENT AFTER TABLE
*                                      WAS MOVED TO BUFFER           */
         L     NEWCUR,ABDCP(,ABDARPTR)                             0081
*       TOPRINT=TOPRINT+32;         /* START WITH ONE AFTER CURRENT  */
         LA    TOPRINT,32(,TOPRINT)                                0082
*       ABDLINEA(1:12)=TRACEMSG;    /* SET UP HEADING MESSAGE        */
         MVC   ABDLINEA(12,ABDARPTR),TRACEMSG                      0083
*       CALL IEAVAD11;              /* OUTPUT THE HEADER             */
         L     @15,@CV00118                                        0084
         BALR  @14,@15                                             0084
*       IF RETCODE^=0               /* IF ERRED                      */
*         THEN                      /* LEAVE WITH BAD CODE           */
         LTR   RETCODE,RETCODE                                     0085
         BNZ   @RT00085                                            0085
*         GOTO FREEBUF;             /* FREE BUFFER AND LEAVE         */
*       ABDCC='0';                  /* DOUBLE SPACE FIRST ENTRY      */
         MVI   ABDCC(ABDARPTR),C'0'                                0087
*CHKEND:                                                           0088
*       IF TOPRINT>TRACEND          /* IF ABOVE TABLE                */
*         THEN                      /* START FROM BOTTOM             */
CHKEND   CR    TOPRINT,TRACEND                                     0088
         BNH   @RF00088                                            0088
*         TOPRINT=TRACESRT;                                        0089
         LR    TOPRINT,TRACESRT                                    0089
*       IF TOPRINT=OLDCUR           /* IF THIS IS LAST ENTRY TO      */
*         THEN                      /* PRINT                         */
@RF00088 CR    TOPRINT,OLDCUR                                      0090
         BNE   @RF00090                                            0090
*         OLDCUR=0;                 /* SET AN INDICATOR FOR RETURN   */
         SLR   OLDCUR,OLDCUR                                       0091
*       IF SUSPEND='1'B             /* IF GTF SUSPENDED TRACE        */
*         THEN                      /* PRINT A MESSAGE               */
@RF00090 TM    SUSPEND(TOPRINT),B'10000000'                        0092
         BNO   @RF00092                                            0092
*         DO;                                                      0093
*           ABDLINEA(1:39)=SUSMSG;  /* SET UP MESSAGE                */
         MVC   ABDLINEA(39,ABDARPTR),SUSMSG                        0094
*           CALL IEAVAD11;          /* PRINT MESSAGE                 */
         L     @15,@CV00118                                        0095
         BALR  @14,@15                                             0095
*           IF RETCODE^=0           /* IF ERRED                      */
*             THEN                  /* LEAVE WITH BAD CODE           */
         LTR   RETCODE,RETCODE                                     0096
         BNZ   @RT00096                                            0096
*             GOTO FREEBUF;         /* FREE BUFFER AND LEAVE         */
*         END;                                                     0098
*                                                                  0099
*       /*************************************************************/
*       /*                                                           */
*       /* THE FOLLOWING CODE FORMATS EACH TRACE ENTRY INTO THE      */
*       /* OUTPUT LINE IN ABDAREA, IN A GENERAL FORMAT WHICH CAN BE  */
*       /* MODIFIED BY CHANGING LABELS TO PRODUCE THE EIGHT TYPES OF */
*       /* TRACE ENTRY DISPLAY.                                      */
*       /*                                                           */
*       /*************************************************************/
*                                                                  0099
*       ABDBPTR=TOPRINT;            /* SET UP FORMAT PARAMETERS      */
@RF00092 ST    TOPRINT,ABDBPTR(,ABDARPTR)                          0099
*       ABDLLINE=ADDR(TTLINE);      /* SET UP LAYOUT LINE            */
         LA    @14,TTLINE                                          0100
         ST    @14,ABDLLINE(,ABDARPTR)                             0100
*       CALL IEAVAD31;              /* FORMAT FIRST THIRD OF LINE    */
         L     @15,@CV00119                                        0101
         BALR  @14,@15                                             0101
*       IF RETCODE^=0               /* IF ERRED                      */
*         THEN                      /* LEAVE WITH BAD CODE           */
         LTR   RETCODE,RETCODE                                     0102
         BNZ   @RT00102                                            0102
*         GOTO FREEBUF;             /* FREE BUFFER AND LEAVE         */
*       CALL IEAVAD31;              /* FORMAT SECOND THIRD OF LINE   */
         L     @15,@CV00119                                        0104
         BALR  @14,@15                                             0104
*       IF RETCODE^=0               /* IF ERRED                      */
*         THEN                      /* LEAVE WITH BAD CODE           */
         LTR   RETCODE,RETCODE                                     0105
         BNZ   @RT00105                                            0105
*         GOTO FREEBUF;             /* FREE BUFFER AND LEAVE         */
*       CALL IEAVAD31;              /* FORMAT LAST THIRD OF LINE     */
         L     @15,@CV00119                                        0107
         BALR  @14,@15                                             0107
*       IF RETCODE^=0               /* IF ERRED                      */
*         THEN                      /* LEAVE WITH BAD CODE           */
         LTR   RETCODE,RETCODE                                     0108
         BNZ   @RT00108                                            0108
*         GOTO FREEBUF;             /* FREE BUFFER AND LEAVE         */
*                                                                  0110
*       /*************************************************************/
*       /*                                                           */
*       /* THE FOLLOWING CODE INSERTS THE PROPER LABELS TO CHANGE THE*/
*       /* GENERAL FORMATTED LINE INTO THE SPECIFIC TRACE TABLE      */
*       /* ENTRY. IT IS DEPENDANT ON THE BIT PATTERNS FOR EACH TYPE  */
*       /* OF ENTRY.                                                 */
*       /*                                                           */
*       /*************************************************************/
*                                                                  0110
*       IF ISDSP='111'B             /* IS DISPATHCHER ENTRY          */
*         THEN                      /* DO DISPATCHER LABELS          */
         TM    ISDSP(TOPRINT),B'01110000'                          0110
         BNO   @RF00110                                            0110
*         DO;                                                      0111
*           IDPTR=ADDR(DSPMSG);     /* SET UP DSP LABEL              */
         LA    IDPTR,DSPMSG                                        0112
*           ABDLINEA(7:9)=NEWMSG;   /* NEW PSW INSTEAD OF OLD        */
*        MVC   ABDLINEA+6(3,ABDARPTR),NEWMSG                       0113
         MVC   ABDLINEA+9(3,ABDARPTR),NEWMSG                    ZP60018
*           GOTO OUTPUTUM;          /* OUTPUT THE ENTRY              */
         B     OUTPUTUM                                            0114
*         END;                                                     0115
*       IF ISSSR='11'B              /* IF SUSPENDED SRB       @Y02705*/
*         THEN                      /* REDISPATCH ENTRY       @Y02705*/
@RF00110 TM    ISSSR(TOPRINT),B'01100000'                          0116
         BNO   @RF00116                                            0116
*         DO;                       /* SET UP SSR LABELS      @Y02705*/
*           IDPTR=ADDR(SSRMSG);     /* GET LABEL ADDRESS      @Y02705*/
         LA    IDPTR,SSRMSG                                        0118
*           ABDLINEA(38:43)=ASIDWD; /* SET UP ASID LBL        @Y02705*/
         MVC   ABDLINEA+37(6,ABDARPTR),ASIDWD                      0119
*           GOTO OUTPUTUM;          /* PRINT THE ENTRY        @Y02705*/
         B     OUTPUTUM                                            0120
*         END;                      /*                        @Y02705*/
*       IF ISIO='1'B&ISONE='1'B     /* IF IO ENTRY            @Y02705*/
*         THEN                      /* DO IO LABELS           @Y02705*/
@RF00116 TM    ISIO(TOPRINT),B'01010000'                           0122
         BNO   @RF00122                                            0122
*         DO;                       /*                        @Y02705*/
*           IDPTR=ADDR(IOMSG);      /* SET UP IO LABEL        @Y02705*/
         LA    IDPTR,IOMSG                                         0124
*           ABDLINEA(38:43)=SIOMSG2;/* MOVE IN CSW LBL        @Y02705*/
         MVC   ABDLINEA+37(6,ABDARPTR),SIOMSG2                     0125
*           ABDLINEA(65:67)=RESWD;  /* OVRLY R1 WITH RES      @Y02705*/
         MVC   ABDLINEA+64(3,ABDARPTR),RESWD                       0126
*           GOTO OUTPUTUM;          /* PRINT THE ENTRY        @Y02705*/
         B     OUTPUTUM                                            0127
*         END;                      /*                        @Y02705*/
*       IF ISISD='1'B               /* IF INITIAL SRB DISP    @Y02705*/
*         THEN                      /* ENTRY                  @Y02705*/
@RF00122 TM    ISISD(TOPRINT),B'01000000'                          0129
         BNO   @RF00129                                            0129
*         DO;                       /* SET UP ISD LABEL       @Y02705*/
*           IDPTR=ADDR(ISDMSG);     /* GET LABEL ADDRESS      @Y02705*/
         LA    IDPTR,ISDMSG                                        0131
*           ABDLINEA(38:43)=ASIDWD; /* GET ASID LBL          @YM06193*/
         MVC   ABDLINEA+37(6,ABDARPTR),ASIDWD                      0132
*           GOTO OUTPUTUM;          /* PRINT THE ENTRY        @Y02705*/
         B     OUTPUTUM                                            0133
*         END;                      /*                        @Y02705*/
*       IF ISPGM='1'B&ISONE='1'B    /* IF PGM ENTRY           @Y02705*/
*         THEN                      /* DO PGM LABELS                 */
@RF00129 TM    ISPGM(TOPRINT),B'00110000'                          0135
         BNO   @RF00135                                            0135
*         DO;                                                      0136
*   /*****************************************************************/
*   /* START OF MAIN CHANGE BLOCK 1 ADDED BY USERMOD ZP60018         */
*   /*****************************************************************/
         SR    @15,@15
         ICM   @15,7,5(TOPRINT)     GET TRACED PSW ADDRESS
         CLI   3(TOPRINT),X'10'     CURRENT INSTRUCTION?
         BL    @NXTINST             NO, NEXT INSTRUCTION
         CLI   3(TOPRINT),X'12'     CURRENT INSTRUCTION?
         BNH   @THSINST             YES
@NXTINST BCTR  @15,0
         BCTR  @15,0
         TM    4(TOPRINT),X'C0'     2-BYTE INSTRUCTION?
         BZ    @THSINST             YES
         BCTR  @15,0
         BCTR  @15,0
         TM    4(TOPRINT),X'C0'     4-BYTE INSTRUCTION?
         BNO   @THSINST             YES
         BCTR  @15,0                NO, MUST BE A 6-BYTE ONE
         BCTR  @15,0
@THSINST L     @14,CVTPTR           POINT TO THE CVT
         USING CVT,@14
         L     @14,CVTLPDIA         POINT TO PLPA DIRECTORY
         DROP  @14                  CVT
         LA    @14,0(,@14)          CLEAR FLAGS
         CR    @15,@14              ADDRESS IN PLPA?
         BL    @DONELPA             NO - THIS CATCHES -VE R15
         USING LPDE,@14
@CHKLPDE CLM   @15,7,LPDENTP+1      MATCHING ENTRY POINT?
         BE    @GOTLPDE             YES
         CLC   ABDLINEA+41(2,ABDARPTR),EPMSG
         BE    @NXTLPDE             DO NOT NEED AN EXTENT MATCH
         TM    LPDEATTR,LPDEMIN     JUST A MINOR LPDE?
         BO    @NXTLPDE             YES
         L     IDPTR,LPDEXTAD       POINT TO MODULE EXTENT
         CR    @15,IDPTR            ADDRESS BEFORE MODULE START?
         BL    @NXTLPDE             YES
         A     IDPTR,LPDEXTLN       POINT PAST MODULE EXTENT
         LA    IDPTR,0(,IDPTR)      CLEAR LAST EXTENT FLAG
         CR    @15,IDPTR            ADDRESS AFTER MODULE END?
         BNL   @NXTLPDE             YES
         MVC   ABDLINEA+53(8,ABDARPTR),LPDENAME
         MVC   ABDLINEA+41(2,ABDARPTR),NMMSG
         CLC   4(4,@15),B@PROLOG    ZAPPED MC INTO PROLOG?
         BNE   @NXTLPDE             NO
         MVC   ABDLINEA+56(5,ABDARPTR),8(@15)
         MVC   ABDLINEA+41(2,ABDARPTR),EPMSG
         B     @DONELPA             YES, COPY EYECATCHER
@NXTLPDE LA    @14,40(,@14)         POINT TO NEXT LPDE
         CLI   LPDENAME,X'FF'       END OF PLPA DIRECTORY?
         BE    @DONELPA             YES
         B     @CHKLPDE
@GOTLPDE MVC   ABDLINEA+53(8,ABDARPTR),LPDENAME
         MVC   ABDLINEA+41(2,ABDARPTR),EPMSG
         TM    LPDEATTR,LPDEMIN     JUST A MINOR LPDE?
         BO    @NXTLPDE             YES, SEE IF A MAJOR NAME MATCHES
         DROP  @14                  LPDE
@DONELPA EQU   *
         CLI   3(TOPRINT),X'11'     PAGE FAULT?
         BE    @DONEPGM             YES, PROBABLY NO PROBLEM
         MVC   ABDLINEA+5(2,ABDARPTR),STARMSG
         TM    3(TOPRINT),X'C0'     PER OR MC?
         BZ    @DONEPGM             NO, HIGHLIGHT WITH STARS
         MVC   ABDLINEA+5(2,ABDARPTR),MCMSG
         CLI   3(TOPRINT),X'40'     MONITOR CALL?
         BE    @DONEPGM             YES
         MVC   ABDLINEA+5(3,ABDARPTR),PERMSG
@DONEPGM EQU   *
*   /*****************************************************************/
*   /* END OF MAIN CHANGE BLOCK 1 ADDED BY USERMOD ZP60018           */
*   /*****************************************************************/
*           IDPTR=ADDR(PGMMSG);     /* SET UP PGM LABEL              */
         LA    IDPTR,PGMMSG                                        0137
*           GOTO OUTPUTUM;          /* PRINT THE ENTRY               */
         B     OUTPUTUM                                            0138
*         END;                                                     0139
*       IF ISSVC='1'B               /* IF SVC ENTRY                  */
*         THEN                      /* SET UP SVC LABELS             */
@RF00135 TM    ISSVC(TOPRINT),B'00100000'                          0140
         BNO   @RF00140                                            0140
*         DO;                                                      0141
*   /*****************************************************************/
*   /* START OF MAIN CHANGE BLOCK 2 ADDED BY USERMOD ZP60018         */
*   /*****************************************************************/
         SR    IDPTR,IDPTR
         IC    IDPTR,3(,TOPRINT)    GET INTERRUPT CODE
         CVD   IDPTR,24(,SAVEPTR)   SHOW SVC NUMBER IN DECIMAL
         MVC   ABDLINEA+4(4,ABDARPTR),ED3MSK
         ED    ABDLINEA+4(4,ABDARPTR),30(SAVEPTR)
         CLI   ABDLINEA+5(ABDARPTR),C' '
         BNE   @SVC#OK
         MVC   ABDLINEA+5(3,ABDARPTR),ABDLINEA+6(ABDARPTR)
         CLI   ABDLINEA+5(ABDARPTR),C' '
         BNE   @SVC#OK
         MVC   ABDLINEA+5(3,ABDARPTR),ABDLINEA+6(ABDARPTR)
@SVC#OK  EQU   *
*   /*****************************************************************/
*   /* END OF MAIN CHANGE BLOCK 2 ADDED BY USERMOD ZP60018           */
*   /*****************************************************************/
*           IDPTR=ADDR(SVCMSG);     /* SET UP SVC LABEL              */
         LA    IDPTR,SVCMSG                                        0142
*           GOTO OUTPUTUM;          /* PRINT THE ENTRY               */
         B     OUTPUTUM                                            0143
*         END;                                                     0144
*       IF ISEXT='1'B               /* IF EXTERNAL ENTRY             */
*         THEN                      /* SET EXTERNAL LABELS           */
@RF00140 TM    ISEXT(TOPRINT),B'00010000'                          0145
         BNO   @RF00145                                            0145
*         DO;                                                      0146
*           IDPTR=ADDR(EXTMSG);     /* SET UP EXTERNAL LABEL         */
         LA    IDPTR,EXTMSG                                        0147
*         END;                      /* PRINT THE ENTRY               */
*       ELSE                        /* SET UP SIO LABEL              */
*         DO;                                                      0149
         B     @RC00145                                            0149
@RF00145 DS    0H                                                  0150
*           ABDLINEA(7:16)=SIOMSG1; /* SET UP FIRST SIO MSG          */
         MVC   ABDLINEA+6(10,ABDARPTR),SIOMSG1                     0150
*           ABDLINEA(38:43)=SIOMSG2;/* SECOND LABEL CHANGE           */
         MVC   ABDLINEA+37(6,ABDARPTR),SIOMSG2                     0151
*           ABDLINEA(65:67)=ISBWD;  /* OVERLAY R1 WITH ISB           */
         MVC   ABDLINEA+64(3,ABDARPTR),ISBWD                       0152
*           IDPTR=ADDR(SIOMSG);     /* SET UP SIO MSG                */
         LA    IDPTR,SIOMSG                                        0153
*         END;                      /* PRINT THE ENTRY               */
*OUTPUTUM:                                                         0155
*       ABDLINEA(2:4)=TRACEID;      /* MOVE IN PROPER LABEL          */
@RC00145 DS    0H                                                  0155
OUTPUTUM MVC   ABDLINEA+1(3,ABDARPTR),TRACEID(IDPTR)               0155
*       CALL IEAVAD11;              /* OUTPUT THE ENTRY              */
         L     @15,@CV00118                                        0156
         BALR  @14,@15                                             0156
*       IF RETCODE^=0               /* IF ERRED                      */
*         THEN                      /* LEAVE WITH BAD CODE           */
         SLR   @14,@14                                             0157
         CR    RETCODE,@14                                         0157
         BNE   @RT00157                                            0157
*         GOTO FREEBUF;             /* FREE BUFFER AND LEAVE         */
*       IF OLDCUR=0                 /* IF LAST ENTRY TO PRINT        */
*         THEN                      /* LEAVE CODE=0                  */
         CR    OLDCUR,@14                                          0159
         BNE   @RF00159                                            0159
*         DO;                                                      0160
*           RETCODE=0;              /* SET RETURN CODE TO ZERO       */
*                                                                  0161
         SLR   RETCODE,RETCODE                                     0161
*           /*********************************************************/
*           /*                                                       */
*           /* THE FOLLOWING AREA IS ENTERED AT END OF TRACE FORMAT  */
*           /* OR IF AN ERROR OCCURED FORMATTING TRACE TABLE @G33SPHW*/
*           /*                                                       */
*           /*********************************************************/
*                                                                  0162
*FREEBUF:                           /* FREE TRACE BUFFER             */
*           SAVERET=RETCODE;        /* SAVE RETURN CODE OVER FREE    */
FREEBUF  LR    SAVERET,RETCODE                                     0162
*         END;                                                     0163
*       ELSE                        /* CONTINUE WITH NEXT ENTRY      */
*         DO;                                                      0164
         B     @RC00159                                            0164
@RF00159 DS    0H                                                  0165
*           TOPRINT=TOPRINT+32;     /* POINT TO NEXT ENTRY           */
         LA    TOPRINT,32(,TOPRINT)                                0165
*           GOTO CHKEND;            /* CONTINUE LOOP                 */
         B     CHKEND                                              0166
*         END;                                                     0167
*     END;                                                         0168
@RC00159 DS    0H                                                  0169
*                                                                  0169
*   /*****************************************************************/
*   /*                                                               */
*   /* FINAL PROCESSING CONSISTS OF UNCHAINING THE SAVE AREA, FREEING*/
*   /* IT, SETTING THE RETURN CODE TO THE VALUE SAVED, AND RETURNING */
*   /* TO THE MAINLINE OF ABDUMP, AFTER THE REGISTERS ARE RESTORED.  */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0169
*FREERET:                                                          0169
*   PARMPTR=ADDR(ABDASIZE);         /* SET UP FREEMAIN LIST REG      */
@RC00072 DS    0H                                                  0169
FREERET  LA    PARMPTR,ABDASIZE(,ABDARPTR)                         0169
*   ABDAAREA=SAVEPTR;               /* SET UP ADDRESS TO FREE        */
         ST    SAVEPTR,ABDAAREA(,ABDARPTR)                         0170
*   SAVEPTR=SAVEPTR->PRESAPTR;      /* SET UP PREVIOUS SAVE AREA     */
         L     SAVEPTR,PRESAPTR(,SAVEPTR)                          0171
*   NEXSAPTR=0;                     /* CLEAR POINTER TO AREA TO BE 0172
*                                      FREED                         */
         SLR   @14,@14                                             0172
         ST    @14,NEXSAPTR(,SAVEPTR)                              0172
*   GENERATE;                                                      0173
         FREEMAIN E,LV=72,                                             *
               SP=253,                                                 *
               A=ABDGMA(ABDARPTR),                                     *
               MF=(E,(1))
*   RETCODE=SAVERET;                /* PASS RETURN CODE TO MAIN      */
         LR    RETCODE,SAVERET                                     0174
*   DCL                                                            0175
*     PATCHLOC FIXED(31) STATIC;                                   0175
*   GEN DATA DEFS(PATCHLOC);                                       0176
*   DECLARE                         /* COMMON VARIABLES              */
*     I256C CHAR(256) BASED,                                       0177
*     I031F FIXED(31) BASED,                                       0177
*     I031P PTR(31) BASED,                                         0177
*     I015F FIXED(15) BASED,                                       0177
*     I015P PTR(15) BASED,                                         0177
*     I008P PTR(8) BASED,                                          0177
*     I001C CHAR(1) BASED;          /*                        @Y02705*/
*   END IEAVAD0C                                                   0178
*                                                                  0178
*/* THE FOLLOWING INCLUDE STATEMENTS WERE FOUND IN THIS PROGRAM.     */
*/*%INCLUDE SYSLIB  (IHAABDA )                                       */
*/*%INCLUDE SYSLIB  (CVT     )                                       */
*/*%INCLUDE SYSLIB  (IHAPSA  )                                       */
*/*%INCLUDE SYSLIB  (IKJTCB  )                                       */
*/*%INCLUDE SYSLIB  (IEAPXNIP)                                       */
*                                                                  0178
*       ;                                                          0178
         B     @EL00001                                            0178
@DATA    DS    0H
@DATD    DSECT
         DS    0F
IEAVAD0C CSECT
         DS    0F
@DATD    DSECT
         DS    0D
IEAVAD0C CSECT
         DS    0F
@SIZDATD DC    AL1(0)
         DC    AL3(@ENDDATD-@DATD)
@CV00118 DC    V(IEAVAD11)
@CV00119 DC    V(IEAVAD31)
         DS    0D
B@PROLOG B     22(,@15)             ZP60018
ED3MSK   DC    XL4'40202120'        ZP60018
EPMSG    DC    CL2'EP'              ZP60018
NMMSG    DC    CL2'NM'              ZP60018
STARMSG  DC    CL2'**'              ZP60018
MCMSG    DC    CL2'MC'              ZP60018
PERMSG   DC    CL3'PER'             ZP60018
DSPMSG   DC    CL3'DSP'
EXTMSG   DC    CL3'EXT'
IOMSG    DC    CL3'I/O'
PGMMSG   DC    CL3'PGM'
SVCMSG   DC    CL3'SVC'
SIOMSG   DC    CL3'SIO'
ISDMSG   DC    CL3'ISD'
SSRMSG   DC    CL3'SSR'
RESWD    DC    CL3'RES'
ISBWD    DC    CL3'ISB'
NEWMSG   DC    CL3'NEW'
SIOMSG1  DC    CL10'CC/DEV/CAW'
SIOMSG2  DC    CL6'CSW   '
ASIDWD   DC    CL6'ASD/R0'
NOBUFMSG DS    CL24
         ORG   NOBUFMSG
@NM00002 DC    CL12'-NO CORE FOR'
TRACEMSG DC    CL12'-TRACE TABLE'
         ORG   NOBUFMSG+24
SUSMSG   DC    CL39'-SUPERVISOR TRACE TEMPORARILY SUSPENDED'
TTLINE   DS    CL48
         ORG   TTLINE
@NM00003 DC    X'086301'            ZP60018: WAS X'056304'
@NM00004 DC    CL7'OLD PSW'
@NM00005 DC    X'FF'
@NM00006 DC    X'180340'
@NM00007 DC    CL1' '
@NM00008 DC    X'5341'
@NM00009 DC    CL6'R15/R0'
@NM00010 DC    X'FF'
@NM00011 DC    X'330340'
@NM00012 DC    CL1' '
@NM00013 DC    X'1342'
@NM00014 DC    CL2'R1'
@NM00015 DC    X'2341'
@NM00016 DC    CL3'IDS'
@NM00017 DC    X'2341'
@NM00018 DC    CL3'TCB'
@NM00019 DC    X'2341'
@NM00020 DC    CL3'TME'
@NM00021 DC    X'FF'
         ORG   TTLINE+48
IEAVAD0C CSECT
         DS   0H
PATCHLOC DC ((@DATA-@PSTART)/20)X'00'
@DATD    DSECT
         ORG   *+1-(*-@DATD)/(*-@DATD) INSURE DSECT DATA
@ENDDATD EQU   *
IEAVAD0C CSECT
@00      EQU   00                      EQUATES FOR REGISTERS 0-15
@01      EQU   01
@02      EQU   02
@03      EQU   03
@04      EQU   04
@05      EQU   05
@06      EQU   06
@07      EQU   07
@08      EQU   08
@09      EQU   09
@10      EQU   10
@11      EQU   11
@12      EQU   12
@13      EQU   13
@14      EQU   14
@15      EQU   15
PARMPTR  EQU   @01
ABDARPTR EQU   @02
NEWCUR   EQU   @03
TOPRINT  EQU   @03
IDPTR    EQU   @04
OLDCUR   EQU   @05
TCBPTR   EQU   @06
TRACEND  EQU   @06
TRACENDA EQU   @06
TRACESRT EQU   @07
SAVERET  EQU   @08
AUTOPTR  EQU   @12
SAVEPTR  EQU   @13
RETCODE  EQU   @15
ENTRYPTR EQU   @15
ENTRYREG EQU   @00
RETERN   EQU   @14
NEXSAPTR EQU   8
PRESAPTR EQU   4
TRACEID  EQU   0
TRTYPE   EQU   2
ISDSP    EQU   TRTYPE
ISSSR    EQU   ISDSP
ISIO     EQU   ISSSR
ISISD    EQU   ISIO
ISSVC    EQU   ISSSR
ISPGM    EQU   ISSVC
ISEXT    EQU   ISDSP
ISONE    EQU   ISEXT
TRGTFBIT EQU   20
SUSPEND  EQU   TRGTFBIT
ABDAREA  EQU   0
ABDPARMS EQU   ABDAREA+12
ABDFLAG  EQU   ABDPARMS+1
APFSDATA EQU   ABDPARMS+4
APFSDAT1 EQU   ABDPARMS+5
APFPDATA EQU   ABDPARMS+6
ABDPTRS  EQU   ABDAREA+84
ABDCP    EQU   ABDAREA+96
ABDFP    EQU   ABDAREA+100
ABDLP    EQU   ABDAREA+104
ABDCP1   EQU   ABDAREA+108
ABDFLAG1 EQU   ABDAREA+136
ABDFLAG2 EQU   ABDAREA+137
ABDTRBIT EQU   ABDFLAG2
ABDRES1  EQU   ABDAREA+139
ABDBPTR  EQU   ABDAREA+244
ABDLLINE EQU   ABDAREA+248
ABDUPRF  EQU   ABDAREA+265
ABDUPRFN EQU   ABDAREA+266
ABDBLNKS EQU   ABDAREA+267
ABDBUFFS EQU   ABDAREA+308
ABDGMA   EQU   ABDAREA+316
ABDAAREA EQU   ABDGMA
ABDASIZE EQU   ABDGMA+4
ESPARM   EQU   ABDAREA+340
ESTAEFLG EQU   ESPARM+28
ABDLINEA EQU   ABDAREA+384
ABDCC    EQU   ABDLINEA
TRHDR    EQU   0
TRSIZE   EQU   TRHDR+12
IPLDATA  EQU   0
GENLIST  EQU   0
I001C    EQU   0
I008P    EQU   0
I015F    EQU   0
I015P    EQU   0
I031F    EQU   0
I031P    EQU   0
I256C    EQU   0
         AGO   .@UNREFD                START UNREFERENCED COMPONENTS
NIPTRACE EQU   GENLIST+2
@NM00078 EQU   GENLIST
IPLDNUCE EQU   IPLDATA+16
IPLDNUCS EQU   IPLDATA+12
IPLDNUCM EQU   IPLDATA+11
IPLDVTOC EQU   IPLDATA+6
IPLDVSER EQU   IPLDATA
TRLEN    EQU   TRSIZE+1
TRSBPOOL EQU   TRSIZE
TREND    EQU   TRHDR+8
TRBEG    EQU   TRHDR+4
TRPTR    EQU   TRHDR
@NM00077 EQU   TCBXTNT2+32
@NM00076 EQU   TCB+296
@NM00075 EQU   TCBRECDE
@NM00074 EQU   TCBPMASK
@NM00049 EQU   CVTFIX+248
@NM00048 EQU   CVTFIX
ABDSAVE1 EQU   ABDAREA+672
ABDSAVHD EQU   ABDAREA+592
ABDPADC  EQU   ABDAREA+589
ABDLINE  EQU   ABDLINEA+1
ABDSRC2  EQU   ABDAREA+382
ABDLOG   EQU   ABDAREA+380
ABDSRC1  EQU   ABDAREA+378
ABDPHY   EQU   ABDAREA+376
ABDLCNT  EQU   ABDAREA+372
@NM00047 EQU   ESPARM+29
@NM00046 EQU   ESTAEFLG
EDCB     EQU   ESTAEFLG
ETCB     EQU   ESTAEFLG
ESUBSYT  EQU   ESTAEFLG
ERESTART EQU   ESPARM+24
EMODNAME EQU   ESPARM+16
EABDAREA EQU   ESPARM+12
ESAUTOP  EQU   ESPARM+8
ESBSREG2 EQU   ESPARM+4
ESBSREG  EQU   ESPARM
ABDRTRN  EQU   ABDAREA+336
ABDSVCB  EQU   ABDAREA+332
@NM00045 EQU   ABDGMA+14
ABDCDID  EQU   ABDGMA+12
ABDPAREA EQU   ABDGMA+8
ABDBUFSZ EQU   ABDBUFFS+4
ABDBUFAD EQU   ABDBUFFS
ABSLCTSV EQU   ABDAREA+304
ABDSTADS EQU   ABDAREA+300
ABDUPRID EQU   ABDAREA+296
ABDPCAP  EQU   ABDAREA+295
ABDLLNGH EQU   ABDAREA+294
ABDINCPL EQU   ABDAREA+292
ABDFWORK EQU   ABDAREA+288
ABDIDENT EQU   ABDAREA+286
ABDIND   EQU   ABDAREA+284
ABDSIZE  EQU   ABDAREA+280
ABDLENTH EQU   ABDAREA+276
ABDBLOCK EQU   ABDAREA+272
ABDSTAD  EQU   ABDAREA+268
@NM00044 EQU   ABDBLNKS
@NM00043 EQU   ABDBLNKS
@NM00042 EQU   ABDBLNKS
@NM00041 EQU   ABDBLNKS
@NM00040 EQU   ABDBLNKS
@NM00039 EQU   ABDBLNKS
@NM00038 EQU   ABDBLNKS
ABDBLKN3 EQU   ABDBLNKS
@NM00037 EQU   ABDUPRFN
@NM00036 EQU   ABDUPRFN
@NM00035 EQU   ABDUPRFN
@NM00034 EQU   ABDUPRFN
@NM00033 EQU   ABDUPRFN
@NM00032 EQU   ABDUPRFN
ABDUPRSL EQU   ABDUPRFN
ABDUPRPM EQU   ABDUPRFN
@NM00031 EQU   ABDUPRF
@NM00030 EQU   ABDUPRF
@NM00029 EQU   ABDUPRF
@NM00028 EQU   ABDUPRF
@NM00027 EQU   ABDUPRF
UPRFMT20 EQU   ABDUPRF
UPRFMET  EQU   ABDUPRF
UPRFMAT  EQU   ABDUPRF
ABDFMTWK EQU   ABDAREA+256
ABDLPTR  EQU   ABDAREA+252
ABDSSPAR EQU   ABDAREA+188
ABDWORK  EQU   ABDAREA+140
@NM00026 EQU   ABDRES1
ABDPGHD  EQU   ABDRES1
ABDFMTLD EQU   ABDRES1
ABDSQA   EQU   ABDRES1
ABDOLSQA EQU   ABDRES1
ABDREGS  EQU   ABDRES1
ABDPSW   EQU   ABDRES1
ABDKEY   EQU   ABDAREA+138
ABDLSQA  EQU   ABDFLAG2
ABDSTAT  EQU   ABDFLAG2
ABDSWAP  EQU   ABDFLAG2
ABDGTFCL EQU   ABDFLAG2
ABDTRNAV EQU   ABDFLAG2
ABDGTF   EQU   ABDFLAG2
ABDSUPER EQU   ABDFLAG2
ABDIDX   EQU   ABDFLAG1
ABDSPHD  EQU   ABDFLAG1
ABDSVCHD EQU   ABDFLAG1
ABDSQSDM EQU   ABDFLAG1
ABDLSTHD EQU   ABDFLAG1
ABDQCBMN EQU   ABDFLAG1
ABDQCBMJ EQU   ABDFLAG1
ABDQCBHD EQU   ABDFLAG1
ABDPGWK  EQU   ABDAREA+128
ABDWORK1 EQU   ABDAREA+120
ABDRESV3 EQU   ABDAREA+116
ABDUPRXT EQU   ABDAREA+112
ABDPTRS3 EQU   ABDPTRS+8
ABDPTRS2 EQU   ABDPTRS+4
ABDPTRS1 EQU   ABDPTRS
ABDPCTR  EQU   ABDAREA+82
ABDLCTR  EQU   ABDAREA+80
ABDIOBP  EQU   ABDAREA+76
ABDPTR   EQU   ABDAREA+72
ABDDCB   EQU   ABDAREA+68
ABDLEN   EQU   ABDAREA+66
ABDTYPE  EQU   ABDAREA+64
ABDECB   EQU   ABDAREA+60
ABDSAVE  EQU   ABDAREA+56
ABDDECB  EQU   ABDAREA+52
ABDPARA  EQU   ABDAREA+48
ABDTABPT EQU   ABDAREA+44
ABDINXPT EQU   ABDAREA+40
ABDCTR   EQU   ABDAREA+38
ABDTRLN  EQU   ABDAREA+36
ABDHDRAD EQU   ABDPARMS+20
ABDSNAPP EQU   ABDPARMS+16
ABDPTCBP EQU   ABDPARMS+12
ABDPDCBP EQU   ABDPARMS+8
@NM00025 EQU   ABDPARMS+7
@NM00024 EQU   APFPDATA
APFSPALL EQU   APFPDATA
APFPSW   EQU   APFPDATA
APFJPA   EQU   APFPDATA
APFLPA   EQU   APFPDATA
APFREGS  EQU   APFPDATA
APFSAVE2 EQU   APFPDATA
APFSAVE  EQU   APFPDATA
@NM00023 EQU   APFSDAT1
APFERR   EQU   APFSDAT1
APFIO    EQU   APFSDAT1
APFDM    EQU   APFSDATA
APFQCB   EQU   APFSDATA
APFSUPDA EQU   APFSDATA
APFTRACE EQU   APFSDATA
APFSWA   EQU   APFSDATA
APFLSQA  EQU   APFSDATA
APFSQA   EQU   APFSDATA
APFNUC   EQU   APFSDATA
@NM00022 EQU   ABDPARMS+2
APFHDR   EQU   ABDFLAG
APFSNAPP EQU   ABDFLAG
APFTCB   EQU   ABDFLAG
APFID    EQU   ABDFLAG
APFABEND EQU   ABDFLAG
APFENVS2 EQU   ABDFLAG
APFVS2   EQU   ABDFLAG
APFSNAP  EQU   ABDFLAG
ABDPID   EQU   ABDPARMS
ABDCRB   EQU   ABDAREA+8
ABDCTCB  EQU   ABDAREA+4
ABDTCB   EQU   ABDAREA
@NM00001 EQU   TRTYPE
@NM00073 EQU   PSA+3668
@NM00072 EQU   PSA+1048
@NM00071 EQU   PSA+621
@NM00070 EQU   PSA+613
@NM00069 EQU   FLCFSA
@NM00068 EQU   FLCMCLA+72
@NM00067 EQU   FLCMCLA+20
@NM00066 EQU   FLCIOA
@NM00065 EQU   FLCMCLA+15
@NM00064 EQU   FLCMCLA+14
@NM00063 EQU   FLCMCLA+12
@NM00062 EQU   FLCIOEL
@NM00061 EQU   PSA+160
@NM00060 EQU   PSA+156
@NM00059 EQU   FLCPER
@NM00058 EQU   PSA+151
@NM00057 EQU   PSA+148
@NM00056 EQU   FLCTEA
@NM00055 EQU   FLCPIILC
@NM00054 EQU   PSAEPPSW
@NM00053 EQU   FLCSVILC
@NM00052 EQU   PSAESPSW
@NM00051 EQU   PSA+128
@NM00050 EQU   FLCICCW2+4
.@UNREFD ANOP                          END UNREFERENCED COMPONENTS
@RT00085 EQU   FREEBUF
@RT00096 EQU   FREEBUF
@RT00102 EQU   FREEBUF
@RT00105 EQU   FREEBUF
@RT00108 EQU   FREEBUF
@RT00157 EQU   FREEBUF
@ENDDATA EQU   *
         CVT   DSECT=YES
         IHALPDE
         END   IEAVAD0C,(C'PLS1951',0701,77049)
/*
//*
//STEP3   EXEC PGM=IEBGENER
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  *
  IDENTIFY IEAVAD0C('ZP60018')
/*
//SYSUT2   DD  DSN=&&SMPMCS,DISP=(MOD,PASS)
//SYSIN    DD  DUMMY
//*
//STEP4   EXEC SMPREC,WORK='SYSALLDA'
//SMPPTFIN DD  DSN=&&SMPMCS,DISP=(OLD,DELETE)
//SMPCNTL  DD  *
  RECEIVE
          SELECT(ZP60018)
          .
/*
//*
//STEP5CK EXEC SMPAPP,WORK='SYSALLDA'
//SMPCNTL  DD  *
  APPLY
        SELECT(ZP60018)
        CHECK
        .
/*
//*
//STEP5   EXEC SMPAPP,COND=(0,NE),WORK='SYSALLDA'
//SMPCNTL  DD  *
  APPLY
        SELECT(ZP60018)
        DIS(WRITE)
        .
/*
//
//ZP60019  JOB (SYSGEN),'J05 M33: ZP60019',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             REGION=4096K
/*JOBPARM LINES=100
//JOBCAT   DD  DSN=SYS1.VSAM.MASTER.CATALOG,DISP=SHR
//*
//*  ZAP TO RECORD CPU TIME USED BY JOBS WITH TIME=1440 IN JCL.
//*
//RECEIVE EXEC SMPREC,WORK='SYSALLDA'
//SMPPTFIN DD  *
++USERMOD(ZP60019)       /* RECORD CPU TIME WHEN TIME=1440 */  .
++VER(Z038) FMID(FBB1221) PRE(UZ67391)
 /*
   PROBLEM DESCRIPTION:
     NO RECORD IS KEPT OF THE CPU TIME USED WHEN TIME=1440 IS USED.
       SPECIFYING TIME=1440 ON THE JOB OR EXEC JCL STATEMENT IS
       A METHOD OF INDICATING TO THE SYSTEM THAT THERE IS NO LIMIT
       TO BE APPLIED BY THE SYSTEM TO THE STEP'S ACCUMULATION OF
       CPU TIME OR WAIT TIME.  THIS FACILITY WAS CONCEPTUALLY
       EXTENDED TO MEAN THAT SUCH JOBS WERE NOT TO BE SUBJECT TO
       CHARGEBACK, AND SO CPU TIME USED BY SUCH JOBS WAS "FREE".
       AS A RESULT, THE CPU TIME USED BY JOB STEPS WHEN TIME=1440
       IN EFFECT IS NOT RECORDED, AND ZERO TIME IS REPORTED IN THE
       STEP (IEF374I) AND JOB (IEF376I) END MESSAGES, AND IN SMF
       (RECORD TYPES 4, 5, 34 AND 35) THUS EFFECTIVELY FAILING TO
       CAPTURE SIGNIFICANT JOB AND SYSTEM PERFORMANCE DATA.

       THIS USERMOD UPDATES THE INITIATOR ATTACH ROUTINE TO MOVE
       THE TEST TO SEE IF JOB/STEP TIMING LIMITS ARE IN EFFECT
       UNTIL AFTER THE TCB AND SRB CPU TIMES USED BY THE STEP HAVE
       BEEN SAVED.

   SPECIAL CONDITIONS:
     ACTION:
       A "CLPA" MUST BE PERFORMED AT IPL TIME FOR THIS SYSMOD TO
       BECOME ACTIVE.

   COMMENTS:
     PRYCROFT SIX P/L PUBLIC DOMAIN USERMOD FOR MVS 3.8 NO. 19.

     THE FOLLOWING MODULES AND/OR MACROS ARE AFFECTED BY THIS USERMOD:
     MODULES:
       IEFSD263
 */.
++ZAP(IEFSD263) DISTLIB(AOSB3).
 NAME IEFSD263
 IDRDATA ZP60019
VER 062A 1277                  LTR   R7,R7
VER 062C 4720,C692             BC    2,NOTEST
VER 0630 9823,A040             LM    R2,R3,ASCBEJST
VER 0634 8C20,000C             SRDL  R2,12
VER 0638 5D20,C90A             D     R2,TEN1000
VER 063C 5920,C916             C     R2,FIVE1000
VER 0640 4740,C646             BL    NOROUND2
VER 0644 5A30,C912             A     R3,ROUNDUP
VER 0648 5030,405C    NOROUND2 ST    R3,LCTTSTU4
VER 064C 9823,A0C8             LM    R2,R3,ASCBSRBT
VER 0650 8C20,000C             SRDL  R2,12
VER 0654 5D20,C90A             D     R2,TEN1000
VER 0658 5920,C916             C     R2,FIVE1000
VER 065C 4740,C662             BL    NOROUND3
VER 0660 5A30,C912             A     R3,ROUNDUP
VER 0664 5030,415C    NOROUND3 ST    R3,LCTTSRB4
REP 062A 9823,A040             LM    R2,R3,ASCBEJST
REP 062E 8C20,000C             SRDL  R2,12
REP 0632 5D20,C90A             D     R2,TEN1000
REP 0636 5920,C916             C     R2,FIVE1000
REP 063A 4740,C640             BL    NOROUND2
REP 063E 5A30,C912             A     R3,ROUNDUP
REP 0642 5030,405C    NOROUND2 ST    R3,LCTTSTU4
REP 0646 9823,A0C8             LM    R2,R3,ASCBSRBT
REP 064A 8C20,000C             SRDL  R2,12
REP 064E 5D20,C90A             D     R2,TEN1000
REP 0652 5920,C916             C     R2,FIVE1000
REP 0656 4740,C65C             BL    NOROUND3
REP 065A 5A30,C912             A     R3,ROUNDUP
REP 065E 5030,415C    NOROUND3 ST    R3,LCTTSRB4
REP 0662 1277                  LTR   R7,R7
REP 0664 4720,C692             BC    2,NOTEST
/*
//SMPCNTL  DD  *
  RECEIVE
          SELECT(ZP60019)
          .
/*
//*
//APPLYCK  EXEC SMPAPP,WORK='SYSALLDA'
//SMPCNTL  DD  *
  APPLY
        SELECT(ZP60019)
        CHECK
        .
/*
//*
//APPLY   EXEC SMPAPP,COND=(0,NE),WORK='SYSALLDA'
//SMPCNTL  DD  *
  APPLY
        SELECT(ZP60019)
        DIS(WRITE)
        .
/*
//
//ZP60020  JOB (SYSGEN),'J06 M34: ZP60020',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             REGION=4096K
/*JOBPARM LINES=100
//JOBCAT   DD  DSN=SYS1.VSAM.MASTER.CATALOG,DISP=SHR
//*
//*  ZAP TO REMOVE THE LINKAGE EDITOR SYSLIN BLKSIZE LIMIT OF 3200.
//*
//RECEIVE EXEC SMPREC
//SMPPTFIN DD  *
++USERMOD(ZP60020)       /* REMOVE SYSLIN BLKSIZE 3200 LIMIT */  .
++VER(Z038) FMID(EPM1102) PRE(UZ48373,UZ69717)
 /*
   PROBLEM DESCRIPTION:
     THE LINKAGE EDITOR HAS A BLOCK SIZE LIMIT OF 3200 FOR SYSLIN.
       WHEN READING CONTROL STATEMENTS AND OBJECT DECKS FROM
       THE SYSLIN FILE THE LINKAGE EDITOR CANNOT PROCESS 80-BYTE
       RECORDS WITH A BLOCKING FACTOR OF MORE THAN FORTY.

       THIS USERMOD INCREASES THE LIMIT TO 32720 OR A BLOCKING
       FACTOR OF 409 WHICH IS THE MAXIMUM SUPPORTED BY MVS
       ACCESS METHODS.

   SPECIAL CONDITIONS:
     NONE.

   COMMENTS:
     PRYCROFT SIX P/L PUBLIC DOMAIN USERMOD FOR MVS 3.8 NO. 20.

       THIS USERMOD IS A REFIT OF THE USERMOD PUBLISHED IN SAM
       GOLOB'S MVS TOOLS AND TRICKS OF THE TRADE COLUMN IN NASPA'S
       TECHNICAL SUPPORT MAGAZINE IN SEPTEMBER 1996 AND SEPTEMBER
       1998.  THIS REFIT, INCLUDING THE ALTERATION OF THE DEFAULT
       SIZE SETTING, WAS DEVELOPED BY ENRICO SORICHETTI.


     THE FOLLOWING MODULES AND/OR MACROS ARE AFFECTED BY THIS USERMOD:
     MODULES:
       HEWLFINT
       HEWLFAPT
 */.
++ZAP(HEWLFINT) DISTLIB(AOS04).
 NAME HEWLFINT
 IDRDATA ZP60020
VER 0910 0C80       LINMAX   DC    H'3200'
VER 0D4E 0028       M40      DC    H'40'
REP 0910 7FD0       LINMAX   DC    H'32720'
REP 0D4E 0199       M40      DC    H'409'
++ZAP(HEWLFAPT) DISTLIB(AOS04).
 NAME HEWLFAPT
 IDRDATA ZP60020
VER 0216 0028       MAXBF    DC    H'40'   MAXIMUM BLOCKING FACTOR
REP 0216 0199       MAXBF    DC    H'409'  MAXIMUM BLOCKING FACTOR
 NAME HEWLFDEF
 IDRDATA ZP60020
VER 0008 00030000   SIZE=192K
REP 0008 00040000   SIZE=256K
/*
//SMPCNTL  DD  *
  RECEIVE
          SELECT(ZP60020)
          .
/*
//APPLYCK  EXEC SMPAPP,WORK='SYSALLDA'
//SMPCNTL  DD  *
  APPLY
        SELECT(ZP60020)
        CHECK
        .
/*
//*
//APPLY    EXEC SMPAPP,COND=(0,NE),WORK='SYSALLDA'
//SMPCNTL  DD  *
  APPLY
        SELECT(ZP60020)
        DIS(WRITE)
        .
/*
//
//ZP60021  JOB (SYSGEN),'J07 M35: ZP60021',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             REGION=4096K
/*JOBPARM LINES=100
//JOBCAT   DD  DSN=SYS1.VSAM.MASTER.CATALOG,DISP=SHR
//*
//*  ZAP TO DISPLAY KEYBOARD CHARACTERS IN A PRINTED DUMP.
//*
//RECEIVE EXEC SMPREC,WORK='SYSALLDA'
//SMPPTFIN DD  *
++USERMOD(ZP60021)       /* SHOW KEYBOARD CHARACTERS IN DUMP */  .
++VER(Z038) FMID(EBB1102) PRE(UZ61115)
 /*
   PROBLEM DESCRIPTION:
     DUMPS SHOW PERIODS OBSCURING LOWER CASE AND OTHER CHARACTERS.
       WHEN A DUMP SUCH AS A SYSUDUMP FORMATS STORAGE CONTENTS
       INTO HEXADECIMAL AND CHARACTER DATA, THE CHARACTER DATA
       WRITTEN IS ALL PERIODS EXCEPT FOR SPACES, UPPER CASE
       ALPHABETIC AND NUMERIC EBCDIC CHARACTERS.

       THIS USERMOD REPLACES THE PERIODS TO BE USED FOR THE OTHER
       US KEYBOARD CHARACTERS IN THE RELEVANT TRANSLATE TABLE
       WITH THE CODE POINTS THEMSELVES SO THAT THESE CHARACTERS
       CAN APPEAR IN STORAGE DUMP.

   SPECIAL CONDITIONS:
     ACTION:
       A "CLPA" MUST BE PERFORMED AT IPL TIME FOR THIS SYSMOD TO
       BECOME ACTIVE.

   COMMENTS:
     PRYCROFT SIX P/L PUBLIC DOMAIN USERMOD FOR MVS 3.8 NO. 21.

     THE FOLLOWING MODULES AND/OR MACROS ARE AFFECTED BY THIS USERMOD:
     MODULES:
       IEAVAD51
 */.
++ZAP(IEAVAD51) DISTLIB(AOSC5).
 NAME IEAVAD51
 IDRDATA ZP60021
VER 01E6 4B4B4B4B4B4B4B4B   ........    HEX 00 -> 07
VER 0226 404B4B4B4B4B4B4B    .......    HEX 40 -> 47
VER 02D6 F0F1F2F3F4F5F6F7   01234567    HEX F0 -> F7
VER 02DE F8F94B4B4B4B4B4B   89......    HEX F8 -> FF
REP 022E 4B4B4A4B4C4D4E4F   ..¢.<(+|    HEX 48 -> 4F
REP 0236 504B4B4B4B4B4B4B   &.......    HEX 50 -> 57
REP 023E 4B4B5A5B5C5D5E5F   ..!$*);¬    HEX 58 -> 5F
REP 0246 60614B4B4B4B4B4B   -/......    HEX 60 -> 67
REP 024E 4B4B6A6B6C6D6E6F   ..Š,%_>?    HEX 68 -> 6F
REP 025E 4B797A7B7C7D7E7F   .`:#@'="    HEX 78 -> 7F
REP 0266 4B81828384858687   .abcdefg    HEX 80 -> 87
REP 026E 88894B4B4B4B4B4B   hi......    HEX 88 -> 8F
REP 0276 4B91929394959697   .jklmnop    HEX 90 -> 97
REP 027E 98994B4B4B4B4B4B   qr......    HEX 98 -> 9F
REP 0286 4BA1A2A3A4A5A6A7   .~stuvwx    HEX A0 -> A7
REP 028E A8A94B4B4BAD4B4B   yz...[..    HEX A8 -> AF
REP 029E 4B4B4B4B4BBD4B4B   .....]..    HEX B8 -> BF
REP 02A6 C0C1C2C3C4C5C6C7   {ABCDEFG    HEX C0 -> C7
REP 02AE C8C94B4B4B4B4B4B   HI......    HEX C8 -> CF
REP 02B6 D0D1D2D3D4D5D6D7   }JKLMNOP    HEX D0 -> D7
REP 02BE D8D94B4B4B4B4B4B   QR......    HEX D8 -> DF
REP 02C6 E04BE2E3E4E5E6E7   \.STUVWX    HEX E0 -> E7
REP 02CE E8E94B4B4B4B4B4B   YZ......    HEX E8 -> EF
/*
//SMPCNTL  DD  *
  RECEIVE
          SELECT(ZP60021)
          .
/*
//*
//APPLYCK EXEC SMPAPP,WORK='SYSALLDA'
//SMPCNTL  DD  *
  APPLY
        SELECT(ZP60021)
        CHECK
        .
/*
//*
//APPLY   EXEC SMPAPP,COND=(0,NE),WORK='SYSALLDA'
//SMPCNTL  DD  *
  APPLY
        SELECT(ZP60021)
        DIS(WRITE)
        .
/*
//
//ZP60022  JOB (SYSGEN),'J08 M36: ZP60022',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             REGION=4096K
/*JOBPARM LINES=100
//JOBCAT   DD  DSN=SYS1.VSAM.MASTER.CATALOG,DISP=SHR
//*
//*  ZAP TO ALLOW FORMAT 1 STAX PARAMETER LIST TO FUNCTION CORRECTLY.
//*
//RECEIVE EXEC SMPREC,WORK='SYSALLDA'
//SMPPTFIN DD  *
++USERMOD(ZP60022)       /* SUPPORT FORMAT 1 STAX PLIST */  .
++VER(Z038) FMID(EBB1102) PRE(UZ51847)
 /*
   PROBLEM DESCRIPTION:
     STAX MACROS FROM LATER MVS VERSIONS CAUSE ABENDS UNDER MVS/370.
       WHEN PROGRAMS WITH STAX MACROS ASSEMBLED WITH MACLIB FROM
       MVS/XA OR LATER WITHOUT SPLEVEL SET TO 1 RUN ON MVS/370,
       THE SVC PARAMETER LIST HAS A DIFFERENT FORMAT WHICH IS NOT
       CORRECTLY PROCESSED, AND WHICH CAN CAUSE ABENDS SUCH AS
       S0C4.

       THIS USERMOD CHANGES THE STAX SVC SERVICE ROUTINE TO CHECK
       FOR A FORMAT 1 STAX PARAMETER LIST DURING PARAMETER VALIDITY
       TESTING AND WHEN THE PARAMETER LIST IS COPIED FOR PROCESSING.

   SPECIAL CONDITIONS:
     ACTION:
       A "CLPA" MUST BE PERFORMED AT IPL TIME FOR THIS SYSMOD TO
       BECOME ACTIVE.

   COMMENTS:
     PRYCROFT SIX P/L PUBLIC DOMAIN USERMOD FOR MVS 3.8 NO. 22.

     THE FOLLOWING MODULES AND/OR MACROS ARE AFFECTED BY THIS USERMOD:
       IEAVAX00
 */.
++ZAP(IEAVAX00) DISTLIB(AOSC5).
 NAME IGC0009F
 IDRDATA ZP60022
VER 00A4 BFD7B011       ICM   R13,7,STXUSER(R11)
VER 00A8 B20A0000     A MODESET  EXTKEY=ZERO
VER 00B0 D2131000B000   MVC   STXLIST(20,STXPARM),STXLIST(R11)
VER 00B6 50B01014       ST    R11,STXSAVE(,STXPARM)
VER 00BA 910C1010     B TM    STXOPTS(STXPARM),STXATTON
VER 07FC 00000000       PATCH AREA
REP 00A4 47F067FC       B     C
REP 00B6 47F06810       B     D
REP 07FC BFD7B011     C ICM   R13,7,STXUSER(R11)
REP 0800 9102B010       TM    STXOPTS(R11),X'02' FORMAT # PRESENT?
REP 0804 47E060A8       BNO   A                  NO
REP 0808 58D0B014       L     R13,STXUSER1(,R11) TEST EXTRA WORD
REP 080C 47F060A8       B     A                  TESTING DONE
REP 0810 50B01014     D ST    R11,STXSAVE(,STXPARM)
REP 0814 9102B010       TM    STXOPTS(R11),X'02' FORMAT # PRESENT?
REP 0818 47E060BA       BNO   B                  NO
REP 081C 9501B011       CLI   STXUSER(R11),1     FORMAT 1?
REP 0820 477060BA       BNE   B                  NO
REP 0824 D2021011B015   MVC   STXUSER(3,STXPARM),STXUSER1+1(R11)
REP 082A 47F060BA       B     B                  GOT RIGHT PARM
/*
//SMPCNTL  DD  *
  RECEIVE
          SELECT(ZP60022)
          .
/*
//*
//APPLYCK EXEC SMPAPP,WORK='SYSALLDA'
//SMPCNTL  DD  *
  APPLY
        SELECT(ZP60022)
        CHECK
        .
/*
//*
//APPLY   EXEC SMPAPP,COND=(0,NE),WORK='SYSALLDA'
//SMPCNTL  DD  *
  APPLY
        SELECT(ZP60022)
        DIS(WRITE)
        .
/*
//
//ZP60026  JOB (SYSGEN),'J09 M37: ZP60026',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             REGION=4096K
/*JOBPARM LINES=100
//JOBCAT   DD  DSN=SYS1.VSAM.MASTER.CATALOG,DISP=SHR
//*
//*  ADD THE REUSE OPERAND TO THE ALLOCATE TSO COMMAND.
//*
//STEP01  EXEC PGM=IEBGENER
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  *
++USERMOD(ZP60026)       /* ADD REUSE OPERAND TO ALLOCATE */  .
++VER(Z038) FMID(EBB1102) PRE(UZ80347,UZ69512,UZ58132,UZ65229,UZ52701)
 /*
   PROBLEM DESCRIPTION:
     THERE IS NO WAY TO INFORM ALLOCATE TO REUSE A DDNAME IN USE.
       AN ALLOCATE COMMAND IS SOMETIMES ISSUED WHICH SPECIFIES A
       FILE OR DD NAME OF AN EXISTING ALLOCATION.  THERE IS NO
       METHOD OF SPECIFYING IN THE INITIAL REQUEST THAT ALLOCATE
       SHOULD FREE THE EXISTING ALLOCATION BEFORE PROCEDING WITH
       THE REQUESTED ALLOCATION.  SUCH A SCENARIO TRIGGERS A
       PROMPT FOR 'FREE' (TO PROCEED) OR 'END' (TO TERMINATE).
       CLISTS WHICH ALLOCATE SPECIFIC DDNAMES SHOULD ISSUE FREE
       COMMANDS FOR THOSE DDNAMES BEFORE THE ALLOCATE COMMANDS,
       WHICH CAN THEN TRIGGER UNWANTED MESSAGES WHICH ARE OFTEN
       SUPPRESSED WITH CLIST 'CONTROL NOMSG' STATEMENTS.

       THIS USERMOD ADDS A 'REUSE' KEYWORD OPERAND TO THE TSO
       ALLOCATE COMMAND.  THE USE OF THIS OPERAND ON AN ALLOCATE
       COMMAND CAUSES ALLOCATE TO BYPASS THE PROMPT FOR 'FREE' OR
       'END' AND TO PROCEED AS IF 'FREE' HAS BEEN ENTERED.

       THIS OPERAND IS COMPATIBLE WITH TSO/E.  CLISTS FROM TSO/E
       ENVIRONMENTS WILL NOT NEED EXTRA CONTROL/FREE/CONTROL
       STATEMENTS ADDED AND ALLOCATE 'REUSE' OPERANDS REMOVED IN
       ORDER TO RUN IN AN ENVIRONMENT WITH THIS USERMOD APPLIED.

   SPECIAL CONDITIONS:
     NONE.

   COMMENTS:
     PRYCROFT SIX P/L PUBLIC DOMAIN USERMOD FOR MVS 3.8 NUMBER 26.

     REWORK HISTORY:
       2010-02-27: UPDATE ALLOCATE TSO HELP MEMBER.

     THE FOLLOWING MODULES AND/OR MACROS ARE AFFECTED BY THIS USERMOD:
     MODULES:
       IKJEFD30
       IKJEFD31
       IKJEFD32
       IKJEFD33
       IKJEFD34
       IKJEFD35
       IKJEFD36
       IKJEFD37
     MACROS:
       ALLOCATE
 */.
++MACUPD(ALLOCATE) DISTLIB(AHELP).
./ CHANGE NAME=ALLOCATE
                    VSEQ('VOL-SEQ-NUMBER') ROUND  REUSE                 21250002
))REUSE     -     FILE BEING ALLOCATED IS TO BE FREED AND RE-ALLOCATED  76274004
                  IF CURRENTLY IN USE.                                  76275004
++MOD(IKJEFD30) DISTLIB(AOST4).
/*
//SYSUT2   DD  DSN=&&SMPMCS,DISP=(NEW,PASS),UNIT=SYSALLDA,
//             SPACE=(CYL,3),
//             DCB=(DSORG=PS,RECFM=FB,LRECL=80,BLKSIZE=4080)
//SYSIN    DD  DUMMY
//*
//STEP02  EXEC PGM=IFOX00,PARM='OBJECT,NODECK,NOTERM,XREF(SHORT)'
//SYSPRINT DD  SYSOUT=*
//SYSLIB   DD  DSN=SYS1.MACLIB,DISP=SHR
//         DD  DSN=SYS1.SMPMTS,DISP=SHR
//         DD  DSN=SYS1.AMODGEN,DISP=SHR
//SYSUT1   DD  UNIT=SYSALLDA,SPACE=(CYL,10)
//SYSUT2   DD  UNIT=SYSALLDA,SPACE=(CYL,10)
//SYSUT3   DD  UNIT=SYSALLDA,SPACE=(CYL,10)
//SYSGO    DD  DSN=&&SMPMCS,DISP=(MOD,PASS)
//SYSIN    DD  *
         TITLE 'ALLOCATE COMMAND PROCESSOR                             *
                        '
***********************************************************************
*  $26   ZP60026  2010-01-23 GREG PRICE: ADD 'REUSE' OPERAND.         *
*                            CHANGES APPLIED TO UZ80347 LEVEL.        *
***********************************************************************
IKJEFD30 CSECT ,                                                   0001
@MAINENT DS    0H                                                  0001
         USING *,R15                                               0001
         B     @PROLOG                                             0001
         DC    AL1(33)                                             @26C
         DC    C'IKJEFD30  85.049'                                 0001
         DC    CL17' ZP60026 2010.023'                             @26A
         DROP  R15
@PROLOG  ST    R14,12(,R13)                                        0001
         STM   R0,R12,20(R13)                                      0001
         BALR  R12,0                                               0001
@PSTART  DS    0H                                                  0001
         USING @PSTART,R12                                         0001
         L     R0,@SIZDATD                                         0001
         GETMAIN  R,LV=(0)
         LR    R11,R1                                              0001
         USING @DATD,R11                                           0001
         ST    R13,@SA00001+4                                      0001
         LM    R0,R1,20(R13)                                       0001
         ST    R11,8(,R13)                                         0001
         LR    R13,R11                                             0001
         XC    @ZTEMPS(@ZLEN),@ZTEMPS
         MVC   @PC00001(4),0(R1)                                   0001
*        AWAPTR = ADDR(WACORE);         /* PTR TO WORK AREA          */
         LA    R2AWAPTR,WACORE                                     0067
*        WACORE = WACORE && WACORE;     /* ZERO OUT WORKAREA         */
         XC    WACORE(192),WACORE                                  0068
*        CPPLPTR = R1;                  /* GET INPUT PARAMETERS      */
         ST    R1,CPPLPTR(,R2AWAPTR)                               0069
*        CALL INITIAL;                  /* PERFORM INITIALIZATION    */
         BAL   R14,INITIAL                                         0070
*        CALL COMMSCAN;                 /* SYNTAX CHECK THE COMMAND  */
*                                                                  0071
         BAL   R14,COMMSCAN                                        0071
*        IF RBCODE12 = '0'B THEN        /* IF SUCCESSFUL,            */
         TM    RBCODE12(R2AWAPTR),B'10000000'                      0072
         BNZ   @RF00072                                            0072
*          CALL REQTYPE;                /* PROCESS REQUEST           */
*                                                                  0073
         BAL   R14,REQTYPE                                         0073
*        CALL EXIT1;                    /* PERFORM CLEAN-UP          */
@RF00072 BAL   R14,EXIT1                                           0074
*        RETURN CODE(RTRNCODE);         /* RETURN TO CALLER          */
         L     R10,RGSVAREA(,R13)                                  0075
         L     R10,RTRNCODE(,R10)                                  0075
         L     R13,4(,R13)                                         0075
         L     R0,@SIZDATD                                         0075
         LR    R1,R11                                              0075
         FREEMAIN R,LV=(0),A=(1)
         LR    R15,R10                                             0075
         L     R14,12(,R13)                                        0075
         LM    R0,R12,20(R13)                                      0075
         BR    R14                                                 0075
*/********************************************************************/
*/*                                                                  */
*/* SEGMENT NAME -                                                   */
*/*      INITIAL                                                     */
*/*                                                                  */
*/* FUNCTION -                                                       */
*/*      THIS ROUTINE INITIALIZES THE ALLOCATE WORKAREA              */
*/*      AND THE GENTRANS PARAMETER LIST.                            */
*/*                                                                  */
*/* OPERATION -                                                      */
*/*      KNOWN FIELDS IN THE WORKAREA ARE FILLED IN AT THIS TIME.    */
*/*      THE GENTRANS PARAMETER LIST IS FILLED IN AS MUCH AS         */
*/*      POSSIBLE.                                                   */
*/*                                                                  */
*/* INPUT -                                                          */
*/*      CPPL                                                        */
*/*                                                                  */
*/* OUTPUT -                                                         */
*/*      WORKAREA AND GENTRANS PARAMETER LIST                        */
*/*                                                                  */
*/********************************************************************/
*                                                                  0076
*    INITIAL: PROC;                                                0076
INITIAL  STM   R14,R12,12(R13)                                     0076
*        TXT = ADDR(ADDUNIT);          /* INIT ADDBLOCK PTR          */
         LA    R10,ADDUNIT                                         0077
         ST    R10,TXT(,R2AWAPTR)                                  0077
*        PTRMSGS = ADDR(IKJEFD31);     /* PTR TO MESSAGE MODULE      */
         L     R10,VCONSD31                                        0078
         ST    R10,PTRMSGS(,R2AWAPTR)                              0078
*        MSGPTR = ADDR(MSGAREA);       /* PTR TO MSG AREA            */
         LA    R10,MSGAREA                                         0079
         ST    R10,MSGPTR(,R2AWAPTR)                               0079
*        MSGAREA = ''X;                /* ZERO MSG AREA      @YM05479*/
         XC    MSGAREA(64),MSGAREA                                 0080
*    /* SET UP GENTRANS PARAMETER LIST                               */
*        GTPLPTR = ADDR(GTPLAREA);     /* PTR TO PARM LIST           */
         LA    R10,GTPLAREA                                        0081
         ST    R10,GTPLPTR(,R2AWAPTR)                              0081
*        KEY2 = KEY1;                  /* COPY KEYLIST               */
         MVC   KEY2(80),KEY1                                       0082
*        GTPLKLST = ADDR(KEYLIST);     /* KEYLIST ADDRESS            */
         LA    R3,KEYLIST                                          0083
         ST    R3,GTPLKLST(,R10)                                   0083
*        GTPLTBLE = ADDR(KEYWDTAB);    /* KEYWORD TABLE ADDRESS      */
         LA    R3,KEYWDTAB                                         0084
         ST    R3,GTPLTBLE(,R10)                                   0084
*        GTPLABLK = ADDR(TEXTCORE);    /* ADDBLOCK ADDRESS           */
         LA    R3,TEXTCORE                                         0085
         ST    R3,GTPLABLK(,R10)                                   0085
*        GTPLOUTA = 0;                 /* OUTPUT ADDRESS -SET TO 0   */
         SLR   R3,R3                                               0086
         ST    R3,GTPLOUTA(,R10)                                   0086
*        ADDNUM = 0;                   /* INIT ADDBLOCK COUNTER      */
         STH   R3,ADDNUM                                           0087
*    END INITIAL;                                                  0088
@EL00002 DS    0H                                                  0088
@EF00002 DS    0H                                                  0088
@ER00002 LM    R14,R12,12(R13)                                     0088
         BR    R14                                                 0088
*                                                                  0089
*                                                                  0089
*/********************************************************************/
*/*                                                                  */
*/* SEGMENT NAME -                                                   */
*/*      COMMSCAN                                                    */
*/*                                                                  */
*/* FUNCTION -                                                       */
*/*      THIS ROUTINE INVOKES THE PARSE SERVICE ROUTINE AND EXAMINES */
*/*      THE RETURN CODE FROM PARSE.                                 */
*/*                                                                  */
*/* OPERATION -                                                      */
*/*      THE PARSE PARAMETER LIST IS SET UP, AND PARSE IS INVOKED.   */
*/*      IF THE RETURN CODE FROM PARSE IS 0, ROUTINE REQTYPE IS      */
*/*      INVOKED. IF THE RETURN CODE IS 4 (NO PROMPT MODE),          */
*/*      TERMINATE. IF RETURN CODE IS 16, ISSUE MESSAGE INDICATING   */
*/*      NOT ENOUGH MAIN STORAGE TO EXECUTE COMMAND, AND TERMINATE.  */
*/*      FOR ANY OTHER NON-ZERO RETURN CODE, ISSUE A MESSAGE GIVING  */
*/*      THE RETURN CODE, AND TERMINATE.                             */
*/*                                                                  */
*/* INPUT -                                                          */
*/*      WORKAREA                                                    */
*/*                                                                  */
*/* OUTPUT -                                                         */
*/*      COMMAND PROCESSED OR                                        */
*/*      ERROR MESSAGE                                               */
*/*                                                                  */
*/* NOTE:  THIS ROUTINE REPRESENTS A DIRECT LINE FUNCTION AND CAN    */
*/*        BE INCORPORATED AS AN INCLUDED CODE SEGMENT WHEN THE      */
*/*        INCLUDE FUNCTION HAS RECEIVED A STANDARD DEFINITION.      */
*/********************************************************************/
*   COMMSCAN:  PROC;                                               0089
COMMSCAN STM   R14,R12,@SA00003                                    0089
*        PPLPTR = ADDR(PPLCORE);       /* PTR TO PPL                 */
         LA    R10,PPLCORE                                         0090
         ST    R10,PPLPTR(,R2AWAPTR)                               0090
*        PPLUPT = CPPLUPT;             /* UPT IN PPL                 */
         L     R3,CPPLPTR(,R2AWAPTR)                               0091
         MVC   PPLUPT(4,R10),CPPLUPT(R3)                           0091
*        PPLECT = CPPLECT;             /* ECT IN PPL                 */
         MVC   PPLECT(4,R10),CPPLECT(R3)                           0092
*        PPLECB = ADDR(COMMECB);       /* ECB IN PPL                 */
         LA    R3,COMMECB(,R2AWAPTR)                               0093
         ST    R3,PPLECB(,R10)                                     0093
*        COMMECB = 0;                  /* SET ECB TO 0               */
         SLR   R10,R10                                             0094
         ST    R10,COMMECB(,R2AWAPTR)                              0094
*        ENTRYCD = 1;                  /* INDICATE MAIN CALL TO PARSE*/
         MVC   ENTRYCD(2,R2AWAPTR),@HW1                            0095
*        R1 = AWAPTR;                  /* WORKAREA PTR IN REG 1      */
         LR    R1,R2AWAPTR                                         0096
*        CALL IKJEFD37;                /* INVOKE PARSE               */
         L     R15,VCONSD37                                        0097
         BALR  R14,R15                                             0097
*        IF RCODESV > 0 THEN           /* IF PARSE FAILED,           */
         L     R10,RCODESV(,R2AWAPTR)                              0098
         LTR   R10,R10                                             0098
         BNP   @RF00098                                            0098
*          DO;                         /* THEN                       */
*            IF AWARSV1 = '1'B THEN    /* IF MSG ALREADY ISSUED,     */
         TM    AWARSV1(R2AWAPTR),B'00000010'                       0100
         BNO   @RF00100                                            0100
*              DO;                     /*                    @YM05479*/
*                AWARSV1 = '0'B;       /* TURN OFF INDICATOR         */
*                RBCODE12 = '1'B;      /* INDICATE ALLOC ERR @YM05479*/
         OI    RBCODE12(R2AWAPTR),B'10000000'                      0103
         NI    AWARSV1(R2AWAPTR),B'11111101'                       0103
*              END;                    /*                    @YM05479*/
*            ELSE                                                  0105
*              DO;                     /* OTHERWISE,                 */
         B     @RC00100                                            0105
@RF00100 DS    0H                                                  0106
*            ENTRYCD = 2;              /* INDICATE PARSE ERROR       */
         MVC   ENTRYCD(2,R2AWAPTR),@HW2                            0106
*            R1 = AWAPTR;              /* PTR TO WORKAREA IN REG 1   */
         LR    R1,R2AWAPTR                                         0107
*            CALL IKJEFD35;            /* ISSUE ERROR MESSAGE        */
         L     R15,VCONSD35                                        0108
         BALR  R14,R15                                             0108
*              END;                                                0109
*          END;                                                    0110
*        ELSE                          /* IF PARSE SUCCESSFUL,       */
*          DO;                                                     0111
         B     @RC00098                                            0111
@RF00098 DS    0H                                                  0112
*            GTPLPDL = PTRPDL;         /* PDL ADDRESS IN GTPL        */
         L     R10,GTPLPTR(,R2AWAPTR)                              0112
         MVC   GTPLPDL(4,R10),PTRPDL(R2AWAPTR)                     0112
*            IF REUSEPDE = 1 THEN                                  #26A
*              REUSE = '1'B;                                       #26A
         L     R3,PTRPDL(,R2AWAPTR)                                @26A
         CLC   REUSEPDE(2,R3),@HW1                                 @26A
         BNE   *+8                                                 @26A
         OI    REUSE(R2AWAPTR),B'00000010'                         @26A
*            GTPLPCL = PPLPCL;         /* PCL ADDRESS IN GTPL        */
         L     R3,PPLPTR(,R2AWAPTR)                                0113
         MVC   GTPLPCL(4,R10),PPLPCL(R3)                           0113
*          END;                                                    0114
*   END  COMMSCAN;                                                 0115
@EL00003 DS    0H                                                  0115
@EF00003 DS    0H                                                  0115
@ER00003 LM    R14,R12,@SA00003                                    0115
         BR    R14                                                 0115
*/********************************************************************/
*/*                                                                  */
*/* SEGMENT NAME -                                                   */
*/*      REQTYPE                                                     */
*/*                                                                  */
*/* FUNCTION -                                                       */
*/*      THIS ROUTINE DETERMINES THE TYPE OF ALLOCATION REQUEST      */
*/*      BEING MADE, AND ROUTES CONTROL TO THE APPROPRIATE           */
*/*      PROCESSOR.                                                  */
*/*                                                                  */
*/* OPERATION -                                                      */
*/*      IF STATUS WAS NOT ENTERED, BUT SPACE PARAMETERS ARE ENTERED */
*/*      THEN THE STATUS IS DEFAULTED TO NEW. IF STATUS NOT ENTERED  */
*/*      BUT DATASET NAME SPECIFIED, STATUS IS DEFAULTED TO OLD.     */
*/*      OTHERWISE, THE USER IS PROMPTED TO ENTER STATUS.  IF THE    */
*/*      REQUEST IS FOR A DUMMY DATA SET, INVOKE DMMYREQ. IF REQUEST */
*/*      IS FOR DATA SET CONCATENATION, INVOKE CONCATRQ. IF REQUEST  */
*/*      IS FOR TERMINAL, INVOKE TERMREQ. IF REQUEST IS FOR SYSOUT   */
*/*      DATA SET, INVOKE SYSOTREQ.  OTHERWISE, DETERMINE DATA SET   */
*/*      STATUS.                                                     */
*/*                                                                  */
*/* INPUT -                                                          */
*/*      PTR TO PDL                                                  */
*/*                                                                  */
*/* OUTPUT -                                                         */
*/*      REQUEST PROCESSED                                           */
*/*                                                                  */
*/* NOTE:  THIS ROUTINE REPRESENTS A DIRECT LINE FUNCTION AND CAN    */
*/*        BE INCORPORATED AS AN INCLUDED CODE SEGMENT WHEN THE      */
*/*        INCLUDE FUNCTION HAS RECEIVED A STANDARD DEFINITION.      */
*/********************************************************************/
*REQTYPE: PROC;                                                    0116
REQTYPE  STM   R14,R12,@SA00004                                    0116
*   IF VLISTCTR > 255 THEN             /* IF OVER 255 VOLSERS        */
         LH    R10,VLISTCTR(,R2AWAPTR)                             0117
         CH    R10,@HW255                                          0117
         BNH   @RF00117                                            0117
*     DO;                              /* THEN                       */
*       MSGID = '112A';                /* SET MESSAGE ID             */
         L     R10,MSGPTR(,R2AWAPTR)                               0119
         MVC   MSGID(4,R10),@CC00805                               0119
*       CALL PUTMSG;                   /* ISSUE ERROR MESSAGE        */
         BAL   R14,PUTMSG                                          0120
*     END;                                                         0121
*   ELSE                               /* ELSE                       */
*     IF MBRPRES = '1'B &              /* IF MEMBER NAME SPECIFIED   */
*        DSNNEXT = PDECMPRE &          /* AND NOT CONCAT REQUEST     */
*        DISPPDE = 2 THEN              /* AND DELETE SPECIFIED       */
         B     @RC00117                                            0122
@RF00117 L     R10,PTRPDL(,R2AWAPTR)                               0122
         TM    MBRPRES(R10),B'10000000'                            0122
         BNO   @RF00122                                            0122
         CLC   DSNNEXT(4,R10),PDECMPRE                             0122
         BNE   @RF00122                                            0122
         CLC   DISPPDE(2,R10),@HW2                                 0122
         BNE   @RF00122                                            0122
*       DO;                            /* THEN                       */
*         ENTRYCD = 5;                 /* SET ENTRY CODE             */
         MVC   ENTRYCD(2,R2AWAPTR),@HW5                            0124
*         R1 = AWAPTR;                 /* WORKAREA PTR IN REG 1      */
         LR    R1,R2AWAPTR                                         0125
*         CALL IKJEFD35;               /* ISSUE ERROR MESSAGE        */
         L     R15,VCONSD35                                        0126
         BALR  R14,R15                                             0126
*       END;                                                       0127
*     ELSE                             /* ELSE                       */
*       DO;                                                        0128
         B     @RC00122                                            0128
@RF00122 DS    0H                                                  0129
*         S99RB = S99RB && S99RB;      /* INIT REQUEST BLOCK         */
         XC    S99RB(20),S99RB                                     0129
*         S99RBLN = LENGTH(S99RB);     /* LENGTH OF RB IN RB         */
         MVI   S99RBLN,X'14'                                       0130
*         S99VERB = S99VRBAL;          /* VERB CODE IN RB            */
         MVI   S99VERB,X'01'                                       0131
*         S99RBPTR = ADDR(S99RB);      /* PTR TO REQUEST BLOCK       */
         LA    R10,S99RB                                           0132
         ST    R10,S99RBPTR                                        0132
*         PTRS99RB = S99RBPTR;         /* PTR TO RB IN WORKAREA      */
         ST    R10,PTRS99RB(,R2AWAPTR)                             0133
*         IF STATSPDE = 0 THEN         /* IF STATUS NOT SPECIFIED,   */
         SLR   R10,R10                                             0134
         L     R3,PTRPDL(,R2AWAPTR)                                0134
         CH    R10,STATSPDE(,R3)                                   0134
         BNE   @RF00134                                            0134
*           DO;                        /* THEN                       */
*             IF BLOKPDE > 0 |         /* IF BLOCK,                  */
*                SPACEPDE > 0 |        /* SPACE, OR                  */
*                DIRPDE > 0 THEN       /* DIR PARAMETERS SPECIFIED,  */
         CH    R10,BLOKPDE(,R3)                                    0136
         BL    @RT00136                                            0136
         CH    R10,SPACEPDE(,R3)                                   0136
         BL    @RT00136                                            0136
         CH    R10,DIRPDE(,R3)                                     0136
         BNL   @RF00136                                            0136
@RT00136 DS    0H                                                  0137
*               STATSPDE = 4;          /* DEFAULT STATUS TO NEW      */
         L     R10,PTRPDL(,R2AWAPTR)                               0137
         MVC   STATSPDE(2,R10),@HW4                                0137
*             ELSE                     /* ELSE                       */
*               IF DSPDE > 0 THEN      /* IF DSNAME SPECIFIED,       */
         B     @RC00136                                            0138
@RF00136 L     R10,PTRPDL(,R2AWAPTR)                               0138
         LH    R3,DSPDE(,R10)                                      0138
         LTR   R3,R3                                               0138
         BNP   @RF00138                                            0138
*                 STATSPDE = 1;        /* DEFAULT STATUS TO OLD      */
         MVC   STATSPDE(2,R10),@HW1                                0139
*               ELSE                   /* ELSE                       */
*                 CALL PRMPTSTS;       /* PROMPT FOR STATUS          */
         B     @RC00138                                            0140
@RF00138 BAL   R14,PRMPTSTS                                        0140
*           END;                                                   0141
@RC00138 DS    0H                                                  0141
@RC00136 DS    0H                                                  0142
*         IF RBCODE12 = '0'B THEN      /* IF STATUS PROMPT SUCCESSFUL*/
@RF00134 TM    RBCODE12(R2AWAPTR),B'10000000'                      0142
         BNZ   @RF00142                                            0142
*           DO;                        /* THEN                       */
*             IF DSPDE = 1 &           /* IF CONCATENATION REQUEST   */
*                DSNNEXT ^= PDECMPRE THEN                          0144
         L     R10,PTRPDL(,R2AWAPTR)                               0144
         CLC   DSPDE(2,R10),@HW1                                   0144
         BNE   @RF00144                                            0144
         CLC   DSNNEXT(4,R10),PDECMPRE                             0144
         BE    @RF00144                                            0144
*               DO;                                                0145
*               R1 = AWAPTR;           /* WORKAREA PTR IN REG 1      */
         LR    R1,R2AWAPTR                                         0146
*                                      /* CALL CONCAT ROUTINE        */
*               CALL IKJEFD34;                                     0147
         L     R15,VCONSD34                                        0147
         BALR  R14,R15                                             0147
*               END;                                               0148
*             ELSE                     /* ELSE                       */
*               DO;                                                0149
         B     @RC00144                                            0149
@RF00144 DS    0H                                                  0150
*               R1 = AWAPTR;           /* WORKAREA PTR IN REG 1      */
         LR    R1,R2AWAPTR                                         0150
*                                      /* CALL OTHER REQUEST ROUTINE */
*               CALL IKJEFD32;                                     0151
         L     R15,VCONSD32                                        0151
         BALR  R14,R15                                             0151
*               END;                                               0152
*           END;                                                   0153
*       END;                                                       0154
*END REQTYPE;                                                      0155
@EL00004 DS    0H                                                  0155
@EF00004 DS    0H                                                  0155
@ER00004 LM    R14,R12,@SA00004                                    0155
         BR    R14                                                 0155
*/********************************************************************/
*/*                                                                  */
*/* SEGMENT NAME -                                                   */
*/*      EXIT1                                                       */
*/*                                                                  */
*/* FUNCTION -                                                       */
*/*      THIS ROUTINE PERFORMS CLEAN-UP BEFORE RETURNING TO TMP.     */
*/*                                                                  */
*/* OPERATION -                                                      */
*/*      IF THE POINTER TO THE PDL IS NON-ZERO, THEN THE PDL STORAGE */
*/*      IS RELEASED. IF ALLOCATE WAS UNSUCCESSFUL, THE STACK IS     */
*/*      FLUSHED AND THE RETURN CODE IS SET TO 12.                   */
*/*                                                                  */
*/* INPUT -                                                          */
*/*      PTR TO PDL                                                  */
*/*      RBCODE12 = 1 - ALLOCATE UNSUCCESSFUL                        */
*/*                                                                  */
*/* OUTPUT -                                                         */
*/*      PDL STORAGE RELEASED, AND                                   */
*/*      STACK FLUSHED,IF NECESSARY                                  */
*/*      RETURN CODE = 0 - SUCCESSFUL                                */
*/*                   12 - UNSUCCESSFUL                              */
*/*                                                                  */
*/* NOTE:  THIS ROUTINE REPRESENTS A DIRECT LINE FUNCTION AND CAN    */
*/*        BE INCORPORATED AS AN INLCUDED CODE SEGMENT WHEN THE      */
*/*        INCLUDE FUNCTION HAS RECEIVED A STANDARD DEFINITION.      */
*/*                                                                  */
*/********************************************************************/
*    EXIT1: PROC;                                                  0156
EXIT1    STM   R14,R12,@SA00005                                    0156
*        IF PTRPDL ^= 0 THEN           /* IF PDL CREATED,            */
         L     R10,PTRPDL(,R2AWAPTR)                               0157
         LTR   R10,R10                                             0157
         BZ    @RF00157                                            0157
*          DO;                                                     0158
*            R3 = ADDR(PTRPDL);        /* PUT ADDRESS IN REG 3       */
         LA    R3,PTRPDL(,R2AWAPTR)                                0159
*            GEN (IKJRLSA (3));        /* RELEASE THE PDL            */
         IKJRLSA (3)
*          END;                                                    0161
*        IF GTPLOUTA ^= 0 THEN         /* IF TEXT CORE TO FREE,      */
@RF00157 L     R10,GTPLPTR(,R2AWAPTR)                              0162
         L     R10,GTPLOUTA(,R10)                                  0162
         LTR   R10,R10                                             0162
         BZ    @RF00162                                            0162
*          DO;                                                     0163
*            R0 = SBPLSIZE;            /* SUPBOOL AND SIZE IN REG 0  */
         L     R0,SBPLSIZE(,R10)                                   0164
*            R1 = GTPLOUTA;            /* ADDRESS IN REG 1           */
         LR    R1,R10                                              0165
*            GEN (FREEMAIN R,LV=(0),A=(1));  /* ISSUE FREEMAIN       */
         FREEMAIN R,LV=(0),A=(1)
*          END;                                                    0167
*        IF RBCODE12 = '1'B THEN       /* IF ALLOCATE FAILED,        */
@RF00162 TM    RBCODE12(R2AWAPTR),B'10000000'                      0168
         BNO   @RF00168                                            0168
*          DO;                         /* FLUSH THE STACK            */
*        GEN DATA;                                                 0170
*/* MOVE LIST FORM INTO GEN DATA AREA                                */
*            GEN (MVC STACKLSD(LISTE-LISTB),STACKLST);             0171
         MVC STACKLSD(LISTE-LISTB),STACKLST
*            STPLPTR = ADDR(STPLAREA);                             0172
         LA    STPLPTR,STPLAREA                                    0172
*            STPLUPT = CPPLUPT;                                    0173
         L     R10,CPPLPTR(,R2AWAPTR)                              0173
         MVC   STPLUPT(4,STPLPTR),CPPLUPT(R10)                     0173
*            STPLECT = CPPLECT;                                    0174
         MVC   STPLECT(4,STPLPTR),CPPLECT(R10)                     0174
*            STPLECB = ADDR(COMMECB);                              0175
         LA    R10,COMMECB(,R2AWAPTR)                              0175
         ST    R10,STPLECB(,STPLPTR)                               0175
*            COMMECB = 0;                                          0176
         SLR   R10,R10                                             0176
         ST    R10,COMMECB(,R2AWAPTR)                              0176
*            R3 = ADDR(STACKLSD);                                  0177
         LA    R3,STACKLSD                                         0177
*            R1 = STPLPTR;                                         0178
         LR    R1,STPLPTR                                          0178
*            GEN (STACK PARM=(R3),DELETE=ALL,MF=(E,(1)));          0179
         STACK PARM=(R3),DELETE=ALL,MF=(E,(1))
*            GEN (TCLEARQ INPUT);                                  0180
         TCLEARQ INPUT
*            RTRNCODE = 12;            /* SET RETURN CODE TO 12      */
         L     R10,RGSVAREA(,R13)                                  0181
         MVC   RTRNCODE(4,R10),@FW12                               0181
*          END;                                                    0182
*        ELSE                          /* ELSE,                      */
*          RTRNCODE = 0;               /* SET RETURN CODE TO 0       */
         B     @RC00168                                            0183
@RF00168 L     R10,RGSVAREA(,R13)                                  0183
         SLR   R15,R15                                             0183
         ST    R15,RTRNCODE(,R10)                                  0183
*   END EXIT1;                                                     0184
@EL00005 DS    0H                                                  0184
@EF00005 DS    0H                                                  0184
@ER00005 LM    R14,R12,@SA00005                                    0184
         BR    R14                                                 0184
*/********************************************************************/
*/*                                                                  */
*/* SEGMENT NAME -                                                   */
*/*      PRMPTSTS                                                    */
*/*                                                                  */
*/* FUNCTION -                                                       */
*/*      THIS ROUTINE PROMPTS THE USER FOR A STATUS WHEN IT IS       */
*/*      REQUIRED.                                                   */
*/*                                                                  */
*/* OPERATION -                                                      */
*/*      PARSE IS INVOKED WITH A SECONDARY PCL AND PDL ADDRESS.  IF  */
*/*      PARSE SUCCESSFUL, SAVE THE PARAMETER ENTERED AND RELEASE    */
*/*      THE SECONDARY PDL.                                          */
*/*                                                                  */
*/* INPUT -                                                          */
*/*      PTR TO PDL                                                  */
*/*                                                                  */
*/* OUTPUT -                                                         */
*/*      PDL OVERLAID WITH PROMPT DATA                               */
*/*                                                                  */
*/* NOTE:  THIS ROUTINE REPRESENTS A DIRECT LINE FUNCITON AND CAN    */
*/*        BE INCORPORATED AS AN INCLUDED CODE SEGMENT WHEN THE      */
*/*        INCLUDE FUNCTION HAS RECEIVED A STANDARD DEFINITION.      */
*/********************************************************************/
*     PRMPTSTS: PROC;                                              0185
PRMPTSTS STM   R14,R12,@SA00006                                    0185
*        ENTRYCD = 2;                  /* INDICATE PROMPT FOR STATUS */
         MVC   ENTRYCD(2,R2AWAPTR),@HW2                            0186
*        R1 = AWAPTR;                  /* WORKAREA PTR IN REG 1      */
         LR    R1,R2AWAPTR                                         0187
*        CALL IKJEFD37;                /* INVOKE PARSE               */
         L     R15,VCONSD37                                        0188
         BALR  R14,R15                                             0188
*        IF RCODESV > 0 THEN           /* IF PARSE FAILED,           */
         L     R10,RCODESV(,R2AWAPTR)                              0189
         LTR   R10,R10                                             0189
         BNP   @RF00189                                            0189
*          DO;                         /* THEN                       */
*            ENTRYCD = 2;              /* INDICATE PARSE ERROR       */
         MVC   ENTRYCD(2,R2AWAPTR),@HW2                            0191
*            R1 = AWAPTR;              /* WROKAREA PTR IN REG 1      */
         LR    R1,R2AWAPTR                                         0192
*            CALL IKJEFD35;            /* ISSUE ERROR MESSAGE        */
         L     R15,VCONSD35                                        0193
         BALR  R14,R15                                             0193
*          END;                                                    0194
*        ELSE                                                      0195
*          DO;                         /* ELSE,                      */
         B     @RC00189                                            0195
@RF00189 DS    0H                                                  0196
*            RFY R5 RSTD;              /* RESTRICT REG 5             */
*            R5 = ADDR(PMPTPDL);       /* PDL ADDRESS IN REG 5       */
         LA    R5,PMPTPDL(,R2AWAPTR)                               0197
*            GEN (IKJRLSA (5));        /* RELEASE PDL                */
         IKJRLSA (5)
*            RFY R5 UNRSTD;            /* UNRESTRICT REG 5           */
*          END;                                                    0200
*    END PRMPTSTS;                                                 0201
@EL00006 DS    0H                                                  0201
@EF00006 DS    0H                                                  0201
@ER00006 LM    R14,R12,@SA00006                                    0201
         BR    R14                                                 0201
*/********************************************************************/
*/*                                                                  */
*/* SEGMENT NAME -                                                   */
*/*      PUTMSG                                                      */
*/*                                                                  */
*/* FUNCTION -                                                       */
*/*      THIS ROUTINE INVOKES IKJEFF02 TO ISSUE MESSAGES             */
*/*                                                                  */
*/* OPERATION -                                                       *
*/*      THE PARAMETER LIST FOR THE MESSAGE PROCESSOR IKJEFF02 IS    */
*/*      COMPLETED. IKJEFF02 IS LINKED TO. AN INDICATOR IS SET       */
*/*      SHOWING ALLOCATE HAS FAILED.                                */
*/*                                                                  */
*/* INPUT -                                                          */
*/*      IKJEFF02 PARAMETER LIST                                     */
*/*                                                                  */
*/* OUTPUT -                                                         */
*/*      MESSAGE ISSUED                                              */
*/*                                                                  */
*/********************************************************************/
*PUTMSG: PROC;                                                     0202
PUTMSG   STM   R14,R12,@SA00007                                    0202
*   LISTPTR = ADDR(MSGCSECT);                                      0203
         L     R10,MSGPTR(,R2AWAPTR)                               0203
         LA    R3,MSGCSECT(,R10)                                   0203
         ST    R3,LISTPTR(,R10)                                    0203
*   MTCPPL = CPPLPTR;                                              0204
         MVC   MTCPPL(4,R10),CPPLPTR(R2AWAPTR)                     0204
*   ECBPTR = ADDR(COMMECB);            /* POINTER TO ECB             */
         LA    R3,COMMECB(,R2AWAPTR)                               0205
         ST    R3,ECBPTR(,R10)                                     0205
*   COMMECB = 0;                       /* INIT ECB TO 0              */
         SLR   R3,R3                                               0206
         ST    R3,COMMECB(,R2AWAPTR)                               0206
*   MTHIGH = '1'B;                     /* HIGH ORDER BIT ON          */
         OI    MTHIGH(R10),B'10000000'                             0207
*   MTPUTLSW = '1'B;                   /* INDICATE PUTLINE           */
         OI    MTPUTLSW(R10),B'01000000'                           0208
*   MSGCSECT = PTRMSGS;                /* ADDRESS OF MSG CSECT       */
*                                                                  0209
         MVC   MSGCSECT(4,R10),PTRMSGS(R2AWAPTR)                   0209
*   VAR1 = DSNPTR;                     /* PTR TO DSNAME VARIABLE     */
         L     R3,PTRPDL(,R2AWAPTR)                                0210
         MVC   VAR1(3,R10),DSNPTR+1(R3)                            0210
*   L1 = DSNLENGH;                     /* LENGTH OF DSNAME VARIABLE  */
         MVC   L1(1,R10),DSNLENGH+1(R3)                            0211
*   R1 = ADDR(MSGTABLE);                                           0212
         LR    R1,R10                                              0212
*   GEN (LINK EP=IKJEFF02);                                        0213
*                                                                  0213
         LINK EP=IKJEFF02
*   RBCODE12 = '1'B;                   /* INDICATE ALLOC FAILED      */
         OI    RBCODE12(R2AWAPTR),B'10000000'                      0214
*END PUTMSG;                                                       0215
@EL00007 DS    0H                                                  0215
@EF00007 DS    0H                                                  0215
@ER00007 LM    R14,R12,@SA00007                                    0215
         BR    R14                                                 0215
*        GENERATE;                                                 0216
         DS 0F
LISTB    EQU *
STACKLST STACK MF=L
LISTE    EQU *
*    END IKJEFD30                                                  0217
*/* THE FOLLOWING INCLUDE STATEMENTS WERE FOUND IN THIS PROGRAM.      *
*/*%INCLUDE SYSLIB  (IKJZT430)                                        *
*/*%INCLUDE SYSLIB  (IKJEFFMT)                                        *
*/*%INCLUDE SYSLIB  (IKJCPPL )                                        *
*/*%INCLUDE SYSLIB  (IKJPPL  )                                        *
*/*%INCLUDE SYSLIB  (IKJSTPL )                                        *
*/*%INCLUDE SYSLIB  (IEFZB4D0)                                        *
*/*%INCLUDE SYSLIB  (IEFZB4D2)                                        *
*/*%INCLUDE SYSLIB  (IKJZB831)                                        *
*;                                                                 0217
@EL00001 L     R13,4(,R13)                                         0217
@EF00001 L     R0,@SIZDATD                                         0217
         LR    R1,R11                                              0217
         FREEMAIN R,LV=(0),A=(1)
@ER00001 L     R14,12(,R13)                                        0217
         LM    R0,R12,20(R13)                                      0217
         BR    R14                                                 0217
@DATA    DC    0H'0'
@HW1     DC    H'1'
@HW2     DC    H'2'
@HW4     DC    H'4'
@HW5     DC    H'5'
@HW255   DC    H'255'
@DATD    DSECT
         DS    0F
@SA00001 DS    18F
@PC00001 DS    1F
@SA00004 DS    15F
@SA00005 DS    15F
@SA00007 DS    15F
@SA00006 DS    15F
@SA00003 DS    0F
         DS    15F
@ZTEMPS  DS    C
@ZTEMPND EQU   *
@ZLEN    EQU   @ZTEMPND-@ZTEMPS
IKJEFD30 CSECT
         DC    0F'0'
@FW12    DC    F'12'
@DATD    DSECT
         DS    0D
S99RBPTR DS    AL4
         ORG   S99RBPTR
S99RBPND DS    BL1
         ORG   S99RBPTR+4
S99TUFP  DS    A
WACORE   DS    CL192
MSGAREA  DS    CL64
PPLCORE  DS    CL28
STPLAREA DS    CL16
TEXTCORE DS    CL96
         ORG   TEXTCORE
ADDNUM   DS    FL2
@NM00012 DS    FL2
ADDTEXT  DS    8AL4
ADDUNIT  DS    CL60
         ORG   TEXTCORE+96
S99RB    DS    CL20
         ORG   S99RB
S99RBLN  DS    CL1
S99VERB  DS    CL1
S99FLAG1 DS    CL2
         ORG   S99FLAG1
S99FLG11 DS    CL1
         ORG   S99FLG11
S99ONCNV DS    BL1
S99NOCNV EQU   S99FLG11+0
S99NOMNT EQU   S99FLG11+0
S99JBSYS EQU   S99FLG11+0
         ORG   S99FLAG1+1
S99FLG12 DS    CL1
         ORG   S99RB+4
S99RSC   DS    CL4
         ORG   S99RSC
S99ERROR DS    CL2
S99INFO  DS    CL2
         ORG   S99RB+8
S99TXTPP DS    AL4
S99RSV01 DS    FL4
S99FLAG2 DS    CL4
         ORG   S99FLAG2
S99FLG21 DS    CL1
         ORG   S99FLG21
S99WTVOL DS    BL1
S99WTDSN EQU   S99FLG21+0
S99NORES EQU   S99FLG21+0
S99WTUNT EQU   S99FLG21+0
S99OFFLN EQU   S99FLG21+0
S99TIONQ EQU   S99FLG21+0
S99CATLG EQU   S99FLG21+0
S99MOUNT EQU   S99FLG21+0
         ORG   S99FLAG2+1
S99FLG22 DS    CL1
         ORG   S99FLG22
S99UDEVT DS    BL1
S99PCINT EQU   S99FLG22+0
         ORG   S99FLAG2+2
S99FLG23 DS    CL1
S99FLG24 DS    CL1
         ORG   S99RB+20
GTPLAREA DS    CL24
KEYLIST  DS    40H
IKJEFD30 CSECT
         DC    0F'0'
@SIZDATD DC    AL1(1)
         DC    AL3(@ENDDATD-@DATD)
VCONSD31 DC    V(IKJEFD31)
VCONSD32 DC    V(IKJEFD32)
VCONSD34 DC    V(IKJEFD34)
VCONSD37 DC    V(IKJEFD37)
VCONSD35 DC    V(IKJEFD35)
         DC    0D'0'
@CC00805 DC    C'112A'
PDECMPRE DC    X'FF000000'
KEYWDTAB DS    CL77
         ORG   KEYWDTAB
STATSTB  DS    CL8
         ORG   STATSTB
STSKEY   DC    X'0004'
STSCNT   DC    AL2(4)
STATS1   DC    X'01'
STATS2   DC    X'08'
STATS3   DC    X'02'
STATS4   DC    X'04'
         ORG   KEYWDTAB+8
DISPTB   DS    CL8
         ORG   DISPTB
DSPKEY   DC    X'0005'
DSPCNT   DC    AL2(4)
DISP1    DC    X'08'
DISP2    DC    X'04'
DISP3    DC    X'02'
DISP4    DC    X'01'
         ORG   KEYWDTAB+16
TRKSTB   DS    CL7
         ORG   TRKSTB
TRKSKEY  DC    X'0007'
TRKSCNT  DC    AL2(3)
TRKS1    DC    X'FF'
TRKS2    DC    X'FF'
TRKS3    DC    X'00'
         ORG   KEYWDTAB+23
CYLSTB   DS    CL8
         ORG   CYLSTB
CYLSKEY  DC    X'0008'
CYLSCNT  DC    AL2(4)
CYLS1    DC    X'FF'
CYLS2    DC    X'FF'
CYLS3    DC    X'FF'
CYLS4    DC    X'00'
         ORG   KEYWDTAB+31
RLSETB   DS    CL5
         ORG   RLSETB
RLSEKEY  DC    X'000D'
RLSECNT  DC    AL2(1)
RLSE1    DC    X'00'
         ORG   KEYWDTAB+36
RNDTB    DS    CL5
         ORG   RNDTB
RNDKEY   DC    X'000F'
RNDCNT   DC    AL2(1)
RND1     DC    X'00'
         ORG   KEYWDTAB+41
PRIVTB   DS    CL5
         ORG   PRIVTB
PRIVKEY  DC    X'0011'
PRIVCNT  DC    AL2(1)
PRIV1    DC    X'00'
         ORG   KEYWDTAB+46
PARLTB   DS    CL6
         ORG   PARLTB
PARLKEY  DC    X'0017'
PARLCNT  DC    AL2(2)
PARL1    DC    X'FF'
PARL2    DC    X'00'
         ORG   KEYWDTAB+52
LABLTB   DS    CL12
         ORG   LABLTB
LABLKEY  DC    X'001E'
LABLCNT  DC    AL2(8)
LABL1    DC    X'02'
LABL2    DC    X'0A'
LABL3    DC    X'40'
LABL4    DC    X'48'
LABL5    DC    X'01'
LABL6    DC    X'04'
LABL7    DC    X'21'
LABL8    DC    X'10'
         ORG   KEYWDTAB+64
DUMMYTB  DS    CL6
         ORG   DUMMYTB
DMYKEY   DC    X'0024'
CMYCNT   DC    AL2(2)
DMY1     DC    X'FF'
DMY2     DC    X'00'
         ORG   KEYWDTAB+70
HOLDTB   DS    CL5
         ORG   HOLDTB
HOLDKEY  DC    X'0059'
HOLDCNT  DC    AL2(1)
HOLD1    DC    X'00'
         ORG   KEYWDTAB+75
ENDTB    DS    CL2
         ORG   ENDTB
ENDKEY   DC    X'FFFF'
         ORG   KEYWDTAB+77
         DC    0H'0'
KYLIST   DC    XL2'0024'
         DC    XL2'0002'
         DC    XL2'0000'
         DC    XL2'0001'
         DC    XL2'0004'
         DC    XL2'0018'
         DC    XL2'0000'
         DC    XL2'0010'
         DC    XL2'0000'
         DC    XL2'0009'
         DC    XL2'0000'
         DC    XL2'000A'
         DC    XL2'0000'
         DC    XL2'000C'
         DC    XL2'0000'
         DC    XL2'002D'
         DC    XL2'0000'
         DC    XL2'0058'
         DC    XL2'0059'
         DC    XL2'0000'
         DC    XL2'0000'
         DC    XL2'0015'
         DC    XL2'0017'
         DC    XL2'0016'
         DC    XL2'0000'
         DC    XL2'001E'
         DC    XL2'0000'
         DC    XL2'001F'
         DC    XL2'0000'
         DC    XL2'0013'
         DC    XL2'0011'
         DC    XL2'0000'
         DC    XL2'0000'
         DC    XL2'0012'
         DC    XL2'000D'
         DC    XL2'0000'
         DC    XL2'000F'
         DC    XL2'0000'
         DC    XL2'0005'
         DC    XL2'0000'
         DC    0F'0'
PATCH    DC    25F'0'
         DC    0D'0'                   END OF CSECT                @26A
@DATD    DSECT
         DS    0F
STACKLSD STACK MF=L
@DATD    DSECT
         ORG   *+1-(*-@DATD)/(*-@DATD) ENSURE DSECT DATA
@ENDDATD EQU   *
IKJEFD30 CSECT
R0       EQU   00                      EQUATES FOR REGISTERS 0-15
R1       EQU   01
R2       EQU   02
R3       EQU   03
R4       EQU   04
R5       EQU   05
R6       EQU   06
R7       EQU   07
R8       EQU   08
R9       EQU   09
R10      EQU   10
R11      EQU   11
R12      EQU   12
R13      EQU   13
R14      EQU   14
R15      EQU   15
STPLPTR  EQU   R4
R2AWAPTR EQU   R2
ALLOCWA  EQU   0
CPPLPTR  EQU   ALLOCWA
PTRPDL   EQU   ALLOCWA+4
GTPLPTR  EQU   ALLOCWA+8
SWITCH   EQU   ALLOCWA+12
RBCODE12 EQU   SWITCH
AWARSV1  EQU   SWITCH
SWITCH2  EQU   ALLOCWA+13
REUSE    EQU   SWITCH2                                             @26A
ENTRYCD  EQU   ALLOCWA+14
TXT      EQU   ALLOCWA+16
PTRS99RB EQU   ALLOCWA+20
RCODESV  EQU   ALLOCWA+24
VLISTCTR EQU   ALLOCWA+28
PTRMSGS  EQU   ALLOCWA+32
VCFLAGS  EQU   ALLOCWA+36
PMPTPDL  EQU   ALLOCWA+40
MSGPTR   EQU   ALLOCWA+48
CMDTWO   EQU   ALLOCWA+60
COMMECB  EQU   ALLOCWA+160
PPLPTR   EQU   ALLOCWA+172
ALLOCPDL EQU   0
DSPDE    EQU   ALLOCPDL+8
STATSPDE EQU   ALLOCPDL+12
BLOKPDE  EQU   ALLOCPDL+16
SPACEPDE EQU   ALLOCPDL+18
DIRPDE   EQU   ALLOCPDL+20
DISPPDE  EQU   ALLOCPDL+46
REUSEPDE EQU   ALLOCPDL+48             (16-BIT NUMBER)             @26A
DSNPDE   EQU   ALLOCPDL+48+4           (WORD ALIGNED)              @26C
DSNPTR   EQU   DSNPDE
DSNLENGH EQU   DSNPDE+4
DSNFLAGS EQU   DSNPDE+6
MBRPTR   EQU   DSNPDE+8
MBRFLAGS EQU   DSNPDE+14
MBRPRES  EQU   MBRFLAGS
PASSPTR  EQU   DSNPDE+16
PASSFLAG EQU   DSNPDE+22
DSNNEXT  EQU   DSNPDE+24
DDNMEPDE EQU   ALLOCPDL+76+4                                       @26C
DDNPTR   EQU   DDNMEPDE
DDNFLAGS EQU   DDNMEPDE+6
CLASPDE  EQU   ALLOCPDL+84+4                                       @26C
CLASPTR  EQU   CLASPDE
CLASFLG  EQU   CLASPDE+6
SERPDE   EQU   ALLOCPDL+92+4                                       @26C
SERPTR   EQU   SERPDE
SERFLAGS EQU   SERPDE+6
BLKPDE   EQU   ALLOCPDL+104+4                                      @26C
BLKPTR   EQU   BLKPDE
BLKFLAGS EQU   BLKPDE+6
ABLKPDE  EQU   ALLOCPDL+112+4                                      @26C
ABLKPTR  EQU   ABLKPDE
ABLKFLG  EQU   ABLKPDE+6
SPACPDE  EQU   ALLOCPDL+120+4                                      @26C
SPACEPTR EQU   SPACPDE
SPACEFLG EQU   SPACPDE+6
SPACPDE2 EQU   ALLOCPDL+128+4                                      @26C
SPACPTR2 EQU   SPACPDE2
SPACFLG2 EQU   SPACPDE2+6
DIRECPDE EQU   ALLOCPDL+136+4                                      @26C
DIRPTR   EQU   DIRECPDE
DIRFLAGS EQU   DIRECPDE+6
ATTRPDE  EQU   ALLOCPDL+144+4                                      @26C
ATTRPTR  EQU   ATTRPDE
ATTRFLAG EQU   ATTRPDE+6
USIDPDE  EQU   ALLOCPDL+152+4                                      @26C
USIDPTR  EQU   USIDPDE
USIDFLAG EQU   USIDPDE+6
UTYPPDE  EQU   ALLOCPDL+160+4                                      @26C
UTYPPTR  EQU   UTYPPDE
UTYPFLAG EQU   UTYPPDE+6
UNCNTPDE EQU   ALLOCPDL+168+4                                      @26C
UCNTPTR  EQU   UNCNTPDE
UCNTFLAG EQU   UNCNTPDE+6
POSTPDE  EQU   ALLOCPDL+180+4                                      @26C
POSPTR   EQU   POSTPDE
POSFLAG  EQU   POSTPDE+6
MXVOLPDE EQU   ALLOCPDL+188+4                                      @26C
MXVOLPTR EQU   MXVOLPDE
MXVOLFLG EQU   MXVOLPDE+6
VOLSQPDE EQU   ALLOCPDL+196+4                                      @26C
VOLSQPTR EQU   VOLSQPDE
VOLSQFLG EQU   VOLSQPDE+6
MSVGPDE  EQU   ALLOCPDL+204+4                                      @26C
MSVGPTR  EQU   MSVGPDE
MSVGFLG  EQU   MSVGPDE+6
DSNBUF   EQU   0
MSGTABLE EQU   0
LISTPTR  EQU   MSGTABLE
TMCTPTR  EQU   MSGTABLE+4
MTCPPL   EQU   TMCTPTR
ECBPTR   EQU   MSGTABLE+8
@NM00007 EQU   MSGTABLE+12
MTHIGH   EQU   @NM00007
MSGCSECT EQU   MSGTABLE+16
SW       EQU   MSGTABLE+20
MTPUTLSW EQU   SW
RETMSG   EQU   MSGTABLE+21
SW2      EQU   MSGTABLE+24
MSGID    EQU   MSGTABLE+40
MTINSRTS EQU   MSGTABLE+44
L1       EQU   MTINSRTS
VAR1     EQU   MTINSRTS+1
L2       EQU   MTINSRTS+4
L3       EQU   MTINSRTS+8
L4       EQU   MTINSRTS+12
RET      EQU   0
CPPL     EQU   0
CPPLUPT  EQU   CPPL+4
CPPLECT  EQU   CPPL+12
RGSVAREA EQU   4
RTRNCODE EQU   16
PPL      EQU   0
PPLUPT   EQU   PPL
PPLECT   EQU   PPL+4
PPLECB   EQU   PPL+8
PPLPCL   EQU   PPL+12
STPL     EQU   0
STPLUPT  EQU   STPL
STPLECT  EQU   STPL+4
STPLECB  EQU   STPL+8
TEXTRET  EQU   0
SBPLSIZE EQU   TEXTRET
S99TUPL  EQU   0
S99TUPTR EQU   S99TUPL
S99TUP   EQU   0
S99TUNIT EQU   0
S99TUENT EQU   S99TUNIT+4
S99TUFLD EQU   0
GTPL     EQU   0
GTPLPDL  EQU   GTPL
GTPLPCL  EQU   GTPL+4
GTPLKLST EQU   GTPL+8
GTPLTBLE EQU   GTPL+12
GTPLABLK EQU   GTPL+16
GTPLOUTA EQU   GTPL+20
ABLKBUF  EQU   0
ATTRBUF  EQU   0
BLKBUF   EQU   0
CLASSBUF EQU   0
DDNBUF   EQU   0
DIRBUF   EQU   0
MBRBUF   EQU   0
MSGECB   EQU   0
MSGTMCT  EQU   0
MSVGBUF  EQU   0
MXVOLBUF EQU   0
PASSBUF  EQU   0
POSTBUF  EQU   0
SERBUF   EQU   0
SPACBUF2 EQU   0
SPACEBUF EQU   0
UCNTBUF  EQU   0
USIDBUF  EQU   0
UTYPBUF  EQU   0
VSEQBUF  EQU   0
TMCTPARM EQU   0
KEY2     EQU   KEYLIST
KEY1     EQU   KYLIST
         AGO   .@UNREFD                START UNREFERENCED COMPONENTS
S99TUPRM EQU   S99TUFLD+2
S99TULEN EQU   S99TUFLD
S99TUPAR EQU   S99TUENT+2
S99TULNG EQU   S99TUENT
S99TUNUM EQU   S99TUNIT+2
S99TUKEY EQU   S99TUNIT
S99TUPND EQU   S99TUP
S99TUPLN EQU   S99TUPTR
STPLSTPB EQU   STPL+12
PPLUWA   EQU   PPL+24
PPLCBUF  EQU   PPL+20
PPLANS   EQU   PPL+16
CPPLPSCB EQU   CPPL+8
CPPLCBUF EQU   CPPL
RETCHAR  EQU   RET+2
RETSIZE  EQU   RET
MSGRTN   EQU   MSGTABLE+60
VAR4     EQU   MTINSRTS+13
HIGHL4   EQU   L4
VAR3     EQU   MTINSRTS+9
HIGHL3   EQU   L3
VAR2     EQU   MTINSRTS+5
HIGHL2   EQU   L2
HIGHL1   EQU   L1
@NM00011 EQU   MSGTABLE+36
@NM00010 EQU   MSGTABLE+32
MTOLDPTR EQU   MSGTABLE+28
@NM00009 EQU   MSGTABLE+25
@NM00008 EQU   SW2
MTPGMSW  EQU   SW2
MTNPLMSW EQU   SW2
MTNOXQSW EQU   SW2
MTDOMSW  EQU   SW2
MT2OLDSW EQU   SW2
MTNHEXSW EQU   SW
MTWTPSW  EQU   SW
MTJOBISW EQU   SW
MTKEY1SW EQU   SW
MTHEXSW  EQU   SW
MTWTOSW  EQU   SW
MTNOIDSW EQU   SW
DSNTERM  EQU   DSNBUF
MSVGRSV2 EQU   MSVGPDE+7
MSVGRSV1 EQU   MSVGFLG
MSVGPRES EQU   MSVGFLG
MSVGLEN  EQU   MSVGPDE+4
VOLRSV2  EQU   VOLSQPDE+7
VOLRSV1  EQU   VOLSQFLG
VOLPRES  EQU   VOLSQFLG
VOLSQLEN EQU   VOLSQPDE+4
MXVRSV2  EQU   MXVOLPDE+7
MXVRSV1  EQU   MXVOLFLG
MXVPRES  EQU   MXVOLFLG
MXVOLLEN EQU   MXVOLPDE+4
POSRSV2  EQU   POSTPDE+7
POSRSV1  EQU   POSFLAG
POSPRES  EQU   POSFLAG
POSLEN   EQU   POSTPDE+4
LABLRSV1 EQU   ALLOCPDL+178+4                                      @26C
LABELPDE EQU   ALLOCPDL+176+4                                      @26C
UCNTRSV2 EQU   UNCNTPDE+7
UCNTRSV1 EQU   UCNTFLAG
UCNTPRES EQU   UCNTFLAG
UCNTLEN  EQU   UNCNTPDE+4
UTYPRSV2 EQU   UTYPPDE+7
UTYPRSV1 EQU   UTYPFLAG
UTYPPRES EQU   UTYPFLAG
UTYPLEN  EQU   UTYPPDE+4
USIDRSV2 EQU   USIDPDE+7
USIDRSV1 EQU   USIDFLAG
USIDPRES EQU   USIDFLAG
USIDLEN  EQU   USIDPDE+4
ATTRRSV2 EQU   ATTRPDE+7
ATTRRSV1 EQU   ATTRFLAG
ATTRPRES EQU   ATTRFLAG
ATTRLEN  EQU   ATTRPDE+4
DIRRSV2  EQU   DIRECPDE+7
DIRRSV1  EQU   DIRFLAGS
DIRPRES  EQU   DIRFLAGS
DIRLEN   EQU   DIRECPDE+4
SPCRSV22 EQU   SPACPDE2+7
SPCRSV12 EQU   SPACFLG2
SPCPRES2 EQU   SPACFLG2
SPACLEN2 EQU   SPACPDE2+4
SPACRSV2 EQU   SPACPDE+7
SPACRSV1 EQU   SPACEFLG
SPACPRES EQU   SPACEFLG
SPACELEN EQU   SPACPDE+4
ABLKRSV2 EQU   ABLKPDE+7
ABLKRSV1 EQU   ABLKFLG
ABLKPRES EQU   ABLKFLG
ABLKLEN  EQU   ABLKPDE+4
BLKRSV2  EQU   BLKPDE+7
BLKRSV1  EQU   BLKFLAGS
BLKPRES  EQU   BLKFLAGS
BLKLEN   EQU   BLKPDE+4
SERNEXT  EQU   SERPDE+8
SERRSV2  EQU   SERPDE+7
SERRSV1  EQU   SERFLAGS
SERPRES  EQU   SERFLAGS
SERLEN   EQU   SERPDE+4
CLASRSV2 EQU   CLASPDE+7
CLASRSV1 EQU   CLASFLG
CLASPRES EQU   CLASFLG
CLASLEN  EQU   CLASPDE+4
DDNRSV2  EQU   DDNMEPDE+7
DDNRSV1  EQU   DDNFLAGS
DDNPRES  EQU   DDNFLAGS
DDNLEN   EQU   DDNMEPDE+4
PASSRSV2 EQU   DSNPDE+23
PASSRSV1 EQU   PASSFLAG
PASSPRES EQU   PASSFLAG
PASSLEN  EQU   DSNPDE+20
MBRRSV2  EQU   DSNPDE+15
MBRRSV1  EQU   MBRFLAGS
MBRLEN   EQU   DSNPDE+12
DSNRSV2  EQU   DSNPDE+7
DSNRSV1  EQU   DSNFLAGS
DSNQUOT  EQU   DSNFLAGS
DSNPRES  EQU   DSNFLAGS
RNDPDE   EQU   ALLOCPDL+44
RLSEPDE  EQU   ALLOCPDL+42
VSEQPDE  EQU   ALLOCPDL+40
PRIVPDE  EQU   ALLOCPDL+38
MAXVPDE  EQU   ALLOCPDL+36
POSPDE   EQU   ALLOCPDL+34
LABLPDE  EQU   ALLOCPDL+32
UCNTPDE  EQU   ALLOCPDL+30
UNITPDE  EQU   ALLOCPDL+28
HOLDPDE  EQU   ALLOCPDL+26
DESTPDE  EQU   ALLOCPDL+24
USINGPDE EQU   ALLOCPDL+22
VOLPDE   EQU   ALLOCPDL+14
FILEPDE  EQU   ALLOCPDL+10
@NM00006 EQU   ALLOCPDL+4
@NM00005 EQU   ALLOCPDL
FIRSTPTR EQU   ALLOCWA+176
NBLKBUF1 EQU   ALLOCWA+164
PDLADDR3 EQU   ALLOCWA+156
PPLTWO   EQU   ALLOCWA+128
CMDTWOBF EQU   CMDTWO+4
CMDTWOOF EQU   CMDTWO+2
CMDTWOLN EQU   CMDTWO
Q        EQU   ALLOCWA+56
P        EQU   ALLOCWA+52
PRCODE   EQU   ALLOCWA+44
@NM00004 EQU   ALLOCWA+38
NCLASBUF EQU   ALLOCWA+37
@NM00003 EQU   VCFLAGS
SPACE2   EQU   VCFLAGS
SPACE1   EQU   VCFLAGS
AVBLK    EQU   VCFLAGS
DSNCTR   EQU   ALLOCWA+30
@NM00002 EQU   SWITCH2
DSEXISTS EQU   SWITCH2
@NM00001 EQU   SWITCH2
FIRSTPDE EQU   SWITCH
ASTRSK   EQU   SWITCH
TERMOPT  EQU   SWITCH
CONT     EQU   SWITCH
PROMPT   EQU   SWITCH
DSOVRLAY EQU   SWITCH
.@UNREFD ANOP                          END UNREFERENCED COMPONENTS
@RC00098 EQU   @EL00003
@RC00122 EQU   @EL00004
@RC00117 EQU   @EL00004
@RF00142 EQU   @EL00004
@RC00144 EQU   @EL00004
@RC00168 EQU   @EL00005
@RC00189 EQU   @EL00006
@RC00100 EQU   @RC00098
@ENDDATA EQU   *
         END   IKJEFD30,(C'PLS1113',0902,85049)
/*
//*
//STEP03  EXEC PGM=IEBGENER
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  *
  IDENTIFY IKJEFD30('ZP60026')
++MOD(IKJEFD31) DISTLIB(AOST4).
/*
//SYSUT2   DD  DSN=&&SMPMCS,DISP=(MOD,PASS)
//SYSIN    DD  DUMMY
//*
//STEP04  EXEC PGM=IFOX00,PARM='OBJECT,NODECK,NOTERM,XREF(SHORT)'
//SYSPRINT DD  SYSOUT=*
//SYSLIB   DD  DSN=SYS1.MACLIB,DISP=SHR
//         DD  DSN=SYS1.SMPMTS,DISP=SHR
//         DD  DSN=SYS1.AMODGEN,DISP=SHR
//SYSUT1   DD  UNIT=SYSALLDA,SPACE=(CYL,10)
//SYSUT2   DD  UNIT=SYSALLDA,SPACE=(CYL,10)
//SYSUT3   DD  UNIT=SYSALLDA,SPACE=(CYL,10)
//SYSGO    DD  DSN=&&SMPMCS,DISP=(MOD,PASS)
//SYSIN    DD  *
**********************************************************************
*                                                                    *
* MODULE -                                                           *
*        IKJEFD31 - MESSAGE CSECT                                    *
*                                                                    *
* FUNCTION -                                                         *
*        THIS MODULE CONTAINS ALL OF THE MESSAGES TO BE ISSUED BY    *
*        THE ALLOCATE CP.  THE MESSAGES ARE CODED USING THE IKJTSMSG *
*        MACRO, AND ARE PROCESSED BY MODULE IKJEFF02.                *
*                                                                    *
* ENTRY POINT -                                                      *
*        IKJEFD31 - IKJEFF02 SEARCHES FOR THE MESSAGE TEXT TO BE     *
*                   PUT OUT TO THE TERMINAL.                         *
*                                                                    *
* INPUT -                                                            *
*        NONE                                                        *
*                                                                    *
* OUTPUT -                                                           *
*        NONE                                                        *
*                                                                    *
* EXTERNAL REFERENCES -                                              *
*        NONE                                                        *
*                                                                    *
* EXITS, NORMAL -                                                    *
*        NONE                                                        *
*                                                                    *
* EXITS, ERROR -                                                     *
*        NONE                                                        *
*                                                                    *
* TABLES/WORKAREAS -                                                 *
*        NONE                                                        *
*                                                                    *
* ATTRIBUTES -                                                       *
*        REENTRANT AND REFRESHABLE                                   *
*                                                                    *
* CHARACTER CODE DEPENDENCY -                                        *
*        EBCDIC CHARACTER SET USED IN ASSEMBLING.  MODULE MUST BE    *
*        REASSEMBLED IF A DIFFERENT CHARACTER SET IS USED.           *
**********************************************************************
IKJEFD31 CSECT
M100A    IKJTSMSG ('IKJ56101I NOT ENOUGH MAIN STORAGE TO EXECUTE COMMANX
               D'),100A
M101A    IKJTSMSG ('IKJ56104I COMMAND SYSTEM ERROR+'),101A
M102A    IKJTSMSG ('IKJ56104I ',,' ERROR CODE ',),102A,101A
M103A    IKJTSMSG ('IKJ56107I ',,' QUANTITY EXCEEDS MAXIMUM ALLOWABLE+'X
               ),103A
M104A    IKJTSMSG ('IKJ56107I MAXIMUM OF 65535 FOR BLOCK AND AVBLOCK, MX
               AXIMUM OF 16777215 FOR SPACE, SPACE INCREMENT AND DIR'),X
               104A,103A
M105A    IKJTSMSG ('IKJ56109I DATA SETS NOT CONCATENATED+'),105A
M106A    IKJTSMSG ('IKJ56109I COMBINING UNLIKE DATA SET ORGANIZATIONS IX
               S INVALID'),106A,105A
M107A    IKJTSMSG ('IKJ56109I NUMBER OF DATA SETS EXCEEDS MAXIMUM, MAXIX
               MUM IS 16 PARTITIONED OR 255 SEQUENTIAL DATA SETS'),107AX
               ,105A
M108A    IKJTSMSG ('IKJ56109I ',,' INVALID WITH CONCATENATION REQUEST')X
               ,108A,105A
M109A    IKJTSMSG ('IKJ56109I FAILURE TO ALLOCATE DATA SET ',),109A,105X
               A
M110A    IKJTSMSG ('IKJ56109I DATA SET ORGANIZATION NOT SEQUENTIAL OR PX
               ARTITIONED'),110A,105A
M111A    IKJTSMSG ('IKJ56109I DATA SET NAME ASTERISK (*) INVALID WITH CX
               ONCATENATION REQUEST'),111A,105A
M112A    IKJTSMSG ('IKJ56110I DATA SET ',,' NOT ALLOCATED, MORE THAN 25X
               5 VOLUME SERIAL NUMBERS SPECIFIED'),112A
M113A    IKJTSMSG ('IKJ56112A ENTER ''FREE'' OR ''END''+-'),113A,114A
M114A    IKJTSMSG ('IKJ56112A ENTER ''FREE'' TO FREE AND RE-ALLOCATE THX
               E FILE ',,', OR ''END'' TO TERMINATE THE COMMAND-'),114A
M115A    IKJTSMSG ('IKJ56113I INVALID RESPONSE, ',),115A
M115B    IKJTSMSG ('IKJ56113I INVALID RESPONSE'),115B
M116A    IKJTSMSG ('IKJ56113A REENTER+-'),116A,117A
M117A    IKJTSMSG ('IKJ56113A ENTER ''FREE'' TO FREE AND RE-ALLOCATE THX
               E FILE ',,', OR ''END'' TO TERMINATE THE COMMAND-'),117A
M118A    IKJTSMSG ('IKJ56111I DATA SET ',,' NOT ALLOCATED, DELETE INVALX
               ID FOR MEMBERS OF PARTITIONED DATA SETS'),118A
         IKJTSMSG
         DC    0D'0'                   END OF CSECT                @26A
         END
/*
//*
//STEP05  EXEC PGM=IEBGENER
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  *
  IDENTIFY IKJEFD31('ZP60026')
++MOD(IKJEFD32) DISTLIB(AOST4).
/*
//SYSUT2   DD  DSN=&&SMPMCS,DISP=(MOD,PASS)
//SYSIN    DD  DUMMY
//*
//STEP06  EXEC PGM=IFOX00,PARM='OBJECT,NODECK,NOTERM,XREF(SHORT)'
//SYSPRINT DD  SYSOUT=*
//SYSLIB   DD  DSN=SYS1.MACLIB,DISP=SHR
//         DD  DSN=SYS1.SMPMTS,DISP=SHR
//         DD  DSN=SYS1.AMODGEN,DISP=SHR
//SYSUT1   DD  UNIT=SYSALLDA,SPACE=(CYL,10)
//SYSUT2   DD  UNIT=SYSALLDA,SPACE=(CYL,10)
//SYSUT3   DD  UNIT=SYSALLDA,SPACE=(CYL,10)
//SYSGO    DD  DSN=&&SMPMCS,DISP=(MOD,PASS)
//SYSIN    DD  *
         TITLE 'ALLOCATION REQUEST PROCESSOR                           *
                        '
***********************************************************************
*  $26   ZP60026  2010-01-23 GREG PRICE: ADD 'REUSE' OPERAND.         *
*                            CHANGES APPLIED TO UZ69512 LEVEL.        *
***********************************************************************
IKJEFD32 CSECT ,                                                   0001
@MAINENT DS    0H                                                  0001
         USING *,R15                                               0001
         B     @PROLOG                                             0001
         DC    AL1(16)                                             0001
         DC    AL1(33)                                             @26C
         DC    C'IKJEFD32  83.336'                                 0001
         DC    CL17' ZP60026 2010.023'                             @26A
         DROP  R15
@PROLOG  STM   R14,R12,12(R13)                                     0001
         BALR  R12,0                                               0001
@PSTART  DS    0H                                                  0001
         USING @PSTART,R12                                         0001
         L     R0,@SIZDATD                                         0001
         GETMAIN  R,LV=(0)
         LR    R11,R1                                              0001
         USING @DATD,R11                                           0001
         ST    R13,@SA00001+4                                      0001
         LM    R0,R1,20(R13)                                       0001
         ST    R11,8(,R13)                                         0001
         LR    R13,R11                                             0001
         XC    @ZTEMPS(@ZLEN),@ZTEMPS
         MVC   @PC00001(4),0(R1)                                   0001
*                                                                  0093
*                                                                  0093
*/********************************************************************/
*/*                                                                  */
*/* SEGMENT NAME -                                                   */
*/*      REQTYPE                                                     */
*/*                                                                  */
*/* FUNCTION -                                                       */
*/*      THIS ROUTINE DETERMINES THE TYPE OF ALLOCATION REQUEST      */
*/*      BEING MADE, AND ROUTES CONTROL TO THE APPROPRIATE           */
*/*      PROCESSOR.                                                  */
*/*                                                                  */
*/* OPERATION -                                                      */
*/*      IF THE REQUEST IS FOR A DUMMY DATA SET, INVOKE DMMYREQ.     */
*/*      IF REQUEST IF FOR A TERMINAL, INVOKE TERMREQ.  IF REQUEST   */
*/*      IF FOR SYSOUT DATA SET, INVOKE SYSOTREQ. OTHERWISE,         */
*/*      DETERMINE DATA SET STATUS AND INVOKE APPROPRIATE ROUTINE.   */
*/*                                                                  */
*/* INPUT -                                                          */
*/*      PTR TO PDL                                                  */
*/*                                                                  */
*/* OUTPUT -                                                         */
*/*      REQUEST PROCESSED                                           */
*/*                                                                  */
*/* NOTE:  THIS ROUTINE REPRESENTS A DIRECT LINE FUNCTION AND CAN    */
*/*        BE INCORPORATED AS AN INCLUDED CODE SEGMENT WHEN THE      */
*/*        INCLUDE FUNCTION HAS RECEIVED A STANDARD DEFINITION.      */
*/********************************************************************/
*                                                                  0093
*REQTYPE:                                                          0093
*   R3AWAPTR=R1;                    /* PTR TO WORKAREA               */
REQTYPE  LR    R3AWAPTR,R1                                         0093
*   INC=ADDNUM+1;                   /* ADDBLOCK SUBSCRIPT            */
         L     R10,GTPLPTR(,R3AWAPTR)                              0094
         L     R10,GTPLABLK(,R10)                                  0094
         LA    R4INC,1                                             0094
         MVC   @TF00001(2),ADDNUM(R10)                             0094
         AH    R4INC,@TF00001                                      0094
*   IF DSPDE=2 THEN                 /* IF DUMMY SPECIFIED,           */
         L     R10,PTRPDL(,R3AWAPTR)                               0095
         CLC   DSPDE(2,R10),@HW2                                   0095
         BNE   @RF00095                                            0095
*     CALL DMMYREQ;                 /* INVOKE DUMMY PROCESSOR        */
         BAL   R14,DMMYREQ                                         0096
*   ELSE                                                           0097
*     IF DSPDE=1&                   /* IF * ENTERED AS               */
*         DSNPRES='1'B&             /* DSNAME,                       */
*         DSNTERM='*' THEN          /* INVOKE TERMINAL               */
         B     @RC00095                                            0097
@RF00095 L     R10,PTRPDL(,R3AWAPTR)                               0097
         CLC   DSPDE(2,R10),@HW1                                   0097
         BNE   @RF00097                                            0097
         TM    DSNPRES(R10),B'10000000'                            0097
         BNO   @RF00097                                            0097
         L     R10,DSNPTR(,R10)                                    0097
         CLI   DSNTERM(R10),C'*'                                   0097
         BNE   @RF00097                                            0097
*       CALL TERMREQ;               /* PROCESSOR                     */
         BAL   R14,TERMREQ                                         0098
*     ELSE                                                         0099
*       IF STATSPDE=5 THEN          /* IF SYSOUT ENTERED,            */
         B     @RC00097                                            0099
@RF00097 L     R10,PTRPDL(,R3AWAPTR)                               0099
         CLC   STATSPDE(2,R10),@HW5                                0099
         BNE   @RF00099                                            0099
*         CALL SYSOTREQ;            /* INVOKE SYSOUT PROC.           */
         BAL   R14,SYSOTREQ                                        0100
*       ELSE                                                       0101
*         DO;                       /*                       @Y30LPKH*/
         B     @RC00099                                            0101
@RF00099 DS    0H                                                  0102
*           IF VOLPDE=2 THEN        /* IF MSVGP SPEC         @Y30LPKH*/
         L     R10,PTRPDL(,R3AWAPTR)                               0102
         CLC   VOLPDE(2,R10),@HW2                                  0102
         BNE   @RF00102                                            0102
*             KEYLIST(8)=DALMSVGP;  /* SET KEY               @Y30LPKH*/
         L     R10,GTPLPTR(,R3AWAPTR)                              0103
         L     R10,GTPLKLST(,R10)                                  0103
         MVC   KEYLIST+14(2,R10),@HWMSVGP                          0103
*           IF STATSPDE=<2 THEN     /* IF STATUS OLD OR SHR          */
@RF00102 L     R10,PTRPDL(,R3AWAPTR)                               0104
         LH    R10,STATSPDE(,R10)                                  0104
         C     R10,@FW2                                            0104
         BH    @RF00104                                            0104
*             CALL OLDDS;           /* INVOKE PROCESSOR              */
         BAL   R14,OLDDS                                           0105
*           ELSE                                                   0106
*             IF STATSPDE=3 THEN    /* STATUS MOD,                   */
         B     @RC00104                                            0106
@RF00104 L     R10,PTRPDL(,R3AWAPTR)                               0106
         CLC   STATSPDE(2,R10),@HW3                                0106
         BNE   @RF00106                                            0106
*               CALL MODDS;         /* INVOKE MOD PROCESSOR          */
         BAL   R14,MODDS                                           0107
*             ELSE                  /* ELSE,                         */
*               CALL NEWDS;         /* INVOKE NEW PROCESSOR          */
         B     @RC00106                                            0108
@RF00106 BAL   R14,NEWDS                                           0108
*         END;                                                     0109
@RC00106 DS    0H                                                  0109
@RC00104 DS    0H                                                  0110
*   IF RBCODE12='1'B THEN           /* IF ERROR OCCURRED,            */
@RC00099 DS    0H                                                  0110
@RC00097 DS    0H                                                  0110
@RC00095 TM    RBCODE12(R3AWAPTR),B'10000000'                      0110
         BNO   @RF00110                                            0110
*     RETCODE=12;                   /* SET RETURN CODE TO 12         */
         L     R10,REGSAVE(,R13)                                   0111
         MVC   RETCODE(4,R10),@FW12                                0111
*   ELSE                            /* ELSE                          */
*     RETCODE=0;                    /* RETURN CODE 0                 */
         B     @RC00110                                            0112
@RF00110 L     R10,REGSAVE(,R13)                                   0112
         SLR   R15,R15                                             0112
         ST    R15,RETCODE(,R10)                                   0112
*   RETURN;                                                        0113
@EL00001 L     R13,4(,R13)                                         0113
@EF00001 L     R0,@SIZDATD                                         0113
         LR    R1,R11                                              0113
         FREEMAIN R,LV=(0),A=(1)
@ER00001 LM    R14,R12,12(R13)                                     0113
         BR    R14                                                 0113
*                                                                  0114
*/********************************************************************/
*/*                                                                  */
*/* SEGMENT NAME -                                                   */
*/*      NEWDS                                                       */
*/*                                                                  */
*/* FUNCTION -                                                       */
*/*      THIS ROUTINE ALLOCATES A DATA SET HAVING A STATUS OF NEW.   */
*/*                                                                  */
*/* OPERATION -                                                      */
*/*      EXTRANEOUS PARAMETERS ARE IGNORED. IF DISPOSITION WAS NOT   */
*/*      SPECIFIED, AND DSNAME WAS NOT SPECIFIED, SET DISPOSITION TO */
*/*      DELETE. ELSE, SET DISPOSITION TO CATALOG. INVOKE IKJEFD33   */
*/*      TO PROCESS SPACE PARAMETERS. SET TRACKS OR CYLINDERS KEY IN */
*/*      KEY LIST, WHICHEVER IS APPROPRIATE. INVOKE GENTRANS. IF     */
*/*      GENTRANS SUCCESSFUL, INVOKE DYNAMIC ALLOCATION.             */
*/*                                                                  */
*/* INPUT -                                                          */
*/*      PTR TO PDL                                                  */
*/*      GENTRANS PARAMETER LIST                                     */
*/*      DYNAMIC ALLOCATION REQUEST BLOCK                            */
*/* OUTPUT -                                                         */
*/*      RBCODE12 = 0 - ALLOCATION SUCCESSFUL                        */
*/*                 1 - FAILURE IN GENTRANS OR ALLOCATION            */
*/*                                                                  */
*/* NOTE:  THIS ROUTINE REPRESENTS A DIRECT LINE FUNCTION AND CAN    */
*/*        BE INCORPORATED AS AN INCLUDED CODE SEGMENT WHEN THE      */
*/*        INCLUDE FUNCTION HAS RECEIVED A STANDARD DEFINITION.      */
*/*                                                                  */
*/*        IF TRACKS WAS SPECIFIED, THE KEY PASSED TO GENTRANS IS    */
*/*        THE TRACKS KEY. OTHERWISE, THE KEY IS FOR CYLINDERS. IF   */
*/*        BLOCK OR AVBLOCK WAS ENTERED, THE CYLINDERS KEY WILL NOT  */
*/*        BE LOOKED AT.                                             */
*/********************************************************************/
*                                                                  0114
*NEWDS:                                                            0114
*   PROC;                                                          0114
         B     @EL00001                                            0114
NEWDS    STM   R14,R12,@SA00002                                    0114
*   I=1;                            /* INITIALIZE SUBSCRIPT          */
*                                                                  0115
         LA    R2I,1                                               0115
*   /*****************************************************************/
*   /*                                                               */
*   /* MARK EXTRANEOUS PARAMETERS NOT ENTERED IN PDL                 */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0116
*   DO WHILE(CONT='0'B);                                           0116
         B     @DE00116                                            0116
@DL00116 DS    0H                                                  0117
*     IF NEWEXT(I)='00'X THEN       /* IF END OF LIST,               */
         LA    R10,NEWEXT-1(R2I)                                   0117
         CLI   0(R10),X'00'                                        0117
         BNE   @RF00117                                            0117
*       CONT='1'B;                  /* EXIT DO-WHILE                 */
         OI    CONT(R3AWAPTR),B'00010000'                          0118
*     ELSE                          /* ELSE,                         */
*       DO;                         /* GET ADDRESS OF PDE            */
         B     @RC00117                                            0119
@RF00117 DS    0H                                                  0120
*         EXTRAN=ADDR(ALLOCPDL)+NEWEXT(I);                         0120
         SLR   R5EXTRAN,R5EXTRAN                                   0120
         IC    R5EXTRAN,NEWEXT-1(R2I)                              0120
         AL    R5EXTRAN,PTRPDL(,R3AWAPTR)                          0120
*         PDEEXT=0;                 /* SET PDE TO 0                  */
         SLR   R10,R10                                             0121
         STH   R10,PDEEXT(,R5EXTRAN)                               0121
*         I=I+1;                    /* INCREMENT SUBSCRIPT           */
         AL    R2I,@FW1                                            0122
*       END;                                                       0123
*   END;                            /* END DO-WHILE                  */
@RC00117 DS    0H                                                  0124
@DE00116 TM    CONT(R3AWAPTR),B'00010000'                          0124
         BZ    @DL00116                                            0124
*   CONT='0'B;                      /* RE-SET SWITCH                 */
         NI    CONT(R3AWAPTR),B'11101111'                          0125
*   IF DISPPDE=0 THEN               /* IF DISP NOT SPECIFIED         */
         L     R10,PTRPDL(,R3AWAPTR)                               0126
         LH    R7,DISPPDE(,R10)                                    0126
         LTR   R7,R7                                               0126
         BNZ   @RF00126                                            0126
*     DO;                           /* AND                           */
*       IF DSPDE=1 THEN             /* IF DSNAME ENTERED,            */
         CLC   DSPDE(2,R10),@HW1                                   0128
         BNE   @RF00128                                            0128
*         DISPPDE=3;                /* DEFAULT CATALOG               */
         MVC   DISPPDE(2,R10),@HW3                                 0129
*       ELSE                        /* IF DSNAME NOT ENTERED         */
*         DISPPDE=2;                /* DEFAULT DELETE                */
         B     @RC00128                                            0130
@RF00128 L     R10,PTRPDL(,R3AWAPTR)                               0130
         MVC   DISPPDE(2,R10),@HW2                                 0130
*     END;                                                         0131
@RC00128 DS    0H                                                  0132
*   R1=R3AWAPTR;                    /* WORKAREA PTR IN REG 1         */
@RF00126 LR    R1,R3AWAPTR                                         0132
*   CALL IKJEFD33;                  /* PROCESS SPACE PARAMETERS      */
         L     R15,VCONSD33                                        0133
         BALR  R14,R15                                             0133
*   IF RBCODE12='0'B THEN           /* IF ERROR OCCURRED,            */
         TM    RBCODE12(R3AWAPTR),B'10000000'                      0134
         BNZ   @RF00134                                            0134
*     DO;                                                          0135
*       INC=ADDNUM+1;               /* SET ADDBLOCK SUBSCRIPT        */
         L     R10,GTPLPTR(,R3AWAPTR)                              0136
         L     R7,GTPLABLK(,R10)                                   0136
         LA    R4INC,1                                             0136
         MVC   @TF00001(2),ADDNUM(R7)                              0136
         AH    R4INC,@TF00001                                      0136
*       IF BLOKPDE=3 THEN           /* IF TRK ENTERED, GET           */
         L     R7,PTRPDL(,R3AWAPTR)                                0137
         CLC   BLOKPDE(2,R7),@HW3                                  0137
         BNE   @RF00137                                            0137
*         KEYLIST(9)=DALTRK;        /* KEY IN KEYLIST                */
         L     R10,GTPLKLST(,R10)                                  0138
         MVC   KEYLIST+16(2,R10),@HWTRK                            0138
*       ELSE                        /* ELSE                          */
*         KEYLIST(9)=DALCYL;        /* CYL KEY IN KEYLIST            */
         B     @RC00137                                            0139
@RF00137 L     R10,GTPLPTR(,R3AWAPTR)                              0139
         L     R10,GTPLKLST(,R10)                                  0139
         MVC   KEYLIST+16(2,R10),@HWCYL                            0139
*       CALL TRANSRTN;              /* TRANSLATE PARAMETERS          */
@RC00137 BAL   R14,TRANSRTN                                        0140
*       IF RBCODE12='0'B THEN       /* IF SUCCESSFUL,                */
         TM    RBCODE12(R3AWAPTR),B'10000000'                      0141
         BNZ   @RF00141                                            0141
*         DO;                                                      0142
*           S99NOCNV='1'B;          /* REQUIRE NEW ALLOCATION        */
         L     R10,PTRS99RB(,R3AWAPTR)
         OI    S99NOCNV(R10),B'01000000'
*           CALL DYNSVC;            /* INVOKE ALLOCATION             */
         BAL   R14,DYNSVC                                          0143
*           IF RCODESV>0 THEN       /* IF ALLOC FAILED,              */
         L     R10,RCODESV(,R3AWAPTR)                              0144
         LTR   R10,R10                                             0144
         BNP   @RF00144                                            0144
*             DO;                   /* AND                           */
*               IF S99ERROR='0410'X THEN/* IF FILE IN USE,           */
         L     R10,PTRS99RB(,R3AWAPTR)                             0146
         CLC   S99ERROR(2,R10),@HEX0410                            0146
         BNE   @RF00146                                            0146
*                 DO;                                              0147
*                   R1=R3AWAPTR;    /* WORKAREA PTR IN REG 1         */
         LR    R1,R3AWAPTR                                         0148
*                   CALL IKJEFD36;  /* PROMPT FOR OPTION             */
         L     R15,VCONSD36                                        0149
         BALR  R14,R15                                             0149
*                 END;                                             0150
*               ELSE                /* OTHER ERROR,                  */
*                 DO;                                              0151
         B     @RC00146                                            0151
@RF00146 DS    0H                                                  0152
*                   RBCODE12='1'B;  /* INDICATE ALLOC FAILED         */
         OI    RBCODE12(R3AWAPTR),B'10000000'                      0152
*                   ENTRYCD=1;      /* CODE FOR DYNALLOC ERR         */
         MVC   ENTRYCD(2,R3AWAPTR),@HW1                            0153
*                   R1=R3AWAPTR;    /* WORKAREA PTR IN REG 1         */
         LR    R1,R3AWAPTR                                         0154
*                   CALL IKJEFD35;  /* ISSUE ERROR MESSAGE           */
         L     R15,VCONSD35                                        0155
         BALR  R14,R15                                             0155
*                 END;                                             0156
*             END;                                                 0157
*         END;                                                     0158
*     END;                                                         0159
*   END NEWDS;                                                     0160
@EL00002 DS    0H                                                  0160
@EF00002 DS    0H                                                  0160
@ER00002 LM    R14,R12,@SA00002                                    0160
         BR    R14                                                 0160
*                                                                  0161
*/********************************************************************/
*/*                                                                  */
*/* SEGMENT NAME -                                                   */
*/*      TERMREQ                                                     */
*/*                                                                  */
*/* FUNCTION -                                                       */
*/*      THIS ROUTINE HANDLES REQUESTS FOR TERMINAL DATA SET         */
*/*      ALLOCATION.                                                 */
*/*                                                                  */
*/* OPERATION -                                                      */
*/*      EXTRANEOUS PARAMETERS ARE MARKED NOT ENTERED IN THE PDL.    */
*/*      IF BLOCK WAS ENTERED INVOKE IKJEFD33 TO SET UP DCB BLOCK    */
*/*      SIZE TEXT UNIT.  SET UP THE TEXT UNIT WHICH                 */
*/*      INDICATES TERMINAL ALLOCATION, AND MARK DATASET NOT         */
*/*      ENTERED IN THE PDL. INVOKE GENTRANS. IF GENTRANS SUCCESSFUL */
*/*      INVOKE DYNAMIC ALLOCATION.                                  */
*/*                                                                  */
*/* INPUT -                                                          */
*/*      PTR TO PDL                                                  */
*/*                                                                  */
*/* OUTPUT -                                                         */
*/*      RBCODE12 = 0 - ALLOCATION SUCCESSFUL                        */
*/*                 1 - FAILURE IN GENTRANS OR ALLOCATION            */
*/*                                                                  */
*/* NOTE:  THIS ROUTINE REPRESENTS A DIRECT LINE FUNCTION AND CAN    */
*/*        BE INCORPORATED AS AN INCLUDED CODE SEGMENT WHEN THE      */
*/*        INCLUDE FUNCTION HAS RECEIVED A STANDARD DEFINITION.      */
*/*                                                                  */
*/*        THE TEXT UNIT SPECIFYING TERMINAL ALLOCATION MUST BE SET  */
*/*        UP SINCE GENTRANS DOES NOT RECOGNIZE A DATA SET NAME OF   */
*/*        ASTERISK (*).                                             */
*/********************************************************************/
*                                                                  0161
*TERMREQ:                                                          0161
*   PROC;                                                          0161
TERMREQ  STM   R14,R12,@SA00003                                    0161
*   I=1;                            /* INITIALIZE SUBSCRIPT          */
*                                                                  0162
         LA    R2I,1                                               0162
*   /*****************************************************************/
*   /*                                                               */
*   /* MARK EXTRANEOUS PARAMETERS NOT ENTERED IN PDL                 */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0163
*   DO WHILE(CONT='0'B);                                           0163
         B     @DE00163                                            0163
@DL00163 DS    0H                                                  0164
*     IF TERMEXT(I)='00'X THEN      /* IF END OF LIST,               */
         LA    R10,TERMEXT-1(R2I)                                  0164
         CLI   0(R10),X'00'                                        0164
         BNE   @RF00164                                            0164
*       CONT='1'B;                  /* EXIT DO-WHILE                 */
         OI    CONT(R3AWAPTR),B'00010000'                          0165
*     ELSE                          /* ELSE,                         */
*       DO;                         /* GET ADDRESS OF PDE            */
         B     @RC00164                                            0166
@RF00164 DS    0H                                                  0167
*         EXTRAN=ADDR(ALLOCPDL)+TERMEXT(I);                        0167
         SLR   R5EXTRAN,R5EXTRAN                                   0167
         IC    R5EXTRAN,TERMEXT-1(R2I)                             0167
         AL    R5EXTRAN,PTRPDL(,R3AWAPTR)                          0167
*         PDEEXT=0;                 /* SET PDE TO 0                  */
         SLR   R10,R10                                             0168
         STH   R10,PDEEXT(,R5EXTRAN)                               0168
*         I=I+1;                    /* INCREMENT SUBSCRIPT           */
         AL    R2I,@FW1                                            0169
*       END;                                                       0170
*   END;                                                           0171
@RC00164 DS    0H                                                  0171
@DE00163 TM    CONT(R3AWAPTR),B'00010000'                          0171
         BZ    @DL00163                                            0171
*   CONT='0'B;                                                     0172
         NI    CONT(R3AWAPTR),B'11101111'                          0172
*   IF BLOKPDE=1 THEN               /* IF BLOCK ENTERED,             */
         L     R10,PTRPDL(,R3AWAPTR)                               0173
         CLC   BLOKPDE(2,R10),@HW1                                 0173
         BNE   @RF00173                                            0173
*     DO;                                                          0174
*       R1=R3AWAPTR;                /* WORKAREA PTR IN REG 1         */
         LR    R1,R3AWAPTR                                         0175
*       CALL IKJEFD33;              /* PROCESS PARM                  */
         L     R15,VCONSD33                                        0176
         BALR  R14,R15                                             0176
*       INC=ADDNUM+1;               /* SET ADDBLOCK SUBSCRIPT        */
         L     R10,GTPLPTR(,R3AWAPTR)                              0177
         L     R10,GTPLABLK(,R10)                                  0177
         LA    R4INC,1                                             0177
         MVC   @TF00001(2),ADDNUM(R10)                             0177
         AH    R4INC,@TF00001                                      0177
*     END;                                                         0178
*   BLOKPDE=0;                      /* MARK BLOCK NOT ENTERED        */
@RF00173 SLR   R10,R10                                             0179
         L     R7,PTRPDL(,R3AWAPTR)                                0179
         STH   R10,BLOKPDE(,R7)                                    0179
*   ADDNUM=ADDNUM+1;                /* INCREMENT ENTRY CTR           */
         L     R5,GTPLPTR(,R3AWAPTR)                               0180
         L     R5,GTPLABLK(,R5)                                    0180
         LA    R2,1                                                0180
         MVC   @TF00001(2),ADDNUM(R5)                              0180
         LH    R15,@TF00001                                        0180
         ALR   R15,R2                                              0180
         ST    R15,@TF00001                                        0180
         MVC   ADDNUM(2,R5),@TF00001+2                             0180
*   ADLKEY=DALTERM;                 /* TERMINAL KEY IN TEXT          */
         L     R15,TXT(,R3AWAPTR)                                  0181
         MVC   ADLKEY(2,R15),@HWTERM                               0181
*   ADLNBR=0;                       /* SET NUMBER ZERO               */
         STH   R10,ADLNBR(,R15)                                    0182
*   ADDTEXT(INC)=TXT;               /* TEXT PTR IN ADDRESS LIST      */
         LR    R14,R4INC                                           0183
         SLA   R14,2                                               0183
         ST    R15,@TF00001                                        0183
         ALR   R5,R14                                              0183
         MVC   ADDTEXT-4(4,R5),@TF00001                            0183
*   TXT=TXT+4;                      /* INCREMENT TEXT PTR            */
         AL    R15,@FW4                                            0184
         ST    R15,TXT(,R3AWAPTR)                                  0184
*   INC=INC+1;                      /* INCREMENT SUBSCRIPT           */
         ALR   R4INC,R2                                            0185
*   DSPDE=0;                        /* MARK DATASET NOT ENTERED      */
         STH   R10,DSPDE(,R7)                                      0186
*   CALL TRANSRTN;                  /* TRANSLATE PARAMETERS          */
         BAL   R14,TRANSRTN                                        0187
*   IF RBCODE12='0'B THEN           /* IF TRANSLATE SUCCESSFUL,      */
         TM    RBCODE12(R3AWAPTR),B'10000000'                      0188
         BNZ   @RF00188                                            0188
*     DO;                           /* THEN                          */
*       CALL DYNSVC;                /* INVOKE DYN ALLOCATION         */
         BAL   R14,DYNSVC                                          0190
*       IF RCODESV>0 THEN           /* IF ALLOCATION FAILED,         */
         L     R10,RCODESV(,R3AWAPTR)                              0191
         LTR   R10,R10                                             0191
         BNP   @RF00191                                            0191
*         DO;                       /* DETERMINE IF FILE IN USE      */
*           IF S99ERROR='0410'X THEN                               0193
         L     R10,PTRS99RB(,R3AWAPTR)                             0193
         CLC   S99ERROR(2,R10),@HEX0410                            0193
         BNE   @RF00193                                            0193
*             DO;                   /* YES -                         */
*               R1=R3AWAPTR;        /* WORKAREA PTR IN REG 1         */
         LR    R1,R3AWAPTR                                         0195
*               CALL IKJEFD36;      /* PROMPT FOR OPTION             */
         L     R15,VCONSD36                                        0196
         BALR  R14,R15                                             0196
*             END;                                                 0197
*           ELSE                    /* NO -                          */
*             DO;                   /* THEN,                         */
         B     @RC00193                                            0198
@RF00193 DS    0H                                                  0199
*               RBCODE12='1'B;      /* INDICATE ALLOCATE FAILED      */
         OI    RBCODE12(R3AWAPTR),B'10000000'                      0199
*               ENTRYCD=1;          /* CODE FOR DYNALLOC ERR         */
         MVC   ENTRYCD(2,R3AWAPTR),@HW1                            0200
*               R1=R3AWAPTR;        /* WORKAREA PTR IN REG 1 ISSUE 0201
*                                      ERROR MESSAGE                 */
         LR    R1,R3AWAPTR                                         0201
*               CALL IKJEFD35;                                     0202
         L     R15,VCONSD35                                        0202
         BALR  R14,R15                                             0202
*             END;                                                 0203
*         END;                                                     0204
*     END;                                                         0205
*   END TERMREQ;                                                   0206
@EL00003 DS    0H                                                  0206
@EF00003 DS    0H                                                  0206
@ER00003 LM    R14,R12,@SA00003                                    0206
         BR    R14                                                 0206
*                                                                  0207
*/********************************************************************/
*/*                                                                  */
*/* SEGMENT NAME -                                                   */
*/*      SYSOTREQ                                                    */
*/*                                                                  */
*/* FUNCTION -                                                       */
*/*      THIS ROUTINE PROCESSES REQUESTS FOR A SYSOUT DATA SET.      */
*/*                                                                  */
*/* OPERATION -                                                      */
*/*      EXTRANEOUS PARAMETERS ARE MARKED NOT ENTERED IN THE PDL.    */
*/*      IF BLOCK IS ENTERED INVOKE IKJEFD33 TO SET UP               */
*/*      THE TEXT UNIT FOR DCB BLOCK SIZE FROM THE BLOCK VALUE       */
*/*      IF NOHOLD WAS ENTERED, MARK THE HOLD PARAMETER              */
*/*      NOT ENTERED IN THE PDL. IF SYSOUT CLASS WAS NOT SPECIFIED,  */
*/*      SET UP THE TEXT UNIT TO INDICATE CLASS DEFAULT AND MARK     */
*/*      THE SYSOUT PARAMETER NOT ENTERED IN THE PDL. INVOKE         */
*/*      GENTRANS TO TRANSLATE THE PARAMETERS. IF GENTRANS SUCCESSFUL*/
*/*      INVOKE DYNAMIC ALLOCATION. IF ALLOCATION FAILED BECAUSE     */
*/*      OF FILE IN USE, INVOKE IKJEFD36 TO PROMPT THE USER FOR      */
*/*      THE OPTION. ELSE, INVOKE IKJEFD35 TO ISSUE AN ERROR MESSAGE */
*/*                                                                  */
*/* INPUT -                                                          */
*/*      PTR TO PDL                                                  */
*/*                                                                  */
*/* OUTPUT -                                                         */
*/*      RBCODE12 = 0 - ALLOCATION SUCCESSFUL                        */
*/*                 1 - ERROR IN GENTRANS OR ALLOCATION              */
*/*                                                                  */
*/* NOTE:  THIS ROUTINE REPRESENTS A DIRECT LINE FUNCTION AND CAN    */
*/*        BE INCORPORATED AS AN INCLUDED CODE SEGMENT WHEN THE      */
*/*        INCLUDE FUNCTION HAS RECEIVED A STANDARD DEFINITION.      */
*/*                                                                  */
*/*        IF THE CLASS SUBFIELD WAS NOT ENTERED, THE TEXT UNIT FOR  */
*/*        SYSOUT MUST BE SET UP SINCE GENTRANS EXPECTS THE SUBFIELD */
*/*        TO BE PRESENT, AND WILL FLAG IT AS AN ERROR IF IT IS NOT. */
*/********************************************************************/
*                                                                  0207
*SYSOTREQ:                                                         0207
*   PROC;                                                          0207
SYSOTREQ STM   R14,R12,@SA00004                                    0207
*   I=1;                            /* INITIALIZE SUBSCRIPT          */
*                                                                  0208
         LA    R2I,1                                               0208
*   /*****************************************************************/
*   /*                                                               */
*   /* MARK EXTRANEOUS PARAMETERS NOT ENTERED IN PDL                 */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0209
*   DO WHILE(CONT='0'B);                                           0209
         B     @DE00209                                            0209
@DL00209 DS    0H                                                  0210
*     IF SYSOTEXT(I)='00'X THEN     /* IF END OF LIST,               */
         LA    R10,SYSOTEXT-1(R2I)                                 0210
         CLI   0(R10),X'00'                                        0210
         BNE   @RF00210                                            0210
*       CONT='1'B;                  /* EXIT DO-WHILE                 */
         OI    CONT(R3AWAPTR),B'00010000'                          0211
*     ELSE                          /* ELSE,                         */
*       DO;                         /* GET ADDRESS OF PDE            */
         B     @RC00210                                            0212
@RF00210 DS    0H                                                  0213
*         EXTRAN=ADDR(ALLOCPDL)+SYSOTEXT(I);                       0213
         SLR   R5EXTRAN,R5EXTRAN                                   0213
         IC    R5EXTRAN,SYSOTEXT-1(R2I)                            0213
         AL    R5EXTRAN,PTRPDL(,R3AWAPTR)                          0213
*         PDEEXT=0;                 /* SET PDE TO 0                  */
         SLR   R10,R10                                             0214
         STH   R10,PDEEXT(,R5EXTRAN)                               0214
*         I=I+1;                    /* INCREMENT SUBSCRIPT           */
         AL    R2I,@FW1                                            0215
*       END;                                                       0216
*   END;                            /* END DO-WHILE                  */
@RC00210 DS    0H                                                  0217
@DE00209 TM    CONT(R3AWAPTR),B'00010000'                          0217
         BZ    @DL00209                                            0217
*   CONT='0'B;                      /* RESET SWITCH                  */
         NI    CONT(R3AWAPTR),B'11101111'                          0218
*   IF SPACEPDE>0|                  /* IF SPACE OR                   */
*       BLOKPDE>0 THEN              /* BLOCK ENTERED THEN            */
         SLR   R10,R10                                             0219
         L     R7,PTRPDL(,R3AWAPTR)                                0219
         CH    R10,SPACEPDE(,R7)                                   0219
         BL    @RT00219                                            0219
         CH    R10,BLOKPDE(,R7)                                    0219
         BNL   @RF00219                                            0219
@RT00219 DS    0H                                                  0220
*     DO;                                                          0220
*       R1=R3AWAPTR;                /* WORKAREA PTR IN REG 1         */
         LR    R1,R3AWAPTR                                         0221
*       CALL IKJEFD33;              /* PROCESS SPACE                 */
         L     R15,VCONSD33                                        0222
         BALR  R14,R15                                             0222
*       INC=ADDNUM+1;               /* SET ADDBLOCK SUBSCRIPT        */
         L     R10,GTPLPTR(,R3AWAPTR)                              0223
         L     R7,GTPLABLK(,R10)                                   0223
         LA    R4INC,1                                             0223
         MVC   @TF00001(2),ADDNUM(R7)                              0223
         AH    R4INC,@TF00001                                      0223
*       IF BLOKPDE=3 THEN                                          0224
         L     R7,PTRPDL(,R3AWAPTR)                                0224
         CLC   BLOKPDE(2,R7),@HW3                                  0224
         BNE   @RF00224                                            0224
*         KEYLIST(9)=DALTRK;                                       0225
         L     R10,GTPLKLST(,R10)                                  0225
         MVC   KEYLIST+16(2,R10),@HWTRK                            0225
*       ELSE                                                       0226
*         KEYLIST(9)=DALCYL;                                       0226
         B     @RC00224                                            0226
@RF00224 L     R10,GTPLPTR(,R3AWAPTR)                              0226
         L     R10,GTPLKLST(,R10)                                  0226
         MVC   KEYLIST+16(2,R10),@HWCYL                            0226
*     END;                                                         0227
@RC00224 DS    0H                                                  0228
*   IF RBCODE12='0'B THEN                                          0228
@RF00219 TM    RBCODE12(R3AWAPTR),B'10000000'                      0228
         BNZ   @RF00228                                            0228
*     DO;                                                          0229
*       IF HOLDPDE=2 THEN           /* IF NOHOLD SPECIFIED,          */
         L     R10,PTRPDL(,R3AWAPTR)                               0230
         CLC   HOLDPDE(2,R10),@HW2                                 0230
         BNE   @RF00230                                            0230
*         HOLDPDE=0;                /* MARK NOT ENTERED              */
         SLR   R7,R7                                               0231
         STH   R7,HOLDPDE(,R10)                                    0231
*       IF CLASPRES='0'B THEN       /* IF CLASS NOT ENTERED,         */
@RF00230 L     R10,PTRPDL(,R3AWAPTR)                               0232
         TM    CLASPRES(R10),B'10000000'                           0232
         BNZ   @RF00232                                            0232
*         DO;                       /* THEN                          */
*           ADDNUM=ADDNUM+1;        /* INCREMENT TEXT COUNTER        */
         L     R7,GTPLPTR(,R3AWAPTR)                               0234
         L     R7,GTPLABLK(,R7)                                    0234
         LA    R5,1                                                0234
         MVC   @TF00001(2),ADDNUM(R7)                              0234
         LH    R2,@TF00001                                         0234
         ALR   R2,R5                                               0234
         ST    R2,@TF00001                                         0234
         MVC   ADDNUM(2,R7),@TF00001+2                             0234
*           ADLKEY=DALSYSOU;        /* SET UP TEXT - KEY,            */
         L     R2,TXT(,R3AWAPTR)                                   0235
         MVC   ADLKEY(2,R2),@HWSYSOU                               0235
*           ADLNBR=0;               /* AND NUMBER                    */
         SLR   R15,R15                                             0236
         STH   R15,ADLNBR(,R2)                                     0236
*           ADDTEXT(INC)=TXT;       /* PUT ADDRESS IN LIST           */
         LR    R14,R4INC                                           0237
         SLA   R14,2                                               0237
         ST    R2,@TF00001                                         0237
         ALR   R7,R14                                              0237
         MVC   ADDTEXT-4(4,R7),@TF00001                            0237
*           TXT=TXT+4;              /* INCREMENT TEXT PTR            */
         AL    R2,@FW4                                             0238
         ST    R2,TXT(,R3AWAPTR)                                   0238
*           INC=INC+1;              /* INCREMENT ADDR SUBSCRIPT      */
         ALR   R4INC,R5                                            0239
*           STATSPDE=0;             /* MARK SYSOUT NOT ENTERED       */
         STH   R15,STATSPDE(,R10)                                  0240
*         END;                                                     0241
*       CALL TRANSRTN;              /* TRANSLATE PARAMETERS          */
@RF00232 BAL   R14,TRANSRTN                                        0242
*       IF RBCODE12='0'B THEN       /* IF TRANSLATE SUCCESSFUL,      */
         TM    RBCODE12(R3AWAPTR),B'10000000'                      0243
         BNZ   @RF00243                                            0243
*         DO;                       /* THEN                          */
*           CALL DYNSVC;            /* INVOKE DYN ALLOCATION         */
         BAL   R14,DYNSVC                                          0245
*           IF RCODESV>0 THEN       /* IF ALLOCATION FAILED,         */
         L     R10,RCODESV(,R3AWAPTR)                              0246
         LTR   R10,R10                                             0246
         BNP   @RF00246                                            0246
*             DO;                   /* AND IF FILE IN USE,           */
*               IF S99ERROR='0410'X THEN                           0248
         L     R10,PTRS99RB(,R3AWAPTR)                             0248
         CLC   S99ERROR(2,R10),@HEX0410                            0248
         BNE   @RF00248                                            0248
*                 DO;                                              0249
*                   R1=R3AWAPTR;    /* WORKAREA PTR IN REG 1         */
         LR    R1,R3AWAPTR                                         0250
*                   CALL IKJEFD36;  /* PROMPT USER FOR OPTION        */
         L     R15,VCONSD36                                        0251
         BALR  R14,R15                                             0251
*                 END;                                             0252
*               ELSE                                               0253
*                 DO;               /* OTHERWISE,                    */
         B     @RC00248                                            0253
@RF00248 DS    0H                                                  0254
*                   RBCODE12='1'B;  /* INDICATE ALLOC FAILED         */
         OI    RBCODE12(R3AWAPTR),B'10000000'                      0254
*                   ENTRYCD=1;      /* CODE FOR DYNALLOC ERR         */
         MVC   ENTRYCD(2,R3AWAPTR),@HW1                            0255
*                   R1=R3AWAPTR;    /* WORKAREA PTR IN REG 1         */
         LR    R1,R3AWAPTR                                         0256
*                   CALL IKJEFD35;  /* INVOKE DAIRFAIL               */
         L     R15,VCONSD35                                        0257
         BALR  R14,R15                                             0257
*                 END;                                             0258
*             END;                                                 0259
*         END;                                                     0260
*     END;                                                         0261
*   END SYSOTREQ;                                                  0262
@EL00004 DS    0H                                                  0262
@EF00004 DS    0H                                                  0262
@ER00004 LM    R14,R12,@SA00004                                    0262
         BR    R14                                                 0262
*                                                                  0263
*/********************************************************************/
*/*                                                                  */
*/* SEGMENT NAME -                                                   */
*/*      DMMYREQ                                                     */
*/*                                                                  */
*/* FUNCTION -                                                       */
*/*      THIS ROUTINE ALLOCATES A DUMMY DATA SET.                    */
*/*                                                                  */
*/* OPERATION -                                                      */
*/*      EXTRANEOUS PARAMETERS ARE MARKED NOT ENTERED IN THE PDL.    */
*/*      IF BLOCK WAS ENTERED INVOKE IKJEFD33 TO SET UP THE          */
*/*      TEXT UNIT FOR DCB BLOCK SIZE. INVOKE GENTRANS. IF GENTRANS  */
*/*      SUCCESSFUL, INVOKE DYNAMIC ALLOCATION. IF DYNAMIC           */
*/*      ALLOCATION FAILED BECAUSE OF FILE IN USE, CALL ROUTINE      */
*/*      IKJEFD36 TO PROMPT FOR OPTION. ELSE, INVOKE IKJEFD35        */
*/*      TO ISSUE ERROR MESSAGE.                                     */
*/*                                                                  */
*/* INPUT -                                                          */
*/*      PTR TO PDL                                                  */
*/*                                                                  */
*/* OUTPUT -                                                         */
*/*      RBCODE12 = 0 - ALLOCATION SUCCESSFUL                        */
*/*                 1 - FAILURE IN GENTRANS OR ALLOCATION            */
*/*                                                                  */
*/* NOTE:  THIS ROUTINE REPRESENTS A DIRECT LINE FUNCTION AND CAN    */
*/*        BE INCORPORATED AS AN INCLUDED CODE SEGMENT WHEN THE      */
*/*        INCLUDE FUNCTION HAS RECEIVED A STANDARD DEFINITION.      */
*/********************************************************************/
*                                                                  0263
*DMMYREQ:                                                          0263
*   PROC;                                                          0263
DMMYREQ  STM   R14,R12,@SA00005                                    0263
*   I=1;                            /* INITIALIZE SUBSCRIPT          */
*                                                                  0264
         LA    R2I,1                                               0264
*   /*****************************************************************/
*   /*                                                               */
*   /* MARK EXTRANEOUS PARAMETERS NOT ENTERED IN PDL                 */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0265
*   DO WHILE(CONT='0'B);                                           0265
         B     @DE00265                                            0265
@DL00265 DS    0H                                                  0266
*     IF DMMYEXT(I)='00'X THEN      /* IF END OF LIST,               */
         LA    R10,DMMYEXT-1(R2I)                                  0266
         CLI   0(R10),X'00'                                        0266
         BNE   @RF00266                                            0266
*       CONT='1'B;                  /* EXIT DO-WHILE                 */
         OI    CONT(R3AWAPTR),B'00010000'                          0267
*     ELSE                          /* ELSE,                         */
*       DO;                         /* GET ADDRESS OF PDE            */
         B     @RC00266                                            0268
@RF00266 DS    0H                                                  0269
*         EXTRAN=ADDR(ALLOCPDL)+DMMYEXT(I);                        0269
         SLR   R5EXTRAN,R5EXTRAN                                   0269
         IC    R5EXTRAN,DMMYEXT-1(R2I)                             0269
         AL    R5EXTRAN,PTRPDL(,R3AWAPTR)                          0269
*         PDEEXT=0;                 /* SET PDE TO 0                  */
         SLR   R10,R10                                             0270
         STH   R10,PDEEXT(,R5EXTRAN)                               0270
*         I=I+1;                    /* INCREMENT SUBSCRIPT           */
         AL    R2I,@FW1                                            0271
*       END;                                                       0272
*   END;                            /* END DO-WHILE                  */
@RC00266 DS    0H                                                  0273
@DE00265 TM    CONT(R3AWAPTR),B'00010000'                          0273
         BZ    @DL00265                                            0273
*   CONT='0'B;                      /* RESET SWITCH                  */
         NI    CONT(R3AWAPTR),B'11101111'                          0274
*   IF BLOKPDE=1 THEN               /* IF BLOCK ENTERED,             */
         L     R10,PTRPDL(,R3AWAPTR)                               0275
         CLC   BLOKPDE(2,R10),@HW1                                 0275
         BNE   @RF00275                                            0275
*     DO;                                                          0276
*       R1=R3AWAPTR;                /* WORKAREA PTR IN REG 1         */
         LR    R1,R3AWAPTR                                         0277
*       CALL IKJEFD33;              /* PROCESS PARAMETER             */
         L     R15,VCONSD33                                        0278
         BALR  R14,R15                                             0278
*       INC=ADDNUM+1;               /* SET ADDBLOCK SUBSCRIPT        */
         L     R10,GTPLPTR(,R3AWAPTR)                              0279
         L     R10,GTPLABLK(,R10)                                  0279
         LA    R4INC,1                                             0279
         MVC   @TF00001(2),ADDNUM(R10)                             0279
         AH    R4INC,@TF00001                                      0279
*     END;                                                         0280
*   IF RBCODE12='0'B THEN           /* IF NO ERROR OCCURRED,         */
@RF00275 TM    RBCODE12(R3AWAPTR),B'10000000'                      0281
         BNZ   @RF00281                                            0281
*     DO;                           /* THEN,                         */
*       BLOKPDE=0;                  /* MARK BLOCK NOT ENTERED        */
         L     R10,PTRPDL(,R3AWAPTR)                               0283
         SLR   R7,R7                                               0283
         STH   R7,BLOKPDE(,R10)                                    0283
*       CALL TRANSRTN;              /* TRANSLATE PARAMETERS          */
         BAL   R14,TRANSRTN                                        0284
*       IF RBCODE12='0'B THEN       /* IF TRANSLATE SUCCESSFUL,      */
         TM    RBCODE12(R3AWAPTR),B'10000000'                      0285
         BNZ   @RF00285                                            0285
*         DO;                       /* THEN                          */
*           CALL DYNSVC;            /* INVOKE DYN ALLOCATION         */
         BAL   R14,DYNSVC                                          0287
*           IF RCODESV>0 THEN       /* IF ALLOCATION FAILED,         */
         L     R10,RCODESV(,R3AWAPTR)                              0288
         LTR   R10,R10                                             0288
         BNP   @RF00288                                            0288
*             DO;                   /* THEN IF FILE IN USE,          */
*               IF S99ERROR='0410'X THEN                           0290
         L     R10,PTRS99RB(,R3AWAPTR)                             0290
         CLC   S99ERROR(2,R10),@HEX0410                            0290
         BNE   @RF00290                                            0290
*                 DO;                                              0291
*                   R1=R3AWAPTR;    /* WORKAREA PTR IN REG 1 PROMPT
*                                      FOR OPTION                    */
         LR    R1,R3AWAPTR                                         0292
*                   CALL IKJEFD36;                                 0293
         L     R15,VCONSD36                                        0293
         BALR  R14,R15                                             0293
*                 END;                                             0294
*               ELSE                                               0295
*                 DO;               /* OTHER ERROR -                 */
         B     @RC00290                                            0295
@RF00290 DS    0H                                                  0296
*                   RBCODE12='1'B;  /* SET ALLOC FAILED INDICATOR    */
         OI    RBCODE12(R3AWAPTR),B'10000000'                      0296
*                   ENTRYCD=1;      /* SET ENTRY CODE                */
         MVC   ENTRYCD(2,R3AWAPTR),@HW1                            0297
*                   R1=R3AWAPTR;    /* WORKAREA PTR IN REG 1 ISSUE 0298
*                                      ERROR MESSAGE                 */
         LR    R1,R3AWAPTR                                         0298
*                   CALL IKJEFD35;                                 0299
         L     R15,VCONSD35                                        0299
         BALR  R14,R15                                             0299
*                 END;                                             0300
*             END;                                                 0301
*         END;                                                     0302
*     END;                                                         0303
*   END DMMYREQ;                                                   0304
@EL00005 DS    0H                                                  0304
@EF00005 DS    0H                                                  0304
@ER00005 LM    R14,R12,@SA00005                                    0304
         BR    R14                                                 0304
*                                                                  0305
*/********************************************************************/
*/*                                                                  */
*/* SEGMENT NAME -                                                   */
*/*      MODDS                                                       */
*/*                                                                  */
*/* FUNCTION -                                                       */
*/*      THIS ROUTINE HANDLES REQUESTS FOR ALLOCATION OF MOD DATA    */
*/*      SETS.                                                       */
*/*                                                                  */
*/* OPERATION -                                                      */
*/*      IF DATA SET NAME NOT ENTERED, PROMPT THE USER. MARK         */
*/*      EXTRANEOUS PARAMETERS NOT ENTERED IN PDL. IF DISPOSITION    */
*/*      NOT ENTERED, DETERMINE WHETHER DEFAULT SHOULD BE KEEP OR    */
*/*      CATALOG.  IF SPACE PARAMETERS HAVE BEEN ENTERED,            */
*/*      INVOKE IKJEFD33 TO PROCESS THEM. SET APPROPRIATE            */
*/*      TRACKS OR CYLINDERS KEY IN KEY LIST. INVOKE GENTRANS        */
*/*      TO TRANSLATE PARAMETERS. IF GENTRANS SUCCESSFUL, INVOKE     */
*/*      DYNAMIC ALLOCATION. IF ALLOCATION FAILED, DETERMINE         */
*/*      IF FILE IN USE. IF SO, PROMPT THE USER TO SPECIFY THE       */
*/*      OPTION. ELSE, ISSUE ERROR MESSAGE.                          */
*/*                                                                  */
*/* INPUT -                                                          */
*/*      PTR TO PDL                                                  */
*/*                                                                  */
*/* OUTPUT -                                                         */
*/*      RBCODE12 = 0 - ALLOCATION SUCCESSFUL                        */
*/*                 1 - ERROR IN GENTRANS OR ALLOCATION              */
*/*                                                                  */
*/* NOTE:  THIS ROUTINE REPRESENTS A DIRECT LINE FUNCTION AND CAN    */
*/*        BE INCORPORATED AS AN INCLUDED CODE SEGMENT WHEN THE      */
*/*        INCLUDE FUNCTION HAS RECEIVED A STANDARD DEFINITION.      */
*/********************************************************************/
*                                                                  0305
*MODDS:                                                            0305
*   PROC;                                                          0305
MODDS    STM   R14,R12,@SA00006                                    0305
*   IF DSPDE=0 THEN                 /* IF DSN NOT ENTERED,           */
         L     R10,PTRPDL(,R3AWAPTR)                               0306
         LH    R10,DSPDE(,R10)                                     0306
         LTR   R10,R10                                             0306
         BNZ   @RF00306                                            0306
*     DO;                           /* THEN                          */
*       CALL PRMPDSN;               /* PROMPT FOR DSNAME             */
         BAL   R14,PRMPDSN                                         0308
*       IF RBCODE12='0'B THEN                                      0309
         TM    RBCODE12(R3AWAPTR),B'10000000'                      0309
         BNZ   @RF00309                                            0309
*         DO;                                                      0310
*           DSPDE=1;                /* MARK DS KEYWORD ENTERED       */
         L     R10,PTRPDL(,R3AWAPTR)                               0311
         MVC   DSPDE(2,R10),@HW1                                   0311
*           DSOVRLAY='1'B;          /* INDICATE PDL TO BE FREED      */
         OI    DSOVRLAY(R3AWAPTR),B'01000000'                      0312
*           SAVDSPDL=PMPTPDL;       /* SAVE PDL PTR          @YM04998*/
*                                                                  0313
         L     R7,PMPTPDL(,R3AWAPTR)                               0313
         ST    R7,SAVDSPDL                                         0313
*           /*********************************************************/
*           /*                                                       */
*           /* DETERMINE LENGTH OF PDE TO BE COPIED, AND OVERLAY     */
*           /* ORIGINAL PDE WITH PROMPT PDE.                         */
*           /*                                                       */
*           /*********************************************************/
*                                                                  0314
*           MOVELEN=ADDR(DDNMEPDE)-ADDR(DSNPDE);                   0314
         LA    MOVELEN,28                                          0314
*           DSNPDE(1:MOVELEN)=DSNAMPDE(1:MOVELEN);                 0315
         LR    R15,MOVELEN                                         0315
         BCTR  R15,0                                               0315
         EX    R15,@SM01509                                        0315
*         END;                                                     0316
*     END;                                                         0317
@RF00309 DS    0H                                                  0318
*   IF RBCODE12='0'B THEN           /* IF NO ERROR FOUND,            */
@RF00306 TM    RBCODE12(R3AWAPTR),B'10000000'                      0318
         BNZ   @RF00318                                            0318
*     DO;                           /* CONTINUE -                    */
*       I=1;                        /* INITIALIZE SUBSCRIPT          */
*                                                                  0320
         LA    R2I,1                                               0320
*       /*************************************************************/
*       /*                                                           */
*       /* MARK EXTRANEOUS PARAMETERS NOT ENTERED IN PDL             */
*       /*                                                           */
*       /*************************************************************/
*                                                                  0321
*       DO WHILE(CONT='0'B);                                       0321
         B     @DE00321                                            0321
@DL00321 DS    0H                                                  0322
*         IF NEWEXT(I)='00'X THEN   /* IF END OF LIST,               */
         LA    R10,NEWEXT-1(R2I)                                   0322
         CLI   0(R10),X'00'                                        0322
         BNE   @RF00322                                            0322
*           CONT='1'B;              /* EXIT DO-WHILE                 */
         OI    CONT(R3AWAPTR),B'00010000'                          0323
*         ELSE                      /* ELSE,                         */
*           DO;                     /* GET ADDRESS OF PDE            */
         B     @RC00322                                            0324
@RF00322 DS    0H                                                  0325
*             EXTRAN=ADDR(ALLOCPDL)+NEWEXT(I);                     0325
         SLR   R5EXTRAN,R5EXTRAN                                   0325
         IC    R5EXTRAN,NEWEXT-1(R2I)                              0325
         AL    R5EXTRAN,PTRPDL(,R3AWAPTR)                          0325
*             PDEEXT=0;             /* SET PDE TO 0                  */
         SLR   R10,R10                                             0326
         STH   R10,PDEEXT(,R5EXTRAN)                               0326
*             I=I+1;                /* INCREMENT SUBSCRIPT           */
         AL    R2I,@FW1                                            0327
*           END;                                                   0328
*       END;                        /* END DO-WHILE                  */
@RC00322 DS    0H                                                  0329
@DE00321 TM    CONT(R3AWAPTR),B'00010000'                          0329
         BZ    @DL00321                                            0329
*       CONT='0'B;                  /* RESET SWITCH                  */
         NI    CONT(R3AWAPTR),B'11101111'                          0330
*       CALL CHKDISP;               /* DEFAULT               @YM02616*/
         BAL   R14,CHKDISP                                         0331
*       IF RBCODE12='0'B THEN                                      0332
         TM    RBCODE12(R3AWAPTR),B'10000000'                      0332
         BNZ   @RF00332                                            0332
*         DO;                                                      0333
*           R1=R3AWAPTR;            /* WORKAREA PTR IN REG 1         */
         LR    R1,R3AWAPTR                                         0334
*           CALL IKJEFD33;          /* PROCESS SPACE PARAMETERS      */
         L     R15,VCONSD33                                        0335
         BALR  R14,R15                                             0335
*           INC=ADDNUM+1;           /* SET ADDBLOCK SUBSCRIPT        */
         L     R10,GTPLPTR(,R3AWAPTR)                              0336
         L     R7,GTPLABLK(,R10)                                   0336
         LA    R4INC,1                                             0336
         MVC   @TF00001(2),ADDNUM(R7)                              0336
         AH    R4INC,@TF00001                                      0336
*           IF BLOKPDE=3 THEN       /* IF TRACKS ENTERED,            */
         L     R7,PTRPDL(,R3AWAPTR)                                0337
         CLC   BLOKPDE(2,R7),@HW3                                  0337
         BNE   @RF00337                                            0337
*             KEYLIST(9)=DALTRK;    /* INDICATE IN KEYLIST           */
         L     R10,GTPLKLST(,R10)                                  0338
         MVC   KEYLIST+16(2,R10),@HWTRK                            0338
*           ELSE                    /* ELSE                          */
*             KEYLIST(9)=DALCYL;    /* INDICATE CYL IN KEYLIST       */
         B     @RC00337                                            0339
@RF00337 L     R10,GTPLPTR(,R3AWAPTR)                              0339
         L     R10,GTPLKLST(,R10)                                  0339
         MVC   KEYLIST+16(2,R10),@HWCYL                            0339
*           CALL TRANSRTN;          /* TRANSLATE PARAMETERS          */
@RC00337 BAL   R14,TRANSRTN                                        0340
*           IF RBCODE12='0'B THEN   /* IF TRANSLATE SUCCESSFUL       */
         TM    RBCODE12(R3AWAPTR),B'10000000'                      0341
         BNZ   @RF00341                                            0341
*             DO;                   /* THEN                          */
*               CALL DYNSVC;        /* INVOKE DYN ALLOCATION         */
         BAL   R14,DYNSVC                                          0343
*               IF RCODESV>0 THEN   /* IF RETURN CODE NON-ZERO       */
         L     R10,RCODESV(,R3AWAPTR)                              0344
         LTR   R10,R10                                             0344
         BNP   @RF00344                                            0344
*                 DO;               /* THEN DETERMINE IF FILE IN USE */
*                   IF S99ERROR='0410'X THEN                       0346
         L     R10,PTRS99RB(,R3AWAPTR)                             0346
         CLC   S99ERROR(2,R10),@HEX0410                            0346
         BNE   @RF00346                                            0346
*                     DO;           /* YES -                         */
*                       R1=R3AWAPTR; /* WORKAREA PTR IN REG 1 PROMPT
*                                      FOR OPTION                    */
         LR    R1,R3AWAPTR                                         0348
*                       CALL IKJEFD36;                             0349
         L     R15,VCONSD36                                        0349
         BALR  R14,R15                                             0349
*                     END;                                         0350
*                   ELSE                                           0351
*                     DO;           /* NO -                          */
         B     @RC00346                                            0351
@RF00346 DS    0H                                                  0352
*                       RBCODE12='1'B;/* INDICATE ALLOC FAILED       */
         OI    RBCODE12(R3AWAPTR),B'10000000'                      0352
*                       ENTRYCD=1;  /* SET ENTRY CODE                */
         MVC   ENTRYCD(2,R3AWAPTR),@HW1                            0353
*                       R1=R3AWAPTR;  /* WORKAREA PTR IN REG 1     0354
*                                      ISSUE ERROR MESSAGE           */
         LR    R1,R3AWAPTR                                         0354
*                       CALL IKJEFD35;                             0355
         L     R15,VCONSD35                                        0355
         BALR  R14,R15                                             0355
*                     END;                                         0356
*                 END;                                             0357
@RC00346 DS    0H                                                  0358
*             END;                                                 0358
@RF00344 DS    0H                                                  0359
*           IF DSOVRLAY='1'B THEN   /* IF PDL TO FREE,               */
@RF00341 TM    DSOVRLAY(R3AWAPTR),B'01000000'                      0359
         BNO   @RF00359                                            0359
*             DO;                   /* THEN                          */
*               DSOVRLAY='0'B;      /* TURN OFF INDICATOR            */
         NI    DSOVRLAY(R3AWAPTR),B'10111111'                      0361
*               RFY                                                0362
*                 R5 RSTD;          /* RESTRICT REG 5                */
*               R5=ADDR(SAVDSPDL);  /* PDL ADDR IN REG 5     @YM04998*/
         LA    R5,SAVDSPDL                                         0363
*               GEN(IKJRLSA (5));   /* RELEASE PDL                   */
         IKJRLSA (5)
*               RFY                                                0365
*                 R5 UNRSTD;        /* UNRESTRICT REG 5              */
*             END;                                                 0366
*         END;                                                     0367
*     END;                                                         0368
*   END MODDS;                                                     0369
@EL00006 DS    0H                                                  0369
@EF00006 DS    0H                                                  0369
@ER00006 LM    R14,R12,@SA00006                                    0369
         BR    R14                                                 0369
*                                                                  0370
*/********************************************************************
*                                                                    */
*/*                                                           @YM02616
*                                                                    */
*/* SEGMENT NAME -                                            @YM02616
*                                                                    */
*/*      CHKDISP                                              @YM02616
*                                                                    */
*/*                                                           @YM02616
*                                                                    */
*/* FUNCTION -                                                @YM02616
*                                                                    */
*/*      THIS ROUTINE DETERMINES THE DEFAULT DISPOSITION      @YM02616
*                                                                    */
*/*      FOR MOD DATA SETS.                                   @YM02616
*                                                                    */
*/*                                                           @YM02616
*                                                                    */
*/* OPERATION -                                               @YM02616
*                                                                    */
*/*      IF VOLUME WAS SPECIFIED, THE DEFAULT DISPOSITION IS  @YM02616
*                                                                    */
*/*      KEEP. OTHERWISE, INVOKE DAIR WITH ENTRY CODE '04' TO @YM02616
*                                                                    */
*/*      DETERMINE IF THE DATA SET EXISTS. IF SO, THE DEFAULT @YM02616
*                                                                    */
*/*      DISPOSITION IS KEEP. IF NOT, THE DEFAULT DISPOSITION @YM02616
*                                                                    */
*/*      IS CATALOG.                                          @YM02616
*                                                                    */
*/*                                                           @YM02616
*                                                                    */
*/* INPUT -                                                   @YM02616
*                                                                    */
*/*      PTR TO PDL                                           @YM02616
*                                                                    */
*/*                                                           @YM02616
*                                                                    */
*/* OUTPUT -                                                  @YM02616
*                                                                    */
*/*      DEFUALT DISPOSITION SET IN PDL                       @YM02616
*                                                                    */
*/*                                                           @YM02616
*                                                                    */
*/* NOTE:  THIS ROUTINE REPRESENTS A DIRECT LINE FUNCTION     @YM02616
*                                                                    */
*/*        AND CAN BE INCORPORATED AS AN INCLUDED CODE        @YM02616
*                                                                    */
*/*        SEGMENT WHEN THE INCLUDE FUNCTION HAS RECEIVED A   @YM02616
*                                                                    */
*/*        STANDARD DEFINITION.                               @YM02616
*                                                                    */
*/********************************************************************
*                                                                    */
*                                                                  0370
*CHKDISP:                                                          0370
*   PROC;                                                          0370
CHKDISP  STM   R14,R12,@SA00007                                    0370
*   DAPLPTR=ADDR(DAPLAREA);         /* PTR TO DAPL           @YM02616*/
         LA    DAPLPTR,DAPLAREA                                    0371
*   DAPLUPT=CPPLUPT;                /* UPT IN DAPL           @YM02616*/
         L     R10,CPPLPTR(,R3AWAPTR)                              0372
         L     R15,CPPLUPT(,R10)                                   0372
         ST    R15,DAPLUPT(,DAPLPTR)                               0372
*   DAPLECT=CPPLECT;                /* ECT IN DAPL           @YM02616*/
         L     R15,CPPLECT(,R10)                                   0373
         ST    R15,DAPLECT(,DAPLPTR)                               0373
*   DAPLECB=ADDR(COMMECB);          /* ECB IN DAPL           @YM02616*/
         LA    R15,COMMECB(,R3AWAPTR)                              0374
         ST    R15,DAPLECB(,DAPLPTR)                               0374
*   DAPLPSCB=CPPLPSCB;              /* PSCB IN DAPL          @YM02616*/
         L     R10,CPPLPSCB(,R10)                                  0375
         ST    R10,DAPLPSCB(,DAPLPTR)                              0375
*   DAPB0PTR=ADDR(DAIRAREA);        /* PTR TO DAPB04         @YM02616
*                                      ZERO DAPB04           @YM02616*/
         LA    DAPB0PTR,DAIRAREA                                   0376
*   DAIRAREA=DAIRAREA&&DAIRAREA;                                   0377
         XC    DAIRAREA(16),DAIRAREA                               0377
*   DAIRDSBF=' ';                   /* BLANK DSN BUFFER      @YM02616*/
         MVI   DAIRDSBF+1,C' '                                     0378
         MVC   DAIRDSBF+2(44),DAIRDSBF+1                           0378
         MVI   DAIRDSBF,C' '                                       0378
*   DA04CD='0004'X;                 /* FUNCTION CODE         @YM02616*/
         MVC   DA04CD(2,DAPB0PTR),@HEX0004                         0379
*   DAIRDSLN=DSNLENGH;              /* LENGTH OF DSNAME      @YM02616
*                                      MOVE DSN TO BUFFER    @YM02616*/
         L     R10,PTRPDL(,R3AWAPTR)                               0380
         LH    R15,DSNLENGH(,R10)                                  0380
         ST    R15,@TF00001                                        0380
         MVC   DAIRDSLN(2),@TF00001+2                              0380
*   DAIRDSN(1:DSNLENGH)=DSNBUF(1:DSNLENGH);                        0381
         BCTR  R15,0                                               0381
         L     R10,DSNPTR(,R10)                                    0381
         EX    R15,@SM01512                                        0381
*   DAIRDSPT=ADDR(DAIRDSBF);        /* GET DSNAME ADDRESS    @YM02616*/
         LA    DAIRDSPT,DAIRDSBF                                   0382
*   DA04PDSN=DAIRDSPT;              /* PTR TO DSNAME         @YM02616*/
         ST    DAIRDSPT,DA04PDSN(,DAPB0PTR)                        0383
*   DAPLDAPB=DAPB0PTR;              /* PTR TO DAPB04         @YM02616*/
         ST    DAPB0PTR,DAPLDAPB(,DAPLPTR)                         0384
*   R1=DAPLPTR;                     /* PTR IN REG 1          @YM02616
*                                      INVOKE DAIR           @Y30LPKH*/
         LR    R1,DAPLPTR                                          0385
*   DO;                             /* CALLTSSR EP(IKJDAIR)          */
*     RESPECIFY                                                    0387
*       GPR01P RSTD;                                               0387
*     IF CVTDAIR=0 THEN                                            0388
         L     R10,CVTPTR                                          0388
         L     R10,CVTDAIR-CVT(,R10)                               0388
         LTR   R10,R10                                             0388
         BNZ   @RF00388                                            0388
*       GEN(LINK EP=IKJDAIR);                                      0389
         LINK EP=IKJDAIR
*     ELSE                                                         0390
*       CALL CVTDAIR;                                              0390
         B     @RC00388                                            0390
@RF00388 L     R10,CVTPTR                                          0390
         L     R15,CVTDAIR-CVT(,R10)                               0390
         BALR  R14,R15                                             0390
*     RESPECIFY                                                    0391
*       GPR01P UNRSTD;                                             0391
@RC00388 DS    0H                                                  0392
*   END;                                                           0392
*   RCODESV=R15;                    /* SAVE DAIR RETURN COD@YM02616  */
         ST    R15,RCODESV(,R3AWAPTR)                              0393
*   IF RCODESV=0 THEN               /* IF DAIR SUCCESSFUL    @OZ05962*/
         L     R10,RCODESV(,R3AWAPTR)                              0394
         LTR   R10,R10                                             0394
         BNZ   @RF00394                                            0394
*     DO;                           /* THEN                  @OZ05962*/
*       IF DA04CAT='1'B|            /* IF DS IN CATALOG      @YM02616*/
*           DA04DSE='1'B THEN       /* OR IN DSE THEN        @YM02616*/
         TM    DA04CAT(DAPB0PTR),B'00000110'                       0396
         BZ    @RF00396                                            0396
*         DSEXISTS='1'B;            /* SHOW DATASET EXISTS   @OZ05962*/
         OI    DSEXISTS(R3AWAPTR),B'00100000'                      0397
*       IF DISPPDE=0 THEN           /* IF DISP NOT SPEC      @OZ05962*/
@RF00396 L     R10,PTRPDL(,R3AWAPTR)                               0398
         LH    R2,DISPPDE(,R10)                                    0398
         LTR   R2,R2                                               0398
         BNZ   @RF00398                                            0398
*         DO;                       /* THEN                  @OZ05962*/
*           IF VOLPDE=1|            /* IF VOL SPECIFIED      @OZ05962*/
*               DSEXISTS='1'B THEN  /* OR DATASET OLD        @OZ05962*/
         CLC   VOLPDE(2,R10),@HW1                                  0400
         BE    @RT00400                                            0400
         TM    DSEXISTS(R3AWAPTR),B'00100000'                      0400
         BNO   @RF00400                                            0400
@RT00400 DS    0H                                                  0401
*             DISPPDE=1;            /* DEFAULT DISP TO KEEP  @OZ05962*/
         L     R10,PTRPDL(,R3AWAPTR)                               0401
         MVC   DISPPDE(2,R10),@HW1                                 0401
*           ELSE                    /* OTHERWISE             @OZ05962*/
*             DISPPDE=3;            /* DEFAULT DISP CATLG    @OZ05962*/
         B     @RC00400                                            0402
@RF00400 L     R10,PTRPDL(,R3AWAPTR)                               0402
         MVC   DISPPDE(2,R10),@HW3                                 0402
*         END;                      /*                       @OZ05962*/
*     END;                          /*                       @OZ05962*/
*   ELSE                            /* IF DAIR FAILED        @OZ05962*/
*     DO;                           /* THEN                  @OZ05962*/
         B     @RC00394                                            0405
@RF00394 DS    0H                                                  0406
*       IF(RCODESV^=8)|             /* IF NOT CATLG ERROR    @OZ05962*/
*           (RCODESV=8&DA04CTRC^=8) THEN/*                   @OZ05962*/
         LA    R10,8                                               0406
         L     R5,RCODESV(,R3AWAPTR)                               0406
         CR    R5,R10                                              0406
         BNE   @RT00406                                            0406
         CR    R5,R10                                              0406
         BNE   @RF00406                                            0406
         CH    R10,DA04CTRC(,DAPB0PTR)                             0406
         BE    @RF00406                                            0406
@RT00406 DS    0H                                                  0407
*         DO;                       /* INVOKE DAIRFAIL       @OZ05962*/
*           RBCODE12='1'B;          /* INDICATE ERROR        @YM02616
*                                      GET ADDR OF DAPB      @YM02616*/
         OI    RBCODE12(R3AWAPTR),B'10000000'                      0408
*           DFDAPLP=ADDR(DAPB04);   /* GET ADDR OF RET CODE@YM02616  */
****     ST    DAPB0PTR,DFDAPLP                                    0409
         LA    R10,DAPLAREA                                        0409
         ST    R10,DFDAPLP                                         0409
*           DFRCP=ADDR(RCODESV);    /* GET ADDR OF MSG RTN   @YM02616*/
         LA    R10,RCODESV(,R3AWAPTR)                              0410
         ST    R10,DFRCP                                           0410
*           DFJEFF02=ADDR(ADMSG);   /* GET ADDR OF ID        @YM02616*/
         LA    R10,ADMSG                                           0411
         ST    R10,DFJEFF02                                        0411
*           DFIDP=ADDR(CALLERNO);                                  0412
         LA    R10,CALLERNO                                        0412
         ST    R10,DFIDP                                           0412
*           GENERATE;                                              0413
*                                      /* LOAD DAIRFAIL RTN   @YM02616*
                     LOAD EP=IKJEFF18
*           R1=ADDR(DFPARMS);                                      0414
         LA    R1,DFPARMS                                          0414
*           CALL R0;                /* INVOKE DAIRFAIL       @YM02616*/
         LR    R15,R0                                              0415
         BALR  R14,R15                                             0415
*           GENERATE;                                              0416
*                                      /* DELETE DAIRFAIL RTN @YM02616*
                     DELETE EP=IKJEFF18
*         END;                      /*                       @YM02616*/
****    ELSE                        /* DS NOT CATALOGED      @ZA10491*/
*         DISPPDE=3;                /* DEFAULT DISP=CATLG    @ZA10491*/
****     B     @RC00406                                            0418
@RF00406 L     R10,PTRPDL(,R3AWAPTR)                               0418
         LH    R2,DISPPDE(,R10)     LOAD CURRENT DISPOSITION
         LTR   R2,R2                ALREADY SET?
         BNZ   @EL00007             YES, DO NOT OVERLAY
         MVC   DISPPDE(2,R10),@HW3                                 0418
*     END;                                                         0419
*   END CHKDISP;                                                   0420
@EL00007 DS    0H                                                  0420
@EF00007 DS    0H                                                  0420
@ER00007 LM    R14,R12,@SA00007                                    0420
         BR    R14                                                 0420
*                                                                  0421
*/********************************************************************/
*/*                                                                  */
*/* SEGMENT NAME -                                                   */
*/*      OLDDS                                                       */
*/*                                                                  */
*/* FUNCTION -                                                       */
*/*      THIS ROUTINE PROCESSES REQUESTS FOR AN OLD DATA SET.        */
*/*                                                                  */
*/* OPERATION -                                                      */
*/*      IF DSNAME IS NOT SPECIFIED, THE USER IS PROMPTED TO ENTER   */
*/*      A DSNAME. EXTRANEOUS PARAMETERS ARE MARKED NOT ENTERED IN   */
*/*      THE PDL. IF DISPOSITION WAS OMITTED, INDICATE KEEP IN THE   */
*/*      PDL. INVOKE GENTRANS TO TRANSLATE THE PARAMETERS. IF        */
*/*      GENTRANS SUCCESSFUL, INVOKE DYNAMIC ALLOCATION. IF DYNAMIC  */
*/*      ALLOCATION INDICATES FILE IN USE, GO TO PROMPT THE USER     */
*/*      FOR THE OPTION. IF DATA SET COULD NOT BE FOUND, PROMPT FOR  */
*/*      A NEW DSNAME, AND RE-INVOKE DYNAMIC ALLOCATION.             */
*/*                                                                  */
*/* INPUT -                                                          */
*/*      PTR TO PDL                                                  */
*/*                                                                  */
*/* OUTPUT -                                                         */
*/*      RBCODE12 = 0 - ALLOCATION SUCCESSFUL                        */
*/*                 1 - ERROR IN GENTRANS OR ALLOCATION              */
*/*                                                                  */
*/* NOTE:  THIS ROUTINE REPRESENTS A DIRECT LINE FUNCTION AND CAN    */
*/*        BE INCORPORATED AS AN INCLUDED CODE SEGMENT WHEN THE      */
*/*        INCLUDE FUNCTION HAS RECEIVED A STANDARD DEFINITION.      */
*/********************************************************************/
*                                                                  0421
*OLDDS:                                                            0421
*   PROC;                                                          0421
OLDDS    STM   R14,R12,@SA00008                                    0421
*   IF DSPDE=0 THEN                 /* IF DSN NOT ENTERED,           */
         L     R10,PTRPDL(,R3AWAPTR)                               0422
         LH    R10,DSPDE(,R10)                                     0422
         LTR   R10,R10                                             0422
         BNZ   @RF00422                                            0422
*     DO;                           /* THEN                          */
*       CALL PRMPDSN;               /* PROMPT FOR NEW DSNAME         */
         BAL   R14,PRMPDSN                                         0424
*       IF RBCODE12='0'B THEN       /* IF PROMPT SUCCESSFUL,         */
         TM    RBCODE12(R3AWAPTR),B'10000000'                      0425
         BNZ   @RF00425                                            0425
*         DO;                       /* THEN,                         */
*           DSPDE=1;                /* INDICATE DSN ENTERED          */
         L     R10,PTRPDL(,R3AWAPTR)                               0427
         MVC   DSPDE(2,R10),@HW1                                   0427
*           DSOVRLAY='1'B;          /* INDICATE PDE OVERLAID         */
*                                                                  0428
         OI    DSOVRLAY(R3AWAPTR),B'01000000'                      0428
*           /*********************************************************/
*           /*                                                       */
*           /* DETERMINE LENGTH OF PDE TO BE COPIED, AND OVERLAY     */
*           /* ORIGINAL PDE WITH PROMPT PDE.                         */
*           /*                                                       */
*           /*********************************************************/
*                                                                  0429
*           MOVELEN=ADDR(DDNMEPDE)-ADDR(DSNPDE);                   0429
         LA    MOVELEN,28                                          0429
*           DSNPDE(1:MOVELEN)=DSNAMPDE(1:MOVELEN);                 0430
         LR    R7,MOVELEN                                          0430
         BCTR  R7,0                                                0430
         L     R5,PMPTPDL(,R3AWAPTR)                               0430
         EX    R7,@SM01515                                         0430
*         END;                                                     0431
*     END;                                                         0432
@RF00425 DS    0H                                                  0433
*   IF RBCODE12='0'B THEN           /* IF NO ERROR FOUND,            */
@RF00422 TM    RBCODE12(R3AWAPTR),B'10000000'                      0433
         BNZ   @RF00433                                            0433
*     DO;                           /* CONTINUE                      */
*       I=1;                        /* INITIALIZE SUBSCRIPT          */
*                                                                  0435
         LA    R2I,1                                               0435
*       /*************************************************************/
*       /*                                                           */
*       /* MARK EXTRANEOUS PARAMETERS NOT ENTERED IN PDL             */
*       /*                                                           */
*       /*************************************************************/
*                                                                  0436
*       DO WHILE(CONT='0'B);                                       0436
         B     @DE00436                                            0436
@DL00436 DS    0H                                                  0437
*         IF OLDEXT(I)='00'X THEN   /* IF END OF LIST,               */
         LA    R10,OLDEXT-1(R2I)                                   0437
         CLI   0(R10),X'00'                                        0437
         BNE   @RF00437                                            0437
*           CONT='1'B;              /* EXIT DO-WHILE                 */
         OI    CONT(R3AWAPTR),B'00010000'                          0438
*         ELSE                      /* ELSE,                         */
*           DO;                     /* GET ADDRESS OF PDE            */
         B     @RC00437                                            0439
@RF00437 DS    0H                                                  0440
*             EXTRAN=ADDR(ALLOCPDL)+OLDEXT(I);                     0440
         SLR   R5EXTRAN,R5EXTRAN                                   0440
         IC    R5EXTRAN,OLDEXT-1(R2I)                              0440
         AL    R5EXTRAN,PTRPDL(,R3AWAPTR)                          0440
*             PDEEXT=0;             /* SET PDE TO 0                  */
         SLR   R10,R10                                             0441
         STH   R10,PDEEXT(,R5EXTRAN)                               0441
*             I=I+1;                /* INCREMENT SUBSCRIPT           */
         AL    R2I,@FW1                                            0442
*           END;                                                   0443
*       END;                        /* END DO-WHILE                  */
@RC00437 DS    0H                                                  0444
@DE00436 TM    CONT(R3AWAPTR),B'00010000'                          0444
         BZ    @DL00436                                            0444
*       CONT='0'B;                  /* RESET SWITCH                  */
         NI    CONT(R3AWAPTR),B'11101111'                          0445
*       IF DISPPDE=0 THEN           /* IF DISP NOT ENTERED,          */
         L     R10,PTRPDL(,R3AWAPTR)                               0446
         LH    R7,DISPPDE(,R10)                                    0446
         LTR   R7,R7                                               0446
         BNZ   @RF00446                                            0446
*         DISPPDE=1;                /* INDICATE DEFAULT OF KEEP      */
         MVC   DISPPDE(2,R10),@HW1                                 0447
*       CALL TRANSRTN;              /* TRANSLATE PARAMETERS          */
@RF00446 BAL   R14,TRANSRTN                                        0448
*       IF RBCODE12='0'B THEN       /* IF TRANSLATE SUCCESSFUL       */
         TM    RBCODE12(R3AWAPTR),B'10000000'                      0449
         BNZ   @RF00449                                            0449
*         DO;                       /* THEN                          */
*           IF DSOVRLAY='1'B THEN   /* IF 2ND PDL TO FREE,           */
         TM    DSOVRLAY(R3AWAPTR),B'01000000'                      0451
         BNO   @RF00451                                            0451
*             DO;                   /* THEN                          */
*               DSOVRLAY='0'B;      /* TURN OFF INDICATOR            */
         NI    DSOVRLAY(R3AWAPTR),B'10111111'                      0453
*               RFY                                                0454
*                 R5 RSTD;          /* RESTRICT REG 5                */
*               R5=ADDR(PMPTPDL);   /* PDL ADDRESS IN REG 5          */
         LA    R5,PMPTPDL(,R3AWAPTR)                               0455
*               GEN(IKJRLSA (5));   /* RELEASE PDL                   */
         IKJRLSA (5)
*               RFY                                                0457
*                 R5 UNRSTD;        /* UNRESTRICT REG 5              */
*             END;                                                 0458
*           CALL DYNSVC;            /* INVOKE DYNAMIC ALLOCATION     */
@RF00451 BAL   R14,DYNSVC                                          0459
*           IF RCODESV>0 THEN       /* IF RETURN CODE NON-ZERO       */
         L     R10,RCODESV(,R3AWAPTR)                              0460
         LTR   R10,R10                                             0460
         BNP   @RF00460                                            0460
*             DO;                   /* CHECK IF FILE IN USE          */
*               IF S99ERROR='0410'X THEN                           0462
         L     R10,PTRS99RB(,R3AWAPTR)                             0462
         CLC   S99ERROR(2,R10),@HEX0410                            0462
         BNE   @RF00462                                            0462
*                 DO;               /* YES -                         */
*                   R1=R3AWAPTR;    /* WORKAREA PTR IN REG 1         */
         LR    R1,R3AWAPTR                                         0464
*                   CALL IKJEFD36;  /* PROMPT FOR OPTION             */
         L     R15,VCONSD36                                        0465
         BALR  R14,R15                                             0465
*                 END;                                             0466
*               ELSE                /* NO - CHECK IF DSN NOT FOUND   */
*                 IF S99ERROR='1708'X THEN                         0467
         B     @RC00462                                            0467
@RF00462 L     R10,PTRS99RB(,R3AWAPTR)                             0467
         CLC   S99ERROR(2,R10),@HEX1708                            0467
         BNE   @RF00467                                            0467
*                   DO;             /* YES -                         */
*                     ENTRYCD=1;    /* SET ENTRY CODE                */
         MVC   ENTRYCD(2,R3AWAPTR),@HW1                            0469
*                     R1=R3AWAPTR;  /* WORKAREA PTR IN REG 1 ISSUE 0470
*                                      NOT FOUND MSG                 */
         LR    R1,R3AWAPTR                                         0470
*                     CALL IKJEFD35;                               0471
         L     R15,VCONSD35                                        0471
         BALR  R14,R15                                             0471
*                     CALL PRMPDSN; /* PROMPT FOR DSNAME             */
         BAL   R14,PRMPDSN                                         0472
*                     IF RBCODE12='0'B THEN                        0473
         TM    RBCODE12(R3AWAPTR),B'10000000'                      0473
         BNZ   @RF00473                                            0473
*                       CALL DSNADDT;/* PROCESS NEW DSNAME           */
         BAL   R14,DSNADDT                                         0474
*                   END;                                           0475
*                 ELSE                                             0476
*                   DO;             /* OTHERWISE,                    */
         B     @RC00467                                            0476
@RF00467 DS    0H                                                  0477
*                     RBCODE12='1'B;/* INDICATE ALLOC FAILED         */
         OI    RBCODE12(R3AWAPTR),B'10000000'                      0477
*                     ENTRYCD=1;    /* SET ENTRY CODE                */
         MVC   ENTRYCD(2,R3AWAPTR),@HW1                            0478
*                     R1=R3AWAPTR;  /* WORKAREA PTR IN REG 1 ISSUE 0479
*                                      ERROR MESSAGE                 */
         LR    R1,R3AWAPTR                                         0479
*                     CALL IKJEFD35;                               0480
         L     R15,VCONSD35                                        0480
         BALR  R14,R15                                             0480
*                   END;                                           0481
*             END;                                                 0482
*         END;                                                     0483
*     END;                                                         0484
*   END OLDDS;                                                     0485
@EL00008 DS    0H                                                  0485
@EF00008 DS    0H                                                  0485
@ER00008 LM    R14,R12,@SA00008                                    0485
         BR    R14                                                 0485
*                                                                  0486
*/********************************************************************/
*/*                                                                  */
*/* SEGMENT NAME -                                                   */
*/*      DSNADDT                                                     */
*/*                                                                  */
*/* FUNCTION -                                                       */
*/*      THIS ROUTINE BUILDS THE TEXT UNIT FOR THE DSNAME ENTERED    */
*/*      ON THE PROMPT AND RE-INVOKES DYNAMIC ALLOCATION.            */
*/*                                                                  */
*/* OPERATION -                                                      */
*/*      INVOKE GENTRANS, PASSING THE SECONDARY PCL AND PDL. IF      */
*/*      GENTRANS FAILED, ISSUE ERROR MESSAGE. ELSE, FREE THE        */
*/*      SECONDARY PDL. COPY THE POINTERS TO THE NEW TEXT UNITS      */
*/*      INTO THE ORIGINAL ADDRESS LIST. INVOKE DYNAMIC ALLOCATION.  */
*/*      IF ALLOCATION INDICATES FILE IN USE, GO TO PROMPT FOR       */
*/*      OPTION. IF DSNAME NOT FOUND, PROMPT FOR NEW DSNAME AND      */
*/*      REPEAT ABOVE PROCEDURE. ELSE, ISSUE ERROR MSG. FREE THE     */
*/*      STORAGE USED TO BUILD THE SECONDARY TEXT UNITS.             */
*/*                                                                  */
*/* INPUT -                                                          */
*/*      PTR TO PROMPT PCL                                           */
*/*      PTR TO PROMPT PDL                                           */
*/*                                                                  */
*/* OUTPUT -                                                         */
*/*      RBCODE12 = 0 - ALLOCATION SUCCESSFUL                        */
*/*                 1 - ERROR IN GENTRANS OR ALLOCATION              */
*/*                                                                  */
*/* NOTE:  THIS ROUTINE REPRESENTS A DIRECT LINE FUNCTION AND CAN    */
*/*        BE INCORPORATED AS AN INCLUDED CODE SEGMENT WHEN THE      */
*/*        INCLUDE FUNCTION HAS RECEIVED A STANDARD DEFINITION.      */
*/*                                                                  */
*/*        IF THE DSNAME COULD NOT BE FOUND, AND THE USER IS         */
*/*        PROMPTED TO REENTER DSNAME, THEN THE TEXT UNITS FOR THE   */
*/*        OTHER PARAMETERS HAVE ALREADY BEEN BUILT. GENTRANS IS     */
*/*        INVOKED TO TRANSLATE ONLY THE DSNAME ENTERED ON THE       */
*/*        PROMPT. THE TEXT UNITS BUILT ARE PUT INTO THE ORIGINAL    */
*/*        LIST OF TEXT UNITS.                                       */
*/********************************************************************/
*                                                                  0486
*DSNADDT:                                                          0486
*   PROC;                                                          0486
DSNADDT  STM   R14,R12,@SA00009                                    0486
*DSNAD1:                                                           0487
*   S99ERROR=S99ERROR&&S99ERROR;    /* ZERO ERROR FIELD              */
DSNAD1   L     R10,PTRS99RB(,R3AWAPTR)                             0487
         XC    S99ERROR(2,R10),S99ERROR(R10)                       0487
*   GTPLPDL=PMPTPDL;                /* SET PDL PTR                   */
         L     R10,GTPLPTR(,R3AWAPTR)                              0488
         L     R7,PMPTPDL(,R3AWAPTR)                               0488
         ST    R7,GTPLPDL(,R10)                                    0488
*   GTPLPCL=PPLPCL;                 /* SET PCL PTR                   */
         L     R7,PPLPTR(,R3AWAPTR)                                0489
         L     R7,PPLPCL(,R7)                                      0489
         ST    R7,GTPLPCL(,R10)                                    0489
*   GTPLKLST=ADDR(DSNKLIST);        /* PTR TO KEYLIST                */
         LA    R7,DSNKLIST                                         0490
         ST    R7,GTPLKLST(,R10)                                   0490
*   GTPLTBLE=0;                     /* NO KEYWORD TABLE              */
         SLR   R7,R7                                               0491
         ST    R7,GTPLTBLE(,R10)                                   0491
*   GTPLABLK=0;                     /* NO ADDITIONAL BLOCK           */
         ST    R7,GTPLABLK(,R10)                                   0492
*   GTPLOUTA=0;                     /* ZERO PTR TO TEXT              */
         ST    R7,GTPLOUTA(,R10)                                   0493
*   R1=GTPLPTR;                     /* GTPL ADDR IN REG 1            */
         LR    R1,R10                                              0494
*   GEN(LINK EP=IKJCB831);          /* INVOKE GENTRANS               */
         LINK EP=IKJCB831
*   RCODESV=R15;                    /* SAVE RETURN CODE              */
         ST    R15,RCODESV(,R3AWAPTR)                              0496
*   IF RCODESV>0 THEN               /* IF GENTRANS FAILED            */
         L     R10,RCODESV(,R3AWAPTR)                              0497
         LTR   R10,R10                                             0497
         BNP   @RF00497                                            0497
*     DO;                           /* THEN                          */
*       RBCODE12='1'B;              /* INDICATE ALLOC FAILED         */
         OI    RBCODE12(R3AWAPTR),B'10000000'                      0499
*       ENTRYCD=3;                  /* SET ENTRY CODE                */
         MVC   ENTRYCD(2,R3AWAPTR),@HW3                            0500
*       R1=R3AWAPTR;                /* WORKAREA PTR IN REG 1         */
         LR    R1,R3AWAPTR                                         0501
*       CALL IKJEFD35;              /* ISSUE SYSTEM ERROR MSG        */
         L     R15,VCONSD35                                        0502
         BALR  R14,R15                                             0502
*     END;                                                         0503
*   ELSE                                                           0504
*     DO;                           /* IF GENTRANS SUCCESSFUL,       */
         B     @RC00497                                            0504
@RF00497 DS    0H                                                  0505
*       RFY                                                        0505
*         R5 RSTD;                  /* RESTRICT REG 5                */
*       R5=ADDR(PMPTPDL);           /* PDL PTR IN REG 5              */
         LA    R5,PMPTPDL(,R3AWAPTR)                               0506
*       GEN(IKJRLSA (5));           /* RELEASE 2ND PDL               */
         IKJRLSA (5)
*       RFY                                                        0508
*         R5 UNRSTD;                /* UNRESTRICT REG 5              */
*       TEXTPTR2=GTPLOUTA;          /* SAVE PTR TO TEXT              */
         L     R10,GTPLPTR(,R3AWAPTR)                              0509
         L     TEXTPTR2,GTPLOUTA(,R10)                             0509
*       TUDSN=ADDRLST2(3);          /* NEW DSN TEXT POINTER          */
         L     R10,ADDRLST2+8(,TEXTPTR2)                           0510
         ST    R10,TUDSN(,GTPLOUT1)                                0510
*       TUMBR=ADDRLST2(4);          /* NEW MEMBER TEXT POINTER       */
         L     R10,ADDRLST2+12(,TEXTPTR2)                          0511
         ST    R10,TUMBR(,GTPLOUT1)                                0511
*       TUPSWD=ADDRLST2(5);         /* NEW PSWORD TEXT POINTER       */
         L     R10,ADDRLST2+16(,TEXTPTR2)                          0512
         ST    R10,TUPSWD(,GTPLOUT1)                               0512
*       TUPSWDND='0'B;              /* ZERO HIGH ORDER BIT           */
         NI    TUPSWDND(GTPLOUT1),B'01111111'                      0513
*       CALL DYNSVC;                /* INVOKE DYN ALLOCATION         */
         BAL   R14,DYNSVC                                          0514
*       IF RCODESV>0 THEN           /* IF ALLOCATION FAILED,         */
         L     R10,RCODESV(,R3AWAPTR)                              0515
         LTR   R10,R10                                             0515
         BNP   @RF00515                                            0515
*         DO;                       /* THEN CHECK IF FILE IN USE,    */
*           IF S99ERROR='0410'X THEN                               0517
         L     R10,PTRS99RB(,R3AWAPTR)                             0517
         CLC   S99ERROR(2,R10),@HEX0410                            0517
         BNE   @RF00517                                            0517
*             DO;                   /* YES -                         */
*               R1=R3AWAPTR;        /* WORKAREA PTR IN REG 1 PROMPT
*                                      FOR OPTION                    */
         LR    R1,R3AWAPTR                                         0519
*               CALL IKJEFD36;                                     0520
         L     R15,VCONSD36                                        0520
         BALR  R14,R15                                             0520
*             END;                                                 0521
*           ELSE                    /* NO - CHECK IF DSN NOT FOUND   */
*             IF S99ERROR='1708'X THEN                             0522
         B     @RC00517                                            0522
@RF00517 L     R10,PTRS99RB(,R3AWAPTR)                             0522
         CLC   S99ERROR(2,R10),@HEX1708                            0522
         BNE   @RF00522                                            0522
*               DO;                 /* YES -                         */
*                 ENTRYCD=1;        /* SET ENTRY CODE                */
         MVC   ENTRYCD(2,R3AWAPTR),@HW1                            0524
*                 R1=R3AWAPTR;      /* WORKAREA PTR IN REG 1 ISSUE 0525
*                                      NOT FOUND MSG                 */
         LR    R1,R3AWAPTR                                         0525
*                 CALL IKJEFD35;                                   0526
         L     R15,VCONSD35                                        0526
         BALR  R14,R15                                             0526
*                 CALL PRMPDSN;     /* PROMPT FOR NEW DSNAME         */
         BAL   R14,PRMPDSN                                         0527
*                 IF RBCODE12='0'B THEN                            0528
         TM    RBCODE12(R3AWAPTR),B'10000000'                      0528
         BZ    @RT00528                                            0528
*                   GO TO DSNAD1;   /* PROCESS NEW DSNAME            */
*               END;                                               0530
*             ELSE                                                 0531
*               DO;                 /* NO -                          */
         B     @RC00522                                            0531
@RF00522 DS    0H                                                  0532
*                 RBCODE12='1'B;    /* INDICATE ALLOC FAILED         */
         OI    RBCODE12(R3AWAPTR),B'10000000'                      0532
*                 ENTRYCD=1;        /* SET ENTRY CODE                */
         MVC   ENTRYCD(2,R3AWAPTR),@HW1                            0533
*                 R1=R3AWAPTR;      /* WORKAREA PTR IN REG 1 ISSUE 0534
*                                      ERROR MSG                     */
         LR    R1,R3AWAPTR                                         0534
*                 CALL IKJEFD35;                                   0535
         L     R15,VCONSD35                                        0535
         BALR  R14,R15                                             0535
*               END;                                               0536
*         END;                                                     0537
@RC00522 DS    0H                                                  0537
@RC00517 DS    0H                                                  0538
*       R0=ADDRLST2(1);             /* GET SUBPOOL AND LENGTH        */
@RF00515 L     R0,ADDRLST2(,TEXTPTR2)                              0538
*       R1=TEXTPTR2;                /* AND ADDR OF TEXT STORAGE      */
         LR    R1,TEXTPTR2                                         0539
*       GEN(FREEMAIN R,LV=(0),A=(1));/* FREE TEXT STORAGE            */
         FREEMAIN R,LV=(0),A=(1)
*       GTPLOUTA=GTPLOUT1;          /* RESET TEXT PTR                */
         L     R10,GTPLPTR(,R3AWAPTR)                              0541
         ST    GTPLOUT1,GTPLOUTA(,R10)                             0541
*     END;                                                         0542
*   END DSNADDT;                                                   0543
@EL00009 DS    0H                                                  0543
@EF00009 DS    0H                                                  0543
@ER00009 LM    R14,R12,@SA00009                                    0543
         BR    R14                                                 0543
*                                                                  0544
*/********************************************************************/
*/*                                                                  */
*/* SEGMENT NAME -                                                   */
*/*      PRMPDSN                                                     */
*/*                                                                  */
*/* FUNCTION -                                                       */
*/*      THIS ROUTINE INVOKES PARSE TO PROMPT THE USER FOR A         */
*/*      DATA SET NAME.                                              */
*/*                                                                  */
*/* OPERATION -                                                      */
*/*      SET UP PARSE PARAMETER LIST. INVOKE PARSE. IF PARSE         */
*/*      FAILED, ISSUE ERROR MESSAGE.                                */
*/*                                                                  */
*/* INPUT -                                                          */
*/*      PTR TO PCL                                                  */
*/*                                                                  */
*/* OUTPUT -                                                         */
*/*      PTR TO PROMPT PDL                                           */
*/*                                                                  */
*/* NOTE:  THIS ROUTINE REPRESENTS A DIRECT LINE FUNCTION AND CAN    */
*/*        BE INCORPORATED AS AN INCLUDED CODE SEGMENT WHEN THE      */
*/*        INCLUDE FUNCTION HAS RECEIVED A STANDARD DEFINITION.      */
*/********************************************************************/
*                                                                  0544
*PRMPDSN:                                                          0544
*   PROC;                                                          0544
PRMPDSN  STM   R14,R12,@SA00010                                    0544
*   ENTRYCD=3;                      /* INDICATE PROMPT FOR DSN       */
         MVC   ENTRYCD(2,R3AWAPTR),@HW3                            0545
*   R1=R3AWAPTR;                    /* WORKAREA PTR IN REG 1         */
         LR    R1,R3AWAPTR                                         0546
*   CALL IKJEFD37;                  /* INVOKE PARSE                  */
         L     R15,VCONSD37                                        0547
         BALR  R14,R15                                             0547
*   IF RCODESV>0 THEN               /* IF PARSE FAILED,              */
         L     R10,RCODESV(,R3AWAPTR)                              0548
         LTR   R10,R10                                             0548
         BNP   @RF00548                                            0548
*     DO;                           /* THEN,                         */
*       IF AWARSV1='1'B THEN        /* IF MSG ALREADY ISSUED,        */
         TM    AWARSV1(R3AWAPTR),B'00000010'                       0550
         BNO   @RF00550                                            0550
*         AWARSV1='0'B;             /* TURN OFF INDICATOR            */
         NI    AWARSV1(R3AWAPTR),B'11111101'                       0551
*       ELSE                                                       0552
*         DO;                       /* OTHERWISE,                    */
         B     @RC00550                                            0552
@RF00550 DS    0H                                                  0553
*           ENTRYCD=2;              /* SET ENTRY CODE                */
         MVC   ENTRYCD(2,R3AWAPTR),@HW2                            0553
*           R1=R3AWAPTR;            /* WORKAREA PTR IN REG 1         */
         LR    R1,R3AWAPTR                                         0554
*           CALL IKJEFD35;          /* ISSUE ERROR MESSAGE           */
         L     R15,VCONSD35                                        0555
         BALR  R14,R15                                             0555
*         END;                                                     0556
*     END;                                                         0557
*   ELSE                                                           0558
*     DO;                                                          0558
         B     @RC00548                                            0558
@RF00548 DS    0H                                                  0559
*       IF MBRPRES1='1'B&           /* IF MEMBER SPECIFIED           */
*           DISPPDE=2 THEN          /* AND DISP OF DELETE            */
         L     R10,PMPTPDL(,R3AWAPTR)                              0559
         TM    MBRPRES1(R10),B'10000000'                           0559
         BNO   @RF00559                                            0559
         L     R2,PTRPDL(,R3AWAPTR)                                0559
         CLC   DISPPDE(2,R2),@HW2                                  0559
         BNE   @RF00559                                            0559
*         DO;                                                      0560
*           ENTRYCD=5;              /* SET ENTRY CODE COPY NEW DSNAME
*                                      INTO PDE                      */
         MVC   ENTRYCD(2,R3AWAPTR),@HW5                            0561
*           MOVELEN=ADDR(DDNMEPDE)-ADDR(DSNPDE);                   0562
         LA    MOVELEN,28                                          0562
*           DSNPDE(1:MOVELEN)=DSNAMPDE(1:MOVELEN);                 0563
         L     R15,PTRPDL(,R3AWAPTR)                               0563
         LR    R14,MOVELEN                                         0563
         BCTR  R14,0                                               0563
         EX    R14,@SM01519                                        0563
*           R1=R3AWAPTR;            /* WORKAREA PTR IN REG 1 ISSUE 0564
*                                      ERROR MESSAGE                 */
         LR    R1,R3AWAPTR                                         0564
*           CALL IKJEFD35;                                         0565
         L     R15,VCONSD35                                        0565
         BALR  R14,R15                                             0565
*           RFY                                                    0566
*             R5 RSTD;              /* RESTRICT REG 5                */
*           R5=ADDR(PMPTPDL);       /* PDL ADDRESS IN REG 5          */
         LA    R5,PMPTPDL(,R3AWAPTR)                               0567
*           GEN(IKJRLSA (5));       /* RELEASE PDL                   */
         IKJRLSA (5)
*           RFY                                                    0569
*             R5 UNRSTD;            /* UNRESTRICT REG 5              */
*         END;                                                     0570
*     END;                                                         0571
*   END PRMPDSN;                                                   0572
@EL00010 DS    0H                                                  0572
@EF00010 DS    0H                                                  0572
@ER00010 LM    R14,R12,@SA00010                                    0572
         BR    R14                                                 0572
*                                                                  0573
*/********************************************************************/
*/*                                                                  */
*/* SEGMENT NAME -                                                   */
*/*      TRANSRTN                                                    */
*/*                                                                  */
*/* FUNCTION -                                                       */
*/*      THIS ROUTINE INVOKES THE GENERALIZED TRANSLATE ROUTINE AND  */
*/*      CHECKS ITS RETURN CODE.                                     */
*/*                                                                  */
*/* OPERATION -                                                      */
*/*      THE TEXT UNIT FOR PERMANENT ALLOCATION IS SET UP IN THE     */
*/*      ADDITIONAL BLOCK OF TEXT TO BE PASSED.  THE POINTER TO THE  */
*/*      GENTRANS PARAMETER LIST IS PUT IN REGISTER 1 AND GENTRANS   */
*/*      IS INVOKED.  IF THE RETURN CODE FROM GENTRANS IS NON-ZERO,  */
*/*      AN INDICATOR IS SET. ELSE, THE TEXT POINTER RETURNED BY     */
*/*      GENTRANS IS PUT INTO THE DYNAMIC ALLOCATION REQUEST BLOCK.  */
*/*                                                                  */
*/* INPUT -                                                          */
*/*      PTR TO GENTRANS PARAMETER LIST                              */
*/*                                                                  */
*/* OUTPUT -                                                         */
*/*      RBCODE12 = 0 - GENTRANS SUCCESSFUL                          */
*/*               = 1 - GENTRANS FAILED                              */
*/*                                                                  */
*/********************************************************************/
*                                                                  0573
*TRANSRTN:                                                         0573
*   PROC;                                                          0573
TRANSRTN STM   R14,R5,@SA00011                                     0573
         STM   R7,R12,@SA00011+32                                  0573
*   ADLKEY=DALPERMA;                /* PERM ALLOC TEXT - KEY         */
         L     R10,TXT(,R3AWAPTR)                                  0574
         MVC   ADLKEY(2,R10),@HWPERMA                              0574
*   ADLNBR=0;                       /* NUMBER                        */
         SLR   R7,R7                                               0575
         STH   R7,ADLNBR(,R10)                                     0575
*   ADDTEXT(INC)=TXT;               /* ADDRESS IN LIST               */
         LR    R7,R4INC                                            0576
         SLA   R7,2                                                0576
         L     R5,GTPLPTR(,R3AWAPTR)                               0576
         L     R2,GTPLABLK(,R5)                                    0576
         ST    R10,@TF00001                                        0576
         ALR   R7,R2                                               0576
         MVC   ADDTEXT-4(4,R7),@TF00001                            0576
*   ADDNUM=ADDNUM+1;                /* INCREMENT ENTRIES CTR         */
         LA    R7,1                                                0577
         MVC   @TF00001(2),ADDNUM(R2)                              0577
         LH    R15,@TF00001                                        0577
         ALR   R15,R7                                              0577
         ST    R15,@TF00001                                        0577
         MVC   ADDNUM(2,R2),@TF00001+2                             0577
*   INC=INC+1;                      /* INCREMENT SUBSCRIPT           */
         ALR   R4INC,R7                                            0578
*   TXT=TXT+4;                      /* INCREMENT TEXT PTR            */
         AL    R10,@FW4                                            0579
         ST    R10,TXT(,R3AWAPTR)                                  0579
*   ADLKEY=DALRTORG;                /* RETURN DSORG KEY              */
         MVC   ADLKEY(2,R10),@HWRTORG                              0580
*   ADLNBR=1;                       /* RETURN DSORG NUMBER           */
         STH   R7,ADLNBR(,R10)                                     0581
*   ADLLEN=2;                       /* RETURN DSORG LENGTH           */
         MVC   ADLLEN(2,R10),@HW2                                  0582
*   ADDTEXT(INC)=TXT;               /* ADDRESS IN LIST               */
         LR    R14,R4INC                                           0583
         SLA   R14,2                                               0583
         ST    R10,@TF00001                                        0583
         ALR   R14,R2                                              0583
         MVC   ADDTEXT-4(4,R14),@TF00001                           0583
*   TXT=TXT+8;                      /* INCREMENT TEXT PTR            */
         AL    R10,@FW8                                            0584
         ST    R10,TXT(,R3AWAPTR)                                  0584
*   INC=INC+1;                      /* INCREMENT SUBSCRIPT           */
         ALR   R4INC,R7                                            0585
*   ADDNUM=ADDNUM+1;                /* INCREMENT ENTRIES CTR         */
         ALR   R15,R7                                              0586
         ST    R15,@TF00001                                        0586
         MVC   ADDNUM(2,R2),@TF00001+2                             0586
*   R1=GTPLPTR;                     /* PARM LIST IN REG 1            */
         LR    R1,R5                                               0587
*   GEN(LINK EP=IKJCB831);          /* INVOKE GENTRANS               */
         LINK EP=IKJCB831
*   RCODESV=R15;                    /* SAVE RETURN CODE              */
         ST    R15,RCODESV(,R3AWAPTR)                              0589
*   IF RCODESV>0 THEN               /* IF GENTRANS FAILED,           */
         L     R10,RCODESV(,R3AWAPTR)                              0590
         LTR   R10,R10                                             0590
         BNP   @RF00590                                            0590
*     DO;                                                          0591
*       ENTRYCD=3;                  /* SET ENTRY CODE                */
         MVC   ENTRYCD(2,R3AWAPTR),@HW3                            0592
*       R1=R3AWAPTR;                /* WORKAREA PTR IN REG 1         */
         LR    R1,R3AWAPTR                                         0593
*       CALL IKJEFD35;              /* ISSUE ERROR MSG               */
         L     R15,VCONSD35                                        0594
         BALR  R14,R15                                             0594
*     END;                                                         0595
*   ELSE                            /* OTHERWISE,                    */
*     DO;                                                          0596
         B     @RC00590                                            0596
@RF00590 DS    0H                                                  0597
*       S99TXTPP=GTPLOUTA+8;        /* PUT TEXT PTR IN RB            */
         L     R10,PTRS99RB(,R3AWAPTR)                             0597
         L     R7,GTPLPTR(,R3AWAPTR)                               0597
         L     R7,GTPLOUTA(,R7)                                    0597
         LA    R2,8                                                0597
         ALR   R2,R7                                               0597
         ST    R2,S99TXTPP(,R10)                                   0597
*       GTPLOUT1=GTPLOUTA;          /* SAVE TEXT PTR                 */
         LR    GTPLOUT1,R7                                         0598
*     END;                                                         0599
*   END TRANSRTN;                                                  0600
@EL00011 DS    0H                                                  0600
@EF00011 DS    0H                                                  0600
@ER00011 LM    R14,R5,@SA00011                                     0600
         LM    R7,R12,@SA00011+32                                  0600
         BR    R14                                                 0600
*                                                                  0601
*/********************************************************************/
*/*                                                                  */
*/* SEGMENT NAME -                                                   */
*/*      DYNSVC                                                      */
*/*                                                                  */
*/* FUNCTION -                                                       */
*/*      THIS ROUTINE ISSUES SVC 99 TO INVOKE DYNAMIC ALLOCATION.    */
*/*                                                                  */
*/* OPERATION -                                                      */
*/*      THE DYNALLOC MACRO IS ISSUED. ON RETURN, THE RETURN CODE IN */
*/*      REGISTER 15 IS SAVED.                                       */
*/*                                                                  */
*/* INPUT -                                                          */
*/*      WORKAREA                                                    */
*/*                                                                  */
*/* OUTPUT -                                                         */
*/*      RETURN CODE = 0 - ALLOCATION SUCCESSFUL                     */
*/*                 ^= 0 - ERROR                                     */
*/*                                                                  */
*/********************************************************************/
*                                                                  0601
*DYNSVC:                                                           0601
*   PROC;                                                          0601
DYNSVC   STM   R14,R12,12(R13)                                     0601
*   R1=ADDR(S99RBPTR);              /* REQ BLOCK PTR IN REG1         */
         LA    R1,S99RBPTR                                         0602
*   S99RBPTR=PTRS99RB;              /* PTR TO REQUEST BLOCK          */
         L     R10,PTRS99RB(,R3AWAPTR)                             0603
         ST    R10,S99RBPTR                                        0603
*   S99RBPND='1'B;                  /* LAST REQ BLOCK                */
*                                                                  0604
         OI    S99RBPND,B'10000000'                                0604
*   /*****************************************************************/
*   /*                                                               */
*   /* MACDATE Y-2 73082                                             */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0605
*   SVC(99);                        /* INVOKE DYN ALLOCATION         */
         SVC   99                                                  0605
*   RCODESV=R15;                    /* SAVE RETURN CODE              */
         ST    R15,RCODESV(,R3AWAPTR)                              0606
*   END DYNSVC;                                                    0607
@EL00012 DS    0H                                                  0607
@EF00012 DS    0H                                                  0607
@ER00012 LM    R14,R12,12(R13)                                     0607
         BR    R14                                                 0607
*   DECLARE                         /* GENERAL PURPOSE REGISTERS     */
*     GPR00P PTR(31) REG(0),                                       0608
*     GPR01P PTR(31) REG(1),                                       0608
*     GPR02P PTR(31) REG(2),                                       0608
*     GPR03P PTR(31) REG(3),                                       0608
*     GPR04P PTR(31) REG(4),                                       0608
*     GPR05P PTR(31) REG(5),                                       0608
*     GPR06P PTR(31) REG(6),                                       0608
*     GPR07P PTR(31) REG(7),                                       0608
*     GPR08P PTR(31) REG(8),                                       0608
*     GPR09P PTR(31) REG(9),                                       0608
*     GPR14P PTR(31) REG(14),                                      0608
*     GPR15P PTR(31) REG(15);                                      0608
*   DECLARE                         /* COMMON VARIABLES              */
*     I256C CHAR(256) BASED,                                       0609
*     I031F FIXED(31) BASED,                                       0609
*     I031P PTR(31) BASED,                                         0609
*     I015F FIXED(15) BASED,                                       0609
*     I015P PTR(15) BASED,                                         0609
*     I008P PTR(8) BASED,                                          0609
*     I001C CHAR(1) BASED;          /*                       @Y30LPKH*/
@DATA    DS    0H
@HW3     DC    H'3'
@HW5     DC    H'5'
@HWTRK   DC    XL2'0007'
@HWCYL   DC    XL2'0008'
@HWSYSOU DC    XL2'0018'
@HWTERM  DC    XL2'0028'
@HWPERMA DC    XL2'0052'
@HWRTORG DC    XL2'0057'
@HWMSVGP DC    XL2'005E'
@SM01509 MVC   DSNPDE(0,R10),DSNAMPDE(R7)
@SM01512 MVC   DAIRDSN(0),DSNBUF(R10)
@SM01515 MVC   DSNPDE(0,R10),DSNAMPDE(R5)
@SM01519 MVC   DSNPDE(0,R15),DSNAMPDE(R10)
@DATD    DSECT
         DS    0F
@SA00001 DS    18F
@PC00001 DS    1F
@SAV001  EQU   @SA00001
@SA00005 DS    15F
@SA00003 DS    15F
@SA00004 DS    15F
@SA00008 DS    15F
@SA00006 DS    15F
@SA00002 DS    15F
@SA00011 DS    14F
@SA00010 DS    15F
@SA00007 DS    15F
@SA00009 DS    15F
@TF00001 DS    F
@ZTEMPS  DS    C
@ZTEMPND EQU   *
@ZLEN    EQU   @ZTEMPND-@ZTEMPS
@TEMPS   EQU   @ZTEMPS
@L       EQU   @ZLEN
IKJEFD32 CSECT
         DS    0F
@FW1     DC    F'1'
@HW1     EQU   @FW1+2
@FW2     DC    F'2'
@HW2     EQU   @FW2+2
@FW4     DC    F'4'
@FW8     DC    F'8'
@FW12    DC    F'12'
@DATD    DSECT
         DS    0D
SAVDSPDL DS    A
S99TUFP  DS    A
S99RBPTR DS    AL4
         ORG   S99RBPTR
S99RBPND DS    BL1
         ORG   S99RBPTR+4
DAPLAREA DS    CL20
DAIRAREA DS    CL16
DFPARMS  DS    CL20
         ORG   DFPARMS
DFS99RBP DS    AL4
         ORG   DFS99RBP
DFDAPLP  DS    AL4
         ORG   DFPARMS+4
DFRCP    DS    AL4
DFJEFF02 DS    AL4
DFIDP    DS    AL4
DFCPPLP  DS    AL4
         ORG   DFPARMS+24              ****
DAIRDSBF DS    CL46
         ORG   DAIRDSBF
DAIRDSLN DS    FL2
DAIRDSN  DS    CL44
         ORG   DAIRDSBF+46
         ORG   *+1-(*-@DATD)/(*-@DATD) ENSURE DSECT DATA
@ENDDATD EQU   *
@DATEND  EQU   *
IKJEFD32 CSECT
         DC    0F'0'
@SIZDATD DC    AL1(1)
         DC    AL3(@ENDDATD-@DATD)
@SIZ001  EQU   @SIZDATD
VCONSD33 DC    V(IKJEFD33)
VCONSD35 DC    V(IKJEFD35)
VCONSD37 DC    V(IKJEFD37)
VCONSD36 DC    V(IKJEFD36)
         DC    0D'0'
ADMSG    DC    F'0'
CALLERNO DC    H'1'
@HEX0004 DC    X'0004'
@HEX0410 DC    X'0410'
@HEX1708 DC    X'1708'
PATCH32  DC    25F'0'
DSNKLIST DC    XL2'0002'
         DC    XL2'0000'
TERMEXT  DC    X'0C'
         DC    X'0E'
         DC    X'12'
         DC    X'14'
         DC    X'18'
         DC    X'1A'
         DC    X'1C'
         DC    X'1E'
         DC    X'20'
         DC    X'22'
         DC    X'24'
         DC    X'26'
         DC    X'28'
         DC    X'2A'
         DC    X'2C'
         DC    X'2E'
         DC    X'30'                                               @26A
         DC    X'00'
SYSOTEXT DC    X'08'
         DC    X'0E'
         DC    X'14'
         DC    X'26'
         DC    X'20'
         DC    X'22'
         DC    X'24'
         DC    X'28'
         DC    X'2E'
         DC    X'30'                                               @26A
         DC    X'00'
OLDEXT   DC    X'10'
         DC    X'12'
         DC    X'14'
         DC    X'18'
         DC    X'1A'
         DC    X'2C'
         DC    X'30'                                               @26A
         DC    X'00'
NEWEXT   DC    X'18'
         DC    X'1A'
         DC    X'30'                                               @26A
         DC    X'00'
DMMYEXT  DC    X'0C'
         DC    X'0E'
         DC    X'12'
         DC    X'14'
         DC    X'18'
         DC    X'1A'
         DC    X'1C'
         DC    X'1E'
         DC    X'20'
         DC    X'22'
         DC    X'24'
         DC    X'26'
         DC    X'28'
         DC    X'2A'
         DC    X'2C'
         DC    X'2E'
         DC    X'30'                                               @26A
         DC    X'00'
         DC    0D'0'                   END OF CSECT                @26A
R0       EQU   00                      EQUATES FOR REGISTERS 0-15
R1       EQU   01
R2       EQU   02
R3       EQU   03
R4       EQU   04
R5       EQU   05
R6       EQU   06
R7       EQU   07
R8       EQU   08
R9       EQU   09
R10      EQU   10
R11      EQU   11
R12      EQU   12
R13      EQU   13
R14      EQU   14
R15      EQU   15
R4INC    EQU   R4
GTPLOUT1 EQU   R6
DAIRDSPT EQU   R2
DAPLPTR  EQU   R5
DAPB0PTR EQU   R4
R5EXTRAN EQU   R5
MOVELEN  EQU   R2
TEXTPTR2 EQU   R4
R3AWAPTR EQU   R3
R2I      EQU   R2
REGSAVE  EQU   4
RETCODE  EQU   16
ALLOCWA  EQU   0
CPPLPTR  EQU   ALLOCWA
PTRPDL   EQU   ALLOCWA+4
GTPLPTR  EQU   ALLOCWA+8
SWITCH   EQU   ALLOCWA+12
RBCODE12 EQU   SWITCH
DSOVRLAY EQU   SWITCH
CONT     EQU   SWITCH
AWARSV1  EQU   SWITCH
SWITCH2  EQU   ALLOCWA+13
DSEXISTS EQU   SWITCH2
ENTRYCD  EQU   ALLOCWA+14
TXT      EQU   ALLOCWA+16
PTRS99RB EQU   ALLOCWA+20
RCODESV  EQU   ALLOCWA+24
VCFLAGS  EQU   ALLOCWA+36
PMPTPDL  EQU   ALLOCWA+40
CMDTWO   EQU   ALLOCWA+60
COMMECB  EQU   ALLOCWA+160
PPLPTR   EQU   ALLOCWA+172
ALLOCPDL EQU   0
DSPDE    EQU   ALLOCPDL+8
STATSPDE EQU   ALLOCPDL+12
VOLPDE   EQU   ALLOCPDL+14
BLOKPDE  EQU   ALLOCPDL+16
SPACEPDE EQU   ALLOCPDL+18
HOLDPDE  EQU   ALLOCPDL+26
DISPPDE  EQU   ALLOCPDL+46
DSNPDE   EQU   ALLOCPDL+48+4                                       @26C
DSNPTR   EQU   DSNPDE
DSNLENGH EQU   DSNPDE+4
DSNFLAGS EQU   DSNPDE+6
DSNPRES  EQU   DSNFLAGS
MBRPTR   EQU   DSNPDE+8
MBRFLAGS EQU   DSNPDE+14
PASSPTR  EQU   DSNPDE+16
PASSFLAG EQU   DSNPDE+22
DDNMEPDE EQU   ALLOCPDL+76+4                                       @26C
DDNPTR   EQU   DDNMEPDE
DDNFLAGS EQU   DDNMEPDE+6
CLASPDE  EQU   ALLOCPDL+84+4                                       @26C
CLASPTR  EQU   CLASPDE
CLASFLG  EQU   CLASPDE+6
CLASPRES EQU   CLASFLG
SERPDE   EQU   ALLOCPDL+92+4                                       @26C
SERPTR   EQU   SERPDE
SERFLAGS EQU   SERPDE+6
BLKPDE   EQU   ALLOCPDL+104+4                                      @26C
BLKPTR   EQU   BLKPDE
BLKFLAGS EQU   BLKPDE+6
ABLKPDE  EQU   ALLOCPDL+112+4                                      @26C
ABLKPTR  EQU   ABLKPDE
ABLKFLG  EQU   ABLKPDE+6
SPACPDE  EQU   ALLOCPDL+120+4                                      @26C
SPACEPTR EQU   SPACPDE
SPACEFLG EQU   SPACPDE+6
SPACPDE2 EQU   ALLOCPDL+128+4                                      @26C
SPACPTR2 EQU   SPACPDE2
SPACFLG2 EQU   SPACPDE2+6
DIRECPDE EQU   ALLOCPDL+136+4                                      @26C
DIRPTR   EQU   DIRECPDE
DIRFLAGS EQU   DIRECPDE+6
ATTRPDE  EQU   ALLOCPDL+144+4                                      @26C
ATTRPTR  EQU   ATTRPDE
ATTRFLAG EQU   ATTRPDE+6
USIDPDE  EQU   ALLOCPDL+152+4                                      @26C
USIDPTR  EQU   USIDPDE
USIDFLAG EQU   USIDPDE+6
UTYPPDE  EQU   ALLOCPDL+160+4                                      @26C
UTYPPTR  EQU   UTYPPDE
UTYPFLAG EQU   UTYPPDE+6
UNCNTPDE EQU   ALLOCPDL+168+4                                      @26C
UCNTPTR  EQU   UNCNTPDE
UCNTFLAG EQU   UNCNTPDE+6
POSTPDE  EQU   ALLOCPDL+180+4                                      @26C
POSPTR   EQU   POSTPDE
POSFLAG  EQU   POSTPDE+6
MXVOLPDE EQU   ALLOCPDL+188+4                                      @26C
MXVOLPTR EQU   MXVOLPDE
MXVOLFLG EQU   MXVOLPDE+6
VOLSQPDE EQU   ALLOCPDL+196+4                                      @26C
VOLSQPTR EQU   VOLSQPDE
VOLSQFLG EQU   VOLSQPDE+6
MSVGPDE  EQU   ALLOCPDL+204+4                                      @26C
MSVGPTR  EQU   MSVGPDE
MSVGFLG  EQU   MSVGPDE+6
DSNBUF   EQU   0
DSNTERM  EQU   DSNBUF
CPPL     EQU   0
CPPLUPT  EQU   CPPL+4
CPPLPSCB EQU   CPPL+8
CPPLECT  EQU   CPPL+12
PPL      EQU   0
PPLPCL   EQU   PPL+12
DSNPDE1  EQU   0
DSNAMPDE EQU   DSNPDE1+8
DSNPTR1  EQU   DSNAMPDE
DSNFLG1  EQU   DSNAMPDE+6
MBRPTR1  EQU   DSNAMPDE+8
MBRFLG1  EQU   DSNAMPDE+14
MBRPRES1 EQU   MBRFLG1
PSWDPTR1 EQU   DSNAMPDE+16
@NM00013 EQU   DSNAMPDE+22
ADDRLST2 EQU   0
PDEEXT   EQU   0
DAPL     EQU   0
DAPLUPT  EQU   DAPL
DAPLECT  EQU   DAPL+4
DAPLECB  EQU   DAPL+8
DAPLPSCB EQU   DAPL+12
DAPLDAPB EQU   DAPL+16
DFID     EQU   0
@NM00017 EQU   DFID
DAPB04   EQU   0
DA04CD   EQU   DAPB04
DA04FLG  EQU   DAPB04+2
DA04CAT  EQU   DA04FLG
DA04DSE  EQU   DA04FLG
DA04CTRC EQU   DAPB04+6
DA04PDSN EQU   DAPB04+8
DA04CTL  EQU   DAPB04+12
TEXTRET  EQU   0
TUDSN    EQU   TEXTRET+8
TUMBR    EQU   TEXTRET+12
TUPSWD   EQU   TEXTRET+16
TUPSWDND EQU   TUPSWD
KEYLIST  EQU   0
TEXTCORE EQU   0
ADDNUM   EQU   TEXTCORE
ADDTEXT  EQU   TEXTCORE+4
ADLTEXT  EQU   0
ADLKEY   EQU   ADLTEXT
ADLNBR   EQU   ADLTEXT+2
ADLLEN   EQU   ADLTEXT+4
GTPL     EQU   0
GTPLPDL  EQU   GTPL
GTPLPCL  EQU   GTPL+4
GTPLKLST EQU   GTPL+8
GTPLTBLE EQU   GTPL+12
GTPLABLK EQU   GTPL+16
GTPLOUTA EQU   GTPL+20
S99RB    EQU   0
S99FLAG1 EQU   S99RB+2
S99FLG11 EQU   S99FLAG1
S99RSC   EQU   S99RB+4
S99ERROR EQU   S99RSC
S99TXTPP EQU   S99RB+8
S99FLAG2 EQU   S99RB+16
S99FLG21 EQU   S99FLAG2
S99FLG22 EQU   S99FLAG2+1
S99TUPL  EQU   0
S99TUPTR EQU   S99TUPL
S99TUP   EQU   0
S99TUNIT EQU   0
S99TUENT EQU   S99TUNIT+4
S99TUFLD EQU   0
ABLKBUF  EQU   0
ATTRBUF  EQU   0
BLKBUF   EQU   0
BUFRDSN  EQU   0
BUFRMBR  EQU   0
BUFRPASS EQU   0
CLASSBUF EQU   0
DDNBUF   EQU   0
DFMSGWTP EQU   0
DFRC     EQU   0
DIRBUF   EQU   0
I001C    EQU   0
I008P    EQU   0
I015F    EQU   0
I015P    EQU   0
I031F    EQU   0
I031P    EQU   0
I256C    EQU   0
MBRBUF   EQU   0
MSVGBUF  EQU   0
MXVOLBUF EQU   0
PASSBUF  EQU   0
POSTBUF  EQU   0
SERBUF   EQU   0
SPACBUF2 EQU   0
SPACEBUF EQU   0
UCNTBUF  EQU   0
USIDBUF  EQU   0
UTYPBUF  EQU   0
VSEQBUF  EQU   0
INPARMS  EQU   0
*                                      START UNREFERENCED COMPONENTS
S99TUPRM EQU   S99TUFLD+2
S99TULEN EQU   S99TUFLD
S99TUPAR EQU   S99TUENT+2
S99TULNG EQU   S99TUENT
S99TUNUM EQU   S99TUNIT+2
S99TUKEY EQU   S99TUNIT
S99TUPND EQU   S99TUP
S99TUPLN EQU   S99TUPTR
S99FLG24 EQU   S99FLAG2+3
S99FLG23 EQU   S99FLAG2+2
S99PCINT EQU   S99FLG22
S99UDEVT EQU   S99FLG22
S99MOUNT EQU   S99FLG21
S99CATLG EQU   S99FLG21
S99TIONQ EQU   S99FLG21
S99OFFLN EQU   S99FLG21
S99WTUNT EQU   S99FLG21
S99NORES EQU   S99FLG21
S99WTDSN EQU   S99FLG21
S99WTVOL EQU   S99FLG21
S99RSV01 EQU   S99RB+12
S99INFO  EQU   S99RSC+2
S99FLG12 EQU   S99FLAG1+1
S99JBSYS EQU   S99FLG11
S99NOMNT EQU   S99FLG11
S99NOCNV EQU   S99FLG11
S99ONCNV EQU   S99FLG11
S99VERB  EQU   S99RB+1
S99RBLN  EQU   S99RB
ADLPARM  EQU   ADLTEXT+6
ADDUNIT  EQU   TEXTCORE+36
@NM00034 EQU   TEXTCORE+2
TURTDSO  EQU   TEXTRET+100
TURTDDN  EQU   TEXTRET+96
TUDISP   EQU   TEXTRET+92
TURND    EQU   TEXTRET+88
TURLSE   EQU   TEXTRET+84
TUVSEQ   EQU   TEXTRET+80
TUPRIV   EQU   TEXTRET+76
TUMAXV   EQU   TEXTRET+72
TUPOS    EQU   TEXTRET+68
TULABL   EQU   TEXTRET+64
TUUCNT   EQU   TEXTRET+60
TUUNIT   EQU   TEXTRET+56
TUHOLD   EQU   TEXTRET+52
TUDEST   EQU   TEXTRET+48
TUUSING  EQU   TEXTRET+44
TUDIR    EQU   TEXTRET+40
TUSPACE  EQU   TEXTRET+36
TUBLOK   EQU   TEXTRET+32
TUVOL    EQU   TEXTRET+28
TUSTATS  EQU   TEXTRET+24
TUFILE   EQU   TEXTRET+20
ENTRYNBR EQU   TEXTRET+4
SBPLSIZE EQU   TEXTRET
DA04DSO  EQU   DAPB04+15
@NM00033 EQU   DAPB04+13
@NM00032 EQU   DA04CTL
@NM00031 EQU   DA04CTL
@NM00030 EQU   DA04CTL
@NM00029 EQU   DA04CTL
@NM00028 EQU   DA04CTL
DA04UID  EQU   DA04CTL
@NM00027 EQU   DA04CTL
@NM00026 EQU   DA04CTL
@NM00025 EQU   DAPB04+4
@NM00024 EQU   DA04FLG
@NM00023 EQU   DA04FLG
@NM00022 EQU   DA04FLG
@NM00021 EQU   DA04FLG
@NM00020 EQU   DA04FLG
@NM00019 EQU   DA04FLG
IDNUM    EQU   DFID+1
@NM00018 EQU   @NM00017
DFWTP    EQU   @NM00017
@NM00016 EQU   DSNAMPDE+24
@NM00015 EQU   DSNAMPDE+23
@NM00014 EQU   @NM00013
PSWDPRS1 EQU   @NM00013
PSWDLEN1 EQU   DSNAMPDE+20
@NM00012 EQU   DSNAMPDE+15
@NM00011 EQU   MBRFLG1
MBRLEN1  EQU   DSNAMPDE+12
@NM00010 EQU   DSNAMPDE+7
@NM00009 EQU   DSNFLG1
QUALIF   EQU   DSNFLG1
ONPRESDS EQU   DSNFLG1
DSNLEN1  EQU   DSNAMPDE+4
@NM00008 EQU   DSNPDE1+4
@NM00007 EQU   DSNPDE1
PPLUWA   EQU   PPL+24
PPLCBUF  EQU   PPL+20
PPLANS   EQU   PPL+16
PPLECB   EQU   PPL+8
PPLECT   EQU   PPL+4
PPLUPT   EQU   PPL
CPPLCBUF EQU   CPPL
MSVGRSV2 EQU   MSVGPDE+7
MSVGRSV1 EQU   MSVGFLG
MSVGPRES EQU   MSVGFLG
MSVGLEN  EQU   MSVGPDE+4
VOLRSV2  EQU   VOLSQPDE+7
VOLRSV1  EQU   VOLSQFLG
VOLPRES  EQU   VOLSQFLG
VOLSQLEN EQU   VOLSQPDE+4
MXVRSV2  EQU   MXVOLPDE+7
MXVRSV1  EQU   MXVOLFLG
MXVPRES  EQU   MXVOLFLG
MXVOLLEN EQU   MXVOLPDE+4
POSRSV2  EQU   POSTPDE+7
POSRSV1  EQU   POSFLAG
POSPRES  EQU   POSFLAG
POSLEN   EQU   POSTPDE+4
LABLRSV1 EQU   ALLOCPDL+178+4                                      @26C
LABELPDE EQU   ALLOCPDL+176+4                                      @26C
UCNTRSV2 EQU   UNCNTPDE+7
UCNTRSV1 EQU   UCNTFLAG
UCNTPRES EQU   UCNTFLAG
UCNTLEN  EQU   UNCNTPDE+4
UTYPRSV2 EQU   UTYPPDE+7
UTYPRSV1 EQU   UTYPFLAG
UTYPPRES EQU   UTYPFLAG
UTYPLEN  EQU   UTYPPDE+4
USIDRSV2 EQU   USIDPDE+7
USIDRSV1 EQU   USIDFLAG
USIDPRES EQU   USIDFLAG
USIDLEN  EQU   USIDPDE+4
ATTRRSV2 EQU   ATTRPDE+7
ATTRRSV1 EQU   ATTRFLAG
ATTRPRES EQU   ATTRFLAG
ATTRLEN  EQU   ATTRPDE+4
DIRRSV2  EQU   DIRECPDE+7
DIRRSV1  EQU   DIRFLAGS
DIRPRES  EQU   DIRFLAGS
DIRLEN   EQU   DIRECPDE+4
SPCRSV22 EQU   SPACPDE2+7
SPCRSV12 EQU   SPACFLG2
SPCPRES2 EQU   SPACFLG2
SPACLEN2 EQU   SPACPDE2+4
SPACRSV2 EQU   SPACPDE+7
SPACRSV1 EQU   SPACEFLG
SPACPRES EQU   SPACEFLG
SPACELEN EQU   SPACPDE+4
ABLKRSV2 EQU   ABLKPDE+7
ABLKRSV1 EQU   ABLKFLG
ABLKPRES EQU   ABLKFLG
ABLKLEN  EQU   ABLKPDE+4
BLKRSV2  EQU   BLKPDE+7
BLKRSV1  EQU   BLKFLAGS
BLKPRES  EQU   BLKFLAGS
BLKLEN   EQU   BLKPDE+4
SERNEXT  EQU   SERPDE+8
SERRSV2  EQU   SERPDE+7
SERRSV1  EQU   SERFLAGS
SERPRES  EQU   SERFLAGS
SERLEN   EQU   SERPDE+4
CLASRSV2 EQU   CLASPDE+7
CLASRSV1 EQU   CLASFLG
CLASLEN  EQU   CLASPDE+4
DDNRSV2  EQU   DDNMEPDE+7
DDNRSV1  EQU   DDNFLAGS
DDNPRES  EQU   DDNFLAGS
DDNLEN   EQU   DDNMEPDE+4
DSNNEXT  EQU   DSNPDE+24
PASSRSV2 EQU   DSNPDE+23
PASSRSV1 EQU   PASSFLAG
PASSPRES EQU   PASSFLAG
PASSLEN  EQU   DSNPDE+20
MBRRSV2  EQU   DSNPDE+15
MBRRSV1  EQU   MBRFLAGS
MBRPRES  EQU   MBRFLAGS
MBRLEN   EQU   DSNPDE+12
DSNRSV2  EQU   DSNPDE+7
DSNRSV1  EQU   DSNFLAGS
DSNQUOT  EQU   DSNFLAGS
RNDPDE   EQU   ALLOCPDL+44
RLSEPDE  EQU   ALLOCPDL+42
VSEQPDE  EQU   ALLOCPDL+40
PRIVPDE  EQU   ALLOCPDL+38
MAXVPDE  EQU   ALLOCPDL+36
POSPDE   EQU   ALLOCPDL+34
LABLPDE  EQU   ALLOCPDL+32
UCNTPDE  EQU   ALLOCPDL+30
UNITPDE  EQU   ALLOCPDL+28
DESTPDE  EQU   ALLOCPDL+24
USINGPDE EQU   ALLOCPDL+22
DIRPDE   EQU   ALLOCPDL+20
FILEPDE  EQU   ALLOCPDL+10
@NM00006 EQU   ALLOCPDL+4
@NM00005 EQU   ALLOCPDL
FIRSTPTR EQU   ALLOCWA+176
NBLKBUF1 EQU   ALLOCWA+164
PDLADDR3 EQU   ALLOCWA+156
PPLTWO   EQU   ALLOCWA+128
CMDTWOBF EQU   CMDTWO+4
CMDTWOOF EQU   CMDTWO+2
CMDTWOLN EQU   CMDTWO
Q        EQU   ALLOCWA+56
P        EQU   ALLOCWA+52
MSGPTR   EQU   ALLOCWA+48
PRCODE   EQU   ALLOCWA+44
@NM00004 EQU   ALLOCWA+38
NCLASBUF EQU   ALLOCWA+37
@NM00003 EQU   VCFLAGS
SPACE2   EQU   VCFLAGS
SPACE1   EQU   VCFLAGS
AVBLK    EQU   VCFLAGS
PTRMSGS  EQU   ALLOCWA+32
DSNCTR   EQU   ALLOCWA+30
VLISTCTR EQU   ALLOCWA+28
@NM00002 EQU   SWITCH2
@NM00001 EQU   SWITCH2
FIRSTPDE EQU   SWITCH
ASTRSK   EQU   SWITCH
TERMOPT  EQU   SWITCH
PROMPT   EQU   SWITCH
*                                      END UNREFERENCED COMPONENTS
@RC00110 EQU   @EL00001
@RF00134 EQU   @EL00002
@RF00141 EQU   @EL00002
@RF00144 EQU   @EL00002
@RC00146 EQU   @EL00002
@RF00188 EQU   @EL00003
@RF00191 EQU   @EL00003
@RC00193 EQU   @EL00003
@RF00228 EQU   @EL00004
@RF00243 EQU   @EL00004
@RF00246 EQU   @EL00004
@RC00248 EQU   @EL00004
@RF00281 EQU   @EL00005
@RF00285 EQU   @EL00005
@RF00288 EQU   @EL00005
@RC00290 EQU   @EL00005
@RF00318 EQU   @EL00006
@RF00332 EQU   @EL00006
@RF00359 EQU   @EL00006
@RC00394 EQU   @EL00007
@RC00406 EQU   @EL00007
@RF00433 EQU   @EL00008
@RF00449 EQU   @EL00008
@RF00460 EQU   @EL00008
@RC00467 EQU   @EL00008
@RC00462 EQU   @EL00008
@RT00528 EQU   DSNAD1
@RC00497 EQU   @EL00009
@RC00548 EQU   @EL00010
@RF00559 EQU   @EL00010
@RC00590 EQU   @EL00011
@RF00398 EQU   @RC00394
@RC00400 EQU   @RC00394
@RF00473 EQU   @RC00467
@RC00550 EQU   @RC00548
@EL01    EQU   @EL00001
@ENDDATA EQU   *
*   END IKJEFD32                                                   0610
*                                                                  0610
         PRINT NOGEN
*/* THE FOLLOWING INCLUDE STATEMENTS WERE FOUND IN THIS PROGRAM.     */
*/*%INCLUDE SYSLIB  (IKJZT430)                                       */
*/*%INCLUDE SYSLIB  (IKJCPPL )                                       */
*/*%INCLUDE SYSLIB  (IKJPPL  )                                       */
*/*%INCLUDE SYSLIB  (IKJDAPL )                                       */
*/*%INCLUDE SYSLIB  (IKJEFFDF)                                       */
*/*%INCLUDE SYSLIB  (IKJDAP04)                                       */
*/*%INCLUDE SYSLIB  (IKJZB831)                                       */
*/*%INCLUDE SYSLIB  (IEFZB4D0)                                       */
*/*%INCLUDE SYSLIB  (IEFZB4D2)                                       */
*/*%INCLUDE SYSLIB  (CVT     )                                       */
*                                                                  0610
         CVT   DSECT=YES
*       ;                                                          0610
         END   IKJEFD32,(C'PLS1647',0702,83336)
/*
//*
//STEP07  EXEC PGM=IEBGENER
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  *
  IDENTIFY IKJEFD32('ZP60026')
++MOD(IKJEFD33) DISTLIB(AOST4).
/*
//SYSUT2   DD  DSN=&&SMPMCS,DISP=(MOD,PASS)
//SYSIN    DD  DUMMY
//*
//STEP08  EXEC PGM=IFOX00,PARM='OBJECT,NODECK,NOTERM,XREF(SHORT)'
//SYSPRINT DD  SYSOUT=*
//SYSLIB   DD  DSN=SYS1.MACLIB,DISP=SHR
//         DD  DSN=SYS1.SMPMTS,DISP=SHR
//         DD  DSN=SYS1.AMODGEN,DISP=SHR
//SYSUT1   DD  UNIT=SYSALLDA,SPACE=(CYL,10)
//SYSUT2   DD  UNIT=SYSALLDA,SPACE=(CYL,10)
//SYSUT3   DD  UNIT=SYSALLDA,SPACE=(CYL,10)
//SYSGO    DD  DSN=&&SMPMCS,DISP=(MOD,PASS)
//SYSIN    DD  *
         TITLE 'ALLOCATE SPACE PROCESSOR                               *
                        '
***********************************************************************
*  $26   ZP60026  2010-01-23 GREG PRICE: ADD 'REUSE' OPERAND.         *
*                            CHANGES APPLIED TO UZ58132 LEVEL.        *
***********************************************************************
IKJEFD33 CSECT ,                                                   0001
@MAINENT DS    0H                                                  0001
         USING *,R15                                               0001
         B     @PROLOG                                             0001
         DC    AL1(33)                                             @26C
         DC    C'IKJEFD33  82.173'                                 0001
         DC    CL17' ZP60026 2010.023'                             @26A
         DROP  R15
@PROLOG  STM   R14,R12,12(R13)                                     0001
         BALR  R12,0                                               0001
@PSTART  DS    0H                                                  0001
         USING @PSTART,R12                                         0001
         L     R0,@SIZDATD                                         0001
         GETMAIN  R,LV=(0)
         LR    R11,R1                                              0001
         USING @DATD,R11                                           0001
         ST    R13,@SA00001+4                                      0001
         LM    R0,R1,20(R13)                                       0001
         ST    R11,8(,R13)                                         0001
         LR    R13,R11                                             0001
         XC    @ZTEMPS(@ZLEN),@ZTEMPS
         MVC   @PC00001(4),0(R1)                                   0001
*/********************************************************************/
*/*                                                                  */
*/* SEGMENT NAME -                                                   */
*/*      SPACERTE                                                    */
*/*                                                                  */
*/* FUNCTION -                                                       */
*/*      THIS ROUTINE HANDLES ALL SPACE PARAMETERS (SPACE, SPACE     */
*/*      INCREMENT, BLOCK, AVBLOCK, TRACKS, CYLINDERS, AND DIR).     */
*/*      PROMPTING AND BUILDING TEXT UNITS IS DONE WHERE NECESSARY.  */
*/*                                                                  */
*/* OPERATION -                                                      */
*/*      IF BLOCK ENTERED, AND USING NOT, BUILD TEXT UNIT FOR DCB    */
*/*      PARAMETER BLOCKSIZE. IF BLOCK IS ENTERED AND SPACE IS NOT,  */
*/*      MARK BLOCK NOT ENTERED - TO BE USED ONLY AS DCB BLOCKSIZE.  */
*/*      IF BLOCK IS ENTERED, AND SPACE IS ENTERED, INVOKE SPCTEXT   */
*/*      TO PROCESS SPACE. IF BLOCK NOT ENTERED, BUT AVBLOCK, TRACKS */
*/*      OR CYLINDERS IS ENTERED AND SPACE IS ENTERED, INVOKE        */
*/*      SPCTEXT TO PROCESS SPACE. IF SPACE IS NOT ENTERED, PROMPT   */
*/*      FOR SPACE PARAMETER. IF NEITHER OF THE BLOCK, AVBLOCK,      */
*/*      TRACKS OR CYLINDERS IS ENTERED, AND SPACE IS ENTERED,       */
*/*      PROMPT FOR BLOCK PARAMETER AND REPEAT ABOVE PROCESS. IF     */
*/*      DIR IS NOT ENTERED AND A MEMBER NAME IS SPECIFIED, PROMPT   */
*/*      FOR DIR VALUE.                                              */
*/*      IF RLSE OR ROUND ARE ENTERED BUT SPACE AND UNIT OF  @ZA08703*/
*/*      SPACE WERE NOT ENTERED SPACE AND UNIT OF SPACE WILL @ZA08703*/
*/*      BE DEFAULTED.                                       @ZA08703*/
*/*                                                                  */
*/* INPUT -                                                          */
*/*      PTR TO SPACE PDE                                            */
*/*      PTR TO USING PDE                                            */
*/*      PTR TO DSNAME PDE                                           */
*/*      PTR TO BLOCK/AVBLOCK/TRACKS/CYLINDERS PDE                   */
*/*      PTR TO DIR PDE                                              */
*/*                                                                  */
*/* OUTPUT -                                                         */
*/*      NECESSARY PROMPTING DONE                                    */
*/*      TEXT UNITS BUILT WHERE NEEDED                               */
*/*                                                                  */
*/* NOTE:  THIS ROUTINE REPRESENTS A DIRECT LINE FUNCTION AND CAN    */
*/*        BE INCORPORATED AS AN INCLUDED CODE SEGMENT WHEN THE      */
*/*        INCLUDE FUNCTION HAS RECEIVED A STANDARD DEFINITION.      */
*/********************************************************************/
*    SPACERTE:                                                     0061
*        AWAPTR = R1;                  /* PTR TO WORKAREA            */
SPACERTE LR    R3AWAPTR,R1                                         0061
*        CNVRTYPE = '00'X;             /* INITIALIZE FLAGS           */
         MVI   CNVRTYPE,X'00'                                      0062
*        INC = ADDNUM + 1;             /* INITIALIZE SUBSCRIPT       */
         L     R10,GTPLPTR(,R3AWAPTR)                              0063
         L     R10,GTPLABLK(,R10)                                  0063
         LA    R4INC,1                                             0063
         AH    R4INC,ADDNUM(,R10)                                  0063
*        IF DIRPDE = 0 THEN            /* IF DIR NOT ENTERED,        */
         L     R10,PTRPDL(,R3AWAPTR)                               0064
         LH    R2,DIRPDE(,R10)                                     0064
         LTR   R2,R2                                               0064
         BNZ   @RF00064                                            0064
*          DO;                         /* AND                        */
*            IF MBRPRES = '1'B &       /* IF MEMBER SPEC AND @OZ05962*/
*              DSEXISTS = '0'B THEN   /* DATA SET NEW       @OZ05962*/
         TM    MBRPRES(R10),B'10000000'                            0066
         BNO   @RF00066                                            0066
         TM    DSEXISTS(R3AWAPTR),B'00100000'                      0066
         BNZ   @RF00066                                            0066
*              CALL PRMPTDIR;          /* POINT FOR DIRECTORY        */
         BAL   R14,PRMPTDIR                                        0067
*          END;                                                    0068
@RF00066 DS    0H                                                  0069
*        DO WHILE (CONT ='0'B);        /* CHECK CONTROL SWITCH       */
@RF00064 B     @DE00069                                            0069
@DL00069 DS    0H                                                  0070
*        IF BLOKPDE = 1 THEN           /* IF BLOCK ENTERED,          */
         L     R10,PTRPDL(,R3AWAPTR)                               0070
         CLC   BLOKPDE(2,R10),@HW1                                 0070
         BNE   @RF00070                                            0070
*          DO;                                                     0071
*            CONT= '1'B;               /* INDICATE TO EXIT DO-WHILE  */
         OI    CONT(R3AWAPTR),B'00010000'                          0072
*            IF USINGPDE = 0 THEN      /* IF USING NOT ENTERED,      */
         LH    R10,USINGPDE(,R10)                                  0073
         LTR   R10,R10                                             0073
         BNZ   @RF00073                                            0073
*              DO;                                                 0074
*                BLKCNVRT = '1'B;      /* INDICATE TO CONVERT BLOCK  */
         OI    BLKCNVRT,B'10000000'                                0075
*                CALL CONVERT;         /* INVOKE CONVERSION RTN      */
         BAL   R14,CONVERT                                         0076
*                CALL DCBTEXT;         /* SET UP DCB TEXT            */
         BAL   R14,DCBTEXT                                         0077
*              END;                                                0078
*            IF SPACEPDE = 0 & DIRPDE=0 THEN /* IF SPACE NOT       0079
*                                               ENTERED,     @YM06119*/
@RF00073 SLR   R10,R10                                             0079
         L     R2,PTRPDL(,R3AWAPTR)                                0079
         CH    R10,SPACEPDE(,R2)                                   0079
         BNE   @RF00079                                            0079
         CH    R10,DIRPDE(,R2)                                     0079
         BNE   @RF00079                                            0079
*              BLOKPDE = 0;            /* MARK BLOCK IGNORED         */
         STH   R10,BLOKPDE(,R2)                                    0080
*            ELSE                      /* IF SPACE ENTERED,          */
*              DO;                                   /*      @YM06119*/
         B     @RC00079                                            0081
@RF00079 DS    0H                                                  0082
*                IF SPACEPDE = 0 & DIRPDE = 1 THEN   /*      @YM06119*/
         L     R10,PTRPDL(,R3AWAPTR)                               0082
         LH    R2,SPACEPDE(,R10)                                   0082
         LTR   R2,R2                                               0082
         BNZ   @RF00082                                            0082
         CLC   DIRPDE(2,R10),@HW1                                  0082
         BNE   @RF00082                                            0082
*                  CALL PRMPTSPC;                    /*      @YM06119*/
         BAL   R14,PRMPTSPC                                        0083
*                CALL SPCTEXT;                       /*      @YM06119*/
@RF00082 BAL   R14,SPCTEXT                                         0084
*              END;                                  /*      @YM06119*/
*          END;                                                    0086
*        ELSE                                                      0087
*          DO;                         /* IF BLOCK NOT ENTERED,      */
         B     @RC00070                                            0087
@RF00070 DS    0H                                                  0088
*            IF BLOKPDE > 1 THEN       /* BUT AVBLOCK, TRK OR CYL    */
         LA    R10,1                                               0088
         L     R2,PTRPDL(,R3AWAPTR)                                0088
         CH    R10,BLOKPDE(,R2)                                    0088
         BNL   @RF00088                                            0088
*              DO;                     /* ENTERED,                   */
*                CONT = '1'B;          /* INDICATE TO EXIT DO-WHILE  */
         OI    CONT(R3AWAPTR),B'00010000'                          0090
*                IF SPACEPDE = 1 THEN  /* IF SPACE ENTERED,          */
         CH    R10,SPACEPDE(,R2)                                   0091
         BNE   @RF00091                                            0091
*                  CALL SPCTEXT;       /* GO TO PROCESS              */
         BAL   R14,SPCTEXT                                         0092
*                ELSE                  /* IF SPACE NOT ENTERED,      */
*                  DO;                 /* THEN                       */
         B     @RC00091                                            0093
@RF00091 DS    0H                                                  0094
*                    CALL PRMPTSPC;    /* PROMPT FOR SPACE VALUE     */
         BAL   R14,PRMPTSPC                                        0094
*                    IF RBCODE12 = '0'B THEN                       0095
         TM    RBCODE12(R3AWAPTR),B'10000000'                      0095
         BNZ   @RF00095                                            0095
*                      CALL SPCTEXT;   /* PROCESS SPACE PARMS        */
         BAL   R14,SPCTEXT                                         0096
*                  END;                                            0097
*              END;                                                0098
*            ELSE                      /* IF NEITHER BLOCK,AVBLOCK,  */
*              DO;                     /* TRK OR CYL ENTERED,        */
         B     @RC00088                                            0099
@RF00088 DS    0H                                                  0100
*                IF SPACEPDE = 1 | DIRPDE=1 THEN /* AND SPACE IS   0100
*                                                   ENTERED, @YM06119*/
         LA    R10,1                                               0100
         L     R2,PTRPDL(,R3AWAPTR)                                0100
         CH    R10,SPACEPDE(,R2)                                   0100
         BE    @RT00100                                            0100
         CH    R10,DIRPDE(,R2)                                     0100
         BNE   @RF00100                                            0100
@RT00100 DS    0H                                                  0101
*                  DO;                                             0101
*                  CALL PRMPTBLK;      /* PROMPT FOR BLOCK PARM      */
         BAL   R14,PRMPTBLK                                        0102
*                  IF RBCODE12 = '1'B THEN                         0103
         TM    RBCODE12(R3AWAPTR),B'10000000'                      0103
         BNO   @RF00103                                            0103
*                    CONT = '1'B;                                  0104
         OI    CONT(R3AWAPTR),B'00010000'                          0104
*                  END;                                            0105
*                ELSE                  /* OTHERWISE,                 */
*                  IF RLSEPDE ^= 0 |   /* IF ANY OTHER SPACE @ZA08703*/
*                     RNDPDE ^= 0      /* TYPE INFO SPECIFIED@ZA08703*/
*                    THEN              /* SPACE AND UNIT OF  @ZA08703*/
         B     @RC00100                                            0106
@RF00100 SLR   R10,R10                                             0106
         L     R2,PTRPDL(,R3AWAPTR)                                0106
         CH    R10,RLSEPDE(,R2)                                    0106
         BNE   @RT00106                                            0106
         CH    R10,RNDPDE(,R2)                                     0106
         BE    @RF00106                                            0106
@RT00106 DS    0H                                                  0107
*                      CALL SPCDEFLT;  /* SPACE GET DEFAULTED@ZA08703*/
         BAL   R14,SPCDEFLT                                        0107
*                    ELSE              /* OTHERWISE          @ZA08703*/
*                      CONT = '1'B;    /* GET OUT OF LOOP    @ZA08703*/
         B     @RC00106                                            0108
@RF00106 OI    CONT(R3AWAPTR),B'00010000'                          0108
*              END;                                                0109
@RC00106 DS    0H                                                  0109
@RC00100 DS    0H                                                  0110
*          END;                                                    0110
@RC00088 DS    0H                                                  0111
*        END;                          /* END DO-WHILE               */
@RC00070 DS    0H                                                  0111
@DE00069 TM    CONT(R3AWAPTR),B'00010000'                          0111
         BZ    @DL00069                                            0111
*        RETURN;                                                   0112
@EL00001 L     R13,4(,R13)                                         0112
@EF00001 L     R0,@SIZDATD                                         0112
         LR    R1,R11                                              0112
         FREEMAIN R,LV=(0),A=(1)
@ER00001 LM    R14,R12,12(R13)                                     0112
         BR    R14                                                 0112
*/********************************************************************/
*/*                                                                  */
*/* SEGMENT NAME -                                                   */
*/*      SPCTEXT                                                     */
*/*                                                                  */
*/* FUNCTION -                                                       */
*/*      THIS ROUTINE CONVERTS THE PRIMARY AND SECONDARY SPACE       */
*/*      PARAMETERS TO BINARY, AND BUILDS THE TEXT UNITS FOR SPACE.  */
*/*                                                                  */
*/* OPERATION -                                                      */
*/*      THE PRIMARY SPACE QUANTITY IS CONVERTED TO BINARY. THE TEXT */
*/*      UNIT IS BUILT. IF A SECONDARY SPACE VALUE IS SPECIFIED, IT  */
*/*      IS CONVERTED TO BINARY. THE TEXT UNIT IS BUILT. THE SPACE   */
*/*      PDE IS MARKED NOT ENTERED - SO THAT GENTRANS WILL NOT TRY   */
*/*      TO TRANSLATE IT.                                            */
*/*                                                                  */
*/* INPUT -                                                          */
*/*      SPACE PDE                                                   */
*/*                                                                  */
*/* OUTPUT -                                                         */
*/*      SPACE TEXT UNITS IN LIST                                    */
*/*                                                                  */
*/* NOTE:  THIS ROUTINE REPRESENTS A DIRECT LINE FUNCTION AND CAN    */
*/*        BE INCORPORATED AS AN INCLUDED CODE SEGMENT WHEN THE      */
*/*        INCLUDE FUNCTION HAS RECEIVED A STANDARD DEFINITION.      */
*/*                                                                  */
*/*        THE GENTRANS ROUTINE CANNOT HANDLE THE SPACE PARAMETER    */
*/*        BECAUSE OF THE FORMAT - A KEYWORD WITH A SUBFIELD LIST,   */
*/*        WHERE EACH ELEMENT IN THE LIST HAS A DIFFERENT KEY.  IF   */
*/*        THE FORMAT CHANGES, OR GENTRANS CHANGES, THESE TEXT UNITS */
*/*        MAY BE HANDLED BY GENTRANS.                               */
*/********************************************************************/
*    SPCTEXT: PROC;                                                0113
SPCTEXT  STM   R14,R3,@SA00002                                     0113
         STM   R6,R12,@SA00002+24                                  0113
*        SPCCNVRT = '1'B;              /* INDICATE TO CONVERT SPACE  */
         OI    SPCCNVRT,B'01000000'                                0114
*        CALL CONVERT;                 /* INVOKE CONVERSION RTN      */
         BAL   R14,CONVERT                                         0115
*        ADDNUM = ADDNUM + 1;          /* INCREMENT TEXT COUNTER     */
         L     R10,GTPLPTR(,R3AWAPTR)                              0116
         L     R10,GTPLABLK(,R10)                                  0116
         LA    R2,1                                                0116
         LH    R15,ADDNUM(,R10)                                    0116
         ALR   R15,R2                                              0116
         STH   R15,ADDNUM(,R10)                                    0116
*        ADLKEY = DALPRIME;            /* SET KEY,                   */
         L     R15,TXT(,R3AWAPTR)                                  0117
         MVC   ADLKEY(2,R15),@HWPRIME                              0117
*        ADLNBR = 1;                   /* NUMBER,                    */
         STH   R2,ADLNBR(,R15)                                     0118
*        ADLLEN = 3;                   /* LENGTH AND PARM IN TEXT    */
         LA    R14,3                                               0119
         STH   R14,ADLLEN(,R15)                                    0119
*        ADLPARM(1:ADLLEN)=SAVPARM(2:4);                           0120
         LR    R9,R14                                              0120
         BCTR  R9,0                                                0120
         EX    R9,@SM00677                                         0120
*        ADDTEXT(INC) = TXT;           /* PTR TO TEXT IN LIST        */
         LR    R9,R4INC                                            0121
         SLA   R9,2                                                0121
         ST    R15,ADDTEXT-4(R9,R10)                               0121
*        TXT=TXT+6 + ADLLEN;           /* INCREMNT TEXT PTR          */
         AL    R15,@FW6                                            0122
         ALR   R15,R14                                             0122
         ST    R15,TXT(,R3AWAPTR)                                  0122
*        INC=INC+1;                    /* INCREMENT SUBSCRIPT        */
         ALR   R4INC,R2                                            0123
*        IF SPCPRES2 = '1'B THEN       /* IF SECONDARY SPACE ENTERED */
         L     R10,PTRPDL(,R3AWAPTR)                               0124
         TM    SPCPRES2(R10),B'10000000'                           0124
         BNO   @RF00124                                            0124
*          DO;                                                     0125
*            INCCNVRT = '1'B;          /* INDICATE TO CONVERT SECND  */
         OI    INCCNVRT,B'00100000'                                0126
*            CALL CONVERT;             /* INVOKE CONVERSION RTN      */
         BAL   R14,CONVERT                                         0127
*            ADDNUM = ADDNUM + 1;      /* INCREMENT TEXT COUNTER     */
         L     R10,GTPLPTR(,R3AWAPTR)                              0128
         L     R10,GTPLABLK(,R10)                                  0128
         LA    R2,1                                                0128
         LH    R15,ADDNUM(,R10)                                    0128
         ALR   R15,R2                                              0128
         STH   R15,ADDNUM(,R10)                                    0128
*            ADLKEY = DALSECND;        /* SET KEY,                   */
         L     R15,TXT(,R3AWAPTR)                                  0129
         MVC   ADLKEY(2,R15),@HWSECND                              0129
*            ADLNBR = 1;               /* NUMBER,                    */
         STH   R2,ADLNBR(,R15)                                     0130
*            ADLLEN = 3;               /* LENGTH AND PARM IN TEXT    */
         LA    R14,3                                               0131
         STH   R14,ADLLEN(,R15)                                    0131
*            ADLPARM(1:ADLLEN)=SAVPARM(2:4);                       0132
         LR    R9,R14                                              0132
         BCTR  R9,0                                                0132
         EX    R9,@SM00677                                         0132
*            ADDTEXT(INC)= TXT;        /* PTR TO TEXT IN LIST        */
         LR    R9,R4INC                                            0133
         SLA   R9,2                                                0133
         ST    R15,ADDTEXT-4(R9,R10)                               0133
*            TXT=TXT+6 + ADLLEN;       /* INCREMENT TEXT PTR         */
         AL    R15,@FW6                                            0134
         ALR   R15,R14                                             0134
         ST    R15,TXT(,R3AWAPTR)                                  0134
*            INC=INC+1;                /* INCREMENT SUBSCRIPT        */
         ALR   R4INC,R2                                            0135
*          END;                                                    0136
*        SPACEPDE = 0;                 /* MARK SPACE NOT ENTERED     */
@RF00124 L     R10,PTRPDL(,R3AWAPTR)                               0137
         SLR   R2,R2                                               0137
         STH   R2,SPACEPDE(,R10)                                   0137
* END SPCTEXT;                                                     0138
@EL00002 DS    0H                                                  0138
@EF00002 DS    0H                                                  0138
@ER00002 LM    R14,R3,@SA00002                                     0138
         LM    R6,R12,@SA00002+24                                  0138
         BR    R14                                                 0138
*/********************************************************************/
*/*                                                                  */
*/* SEGMENT NAME -                                                   */
*/*      CONVERT                                                     */
*/*                                                                  */
*/* FUNCTION -                                                       */
*/*      THIS ROUTINE CONVERTS THE NUMERIC VALUES FOR BLOCK, SPACE   */
*/*      AND SPACE INCREMNT TO BINARY.                               */
*/*                                                                  */
*/* OPERATION -                                                      */
*/*      THE PACK AREA IS CLEARED. DETERMINE THE PARAMETER TO BE     */
*/*      CONVERTED, AND SAVE THE APPROPRIATE LENGTH AND VALUE.       */
*/*      PERFORM THE CONVERSION AND SAVE THE RESULTS.                */
*/*                                                                  */
*/* INPUT -                                                          */
*/*      PTR TO SPACE PDE                                            */
*/*      PTR TO BLOCK PDE                                            */
*/*                                                                  */
*/* OUTPUT -                                                         */
*/*      NUMERIC VALUE CONVERTED TO BINARY                           */
*/*                                                                  */
*/* NOTE:  THIS ROUTINE REPRESENTS A DIRECT LINE FUNCTION AND CAN    */
*/*        BE INCORPORATED AS AN INCLUDED CODE SEGMENT WHEN THE      */
*/*        INCLUDE FUNCTION HAS RECEIVED A STANDARD DEFINITION.      */
*/********************************************************************/
*    CONVERT:  PROC;                                               0139
CONVERT  STM   R14,R4,@SA00003                                     0139
         STM   R6,R12,@SA00003+28                                  0139
*        PKAREA(1) = 'F0'X;            /* FIRST BYTE SET TO F0       */
         MVI   PKAREA,X'F0'                                        0140
*        P = ADDR(PKAREA(1));          /* POINT TO FIRST BYTE        */
         LA    R10,PKAREA                                          0141
         ST    R10,P(,R3AWAPTR)                                    0141
*        Q = ADDR(PKAREA(2));          /* POINT TO SECOND BYTE       */
         LA    R4,PKAREA+1                                         0142
         ST    R4,Q(,R3AWAPTR)                                     0142
*        Q -> AREA1 = P -> AREA2;      /* SET PACKAREA TO ALL F0     */
         MVC   AREA1(15,R4),AREA2(R10)                             0143
*        IF BLKCNVRT = '1'B THEN       /* IF BLOCK TO BE CONVERTED,  */
         TM    BLKCNVRT,B'10000000'                                0144
         BNO   @RF00144                                            0144
*          DO;                                                     0145
*            VALLEN = BLKLEN;          /* SAVE BLOCK LENGTH AND      */
         L     R10,PTRPDL(,R3AWAPTR)                               0146
         LH    R5VALLEN,BLKLEN(,R10)                               0146
*            VALBUF = BLKBUF;          /* BLOCK BUFFER               */
         L     R10,BLKPTR(,R10)                                    0147
         MVC   VALBUF(8),BLKBUF(R10)                               0147
*          END;                                                    0148
*        ELSE                                                      0149
*          IF SPCCNVRT = '1'B THEN     /* IF SPACE TO BE CONVERTED   */
         B     @RC00144                                            0149
@RF00144 TM    SPCCNVRT,B'01000000'                                0149
         BNO   @RF00149                                            0149
*            DO;                                                   0150
*              VALLEN = SPACELEN;      /* SAVE SPACE LENGTH AND      */
         L     R10,PTRPDL(,R3AWAPTR)                               0151
         LH    R5VALLEN,SPACELEN(,R10)                             0151
*              VALBUF = SPACEBUF;      /* SPACE BUFFER               */
         LR    R4,R5VALLEN             COPY LENGTH
         BCTR  R4,0                    DECREMENT FOR EXECUTE
         L     R10,SPACEPTR(,R10)                                  0152
****     MVC   VALBUF(8),SPACEBUF(R10)                             0152
         EX    R4,D33L0678
*            END;                                                  0153
*          ELSE                                                    0154
*            IF INCCNVRT = '1'B THEN   /* IF SECONDARY SPACE TO BE   */
         B     @RC00149                                            0154
@RF00149 TM    INCCNVRT,B'00100000'                                0154
         BNO   @RF00154                                            0154
*              DO;                     /* CONVERTED,                 */
*                VALLEN = SPACLEN2;    /* SAVE SEC SPACE LENGTH      */
         L     R10,PTRPDL(,R3AWAPTR)                               0156
         LH    R5VALLEN,SPACLEN2(,R10)                             0156
*                VALBUF = SPACBUF2;    /* AND SEC SPACE BUFFER       */
         LR    R4,R5VALLEN             COPY LENGTH
         BCTR  R4,0                    DECREMENT FOR EXECUTE
         L     R10,SPACPTR2(,R10)                                  0157
****     MVC   VALBUF(8),SPACBUF2(R10)                             0157
         EX    R4,D33L067E
*              END;                                                0158
*        RFY Y RSTD;                                               0159
@RF00154 DS    0H                                                  0159
@RC00149 DS    0H                                                  0159
@RC00144 DS    0H                                                  0160
*        Y = 8 - VALLEN + ADDR(PKAREA);/* POINT TO BUFFER POSITION   */
         LA    R8Y,8                                               0160
         SLR   R8Y,R5VALLEN                                        0160
         LA    R10,PKAREA                                          0160
         ALR   R8Y,R10                                             0160
*        PKAREA1(1:VALLEN)= VALBUF(1:VALLEN);/* MOVE PARM TO BUFFER  */
         LR    R10,R5VALLEN                                        0161
         BCTR  R10,0                                               0161
         EX    R10,@SM00680                                        0161
*        GENERATE REFS(CVBAREA);                                   0162
         PACK  CVBAREA(8),PKAREA(8)
         CVB   R8Y,CVBAREA
*        SAVPARM = Y;                  /* SAVE RESULT                */
         ST    R8Y,@TF00001                                        0163
         MVC   SAVPARM(4),@TF00001                                 0163
*        RFY Y UNRSTD;                                             0164
*        CNVRTYPE = '00'X;             /* CLEAR FLAG FIELD           */
         MVI   CNVRTYPE,X'00'                                      0165
*   END CONVERT;                                                   0166
@EL00003 DS    0H                                                  0166
@EF00003 DS    0H                                                  0166
@ER00003 LM    R14,R4,@SA00003                                     0166
         LM    R6,R12,@SA00003+28                                  0166
         BR    R14                                                 0166
*/********************************************************************/
*/*                                                                  */
*/* SEGMENT NAME -                                                   */
*/*      DCBTEXT                                                     */
*/*                                                                  */
*/* FUNCTION -                                                       */
*/*      THIS ROUTINE SETS UP THE TEXT UNIT FOR DCB BLOCK SIZE.      */
*/*                                                                  */
*/* OPERATION -                                                      */
*/*      THE COUNTER FOR THE NUMBER OF ENTRIES IS INCREMENTED. THE   */
*/*      KEY, NUMBER, LENGTH, AND VALUE ARE PUT INTO THE TEXT. THE   */
*/*      ADDRESS OF THE TEXT IS PUT INTO THE ADDRESS LIST.           */
*/*                                                                  */
*/* INPUT -                                                          */
*/*      PTR TO VALUE                                                */
*/*                                                                  */
*/* OUTPUT -                                                         */
*/*      TEXT UNIT BUILT                                             */
*/* NOTE:  THIS ROUTINE REPRESENTS A DIRECT LINE FUNCTION AND CAN    */
*/*        BE INCORPORATED AS AN INCLUDED CODE SEGMENT WHEN THE      */
*/*        INCLUDE FUNCTION HAS RECEIVED A STANDARD DEFINITION.      */
*/*                                                                  */
*/*        THIS TEXT UNIT MUST BE SET UP SINCE THE BLOCK PARAMETER   */
*/*        NEEDS 2 KEYS - DCB BLOCK SIZE, AND AVERAGE RECORD LENGTH. */
*/*        GENTRANS CAN PROCESS THE KEY FOR AVERAGE RECORD LENGTH.   */
*/********************************************************************/
*    DCBTEXT:  PROC OPTIONS(NOSAVEAREA);                           0167
DCBTEXT  STM   R14,R3,12(R13)                                      0167
         STM   R5,R12,40(R13)                                      0167
*        ADDNUM = ADDNUM + 1;               /* INCREMENT ENTRY CTR   */
         L     R10,GTPLPTR(,R3AWAPTR)                              0168
         L     R10,GTPLABLK(,R10)                                  0168
         LA    R5,1                                                0168
         LH    R2,ADDNUM(,R10)                                     0168
         ALR   R2,R5                                               0168
         STH   R2,ADDNUM(,R10)                                     0168
*        ADLKEY = DALBLKSZ;                 /* PUT KEY,              */
         L     R2,TXT(,R3AWAPTR)                                   0169
         MVC   ADLKEY(2,R2),@HWBLKSZ                               0169
*        ADLNBR = 1;                        /* NUMBER,               */
         STH   R5,ADLNBR(,R2)                                      0170
*        ADLLEN = 2;                        /* LENGTH AND            */
         LA    R15,2                                               0171
         STH   R15,ADLLEN(,R2)                                     0171
*        ADLPARM(1:ADLLEN)=SAVPARM(3:4);    /* PARM INTO TEXT        */
         BCTR  R15,0                                               0172
         EX    R15,@SM00682                                        0172
*        ADDTEXT(INC) = TXT;                /* TEXT PTR IN LIST      */
         LR    R15,R4INC                                           0173
         SLA   R15,2                                               0173
         ST    R2,ADDTEXT-4(R15,R10)                               0173
*        TXT = TXT + 8;                     /* INCREMENT TEXT PTR    */
         AL    R2,@FW8                                             0174
         ST    R2,TXT(,R3AWAPTR)                                   0174
*        INC = INC + 1;                     /* INCREMENT SUBSCRIPT   */
         ALR   R4INC,R5                                            0175
*   END  DCBTEXT;                                                  0176
@EL00004 DS    0H                                                  0176
@EF00004 DS    0H                                                  0176
@ER00004 LM    R14,R3,12(R13)                                      0176
         LM    R5,R12,40(R13)                                      0176
         BR    R14                                                 0176
*/********************************************************************/
*/*                                                                  */
*/* SEGMENT NAME -                                                   */
*/*      PRMPTSPC                                                    */
*/*                                                                  */
*/* FUNCTION -                                                       */
*/*      THIS ROUTINE REENTERS PARSE TO PROMPT FOR A SPACE VALUE     */
*/*      WHEN ONE HAS NOT BEEN ENTERED AND IS REQUIRED.              */
*/*                                                                  */
*/* OPERATION -                                                      */
*/*      INVOKE PARSE WITH A SECONDARY PCL AND PDL ADDRESS. IF PARSE */
*/*      FAILS, INVOKE PARSERR ROUTINE. ELSE, OVERLAY THE ORIGINAL   */
*/*      PDE WITH THE PDE ROTURNED FORM THE PROMPT. RELEASE THE      */
*/*      PROMPT PDE.                                                 */
*/*                                                                  */
*/* INPUT -                                                          */
*/*      PTR TO SPACE PDE                                            */
*/*      PTR TO SECONDARY PCL                                        */
*/*                                                                  */
*/* OUTPUT -                                                         */
*/*      SPACE PDE OVERLAID  WITH NEW DATA                           */
*/*                                                                  */
*/* NOTE:  THIS ROTINE REPRESENTS A DIRECT LINE FUNCTION AND CAN     */
*/*        BE INCORPORATED AS AN INCLUDED CODE SEGMENT WHEN THE      */
*/*        INCLUDE FUNCTION HAS RECEIVED A STANDARD DEFINITION.      */
*/********************************************************************/
*    PRMPTSPC: PROC;                                               0177
PRMPTSPC STM   R14,R12,@SA00005                                    0177
*        ENTRYCD = 5;                  /* INDICATE SPACE PROMPT      */
         MVC   ENTRYCD(2,R3AWAPTR),@HW5                            0178
*        R1 = AWAPTR;                  /* WORKAREA PTR IN REG 1      */
         LR    R1,R3AWAPTR                                         0179
*        CALL IKJEFD37;                /* INVOKE PARSE               */
         L     R15,VCONSD37                                        0180
         BALR  R14,R15                                             0180
*        IF RCODESV > 0 THEN           /* IF PARSE FAILED,           */
         L     R10,RCODESV(,R3AWAPTR)                              0181
         LTR   R10,R10                                             0181
         BNP   @RF00181                                            0181
*          DO;                                                     0182
*            ENTRYCD = 2;              /* SET ENTRY CODE             */
         MVC   ENTRYCD(2,R3AWAPTR),@HW2                            0183
*            R1 = AWAPTR;              /* WORKAREA PTR IN REG 1      */
         LR    R1,R3AWAPTR                                         0184
*            CALL IKJEFD35;            /* PROCESS PARSE ERROR        */
         L     R15,VCONSD35                                        0185
         BALR  R14,R15                                             0185
*          END;                                                    0186
*        ELSE                                                      0187
*          DO;                         /* IF PARSE SUCCESSFUL,       */
         B     @RC00181                                            0187
@RF00181 DS    0H                                                  0188
*            SPACEPTR = ADDR(NSPCBUF1);/* PTR IN PDE TO NEW BUFFER   */
         L     R10,PTRPDL(,R3AWAPTR)                               0188
         LA    R2,NSPCBUF1                                         0188
         ST    R2,SPACEPTR(,R10)                                   0188
*            NSPCBUF1(1:SPCLEN1)=SPCBUF1(1:SPCLEN1);               0189
         L     R2,PMPTPDL(,R3AWAPTR)                               0189
         LH    R15,SPCLEN1(,R2)                                    0189
         LR    R14,R15                                             0189
         BCTR  R14,0                                               0189
         L     R1,SPCPTR1(,R2)                                     0189
         EX    R14,@SM00684                                        0189
*            SPACELEN = SPCLEN1;       /* LENGTH, AND                */
         STH   R15,SPACELEN(,R10)                                  0190
*            SPACEFLG = SPCFLAG1;      /* FLAGS - INTO OLD PDE       */
         MVC   SPACEFLG(1,R10),SPCFLAG1(R2)                        0191
*            IF SPC2PRES = '1'B THEN   /* IF SEC SPACE ENTERED,      */
         TM    SPC2PRES(R2),B'10000000'                            0192
         BNO   @RF00192                                            0192
*              DO;                                                 0193
*                SPACPTR2 = ADDR(NSPCBUF2);  /* MOVE POINTER,        */
         LA    R15,NSPCBUF2                                        0194
         ST    R15,SPACPTR2(,R10)                                  0194
*                NSPCBUF2(1:SPCLEN2)=SPCBUF2(1:SPCLEN2);           0195
         LH    R15,SPCLEN2(,R2)                                    0195
         LR    R14,R15                                             0195
         BCTR  R14,0                                               0195
         L     R1,SPCPTR2(,R2)                                     0195
         EX    R14,@SM00686                                        0195
*                SPACLEN2 = SPCLEN2;         /* LENGTH,  AND         */
         STH   R15,SPACLEN2(,R10)                                  0196
*                SPACFLG2 = SPCFLAG2;        /* FLAGS INTO OLD PDE   */
         MVC   SPACFLG2(1,R10),SPCFLAG2(R2)                        0197
*             END;                                                 0198
*            RFY R5 RSTD;                                          0199
@RF00192 DS    0H                                                  0200
*            R5 = ADDR(PMPTPDL);       /* PROMPT PDL IN REG 5        */
         LA    R5,PMPTPDL(,R3AWAPTR)                               0200
*            GEN (IKJRLSA (5));        /* RELEASE PROMPT PDL         */
         IKJRLSA (5)
*            RFY R5 UNRSTD;                                        0202
*          END;                                                    0203
*   END PRMPTSPC;                                                  0204
@EL00005 DS    0H                                                  0204
@EF00005 DS    0H                                                  0204
@ER00005 LM    R14,R12,@SA00005                                    0204
         BR    R14                                                 0204
*/********************************************************************/
*/*                                                                  */
*/* SEGMENT NAME -                                                   */
*/*      PRMPTBLK                                                    */
*/*                                                                  */
*/* FUNCTION -                                                       */
*/*      THIS ROUTINE PROMPTS THE USER TO ENTER BLOCK, AVBLOCK,      */
*/*      TRACKS OR CYLINDERS WHEN THE PARAMETER HAS BEEN OMITTED     */
*/*      AND IS REQUIRED.                                            */
*/*                                                                  */
*/* OPERATION -                                                      */
*/*      INVOKE PARSE WITH A SECONDARY PCL AND PDL ADDRESS. IF PARSE */
*/*      FAILS, INVOKE PARSERR ROUTINE. ELSE, OVERLAY THE ORIGINAL   */
*/*      PDE WITH THE PADE RETURNED FROM THE PROMPT. RELEASE THE     */
*/*      PROMPT PDL.                                                 */
*/*                                                                  */
*/* INPUT -                                                          */
*/*      PTR TO BLOCK PDE                                            */
*/*      PTR TO SECONDARY PCL                                        */
*/*                                                                  */
*/* OUTPUT -                                                         */
*/*      BLOCK PDE OVERLAID WITH NEW DATA                            */
*/*                                                                  */
*/* NOTE:  THIS ROUTINE REPRESENTS A DIRECT LINE FUNCTION AND CAN    */
*/*        BE INCORPORATED AS AN INCLUDED CODE SEGMENT WHEN THE      */
*/*        INLCUDE FUNCTION HAS RECEIVED A STANDARD DEFINITION.      */
*/*************************(******************************************/
*    PRMPTBLK: PROC;                                               0205
PRMPTBLK STM   R14,R12,@SA00006                                    0205
*        ENTRYCD = 7;                  /* INDICATE PROMPT FOR BLOCK  */
         MVC   ENTRYCD(2,R3AWAPTR),@HW7                            0206
*        R1 = AWAPTR;                  /* WORKAREA PTR IN REG 1      */
         LR    R1,R3AWAPTR                                         0207
*        CALL IKJEFD37;                /* INVOKE PARSE               */
         L     R15,VCONSD37                                        0208
         BALR  R14,R15                                             0208
*        IF RCODESV > 0 THEN           /* IF PARSE FAILED,           */
         L     R10,RCODESV(,R3AWAPTR)                              0209
         LTR   R10,R10                                             0209
         BNP   @RF00209                                            0209
*          DO;                         /* THEN                       */
*            ENTRYCD = 2;              /* INDICATE PARSE ERROR       */
         MVC   ENTRYCD(2,R3AWAPTR),@HW2                            0211
*            R1 = AWAPTR;              /* WORKAREA PTR IN REG 1      */
         LR    R1,R3AWAPTR                                         0212
*            CALL IKJEFD35;            /* ISSUE ERROR MESSAGE        */
         L     R15,VCONSD35                                        0213
         BALR  R14,R15                                             0213
*          END;                                                    0214
*        ELSE                                                      0215
*          DO;                         /* IF PARSE SUCCESSFUL,       */
         B     @RC00209                                            0215
@RF00209 DS    0H                                                  0216
*            RFY R5 RSTD;              /* RESTRICT REG 5             */
*            R5 = ADDR(PMPTPDL);       /* PROMPT PDL IN REG 5        */
         LA    R5,PMPTPDL(,R3AWAPTR)                               0217
*            GEN (IKJRLSA (5));        /* RELEASE PROMPT PDL         */
         IKJRLSA (5)
*            RFY R5 UNRSTD;            /* UNRESTRICT REG 5           */
*          END;                                                    0220
*   END PRMPTBLK;                                                  0221
@EL00006 DS    0H                                                  0221
@EF00006 DS    0H                                                  0221
@ER00006 LM    R14,R12,@SA00006                                    0221
         BR    R14                                                 0221
*/********************************************************************/
*/*                                                                  */
*/* SEGMENT NAME -                                                   */
*/*      PRMPTDIR                                                    */
*/*                                                                  */
*/* FUNCTION -                                                       */
*/*      THIS ROUTINE PROMPTS THE USER TO ENTER A DIR VALUE WHEN     */
*/*      THE PARAMETER HAS BEEN OMITTED AND IS REQUIRED.             */
*/*                                                                  */
*/* OPERATION -                                                      */
*/*      INVOKE PARSE WITH A SECONDARY PCL AND PDL ADDRESS. IF PARSE */
*/*      FAILS, INVOKE PARSERR ROUTINE. ELSE, OVERLAY THE ORIGINAL   */
*/*      PDE WITH THE PDE RETURNED FROM THE PROMPT. RELEASE THE      */
*/*      PROMPT PDL.                                                 */
*/*                                                                  */
*/* INPUT -                                                          */
*/*      PTR TO DIR PDE                                              */
*/*      PTR TO SECONDARY PCL                                        */
*/*                                                                  */
*/* OUTPUT -                                                         */
*/*      DIR PDE OVERLAID WITH NEW DATA                              */
*/*                                                                  */
*/* NOTE:  THIS ROUTINE REPRESENTS A DIRECT LINE FUNCTION AND CAN    */
*/*        BE INCORPORATED AS AN INCLUDED CODE SEGMENT WHEN THE      */
*/*        INCLUDE FUNCTION HAS RECEIVED A STANDARD DEFINITION.      */
*/********************************************************************/
*   PRMPTDIR:  PROC;                                               0222
PRMPTDIR STM   R14,R12,@SA00007                                    0222
*        ENTRYCD = 6;                  /* INDICATE PROMPT FOR DIR    */
         MVC   ENTRYCD(2,R3AWAPTR),@HW6                            0223
*        R1 = AWAPTR;                  /* WORKAREA PTR IN REG 1      */
         LR    R1,R3AWAPTR                                         0224
*        CALL IKJEFD37;                /* INVOKE PARSE               */
         L     R15,VCONSD37                                        0225
         BALR  R14,R15                                             0225
*        IF RCODESV > 0 THEN           /* IF PARSE FAILED,           */
         L     R10,RCODESV(,R3AWAPTR)                              0226
         LTR   R10,R10                                             0226
         BNP   @RF00226                                            0226
*          DO;                         /* THEN                       */
*            ENTRYCD = 2;              /* SET ENTRY CODE             */
         MVC   ENTRYCD(2,R3AWAPTR),@HW2                            0228
*            R1 = AWAPTR;              /* WORKAREA PTR IN REG 1      */
         LR    R1,R3AWAPTR                                         0229
*            CALL IKJEFD35;            /* PROCESS PARSE ERROR        */
         L     R15,VCONSD35                                        0230
         BALR  R14,R15                                             0230
*          END;                                                    0231
*        ELSE                                                      0232
*          DO;                         /* IF PARSE SUCCESSFUL,       */
         B     @RC00226                                            0232
@RF00226 DS    0H                                                  0233
*            DIRPDE = 1;                                           0233
         L     R10,PTRPDL(,R3AWAPTR)                               0233
         MVC   DIRPDE(2,R10),@HW1                                  0233
*            DIRPTR = ADDR(NDIRBUF1);  /* PTR IN PDE TO NEW BUFFER   */
         LA    R2,NDIRBUF1                                         0234
         ST    R2,DIRPTR(,R10)                                     0234
*            NDIRBUF1(1:DIRLEN1)=DIRBUF1(1:DIRLEN1);               0235
         L     R2,PMPTPDL(,R3AWAPTR)                               0235
         LH    R15,DIRLEN1(,R2)                                    0235
         LR    R14,R15                                             0235
         BCTR  R14,0                                               0235
         L     R1,DIRPTR1(,R2)                                     0235
         EX    R14,@SM00688                                        0235
*            DIRLEN = DIRLEN1;         /* LENGTH, AND                */
         STH   R15,DIRLEN(,R10)                                    0236
*            DIRFLAGS = DIRFLAG1;      /* FLAGS INTO OLD PDE         */
         MVC   DIRFLAGS(1,R10),DIRFLAG1(R2)                        0237
*            RFY R5 RSTD;              /* RESTRICT REG 5             */
*            R5 = ADDR(PMPTPDL);       /* PROMPT PDL IN REG 5        */
         LA    R5,PMPTPDL(,R3AWAPTR)                               0239
*            GEN (IKJRLSA (5));        /* RELEASE PROMPT PDL         */
         IKJRLSA (5)
*            RFY R5 UNRSTD;            /* UNRESTRICT REG 5           */
*          END;                                                    0242
*   END PRMPTDIR;                                                  0243
@EL00007 DS    0H                                                  0243
@EF00007 DS    0H                                                  0243
@ER00007 LM    R14,R12,@SA00007                                    0243
         BR    R14                                                 0243
*                                                                  0244
*                                                                  0244
*                                                                  0244
*    /****************************************************************/
*    /*                                                              */
*    /* SUBROUTINE - SPCDEFLT                                        */
*    /* FUNCTION - PROVIDE MINIMAL DEFAULTS FOR PRIMARY, SECONDARY   */
*    /*     AND UNIT OF SPACE WHEN NO SPACE DEFAULTING CAN BE        */
*    /*     PERFORMED BY SVC99. THIS OCCURS WHEN KEYWORDS RLSE OR    */
*    /*     ROUND HAVE BEEN SPECIFIED BY THE TERMINAL USER WITHOUT   */
*    /*     ANY OTHER SPACE TYPE PARAMETERS.                 @ZA08703*/
*    /*                                                              */
*    /* OPERATION - LOAD IEFAB445 (ALLOCATION DEFAULT CSECT)         */
*    /*     CONVERT PRIMARY,SECONDARY,AVBLKSIZE AND DIRECTORY        */
*    /*     VALUES TO EBCDIC AND UPDATE PDE'S FOR THESE VALUES       */
*    /*     TO INDICATE THEIR PRESENCE AND POINT TO THESE DEFAULT    */
*    /*     VALUES. DELETE IEFAB445.                                 */
*    /*                                                              */
*    /* INPUT - ALLOCATE COMMAND'S PDL                               */
*    /*         IEFAB445 (ALLOCATION DEFAULT CSECT)                  */
*    /*                                                              */
*    /* OUTPUT - ALLOCATE COMMAND'S PDL OVERLAID WITH DEFAULTED      */
*    /*          DATA.                                               */
*    /*                                                              */
*    /* NOTE - IEFAB445 IS THE ALLOCATION DEFAULT CSECT. IT IS       */
*    /*        MAPPED BY MACRO IEFZB445.                             */
*    /*                                                              */
*    /****************************************************************/
*    SPCDEFLT: PROC;                   /* DEFAULT SPACE      @ZA08703*/
SPCDEFLT STM   R14,R12,@SA00008                                    0244
*          RFY (R0) RSTD;              /* RESERVE REG 0      @ZA08703*/
*          GEN SETS(R0) (LOAD EP=IEFAB445); /* LOAD DEFAULT  @ZA08703*/
         LOAD EP=IEFAB445
*          AB445PTR = R0;              /* CSECT,SET BASE FOR @ZA08703*/
         LR    AB445PTR,R0                                         0247
*          RFY (R0) UNRSTD;            /* MAPPING MACRO      @ZA08703*/
*          INPTR = ADDR(DEFPQTY);      /* MOVE OVERLAY       @ZA08703*/
         LR    R5INPTR,AB445PTR                                    0249
*          CVD(FIXED24,PKAREA);        /* GET EBCDIC FOR     @ZA08703*/
         LR    R10,R5INPTR                                         0250
         BCTR  R10,0                                               0250
         L     R10,FIXED24(,R10)                                   0250
         LA    R10,0(,R10)                                         0250
         CVD   R10,@TS00001                                        0250
         MVC   PKAREA(8),@TS00001                                  0250
*          UNPK(NSPCBUF1,PKAREA(4:8)); /* PRIMARY SPACE      @ZA08703*/
         UNPK  NSPCBUF1(8),PKAREA+3(5)                             0251
*          SPACEPTR = ADDR(NSPCBUF1);  /* OVERLAY PDE        @ZA08703*/
         L     R10,PTRPDL(,R3AWAPTR)                               0252
         LA    R5,NSPCBUF1                                         0252
         ST    R5,SPACEPTR(,R10)                                   0252
*          SPACELEN = 8;               /* SET LENGTH         @ZA08703*/
         LA    R5,8                                                0253
         STH   R5,SPACELEN(,R10)                                   0253
*          SPACPRES = '1'B;            /* INDICATE SPACE     @ZA08703*/
         OI    SPACPRES(R10),B'10000000'                           0254
*          SPACEPDE = 1;               /* PRESENT            @ZA08703*/
         MVC   SPACEPDE(2,R10),@HW1                                0255
*          IF DEFSQTY ^= 0             /* IF SECONDARY AMT   @ZA08703*/
*            THEN DO;                  /* HAS DEFAULT        @ZA08703*/
         MVC   @ZT00001+1(3),DEFSQTY(AB445PTR)                     0256
         L     R2,@ZT00001                                         0256
         LTR   R2,R2                                               0256
         BZ    @RF00256                                            0256
*              INPTR = ADDR(DEFSQTY);  /* SET OVERLAY        @ZA08703*/
         LA    R5INPTR,DEFSQTY(,AB445PTR)                          0258
*              CVD(FIXED24,PKAREA);    /* CONVERT TO EBCDIC  @ZA08703*/
         LR    R2,R5INPTR                                          0259
         BCTR  R2,0                                                0259
         L     R2,FIXED24(,R2)                                     0259
         LA    R2,0(,R2)                                           0259
         CVD   R2,@TS00001                                         0259
         MVC   PKAREA(8),@TS00001                                  0259
*              UNPK(NSPCBUF2,PKAREA(4:8));/* FOR PDE         @ZA08703*/
         UNPK  NSPCBUF2(8),PKAREA+3(5)                             0260
*              SPACPTR2 = ADDR(NSPCBUF2); /* OVERLAY PDE     @ZA08703*/
         LA    R5,NSPCBUF2                                         0261
         ST    R5,SPACPTR2(,R10)                                   0261
*              SPACLEN2 = 8;              /* SET LENGTH      @ZA08703*/
         LA    R5,8                                                0262
         STH   R5,SPACLEN2(,R10)                                   0262
*              SPCPRES2 = '1'B;        /* INDICATE VALUE     @ZA08703*/
         OI    SPCPRES2(R10),B'10000000'                           0263
*            END;                      /* PRESENT            @ZA08703*/
*          IF DEFDQTY ^= 0             /* IF DEFAULT FOR     @ZA08703*/
*            THEN DO;                  /* DIRECTORY BLOCKS   @ZA08703*/
@RF00256 L     R10,DEFDQTY-1(,AB445PTR)                            0265
         LA    R10,0(,R10)                                         0265
         LTR   R10,R10                                             0265
         BZ    @RF00265                                            0265
*              INPTR = ADDR(DEFDQTY);  /* SET OVERLAY        @ZA08703*/
         LA    R5INPTR,DEFDQTY(,AB445PTR)                          0267
*              CVD(FIXED24,PKAREA);    /* GET IT'S EBCDIC    @ZA08703*/
         LR    R10,R5INPTR                                         0268
         BCTR  R10,0                                               0268
         L     R10,FIXED24(,R10)                                   0268
         LA    R10,0(,R10)                                         0268
         CVD   R10,@TS00001                                        0268
         MVC   PKAREA(8),@TS00001                                  0268
*              UNPK(NDIRBUF1,PKAREA(4:8));  /* EQUIVALENT    @ZA08703*/
         UNPK  NDIRBUF1(8),PKAREA+3(5)                             0269
*              DIRPDE = 1;             /* SHOW KEYWD PRESENT @ZA08703*/
         L     R10,PTRPDL(,R3AWAPTR)                               0270
         MVC   DIRPDE(2,R10),@HW1                                  0270
*              DIRPTR = ADDR(NDIRBUF1);/* OVERLAY PDE        @ZA08703*/
         LA    R5,NDIRBUF1                                         0271
         ST    R5,DIRPTR(,R10)                                     0271
*              DIRLEN = 8;             /* SET LENGTH         @ZA08703*/
         MVC   DIRLEN(2,R10),@HW8                                  0272
*              DIRPRES = '1'B;         /* SHOW VALUE PRESENT @ZA08703*/
         OI    DIRPRES(R10),B'10000000'                            0273
*            END;                      /*                    @ZA08703*/
*          IF DEFBLKLN = '1'B          /* IF UNITS OF SPACE  @ZA08703*/
*            THEN DO;                  /* IN AVBLOCK         @ZA08703*/
@RF00265 TM    DEFBLKLN(AB445PTR),B'00100000'                      0275
         BNO   @RF00275                                            0275
*              BLOKPDE = 2;            /* SHOW AVBLOCK REQ'D @ZA08703*/
         L     R10,PTRPDL(,R3AWAPTR)                               0277
         MVC   BLOKPDE(2,R10),@HW2                                 0277
*              INPTR = ADDR(DEFDRLH);  /* SET OVERLAY        @ZA08703*/
         LA    R5INPTR,DEFDRLH(,AB445PTR)                          0278
*              CVD(FIXED24,PKAREA);    /* GET EBCDIC FOR     @ZA08703*/
         LR    R2,R5INPTR                                          0279
         BCTR  R2,0                                                0279
         L     R2,FIXED24(,R2)                                     0279
         LA    R2,0(,R2)                                           0279
         CVD   R2,@TS00001                                         0279
         MVC   PKAREA(8),@TS00001                                  0279
*              UNPK(NBLKBUF1,PKAREA(4:8));    /* AVBLOCK LEN @ZA08703*/
         UNPK  NBLKBUF1(8,R3AWAPTR),PKAREA+3(5)                    0280
*              ABLKPTR = ADDR(NBLKBUF1);      /* OVERLAY PDE @ZA08703*/
         LA    R5,NBLKBUF1(,R3AWAPTR)                              0281
         ST    R5,ABLKPTR(,R10)                                    0281
*              ABLKLEN = 8;            /* SET LENGTH         @ZA08703*/
         MVC   ABLKLEN(2,R10),@HW8                                 0282
*              ABLKPRES = '1'B;        /* INDICATE AVBLOCK   @ZA08703*/
         OI    ABLKPRES(R10),B'10000000'                           0283
*            END;                      /* VALUE SUPPLIED     @ZA08703*/
*          IF DEFTRK = '1'B            /* IF UNITS OF SPACE  @ZA08703*/
*            THEN                      /* ARE IN TRACKS      @ZA08703*/
@RF00275 TM    DEFTRK(AB445PTR),B'10000000'                        0285
         BNO   @RF00285                                            0285
*              BLOKPDE = 3;            /* INDICATE TRACKS    @ZA08703*/
         L     R10,PTRPDL(,R3AWAPTR)                               0286
         MVC   BLOKPDE(2,R10),@HW3                                 0286
*          IF DEFCYL = '1'B            /* IF UNITS OF SPACE  @ZA08703*/
*            THEN                      /* ARE IN CYLINDERS   @ZA08703*/
@RF00285 TM    DEFCYL(AB445PTR),B'01000000'                        0287
         BNO   @RF00287                                            0287
*              BLOKPDE = 4;            /* INDICATE CYLINDERS @ZA08703*/
         L     R10,PTRPDL(,R3AWAPTR)                               0288
         MVC   BLOKPDE(2,R10),@HW4                                 0288
*          GEN(DELETE EP=IEFAB445);    /* DELETE DEFAULT     @ZA08703*/
@RF00287 DS    0H                                                  0289
         DELETE EP=IEFAB445
*        END SPCDEFLT;                 /* CSECT              @ZA08703*/
@EL00008 DS    0H                                                  0290
@EF00008 DS    0H                                                  0290
@ER00008 LM    R14,R12,@SA00008                                    0290
         BR    R14                                                 0290
*                                                                  0291
*    END IKJEFD33                                                  0291
*/* THE FOLLOWING INCLUDE STATEMENTS WERE FOUND IN THIS PROGRAM.      *
*/*%INCLUDE SYSLIB  (IKJZT430)                                        *
*/*%INCLUDE SYSLIB  (IKJZB831)                                        *
*/*%INCLUDE SYSLIB  (IKJPPL  )                                        *
*/*%INCLUDE SYSLIB  (IKJCPPL )                                        *
*/*%INCLUDE SYSLIB  (IEFZB4D2)                                        *
*/*%INCLUDE SYSLIB  (IEFZB445)                                        *
*;                                                                 0291
@DATA    DC    0H'0'
@HW1     DC    H'1'
@HW2     DC    H'2'
@HW3     DC    H'3'
@HW4     DC    H'4'
@HW5     DC    H'5'
@HW7     DC    H'7'
@HWPRIME DC    XL2'000A'
@HWSECND DC    XL2'000B'
@HWBLKSZ DC    XL2'0030'
@SM00677 MVC   ADLPARM(0,R15),SAVPARM+1
D33L0678 MVC   VALBUF(0),SPACEBUF(R10)         ****
D33L067E MVC   VALBUF(0),SPACBUF2(R10)         ****
@SM00680 MVC   PKAREA1(0,R8Y),VALBUF
@SM00682 MVC   ADLPARM(0,R2),SAVPARM+2
@SM00684 MVC   NSPCBUF1(0),SPCBUF1(R1)
@SM00686 MVC   NSPCBUF2(0),SPCBUF2(R1)
@SM00688 MVC   NDIRBUF1(0),DIRBUF1(R1)
@DATD    DSECT
         DS    0F
@SA00001 DS    18F
@PC00001 DS    1F
@SAV001  EQU   @SA00001
@SA00007 DS    15F
@SA00003 DS    14F
@SA00005 DS    15F
@SA00002 DS    13F
@SA00006 DS    15F
@SA00008 DS    15F
@TF00001 DS    F
@ZTEMPS  DS    0F
@ZT00001 DC    F'0'
@ZTEMPND EQU   *
@ZLEN    EQU   @ZTEMPND-@ZTEMPS
@TEMPS   EQU   @ZTEMPS
@L       EQU   @ZLEN
IKJEFD33 CSECT
         DC    0F'0'
@FW6     DC    F'6'
@HW6     EQU   @FW6+2
@FW8     DC    F'8'
@HW8     EQU   @FW8+2
@DATD    DSECT
         DS    0D
PDLADDR2 DS    A
         DS    0D
@TS00001 DS    CL8
CNVRTYPE DS    CL1
         ORG   CNVRTYPE
BLKCNVRT DS    BL1
SPCCNVRT EQU   CNVRTYPE+0
INCCNVRT EQU   CNVRTYPE+0
@NM00008 EQU   CNVRTYPE+0
         ORG   CNVRTYPE+1
SAVPARM  DS    CL4
VALBUF   DS    CL8
NSPCBUF1 DS    CL8
NSPCBUF2 DS    CL8
NDIRBUF1 DS    CL8
PKAREA   DS    CL8
         DS    CL3
CVBAREA  DS    CL8
         ORG   *+1-(*-@DATD)/(*-@DATD) ENSURE DSECT DATA
@ENDDATD EQU   *
@DATEND  EQU   *
IKJEFD33 CSECT
         DC    0F'0'
@SIZDATD DC    AL1(1)
         DC    AL3(@ENDDATD-@DATD)
@SIZ001  EQU   @SIZDATD
VCONSD35 DC    V(IKJEFD35)
VCONSD37 DC    V(IKJEFD37)
         DC    0D'0'                   END OF CSECT
R0       EQU   00                      EQUATES FOR REGISTERS 0-15
R1       EQU   01
R2       EQU   02
R3       EQU   03
R4       EQU   04
R5       EQU   05
R6       EQU   06
R7       EQU   07
R8       EQU   08
R9       EQU   09
R10      EQU   10
R11      EQU   11
R12      EQU   12
R13      EQU   13
R14      EQU   14
R15      EQU   15
R5INPTR  EQU   R5
AB445PTR EQU   R4
R5VALLEN EQU   R5
R4INC    EQU   R4
R3AWAPTR EQU   R3
R8Y      EQU   R8
ALLOCWA  EQU   0
CPPLPTR  EQU   ALLOCWA
PTRPDL   EQU   ALLOCWA+4
GTPLPTR  EQU   ALLOCWA+8
SWITCH   EQU   ALLOCWA+12
RBCODE12 EQU   SWITCH
CONT     EQU   SWITCH
SWITCH2  EQU   ALLOCWA+13
DSEXISTS EQU   SWITCH2
ENTRYCD  EQU   ALLOCWA+14
TXT      EQU   ALLOCWA+16
RCODESV  EQU   ALLOCWA+24
VCFLAGS  EQU   ALLOCWA+36
PMPTPDL  EQU   ALLOCWA+40
P        EQU   ALLOCWA+52
Q        EQU   ALLOCWA+56
CMDTWO   EQU   ALLOCWA+60
NBLKBUF1 EQU   ALLOCWA+164
PPLPTR   EQU   ALLOCWA+172
ALLOCPDL EQU   0
BLOKPDE  EQU   ALLOCPDL+16
SPACEPDE EQU   ALLOCPDL+18
DIRPDE   EQU   ALLOCPDL+20
USINGPDE EQU   ALLOCPDL+22
RLSEPDE  EQU   ALLOCPDL+42
RNDPDE   EQU   ALLOCPDL+44
DSNPDE   EQU   ALLOCPDL+48+4                                       @26C
DSNPTR   EQU   DSNPDE
DSNFLAGS EQU   DSNPDE+6
MBRPTR   EQU   DSNPDE+8
MBRFLAGS EQU   DSNPDE+14
MBRPRES  EQU   MBRFLAGS
PASSPTR  EQU   DSNPDE+16
PASSFLAG EQU   DSNPDE+22
DDNMEPDE EQU   ALLOCPDL+76+4                                       @26C
DDNPTR   EQU   DDNMEPDE
DDNFLAGS EQU   DDNMEPDE+6
CLASPDE  EQU   ALLOCPDL+84+4                                       @26C
CLASPTR  EQU   CLASPDE
CLASFLG  EQU   CLASPDE+6
SERPDE   EQU   ALLOCPDL+92+4                                       @26C
SERPTR   EQU   SERPDE
SERFLAGS EQU   SERPDE+6
BLKPDE   EQU   ALLOCPDL+104+4                                      @26C
BLKPTR   EQU   BLKPDE
BLKLEN   EQU   BLKPDE+4
BLKFLAGS EQU   BLKPDE+6
ABLKPDE  EQU   ALLOCPDL+112+4                                      @26C
ABLKPTR  EQU   ABLKPDE
ABLKLEN  EQU   ABLKPDE+4
ABLKFLG  EQU   ABLKPDE+6
ABLKPRES EQU   ABLKFLG
SPACPDE  EQU   ALLOCPDL+120+4                                      @26C
SPACEPTR EQU   SPACPDE
SPACELEN EQU   SPACPDE+4
SPACEFLG EQU   SPACPDE+6
SPACPRES EQU   SPACEFLG
SPACPDE2 EQU   ALLOCPDL+128+4                                      @26C
SPACPTR2 EQU   SPACPDE2
SPACLEN2 EQU   SPACPDE2+4
SPACFLG2 EQU   SPACPDE2+6
SPCPRES2 EQU   SPACFLG2
DIRECPDE EQU   ALLOCPDL+136+4                                      @26C
DIRPTR   EQU   DIRECPDE
DIRLEN   EQU   DIRECPDE+4
DIRFLAGS EQU   DIRECPDE+6
DIRPRES  EQU   DIRFLAGS
ATTRPDE  EQU   ALLOCPDL+144+4                                      @26C
ATTRPTR  EQU   ATTRPDE
ATTRFLAG EQU   ATTRPDE+6
USIDPDE  EQU   ALLOCPDL+152+4                                      @26C
USIDPTR  EQU   USIDPDE
USIDFLAG EQU   USIDPDE+6
UTYPPDE  EQU   ALLOCPDL+160+4                                      @26C
UTYPPTR  EQU   UTYPPDE
UTYPFLAG EQU   UTYPPDE+6
UNCNTPDE EQU   ALLOCPDL+168+4                                      @26C
UCNTPTR  EQU   UNCNTPDE
UCNTFLAG EQU   UNCNTPDE+6
POSTPDE  EQU   ALLOCPDL+180+4                                      @26C
POSPTR   EQU   POSTPDE
POSFLAG  EQU   POSTPDE+6
MXVOLPDE EQU   ALLOCPDL+188+4                                      @26C
MXVOLPTR EQU   MXVOLPDE
MXVOLFLG EQU   MXVOLPDE+6
VOLSQPDE EQU   ALLOCPDL+196+4                                      @26C
VOLSQPTR EQU   VOLSQPDE
VOLSQFLG EQU   VOLSQPDE+6
MSVGPDE  EQU   ALLOCPDL+204+4                                      @26C
MSVGPTR  EQU   MSVGPDE
MSVGFLG  EQU   MSVGPDE+6
DSNBUF   EQU   0
BLKBUF   EQU   0
SPACEBUF EQU   0
SPACBUF2 EQU   0
GTPL     EQU   0
GTPLABLK EQU   GTPL+16
TEXTCORE EQU   0
ADDNUM   EQU   TEXTCORE
ADDTEXT  EQU   TEXTCORE+4
ADLTEXT  EQU   0
ADLKEY   EQU   ADLTEXT
ADLNBR   EQU   ADLTEXT+2
ADLLEN   EQU   ADLTEXT+4
ADLPARM  EQU   ADLTEXT+6
PPL      EQU   0
CPPL     EQU   0
SPCPDE1  EQU   0
SPCPTR1  EQU   SPCPDE1+8
SPCLEN1  EQU   SPCPDE1+12
SPCFLAG1 EQU   SPCPDE1+14
SPCPTR2  EQU   SPCPDE1+16
SPCLEN2  EQU   SPCPDE1+20
SPCFLAG2 EQU   SPCPDE1+22
SPC2PRES EQU   SPCFLAG2
SPCBUF1  EQU   0
SPCBUF2  EQU   0
DIRPDE1  EQU   0
DIRPTR1  EQU   DIRPDE1+8
DIRLEN1  EQU   DIRPDE1+12
DIRFLAG1 EQU   DIRPDE1+14
DIRBUF1  EQU   0
IEFAB445 EQU   0
DEFSPACE EQU   IEFAB445
DEFPQTY  EQU   DEFSPACE
DEFSQTY  EQU   DEFSPACE+3
DEFDRLH  EQU   DEFSPACE+6
DEFDQTY  EQU   DEFSPACE+9
@NM00017 EQU   DEFSPACE+12
DEFTRK   EQU   @NM00017
DEFCYL   EQU   @NM00017
DEFBLKLN EQU   @NM00017
PKAREA1  EQU   0
AREA1    EQU   0
AREA2    EQU   0
FIXED24  EQU   0
ABLKBUF  EQU   0
ATTRBUF  EQU   0
CLASSBUF EQU   0
DDNBUF   EQU   0
DIRBUF   EQU   0
MBRBUF   EQU   0
MSVGBUF  EQU   0
MXVOLBUF EQU   0
PASSBUF  EQU   0
POSTBUF  EQU   0
SERBUF   EQU   0
UCNTBUF  EQU   0
USIDBUF  EQU   0
UTYPBUF  EQU   0
VSEQBUF  EQU   0
INPARMS  EQU   0
*                                      START UNREFERENCED COMPONENTS
DEFUNIT  EQU   IEFAB445+13
DEFROUND EQU   @NM00017
DEFALX   EQU   @NM00017
DEFMIXG  EQU   @NM00017
DEFCONTG EQU   @NM00017
DEFRLSE  EQU   @NM00017
@NM00016 EQU   DIRPDE1+15
@NM00015 EQU   DIRPDE1+4
@NM00014 EQU   DIRPDE1
@NM00013 EQU   SPCPDE1+23
@NM00012 EQU   SPCFLAG2
@NM00011 EQU   SPCPDE1+15
@NM00010 EQU   SPCPDE1+4
@NM00009 EQU   SPCPDE1
CPPLECT  EQU   CPPL+12
CPPLPSCB EQU   CPPL+8
CPPLUPT  EQU   CPPL+4
CPPLCBUF EQU   CPPL
PPLUWA   EQU   PPL+24
PPLCBUF  EQU   PPL+20
PPLANS   EQU   PPL+16
PPLPCL   EQU   PPL+12
PPLECB   EQU   PPL+8
PPLECT   EQU   PPL+4
PPLUPT   EQU   PPL
ADDUNIT  EQU   TEXTCORE+36
@NM00007 EQU   TEXTCORE+2
GTPLOUTA EQU   GTPL+20
GTPLTBLE EQU   GTPL+12
GTPLKLST EQU   GTPL+8
GTPLPCL  EQU   GTPL+4
GTPLPDL  EQU   GTPL
DSNTERM  EQU   DSNBUF
MSVGRSV2 EQU   MSVGPDE+7
MSVGRSV1 EQU   MSVGFLG
MSVGPRES EQU   MSVGFLG
MSVGLEN  EQU   MSVGPDE+4
VOLRSV2  EQU   VOLSQPDE+7
VOLRSV1  EQU   VOLSQFLG
VOLPRES  EQU   VOLSQFLG
VOLSQLEN EQU   VOLSQPDE+4
MXVRSV2  EQU   MXVOLPDE+7
MXVRSV1  EQU   MXVOLFLG
MXVPRES  EQU   MXVOLFLG
MXVOLLEN EQU   MXVOLPDE+4
POSRSV2  EQU   POSTPDE+7
POSRSV1  EQU   POSFLAG
POSPRES  EQU   POSFLAG
POSLEN   EQU   POSTPDE+4
LABLRSV1 EQU   ALLOCPDL+178+4                                      @26C
LABELPDE EQU   ALLOCPDL+176+4                                      @26C
UCNTRSV2 EQU   UNCNTPDE+7
UCNTRSV1 EQU   UCNTFLAG
UCNTPRES EQU   UCNTFLAG
UCNTLEN  EQU   UNCNTPDE+4
UTYPRSV2 EQU   UTYPPDE+7
UTYPRSV1 EQU   UTYPFLAG
UTYPPRES EQU   UTYPFLAG
UTYPLEN  EQU   UTYPPDE+4
USIDRSV2 EQU   USIDPDE+7
USIDRSV1 EQU   USIDFLAG
USIDPRES EQU   USIDFLAG
USIDLEN  EQU   USIDPDE+4
ATTRRSV2 EQU   ATTRPDE+7
ATTRRSV1 EQU   ATTRFLAG
ATTRPRES EQU   ATTRFLAG
ATTRLEN  EQU   ATTRPDE+4
DIRRSV2  EQU   DIRECPDE+7
DIRRSV1  EQU   DIRFLAGS
SPCRSV22 EQU   SPACPDE2+7
SPCRSV12 EQU   SPACFLG2
SPACRSV2 EQU   SPACPDE+7
SPACRSV1 EQU   SPACEFLG
ABLKRSV2 EQU   ABLKPDE+7
ABLKRSV1 EQU   ABLKFLG
BLKRSV2  EQU   BLKPDE+7
BLKRSV1  EQU   BLKFLAGS
BLKPRES  EQU   BLKFLAGS
SERNEXT  EQU   SERPDE+8
SERRSV2  EQU   SERPDE+7
SERRSV1  EQU   SERFLAGS
SERPRES  EQU   SERFLAGS
SERLEN   EQU   SERPDE+4
CLASRSV2 EQU   CLASPDE+7
CLASRSV1 EQU   CLASFLG
CLASPRES EQU   CLASFLG
CLASLEN  EQU   CLASPDE+4
DDNRSV2  EQU   DDNMEPDE+7
DDNRSV1  EQU   DDNFLAGS
DDNPRES  EQU   DDNFLAGS
DDNLEN   EQU   DDNMEPDE+4
DSNNEXT  EQU   DSNPDE+24
PASSRSV2 EQU   DSNPDE+23
PASSRSV1 EQU   PASSFLAG
PASSPRES EQU   PASSFLAG
PASSLEN  EQU   DSNPDE+20
MBRRSV2  EQU   DSNPDE+15
MBRRSV1  EQU   MBRFLAGS
MBRLEN   EQU   DSNPDE+12
DSNRSV2  EQU   DSNPDE+7
DSNRSV1  EQU   DSNFLAGS
DSNQUOT  EQU   DSNFLAGS
DSNPRES  EQU   DSNFLAGS
DSNLENGH EQU   DSNPDE+4
DISPPDE  EQU   ALLOCPDL+46
VSEQPDE  EQU   ALLOCPDL+40
PRIVPDE  EQU   ALLOCPDL+38
MAXVPDE  EQU   ALLOCPDL+36
POSPDE   EQU   ALLOCPDL+34
LABLPDE  EQU   ALLOCPDL+32
UCNTPDE  EQU   ALLOCPDL+30
UNITPDE  EQU   ALLOCPDL+28
HOLDPDE  EQU   ALLOCPDL+26
DESTPDE  EQU   ALLOCPDL+24
VOLPDE   EQU   ALLOCPDL+14
STATSPDE EQU   ALLOCPDL+12
FILEPDE  EQU   ALLOCPDL+10
DSPDE    EQU   ALLOCPDL+8
@NM00006 EQU   ALLOCPDL+4
@NM00005 EQU   ALLOCPDL
FIRSTPTR EQU   ALLOCWA+176
COMMECB  EQU   ALLOCWA+160
PDLADDR3 EQU   ALLOCWA+156
PPLTWO   EQU   ALLOCWA+128
CMDTWOBF EQU   CMDTWO+4
CMDTWOOF EQU   CMDTWO+2
CMDTWOLN EQU   CMDTWO
MSGPTR   EQU   ALLOCWA+48
PRCODE   EQU   ALLOCWA+44
@NM00004 EQU   ALLOCWA+38
NCLASBUF EQU   ALLOCWA+37
@NM00003 EQU   VCFLAGS
SPACE2   EQU   VCFLAGS
SPACE1   EQU   VCFLAGS
AVBLK    EQU   VCFLAGS
PTRMSGS  EQU   ALLOCWA+32
DSNCTR   EQU   ALLOCWA+30
VLISTCTR EQU   ALLOCWA+28
PTRS99RB EQU   ALLOCWA+20
@NM00002 EQU   SWITCH2
@NM00001 EQU   SWITCH2
FIRSTPDE EQU   SWITCH
AWARSV1  EQU   SWITCH
ASTRSK   EQU   SWITCH
TERMOPT  EQU   SWITCH
PROMPT   EQU   SWITCH
DSOVRLAY EQU   SWITCH
*                                      END UNREFERENCED COMPONENTS
@RC00079 EQU   @RC00070
@RC00091 EQU   @RC00088
@RF00095 EQU   @RC00088
@RF00103 EQU   @RC00100
@RC00181 EQU   @EL00005
@RC00209 EQU   @EL00006
@RC00226 EQU   @EL00007
@EL01    EQU   @EL00001
@ENDDATA EQU   *
         END   IKJEFD33,(C'PLS1343',0702,82173)
/*
//*
//STEP09  EXEC PGM=IEBGENER
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  *
  IDENTIFY IKJEFD33('ZP60026')
++MOD(IKJEFD34) DISTLIB(AOST4).
/*
//SYSUT2   DD  DSN=&&SMPMCS,DISP=(MOD,PASS)
//SYSIN    DD  DUMMY
//*
//STEP10  EXEC PGM=IFOX00,PARM='OBJECT,NODECK,NOTERM,XREF(SHORT)'
//SYSPRINT DD  SYSOUT=*
//SYSLIB   DD  DSN=SYS1.MACLIB,DISP=SHR
//         DD  DSN=SYS1.SMPMTS,DISP=SHR
//         DD  DSN=SYS1.AMODGEN,DISP=SHR
//SYSUT1   DD  UNIT=SYSALLDA,SPACE=(CYL,10)
//SYSUT2   DD  UNIT=SYSALLDA,SPACE=(CYL,10)
//SYSUT3   DD  UNIT=SYSALLDA,SPACE=(CYL,10)
//SYSGO    DD  DSN=&&SMPMCS,DISP=(MOD,PASS)
//SYSIN    DD  *
         TITLE 'CONCATENATION ROUTINE                                  *
                        '
***********************************************************************
*  $26   ZP60026  2010-01-23 GREG PRICE: ADD 'REUSE' OPERAND.         *
*                            CHANGES APPLIED TO EBB1102 LEVEL.        *
***********************************************************************
IKJEFD34 CSECT ,                                                   0001
@MAINENT DS    0H                                                  0001
         USING *,R15                                               0001
         B     @PROLOG                                             0001
         DC    AL1(33)                                             @26C
         DC    C'IKJEFD34  75.248'                                 0001
         DC    CL17' ZP60026 2010.023'                             @26A
         DROP  R15
@PROLOG  STM   R14,R12,12(R13)                                     0001
         BALR  R12,0                                               0001
@PSTART  DS    0H                                                  0001
         USING @PSTART,R12                                         0001
         L     R0,@SIZDATD                                         0001
         GETMAIN  R,LV=(0)
         LR    R11,R1                                              0001
         USING @DATD,R11                                           0001
         ST    R13,@SA00001+4                                      0001
         LM    R0,R1,20(R13)                                       0001
         ST    R11,8(,R13)                                         0001
         LR    R13,R11                                             0001
         XC    @ZTEMPS(@ZLEN),@ZTEMPS
         MVC   @PC00001(4),0(R1)                                   0001
*/********************************************************************/
*/*                                                                  */
*/* SEGMENT NAME -                                                   */
*/*      CONCATRQ                                                    */
*/*                                                                  */
*/* FUNCTION -                                                       */
*/*      THIS ROUTINE PROCESSES A CONCATENATION REQUEST.             */
*/*                                                                  */
*/* OPERATION -                                                      */
*/*      IF A STATUS OF MOD, NEW OR SYSOUT WAS SPECIFIED,            */
*/*      THEN ISSUE AN ERROR MESSAGE AND TERMINATE.                  */
*/*      IF FILE WAS NOT SPECIFIED, PROMPT THE USER                  */
*/*      TO ENTER A FILENAME. MARK EXTRANEOUS PARAMETERS NOT         */
*/*      ENTERED IN THE PDL. ISSUE A GETMAIN FOR STORAGE TO HOLD     */
*/*      THE TEXT UNITS FOR THE DDNAMES ASSOCIATED WITH EACH DATA    */
*/*      SET. TRANSLATE PARAMETERS TO TEXT FORMAT. INVOKE ROUTINE    */
*/*      CONCTDSN TO ALLOCATE EACH DATA SET. IF THE ALLOCATION WAS   */
*/*      SUCCESSFUL, INVOKE DYNAMIC ALLOCATION TO CONCATENATE THE    */
*/*      DDNAMES.                                                    */
*/*                                                                  */
*/* INPUT -                                                          */
*/*      PTR TO PDL                                                  */
*/*                                                                  */
*/* OUTPUT -                                                         */
*/*      RBCODE12 = 0 - CONCATENATION SUCCESSFUL                     */
*/*                 1 - ERROR IN ALLOCATION OR CONCATENATION         */
*/*                                                                  */
*/* NOTE:  THIS ROUTINE REPRESENTS A DIRECT LINE FUNCTION AND CAN    */
*/*        BE INCORPORATED AS AN INCLUDED CODE SEGMENT WHEN THE      */
*/*        INCLUDE FUNCTION HAS RECEIVED A STANDARD DEFINITION.      */
*/********************************************************************/
*    CONCATRQ:                                                     0088
*          AWAPTR = R1;                     /* POINTER TO WORKAREA   */
CONCATRQ LR    R3AWAPTR,R1                                         0088
*          INC = ADDNUM + 1;                /* SUBSCRIPT USED FOR  0089
*                                              ADDITIONAL TEXT       */
         L     R6,GTPLPTR(,R3AWAPTR)                               0089
         L     R6,GTPLABLK(,R6)                                    0089
         LA    R8INC,1                                             0089
         AH    R8INC,ADDNUM(,R6)                                   0089
*          CNCTFLAG = '00'X;                /* INITIALIZE FLAGS      */
         MVI   CNCTFLAG,X'00'                                      0090
*          IF STATSPDE > 2 THEN             /* IF MOD,NEW OR SYSOUT, */
         L     R6,PTRPDL(,R3AWAPTR)                                0091
         LH    R6,STATSPDE(,R6)                                    0091
         C     R6,@FW2                                             0091
         BNH   @RF00091                                            0091
*            DO;                            /* THEN                  */
*              RBCODE12 = '1'B;             /* INDICATE ALLOC FAILED */
         OI    RBCODE12(R3AWAPTR),B'10000000'                      0093
*              STATSERR = '1'B;             /* INDICATE ERROR TYPE   */
         OI    STATSERR,B'10000000'                                0094
*              CALL CCERRMSG;               /* ISSUE ERROR MSG       */
         BAL   R14,CCERRMSG                                        0095
*            END;                                                  0096
*          ELSE                                                    0097
*            DO;                                                   0097
         B     @RC00091                                            0097
@RF00091 DS    0H                                                  0098
*              IF FILEPDE = 0 THEN          /* IF FILE NOT ENTERED,  */
         L     R6,PTRPDL(,R3AWAPTR)                                0098
         LH    R6,FILEPDE(,R6)                                     0098
         LTR   R6,R6                                               0098
         BNZ   @RF00098                                            0098
*                CALL PRMPTDDN;             /* PROMPT FOR FILENAME   */
         BAL   R14,PRMPTDDN                                        0099
*              I = 1;                       /* INITIALIZE SUBSCRIPT  */
@RF00098 LA    R2I,1                                               0100
*    /* MARK EXTRANEOUS PARAMETERS NOT ENTERED IN PDL                */
*              DO WHILE (CONT='0'B);                               0101
         B     @DE00101                                            0101
@DL00101 DS    0H                                                  0102
*              IF CONCTEXT(I)='00'X THEN    /* IF END OF LIST,       */
         LA    R6,CONCTEXT-1(R2I)                                  0102
         CLI   0(R6),X'00'                                         0102
         BNE   @RF00102                                            0102
*                CONT='1'B;                 /* EXIT DO-WHILE         */
         OI    CONT(R3AWAPTR),B'00010000'                          0103
*              ELSE                         /* ELSE,                 */
*                DO;                        /* GET ADDRESS OF PDE    */
         B     @RC00102                                            0104
@RF00102 DS    0H                                                  0105
*                 EXTRAN = ADDR(ALLOCPDL)+CONCTEXT(I);             0105
         SLR   R5EXTRAN,R5EXTRAN                                   0105
         IC    R5EXTRAN,CONCTEXT-1(R2I)                            0105
         AL    R5EXTRAN,PTRPDL(,R3AWAPTR)                          0105
*                 PDEEXT = 0;               /* SET PDE TO 0          */
         SLR   R6,R6                                               0106
         STH   R6,PDEEXT(,R5EXTRAN)                                0106
*                 I = I+1;                  /* INCREMENT SUBSCRIPT   */
         AL    R2I,@FW1                                            0107
*                END;                                              0108
*              END;                         /* END DO-WHILE          */
@RC00102 DS    0H                                                  0109
@DE00101 TM    CONT(R3AWAPTR),B'00010000'                          0109
         BZ    @DL00101                                            0109
*              CONT = '0'B;                 /* RESET SWITCH          */
         NI    CONT(R3AWAPTR),B'11101111'                          0110
*              SIZE = (DSNCTR*10) + 16;     /* CORE NEEDED FOR CONCAT*/
         LH    R2,DSNCTR(,R3AWAPTR)                                0111
         MH    R2,@HW10                                            0111
         LA    R2,16(,R2)                                          0111
         ST    R2,@TF00001                                         0111
         MVC   SIZE(3),@TF00001+1                                  0111
*              SPNO = '01'X;                /* SUBPOOL NUMBER        */
         MVI   SPNO,X'01'                                          0112
*              R0 = CORE;                   /* SBPOOL AND SIZE IN 0  */
         L     R0,CORE                                             0113
*              GEN (GETMAIN R,LV=(0));      /* ISSUE GETMAIN         */
         GETMAIN R,LV=(0)
*              DDNAPTR = R1;                /* PTR TO STORAGE        */
         LR    DDNAPTR,R1                                          0115
*              DDLST = ADDR(DDNMLEN);       /* PTR TO LIST POSITION  */
         LA    R5DDLST,DDNMLEN(,DDNAPTR)                           0116
*              PERMCTPT = ADDR(PERMCTXT);   /* TEXT FOR PERM CONCAT  */
         LA    R2,PERMCTXT(,DDNAPTR)                               0117
         ST    R2,PERMCTPT(,DDNAPTR)                               0117
*              PERMCTKY = DCCPERMC;         /* KEY AND               */
         MVC   PERMCTKY(2,DDNAPTR),@HWPERMC                        0118
*              PERMCTNM = 0;                /* NUMBER                */
         SLR   R2,R2                                               0119
         STH   R2,PERMCTNM(,DDNAPTR)                               0119
*              DDNMPTR = ADDR(DDNMTXT);     /* TEXT FOR DDNAMES      */
         LA    R2,DDNMTXT(,DDNAPTR)                                0120
         ST    R2,DDNMPTR(,DDNAPTR)                                0120
*              DDNMKEY = DCCDDNAM;          /* KEY AND               */
         MVC   DDNMKEY(2,DDNAPTR),@HWDDNAM                         0121
*              DDNMNBR = DSNCTR;            /* NUMBER                */
         LH    R2,DSNCTR(,R3AWAPTR)                                0122
         STH   R2,DDNMNBR(,DDNAPTR)                                0122
*              DDNMEND = '1'B;              /* END OF ADDR LIST      */
         OI    DDNMEND(DDNAPTR),B'10000000'                        0123
*              CALL CNCATTXT;               /* SET UP TEXT UNITS     */
         BAL   R14,CNCATTXT                                        0124
*              CALL TRANSRTN;               /* TRANSLATE PARAMETERS  */
         BAL   R14,TRANSRTN                                        0125
*              IF RBCODE12 = '0'B THEN      /* IF TRANSLATE          */
         TM    RBCODE12(R3AWAPTR),B'10000000'                      0126
         BNZ   @RF00126                                            0126
*                DO;                        /* SUCCESSFUL,           */
*                  CALL CONCTDSN;           /* ALLOC EACH DATASET    */
         BAL   R14,CONCTDSN                                        0128
*                  IF RBCODE12 = '0'B THEN  /* IF ALLOCATION GOOD,   */
         TM    RBCODE12(R3AWAPTR),B'10000000'                      0129
         BNZ   @RF00129                                            0129
*                    DO;                                           0130
*                  S99VERB = S99VRBCC;      /* SET CONCAT VERB       */
         L     R10,PTRS99RB(,R3AWAPTR)                             0131
         MVI   S99VERB(R10),X'03'                                  0131
*                  S99TXTPP = DDNAPTR;      /* SET PTR TO CONCAT TEXT*/
         ST    DDNAPTR,S99TXTPP(,R10)                              0132
*                  CALL DYNSVC;             /* INVOKE DYN ALLOCATION */
         BAL   R14,DYNSVC                                          0133
*                  IF RCODESV > 0 THEN      /* IF ALLOCATION FAILED  */
         L     R10,RCODESV(,R3AWAPTR)                              0134
         LTR   R10,R10                                             0134
         BNP   @RF00134                                            0134
*                    DO;                    /* THEN                  */
*                      RBCODE12 = '1'B;     /* INDICATE ALLOC FAILED */
         OI    RBCODE12(R3AWAPTR),B'10000000'                      0136
*                      CALL CONCTERR;       /* ISSUE MSG & FREE DS   */
         BAL   R14,CONCTERR                                        0137
*                    END;                                          0138
*                    END;                                          0139
@RF00134 DS    0H                                                  0140
*                END;                                              0140
@RF00129 DS    0H                                                  0141
*            END;                                                  0141
@RF00126 DS    0H                                                  0142
*        IF RBCODE12 = '1'B THEN       /* IF ERROR OCCURRED          */
@RC00091 TM    RBCODE12(R3AWAPTR),B'10000000'                      0142
         BNO   @RF00142                                            0142
*          RETCODE = 12;               /* RETURN CODE OF 12          */
         L     R10,REGSAVE(,R13)                                   0143
         MVC   RETCODE(4,R10),@FW12                                0143
*        ELSE                          /* ELSE                       */
*          RETCODE = 0;                /* RETURN CODE 0              */
         B     @RC00142                                            0144
@RF00142 L     R10,REGSAVE(,R13)                                   0144
         SLR   R15,R15                                             0144
         ST    R15,RETCODE(,R10)                                   0144
*        RETURN;                                                   0145
@EL00001 L     R13,4(,R13)                                         0145
@EF00001 L     R0,@SIZDATD                                         0145
         LR    R1,R11                                              0145
         FREEMAIN R,LV=(0),A=(1)
@ER00001 LM    R14,R12,12(R13)                                     0145
         BR    R14                                                 0145
*/********************************************************************/
*/*                                                                  */
*/* SEGMENT NAME -                                                   */
*/*      CNCATTXT                                                    */
*/*                                                                  */
*/* FUNCTION -                                                       */
*/*      THIS ROUTINE BUILDS ADDITIONAL TEXT UNITS NEEDED WHEN       */
*/*      ALLOCATING DATA SETS TO BE CONCATENATED.                    */
*/*                                                                  */
*/* OPERATION -                                                      */
*/*      BUILD TEXT UNIT TO RETURN DDNAME. BUILD TEXT UNIT TO RETURN */
*/*      DSORG.                                                      */
*/*                                                                  */
*/* INPUT -                                                          */
*/*      WORKAREA                                                    */
*/*                                                                  */
*/* OUTPUT -                                                         */
*/*      TEXT UNITS BUILT                                            */
*/*                                                                  */
*/* NOTE:  THIS ROUTINE REPRESENTS A DIRECT LINE FUNCTION AND CAN    */
*/*        BE INCORPORATED AS AN INCLUDED CODE SEGMENT WHEN THE      */
*/*        INCLUDE FUNCTION HAS RECEIVED A STANDARD DEFINITION.      */
*/*                                                                  */
*/*        THESE TEXT UNITS HAVE TO BE SET UP HERE BECAUSE GENTRANS  */
*/*        CAN ONLY BUILD TEXT UNITS RELATED TO PARAMETERS IN        */
*/*        THE PDL.                                                  */
*/********************************************************************/
*    CNCATTXT: PROC;                                               0146
         B     @EL00001                                            0146
CNCATTXT STM   R14,R7,12(R13)                                      0146
         STM   R9,R12,56(R13)                                      0146
*        ADDNUM = ADDNUM + 1;          /* INCREMENT TEXT COUNTER     */
         L     R10,GTPLPTR(,R3AWAPTR)                              0147
         L     R10,GTPLABLK(,R10)                                  0147
         LA    R9,1                                                0147
         LH    R2,ADDNUM(,R10)                                     0147
         ALR   R2,R9                                               0147
         STH   R2,ADDNUM(,R10)                                     0147
*        ADLKEY = DALRTDDN;            /* RETURN DDNAME KEY          */
         L     R15,TXT(,R3AWAPTR)                                  0148
         MVC   ADLKEY(2,R15),@HWRTDDN                              0148
*        ADLNBR = 1;                   /* NUMBER                     */
         STH   R9,ADLNBR(,R15)                                     0149
*        ADLLEN = 8;                   /* MAX LENGTH                 */
         LA    R14,8                                               0150
         STH   R14,ADLLEN(,R15)                                    0150
*        ADDTEXT(INC) = TXT;           /* PTR TO TEXT UNIT IN LIST   */
         LR    R7,R8INC                                            0151
         SLA   R7,2                                                0151
         ST    R15,@TF00001                                        0151
         ALR   R7,R10                                              0151
         MVC   ADDTEXT-4(4,R7),@TF00001                            0151
*        TXT = TXT + 14;               /* INCREMENT TEXT PTR         */
         AL    R15,@FW14                                           0152
         ST    R15,TXT(,R3AWAPTR)                                  0152
*        INC = INC + 1;                /* INCREMENT ADDR SUBSCRIPT   */
         ALR   R8INC,R9                                            0153
*        ADDNUM = ADDNUM + 1;          /* INCREMENT TEXT COUNTER     */
         ALR   R2,R9                                               0154
         STH   R2,ADDNUM(,R10)                                     0154
*        ADLKEY = DALRTORG;            /* RETURN DSORG KEY           */
         MVC   ADLKEY(2,R15),@HWRTORG                              0155
*        ADLNBR = 1;                   /* NUMBER                     */
         STH   R9,ADLNBR(,R15)                                     0156
*        ADLLEN = 2;                   /* LENGTH                     */
         MVC   ADLLEN(2,R15),@HW2                                  0157
*        ADDTEXT(INC) = TXT;           /* PTR TO TEXT UNIT IN LIST   */
         LR    R2,R8INC                                            0158
         SLA   R2,2                                                0158
         ST    R15,@TF00001                                        0158
         ALR   R10,R2                                              0158
         MVC   ADDTEXT-4(4,R10),@TF00001                           0158
*        TXT = TXT + 8;                /* INCREMENT TEXT PTR         */
         ALR   R15,R14                                             0159
         ST    R15,TXT(,R3AWAPTR)                                  0159
*        INC = INC + 1;                /* INCREMENT ADDR SUBSCRIPT   */
         ALR   R8INC,R9                                            0160
*   END CNCATTXT;                                                  0161
@EL00002 DS    0H                                                  0161
@EF00002 DS    0H                                                  0161
@ER00002 LM    R14,R7,12(R13)                                      0161
         LM    R9,R12,56(R13)                                      0161
         BR    R14                                                 0161
*/********************************************************************/
*/*                                                                  */
*/* SEGMENT NAME -                                                   */
*/*      CONCTDSN                                                    */
*/*                                                                  */
*/* FUNCTION -                                                       */
*/*      THIS ROUTINE ALLOCATES A LIST OF DATA SETS TO BE            */
*/*      CONCATENATED.                                               */
*/*                                                                  */
*/* OPERATION -                                                      */
*/*      ALLOCATE ONE DATA SET AT A TIME. IF THE ALLOCATION IS NOT   */
*/*      SUCCESSFUL, ISSUE AN ERROR MESSAGE AND TERMINATE. IF THE    */
*/*      DSORG IS NOT THE SAME FOR ALL THE DATA SETS IN THE LIST,    */
*/*      ISSUE AN ERROR MESSAGE AND TERMINATE. IF EITHER OF THE      */
*/*      ABOVE ERRORS ARE FOUND, ROUTINE CONCTERR IS INVOKED TO      */
*/*      FREE ANY PREVIOUSLY ALLOCATED DATA SETS.                    */
*/*                                                                  */
*/* INPUT -                                                          */
*/*      PTR TO DSNAME, MEMBER AND PASSWORD TEXT UNITS               */
*/*                                                                  */
*/* OUTPUT -                                                         */
*/*      RBCODE12 = 0 - ALLOCATION OF LIST SUCCESSFUL                */
*/*                 1 - ERROR IN ALLOCATION                          */
*/*                                                                  */
*/* NOTE:  THIS ROUTINE REPRESENTS A DIRECT LINE FUNCTION AND CAN    */
*/*        BE INCORPORATED AS AN INCLUDED CODE SEGMENT WHEN THE      */
*/*        INCLUDE FUNCTION HAS RECEIVED A STANDARD DEFINITION.      */
*/********************************************************************/
*    CONCTDSN: PROC;                                               0162
CONCTDSN STM   R14,R7,@SA00003                                     0162
         STM   R9,R12,@SA00003+40                                  0162
*        DPTR2 = ADDR(DALEN);               /* PTR TO DSN LEN & PARM */
         L     R15,TUDSN(,GTPLOUT1)                                0163
         LA    R2DPTR2,DALEN(,R15)                                 0163
*        LISTNMBR = DSNCTR;                 /* SAVE DSN NUMBER       */
         LH    LISTNMBR,DSNCTR(,R3AWAPTR)                          0164
*        DANUM = 1;                         /* SET DSN NUMBER TO 1   */
         MVC   DANUM(2,R15),@HW1                                   0165
*        IF TUMBR ^= 0 THEN                 /* IF MEMBER UNIT BUILT  */
         SLR   R15,R15                                             0166
         L     R14,TUMBR(,GTPLOUT1)                                0166
         CR    R14,R15                                             0166
         BE    @RF00166                                            0166
*         DO;                               /* THEN                  */
*           MPTR2 = ADDR(MBLEN);            /* PTR TO MBR LEN & PARM */
         LA    R7MPTR2,MBLEN(,R14)                                 0168
*           SAVMBLEN = MEMLEN;              /* SAVE MEMBER LENGTH    */
         LH    R1,MEMLEN(,R7MPTR2)                                 0169
         LR    SAVMBLEN,R1                                         0169
*           IF MEMLEN = 0 THEN              /* IF MBR NOT ENTERED,   */
         CR    R1,R15                                              0170
         BNE   @RF00170                                            0170
*             DO;                           /* THEN,                 */
*               MBKEY = 0;                  /* SET KEY TO 0          */
         STH   R15,MBKEY(,R14)                                     0172
*               MBNUM = 0;                  /* SET NUMBER TO 0       */
         STH   R15,MBNUM(,R14)                                     0173
*             END;                                                 0174
*           ELSE                            /* ELSE,                 */
*             MBNUM = 1;                    /* SET MBR NUMBER TO 1   */
         B     @RC00170                                            0175
@RF00170 L     R1,TUMBR(,GTPLOUT1)                                 0175
         MVC   MBNUM(2,R1),@HW1                                    0175
*         END;                                                     0176
@RC00170 DS    0H                                                  0177
*        IF TUPSWD ^= 0 THEN                /* IF PSWORD UNIT BUILT  */
@RF00166 SLR   R15,R15                                             0177
         L     R14,TUPSWD(,GTPLOUT1)                               0177
         CR    R14,R15                                             0177
         BE    @RF00177                                            0177
*         DO;                               /* THEN                  */
*           PPTR2 = ADDR(PWLEN);            /* PTR TO PSW LEN & PARM */
         LA    R9PPTR2,PWLEN(,R14)                                 0179
*           SAVPWLEN = PSWLEN;              /* SAVE PASSWORD LENGTH  */
         LH    R1,PSWLEN(,R9PPTR2)                                 0180
         STH   R1,SAVPWLEN                                         0180
*           IF PSWLEN = 0 THEN              /* IF PSW NOT ENTERED,   */
         CR    R1,R15                                              0181
         BNE   @RF00181                                            0181
*             DO;                           /* THEN                  */
*               PWKEY = 0;                  /* SET KEY TO 0          */
         STH   R15,PWKEY(,R14)                                     0183
*               PWNUM = 0;                  /* SET NUMBER TO 0       */
         STH   R15,PWNUM(,R14)                                     0184
*             END;                                                 0185
*           ELSE                            /* ELSE,                 */
*             PWNUM = 1;                    /* SET PSW NUMBER TO 1   */
         B     @RC00181                                            0186
@RF00181 L     R1,TUPSWD(,GTPLOUT1)                                0186
         MVC   PWNUM(2,R1),@HW1                                    0186
*         END;                                                     0187
@RC00181 DS    0H                                                  0188
*        CNCTFLG1 = '00'X;                  /* SET FLAGS TO 0        */
@RF00177 MVI   CNCTFLG1,X'00'                                      0188
*        S99NOCNV = '1'B;                   /* DONT USE EXISTING   0189
*                                              ALLOCATIONS           */
         L     R1,PTRS99RB(,R3AWAPTR)                              0189
         OI    S99NOCNV(R1),B'01000000'                            0189
*        DO WHILE (SEARCH = '0'B);          /* ALLOCATE DS LIST      */
         B     @DE00190                                            0190
@DL00190 DS    0H                                                  0191
*          CALL DYNSVC;                     /* INVOKE DYN ALLOCATION */
         BAL   R14,DYNSVC                                          0191
*          IF LISTNMBR = DSNCTR THEN        /* IF 1ST DS IN LIST,    */
         CH    LISTNMBR,DSNCTR(,R3AWAPTR)                          0192
         BNE   @RF00192                                            0192
*            CALL FIRSTCC;                  /* PROCESS               */
         BAL   R14,FIRSTCC                                         0193
*          ELSE                             /* IF NOT 1ST DS,        */
*            CALL OTHERCC;                  /* PROCESS               */
         B     @RC00192                                            0194
@RF00192 BAL   R14,OTHERCC                                         0194
*          IF RBCODE12 = '0'B THEN          /* IF NO ERROR FOUND     */
@RC00192 TM    RBCODE12(R3AWAPTR),B'10000000'                      0195
         BNZ   @RF00195                                            0195
*            DO;                            /* WHILE PROCESSING      */
*              LISTNMBR = LISTNMBR-1;       /* DECREMENT COUNTER     */
         BCTR  LISTNMBR,0                                          0197
*              IF LISTNMBR = 0 THEN         /* IF COUNTER 0          */
         LTR   LISTNMBR,LISTNMBR                                   0198
         BNZ   @RF00198                                            0198
*                SEARCH = '1'B;             /* EXIT FROM DO-WHILE    */
         OI    SEARCH,B'10000000'                                  0199
*              ELSE                         /* OTHERWISE             */
*                DO;                        /* PROCESS NEXT DSN      */
         B     @RC00198                                            0200
@RF00198 DS    0H                                                  0201
*                  DPTR2 = DPTR2+2+DALEN;   /* UPDATE DSN PTR,       */
         LA    R15,2                                               0201
         LR    R14,R2DPTR2                                         0201
         ALR   R14,R15                                             0201
         L     R1,TUDSN(,GTPLOUT1)                                 0201
         AH    R14,DALEN(,R1)                                      0201
         LR    R2DPTR2,R14                                         0201
*                  DALEN = DATALEN;         /* MOVE DS LEN & PARM    */
         LH    R0,DATALEN(,R2DPTR2)                                0202
         STH   R0,DALEN(,R1)                                       0202
*                  DAPARM(1:DALEN)=DATAVAL(1:DALEN);               0203
         BCTR  R0,0                                                0203
         LR    R14,R0                                              0203
         EX    R14,@SM00888                                        0203
*                  IF TUMBR ^= 0 THEN       /* IF MEMBER UNIT BUILT  */
         SLR   R14,R14                                             0204
         L     R1,TUMBR(,GTPLOUT1)                                 0204
         CR    R1,R14                                              0204
         BE    @RF00204                                            0204
*                   DO;                     /* THEN                  */
*                  MPTR2 = MPTR2+2+SAVMBLEN;/* PTR TO MBR LEN & PARM */
         ALR   R15,R7MPTR2                                         0206
         ALR   R15,SAVMBLEN                                        0206
         LR    R7MPTR2,R15                                         0206
*                  SAVMBLEN = MEMLEN;       /* SAVE MEMBER LENGTH    */
         LH    R15,MEMLEN(,R7MPTR2)                                0207
         LR    SAVMBLEN,R15                                        0207
*                  IF MEMLEN = 0 THEN       /* IF MBR NOT ENTERED,   */
         CR    R15,R14                                             0208
         BNE   @RF00208                                            0208
*                    DO;                    /* THEN                  */
*                    MBKEY = 0;             /* SET KEY TO 0          */
         STH   R14,MBKEY(,R1)                                      0210
*                    MBNUM = 0;             /* SET NUMBER TO 0       */
         STH   R14,MBNUM(,R1)                                      0211
*                    END;                                          0212
*                  ELSE                                            0213
*                    DO;                    /* OTHERWISE             */
         B     @RC00208                                            0213
@RF00208 DS    0H                                                  0214
*                      MBKEY = DALMEMBR;    /* SET MEMBER KEY        */
         L     R15,TUMBR(,GTPLOUT1)                                0214
         MVC   MBKEY(2,R15),@HWMEMBR                               0214
*                      MBNUM = 1;           /* SET MBR NUMBER        */
         MVC   MBNUM(2,R15),@HW1                                   0215
*                      MBLEN = MEMLEN;      /* MOVE MBR LEN & PARM   */
         LH    R14,MEMLEN(,R7MPTR2)                                0216
         STH   R14,MBLEN(,R15)                                     0216
*                      MBPARM(1:MBLEN)=MBRVAL(1:MBLEN);            0217
         BCTR  R14,0                                               0217
         EX    R14,@SM00890                                        0217
*                    END;                                          0218
*                   END;                                           0219
@RC00208 DS    0H                                                  0220
*                  IF TUPSWD ^= 0 THEN      /* IF PSWORD UNIT BUILT  */
@RF00204 SLR   R15,R15                                             0220
         L     R14,TUPSWD(,GTPLOUT1)                               0220
         CR    R14,R15                                             0220
         BE    @RF00220                                            0220
*                   DO;                     /* THEN                  */
*                  PPTR2 = PPTR2+2+SAVPWLEN;/* PTR TO PSW LEN & PARM */
         LA    R1,2                                                0222
         ALR   R1,R9PPTR2                                          0222
         AH    R1,SAVPWLEN                                         0222
         LR    R9PPTR2,R1                                          0222
*                  SAVPWLEN = PSWLEN;       /* SAVE PASSWORD LENGTH  */
         LH    R1,PSWLEN(,R9PPTR2)                                 0223
         STH   R1,SAVPWLEN                                         0223
*                  IF PSWLEN = 0 THEN       /* IF PSW NOT ENTERED,   */
         CR    R1,R15                                              0224
         BNE   @RF00224                                            0224
*                    DO;                    /* THEN                  */
*                    PWKEY = 0;             /* SET KEY TO 0          */
         STH   R15,PWKEY(,R14)                                     0226
*                    PWNUM = 0;             /* SET NUMBER TO 0       */
         STH   R15,PWNUM(,R14)                                     0227
*                    END;                                          0228
*                  ELSE                                            0229
*                    DO;                    /* OTHERWISE             */
         B     @RC00224                                            0229
@RF00224 DS    0H                                                  0230
*                      PWKEY = DALPASSW;    /* SET PASSWORD KEY      */
         L     R15,TUPSWD(,GTPLOUT1)                               0230
         MVC   PWKEY(2,R15),@HWPASSW                               0230
*                      PWNUM = 1;           /* SET PASSWORD NUMBER   */
         MVC   PWNUM(2,R15),@HW1                                   0231
*                      PWLEN = PSWLEN;      /* MOVE PSW LEN & PARM   */
         LH    R14,PSWLEN(,R9PPTR2)                                0232
         STH   R14,PWLEN(,R15)                                     0232
*                      PWPARM(1:PWLEN)=PSWVAL(1:PWLEN);            0233
         BCTR  R14,0                                               0233
         EX    R14,@SM00892                                        0233
*                    END;                                          0234
*                   END;                                           0235
*                END;                                              0236
*            END;                                                  0237
*          ELSE                             /* IF ERROR IN PROCESSING*/
*            SEARCH = '1'B;                 /* EXIT DO-WHILE         */
         B     @RC00195                                            0238
@RF00195 OI    SEARCH,B'10000000'                                  0238
*        END;                               /* END DO-WHILE          */
@RC00195 DS    0H                                                  0239
@DE00190 TM    SEARCH,B'10000000'                                  0239
         BZ    @DL00190                                            0239
*        SEARCH = '0'B;                     /* RE-SET SWITCH         */
         NI    SEARCH,B'01111111'                                  0240
*        S99NOCNV = '0'B;                   /* TURN OFF NO CONVERT   */
         L     R10,PTRS99RB(,R3AWAPTR)                             0241
         NI    S99NOCNV(R10),B'10111111'                           0241
*   END CONCTDSN;                                                  0242
@EL00003 DS    0H                                                  0242
@EF00003 DS    0H                                                  0242
@ER00003 LM    R14,R7,@SA00003                                     0242
         LM    R9,R12,@SA00003+40                                  0242
         BR    R14                                                 0242
*/********************************************************************/
*/*                                                                  */
*/* SEGMENT NAME -                                                   */
*/*      CONCTERR                                                    */
*/*                                                                  */
*/* FUNCTION -                                                       */
*/*      THIS ROUTINE FREES ANY DATA SETS THAT HAVE BEEN ALLOCATED   */
*/*      PRIOR TO FINDING AN ERROR IN THE CONCATENATION REQUEST.     */
*/*                                                                  */
*/* OPERATION -                                                      */
*/*      EACH DDNAME IS FREED. IF AN ERROR OCCURS WHILE FREEING ANY  */
*/*      OF THE DATA SETS, ISSUE AN ERROR MESSAGE AND CONTINUE.      */
*/*      WHEN ALL THE DATA SETS HAVE BEEN FREED, ISSUE THE           */
*/*      APPROPRIATE ERROR MSG.                                      */
*/*                                                                  */
*/* INPUT -                                                          */
*/*      LIST OF DDNAMES                                             */
*/*                                                                  */
*/* OUTPUT -                                                         */
*/*      LIST OF DDNAMES FREED                                       */
*/*                                                                  */
*/* NOTE:  THIS ROUTINE REPRESENTS A DIRECT LINE FUNCTION AND CAN    */
*/*        BE INCORPORATED AS AN INCLUDED CODE SEGMENT WHEN THE      */
*/*        INCLUDE FUNCTION HAS RECEIVED A STANDARD DEFINITION.      */
*/********************************************************************/
*    CONCTERR: PROC;                                               0243
CONCTERR STM   R14,R4,@SA00004                                     0243
         STM   R6,R12,@SA00004+28                                  0243
*        DDLST = ADDR(DDNMLEN);        /* PTR TO DD LEN & PARM       */
         LA    R5DDLST,DDNMLEN(,DDNAPTR)                           0244
*        IF ALLOCERR = '1'B THEN       /* IF ALLOCATION ERROR        */
         TM    ALLOCERR,B'01000000'                                0245
         BNO   @RF00245                                            0245
*          DSFREED = DSNCTR - LISTNMBR;/* GET NUMBER TO FREE         */
         LH    DSFREED,DSNCTR(,R3AWAPTR)                           0246
         SLR   DSFREED,LISTNMBR                                    0246
*        ELSE                          /* IF OTHER ERROR,            */
*          DSFREED = DSNCTR - LISTNMBR +1;   /* ADD 1 MORE           */
         B     @RC00245                                            0247
@RF00245 LH    DSFREED,DSNCTR(,R3AWAPTR)                           0247
         SLR   DSFREED,LISTNMBR                                    0247
         AL    DSFREED,@FW1                                        0247
*        PERMCTKY = DUNUNALC;          /* SET UNALLOC OPTION KEY     */
@RC00245 MVC   PERMCTKY(2,DDNAPTR),@HWUNALC                        0248
*        DDNMPTR = ADDR(DDFREEBF);     /* PTR TO TEXT TO BE BUILT    */
         LA    R10,DDFREEBF                                        0249
         ST    R10,DDNMPTR(,DDNAPTR)                               0249
*        DDFKEY = DUNDDNAM;            /* SET KEY IN TEXT UNIT       */
         MVC   DDFKEY(2),@HWDDNAM                                  0250
*        DDFNUM = 1;                   /* SET NUMBER TO 1            */
         MVC   DDFNUM(2),@HW1                                      0251
*        DDFLEN = DDNMLNGH;            /* MOVE DDNAME LENGTH         */
         LH    R10,DDNMLNGH(,R5DDLST)                              0252
         STH   R10,DDFLEN                                          0252
*        DDFVAL(1:DDFLEN)=DDNMPARM(1:DDFLEN);  /* MOVE DDNAME        */
         BCTR  R10,0                                               0253
         EX    R10,@SM00896                                        0253
*        S99VERB = S99VRBUN;           /* SET UNALLOC VERB CODE      */
         L     R10,PTRS99RB(,R3AWAPTR)                             0254
         MVI   S99VERB(R10),X'02'                                  0254
*        S99TXTPP = DDNAPTR;           /* PTR TO DDNAMES             */
         ST    DDNAPTR,S99TXTPP(,R10)                              0255
*        S99NOCNV = '0'B;              /* TURN OFF FLAGS             */
         NI    S99NOCNV(R10),B'10111111'                           0256
*        DDNMEND = '1'B;               /* END ADDRESS LIST           */
         OI    DDNMEND(DDNAPTR),B'10000000'                        0257
*        DO WHILE (SEARCH='0'B);       /* UNALLOCATE DDNAMES         */
         B     @DE00258                                            0258
@DL00258 DS    0H                                                  0259
*          CALL DYNSVC;                /* INVOKE DYN ALLOCATION      */
         BAL   R14,DYNSVC                                          0259
*          IF RCODESV > 0 THEN         /* IF UNALLOCATION FAILED,    */
         L     R10,RCODESV(,R3AWAPTR)                              0260
         LTR   R10,R10                                             0260
         BNP   @RF00260                                            0260
*            DO;                                                   0261
*              ENTRYCD = 1;            /* SET ENTRY CODE             */
         MVC   ENTRYCD(2,R3AWAPTR),@HW1                            0262
*              R1 = AWAPTR;            /* WORKAREA PTR IN REG 1      */
         LR    R1,R3AWAPTR                                         0263
*              CALL IKJEFD35;          /* ISSUE ERROR MESSAGE        */
         L     R15,VCONSD35                                        0264
         BALR  R14,R15                                             0264
*            END;                                                  0265
*          DSFREED = DSFREED-1;        /* DECREMENT CTR BY 1         */
@RF00260 BCTR  DSFREED,0                                           0266
*          IF DSFREED = 0 THEN         /* IF CTR IS 0 (END OF LIST)  */
         LTR   DSFREED,DSFREED                                     0267
         BNZ   @RF00267                                            0267
*            SEARCH = '1'B;            /* EXIT FROM DO-WHILE         */
         OI    SEARCH,B'10000000'                                  0268
*          ELSE                                                    0269
*            DO;                       /* OTHERWISE, UPDATE PTR TO   */
         B     @RC00267                                            0269
@RF00267 DS    0H                                                  0270
*              DDLST = DDLST + 2 + DDNMLNGH; /* DD LEN AND PARM      */
         LA    R10,2                                               0270
         ALR   R10,R5DDLST                                         0270
         AH    R10,DDNMLNGH(,R5DDLST)                              0270
         LR    R5DDLST,R10                                         0270
*              DDFLEN = DDNMLNGH;     /* MOVE LEN AND PARM          */
         LH    R10,DDNMLNGH(,R5DDLST)                              0271
         STH   R10,DDFLEN                                          0271
*              DDFVAL(1:DDFLEN)=DDNMPARM(1:DDFLEN);                0272
         BCTR  R10,0                                               0272
         EX    R10,@SM00896                                        0272
*            END;                                                  0273
*        END;                          /* END DO-WHILE               */
@RC00267 DS    0H                                                  0274
@DE00258 TM    SEARCH,B'10000000'                                  0274
         BZ    @DL00258                                            0274
*        SEARCH = '0'B;                /* RESET SWITCH               */
         NI    SEARCH,B'01111111'                                  0275
*        CALL CCERRMSG;                /* ISSUE APPROPRIATE MSG      */
         BAL   R14,CCERRMSG                                        0276
*   END CONCTERR;                                                  0277
@EL00004 DS    0H                                                  0277
@EF00004 DS    0H                                                  0277
@ER00004 LM    R14,R4,@SA00004                                     0277
         LM    R6,R12,@SA00004+28                                  0277
         BR    R14                                                 0277
*/********************************************************************/
*/*                                                                  */
*/* SEGMENT NAME -                                                   */
*/*      FIRSTCC                                                     */
*/*                                                                  */
*/* FUNCTION -                                                       */
*/*      IF DYNAMIC ALLOCATION FAILED DUE TO FILE IN USE, PROMPT     */
*/*      THE USER FOR THE OPTION. IF USER REQUESTS TO TERMINATE,     */
*/*      ISSUE MESSAGE AND TERMINATE. IF MORE THAN 16 PARTITIONED    */
*/*      DATA SETS SPECIFIED, ISSUE ERROR MESSAGE AND TERMINATE.     */
*/*      ELSE, SAVE THE DSORG (FOR LATER COMPARISONS) AND THE        */
*/*      DDNAME.                                                     */
*/*                                                                  */
*/* INPUT -                                                          */
*/*      WORKAREA                                                    */
*/*                                                                  */
*/* OUTPUT -                                                         */
*/*      RBCODE12 = 0 - CONTINUE WITH NEXT DSNAME                    */
*/*                 1 - ERROR FOUND, TERMINATE                       */
*/*                                                                  */
*/* NOTE:  THIS ROUTINE REPRESENTS A DIRECT LINE FUNCTION AND CAN    */
*/*        BE INCORPORATED AS AN INCLUDED CODE SEGMENT WHEN THE      */
*/*        INCLUDE FUNCTION HAS RECEIVED A STANDARD DEFINITION.      */
*/********************************************************************/
*    FIRSTCC: PROC;                                                0278
FIRSTCC  STM   R14,R4,@SA00005                                     0278
         STM   R6,R12,@SA00005+28                                  0278
*        IF RCODESV > 0 & S99ERROR = '0410'X  THEN  /* IF ALLOCATION
*                                                    FAILED  @YM06354*/
         L     R10,RCODESV(,R3AWAPTR)                              0279
         LTR   R10,R10                                             0279
         BNP   @RF00279                                            0279
         L     R9,PTRS99RB(,R3AWAPTR)                              0279
         CLC   S99ERROR(2,R9),@HEX0410                             0279
         BNE   @RF00279                                            0279
*          DO;                         /* & FILE IN USE THEN @YM06354*/
*            SAVERC = RCODESV;         /* SAVE DYN RC        @Z30LPKH*/
         LR    R7SAVERC,R10                                        0281
*            R1 = AWAPTR;              /* WORKAREA PTR IN REG 1      */
         LR    R1,R3AWAPTR                                         0282
*            CALL IKJEFD36;            /* PROMPT FOR FILE OPTION     */
         L     R15,VCONSD36                                        0283
         BALR  R14,R15                                             0283
*            RCODESV = SAVERC;         /* RESTORE DYN RC     @Z30LPKH*/
         ST    R7SAVERC,RCODESV(,R3AWAPTR)                         0284
*            IF TERMOPT = '1'B THEN    /* IF OPTION TO TERMINATE     */
         TM    TERMOPT(R3AWAPTR),B'00001000'                       0285
         BNO   @RF00285                                            0285
*            DO;                       /* THEN                       */
*            TERMOPT = '0'B;           /* TURN OFF INDICATOR         */
         NI    TERMOPT(R3AWAPTR),B'11110111'                       0287
*            ALLOCERR = '1'B;          /* INDICATE ERROR TYPE        */
         OI    ALLOCERR,B'01000000'                                0288
*            CALL CCERRMSG;            /* ISSUE ERROR MESSAGE        */
         BAL   R14,CCERRMSG                                        0289
*            END;                                                  0290
*            ELSE                                 /*         @YM06354*/
*              FREEFL = '1'B;                     /*         @YM06354*/
         B     @RC00285                                            0291
@RF00285 OI    FREEFL,B'00001000'                                  0291
*          END;                                   /*         @YM06354*/
*        ELSE                          /*                    @OZ02964*/
*            IF RCODESV > 0 & S99ERROR ^= '0410'X THEN  /*   @YM06354*/
         B     @RC00279                                            0293
@RF00279 L     R10,RCODESV(,R3AWAPTR)                              0293
         LTR   R10,R10                                             0293
         BNP   @RF00293                                            0293
         L     R10,PTRS99RB(,R3AWAPTR)                             0293
         CLC   S99ERROR(2,R10),@HEX0410                            0293
         BE    @RF00293                                            0293
*              DO;                                                 0294
*                ALLOCERR = '1'B;      /* INDICATE ERROR TYPE        */
         OI    ALLOCERR,B'01000000'                                0295
*                ENTRYCD = 1;          /* SET ENTRY CODE             */
         MVC   ENTRYCD(2,R3AWAPTR),@HW1                            0296
*                R1 = AWAPTR;          /* WORKAREA IN REG 1          */
         LR    R1,R3AWAPTR                                         0297
*                CALL IKJEFD35;        /* ISSUE DYNALLOC ERROR MSG   */
         L     R15,VCONSD35                                        0298
         BALR  R14,R15                                             0298
*                CALL CCERRMSG;        /* ISSUE ERROR MESSAGE        */
         BAL   R14,CCERRMSG                                        0299
*              END;                                                0300
*        IF FREEFL = '1'B | RCODESV = 0 THEN /* IF ALLOCATION      0301
*                                                 SUCCESSFUL @YM06354*/
@RF00293 DS    0H                                                  0301
@RC00279 TM    FREEFL,B'00001000'                                  0301
         BO    @RT00301                                            0301
         L     R10,RCODESV(,R3AWAPTR)                              0301
         LTR   R10,R10                                             0301
         BNZ   @RF00301                                            0301
@RT00301 DS    0H                                                  0302
*          DO;                         /* THEN                       */
*            FILEKY = 0;               /* SET FILE KEY TO 0          */
         SLR   R10,R10                                             0303
         L     R9,TUFILE(,GTPLOUT1)                                0303
         STH   R10,FILEKY(,R9)                                     0303
*            FILENUM = 0;                                          0304
         STH   R10,FILENUM(,R9)                                    0304
*            DDNMLNGH = RTDDNLEN;      /* SAVE DDNAME RETURNED       */
         L     R10,TURTDDN(,GTPLOUT1)                              0305
         LH    R9,RTDDNLEN(,R10)                                   0305
         STH   R9,DDNMLNGH(,R5DDLST)                               0305
*            DDNMPARM(1:RTDDNLEN) = RTDDNAME(1:RTDDNLEN);          0306
         LR    R7,R9                                               0306
         BCTR  R7,0                                                0306
         EX    R7,@SM00900                                         0306
*            DDLST = DDLST + 2 + RTDDNLEN;                         0307
         LA    R7,2                                                0307
         ALR   R7,R5DDLST                                          0307
         ALR   R7,R9                                               0307
         LR    R5DDLST,R7                                          0307
*            RTDDNLEN = 8;             /* SET DDNAME LENGTH TO 8     */
         MVC   RTDDNLEN(2,R10),@HW8                                0308
*                                      /* IF DSORG PARTITIONED AND 0309
*                                         OVER 16 DSNAMES GIVEN,     */
*            IF DSNCTR > 16 & PARTND = '1'B THEN                   0309
         LH    R10,DSNCTR(,R3AWAPTR)                               0309
         C     R10,@FW16                                           0309
         BNH   @RF00309                                            0309
         L     R10,TURTDSO(,GTPLOUT1)                              0309
         TM    PARTND(R10),B'00000010'                             0309
         BNO   @RF00309                                            0309
*              DO;                     /* THEN                       */
*                MAXERR = '1'B;        /* INDICATE ERROR TYPE        */
         OI    MAXERR,B'00100000'                                  0311
*                CALL CONCTERR;        /* ISSUE ERROR MESSAGE        */
         BAL   R14,CONCTERR                                        0312
*              END;                                                0313
*            ELSE                                                  0314
*              DO;                                                 0314
         B     @RC00309                                            0314
@RF00309 DS    0H                                                  0315
*                IF SEQUEN = '1'B THEN /* IF SEQUENTIAL DSORG        */
         L     R10,TURTDSO(,GTPLOUT1)                              0315
         TM    SEQUEN(R10),B'01000000'                             0315
         BNO   @RF00315                                            0315
*                  DSORGSQ = '1'B;     /* INDICATE SO                */
         OI    DSORGSQ,B'01000000'                                 0316
*                ELSE                                              0317
*                  IF PARTND = '1'B THEN   /* IF PARTITIONED DSORG   */
         B     @RC00315                                            0317
@RF00315 L     R10,TURTDSO(,GTPLOUT1)                              0317
         TM    PARTND(R10),B'00000010'                             0317
         BNO   @RF00317                                            0317
*                    IF MBNUM = 1 THEN     /* WITH MEMB SPEC @ZA04568*/
         L     R10,TUMBR(,GTPLOUT1)                                0318
         CLC   MBNUM(2,R10),@HW1                                   0318
         BNE   @RF00318                                            0318
*                      DSORGSQ = '1'B;     /* INDICATE SEQ   @ZA04568*/
         OI    DSORGSQ,B'01000000'                                 0319
*                    ELSE                  /* DSORG. MEM NOT @ZA04568*/
*                      DSORGPR = '1'B;     /* SPEC DSORD IS  @ZA04568*/
         B     @RC00318                                            0320
@RF00318 OI    DSORGPR,B'00100000'                                 0320
*                                          /* PARTITIONED    @ZA04568*/
*                  ELSE                                            0321
*                    IF VSAM = '1'B THEN   /* IF VSAM DATA SET       */
         B     @RC00317                                            0321
@RF00317 L     R10,TURTDSO(,GTPLOUT1)                              0321
         TM    VSAM(R10),B'00010000'                               0321
         BNO   @RF00321                                            0321
*                      DSORGVM = '1'B;     /* INDICATE SO            */
         OI    DSORGVM,B'00010000'                                 0322
*                    ELSE                                          0323
*                      DO;                 /* IF NEITHER OF ABOVE    */
         B     @RC00321                                            0323
@RF00321 DS    0H                                                  0324
*                        INVORG = '1'B;    /* INDICATE ERROR TYPE    */
         OI    INVORG,B'00001000'                                  0324
*                        CALL CONCTERR;    /* ISSUE ERROR MESSAGE    */
         BAL   R14,CONCTERR                                        0325
*                      END;                                        0326
*              END;                                                0327
*          END;                                                    0328
*    END FIRSTCC;                                                  0329
@EL00005 DS    0H                                                  0329
@EF00005 DS    0H                                                  0329
@ER00005 LM    R14,R4,@SA00005                                     0329
         LM    R6,R12,@SA00005+28                                  0329
         BR    R14                                                 0329
*/********************************************************************/
*/*                                                                  */
*/* SEGMENT NAME -                                                   */
*/*      OTHERCC                                                     */
*/*                                                                  */
*/* FUNCTION -                                                       */
*/*      THIS ROUTINE PROCESSES DATA SETS IN THE LIST OTHER THAN     */
*/*      THE FIRST DATA SET.                                         */
*/*                                                                  */
*/* OPERATION -                                                      */
*/*      IF DYNAMIC ALLOCATION FAILED, ISSUE ERROR MESSAGE AND       */
*/*      TERMINATE. IF THE DATA SET ORGANIZATION IS NOT THE SAME     */
*/*      AS THAT OF THE FIRST DATA SET, ISSUE AN ERROR MESSAGE AND   */
*/*      TERMINATE. SAVE THE DDNAME RETURNED BY DYNAMIC ALLOCATION   */
*/*      FOR THIS DATA SET.                                          */
*/*                                                                  */
*/* INPUT -                                                          */
*/*      DATA SET ORGANIZATION                                       */
*/*      DDNAME                                                      */
*/*                                                                  */
*/* OUTPUT -                                                         */
*/*      RBCODE12 = 0 - CONTINUE WITH NEXT DSNAME                    */
*/*                 1 - ERROR FOUND, TERMINATE                       */
*/*                                                                  */
*/* NOTE:  THIS ROUTINE REPRESENTS A DIRECT LINE FUNCTION AND CAN    */
*/*        BE INCORPORATED AS AN INCLUDED CODE SEGMENT WHEN THE      */
*/*        INCLUDE FUNCTION HAS RECEIVED A STANDARD DEFINITION.      */
*/********************************************************************/
*    OTHERCC: PROC;                                                0330
OTHERCC  STM   R14,R4,@SA00006                                     0330
         STM   R6,R12,@SA00006+28                                  0330
*        IF RCODESV > 0 THEN           /* IF ALLOCATION FAILED,      */
         L     R10,RCODESV(,R3AWAPTR)                              0331
         LTR   R10,R10                                             0331
         BNP   @RF00331                                            0331
*          DO;                         /* THEN                       */
*            ALLOCERR = '1'B;          /* INDICATE ERROR TYPE        */
         OI    ALLOCERR,B'01000000'                                0333
*            ENTRYCD = 1;              /* SET ENTRY CODE             */
         MVC   ENTRYCD(2,R3AWAPTR),@HW1                            0334
*            R1 = AWAPTR;              /* WORKAREA IN REG 1          */
         LR    R1,R3AWAPTR                                         0335
*            CALL IKJEFD35;            /* ISSUE DYNALLOC ERROR MSG   */
         L     R15,VCONSD35                                        0336
         BALR  R14,R15                                             0336
*            CALL CONCTERR;            /* ISSUE ERROR MESSAGE        */
         BAL   R14,CONCTERR                                        0337
*          END;                                                    0338
*        ELSE                          /* ELSE,                      */
*          DO;                                                     0339
         B     @RC00331                                            0339
@RF00331 DS    0H                                                  0340
*            DDNMLNGH = RTDDNLEN;      /* SAVE DDNAME RETURNED       */
         L     R10,TURTDDN(,GTPLOUT1)                              0340
         LH    R9,RTDDNLEN(,R10)                                   0340
         STH   R9,DDNMLNGH(,R5DDLST)                               0340
*            DDNMPARM(1:RTDDNLEN) = RTDDNAME(1:RTDDNLEN);          0341
         LR    R7,R9                                               0341
         BCTR  R7,0                                                0341
         EX    R7,@SM00900                                         0341
*            DDLST = DDLST +2 + RTDDNLEN;                          0342
         LA    R10,2                                               0342
         ALR   R10,R5DDLST                                         0342
         ALR   R10,R9                                              0342
         LR    R5DDLST,R10                                         0342
*                                      /* CHECK DSORG TO BE SAME     */
*            IF (DSORGSQ='1'B & (SEQUEN='0'B & MBNUM=0))|  /*@ZA04568*/
*               (DSORGPR='1'B & PARTND='0'B) |                     0343
*               (DSORGPR='1'B & (PARTND='1'B & MBNUM=1))|  /*@ZA04568*/
*                (DSORGVM='1'B & VSAM='0'B) THEN                   0343
         TM    DSORGSQ,B'01000000'                                 0343
         BNO   @GL00007                                            0343
         L     R10,TURTDSO(,GTPLOUT1)                              0343
         TM    SEQUEN(R10),B'01000000'                             0343
         BNZ   @GL00007                                            0343
         L     R10,TUMBR(,GTPLOUT1)                                0343
         LH    R10,MBNUM(,R10)                                     0343
         LTR   R10,R10                                             0343
         BZ    @RT00343                                            0343
@GL00007 TM    DSORGPR,B'00100000'                                 0343
         BNO   @GL00006                                            0343
         L     R10,TURTDSO(,GTPLOUT1)                              0343
         TM    PARTND(R10),B'00000010'                             0343
         BZ    @RT00343                                            0343
@GL00006 TM    DSORGPR,B'00100000'                                 0343
         BNO   @GL00005                                            0343
         L     R10,TURTDSO(,GTPLOUT1)                              0343
         TM    PARTND(R10),B'00000010'                             0343
         BNO   @GL00005                                            0343
         L     R10,TUMBR(,GTPLOUT1)                                0343
         CLC   MBNUM(2,R10),@HW1                                   0343
         BE    @RT00343                                            0343
@GL00005 TM    DSORGVM,B'00010000'                                 0343
         BNO   @RF00343                                            0343
         L     R10,TURTDSO(,GTPLOUT1)                              0343
         TM    VSAM(R10),B'00010000'                               0343
         BNZ   @RF00343                                            0343
@RT00343 DS    0H                                                  0344
*              DO;                     /* NO -                       */
*                DSORGERR = '1'B;      /* INDICATE ERROR TYPE        */
         OI    DSORGERR,B'00010000'                                0345
*                CALL CONCTERR;        /* ISSUE ERROR MESSAGE        */
         BAL   R14,CONCTERR                                        0346
*              END;                                                0347
*          END;                                                    0348
*    END OTHERCC;                                                  0349
@EL00006 DS    0H                                                  0349
@EF00006 DS    0H                                                  0349
@ER00006 LM    R14,R4,@SA00006                                     0349
         LM    R6,R12,@SA00006+28                                  0349
         BR    R14                                                 0349
*/********************************************************************/
*/*                                                                  */
*/* SEGMENT NAME -                                                   */
*/*      CCERRMSG                                                    */
*/*                                                                  */
*/* FUNCTION -                                                       */
*/*      THIS ROUTINE ISSUES A MESSAGE WHEN AN ERROR HAS BEEN FOUND  */
*/*      IN THE CONCATENATION REQUEST.                               */
*/*                                                                  */
*/* OPERATION -                                                      */
*/*      THE FIRST LEVEL MESSAGE NOTIFIES THE USER THAT THE DATA SETS*/
*/*      WERE NOT CONCATENATED. THE SECOND LEVEL MESSAGE GIVES ONE   */
*/*      OF THE FOLLOWING REASONS-                                   */
*/*      NUMBER OF DATA SETS IN THE LIST EXCEEDS THE MAXIMUM, DSORG  */
*/*      IS NOT THE SAME FOR ALL DATA SETS IN THE LIST, STATUS IS    */
*/*      NEW,MOD, OR SYSOUT, OR ALLOCATION ERROR OCCURRED.           */
*/*                                                                  */
*/* INPUT -                                                          */
*/*      ERROR TYPE INDICATOR                                        */
*/*                                                                  */
*/* OUTPUT -                                                         */
*/*      MESSAGE ISSUED                                              */
*/*                                                                  */
*/* NOTE:  THIS ROUTINE REPRESENTS A DIRECT LINE FUNCTION AND CAN    */
*/*        BE INCORPORATED AS AN INCLUDED CODE SEGMENT WHEN THE      */
*/*        INCLUDE FUNCTION HAS RECEIVED A STANDARD DEFINITION.      */
*/********************************************************************/
*    CCERRMSG: PROC;                                               0350
CCERRMSG STM   R14,R12,@SA00007                                    0350
*        MSGTABLE = MSGTABLE && MSGTABLE;/* CLEAR INTERFACE AREA     */
         L     R10,MSGPTR(,R3AWAPTR)                               0351
         XC    MSGTABLE(64,R10),MSGTABLE(R10)                      0351
*        RBCODE12 = '1'B;                /* INDICATE ALLOC FAILED    */
         OI    RBCODE12(R3AWAPTR),B'10000000'                      0352
*        IF MAXERR = '1'B THEN           /* IF MAXIMUM ERROR,        */
         TM    MAXERR,B'00100000'                                  0353
         BNO   @RF00353                                            0353
*          MSGID = '107A';               /* SET MESSAGE ID           */
         MVC   MSGID(4,R10),@CHR107A                               0354
*        IF DSORGERR = '1'B THEN         /* IF DSORG ERROR,          */
@RF00353 TM    DSORGERR,B'00010000'                                0355
         BNO   @RF00355                                            0355
*          MSGID = '106A';               /* SET MESSAGE ID           */
         L     R10,MSGPTR(,R3AWAPTR)                               0356
         MVC   MSGID(4,R10),@CHR106A                               0356
*        IF INVORG = '1'B THEN           /* IF INVALID DSORG,        */
@RF00355 TM    INVORG,B'00001000'                                  0357
         BNO   @RF00357                                            0357
*          MSGID = '110A';               /* SET MESSAGE ID           */
         L     R10,MSGPTR(,R3AWAPTR)                               0358
         MVC   MSGID(4,R10),@CHR110A                               0358
*        IF STATSERR = '1'B THEN         /* IF STATUS ERROR,         */
@RF00357 TM    STATSERR,B'10000000'                                0359
         BNO   @RF00359                                            0359
*          DO;                           /* THEN                     */
*            MSGID = '108A';             /* SET MESSAGE ID           */
         L     R10,MSGPTR(,R3AWAPTR)                               0361
         MVC   MSGID(4,R10),@CHR108A                               0361
*            IF STATSPDE = 3 THEN        /* IF STATUS MOD,           */
         L     R5,PTRPDL(,R3AWAPTR)                                0362
         CLC   STATSPDE(2,R5),@HW3                                 0362
         BNE   @RF00362                                            0362
*              DO;                       /* THEN                     */
*                VAR1 = ADDR(MOD);       /* PUT MOD VARIABLE         */
         LA    R5,MOD                                              0364
         ST    R5,@TF00001                                         0364
         MVC   VAR1(3,R10),@TF00001+1                              0364
*                L1 = LENGTH(MOD);       /* AND LENGTH IN LIST       */
         MVI   L1(R10),X'03'                                       0365
*              END;                                                0366
*            IF STATSPDE = 4 THEN        /* IF STATUS NEW            */
@RF00362 L     R10,PTRPDL(,R3AWAPTR)                               0367
         CLC   STATSPDE(2,R10),@HW4                                0367
         BNE   @RF00367                                            0367
*              DO;                       /* THEN,                    */
*                VAR1 = ADDR(NEW);       /* PUT NEW VARIABLE         */
         L     R10,MSGPTR(,R3AWAPTR)                               0369
         LA    R5,NEW                                              0369
         ST    R5,@TF00001                                         0369
         MVC   VAR1(3,R10),@TF00001+1                              0369
*                L1 = LENGTH(NEW);       /* AND LENGTH IN LIST       */
         MVI   L1(R10),X'03'                                       0370
*              END;                                                0371
*            IF STATSPDE = 5 THEN        /* IF STATUS SYSOUT         */
@RF00367 L     R10,PTRPDL(,R3AWAPTR)                               0372
         CLC   STATSPDE(2,R10),@HW5                                0372
         BNE   @RF00372                                            0372
*              DO;                       /* THEN                     */
*                VAR1 = ADDR(SYSOUT);    /* PUT SYSOUT VARIABLE      */
         L     R10,MSGPTR(,R3AWAPTR)                               0374
         LA    R5,SYSOUT                                           0374
         ST    R5,@TF00001                                         0374
         MVC   VAR1(3,R10),@TF00001+1                              0374
*                L1 = LENGTH(SYSOUT);    /* AND LENGTH IN LIST       */
         MVI   L1(R10),X'06'                                       0375
*              END;                                                0376
*          END;                                                    0377
@RF00372 DS    0H                                                  0378
*        IF ALLOCERR = '1'B THEN         /* IF ALLOCATION ERROR,     */
@RF00359 TM    ALLOCERR,B'01000000'                                0378
         BNO   @RF00378                                            0378
*          DO;                                                     0379
*            MSGID = '109A';             /* SET MESSAGE ID           */
         L     R10,MSGPTR(,R3AWAPTR)                               0380
         MVC   MSGID(4,R10),@CHR109A                               0380
*            VAR1 = ADDR(DAPARM);        /* PUT DSNAME VARIABLE      */
         L     R5,TUDSN(,GTPLOUT1)                                 0381
         LA    R2,DAPARM(,R5)                                      0381
         ST    R2,@TF00001                                         0381
         MVC   VAR1(3,R10),@TF00001+1                              0381
*            L1 = DALEN;                 /* AND LENGTH IN LIST       */
         LH    R5,DALEN(,R5)                                       0382
         STC   R5,L1(,R10)                                         0382
*          END;                                                    0383
*        LISTPTR = ADDR(MSGCSECT);       /* ADDR OF PARM LIST        */
@RF00378 L     R10,MSGPTR(,R3AWAPTR)                               0384
         LA    R5,MSGCSECT(,R10)                                   0384
         ST    R5,LISTPTR(,R10)                                    0384
*        MTCPPL = CPPLPTR;               /* PTR TO CPPL              */
         L     R5,CPPLPTR(,R3AWAPTR)                               0385
         ST    R5,MTCPPL(,R10)                                     0385
*        ECBPTR = ADDR(COMMECB);         /* PTR TO ECB               */
         LA    R5,COMMECB(,R3AWAPTR)                               0386
         ST    R5,ECBPTR(,R10)                                     0386
*        COMMECB = 0;                    /* INIT ECB TO 0            */
         SLR   R5,R5                                               0387
         ST    R5,COMMECB(,R3AWAPTR)                               0387
*        MTHIGH = '1'B;                  /* SET HIGH ORDER BIT ON    */
         OI    MTHIGH(R10),B'10000000'                             0388
*        MTPUTLSW = '1'B;                /* INDICATE PUTLINE         */
         OI    MTPUTLSW(R10),B'01000000'                           0389
*        MSGCSECT = PTRMSGS;             /* MSG CSECT ADDRESS        */
         L     R5,PTRMSGS(,R3AWAPTR)                               0390
         ST    R5,MSGCSECT(,R10)                                   0390
*        R1 = ADDR(MSGTABLE);            /* PARM LIST IN REG 1       */
         LR    R1,R10                                              0391
*        GEN (LINK EP=IKJEFF02);         /* INVOKE MESSAGE PROCESSOR */
*                                                                  0392
         LINK EP=IKJEFF02
*    END CCERRMSG;                                                 0393
@EL00007 DS    0H                                                  0393
@EF00007 DS    0H                                                  0393
@ER00007 LM    R14,R12,@SA00007                                    0393
         BR    R14                                                 0393
*/********************************************************************/
*/*                                                                  */
*/* SEGMENT NAME -                                                   */
*/*      PRMPTDDN                                                    */
*/*                                                                  */
*/* FUNCTION -                                                       */
*/*      THIS ROUTINE PROMPTS THE USER TO ENTER A FILENAME WHEN      */
*/*      THE PARAMETER HAS BEEN OMITTED AND IS REQUIRED.             */
*/*                                                                  */
*/* OPERATION -                                                      */
*/*       INVOKE PARSE WITH A SECONDARY PCL AND PDL ADDRESS.  IF     */
*/*       PARSE FAILS, ISSUE ERROR MESSAGE. ELSE, OVERLAY THE        */
*/*       ORIGINAL PDE WITH THE PDE RETURNED FROM THE PROMPT.        */
*/*                                                                  */
*/* INPUT -                                                          */
*/*       PTR TO FILE PDE                                            */
*/*       PTR TO SECONDARY PCL                                       */
*/*                                                                  */
*/* OUTPUT -                                                         */
*/*       FILE PDE OVERLAID WITH NEW DATA                            */
*/*                                                                  */
*/* NOTE:  THIS ROUTINE REPRESENTS A DIRECT LINE FUNCTION AND CAN    */
*/*        CAN BE INCORPORATED AS AN INCLUDED CODE SEGMENT WHEN THE  */
*/*        INCLUDE FUNCTION HAS RECEIVED A STANDARD DEFINITION.      */
*/********************************************************************/
*    PRMPTDDN: PROC;                                               0394
PRMPTDDN STM   R14,R12,@SA00008                                    0394
*        ENTRYCD = 4;                      /* INDICATE DDN PROMPT    */
         MVC   ENTRYCD(2,R3AWAPTR),@HW4                            0395
*        R1 = AWAPTR;                      /* WORKAREA PTR IN REG 1  */
         LR    R1,R3AWAPTR                                         0396
*        CALL IKJEFD37;                                            0397
         L     R15,VCONSD37                                        0397
         BALR  R14,R15                                             0397
*        IF RCODESV > 0 THEN               /* IF RETURN CODE >0      */
         L     R10,RCODESV(,R3AWAPTR)                              0398
         LTR   R10,R10                                             0398
         BNP   @RF00398                                            0398
*          DO;                                                     0399
*            ENTRYCD = 2;                  /* SET ENTRY CODE         */
         MVC   ENTRYCD(2,R3AWAPTR),@HW2                            0400
*            R1 = AWAPTR;                                          0401
         LR    R1,R3AWAPTR                                         0401
*            CALL IKJEFD35;                                        0402
         L     R15,VCONSD35                                        0402
         BALR  R14,R15                                             0402
*          END;                                                    0403
*        ELSE                                                      0404
*          DO;                             /* ELSE,                  */
         B     @RC00398                                            0404
@RF00398 DS    0H                                                  0405
*            FILEPDE = 1;                  /* INDICATE FILE PRESENT  */
         L     R10,PTRPDL(,R3AWAPTR)                               0405
         MVC   FILEPDE(2,R10),@HW1                                 0405
*            DDNPTR = ADDR(NDDNBUF1);      /* PTR TO NEW BUFFER      */
         LA    R2,NDDNBUF1                                         0406
         ST    R2,DDNPTR(,R10)                                     0406
*            DDNLEN = DDLNGH2;             /* NEW LENGTH             */
         L     R2,PMPTPDL(,R3AWAPTR)                               0407
         LH    R15,DDLNGH2(,R2)                                    0407
         STH   R15,DDNLEN(,R10)                                    0407
*            DDNFLAGS = DDFLAG2;           /* FLAGS AND PARM         */
         MVC   DDNFLAGS(1,R10),DDFLAG2(R2)                         0408
*            NDDNBUF1(1:DDNLEN) = DDNBUF1(1:DDNLEN);               0409
         BCTR  R15,0                                               0409
         L     R10,DDPTR2(,R2)                                     0409
         EX    R15,@SM00903                                        0409
*            RFY R5 RSTD;                  /* RESTRICT REG 5         */
*            R5 = ADDR(PMPTPDL);          /* PROMPT PDL IN REG 5    */
         LA    R5,PMPTPDL(,R3AWAPTR)                               0411
*            GEN (IKJRLSA (5));            /* RELEASE PDL            */
         IKJRLSA (5)
*            RFY R5 UNRSTD;                /* UNRESTRICT REG 5       */
*          END;                                                    0414
*    END PRMPTDDN;                                                 0415
@EL00008 DS    0H                                                  0415
@EF00008 DS    0H                                                  0415
@ER00008 LM    R14,R12,@SA00008                                    0415
         BR    R14                                                 0415
*/********************************************************************/
*/*                                                                  */
*/* SEGMENT NAME -                                                   */
*/*      TRANSRTN                                                    */
*/*                                                                  */
*/* FUNCTION -                                                       */
*/*      THIS MODULE INVOKES GENTRANS TO TRANSLATE PARAMETERS TO     */
*/*      DYNAMIC ALLOCATION TEXT FORMAT.                             */
*/*                                                                  */
*/* OPERATION -                                                      */
*/*      THE TEXT UNIT FOR PERMANENT ALLOCATION IS SET UP. GENTRANS  */
*/*      IS INVOKED. IF GENTRANS FAILS, AN INDICATOR IS SET TO       */
*/*      SHOW THE ERROR. ELSE, THE POINTER TO THE TEXT BUILT BY      */
*/*      GENTRANS IS PUT INTO THE DYNAMIC ALLOCATION REQUEST BLOCK.  */
*/*                                                                  */
*/* INPUT -                                                          */
*/*      GENTRANS PARAMETER LIST                                     */
*/*                                                                  */
*/* OUTPUT -                                                         */
*/*      RBOCDE12 = 0 - GENTRANS SUCCESSFUL                          */
*/*                 1 - GENTRANS ERROR                               */
*/*                                                                  */
*/********************************************************************/
*    TRANSRTN: PROC;                                               0416
TRANSRTN STM   R14,R3,@SA00009                                     0416
         STM   R5,R12,@SA00009+24                                  0416
*        ADLKEY = DALPERMA;            /* PERMANENT ALLOC KEY        */
         L     R10,TXT(,R3AWAPTR)                                  0417
         MVC   ADLKEY(2,R10),@HWPERMA                              0417
*        ADLNBR = 0;                   /* NUMBER 0                   */
         SLR   R9,R9                                               0418
         STH   R9,ADLNBR(,R10)                                     0418
*        ADDTEXT(INC) = TXT;           /* PTR TO TEXT UNIT           */
         LR    R9,R8INC                                            0419
         SLA   R9,2                                                0419
         L     R5,GTPLPTR(,R3AWAPTR)                               0419
         L     R2,GTPLABLK(,R5)                                    0419
         ST    R10,@TF00001                                        0419
         ALR   R9,R2                                               0419
         MVC   ADDTEXT-4(4,R9),@TF00001                            0419
*        ADDNUM = ADDNUM + 1;          /* INCREMENT NUMBER OF ENTRIES *
         LA    R10,1                                               0420
         AH    R10,ADDNUM(,R2)                                     0420
         STH   R10,ADDNUM(,R2)                                     0420
*        R1 = GTPLPTR;                 /* PARM LIST IN REG 1         */
         LR    R1,R5                                               0421
*        GEN (LINK EP=IKJCB831);       /* INVOKE GENTRANS            */
         LINK EP=IKJCB831
*        RCODESV = R15;                /* SAVE RETURN CODE           */
         ST    R15,RCODESV(,R3AWAPTR)                              0423
*        IF RCODESV > 0 THEN           /* IF GENTRANS FAILED,        */
         L     R10,RCODESV(,R3AWAPTR)                              0424
         LTR   R10,R10                                             0424
         BNP   @RF00424                                            0424
*          DO;                         /* THEN                       */
*            ENTRYCD = 3;              /* SET ENTRY CODE             */
         MVC   ENTRYCD(2,R3AWAPTR),@HW3                            0426
*            R1 = AWAPTR;                                          0427
         LR    R1,R3AWAPTR                                         0427
*            CALL IKJEFD35;                                        0428
         L     R15,VCONSD35                                        0428
         BALR  R14,R15                                             0428
*          END;                                                    0429
*        ELSE                          /* OTHERWISE,                 */
*          DO;                                                     0430
         B     @RC00424                                            0430
@RF00424 DS    0H                                                  0431
*            S99TXTPP = GTPLOUTA + 8;  /* TEXT PTR IN REQUEST BLOCK  */
         L     R10,PTRS99RB(,R3AWAPTR)                             0431
         L     R5,GTPLPTR(,R3AWAPTR)                               0431
         L     R5,GTPLOUTA(,R5)                                    0431
         LA    R2,8                                                0431
         ALR   R2,R5                                               0431
         ST    R2,S99TXTPP(,R10)                                   0431
*            GTPLOUT1 = GTPLOUTA;      /* SAVE TEXT PTR              */
         LR    GTPLOUT1,R5                                         0432
*          END;                                                    0433
*                                                                  0433
*    END TRANSRTN;                                                 0434
@EL00009 DS    0H                                                  0434
@EF00009 DS    0H                                                  0434
@ER00009 LM    R14,R3,@SA00009                                     0434
         LM    R5,R12,@SA00009+24                                  0434
         BR    R14                                                 0434
*/********************************************************************/
*/*                                                                  */
*/* SEGMENT NAME -                                                   */
*/*      DYNSVC                                                      */
*/*                                                                  */
*/* FUNCTION -                                                       */
*/*      THIS ROUTINE INVOKES DYNAMIC ALLOCATION.                    */
*/*                                                                  */
*/* OPERATION -                                                      */
*/*      THE PARAMETER LIST IS COMPLETED. SVC 99 IS ISSUED. THE      */
*/*      RETURN CODE IN REGISTER 15 IS SAVED.                        */
*/*                                                                  */
*/* INPUT -                                                          */
*/*      SVC 99 REQUEST BLOCK                                        */
*/*                                                                  */
*/* OUTPUT -                                                         */
*/*      RETURN CODE                                                 */
*/*                                                                  */
*/********************************************************************/
*    DYNSVC: PROC;                                                 0435
*                                                                  0435
DYNSVC   STM   R14,R12,12(R13)                                     0435
*        R1 = ADDR(S99RBPTR);                                      0436
         LA    R1,S99RBPTR                                         0436
*        S99RBPTR = PTRS99RB;                                      0437
         L     R10,PTRS99RB(,R3AWAPTR)                             0437
         ST    R10,S99RBPTR                                        0437
*        S99RBPND = '1'B;              /* HIGH ORDER BIT ON          */
*                                                                  0438
*                                                                  0438
         OI    S99RBPND,B'10000000'                                0438
*         /*MACDATE Y-2 73082*/                                    0439
*         SVC (99);                    /* ISSUE SVC 99               */
*                                                                  0439
         SVC   99                                                  0439
*        RCODESV = R15;                /* SAVE RETURN CODE           */
*                                                                  0440
         ST    R15,RCODESV(,R3AWAPTR)                              0440
*    END DYNSVC;                                                   0441
@EL00010 DS    0H                                                  0441
@EF00010 DS    0H                                                  0441
@ER00010 LM    R14,R12,12(R13)                                     0441
         BR    R14                                                 0441
*    END IKJEFD34                                                  0442
*/* THE FOLLOWING INCLUDE STATEMENTS WERE FOUND IN THIS PROGRAM.      *
*/*%INCLUDE SYSLIB  (IKJZT430)                                        *
*/*%INCLUDE SYSLIB  (IKJCPPL )                                        *
*/*%INCLUDE SYSLIB  (IKJPPL  )                                        *
*/*%INCLUDE SYSLIB  (IKJEFFMT)                                        *
*/*%INCLUDE SYSLIB  (IKJZB831)                                        *
*/*%INCLUDE SYSLIB  (IEFZB4D0)                                        *
*/*%INCLUDE SYSLIB  (IEFZB4D2)                                        *
*;                                                                 0442
@DATA    DC    0H'0'
@HW3     DC    H'3'
@HW4     DC    H'4'
@HW5     DC    H'5'
@HW8     DC    H'8'
@HW10    DC    H'10'
@HWDDNAM DC    XL2'0001'
@HWMEMBR DC    XL2'0003'
@HWPERMC DC    XL2'0004'
@HWUNALC DC    XL2'0007'
@HWPASSW DC    XL2'0050'
@HWPERMA DC    XL2'0052'
@HWRTDDN DC    XL2'0055'
@HWRTORG DC    XL2'0057'
@SM00888 MVC   DAPARM(0,R1),DATAVAL(R2DPTR2)
@SM00890 MVC   MBPARM(0,R15),MBRVAL(R7MPTR2)
@SM00892 MVC   PWPARM(0,R15),PSWVAL(R9PPTR2)
@SM00896 MVC   DDFVAL(0),DDNMPARM(R5DDLST)
@SM00900 MVC   DDNMPARM(0,R5DDLST),RTDDNAME(R10)
@SM00903 MVC   NDDNBUF1(0),DDNBUF1(R10)
@DATD    DSECT
         DS    0F
@SA00001 DS    18F
@PC00001 DS    1F
@SAV001  EQU   @SA00001
@SA00007 DS    15F
@SA00008 DS    15F
@SA00009 DS    14F
@SA00003 DS    14F
@SA00004 DS    14F
@SA00005 DS    14F
@SA00006 DS    14F
@TF00001 DS    F
@ZTEMPS  DS    C
@ZTEMPND EQU   *
@ZLEN    EQU   @ZTEMPND-@ZTEMPS
@TEMPS   EQU   @ZTEMPS
@L       EQU   @ZLEN
IKJEFD34 CSECT
         DC    0F'0'
@FW1     DC    F'1'
@HW1     EQU   @FW1+2
@FW2     DC    F'2'
@HW2     EQU   @FW2+2
@FW12    DC    F'12'
@FW14    DC    F'14'
@FW16    DC    F'16'
@DATD    DSECT
         DS    0D
S99RBPTR DS    AL4
         ORG   S99RBPTR
S99RBPND DS    BL1
         ORG   S99RBPTR+4
S99TUFP  DS    A
SAVPWLEN DS    H
         DS    CL2
CORE     DS    CL4
         ORG   CORE
SPNO     DS    CL1
SIZE     DS    AL3
         ORG   CORE+4
DDFREEBF DS    CL14
         ORG   DDFREEBF
DDFKEY   DS    FL2
DDFNUM   DS    FL2
DDFLEN   DS    FL2
DDFVAL   DS    CL8
         ORG   DDFREEBF+14
CNCTFLAG DS    CL1
         ORG   CNCTFLAG
STATSERR DS    BL1
ALLOCERR EQU   CNCTFLAG+0
MAXERR   EQU   CNCTFLAG+0
DSORGERR EQU   CNCTFLAG+0
INVORG   EQU   CNCTFLAG+0
@NM00011 EQU   CNCTFLAG+0
         ORG   CNCTFLAG+1
CNCTFLG1 DS    CL1
         ORG   CNCTFLG1
SEARCH   DS    BL1
DSORGSQ  EQU   CNCTFLG1+0
DSORGPR  EQU   CNCTFLG1+0
DSORGVM  EQU   CNCTFLG1+0
FREEFL   EQU   CNCTFLG1+0
@NM00021 EQU   CNCTFLG1+0
         ORG   CNCTFLG1+1
NDDNBUF1 DS    CL8
SAVEFLG  DS    CL1
         ORG   *+1-(*-@DATD)/(*-@DATD) ENSURE DSECT DATA
@ENDDATD EQU   *
@DATEND  EQU   *
IKJEFD34 CSECT
         DC    0F'0'
@SIZDATD DC    AL1(1)
         DC    AL3(@ENDDATD-@DATD)
@SIZ001  EQU   @SIZDATD
VCONSD35 DC    V(IKJEFD35)
VCONSD36 DC    V(IKJEFD36)
VCONSD37 DC    V(IKJEFD37)
         DC    0D'0'
@CHR107A DC    C'107A'
@CHR106A DC    C'106A'
@CHR110A DC    C'110A'
@CHR108A DC    C'108A'
@CHR109A DC    C'109A'
@HEX0410 DC    X'0410'
MOD      DC    CL3'MOD'
NEW      DC    CL3'NEW'
SYSOUT   DC    CL6'SYSOUT'
         DC    0F'0'
PATCH32  DC    25F'0'
CONCTEXT DC    X'12'
         DC    X'10'
         DC    X'14'
         DC    X'1E'
         DC    X'2E'
         DC    X'20'
         DC    X'22'
         DC    X'1A'
         DC    X'1C'
         DC    X'2A'
         DC    X'2C'
         DC    X'24'
         DC    X'0E'
         DC    X'26'
         DC    X'28'
         DC    X'18'
         DC    X'16'
         DC    X'30'                                               @26A
         DC    X'00'
         DC    0D'0'                   END OF CSECT                @26A
R0       EQU   00                      EQUATES FOR REGISTERS 0-15
R1       EQU   01
R2       EQU   02
R3       EQU   03
R4       EQU   04
R5       EQU   05
R6       EQU   06
R7       EQU   07
R8       EQU   08
R9       EQU   09
R10      EQU   10
R11      EQU   11
R12      EQU   12
R13      EQU   13
R14      EQU   14
R15      EQU   15
GTPLOUT1 EQU   R4
LISTNMBR EQU   R8
DSFREED  EQU   R7
R5DDLST  EQU   R5
DDNAPTR  EQU   R6
SAVMBLEN EQU   R10
R9PPTR2  EQU   R9
R7MPTR2  EQU   R7
R2DPTR2  EQU   R2
R8INC    EQU   R8
R5EXTRAN EQU   R5
R7SAVERC EQU   R7
R3AWAPTR EQU   R3
R2I      EQU   R2
REGSAVE  EQU   4
RETCODE  EQU   16
ALLOCWA  EQU   0
CPPLPTR  EQU   ALLOCWA
PTRPDL   EQU   ALLOCWA+4
GTPLPTR  EQU   ALLOCWA+8
SWITCH   EQU   ALLOCWA+12
RBCODE12 EQU   SWITCH
CONT     EQU   SWITCH
TERMOPT  EQU   SWITCH
ENTRYCD  EQU   ALLOCWA+14
TXT      EQU   ALLOCWA+16
PTRS99RB EQU   ALLOCWA+20
RCODESV  EQU   ALLOCWA+24
DSNCTR   EQU   ALLOCWA+30
PTRMSGS  EQU   ALLOCWA+32
VCFLAGS  EQU   ALLOCWA+36
PMPTPDL  EQU   ALLOCWA+40
MSGPTR   EQU   ALLOCWA+48
CMDTWO   EQU   ALLOCWA+60
COMMECB  EQU   ALLOCWA+160
PPLPTR   EQU   ALLOCWA+172
ALLOCPDL EQU   0
FILEPDE  EQU   ALLOCPDL+10
STATSPDE EQU   ALLOCPDL+12
DSNPDE   EQU   ALLOCPDL+48+4                                       @26C
DSNPTR   EQU   DSNPDE
DSNFLAGS EQU   DSNPDE+6
MBRPTR   EQU   DSNPDE+8
MBRFLAGS EQU   DSNPDE+14
PASSPTR  EQU   DSNPDE+16
PASSFLAG EQU   DSNPDE+22
DDNMEPDE EQU   ALLOCPDL+76+4                                       @26C
DDNPTR   EQU   DDNMEPDE
DDNLEN   EQU   DDNMEPDE+4
DDNFLAGS EQU   DDNMEPDE+6
CLASPDE  EQU   ALLOCPDL+84+4                                       @26C
CLASPTR  EQU   CLASPDE
CLASFLG  EQU   CLASPDE+6
SERPDE   EQU   ALLOCPDL+92+4                                       @26C
SERPTR   EQU   SERPDE
SERFLAGS EQU   SERPDE+6
BLKPDE   EQU   ALLOCPDL+104+4                                      @26C
BLKPTR   EQU   BLKPDE
BLKFLAGS EQU   BLKPDE+6
ABLKPDE  EQU   ALLOCPDL+112+4                                      @26C
ABLKPTR  EQU   ABLKPDE
ABLKFLG  EQU   ABLKPDE+6
SPACPDE  EQU   ALLOCPDL+120+4                                      @26C
SPACEPTR EQU   SPACPDE
SPACEFLG EQU   SPACPDE+6
SPACPDE2 EQU   ALLOCPDL+128+4                                      @26C
SPACPTR2 EQU   SPACPDE2
SPACFLG2 EQU   SPACPDE2+6
DIRECPDE EQU   ALLOCPDL+136+4                                      @26C
DIRPTR   EQU   DIRECPDE
DIRFLAGS EQU   DIRECPDE+6
ATTRPDE  EQU   ALLOCPDL+144+4                                      @26C
ATTRPTR  EQU   ATTRPDE
ATTRFLAG EQU   ATTRPDE+6
USIDPDE  EQU   ALLOCPDL+152+4                                      @26C
USIDPTR  EQU   USIDPDE
USIDFLAG EQU   USIDPDE+6
UTYPPDE  EQU   ALLOCPDL+160+4                                      @26C
UTYPPTR  EQU   UTYPPDE
UTYPFLAG EQU   UTYPPDE+6
UNCNTPDE EQU   ALLOCPDL+168+4                                      @26C
UCNTPTR  EQU   UNCNTPDE
UCNTFLAG EQU   UNCNTPDE+6
POSTPDE  EQU   ALLOCPDL+180+4                                      @26C
POSPTR   EQU   POSTPDE
POSFLAG  EQU   POSTPDE+6
MXVOLPDE EQU   ALLOCPDL+188+4                                      @26C
MXVOLPTR EQU   MXVOLPDE
MXVOLFLG EQU   MXVOLPDE+6
VOLSQPDE EQU   ALLOCPDL+196+4                                      @26C
VOLSQPTR EQU   VOLSQPDE
VOLSQFLG EQU   VOLSQPDE+6
MSVGPDE  EQU   ALLOCPDL+204+4                                      @26C
MSVGPTR  EQU   MSVGPDE
MSVGFLG  EQU   MSVGPDE+6
DSNBUF   EQU   0
CPPL     EQU   0
PPL      EQU   0
PDEEXT   EQU   0
MSGTABLE EQU   0
LISTPTR  EQU   MSGTABLE
TMCTPTR  EQU   MSGTABLE+4
MTCPPL   EQU   TMCTPTR
ECBPTR   EQU   MSGTABLE+8
@NM00005 EQU   MSGTABLE+12
MTHIGH   EQU   @NM00005
MSGCSECT EQU   MSGTABLE+16
SW       EQU   MSGTABLE+20
MTPUTLSW EQU   SW
RETMSG   EQU   MSGTABLE+21
SW2      EQU   MSGTABLE+24
MSGID    EQU   MSGTABLE+40
MTINSRTS EQU   MSGTABLE+44
L1       EQU   MTINSRTS
VAR1     EQU   MTINSRTS+1
L2       EQU   MTINSRTS+4
L3       EQU   MTINSRTS+8
L4       EQU   MTINSRTS+12
RET      EQU   0
TEXTCORE EQU   0
ADDNUM   EQU   TEXTCORE
ADDTEXT  EQU   TEXTCORE+4
ADLTEXT  EQU   0
ADLKEY   EQU   ADLTEXT
ADLNBR   EQU   ADLTEXT+2
ADLLEN   EQU   ADLTEXT+4
FILETXT  EQU   0
FILEKY   EQU   FILETXT
FILENUM  EQU   FILETXT+2
DSNTUNIT EQU   0
DANUM    EQU   DSNTUNIT+2
DALEN    EQU   DSNTUNIT+4
DAPARM   EQU   DSNTUNIT+6
DSUNIT   EQU   0
DATALEN  EQU   DSUNIT
DATAVAL  EQU   DSUNIT+2
MBRTUNIT EQU   0
MBKEY    EQU   MBRTUNIT
MBNUM    EQU   MBRTUNIT+2
MBLEN    EQU   MBRTUNIT+4
MBPARM   EQU   MBRTUNIT+6
MBUNIT   EQU   0
MEMLEN   EQU   MBUNIT
MBRVAL   EQU   MBUNIT+2
PSWTUNIT EQU   0
PWKEY    EQU   PSWTUNIT
PWNUM    EQU   PSWTUNIT+2
PWLEN    EQU   PSWTUNIT+4
PWPARM   EQU   PSWTUNIT+6
PWUNIT   EQU   0
PSWLEN   EQU   PWUNIT
PSWVAL   EQU   PWUNIT+2
DDNCAREA EQU   0
PERMCTPT EQU   DDNCAREA
DDNMPTR  EQU   DDNCAREA+4
DDNMEND  EQU   DDNMPTR
PERMCTXT EQU   DDNCAREA+8
PERMCTKY EQU   PERMCTXT
PERMCTNM EQU   PERMCTXT+2
DDNMTXT  EQU   DDNCAREA+12
DDNMKEY  EQU   DDNMTXT
DDNMNBR  EQU   DDNMTXT+2
DDNMLEN  EQU   DDNMTXT+4
DDNMLIST EQU   0
DDNMLNGH EQU   DDNMLIST
DDNMPARM EQU   DDNMLIST+2
DADSORG  EQU   0
RTDSORG  EQU   DADSORG+6
SEQUEN   EQU   RTDSORG
PARTND   EQU   RTDSORG
VSAM     EQU   RTDSORG+1
DADDNAME EQU   0
RTDDNLEN EQU   DADDNAME+4
RTDDNAME EQU   DADDNAME+6
DDPDE2   EQU   0
DDPTR2   EQU   DDPDE2+8
DDLNGH2  EQU   DDPDE2+12
DDFLAG2  EQU   DDPDE2+14
DDNBUF1  EQU   0
GTPL     EQU   0
GTPLABLK EQU   GTPL+16
GTPLOUTA EQU   GTPL+20
TEXTRET  EQU   0
TUDSN    EQU   TEXTRET+8
TUMBR    EQU   TEXTRET+12
TUPSWD   EQU   TEXTRET+16
TUFILE   EQU   TEXTRET+20
TURTDDN  EQU   TEXTRET+96+4                                        @26C
TURTDSO  EQU   TEXTRET+100+4                                       @26C
S99RB    EQU   0
S99VERB  EQU   S99RB+1
S99FLAG1 EQU   S99RB+2
S99FLG11 EQU   S99FLAG1
S99NOCNV EQU   S99FLG11
S99RSC   EQU   S99RB+4
S99ERROR EQU   S99RSC
S99TXTPP EQU   S99RB+8
S99FLAG2 EQU   S99RB+16
S99FLG21 EQU   S99FLAG2
S99FLG22 EQU   S99FLAG2+1
S99TUPL  EQU   0
S99TUPTR EQU   S99TUPL
S99TUP   EQU   0
S99TUNIT EQU   0
S99TUENT EQU   S99TUNIT+4
S99TUFLD EQU   0
ABLKBUF  EQU   0
ATTRBUF  EQU   0
BLKBUF   EQU   0
CLASSBUF EQU   0
DDNBUF   EQU   0
DIRBUF   EQU   0
MBRBUF   EQU   0
MSGECB   EQU   0
MSGTMCT  EQU   0
MSVGBUF  EQU   0
MXVOLBUF EQU   0
PASSBUF  EQU   0
POSTBUF  EQU   0
SERBUF   EQU   0
SPACBUF2 EQU   0
SPACEBUF EQU   0
UCNTBUF  EQU   0
USIDBUF  EQU   0
UTYPBUF  EQU   0
VSEQBUF  EQU   0
INPARMS  EQU   0
*                                      START UNREFERENCED COMPONENTS
S99TUPRM EQU   S99TUFLD+2
S99TULEN EQU   S99TUFLD
S99TUPAR EQU   S99TUENT+2
S99TULNG EQU   S99TUENT
S99TUNUM EQU   S99TUNIT+2
S99TUKEY EQU   S99TUNIT
S99TUPND EQU   S99TUP
S99TUPLN EQU   S99TUPTR
S99FLG24 EQU   S99FLAG2+3
S99FLG23 EQU   S99FLAG2+2
S99PCINT EQU   S99FLG22
S99UDEVT EQU   S99FLG22
S99MOUNT EQU   S99FLG21
S99CATLG EQU   S99FLG21
S99TIONQ EQU   S99FLG21
S99OFFLN EQU   S99FLG21
S99WTUNT EQU   S99FLG21
S99NORES EQU   S99FLG21
S99WTDSN EQU   S99FLG21
S99WTVOL EQU   S99FLG21
S99RSV01 EQU   S99RB+12
S99INFO  EQU   S99RSC+2
S99FLG12 EQU   S99FLAG1+1
S99JBSYS EQU   S99FLG11
S99NOMNT EQU   S99FLG11
S99ONCNV EQU   S99FLG11
S99RBLN  EQU   S99RB
TUDISP   EQU   TEXTRET+92
TURND    EQU   TEXTRET+88
TURLSE   EQU   TEXTRET+84
TUVSEQ   EQU   TEXTRET+80
TUPRIV   EQU   TEXTRET+76
TUMAXV   EQU   TEXTRET+72
TUPOS    EQU   TEXTRET+68
TULABEL  EQU   TEXTRET+64
TUUCNT   EQU   TEXTRET+60
TUUNIT   EQU   TEXTRET+56
TUHOLD   EQU   TEXTRET+52
TUDEST   EQU   TEXTRET+48
TUUSING  EQU   TEXTRET+44
TUDIR    EQU   TEXTRET+40
TUSPACE  EQU   TEXTRET+36
TUBLOK   EQU   TEXTRET+32
TUVOL    EQU   TEXTRET+28
TUSTATS  EQU   TEXTRET+24
TUPSWDND EQU   TUPSWD
ENTRYNBR EQU   TEXTRET+4
SBPLSIZE EQU   TEXTRET
GTPLTBLE EQU   GTPL+12
GTPLKLST EQU   GTPL+8
GTPLPCL  EQU   GTPL+4
GTPLPDL  EQU   GTPL
@NM00024 EQU   DDPDE2+15
@NM00023 EQU   DDPDE2+4
@NM00022 EQU   DDPDE2
@NM00020 EQU   DADDNAME+2
@NM00019 EQU   DADDNAME
@NM00018 EQU   RTDSORG+1
@NM00017 EQU   RTDSORG
@NM00016 EQU   RTDSORG
@NM00015 EQU   RTDSORG
@NM00014 EQU   DADSORG+4
@NM00013 EQU   DADSORG+2
@NM00012 EQU   DADSORG
DDNMVAL  EQU   DDNMTXT+6
DAKEY    EQU   DSNTUNIT
ADLPARM  EQU   ADLTEXT+6
ADDUNIT  EQU   TEXTCORE+36
@NM00010 EQU   TEXTCORE+2
RETCHAR  EQU   RET+2
RETSIZE  EQU   RET
MSGRTN   EQU   MSGTABLE+60
VAR4     EQU   MTINSRTS+13
HIGHL4   EQU   L4
VAR3     EQU   MTINSRTS+9
HIGHL3   EQU   L3
VAR2     EQU   MTINSRTS+5
HIGHL2   EQU   L2
HIGHL1   EQU   L1
@NM00009 EQU   MSGTABLE+36
@NM00008 EQU   MSGTABLE+32
MTOLDPTR EQU   MSGTABLE+28
@NM00007 EQU   MSGTABLE+25
@NM00006 EQU   SW2
MTPGMSW  EQU   SW2
MTNPLMSW EQU   SW2
MTNOXQSW EQU   SW2
MTDOMSW  EQU   SW2
MT2OLDSW EQU   SW2
MTNHEXSW EQU   SW
MTWTPSW  EQU   SW
MTJOBISW EQU   SW
MTKEY1SW EQU   SW
MTHEXSW  EQU   SW
MTWTOSW  EQU   SW
MTNOIDSW EQU   SW
PPLUWA   EQU   PPL+24
PPLCBUF  EQU   PPL+20
PPLANS   EQU   PPL+16
PPLPCL   EQU   PPL+12
PPLECB   EQU   PPL+8
PPLECT   EQU   PPL+4
PPLUPT   EQU   PPL
CPPLECT  EQU   CPPL+12
CPPLPSCB EQU   CPPL+8
CPPLUPT  EQU   CPPL+4
CPPLCBUF EQU   CPPL
DSNTERM  EQU   DSNBUF
MSVGRSV2 EQU   MSVGPDE+7
MSVGRSV1 EQU   MSVGFLG
MSVGPRES EQU   MSVGFLG
MSVGLEN  EQU   MSVGPDE+4
VOLRSV2  EQU   VOLSQPDE+7
VOLRSV1  EQU   VOLSQFLG
VOLPRES  EQU   VOLSQFLG
VOLSQLEN EQU   VOLSQPDE+4
MXVRSV2  EQU   MXVOLPDE+7
MXVRSV1  EQU   MXVOLFLG
MXVPRES  EQU   MXVOLFLG
MXVOLLEN EQU   MXVOLPDE+4
POSRSV2  EQU   POSTPDE+7
POSRSV1  EQU   POSFLAG
POSPRES  EQU   POSFLAG
POSLEN   EQU   POSTPDE+4
LABLRSV1 EQU   ALLOCPDL+178+4                                      @26C
LABELPDE EQU   ALLOCPDL+176+4                                      @26C
UCNTRSV2 EQU   UNCNTPDE+7
UCNTRSV1 EQU   UCNTFLAG
UCNTPRES EQU   UCNTFLAG
UCNTLEN  EQU   UNCNTPDE+4
UTYPRSV2 EQU   UTYPPDE+7
UTYPRSV1 EQU   UTYPFLAG
UTYPPRES EQU   UTYPFLAG
UTYPLEN  EQU   UTYPPDE+4
USIDRSV2 EQU   USIDPDE+7
USIDRSV1 EQU   USIDFLAG
USIDPRES EQU   USIDFLAG
USIDLEN  EQU   USIDPDE+4
ATTRRSV2 EQU   ATTRPDE+7
ATTRRSV1 EQU   ATTRFLAG
ATTRPRES EQU   ATTRFLAG
ATTRLEN  EQU   ATTRPDE+4
DIRRSV2  EQU   DIRECPDE+7
DIRRSV1  EQU   DIRFLAGS
DIRPRES  EQU   DIRFLAGS
DIRLEN   EQU   DIRECPDE+4
SPCRSV22 EQU   SPACPDE2+7
SPCRSV12 EQU   SPACFLG2
SPCPRES2 EQU   SPACFLG2
SPACLEN2 EQU   SPACPDE2+4
SPACRSV2 EQU   SPACPDE+7
SPACRSV1 EQU   SPACEFLG
SPACPRES EQU   SPACEFLG
SPACELEN EQU   SPACPDE+4
ABLKRSV2 EQU   ABLKPDE+7
ABLKRSV1 EQU   ABLKFLG
ABLKPRES EQU   ABLKFLG
ABLKLEN  EQU   ABLKPDE+4
BLKRSV2  EQU   BLKPDE+7
BLKRSV1  EQU   BLKFLAGS
BLKPRES  EQU   BLKFLAGS
BLKLEN   EQU   BLKPDE+4
SERNEXT  EQU   SERPDE+8
SERRSV2  EQU   SERPDE+7
SERRSV1  EQU   SERFLAGS
SERPRES  EQU   SERFLAGS
SERLEN   EQU   SERPDE+4
CLASRSV2 EQU   CLASPDE+7
CLASRSV1 EQU   CLASFLG
CLASPRES EQU   CLASFLG
CLASLEN  EQU   CLASPDE+4
DDNRSV2  EQU   DDNMEPDE+7
DDNRSV1  EQU   DDNFLAGS
DDNPRES  EQU   DDNFLAGS
DSNNEXT  EQU   DSNPDE+24
PASSRSV2 EQU   DSNPDE+23
PASSRSV1 EQU   PASSFLAG
PASSPRES EQU   PASSFLAG
PASSLEN  EQU   DSNPDE+20
MBRRSV2  EQU   DSNPDE+15
MBRRSV1  EQU   MBRFLAGS
MBRPRES  EQU   MBRFLAGS
MBRLEN   EQU   DSNPDE+12
DSNRSV2  EQU   DSNPDE+7
DSNRSV1  EQU   DSNFLAGS
DSNQUOT  EQU   DSNFLAGS
DSNPRES  EQU   DSNFLAGS
DSNLENGH EQU   DSNPDE+4
DISPPDE  EQU   ALLOCPDL+46
RNDPDE   EQU   ALLOCPDL+44
RLSEPDE  EQU   ALLOCPDL+42
VSEQPDE  EQU   ALLOCPDL+40
PRIVPDE  EQU   ALLOCPDL+38
MAXVPDE  EQU   ALLOCPDL+36
POSPDE   EQU   ALLOCPDL+34
LABLPDE  EQU   ALLOCPDL+32
UCNTPDE  EQU   ALLOCPDL+30
UNITPDE  EQU   ALLOCPDL+28
HOLDPDE  EQU   ALLOCPDL+26
DESTPDE  EQU   ALLOCPDL+24
USINGPDE EQU   ALLOCPDL+22
DIRPDE   EQU   ALLOCPDL+20
SPACEPDE EQU   ALLOCPDL+18
BLOKPDE  EQU   ALLOCPDL+16
VOLPDE   EQU   ALLOCPDL+14
DSPDE    EQU   ALLOCPDL+8
@NM00004 EQU   ALLOCPDL+4
@NM00003 EQU   ALLOCPDL
FIRSTPTR EQU   ALLOCWA+176
NBLKBUF1 EQU   ALLOCWA+164
PDLADDR3 EQU   ALLOCWA+156
PPLTWO   EQU   ALLOCWA+128
CMDTWOBF EQU   CMDTWO+4
CMDTWOOF EQU   CMDTWO+2
CMDTWOLN EQU   CMDTWO
Q        EQU   ALLOCWA+56
P        EQU   ALLOCWA+52
PRCODE   EQU   ALLOCWA+44
@NM00002 EQU   ALLOCWA+38
NCLASBUF EQU   ALLOCWA+37
@NM00001 EQU   VCFLAGS
SPACE2   EQU   VCFLAGS
SPACE1   EQU   VCFLAGS
AVBLK    EQU   VCFLAGS
VLISTCTR EQU   ALLOCWA+28
SWITCH2  EQU   ALLOCWA+13                                          @26C
FIRSTPDE EQU   SWITCH
AWARSV1  EQU   SWITCH
ASTRSK   EQU   SWITCH
PROMPT   EQU   SWITCH
DSOVRLAY EQU   SWITCH
*                                      END UNREFERENCED COMPONENTS
@RC00142 EQU   @EL00001
@RC00198 EQU   @RC00195
@RF00220 EQU   @RC00195
@RC00224 EQU   @RC00195
@RC00285 EQU   @RC00279
@RF00301 EQU   @EL00005
@RC00309 EQU   @EL00005
@RC00321 EQU   @EL00005
@RC00317 EQU   @EL00005
@RC00315 EQU   @EL00005
@RC00331 EQU   @EL00006
@RF00343 EQU   @EL00006
@RC00398 EQU   @EL00008
@RC00424 EQU   @EL00009
@RC00318 EQU   @RC00317
@EL01    EQU   @EL00001
@ENDDATA EQU   *
         END   IKJEFD34,(C'PLS1704',0701,75248)
/*
//*
//STEP11  EXEC PGM=IEBGENER
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  *
  IDENTIFY IKJEFD34('ZP60026')
++MOD(IKJEFD35) DISTLIB(AOST4).
/*
//SYSUT2   DD  DSN=&&SMPMCS,DISP=(MOD,PASS)
//SYSIN    DD  DUMMY
//*
//STEP12  EXEC PGM=IFOX00,PARM='OBJECT,NODECK,NOTERM,XREF(SHORT)'
//SYSPRINT DD  SYSOUT=*
//SYSLIB   DD  DSN=SYS1.MACLIB,DISP=SHR
//         DD  DSN=SYS1.SMPMTS,DISP=SHR
//         DD  DSN=SYS1.AMODGEN,DISP=SHR
//SYSUT1   DD  UNIT=SYSALLDA,SPACE=(CYL,10)
//SYSUT2   DD  UNIT=SYSALLDA,SPACE=(CYL,10)
//SYSUT3   DD  UNIT=SYSALLDA,SPACE=(CYL,10)
//SYSGO    DD  DSN=&&SMPMCS,DISP=(MOD,PASS)
//SYSIN    DD  *
         TITLE 'ALLOCATE ERROR HANDLING ROUTINE                        *
                        '
***********************************************************************
*  $26   ZP60026  2010-01-23 GREG PRICE: ADD 'REUSE' OPERAND.         *
*                            CHANGES APPLIED TO EBB1102 LEVEL.        *
***********************************************************************
IKJEFD35 CSECT ,                                                   0001
@MAINENT DS    0H                                                  0001
         USING *,R15                                               0001
         B     @PROLOG                                             0001
         DC    AL1(33)                                             @26C
         DC    C'IKJEFD35  74.204'                                 0001
         DC    CL17' ZP60026 2010.023'                             @26A
         DROP  R15
@PROLOG  STM   R14,R12,12(R13)                                     0001
         BALR  R12,0                                               0001
@PSTART  DS    0H                                                  0001
         USING @PSTART,R12                                         0001
         L     R0,@SIZDATD                                         0001
         GETMAIN  R,LV=(0)
         LR    R11,R1                                              0001
         USING @DATD,R11                                           0001
         ST    R13,@SA00001+4                                      0001
         LM    R0,R1,20(R13)                                       0001
         ST    R11,8(,R13)                                         0001
         LR    R13,R11                                             0001
         XC    @ZTEMPS(@ZLEN),@ZTEMPS
         MVC   @PC00001(4),0(R1)                                   0001
*        AWAPTR = R1;                       /* PTR TO WORK AREA      */
         LR    R3AWAPTR,R1                                         0043
*        MSGTABLE = MSGTABLE && MSGTABLE;   /* CLEAR MSG AREA        */
         L     R10,MSGPTR(,R3AWAPTR)                               0044
         XC    MSGTABLE(64,R10),MSGTABLE(R10)                      0044
*        IF ENTRYCD = 1 THEN                /* IF DYNALLOC ERROR     */
         CLC   ENTRYCD(2,R3AWAPTR),@HW1                            0045
         BNE   @RF00045                                            0045
*          CALL DFAILRTN;                   /* INVOKE MSG ROUTINE    */
         BAL   R14,DFAILRTN                                        0046
*        ELSE                               /* ELSE,                 */
*          IF ENTRYCD = 2 THEN              /* IF PARSE ERROR        */
         B     @RC00045                                            0047
@RF00045 CLC   ENTRYCD(2,R3AWAPTR),@HW2                            0047
         BNE   @RF00047                                            0047
*            CALL PARSERR;                  /* INVOKE PARSE MSG RTN  */
         BAL   R14,PARSERR                                         0048
*          ELSE                             /* ELSE,                 */
*            IF ENTRYCD = 3 |               /* IF GENTRANS OR        */
*               ENTRYCD = 4 THEN            /* CMDSCAN ERROR         */
         B     @RC00047                                            0049
@RF00047 LH    R10,ENTRYCD(,R3AWAPTR)                              0049
         C     R10,@FW3                                            0049
         BE    @RT00049                                            0049
         C     R10,@FW4                                            0049
         BNE   @RF00049                                            0049
@RT00049 DS    0H                                                  0050
*              CALL SYSERR;                 /* INVOKE SYSTEM MSG RTN */
         BAL   R14,SYSERR                                          0050
*            ELSE                           /* ELSE,                 */
*              IF ENTRYCD = 5 THEN          /* IF DISPOSITION ERROR  */
         B     @RC00049                                            0051
@RF00049 CLC   ENTRYCD(2,R3AWAPTR),@HW5                            0051
         BNE   @RF00051                                            0051
*                CALL DISPERR;              /* INVOKE DISP MSG RTN   */
*                                                                  0052
         BAL   R14,DISPERR                                         0052
*        IF RBCODE12 = '1'B  THEN           /* IF ERROR OCCURRED,    */
@RF00051 DS    0H                                                  0053
@RC00049 DS    0H                                                  0053
@RC00047 DS    0H                                                  0053
@RC00045 TM    RBCODE12(R3AWAPTR),B'10000000'                      0053
         BNO   @RF00053                                            0053
*          RETCODE = 12;                    /* SET RETURN CODE TO 12 */
         L     R10,REGSAVE(,R13)                                   0054
         MVC   RETCODE(4,R10),@FW12                                0054
*        ELSE                               /* ELSE                  */
*          RETCODE = 0;                     /* RETURN CODE IS 0      */
*                                                                  0055
         B     @RC00053                                            0055
@RF00053 L     R10,REGSAVE(,R13)                                   0055
         SLR   R15,R15                                             0055
         ST    R15,RETCODE(,R10)                                   0055
*        RETURN;                                                   0056
@EL00001 L     R13,4(,R13)                                         0056
@EF00001 L     R0,@SIZDATD                                         0056
         LR    R1,R11                                              0056
         FREEMAIN R,LV=(0),A=(1)
@ER00001 LM    R14,R12,12(R13)                                     0056
         BR    R14                                                 0056
*/************** START OF SPECIFICATIONS *****                     0057
**                                                                 0057
** SEGMENT NAME -                                                  0057
**      SYSERR                                                     0057
**                                                                 0057
** FUNCTION -                                                      0057
**      THIS ROUTINE ISSUES THE MESSAGE NOTIFYING THE USER OF A    0057
**      COMMAND SYSTEM ERROR.                                      0057
**                                                                 0057
** OPERATION -                                                     0057
**      SET UP THE MESSAGE SEGMENTS. INVOKE IKJEFF02 TO ISSUE      0057
**      THE MESSAGE.                                               0057
**                                                                 0057
** INPUT -                                                         0057
**      RETURN CODE                                                0057
**                                                                 0057
** OUTPUT -                                                        0057
**      MESSAGE ISSUED                                             0057
**                                                                 0057
*******************            END OF SPECIFICATIONS ******/       0057
*                                                                  0057
*    SYSERR: PROC;                                                 0057
*                                                                  0057
         B     @EL00001                                            0057
SYSERR   STM   R14,R12,@SA00002                                    0057
*        MSGID = '102A';                    /* SET MESSAGE ID        */
         L     R10,MSGPTR(,R3AWAPTR)                               0058
         MVC   MSGID(4,R10),@CC00457                               0058
*        IF ENTRYCD = 2 THEN                /* IF PARSE ERROR        */
         CLC   ENTRYCD(2,R3AWAPTR),@HW2                            0059
         BNE   @RF00059                                            0059
*          DO;                              /* THEN,                 */
*            VAR1 = ADDR(PARS);             /* PUT PARSE VARIABLE    */
         LA    R2,PARS                                             0061
         ST    R2,@TF00001                                         0061
         MVC   VAR1(3,R10),@TF00001+1                              0061
*            L1 = LENGTH(PARS);             /* AND LENGTH IN LIST    */
         MVI   L1(R10),X'05'                                       0062
*          END;                                                    0063
*        IF ENTRYCD = 3 THEN                /* IF GENTRANS ERROR     */
@RF00059 CLC   ENTRYCD(2,R3AWAPTR),@HW3                            0064
         BNE   @RF00064                                            0064
*          DO;                              /* THEN                  */
*            VAR1 = ADDR(GTRANS);           /* PUT GENTRANS VARIABLE */
         L     R10,MSGPTR(,R3AWAPTR)                               0066
         LA    R2,GTRANS                                           0066
         ST    R2,@TF00001                                         0066
         MVC   VAR1(3,R10),@TF00001+1                              0066
*            L1 = LENGTH(GTRANS);           /* AND LENGTH IN LIST    */
         MVI   L1(R10),X'08'                                       0067
*          END;                                                    0068
*        IF ENTRYCD = 4 THEN                /* IF CMD SCAN ERROR     */
@RF00064 CLC   ENTRYCD(2,R3AWAPTR),@HW4                            0069
         BNE   @RF00069                                            0069
*          DO;                              /* THEN,                 */
*            VAR1 = ADDR(CMDSCAN);          /* PUT CMD SCAN VARIABLE */
         L     R10,MSGPTR(,R3AWAPTR)                               0071
         LA    R2,CMDSCAN                                          0071
         ST    R2,@TF00001                                         0071
         MVC   VAR1(3,R10),@TF00001+1                              0071
*            L1 = LENGTH(CMDSCAN);          /* AND LENGTH IN LIST    */
         MVI   L1(R10),X'0C'                                       0072
*          END;                                                    0073
*                                                                  0073
*        VAR2 = ADDR(RCODESV);              /* PUT RETURN CODE AND   */
@RF00069 L     R10,MSGPTR(,R3AWAPTR)                               0074
         LA    R2,RCODESV(,R3AWAPTR)                               0074
         ST    R2,@TF00001                                         0074
         MVC   VAR2(3,R10),@TF00001+1                              0074
*        L2 = 4;                            /* LENGTH IN LIST        */
         MVI   L2(R10),X'04'                                       0075
*        HIGHL2 = '1'B;                     /* INDICATE CONVERT HEX  */
         OI    HIGHL2(R10),B'10000000'                             0076
*        CALL PUTMSG;                       /* ISSUE MESSAGE         */
         BAL   R14,PUTMSG                                          0077
*        RBCODE12 = '1'B;                   /* INDICATE ALLOC FAILED */
*                                                                  0078
         OI    RBCODE12(R3AWAPTR),B'10000000'                      0078
*    END SYSERR;                                                   0079
@EL00002 DS    0H                                                  0079
@EF00002 DS    0H                                                  0079
@ER00002 LM    R14,R12,@SA00002                                    0079
         BR    R14                                                 0079
*/****************** START OF SPECIFICATIONS     *******           0080
**                                                                 0080
** SEGMENT NAME -                                                  0080
**      PARSERR                                                    0080
**                                                                 0080
** FUNCTION -                                                      0080
**      THIS ROUTINE ISSUES A DIAGNOSTIC MESSAGE WHEN PARSE        0080
**      SERVICE ROUTINE HAS FAILED.                                0080
**                                                                 0080
** OPERATION -                                                     0080
**      IF THE RETURN CODE IS 4, DO NOT ISSUE A MESSAGE. IF THE    0080
**      RETURN CODE IS 16, ISSUE MESSAGE INDICATING NOT ENOUGH     0080
**      STORAGE WAS AVAILABLE.  FOR ANY OTHER RETURN CODE, INVOKE  0080
**      ROUTINE SYSERR.                                            0080
**                                                                 0080
** INPUT -                                                         0080
**      PARSE RETURN CODE                                          0080
**                                                                 0080
** OUTPUT -                                                        0080
**      MESSAGE ISSUED                                             0080
**                                                                 0080
*****************************   END OF SPECIFICATIONS  ******/     0080
*                                                                  0080
*    PARSERR: PROC;                                                0080
*                                                                  0080
PARSERR  STM   R14,R12,@SA00003                                    0080
*        IF RCODESV = 4 THEN                /* IF NO PROMPT MODE,    */
         CLC   RCODESV(4,R3AWAPTR),@FW4                            0081
         BNE   @RF00081                                            0081
*          RBCODE12 = '1'B;                 /* INDICATE ALLOC FAILED */
         OI    RBCODE12(R3AWAPTR),B'10000000'                      0082
*        ELSE                                                      0083
*          IF RCODESV = 16 THEN             /* IF NOT ENOUGH STORAGE */
         B     @RC00081                                            0083
@RF00081 CLC   RCODESV(4,R3AWAPTR),@FW16                           0083
         BNE   @RF00083                                            0083
*            DO;                            /* SET UP MSG -          */
*              RBCODE12 = '1'B;             /* INDICATE ALLOC FAILED */
         OI    RBCODE12(R3AWAPTR),B'10000000'                      0085
*              MSGID = '100A';              /* SET MESSAGE ID        */
         L     R10,MSGPTR(,R3AWAPTR)                               0086
         MVC   MSGID(4,R10),@CC00465                               0086
*              CALL PUTMSG;                 /* ISSUE MESSAGE         */
         BAL   R14,PUTMSG                                          0087
*            END;                                                  0088
*          ELSE                             /* OTHER RETURN CODES -  */
*            CALL SYSERR;                   /* ISSUE SYSTEM ERR MSG  */
*                                                                  0089
         B     @RC00083                                            0089
@RF00083 BAL   R14,SYSERR                                          0089
*    END PARSERR;                                                  0090
@EL00003 DS    0H                                                  0090
@EF00003 DS    0H                                                  0090
@ER00003 LM    R14,R12,@SA00003                                    0090
         BR    R14                                                 0090
*/*******************    START OF SPECIFICATIONS   ****            0091
**                                                                 0091
** SEGMENT NAME -                                                  0091
**      DISPERR                                                    0091
**                                                                 0091
** FUNCTION -                                                      0091
**      THIS ROUTINE ISSUES AN ERROR MESSAGE IF A DISPOSITION OF   0091
**      DELETE HAS BEEN SPECIFIED FOR A MEMBER OF A PARTITIONED    0091
**      DATA SET.                                                  0091
**                                                                 0091
** OPERATION -                                                     0091
**      SET UP MESSAGE TO BE ISSUED. GET DATA SET NAME.  INVOKE    0091
**      IKJEFF02 TO ISSUE MESSAGE.                                 0091
**                                                                 0091
** INPUT -                                                         0091
**      PTR TO DSNAME                                              0091
**                                                                 0091
** OUTPUT -                                                        0091
**      MESSAGE ISSUED                                             0091
**                                                                 0091
*****************              END OF SPECIFICATIONS     ****/     0091
*                                                                  0091
*    DISPERR: PROC;                                                0091
*                                                                  0091
DISPERR  STM   R14,R12,@SA00004                                    0091
*        MSGID = '118A';                    /* SET MESSAGE ID        */
         L     R10,MSGPTR(,R3AWAPTR)                               0092
         MVC   MSGID(4,R10),@CC00469                               0092
*        VAR1 = DSNPTR;                     /* PUT DSNAME VARIABLE   */
         L     R2,PTRPDL(,R3AWAPTR)                                0093
         MVC   VAR1(3,R10),DSNPTR+1(R2)                            0093
*        L1 = DSNLENGH;                     /* AND LENGTH IN LIST    */
*                                                                  0094
         MVC   L1(1,R10),DSNLENGH+1(R2)                            0094
*        CALL PUTMSG;                       /* ISSUE MESSAGE         */
         BAL   R14,PUTMSG                                          0095
*        RBCODE12 = '1'B;                   /* INDICATE ALLOC FAILED */
*                                                                  0096
         OI    RBCODE12(R3AWAPTR),B'10000000'                      0096
*    END DISPERR;                                                  0097
@EL00004 DS    0H                                                  0097
@EF00004 DS    0H                                                  0097
@ER00004 LM    R14,R12,@SA00004                                    0097
         BR    R14                                                 0097
*/************** START OF SPECIFICATIONS   ****                    0098
**                                                                 0098
** SEGMENT NAME -                                                  0098
**      MSGRTN                                                     0098
**                                                                 0098
** FUNCTION -                                                      0098
**      THIS ROUTINE INVOKES DAIRFAIL TO ISSUE AN ERROR MESSAGE.   0098
**                                                                 0098
** OPERATION -                                                     0098
**      DAIRFAIL IS LOADED AND CALLED. BY ANALYZING DATA IN THE    0098
**      REQUEST BLOCK, DAIRFAIL DETERMINES THE APPROPRIATE MESSAGE 0098
**      TO ISSUE TO THE USER.                                      0098
**                                                                 0098
** INPUT -                                                         0098
**      WORKAREA                                                   0098
**                                                                 0098
** OUTPUT -                                                        0098
**      MESSAGE ISSUED                                             0098
**                                                                 0098
********************     END OF SPECIFICATIONS  ****/              0098
*                                                                  0098
*    DFAILRTN: PROC;                                               0098
*                                                                  0098
DFAILRTN STM   R14,R12,@SA00005                                    0098
*        GEN (LOAD EP=IKJEFF18);            /* LOAD DAIRFAIL         */
*                                                                  0099
         LOAD EP=IKJEFF18
*        CALL R0                            /* CALL DAIRFAIL,PASSING */
*            (S99RB,                        /* REQUEST BLOCK         */
*             RCODESV,                      /* RETURN CODE           */
*             ADMSG,                        /* ADDR OF MSG RTN = 0   */
*             CALLERNO,                     /* CALLER ID = 50        */
*             CPPL);                        /* CPPL POINTER          */
*                                                                  0100
         L     R1,PTRS99RB(,R3AWAPTR)                              0100
         ST    R1,@AL00001                                         0100
         LA    R10,RCODESV(,R3AWAPTR)                              0100
         ST    R10,@AL00001+4                                      0100
         LA    R10,ADMSG                                           0100
         ST    R10,@AL00001+8                                      0100
         LA    R10,CALLERNO                                        0100
         ST    R10,@AL00001+12                                     0100
         L     R1,CPPLPTR(,R3AWAPTR)                               0100
         ST    R1,@AL00001+16                                      0100
         LR    R15,R0                                              0100
         LA    R1,@AL00001                                         0100
         BALR  R14,R15                                             0100
*        GEN (DELETE EP=IKJEFF18);          /* DELETE DAIRFAIL       */
*                                                                  0101
         DELETE EP=IKJEFF18
*    END DFAILRTN;                                                 0102
@EL00005 DS    0H                                                  0102
@EF00005 DS    0H                                                  0102
@ER00005 LM    R14,R12,@SA00005                                    0102
         BR    R14                                                 0102
*/***************     START OF SPECIFICATIONS *****                0103
**                                                                 0103
** SEGMENT NAME -                                                  0103
**      PUTMSG                                                     0103
**                                                                 0103
** FUNCTION -                                                      0103
**      THIS ROUTINE INVOKES IKJEFF02 TO ISSUE MESSAGES            0103
**                                                                 0103
** OPERATION -                                                     0103
**      IKJEFF02 IS LOADED. THE PARAMETER LIST IS COMPLETED. CALL  0103
**      IKJEFF02, AND SAVE THE RETURN CODE. DELETE IKJEFF02.       0103
**                                                                 0103
** INPUT -                                                         0103
**      IKJEFF02 PARAMETER LIST                                    0103
**                                                                 0103
** OUTPUT -                                                        0103
**      MESSAGE ISSUED                                             0103
**                                                                 0103
**********************       END OF SPECIFICATIONS *****/          0103
*                                                                  0103
*    PUTMSG: PROC;                                                 0103
*                                                                  0103
PUTMSG   STM   R14,R12,@SA00006                                    0103
*        LISTPTR = ADDR(MSGCSECT);          /* PTR TO MSG PARMS      */
         L     R10,MSGPTR(,R3AWAPTR)                               0104
         LA    R2,MSGCSECT(,R10)                                   0104
         ST    R2,LISTPTR(,R10)                                    0104
*        MTCPPL = CPPLPTR;                  /* PTR TO CPPL           */
         L     R2,CPPLPTR(,R3AWAPTR)                               0105
         ST    R2,MTCPPL(,R10)                                     0105
*        ECBPTR = ADDR(COMMECB);            /* PTR TO ECB            */
         LA    R2,COMMECB(,R3AWAPTR)                               0106
         ST    R2,ECBPTR(,R10)                                     0106
*        COMMECB = 0;                       /* INIT ECB TO 0         */
         SLR   R2,R2                                               0107
         ST    R2,COMMECB(,R3AWAPTR)                               0107
*        MTHIGH = '1'B;                     /* INDICATE END OF LIST  */
         OI    MTHIGH(R10),B'10000000'                             0108
*        MTPUTLSW = '1'B;                   /* INDICATE PUTLINE      */
         OI    MTPUTLSW(R10),B'01000000'                           0109
*        MSGCSECT = PTRMSGS;                /* PTR TO MSG CSECT      */
*                                                                  0110
         L     R2,PTRMSGS(,R3AWAPTR)                               0110
         ST    R2,MSGCSECT(,R10)                                   0110
*        R1 = ADDR(MSGTABLE);               /* MSG PARMS IN REG 1    */
         LR    R1,R10                                              0111
*        GEN (LINK EP=IKJEFF02);            /* INVOKE MSG PROCESSOR  */
         LINK EP=IKJEFF02
*        RCODESV = R15;                     /* SAVE RETURN CODE      */
*                                                                  0113
         ST    R15,RCODESV(,R3AWAPTR)                              0113
*        IF RCODESV > 0 THEN                /* IF NON-ZERO CODE      */
         L     R10,RCODESV(,R3AWAPTR)                              0114
         LTR   R10,R10                                             0114
         BNP   @RF00114                                            0114
*          RBCODE12 = '1'B;                 /* INDICATE ALLOC FAILED */
*                                                                  0115
         OI    RBCODE12(R3AWAPTR),B'10000000'                      0115
*    END PUTMSG;                                                   0116
*                                                                  0116
@EL00006 DS    0H                                                  0116
@EF00006 DS    0H                                                  0116
@ER00006 LM    R14,R12,@SA00006                                    0116
         BR    R14                                                 0116
*    END IKJEFD35                                                  0117
*/* THE FOLLOWING INCLUDE STATEMENTS WERE FOUND IN THIS PROGRAM.      *
*/*%INCLUDE SYSLIB  (IKJZT430)                                        *
*/*%INCLUDE SYSLIB  (IKJEFFMT)                                        *
*/*%INCLUDE SYSLIB  (IKJCPPL )                                        *
*/*%INCLUDE SYSLIB  (IEFZB4D0)                                        *
*;                                                                 0117
@DATA    DC    0H'0'
@HW1     DC    H'1'
@HW2     DC    H'2'
@HW5     DC    H'5'
@DATD    DSECT
         DS    0F
@SA00001 DS    18F
@PC00001 DS    1F
@SAV001  EQU   @SA00001
@SA00005 DS    15F
@SA00003 DS    15F
@SA00002 DS    15F
@SA00004 DS    15F
@SA00006 DS    15F
@AL00001 DS    5A
@TF00001 DS    F
@ZTEMPS  DS    C
@ZTEMPND EQU   *
@ZLEN    EQU   @ZTEMPND-@ZTEMPS
@TEMPS   EQU   @ZTEMPS
@L       EQU   @ZLEN
IKJEFD35 CSECT
         DC    0F'0'
@FW3     DC    F'3'
@HW3     EQU   @FW3+2
@FW4     DC    F'4'
@HW4     EQU   @FW4+2
@FW12    DC    F'12'
@FW16    DC    F'16'
@DATD    DSECT
         DS    0D
S99RBPTR DS    AL4
         ORG   S99RBPTR
S99RBPND DS    BL1
         ORG   S99RBPTR+4
S99TUFP  DS    A
         ORG   *+1-(*-@DATD)/(*-@DATD) ENSURE DSECT DATA
@ENDDATD EQU   *
@DATEND  EQU   *
IKJEFD35 CSECT
         DC    0F'0'
@SIZDATD DC    AL1(1)
         DC    AL3(@ENDDATD-@DATD)
@SIZ001  EQU   @SIZDATD
         DC    0D'0'
ADMSG    DC    F'0'
CALLERNO DC    H'50'
@CC00457 DC    C'102A'
@CC00465 DC    C'100A'
@CC00469 DC    C'118A'
PARS     DC    CL5'PARSE'
GTRANS   DC    CL8'GENTRANS'
CMDSCAN  DC    CL13'COMMAND SCAN '
PATCH35  DC    25F'0'
         DC    0D'0'                   END OF CSECT                @26A
R0       EQU   00                      EQUATES FOR REGISTERS 0-15
R1       EQU   01
R2       EQU   02
R3       EQU   03
R4       EQU   04
R5       EQU   05
R6       EQU   06
R7       EQU   07
R8       EQU   08
R9       EQU   09
R10      EQU   10
R11      EQU   11
R12      EQU   12
R13      EQU   13
R14      EQU   14
R15      EQU   15
R3AWAPTR EQU   R3
REGSAVE  EQU   4
RETCODE  EQU   16
ALLOCWA  EQU   0
CPPLPTR  EQU   ALLOCWA
PTRPDL   EQU   ALLOCWA+4
SWITCH   EQU   ALLOCWA+12
RBCODE12 EQU   SWITCH
ENTRYCD  EQU   ALLOCWA+14
PTRS99RB EQU   ALLOCWA+20
RCODESV  EQU   ALLOCWA+24
PTRMSGS  EQU   ALLOCWA+32
VCFLAGS  EQU   ALLOCWA+36
MSGPTR   EQU   ALLOCWA+48
CMDTWO   EQU   ALLOCWA+60
COMMECB  EQU   ALLOCWA+160
ALLOCPDL EQU   0
DSNPDE   EQU   ALLOCPDL+48+4                                       @26C
DSNPTR   EQU   DSNPDE
DSNLENGH EQU   DSNPDE+4
DSNFLAGS EQU   DSNPDE+6
MBRPTR   EQU   DSNPDE+8
MBRFLAGS EQU   DSNPDE+14
PASSPTR  EQU   DSNPDE+16
PASSFLAG EQU   DSNPDE+22
DDNMEPDE EQU   ALLOCPDL+76+4                                       @26C
DDNPTR   EQU   DDNMEPDE
DDNFLAGS EQU   DDNMEPDE+6
CLASPDE  EQU   ALLOCPDL+84+4                                       @26C
CLASPTR  EQU   CLASPDE
CLASFLG  EQU   CLASPDE+6
SERPDE   EQU   ALLOCPDL+92+4                                       @26C
SERPTR   EQU   SERPDE
SERFLAGS EQU   SERPDE+6
BLKPDE   EQU   ALLOCPDL+104+4                                      @26C
BLKPTR   EQU   BLKPDE
BLKFLAGS EQU   BLKPDE+6
ABLKPDE  EQU   ALLOCPDL+112+4                                      @26C
ABLKPTR  EQU   ABLKPDE
ABLKFLG  EQU   ABLKPDE+6
SPACPDE  EQU   ALLOCPDL+120+4                                      @26C
SPACEPTR EQU   SPACPDE
SPACEFLG EQU   SPACPDE+6
SPACPDE2 EQU   ALLOCPDL+128+4                                      @26C
SPACPTR2 EQU   SPACPDE2
SPACFLG2 EQU   SPACPDE2+6
DIRECPDE EQU   ALLOCPDL+136+4                                      @26C
DIRPTR   EQU   DIRECPDE
DIRFLAGS EQU   DIRECPDE+6
ATTRPDE  EQU   ALLOCPDL+144+4                                      @26C
ATTRPTR  EQU   ATTRPDE
ATTRFLAG EQU   ATTRPDE+6
USIDPDE  EQU   ALLOCPDL+152+4                                      @26C
USIDPTR  EQU   USIDPDE
USIDFLAG EQU   USIDPDE+6
UTYPPDE  EQU   ALLOCPDL+160+4                                      @26C
UTYPPTR  EQU   UTYPPDE
UTYPFLAG EQU   UTYPPDE+6
UNCNTPDE EQU   ALLOCPDL+168+4                                      @26C
UCNTPTR  EQU   UNCNTPDE
UCNTFLAG EQU   UNCNTPDE+6
POSTPDE  EQU   ALLOCPDL+180+4                                      @26C
POSPTR   EQU   POSTPDE
POSFLAG  EQU   POSTPDE+6
MXVOLPDE EQU   ALLOCPDL+188+4                                      @26C
MXVOLPTR EQU   MXVOLPDE
MXVOLFLG EQU   MXVOLPDE+6
VOLSQPDE EQU   ALLOCPDL+196+4                                      @26C
VOLSQPTR EQU   VOLSQPDE
VOLSQFLG EQU   VOLSQPDE+6
VVGRPDE  EQU   ALLOCPDL+204+4                                      @26C
VVGPPTR  EQU   VVGRPDE
VVGPFLG  EQU   VVGRPDE+6
DSNBUF   EQU   0
MSGTABLE EQU   0
LISTPTR  EQU   MSGTABLE
TMCTPTR  EQU   MSGTABLE+4
MTCPPL   EQU   TMCTPTR
ECBPTR   EQU   MSGTABLE+8
@NM00005 EQU   MSGTABLE+12
MTHIGH   EQU   @NM00005
MSGCSECT EQU   MSGTABLE+16
SW       EQU   MSGTABLE+20
MTPUTLSW EQU   SW
RETMSG   EQU   MSGTABLE+21
SW2      EQU   MSGTABLE+24
MSGID    EQU   MSGTABLE+40
MTINSRTS EQU   MSGTABLE+44
L1       EQU   MTINSRTS
VAR1     EQU   MTINSRTS+1
L2       EQU   MTINSRTS+4
HIGHL2   EQU   L2
VAR2     EQU   MTINSRTS+5
L3       EQU   MTINSRTS+8
L4       EQU   MTINSRTS+12
RET      EQU   0
CPPL     EQU   0
S99RB    EQU   0
S99FLAG1 EQU   S99RB+2
S99FLG11 EQU   S99FLAG1
S99RSC   EQU   S99RB+4
S99TXTPP EQU   S99RB+8
S99FLAG2 EQU   S99RB+16
S99FLG21 EQU   S99FLAG2
S99FLG22 EQU   S99FLAG2+1
S99TUPL  EQU   0
S99TUPTR EQU   S99TUPL
S99TUP   EQU   0
S99TUNIT EQU   0
S99TUENT EQU   S99TUNIT+4
S99TUFLD EQU   0
ABLKBUF  EQU   0
ATTRBUF  EQU   0
BLKBUF   EQU   0
CLASSBUF EQU   0
DDNBUF   EQU   0
DIRBUF   EQU   0
MBRBUF   EQU   0
MSGECB   EQU   0
MSGTMCT  EQU   0
MXVOLBUF EQU   0
PASSBUF  EQU   0
POSTBUF  EQU   0
SERBUF   EQU   0
SPACBUF2 EQU   0
SPACEBUF EQU   0
UCNTBUF  EQU   0
USIDBUF  EQU   0
UTYPBUF  EQU   0
VSEQBUF  EQU   0
VVGPBUF  EQU   0
INPARMS  EQU   0
*                                      START UNREFERENCED COMPONENTS
S99TUPRM EQU   S99TUFLD+2
S99TULEN EQU   S99TUFLD
S99TUPAR EQU   S99TUENT+2
S99TULNG EQU   S99TUENT
S99TUNUM EQU   S99TUNIT+2
S99TUKEY EQU   S99TUNIT
S99TUPND EQU   S99TUP
S99TUPLN EQU   S99TUPTR
S99FLG24 EQU   S99FLAG2+3
S99FLG23 EQU   S99FLAG2+2
S99PCINT EQU   S99FLG22
S99UDEVT EQU   S99FLG22
S99MOUNT EQU   S99FLG21
S99CATLG EQU   S99FLG21
S99TIONQ EQU   S99FLG21
S99OFFLN EQU   S99FLG21
S99WTUNT EQU   S99FLG21
S99NORES EQU   S99FLG21
S99WTDSN EQU   S99FLG21
S99WTVOL EQU   S99FLG21
S99RSV01 EQU   S99RB+12
S99INFO  EQU   S99RSC+2
S99ERROR EQU   S99RSC
S99FLG12 EQU   S99FLAG1+1
S99JBSYS EQU   S99FLG11
S99NOMNT EQU   S99FLG11
S99NOCNV EQU   S99FLG11
S99ONCNV EQU   S99FLG11
S99VERB  EQU   S99RB+1
S99RBLN  EQU   S99RB
CPPLECT  EQU   CPPL+12
CPPLPSCB EQU   CPPL+8
CPPLUPT  EQU   CPPL+4
CPPLCBUF EQU   CPPL
RETCHAR  EQU   RET+2
RETSIZE  EQU   RET
MSGRTN   EQU   MSGTABLE+60
VAR4     EQU   MTINSRTS+13
HIGHL4   EQU   L4
VAR3     EQU   MTINSRTS+9
HIGHL3   EQU   L3
HIGHL1   EQU   L1
@NM00009 EQU   MSGTABLE+36
@NM00008 EQU   MSGTABLE+32
MTOLDPTR EQU   MSGTABLE+28
@NM00007 EQU   MSGTABLE+25
@NM00006 EQU   SW2
MTPGMSW  EQU   SW2
MTNPLMSW EQU   SW2
MTNOXQSW EQU   SW2
MTDOMSW  EQU   SW2
MT2OLDSW EQU   SW2
MTNHEXSW EQU   SW
MTWTPSW  EQU   SW
MTJOBISW EQU   SW
MTKEY1SW EQU   SW
MTHEXSW  EQU   SW
MTWTOSW  EQU   SW
MTNOIDSW EQU   SW
DSNTERM  EQU   DSNBUF
VVGPRSV2 EQU   VVGRPDE+7
VVGPRSV1 EQU   VVGPFLG
VVGPRES  EQU   VVGPFLG
VVGPLEN  EQU   VVGRPDE+4
VOLRSV2  EQU   VOLSQPDE+7
VOLRSV1  EQU   VOLSQFLG
VOLPRES  EQU   VOLSQFLG
VOLSQLEN EQU   VOLSQPDE+4
MXVRSV2  EQU   MXVOLPDE+7
MXVRSV1  EQU   MXVOLFLG
MXVPRES  EQU   MXVOLFLG
MXVOLLEN EQU   MXVOLPDE+4
POSRSV2  EQU   POSTPDE+7
POSRSV1  EQU   POSFLAG
POSPRES  EQU   POSFLAG
POSLEN   EQU   POSTPDE+4
LABLRSV1 EQU   ALLOCPDL+178+4                                      @26C
LABELPDE EQU   ALLOCPDL+176+4                                      @26C
UCNTRSV2 EQU   UNCNTPDE+7
UCNTRSV1 EQU   UCNTFLAG
UCNTPRES EQU   UCNTFLAG
UCNTLEN  EQU   UNCNTPDE+4
UTYPRSV2 EQU   UTYPPDE+7
UTYPRSV1 EQU   UTYPFLAG
UTYPPRES EQU   UTYPFLAG
UTYPLEN  EQU   UTYPPDE+4
USIDRSV2 EQU   USIDPDE+7
USIDRSV1 EQU   USIDFLAG
USIDPRES EQU   USIDFLAG
USIDLEN  EQU   USIDPDE+4
ATTRRSV2 EQU   ATTRPDE+7
ATTRRSV1 EQU   ATTRFLAG
ATTRPRES EQU   ATTRFLAG
ATTRLEN  EQU   ATTRPDE+4
DIRRSV2  EQU   DIRECPDE+7
DIRRSV1  EQU   DIRFLAGS
DIRPRES  EQU   DIRFLAGS
DIRLEN   EQU   DIRECPDE+4
SPCRSV22 EQU   SPACPDE2+7
SPCRSV12 EQU   SPACFLG2
SPCPRES2 EQU   SPACFLG2
SPACLEN2 EQU   SPACPDE2+4
SPACRSV2 EQU   SPACPDE+7
SPACRSV1 EQU   SPACEFLG
SPACPRES EQU   SPACEFLG
SPACELEN EQU   SPACPDE+4
ABLKRSV2 EQU   ABLKPDE+7
ABLKRSV1 EQU   ABLKFLG
ABLKPRES EQU   ABLKFLG
ABLKLEN  EQU   ABLKPDE+4
BLKRSV2  EQU   BLKPDE+7
BLKRSV1  EQU   BLKFLAGS
BLKPRES  EQU   BLKFLAGS
BLKLEN   EQU   BLKPDE+4
SERNEXT  EQU   SERPDE+8
SERRSV2  EQU   SERPDE+7
SERRSV1  EQU   SERFLAGS
SERPRES  EQU   SERFLAGS
SERLEN   EQU   SERPDE+4
CLASRSV2 EQU   CLASPDE+7
CLASRSV1 EQU   CLASFLG
CLASPRES EQU   CLASFLG
CLASLEN  EQU   CLASPDE+4
DDNRSV2  EQU   DDNMEPDE+7
DDNRSV1  EQU   DDNFLAGS
DDNPRES  EQU   DDNFLAGS
DDNLEN   EQU   DDNMEPDE+4
DSNNEXT  EQU   DSNPDE+24
PASSRSV2 EQU   DSNPDE+23
PASSRSV1 EQU   PASSFLAG
PASSPRES EQU   PASSFLAG
PASSLEN  EQU   DSNPDE+20
MBRRSV2  EQU   DSNPDE+15
MBRRSV1  EQU   MBRFLAGS
MBRPRES  EQU   MBRFLAGS
MBRLEN   EQU   DSNPDE+12
DSNRSV2  EQU   DSNPDE+7
DSNRSV1  EQU   DSNFLAGS
DSNQUOT  EQU   DSNFLAGS
DSNPRES  EQU   DSNFLAGS
DISPPDE  EQU   ALLOCPDL+46
RNDPDE   EQU   ALLOCPDL+44
RLSEPDE  EQU   ALLOCPDL+42
VSEQPDE  EQU   ALLOCPDL+40
PRIVPDE  EQU   ALLOCPDL+38
MAXVPDE  EQU   ALLOCPDL+36
POSPDE   EQU   ALLOCPDL+34
LABLPDE  EQU   ALLOCPDL+32
UCNTPDE  EQU   ALLOCPDL+30
UNITPDE  EQU   ALLOCPDL+28
HOLDPDE  EQU   ALLOCPDL+26
DESTPDE  EQU   ALLOCPDL+24
USINGPDE EQU   ALLOCPDL+22
DIRPDE   EQU   ALLOCPDL+20
SPACEPDE EQU   ALLOCPDL+18
BLOKPDE  EQU   ALLOCPDL+16
VOLPDE   EQU   ALLOCPDL+14
STATSPDE EQU   ALLOCPDL+12
FILEPDE  EQU   ALLOCPDL+10
DSPDE    EQU   ALLOCPDL+8
@NM00004 EQU   ALLOCPDL+4
@NM00003 EQU   ALLOCPDL
FIRSTPTR EQU   ALLOCWA+176
PPLPTR   EQU   ALLOCWA+172
NBLKBUF1 EQU   ALLOCWA+164
PDLADDR3 EQU   ALLOCWA+156
PPLTWO   EQU   ALLOCWA+128
CMDTWOBF EQU   CMDTWO+4
CMDTWOOF EQU   CMDTWO+2
CMDTWOLN EQU   CMDTWO
Q        EQU   ALLOCWA+56
P        EQU   ALLOCWA+52
PRCODE   EQU   ALLOCWA+44
PMPTPDL  EQU   ALLOCWA+40
@NM00002 EQU   ALLOCWA+38
NCLASBUF EQU   ALLOCWA+37
@NM00001 EQU   VCFLAGS
SPACE2   EQU   VCFLAGS
SPACE1   EQU   VCFLAGS
AVBLK    EQU   VCFLAGS
DSNCTR   EQU   ALLOCWA+30
VLISTCTR EQU   ALLOCWA+28
TXT      EQU   ALLOCWA+16
SWITCH2  EQU   ALLOCWA+13                                          @26C
FIRSTPDE EQU   SWITCH
AWARSV1  EQU   SWITCH
ASTRSK   EQU   SWITCH
TERMOPT  EQU   SWITCH
CONT     EQU   SWITCH
PROMPT   EQU   SWITCH
DSOVRLAY EQU   SWITCH
GTPLPTR  EQU   ALLOCWA+8
*                                      END UNREFERENCED COMPONENTS
@RC00053 EQU   @EL00001
@RC00083 EQU   @EL00003
@RC00081 EQU   @EL00003
@RF00114 EQU   @EL00006
@EL01    EQU   @EL00001
@ENDDATA EQU   *
         END   IKJEFD35,(C'PLS0301',0603,74204)
/*
//*
//STEP13  EXEC PGM=IEBGENER
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  *
  IDENTIFY IKJEFD35('ZP60026')
++MOD(IKJEFD36) DISTLIB(AOST4).
/*
//SYSUT2   DD  DSN=&&SMPMCS,DISP=(MOD,PASS)
//SYSIN    DD  DUMMY
//*
//STEP14  EXEC PGM=IFOX00,PARM='OBJECT,NODECK,NOTERM,XREF(SHORT)'
//SYSPRINT DD  SYSOUT=*
//SYSLIB   DD  DSN=SYS1.MACLIB,DISP=SHR
//         DD  DSN=SYS1.SMPMTS,DISP=SHR
//         DD  DSN=SYS1.AMODGEN,DISP=SHR
//SYSUT1   DD  UNIT=SYSALLDA,SPACE=(CYL,10)
//SYSUT2   DD  UNIT=SYSALLDA,SPACE=(CYL,10)
//SYSUT3   DD  UNIT=SYSALLDA,SPACE=(CYL,10)
//SYSGO    DD  DSN=&&SMPMCS,DISP=(MOD,PASS)
//SYSIN    DD  *
         TITLE 'ALLOCATE OPTION ROUTINE                                *
                        '
***********************************************************************
*  $26   ZP60026  2010-01-23 GREG PRICE: ADD 'REUSE' OPERAND.         *
*                            CHANGES APPLIED TO EBB1102 LEVEL.        *
***********************************************************************
IKJEFD36 CSECT ,                                                   0001
@MAINENT DS    0H                                                  0001
         USING *,R15                                               0001
         B     @PROLOG                                             0001
         DC    AL1(33)                                             @26C
         DC    C'IKJEFD36  74.204'                                 0001
         DC    CL17' ZP60026 2010.023'                             @26A
         DROP  R15
@PROLOG  STM   R14,R12,12(R13)                                     0001
         BALR  R12,0                                               0001
@PSTART  DS    0H                                                  0001
         USING @PSTART,R12                                         0001
         L     R0,@SIZDATD                                         0001
         GETMAIN  R,LV=(0)
         LR    R11,R1                                              0001
         USING @DATD,R11                                           0001
         ST    R13,@SA00001+4                                      0001
         LM    R0,R1,20(R13)                                       0001
         ST    R11,8(,R13)                                         0001
         LR    R13,R11                                             0001
         XC    @ZTEMPS(@ZLEN),@ZTEMPS
         MVC   @PC00001(4),0(R1)                                   0001
*                                                                  0068
*                                                                  0069
*/********************************************************************/
*/*                                                                  */
*/* SEGMENT NAME -                                                   */
*/*      PRMPTFLE                                                    */
*/*                                                                  */
*/* FUNCTION -                                                       */
*/*      THIS ROUTINE NOTIFIES THE USER THAT THE REQUESTED FILENAME  */
*/*      IS IN USE, AND PROMPTS THE USER TO ENTER EITHER FREE OR     */
*/*      CARRIAGE RETURN. IF FREE IS ENTERED, THE FILE IS FREED AND  */
*/*      RE-ALLOCATED. ELSE, ALLOCATE TERMINATES.                    */
*/*                                                                  */
*/* OPERATION -                                                      */
*/*      DAIRFAIL IS INVOKED TO ISSUE THE FILE IN USE MESSAGE. THE   */
*/*      PROMPT MESSAGE IS SET UP AND ROUTINE PRMOPT IS INVOKED TO   */
*/*      ISSUE THE PUTGET AND ANALYZE THE RESPONSE.                  */
*/*                                                                  */
*/* INPUT -                                                          */
*/*      DDNAME PDE                                                  */
*/*                                                                  */
*/* OUTPUT -                                                         */
*/*      FILE FREED AND RE-ALLOCATED, OR                             */
*/*      ALLOCATE TERMINATION INDICATED                              */
*/*                                                                  */
*/* NOTE:  THIS ROUTINE REPRESENTS A DIRECT LINE FUNCTION AND CAN    */
*/*        BE INCORPORATED AS AN INCLUDED CODE SEGMENT WHEN THE      */
*/*        INCLUDE FUNCTION HAS RECEIVED A STANDARD DEFINITION.      */
*/********************************************************************/
*    PRMPTFLE:                                                     0069
*        AWAPTR = R1;                  /* PTR TO WORKAREA            */
PRMPTFLE LR    R3AWAPTR,R1                                         0069
*        MSGTABLE = MSGTABLE && MSGTABLE;                          0070
         L     R10,MSGPTR(,R3AWAPTR)                               0070
         XC    MSGTABLE(64,R10),MSGTABLE(R10)                      0070
*        IF REUSING DDNAME THEN SKIP MESSAGES                      #26A
         TM    REUSE(R3AWAPTR),B'00000010'                         @26A
         BNO   @NOREUSE                                            @26A
         BAL   R14,VALIDRSP            BEHAVE AS IF FREED ENTERED  @26A
         B     @RF00074                                            @26A
@NOREUSE EQU   *                                                   @26A
*        ENTRYCD = 1;                  /* SET ENTRY CODE             */
         MVC   ENTRYCD(2,R3AWAPTR),@HW1                            0071
*        R1 = AWAPTR;                  /* WORKAREA PTR IN REG 1      */
         LR    R1,R3AWAPTR                                         0072
*        CALL IKJEFD35;                /* ISSUE FILE IN USE MSG      */
*                                                                  0073
         L     R15,VCONSD35                                        0073
         BALR  R14,R15                                             0073
*        IF RBCODE12 = '0'B THEN       /* IF NO ERROR,               */
         TM    RBCODE12(R3AWAPTR),B'10000000'                      0074
         BNZ   @RF00074                                            0074
*          DO;                         /* THEN                       */
*            MSGID = '113A';           /* SET MESSAGE ID             */
         L     R10,MSGPTR(,R3AWAPTR)                               0076
         MVC   MSGID(4,R10),@CHR113A                               0076
*            MTPUTLSW = '0'B;          /* INDICATE PUTGET            */
         NI    MTPUTLSW(R10),B'10111111'                           0077
*            VAR1 = DDNPTR;            /* PTR TO DDNAME VARIABLE     */
         L     R2,PTRPDL(,R3AWAPTR)                                0078
         MVC   VAR1(3,R10),DDNPTR+1(R2)                            0078
*            L1 = DDNLEN;              /* DDNAME VARIABLE LENGTH     */
         MVC   L1(1,R10),DDNLEN+1(R2)                              0079
*            CALL PRMOPT;              /* ISSUE MSG AND PROCESS    0080
*                                         RESPONSE                   */
         BAL   R14,PRMOPT                                          0080
*          END;                                                    0081
*        IF RBCODE12 = '1'B THEN                                   0082
@RF00074 TM    RBCODE12(R3AWAPTR),B'10000000'                      0082
         BNO   @RF00082                                            0082
*          RETCODE = 12;                                           0083
         L     R10,REGSAVE(,R13)                                   0083
         MVC   RETCODE(4,R10),@FW12                                0083
*        ELSE                                                      0084
*          RETCODE = 0;                                            0084
*                                                                  0084
         B     @RC00082                                            0084
@RF00082 L     R10,REGSAVE(,R13)                                   0084
         SLR   R15,R15                                             0084
         ST    R15,RETCODE(,R10)                                   0084
*        RETURN;                       /* RETURN TO CALLER           */
@EL00001 L     R13,4(,R13)                                         0085
@EF00001 L     R0,@SIZDATD                                         0085
         LR    R1,R11                                              0085
         FREEMAIN R,LV=(0),A=(1)
@ER00001 LM    R14,R12,12(R13)                                     0085
         BR    R14                                                 0085
*/********************************************************************/
*/*                                                                  */
*/* SEGMENT NAME -                                                   */
*/*      PRMOPT                                                      */
*/*                                                                  */
*/* FUNCTION -                                                       */
*/*      THIS ROUTINE PROMPTS THE USER FOR A TERMINAL RESPONSE, AND  */
*/*      INVOKES COMMAND SCAN TO CHECK THE RESPONSE.                 */
*/*                                                                  */
*/* OPERATION -                                                      */
*/*      ISSUE PROMPT MESSAGE. SET UP THE                            */
*/*      PARAMETER LIST FOR COMMAND SCAN, PASSING THE BUFFER         */
*/*      RETURNED FROM PUTGET. INVOKE COMMAND SCAN. SAVE THE RETURN  */
*/*      CODE AND ANALYZE COMMAND SCAN OUTPUT.                       */
*/*                                                                  */
*/* INPUT -                                                          */
*/*      PROMPT MESSAGE TO BE ISSUED                                 */
*/*                                                                  */
*/* OUTPUT -                                                         */
*/*      PROMPT MESSAGE ISSUED                                       */
*/*      RESPONSE SCANNED FOR VALIDITY                               */
*/*                                                                  */
*/* NOTE:  THIS ROUTINE REPRESENTS A DIRECT LINE FUNCTION AND CAN    */
*/*        BE INCORPORATED AS AN INCLUDED CODE SEGMENT WHEN THE      */
*/*        INCLUDE FUNCTION HAS RECEIVED A STANDARD DEFINITION.      */
*/********************************************************************/
*    PRMOPT: PROC;                                                 0086
         B     @EL00001                                            0086
PRMOPT   STM   R14,R12,@SA00002                                    0086
*    PRMOPT1:                                                      0087
*        CALL PUTMSG;                  /* ISSUE PROMPT MSG           */
PRMOPT1  BAL   R14,PUTMSG                                          0087
*        NULLRSP = '0'B;               /* INIT FLAG          @Y30NQKH*/
         NI    NULLRSP,B'01111111'                                 0088
*        IF RCODESV > 0 THEN           /* IF PUTGET FAILED,          */
         L     R10,RCODESV(,R3AWAPTR)                              0089
         LTR   R10,R10                                             0089
         BNP   @RF00089                                            0089
*          RBCODE12 = '1'B;            /* SET ALLOC FAILED INDICATOR */
         OI    RBCODE12(R3AWAPTR),B'10000000'                      0090
*        ELSE                          /* ELSE               @YM05480*/
*          IF RETSIZE = 2 THEN         /* IF NULL RESPONSE   @YM05480*/
         B     @RC00089                                            0091
@RF00089 L     R10,MSGPTR(,R3AWAPTR)                               0091
         L     R10,RETMSG-1(,R10)                                  0091
         LA    R10,0(,R10)                                         0091
         CLC   RETSIZE(2,R10),@HW2                                 0091
         BNE   @RF00091                                            0091
*            DO;                       /* THEN               @Y30NQKH*/
*              NULLRSP = '1'B;         /* INDICATE SO        @Y30NQKH*/
         OI    NULLRSP,B'10000000'                                 0093
*              GO TO INVRSP;           /* PROCESS INVALID    @YM05480*/
         B     INVRSP                                              0094
*            END;                      /*                    @Y30NQKH*/
*        ELSE                                                      0096
*          DO;                         /* IF PUTGET SUCCESSFUL,      */
@RF00091 DS    0H                                                  0097
*            CSPLPTR = ADDR(SCANAREA); /* CSCAN PARM LIST PTR        */
         LA    CSPLPTR,SCANAREA                                    0097
*            SYNCHK = '1'B;            /* NO SYNTAX CHECK            */
         OI    SYNCHK,B'10000000'                                  0098
*            CSPLFLG = ADDR(FLAGWORD); /* FLAGS IN CSCAN LIST        */
         LA    R10,FLAGWORD                                        0099
         ST    R10,CSPLFLG(,CSPLPTR)                               0099
*            IF RETSIZE > 20 THEN      /* IF LENGTH > 20     @YM05480*/
         LA    R10,20                                              0100
         L     R5,MSGPTR(,R3AWAPTR)                                0100
         L     R5,RETMSG-1(,R5)                                    0100
         LA    R5,0(,R5)                                           0100
         CH    R10,RETSIZE(,R5)                                    0100
         BNL   @RF00100                                            0100
*              RETSIZE = 20;           /* SET LENGTH TO 20   @YM05480*/
         STH   R10,RETSIZE(,R5)                                    0101
*            PGLENGH = RETSIZE+2;      /* SET LEN OF INPUT   @YM05468*/
@RF00100 L     R10,MSGPTR(,R3AWAPTR)                               0102
         L     R10,RETMSG-1(,R10)                                  0102
         LA    R10,0(,R10)                                         0102
         LH    R5,RETSIZE(,R10)                                    0102
         LA    R2,2                                                0102
         ALR   R2,R5                                               0102
         STH   R2,PGLENGH                                          0102
*            PGOFFST = 0;              /* SET OFFSET TO 0            */
         SLR   R2,R2                                               0103
         STH   R2,PGOFFST                                          0103
*                                      /* COPY INPUT INTO BUFFER     */
*            PGINPUT(1:RETSIZE-2) = RETCHAR(1:RETSIZE-2); /* @YM05480*/
         SL    R5,@CF00321                                         0104
         EX    R5,@SM01407                                         0104
*            CSPLCBUF = ADDR(PGBUFFR); /* RESPONSE IN LIST           */
         LA    R10,PGBUFFR                                         0105
         ST    R10,CSPLCBUF(,CSPLPTR)                              0105
*            CSPLOA = ADDR(SCANOUT);   /* PTR TO OUTPUT AREA         */
         LA    R10,SCANOUT                                         0106
         ST    R10,CSPLOA(,CSPLPTR)                                0106
*            CSPLUPT = CPPLUPT;        /* PTR TO UPT                 */
         L     R10,CPPLPTR(,R3AWAPTR)                              0107
         L     R5,CPPLUPT(,R10)                                    0107
         ST    R5,CSPLUPT(,CSPLPTR)                                0107
*            CSPLECT = CPPLECT;        /* PTR TO ECT                 */
         L     R10,CPPLECT(,R10)                                   0108
         ST    R10,CSPLECT(,CSPLPTR)                               0108
*            CSPLECB = ADDR(COMMECB);  /* PTR TO ECB                 */
         LA    R10,COMMECB(,R3AWAPTR)                              0109
         ST    R10,CSPLECB(,CSPLPTR)                               0109
*            R1 = CSPLPTR;             /* PTR IN REGISTER 1          */
         LR    R1,CSPLPTR                                          0110
*              DO; /*CALLTSSR EP(IKJSCAN)*/                        0111
*                RESPECIFY GPR01P RSTD;                            0112
*                IF CVTSCAN = 0                                    0113
*                  THEN                                            0113
         L     R10,CVTPTR                                          0113
         C     R2,CVTSCAN-CVT(,R10)                                0113
         BNE   @RF00113                                            0113
*                    GEN(LINK EP=IKJSCAN);                         0114
         LINK EP=IKJSCAN
*                  ELSE                                            0115
*                    CALL CVTSCAN;                                 0115
         B     @RC00113                                            0115
@RF00113 L     R10,CVTPTR                                          0115
         L     R15,CVTSCAN-CVT(,R10)                               0115
         BALR  R14,R15                                             0115
*                RESPECIFY GPR01P UNRSTD;                          0116
@RC00113 DS    0H                                                  0117
*              END;   /* INVOKE COMMAND SCAN@Y30NQKH*/             0117
*            RCODESV = R15;            /* SAVE RETURN CODE           */
         ST    R15,RCODESV(,R3AWAPTR)                              0118
*            IF RCODESV > 0 THEN       /* IF COMMAND SCAN FAILED     */
         L     R10,RCODESV(,R3AWAPTR)                              0119
         LTR   R10,R10                                             0119
         BNP   @RF00119                                            0119
*              DO;                     /* ISSUE SYSTEM ERROR MSG     */
*                ENTRYCD = 4;          /* SET ENTRY CODE             */
         MVC   ENTRYCD(2,R3AWAPTR),@HW4                            0121
*                R1 = AWAPTR;          /* WORKAREA PTR IN REG 1      */
         LR    R1,R3AWAPTR                                         0122
*                                      /* ISSUE SYSTEM ERROR MSG     */
*                CALL IKJEFD35;                                    0123
         L     R15,VCONSD35                                        0123
         BALR  R14,R15                                             0123
*              END;                                                0124
*            ELSE                                                  0125
*              DO;                     /* IF CMD SCAN SUCCESSFUL     */
         B     @RC00119                                            0125
@RF00119 DS    0H                                                  0126
*                CSOAPTR = CSPLOA;     /* SET PTR TO OUTPUT AREA     */
         L     CSOAPTR,CSPLOA(,CSPLPTR)                            0126
*                IF CSOANOC='1'B THEN  /* IF NULL RESPONSE   @Y30NQKH*/
         TM    CSOANOC(CSOAPTR),B'00010000'                        0127
         BNO   @RF00127                                            0127
*                  DO;                 /* THEN               @Y30NQKH*/
*                    NULLRSP = '1'B;   /* INDICATE SO        @Y30NQKH*/
         OI    NULLRSP,B'10000000'                                 0129
*                    GO TO INVRSP;     /* PROCESS INVALID    @Y30NQKH*/
         B     INVRSP                                              0130
*                  END;                /*                    @Y30NQKH*/
*                ELSE                  /* ELSE               @Y30NQKH*/
*                IF CSOACNM = 0 |      /* IF PTR IS 0,       @Y30NQKH*/
*                   CSOALNM > 4 THEN   /* LENGTH > 4, THEN   @Y30NQKH*/
@RF00127 L     R10,CSOACNM(,CSOAPTR)                               0132
         LTR   R10,R10                                             0132
         BZ    @RT00132                                            0132
         LH    R15,CSOALNM(,CSOAPTR)                               0132
         C     R15,@FW4                                            0132
         BH    @RT00132                                            0132
*                  GO TO INVRSP;       /* PROCESS INVALID    @Y30NQKH
*                                         RESPONSE           @Y30NQKH*/
*                ELSE                  /* ELSE               @Y30NQKH*/
*                                      /* IF FREE ENTERED,   @Y30NQKH*/
*                  IF CSOABUF(1:CSOALNM) = 'FREE' THEN    /* @Y30NQKH*/
         BCTR  R15,0                                               0134
         EX    R15,@SC01409                                        0134
         BNE   @RF00134                                            0134
*                    CALL VALIDRSP;    /* PROCESS VALID REPLY@Y30NQKH*/
         BAL   R14,VALIDRSP                                        0135
*                  ELSE                /* ELSE               @Y30NQKH*/
*                    IF CSOALNM>3 THEN /* IF LENGTH > 3 THEN @Y30NQKH*/
         B     @RC00134                                            0136
@RF00134 LH    R10,CSOALNM(,CSOAPTR)                               0136
         C     R10,@CF00321                                        0136
         BH    @RT00136                                            0136
*                      GO TO INVRSP;   /* PROCESS INVALID    @Y30NQKH
*                                         RESPONSE           @Y30NQKH*/
*                    ELSE              /* ELSE               @Y30NQKH*/
*                                      /* IF END ENTERED     @Y30NQKH*/
*                      IF CSOABUF(1:CSOALNM) = 'END' THEN /* @Y30NQKH*/
         L     R1,CSOACNM(,CSOAPTR)                                0138
         BCTR  R10,0                                               0138
         EX    R10,@SC01411                                        0138
         BNE   @RF00138                                            0138
*                        DO;           /* THEN               @Y30NQKH*/
*                                     /* INDICATE ALLOC ERROR@Y30NQKH*/
*                          RBCODE12 = '1'B;               /* @Y30NQKH*/
*                                      /* TERMINATION REQ    @Y30NQKH*/
*                          TERMOPT = '1'B;                /* @Y30NQKH*/
         OI    RBCODE12(R3AWAPTR),B'10001000'                      0141
*                        END;          /*                    @Y30NQKH*/
*                      ELSE            /* ELSE, PROCESS      @Y30NQKH*/
*                        DO;           /* INVALID RESPONSE   @Y30NQKH*/
         B     @RC00138                                            0143
@RF00138 DS    0H                                                  0144
*         INVRSP:                                                  0144
*                                      /* IF NULL RESPONSE   @Y30NQKH*/
*                        IF NULLRSP = '1'B THEN           /* @Y30NQKH*/
INVRSP   TM    NULLRSP,B'10000000'                                 0144
         BNO   @RF00144                                            0144
*                          DO;         /* THEN               @Y30NQKH*/
*                                      /* CLEAR INDICATOR    @Y30NQKH*/
*                            NULLRSP = '0'B;              /* @Y30NQKH*/
         NI    NULLRSP,B'01111111'                                 0146
*                                      /* SET MESSAGE ID     @Y30NQKH*/
*                            MSGID = '115B';              /* @Y30NQKH*/
         L     R10,MSGPTR(,R3AWAPTR)                               0147
         MVC   MSGID(4,R10),@CHR115B                               0147
*                          END;        /*                    @Y30NQKH*/
*                        ELSE          /* ELSE,PROCESS       @Y30NQKH*/
*                          DO;         /* OTHER THAN NULL    @Y30NQKH*/
         B     @RC00144                                            0149
@RF00144 DS    0H                                                  0150
*                                      /* SET MESSAGE ID     @Y30NQKH*/
*                            MSGID = '115A';              /* @Y30NQKH*/
         L     R10,MSGPTR(,R3AWAPTR)                               0150
         MVC   MSGID(4,R10),@CHR115A                               0150
*                                      /* PTR TO RESPONSE    @Y30NQKH*/
*                            VAR1 = ADDR(RETCHAR);        /* @Y30NQKH*/
         L     R2,RETMSG-1(,R10)                                   0151
         LA    R2,0(,R2)                                           0151
         LA    R15,RETCHAR(,R2)                                    0151
         ST    R15,@TF00001                                        0151
         MVC   VAR1(3,R10),@TF00001+1                              0151
*                                      /* LEN OF VARIABLE    @Y30NQKH*/
*                            L1 = RETSIZE-2;              /* @Y30NQKH*/
         LH    R2,RETSIZE(,R2)                                     0152
         BCTR  R2,0                                                0152
         BCTR  R2,0                                                0152
         STC   R2,L1(,R10)                                         0152
*                          END;        /*                    @Y30NQKH*/
*                        MTPUTLSW = '1'B;/* INDICATE PUTLINE         */
@RC00144 L     R10,MSGPTR(,R3AWAPTR)                               0154
         OI    MTPUTLSW(R10),B'01000000'                           0154
*                        CALL PUTMSG;    /* ISSUE MESSAGE            */
         BAL   R14,PUTMSG                                          0155
*                                        /* IF PUTLINE FAILED,       */
*                        IF RCODESV > 0 THEN                       0156
         L     R10,RCODESV(,R3AWAPTR)                              0156
         LTR   R10,R10                                             0156
         BNP   @RF00156                                            0156
*                                        /* INDICATE SO              */
*                          RBCODE12 = '1'B;                        0157
         OI    RBCODE12(R3AWAPTR),B'10000000'                      0157
*                        ELSE                                      0158
*                          DO;           /* OTHERWISE,               */
         B     @RC00156                                            0158
@RF00156 DS    0H                                                  0159
*                                        /* SET MESSAGE ID           */
*                            MSGID = '116A';                       0159
         L     R10,MSGPTR(,R3AWAPTR)                               0159
         MVC   MSGID(4,R10),@CHR116A                               0159
*                                        /* INDICATE PUTGET          */
*                            MTPUTLSW='0'B;                        0160
         NI    MTPUTLSW(R10),B'10111111'                           0160
*                            VAR1=DDNPTR;/* PTR TO DDNAME VARIABLE   */
         L     R2,PTRPDL(,R3AWAPTR)                                0161
         MVC   VAR1(3,R10),DDNPTR+1(R2)                            0161
*                            L1 = DDNLEN;/* SET VARIABLE LENGTH      */
         MVC   L1(1,R10),DDNLEN+1(R2)                              0162
*                                        /* ISSUE MESSAGE            */
*                            GO TO PRMOPT1;                        0163
         B     PRMOPT1                                             0163
*                          END;                                    0164
*                      END;                                        0165
*               END;                                               0166
*          END;                                                    0167
*    END PRMOPT;                                                   0168
@EL00002 DS    0H                                                  0168
@EF00002 DS    0H                                                  0168
@ER00002 LM    R14,R12,@SA00002                                    0168
         BR    R14                                                 0168
*/********************************************************************/
*/*                                                                  */
*/* SEGMENT NAME -                                                   */
*/*      VALIDRSP                                                    */
*/*                                                                  */
*/* FUNCTION -                                                       */
*/*      THIS ROUTINE FREES THE FILE AND RE-ALLOCATES IT.            */
*/*                                                                  */
*/* OPERATION -                                                      */
*/*      POINTER TO CURRENT TEXT IS SAVED. A GETMAIN IS ISSUED FOR   */
*/*      STORAGE FOR BUILDING THE FREE FUNCTION TEXT. TWO TEXT       */
*/*      UNITS ARE SET UP IN THE STORAGE - ONE SPECIFYING THE        */
*/*      DDNAME TO BE FREED, AND ONE SPECIFYING THAT PERMANENTLY     */
*/*      ALLOCATED DATA SETS ARE TO BE FREED. THE VERB IN THE        */
*/*      REQUEST BLOCK IS SET TO UNALLOC, AND DYNAMIC ALLOCATION IS  */
*/*      INVOKED. THE ORIGINAL TEXT POINTER IS SET BACK IN THE       */
*/*      REQUEST BLOCK AND THE VERB IS SET TO ALLOC. DYNAMIC         */
*/*      ALLOCATION IS INVOKED AGAIN.                                */
*/*                                                                  */
*/* INPUT -                                                          */
*/*      SVC 99 REQUEST BLOCK                                        */
*/*                                                                  */
*/* OUTPUT -                                                         */
*/*      FILE FREED AND RE-ALLOCATED                                 */
*/*                                                                  */
*/* NOTE:  THIS ROUTINE REPRESENTS A DIRECT LINE FUNCTION AND CAN    */
*/*        BE INCORPORATED AS AN INCLUDED CODE SEGMENT WHEN THE      */
*/*        INCLUDE FUNCTION HAS RECEIVED A STANDARD DEFINITION.      */
*/********************************************************************/
*    VALIDRSP: PROC;                                               0169
VALIDRSP STM   R14,R12,@SA00003                                    0169
*        SAVETXTP = S99TXTPP;          /* SAVE PTR TO TEXT           */
         L     R10,PTRS99RB(,R3AWAPTR)                             0170
         L     SAVETXTP,S99TXTPP(,R10)                             0170
*        PRMPTXTP = ADDR(PRMTEXT);     /* SET POINTER TO STORAGE     */
         LA    PRMPTXTP,PRMTEXT                                    0171
*        FILEADDR = ADDR(FILEKEY);     /* ADDRESS OF FILE TEXT       */
         LA    R5,FILEKEY                                          0172
         ST    R5,FILEADDR                                         0172
*        PERMADDR = ADDR(PERMKEY);     /* ADDRESS OF PERM TEXT       */
         LA    R5,PERMKEY                                          0173
         ST    R5,PERMADDR                                         0173
*        ENDPRMT = '1'B;               /* END OF ADDRESS LIST        */
         OI    ENDPRMT,B'10000000'                                 0174
*        FILEKEY = DUNDDNAM;           /* FILE TEXT - KEY,           */
         MVC   FILEKEY(2),@HWDDNAM                                 0175
*        FILENUM = 1;                  /* NUMBER,                    */
         MVC   FILENUM(2),@HW1                                     0176
*        FILELEN = DDNLEN;             /* LENGTH AND PARM            */
         L     R5,PTRPDL(,R3AWAPTR)                                0177
         LH    R15,DDNLEN(,R5)                                     0177
         STH   R15,FILELEN                                         0177
*        FILEBUF(1:FILELEN)=DDNBUF(1:FILELEN);                     0178
         BCTR  R15,0                                               0178
         L     R5,DDNPTR(,R5)                                      0178
         EX    R15,@SM01414                                        0178
*        PERMKEY = DUNUNALC;           /* PERM TEXT - KEY,           */
         MVC   PERMKEY(2),@HWUNALC                                 0179
*        PERMNUM = 0;                  /* AND NUMBER                 */
         SLR   R5,R5                                               0180
         STH   R5,PERMNUM                                          0180
*        S99TXTPP = PRMPTXTP;          /* PTR IN REQUEST BLOCK       */
         ST    PRMPTXTP,S99TXTPP(,R10)                             0181
*        S99VERB = S99VRBUN;           /* UNALLOC VERB               */
         MVI   S99VERB(R10),X'02'                                  0182
*        S99ERROR = S99ERROR && S99ERROR;/* CLEAR ERROR FIELD        */
         XC    S99ERROR(2,R10),S99ERROR(R10)                       0183
*        SAVEFLG = S99FLG11;             /* SAVE FLAG SETTING        */
         MVC   SAVEFLG(1),S99FLG11(R10)                            0184
*        S99FLG11 = S99FLG11 && S99FLG11;/* CLEAR FLAGS              */
         XC    S99FLG11(1,R10),S99FLG11(R10)                       0185
*        CALL DYNSVC;                  /* INVOKE DYN ALLOCATION      */
         BAL   R14,DYNSVC                                          0186
*        IF RCODESV = 0 THEN           /* IF DYN ALLOC SUCCESSFUL    */
         L     R10,RCODESV(,R3AWAPTR)                              0187
         LTR   R10,R10                                             0187
         BNZ   @RF00187                                            0187
*          DO;                                                     0188
*            S99TXTPP = SAVETXTP;      /* RESTORE FORMER TEXT PTR    */
         L     R10,PTRS99RB(,R3AWAPTR)                             0189
         ST    SAVETXTP,S99TXTPP(,R10)                             0189
*            S99FLG11 = SAVEFLG;       /* RESTORE FLAG SETTINGS      */
         MVC   S99FLG11(1,R10),SAVEFLG                             0190
*            S99VERB = S99VRBAL;       /* SET ALLOC VERB             */
         MVI   S99VERB(R10),X'01'                                  0191
*            CALL DYNSVC;              /* INVOKE DYN ALLOCATION      */
         BAL   R14,DYNSVC                                          0192
*            IF RCODESV > 0 THEN       /* IF DYN ALLOC FAILED,       */
         L     R10,RCODESV(,R3AWAPTR)                              0193
         LTR   R10,R10                                             0193
         BNP   @RF00193                                            0193
*              DO;                                                 0194
*                RBCODE12 = '1'B;      /* INDICATE ALLOC FAILED      */
         OI    RBCODE12(R3AWAPTR),B'10000000'                      0195
*                ENTRYCD = 1;          /* INDICATE ALLOC ERROR       */
         MVC   ENTRYCD(2,R3AWAPTR),@HW1                            0196
*                R1 = AWAPTR;          /* WORKAREA PTR IN REG 1      */
         LR    R1,R3AWAPTR                                         0197
*                CALL IKJEFD35;        /* ISSUE ERROR MESSAGE        */
         L     R15,VCONSD35                                        0198
         BALR  R14,R15                                             0198
*              END;                                                0199
*          END;                                                    0200
*        ELSE                          /* ELSE,                      */
*          DO;                         /* IF DYN ALLOC FAILED        */
         B     @RC00187                                            0201
@RF00187 DS    0H                                                  0202
*            RBCODE12 = '1'B;          /* INDICATE ALLOC FAILED      */
         OI    RBCODE12(R3AWAPTR),B'10000000'                      0202
*            ENTRYCD = 1;              /* INDICATE ALLOC ERROR       */
         MVC   ENTRYCD(2,R3AWAPTR),@HW1                            0203
*            R1 = AWAPTR;              /* WORKAREA PTR IN REG 1      */
         LR    R1,R3AWAPTR                                         0204
*            CALL IKJEFD35;            /* ISSUE ERROR MESSAGE        */
         L     R15,VCONSD35                                        0205
         BALR  R14,R15                                             0205
*          END;                                                    0206
*    END VALIDRSP;                                                 0207
@EL00003 DS    0H                                                  0207
@EF00003 DS    0H                                                  0207
@ER00003 LM    R14,R12,@SA00003                                    0207
         BR    R14                                                 0207
*/********************************************************************/
*/*                                                                  */
*/* SEGMENT NAME -                                                   */
*/*      DYNSVC                                                      */
*/*                                                                  */
*/* FUNCTION -                                                       */
*/*      THIS ROUTINE INVOKES DYNAMIC ALLOCATION                     */
*/*                                                                  */
*/* OPERATION -                                                      */
*/*      THE PARAMETER LIST IS COMPLETED. THE MACRO TO INVOKE SVC 99 */
*/*      IS ISSUED. THE RETURN CODE FROM DYNAMIC ALLOCATION IS       */
*/*      SAVED.                                                      */
*/*                                                                  */
*/* INPUT -                                                          */
*/*      SVC 99 REQUEST BLOCK                                        */
*/*                                                                  */
*/* OUTPUT -                                                         */
*/*      RETURN CODE                                                 */
*/*                                                                  */
*/********************************************************************/
*    DYNSVC: PROC;                                                 0208
*                                                                  0208
DYNSVC   STM   R14,R12,12(R13)                                     0208
*        R1 = ADDR(S99RBPTR);          /* PTR IN REGISTER 1          */
         LA    R1,S99RBPTR                                         0209
*        S99RBPTR = PTRS99RB;          /* PTR TO REQUEST BLOCK       */
         L     R10,PTRS99RB(,R3AWAPTR)                             0210
         ST    R10,S99RBPTR                                        0210
*        S99RBPND = '1'B;              /* HIGH ORDER BIT ON          */
*                                                                  0211
*                                                                  0211
         OI    S99RBPND,B'10000000'                                0211
*         /*MACDATE Y-2 73082*/                                    0212
*         SVC (99);                    /* ISSUE SVC 99               */
*                                                                  0212
         SVC   99                                                  0212
*        RCODESV = R15;                /* SAVE RETURN CODE           */
*                                                                  0213
         ST    R15,RCODESV(,R3AWAPTR)                              0213
*    END DYNSVC;                                                   0214
@EL00004 DS    0H                                                  0214
@EF00004 DS    0H                                                  0214
@ER00004 LM    R14,R12,12(R13)                                     0214
         BR    R14                                                 0214
*/********************************************************************/
*/*                                                                  */
*/* SEGMENT NAME -                                                   */
*/*      PUTMSG                                                      */
*/*                                                                  */
*/* FUNCTION -                                                       */
*/*      THIS ROUTINE INVOKES THE MESSAGE PROCESSOR, IKJEFF02.       */
*/*                                                                  */
*/* OPERATION -                                                      */
*/*      THE PARAMETER LIST TO IKJEFF02 IS COMPLETED. IKJEFF02 IS    */
*/*      LOADED IN, AND CALLED. THE RETURN CODE IN REGISTER 15 IS    */
*/*      SAVED. THE MESSAGE PROCESSOR IS DELETED.                    */
*/*                                                                  */
*/* INPUT -                                                          */
*/*      IKJEFF02 PARAMETER LIST                                     */
*/*                                                                  */
*/* OUTPUT -                                                         */
*/*      RETURN CODE  = 0 - PUTLINE/PUTGET SUCCESSFUL                */
*/*                  ^= 0 - PUTLINE/PUTGET FAILED                    */
*/*                                                                  */
*/********************************************************************/
*    PUTMSG: PROC;                                                 0215
PUTMSG   STM   R14,R12,@SA00005                                    0215
*        LISTPTR = ADDR(MSGCSECT);     /* ADDR OF PARM LIST          */
         L     R10,MSGPTR(,R3AWAPTR)                               0216
         LA    R2,MSGCSECT(,R10)                                   0216
         ST    R2,LISTPTR(,R10)                                    0216
*        MTCPPL = CPPLPTR;             /* PTR TO CPPL                */
         L     R2,CPPLPTR(,R3AWAPTR)                               0217
         ST    R2,MTCPPL(,R10)                                     0217
*        ECBPTR = ADDR(COMMECB);       /* COMMUNICATIONS ECB         */
         LA    R2,COMMECB(,R3AWAPTR)                               0218
         ST    R2,ECBPTR(,R10)                                     0218
*        COMMECB = 0;                  /* SET ECB TO 0               */
         SLR   R2,R2                                               0219
         ST    R2,COMMECB(,R3AWAPTR)                               0219
*        MTHIGH = '1'B;                /* HIGH ORDER BIT ON          */
         OI    MTHIGH(R10),B'10000000'                             0220
*        MSGCSECT = PTRMSGS;           /* PTR TO MSG CSECT           */
         L     R2,PTRMSGS(,R3AWAPTR)                               0221
         ST    R2,MSGCSECT(,R10)                                   0221
*        R1 = ADDR(MSGTABLE);          /* PARM LIST IN REG 1         */
*                                                                  0222
         LR    R1,R10                                              0222
*        GEN (LINK EP=IKJEFF02);       /* INVOKE MSG PROCESSOR       */
*                                                                  0223
         LINK EP=IKJEFF02
*        RCODESV = R15;                /* SAVE RETURN CODE           */
*                                                                  0224
         ST    R15,RCODESV(,R3AWAPTR)                              0224
*    END PUTMSG;                                                   0225
*                                                                  0225
*                                                                  0225
@EL00005 DS    0H                                                  0225
@EF00005 DS    0H                                                  0225
@ER00005 LM    R14,R12,@SA00005                                    0225
         BR    R14                                                 0225
*  DECLARE /*GENERAL PURPOSE REGISTERS */                          0226
*    GPR00P PTR(31) REG(0),                                        0226
*    GPR01P PTR(31) REG(1),                                        0226
*    GPR02P PTR(31) REG(2),                                        0226
*    GPR03P PTR(31) REG(3),                                        0226
*    GPR04P PTR(31) REG(4),                                        0226
*    GPR05P PTR(31) REG(5),                                        0226
*    GPR06P PTR(31) REG(6),                                        0226
*    GPR07P PTR(31) REG(7),                                        0226
*    GPR08P PTR(31) REG(8),                                        0226
*    GPR09P PTR(31) REG(9),                                        0226
*    GPR14P PTR(31) REG(14),                                       0226
*    GPR15P PTR(31) REG(15);                                       0226
*                                                                  0226
*  DECLARE /* COMMON VARIABLES */                                  0227
*    I256C CHAR(256) BASED,                                        0227
*    I031F FIXED(31) BASED,                                        0227
*    I031P PTR(31)   BASED,                                        0227
*    I015F FIXED(15) BASED,                                        0227
*    I015P PTR(15)   BASED,                                        0227
*    I008P PTR(8)    BASED,                                        0227
*    I001C CHAR(1)   BASED;                                        0227
*                                                                  0227
@DATA    DC    0H'0'
@HW1     DC    H'1'
@HW2     DC    H'2'
@HWDDNAM DC    XL2'0001'
@HWUNALC DC    XL2'0007'
@SM01407 MVC   PGINPUT(0),RETCHAR(R10)
@SC01409 CLC   CSOABUF(0,R10),@CHRFREE
@SC01411 CLC   CSOABUF(0,R1),@CHREND
@SM01414 MVC   FILEBUF(0),DDNBUF(R5)
@DATD    DSECT
         DS    0F
@SA00001 DS    18F
@PC00001 DS    1F
@SAV001  EQU   @SA00001
@SA00002 DS    15F
@SA00005 DS    15F
@SA00003 DS    15F
@TF00001 DS    F
@ZTEMPS  DS    C
@ZTEMPND EQU   *
@ZLEN    EQU   @ZTEMPND-@ZTEMPS
@TEMPS   EQU   @ZTEMPS
@L       EQU   @ZLEN
IKJEFD36 CSECT
         DC    0F'0'
@CF00321 DC    F'3'
@FW4     DC    F'4'
@HW4     EQU   @FW4+2
@FW12    DC    F'12'
@DATD    DSECT
         DS    0D
S99RBPTR DS    AL4
         ORG   S99RBPTR
S99RBPND DS    BL1
         ORG   S99RBPTR+4
S99TUFP  DS    A
PRMFLG   DS    CL1
         ORG   PRMFLG
NULLRSP  DS    BL1
@NM00001 EQU   PRMFLG+0
         ORG   PRMFLG+1
         DS    CL3
PGBUFFR  DS    CL22
         ORG   PGBUFFR
PGLENGH  DS    FL2
PGOFFST  DS    FL2
PGINPUT  DS    CL18
         ORG   PGBUFFR+22
         DS    CL2
FLAGWORD DS    CL4
         ORG   FLAGWORD
SYNCHK   DS    BL1
@NM00011 EQU   FLAGWORD+0
@NM00012 DS    AL3
         ORG   FLAGWORD+4
SCANAREA DS    CL24
SCANOUT  DS    CL8
PRMTEXT  DS    CL26
         ORG   PRMTEXT
FILEADDR DS    AL4
PERMADDR DS    AL4
         ORG   PERMADDR
ENDPRMT  DS    BL1
         ORG   PRMTEXT+8
FILEKEY  DS    FL2
FILENUM  DS    FL2
FILELEN  DS    FL2
FILEBUF  DS    CL8
PERMKEY  DS    FL2
PERMNUM  DS    FL2
         ORG   PRMTEXT+26
SAVEFLG  DS    CL1
         ORG   *+1-(*-@DATD)/(*-@DATD) ENSURE DSECT DATA
@ENDDATD EQU   *
@DATEND  EQU   *
IKJEFD36 CSECT
         DC    0F'0'
@SIZDATD DC    AL1(1)
         DC    AL3(@ENDDATD-@DATD)
@SIZ001  EQU   @SIZDATD
VCONSD35 DC    V(IKJEFD35)
         DC    0D'0'
@CHR113A DC    C'113A'
@CHRFREE DC    C'FREE'
@CHR115B DC    C'115B'
@CHR115A DC    C'115A'
@CHR116A DC    C'116A'
@CHREND  DC    C'END'
         DC    0F'0'
PATCH36  DC    25F'0'
         DC    0D'0'                   END OF CSECT                @26A
R0       EQU   00                      EQUATES FOR REGISTERS 0-15
R1       EQU   01
R2       EQU   02
R3       EQU   03
R4       EQU   04
R5       EQU   05
R6       EQU   06
R7       EQU   07
R8       EQU   08
R9       EQU   09
R10      EQU   10
R11      EQU   11
R12      EQU   12
R13      EQU   13
R14      EQU   14
R15      EQU   15
SAVETXTP EQU   R4
PRMPTXTP EQU   R2
CSOAPTR  EQU   R2
CSPLPTR  EQU   R4
R3AWAPTR EQU   R3
REGSAVE  EQU   4
RETCODE  EQU   16
ALLOCWA  EQU   0
CPPLPTR  EQU   ALLOCWA
PTRPDL   EQU   ALLOCWA+4
SWITCH   EQU   ALLOCWA+12
RBCODE12 EQU   SWITCH
TERMOPT  EQU   SWITCH
SWITCH2  EQU   ALLOCWA+13                                          @26A
REUSE    EQU   SWITCH2                                             @26A
ENTRYCD  EQU   ALLOCWA+14
PTRS99RB EQU   ALLOCWA+20
RCODESV  EQU   ALLOCWA+24
PTRMSGS  EQU   ALLOCWA+32
VCFLAGS  EQU   ALLOCWA+36
MSGPTR   EQU   ALLOCWA+48
CMDTWO   EQU   ALLOCWA+60
COMMECB  EQU   ALLOCWA+160
ALLOCPDL EQU   0
DSNPDE   EQU   ALLOCPDL+48+4                                       @26C
DSNPTR   EQU   DSNPDE
DSNFLAGS EQU   DSNPDE+6
MBRPTR   EQU   DSNPDE+8
MBRFLAGS EQU   DSNPDE+14
PASSPTR  EQU   DSNPDE+16
PASSFLAG EQU   DSNPDE+22
DDNMEPDE EQU   ALLOCPDL+76+4                                       @26C
DDNPTR   EQU   DDNMEPDE
DDNLEN   EQU   DDNMEPDE+4
DDNFLAGS EQU   DDNMEPDE+6
CLASPDE  EQU   ALLOCPDL+84+4                                       @26C
CLASPTR  EQU   CLASPDE
CLASFLG  EQU   CLASPDE+6
SERPDE   EQU   ALLOCPDL+92+4                                       @26C
SERPTR   EQU   SERPDE
SERFLAGS EQU   SERPDE+6
BLKPDE   EQU   ALLOCPDL+104+4                                      @26C
BLKPTR   EQU   BLKPDE
BLKFLAGS EQU   BLKPDE+6
ABLKPDE  EQU   ALLOCPDL+112+4                                      @26C
ABLKPTR  EQU   ABLKPDE
ABLKFLG  EQU   ABLKPDE+6
SPACPDE  EQU   ALLOCPDL+120+4                                      @26C
SPACEPTR EQU   SPACPDE
SPACEFLG EQU   SPACPDE+6
SPACPDE2 EQU   ALLOCPDL+128+4                                      @26C
SPACPTR2 EQU   SPACPDE2
SPACFLG2 EQU   SPACPDE2+6
DIRECPDE EQU   ALLOCPDL+136+4                                      @26C
DIRPTR   EQU   DIRECPDE
DIRFLAGS EQU   DIRECPDE+6
ATTRPDE  EQU   ALLOCPDL+144+4                                      @26C
ATTRPTR  EQU   ATTRPDE
ATTRFLAG EQU   ATTRPDE+6
USIDPDE  EQU   ALLOCPDL+152+4                                      @26C
USIDPTR  EQU   USIDPDE
USIDFLAG EQU   USIDPDE+6
UTYPPDE  EQU   ALLOCPDL+160+4                                      @26C
UTYPPTR  EQU   UTYPPDE
UTYPFLAG EQU   UTYPPDE+6
UNCNTPDE EQU   ALLOCPDL+168+4                                      @26C
UCNTPTR  EQU   UNCNTPDE
UCNTFLAG EQU   UNCNTPDE+6
POSTPDE  EQU   ALLOCPDL+180+4                                      @26C
POSPTR   EQU   POSTPDE
POSFLAG  EQU   POSTPDE+6
MXVOLPDE EQU   ALLOCPDL+188+4                                      @26C
MXVOLPTR EQU   MXVOLPDE
MXVOLFLG EQU   MXVOLPDE+6
VOLSQPDE EQU   ALLOCPDL+196+4                                      @26C
VOLSQPTR EQU   VOLSQPDE
VOLSQFLG EQU   VOLSQPDE+6
VVGRPDE  EQU   ALLOCPDL+204+4                                      @26C
VVGPPTR  EQU   VVGRPDE
VVGPFLG  EQU   VVGRPDE+6
DSNBUF   EQU   0
DDNBUF   EQU   0
CPPL     EQU   0
CPPLUPT  EQU   CPPL+4
CPPLECT  EQU   CPPL+12
MSGTABLE EQU   0
LISTPTR  EQU   MSGTABLE
TMCTPTR  EQU   MSGTABLE+4
MTCPPL   EQU   TMCTPTR
ECBPTR   EQU   MSGTABLE+8
@NM00006 EQU   MSGTABLE+12
MTHIGH   EQU   @NM00006
MSGCSECT EQU   MSGTABLE+16
SW       EQU   MSGTABLE+20
MTPUTLSW EQU   SW
RETMSG   EQU   MSGTABLE+21
SW2      EQU   MSGTABLE+24
MSGID    EQU   MSGTABLE+40
MTINSRTS EQU   MSGTABLE+44
L1       EQU   MTINSRTS
VAR1     EQU   MTINSRTS+1
L2       EQU   MTINSRTS+4
L3       EQU   MTINSRTS+8
L4       EQU   MTINSRTS+12
RET      EQU   0
RETSIZE  EQU   RET
RETCHAR  EQU   RET+2
CSPL     EQU   0
CSPLUPT  EQU   CSPL
CSPLECT  EQU   CSPL+4
CSPLECB  EQU   CSPL+8
CSPLFLG  EQU   CSPL+12
CSPLOA   EQU   CSPL+16
CSPLCBUF EQU   CSPL+20
CSOA     EQU   0
CSOACNM  EQU   CSOA
CSOALNM  EQU   CSOA+4
CSOAFLG  EQU   CSOA+6
CSOANOC  EQU   CSOAFLG
CSOABUF  EQU   0
S99RB    EQU   0
S99VERB  EQU   S99RB+1
S99FLAG1 EQU   S99RB+2
S99FLG11 EQU   S99FLAG1
S99RSC   EQU   S99RB+4
S99ERROR EQU   S99RSC
S99TXTPP EQU   S99RB+8
S99FLAG2 EQU   S99RB+16
S99FLG21 EQU   S99FLAG2
S99FLG22 EQU   S99FLAG2+1
S99TUPL  EQU   0
S99TUPTR EQU   S99TUPL
S99TUP   EQU   0
S99TUNIT EQU   0
S99TUENT EQU   S99TUNIT+4
S99TUFLD EQU   0
ABLKBUF  EQU   0
ATTRBUF  EQU   0
BLKBUF   EQU   0
CLASSBUF EQU   0
DIRBUF   EQU   0
I001C    EQU   0
I008P    EQU   0
I015F    EQU   0
I015P    EQU   0
I031F    EQU   0
I031P    EQU   0
I256C    EQU   0
MBRBUF   EQU   0
MSGECB   EQU   0
MSGTMCT  EQU   0
MXVOLBUF EQU   0
PASSBUF  EQU   0
POSTBUF  EQU   0
SERBUF   EQU   0
SPACBUF2 EQU   0
SPACEBUF EQU   0
UCNTBUF  EQU   0
USIDBUF  EQU   0
UTYPBUF  EQU   0
VSEQBUF  EQU   0
VVGPBUF  EQU   0
INPARMS  EQU   0
*                                      START UNREFERENCED COMPONENTS
S99TUPRM EQU   S99TUFLD+2
S99TULEN EQU   S99TUFLD
S99TUPAR EQU   S99TUENT+2
S99TULNG EQU   S99TUENT
S99TUNUM EQU   S99TUNIT+2
S99TUKEY EQU   S99TUNIT
S99TUPND EQU   S99TUP
S99TUPLN EQU   S99TUPTR
S99FLG24 EQU   S99FLAG2+3
S99FLG23 EQU   S99FLAG2+2
S99PCINT EQU   S99FLG22
S99UDEVT EQU   S99FLG22
S99MOUNT EQU   S99FLG21
S99CATLG EQU   S99FLG21
S99TIONQ EQU   S99FLG21
S99OFFLN EQU   S99FLG21
S99WTUNT EQU   S99FLG21
S99NORES EQU   S99FLG21
S99WTDSN EQU   S99FLG21
S99WTVOL EQU   S99FLG21
S99RSV01 EQU   S99RB+12
S99INFO  EQU   S99RSC+2
S99FLG12 EQU   S99FLAG1+1
S99JBSYS EQU   S99FLG11
S99NOMNT EQU   S99FLG11
S99NOCNV EQU   S99FLG11
S99ONCNV EQU   S99FLG11
S99RBLN  EQU   S99RB
@NM00015 EQU   CSOA+7
@NM00014 EQU   CSOAFLG
@NM00013 EQU   CSOAFLG
CSOAEXEC EQU   CSOAFLG
CSOABAD  EQU   CSOAFLG
CSOAQM   EQU   CSOAFLG
CSOAVNP  EQU   CSOAFLG
CSOAVWP  EQU   CSOAFLG
MSGRTN   EQU   MSGTABLE+60
VAR4     EQU   MTINSRTS+13
HIGHL4   EQU   L4
VAR3     EQU   MTINSRTS+9
HIGHL3   EQU   L3
VAR2     EQU   MTINSRTS+5
HIGHL2   EQU   L2
HIGHL1   EQU   L1
@NM00010 EQU   MSGTABLE+36
@NM00009 EQU   MSGTABLE+32
MTOLDPTR EQU   MSGTABLE+28
@NM00008 EQU   MSGTABLE+25
@NM00007 EQU   SW2
MTPGMSW  EQU   SW2
MTNPLMSW EQU   SW2
MTNOXQSW EQU   SW2
MTDOMSW  EQU   SW2
MT2OLDSW EQU   SW2
MTNHEXSW EQU   SW
MTWTPSW  EQU   SW
MTJOBISW EQU   SW
MTKEY1SW EQU   SW
MTHEXSW  EQU   SW
MTWTOSW  EQU   SW
MTNOIDSW EQU   SW
CPPLPSCB EQU   CPPL+8
CPPLCBUF EQU   CPPL
DSNTERM  EQU   DSNBUF
VVGPRSV2 EQU   VVGRPDE+7
VVGPRSV1 EQU   VVGPFLG
VVGPRES  EQU   VVGPFLG
VVGPLEN  EQU   VVGRPDE+4
VOLRSV2  EQU   VOLSQPDE+7
VOLRSV1  EQU   VOLSQFLG
VOLPRES  EQU   VOLSQFLG
VOLSQLEN EQU   VOLSQPDE+4
MXVRSV2  EQU   MXVOLPDE+7
MXVRSV1  EQU   MXVOLFLG
MXVPRES  EQU   MXVOLFLG
MXVOLLEN EQU   MXVOLPDE+4
POSRSV2  EQU   POSTPDE+7
POSRSV1  EQU   POSFLAG
POSPRES  EQU   POSFLAG
POSLEN   EQU   POSTPDE+4
LABLRSV1 EQU   ALLOCPDL+178+4                                      @26C
LABELPDE EQU   ALLOCPDL+176+4                                      @26C
UCNTRSV2 EQU   UNCNTPDE+7
UCNTRSV1 EQU   UCNTFLAG
UCNTPRES EQU   UCNTFLAG
UCNTLEN  EQU   UNCNTPDE+4
UTYPRSV2 EQU   UTYPPDE+7
UTYPRSV1 EQU   UTYPFLAG
UTYPPRES EQU   UTYPFLAG
UTYPLEN  EQU   UTYPPDE+4
USIDRSV2 EQU   USIDPDE+7
USIDRSV1 EQU   USIDFLAG
USIDPRES EQU   USIDFLAG
USIDLEN  EQU   USIDPDE+4
ATTRRSV2 EQU   ATTRPDE+7
ATTRRSV1 EQU   ATTRFLAG
ATTRPRES EQU   ATTRFLAG
ATTRLEN  EQU   ATTRPDE+4
DIRRSV2  EQU   DIRECPDE+7
DIRRSV1  EQU   DIRFLAGS
DIRPRES  EQU   DIRFLAGS
DIRLEN   EQU   DIRECPDE+4
SPCRSV22 EQU   SPACPDE2+7
SPCRSV12 EQU   SPACFLG2
SPCPRES2 EQU   SPACFLG2
SPACLEN2 EQU   SPACPDE2+4
SPACRSV2 EQU   SPACPDE+7
SPACRSV1 EQU   SPACEFLG
SPACPRES EQU   SPACEFLG
SPACELEN EQU   SPACPDE+4
ABLKRSV2 EQU   ABLKPDE+7
ABLKRSV1 EQU   ABLKFLG
ABLKPRES EQU   ABLKFLG
ABLKLEN  EQU   ABLKPDE+4
BLKRSV2  EQU   BLKPDE+7
BLKRSV1  EQU   BLKFLAGS
BLKPRES  EQU   BLKFLAGS
BLKLEN   EQU   BLKPDE+4
SERNEXT  EQU   SERPDE+8
SERRSV2  EQU   SERPDE+7
SERRSV1  EQU   SERFLAGS
SERPRES  EQU   SERFLAGS
SERLEN   EQU   SERPDE+4
CLASRSV2 EQU   CLASPDE+7
CLASRSV1 EQU   CLASFLG
CLASPRES EQU   CLASFLG
CLASLEN  EQU   CLASPDE+4
DDNRSV2  EQU   DDNMEPDE+7
DDNRSV1  EQU   DDNFLAGS
DDNPRES  EQU   DDNFLAGS
DSNNEXT  EQU   DSNPDE+24
PASSRSV2 EQU   DSNPDE+23
PASSRSV1 EQU   PASSFLAG
PASSPRES EQU   PASSFLAG
PASSLEN  EQU   DSNPDE+20
MBRRSV2  EQU   DSNPDE+15
MBRRSV1  EQU   MBRFLAGS
MBRPRES  EQU   MBRFLAGS
MBRLEN   EQU   DSNPDE+12
DSNRSV2  EQU   DSNPDE+7
DSNRSV1  EQU   DSNFLAGS
DSNQUOT  EQU   DSNFLAGS
DSNPRES  EQU   DSNFLAGS
DSNLENGH EQU   DSNPDE+4
DISPPDE  EQU   ALLOCPDL+46
RNDPDE   EQU   ALLOCPDL+44
RLSEPDE  EQU   ALLOCPDL+42
VSEQPDE  EQU   ALLOCPDL+40
PRIVPDE  EQU   ALLOCPDL+38
MAXVPDE  EQU   ALLOCPDL+36
POSPDE   EQU   ALLOCPDL+34
LABLPDE  EQU   ALLOCPDL+32
UCNTPDE  EQU   ALLOCPDL+30
UNITPDE  EQU   ALLOCPDL+28
HOLDPDE  EQU   ALLOCPDL+26
DESTPDE  EQU   ALLOCPDL+24
USINGPDE EQU   ALLOCPDL+22
DIRPDE   EQU   ALLOCPDL+20
SPACEPDE EQU   ALLOCPDL+18
BLOKPDE  EQU   ALLOCPDL+16
VOLPDE   EQU   ALLOCPDL+14
STATSPDE EQU   ALLOCPDL+12
FILEPDE  EQU   ALLOCPDL+10
DSPDE    EQU   ALLOCPDL+8
@NM00005 EQU   ALLOCPDL+4
@NM00004 EQU   ALLOCPDL
FIRSTPTR EQU   ALLOCWA+176
PPLPTR   EQU   ALLOCWA+172
NBLKBUF1 EQU   ALLOCWA+164
PDLADDR3 EQU   ALLOCWA+156
PPLTWO   EQU   ALLOCWA+128
CMDTWOBF EQU   CMDTWO+4
CMDTWOOF EQU   CMDTWO+2
CMDTWOLN EQU   CMDTWO
Q        EQU   ALLOCWA+56
P        EQU   ALLOCWA+52
PRCODE   EQU   ALLOCWA+44
PMPTPDL  EQU   ALLOCWA+40
@NM00003 EQU   ALLOCWA+38
NCLASBUF EQU   ALLOCWA+37
@NM00002 EQU   VCFLAGS
SPACE2   EQU   VCFLAGS
SPACE1   EQU   VCFLAGS
AVBLK    EQU   VCFLAGS
DSNCTR   EQU   ALLOCWA+30
VLISTCTR EQU   ALLOCWA+28
TXT      EQU   ALLOCWA+16
*AWARSV2 EQU   ALLOCWA+13                                          @26D
FIRSTPDE EQU   SWITCH
AWARSV1  EQU   SWITCH
ASTRSK   EQU   SWITCH
CONT     EQU   SWITCH
PROMPT   EQU   SWITCH
DSOVRLAY EQU   SWITCH
GTPLPTR  EQU   ALLOCWA+8
*                                      END UNREFERENCED COMPONENTS
@RC00082 EQU   @EL00001
@RT00132 EQU   INVRSP
@RT00136 EQU   INVRSP
@RC00089 EQU   @EL00002
@RC00119 EQU   @EL00002
@RC00138 EQU   @EL00002
@RC00134 EQU   @EL00002
@RC00156 EQU   @EL00002
@RC00187 EQU   @EL00003
@RF00193 EQU   @RC00187
@EL01    EQU   @EL00001
@ENDDATA EQU   *
*    END IKJEFD36                                                  0228
         PRINT NOGEN
*/* THE FOLLOWING INCLUDE STATEMENTS WERE FOUND IN THIS PROGRAM.      *
*/*%INCLUDE SYSLIB  (IKJZT430)                                        *
*/*%INCLUDE SYSLIB  (IKJCPPL )                                        *
*/*%INCLUDE SYSLIB  (IKJEFFMT)                                        *
*/*%INCLUDE SYSLIB  (IKJCSPL )                                        *
*/*%INCLUDE SYSLIB  (IKJCSOA )                                        *
*/*%INCLUDE SYSLIB  (IEFZB4D0)                                        *
*/*%INCLUDE SYSLIB  (IEFZB4D2)                                        *
*/*%INCLUDE SYSLIB  (CVT     )                                        *
         CVT   DSECT=YES
*;                                                                 0228
         END   IKJEFD36,(C'PLS0315',0603,74204)
/*
//*
//STEP15  EXEC PGM=IEBGENER
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  *
  IDENTIFY IKJEFD36('ZP60026')
++MOD(IKJEFD37) DISTLIB(AOST4).
/*
//SYSUT2   DD  DSN=&&SMPMCS,DISP=(MOD,PASS)
//SYSIN    DD  DUMMY
//*
//STEP16  EXEC PGM=IFOX00,PARM='OBJECT,NODECK,NOTERM,XREF(SHORT)'
//SYSPRINT DD  SYSOUT=*
//SYSLIB   DD  DSN=SYS1.MACLIB,DISP=SHR
//         DD  DSN=SYS1.SMPMTS,DISP=SHR
//         DD  DSN=SYS1.AMODGEN,DISP=SHR
//SYSUT1   DD  UNIT=SYSALLDA,SPACE=(CYL,10)
//SYSUT2   DD  UNIT=SYSALLDA,SPACE=(CYL,10)
//SYSUT3   DD  UNIT=SYSALLDA,SPACE=(CYL,10)
//SYSGO    DD  DSN=&&SMPMCS,DISP=(MOD,PASS)
//SYSIN    DD  *
         TITLE 'ALLOCATE VALIDITY CHECK EXITS                          *
                        '
***********************************************************************
*  $26   ZP60026  2010-01-23 GREG PRICE: ADD 'REUSE' OPERAND.         *
*                            CHANGES APPLIED TO UZ65229 LEVEL.        *
***********************************************************************
IKJEFD37 CSECT ,                                                   0001
@MAINENT DS    0H                                                  0001
         USING *,R15                                               0001
         B     @PROLOG                                             0001
         DC    AL1(33)                                             @26C
         DC    C'IKJEFD37  83.145'                                 0001
         DC    CL17' ZP60026 2010.023'                             @26A
         DROP  R15
@PROLOG  STM   R14,R12,12(R13)                                     0001
         BALR  R12,0                                               0001
@PSTART  DS    0H                                                  0001
         USING @PSTART,R12                                         0001
         L     R0,@SIZDATD                                         0001
         GETMAIN  R,LV=(0)
         LR    R11,R1                                              0001
         USING @DATD,R11                                           0001
         ST    R13,@SA00001+4                                      0001
         LM    R0,R1,20(R13)                                       0001
         ST    R11,8(,R13)                                         0001
         LR    R13,R11                                             0001
**  OLD  XC    @ZTEMPS(@ZLEN),@ZTEMPS
*        AWAPTR = R1;                  /* SAVE PTR TO WORKAREA       */
         LR    R6AWAPTR,R1                                         0072
*        PPLUWA = ADDR(SAVEVAL);       /* USER WORD CONTAINS REGS    */
         L     R7,PPLPTR(,R6AWAPTR)                                0073
         LA    R10,SAVEVAL                                         0073
         ST    R10,PPLUWA(,R7)                                     0073
*        GEN (STM 2,12,SAVEVAL);       /* STORE REGISTERS            */
         STM 2,12,SAVEVAL
*        PPLCBUF = CPPLCBUF;           /* CMD BUFFER IN PPL          */
         L     R7,PPLPTR(,R6AWAPTR)                                0075
         L     R10,CPPLPTR(,R6AWAPTR)                              0075
         L     R10,CPPLCBUF(,R10)                                  0075
         ST    R10,PPLCBUF(,R7)                                    0075
*        IF ENTRYCD = 1 THEN           /* IF MAIN CALL TO PARSE,     */
         CLC   ENTRYCD(2,R6AWAPTR),@HW1                            0076
         BNE   @RF00076                                            0076
*          DO;                         /* THEN                       */
*            PPLPCL = ADCONPCL;        /* PUT PCL ADDRESS IN PPL     */
         L     R10,ADCONPCL                                        0078
         ST    R10,PPLPCL(,R7)                                     0078
*            PPLANS = ADDR(PDLADDR);   /* PUT PDL ADDRESS IN PPL     */
         LA    R10,PDLADDR                                         0079
         ST    R10,PPLANS(,R7)                                     0079
*            PDLADDR = 0;              /* SET PDL ADDRESS TO 0       */
         SLR   R7,R7                                               0080
         ST    R7,PDLADDR                                          0080
*          END;                                                    0081
*        ELSE                          /* ELSE,                      */
*          DO;                         /* IF PROMPT CALL TO PARSE,   */
         B     @RC00076                                            0082
@RF00076 DS    0H                                                  0083
*             PPLANS = ADDR(PDLADDR2); /* PUT PDL ADDRESS IN PPL     */
         L     R10,PPLPTR(,R6AWAPTR)                               0083
         LA    R7,PDLADDR2                                         0083
         ST    R7,PPLANS(,R10)                                     0083
*             PDLADDR2 = 0;            /* SET PDL ADDR TO 0          */
         SLR   R7,R7                                               0084
         ST    R7,PDLADDR2                                         0084
*             IF ENTRYCD = 2 THEN      /* IF STATUS PROMPT,          */
         CLC   ENTRYCD(2,R6AWAPTR),@HW2                            0085
         BNE   @RF00085                                            0085
*               PPLPCL = ADCNPCLF;     /* SET PCL                    */
         L     R7,ADCNPCLF                                         0086
         ST    R7,PPLPCL(,R10)                                     0086
*             ELSE                                                 0087
*             IF ENTRYCD = 3 THEN      /* IF DSNAME PROMPT,          */
         B     @RC00085                                            0087
@RF00085 CLC   ENTRYCD(2,R6AWAPTR),@HW3                            0087
         BNE   @RF00087                                            0087
*               PPLPCL = ADCNPCLA;     /* SET PCL                    */
         L     R10,PPLPTR(,R6AWAPTR)                               0088
         L     R7,ADCNPCLA                                         0088
         ST    R7,PPLPCL(,R10)                                     0088
*             ELSE                                                 0089
*             IF ENTRYCD = 4 THEN      /* IF FILENAME PROMPT,        */
         B     @RC00087                                            0089
@RF00087 CLC   ENTRYCD(2,R6AWAPTR),@HW4                            0089
         BNE   @RF00089                                            0089
*               PPLPCL = ADCNPCLE;     /* SET PCL                    */
         L     R10,PPLPTR(,R6AWAPTR)                               0090
         L     R7,ADCNPCLE                                         0090
         ST    R7,PPLPCL(,R10)                                     0090
*             ELSE                                                 0091
*             IF ENTRYCD = 5 THEN      /* IF SPACE PROMPT,           */
         B     @RC00089                                            0091
@RF00089 CLC   ENTRYCD(2,R6AWAPTR),@HW5                            0091
         BNE   @RF00091                                            0091
*               PPLPCL = ADCNPCLB;     /* SET PCL                    */
         L     R10,PPLPTR(,R6AWAPTR)                               0092
         L     R7,ADCNPCLB                                         0092
         ST    R7,PPLPCL(,R10)                                     0092
*             ELSE                                                 0093
*             IF ENTRYCD = 6 THEN      /* IF DIR PROMPT,             */
         B     @RC00091                                            0093
@RF00091 CLC   ENTRYCD(2,R6AWAPTR),@HW6                            0093
         BNE   @RF00093                                            0093
*               PPLPCL = ADCNPCLD;     /* SET PCL                    */
         L     R10,PPLPTR(,R6AWAPTR)                               0094
         L     R7,ADCNPCLD                                         0094
         ST    R7,PPLPCL(,R10)                                     0094
*             ELSE                                                 0095
*             IF ENTRYCD = 7 THEN      /* IF BLOCK PROMPT,           */
         B     @RC00093                                            0095
@RF00093 CLC   ENTRYCD(2,R6AWAPTR),@HW7                            0095
         BNE   @RF00095                                            0095
*               PPLPCL = ADCNPCLC;     /* SET PCL                    */
         L     R10,PPLPTR(,R6AWAPTR)                               0096
         L     R7,ADCNPCLC                                         0096
         ST    R7,PPLPCL(,R10)                                     0096
*           END;                                                   0097
@RF00095 DS    0H                                                  0097
@RC00093 DS    0H                                                  0097
@RC00091 DS    0H                                                  0097
@RC00089 DS    0H                                                  0097
@RC00087 DS    0H                                                  0097
@RC00085 DS    0H                                                  0098
*        VCFLAGS = '00'X;              /* INITIALIZE FLAGS           */
@RC00076 MVI   VCFLAGS(R6AWAPTR),X'00'                             0098
*        R1 = PPLPTR;                  /* PPL IN REG 1               */
         L     R1,PPLPTR(,R6AWAPTR)                                0099
*          DO; /*CALLTSSR EP(IKJPARS)*/                            0100
*            RESPECIFY GPR01P RSTD;                                0101
*            IF CVTPARS = 0                                        0102
*              THEN                                                0102
         L     R7,CVTPTR                                           0102
         L     R10,CVTPARS-CVT(,R7)                                0102
         LTR   R10,R10                                             0102
         BNZ   @RF00102                                            0102
*                GEN(LINK EP=IKJPARS);                             0103
         LINK EP=IKJPARS
*              ELSE                                                0104
*                CALL CVTPARS;                                     0104
         B     @RC00102                                            0104
@RF00102 L     R7,CVTPTR                                           0104
         L     R15,CVTPARS-CVT(,R7)                                0104
         BALR  R14,R15                                             0104
*            RESPECIFY GPR01P UNRSTD;                              0105
@RC00102 DS    0H                                                  0106
*          END;       /* INVOKE PARSE       @Y30NQKK*/             0106
*                                                                  0106
*        IF R15 > 0 THEN               /* IF PARSE FAILED            */
         SLR   R7,R7                                               0107
         CR    R15,R7                                              0107
         BNH   @RF00107                                            0107
*          DO;                         /* AND                        */
*            IF RCODESV = 0 THEN       /* IF NO ERROR IN VAL CHECK   */
         C     R7,RCODESV(,R6AWAPTR)                               0109
         BNE   @RF00109                                            0109
*              RCODESV = R15;          /* SAVE RETURN CODE           */
         ST    R15,RCODESV(,R6AWAPTR)                              0110
*          END;                                                    0111
*        ELSE                          /* ELSE                       */
*          RCODESV = R15;              /* SAVE GOOD RETURN CODE      */
         B     @RC00107                                            0112
@RF00107 ST    R15,RCODESV(,R6AWAPTR)                              0112
*        IF ENTRYCD = 1 THEN           /* IF MAIN CALL TO PARSE,     */
@RC00107 CLC   ENTRYCD(2,R6AWAPTR),@HW1                            0113
         BNE   @RF00113                                            0113
*          PTRPDL = PDLADDR;           /* SAVE MAIN PDL ADDRESS      */
         L     R7,PDLADDR                                          0114
         ST    R7,PTRPDL(,R6AWAPTR)                                0114
*        ELSE                          /* ELSE                       */
*          PMPTPDL = PDLADDR2;         /* SAVE PROMPT PDL            */
*                                                                  0115
         B     @RC00113                                            0115
@RF00113 L     R7,PDLADDR2                                         0115
         ST    R7,PMPTPDL(,R6AWAPTR)                               0115
*        RETURN;                       /* RETURN TO CALLER           */
@EL00001 L     R13,4(,R13)                                         0116
@EF00001 L     R0,@SIZDATD                                         0116
         LR    R1,R11                                              0116
         FREEMAIN R,LV=(0),A=(1)
@ER00001 LM    R14,R12,12(R13)                                     0116
         BR    R14                                                 0116
*        GENERATE;                                                 0117
IKJEFD3Z  IKJPARM DSECT=PCL
DSKEYWRD  IKJKEYWD
          IKJNAME  'DATASET',SUBFLD=DSNAME,ALIAS='DSNAME'      @Y30NQKK
          IKJNAME  'DUMMY'
FILE      IKJKEYWD
          IKJNAME  'FILE',SUBFLD=DDNAMFLD,ALIAS='DDNAME'       @Y30NQKK
STATUS    IKJKEYWD
          IKJNAME  'OLD'
          IKJNAME  'SHR'
          IKJNAME  'MOD'
          IKJNAME  'NEW'
          IKJNAME  'SYSOUT',SUBFLD=SYSCLASS
VOLUME    IKJKEYWD
          IKJNAME  'VOLUME',SUBFLD=SERIAL
*                                                              @Y30LPKH
          IKJNAME  'MSVGP',SUBFLD=VVIDENT
BLOCK     IKJKEYWD
          IKJNAME  'BLOCK',SUBFLD=BLKSIZE,ALIAS='BLKSIZE'      @Y30NQKK
          IKJNAME  'AVBLOCK',SUBFLD=ABLKSIZE
          IKJNAME  'TRACKS'
          IKJNAME  'CYLINDERS'
SPACE     IKJKEYWD
          IKJNAME  'SPACE',SUBFLD=QUANT
DIR       IKJKEYWD
          IKJNAME  'DIR',SUBFLD=INTEGER
USING     IKJKEYWD
          IKJNAME  'USING',SUBFLD=ATTRNAME
DEST      IKJKEYWD
          IKJNAME  'DEST',SUBFLD=USERID
HOLD      IKJKEYWD  DEFAULT='NOHOLD'
          IKJNAME  'HOLD'
          IKJNAME  'NOHOLD'
UNIT      IKJKEYWD
          IKJNAME  'UNIT',SUBFLD=UNITYPE
*                                                              @YM07624
UCOUNT    IKJKEYWD
          IKJNAME  'UCOUNT',SUBFLD=UNCNTSUB
          IKJNAME  'PARALLEL'
LABEL     IKJKEYWD
          IKJNAME  'LABEL',SUBFLD=LBLTYPE
POSITION  IKJKEYWD
          IKJNAME  'POSITION',SUBFLD=DSSEQ
MAXVOL    IKJKEYWD
          IKJNAME  'MAXVOL',SUBFLD=MXVOLSUB
PRIVATE   IKJKEYWD
          IKJNAME  'PRIVATE'
VSEQ      IKJKEYWD
          IKJNAME  'VSEQ',SUBFLD=VSSEQSUB
RELSE     IKJKEYWD
          IKJNAME  'RELEASE'
ROUND     IKJKEYWD
          IKJNAME  'ROUND'
DISP      IKJKEYWD
          IKJNAME  'KEEP'
          IKJNAME  'DELETE'
          IKJNAME  'CATALOG'
          IKJNAME  'UNCATALOG'
REUSE     IKJKEYWD ,                                               @26A
          IKJNAME  'REUSE'                                         @26A
DSNAME    IKJSUBF
*                                                              @YM02616
DSNAMES   IKJPOSIT  DSTHING,LIST,USID,                                 X
               PROMPT='DATA SET NAME OR *',                            X
               HELP='NAME OF DATA SET TO BE ALLOCATED',                X
               VALIDCK=SYSDSN
DDNAMFLD  IKJSUBF
DDNMFLDS  IKJIDENT  'FILE NAME',                                       X
               MAXLNTH=8,                                              X
               FIRST=ALPHA,                                            X
               OTHER=ALPHANUM,                                         X
               PROMPT='FILE NAME',                                     X
               HELP='FILE NAME TO BE USED'
SYSCLASS  IKJSUBF
SYSOUTC   IKJIDENT  'SYSOUT CLASS',                                    X
               MAXLNTH=1,                                              X
               FIRST=NONATNUM,                                         X
               HELP='SYSTEM OUTPUT CLASS'
SERIAL    IKJSUBF
SERIALS   IKJIDENT  'VOLUME SERIAL',LIST,                              X
               FIRST=ALPHANUM,                                         X
               OTHER=ALPHANUM,                                         X
               MAXLNTH=6,                                              X
               PROMPT='VOLUME SERIAL NUMBER',                          X
               HELP='VOLUME SERIAL NUMBER ON WHICH DATA SET RESIDES',  X
               VALIDCK=SYSVLIST
BLKSIZE   IKJSUBF
BLKSIZES  IKJIDENT  'BLOCK SIZE',                                      X
               MAXLNTH=8,                                              X
               FIRST=NUMERIC,                                          X
               OTHER=NUMERIC,                                          X
               PROMPT='BLOCK SIZE',                                    X
               HELP='LENGTH OF OUTPUT RECORDS',                        X
               VALIDCK=SYSBLOCK
ABLKSIZE  IKJSUBF
ABLKSUB   IKJIDENT  'AVERAGE RECORD LENGTH',                           X
               MAXLNTH=8,                                              X
               FIRST=NUMERIC,                                          X
               OTHER=NUMERIC,                                          X
               VALIDCK=SYSABLOK,                                       X
               PROMPT='AVERAGE RECORD LENGTH',                         X
               HELP='LENGTH OF OUTPUT RECORDS'
QUANT     IKJSUBF
QUANTITY  IKJIDENT  'SPACE VALUE',                                     X
               MAXLNTH=8,                                              X
               FIRST=NUMERIC,                                          X
               OTHER=NUMERIC,                                          X
               PROMPT='SPACE VALUE AND IF ANY, INCREMENT VALUE',       X
               HELP='NUMBER OF RECORDS',                               X
               VALIDCK=SYSPACE1
QUANTIT2  IKJIDENT  'INCREMENT VALUE',                                 X
               MAXLNTH=8,                                              X
               FIRST=NUMERIC,                                          X
               OTHER=NUMERIC,                                          X
               VALIDCK=SYSPACE2
INTEGER   IKJSUBF
INTEGERS  IKJIDENT  'DIRECTORY QUANTITY',                              X
               MAXLNTH=8,                                              X
               FIRST=NUMERIC,                                          X
               OTHER=NUMERIC,                                          X
               PROMPT='NUMBER OF DIRECTORY BLOCKS',                    X
               HELP='NUMBER OF BLOCKS TO BE USED',                     X
               VALIDCK=SYSDIR
ATTRNAME  IKJSUBF
ATTRNAMS  IKJIDENT  'ATTR-LIST-NAME',                                  X
               MAXLNTH=8,                                              X
               FIRST=ALPHA,                                            X
               OTHER=ALPHANUM,                                         X
               PROMPT='ATTR-LIST-NAME',                                X
               HELP='ATTR-LIST-NAME TO BE USED'
USERID    IKJSUBF
USERIDS   IKJIDENT  'ID IN THE DEST SUBFIELD',                         X
               MAXLNTH=7,                                              X
               FIRST=ALPHANUM,                                         X
               OTHER=ALPHANUM,                                         X
               PROMPT='ID FOR THE DEST SUBFIELD',                      X
               HELP='ID OF A REMOTE USER OR DEVICE'
UNITYPE   IKJSUBF
UNTYPES   IKJIDENT  'DEVICE TYPE',                                     X
               MAXLNTH=8,                                              X
               FIRST=ANY,                                              X
               OTHER=ANY,                                              X
               PROMPT='DEVICE TYPE TO BE ALLOCATED',                   X
               HELP='GENERIC DEVICE NAME, ESOTERIC DEVICE NAME, OR DEVIX
               CE ADDRESS'
UNCNTSUB  IKJSUBF
UCNTSUBS  IKJIDENT  'UNIT COUNT',                                      X
               MAXLNTH=2,                                              X
               FIRST=NUMERIC,                                          X
               OTHER=NUMERIC,                                          X
               PROMPT='UNIT COUNT',                                    X
               HELP='NUMBER OF DEVICES TO BE ALLOCATED',               X
               VALIDCK=SYSUCNT
LBLTYPE   IKJSUBF
LBLTYPES  IKJKEYWD  DEFAULT='SL'
          IKJNAME  'SL'
          IKJNAME  'SUL'
          IKJNAME  'AL'
          IKJNAME  'AUL'
          IKJNAME  'NL'
          IKJNAME  'NSL'
          IKJNAME  'LTM'
          IKJNAME  'BLP'
DSSEQ     IKJSUBF
DSSEQSUB  IKJIDENT  'DATA SET SEQUENCE NUMBER',                        X
               MAXLNTH=4,                                              X
               FIRST=NUMERIC,                                          X
               OTHER=NUMERIC,                                          X
               PROMPT='DATA SET SEQUENCE NUMBER',                      X
               HELP='RELATIVE POSITION OF DATA SET'
MXVOLSUB  IKJSUBF
MXVOLS    IKJIDENT  'VOLUME COUNT',                                    X
               MAXLNTH=3,                                              X
               FIRST=NUMERIC,                                          X
               OTHER=NUMERIC,                                          X
               PROMPT='VOLUME COUNT',                                  X
               HELP='MAXIMUM NUMBER OF VOLUMES',                       X
               VALIDCK=SYSMAX
VSSEQSUB  IKJSUBF
VSSEQSB   IKJIDENT  'VOLUME SEQUENCE NUMBER',                          X
               MAXLNTH=3,                                              X
               FIRST=NUMERIC,                                          X
               OTHER=NUMERIC,                                          X
               PROMPT='VOLUME SEQUENCE NUMBER',                        X
               VALIDCK=SYSMAX
*                                                              @Y30LPKH
VVIDENT  IKJSUBF
VVGPSUB  IKJIDENT   'MASS STORAGE GROUP',                              X
               MAXLNTH=8,                                              X
               FIRST=ALPHANUM,                                         X
               OTHER=ALPHANUM,                                         X
               PROMPT='MASS STORAGE GROUP',                            X
               HELP='MASS STORAGE GROUP ON WHICH DATA SET IS TO RESIDE'
         IKJENDP
ADCONPCL  DC A(IKJEFD3Z)
*        GENERATE;                                                 0118
* THE FOLLOWING PARAMETER CONTROL LIST IS USED WHEN IT IS NECESSARY
* TO PROMPT FOR THE DATA SET NAME
IKJEFD3A  IKJPARM DSECT=PCLA
*                                                              @YM02616
DSN       IKJPOSIT DSNAME,PROMPT='DATA SET NAME',LIST,USID,            X
               HELP='NAME OF DATA SET TO BE ALLOCATED'
          IKJENDP
ADCNPCLA  DC A(IKJEFD3A)
*        GENERATE;                                                 0119
* THE FOLLOWING IS THE PCL THAT IS USED FOR THE BLOCK PARAMETER WHEN
* IT MUST BE OBTAINED AFTER PARSE HAS FINISHED ITS INITIAL SCAN
IKJEFD3C  IKJPARM DSECT=PCLC
PBLOK     IKJTERM  'UNIT OF SPACE KEYWORD',                            X
               TYPE=VAR,SBSCRPT=PBLOKSUB,                              X
               PROMPT='UNIT OF SPACE KEYWORD WITH VALUE, IF ANY',      X
               HELP='BLOCK, BLKSIZE, AVBLOCK, TRACKS OR CYLINDERS',    X
               VALIDCK=SYSPBLK
PBLOKSUB  IKJTERM  'BLOCK OR AVBLOCK VALUE',                           X
               TYPE=CNST,SBSCRPT
          IKJENDP
ADCNPCLC  DC A(IKJEFD3C)
*        GENERATE;                                                 0120
* THE FOLLOWING PCL IS USED TO SCAN THE VALUE ENTERED ON THE UNIT
* OF SPACE PROMPT FOR A VALID PARAMETER
IKJEFD3Y  IKJPARM DSECT=PCLY
BLOKKY    IKJKEYWD
          IKJNAME  'BLOCK',SUBFLD=BLOKSUB,ALIAS='BLKSIZE'      @Y30NQKK
          IKJNAME  'AVBLOCK',SUBFLD=ABLOKSUB
          IKJNAME  'TRACKS'
          IKJNAME  'CYLINDERS'
BLOKSUB   IKJSUBF
BLOKS     IKJIDENT  'BLOCK SIZE',                                      X
               MAXLNTH=8,FIRST=NUMERIC,OTHER=NUMERIC,                  X
               PROMPT='BLOCK SIZE'
ABLOKSUB  IKJSUBF
ABLOKS    IKJIDENT  'AVERAGE RECORD LENGTH',                           X
               MAXLNTH=8,FIRST=NUMERIC,OTHER=NUMERIC,                  X
               PROMPT='AVERAGE RECORD LENGTH'
          IKJENDP
ADCNPCLY  DC   A(IKJEFD3Y)
*        GENERATE;                                                 0121
* THE FOLLOWING IS THE PCL THAT IS USED FOR THE SPACE PARAMETER WHEN IT
* MUST BE OBTAINED AFTER PARSE HAS FINISHED ITS INITIAL SCAN
IKJEFD3B  IKJPARM DSECT=PCLB
SPACEP    IKJIDENT  'SPACE VALUE',MAXLNTH=8,FIRST=NUMERIC,             X
               OTHER=NUMERIC,PROMPT='SPACE VALUE AND IF ANY, SPACE INCRX
               EMENT VALUE',HELP='NUMBER OF RECORDS',VALIDCK=SYSPACE1
SPACES    IKJIDENT  'SPACE INCREMENT VALUE', MAXLNTH=8,                X
               FIRST=NUMERIC,OTHER=NUMERIC,VALIDCK=SYSPACE2
          IKJENDP
ADCNPCLB  DC A(IKJEFD3B)
*        GENERATE;                                                 0122
* THE FOLLOWING PCL IS USED TO PROMPT FOR THE DIRECTORY QUANTITY WHEN
* A MEMBER NAME IS GIVEN WITHOUT A DIRECTORY AMOUNT
IKJEFD3D IKJPARM DSECT=PCLD
DIRECT   IKJIDENT  'DIRECTORY QUANTITY',MAXLNTH=8,FIRST=NUMERIC,       X
               OTHER=NUMERIC,PROMPT='NUMBER OF DIRECTORY BLOCKS',      X
               HELP='NUMBER OF BLOCKS TO BE USED',VALIDCK=SYSDIR
         IKJENDP
ADCNPCLD  DC A(IKJEFD3D)
*        GENERATE;                                                 0123
* THE FOLLOWING IS THE PCL USED TO PROMPT FOR STATUS
IKJEFD3F   IKJPARM  DSECT=PCLF
STATUS1  IKJTERM 'STATUS OR SYSOUT KEYWORD',                           X
               TYPE=VAR,SBSCRPT=STATSUB,                               X
               PROMPT='STATUS OR SYSOUT KEYWORD',                      X
               HELP='STATUS OF OLD, MOD, SHR, OR NEW, OR SYSOUT',      X
               VALIDCK=SYSTATS
STATSUB  IKJTERM  'OUTPUT CLASS',                                      X
               TYPE=ANY,SBSCRPT
          IKJENDP
ADCNPCLF DC A(IKJEFD3F)
*        GENERATE;                                                 0124
* THE FOLLOWING IS THE PCL USED TO SCAN STATUS ON THE CALL TO
* PARSE FROM THE VALIDITY CHECK EXIT SYSTATS
IKJEFD3X IKJPARM DSECT=PCLX
STATKEY  IKJKEYWD
         IKJNAME 'OLD'
         IKJNAME 'SHR'
         IKJNAME 'MOD'
         IKJNAME 'NEW'
         IKJNAME 'SYSOUT',SUBFLD=OUTCLASS
OUTCLASS IKJSUBF
OUTCLASB IKJIDENT 'SYSOUT CLASS',MAXLNTH=1,FIRST=NONATNUM
         IKJENDP
ADCNPCLX DC A(IKJEFD3X)
*        GENERATE;                                                 0125
* THE FOLLOWING PCL IS USED TO PROMPT FOR DDNAME
IKJEFD3E  IKJPARM DSECT=PCLE
PDDNAME   IKJIDENT 'FILE NAME',MAXLNTH=8,FIRST=ALPHA,OTHER=ALPHANUM,   X
               PROMPT='FILE NAME'
          IKJENDP
ADCNPCLE  DC  A(IKJEFD3E)
*/********************************************************************/
*/*                                                                  */
*/* SEGMENT NAME -                                                   */
*/*      SYSBLOCK                                                    */
*/*                                                                  */
*/* FUNCTION -                                                       */
*/*      THIS VALIDITY CHECK ROUTINE CHECKS THE BLOCK AND            */
*/*      AVBLOCK VALUES FOR THE MAXIMUM.                             */
*/*                                                                  */
*/* OPERATION -                                                      */
*/*      TWO ENTRY POINTS ARE DEFINED TO INDICATE WHICH PARAMETER    */
*/*      IS BEING PROCESSED - SYSBLOCK AND SYSABLOK. BOTH ENTRY      */
*/*      POINTS INVOKE THE MAIN PROCESSOR, SYSBLK. THE PARAMETER IS  */
*/*      CONVERTED TO BINARY AND CHECKED AGAINST THE MAXIMUM OF      */
*/*      65535. IF GREATER, THE RETURN CODE IS SET TO 8 AND AN       */
*/*      ERROR MESSAGE IS ISSUED.                                    */
*/*                                                                  */
*/* INPUT -                                                          */
*/*      PTR TO PDE                                                  */
*/*                                                                  */
*/* OUTPUT -                                                         */
*/*      RETURN CODE = 0-VALID PARAMETER                             */
*/*                    8-INVALID PARAMETER, PARSE PROMPTS            */
*/*                                                                  */
*/* NOTE:  THIS ROUTINE REPRESENTS A DIRECT LINE FUNCTION AND CAN    */
*/*        BE INCORPORATED AS AN INCLUDED CODE SEGMENT WHEN THE      */
*/*        INCLUDE FUNCTION HAS RECEIVED A STANDARD DEFINITION.      */
*/********************************************************************/
*                                      /* ENTRY FOR BLOCK            */
*    SYSBLOCK: PROC OPTIONS(NOSAVEAREA);                           0126
         B     @EL00001                                            0126
SYSBLOCK STM   R14,R12,12(R13)                                     0126
*        R15 = R1->VALWORK;            /* STORED REGISTERS           */
         L     R15,VALWORK(,R1)                                    0127
*        GENERATE;                     /* SET UP ADDRESSABILITY      */
*                                                                  0128
         LM  2,12,0(15)                   RESTORE ALLOC REGISTERS
         LA  14,SAVEVC                    VC SAVER IN REG 14
         ST  14,8(13)                     VC SAVER IN PARSE SAVER
         ST  13,4(14)                     PARSE SAVER IN VC SAVER
         LR  13,14                        VC SAVER IN REG 13
*        AVBLK = '0'B;                 /* INDICATE BLOCK ENTERED     */
         NI    AVBLK(R6AWAPTR),B'01111111'                         0129
*        CALL SYSBLK;                  /* PROCESS BLOCK PARAMETER    */
         BAL   R14,SYSBLK                                          0130
*        GEN (L 13,4(13));                                         0131
         L 13,4(13)
*        PARSRTC = PRCODE;                                         0132
         L     R10,PRCODE(,R6AWAPTR)                               0132
         ST    R10,PARSRTC(,R13)                                   0132
*        RETURN;                       /* RETURN TO PARSE            */
@EL00002 DS    0H                                                  0133
@EF00002 DS    0H                                                  0133
@ER00002 LM    R14,R12,12(R13)                                     0133
         BR    R14                                                 0133
*     END SYSBLOCK;                                                0134
*                                      /* ENTRY FOR AVBLOCK          */
*    SYSABLOK: PROC OPTIONS(NOSAVEAREA);                           0135
SYSABLOK STM   R14,R12,12(R13)                                     0135
*        R15 = R1->VALWORK;            /* GET STORED REGISTERS       */
         L     R15,VALWORK(,R1)                                    0136
*        GENERATE;                     /* SET UP ADDRESSABILITY      */
*                                                                  0137
         LM  2,12,0(15)                   RESTORE ALLOC REGISTERS
         LA  14,SAVEVC                    VC SAVER IN REG 14
         ST  14,8(13)                     VC SAVER IN PARSE SAVER
         ST  13,4(14)                     PARSE SAVER IN VC SAVER
         LR  13,14                        VC SAVER IN REG 13
*        AVBLK = '1'B;                 /* INDICATE AVBLOCK ENTERED   */
         OI    AVBLK(R6AWAPTR),B'10000000'                         0138
*        CALL SYSBLK;                  /* PROCESS PARAMETER          */
         BAL   R14,SYSBLK                                          0139
*        AVBLK = '0'B;                 /* TURN OFF INDICATOR         */
         NI    AVBLK(R6AWAPTR),B'01111111'                         0140
*        GEN (L 13,4(13));                                         0141
         L 13,4(13)
*        PARSRTC = PRCODE;                                         0142
         L     R10,PRCODE(,R6AWAPTR)                               0142
         ST    R10,PARSRTC(,R13)                                   0142
*        RETURN;                       /* RETURN TO PARSE            */
@EL00003 DS    0H                                                  0143
@EF00003 DS    0H                                                  0143
@ER00003 LM    R14,R12,12(R13)                                     0143
         BR    R14                                                 0143
*     END SYSABLOK;                                                0144
*                                                                  0144
*    SYSBLK: PROC;                     /* COMMON BLOCK & AVBLOCK RTN */
*                                                                  0145
SYSBLK   STM   R14,R12,@SA00004                                    0145
*         VCKPTR = R1;                 /* PARM LIST FROM REG 1       */
         LR    R5VCKPTR,R1                                         0146
*         PRCODE = 0;                  /* INIT RETURN CODE           */
         SLR   R4,R4                                               0147
         ST    R4,PRCODE(,R6AWAPTR)                                0147
*         RCODESV = 0;                 /* INIT RET CODE IN WORKAREA  */
         ST    R4,RCODESV(,R6AWAPTR)                               0148
*         PKAREA(1) = 'F0'X;           /* 1ST BYTE IN PACKAREA F0    */
         MVI   PKAREA,X'F0'                                        0149
*         P = ADDR(PKAREA(1));         /* POINT TO 1ST BYTE          */
         LA    R7,PKAREA                                           0150
         ST    R7,P(,R6AWAPTR)                                     0150
*         Q = ADDR(PKAREA(2));         /* POINT TO 2ND BYTE          */
         LA    R10,PKAREA+1                                        0151
         ST    R10,Q(,R6AWAPTR)                                    0151
*         Q->AREA1 = P->AREA2;         /* CLEAR PACK AREA TO ALL F0  */
         MVC   AREA1(15,R10),AREA2(R7)                             0152
*         RFY Y RSTD;                  /* RESTRICT WORK REGISTER     */
*         Y = 8-VCBLKLEN+ADDR(PKAREA); /* MOVE VALUE INTO PACK AREA  */
         L     R4,VALPDE(,R5VCKPTR)                                0154
         LH    R7,VCBLKLEN(,R4)                                    0154
         LA    R8Y,8                                               0154
         SLR   R8Y,R7                                              0154
         LA    R10,PKAREA                                          0154
         ALR   R8Y,R10                                             0154
*         PKAREA1(1:VCBLKLEN) = VCBLKBUF(1:VCBLKLEN);              0155
         BCTR  R7,0                                                0155
         L     R4,VCBLKPTR(,R4)                                    0155
         EX    R7,@SM01301                                         0155
*         GENERATE REFS(CVBAREA);      /* PACK AND CONVERT TO BINARY */
         PACK  CVBAREA(8),PKAREA(8)
         CVB   R8Y,CVBAREA
*         IF Y > 65535 THEN            /* IF VALUE EXCEEDS MAXIMUM,  */
         C     R8Y,@FW4FOX                                         0157
         BNH   @RF00157                                            0157
*           DO;                        /* THEN,                      */
*             PRCODE = 8;              /* INDICATE PARSE PROMPT      */
         MVC   PRCODE(4,R6AWAPTR),@FW8                             0159
*             MSGID = '103A';          /* SET MESSAGE ID             */
         L     R7,MSGPTR(,R6AWAPTR)                                0160
         MVC   MSGID(4,R7),@CHR103A                                0160
*             IF AVBLK = '1'B THEN     /* IF PROCESSING AVBLOCK,     */
         TM    AVBLK(R6AWAPTR),B'10000000'                         0161
         BNO   @RF00161                                            0161
*               DO;                    /* THEN                       */
*                 VAR1 = ADDR(AVBLOCK);/* PTR TO AVBLOCK VARIABLE    */
         LA    R9,AVBLOCK                                          0163
** STCM  ST    R9,@TF00001                                         0163
** STCM  MVC   VAR1(3,R7),@TF00001+1                               0163
         STCM  R9,7,VAR1(R7)                                       0163
*                 L1 = LENGTH(AVBLOCK);/* AND LENGTH IN PARM LIST    */
         MVI   L1(R7),X'07'                                        0164
*               END;                                               0165
*             ELSE                     /* ELSE,                      */
*               DO;                    /* ASSUME BLOCK -             */
         B     @RC00161                                            0166
@RF00161 DS    0H                                                  0167
*                 VAR1 = ADDR(BLKVAR); /* PTR TO BLOCK VARIABLE      */
         L     R10,MSGPTR(,R6AWAPTR)                               0167
         LA    R4,BLKVAR                                           0167
** STCM  ST    R9,@TF00001                                         0167
** STCM  MVC   VAR1(3,R4),@TF00001+1                               0167
         STCM  R4,7,VAR1(R10)                                      0167
*                 L1 = LENGTH(BLKVAR); /* AND LENGTH IN PARM LIST    */
         MVI   L1(R10),X'0A'                                       0168
*               END;                                               0169
*             CALL VCPUTMSG;           /* ISSUE MESSAGE              */
@RC00161 BAL   R14,VCPUTMSG                                        0170
*             IF RCODESV > 0 THEN      /* IF MSG ROUTINE FAILED,     */
         L     R7,RCODESV(,R6AWAPTR)                               0171
         LTR   R7,R7                                               0171
         BNP   @RF00171                                            0171
*               PRCODE = 12;           /* INDICATE PARSE TERMINATE   */
         MVC   PRCODE(4,R6AWAPTR),@FW12                            0172
*             ELSE                     /* ELSE,                      */
*               VALMSG = ADDR(MSG01);  /* PASS PARSE 2ND LEVEL MSG   */
         B     @RC00171                                            0173
@RF00171 LA    R9,MSG01                                            0173
         ST    R9,VALMSG(,R5VCKPTR)                                0173
*           END;                                                   0174
*    END SYSBLK;                                                   0175
@EL00004 DS    0H                                                  0175
@EF00004 DS    0H                                                  0175
@ER00004 LM    R14,R12,@SA00004                                    0175
         BR    R14                                                 0175
*        RFY Y UNRSTD;                                             0176
*                                                                  0177
*/********************************************************************/
*/*                                                                  */
*/* SEGMENT NAME -                                                   */
*/*      SYSPACE                                                     */
*/*                                                                  */
*/* FUNCTION -                                                       */
*/*      THIS VALIDITY CHECK ROUTINE CHECKS THE SPACE, SPACE         */
*/*      INCREMENT, AND DIRECTORY VALUES FOR THE MAXIMUM.            */
*/*                                                                  */
*/* OPERATION -                                                      */
*/*      THREE ENTRY POINTS ARE DEFINED TO INDICATE WHICH PARAMETER  */
*/*      IS BEING PROCESSED - SYSPACE1, SYSPACE2, SYSDIR. EACH       */
*/*      ENTRY POINT INVOKES THE MAIN PROCESSOR, SYSPACE. THE        */
*/*      PARAMETER IS CONVERTED TO BINARY AND CHECKED AGAINST THE    */
*/*      MAXIMUM OF 16,777,215. IF GREATER, THE RETURN CODE IS SET   */
*/*      TO 8, AND AN ERROR MESSAGE IS ISSUED.                       */
*/*                                                                  */
*/* INPUT -                                                          */
*/*      PTR TO PDE                                                  */
*/*                                                                  */
*/* OUTPUT -                                                         */
*/*      RETURN CODE = 0 - VALID PARAMETER                           */
*/*                    8 - INVALID PARAMETER, PARSE PROMPTS          */
*/*                                                                  */
*/* NOTE:  THIS ROUTINE REPRESENTS A DIRECT LINE FUNCTION AND CAN    */
*/*        BE INCORPORATED AS AN INCLUDED CODE SEGMENT WHEN THE      */
*/*        INCLUDE FUNCTION HAS RECEIVED A STANDARD DEFINITION.      */
*/********************************************************************/
*                                      /* ENTRY FOR SPACE            */
*    SYSPACE1: PROC OPTIONS(NOSAVEAREA);                           0177
SYSPACE1 STM   R14,R12,12(R13)                                     0177
*        R15 = R1->VALWORK;            /* GET STORED REGISTERS       */
         L     R15,VALWORK(,R1)                                    0178
*        GENERATE;                     /* SET UP ADDRESSABILITY      */
*                                                                  0179
         LM  2,12,0(15)                   RESTORE ALLOC REGISTERS
         LA  14,SAVEVC                    VC SAVER IN REG 14
         ST  14,8(13)                     VC SAVER IN PARSE SAVER
         ST  13,4(14)                     PARSE SAVER IN VC SAVER
         LR  13,14                        VC SAVER IN REG 13
*        SPACE1 = '1'B;                /* INDICATE SPACE             */
         OI    SPACE1(R6AWAPTR),B'01000000'                        0180
*        CALL SYSPACE;                 /* PROCESS PARAMETER          */
         BAL   R14,SYSPACE                                         0181
*        SPACE1 = '0'B;                /* TURN OFF INDICATOR         */
         NI    SPACE1(R6AWAPTR),B'10111111'                        0182
*        GEN (L 13,4(13));                                         0183
         L 13,4(13)
*        PARSRTC = PRCODE;                                         0184
         L     R10,PRCODE(,R6AWAPTR)                               0184
         ST    R10,PARSRTC(,R13)                                   0184
*        RETURN;                       /* RETURN TO PARSE            */
@EL00005 DS    0H                                                  0185
@EF00005 DS    0H                                                  0185
@ER00005 LM    R14,R12,12(R13)                                     0185
         BR    R14                                                 0185
*     END SYSPACE1;                                                0186
*                                                                  0186
*                                      /* ENTRY FOR SPACE INCREMENT  */
*   SYSPACE2:  PROC OPTIONS(NOSAVEAREA);                           0187
SYSPACE2 STM   R14,R12,12(R13)                                     0187
*        R15 = R1->VALWORK;            /* GET STORED REGISTERS       */
         L     R15,VALWORK(,R1)                                    0188
*        GENERATE;                     /* SET UP ADDRESSABILITY      */
         LM  2,12,0(15)                   RESTORE ALLOC REGISTERS
         LA  14,SAVEVC                    VC SAVER IN REG 14
         ST  14,8(13)                     VC SAVER IN PARSE SAVER
         ST  13,4(14)                     PARSE SAVER IN VC SAVER
         LR  13,14                        VC SAVER IN REG 13
*        SPACE2 = '1'B;                /* INDICATE SECONDARY SPACE   */
         OI    SPACE2(R6AWAPTR),B'00100000'                        0190
*        CALL SYSPACE;                 /* PROCESS PARAMETER          */
         BAL   R14,SYSPACE                                         0191
*        SPACE2 = '0'B;                /* RESET INDICATOR            */
         NI    SPACE2(R6AWAPTR),B'11011111'                        0192
*        GEN (L 13,4(13));                                         0193
         L 13,4(13)
*        PARSRTC = PRCODE;                                         0194
         L     R10,PRCODE(,R6AWAPTR)                               0194
         ST    R10,PARSRTC(,R13)                                   0194
*        RETURN;                       /* RETURN TO PARSE            */
@EL00006 DS    0H                                                  0195
@EF00006 DS    0H                                                  0195
@ER00006 LM    R14,R12,12(R13)                                     0195
         BR    R14                                                 0195
*     END SYSPACE2;                                                0196
*                                                                  0196
*                                      /* ENTRY FOR DIRECTORY        */
*   SYSDIR: PROC OPTIONS(NOSAVEAREA);                              0197
SYSDIR   STM   R14,R12,12(R13)                                     0197
*        R15 = R1->VALWORK;            /* GET STORED REGISTERS       */
         L     R15,VALWORK(,R1)                                    0198
*        GENERATE;                     /* SET UP ADDRESSABILITY      */
         LM  2,12,0(15)                   RESTORE ALLOC REGISTERS
         LA  14,SAVEVC                    VC SAVER IN REG 14
         ST  14,8(13)                     VC SAVER IN PARSE SAVER
         ST  13,4(14)                     PARSE SAVER IN VC SAVER
         LR  13,14                        VC SAVER IN REG 13
*        VCFLAGS = '00'X;              /* INDICATE DIRECTORY         */
         MVI   VCFLAGS(R6AWAPTR),X'00'                             0200
*        CALL SYSPACE;                 /* PROCESS PARAMETER          */
         BAL   R14,SYSPACE                                         0201
*        GEN (L 13,4(13));                                         0202
         L 13,4(13)
*        PARSRTC = PRCODE;                                         0203
         L     R10,PRCODE(,R6AWAPTR)                               0203
         ST    R10,PARSRTC(,R13)                                   0203
*        RETURN;                       /* RETURN TO PARSE            */
@EL00007 DS    0H                                                  0204
@EF00007 DS    0H                                                  0204
@ER00007 LM    R14,R12,12(R13)                                     0204
         BR    R14                                                 0204
*     END SYSDIR;                                                  0205
*                                                                  0205
*   SYSPACE: PROC;                     /* COMMON SPACE AND DIR RTN   */
SYSPACE  STM   R14,R12,@SA00008                                    0206
*        VCKPTR = R1;                  /* PARM LIST FROM REG 1       */
         LR    R5VCKPTR,R1                                         0207
*        PRCODE = 0;                   /* INIT RETURN CODE           */
         SLR   R4,R4                                               0208
         ST    R4,PRCODE(,R6AWAPTR)                                0208
*        RCODESV = 0;                  /* INIT RET CODE IN WORKAREA  */
         ST    R4,RCODESV(,R6AWAPTR)                               0209
*        PKAREA(1) = 'F0'X;            /* 1ST BYTE IN PACKAREA F0    */
         MVI   PKAREA,X'F0'                                        0210
*        P = ADDR(PKAREA(1));          /* POINT TO 1ST BYTE          */
         LA    R7,PKAREA                                           0211
         ST    R7,P(,R6AWAPTR)                                     0211
*        Q = ADDR(PKAREA(2));          /* POINT TO 2ND BYTE          */
         LA    R10,PKAREA+1                                        0212
         ST    R10,Q(,R6AWAPTR)                                    0212
*        Q->AREA1 = P->AREA2;          /* CLEAR PACKAREA TO ALL F0   */
         MVC   AREA1(15,R10),AREA2(R7)                             0213
*        RFY Y RSTD;                   /* RESTRICT WORK REGISTER     */
*        Y = 8-VCBLKLEN+ADDR(PKAREA);  /* MOVE PARM INTO AREA        */
         L     R4,VALPDE(,R5VCKPTR)                                0215
         LH    R7,VCBLKLEN(,R4)                                    0215
         LA    R8Y,8                                               0215
         SLR   R8Y,R7                                              0215
         LA    R10,PKAREA                                          0215
         ALR   R8Y,R10                                             0215
*        PKAREA1(1:VCBLKLEN)=VCBLKBUF(1:VCBLKLEN);                 0216
         BCTR  R7,0                                                0216
         L     R4,VCBLKPTR(,R4)                                    0216
         EX    R7,@SM01301                                         0216
*        GENERATE REFS(CVBAREA);       /* PACK & CONVERT TO BINARY   */
         PACK  CVBAREA(8),PKAREA(8)
         CVB   R8Y,CVBAREA
*        IF Y > 16777215 THEN          /* IF VALUE EXCEEDS MAX,      */
         C     R8Y,@FW6FOX                                         0218
         BNH   @RF00218                                            0218
*          DO;                         /* THEN -                     */
*            PRCODE = 8;               /* INDICATE PARSE PROMPT      */
         MVC   PRCODE(4,R6AWAPTR),@FW8                             0220
*            MSGID = '103A';           /* SET MESSAGE ID             */
         L     R7,MSGPTR(,R6AWAPTR)                                0221
         MVC   MSGID(4,R7),@CHR103A                                0221
*            IF SPACE1 = '1'B THEN     /* IF SPACE ENTRY,            */
         TM    SPACE1(R6AWAPTR),B'01000000'                        0222
         BNO   @RF00222                                            0222
*              DO;                     /* THEN                       */
*                VAR1 = ADDR(SPC1);    /* PTR TO SPACE VARIABLE      */
         LA    R9,SPC1                                             0224
** STCM  ST    R9,@TF00001                                         0224
** STCM  MVC   VAR1(3,R10),@TF00001+1                              0224
         STCM  R9,7,VAR1(R7)                                       0224
*                L1 = LENGTH(SPC1);    /* AND LENGTH IN PARM LIST    */
         MVI   L1(R7),X'05'                                        0225
*              END;                                                0226
*            ELSE                      /* ELSE                       */
*            IF SPACE2 = '1'B THEN     /* IF SEC SPACE ENTRY,        */
         B     @RC00222                                            0227
@RF00222 TM    SPACE2(R6AWAPTR),B'00100000'                        0227
         BNO   @RF00227                                            0227
*              DO;                     /* THEN                       */
*                VAR1 = ADDR(SPC2);    /* PTR TO INCREMENT VARIABLE  */
         L     R10,MSGPTR(,R6AWAPTR)                               0229
         LA    R4,SPC2                                             0229
** STCM  ST    R4,@TF00001                                         0229
** STCM  MVC   VAR1(3,R10),@TF00001+1                              0229
         STCM  R4,7,VAR1(R10)                                      0229
*                L1 = LENGTH(SPC2);    /* AND LENGTH IN PARM LIST    */
         MVI   L1(R10),X'0F'                                       0230
*              END;                                                0231
*            ELSE                      /* OTHERWISE,                 */
*              DO;                     /* ASSUME DIR ENTRY -         */
         B     @RC00227                                            0232
@RF00227 DS    0H                                                  0233
*                VAR1 = ADDR(DIR1);    /* PTR TO DIR VARIABLE        */
         L     R7,MSGPTR(,R6AWAPTR)                                0233
         LA    R9,DIR1                                             0233
** STCM  ST    R9,@TF00001                                         0233
** STCM  MVC   VAR1(3,R10),@TF00001+1                              0233
         STCM  R9,7,VAR1(R7)                                       0233
*                L1 = LENGTH(DIR1);    /* AND LENGTH IN PARM LIST    */
         MVI   L1(R7),X'03'                                        0234
*              END;                                                0235
*            CALL VCPUTMSG;            /* ISSUE MESSAGE              */
@RC00227 DS    0H                                                  0236
@RC00222 BAL   R14,VCPUTMSG                                        0236
*            IF RCODESV > 0 THEN       /* IF MSG PROCESSOR FAILED,   */
         L     R10,RCODESV(,R6AWAPTR)                              0237
         LTR   R10,R10                                             0237
         BNP   @RF00237                                            0237
*              PRCODE = 12;            /* INDICATE PARSE TERMINATE   */
         MVC   PRCODE(4,R6AWAPTR),@FW12                            0238
*            ELSE                      /* ELSE,                      */
*              VALMSG = ADDR(MSG01);   /* PASS 2ND LEVEL MSG         */
         B     @RC00237                                            0239
@RF00237 LA    R4,MSG01                                            0239
         ST    R4,VALMSG(,R5VCKPTR)                                0239
*          END;                                                    0240
*   END  SYSPACE;                      /* RETURN                     */
@EL00008 DS    0H                                                  0241
@EF00008 DS    0H                                                  0241
@ER00008 LM    R14,R12,@SA00008                                    0241
         BR    R14                                                 0241
*        RFY Y UNRSTD;                                             0242
*/********************************************************************/
*/*                                                                  */
*/* SEGMENT NAME -                                                   */
*/*      SYSDSN                                                      */
*/*                                                                  */
*/* FUNCTION -                                                       */
*/*      THIS VALIDITY CHECK ROUTINE CHECKS THE DATASET PARAMETER    */
*/*      FOR A MAXIMUM OF 255 DSNAMES IN THE LIST, AN ASTERISK       */
*/*      ENTERED AS DSNAME WITHIN THE LIST, AND A MEMBER NAME        */
*/*      SPECIFIED WITH NO DSNAME.                                   */
*/*                                                                  */
*/* OPERATION -                                                      */
*/*      A COUNTER IS INCREMENTED UPON ENTRY TO THE ROUTINE. IF THE  */
*/*      COUNTER EXCEEDS 255, THE RETURN CODE IS SET TO 12 AND AN    */
*/*      ERROR MESSAGE IS ISSUED. IF THE COUNTER IS GREATER THAN 1   */
*/*      AND AN ASTERISK HAS BEEN ENTERED AS DSNAME, THE RETURN CODE */
*/*      IS SET TO 12, AND AN ERROR MESSAGE IS ISSUED. IF A MEMBER   */
*/*      NAME HAS BEEN SPECIFIED WITH NO DSNAME, THE RETURN CODE IS  */
*/*      SET TO 4, AND A SECOND LEVEL MESSAGE IS PASSED TO PARSE.    */
*/*                                                                  */
*/* INPUT -                                                          */
*/*      PTR TO PDE                                                  */
*/*      DSNAME COUNTER                                              */
*/*                                                                  */
*/* OUTPUT -                                                         */
*/*      RETURN CODE = 0 - VALID PARAMETER                           */
*/*                    4 - INVALID PARAMETER, PARSE PROMPTS          */
*/*                   12 - INVALID PARAMETER, PARSE TERMINATES       */
*/*                                                                  */
*/* NOTE:  THIS ROUTINE REPRESENTS A DIRECT LINE FUNCTION AND CAN    */
*/*        BE INCORPORATED AS AN INCLUDED CODE SEGMENT WHEN THE      */
*/*        INCLUDE FUNCTION HAS RECEIVED A STANDARD DEFINITION.      */
*/********************************************************************/
*                                      /* ENTRY FOR DSNAME           */
*    SYSDSN: PROC OPTIONS(NOSAVEAREA);                             0243
SYSDSN   STM   R14,R12,12(R13)                                     0243
*        R15 = R1->VALWORK;            /* GET STORED REGISTERS       */
         L     R15,VALWORK(,R1)                                    0244
*        GENERATE;                     /* SET UP ADDRESSABILITY      */
         LM  2,12,0(15)                   RESTORE ALLOC REGISTERS
         LA  14,SAVEVC                    VC SAVER IN REG 14
         ST  14,8(13)                     VC SAVER IN PARSE SAVER
         ST  13,4(14)                     PARSE SAVER IN VC SAVER
         LR  13,14                        VC SAVER IN REG 13
*        VCKPTR = R1;                  /* PARM LIST FROM REG 1       */
         LR    R5VCKPTR,R1                                         0246
*        PRCODE = 0;                   /* INIT PARSE RETURN CODE     */
         SLR   R4,R4                                               0247
         ST    R4,PRCODE(,R6AWAPTR)                                0247
*        RCODESV = 0;                  /* INIT RET CODE IN WORKAREA  */
         ST    R4,RCODESV(,R6AWAPTR)                               0248
*        IF FIRSTPDE = '0'B THEN       /* IS THIS 1ST DSNAME @YM05479*/
         TM    FIRSTPDE(R6AWAPTR),B'00000001'                      0249
         BNZ   @RF00249                                            0249
*          DO;                         /* YES -              @YM05479*/
*            FIRSTPDE = '1'B;          /* INDICATE SO        @YM05479*/
         OI    FIRSTPDE(R6AWAPTR),B'00000001'                      0251
*            FIRSTPTR = VALPDE;        /* SAVE PDE ADDR      @YM05479*/
         L     R10,VALPDE(,R5VCKPTR)                               0252
         ST    R10,FIRSTPTR(,R6AWAPTR)                             0252
*          END;                        /*                    @YM05479*/
*        ELSE                          /* NO -               @YM05479*/
*          IF FIRSTPTR = VALPDE THEN   /* IS THIS PDE ADDR   @YM05479
*                                         IN 1ST LOCATION    @YM05479*/
         B     @RC00249                                            0254
@RF00249 CLC   FIRSTPTR(4,R6AWAPTR),VALPDE(R5VCKPTR)               0254
         BNE   @RF00254                                            0254
*            DO;                       /* YES -              @YM05479*/
*              DSNCTR = 0;             /* RESET DSN COUNTER  @YM05479*/
         SLR   R4,R4                                               0256
         STH   R4,DSNCTR(,R6AWAPTR)                                0256
*              ASTRSK = '0'B;          /* RESET * INDICATOR  @YM05479*/
         NI    ASTRSK(R6AWAPTR),B'11111011'                        0257
*            END;                      /*                    @YM05479*/
*        IF ASTRSK = '1'B THEN         /* IF * FOUND AS PREVIOUS DSN */
@RF00254 DS    0H                                                  0259
@RC00249 TM    ASTRSK(R6AWAPTR),B'00000100'                        0259
         BO    @RT00259                                            0259
*          GO TO INVASTK;              /* PROCESS ERROR              */
*        DSNCTR = DSNCTR + 1;          /* INCREMENT COUNTER          */
         LA    R7,1                                                0261
         AH    R7,DSNCTR(,R6AWAPTR)                                0261
         STH   R7,DSNCTR(,R6AWAPTR)                                0261
*        IF DSNCTR > 255 THEN          /* IF MORE THAN 255 DSNAMES,  */
         C     R7,@FW255                                           0262
         BNH   @RF00262                                            0262
*          DO;                         /* THEN                       */
*            PRCODE  = 12;             /* INDICATE PARSE TERMINATE   */
         MVC   PRCODE(4,R6AWAPTR),@FW12                            0264
*            MSGID = '107A';           /* SET MESSAGE ID             */
         L     R10,MSGPTR(,R6AWAPTR)                               0265
         MVC   MSGID(4,R10),@CHR107A                               0265
*            CALL VCPUTMSG;            /* ISSUE ERROR MESSAGE        */
         BAL   R14,VCPUTMSG                                        0266
*            AWARSV1 = '1'B;           /* INDICATE MSG ISSUED        */
         OI    AWARSV1(R6AWAPTR),B'00000010'                       0267
*          END;                                                    0268
*        ELSE                                                      0269
*          IF VCTERM = '*' THEN        /* IF THIS DSNAME IS *        */
         B     @RC00262                                            0269
@RF00262 L     R4,VALPDE(,R5VCKPTR)                                0269
         L     R7,VCDSNPTR(,R4)                                    0269
         CLI   VCTERM(R7),C'*'                                     0269
         BNE   @RF00269                                            0269
*            DO;                       /* THEN                       */
*              IF DSNCTR = 1 THEN      /* IF 1ST DSNAME,             */
         CLC   DSNCTR(2,R6AWAPTR),@HW1                             0271
         BNE   @RF00271                                            0271
*                ASTRSK = '1'B;        /* SET INDICATOR              */
         OI    ASTRSK(R6AWAPTR),B'00000100'                        0272
*              ELSE                    /* ELSE,                      */
*                DO;                   /* IF * WITHIN DSN LIST -     */
         B     @RC00271                                            0273
@RF00271 DS    0H                                                  0274
* INVASTK:                                                         0274
*                   PRCODE  = 12;      /* INDICATE PARSE TERMINATE   */
INVASTK  MVC   PRCODE(4,R6AWAPTR),@FW12                            0274
*                   MSGID = '111A';    /* SET MESSAGE ID             */
         L     R10,MSGPTR(,R6AWAPTR)                               0275
         MVC   MSGID(4,R10),@CHR111A                               0275
*                   CALL VCPUTMSG;     /* ISSUE ERROR MESSAGE        */
         BAL   R14,VCPUTMSG                                        0276
*                   AWARSV1 = '1'B;    /* INDICATE MSG ISSUED        */
         OI    AWARSV1(R6AWAPTR),B'00000010'                       0277
*                END;                                              0278
*            END;                                                  0279
*          ELSE                                                    0280
*            IF VCDSNPRS = '0'B &      /* IF DSN NOT ENTERED AND     */
*               VCMBRPRS = '1'B THEN   /* MEMBER NAME IS ENTERED,    */
         B     @RC00269                                            0280
@RF00269 L     R4,VALPDE(,R5VCKPTR)                                0280
         TM    VCDSNPRS(R4),B'10000000'                            0280
         BNZ   @RF00280                                            0280
         TM    VCMBRPRS(R4),B'10000000'                            0280
         BNO   @RF00280                                            0280
*              DO;                     /* THEN                       */
*                PRCODE = 4;           /* INDICATE PARSE PROMPT      */
         MVC   PRCODE(4,R6AWAPTR),@FW4                             0282
*                VALMSG = ADDR(MSG02); /* PASS 2ND LEVEL MSG         */
         LA    R7,MSG02                                            0283
         ST    R7,VALMSG(,R5VCKPTR)                                0283
*              END;                                                0284
*        GEN (L 13,4(13));             /* RESTORE PARSE REGISTERS    */
@RF00280 DS    0H                                                  0285
@RC00269 DS    0H                                                  0285
@RC00262 DS    0H                                                  0285
         L 13,4(13)
*        PARSRTC = PRCODE;             /* RETURN CODE IN REG 15      */
         L     R7,PRCODE(,R6AWAPTR)                                0286
         ST    R7,PARSRTC(,R13)                                    0286
*   END  SYSDSN;                                                   0287
@EL00009 DS    0H                                                  0287
@EF00009 DS    0H                                                  0287
@ER00009 LM    R14,R12,12(R13)                                     0287
         BR    R14                                                 0287
*/********************************************************************/
*/*                                                                  */
*/* SEGMENT NAME -                                                   */
*/*      SYSMAX                                                      */
*/*                                                                  */
*/* FUNCTION -                                                       */
*/*      THIS VALIDITY CHECK ROUTINE CHECKS THE VOLUME COUNT AND     */
*/*      VOLUME SEQUENCE NUMBER FOR THE MAXIMUM VALUE.               */
*/*                                                                  */
*/* OPERATION -                                                      */
*/*      THE VALUE IN THE PDE IS CONVERTED TO BINARY. IF GREATER     */
*/*      THAN 255, SET PARSE RETURN CODE TO 4.                       */
*/*                                                                  */
*/* INPUT -                                                          */
*/*      PTR TO PDE                                                  */
*/*                                                                  */
*/* OUTPUT -                                                         */
*/*      RETURN CODE = 0 - VALID PARAMETER                           */
*/*                    4 - INVALID PARAMETER, PARSE PROMPTS          */
*/*                                                                  */
*/* NOTE:  THIS ROUTINE REPRESENTS A DIRECT LINE FUNCTION AND CAN    */
*/*        BE INCORPORATED AS AN INCLUDED CODE SEGMENT WHEN THE      */
*/*        INCLUDE FUNCTION HAS RECEIVED A STANDARD DEFINITION.      */
*/********************************************************************/
*                                      /* ENTRY FOR MAXVOL & VSEQ    */
*    SYSMAX: PROC OPTIONS(NOSAVEAREA);                             0288
SYSMAX   STM   R14,R12,12(R13)                                     0288
*        R15 = R1->VALWORK;            /* GET STORED REGISTERS       */
         L     R15,VALWORK(,R1)                                    0289
*        GEN (LM 2,12,0(15));          /* RESTORE ALLOC REGISTERS    */
         LM 2,12,0(15)
*        VCKPTR = R1;                  /* PARM LIST FROM REG 1       */
         LR    R5VCKPTR,R1                                         0291
*        PARSRTC = 0;                  /* INIT RETURN CODE           */
         SLR   R4,R4                                               0292
         ST    R4,PARSRTC(,R13)                                    0292
*        RCODESV = 0;                  /* INIT RET CODE IN WORKAREA  */
         ST    R4,RCODESV(,R6AWAPTR)                               0293
*        PKAREA(1) = 'F0'X;            /* 1ST BYTE IN PACKAREA F0    */
         MVI   PKAREA,X'F0'                                        0294
*        P = ADDR(PKAREA(1));          /* POINT TO 1ST BYTE          */
         LA    R7,PKAREA                                           0295
         ST    R7,P(,R6AWAPTR)                                     0295
*        Q = ADDR(PKAREA(2));          /* POINT TO 2ND BYTE          */
         LA    R10,PKAREA+1                                        0296
         ST    R10,Q(,R6AWAPTR)                                    0296
*        Q->AREA1 = P->AREA2;          /* CLEAR PACKAREA TO ALL F0   */
         MVC   AREA1(15,R10),AREA2(R7)                             0297
*        RFY Y RSTD;                   /* RESTRICT WORK REGISTER     */
*        Y = 8-VCBLKLEN+ADDR(PKAREA);  /* MOVE VALUE TO PACKAREA     */
         L     R4,VALPDE(,R5VCKPTR)                                0299
         LH    R7,VCBLKLEN(,R4)                                    0299
         LA    R8Y,8                                               0299
         SLR   R8Y,R7                                              0299
         LA    R10,PKAREA                                          0299
         ALR   R8Y,R10                                             0299
*        PKAREA1(1:VCBLKLEN)=VCBLKBUF(1:VCBLKLEN);                 0300
         BCTR  R7,0                                                0300
         L     R4,VCBLKPTR(,R4)                                    0300
         EX    R7,@SM01301                                         0300
*        GENERATE REFS(CVBAREA);       /* PACK & CONVERT TO BINARY   */
         PACK  CVBAREA(8),PKAREA(8)
         CVB   R8Y,CVBAREA
*        IF Y> 255 THEN                /* IF VALUE EXCEEDS 255,      */
         C     R8Y,@FW255                                          0302
         BNH   @RF00302                                            0302
*          PARSRTC = 4;                /* INDICATE PARSE PROMPT      */
         MVC   PARSRTC(4,R13),@FW4                                 0303
*   END SYSMAX;                                                    0304
@EL00010 DS    0H                                                  0304
@EF00010 DS    0H                                                  0304
@ER00010 LM    R14,R12,12(R13)                                     0304
         BR    R14                                                 0304
*        RFY Y UNRSTD;                                             0305
*/********************************************************************/
*/*                                                                  */
*/* SEGMENT NAME -                                                   */
*/*      SYSUCNT                                                     */
*/*                                                                  */
*/* FUNCTION -                                                       */
*/*      THIS VALIDITY CHECK ROUTINE CHECKS THE UNITCOUNT PARAMETER  */
*/*      FOR THE MAXIMUM VALUE.                                      */
*/*                                                                  */
*/* OPERATION -                                                      */
*/*      THE VALUE IN THE PDE IS CONVERTED TO BINARY. IF GREATER     */
*/*      THAN 59, SET PARSE RETURN CODE TO 4.                        */
*/*                                                                  */
*/* INPUT -                                                          */
*/*      PTR TO PDE                                                  */
*/*                                                                  */
*/* OUTPUT -                                                         */
*/*      RETURN CODE = 0 - VALID PARAMETER                           */
*/*                    4 - INVALID PARAMETER, PARSE PROMPTS          */
*/*                                                                  */
*/* NOTE:  THIS ROUTINE REPRESENTS A DIRECT LINE FUNCTION AND CAN    */
*/*        BE INCORPORATED AS AN INCLUDED CODE SEGMENT WHEN THE      */
*/*        INCLUDE FUNCTION HAS RECEIVED A STANDARD DEFINITION.      */
*/********************************************************************/
*                                      /* ENTRY FOR UCOUNT           */
*    SYSUCNT:  PROC OPTIONS(NOSAVEAREA);                           0306
SYSUCNT  STM   R14,R12,12(R13)                                     0306
*        R15 = R1->VALWORK;            /* GET STORED REGISTERS       */
         L     R15,VALWORK(,R1)                                    0307
*        GEN (LM 2,12,0(15));          /* RESTORE ALLOC REGISTERS    */
         LM 2,12,0(15)
*        VCKPTR = R1;                  /* PARM LIST FROM REG 1       */
         LR    R5VCKPTR,R1                                         0309
*        PARSRTC = 0;                  /* INIT RETURN CODE           */
         SLR   R4,R4                                               0310
         ST    R4,PARSRTC(,R13)                                    0310
*        RCODESV = 0;                  /* INIT RET CODE IN WORKAREA  */
         ST    R4,RCODESV(,R6AWAPTR)                               0311
*        PKAREA(1) = 'F0'X;            /* 1ST BYTE IN PACKAREA F0    */
         MVI   PKAREA,X'F0'                                        0312
*        P = ADDR(PKAREA(1));          /* POINT TO 1ST BYTE          */
         LA    R7,PKAREA                                           0313
         ST    R7,P(,R6AWAPTR)                                     0313
*        Q = ADDR(PKAREA(2));          /* POINT TO 2ND BYTE          */
         LA    R10,PKAREA+1                                        0314
         ST    R10,Q(,R6AWAPTR)                                    0314
*        Q->AREA1 = P->AREA2;          /* CLEAR PACKAREA TO ALL F0   */
         MVC   AREA1(15,R10),AREA2(R7)                             0315
*        RFY Y RSTD;                   /* RESTRICT WORK REGISTER     */
*        Y = 8-VCBLKLEN+ADDR(PKAREA);  /* MOVE VALUE INTO PACKAREA   */
         L     R4,VALPDE(,R5VCKPTR)                                0317
         LH    R7,VCBLKLEN(,R4)                                    0317
         LA    R8Y,8                                               0317
         SLR   R8Y,R7                                              0317
         LA    R10,PKAREA                                          0317
         ALR   R8Y,R10                                             0317
*        PKAREA1(1:VCBLKLEN)=VCBLKBUF(1:VCBLKLEN);                 0318
         BCTR  R7,0                                                0318
         L     R4,VCBLKPTR(,R4)                                    0318
         EX    R7,@SM01301                                         0318
*        GENERATE REFS(CVBAREA);       /* PACK & CONVERT TO BINARY   */
         PACK  CVBAREA(8),PKAREA(8)
         CVB   R8Y,CVBAREA
*        IF Y > 59 THEN                /* IF PARM EXCEEDS 59,        */
         C     R8Y,@FW59                                           0320
         BNH   @RF00320                                            0320
*          PARSRTC = 4;                /* PARSE PROMPTS              */
         MVC   PARSRTC(4,R13),@FW4                                 0321
*   END  SYSUCNT;                                                  0322
@EL00011 DS    0H                                                  0322
@EF00011 DS    0H                                                  0322
@ER00011 LM    R14,R12,12(R13)                                     0322
         BR    R14                                                 0322
*        RFY Y UNRSTD;                                             0323
*/********************************************************************/
*/*                                                                  */
*/* SEGMENT NAME -                                                   */
*/*      SYSVLIST                                                    */
*/*                                                                  */
*/* FUNCTION -                                                       */
*/*      THIS VALIDITY CHECK ROUTINE COUNTS THE NUMBER OF VOLUME     */
*/*      SERIALS SPECIFIED ON THE VOLUME PARAMETER.                  */
*/*                                                                  */
*/* OPERATION -                                                      */
*/*      A COUNTER IS INCREMENTED.                                   */
*/*                                                                  */
*/* INPUT -                                                          */
*/*      PTR TO PDE                                                  */
*/*      VLISTCTR - COUNTER FOR VOLUME SERIALS                       */
*/*                                                                  */
*/* OUTPUT -                                                         */
*/*      RETURN CODE = 0 - VALID PARAMETER                           */
*/*                                                                  */
*/* NOTE:  THIS ROUTINE REPRESENTS A DIRECT LINE FUNCTION AND CAN    */
*/*        BE INCORPORATED AS AN INCLUDED CODE SEGMENT WHEN THE      */
*/*        INCLUDE FUNCTION HAS RECEIVED A STANDARD DEFINITION.      */
*/********************************************************************/
*                                      /* ENTRY FOR VOLUME           */
*    SYSVLIST: PROC OPTIONS(NOSAVEAREA);                           0324
SYSVLIST STM   R14,R12,12(R13)                                     0324
*        R15 = R1->VALWORK;            /* GET STORED REGISTERS       */
         L     R15,VALWORK(,R1)                                    0325
*        GEN (LM 2,12,0(15));          /* RESTORE ALLOC REGISTERS    */
         LM 2,12,0(15)
*        VCKPTR = R1;                  /* PARM LIST FROM REG 1       */
         LR    R5VCKPTR,R1                                         0327
*        PARSRTC = 0;                  /* INITIALIZE RETURN CODE     */
         SLR   R7,R7                                               0328
         ST    R7,PARSRTC(,R13)                                    0328
*        RCODESV = 0;                  /* INIT RET CODE IN WORKAREA  */
         ST    R7,RCODESV(,R6AWAPTR)                               0329
*        VLISTCTR = VLISTCTR + 1;      /* INCREMENT COUNTER          */
         LA    R10,1                                               0330
         AH    R10,VLISTCTR(,R6AWAPTR)                             0330
         STH   R10,VLISTCTR(,R6AWAPTR)                             0330
*   END SYSVLIST;                                                  0331
@EL00012 DS    0H                                                  0331
@EF00012 DS    0H                                                  0331
@ER00012 LM    R14,R12,12(R13)                                     0331
         BR    R14                                                 0331
*/********************************************************************/
*/*                                                                  */
*/* SEGMENT NAME -                                                   */
*/*      SYSTATS                                                     */
*/*                                                                  */
*/* FUNCTION -                                                       */
*/*      THIS VALIDITY CHECK ROUTINE CHECKS THE STATUS ENTERED ON    */
*/*      THE PROMPT FOR A VALID KEYWORD.                             */
*/*                                                                  */
*/* OPERATION -                                                      */
*/*      IF MORE THAN ONE SUBSCRIPT IS ENTERED, PARSE RETURN CODE    */
*/*      IS SET TO 4. ELSE, SET UP A NEW COMMAND BUFFER WITH THE     */
*/*      PARAMETERS ENTERED ON THE PROMPT. INVOKE PARSE, PASSING     */
*/*      A LIST OF KEYWORDS IN THE PCL. IF PARSE FINDS A MATCH,      */
*/*      MOVE THE PARAMETER INTO THE ORIGINAL PDE.                   */
*/*                                                                  */
*/* INPUT -                                                          */
*/*      PTR TO PDE                                                  */
*/*      THIRD LEVEL PCL                                             */
*/*                                                                  */
*/* OUTPUT -                                                         */
*/*      RETURN CODE = 0 - VALID PARAMETER                           */
*/*                    4 - INVALID PARAMETER, PARSE PROMPTS          */
*/********************************************************************/
*                                      /* ENTRY FOR STATUS           */
*    SYSTATS: PROC OPTIONS(NOSAVEAREA);                            0332
SYSTATS  STM   R14,R12,12(R13)                                     0332
*        R15 = R1->VALWORK;            /* GET STORED REGISTERS       */
         L     R15,VALWORK(,R1)                                    0333
*        GENERATE;                     /* SET UP ADDRESSABILITY      */
         LM  2,12,0(15)                   RESTORE ALLOC REGISTERS
         LA  14,SAVEVC                    VC SAVER IN REG 14
         ST  14,8(13)                     VC SAVER IN PARSE SAVER
         ST  13,4(14)                     PARSE SAVER IN VC SAVER
         LR  13,14                        VC SAVER IN REG 14
*        VCKPTR = R1;                  /* PARM LIST FROM REG 1       */
         LR    R5VCKPTR,R1                                         0335
*        PRCODE = 0;                   /* INIT PARSE RETURN CODE     */
         SLR   R4,R4                                               0336
         ST    R4,PRCODE(,R6AWAPTR)                                0336
*        IF SUBNMBR > 1 THEN           /* IF MORE THAN 1 SUBSCRIPT,  */
         L     R7,VALPDE(,R5VCKPTR)                                0337
         CLI   SUBNMBR(R7),1                                       0337
         BNH   @RF00337                                            0337
*          PRCODE = 4;                 /* INDICATE PARSE PROMPT      */
         MVC   PRCODE(4,R6AWAPTR),@FW4                             0338
*        ELSE                                                      0339
*          DO;                         /* ELSE,                      */
         B     @RC00337                                            0339
@RF00337 DS    0H                                                  0340
*            SAVPPL = PPLPTR;                                      0340
         L     R2SAVPPL,PPLPTR(,R6AWAPTR)                          0340
*            CALL PARSLINK;            /* SET UP FOR CALL TO PARSE   */
         BAL   R14,PARSLINK                                        0341
*            R1 = PPLPTR;              /* PPL IN REG 1               */
         L     R1,PPLPTR(,R6AWAPTR)                                0342
*              DO; /*CALLTSSR EP(IKJPARS)*/                        0343
*                RESPECIFY GPR01P RSTD;                            0344
*                IF CVTPARS = 0                                    0345
*                  THEN                                            0345
         L     R3,CVTPTR                                           0345
         L     R7,CVTPARS-CVT(,R3)                                 0345
         LTR   R7,R7                                               0345
         BNZ   @RF00345                                            0345
*                    GEN(LINK EP=IKJPARS);                         0346
         LINK EP=IKJPARS
*                  ELSE                                            0347
*                    CALL CVTPARS;                                 0347
         B     @RC00345                                            0347
@RF00345 L     R10,CVTPTR                                          0347
         L     R15,CVTPARS-CVT(,R10)                               0347
         BALR  R14,R15                                             0347
*                RESPECIFY GPR01P UNRSTD;                          0348
@RC00345 DS    0H                                                  0349
*              END;   /* INVOKE PARSE       @Y30NQKK*/             0349
*            RCODESV = R15;            /* SAVE RETURN CODE           */
         ST    R15,RCODESV(,R6AWAPTR)                              0350
*            IF RCODESV > 0 THEN       /* IF PARSE FAILED,           */
         L     R3,RCODESV(,R6AWAPTR)                               0351
         LTR   R3,R3                                               0351
         BNP   @RF00351                                            0351
*                PRCODE = 12;          /* INDICATE PARSE TERMINATE   */
         MVC   PRCODE(4,R6AWAPTR),@FW12                            0352
*            ELSE                      /* OTHERWISE                  */
*              DO;                                                 0353
         B     @RC00351                                            0353
@RF00351 DS    0H                                                  0354
*                IF STAT2PDE = 0 THEN  /* IF NO STATUS ENTERED,      */
         L     R7,PDLADDR3(,R6AWAPTR)                              0354
         LH    R10,STAT2PDE(,R7)                                   0354
         LTR   R10,R10                                             0354
         BNZ   @RF00354                                            0354
*                  PRCODE = 4;         /* INDICATE PARSE PROMPT      */
         MVC   PRCODE(4,R6AWAPTR),@FW4                             0355
*                ELSE                  /* IF STATUS WAS ENTERED,     */
*                   DO;                /* SAVE STATUS IN PDE         */
         B     @RC00354                                            0356
@RF00354 DS    0H                                                  0357
*                     STATSPDE = STAT2PDE;                         0357
         L     R3,PTRPDL(,R6AWAPTR)                                0357
         L     R7,PDLADDR3(,R6AWAPTR)                              0357
         LH    R10,STAT2PDE(,R7)                                   0357
         STH   R10,STATSPDE(,R3)                                   0357
*                     IF STATSPDE=5 &  /* IF SYSOUT AND              */
*                                      /* CLASS SPECIFIED THEN       */
*                       PCLASPRS = '1'B THEN                       0358
         C     R10,@FW5                                            0358
         BNE   @RF00358                                            0358
         TM    PCLASPRS(R7),B'10000000'                            0358
         BNO   @RF00358                                            0358
*                       DO;            /* OVERLAY PDE -              */
*                         CLASPTR = ADDR(NCLASBUF);                0360
         LA    R10,NCLASBUF(,R6AWAPTR)                             0360
         ST    R10,CLASPTR(,R3)                                    0360
*                                      /* CLASS VALUE IN PDE         */
*                         NCLASBUF(1) = CLASBUF1(1);               0361
         L     R10,PCLASPTR(,R7)                                   0361
         MVC   NCLASBUF(1,R6AWAPTR),CLASBUF1(R10)                  0361
*                                      /* LENGTH IN PDE              */
*                         CLASLEN = PCLASLEN;                      0362
         LH    R10,PCLASLEN(,R7)                                   0362
         STH   R10,CLASLEN(,R3)                                    0362
*                                      /* FLAGS IN PDE               */
*                         CLASFLG = PCLASFLG;                      0363
         MVC   CLASFLG(1,R3),PCLASFLG(R7)                          0363
*                       END;                                       0364
*                  END;                                            0365
@RF00358 DS    0H                                                  0366
*                RFY R5 RSTD;          /* RESTRICT REG 5             */
@RC00354 DS    0H                                                  0367
*                R5 = ADDR(PDLADDR3);  /* PDLADDR IN REG 5           */
         LA    R5,PDLADDR3(,R6AWAPTR)                              0367
*                GEN (IKJRLSA (5));    /* RELEASE PDL                */
         IKJRLSA (5)
*                RFY R5 UNRSTD;        /* UNRESTRICT REG 5           */
*              END;                                                0370
*            PPLPTR = SAVPPL;          /* RESTORE PPL PTR            */
@RC00351 ST    R2SAVPPL,PPLPTR(,R6AWAPTR)                          0371
*          END;                                                    0372
*        GEN (L 13,4(13));             /* RESTORE PARSE REGISTERS    */
@RC00337 DS    0H                                                  0373
         L 13,4(13)
*        PARSRTC = PRCODE;             /* RETURN CODE IN REG 15      */
         L     R10,PRCODE(,R6AWAPTR)                               0374
         ST    R10,PARSRTC(,R13)                                   0374
*        RETURN;                                                   0375
@EL00013 DS    0H                                                  0375
@EF00013 DS    0H                                                  0375
@ER00013 LM    R14,R12,12(R13)                                     0375
         BR    R14                                                 0375
*   END SYSTATS;                                                   0376
*/********************************************************************/
*/*                                                                  */
*/* SEGMENT NAME -                                                   */
*/*      PARSLINK                                                    */
*/*                                                                  */
*/* FUNCTION -                                                       */
*/*      THIS ROUTINE SETS UP THE COMMAND BUFFER AND PARSE PARAMETER */
*/*      LIST.                                                       */
*/*                                                                  */
*/* OPERATION -                                                      */
*/*      THE PARAMETER ENTERED ON THE PROMPT IS PLACED IN A          */
*/*      BUFFER. THE PARSE PARAMETER LIST IS SET UP.                 */
*/*                                                                  */
*/* INPUT -                                                          */
*/*      PTR TO PDE                                                  */
*/*                                                                  */
*/* OUTPUT -                                                         */
*/*      PPL                                                         */
*/*                                                                  */
*/********************************************************************/
*        RFY I RSTD;                                               0377
*    PARSLINK: PROC OPTIONS(NOSAVEAREA);                           0378
PARSLINK STM   R14,R12,12(R13)                                     0378
*        CMDTWOBF(1) = ' ';                 /* BLANK IN 1ST POSITION */
         MVI   CMDTWOBF(R6AWAPTR),C' '                             0379
*        I = STATLEN;                       /* SET BUFFER PTR        */
         L     R4,VALPDE(,R5VCKPTR)                                0380
         SLR   R7I,R7I                                             0380
         IC    R7I,STATLEN(,R4)                                    0380
*        CMDTWOBF(2:I+1) = STATBUF(1:I);    /* MOVE IN STATUS        */
         LR    R10,R7I                                             0381
         BCTR  R10,0                                               0381
         L     R1,STATPTR(,R4)                                     0381
         EX    R10,@SM01311                                        0381
*        I = I + 1;                         /* INCREMENT BUFFER PTR  */
         LA    R10,1                                               0382
         ALR   R7I,R10                                             0382
*        IF SUBPRES = '1'B THEN             /* IF SUBSCRIPT ENTERED, */
         TM    SUBPRES(R4),B'10000000'                             0383
         BNO   @RF00383                                            0383
*          DO;                              /* THEN                  */
*            I = I + 1;                     /* INCREMENT BUFFER PTR  */
         ALR   R7I,R10                                             0385
*            CMDTWOBF(I) = '(';             /* MOVE IN LEFT PAREN    */
         LR    R1,R6AWAPTR                                         0386
         ALR   R1,R7I                                              0386
         MVI   CMDTWOBF-1(R1),C'('                                 0386
*            I = I+1;                       /* INCREMENT BUFFER PTR  */
         ALR   R7I,R10                                             0387
*            IF CNSTSUB = '1'B THEN         /* IF SUBSCRIPT A CNST,  */
         TM    CNSTSUB(R4),B'01000000'                             0388
         BNO   @RF00388                                            0388
*              DO;                          /* MOVE IN CONSTANT      */
*                CMDTWOBF(I:I+CNSTLEN-1) = CNSTBUF(1:CNSTLEN);     0390
         LR    R10,R6AWAPTR                                        0390
         ALR   R10,R7I                                             0390
         SLR   R14,R14                                             0390
         IC    R14,CNSTLEN(,R4)                                    0390
         LR    R15,R14                                             0390
         BCTR  R15,0                                               0390
         L     R4,CNSTPTR(,R4)                                     0390
         EX    R15,@SM01313                                        0390
*                I = I + CNSTLEN;           /* INCREMENT BUFFER PTR  */
         ALR   R7I,R14                                             0391
*              END;                                                0392
*            ELSE                           /* ELSE,                 */
*              DO;                          /* MOVE IN VARIABLE      */
         B     @RC00388                                            0393
@RF00388 DS    0H                                                  0394
*                CMDTWOBF(I:I+VARLEN-1) = VARBUF(1:VARLEN);        0394
         LR    R10,R6AWAPTR                                        0394
         ALR   R10,R7I                                             0394
         L     R4,VALPDE(,R5VCKPTR)                                0394
         SLR   R8,R8                                               0394
         IC    R8,VARLEN(,R4)                                      0394
         LR    R9,R8                                               0394
         BCTR  R9,0                                                0394
         L     R4,VARPTR(,R4)                                      0394
         EX    R9,@SM01315                                         0394
*                I = I + VARLEN;            /* INCREMENT BUFFER PTR  */
         ALR   R7I,R8                                              0395
*              END;                                                0396
*            CMDTWOBF(I) = ')';             /* MOVE IN RIGHT PAREN   */
@RC00388 LR    R10,R6AWAPTR                                        0397
         ALR   R10,R7I                                             0397
         MVI   CMDTWOBF-1(R10),C')'                                0397
*          END;                                                    0398
*        CMDTWOLN = I + 4;                  /* SET BUFFER LENGTH     */
@RF00383 LA    R10,4                                               0399
         ALR   R10,R7I                                             0399
         STH   R10,CMDTWOLN(,R6AWAPTR)                             0399
*        CMDTWOOF = 0;                      /* BUFFER OFFSET ZERO    */
         SLR   R7,R7                                               0400
         STH   R7,CMDTWOOF(,R6AWAPTR)                              0400
*        PPLPTR = ADDR(PPLTWO);             /* PTR TO PPL            */
         LA    R10,PPLTWO(,R6AWAPTR)                               0401
         ST    R10,PPLPTR(,R6AWAPTR)                               0401
*        PPLUPT = CPPLUPT;                  /* UPT                   */
         L     R14,CPPLPTR(,R6AWAPTR)                              0402
         L     R15,CPPLUPT(,R14)                                   0402
         ST    R15,PPLUPT(,R10)                                    0402
*        PPLECT = CPPLECT;                  /* ECT                   */
         L     R0,CPPLECT(,R14)                                    0403
         ST    R0,PPLECT(,R10)                                     0403
*        PPLECB = ADDR(COMMECB);            /* ECB                   */
         LA    R1,COMMECB(,R6AWAPTR)                               0404
         ST    R1,PPLECB(,R10)                                     0404
*        COMMECB = 0;                       /* ECB SET TO 0          */
         ST    R7,COMMECB(,R6AWAPTR)                               0405
*        PPLPCL = ADCNPCLX;                 /* PCL ADDRESS           */
         L     R3,ADCNPCLX                                         0406
         ST    R3,PPLPCL(,R10)                                     0406
*        PPLANS = ADDR(PDLADDR3);           /* PDL ADDRESS           */
         LA    R4,PDLADDR3(,R6AWAPTR)                              0407
         ST    R4,PPLANS(,R10)                                     0407
*        PPLCBUF = ADDR(CMDTWO);            /* COMMAND BUFFER        */
         LA    R5,CMDTWO(,R6AWAPTR)                                0408
         ST    R5,PPLCBUF(,R10)                                    0408
*        PPLUWA = 0;                        /* USER WORD             */
         ST    R7,PPLUWA(,R10)                                     0409
*   END PARSLINK;                                                  0410
@EL00014 DS    0H                                                  0410
@EF00014 DS    0H                                                  0410
@ER00014 LM    R14,R12,12(R13)                                     0410
         BR    R14                                                 0410
*        RFY I UNRSTD;                                             0411
*/********************************************************************/
*/*                                                                  */
*/* SEGMENT NAME -                                                   */
*/*      SYSPBLK                                                     */
*/*                                                                  */
*/* FUNCTION -                                                       */
*/*      THIS VALIDITY CHECK ROUTINE CHECKS THE VALUE ENTERED ON     */
*/*      THE PROMPT FOR A VALID KEYWORD OF BLOCK, AVBLOCK, TRACKS,   */
*/*      OR CYLINDERS.                                               */
*/*                                                                  */
*/* OPERATION -                                                      */
*/*      IF MORE THAN ONE SUBSCRIPT IS ENTERED, PARSE RETURN CODE    */
*/*      IS SET TO 4. ELSE, SET UP A NEW COMMAND BUFFER WITH THE     */
*/*      PARAMETERS ENTERED ON THE PROMPT. INVOKE PARSE, PASSING     */
*/*      A LIST OF KEYWORDS IN THE PCL. IF PARSE FINDS A MATCH,      */
*/*      MOVE THE PARAMETER INTO THE ORIGINAL PDE.                   */
*/*                                                                  */
*/* INPUT -                                                          */
*/*      PTR TO PDE                                                  */
*/*      THIRD LEVEL PCL                                             */
*/*                                                                  */
*/* OUTPUT -                                                         */
*/*      RETURN CODE = 0 - VALID PARAMETER                           */
*/*                    4 - INVALID PARAMETER, PARSE PROMPTS          */
*/********************************************************************/
*                                      /* ENTRY FOR BLOCK PROMPT     */
*    SYSPBLK: PROC OPTIONS(NOSAVEAREA);                            0412
SYSPBLK  STM   R14,R12,12(R13)                                     0412
*        R15 = R1->VALWORK;            /* GET STORED REGISTERS       */
         L     R15,VALWORK(,R1)                                    0413
*        GENERATE;                     /* SET UP ADDRESSABILITY      */
         LM  2,12,0(15)                   RESTORE ALLOC REGISTERS
         LA  14,SAVEVC                    VC SAVER IN REG 14
         ST  14,8(13)                     VC SAVER IN PARSE SAVER
         ST  13,4(14)                     PARSE SAVER IN VC SAVER
         LR  13,14                        VC SAVER IN REG 13
*        VCKPTR = R1;                  /* PARM LIST FROM REG 1       */
         LR    R5VCKPTR,R1                                         0415
*        PRCODE = 0;                   /* INIT RETURN CODE           */
         SLR   R4,R4                                               0416
         ST    R4,PRCODE(,R6AWAPTR)                                0416
*        IF SUBNUM2 > 1 THEN           /* IF MORE THAN ONE SUBSCRIPT,*/
         L     R7,VALPDE(,R5VCKPTR)                                0417
         CLI   SUBNUM2(R7),1                                       0417
         BNH   @RF00417                                            0417
*          PRCODE = 4;                 /* THEN PARSE PROMPTS         */
         MVC   PRCODE(4,R6AWAPTR),@FW4                             0418
*        ELSE                                                      0419
*          DO;                         /* ELSE                       */
         B     @RC00417                                            0419
@RF00417 DS    0H                                                  0420
*            SAVPPL = PPLPTR;          /* SAVE PPL PTR               */
         L     R2SAVPPL,PPLPTR(,R6AWAPTR)                          0420
*            CALL PARSLNK2;            /* SET UP FOR LINK TO PARSE   */
         BAL   R14,PARSLNK2                                        0421
*            R1 = PPLPTR;              /* PARM LIST IN REGISTER 1    */
         L     R1,PPLPTR(,R6AWAPTR)                                0422
*              DO; /*CALLTSSR EP(IKJPARS)*/                        0423
*                RESPECIFY GPR01P RSTD;                            0424
*                IF CVTPARS = 0                                    0425
*                  THEN                                            0425
         L     R3,CVTPTR                                           0425
         L     R4,CVTPARS-CVT(,R3)                                 0425
         LTR   R4,R4                                               0425
         BNZ   @RF00425                                            0425
*                    GEN(LINK EP=IKJPARS);                         0426
         LINK EP=IKJPARS
*                  ELSE                                            0427
*                    CALL CVTPARS;                                 0427
         B     @RC00425                                            0427
@RF00425 L     R7,CVTPTR                                           0427
         L     R15,CVTPARS-CVT(,R7)                                0427
         BALR  R14,R15                                             0427
*                RESPECIFY GPR01P UNRSTD;                          0428
@RC00425 DS    0H                                                  0429
*              END;   /* INVOKE PARSE       @Y30NQKK*/             0429
*            RCODESV = R15;            /* SAVER RETURN CODE          */
         ST    R15,RCODESV(,R6AWAPTR)                              0430
*            IF RCODESV > 0 THEN       /* IF PARSE FAILED            */
         L     R3,RCODESV(,R6AWAPTR)                               0431
         LTR   R3,R3                                               0431
         BNP   @RF00431                                            0431
*                PRCODE = 12;          /* PARSE TERMINATE            */
         MVC   PRCODE(4,R6AWAPTR),@FW12                            0432
*            ELSE                                                  0433
*              DO;                     /* OTHERWISE,                 */
         B     @RC00431                                            0433
@RF00431 DS    0H                                                  0434
*                IF BLOKNBR = 0 THEN   /* IF PARM NOT ENTERED,       */
         L     R4,PDLADDR3(,R6AWAPTR)                              0434
         LH    R7,BLOKNBR(,R4)                                     0434
         LTR   R7,R7                                               0434
         BNZ   @RF00434                                            0434
*                  PRCODE = 4;         /* INDICATE PARSE PROMPT      */
         MVC   PRCODE(4,R6AWAPTR),@FW4                             0435
*                ELSE                                              0436
*                  DO;                 /* IF PARM IS ENTERED,        */
         B     @RC00434                                            0436
@RF00434 DS    0H                                                  0437
*                                      /* SAVE PARM IN PDE           */
*                    BLOKPDE = BLOKNBR;                            0437
         L     R10,PTRPDL(,R6AWAPTR)                               0437
         L     R3,PDLADDR3(,R6AWAPTR)                              0437
         LH    R4,BLOKNBR(,R3)                                     0437
         STH   R4,BLOKPDE(,R10)                                    0437
*                                      /* IF BLOCK OR AVBLOCK,       */
*                    IF BLOKPDE < 3 THEN                           0438
         C     R4,@FW3                                             0438
         BNL   @RF00438                                            0438
*                      CALL MAXCHK;    /* CHECK FOR MAXIMUM          */
         BAL   R14,MAXCHK                                          0439
*                  END;                                            0440
@RF00438 DS    0H                                                  0441
*                RFY R5 RSTD;          /* RESTRICT REG 5             */
@RC00434 DS    0H                                                  0442
*                R5 = ADDR(PDLADDR3);  /* PDL ADDRESS IN REG 5       */
         LA    R5,PDLADDR3(,R6AWAPTR)                              0442
*                GEN (IKJRLSA (5));    /* RELEASE PDL                */
         IKJRLSA (5)
*                RFY R5 UNRSTD;        /* UNRESTRICT REG 5           */
*              END;                                                0445
*            PPLPTR = SAVPPL;          /* RESTORE PPL PTR            */
@RC00431 ST    R2SAVPPL,PPLPTR(,R6AWAPTR)                          0446
*           END;                                                   0447
*        GEN (L 13,4(13));             /* RESTORE PARSE REGISTERS    */
@RC00417 DS    0H                                                  0448
         L 13,4(13)
*        PARSRTC = PRCODE;             /* RETURN CODE IN REG 15      */
         L     R10,PRCODE(,R6AWAPTR)                               0449
         ST    R10,PARSRTC(,R13)                                   0449
*    END SYSPBLK;                                                  0450
@EL00015 DS    0H                                                  0450
@EF00015 DS    0H                                                  0450
@ER00015 LM    R14,R12,12(R13)                                     0450
         BR    R14                                                 0450
*/********************************************************************/
*/*                                                                  */
*/* SEGMENT NAME -                                                   */
*/*      MAXCHK                                                      */
*/*                                                                  */
*/* FUNCTION -                                                       */
*/*      THIS ROUTINE CONVERTS THE BLOCK OR AVBLOCK VALUE TO BINARY  */
*/*      AND CHECK FOR A MAXIMUM OF 65535.                           */
*/*                                                                  */
*/* OPERATION -                                                      */
*/*      THE VALUE IS CONVERTED TO BINARY.  IF THE VALUE EXCEEDS THE */
*/*      MAXIMUM, AN ERROR MESSAGE IS ISSUED AND PARSE RETURN CODE   */
*/*      IS SET TO 8. ELSE, THE ORIGINAL PDE IS OVERLAID WITH THE    */
*/*      NEW VALUE.                                                  */
*/*                                                                  */
*/* INPUT -                                                          */
*/*      PTR TO PDE                                                  */
*/*                                                                  */
*/* OUTPUT -                                                         */
*/*      RETURN CODE = 0 - VALID PARAMETER                           */
*/*                    8 - INVALID PARAMETER, PARSE PROMPTS          */
*/********************************************************************/
*        RFY Y RSTD;                                               0451
*    MAXCHK: PROC;                                                 0452
MAXCHK   STM   R14,R12,@SA00016                                    0452
*        PKAREA(1) = 'F0'X;                 /* 1ST BYTE IN PACKAREA
*                                              SET TO F0             */
         MVI   PKAREA,X'F0'                                        0453
*        P = ADDR(PKAREA(1));               /* POINT TO 1ST BYTE     */
         LA    R4,PKAREA                                           0454
         ST    R4,P(,R6AWAPTR)                                     0454
*        Q = ADDR(PKAREA(2));               /* POINT TO 2ND BYTE     */
         LA    R7,PKAREA+1                                         0455
         ST    R7,Q(,R6AWAPTR)                                     0455
*        Q->AREA1 = P->AREA2;               /* CLEAR PACK AREA TO F0 */
         MVC   AREA1(15,R7),AREA2(R4)                              0456
*        IF BLOKNBR = 1 THEN                /* IF BLOCK ENTERED,     */
         L     R10,PDLADDR3(,R6AWAPTR)                             0457
         CLC   BLOKNBR(2,R10),@HW1                                 0457
         BNE   @RF00457                                            0457
*          DO;                              /* THEN                  */
*            Y = 8-PBLKLEN+ADDR(PKAREA);    /* PUT VALUE IN PACK AREA*/
         LH    R4,PBLKLEN(,R10)                                    0459
         LA    R8Y,8                                               0459
         SLR   R8Y,R4                                              0459
         LA    R7,PKAREA                                           0459
         ALR   R8Y,R7                                              0459
*            PKAREA1(1:PBLKLEN)=PBLKBUF(1:PBLKLEN);                0460
         BCTR  R4,0                                                0460
         L     R9,PBLKPTR(,R10)                                    0460
         EX    R4,@SM01317                                         0460
*            VAR1 = ADDR(BLKVAR);           /* BLOCK VARIABLE PTR    */
         L     R10,MSGPTR(,R6AWAPTR)                               0461
         LA    R4,BLKVAR                                           0461
** STCM  ST    R4,@TF00001                                         0461
** STCM  MVC   VAR1(3,R10),@TF00001+1                              0461
         STCM  R4,7,VAR1(R10)                                      0461
*            L1 = LENGTH(BLKVAR);           /* AND VARIABLE LENGTH   */
         MVI   L1(R10),X'0A'                                       0462
*          END;                                                    0463
*        ELSE                               /* ELSE,                 */
*          DO;                              /* ASSUME AVBLOCK        */
         B     @RC00457                                            0464
@RF00457 DS    0H                                                  0465
*            Y = 8-PABLKLEN+ADDR(PKAREA);   /* PUT VALUE IN PACK AREA*/
         L     R7,PDLADDR3(,R6AWAPTR)                              0465
         LH    R10,PABLKLEN(,R7)                                   0465
         LA    R8Y,8                                               0465
         SLR   R8Y,R10                                             0465
         LA    R4,PKAREA                                           0465
         ALR   R8Y,R4                                              0465
*            PKAREA1(1:PABLKLEN)=PABLKBUF(1:PABLKLEN);             0466
         BCTR  R10,0                                               0466
         L     R7,PABLKPTR(,R7)                                    0466
         EX    R10,@SM01319                                        0466
*            VAR1 = ADDR(AVBLOCK);          /* AVBLOCK VARIABLE PTR  */
         L     R10,MSGPTR(,R6AWAPTR)                               0467
         LA    R4,AVBLOCK                                          0467
** STCM  ST    R4,@TF00001                                         0467
** STCM  MVC   VAR1(3,R10),@TF00001+1                              0467
         STCM  R4,7,VAR1(R10)                                      0467
*            L1 = LENGTH(AVBLOCK);          /* AND VARIABLE LENGTH   */
         MVI   L1(R10),X'07'                                       0468
*          END;                                                    0469
*                                                                  0469
*        GENERATE REFS(CVBAREA);            /* CONVERT TO BINARY     */
@RC00457 DS    0H                                                  0470
         PACK  CVBAREA(8),PKAREA(8)            PACK VALUE
         CVB   R8Y,CVBAREA                     CONVERT TO BINARY
*        IF Y > 65535 THEN                  /* IF VALUE EXCEEDS MAX, */
         C     R8Y,@FW4FOX                                         0471
         BNH   @RF00471                                            0471
*          DO;                                                     0472
*            PRCODE = 8;                    /* INDICATE PARSE PROMPT */
         MVC   PRCODE(4,R6AWAPTR),@FW8                             0473
*            MSGID = '103A';                /* SET MESSAGE ID        */
         L     R7,MSGPTR(,R6AWAPTR)                                0474
         MVC   MSGID(4,R7),@CHR103A                                0474
*            CALL VCPUTMSG;                 /* ISSUE MESSAGE         */
         BAL   R14,VCPUTMSG                                        0475
*            IF RCODESV > 0 THEN            /* IF ERROR IN MSG,      */
         L     R10,RCODESV(,R6AWAPTR)                              0476
         LTR   R10,R10                                             0476
         BNP   @RF00476                                            0476
*              PRCODE = 12;                 /* INDICATE PARSE END    */
         MVC   PRCODE(4,R6AWAPTR),@FW12                            0477
*            ELSE                                                  0478
*              VALMSG = ADDR(MSG01);        /* PASS 2ND LEVEL MSG    */
         B     @RC00476                                            0478
@RF00476 LA    R4,MSG01                                            0478
         ST    R4,VALMSG(,R5VCKPTR)                                0478
*          END;                                                    0479
*        ELSE                               /* IF VALUE DOES NOT     */
*          DO;                              /* EXCEED MAX, THEN      */
         B     @RC00471                                            0480
@RF00471 DS    0H                                                  0481
*            IF BLOKNBR = 1 THEN            /* IF BLOCK VALUE,       */
         L     R7,PDLADDR3(,R6AWAPTR)                              0481
         CLC   BLOKNBR(2,R7),@HW1                                  0481
         BNE   @RF00481                                            0481
*              DO;                          /* OVERLAY PDE           */
*                BLKPTR = ADDR(NBLKBUF1);   /* POINTER TO VALUE      */
         L     R10,PTRPDL(,R6AWAPTR)                               0483
         LA    R14,NBLKBUF1(,R6AWAPTR)                             0483
         ST    R14,BLKPTR(,R10)                                    0483
*                                           /* BLOCK VALUE           */
*                NBLKBUF1(1:PBLKLEN)=PBLKBUF(1:PBLKLEN);           0484
         LH    R15,PBLKLEN(,R7)                                    0484
         LR    R14,R15                                             0484
         BCTR  R14,0                                               0484
         L     R1,PBLKPTR(,R7)                                     0484
         EX    R14,@SM01321                                        0484
*                BLKLEN = PBLKLEN;          /* LENGTH                */
         STH   R15,BLKLEN(,R10)                                    0485
*                BLKFLAGS = PBLKFLG;        /* FLAGS                 */
         MVC   BLKFLAGS(1,R10),PBLKFLG(R7)                         0486
*              END;                                                0487
*            ELSE                           /* ELSE,                 */
*              DO;                          /* OVERLAY AVBLOCK PDE   */
         B     @RC00481                                            0488
@RF00481 DS    0H                                                  0489
*                ABLKPTR = ADDR(NBLKBUF1);  /* POINTER TO VALUE      */
         L     R7,PTRPDL(,R6AWAPTR)                                0489
         LA    R10,NBLKBUF1(,R6AWAPTR)                             0489
         ST    R10,ABLKPTR(,R7)                                    0489
*                                           /* AVBLOCK VALUE         */
*                NBLKBUF1(1:PABLKLEN)=PABLKBUF(1:PABLKLEN);        0490
         L     R10,PDLADDR3(,R6AWAPTR)                             0490
         LH    R14,PABLKLEN(,R10)                                  0490
         LR    R15,R14                                             0490
         BCTR  R15,0                                               0490
         L     R1,PABLKPTR(,R10)                                   0490
         EX    R15,@SM01323                                        0490
*                ABLKLEN = PABLKLEN;        /* LENGTH                */
         STH   R14,ABLKLEN(,R7)                                    0491
*                ABLKFLG = PABLKFLG;        /* FLAGS                 */
         MVC   ABLKFLG(1,R7),PABLKFLG(R10)                         0492
*              END;                                                0493
*          END;                                                    0494
*    END MAXCHK;                                                   0495
@EL00016 DS    0H                                                  0495
@EF00016 DS    0H                                                  0495
@ER00016 LM    R14,R12,@SA00016                                    0495
         BR    R14                                                 0495
*        RFY Y UNRSTD;                                             0496
*/********************************************************************/
*/*                                                                  */
*/* SEGMENT NAME -                                                   */
*/*      PARSLNK2                                                    */
*/*                                                                  */
*/* FUNCTION -                                                       */
*/*      THIS ROUTINE SETS UP THE NEW COMMAND BUFFER AND THE PARSE   */
*/*      PARAMETER LIST (PPL).                                       */
*/*                                                                  */
*/* OPERATION -                                                      */
*/*      THE PARAMETER ENTERED ON THE PROMPT IS PUT INTO THE BUFFER. */
*/*      THE PPL IS SET UP,                                          */
*/*                                                                  */
*/* INPUT -                                                          */
*/*      PTR TO PDE                                                  */
*/*                                                                  */
*/* OUTPUT -                                                         */
*/*      PPL                                                         */
*/********************************************************************/
*        RFY I RSTD;                                               0497
*    PARSLNK2: PROC OPTIONS(NOSAVEAREA);                           0498
PARSLNK2 STM   R14,R12,12(R13)                                     0498
*         CMDTWOBF(1) = ' ';                /* BLANK IN 1ST POSITION */
         MVI   CMDTWOBF(R6AWAPTR),C' '                             0499
*         I = BATCLEN;                      /* SET BUFFER PTR        */
         L     R4,VALPDE(,R5VCKPTR)                                0500
         SLR   R7I,R7I                                             0500
         IC    R7I,BATCLEN(,R4)                                    0500
*         CMDTWOBF(2:I+1) = BATCBUF(1:I);   /* MOVE IN PARAMETER     */
         LR    R10,R7I                                             0501
         BCTR  R10,0                                               0501
         L     R1,BATCPTR(,R4)                                     0501
         EX    R10,@SM01325                                        0501
*         I = I+1;                          /* INCREMENT BUFFER PTR  */
         LA    R10,1                                               0502
         ALR   R7I,R10                                             0502
*         IF SUB2PRES = '1'B THEN           /* IF SUBSCRIPT ENTERED, */
         TM    SUB2PRES(R4),B'10000000'                            0503
         BNO   @RF00503                                            0503
*           DO;                             /* THEN                  */
*             I = I+1;                      /* INCREMENT BUFFER PTR  */
         ALR   R7I,R10                                             0505
*             CMDTWOBF(I) = '(';            /* LEFT PAREN            */
         LR    R1,R6AWAPTR                                         0506
         ALR   R1,R7I                                              0506
         MVI   CMDTWOBF-1(R1),C'('                                 0506
*             I = I+1;                      /* INCREMENT BUFFER PTR  */
         ALR   R7I,R10                                             0507
*                                           /* SUBFIELD VALUE        */
*             CMDTWOBF(I:I+CNST2LEN-1) = CNST2BUF(1:CNST2LEN);     0508
         LR    R10,R6AWAPTR                                        0508
         ALR   R10,R7I                                             0508
         SLR   R14,R14                                             0508
         IC    R14,CNST2LEN(,R4)                                   0508
         LR    R15,R14                                             0508
         BCTR  R15,0                                               0508
         L     R4,CNST2PTR(,R4)                                    0508
         EX    R15,@SM01327                                        0508
*             I = I+CNST2LEN;               /* INCREMENT BUFFER PTR  */
         ALR   R7I,R14                                             0509
*             CMDTWOBF(I) = ')';            /* MOVE IN RIGHT PAREN   */
         LR    R10,R6AWAPTR                                        0510
         ALR   R10,R7I                                             0510
         MVI   CMDTWOBF-1(R10),C')'                                0510
*           END;                                                   0511
*        CMDTWOLN = I+4;                    /* SET BUFFER LENGTH     */
@RF00503 LA    R4,4                                                0512
         ALR   R4,R7I                                              0512
         STH   R4,CMDTWOLN(,R6AWAPTR)                              0512
*        CMDTWOOF = 0;                      /* OFFSET SET TO ZERO    */
         SLR   R7,R7                                               0513
         STH   R7,CMDTWOOF(,R6AWAPTR)                              0513
*        PPLPTR = ADDR(PPLTWO);             /* SET PTR TO PPL        */
         LA    R10,PPLTWO(,R6AWAPTR)                               0514
         ST    R10,PPLPTR(,R6AWAPTR)                               0514
*        PPLUPT = CPPLUPT;                  /* UPT                   */
         L     R4,CPPLPTR(,R6AWAPTR)                               0515
         L     R9,CPPLUPT(,R4)                                     0515
         ST    R9,PPLUPT(,R10)                                     0515
*        PPLECT = CPPLECT;                  /* ECT                   */
         L     R4,CPPLECT(,R4)                                     0516
         ST    R4,PPLECT(,R10)                                     0516
*        PPLECB = ADDR(COMMECB);            /* ECB                   */
         LA    R4,COMMECB(,R6AWAPTR)                               0517
         ST    R4,PPLECB(,R10)                                     0517
*        COMMECB = 0;                       /* SET ECB TO 0          */
         ST    R7,COMMECB(,R6AWAPTR)                               0518
*        PPLPCL = ADCNPCLY;                 /* PTR TO PCL            */
         L     R4,ADCNPCLY                                         0519
         ST    R4,PPLPCL(,R10)                                     0519
*        PPLANS = ADDR(PDLADDR3);           /* ANSWER PLACE          */
         LA    R4,PDLADDR3(,R6AWAPTR)                              0520
         ST    R4,PPLANS(,R10)                                     0520
*        PDLADDR3 = 0;                      /* SET PDL TO 0          */
         ST    R7,PDLADDR3(,R6AWAPTR)                              0521
*        PPLCBUF = ADDR(CMDTWO);            /* COMMAND BUFFER        */
         LA    R4,CMDTWO(,R6AWAPTR)                                0522
         ST    R4,PPLCBUF(,R10)                                    0522
*        PPLUWA = 0;                        /* USER WORD             */
         ST    R7,PPLUWA(,R10)                                     0523
*    END PARSLNK2;                                                 0524
@EL00017 DS    0H                                                  0524
@EF00017 DS    0H                                                  0524
@ER00017 LM    R14,R12,12(R13)                                     0524
         BR    R14                                                 0524
*        RFY I UNRSTD;                                             0525
*/********************************************************************/
*/*                                                                  */
*/* SEGMENT NAME -                                                   */
*/*      VCPUTMSG                                                    */
*/*                                                                  */
*/* FUNCTION -                                                       */
*/*      THIS ROUTINE ISSUES ERROR MESSAGES.                         */
*/*                                                                  */
*/* OPERATION -                                                      */
*/*      THE PARAMETER LIST TO IKJEFF02 IS COMPLETED.  IKJEFF02 IS   */
*/*      LINKED TO. THE RETURN CODE FROM IKJEFF02 IS SAVED.          */
*/*                                                                  */
*/* INPUT -                                                          */
*/*      WORKAREA                                                    */
*/*                                                                  */
*/* OUTPUT -                                                         */
*/*      MESSAGE ISSUED                                              */
*/********************************************************************/
*    VCPUTMSG: PROC;                                               0526
VCPUTMSG STM   R14,R12,@SA00018                                    0526
*        LISTPTR = ADDR(MSGCSECT);           /* PTR TO MSG PARM LIST */
         L     R7,MSGPTR(,R6AWAPTR)                                0527
         LA    R10,MSGCSECT(,R7)                                   0527
         ST    R10,LISTPTR(,R7)                                    0527
*        MTCPPL = CPPLPTR;                   /* PTR TO CPPL          */
         L     R10,CPPLPTR(,R6AWAPTR)                              0528
         ST    R10,MTCPPL(,R7)                                     0528
*        ECBPTR = ADDR(COMMECB);             /* PTR TO ECB           */
         LA    R10,COMMECB(,R6AWAPTR)                              0529
         ST    R10,ECBPTR(,R7)                                     0529
*        COMMECB = 0;                        /* ECB SET TO ZERO      */
         SLR   R10,R10                                             0530
         ST    R10,COMMECB(,R6AWAPTR)                              0530
*        MTHIGH = '1'B;                      /* HIGH ORDER BIT ON    */
         OI    MTHIGH(R7),B'10000000'                              0531
*        MTPUTLSW = '1'B;                    /* INDICATE PUTLINE     */
         OI    MTPUTLSW(R7),B'01000000'                            0532
*        MSGCSECT = PTRMSGS;                 /* MSG CSECT ADDRESS    */
         L     R10,PTRMSGS(,R6AWAPTR)                              0533
         ST    R10,MSGCSECT(,R7)                                   0533
*        R1 = ADDR(MSGTABLE);                /* PARM LIST IN REG 1   */
         LR    R1,R7                                               0534
*        GEN (LINK EP=IKJEFF02);             /* INVOKE MSG PROCESSOR */
         LINK EP=IKJEFF02
*        RCODESV = R15;                      /* SAVE RETURN CODE     */
         ST    R15,RCODESV(,R6AWAPTR)                              0536
*    END VCPUTMSG;                                                 0537
*                                                                  0537
*                                                                  0537
*                                                                  0537
@EL00018 DS    0H                                                  0537
@EF00018 DS    0H                                                  0537
@ER00018 LM    R14,R12,@SA00018                                    0537
         BR    R14                                                 0537
*      DECLARE /*GENERAL PURPOSE REGISTERS */                      0538
*        GPR00P PTR(31) REG(0),                                    0538
*        GPR01P PTR(31) REG(1),                                    0538
*        GPR02P PTR(31) REG(2),                                    0538
*        GPR03P PTR(31) REG(3),                                    0538
*        GPR04P PTR(31) REG(4),                                    0538
*        GPR05P PTR(31) REG(5),                                    0538
*        GPR06P PTR(31) REG(6),                                    0538
*        GPR07P PTR(31) REG(7),                                    0538
*        GPR08P PTR(31) REG(8),                                    0538
*        GPR09P PTR(31) REG(9),                                    0538
*        GPR14P PTR(31) REG(14),                                   0538
*        GPR15P PTR(31) REG(15);                                   0538
*                                                                  0538
*      DECLARE /* COMMON VARIABLES */                              0539
*        I256C CHAR(256) BASED,                                    0539
*        I031F FIXED(31) BASED,                                    0539
*        I031P PTR(31)   BASED,                                    0539
*        I015F FIXED(15) BASED,                                    0539
*        I015P PTR(15)   BASED,                                    0539
*        I008P PTR(8)    BASED,                                    0539
*        I001C CHAR(1)   BASED;                               /*   0539
*      @Y30NQKK*/                                                  0539
@DATA    DS    0H
@HW1     DC    H'1'
@HW2     DC    H'2'
@HW6     DC    H'6'
@HW7     DC    H'7'
@SM01301 MVC   PKAREA1(0,R8Y),VCBLKBUF(R4)
@SM01311 MVC   CMDTWOBF+1(0,R6AWAPTR),STATBUF(R1)
@SM01313 MVC   CMDTWOBF-1(0,R10),CNSTBUF(R4)
@SM01315 MVC   CMDTWOBF-1(0,R10),VARBUF(R4)
@SM01317 MVC   PKAREA1(0,R8Y),PBLKBUF(R9)
@SM01319 MVC   PKAREA1(0,R8Y),PABLKBUF(R7)
@SM01321 MVC   NBLKBUF1(0,R6AWAPTR),PBLKBUF(R1)
@SM01323 MVC   NBLKBUF1(0,R6AWAPTR),PABLKBUF(R1)
@SM01325 MVC   CMDTWOBF+1(0,R6AWAPTR),BATCBUF(R1)
@SM01327 MVC   CMDTWOBF-1(0,R10),CNST2BUF(R4)
@DATD    DSECT
         DS    0F
@SA00001 DS    18F
@SAV001  EQU   @SA00001
@SA00004 DS    15F
@SA00018 DS    15F
@SA00008 DS    15F
@SA00016 DS    15F
*TF00001 DS    F     @TF00001 AREA NOT NEEDED WITH 370 INSTRUCTIONS
@ZTEMPS  DS    C
@ZTEMPND EQU   *
@ZLEN    EQU   @ZTEMPND-@ZTEMPS
@TEMPS   EQU   @ZTEMPS
@L       EQU   @ZLEN
IKJEFD37 CSECT
         DS    0F
@FW3     DC    F'3'
@HW3     EQU   @FW3+2
@FW4     DC    F'4'
@HW4     EQU   @FW4+2
@FW5     DC    F'5'
@HW5     EQU   @FW5+2
@FW8     DC    F'8'
@FW12    DC    F'12'
@FW59    DC    F'59'
@FW255   DC    F'255'
@FW4FOX  DC    F'65535'
@FW6FOX  DC    F'16777215'
@DATD    DSECT
         DS    0D
PDLADDR  DS    A
PDLADDR2 DS    A
SAVEVAL  DS    CL72
PKAREA   DS    CL8
CVBAREA  DS    CL8
SAVEVC   DS    18A
         ORG   *+1-(*-@DATD)/(*-@DATD) ENSURE DSECT DATA
@ENDDATD EQU   *
@DATEND  EQU   *
IKJEFD37 CSECT
         DS    0F
@SIZDATD DC    AL1(1)
         DC    AL3(@ENDDATD-@DATD)
@SIZ001  EQU   @SIZDATD
         DS    0D
@CHR103A DC    C'103A'
@CHR107A DC    C'107A'
@CHR111A DC    C'111A'
SPC1     DC    CL5'SPACE'
SPC2     DC    CL15'SPACE INCREMENT'
DIR1     DC    CL3'DIR'
BLKVAR   DC    CL10'BLOCK SIZE'
AVBLOCK  DC    CL7'AVBLOCK'
MSG01    DS    CL108
         ORG   MSG01
MSG01LEN DC    H'108'
MSG01OFF DC    H'0'
MSG01TXT DC    CL104'IKJ56107I MAXIMUM OF 65535 FOR BLOCK AND AVBLOCK. C
               MAXIMUM OF 16777215 FOR SPACE, SPACE INCREMENT AND DIR'
         ORG   MSG01+108
MSG02    DS    CL53
         ORG   MSG02
MSG02LEN DC    H'53'
MSG02OFF DC    H'0'
MSG02TXT DC    CL49'IKJ56114I DATA SET NAME REQUIRED WITH MEMBER NAME'
         ORG   MSG02+53
         DC    0D'0'                   END OF CSECT                @26A
R0       EQU   00                      EQUATES FOR REGISTERS 0-15
R1       EQU   01
R2       EQU   02
R3       EQU   03
R4       EQU   04
R5       EQU   05
R6       EQU   06
R7       EQU   07
R8       EQU   08
R9       EQU   09
R10      EQU   10
R11      EQU   11
R12      EQU   12
R13      EQU   13
R14      EQU   14
R15      EQU   15
R2SAVPPL EQU   R2
R5VCKPTR EQU   R5
R6AWAPTR EQU   R6
R7I      EQU   R7
R8Y      EQU   R8
CPPL     EQU   0
CPPLCBUF EQU   CPPL
CPPLUPT  EQU   CPPL+4
CPPLECT  EQU   CPPL+12
PPL      EQU   0
PPLUPT   EQU   PPL
PPLECT   EQU   PPL+4
PPLECB   EQU   PPL+8
PPLPCL   EQU   PPL+12
PPLANS   EQU   PPL+16
PPLCBUF  EQU   PPL+20
PPLUWA   EQU   PPL+24
ALLOCWA  EQU   0
CPPLPTR  EQU   ALLOCWA
PTRPDL   EQU   ALLOCWA+4
SWITCH   EQU   ALLOCWA+12
ASTRSK   EQU   SWITCH
AWARSV1  EQU   SWITCH
FIRSTPDE EQU   SWITCH
ENTRYCD  EQU   ALLOCWA+14
RCODESV  EQU   ALLOCWA+24
VLISTCTR EQU   ALLOCWA+28
DSNCTR   EQU   ALLOCWA+30
PTRMSGS  EQU   ALLOCWA+32
VCFLAGS  EQU   ALLOCWA+36
AVBLK    EQU   VCFLAGS
SPACE1   EQU   VCFLAGS
SPACE2   EQU   VCFLAGS
NCLASBUF EQU   ALLOCWA+37
PMPTPDL  EQU   ALLOCWA+40
PRCODE   EQU   ALLOCWA+44
MSGPTR   EQU   ALLOCWA+48
P        EQU   ALLOCWA+52
Q        EQU   ALLOCWA+56
CMDTWO   EQU   ALLOCWA+60
CMDTWOLN EQU   CMDTWO
CMDTWOOF EQU   CMDTWO+2
CMDTWOBF EQU   CMDTWO+4
PPLTWO   EQU   ALLOCWA+128
PDLADDR3 EQU   ALLOCWA+156
COMMECB  EQU   ALLOCWA+160
NBLKBUF1 EQU   ALLOCWA+164
PPLPTR   EQU   ALLOCWA+172
FIRSTPTR EQU   ALLOCWA+176
ALLOCPDL EQU   0
STATSPDE EQU   ALLOCPDL+12
BLOKPDE  EQU   ALLOCPDL+16
DSNPDE   EQU   ALLOCPDL+48+4                                       @26C
DSNPTR   EQU   DSNPDE
DSNFLAGS EQU   DSNPDE+6
MBRPTR   EQU   DSNPDE+8
MBRFLAGS EQU   DSNPDE+14
PASSPTR  EQU   DSNPDE+16
PASSFLAG EQU   DSNPDE+22
DDNMEPDE EQU   ALLOCPDL+76+4                                       @26C
DDNPTR   EQU   DDNMEPDE
DDNFLAGS EQU   DDNMEPDE+6
CLASPDE  EQU   ALLOCPDL+84+4                                       @26C
CLASPTR  EQU   CLASPDE
CLASLEN  EQU   CLASPDE+4
CLASFLG  EQU   CLASPDE+6
SERPDE   EQU   ALLOCPDL+92+4                                       @26C
SERPTR   EQU   SERPDE
SERFLAGS EQU   SERPDE+6
BLKPDE   EQU   ALLOCPDL+104+4                                      @26C
BLKPTR   EQU   BLKPDE
BLKLEN   EQU   BLKPDE+4
BLKFLAGS EQU   BLKPDE+6
ABLKPDE  EQU   ALLOCPDL+112+4                                      @26C
ABLKPTR  EQU   ABLKPDE
ABLKLEN  EQU   ABLKPDE+4
ABLKFLG  EQU   ABLKPDE+6
SPACPDE  EQU   ALLOCPDL+120+4                                      @26C
SPACEPTR EQU   SPACPDE
SPACEFLG EQU   SPACPDE+6
SPACPDE2 EQU   ALLOCPDL+128+4                                      @26C
SPACPTR2 EQU   SPACPDE2
SPACFLG2 EQU   SPACPDE2+6
DIRECPDE EQU   ALLOCPDL+136+4                                      @26C
DIRPTR   EQU   DIRECPDE
DIRFLAGS EQU   DIRECPDE+6
ATTRPDE  EQU   ALLOCPDL+144+4                                      @26C
ATTRPTR  EQU   ATTRPDE
ATTRFLAG EQU   ATTRPDE+6
USIDPDE  EQU   ALLOCPDL+152+4                                      @26C
USIDPTR  EQU   USIDPDE
USIDFLAG EQU   USIDPDE+6
UTYPPDE  EQU   ALLOCPDL+160+4                                      @26C
UTYPPTR  EQU   UTYPPDE
UTYPFLAG EQU   UTYPPDE+6
UNCNTPDE EQU   ALLOCPDL+168+4                                      @26C
UCNTPTR  EQU   UNCNTPDE
UCNTFLAG EQU   UNCNTPDE+6
POSTPDE  EQU   ALLOCPDL+180+4                                      @26C
POSPTR   EQU   POSTPDE
POSFLAG  EQU   POSTPDE+6
MXVOLPDE EQU   ALLOCPDL+188+4                                      @26C
MXVOLPTR EQU   MXVOLPDE
MXVOLFLG EQU   MXVOLPDE+6
VOLSQPDE EQU   ALLOCPDL+196+4                                      @26C
VOLSQPTR EQU   VOLSQPDE
VOLSQFLG EQU   VOLSQPDE+6
MSVGPDE  EQU   ALLOCPDL+204+4                                      @26C
MSVGPTR  EQU   MSVGPDE
MSVGFLG  EQU   MSVGPDE+6
DSNBUF   EQU   0
MSGTABLE EQU   0
LISTPTR  EQU   MSGTABLE
TMCTPTR  EQU   MSGTABLE+4
MTCPPL   EQU   TMCTPTR
ECBPTR   EQU   MSGTABLE+8
@NM00005 EQU   MSGTABLE+12
MTHIGH   EQU   @NM00005
MSGCSECT EQU   MSGTABLE+16
SW       EQU   MSGTABLE+20
MTPUTLSW EQU   SW
RETMSG   EQU   MSGTABLE+21
SW2      EQU   MSGTABLE+24
MSGID    EQU   MSGTABLE+40
MTINSRTS EQU   MSGTABLE+44
L1       EQU   MTINSRTS
VAR1     EQU   MTINSRTS+1
L2       EQU   MTINSRTS+4
L3       EQU   MTINSRTS+8
L4       EQU   MTINSRTS+12
RET      EQU   0
VALPARM  EQU   0
VALPDE   EQU   VALPARM
VALWORK  EQU   VALPARM+4
VALMSG   EQU   VALPARM+8
PARSRTC  EQU   16
VCBLKPDE EQU   0
VCBLKPTR EQU   VCBLKPDE
VCBLKLEN EQU   VCBLKPDE+4
VCBLKBUF EQU   0
VCDSNPDE EQU   0
VCDSNPTR EQU   VCDSNPDE
VCDSNFLG EQU   VCDSNPDE+6
VCDSNPRS EQU   VCDSNFLG
VCMBRFLG EQU   VCDSNPDE+14
VCMBRPRS EQU   VCMBRFLG
VCDSNBUF EQU   0
VCTERM   EQU   VCDSNBUF
PSTATPDE EQU   0
STATPTR  EQU   PSTATPDE
STATLEN  EQU   PSTATPDE+4
STATFLG  EQU   PSTATPDE+6
SUBNMBR  EQU   PSTATPDE+18
VARPTR   EQU   PSTATPDE+20
CNSTLEN  EQU   VARPTR
VARLEN   EQU   PSTATPDE+24
ANYFLAG  EQU   PSTATPDE+26
SUBPRES  EQU   ANYFLAG
CNSTSUB  EQU   ANYFLAG
CNSTPTR  EQU   PSTATPDE+28
STATBUF  EQU   0
CNSTBUF  EQU   0
VARBUF   EQU   0
BATCPDE  EQU   0
BATCPTR  EQU   BATCPDE
BATCLEN  EQU   BATCPDE+4
BATCFLG  EQU   BATCPDE+6
SUBNUM2  EQU   BATCPDE+18
VAR2PTR  EQU   BATCPDE+20
CNST2LEN EQU   VAR2PTR
ANYFLAG2 EQU   BATCPDE+26
SUB2PRES EQU   ANYFLAG2
CNST2PTR EQU   BATCPDE+28
BATCBUF  EQU   0
CNST2BUF EQU   0
PBLKPDE1 EQU   0
BLOKNBR  EQU   PBLKPDE1+8
PBLKPTR  EQU   PBLKPDE1+12
PBLKLEN  EQU   PBLKPDE1+16
PBLKFLG  EQU   PBLKPDE1+18
PABLKPTR EQU   PBLKPDE1+20
PABLKLEN EQU   PBLKPDE1+24
PABLKFLG EQU   PBLKPDE1+26
PBLKBUF  EQU   0
PABLKBUF EQU   0
PSTSPDE  EQU   0
STAT2PDE EQU   PSTSPDE+8
PCLASPTR EQU   PSTSPDE+12
PCLASLEN EQU   PSTSPDE+16
PCLASFLG EQU   PSTSPDE+18
PCLASPRS EQU   PCLASFLG
CLASBUF1 EQU   0
AREA1    EQU   0
AREA2    EQU   0
PKAREA1  EQU   0
ABLKBUF  EQU   0
ATTRBUF  EQU   0
BLKBUF   EQU   0
CLASSBUF EQU   0
DDNBUF   EQU   0
DIRBUF   EQU   0
I001C    EQU   0
I008P    EQU   0
I015F    EQU   0
I015P    EQU   0
I031F    EQU   0
I031P    EQU   0
I256C    EQU   0
MBRBUF   EQU   0
MSGECB   EQU   0
MSGTMCT  EQU   0
MSVGBUF  EQU   0
MXVOLBUF EQU   0
PASSBUF  EQU   0
POSTBUF  EQU   0
SERBUF   EQU   0
SPACBUF2 EQU   0
SPACEBUF EQU   0
UCNTBUF  EQU   0
USIDBUF  EQU   0
UTYPBUF  EQU   0
VAR2BUF  EQU   0
VSEQBUF  EQU   0
*                                      START UNREFERENCED COMPONENTS
@NM00046 EQU   PSTSPDE+19
@NM00045 EQU   PCLASFLG
@NM00044 EQU   PSTSPDE+10
@NM00043 EQU   PSTSPDE+4
@NM00042 EQU   PSTSPDE
@NM00041 EQU   PBLKPDE1+27
@NM00040 EQU   PBLKPDE1+19
@NM00039 EQU   PBLKPDE1+10
@NM00038 EQU   PBLKPDE1+4
@NM00037 EQU   PBLKPDE1
@NM00036 EQU   BATCPDE+27
@NM00035 EQU   ANYFLAG2
VAR2SUB  EQU   ANYFLAG2
CNST2SUB EQU   ANYFLAG2
@NM00034 EQU   BATCPDE+25
VAR2LEN  EQU   BATCPDE+24
@NM00033 EQU   BATCPDE+19
@NM00032 EQU   BATCPDE+16
@NM00031 EQU   BATCPDE+12
@NM00030 EQU   BATCPDE+8
@NM00029 EQU   BATCPDE+7
@NM00028 EQU   BATCFLG
BATCPRES EQU   BATCFLG
@NM00027 EQU   BATCPDE+5
@NM00026 EQU   PSTATPDE+27
@NM00025 EQU   ANYFLAG
VARSUB   EQU   ANYFLAG
@NM00024 EQU   PSTATPDE+25
@NM00023 EQU   PSTATPDE+19
@NM00022 EQU   PSTATPDE+16
@NM00021 EQU   PSTATPDE+12
@NM00020 EQU   PSTATPDE+8
@NM00019 EQU   PSTATPDE+7
@NM00018 EQU   STATFLG
STATPRES EQU   STATFLG
@NM00017 EQU   PSTATPDE+5
@NM00016 EQU   VCDSNPDE+23
VCPSWFLG EQU   VCDSNPDE+22
VCPSWLEN EQU   VCDSNPDE+20
VCPSWPTR EQU   VCDSNPDE+16
@NM00015 EQU   VCDSNPDE+15
@NM00014 EQU   VCMBRFLG
VCMBRLEN EQU   VCDSNPDE+12
VCMBRPTR EQU   VCDSNPDE+8
@NM00013 EQU   VCDSNPDE+7
@NM00012 EQU   VCDSNFLG
VCDSNLEN EQU   VCDSNPDE+4
VCBLKFLG EQU   VCBLKPDE+6
RETCHAR  EQU   RET+2
RETSIZE  EQU   RET
MSGRTN   EQU   MSGTABLE+60
VAR4     EQU   MTINSRTS+13
HIGHL4   EQU   L4
VAR3     EQU   MTINSRTS+9
HIGHL3   EQU   L3
VAR2     EQU   MTINSRTS+5
HIGHL2   EQU   L2
HIGHL1   EQU   L1
@NM00009 EQU   MSGTABLE+36
@NM00008 EQU   MSGTABLE+32
MTOLDPTR EQU   MSGTABLE+28
@NM00007 EQU   MSGTABLE+25
@NM00006 EQU   SW2
MTPGMSW  EQU   SW2
MTNPLMSW EQU   SW2
MTNOXQSW EQU   SW2
MTDOMSW  EQU   SW2
MT2OLDSW EQU   SW2
MTNHEXSW EQU   SW
MTWTPSW  EQU   SW
MTJOBISW EQU   SW
MTKEY1SW EQU   SW
MTHEXSW  EQU   SW
MTWTOSW  EQU   SW
MTNOIDSW EQU   SW
DSNTERM  EQU   DSNBUF
MSVGRSV2 EQU   MSVGPDE+7
MSVGRSV1 EQU   MSVGFLG
MSVGPRES EQU   MSVGFLG
MSVGLEN  EQU   MSVGPDE+4
VOLRSV2  EQU   VOLSQPDE+7
VOLRSV1  EQU   VOLSQFLG
VOLPRES  EQU   VOLSQFLG
VOLSQLEN EQU   VOLSQPDE+4
MXVRSV2  EQU   MXVOLPDE+7
MXVRSV1  EQU   MXVOLFLG
MXVPRES  EQU   MXVOLFLG
MXVOLLEN EQU   MXVOLPDE+4
POSRSV2  EQU   POSTPDE+7
POSRSV1  EQU   POSFLAG
POSPRES  EQU   POSFLAG
POSLEN   EQU   POSTPDE+4
LABLRSV1 EQU   ALLOCPDL+178+4                                      @26C
LABELPDE EQU   ALLOCPDL+176+4                                      @26C
UCNTRSV2 EQU   UNCNTPDE+7
UCNTRSV1 EQU   UCNTFLAG
UCNTPRES EQU   UCNTFLAG
UCNTLEN  EQU   UNCNTPDE+4
UTYPRSV2 EQU   UTYPPDE+7
UTYPRSV1 EQU   UTYPFLAG
UTYPPRES EQU   UTYPFLAG
UTYPLEN  EQU   UTYPPDE+4
USIDRSV2 EQU   USIDPDE+7
USIDRSV1 EQU   USIDFLAG
USIDPRES EQU   USIDFLAG
USIDLEN  EQU   USIDPDE+4
ATTRRSV2 EQU   ATTRPDE+7
ATTRRSV1 EQU   ATTRFLAG
ATTRPRES EQU   ATTRFLAG
ATTRLEN  EQU   ATTRPDE+4
DIRRSV2  EQU   DIRECPDE+7
DIRRSV1  EQU   DIRFLAGS
DIRPRES  EQU   DIRFLAGS
DIRLEN   EQU   DIRECPDE+4
SPCRSV22 EQU   SPACPDE2+7
SPCRSV12 EQU   SPACFLG2
SPCPRES2 EQU   SPACFLG2
SPACLEN2 EQU   SPACPDE2+4
SPACRSV2 EQU   SPACPDE+7
SPACRSV1 EQU   SPACEFLG
SPACPRES EQU   SPACEFLG
SPACELEN EQU   SPACPDE+4
ABLKRSV2 EQU   ABLKPDE+7
ABLKRSV1 EQU   ABLKFLG
ABLKPRES EQU   ABLKFLG
BLKRSV2  EQU   BLKPDE+7
BLKRSV1  EQU   BLKFLAGS
BLKPRES  EQU   BLKFLAGS
SERNEXT  EQU   SERPDE+8
SERRSV2  EQU   SERPDE+7
SERRSV1  EQU   SERFLAGS
SERPRES  EQU   SERFLAGS
SERLEN   EQU   SERPDE+4
CLASRSV2 EQU   CLASPDE+7
CLASRSV1 EQU   CLASFLG
CLASPRES EQU   CLASFLG
DDNRSV2  EQU   DDNMEPDE+7
DDNRSV1  EQU   DDNFLAGS
DDNPRES  EQU   DDNFLAGS
DDNLEN   EQU   DDNMEPDE+4
DSNNEXT  EQU   DSNPDE+24
PASSRSV2 EQU   DSNPDE+23
PASSRSV1 EQU   PASSFLAG
PASSPRES EQU   PASSFLAG
PASSLEN  EQU   DSNPDE+20
MBRRSV2  EQU   DSNPDE+15
MBRRSV1  EQU   MBRFLAGS
MBRPRES  EQU   MBRFLAGS
MBRLEN   EQU   DSNPDE+12
DSNRSV2  EQU   DSNPDE+7
DSNRSV1  EQU   DSNFLAGS
DSNQUOT  EQU   DSNFLAGS
DSNPRES  EQU   DSNFLAGS
DSNLENGH EQU   DSNPDE+4
DISPPDE  EQU   ALLOCPDL+46
RNDPDE   EQU   ALLOCPDL+44
RLSEPDE  EQU   ALLOCPDL+42
VSEQPDE  EQU   ALLOCPDL+40
PRIVPDE  EQU   ALLOCPDL+38
MAXVPDE  EQU   ALLOCPDL+36
POSPDE   EQU   ALLOCPDL+34
LABLPDE  EQU   ALLOCPDL+32
UCNTPDE  EQU   ALLOCPDL+30
UNITPDE  EQU   ALLOCPDL+28
HOLDPDE  EQU   ALLOCPDL+26
DESTPDE  EQU   ALLOCPDL+24
USINGPDE EQU   ALLOCPDL+22
DIRPDE   EQU   ALLOCPDL+20
SPACEPDE EQU   ALLOCPDL+18
VOLPDE   EQU   ALLOCPDL+14
FILEPDE  EQU   ALLOCPDL+10
DSPDE    EQU   ALLOCPDL+8
@NM00004 EQU   ALLOCPDL+4
@NM00003 EQU   ALLOCPDL
@NM00002 EQU   ALLOCWA+38
@NM00001 EQU   VCFLAGS
PTRS99RB EQU   ALLOCWA+20
TXT      EQU   ALLOCWA+16
SWITCH2  EQU   ALLOCWA+13                                          @26C
TERMOPT  EQU   SWITCH
CONT     EQU   SWITCH
PROMPT   EQU   SWITCH
DSOVRLAY EQU   SWITCH
RBCODE12 EQU   SWITCH
GTPLPTR  EQU   ALLOCWA+8
CPPLPSCB EQU   CPPL+8
*                                      END UNREFERENCED COMPONENTS
@RF00109 EQU   @RC00107
@RC00113 EQU   @EL00001
@RF00157 EQU   @EL00004
@RC00171 EQU   @EL00004
@RF00218 EQU   @EL00008
@RC00237 EQU   @EL00008
@RT00259 EQU   INVASTK
@RC00271 EQU   @RC00269
@RF00302 EQU   @EL00010
@RF00320 EQU   @EL00011
@RC00471 EQU   @EL00016
@RC00481 EQU   @EL00016
@PB00018 EQU   @EL00001
@RC00476 EQU   @RC00471
@PB00017 EQU   @PB00018
@PB00016 EQU   @PB00017
@PB00015 EQU   @PB00016
@PB00014 EQU   @PB00015
@PB00013 EQU   @PB00014
@PB00012 EQU   @PB00013
@PB00011 EQU   @PB00012
@PB00010 EQU   @PB00011
@PB00009 EQU   @PB00010
@PB00008 EQU   @PB00009
@PB00007 EQU   @PB00008
@PB00006 EQU   @PB00007
@PB00005 EQU   @PB00006
@PB00004 EQU   @PB00005
@PB00003 EQU   @PB00004
@PB00002 EQU   @PB00003
@EL01    EQU   @EL00001
@ENDDATA DS    0D
*    END IKJEFD37                                                  0540
         PRINT NOGEN
*/* THE FOLLOWING INCLUDE STATEMENTS WERE FOUND IN THIS PROGRAM.      *
*/*%INCLUDE SYSLIB  (IKJCPPL )                                        *
*/*%INCLUDE SYSLIB  (IKJPPL  )                                        *
*/*%INCLUDE SYSLIB  (IKJZT430)                                        *
*/*%INCLUDE SYSLIB  (IKJEFFMT)                                        *
*/*%INCLUDE SYSLIB  (CVT     )                                        *
         CVT   DSECT=YES
*;                                                                 0540
         END   IKJEFD37,(C'PLS-III',0300,85145)
/*
//*
//STEP17  EXEC PGM=IEBGENER
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  *
  IDENTIFY IKJEFD37('ZP60026')
/*
//SYSUT2   DD  DSN=&&SMPMCS,DISP=(MOD,PASS)
//SYSIN    DD  DUMMY
//*
//STEP18  EXEC SMPREC,WORK='SYSALLDA'
//SMPPTFIN DD  DSN=&&SMPMCS,DISP=(OLD,DELETE)
//SMPCNTL  DD  *
  RECEIVE
          SELECT(ZP60026)
          .
/*
//*
//STEP19CK EXEC SMPAPP,WORK='SYSALLDA'
//SMPCNTL  DD  *
  APPLY
        SELECT(ZP60026)
        CHECK
        .
/*
//*
//STEP19  EXEC SMPAPP,COND=(0,NE),WORK='SYSALLDA'
//SMPCNTL  DD  *
  APPLY
        SELECT(ZP60026)
        DIS(WRITE)
        COMPRESS(ALL)
        .
/*
//
