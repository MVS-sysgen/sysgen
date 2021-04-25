//*
//*        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//*        !!                                     !!
//*        !! DO NOT RENUMBER THIS JOBSTREAM FILE !!
//*        !!                                     !!
//*        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//*
//*
//ZP60027  JOB (SYSGEN),'J01 M38: ZP60027',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             REGION=4096K
/*JOBPARM LINES=100
//JOBCAT   DD  DSN=SYS1.VSAM.MASTER.CATALOG,DISP=SHR
//*
//*  ADD TIMESTAMP SUPPORT FOR LINK EDIT IDENTIFICATION RECORD
//*
//RECEIVE EXEC SMPREC,WORK='SYSALLDA'
//SMPPTFIN DD  *
++USERMOD(ZP60027)    /* ADD TIME OF DAY TO LINK EDIT IDR */  .
++VER(Z038) FMID(EPM1102) PRE(UZ52497,UZ75398)
 /*
   PROBLEM DESCRIPTION:
     THE IDR CREATED BY THE LINKAGE EDITOR DOES NOT CONTAIN A TIME.
       THE ORIGINAL IDENTIFICATION RECORD (IDR) CREATED BY THE
       LINKAGE EDITOR CONTAINS THE DATE OF THE LINK EDIT BUT NOT
       THE TIME.  WITH APAR OW29593 IBM EXTENDED THE IDR CREATED
       BY THE PROGRAM BINDER TO INCLUDE THE TIME.

       THIS USERMOD UPDATES THE LINKAGE EDITOR INTERMEDIATE
       OUTPUT PROCESSOR HEWLFOUT TO EXTEND THE IDR BY FOUR BYTES
       TO APPEND THE TIME-OF-DAY IN PACKED DECIMAL FORMAT.

       FURTHER, THE IDR LISTING PROCESSOR HMBLKIDR OF THE AMBLIST
       SERVICE AID IS ALSO UPDATED TO PRINT THIS TIME IN ITS
       LISTIDR REPORT.

   SPECIAL CONDITIONS:
     NONE.

   COMMENTS:
     PRYCROFT SIX P/L PUBLIC DOMAIN USERMOD FOR MVS 3.8 NUMBER 27.
     THIS USERMOD WAS CONTRIBUTED BY TOM ARMSTRONG.

     THE FOLLOWING MODULES AND/OR MACROS ARE AFFECTED BY THIS USERMOD:
     MODULES:
       HEWLFOUT
       AMBLKIDR
 */.
++ZAP(HEWLFOUT) DISTLIB(AOS04).
  NAME HEWLFOUT
  IDRDATA ZP60027
  VER   0832  4100,0012        LA    WRITECT,LKRECLEN SET REC LEN
  VER   085E  5010,CD1E        ST    DATE,DATESAVE    STORE DATE
  VER   0862  D202,CAEE,CD1F   MVC   LKDATE(DATELEN),DATESAV
  VER   0868  5800,CD1A        L     TIME,TIMESAVE    RESTORE R0
  VER   0AC8  8011,02F5,F7F5,F2E2    LKED IDR HEADER AND COMP ID
  VER   0DC0  0000                   PATCH AREA
  VER   0DC2  0000,0000,0000,0000    PATCH AREA
*
* MAKE CHANGES
*
  REP   0832  4100,0016         LA  WRITECT,LKRECLEN+4 FOR TIME STAMP
  REP   085E  BE17,CAEE         STCM R1,B'0111',LKDATE STORE YYDDDF
  REP   0862  47F0,CDC0         B PATCH
  REP   0866  0700              NOPR R0    NULL OUT REST OF MVC INST
  REP   0AC8  8015,02F5,F7F5,F2E2    LKED IDR HEADER AND COMP ID
  REP   0DC2  8800,0004         SRL R0,4   INSERT HIORDER 0 NIBBLE
  REP   0DC6  5000,CAF1         ST  R0,LKDATE+3 STORE 0HHMMSSS
  REP   0DCA  960F,CAF4         OI  LKDATE+6,X'0F' MAKE PACKED FMT
  REP   0DCE  47F0,C866         B   RETURN TO CODE, EXIT PATCH
++ZAP(HMBLKIDR) DISTLIB(AOS12).
  NAME HMBLKIDR
  IDRDATA ZP60027
  VER  0D48  924B,A069      MVI  105(R10),C'.'  END MSG WITH FULL STOP
  VER  18C0  4040,4040                              MAINT AREA
  VER  18C4  4040,4040                              MAINT AREA
  VER  18C8  4040,4040                              MAINT AREA
  VER  18CC  4040,4040                              MAINT AREA
  VER  18D0  4040,4040,4040                         MAINT AREA
  VER  18D6  4040,4040,4040                         MAINT AREA
*
*  ADRESSABILITY
*  R9  - HMBLKIDR+X'18'
*  R11 - HMBLKIDR+X'1017'
*  R8  -> INPUT IDR RECORD
*  R10 -> PRINT LINE
*
*  MAKE CHANGES
*
  REP 0D48 47F0,B899         BRANCH TO MODULE MAINT AREA
  REP 18B0 9515,8001         L'LKEDRECORD >= X'15'
  REP 18B4 47B0,B8A9         YES, BRANCH. TIME IN RECORD
*                            NO, LKED TIME NOT PRESENT
  REP 18B8 924B,A069         END MSG WITH FULL STOP
  REP 18BC 47F0,9D34         RETURN TO MAINLINE CODE
  REP 18C0 91F0,8012         LKED TIME HAVE LEADING ZERO NIBBLE?
  REP 18C4 4770,B8A1         NO, DO NOT FORMAT TIME OF LKED
  REP 18C8 910F,8015         LKED TIME HAVE X'F' IN RH NIBBLE ?
  REP 18CC 47E0,B8A1         NO, DO NOT FORMAT TIME OF LKED
  REP 18D0 D20C,A069,B8CD    MOVE PATTERN TO PRINT LINE
  REP 18D6 DE0C,A069,8012    FORMAT TIME OF LKED FOR PRINTING
  REP 18DC 924B,A076         END MSG WITH FULL STOP
  REP 18E0 47F0,9D34         RETURN TO MAINLINE CODE
  REP 18E4 4021,C1E3,4020,207A,2020,7A20,2040    PATTERN FOR EDIT
/*
//SMPCNTL  DD  *
  RECEIVE
          SELECT(ZP60027)
          .
/*
//*
//APPLYCK EXEC SMPAPP
//SMPCNTL  DD  *
  APPLY
        SELECT(ZP60027)
        CHECK
        .
/*
//*
//APPLY   EXEC SMPAPP,COND=(0,NE),WORK='SYSALLDA'
//SMPCNTL  DD  *
  APPLY
        SELECT(ZP60027)
        DIS(WRITE)
        .
/*
//
//ZP60028  JOB (SYSGEN),'J02 M39: ZP60028',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             REGION=4096K
/*JOBPARM LINES=100
//JOBCAT   DD  DSN=SYS1.VSAM.MASTER.CATALOG,DISP=SHR
//*
//*  HANDLE UNPRINTABLES IN DUMP FORMAT OF MODULE HEADER.
//*
//STEP1   EXEC PGM=IEBGENER
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  *
++USERMOD(ZP60028)            /* IMPROVE MODULE HEADER FORMAT */  .
++VER(Z038) FMID(EBB1102) PRE(UZ60132)
 /*
   PROBLEM DESCRIPTION:
     DUMP FORMAT OF MODULE HEADER HIDES SOME USEFUL CHARACTERS.
       UZ60132 SUPPRESSED THE OUTPUT OF UNPRINTABLE CHARACTERS
       IN THE FORMATTING OF MODULE HEADERS BY INPLEMENTING
       TRANSLATION OF THE ORIGINAL DATA.  THE SUPPLIED TRANSLATE
       TABLE CONVERTS ALL CHARACTERS TO PERIODS EXCEPT EBCDIC
       DECIMAL DIGITS AND UPPER CASE LETTERS.

       THIS USERMODS ALLOWS THE OUTPUT OF ALL STANDARD KEYBOARD
       CHARACTERS WHILE TRANSLATING THE REST TO THE TILDE OR
       "SQUIGGLE" CHARACTER.

   SPECIAL CONDITIONS:
     ACTION:
       A "CLPA" MUST BE PERFORMED AT IPL TIME FOR THIS SYSMOD TO
       BECOME ACTIVE.

   COMMENTS:
     PRYCROFT SIX P/L PUBLIC DOMAIN USERMOD FOR MVS 3.8 NUMBER 28.
     THIS USERMOD WAS CONTRIBUTED BY TOM ARMSTRONG.

     THE FOLLOWING MODULES AND/OR MACROS ARE AFFECTED BY THIS USERMOD:
     MODULES:
       IEAVAD07
 */.
++MOD(IEAVAD07) DISTLIB(AOSC5).
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
         TITLE 'IEAVAD07 - FORMAT SAVE AREAS ZP60028'
*
*        THE SOURCE FOR IEAVAD07 WAS UPDATED TO THE UZ60132
*        LEVEL FROM THE AVAILABLE MVS 3.8 SOURCE CODE
*        DISTRIBUTION.
*
*        MANY OF THE CHANGES FOR UZ60132 WERE THE RESULT OF THE
*        MVS 3.8 MODULE BEING TRANSLATED BY A DOWN LEVEL VERSION
*        OF PL/S THAT DID NOT GENERATE SYS/370 INSTRUCTIONS LIKE
*        ICM BUT INSTEAD GENERATED A LOAD FOLLOWED BY LOAD ADDRESS.
*
*        THE PRIMARY FUNCTION OF UZ60132 WAS TO PREVENT
*        UNPRINTABLE CHARACTERS APPEARING IN THE EYECATCHER TEXT
*        DURING THE SAVE AREA TRACE DUMP LISTING. THE TRANSLATE
*        TABLE CONVERTED ALL CHARACTERS TO A PERIOD EXCEPT FOR
*        0-9 AND UPPERCASE A-Z.
*
*        ZP60028 PROVIDES FOR ALL VALID PRINTABLE CHARACTERS TO
*        APPEAR IN THE DUMP LISTING. UNPRINTABLE CHARACTERS ARE
*        TRANSLATED TO X'A1' WHICH APPEARS IN THE LISTING AS THE
*        SQIGGLE CHARACTER. ALSO INCLUDED IS SUPPORT FOR THE
*        MODULE EYECATCHER TEXT TO BE BRANCHED AROUND WITH THE
*        BRANCH RELATIVE INSTRUCTION.
*
IEAVAD07 CSECT ,                                                   0001
@MAINENT DS    0H                                                  0001
         USING *,@15                                               0001
         B     @PROLOG                                             0001
         DC    AL1(24)                                   @UZ60132@ 0001
         DC    CL24'IEAVAD07 82300  UZ60132'             @UZ60132@ 0001
         DROP  @15
@PROLOG  ST    @14,12(,@13)                                        0001
         STM   @00,@12,20(@13)                                     0001
         BALR  @11,0                                               0001
@PSTART  DS    0H                                                  0001
         USING @PSTART,@11                                         0001
RBPRFXLN EQU 64
RBPRFLNA EQU 32
PRBLEN   EQU 136
SIRBLEN  EQU 200
TIRBLEN  EQU 136
IRBLEN   EQU 128
SVRBLEN  EQU 224
*                                                            @G33CPHE*/
*                                                                  0109
*   /*****************************************************************/
*   /*                                                               */
*   /* INITIALIZATION SAVES THE CALLERS REGISTERS IN THE PROVIDED    */
*   /* SAVE AREA, BUILDS A CONDITIONAL GETMAIN PARAMETER LIST IN     */
*   /* ABDGMA FOR SPACE FOR ITS OWN SAVE AREA AND FOR SPACE FOR THE  */
*   /* COMPILERS REQUIREMENTS. IF THE GETMAIN FAILS, REGISTERS ARE   */
*   /* RESTORED, GPR 15 IS SET TO 8 AND CONTROL IS RETURNED TO THE   */
*   /* CALLER. OTHERWISE, THE SAVE AREAS ARE CHAINED TOGETHER IN     */
*   /* STANDARD FASHION.                                             */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0110
*   ABDARPTR=PARMPTR;               /* ESTABLISH ADDR OF ABDAREA     */
         LR    ABDARPTR,PARMPTR                                    0110
*   TCBPTR=ABDTCB;                  /* INIT ADDR OF DUMPED TCB       */
         L     TCBPTR,ABDTCB(,ABDARPTR)                            0111
*   PARMPTR=ADDR(ABDASIZE);         /* REG 1 POINTS TO PARM LIST FOR
*                                      GETMAIN                       */
*                                                                  0112
         LA    PARMPTR,ABDASIZE(,ABDARPTR)                         0112
*   /*****************************************************************/
*   /*                                                               */
*   /* GENERATE THE GETMAIN SVC                                      */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0113
*   GENERATE;                                                      0113
*                                                                  0113
         GETMAIN EC,LV=@ENDDATD-@DATD+72,                              *
               SP=253,                                                 *
               A=ABDGMA(ABDARPTR),                                     *
               MF=(E,(1))
*   /*****************************************************************/
*   /*                                                               */
*   /* IF SPACE IS NOT AVAILABLE ROUTINE CANNOT FUNCTION             */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0114
*   IF RETCODE^=0                   /* DID GETMAIN FAIL              */
*     THEN                          /* YES                           */
         LTR   RETCODE,RETCODE                                     0114
         BZ    @RF00114                                            0114
*     DO;                                                          0115
*       RETCODE=8;                  /* SET RETURN CODE               */
         LA    RETCODE,8                                           0116
*       RETURN;                     /* RETURN TO IEAVAD01            */
@EL00001 DS    0H                                                  0117
@EF00001 DS    0H                                                  0117
@ER00001 L     @14,12(,@13)                                        0117
         LM    @00,@12,20(@13)                                     0117
         BR    @14                                                 0117
*     END;                                                         0118
*   ELSE                                                           0119
*     ;                             /* CONTINUE PROCESSING           */
@RF00114 DS    0H                                                  0120
*   PARMPTR=ABDAAREA;               /* RETRIEVE ADDR GOTTEN CORE     */
         L     PARMPTR,ABDAAREA(,ABDARPTR)                         0120
*   SAVEREG->NEXTSAVE=PARMPTR;      /* OLD SAVE POINTS TO NEW SAVE 0121
*                                      AREA                          */
         STCM  PARMPTR,7,NEXTSAVE(SAVEREG)                         0121
*   PARMPTR->PREVSAVE=SAVEREG;      /* NEW SAVE POINTS BACK TO OLD 0122
*                                      SAVE AREA                     */
         STCM  SAVEREG,7,PREVSAVE(PARMPTR)                         0122
*   SAVEREG=PARMPTR;                /* ESTABLISH ADDR OF NEW SAVE  0123
*                                      AREA IN REGISTER 13           */
*                                                                  0123
         LR    SAVEREG,PARMPTR                                     0123
*   /*****************************************************************/
*   /*                                                               */
*   /* ESTABLISH BASE ADDR FOR TEMPORARY STORAGE                     */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0124
*   AUTOPTR=PARMPTR+72;             /* ADDR OF TEMP IS GM AREA+72    */
*                                                                  0124
         LA    AUTOPTR,72                                          0124
         ALR   AUTOPTR,PARMPTR                                     0124
*   /*****************************************************************/
*   /*                                                               */
*   /* GENERATE A USING FOR THE COMPILED CODE AND CLEAR IT           */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0125
*   GENERATE;                                                      0125
*                                                                  0125
         USING @DATD,AUTOPTR           ESTABLISH ADDRESSABILITY
         XC    0(@ENDDATD-@DATD,AUTOPTR),0(AUTOPTR) 0 TEMP SPACE
*   /*****************************************************************/
*   /*                                                               */
*   /* IF THE TASK BEING DUMPED IS FLAGGED COMPLETE THERE CAN BE NO  */
*   /* SAVE AREAS TO DUMP. THEREFORE, THE ROUTINE RETURNS CONTROL TO */
*   /* THE CALLER. OTHERWISE THE RB QUEUE IS SCANNED FOR IRBS. FOR   */
*   /* EACH IRB, THE ROUTINE DISPLAYS THE PP SAVE AREA ASSOCIATED    */
*   /* WITH IT. AFTER THE LAST SUCH SAVE AREA HAS BEEN DISPLAYED,    */
*   /* CONTROL PASSES TO THE NEXT SECTION OF THE ROUTINE             */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0126
*   SAVE=SAVEMSG1;                  /* SET UP INIT MSG IN DUMP       */
         MVC   SAVE(16,ABDARPTR),SAVEMSG1                          0126
*   PARMPTR=ABDARPTR;               /* SET ADDRESS OF PARAM LIST     */
         LR    PARMPTR,ABDARPTR                                    0127
*   CALL IEAVAD11;                  /* PRINT THE HEADING             */
*                                                                  0128
         L     @15,@CV02411                                        0128
         BALR  @14,@15                                             0128
*   /*****************************************************************/
*   /*                                                               */
*   /* IF THE ROUTINE FAILED FOR ANY REASON KILL THE DUMP            */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0129
*   IF RETCODE^=0                   /* OUTPUT FAIL                   */
*     THEN                          /* YES                           */
         LTR   RETCODE,RETCODE                                     0129
         BNZ   @RT00129                                            0129
*     GOTO DIE;                     /* END OF ROUTINE AND DUMP       */
*   ELSE                                                           0131
*     ;                             /* CONTINUE PROCESSING           */
*                                                                  0131
*   /*****************************************************************/
*   /*                                                               */
*   /* IF THE TASK IS FLAGGED COMPLETE RETURN TO CALLER CODE 0       */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0132
*   IF TCBFC='1'B                   /* TASK COMPLETE                 */
*     THEN                          /* YES                           */
         TM    TCBFLGS5-TCB(TCBPTR),TCBFC                          0132
         BO    @RT00132                                            0132
*     GOTO AWAY;                    /* START EXITING                 */
*   ELSE                                                           0134
*     ;                             /* CONTINUE                      */
*   RBSECPTR=TCBRBP;                /* ADDR OF TOP RB                */
*                                                                  0135
         L     RBSECPTR,TCBRBP-TCB(,TCBPTR)                        0135
*   /*****************************************************************/
*   /*                                                               */
*   /* SEARCH FOR IRB. IGNORE ALL OTHERS                             */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0136
*TESTIRB:                           /* REENTER FOR EACH IRB          */
*   IF RBFTP^='010'B                /* IS RB AN IRB                  */
*     THEN                          /* NO                            */
TESTIRB  TM    RBFTP(RBSECPTR),B'01000000'                         0136
         BNO   @RT00136                                            0136
         TM    RBFTP(RBSECPTR),B'10100000'                         0136
         BNZ   @RT00136                                            0136
         L     CURSA,RBPPSAV(,RBSECPTR)                  @UZ60132@
         LA    CURSA,0(,CURSA)                           @UZ60132@
         LTR   CURSA,CURSA                               @UZ60132@
         BZ    NEXTRB                                    @UZ60132@
*     GOTO NEXTRB;                  /* IGNORE THIS RB                */
*   ELSE                                                           0138
*     ;                             /* PROCESS THE IRB               */
*   CURSA=RBPPSAV1;                 /* GET ADR PP SAVE YM2693        */
*        SLR   CURSA,CURSA                               @UZ60132@ 0139
*        ICM   CURSA,7,RBPPSAV1(RBSECPTR)                @UZ60132@ 0139
*   IF CURSA=0                      /* DOES IT EXIST YM2693          */
*     THEN                          /* NO YM2693                     */
*        LTR   CURSA,CURSA                               @UZ60132@ 0140
*        BZ    @RT00140                                  @UZ60132@ 0140
*     GOTO NEXTRB;                  /* IGNORE THIS IRB YM2693        */
*   CALL SAPRINT3;                  /* PRINT SAVE AREA               */
*                                                                  0142
         BAL   @14,SAPRINT3                                        0142
*   /*****************************************************************/
*   /*                                                               */
*   /* IF SUBROUTINE FAILED FOR LACK OF SPACE TERMINATE THE TASK     */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0143
*   IF RETCODE=4                    /* OUT OF SPACE                  */
*     THEN                          /* YES                           */
         C     RETCODE,@CH00070                          @UZ60132@ 0143
         BE    @RT00143                                            0143
*     GOTO DIE;                     /* END OF DUMP                   */
*   ELSE                                                           0145
*     ;                             /* UPR EVENT DOES NOT KILL THE 0145
*                                      DUMP. ONLY THE DISPLAY OF THE
*                                      CURRENT SAVE AREA             */
*                                                                  0145
*   /*****************************************************************/
*   /*                                                               */
*   /* IRB SEARCH LOOP CONTROL                                       */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0146
*NEXTRB:                            /* SKIP RB ENTERS HERE           */
*   IF RBTCBNXT='1'B                /* IS THIS OLDEST RB ON QUEUE    */
*     THEN                          /* YES                           */
NEXTRB   TM    RBTCBNXT(RBSECPTR),B'10000000'                      0146
         BO    @RT00146                                            0146
*     GOTO FORTRC;                  /* TO NEXT DISPLAY               */
*   ELSE                                                           0148
*     ;                             /* CONTINUE RB CHECKING          */
*   RBSECPTR=RBLINKB;               /* ADDR NEXT RB ON QUEUE         */
*        SLR   @10,@10                                   @UZ60132@ 0149
*        ICM   @10,7,RBLINKB(RBSECPTR)                   @UZ60132@ 0149
         L     @10,RBLINK(,RBSECPTR)                     @UZ60132@ 0149
         LA    @10,0(,@10)                               @UZ60132@
         LR    RBSECPTR,@10                                        0149
*   GOTO TESTIRB;                   /* LOOP                          */
*                                                                  0150
         B     TESTIRB                                             0150
*   /*****************************************************************/
*   /*                                                               */
*   /* NORMAL COMPLETION OF SAVE AREA DISPLAY                        */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0151
*AWAY:                                                             0151
*   RETCODE=0;                      /* NORMAL COMPLETION             */
*                                                                  0151
AWAY     SLR   RETCODE,RETCODE                                     0151
*   /*****************************************************************/
*   /*                                                               */
*   /* ABNORMAL COMPLETION OF DISPLAY                                */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0152
*DIE:                                                              0152
*   SAVERET=RETCODE;                /* SAVE RETURN CODE OVER FREEMAIN*/
DIE      LR    SAVERET,RETCODE                                     0152
*   PARMPTR=SAVEREG;                /* ADDR OF SPACE TO BE FREED     */
         LR    PARMPTR,SAVEREG                                     0153
*   SAVEREG=SAVEREG->PREVSAVE;      /* RESTORE CALLERS SAVE AREA   0154
*                                      ADDRESS                       */
*                                                                  0154
*        SLR   @10,@10                                   @UZ60132@ 0154
*        ICM   @10,7,PREVSAVE(SAVEREG)                   @UZ60132@ 0154
         L     @10,SAVEAREA+4(,SAVEREG)                  @UZ60132@
         LA    @10,0(,@10)                               @UZ60132@
         LR    SAVEREG,@10                                         0154
*   /*****************************************************************/
*   /*                                                               */
*   /* FREE THE SAVE AREA AND DYNAMIC STORAGE                        */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0155
*   GENERATE;                                                      0155
         LA    0,@ENDDATD-@DATD+72     SIZE OF SPACE TO BE FREED
         ICM   0,8,K253                SUBPOOL NUMBER
         SVC   10                      FREEMAIN SVC
*   RETCODE=SAVERET;                /* RETSORE RETURN CODE           */
         LR    RETCODE,SAVERET                                     0156
*   RETURN;                         /* RETURN TO MAINLINE            */
*                                                                  0157
         B     @EL00001                                            0157
*   /*****************************************************************/
*   /*                                                               */
*   /* THIS SECTION OF THE MODULE DISPLAYS THE SAVE AREAS STARTING   */
*   /* WITH THE SAVE AREA ADDRESS IN TCBFSA. EACH ADDRESS IS VALIDITY*/
*   /* CHECKED TO INSURE THAT THE SPACE IS ADDRESSABLE BY THE TASK   */
*   /* BEING DUMPED                                                  */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0158
*FORTRC:                                                           0158
*   LASTSA=0;                       /* INIT FIELD AND INDICATE NO  0158
*                                      BACK CHAIN CHECK              */
FORTRC   SLR   @10,@10                                             0158
         ST    @10,LASTSA                                          0158
*   SANUM=0;                        /* INIT SAVEAREA NO      @ZA01900*/
         STH   @10,SANUM                                           0159
*   FRSTENT='1'B;                   /* SIGNAL FIRST TIME     @ZA01900*/
         OI    FRSTENT,B'10000000'                                 0160
*   EPINMSG='0'B;                   /* NO EP FOUND YET       @ZA03859*/
         NI    EPINMSG,B'01111111'                                 0161
*   CURSA=TCBFSAB;                  /* SET TO FIRST PP SAVE AREA     */
*        SLR   CURSA,CURSA                               @UZ60132@ 0162
*        ICM   CURSA,7,TCBFSAB-TCB(TCBPTR)               @UZ60132@ 0162
         L     CURSA,TCBFSA-TCB(,TCBPTR)                 @UZ60132@
         LA    CURSA,0(,CURSA)                           @UZ60132@
*FORLOOP:                           /* ENTERED FOR EACH SAVE AREA ON
*                                      THE FORWARD CHAIN             */
*                                                                  0163
*   /*****************************************************************/
*   /*                                                               */
*   /* TO NEXT SECTION OF THE DISPLAY WHEN THE LAST SAVE AREA ON THE */
*   /* FORWARD QUEUE HAS BEEN DISPLAYED                              */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0163
*   IF CURSA=0                      /* LAST SAVE AREA DISPLAYED      */
*     THEN                          /* YES                           */
FORLOOP  LTR   CURSA,CURSA                                         0163
         BZ    @RT00163                                            0163
*     GOTO PRBCODE;                 /* NEXT SECTION OF DISPLAY       */
*   ELSE                                                           0165
*     ;                             /* START DISPLAY OF SAVE AREA    */
*   CALL VALID;                     /* VALID CHECK SAVE AREA ADDR    */
*                                                                  0166
         BAL   @14,VALID                                           0166
*   /*****************************************************************/
*   /*                                                               */
*   /* TEST RESULTS OF VALIDITY CHECK. A CODE OF FOUR MEANS THAT AN  */
*   /* INVALID BACK CHAIN WAS DETECTED. A MESSAGE AND THE CONTENTS OF*/
*   /* THE SAVE AREA WILL BE DISPLAYED BUT THAT NO MORE SAVE AREAS   */
*   /* WILL BE DUMPED. A CODE OF EIGHT MEANS THAT THE SAVE AREA IS   */
*   /* NOT ADDRESSABLE AND THE SAVE AREA IS NOT DISPLAYED            */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0167
*   IF RETCODE=4                    /* BACK CHAIN INVALID            */
*     THEN                          /* YES                           */
         C     RETCODE,@CH00070                          @UZ60132@ 0167
         BE    @RT00167                                            0167
*     GOTO BADBACK;                 /* PROCESS THE CONDITION         */
*   ELSE                                                           0169
*     ;                             /* CONTINUE CHECKS               */
*   IF RETCODE=8                    /* NOT ADDRESSABLE               */
*     THEN                          /* YES                           */
         C     RETCODE,@CH00072                          @UZ60132@ 0170
         BE    @RT00170                                            0170
*     GOTO PRBCODE;                 /* NEXT SECTION OF DISPLAY       */
*   ELSE                                                           0172
*     ;                             /* DISPLAY THE SAVE AREA         */
*   CALL SAPRINT1;                  /* PRINT SAVE AREA               */
*                                                                  0173
         BAL   @14,SAPRINT1                                        0173
*   /*****************************************************************/
*   /*                                                               */
*   /* THIS SUBROUTINE RETURNS A CODE OF FOUR IF ABDUMP FAILED FOR   */
*   /* LACK OF STORAGE. IF THIS HAPPENS THE DISPLAY AND THE DUMP ARE */
*   /* TERMINATED. A CODE OF EIGHT IMPLIES THAT THE SAVE AREA        */
*   /* DISAPPEARED WHILE IT WAS BEING DISPLAYED. SINCE THIS IS       */
*   /* EQUIVALENT TO IT NOT BEING ADDRESSABLE BEFORE AN ATTEMPT TO   */
*   /* DISPLAY IT WAS MADE, THE FORWARD TRACE IS ENDED AND THE NEXT  */
*   /* SECTION OF THE DISPLAY IS IS ENTERED                          */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0174
*   IF RETCODE=4                    /* ABDUMP FAIL                   */
*     THEN                          /* YES                           */
         C     RETCODE,@CH00070                          @UZ60132@ 0174
         BE    @RT00174                                            0174
*     GOTO DIE;                     /* KILL DISPLAY AND EXIT         */
*   ELSE                                                           0176
*     ;                             /* CONTINUE CODE CHECK           */
*   IF RETCODE=8                    /* UPR EVENT HAPPEN              */
*     THEN                          /* YES                           */
         C     RETCODE,@CH00072                          @UZ60132@ 0177
         BE    @RT00177                                            0177
*     GOTO PRBCODE;                 /* END OF FORWARD TRACE          */
*   ELSE                                                           0179
*     ;                             /* EVERYTHING OK. CONTINUE       */
*   LASTSA=CURSA;                   /* LAST ADDR IS CURRENT ADDR     */
         ST    CURSA,LASTSA                                        0180
*   ABDUPRXT=ADDR(PRBCODE);         /* IF UPR THEN NEXT DISPLAY      */
         LA    @10,PRBCODE                                         0181
         ST    @10,ABDUPRXT(,ABDARPTR)                             0181
*   CURSA=NEXTSAVE;                 /* CURRENT SAVE AREA BECOMES THE
*                                      NEXT SAVE AREA                */
*        SLR   @10,@10                                   @UZ60132@ 0182
*        ICM   @10,7,NEXTSAVE(CURSA)                     @UZ60132@ 0182
         L     @10,SAVEAREA+8(,CURSA)                    @UZ60132@
         LA    @10,0(,@10)                               @UZ60132@
         LR    CURSA,@10                                           0182
*   ABDUPRXT=0;                     /* DISALLOW UPR                  */
         SLR   @10,@10                                             0183
         ST    @10,ABDUPRXT(,ABDARPTR)                             0183
*   GOTO FORLOOP;                   /* PROCESS NEXT SAVE AREA        */
*                                                                  0184
         B     FORLOOP                                             0184
*   /*****************************************************************/
*   /*                                                               */
*   /* WHEN AN INVALID BACK CHAIN IS DETECTED IN THE SAVE AREA CHAIN */
*   /* IT PROBABLY MEANS THAT THE CURRENT SAVE AREA IS INVALID. A    */
*   /* MESSAGE IS PRINTED INDICATING THE INVALID CHAINING AND THE    */
*   /* CONTENTS OF THE SAVE AREA ARE DISPLAYED HOWEVER, NO ATTEMPT IS*/
*   /* MADE TO PRODUCE THE WAS ENTERED MESSAGE SINCE THIS WOULD MEAN */
*   /* TREATING THE WHATEVER AS A SAVE AREA                          */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0185
*BADBACK:                                                          0185
*   IBC=IBCMSG;                     /* INCORRECT BACK CHAIN MSG      */
BADBACK  MVC   IBC(19,ABDARPTR),IBCMSG                             0185
*   PARMPTR=ABDARPTR;               /* SET ADDRESS OF PARAM LIST     */
         LR    PARMPTR,ABDARPTR                                    0186
*   CALL IEAVAD11;                  /* GO TO PRINT THE MESSAGE       */
*                                                                  0187
         L     @15,@CV02411                                        0187
         BALR  @14,@15                                             0187
*   /*****************************************************************/
*   /*                                                               */
*   /* IF OUTPUT FAILED THEN KILL THE DISPLAY AND THE DUMP           */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0188
*   IF RETCODE^=0                   /* ABDUMP FAIL                   */
*     THEN                          /* YES                           */
         LTR   RETCODE,RETCODE                                     0188
         BNZ   @RT00188                                            0188
*     GOTO DIE;                     /* BEGIN TERMINATION             */
*   ELSE                                                           0190
*     ;                                                            0190
*   CALL SAPRINT2;                  /* DISPLAY SAVE AREA CONTENTS    */
*                                                                  0191
         BAL   @14,SAPRINT2                                        0191
*   /*****************************************************************/
*   /*                                                               */
*   /* ONLY CODE THAT IS MEANINGFUL UNDER THE CURRENT CONDITIONS IS  */
*   /* FOUR WHICH MEANS THAT ABDUMP FAILED. IF THIS HAPPENS THE      */
*   /* DISPLAY AND THE DUMP ARE TERMINATED                           */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0192
*   IF RETCODE=4                    /* ABDUMP FAIL                   */
*     THEN                          /* YES                           */
         C     RETCODE,@CH00070                          @UZ60132@ 0192
         BE    @RT00192                                            0192
*     GOTO DIE;                     /* KILL DISPLAY AND DUMP         */
*   ELSE                                                           0194
*     ;                             /* ENTER NEXT SECTION OF DISPLAY */
*                                                                  0194
*   /*****************************************************************/
*   /*                                                               */
*   /* THIS SECTION OF THE ROUTINE DISPLAYS THE TWO MOST RECENT SAVE */
*   /* AREAS ASSOCIATED WITH THE MOST RECENT PRB ON THE ACTIVE       */
*   /* PROGRAM QUEUE. AGAIN, NO ATTEMPT IS MADE TO DISPLAY THE       */
*   /* SUPPOSED SAVE AREAS IF VALIDITY CHECK FAILS                   */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0195
*PRBCODE:                                                          0195
*   RBSECPTR=TCBRBP;                /* GET ADDR MOST RECENT RB       */
PRBCODE  L     RBSECPTR,TCBRBP-TCB(,TCBPTR)                        0195
*   SANUM=SANUM-1;                  /* DECREMENT TO LST RB@ZA01900   */
         LH    @10,SANUM                                           0196
         BCTR  @10,0                                               0196
         STH   @10,SANUM                                           0196
*RBLOOP:                            /* CONTINUE SEARCH FOR PRB       */
*                                                                  0197
*   /*****************************************************************/
*   /*                                                               */
*   /* SEARCH FOR MOST RECENT PRB ON Q                               */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0197
*   IF RBFTP^='000'B                /* CURRENT RB A PRB              */
*     THEN                          /* NO                            */
RBLOOP   TM    RBFTP(RBSECPTR),B'11100000'                         0197
         BZ    @RF00197                                            0197
*     DO;                           /* STEP DOWN RB QUEUE            */
*                                                                  0198
*       /*************************************************************/
*       /*                                                           */
*       /* CHECK IF AT END OF QUEUE                                  */
*       /*                                                           */
*       /*************************************************************/
*                                                                  0199
*       IF RBTCBNXT='1'B            /* LAST RB ON READY QUEUE        */
*         THEN                      /* YES                           */
         TM    RBTCBNXT(RBSECPTR),B'10000000'                      0199
         BO    @RT00199                                            0199
*         GOTO AWAY;                /* NORMAL END OF DISPLAY         */
*       ELSE                                                       0201
*         ;                         /* PROCESS THIS RB               */
*       SAVERB=RBSECPTR;            /* SAVE ADDR OF CURRENT RB       */
         ST    RBSECPTR,SAVERB                                     0202
*       RBSECPTR=RBLINK;            /* SET ADDR NEXT RB ON QUEUE AS
*                                      CURRENT RB                    */
         L     RBSECPTR,RBLINK(,RBSECPTR)                          0203
*       GOTO RBLOOP;                /* CONTINUE SEARCH FOR PRB       */
         B     RBLOOP                                              0204
*     END;                                                         0205
*   ELSE                                                           0206
*     ;                             /* PRB FOUND PROCESS IT          */
@RF00197 DS    0H                                                  0207
*   ABDBPTR=ADDR(RBOPSW)+5;         /* EDIT RESUME ADDR INTO MSG     */
         LA    @10,RBOPSW(,RBSECPTR)                               0207
*        AH    @10,@CH00123                              @UZ60132@ 0207
         AL    @10,@CH00123                              @UZ60132@ 0207
         ST    @10,ABDBPTR(,ABDARPTR)                              0207
*   ABDLPTR=ADDR(ABDLINE);          /* ADDR WHERE DATA TO GO         */
         LA    @10,ABDLINE(,ABDARPTR)                              0208
         ST    @10,ABDLPTR(,ABDARPTR)                              0208
*   ABDLLINE=ADDR(LLINE1);          /* SET LAYOUT LINE ADDR          */
         LA    @10,LLINE1                                          0209
         ST    @10,ABDLLINE(,ABDARPTR)                             0209
*   PARMPTR=ABDARPTR;               /* SET ADDRESS OF PARAM LIST     */
         LR    PARMPTR,ABDARPTR                                    0210
*   CALL IEAVAD41;                  /* FORMAT DATA WITH NO         0211
*                                      INDENTATION FACTOR            */
*                                                                  0211
         L     @15,@CV02432                                        0211
         BALR  @14,@15                                             0211
*   /*****************************************************************/
*   /*                                                               */
*   /* IF FORMAT FAILED THEN TERMINATE DISPLAY AND DUMP              */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0212
*   IF RETCODE^=0                   /* ABDUMP FAIL                   */
*     THEN                          /* YES                           */
         LTR   RETCODE,RETCODE                                     0212
         BNZ   @RT00212                                            0212
*     GOTO DIE;                     /* START TERMINATION             */
*   ELSE                                                           0214
*     ;                             /* CONTINUE                      */
*   ABDCC='-';                      /* SET TO TRIPLE SPACE LINE      */
         MVI   ABDCC(ABDARPTR),C'-'                                0215
*   PARMPTR=ABDARPTR;               /* SET ADDRESS OF PARAM LIST     */
         LR    PARMPTR,ABDARPTR                                    0216
*   CALL IEAVAD11;                  /* PRINT THE LINE                */
*                                                                  0217
         L     @15,@CV02411                                        0217
         BALR  @14,@15                                             0217
*   /*****************************************************************/
*   /*                                                               */
*   /* IF OUTPUT FAILED THEN TERMINATE DISPLAY AND DUMP              */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0218
*   IF RETCODE^=0                   /* ABDUMP FAIL                   */
*     THEN                          /* YES                           */
         LTR   RETCODE,RETCODE                                     0218
         BNZ   @RT00218                                            0218
*     GOTO DIE;                     /* START TERMINATION             */
*   ELSE                                                           0220
*     ;                             /* CONTINUE                      */
*                                                                  0220
*   /*****************************************************************/
*   /*                                                               */
*   /* BACK CHAIN FROM PRB NOT DISPLAYED IF NOT DUMP OF CURRENT TASK */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0221
*   IF ABDTCB^=ABDCTCB              /* DISPLAY CURRENT TASK          */
*     THEN                          /* NO                            */
         CLC   ABDTCB(4,ABDARPTR),ABDCTCB(ABDARPTR)                0221
         BNE   @RT00221                                            0221
*     GOTO AWAY;                    /* NORMAL RETURN                 */
*   ELSE                                                           0223
*     ;                             /* DISPLAY BACK CHAIN            */
*   CURSA=SAVERB->RBGRS13;          /* GET SAVE AREA ADDR FROM RB    */
         L     @10,SAVERB                                          0224
         L     CURSA,RBGRS13(,@10)                                 0224
*   COUNT=2;                        /* SET LOOP CONTROL              */
         MVC   COUNT(2),@CH00114                                   0225
*PRBLOOP:                           /* REENTERED FOR SECOND SAVE AREA
*                                      DISPLAY                       */
*   LASTSA=0;                       /* INDICATE NO BACK CHAIN      0226
*                                      VALIDITY CHECK TO BE DONE     */
PRBLOOP  SLR   @10,@10                                             0226
         ST    @10,LASTSA                                          0226
*   CALL VALID;                     /* CHECK ADDR VALIDITY           */
*                                                                  0227
         BAL   @14,VALID                                           0227
*   /*****************************************************************/
*   /*                                                               */
*   /* ONLY MEANINGFUL RETURN CODE IS EIGHT WHICH IMPLIES THAT THE   */
*   /* SAVE AREA IS NOT ADDRESSABLE. NO DISPLAY IS MADE BUT NORMAL   */
*   /* RETURN IS TAKEN                                               */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0228
*   IF RETCODE^=0                   /* INVALID ADDRESS               */
*     THEN                          /* YES                           */
         SLR   @14,@14                                             0228
         CR    RETCODE,@14                                         0228
         BNE   @RT00228                                            0228
*     GOTO AWAY;                    /* NORMAL COMPLETION             */
*   ELSE                                                           0230
*     ;                             /* DISPLAY SAVE AREA             */
*   COUNT=COUNT-1;                  /* DECREMENT LOOP CONTROL        */
*                                                                  0231
         LH    @10,COUNT                                           0231
         BCTR  @10,0                                               0231
         STH   @10,COUNT                                           0231
*   /*****************************************************************/
*   /*                                                               */
*   /* BEFORE DISPLAYING FIRST SAVE AREA PRINT THE PROCEEDING BACK   */
*   /* MESSAGE                                                       */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0232
*   IF COUNT^=0                     /* FIRST DISPLAY                 */
*     THEN                          /* YES                           */
         CR    @10,@14                                             0232
         BE    @RF00232                                            0232
*     DO;                           /* PRINT HEADING                 */
*       PB=PBMSG;                   /* SET MESSAGE IN LINE           */
         MVC   PB(27,ABDARPTR),PBMSG                               0234
*       PARMPTR=ABDARPTR;           /* SET ADDRESS OF PARAM LIST     */
         LR    PARMPTR,ABDARPTR                                    0235
*       CALL IEAVAD11;              /* PRINT THE LINE                */
*                                                                  0236
         L     @15,@CV02411                                        0236
         BALR  @14,@15                                             0236
*       /*************************************************************/
*       /*                                                           */
*       /* IF OUTPUT FAILED KILL DISPLAY AND DUMP                    */
*       /*                                                           */
*       /*************************************************************/
*                                                                  0237
*       IF RETCODE^=0               /* ABDUMP FAILURE                */
*         THEN                      /* YES                           */
         LTR   RETCODE,RETCODE                                     0237
         BNZ   @RT00237                                            0237
*         GOTO DIE;                 /* KILL DUMP AND DISPLAY         */
*       ELSE                                                       0239
*         ;                         /* END OF HEADING LOGIC          */
*     END;                                                         0240
*   ELSE                                                           0241
*     ;                             /* DISPLAY SAVE AREA             */
@RF00232 DS    0H                                                  0242
*   CALL SAPRINT1;                  /* PRINT SAVE AREA DATA          */
*                                                                  0242
         BAL   @14,SAPRINT1                                        0242
*   /*****************************************************************/
*   /*                                                               */
*   /* RETURN CODES ARE AS BEFORE. A FOUR MEANS ABDUMP FAILED        */
*   /* SOMEWHERE. THE DISPLAY AND DUMP WILL BE TERMINATED. EIGHT     */
*   /* MEANS THE SAVE AREA DISAPPEARED. THE DISPLAY WILL BE STOPPED  */
*   /* BUT RETURN TO CALLER IS NORMAL                                */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0243
*   IF RETCODE=4                    /* ABDUMP FAILURE                */
*     THEN                          /* YES                           */
*        CH    RETCODE,@CH00070                          @UZ60132@ 0243
         C     RETCODE,@CH00070                          @UZ60132@ 0243
         BE    @RT00243                                            0243
*     GOTO DIE;                     /* KILL DISPLAY AND DUMP         */
*   ELSE                                                           0245
*     ;                             /* CHECK FOR BAD SAVE AREA       */
*   IF RETCODE=8                    /* SAVE AREA BECOME BAD          */
*     THEN                          /* YES                           */
*        CH    RETCODE,@CH00072                          @UZ60132@ 0246
         C     RETCODE,@CH00072                          @UZ60132@ 0246
         BE    @RT00246                                            0246
*     GOTO AWAY;                    /* NORMAL END OF ROUTINE         */
*   ELSE                                                           0248
*     ;                             /* CONTINUE PROCESSING           */
*   ABDUPRXT=ADDR(AWAY);            /* EXIT NORMAL IF UPR HAPPENS    */
         LA    @10,AWAY                                            0249
         ST    @10,ABDUPRXT(,ABDARPTR)                             0249
*   CURSA=PREVSAVE;                 /* SET CURRENT SAVE AREA TO SAVE
*                                      AREA BACK CHAINED TO CURRENT
*                                      SAVE AREA                     */
*        SLR   @10,@10                                   @UZ60132@ 0250
*        ICM   @10,7,PREVSAVE(CURSA)                     @UZ60132@ 0250
         L     @10,SAVEAREA+4(,CURSA)                    @UZ60132@ 0250
         LA    @10,0(,@10)                               @UZ60132@ 0250
         LR    CURSA,@10                                           0250
*   ABDUPRXT=0;                     /* DISALLOW UPR                  */
*                                                                  0251
         SLR   @10,@10                                             0251
         ST    @10,ABDUPRXT(,ABDARPTR)                             0251
*   /*****************************************************************/
*   /*                                                               */
*   /* CHECK IF THIS WAS DISPLAY OF SECOND SAVE AREA. IF SO RETURN TO*/
*   /* CALLER. OTHERWISE REPEAT FOR SECOND SAVE AREA                 */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0252
*   IF COUNT=0                      /* TWO SAVE AREAS DISPLAYED      */
*     THEN                          /* YES                           */
         CH    @10,COUNT                                           0252
         BE    @RT00252                                            0252
*     GOTO AWAY;                    /* NORMAL EXIT                   */
*   ELSE                            /* MORE TO DO                    */
*     GOTO PRBLOOP;                 /* REPEAT ONCE MORE              */
         B     PRBLOOP                                             0254
*                                                                  0255
*   /*****************************************************************/
*   /*                                                               */
*   /* THIS SUBROUTINE CONTAINS THREE ENTRY POINTS WHICH PERFORM THE */
*   /* FUNCTIONS OUTLINED BELOW SAPRINT1 CONSTRUCT AND PRINT THE     */
*   /* STANDARD MESSAGES PRECEDING THE DISPLAY OF SAVE AREA. PASS    */
*   /* CONTROL TO SAPRINT2. SAPRINT2 CHECK IF CALLER REQUESTED THAT  */
*   /* ONLY THE HEADINGS BE DISPLAYED. IF SO RETURN TO THE CALLER    */
*   /* WITH A CODE OF 0. OTHERWISE, PASS CONTROL TO SAPRINT3.        */
*   /* SAPRINT3 DISPLAY THE CONTENTS OF THE CURRENT SAVE AREA        */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0255
*SAPRINT1:                          /* START OF SUBROUTINE           */
*   PROCEDURE OPTIONS(NOSAVEAREA,   /* DO NOT OBTAIN SAVE AREA       */
*       DONTSAVE);                  /* DO NOT SAVE REGISTERS         */
SAPRINT1 DS    0H                                                  0256
*   SA1RET=RETREG;                  /* SAVE RETURN ADDR              */
         ST    RETREG,SA1RET                                       0256
*   ABDUPRXT=ADDR(SAEXIT8);         /* ESTABLISH UPR ROUTINE         */
         LA    @10,SAEXIT8                                         0257
         ST    @10,ABDUPRXT(,ABDARPTR)                             0257
*   SA15=R15A;                      /* GET POSSIBLE EP ADDR          */
*        SLR   @10,@10                                   @UZ60132@ 0258
*        ICM   @10,7,R15A(CURSA)                         @UZ60132@ 0258
         L     @10,SAVEAREA+16(,CURSA)                   @UZ60132@ 0258
         LA    @10,0(,@10)                               @UZ60132@ 0258
         ST    @10,SA15                                            0258
*   ABDUPRXT=0;                     /* CANCEL UPR                    */
         SLR   @15,@15                                             0259
         ST    @15,ABDUPRXT(,ABDARPTR)                             0259
*   HEAD=STANDMSG;                  /* SET HEADING INTO LINE         */
*                                                                  0260
         MVC   HEAD(20,ABDARPTR),STANDMSG                          0260
*   /*****************************************************************/
*   /*                                                               */
*   /* POSSIBLE ENTRY POINT MUST LIE WITHIN THE BOUNDARIES OF THE    */
*   /* MACHINE                                                       */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0261
*   IF SA15=0|                      /* ADDRESS ZER                   */
*       SA15>CVTMZ00                /* VALUE HIGHER THAN MAX ADDR    */
*     THEN                          /* YES                           */
         CR    @10,@15                                             0261
         BE    @RT00261                                            0261
         L     @01,CVTPTR                                          0261
         C     @10,CVTMZ00-CVT(,@01)                               0261
         BH    @RT00261                                            0261
*     GOTO SA1EXITA;                /* CANNOT USE ADDR AND STANDARD
*                                      HEADING WILL BE DISPLAYED     */
*   ELSE                                                           0263
*     ;                             /* ATTEMPT TO DISPLAY MORE     0263
*                                      MEANINGFUL HEADING            */
*   ABDUPRXT=ADDR(SA1EXITA);        /* ESTABLISH UPR ROUTINE         */
*                                                                  0264
         LA    @14,SA1EXITA                                        0264
         ST    @14,ABDUPRXT(,ABDARPTR)                             0264
*   /*****************************************************************/
*   /*                                                               */
*   /* CONSIDERING THE VALUE IN SA15 AS THE ENTRY POINT OF THE       */
*   /* MODULE, DETERMINE IF STANDARD MODULE IDS ARE USED             */
*   /* SUPPORT BRANCH AND BRANCH RELATIVE                   ZP60028  */
*   /*****************************************************************/
*                                                                  0265
*   IF (EPI^='47F0'X)&(EPI^='A7F4'X) /* INS AT SA15 A BRANCH ZP60028 */
*     THEN                          /* NO                            */
         CLC   EPI(2,@10),BRCON                             ZP60028 265
         BE    BRANCHIN                                     ZP60028 265
         CLC   EPI(2,@10),BRREL                             ZP60028 265
         BNE   @RT00265                                            0265
BRANCHIN IC    @14,CNT(,@10)        /* L'EYECATCHER      @UZ60132@   */
         STC   @14,LNG
*     GOTO MSGDONE;                 /* LEAVE EP STUFF OUT            */
*   ELSE                                                           0267
*     ;                             /* TRY TO PUT EP IN MSG          */
*   LNG=CNT;                        /* GET LENGTH OF EP IDENT        */
*        MVC   LNG(1),CNT(@10)                           @UZ60132@ 0268
*   ABDUPRXT=0;                     /* DISALLOW UPR INTERRUPTS       */
*                                                                  0269
         ST    @15,ABDUPRXT(,ABDARPTR)                             0269
*   /*****************************************************************/
*   /*                                                               */
*   /* IF LENGTH OF ID IS 0 THEN MESSAGE IS COMPLETE                 */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0270
*   IF LNG=0                        /* MODULE ID 0 LONG              */
*     THEN                          /* YES                           */
         CLI   LNG,0                                               0270
         BE    @RT00270                                            0270
*     GOTO MSGDONE1;                /* NOTHING TO MOVE               */
*   ELSE                                                           0272
*     ;                             /* MOVE ID                       */
*                                                                  0272
*   /*****************************************************************/
*   /*                                                               */
*   /* MAX ID LENGTH FOR MESSAGE IS 70                               */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0273
*   IF LNG>70                       /* OVER SIZE ID                  */
*     THEN                          /* YES                           */
         CLI   LNG,70                                              0273
         BNH   @RF00273                                            0273
*     LNG=70;                       /* SET MAX SIZE                  */
         MVI   LNG,X'46'                                           0274
*   ELSE                                                           0275
*     ;                             /* MOVE ID                       */
@RF00273 DS    0H                                                  0276
*   ATEPPART='AT EP';               /* SET UP MSG                    */
         MVC   ATEPPART(5,ABDARPTR),@CC02443                       0277
*   ABDUPRXT=ADDR(SA1EXITA);        /* ESTABLISH UPR ROUTINE         */
         LA    @10,SA1EXITA                                        0278
         ST    @10,ABDUPRXT(,ABDARPTR)                             0278
*   EPINMSG='1'B;                   /* EP FOUND FOR MSG      @ZA03859*/
*                                                                  0279
         OI    EPINMSG,B'10000000'                                 0279
*   ABDUPRXT=0;                     /* DISALLOW UPR INTERRUPTS       */
         SLR   @10,@10                                             0281
         ST    @10,ABDUPRXT(,ABDARPTR)                             0281
*                                                        @UZ60132@
         TESTAUTH FCTN=1            /* APF AUTHORIZED ?  @UZ60132@
*                                                        @UZ60132@
         SLR   @14,@14                                   @UZ60132@
         CR    @15,@14              /* TEST RETURN CODE  @UZ60132@   */
         BE    BRAUTH               /* BRANCH IF AUTH    @UZ60132@   */
         CLC   TCBPKF-TCB(,TCBPTR),SPKEY8   /* P.P. KEY ? @UZ60132@  */
         BL    BRAUTH               /* BRANCH IF KEY < 8 @UZ60132@   */
         SLR   @15,@15                                   @UZ60132@
         LA    @10,SA1EXITB         /* SUBROUTINE ADDR   @UZ60132@   */
         ST    @10,ABDUPRXT(,ABDARPTR)                   @UZ60132@
         SLR   @10,@10                                   @UZ60132@
         IC    @10,TCBPKF-TCB(,TCBPTR) /* GET CALLERS KEY @UZ60132@  */
         SPKA  0(@10)               /* SET CALLERS KEY   @UZ60132@   */
         SLR   @01,@01                                   @UZ60132@
         IC    @01,LNG              /* GET L'EYECATCHER  @UZ60132@   */
         L     @15,SA15             /* R15 FROM SAVEAREA @UZ60132@   */
         SLR   @10,@10                                   @UZ60132@
         IC    @10,4(@01,@15)                            @UZ60132@
         SPKA  0(@14)               /* RESTORE KEY 0     @UZ60132@   */
BRAUTH   EQU   *                                         @UZ60132@
         LA    @10,SA1EXITA         /* SUBROUTINE ADDR   @UZ60132@   */
         ST    @10,ABDUPRXT(,ABDARPTR)                   @UZ60132@
*   /*****************************************************************/
*   /*                                                               */
*   /* MOVE AND TRANSLATE EYECATCHER                                 */
*   /*                                                               */
*   /*****************************************************************/
*
*   LNG=LNG-1;                      /* DECREMENT FOR EXEC OF MVC     */
         SLR   @10,@10                                             0276
         IC    @10,LNG                                             0276
         BCTR  @10,0                                               0276
         L     @01,SA15                                  @UZ60132@ 0276
         EX    @10,MOVEID           /* MOVE EYECATCHER   @UZ60132@   */
*                                   /* TO OUTPUT LINE    @UZ60132@   */
         STC   @10,LNG              /* STORE DECR LENGTH @UZ60132@   */
         IC    @15,LNG              /* L'EYECATCHER-1 R15@UZ60132@   */
         EX    @15,EXTRAN           /* TRANSLATE         @UZ60132@   */
         B     MSGDONE              /* BRANCH OVER TR TAB@UZ60132@   */
EXTRAN   TR    DESC(1,ABDARPTR),TRANTAB /* EXECUTED INS  @UZ60132@   */
*   /****************************************************@UZ60132@****/
*   /*                                                   @UZ60132@   */
*   /* TRANSLATE TABLE TO REPLACE UNPRINTABLE CHARS      @UZ60132@   */
*   /*                                                   @UZ60132@   */
*   /****************************************************@UZ60132@****/
*                                                        @UZ60132@
*   @UZ60132@ UPDATED BY ZP60028
*
TRANTAB  DC    X'A1A1A1A1A1A1A1A1'  ~~~~~~~~   00 -> 07        ZP60028
         DC    X'A1A1A1A1A1A1A1A1'  ~~~~~~~~   08 -> 0F        ZP60028
         DC    X'A1A1A1A1A1A1A1A1'  ~~~~~~~~   10 -> 17        ZP60028
         DC    X'A1A1A1A1A1A1A1A1'  ~~~~~~~~   18 -> 1F        ZP60028
         DC    X'A1A1A1A1A1A1A1A1'  ~~~~~~~~   20 -> 27        ZP60028
         DC    X'A1A1A1A1A1A1A1A1'  ~~~~~~~~   28 -> 2F        ZP60028
         DC    X'A1A1A1A1A1A1A1A1'  ~~~~~~~~   30 -> 37        ZP60028
         DC    X'A1A1A1A1A1A1A1A1'  ~~~~~~~~   38 -> 3F        ZP60028
         DC    X'40A1A1A1A1A1A1A1'  ~~~~~~~~   40 -> 47        ZP60028
         DC    X'A1A14A4B4C4D4E4F'  ~~ó.<(+|   48 -> 4F        ZP60028
         DC    X'50A1A1A1A1A1A1A1'  &~~~~~~~   50 -> 57        ZP60028
         DC    X'A1A15A5B5C5D5E5F'  ~~!$*);^   58 -> 5F        ZP60028
         DC    X'6061A1A1A1A1A1A1'  -/~~~~~~   60 -> 67        ZP60028
         DC    X'A1A16A6B6C6D6E6F'  ~~ ,%_>?   68 -> 6F        ZP60028
         DC    X'A1A1A1A1A1A1A1A1'  ~~~~~~~~   70 -> 77        ZP60028
         DC    X'A1797A7B7C7D7E7F'  ~`:#@'="   78 -> 7F        ZP60028
         DC    X'A181828384858687'  ~ABCDEFG   80 -> 87        ZP60028
         DC    X'8889A1A1A1A1A1A1'  HI~~~~~~   88 -> 8F        ZP60028
         DC    X'A191929394959697'  ~JKLMNOP   90 -> 97        ZP60028
         DC    X'9899A1A1A1A1A1A1'  QR~~~~~~   98 -> 9F        ZP60028
         DC    X'A1A1A2A3A4A5A6A7'  ~~STUVWX   A0 -> A7        ZP60028
         DC    X'A8A9A1A1A1ADA1A1'  YZ~~~[~~   A8 -> AF        ZP60028
         DC    X'A1A1A1A1A1A1A1A1'  ~~~~~~~~   B0 -> B7        ZP60028
         DC    X'A1A1A1A1A1BDA1A1'  ~~~~~]~~   B8 -> BF        ZP60028
         DC    X'C0C1C2C3C4C5C6C7'  {ABCDEFG   C0 -> C7        ZP60028
         DC    X'C8C9A1A1A1A1A1A1'  HI~~~~~~   C8 -> CF        ZP60028
         DC    X'D0D1D2D3D4D5D6D7'  }JKLMNOP   D0 -> D7        ZP60028
         DC    X'D8D9A1A1A1A1A1A1'  QR~~~~~~   D8 -> DF        ZP60028
         DC    X'E0A1E2E3E4E5E6E7'  \~STUVWX   E0 -> E7        ZP60028
         DC    X'E8E9A1A1A1A1A1A1'  YZ~~~~~~   E8 -> EF        ZP60028
         DC    X'F0F1F2F3F4F5F6F7'  01234567   F0 -> F7        ZP60028
         DC    X'F8F9A1A1A1A1A1A1'  89~~~~~~   F8 -> FF        ZP60028
*
*MSGDONE:                           /* ENTERED IF NO UPR             */
*   ABDUPRXT=0;                     /* DISALLOW UPR INTERRUPTS       */
MSGDONE  SLR   @10,@10                                             0281
         ST    @10,ABDUPRXT(,ABDARPTR)                             0281
*MSGDONE1:                          /* ENTERED WHEN UPR OCCURRED     */
*   RBSECPTR=TCBRBP;                /* ADDR OF TOP RB                */
MSGDONE1 L     RBSECPTR,TCBRBP-TCB(,TCBPTR)                        0282
*CKRBLOOP:                          /* ENTERED FOR EACH RB           */
*   CALL TESTEP1;                   /* CHECK RB                      */
*                                                                  0283
CKRBLOOP BAL   @14,TESTEP1                                         0283
*   /*****************************************************************/
*   /*                                                               */
*   /* A RETURN CODE OF 4 IMPLIES THAT ABDUMP FAILED. A RETURN CODE  */
*   /* OF EIGHT IMPLIES AN UNEXPECTED UPR. IN EITHER CASE THE DISPLAY*/
*   /* AND THE DUMP ARE TERMINATED                                   */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0284
*   IF RETCODE=4|                   /* ABDUMP FAIL                   */
*       RETCODE=8                   /* UPR                           */
*     THEN                          /* YES                           */
         C     RETCODE,@CH00070                          @UZ60132@ 0284
         BE    @RT00284                                            0284
         C     RETCODE,@CH00072                          @UZ60132@ 0284
         BE    @RT00284                                            0284
*     GOTO TERMINAL;                /* END OF DUMP                   */
*   ELSE                                                           0286
*     ;                             /* CHECK IF DESIRED RB FND       */
*                                                                  0286
*   /*****************************************************************/
*   /*                                                               */
*   /* IF RIGHT RB FOUND THEN BREAK LOOP                             */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0287
*   IF RETCODE=0                    /* RIGHT RB FOUND                */
*     THEN                          /* YES                           */
         LTR   RETCODE,RETCODE                                     0287
         BZ    @RT00287                                            0287
*     GOTO CKHEAD;                  /* BREAK LOOP                    */
*   ELSE                                                           0289
*     ;                             /* CONTINUE SEARCH               */
*                                                                  0289
*   /*****************************************************************/
*   /*                                                               */
*   /* IF LAST RB CHECKED GO TRY LLE                                 */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0290
*   IF RBTCBNXT='1'B                /* END OF RB CHAIN REACHED       */
*     THEN                          /* YES                           */
         TM    RBTCBNXT(RBSECPTR),B'10000000'                      0290
         BO    @RT00290                                            0290
*     GOTO TRYLLE;                  /* SEARCH LLE FOR ENTRY POINT    */
*   ELSE                                                           0292
*     ;                             /* TRY NEXT RB                   */
*   RBSECPTR=RBLINKB;               /* UPDATE RB POINTER             */
*        SLR   @10,@10                                   @UZ60132@ 0293
*        ICM   @10,7,RBLINKB(RBSECPTR)                   @UZ60132@ 0293
         L     @10,RBLINK(,RBSECPTR)                     @UZ60132@ 0293
         LA    @10,0(,@10)                               @UZ60132@
         LR    RBSECPTR,@10                                        0293
*   GOTO CKRBLOOP;                  /* CONTINUE SEARCH               */
         B     CKRBLOOP                                            0294
*SAEXIT8:                           /* A UPR DID NOT OCCUR           */
*   ABDUPRXT=0;                     /* DISALLOW UPR                  */
SAEXIT8  SLR   @10,@10                                             0295
         ST    @10,ABDUPRXT(,ABDARPTR)                             0295
*   RETCODE=8;                      /* INDICATE UPR EVENT            */
         LA    RETCODE,8                                           0296
*TERMINAL:                          /* END OF SUBROUTINE             */
*   RETREG=SA1RET;                  /* RESTORE RETURN ADDR           */
TERMINAL L     RETREG,SA1RET                                       0297
*   RETURN;                         /* EXIT TO CALLER                */
@EL00002 DS    0H                                                  0298
@EF00002 DS    0H                                                  0298
@ER00002 BR    @14                                                 0298
*TRYLLE:                            /* LLE SEARCH FOR ENTRY POINT    */
*   LLEPTR=TCBLLS;                  /* ADDR OF MOST RECENT LLE       */
TRYLLE   L     LLEPTR,TCBLLS-TCB(,TCBPTR)                          0299
*LLELOOP:                           /* REENTER FOR EACH LLE          */
*                                                                  0300
*   /*****************************************************************/
*   /*                                                               */
*   /* WHEN LAST LLE HAS BEEN CHECKED THEN EP CANNOT BE FOUND AND NO */
*   /* HEADING IS DISPLAYED                                          */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0300
*   IF LLEPTR=0                     /* AT END OF LLE QUEUE           */
*     THEN                          /* YES                           */
LLELOOP  LTR   LLEPTR,LLEPTR                                       0300
         BNZ   @RF00300                                            0300
*     DO;                           /* LOAD MODULE UNKNOW    @ZA03859*/
*       IF EPINMSG='1'B THEN        /* IF EP WAS FOUND       @ZA03859*/
         TM    EPINMSG,B'10000000'                                 0302
         BNO   @RF00302                                            0302
*         DO;                       /* THEN PUT OUT MSG      @ZA03859*/
*           MODNAME='UNKNOWN ';     /* SET NAME IN MSG       @ZA03859*/
         MVC   MODNAME(8,ABDARPTR),@CC02456                        0304
*           EPINMSG='0'B;           /* TURN OFF INDIC.       @ZA03859*/
         NI    EPINMSG,B'01111111'                                 0305
*           GOTO WRITE;             /* GO PUT OUT MSG        @ZA03859*/
         B     WRITE                                               0306
*         END;                      /* END EP FOUND          @ZA03859*/
*       ELSE                        /* OTHERWISE, NO MSG     @ZA03859*/
*         GOTO SA1EXITA;            /* BYPASS MESSAGE        @ZA03859*/
*     END;                          /* END LLEPTR ZERO       @ZA03859*/
*   ELSE                                                           0310
*     ;                             /* CHECK LLE FOR EP              */
@RF00300 DS    0H                                                  0311
*   CALL TESTEP2;                   /* TEST LLE                      */
*                                                                  0311
         BAL   @14,TESTEP2                                         0311
*   /*****************************************************************/
*   /*                                                               */
*   /* SUBROUTINE EXITS FOR RETURN CODE OF 4 OR 8 WHICH MEAN THAT    */
*   /* EITHER ABDUMP FAILED OR AN UNEXPECTED UPR HAPPENED            */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0312
*   IF RETCODE=4|                   /* ABDUMP FAIL                   */
*       RETCODE=8                   /* UNEXPECTED UPR                */
*     THEN                          /* YES                           */
         C     RETCODE,@CH00070                          @UZ60132@ 0312
         BE    @RT00312                                            0312
         C     RETCODE,@CH00072                          @UZ60132@ 0312
         BE    @RT00312                                            0312
*     GOTO TERMINAL;                /* END OF SUBROUTINE             */
*   ELSE                                                           0314
*     ;                             /* CONTINUE CHECKS               */
*                                                                  0314
*   /*****************************************************************/
*   /*                                                               */
*   /* A RETURN CODE OF ZERO IMPLIES RIGHT LLE FOUND AND HEADING WAS */
*   /* DISPLAYED                                                     */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0315
*   IF RETCODE=0                    /* LLE FOUND                     */
*     THEN                          /* YES                           */
         LTR   RETCODE,RETCODE                                     0315
         BZ    @RT00315                                            0315
*     GOTO CKHEAD;                  /* HEADING DONE AND DISPLAYED    */
*   ELSE                                                           0317
*     ;                             /* CONTINUE SEARCH               */
*   LLEPTR=LLECHN;                  /* NEXT LLE ON QUEUE             */
         L     LLEPTR,LLECHN(,LLEPTR)                              0318
*   GOTO LLELOOP;                   /* CONTINUE SEARCH               */
         B     LLELOOP                                             0319
*                                                        @UZ60132@
*   RESET PSW KEY TO ZERO PRIOR TO SA1EXITA              @UZ60132@
*                                                        @UZ60132@
SA1EXITB SLR   @10,@10                                   @UZ60132@
         SPKA  0(@10)                                    @UZ60132@
*
*SA1EXITA:                          /* NO HEADING FOR REASON         */
*   ABDLINEA=' ';                   /* BLANK OUTPUT LINE             */
SA1EXITA MVI   ABDLINEA+1(ABDARPTR),C' '                           0320
         MVC   ABDLINEA+2(203,ABDARPTR),ABDLINEA+1(ABDARPTR)       0320
         MVI   ABDLINEA(ABDARPTR),C' '                             0320
         SLR   @10,@10                                   @UZ60132@
         ST    @10,ABDUPRXT(,ABDARPTR)                   @UZ60132@
*CKHEAD:                            /* HEADING HAS BEEN PRODUCED     */
*                                                                  0321
*   /*****************************************************************/
*   /*                                                               */
*   /* IF THE USER REQUESTED THAT ONLY THE HEADINGS BE PRODUCED THE  */
*   /* SUBROUTINE HAS COMPLETED NORMALLY                             */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0321
*   IF APFSAVE2='1'B                /* HEADINGS ONLY                 */
*     THEN                          /* YES                           */
CKHEAD   TM    APFSAVE2(ABDARPTR),B'01000000'                      0321
         BNO   @RF00321                                            0321
*     DO;                           /* END SUBROUTINE                */
*       RETCODE=0;                  /* SET NORMAL END                */
         SLR   RETCODE,RETCODE                                     0323
*       GOTO TERMINAL;              /* START RETURN                  */
         B     TERMINAL                                            0324
*     END;                                                         0325
*   ELSE                                                           0326
*     ;                             /* DISPLAY CONTENTS OF SA        */
@RF00321 DS    0H                                                  0327
*EDITSA:                            /* DISPLAY SA CONTENTS           */
*   ABDCC='0';                      /* DOUBLE SPACING                */
EDITSA   MVI   ABDCC(ABDARPTR),C'0'                                0327
*   WORK=CURSA;                     /* STORE DATA ADDR               */
         ST    CURSA,WORK(,ABDARPTR)                               0328
*   ABDBPTR=ADDR(WORK)+1;           /* FORM ADDR OF SA INTO LINE     */
         LA    @10,WORK(,ABDARPTR)                                 0329
         AL    @10,@CF00065                              @UZ60132@ 0329
         ST    @10,ABDBPTR(,ABDARPTR)                              0329
*   ABDLPTR=ADDR(ABDLINE);          /* ADDR WHERE DATA TO GO         */
         LA    @10,ABDLINE(,ABDARPTR)                              0330
         ST    @10,ABDLPTR(,ABDARPTR)                              0330
*   ABDLLINE=ADDR(SAHDLINE);        /* SET LAYOUT LINE ADDR          */
         LA    @10,SAHDLINE                                        0331
         ST    @10,ABDLLINE(,ABDARPTR)                             0331
*   PARMPTR=ABDARPTR;               /* SET ADDRESS OF PARAM LIST     */
         LR    PARMPTR,ABDARPTR                                    0332
*   CALL IEAVAD41;                  /* FIRST SAVE AREA LINE          */
*                                                                  0333
         L     @15,@CV02432                                        0333
         BALR  @14,@15                                             0333
*   /*****************************************************************/
*   /*                                                               */
*   /* IF ABDUMP FAILED RETURN TO CALLER WITH CODE                   */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0334
*   IF RETCODE^=0                   /* ABDUMP FAIL                   */
*     THEN                          /* YES                           */
         LTR   RETCODE,RETCODE                                     0334
         BNZ   @RT00334                                            0334
*     GOTO TERMINAL;                /* START EXIT                    */
*   ELSE                                                           0336
*     ;                             /* CONTINUE DISPLAY              */
*   LINCNT=3;                       /* NUMBER OF LINES IN DISPLAY    */
         MVI   LINCNT,X'03'                                        0337
*   ABDBPTR=CURSA;                  /* FORMAT CURRENT SAVE AREA      */
         ST    CURSA,ABDBPTR(,ABDARPTR)                            0338
*   ABDLLINE=ADDR(SALLINE1);        /* ADDR OF FIRST LAYOUT LINE     */
         LA    @10,SALLINE1                                        0339
         ST    @10,ABDLLINE(,ABDARPTR)                             0339
*FORMLOOP:                          /* ENTER FOR EA DISPLAY LINE     */
*   UPRFMAT='1'B;                   /* UPR ALLOWABLE IN FORMAT       */
FORMLOOP OI    UPRFMAT(ABDARPTR),B'10000000'                       0340
*   PARMPTR=ABDARPTR;               /* SET ADDRESS OF PARAM LIST     */
         LR    PARMPTR,ABDARPTR                                    0341
*   CALL IEAVAD31;                  /* FIRST SAVE AREA LINE          */
*                                                                  0342
         L     @15,@CV02466                                        0342
         BALR  @14,@15                                             0342
*   /*****************************************************************/
*   /*                                                               */
*   /* IF CALL NOT OK RETURN TO CALLER WITH CODE                     */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0343
*   IF RETCODE^=0                   /* RESULTS OK                    */
*     THEN                          /* NO                            */
         LTR   RETCODE,RETCODE                                     0343
         BNZ   @RT00343                                            0343
*     GOTO TERMINAL;                /* BACK TO CALLER                */
*   ELSE                                                           0345
*     ;                             /* CONTINUE DISPLAY              */
*   PARMPTR=ABDARPTR;               /* SET ADDRESS OF PARAM LIST     */
         LR    PARMPTR,ABDARPTR                                    0346
*   CALL IEAVAD11;                  /* OUTPUT THE LINE               */
*                                                                  0347
         L     @15,@CV02411                                        0347
         BALR  @14,@15                                             0347
*   /*****************************************************************/
*   /*                                                               */
*   /* IF OUTPUT FAILED RETURN TO CALLER WITH CODE                   */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0348
*   IF RETCODE^=0                   /* OUTPUT FAIL                   */
*     THEN                          /* YES                           */
         LTR   RETCODE,RETCODE                                     0348
         BNZ   @RT00348                                            0348
*     GOTO TERMINAL;                /* BACK TO CALLER                */
*   ELSE                                                           0350
*     ;                             /* CONTINUE DISPLAY              */
*   LINCNT=LINCNT-1;                /* DECR COUNT BY ONE             */
*                                                                  0351
         SLR   @10,@10                                             0351
         IC    @10,LINCNT                                          0351
         BCTR  @10,0                                               0351
         STC   @10,LINCNT                                          0351
*   /*****************************************************************/
*   /*                                                               */
*   /* SPLIT LOGIC FOR SECOND AND THIRD LINES OF DISPLAY             */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0352
*   IF LINCNT=2                     /* SECOND LINE TO BE DISPLAYED   */
*     THEN                          /* YES                           */
         CLI   LINCNT,2                                            0352
         BE    @RT00352                                            0352
*     GOTO SAL2;                    /* SECOND LINE LOGIC             */
*   ELSE                                                           0354
*     ;                             /* CONTINUE                      */
*   IF LINCNT=1                     /* DOING THIRD LINE              */
*     THEN                          /* YES                           */
         CLI   LINCNT,1                                            0355
         BE    @RT00355                                            0355
*     GOTO SAL3;                    /* THIRD LINE LOGIC              */
*   ELSE                                                           0357
*     ;                             /* DISPLAY IS DONE               */
*   RETCODE=0;                      /* COMPLETED PROCESSING          */
         SLR   RETCODE,RETCODE                                     0358
*   GOTO TERMINAL;                  /* EXIT TO CALLER                */
         B     TERMINAL                                            0359
*SAL2:                              /* SECOND LINE LOGIC             */
*   ABDBPTR=CURSA+24;               /* NEXT SECTION OF SAVE AREA     */
SAL2     LA    @10,24                                              0360
         ALR   @10,CURSA                                           0360
         ST    @10,ABDBPTR(,ABDARPTR)                              0360
*   ABDLLINE=ADDR(SALLINE2);        /* LINE 2 OF DUMP                */
         LA    @10,SALLINE2                                        0361
         ST    @10,ABDLLINE(,ABDARPTR)                             0361
*   GOTO FORMLOOP;                  /* NEXT DISPLAY LINE             */
         B     FORMLOOP                                            0362
*SAL3:                              /* THIRD LINE LOGIC              */
*   ABDBPTR=CURSA+48;               /* NEXT SECTION OF SAVE AREA     */
SAL3     LA    @10,48                                              0363
         ALR   @10,CURSA                                           0363
         ST    @10,ABDBPTR(,ABDARPTR)                              0363
*   ABDLLINE=ADDR(SALLINE3);        /* LINE 2 OF DUMP                */
         LA    @10,SALLINE3                                        0364
         ST    @10,ABDLLINE(,ABDARPTR)                             0364
*   GOTO FORMLOOP;                  /* NEXT DISPLAY LINE             */
         B     FORMLOOP                                            0365
*SAPRINT2:                          /* SAPRINT2 STARTS HERE          */
*   ENTRY;                                                         0366
SAPRINT2 DS    0H                                                  0367
*   SA1RET=RETREG;                  /* SAVE RETURN ADDRESS           */
         ST    RETREG,SA1RET                                       0367
*   GOTO CKHEAD;                    /* ENTER CODE AT PROPER POINT    */
         B     CKHEAD                                              0368
*SAPRINT3:                          /* SAPRINT3 START                */
*   ENTRY;                                                         0369
SAPRINT3 DS    0H                                                  0370
*   SA1RET=RETREG;                  /* SAVE RETURN ADDRESS           */
         ST    RETREG,SA1RET                                       0370
*   GOTO EDITSA;                    /* ENTER CODE AT PROPER POINT    */
         B     EDITSA                                              0371
*   END SAPRINT1;                   /* END OF SUBROUTINE             */
*                                                                  0373
*   /*****************************************************************/
*   /*                                                               */
*   /* THIS SUBROUTINE DETERMINES IF THE SUPPOSED ENTRY POINT, THE   */
*   /* ADDRESS FROM R15 IN THE SAVE AREA, IS CONTAINED WITHIN THE    */
*   /* LIMITS OF THE PROGRAM DESCRIBED BY THE RB OR THE LLE SUPPLIED */
*   /* BY THE CALLER. IF NOT, RETURN TO THE CALLER WITH A CODE.      */
*   /* OTHERWISE, MOVE THE MODULE NAME TO THE MESSAGE BEING BUILT,   */
*   /* COMPLETE THE INFORMATION IN THE MESSAGE PERTAINING TO HOW     */
*   /* ENTERED AND WRITE THE HEADING. SET RETURN CODE TO CALLER TO   */
*   /* ZERO                                                          */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0373
*TESTEP1:                           /* START OF SUBROUTINE           */
*   PROCEDURE OPTIONS(NOSAVEAREA,   /* DO NOT OBTAIN SAVE AREA       */
*       DONTSAVE);                  /* DO NOT SAVE ANY REGISTERS     */
TESTEP1  DS    0H                                                  0374
*   TESTR14=RETREG;                 /* SAVE RETURN ADDRESS           */
*                                                                  0374
         ST    RETREG,TESTR14                                      0374
*   /*****************************************************************/
*   /*                                                               */
*   /* IF SUPPLIED RB IS NOT PRB BACK TO CALLER                      */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0375
*   IF RBFTP^='000'B                /* RB A PRB                      */
*     THEN                          /* NO                            */
         TM    RBFTP(RBSECPTR),B'11100000'                         0375
         BNZ   @RT00375                                            0375
*     GOTO RC12;                    /* BACK TO CALLER                */
*   ELSE                                                           0377
*     ;                             /* CONTINUE CHECKS               */
*   CDENTPTR=RBCDE1;                /* GET ADDR OF CDE FOR PGM       */
*                                                                  0378
*        SLR   CDENTPTR,CDENTPTR                         @UZ60132@ 0378
*        ICM   CDENTPTR,7,RBCDE1(RBSECPTR)               @UZ60132@ 0378
         L     CDENTPTR,RBCDE(,RBSECPTR)                 @UZ60132@ 0378
         LA    CDENTPTR,0(,CDENTPTR)                     @UZ60132@
*   /*****************************************************************/
*   /*                                                               */
*   /* IF PRB HAS NO CDE THEN IT CANNOT BE PROGRAM FOR EP            */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0379
*   IF CDENTPTR^=0                  /* CDE WITH THE PRB              */
*     THEN                          /* YES                           */
         LTR   CDENTPTR,CDENTPTR                                   0379
         BNZ   @RT00379                                            0379
*     GOTO TEST2A;                  /* CONTINUE CHECKS               */
*   ELSE                                                           0381
*     ;                             /* BACK TO CALLER                */
*RC12:                              /* SET WRONG PRB OR LLE CODE     */
*   RETCODE=12;                     /* SET CODE TO CALLER            */
RC12     LA    RETCODE,12                                          0382
*TESTEXIT:                          /* COMMON RETURN LOGIC           */
*   RETREG=TESTR14;                 /* RESTORE RETURN ADDR           */
TESTEXIT L     RETREG,TESTR14                                      0383
*   RETURN;                         /* BACK TO CALLER                */
*                                                                  0384
@EL00003 DS    0H                                                  0384
@EF00003 DS    0H                                                  0384
@ER00003 BR    @14                                                 0384
*   /*****************************************************************/
*   /*                                                               */
*   /* ENTRY POINT TO SUBROUTINE IF LLE SUPPLIED                     */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0385
*TESTEP2:                           /* START LOGIC                   */
*   ENTRY;                                                         0385
TESTEP2  DS    0H                                                  0386
*   TESTR14=RETREG;                 /* SAVE RET ADDR                 */
         ST    RETREG,TESTR14                                      0386
*   CDENTPTR=LLECDPT;               /* ESTAB CDE ADDR FROM LLE       */
         L     CDENTPTR,LLECDPT(,LLEPTR)                           0387
*TEST2A:                            /* LOGIC FOR BOTH ENRTIES        */
*                                                                  0388
*   /*****************************************************************/
*   /*                                                               */
*   /* IF MODULE IS BEING LOADED IT CANNOT BE DESIRED ONE            */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0388
*   IF CDNIC='1'B                   /* PARTIALLY LOADED              */
*       |(CDMIN='0'B&CDXLE='0'B)    /* OR CDE BUILD BY INDENT.     0388
*                                                            @ZA09231*/
*     THEN                          /* YES                           */
TEST2A   TM    CDNIC(CDENTPTR),B'01000000'                         0388
         BO    @RT00388                                            0388
         TM    CDMIN(CDENTPTR),B'00000100'                         0388
         BNZ   @RF00388                                            0388
         TM    CDXLE(CDENTPTR),B'00100000'                         0388
         BZ    @RT00388                                            0388
*     GOTO RC12;                    /* SET RET COD AND RETURN        */
*   ELSE                                                           0390
*     ;                             /* CHECK IF CDE FOR RIGHT MOD    */
*                                                                  0390
@RF00388 DS    0H                                                  0391
*   /*****************************************************************/
*   /*                                                               */
*   /* REPLACE MINOR WITH ADDR OF MAJOR                              */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0391
*   IF CDMIN='1'B                   /* CDE FOR MINOR                 */
*     THEN                          /* YES                           */
         TM    CDMIN(CDENTPTR),B'00000100'                         0391
         BNO   @RF00391                                            0391
*     CDENTPTR=CDXLMJP;             /* REPLACE WITH ADDR OF MAJOR    */
         L     CDENTPTR,CDXLMJP(,CDENTPTR)                         0392
*   ELSE                                                           0393
*     ;                             /* CONTINUE CHECK                */
@RF00391 DS    0H                                                  0394
*   XTLSTPTR=CDXLMJP;               /* GET ADDR OF EXTENT LIST       */
         L     XTLSTPTR,CDXLMJP(,CDENTPTR)                         0394
*   NOENTSP=XTLNRFAC;               /* GET NUMBER OF LIST ENTRIES    */
*                                                                  0395
         L     NOENTSP,XTLNRFAC(,XTLSTPTR)                         0395
*   /*****************************************************************/
*   /*                                                               */
*   /* CALC ADDR OF PTR TO BEGINNING OF BLOCK FIELDS                 */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0396
*   EXSTARTP=ADDR(XTLMSBAA)+(NOENTS-1)*4;                          0396
         LA    @10,XTLMSBAA(,XTLSTPTR)                             0396
         LR    @15,NOENTS                                          0396
         BCTR  @15,0                                               0396
         SLA   @15,2                                               0396
         ALR   @10,@15                                             0396
         ST    @10,EXSTARTP                                        0396
*   EXENDP=ADDR(XTLMSBLA);          /* ADDR OF SIZE OF BLOCK         */
*                                                                  0397
         LA    @10,XTLMSBLA(,XTLSTPTR)                             0397
         ST    @10,EXENDP                                          0397
*   /*****************************************************************/
*   /*                                                               */
*   /* IF ALL EXTENTS PROCESSED THEN NOT DESIRED CDE                 */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0398
*NEXTEXT:                           /* REENTER FOR EACH EXTENT       */
*   IF NOENTS=0                     /* LAST EXTENT PROCESSED         */
*     THEN                          /* YES                           */
NEXTEXT  LTR   NOENTS,NOENTS                                       0398
         BZ    @RT00398                                            0398
*     GOTO RC12;                    /* BACK TO CALLER                */
*   ELSE                                                           0400
*     ;                             /* CHECK CURRENT EXTENT          */
*   EXEND=EXSTART+EXSIZE;           /* CALC END ADDR OF EXTENT       */
*                                                                  0401
         L     @10,EXSTARTP                                        0401
*        SLR   @15,@15                                   @UZ60132@ 0401
*        ICM   @15,7,EXSTART(@10)                        @UZ60132@ 0401
*        L     @10,EXENDP                                @UZ60132@ 0401
*        SLR   @14,@14                                   @UZ60132@ 0401
*        ICM   @14,7,EXSIZE(@10)                         @UZ60132@ 0401
*        ALR   @14,@15                                   @UZ60132@ 0401
*        ST    @14,EXEND                                 @UZ60132@ 0401
         L     @10,0(,@10)                               @UZ60132@
         LA    @10,0(,@10)                               @UZ60132@
         L     @01,EXENDP                                @UZ60132@
         L     @15,0(,@01)                               @UZ60132@
         LA    @15,0(,@15)                               @UZ60132@
         ALR   @15,@10                                   @UZ60132@
         ST    @15,EXEND                                 @UZ60132@
         L     @14,SA15                                  @UZ60132@
         CR    @14,@10                                   @UZ60132@
         BL    @RF00402                                  @UZ60132@
         CR    @14,@15                                   @UZ60132@
         BL    FOUND                                     @UZ60132@
*   /****************************************************@UZ60132@****/
*   /*                                                   @UZ60132@   */
*   /* IF POSSIBLE ENTRY POINT IS ENTIRELY WITHIN EXTENT @UZ60132@   */
*   /* DESIRED CDE HAS BEEN FOUND                        @UZ60132@   */
*   /*                                                   @UZ60132@   */
*   /****************************************************@UZ60132@****/
*                                                        @UZ60132@ 0402
*   IF SA15>=EXSTART&               /* ENTRY ABOVE LOW EN@UZ60132@   */
*       SA15<EXEND                  /* AND BELOW HIGH END@UZ60132@   */
*     THEN                          /* YES               @UZ60132@   */
*        L     @10,SA15                                  @UZ60132@ 0402
*        CR    @10,@15                                   @UZ60132@ 0402
*        BL    @RF00402                                  @UZ60132@ 0402
*        CR    @10,@14                                   @UZ60132@ 0402
*        BL    @RT00402                                  @UZ60132@ 0402
*     GOTO FOUND;                   /* DESIRED CDE LOCATE@UZ60132@   */
*   ELSE                                                 @UZ60132@ 0404
*     ;                             /* CHECK FOR MORE EXTENTS        */
@RF00402 DS    0H                                                  0405
*   NOENTS=NOENTS-1;                /* DECR NUMBER OF EXTENTS        */
         BCTR  NOENTS,0                                            0405
*   EXSTARTP=EXSTARTP+4;            /* ADDR OF NXT EXT LIST ENTRY    */
         LA    @10,4                                               0406
         L     @15,EXSTARTP                                        0406
         ALR   @15,@10                                             0406
         ST    @15,EXSTARTP                                        0406
*   EXENDP=EXENDP+4;                /* ADDR NXT EXT LIST ENTRY       */
         AL    @10,EXENDP                                          0407
         ST    @10,EXENDP                                          0407
*   GOTO NEXTEXT;                   /* CHECK NXT EXT LIST ENTRY      */
         B     NEXTEXT                                             0408
*FOUND:                             /* PROCESS THE CORRECT CDE       */
*   MODNAME=CDNAME;                 /* SET MODULE NAME INTO MSG      */
FOUND    MVC   MODNAME(8,ABDARPTR),CDNAME(CDENTPTR)                0409
*   ABDUPRXT=ADDR(TESTEX8);         /* ESTAB UPR ROUTINE             */
         LA    @10,TESTEX8                                         0410
         ST    @10,ABDUPRXT(,ABDARPTR)                             0410
*   SA14=R14A;                      /* GET SUPPOSED RET ADDR FROM  0411
*                                      SAVE AREA                     */
*        SLR   @10,@10                                   @UZ60132@ 0411
*        ICM   @10,7,R14A(CURSA)                         @UZ60132@ 0411
         L     @10,R14(,CURSA)                           @UZ60132@ 0411
         LA    @10,0(,@10)                               @UZ60132@
         ST    @10,SA14                                            0411
*   ABDUPRXT=0;                     /* DISALLOW UPR INTERRUPTS       */
*                                                                  0412
         SLR   @15,@15                                             0412
         ST    @15,ABDUPRXT(,ABDARPTR)                             0412
*   /*****************************************************************/
*   /*                                                               */
*   /* CHECK IF SUPPOSED RETURN ADDR FROM SAVE AREAS R14 SLOT IS     */
*   /* ADDRESSABLE                                                   */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0413
*   IF SA14=0|                      /* POSSIBLE ADDR ZERO            */
*       SA14>CVTMZ00                /* OR ABOVE HIGHEST ADDR         */
*     THEN                          /* YES                           */
         CR    @10,@15                                             0413
         BE    @RT00413                                            0413
         L     @01,CVTPTR                                          0413
         C     @10,CVTMZ00-CVT(,@01)                               0413
         BNH   @RF00413                                            0413
@RT00413 DS    0H                                                  0414
*     DO;                           /* NOT A POSSIBLE ADDR           */
*       VIACALL=' ';                /* BLANK ENTERED BY PART OF    0415
*                                      MESSAGE                       */
         MVI   VIACALL+1(ABDARPTR),C' '                            0415
         MVC   VIACALL+2(6,ABDARPTR),VIACALL+1(ABDARPTR)           0415
         MVI   VIACALL(ABDARPTR),C' '                              0415
*       GOTO WRITE;                 /* OUTPUT HEADING                */
         B     WRITE                                               0416
*     END;                                                         0417
*   ELSE                                                           0418
*     ;                             /* BUILD REST OF HEADING         */
@RF00413 DS    0H                                                  0419
*   ABDUPRXT=ADDR(WRITE);           /* ESTABLISH UPR ROUTINE         */
*                                                                  0419
         LA    @10,WRITE                                           0419
         ST    @10,ABDUPRXT(,ABDARPTR)                             0419
*   /*****************************************************************/
*   /*                                                               */
*   /* IF THE RETURN INSTR IS AN SVC 3 THEN ENTRY WAS BY SYSTEM LINK */
*   /* FUNCTION. THE MESSAGE IS CHANGED TO WAS ENTERED VIA LINK AND A*/
*   /* BRANCH IS MADE TO WRITE THE HEADING                           */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0420
*   IF EXITI='0A03'X                /* EXIT INSTRUCTION SVC 3        */
*     THEN                          /* YES                           */
         L     @10,SA14                                            0420
         CLC   EXITI(2,@10),@CB02493                               0420
         BNE   @RF00420                                            0420
*     DO;                           /* ALTER HEADING FOR LINK        */
*       CALLFLD='LINK';             /* CHANGE CALL TO LINK           */
         MVC   CALLFLD(4,ABDARPTR),@CC02494                        0422
*       IF FRSTENT='1'B THEN                                       0423
         TM    FRSTENT,B'10000000'                                 0423
         BNO   @RF00423                                            0423
*         DO;                       /* IF FIRST ENTRY@ZA01900        */
*           FRSTENT='0'B;           /* NO LONGER FRSTENT     @ZA01900*/
         NI    FRSTENT,B'01111111'                                 0425
*           GOTO WRITE;             /* IGNORE FIRST SA       @ZA01900*/
         B     WRITE                                               0426
*         END;                      /* END FRSTENT PROC      @ZA01900*/
*       SANUM=SANUM+1;              /* UP SA COUNT           @ZA01900*/
@RF00423 LH    @10,SANUM                                           0428
         LA    @10,1(,@10)                                         0428
         STH   @10,SANUM                                           0428
*       CALL RBSEARCH;              /* FIND CORRECT RB       @ZA01900*/
         BAL   @14,RBSEARCH                                        0429
*       OPSWSAVE=RBOPSW(5:8);       /* GET RESUME PSW AD     @ZA01900*/
         MVC   OPSWSAVE(4),RBOPSW+4(RBSECPTR)                      0430
*       IF EXITB='4700'X THEN       /* CHECK FOR ID          @ZA01900*/
         L     @10,OPSWSAVE                                        0431
         CLC   EXITB(2,@10),@CH02496                               0431
         BNE   @RF00431                                            0431
*         DO;                       /* SETUP TO CONVERT      @ZA01900*/
*           REG15=OPSWSAVE;         /* SET ADDR IN R15       @ZA01900*/
         L     REG15,OPSWSAVE                                      0433
*           GOTO CNVTENT;           /* GO CONVERT ID         @ZA01900*/
         B     CNVTENT                                             0434
*         END;                      /* END LINKID PROC       @ZA01900*/
*     END;                                                         0436
*   ELSE                                                           0437
*     ;                             /* DO EP NONSENSE                */
*                                                                  0437
@RF00420 DS    0H                                                  0438
*   /*****************************************************************/
*   /*                                                               */
*   /* IF NO CALL ID THEN OUTPUT THE HEADING. OTHERWISE PUT CALL ID  */
*   /* IN HEADING                                                    */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0438
*   IF EXITI^='4700'X               /* CALL ID EXIST                 */
*     THEN                          /* NO                            */
@RC00420 L     @10,SA14                                            0438
         CLC   EXITI(2,@10),@CB02496                               0438
         BNE   @RT00438                                            0438
*     GOTO WRITE;                   /* OUTPUT HEADING                */
*   ELSE                                                           0440
*     ;                             /* PROCESS ID                    */
*                                                                  0440
*   /*****************************************************************/
*   /*                                                               */
*   /* CONVERT CALL ID TO DECIMAL                                    */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0441
*   GENERATE SETS(ABDWORK);                                        0441
         L     15,SA14        ADDR OF EXIT FROM MODULE
CNVTENT  LH    15,2(15)       ID BUILT BY CALL OR 'LINK' MACRO @ZA01900
         CVD   15,ABDWORK+8(ABDARPTR) CONVERT ID TO DECIMAL
* UNPACK CONVERTED ID INTO WORK AREA YIELDING 8 CHARACTER VALUE
         UNPK  ABDWORK+3(8,ABDARPTR),ABDWORK+12(4,ABDARPTR)
         OI    ABDWORK+10(ABDARPTR),X'F0'   FIX UP INVALID SIGN
* MOVE ID VALUE INTO OUTPUT LINE
         MVC   ABDLINE+31(5,ABDARPTR),ABDWORK+6(ABDARPTR)
*WRITE:                             /* OUTPUT THE HEADING            */
*   ABDUPRXT=0;                     /* DISALLOW UPR INTERRUPTS       */
WRITE    SLR   @10,@10                                             0442
         ST    @10,ABDUPRXT(,ABDARPTR)                             0442
*   ABDCC='0';                      /* DOUBLE SPACE                  */
         MVI   ABDCC(ABDARPTR),C'0'                                0443
*   PARMPTR=ABDARPTR;               /* SET ADDRESS OF PARAM LIST     */
         LR    PARMPTR,ABDARPTR                                    0444
*   CALL IEAVAD11;                  /* OUTPUT MSG LINE               */
         L     @15,@CV02411                                        0445
         BALR  @14,@15                                             0445
*   GOTO TESTEXIT;                  /* BACK TO CALLER                */
         B     TESTEXIT                                            0446
*TESTEX8:                           /* UPR OCCURED BACK I GO         */
*   RETCODE=8;                      /* INDICATE UPR TO CALLER        */
TESTEX8  LA    RETCODE,8                                           0447
*   GOTO TESTEXIT;                  /* RETURN TO CALLER              */
         B     TESTEXIT                                            0448
*   END TESTEP1;                    /* END OF SUBROUTINE             */
*                                                                  0450
*/* SUBROUTINE RBSEARCH ENTERED TO FIND THE ADDRESS OF A PRB @ZA01900*/
*/* WITH INTERRUPT CODE OF 0006 OR 000C SHOWING THAT THE PRB @ZA01900*/
*/* ISSUED EITHER A LINK OR A SYNCH SVC.                     @ZA01900*/
*/* THE ONE INPUT PARAMETER IS SANUM, WHICH TELLS US HOW OLD @ZA01900*/
*/* THE PRB WE ARE SEARCHING FOR IS.  E.G. IF SANUM IS ONE,  @ZA01900*/
*/* THEN WE ARE LOOKING FOR THE OLDEST PRB WITH INTERRUPT    @ZA01900*/
*/* CODE OF 6 OR C.  IF SANUM IS 2 THEN WE WANT THE SECOND-  @ZA01900*/
*/* OLDEST, ETC.                                             @ZA01900*/
*/* ALL REGISTERS EXCEPT 8 (RBSECPTR) ARE SAVED AND RESTORED @ZA01900*/
*/* THE OUTPUT IS THE RB PTR IN REG8 - RBSECPTR.             @ZA01900*/
*                                                                  0450
*RBSEARCH:                                                         0450
*   PROC OPTIONS(DONTSAVE(8));      /* SEARCH FOR LINKPRB    @ZA01900*/
RBSEARCH STM   @14,@07,12(@13)                                     0450
         STM   @09,@12,56(@13)                                     0450
*   RBSECPTR=TCBRBP;                /* GET TOP RB ADDRESS    @ZA01900*/
*                                                                  0451
         L     RBSECPTR,TCBRBP-TCB(,TCBPTR)                        0451
*   /*****************************************************************/
*   /*                                                               */
*   /* CHECK FOR PRB AND IC OF 6 FOR LINK OR C FOR SYNCH     @ZA01900*/
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0452
*   IF RBFTP='000'B&(RBINTCOD='0006'X/*                      @ZA01900*/
*        RBINTCOD='000C'X) THEN     /* IF ONE OF OURS THEN   @ZA01900*/
         TM    RBFTP(RBSECPTR),B'11100000'                         0452
         BNZ   @RF00452                                            0452
         LR    @10,RBSECPTR                                        0452
         SL    @10,@CF01199                              @UZ60132@ 0452
         CLC   RBINTCOD(2,@10),@CB02502                            0452
         BE    @RT00452                                            0452
         CLC   RBINTCOD(2,@10),@CB02503                            0452
         BNE   @RF00452                                            0452
@RT00452 DS    0H                                                  0453
*     LINKPRBS=1;                   /* SET COUNT TO ONE      @ZA01900*/
         MVC   LINKPRBS(4),@CF00065                                0453
*   ELSE                                                           0454
*     LINKPRBS=0;                   /* ELSE ZERO COUNT       @ZA01900*/
*                                                                  0454
         B     @RC00452                                            0454
@RF00452 SLR   @10,@10                                             0454
         ST    @10,LINKPRBS                                        0454
*   /*****************************************************************/
*   /*                                                               */
*   /* LOOP OVER RBS TO GET COUNT OF LINKS & SYNCHES         @ZA01900*/
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0455
*   DO WHILE(RBTCBNXT^='1'B);       /* LOOP OVER RB CHAIN    @ZA01900*/
@RC00452 B     @DE00455                                            0455
@DL00455 DS    0H                                                  0456
*     RBSECPTR=RBLINKB;             /* GET NEXT RB           @ZA01900*/
*        SLR   @10,@10                                   @UZ60132@ 0456
*        ICM   @10,7,RBLINKB(RBSECPTR)                   @UZ60132@ 0456
         L     @10,RBLINK(,RBSECPTR)                     @UZ60132@ 0456
         LA    @10,0(,@10)                               @UZ60132@
         LR    RBSECPTR,@10                                        0456
*     IF RBFTP='000'B&(RBINTCOD='0006'X/*                    @ZA01900*/
*         |RBINTCOD='000C'X) THEN   /* IF ONE OF OURS        @ZA01900*/
         TM    RBFTP(RBSECPTR),B'11100000'                         0457
         BNZ   @RF00457                                            0457
         LR    @10,RBSECPTR                                        0457
         SL    @10,@CF01199                              @UZ60132@ 0457
         CLC   RBINTCOD(2,@10),@CB02502                            0457
         BE    @RT00457                                            0457
         CLC   RBINTCOD(2,@10),@CB02503                            0457
         BNE   @RF00457                                            0457
@RT00457 DS    0H                                                  0458
*       LINKPRBS=LINKPRBS+1;        /* THEN UP THE COUNT     @ZA01900*/
         LA    @10,1                                               0458
         AL    @10,LINKPRBS                                        0458
         ST    @10,LINKPRBS                                        0458
*   END;                            /* END RB COUNT LOOP     @ZA01900*/
@RF00457 DS    0H                                                  0459
@DE00455 TM    RBTCBNXT(RBSECPTR),B'10000000'                      0459
         BNO   @DL00455                                            0459
*   RBSECPTR=TCBRBP;                /* START AT TOP AGAIN    @ZA01900*/
*                                                                  0460
         L     RBSECPTR,TCBRBP-TCB(,TCBPTR)                        0460
*   /*****************************************************************/
*   /*                                                               */
*   /* FIGURE INDEX INTO RB CHAIN FOR THE PRB WE ARE         @ZA01900*/
*   /* LOOKING FOR. INPUT IS SANUM WHICH TELLS US THAT WE    @ZA01900*/
*   /* WANT THE ITH OLDEST PRB WITH IC OF 6 OR C, AND        @ZA01900*/
*   /* LINKPRBS WHICH TELLS US HOW MANY SUCH PRBS ARE IN TH@@ZA01900 */
*   /* RB CHAIN. SO WE REVERSE INDEX WITH SANUM TO FIND      @ZA01900*/
*   /* THE INDEX INTO THE RB CHAIN.                          @ZA01900*/
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0461
*   J=LINKPRBS-(SANUM-1);           /* FIGURE INDEX          @ZA01900*/
*                                                                  0461
         LA    @10,1                                               0461
         SH    @10,SANUM                                           0461
         AL    @10,LINKPRBS                                        0461
         ST    @10,J                                               0461
*   /*****************************************************************/
*   /*                                                               */
*   /* NOW LOOP OVER RBS TO FIND THE JTH ONE WITH IC OF 6    @ZA01900*/
*   /* OR C WHICH IS A PRB.                                  @ZA01900*/
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0462
*   I=1;                            /* SET INDEX             @ZA01900*/
         MVC   I(4),@CF00065                                       0462
*   IF RBFTP='000'B&(RBINTCOD='0006'X/* PRB & LINK           @ZA01900*/
*       |RBINTCOD='000C'X) THEN     /* OR PRB & SYNCH THEN   @ZA01900*/
         TM    RBFTP(RBSECPTR),B'11100000'                         0463
         BNZ   @RF00463                                            0463
         LR    @10,RBSECPTR                                        0463
         SL    @10,@CF01199                              @UZ60132@ 0463
         CLC   RBINTCOD(2,@10),@CB02502                            0463
         BE    @RT00463                                            0463
         CLC   RBINTCOD(2,@10),@CB02503                            0463
         BNE   @RF00463                                            0463
@RT00463 DS    0H                                                  0464
*     IF I=J THEN                                                  0464
         L     @10,J                                               0464
         C     @10,I                                               0464
         BE    @RT00464                                            0464
*       GOTO LOOPOUT;               /* IF ONE WE WANT GETOUT @ZA01900*/
*     ELSE                                                         0466
*       J=J-1;                      /* ELSE DECRE. INDEX     @ZA01900*/
         BCTR  @10,0                                               0466
         ST    @10,J                                               0466
*   DO I=1 TO J WHILE(RBTCBNXT^='1'B);/*                     @ZA01900*/
@RF00463 LA    @10,1                                               0467
         B     @DE00467                                            0467
@DL00467 TM    RBTCBNXT(RBSECPTR),B'10000000'                      0467
         BO    @DC00467                                            0467
*     RBSECPTR=RBLINKB;             /* GET NEXT RB           @ZA01900*/
*        SLR   @10,@10                                   @UZ60132@ 0468
*        ICM   @10,7,RBLINKB(RBSECPTR)                   @UZ60132@ 0468
         L     @10,RBLINK(,RBSECPTR)                     @UZ60132@ 0468
         LA    @10,0(,@10)                               @UZ60132@
         LR    RBSECPTR,@10                                        0468
*     IF RBFTP^='000'B|(RBINTCOD^='0006'X/*                  @ZA01900*/
*         &RBINTCOD^='000C'X) THEN  /* IF NOT OURS THEN      @ZA01900*/
         TM    RBFTP(RBSECPTR),B'11100000'                         0469
         BNZ   @RT00469                                            0469
         LR    @10,RBSECPTR                                        0469
         SL    @10,@CF01199                              @UZ60132@ 0469
         CLC   RBINTCOD(2,@10),@CB02502                            0469
         BE    @RF00469                                            0469
         CLC   RBINTCOD(2,@10),@CB02503                            0469
         BE    @RF00469                                            0469
@RT00469 DS    0H                                                  0470
*       I=I-1;                      /* FORCE THROUGH LOOP    @ZA01900*/
         L     @10,I                                               0470
         BCTR  @10,0                                               0470
         ST    @10,I                                               0470
*     ELSE                                                         0471
*       IF I=J THEN                                                0471
         B     @RC00469                                            0471
@RF00469 CLC   I(4),J                                              0471
         BE    @RT00471                                            0471
*         GOTO LOOPOUT;             /* ELSE GET OUT          @ZA01900*/
*   END;                            /* END RBSEARCH LOOP     @ZA01900*/
@RC00469 LA    @10,1                                               0473
         AL    @10,I                                               0473
@DE00467 ST    @10,I                                               0473
         C     @10,J                                               0473
         BNH   @DL00467                                            0473
@DC00467 DS    0H                                                  0474
*LOOPOUT:                                                          0474
*   RETURN;                         /* RETURN TO CALLER      @ZA01900*/
@EL00004 DS    0H                                                  0474
@EF00004 DS    0H                                                  0474
@ER00004 LM    @14,@07,12(@13)                                     0474
         LM    @09,@12,56(@13)                                     0474
         BR    @14                                                 0474
*   END RBSEARCH;                   /* END SUBROUTINE        @ZA01900*/
         B     @EL00004                                            0475
*                                                                  0476
*   /*****************************************************************/
*   /*                                                               */
*   /* SUBROUTINE VALID VALIDITY CHECKS THE ADDRESS OF A POSSIBLE    */
*   /* SAVE AREA. AFTER DISABLING, IEA0VL00 IS CALLED TO CHECK THE   */
*   /* INITIAL AND LAST ADDRESSES OF THE SAVE AREA TO INSURE THAT THE*/
*   /* SPACE WOULD BE ADDRESSABLE BY THE USER. IF NOT, THE ROUTINE   */
*   /* RETURNS TO THE CALLER WITH A CODE OF EIGHT. THE ROUTINE THEN  */
*   /* CHECKS IF THE SAVE AREA FORWARD POINTER IS EQUAL TO THE SAVE  */
*   /* AREA BACK POINTER. IF SO, A CODE OF EIGHT IS RETURNED TO THE  */
*   /* CALLER. THEN, IF THE CALLER REQUESTED BACK CHAINING BE        */
*   /* CHECKED, THE ROUTINE CHECKS IF THE CURRENT SAVE AREA'S BACK   */
*   /* POINTER IS TO THE SAVE AREA POINTED TO BY LASTSA. IF NOT A    */
*   /* RETURN CODE OF FOUR IS PASSED TO THE CALLER. OTHERWISE A CODE */
*   /* OF ZERO IS GIVEN THE CALLER                                   */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0476
*VALID:                             /* START OF SUBROUTINE           */
*   PROCEDURE OPTIONS(NOSAVEAREA,   /* PROVIDE NO SAVE AREA          */
*       DONTSAVE);                  /* SAVE NO REGISTERS             */
VALID    DS    0H                                                  0477
*   VALIDR14=RETREG;                /* SAVE RETURN ADDRESS           */
         ST    RETREG,VALIDR14                                     0477
*   GENERATE REFS(PSALITA,ABDWORK);                                0478
*                                        /*                   @Y02705*/
         STM   11,14,ABDWORK(ABDARPTR)                          @Y02705
         SETLOCK OBTAIN,TYPE=LOCAL,MODE=UNCOND,                 @Y02705C
               RELATED=(SAVEAREA,IEAVAD07(INVALID))             @Y02705
         LM    11,14,ABDWORK(ABDARPTR)                          @Y02705
*   RFY                                                            0479
*    (TCBADDR,                                                     0479
*     INADDR,                                                      0479
*     SVWORK) RSTD;                 /*                       @YM05065*/
*   INADDR=CURSA;                   /* SET INPUT ADDR FOR 0VL00      */
         LR    INADDR,CURSA                                        0480
*   SVWORK=ABDARPTR;                /* SAVE ADDR OF WKAREA    @Y02705*/
         LR    SVWORK,ABDARPTR                                     0481
*   LSTADDR=0;                      /* LAST ADDR 0            @Y02705*/
         SLR   LSTADDR,LSTADDR                                     0482
*   TCBADDR=TCBPTR;                 /* SAVE TCB ADDR          @Y02705*/
         LR    TCBADDR,TCBPTR                                      0483
*   TCBPTR=0;                       /* LET RTN FIND TCB       @Y02705*/
         SLR   TCBPTR,TCBPTR                                       0484
*   VL00ADD=CVT0VL00;               /* SET ADDR OF ROUTINE           */
*                                                                  0485
         L     @14,CVTPTR                                          0485
         L     VL00ADD,CVT0VL00-CVT(,@14)                          0485
*   /*****************************************************************/
*   /*                                                               */
*   /* GENERATE CALL TO IEA0VL00                                     */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0486
*   GENERATE FLOWS(RET8);                                          0486
*                                      /*                     @Y02705*/
         BALR  14,15            BRNCH TO IEA0VL00               @Y02705
         BNZ   RET8             INVALID ADDRESS
*   INADDR=CURSA+71;                /* CALC ADDR LAST BYTE OF SAVE 0487
*                                      AREA                          */
         LA    INADDR,71                                           0487
         ALR   INADDR,CURSA                                        0487
*   TCBPTR=0;                       /* RESET TCB ADDR         @Y02705*/
         SLR   TCBPTR,TCBPTR                                       0488
*   VL00ADD=CVT0VL00;               /* SET ADDR OF ROUTINE           */
*                                                                  0489
         L     @14,CVTPTR                                          0489
         L     VL00ADD,CVT0VL00-CVT(,@14)                          0489
*   /*****************************************************************/
*   /*                                                               */
*   /* GENERATE CALL TO IEA0VL00                                     */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0490
*NVALID:                                                           0490
*   GENERATE FLOWS(RET8);                                          0490
NVALID   DS    0H                                                  0490
*                                      /*                     @Y02705*/
         BALR  14,15            BRNCH TO IEA0VL00               @Y02705
         BNZ   RET8             INVALID ADDRESS
*   ABDARPTR=SVWORK;                /* RESTORE WKAREA ADDR    @Y02705*/
         LR    ABDARPTR,SVWORK                                     0491
*   TCBPTR=TCBADDR;                 /* RESTORE TCB ADDR       @Y02705*/
         LR    TCBPTR,TCBADDR                                      0492
*   GENERATE;                                                      0493
*                                                                  0493
*                                      /*                     @Y02705*/
         STM   11,14,ABDWORK(ABDARPTR)                          @Y02705
         SETLOCK RELEASE,TYPE=LOCAL,RELATED=(SAVEAREA,IEAVAD07(VALID))
         LM    11,14,ABDWORK(ABDARPTR)                          @Y02705
*   /*****************************************************************/
*   /*                                                               */
*   /* CHECK IF FORWARD AND BACK SAVE AREA PTRS ARE EQUAL            */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0494
*   ABDUPRXT=ADDR(RET8A);           /* IF UPR THEN NOT ADDRSSABLE    */
         LA    @00,RET8A                                           0494
         ST    @00,ABDUPRXT(,ABDARPTR)                             0494
*   IF PREVSAVE^=NEXTSAVE           /* FORWARD AND BACK PTRS SAME    */
*     THEN                          /* NO                            */
         CLC   PREVSAVE(3,CURSA),NEXTSAVE(CURSA)                   0495
         BNE   @RT00495                                            0495
*     GOTO BACKCK;                  /* TEST OK CONTINUE CHECKS       */
*   ELSE                                                           0497
*     ;                             /* FAILED CHECK RC OF 8          */
*RET8A:                             /* CHECK FAIL WITH CODE OF 8     */
*   ABDARPTR=SVWORK;                /* RESTORE WKAREA ADDR    @Y02705*/
RET8A    LR    ABDARPTR,SVWORK                                     0498
*   TCBPTR=TCBADDR;                 /* RESTORE TCB ADDR       @Y02705*/
         LR    TCBPTR,TCBADDR                                      0499
*   RETCODE=8;                      /* SET RETURN CODE TO EIGHT      */
         LA    RETCODE,8                                           0500
*   RETREG=VALIDR14;                /* RESTORE RETURN ADDR           */
         L     RETREG,VALIDR14                                     0501
*   RETURN;                         /* BACK TO CALLER                */
@EL00005 DS    0H                                                  0502
@EF00005 DS    0H                                                  0502
@ER00005 BR    @14                                                 0502
*RET8:                              /*                        @Y02705*/
*   ABDARPTR=SVWORK;                /* RESTORE WKAREA ADDR    @Y02705*/
RET8     LR    ABDARPTR,SVWORK                                     0503
*   TCBPTR=TCBADDR;                 /* RESTORE TCB ADDR       @Y02705*/
         LR    TCBPTR,TCBADDR                                      0504
*   GENERATE REFS(PSALITA,ABDWORK);                                0505
*                                        /*                   @Y02705*/
         STM   11,14,ABDWORK(ABDARPTR)                          @Y02705
         SETLOCK RELEASE,TYPE=LOCAL,RELATED=(SAVEAREA,IEAVAD07(VALID))
         LM    11,14,ABDWORK(ABDARPTR)                          @Y02705
*   GOTO RET8A;                     /* EXIT TO CALLER CODE OF 8      */
*                                                                  0506
         B     RET8A                                               0506
*   /*****************************************************************/
*   /*                                                               */
*   /* IF USER DID NOT REQUEST BACK CHAINING TEST RETURN WITH CODE OF*/
*   /* ZERO                                                          */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0507
*BACKCK:                            /* MAKE LAST CHECK               */
*   ABDUPRXT=0;                     /* DISALLOW UPR                  */
BACKCK   SLR   @15,@15                                             0507
         ST    @15,ABDUPRXT(,ABDARPTR)                             0507
*   IF LASTSA=0                     /* NO PREVIOUS SA AREA GIVEN     */
*     THEN                          /* YES                           */
         L     @14,LASTSA                                          0508
         CR    @14,@15                                             0508
         BE    @RT00508                                            0508
*     GOTO NORM;                    /* START NORMAL EXIT             */
*   ELSE                                                           0510
*     ;                             /* MAKE BACK CHAIN CHECK         */
*                                                                  0510
*   /*****************************************************************/
*   /*                                                               */
*   /* MAKE CHECK TO INSURE THAT CURRENT SAVE AREA POINTS BACK TO    */
*   /* PREVIOUS SAVE AREA                                            */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0511
*   ABDUPRXT=ADDR(VALRC4);          /* SET RC TO 4 IF UPR HAPPENS    */
         LA    @00,VALRC4                                          0511
         ST    @00,ABDUPRXT(,ABDARPTR)                             0511
*   IF PREVSAVE=LASTSA              /* CURRENT POINT TO LAST         */
*     THEN                          /* YES                           */
*        SLR   @00,@00                                   @UZ60132@ 0512
         L     @00,4(,@03)                               @UZ60132@
         ICM   @00,8,@CB02502                            @UZ60132@ 0512
         CR    @14,@00                                   @UZ60132@ 0512
         BE    NORM                                                0512
*     GOTO NORM;                    /* START NORMAL EXIT             */
*   ELSE                                                           0514
*     ;                             /* RC 4                          */
*VALRC4:                            /* ENTERED IF UPR HAPPENS        */
*   RETCODE=4;                      /* OTHERWISE GIVE CODE           */
VALRC4   LA    RETCODE,4                                           0515
*   RETREG=VALIDR14;                /* RESTORE RETURN ADDR           */
         L     RETREG,VALIDR14                                     0516
*   RETURN;                         /* EXIT TO CALLER                */
         B     @EL00005                                            0517
*NORM:                              /* NORMAL EXIT                   */
*   ABDUPRXT=0;                     /* DISALLOW UPR                  */
NORM     SLR   @00,@00                                             0518
         ST    @00,ABDUPRXT(,ABDARPTR)                             0518
*   RETCODE=0;                      /* VALIDITY CHECKS ALL OK        */
         SLR   RETCODE,RETCODE                                     0519
*   RETREG=VALIDR14;                /* RESTORE RETURN ADDR           */
         L     RETREG,VALIDR14                                     0520
*   RFY                                                            0521
*    (TCBADDR,                                                     0521
*     INADDR,                                                      0521
*     SVWORK) UNRSTD;               /*                       @YM05065*/
*   END VALID;                      /* END OF SUBROUTINE             */
         B     @EL00005                                            0522
*   DCL                                                            0523
*     PATCHLOC FIXED(31) STATIC;                                   0523
*   GEN DATA DEFS(PATCHLOC);                                       0524
*   DECLARE                         /* COMMON VARIABLES              */
*     I256C CHAR(256) BASED,                                       0525
*     I031F FIXED(31) BASED,                                       0525
*     I031P PTR(31) BASED,                                         0525
*     I015F FIXED(15) BASED,                                       0525
*     I015P PTR(15) BASED,                                         0525
*     I008P PTR(8) BASED,                                          0525
*     I001C CHAR(1) BASED;          /*                        @Y02705*/
*   END IEAVAD07                    /* END OF MODULE                 */
*                                                                  0526
*/* THE FOLLOWING INCLUDE STATEMENTS WERE FOUND IN THIS PROGRAM.     */
*/*%INCLUDE SYSLIB  (IHAABDA )                                       */
*/*%INCLUDE SYSLIB  (CVT     )                                       */
*/*%INCLUDE SYSLIB  (IKJTCB  )                                       */
*/*%INCLUDE SYSLIB  (IHARB   )                                       */
*/*%INCLUDE SYSLIB  (IKJRB   )                                       */
*/*%INCLUDE SYSLIB  (IHACDE  )                                       */
*/*%INCLUDE SYSLIB  (IHALLE  )                                       */
*/*%INCLUDE SYSLIB  (IHAXTLST)                                       */
*/*%INCLUDE SYSLIB  (IHAPSA  )                                       */
*/*%INCLUDE SYSLIB  (IHARTCT )                                       */
*                                                                  0526
*       ;                                                          0526
         B     @EL00001                                            0526
@DATA    DS    0H
@CH00114 DC    H'2'
@CH02496 DC    XL2'4700'
MOVEID   MVC   DESC(1,ABDARPTR),5(1)   MOVE ID           @UZ60132@
@CF00065 DC    F'1'
@CH00070 DC    F'4'                                      @UZ60132@
@CH00123 DC    F'5'                                      @UZ60132@
@CH00072 DC    F'8'                                      @UZ60132@
@CF01199 DC    F'64'                                     @UZ60132@
@DATD    DSECT
         DS    0D
OPSWSAVE DS    A
LINKPRBS DS    A
I        DS    A
J        DS    A
LASTSA   DS    A
SAVERB   DS    A
SA15     DS    A
SA1RET   DS    A
VALIDR14 DS    A
TESTR14  DS    A
EXENDP   DS    A
EXSTARTP DS    A
EXEND    DS    A
SA14     DS    A
SANUM    DS    H
COUNT    DS    H
LNG      DS    AL1
LINCNT   DS    AL1
FRSTENT  DS    BL1
EPINMSG  DS    BL1
IEAVAD07 CSECT
         DS    0F
@SIZDATD DC    AL1(0)
         DC    AL3(@ENDDATD-@DATD)
@CV02411 DC    V(IEAVAD11)
@CV02466 DC    V(IEAVAD31)
@CV02432 DC    V(IEAVAD41)
         DS    0D
@CC02456 DC    C'UNKNOWN '
@CC02443 DC    C'AT EP'
@CC02494 DC    C'LINK'
BRCON    DC    X'47F0'                 BR AROUND EYECATCHER    ZP60028
BRREL    DC    X'A7F4'                 BRREL AROUND EYECATCHER ZP60028
@CB02493 DC    X'0A03'
@CB02496 DC    X'4700'
@CB02502 DC    X'0006'
@CB02503 DC    X'000C'
K253     DC    X'FD00'                 SUBPOOL NUMBER    @UZ60132@
SPKEY8   DC    X'80'                                     @UZ60132@
SAVEMSG1 DC    CL16'-SAVE AREA TRACE'
IBCMSG   DC    CL19'-INVALID BACK CHAIN'
PBMSG    DC    CL27'-PROCEEDING BACK VIA REG 13'
LLINE1   DS    CL15
         ORG   LLINE1
@NM00006 DC    X'B201'
@NM00007 DC    CL12'INTERRUPT AT'
@NM00008 DC    X'FF'
         ORG   LLINE1+15
SAHDLINE DS    CL5
         ORG   SAHDLINE
@NM00009 DC    X'1203'
@NM00010 DC    CL2'SA'
@NM00011 DC    X'FF'
         ORG   SAHDLINE+5
SALLINE1 DS    CL31
         ORG   SALLINE1
@NM00012 DC    X'0D'
@NM00013 DC    X'2301'
@NM00014 DC    CL3'WD1'
@NM00015 DC    X'2341'
@NM00016 DC    CL3'HSA'
@NM00017 DC    X'2341'
@NM00018 DC    CL3'LSA'
@NM00019 DC    X'2341'
@NM00020 DC    CL3'RET'
@NM00021 DC    X'2341'
@NM00022 DC    CL3'EPA'
@NM00023 DC    X'1342'
@NM00024 DC    CL2'R0'
@NM00025 DC    X'FF'
         ORG   SALLINE1+31
SALLINE2 DS    CL26
         ORG   SALLINE2
@NM00026 DC    X'0D'
@NM00027 DC    X'1302'
@NM00028 DC    CL2'R1'
@NM00029 DC    X'1342'
@NM00030 DC    CL2'R2'
@NM00031 DC    X'1342'
@NM00032 DC    CL2'R3'
@NM00033 DC    X'1342'
@NM00034 DC    CL2'R4'
@NM00035 DC    X'1342'
@NM00036 DC    CL2'R5'
@NM00037 DC    X'1342'
@NM00038 DC    CL2'R6'
@NM00039 DC    X'FF'
         ORG   SALLINE2+26
SALLINE3 DS    CL29
         ORG   SALLINE3
@NM00040 DC    X'0D'
@NM00041 DC    X'1302'
@NM00042 DC    CL2'R7'
@NM00043 DC    X'1342'
@NM00044 DC    CL2'R8'
@NM00045 DC    X'1342'
@NM00046 DC    CL2'R9'
@NM00047 DC    X'2341'
@NM00048 DC    CL3'R10'
@NM00049 DC    X'2341'
@NM00050 DC    CL3'R11'
@NM00051 DC    X'2341'
@NM00052 DC    CL3'R12'
@NM00053 DC    X'FF'
         ORG   SALLINE3+29
STANDMSG DC    CL20'WAS ENTERED VIA CALL'
IEAVAD07 CSECT
         DS   0H
PATCHLOC DC ((@DATA-@PSTART)/20)X'00'
         DC    0D'0'
         DC    CL8'ZP60028'
@DATD    DSECT
         ORG   *+1-(*-@DATD)/(*-@DATD) INSURE DSECT DATA
@ENDDATD EQU   *
IEAVAD07 CSECT
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
INADDR   EQU   @01
ABDARPTR EQU   @02
LSTADDR  EQU   @02
CURSA    EQU   @03
TCBPTR   EQU   @04
NOENTSP  EQU   @05
NOENTS   EQU   @05
CDENTPTR EQU   @06
SAVERET  EQU   @06
LLEPTR   EQU   @07
RBSECPTR EQU   @08
XTLSTPTR EQU   @09
TCBADDR  EQU   @09
SVWORK   EQU   @10
AUTOPTR  EQU   @12
SAVEREG  EQU   @13
RETREG   EQU   @14
RETCODE  EQU   @15
REG15    EQU   @15
VL00ADD  EQU   @15
EXITB    EQU   0
SAVEAREA EQU   0
PREVSAVE EQU   SAVEAREA+5
NEXTSAVE EQU   SAVEAREA+9
R14      EQU   SAVEAREA+12
R14A     EQU   R14+1
R15      EQU   SAVEAREA+16
R15A     EQU   R15+1
PROGRAM  EQU   0
EPI      EQU   PROGRAM
CNT      EQU   PROGRAM+4
EXSTARTA EQU   0
EXSTART  EQU   EXSTARTA+1
EXENDA   EQU   0
EXSIZE   EQU   EXENDA+1
PGM      EQU   0
EXITI    EQU   PGM
ABDAREA  EQU   0
ABDTCB   EQU   ABDAREA
ABDCTCB  EQU   ABDAREA+4
ABDPARMS EQU   ABDAREA+12
ABDFLAG  EQU   ABDPARMS+1
APFSDATA EQU   ABDPARMS+4
APFSDAT1 EQU   ABDPARMS+5
APFPDATA EQU   ABDPARMS+6
APFSAVE2 EQU   APFPDATA
ABDPTRS  EQU   ABDAREA+84
ABDUPRXT EQU   ABDAREA+112
ABDFLAG1 EQU   ABDAREA+136
ABDFLAG2 EQU   ABDAREA+137
ABDRES1  EQU   ABDAREA+139
ABDWORK  EQU   ABDAREA+140
ABDBPTR  EQU   ABDAREA+244
ABDLLINE EQU   ABDAREA+248
ABDLPTR  EQU   ABDAREA+252
ABDUPRF  EQU   ABDAREA+265
UPRFMAT  EQU   ABDUPRF
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
ABDLINE  EQU   ABDLINEA+1
RBPRFX   EQU   0
RBBASIC  EQU   0
RBEXRTNM EQU   RBBASIC
RBTMFLD  EQU   RBEXRTNM
RBSTAB   EQU   RBBASIC+10
XSTAB    EQU   RBSTAB
RBSTAB1  EQU   XSTAB
XSTAB1   EQU   RBSTAB1
RBFTCKPT EQU   XSTAB1
RBSTAB2  EQU   XSTAB+1
XSTAB2   EQU   RBSTAB2
RBTCBNXT EQU   XSTAB2
RBFACTV  EQU   XSTAB2
RBFDYN   EQU   XSTAB2
RBECBWT  EQU   XSTAB2
RBOPSW   EQU   RBBASIC+16
XRBPSW   EQU   RBOPSW
RBOPSWB2 EQU   XRBPSW+1
RBLINK   EQU   RBBASIC+28
XRBLNK   EQU   RBLINK
RBWCF    EQU   XRBLNK
RBLINKB  EQU   XRBLNK+1
RBGRSAVE EQU   RBBASIC+32
XRBREG   EQU   RBGRSAVE
RBGRS0   EQU   XRBREG
RBGRS1   EQU   XRBREG+4
RBGRS2   EQU   XRBREG+8
RBGRS3   EQU   XRBREG+12
RBGRS4   EQU   XRBREG+16
RBGRS5   EQU   XRBREG+20
RBGRS6   EQU   XRBREG+24
RBGRS7   EQU   XRBREG+28
RBGRS8   EQU   XRBREG+32
RBGRS9   EQU   XRBREG+36
RBGRS10  EQU   XRBREG+40
RBGRS11  EQU   XRBREG+44
RBGRS12  EQU   XRBREG+48
RBGRS13  EQU   XRBREG+52
RBGRS14  EQU   XRBREG+56
RBGRS15  EQU   XRBREG+60
XRBESA   EQU   RBBASIC+96
RBPREFIX EQU   0
RBRSV019 EQU   RBPREFIX+14
RBRSV028 EQU   RBPREFIX+15
RBRSV041 EQU   RBPREFIX+27
RBRSV054 EQU   RBPREFIX+39
RBRTOPSW EQU   RBPREFIX+40
RBRTPSW2 EQU   RBRTOPSW+8
RBRTICIL EQU   RBRTPSW2
RBFLAGS1 EQU   RBPREFIX+56
RBINTCDA EQU   RBPREFIX+61
RBINTCOD EQU   RBINTCDA+1
RBSECT   EQU   0
RBPPSAV  EQU   RBSECT
RBPPSAV1 EQU   RBPPSAV+1
@NM00107 EQU   RBSECT+10
@NM00108 EQU   @NM00107
RBFTP    EQU   @NM00108
RBTRSVRB EQU   @NM00108
@NM00110 EQU   @NM00107+1
RBETXR   EQU   @NM00110
RBEP     EQU   RBSECT+12
RBPGMQ   EQU   RBSECT+24
@NM00117 EQU   RBSECT+28
@NM00118 EQU   @NM00117
IRBEND   EQU   RBSECT+96
@NM00121 EQU   RBSECT+96
RBRSV138 EQU   @NM00121+7
RBSCBB   EQU   RBSECT+144
RBSPARM  EQU   RBSCBB+8
RBSFLGS1 EQU   RBSPARM
RBSIOPRC EQU   RBSFLGS1
RBSOWNR  EQU   RBSCBB+12
RBSFLGS2 EQU   RBSOWNR
RBSDATA  EQU   RBSCBB+16
RBSFLG3  EQU   RBSDATA
RBRSV150 EQU   RBSECT+167
CDENTRY  EQU   0
CDNAME   EQU   CDENTRY+8
CDXLMJP  EQU   CDENTRY+20
CDATTR   EQU   CDENTRY+28
CDNIC    EQU   CDATTR
CDMIN    EQU   CDATTR
CDATTR2  EQU   CDENTRY+29
CDXLE    EQU   CDATTR2
LLE      EQU   0
LLECHN   EQU   LLE
LLECDPT  EQU   LLE+4
XTLST    EQU   0
XTLNRFAC EQU   XTLST+4
XTLMSBLA EQU   XTLST+8
XTLMSBAA EQU   XTLST+12
RTCT     EQU   0
RTCTPLIB EQU   RTCT+4
RTCTSAP  EQU   RTCTPLIB
RTCTSAP1 EQU   RTCTSAP
RTCTSAP2 EQU   RTCTSAP+1
RTCTSAP3 EQU   RTCTSAP+2
RTCTSUP  EQU   RTCTPLIB+4
RTCTSUP1 EQU   RTCTSUP
RTCTSUP2 EQU   RTCTSUP+1
RTCTSUP3 EQU   RTCTSUP+2
RTCTSYD  EQU   RTCTPLIB+8
RTCTSY01 EQU   RTCTSYD
RTCTSDDS EQU   RTCT+36
RTCTFLG  EQU   RTCTSDDS+3
RTCTSDPL EQU   RTCT+156
RTCTRFLG EQU   RTCT+176
RTCTERID EQU   RTCT+224
RTCTOPT  EQU   RTCT+236
RTCTASO  EQU   RTCT+252
RTCTASO1 EQU   RTCTASO
RTCTASO2 EQU   RTCTASO+1
RTCTSDI  EQU   RTCT+254
RTCTSDF  EQU   RTCT+264
RTCTSDF1 EQU   RTCTSDF
RTCTSDF2 EQU   RTCTSDF+1
RTCTSDF3 EQU   RTCT+268
RTCTSDF4 EQU   RTCTSDF3+2
RTCTMOPT EQU   0
RTCTMSAO EQU   RTCTMOPT
RTCTSASD EQU   RTCTMSAO
RTCTSAO1 EQU   RTCTSASD
RTCTSAO2 EQU   RTCTSASD+1
RTCTSAPD EQU   RTCTMSAO+2
RTCTSAO3 EQU   RTCTSAPD
RTCTSAO4 EQU   RTCTSAPD+1
RTCTSAMG EQU   RTCTSAO4
RTCTMSUO EQU   RTCTMOPT+4
RTCTSUSD EQU   RTCTMSUO
RTCTSUO1 EQU   RTCTSUSD
RTCTSUO2 EQU   RTCTSUSD+1
RTCTSUPD EQU   RTCTMSUO+2
RTCTSUO3 EQU   RTCTSUPD
RTCTSUO4 EQU   RTCTSUPD+1
RTCTSUMG EQU   RTCTSUO4
RTCTMSYO EQU   RTCTMOPT+8
RTCTSD01 EQU   RTCTMSYO
RTCTSD04 EQU   RTCTMSYO+3
RTCTSMMG EQU   RTCTSD04
RTCTMSDO EQU   RTCTMOPT+12
RTCTSDOD EQU   RTCTMSDO
RTCTSDO1 EQU   RTCTSDOD
RTCTSDO2 EQU   RTCTSDOD+1
RTCTSDO3 EQU   RTCTMSDO+2
RTCTSDO4 EQU   RTCTMSDO+3
RTCTSDMG EQU   RTCTSDO4
I001C    EQU   0
I008P    EQU   0
I015F    EQU   0
I015P    EQU   0
I031F    EQU   0
I031P    EQU   0
I256C    EQU   0
HEADING  EQU   ABDLINE
MODNAME  EQU   HEADING
HEAD     EQU   HEADING+9
VIACALL  EQU   HEAD+12
CALLFLD  EQU   VIACALL+4
ATEPPART EQU   HEADING+38
DESC     EQU   HEADING+44
WORK     EQU   ABDGMA
IBC      EQU   ABDLINEA
PB       EQU   ABDLINEA
SAVE     EQU   ABDLINEA
RBSECS03 EQU   RBEP
RBCDE    EQU   RBSECS03
RBCDFLGS EQU   RBCDE
RBCDE1   EQU   RBCDE+1
RBSECS05 EQU   RBPGMQ
RBSQE    EQU   RBSECS05
RBSECS06 EQU   RBPGMQ
RBIQE    EQU   RBSECS06
RBSECS07 EQU   RBPGMQ
RBIQE2   EQU   RBSECS07
RBSECS08 EQU   IRBEND
RBSECS09 EQU   IRBEND
         AGO   .@UNREFD                START UNREFERENCED COMPONENTS
RBRSV161 EQU   RBSECS09+64
RBSIRBWA EQU   RBSECS09
RBIQEWRK EQU   RBSECS08+4
RBNEXAV  EQU   RBSECS08
RBIQEA   EQU   RBIQE2
RBIQE1   EQU   RBIQE+1
RBUSE    EQU   RBIQE
RBSQEA   EQU   RBSQE+1
@NM00122 EQU   RBSQE
RBCDLOAD EQU   RBCDFLGS
RBCDXCTL EQU   RBCDFLGS
RBCDSYNC EQU   RBCDFLGS
RBCDNODE EQU   RBCDFLGS
RBRSV010 EQU   RBCDFLGS
RBCDATCH EQU   RBCDFLGS
RBRSV009 EQU   RBCDFLGS
RBNOCELL EQU   RBCDFLGS
@NM00089 EQU   HEADING+114
@NM00088 EQU   HEADING+43
@NM00087 EQU   HEADING+29
@NM00086 EQU   VIACALL
@NM00085 EQU   HEAD
@NM00084 EQU   HEADING+8
RTCTISVC EQU   RTCTSDO4
RTCTSDOV EQU   RTCTSDMG
@NM00164 EQU   RTCTSDO4
@NM00163 EQU   RTCTSDO3
RTCTSDPH EQU   RTCTSDO3
RTCTSDPG EQU   RTCTSDO3
@NM00162 EQU   RTCTSDO2
RTCTSDPD EQU   RTCTSDO2
RTCTSDPC EQU   RTCTSDO2
RTCTSDPB EQU   RTCTSDO2
RTCTSDPA EQU   RTCTSDO2
RTCTSDP9 EQU   RTCTSDO2
RTCTSDP8 EQU   RTCTSDO2
RTCTSDP7 EQU   RTCTSDO1
RTCTSDP6 EQU   RTCTSDO1
RTCTSDP5 EQU   RTCTSDO1
RTCTSDP4 EQU   RTCTSDO1
RTCTSDP3 EQU   RTCTSDO1
RTCTSDP2 EQU   RTCTSDO1
RTCTSDP1 EQU   RTCTSDO1
RTCTSDP0 EQU   RTCTSDO1
RTCTISYM EQU   RTCTSD04
RTCTSMOV EQU   RTCTSMMG
@NM00161 EQU   RTCTSD04
RTCTSD03 EQU   RTCTMSYO+2
RTCTSD02 EQU   RTCTMSYO+1
RTCTSDS7 EQU   RTCTSD01
RTCTSDS6 EQU   RTCTSD01
RTCTSDS5 EQU   RTCTSD01
RTCTSDS4 EQU   RTCTSD01
RTCTSDS3 EQU   RTCTSD01
RTCTSDS2 EQU   RTCTSD01
RTCTSDS1 EQU   RTCTSD01
RTCTSDS0 EQU   RTCTSD01
RTCTISYU EQU   RTCTSUO4
RTCTSUOV EQU   RTCTSUMG
@NM00160 EQU   RTCTSUO4
RTCTSYDF EQU   RTCTSUO3
RTCTSYDE EQU   RTCTSUO3
RTCTSYDD EQU   RTCTSUO3
RTCTSYDC EQU   RTCTSUO3
RTCTSYDB EQU   RTCTSUO3
RTCTSYDA EQU   RTCTSUO3
RTCTSYD9 EQU   RTCTSUO3
RTCTSYD8 EQU   RTCTSUO3
@NM00159 EQU   RTCTSUO2
RTCTSYDH EQU   RTCTSUO2
RTCTSYDG EQU   RTCTSUO2
RTCTSYD7 EQU   RTCTSUO1
RTCTSYD6 EQU   RTCTSUO1
RTCTSYD5 EQU   RTCTSUO1
RTCTSYD4 EQU   RTCTSUO1
RTCTSYD3 EQU   RTCTSUO1
RTCTSYD2 EQU   RTCTSUO1
RTCTSYD1 EQU   RTCTSUO1
RTCTSYD0 EQU   RTCTSUO1
RTCTISAB EQU   RTCTSAO4
RTCTSAOV EQU   RTCTSAMG
@NM00158 EQU   RTCTSAO4
RTCTSADF EQU   RTCTSAO3
RTCTSADE EQU   RTCTSAO3
RTCTSADD EQU   RTCTSAO3
RTCTSADC EQU   RTCTSAO3
RTCTSADB EQU   RTCTSAO3
RTCTSADA EQU   RTCTSAO3
RTCTSAD9 EQU   RTCTSAO3
RTCTSAD8 EQU   RTCTSAO3
@NM00157 EQU   RTCTSAO2
RTCTSADH EQU   RTCTSAO2
RTCTSADG EQU   RTCTSAO2
RTCTSAD7 EQU   RTCTSAO1
RTCTSAD6 EQU   RTCTSAO1
RTCTSAD5 EQU   RTCTSAO1
RTCTSAD4 EQU   RTCTSAO1
RTCTSAD3 EQU   RTCTSAO1
RTCTSAD2 EQU   RTCTSAO1
RTCTSAD1 EQU   RTCTSAO1
RTCTSAD0 EQU   RTCTSAO1
RTCTZZZ5 EQU   RTCT+340
RTCTZZZ4 EQU   RTCT+336
RTCTMRMQ EQU   RTCT+332
RTCTSDF5 EQU   RTCTSDF3+3
RTCTSDDO EQU   RTCTSDF4
RTCTSDEN EQU   RTCTSDF4
RTCTSDEQ EQU   RTCTSDF4
RTCTSDTR EQU   RTCTSDF4
RTCTSDRM EQU   RTCTSDF4
RTCTSDAN EQU   RTCTSDF4
RTCTSDNC EQU   RTCTSDF4
RTCTSDSS EQU   RTCTSDF4
RTCTSDAS EQU   RTCTSDF3
RTCTZZZ3 EQU   RTCT+266
@NM00156 EQU   RTCTSDF2
RTCTSDRW EQU   RTCTSDF2
RTCTSDSL EQU   RTCTSDF2
RTCTSDWF EQU   RTCTSDF2
RTCTSDLA EQU   RTCTSDF2
RTCTSDDI EQU   RTCTSDF2
RTCTSDTQ EQU   RTCTSDF2
RTCTSDMR EQU   RTCTSDF2
RTCTSDSC EQU   RTCTSDF1
RTCTSDRS EQU   RTCTSDF1
RTCTSDSD EQU   RTCTSDF1
RTCTSDEP EQU   RTCTSDF1
RTCTSDMA EQU   RTCTSDF1
RTCTSDSH EQU   RTCTSDF1
RTCTSDND EQU   RTCTSDF1
RTCTSDNO EQU   RTCTSDF1
RTCTZZZ2 EQU   RTCT+257
RTCTSDPR EQU   RTCTSDI+2
RTCTINDX EQU   RTCTSDI+1
RTCTSDNA EQU   RTCTSDI
@NM00155 EQU   RTCTASO2
RTCTASNQ EQU   RTCTASO2
RTCTASNA EQU   RTCTASO2
RTCTASNS EQU   RTCTASO2
RTCTASSU EQU   RTCTASO2
RTCTASSW EQU   RTCTASO2
RTCTASCS EQU   RTCTASO2
RTCTASTR EQU   RTCTASO1
RTCTASLP EQU   RTCTASO1
RTCTASRG EQU   RTCTASO1
RTCTASLS EQU   RTCTASO1
RTCTASSQ EQU   RTCTASO1
RTCTASNU EQU   RTCTASO1
RTCTASPS EQU   RTCTASO1
RTCTASAL EQU   RTCTASO1
RTCTSDO  EQU   RTCTOPT+12
RTCTSYO  EQU   RTCTOPT+8
RTCTSUO  EQU   RTCTOPT+4
RTCTSAO  EQU   RTCTOPT
RTCTXXX2 EQU   RTCT+234
RTCTETIM EQU   RTCTERID+6
RTCTEASD EQU   RTCTERID+4
RTCTECPU EQU   RTCTERID+2
RTCTESEQ EQU   RTCTERID
RTCTSDWK EQU   RTCT+220
RTCTTDCB EQU   RTCT+184
RTCTSDSW EQU   RTCT+180
RTCTSEQ# EQU   RTCT+178
RTCTXXX1 EQU   RTCT+177
@NM00154 EQU   RTCTRFLG
RTCTRSTF EQU   RTCTRFLG
RTCTRPER EQU   RTCTRFLG
RTCTRTER EQU   RTCTRFLG
RTCTTEST EQU   RTCT+172
RTCTMSRB EQU   RTCT+168
RTCTMLCK EQU   RTCT+164
RTCTFMT  EQU   RTCT+160
RTCTSDIP EQU   RTCTSDPL
RTCTDEV  EQU   RTCTSDDS+8
RTCTDCB  EQU   RTCTSDDS+4
@NM00153 EQU   RTCTFLG
RTCTDETP EQU   RTCTFLG
RTCTDSUS EQU   RTCTFLG
RTCTDSST EQU   RTCTFLG
RTCTDSNM EQU   RTCTSDDS
RTCTRCB  EQU   RTCT+32
RTCTRECB EQU   RTCT+28
RTCTFASB EQU   RTCT+24
RTCTMECB EQU   RTCT+20
RTCTSDID EQU   RTCT+18
RTCTYYY1 EQU   RTCT+16
RTCTSY04 EQU   RTCTSYD+3
RTCTSY03 EQU   RTCTSYD+2
RTCTSY02 EQU   RTCTSYD+1
RTCTSYM7 EQU   RTCTSY01
RTCTSYM6 EQU   RTCTSY01
RTCTSYM5 EQU   RTCTSY01
RTCTSYM4 EQU   RTCTSY01
RTCTSYM3 EQU   RTCTSY01
RTCTSYM2 EQU   RTCTSY01
RTCTSYM1 EQU   RTCTSY01
RTCTSYM0 EQU   RTCTSY01
RTCTSUP4 EQU   RTCTSUP+3
RTCTSUDF EQU   RTCTSUP3
RTCTSUDE EQU   RTCTSUP3
RTCTSUDD EQU   RTCTSUP3
RTCTSUDC EQU   RTCTSUP3
RTCTSUDB EQU   RTCTSUP3
RTCTSUDA EQU   RTCTSUP3
RTCTSUD9 EQU   RTCTSUP3
RTCTSUD8 EQU   RTCTSUP3
@NM00152 EQU   RTCTSUP2
RTCTSUDH EQU   RTCTSUP2
RTCTSUDG EQU   RTCTSUP2
RTCTSUD7 EQU   RTCTSUP1
RTCTSUD6 EQU   RTCTSUP1
RTCTSUD5 EQU   RTCTSUP1
RTCTSUD4 EQU   RTCTSUP1
RTCTSUD3 EQU   RTCTSUP1
RTCTSUD2 EQU   RTCTSUP1
RTCTSUD1 EQU   RTCTSUP1
RTCTSUD0 EQU   RTCTSUP1
RTCTSAP4 EQU   RTCTSAP+3
RTCTSABF EQU   RTCTSAP3
RTCTSABE EQU   RTCTSAP3
RTCTSABD EQU   RTCTSAP3
RTCTSABC EQU   RTCTSAP3
RTCTSABB EQU   RTCTSAP3
RTCTSABA EQU   RTCTSAP3
RTCTSAB9 EQU   RTCTSAP3
RTCTSAB8 EQU   RTCTSAP3
@NM00151 EQU   RTCTSAP2
RTCTSABH EQU   RTCTSAP2
RTCTSABG EQU   RTCTSAP2
RTCTSAB7 EQU   RTCTSAP1
RTCTSAB6 EQU   RTCTSAP1
RTCTSAB5 EQU   RTCTSAP1
RTCTSAB4 EQU   RTCTSAP1
RTCTSAB3 EQU   RTCTSAP1
RTCTSAB2 EQU   RTCTSAP1
RTCTSAB1 EQU   RTCTSAP1
RTCTSAB0 EQU   RTCTSAP1
RTCTNAME EQU   RTCT
XTLMSBAD EQU   XTLMSBAA+1
@NM00126 EQU   XTLMSBAA
XTLMSBLN EQU   XTLMSBLA+1
@NM00125 EQU   XTLMSBLA
XTLLNTH  EQU   XTLST
LLESYSCT EQU   LLE+10
LLECOUNT EQU   LLE+8
CDATTR3  EQU   CDENTRY+30
CDAUTH   EQU   CDATTR2
CDSYSLIB EQU   CDATTR2
CDOLY    EQU   CDATTR2
@NM00124 EQU   CDATTR2
CDRLC    EQU   CDATTR2
CDREL    EQU   CDATTR2
CDSPZ    EQU   CDATTR2
CDNLR    EQU   CDATTR
CDJPA    EQU   CDATTR
CDNFN    EQU   CDATTR
CDSER    EQU   CDATTR
CDREN    EQU   CDATTR
CDNIP    EQU   CDATTR
@NM00123 EQU   CDENTRY+26
CDUSE    EQU   CDENTRY+24
CDENTPT  EQU   CDENTRY+16
CDRRBP   EQU   CDENTRY+4
CDCHAIN  EQU   CDENTRY
SVRBEND  EQU   RBSECT+192
RBFEPARM EQU   RBSECT+168
SIRBEND  EQU   RBSECT+168
RBRSV158 EQU   RBRSV150
RBRSV157 EQU   RBRSV150
RBRSV156 EQU   RBRSV150
RBRSV155 EQU   RBRSV150
RBRSV154 EQU   RBRSV150
RBRSV153 EQU   RBRSV150
RBRSV152 EQU   RBRSV150
RBRSV151 EQU   RBRSV150
RBRSV149 EQU   RBSECT+166
RBRSV148 EQU   RBSECT+164
RBRSV169 EQU   RBSDATA+3
RBSID    EQU   RBSDATA+2
RBSPKEY  EQU   RBSDATA+1
RBRSV168 EQU   RBSFLG3
RBSTERMO EQU   RBSFLG3
RBSBRNTR EQU   RBSFLG3
RBSPRNTR EQU   RBSFLG3
RBSCNCEL EQU   RBSFLG3
RBSRECRD EQU   RBSFLG3
RBSTERMI EQU   RBSFLG3
RBRSV167 EQU   RBSFLG3
RBSOWNRA EQU   RBSOWNR+1
RBSSUPER EQU   RBSFLGS2
RBSKEY0  EQU   RBSFLGS2
RBRSV166 EQU   RBSFLGS2
RBRSV165 EQU   RBSFLGS2
RBSINUSE EQU   RBSFLGS2
RBRSV164 EQU   RBSFLGS2
RBSXCTL2 EQU   RBSFLGS2
RBRSV163 EQU   RBSFLGS2
RBSPARMA EQU   RBSPARM+1
RBSHALT  EQU   RBSIOPRC
RBSNOIOP EQU   RBSIOPRC
RBSASYNC EQU   RBSFLGS1
RBRSV162 EQU   RBSFLGS1
RBSESTAE EQU   RBSFLGS1
RBSDUMMY EQU   RBSFLGS1
RBSSTAR  EQU   RBSFLGS1
RBSSTAI  EQU   RBSFLGS1
RBSEXIT  EQU   RBSCBB+4
RBSCHAIN EQU   RBSCBB
TIRBEND  EQU   @NM00121+8
PRBEND   EQU   @NM00121+8
RBRSV146 EQU   RBRSV138
RBRSV145 EQU   RBRSV138
RBRSV144 EQU   RBRSV138
RBRSV143 EQU   RBRSV138
RBRSV142 EQU   RBRSV138
RBRSV141 EQU   RBRSV138
RBRSV140 EQU   RBRSV138
RBRSV139 EQU   RBRSV138
RBRSV137 EQU   @NM00121+6
RBRSV136 EQU   @NM00121+4
RBRSV135 EQU   @NM00121
@NM00120 EQU   RBSECT+32
@NM00119 EQU   @NM00117+1
RBSCF    EQU   @NM00118
RBPGMQ1  EQU   RBPGMQ+1
@NM00116 EQU   RBPGMQ
@NM00115 EQU   RBSECT+16
@NM00114 EQU   @NM00110
@NM00113 EQU   @NM00110
RBIQETP  EQU   @NM00110
RBUSIQE  EQU   RBETXR
RBATTN   EQU   @NM00110
@NM00112 EQU   @NM00110
@NM00111 EQU   @NM00110
RBPMSVRB EQU   @NM00108
RBATNXIT EQU   @NM00108
@NM00109 EQU   @NM00108
RBWAITP  EQU   @NM00108
RBFNSVRB EQU   RBTRSVRB
RBSIZE   EQU   RBSECT+8
RBABOPSW EQU   RBSECT+4
@NM00106 EQU   RBPPSAV
RBPRFXND EQU   RBPREFIX+64
RBINLNTH EQU   RBINTCDA
RBWCSA   EQU   RBPREFIX+60
RBRSV004 EQU   RBPREFIX+57
RBSSSYN  EQU   RBFLAGS1
RBSCB    EQU   RBFLAGS1
RBLONGWT EQU   RBFLAGS1
RBASIR   EQU   RBFLAGS1
RBRSV159 EQU   RBFLAGS1
RBABEND  EQU   RBFLAGS1
RBXWAIT  EQU   RBFLAGS1
RBSLOCK  EQU   RBFLAGS1
RBRTRAN  EQU   RBRTPSW2+4
RBRTINCD EQU   RBRTICIL+2
RBRTILC  EQU   RBRTICIL+1
RBRSV160 EQU   RBRTICIL
RBRTPSW1 EQU   RBRTOPSW
RBRSV062 EQU   RBRSV054
RBRSV061 EQU   RBRSV054
RBRSV060 EQU   RBRSV054
RBRSV059 EQU   RBRSV054
RBRSV058 EQU   RBRSV054
RBRSV057 EQU   RBRSV054
RBRSV056 EQU   RBRSV054
RBRSV055 EQU   RBRSV054
RBRSV053 EQU   RBPREFIX+38
RBRSV052 EQU   RBPREFIX+36
RBRSV051 EQU   RBPREFIX+32
RBPRFXST EQU   RBPREFIX+32
RBRSV050 EQU   RBPREFIX+28
RBRSV049 EQU   RBRSV041
RBRSV048 EQU   RBRSV041
RBRSV047 EQU   RBRSV041
RBRSV046 EQU   RBRSV041
RBRSV045 EQU   RBRSV041
RBRSV044 EQU   RBRSV041
RBRSV043 EQU   RBRSV041
RBRSV042 EQU   RBRSV041
RBRSV040 EQU   RBPREFIX+26
RBRSV039 EQU   RBPREFIX+24
RBRSV038 EQU   RBPREFIX+20
RBRSV037 EQU   RBPREFIX+16
RBRSV036 EQU   RBRSV028
RBRSV035 EQU   RBRSV028
RBRSV034 EQU   RBRSV028
RBRSV033 EQU   RBRSV028
RBRSV032 EQU   RBRSV028
RBRSV031 EQU   RBRSV028
RBRSV030 EQU   RBRSV028
RBRSV029 EQU   RBRSV028
RBRSV027 EQU   RBRSV019
RBRSV026 EQU   RBRSV019
RBRSV025 EQU   RBRSV019
RBRSV024 EQU   RBRSV019
RBRSV023 EQU   RBRSV019
RBRSV022 EQU   RBRSV019
RBRSV021 EQU   RBRSV019
RBRSV020 EQU   RBRSV019
RBRSV018 EQU   RBPREFIX+13
RBRSV017 EQU   RBPREFIX+12
RBSRV016 EQU   RBPREFIX+11
RBRSV015 EQU   RBPREFIX+10
RBRSV014 EQU   RBPREFIX+8
RBRSV013 EQU   RBPREFIX+4
RBRSV012 EQU   RBPREFIX
RBEXSAVE EQU   XRBESA
XRBREG15 EQU   RBGRS15
XRBREG14 EQU   RBGRS14
XRBREG13 EQU   RBGRS13
XRBREG12 EQU   RBGRS12
XRBREG11 EQU   RBGRS11
XRBREG10 EQU   RBGRS10
XRBREG9  EQU   RBGRS9
XRBREG8  EQU   RBGRS8
XRBREG7  EQU   RBGRS7
XRBREG6  EQU   RBGRS6
XRBREG5  EQU   RBGRS5
XRBREG4  EQU   RBGRS4
XRBREG3  EQU   RBGRS3
XRBREG2  EQU   RBGRS2
XRBREG1  EQU   RBGRS1
XRBREG0  EQU   RBGRS0
XRBLNKA  EQU   RBLINKB
XRBWT    EQU   RBWCF
@NM00105 EQU   RBBASIC+24
RBOPSWPS EQU   RBOPSWB2
@NM00104 EQU   RBOPSWB2
@NM00103 EQU   XRBPSW
@NM00102 EQU   RBBASIC+12
XRBWAIT  EQU   RBECBWT
XRBFRRB  EQU   RBFDYN
@NM00101 EQU   XSTAB2
XRBACTV  EQU   RBFACTV
XRBTCBP  EQU   RBTCBNXT
@NM00100 EQU   XSTAB1
XRBCKPT  EQU   RBFTCKPT
@NM00099 EQU   XSTAB1
@NM00098 EQU   RBBASIC+8
@NM00097 EQU   RBEXRTNM+1
RBTMIND3 EQU   RBTMFLD
RBTMIND2 EQU   RBTMFLD
RBTMCMP  EQU   RBTMFLD
RBWLIM   EQU   RBTMFLD
RBRSV005 EQU   RBTMFLD
RBTMTOD  EQU   RBTMFLD
RBTMQUE  EQU   RBTMFLD
@NM00096 EQU   RBPRFX
ABDSAVE1 EQU   ABDAREA+672
ABDSAVHD EQU   ABDAREA+592
ABDPADC  EQU   ABDAREA+589
ABDSRC2  EQU   ABDAREA+382
ABDLOG   EQU   ABDAREA+380
ABDSRC1  EQU   ABDAREA+378
ABDPHY   EQU   ABDAREA+376
ABDLCNT  EQU   ABDAREA+372
@NM00083 EQU   ESPARM+29
@NM00082 EQU   ESTAEFLG
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
@NM00081 EQU   ABDGMA+14
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
@NM00080 EQU   ABDBLNKS
@NM00079 EQU   ABDBLNKS
@NM00078 EQU   ABDBLNKS
@NM00077 EQU   ABDBLNKS
@NM00076 EQU   ABDBLNKS
@NM00075 EQU   ABDBLNKS
@NM00074 EQU   ABDBLNKS
ABDBLKN3 EQU   ABDBLNKS
@NM00073 EQU   ABDUPRFN
@NM00072 EQU   ABDUPRFN
@NM00071 EQU   ABDUPRFN
@NM00070 EQU   ABDUPRFN
@NM00069 EQU   ABDUPRFN
@NM00068 EQU   ABDUPRFN
ABDUPRSL EQU   ABDUPRFN
ABDUPRPM EQU   ABDUPRFN
@NM00067 EQU   ABDUPRF
@NM00066 EQU   ABDUPRF
@NM00065 EQU   ABDUPRF
@NM00064 EQU   ABDUPRF
@NM00063 EQU   ABDUPRF
UPRFMT20 EQU   ABDUPRF
UPRFMET  EQU   ABDUPRF
ABDFMTWK EQU   ABDAREA+256
ABDSSPAR EQU   ABDAREA+188
@NM00062 EQU   ABDRES1
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
ABDTRBIT EQU   ABDFLAG2
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
ABDCP1   EQU   ABDAREA+108
ABDLP    EQU   ABDAREA+104
ABDFP    EQU   ABDAREA+100
ABDCP    EQU   ABDAREA+96
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
@NM00061 EQU   ABDPARMS+7
@NM00060 EQU   APFPDATA
APFSPALL EQU   APFPDATA
APFPSW   EQU   APFPDATA
APFJPA   EQU   APFPDATA
APFLPA   EQU   APFPDATA
APFREGS  EQU   APFPDATA
APFSAVE  EQU   APFPDATA
@NM00059 EQU   APFSDAT1
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
@NM00058 EQU   ABDPARMS+2
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
@NM00057 EQU   PGM+2
@NM00056 EQU   EXENDA
@NM00055 EQU   EXSTARTA
@NM00054 EQU   PROGRAM+2
R12      EQU   SAVEAREA+68
R11      EQU   SAVEAREA+64
R10      EQU   SAVEAREA+60
R9       EQU   SAVEAREA+56
R8       EQU   SAVEAREA+52
R7       EQU   SAVEAREA+48
R6       EQU   SAVEAREA+44
R5       EQU   SAVEAREA+40
R4       EQU   SAVEAREA+36
R3       EQU   SAVEAREA+32
R2       EQU   SAVEAREA+28
R1       EQU   SAVEAREA+24
R0       EQU   SAVEAREA+20
@NM00005 EQU   R15
@NM00004 EQU   R14
@NM00003 EQU   SAVEAREA+8
@NM00002 EQU   SAVEAREA+4
@NM00001 EQU   SAVEAREA
.@UNREFD ANOP                          END UNREFERENCED COMPONENTS
@RT00129 EQU   DIE
@RT00132 EQU   AWAY
@RT00136 EQU   NEXTRB
@RT00140 EQU   NEXTRB
@RT00143 EQU   DIE
@RT00146 EQU   FORTRC
@RT00163 EQU   PRBCODE
@RT00167 EQU   BADBACK
@RT00170 EQU   PRBCODE
@RT00174 EQU   DIE
@RT00177 EQU   PRBCODE
@RT00188 EQU   DIE
@RT00192 EQU   DIE
@RT00199 EQU   AWAY
@RT00212 EQU   DIE
@RT00218 EQU   DIE
@RT00221 EQU   AWAY
@RT00228 EQU   AWAY
@RT00237 EQU   DIE
@RT00243 EQU   DIE
@RT00246 EQU   AWAY
@RT00252 EQU   AWAY
@RF00252 EQU   PRBLOOP
@RT00261 EQU   SA1EXITA
@RT00265 EQU   MSGDONE
@RT00270 EQU   MSGDONE1
@RT00284 EQU   TERMINAL
@RT00287 EQU   CKHEAD
@RT00290 EQU   TRYLLE
@RF00302 EQU   SA1EXITA
@RT00312 EQU   TERMINAL
@RT00315 EQU   CKHEAD
@RT00334 EQU   TERMINAL
@RT00343 EQU   TERMINAL
@RT00348 EQU   TERMINAL
@RT00352 EQU   SAL2
@RT00355 EQU   SAL3
@RT00375 EQU   RC12
@RT00379 EQU   TEST2A
@RT00388 EQU   RC12
@RT00398 EQU   RC12
@RT00402 EQU   FOUND
@RF00431 EQU   @RC00420
@RT00438 EQU   WRITE
LOOPOUT  EQU   @EL00004
@RT00495 EQU   BACKCK
@RT00508 EQU   NORM
@RT00512 EQU   NORM
@RT00464 EQU   LOOPOUT
@RT00471 EQU   LOOPOUT
@ENDDATA EQU   *
         PRINT NOGEN
         IHAPSA
         CVT   DSECT=YES
         IKJTCB
         END   IEAVAD07,(C'PLS1709',0802,82300)
/*
//*
//STEP3   EXEC PGM=IEBGENER
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  *
  IDENTIFY IEAVAD07('ZP60028')
/*
//SYSUT2   DD  DSN=&&SMPMCS,DISP=(MOD,PASS)
//SYSIN    DD  DUMMY
//*
//STEP4CK EXEC SMPREC,WORK='SYSALLDA'
//SMPPTFIN DD  DSN=&&SMPMCS,DISP=(OLD,DELETE)
//SMPCNTL  DD  *
  RECEIVE
          SELECT(ZP60028)
          .
/*
//*
//STEP5   EXEC SMPAPP,WORK='SYSALLDA'
//SMPCNTL  DD  *
  APPLY
        SELECT(ZP60028)
        CHECK
        .
/*
//*
//STEP5   EXEC SMPAPP,COND=(0,NE),WORK='SYSALLDA'
//SMPCNTL  DD  *
  APPLY
        SELECT(ZP60028)
        DIS(WRITE)
        .
/*
//
//ZP60029  JOB (SYSGEN),'J03 M40: ZP60029',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             REGION=4096K
/*JOBPARM LINES=100
//JOBCAT   DD  DSN=SYS1.VSAM.MASTER.CATALOG,DISP=SHR
//*
//*  ZAP TO CHANGE EBCDIC <=> ASCII TRANSLATE TABLES.
//*
//*  NOTICE: CUSTOMIZE THE TABLES FOR *YOUR* PURPOSES BEFORE
//*          APPLYING THIS USERMOD.
//*
//RECEIVE EXEC SMPREC,WORK='SYSALLDA'
//SMPPTFIN DD  *
++USERMOD(ZP60029)           /* XLATE TRANSLATE TABLES */  .
++VER(Z038) FMID(EDM1102) PRE(UZ54016)
 /*
   PROBLEM DESCRIPTION:
     MVS TRANSLATION BETWEEN EBCDIC AND ASCII IS INADEQUATE.
       CHARACTER SET TRANSLATION BETWEEN EBCDIC AND ASCII CAN
       BE REQUESTED BY THE XLATE MACRO (SVC 103) WHICH IS USED
       BY THE SYSTEM WHEN PROCESSING ASCII TAPE LABLES AND WHEN
       OPTCD=Q IS SET IN THE DCB FOR ASCII TAPE PROCESSING.

       THE EBCDIC-TO-ASCII AND ASCII-TO-EBCDIC TRANSLATE TABLES
       ARE HARD-CODED IN SVC 103, BUT THEY TREAT ASCII AS A
       7-BIT CHARACTER SET WITH ONLY 128 CODE POINTS.  THIS IS
       INADEQUATE FOR MOST CURRENT PROCESSING NEEDS WHERE ANSI
       IS USUALLY USED.  ANSI IS BASED ON ASCII BUT HAS 256
       SINGLE-BYTE CODE POINTS (AS DOES EBCDIC).

       WHEN CONSTRUCTING TRANSLATE TABLES BETWEEN THE TWO
       CHARACTER SETS, ONE DESIGN GOAL NORMALLY ADOPTED
       IS THAT THE APPLICATION OF BOTH TRANSLATE TABLES
       (IN EITHER ORDER) WILL LEAVE ALL POSSIBLE 256 CODE
       POINTS UNCHANGED.

   SPECIAL CONDITIONS:
     ACTION:
       A "CLPA" MUST BE PERFORMED AT IPL TIME FOR THIS SYSMOD TO
       BECOME ACTIVE.

   COMMENTS:
     PRYCROFT SIX P/L PUBLIC DOMAIN USERMOD FOR MVS 3.8 NO. 29.

     THE FOLLOWING MODULES AND/OR MACROS ARE AFFECTED BY THIS USERMOD:
     MODULES:
       IGC0010C
         THE ORIGINAL SOURCE CODE OF THE TRANSLATE TABLES IS
         INCLUDED BELOW.  NOTE THAT X'1A' IS THE ASCII SUBSTITUTE
         CHARACTER, AND X'3F' IS THE EBCDIC SUBSTITUTE CHARACTER.

            *
            *        TABLE FOR TRANSLATING FROM EBCDIC TO ASCII
            *
            TABA     DC    X'000102031A091A7F'      ASCII TABLE
            ENT080FA DC    X'1A1A1A0B0C0D0E0F'        *
            ENT1017A DC    X'101112131A1A081A'        *
            ENT181FA DC    X'18191A1A1C1D1E1F'        *
            ENT2027A DC    X'1A1A1A1A1A0A171B'        *
            ENT282FA DC    X'1A1A1A1A1A050607'        *
            ENT3037A DC    X'1A1A161A1A1A1A04'        *
            ENT383FA DC    X'1A1A1A1A14151A1A'        *
            ENT4047A DC    X'201A1A1A1A1A1A1A'        *
            ENT484FA DC    X'1A1A5B2E3C282B21'        *
            ENT5057A DC    X'261A1A1A1A1A1A1A'        *
            ENT585FA DC    X'1A1A5D242A293B5E'        *
            ENT6067A DC    X'2D2F1A1A1A1A1A1A'        *
            ENT686FA DC    X'1A1A7C2C255F3E3F'        *
            ENT7077A DC    X'1A1A1A1A1A1A1A1A'        *
            ENT787FA DC    X'1A603A2340273D22'        *
            ENT8087A DC    X'1A61626364656667'        *
            ENT888FA DC    X'68691A1A1A1A1A1A'        *
            ENT9097A DC    X'1A6A6B6C6D6E6F70'        *
            ENT989FA DC    X'71721A1A1A1A1A1A'        *
            ENTA0A7A DC    X'1A7E737475767778'        *
            ENTA8AFA DC    X'797A1A1A1A1A1A1A'        *
            ENTB0B7A DC    X'1A1A1A1A1A1A1A1A'        *
            ENTB8BFA DC    X'1A1A1A1A1A1A1A1A'        *
            ENTC0C7A DC    X'7B41424344454647'        *
            ENTC8CFA DC    X'48491A1A1A1A1A1A'        *
            ENTD0D7A DC    X'7D4A4B4C4D4E4F50'        *
            ENTD8DFA DC    X'51521A1A1A1A1A1A'        *
            ENTE0E7A DC    X'5C1A535455565758'        *
            ENTE8EFA DC    X'595A1A1A1A1A1A1A'        *
            ENTF0F7A DC    X'3031323334353637'        *
            ENTF8FFA DC    X'38391A1A1A1A1A1A'        *
            *
            *        TABLE FOR TRANSLATING FROM ASCII TO EBCDIC
            *
            TABE     DC    X'00010203372D2E2F'      EBCDIC TABLE
            ENT080FE DC    X'1605250B0C0D0E0F'        *
            ENT1017E DC    X'101112133C3D3226'        *
            ENT181FE DC    X'18193F271C1D1E1F'        *
            ENT2027E DC    X'404F7F7B5B6C507D'        *
            ENT282FE DC    X'4D5D5C4E6B604B61'        *
            ENT3037E DC    X'F0F1F2F3F4F5F6F7'        *
            ENT383FE DC    X'F8F97A5E4C7E6E6F'        *
            ENT4047E DC    X'7CC1C2C3C4C5C6C7'        *
            ENT484FE DC    X'C8C9D1D2D3D4D5D6'        *
            ENT5057E DC    X'D7D8D9E2E3E4E5E6'        *
            ENT585FE DC    X'E7E8E9ADE0BD5F6D'        *
            ENT6067E DC    X'7981828384858687'        *
            ENT686FE DC    X'8889919293949596'        *
            ENT7077E DC    X'979899A2A3A4A5A6'        *
            ENT787FE DC    X'A7A8A9C06AD0A107'        *
            ENT8087E DC    X'3F3F3F3F3F3F3F3F'        *
            ENT888FE DC    X'3F3F3F3F3F3F3F3F'        *
            ENT9097E DC    X'3F3F3F3F3F3F3F3F'        *
            ENT989FE DC    X'3F3F3F3F3F3F3F3F'        *
            ENTA0A7E DC    X'3F3F3F3F3F3F3F3F'        *
            ENTA8AFE DC    X'3F3F3F3F3F3F3F3F'        *
            ENTB0B7E DC    X'3F3F3F3F3F3F3F3F'        *
            ENTB8BFE DC    X'3F3F3F3F3F3F3F3F'        *
            ENTC0C7E DC    X'3F3F3F3F3F3F3F3F'        *
            ENTC8CFE DC    X'3F3F3F3F3F3F3F3F'        *
            ENTD0D7E DC    X'3F3F3F3F3F3F3F3F'        *
            ENTD8DFE DC    X'3F3F3F3F3F3F3F3F'        *
            ENTE0E7E DC    X'3F3F3F3F3F3F3F3F'        *
            ENTE8EFE DC    X'3F3F3F3F3F3F3F3F'        *
            ENTF0F7E DC    X'3F3F3F3F3F3F3F3F'        *
            ENTF8FFE DC    X'3F3F3F3F3F3F3F3F'        *

         DATA POINTS WHICH ARE EXPECTED TO BE UNCHANGED BY ANY
         LOCAL CUSTOMIZATION ARE USED TO VERIFY THE LOCATION
         OF THE TRANSLATE TABLES IN ORDER TO FACILITATE A
         REAPPLY WITHOUT THE NEED FOR AN ACTUAL RESTORATION
         OF THE ORIGINAL MODULE.
 */.
++ZAP(IGC0010C) DISTLIB(AOSD0).
 NAME IGC0010C
 IDRDATA ZP60029
*
VER 00BE 00010203                EBCDIC->ASCII TABLE START
VER 00FE 20                      C' '
VER 017F 414243444546474849      C'ABCDEFGHI'
VER 018F 4A4B4C4D4E4F505152      C'JKLMNOPQR'
VER 01A0 535455565758595A        C'STUVWXYZ'
VER 01AE 30313233343536373839    C'0123456789'
*
VER 01BE 00010203                ASCII->EBCDIC TABLE START
VER 01DE 40                      C' '
VER 01EE F0F1F2F3F4F5F6F7F8F9    C'0123456789'
VER 01FF C1C2C3C4C5C6C7C8C9      C'ABCDEFGHI'
VER 0208 D1D2D3D4D5D6D7D8D9      C'JKLMNOPQR'
VER 0211 E2E3E4E5E6E7E8E9        C'STUVWXYZ'
*
REP 00BE 000102039C09867F        E->A 00-07
REP 00C6 978D8E0B0C0D0E0F        E->A 08-0F
REP 00CE 101112139D850887        E->A 10-17
REP 00D6 1819928F1C1D1E1F        E->A 18-1F
REP 00DE 80818283840A171B        E->A 20-27
REP 00E6 88898A8B8C050607        E->A 28-2F
REP 00EE 9091169394959604        E->A 30-37
REP 00F6 98999A9B14159E1A        E->A 38-3F
REP 00FE 20A0E2E4E0E1E3E5        E->A 40-47
REP 0106 E7F1A22E3C282B7C        E->A 48-4F
REP 010E 26E9EAEBE8EDEEEF        E->A 50-57
REP 0116 ECDF21242A293BAC        E->A 58-5F
REP 011E 2D2FC2C4C0C1C3C5        E->A 60-67
REP 0126 C7D1A62C255F3E3F        E->A 68-6F
REP 012E F8C9CACBC8CDCECF        E->A 70-77
REP 0136 CC603A2340273D22        E->A 78-7F
REP 013E D861626364656667        E->A 80-87
REP 0146 6869ABBBF0FDFEB1        E->A 88-8F
REP 014E B06A6B6C6D6E6F70        E->A 90-97
REP 0156 7172AABAE6B8C6A4        E->A 98-9F
REP 015E B57E737475767778        E->A A0-A7
REP 0166 797AA1BFD0DDDEAE        E->A A8-AF
REP 016E 5EA3A5B7A9A7B6BC        E->A B0-B7
REP 0176 BDBE5B5DAFA8B4D7        E->A B8-BF
REP 017E 7B41424344454647        E->A C0-C7
REP 0186 4849ADF4F6F2F3F5        E->A C8-CF
REP 018E 7D4A4B4C4D4E4F50        E->A D0-D7
REP 0196 5152B9FBFCF9FAFF        E->A D8-DF
REP 019E 5CF7535455565758        E->A E0-E7
REP 01A6 595AB2D4D6D2D3D5        E->A E8-EF
REP 01AE 3031323334353637        E->A F0-F7
REP 01B6 3839B3DBDCD9DA9F        E->A F8-FF
*
REP 01BE 00010203372D2E2F        A->E 00-07
REP 01C6 1605250B0C0D0E0F        A->E 08-0F
REP 01CE 101112133C3D3226        A->E 10-17
REP 01D6 18193F271C1D1E1F        A->E 18-1F
REP 01DE 405A7F7B5B6C507D        A->E 20-27
REP 01E6 4D5D5C4E6B604B61        A->E 28-2F
REP 01EE F0F1F2F3F4F5F6F7        A->E 30-37
REP 01F6 F8F97A5E4C7E6E6F        A->E 38-3F
REP 01FE 7CC1C2C3C4C5C6C7        A->E 40-47
REP 0206 C8C9D1D2D3D4D5D6        A->E 48-4F
REP 020E D7D8D9E2E3E4E5E6        A->E 50-57
REP 0216 E7E8E9BAE0BBB06D        A->E 58-5F
REP 021E 7981828384858687        A->E 60-67
REP 0226 8889919293949596        A->E 68-6F
REP 022E 979899A2A3A4A5A6        A->E 70-77
REP 0236 A7A8A9C04FD0A107        A->E 78-7F
REP 023E 2021222324150617        A->E 80-87
REP 0246 28292A2B2C090A1B        A->E 88-8F
REP 024E 30311A3334353608        A->E 90-97
REP 0256 38393A3B04143EFF        A->E 98-9F
REP 025E 41AA4AB19FB26AB5        A->E A0-A7
REP 0266 BDB49A8A5FCAAFBC        A->E A8-AF
REP 026E 908FEAFABEA0B6B3        A->E B0-B7
REP 0276 9DDA9B8BB7B8B9AB        A->E B8-BF
REP 027E 6465626663679E68        A->E C0-C7
REP 0286 7471727378757677        A->E C8-CF
REP 028E AC69EDEEEBEFECBF        A->E D0-D7
REP 0296 80FDFEFBFCADAE59        A->E D8-DF
REP 029E 4445424643479C48        A->E E0-E7
REP 02A6 5451525358555657        A->E E8-EF
REP 02AE 8C49CDCECBCFCCE1        A->E F0-F7
REP 02B6 70DDDEDBDC8D8EDF        A->E F8-FF
/*
//SMPCNTL  DD  *
  RECEIVE
          SELECT(ZP60029)
          .
/*
//*
//APPLY   EXEC SMPAPP,WORK='SYSALLDA'
//SMPCNTL  DD  *
  APPLY
        SELECT(ZP60029)
        CHECK
        .
/*
//*
//APPLYCK EXEC SMPAPP,COND=(0,NE),WORK='SYSALLDA'
//SMPCNTL  DD  *
  APPLY
        SELECT(ZP60029)
        DIS(WRITE)
        .
/*
//
//ZP60030  JOB (SYSGEN),'J04 M41: ZP60030',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             REGION=4096K
/*JOBPARM LINES=100
//JOBCAT   DD  DSN=SYS1.VSAM.MASTER.CATALOG,DISP=SHR
//*
//*  CORRECT MF/1 CHANNEL MEASUREMENT AND LOGGING TO SMF (TYPE 73).
//*
//STEP01  EXEC PGM=IEBGENER
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  *
++USERMOD(ZP60030)       /* FIX MF/1 CHANNEL MEASUREMENT */  .
++VER(Z038) FMID(EMF1102)
 /*
   PROBLEM DESCRIPTION:
     MF/1 DOES NOT CORRECTLY ACCESS THE CHANNEL AVAILABILITY TABLE.
       THE CHANNEL AVAILABILITY TABLE (CAT) HAS MOVED FROM THE
       PCCA (WHERE THERE WAS ONE FOR EACH ACTIVE CPU) TO COMMON
       STORAGE (SQA).  FURTHER, CAT ENTRIES HAVE BEEN EXTENDED
       FROM EIGHT BYTES IN LENGTH TO SIXTEEN BYTES IN LENGTH.

       THIS USERMOD UPDATES THE MF/1 CHANNEL SAMPLING MODULE
       IRBMFECH AND THE MF/1 CHANNEL INITIALIZATION MODULE
       IRBMFIHA TO CORRECTLY ACCESS CAT ENTRIES.

       A CONSEQUENCE OF THIS IS THAT TYPE 73 SMF RECORDS WILL
       NOW CONTAIN NON-ZERO CHANNEL DATA.

   SPECIAL CONDITIONS:
     ACTION:
       A "CLPA" MUST BE PERFORMED AT IPL TIME FOR THIS SYSMOD TO
       BECOME ACTIVE.

   COMMENTS:
     PRYCROFT SIX P/L PUBLIC DOMAIN USERMOD FOR MVS 3.8 NUMBER 30.

     REWORK HISTORY:
       2011-01-30: INITIAL VERSION.

     THE FOLLOWING MODULES AND/OR MACROS ARE AFFECTED BY THIS USERMOD:
     MODULES:
       IRBMFECH
       IRBMFIHA
 */.
++MOD(IRBMFECH) DISTLIB(ALPALIB).
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
         TITLE '*/* IRBMFECH - CHANNEL SAMPLING MODULE                 *00001000
                       '                                                00002000
IRBMFECH CSECT ,                                                   0001 00003000
@MAINENT DS    0H                                                  0001 00004000
         USING *,@15                                               0001 00005000
         B     @PROLOG                                             0001 00006000
         DC    AL1(16)                                             0001 00007000
         DC    C'IRBMFECH  74.086'                                 0001 00008000
         DROP  @15                                                      00009000
@PROLOG  STM   @14,@12,12(@13)                                     0001 00010000
         BALR  @09,0                                               0001 00011000
@PSTART  DS    0H                                                  0001 00012000
         USING @PSTART,@09                                         0001 00013000
*                                                                  0022 00014000
*/* BEGIN MAINLINE PROCESSING                                        */ 00015000
*                                                                  0022 00016000
*   ECCESAMP=ECCESAMP+1;            /* INCREMENT SAMPLES             */ 00017000
*                                                                  0022 00018000
         LA    @12,1                                               0022 00019000
         AL    @12,ECCESAMP(,ECCEDPTR)                             0022 00020000
         ST    @12,ECCESAMP(,ECCEDPTR)                             0022 00021000
*   /*****************************************************************/ 00022000
*   /*                                                               */ 00023000
*   /* LOOP THROUGH ALL CDBS FOR VALID CPES AND SAMPLE SIOS. IF CPU  */ 00024000
*   /* IS THIS ONE, ALSO SAMPLE BUSY AND CPU OVERLAP. IF NOT, SIGNAL */ 00025000
*   /* THAT CPU TO COLLECT THAT INFORMATION MACDATE Y-2 73018        */ 00026000
*   /*                                                               */ 00027000
*   /*****************************************************************/ 00028000
*                                                                  0023 00029000
*   RESPECIFY                                                      0023 00030000
*    (ECCPEPTR,                                                    0023 00031000
*     ECCDBPTR)RESTRICTED;                                         0023 00032000
*                                                                  0023 00033000
*   /*****************************************************************/ 00034000
*   /*                                                               */ 00035000
*   /* LOOP THROUGH CPES                                             */ 00036000
*   /*                                                               */ 00037000
*   /*****************************************************************/ 00038000
*                                                                  0024 00039000
*   DO ECCPEPTR=ECCECPEQ BY LENGTH(ECCPE)TO ECCECPEQ+(ECCECPUS-1)* 0024 00040000
*         LENGTH(ECCPE);                                           0024 00041000
         L     ECCPEPTR,ECCECPEQ(,ECCEDPTR)                        0024 00042000
         B     @DE00024                                            0024 00043000
@DL00024 DS    0H                                                  0025 00044000
*     IF ECCPVALD='0'B THEN                                        0025 00045000
         TM    ECCPVALD(ECCPEPTR),B'00000001'                      0025 00046000
         BZ    @RT00025                                            0025 00047000
*       GO TO ECLAB4;               /* SKIP DATA COLLECTION IF     0026 00048000
*                                      INVALID CPU ENTRY             */ 00049000
*                                                                  0026 00050000
*     /***************************************************************/ 00051000
*     /*                                                             */ 00052000
*     /* ESTABLISH ADDRESSABILITY TO PCCA MACDATE Y-2 73018          */ 00053000
*     /*                                                             */ 00054000
*     /***************************************************************/ 00055000
*                                                                  0027 00056000
*     RESPECIFY                                                    0027 00057000
*      (PCCAPTR)RESTRICTED;                                        0027 00058000
*     PCCAPTR=PCCAT00P(((ECCPEPTR-ECCECPEQ)/LENGTH(ECCPE))+1);     0028 00059000
         LA    @12,8                                               0028 00060000
         LR    @08,ECCPEPTR                                        0028 00061000
         S     @08,ECCECPEQ(,ECCEDPTR)                             0028 00062000
         LR    PCCAPTR,@08                                         0028 00063000
         SRDA  PCCAPTR,32                                          0028 00064000
         DR    PCCAPTR,@12                                         0028 00065000
         SLA   @05,2                                               0028 00066000
         L     @15,CVTPTR                                          0028 00067000
         L     @15,CVTPCCAT(,@15)                                  0028 00068000
         L     PCCAPTR,PCCAT00P(@05,@15)                           0028 00069000
*     IF((ECCPEPTR-ECCECPEQ)/LENGTH(ECCPE))=PSACPUSA THEN          0029 00070000
*                                                                  0029 00071000
         LR    @06,@08                                             0029 00072000
         SRDA  @06,32                                              0029 00073000
         DR    @06,@12                                             0029 00074000
         CH    @07,PSACPUSA                                        0029 00075000
         BNE   @RF00029                                            0029 00076000
*       /*************************************************************/ 00077000
*       /*                                                           */ 00078000
*       /* THIS CPE REPRESENTS THE CURRENT CPU                       */ 00079000
*       /*                                                           */ 00080000
*       /*************************************************************/ 00081000
*                                                                  0030 00082000
*       DO;                                                        0030 00083000
*                                                                  0030 00084000
*         /***********************************************************/ 00085000
*         /*                                                         */ 00086000
*         /* LOOP THROUGH CDBS                                       */ 00087000
*         /*                                                         */ 00088000
*         /***********************************************************/ 00089000
*                                                                  0031 00090000
*         DO ECCDBPTR=ECCPCDBQ BY LENGTH(ECCDB)TO ECCPCDBQ+(ECCPCNUM-1) 00091000
*               *LENGTH(ECCDB);                                    0031 00092000
         L     ECCDBPTR,ECCPCDBQ(,ECCPEPTR)                        0031 00093000
         B     @DE00031                                            0031 00094000
@DL00031 DS    0H                                                  0032 00095000
*           IF ECCDVALD='0'B THEN                                  0032 00096000
         TM    ECCDVALD(ECCDBPTR),B'00000100'                      0032 00097000
         BZ    @RT00032                                            0032 00098000
*             GO TO ECLAB2;         /* SKIP DATA COLLECTION IF     0033 00099000
*                                      INVALID CHANNEL ENTRY         */ 00100000
*                                                                  0033 00101000
*           /*********************************************************/ 00102000
*           /*                                                       */ 00103000
*           /* MACDATE Y-2 73018                                     */ 00104000
*           /*                                                       */ 00105000
*           /*********************************************************/ 00106000
*                                                                  0034 00107000
*           RESPECIFY                                              0034 00108000
*            (ECWRKRG1,                                            0034 00109000
*             ECWRKRG2,                                            0034 00110000
*             CATPTR)RESTRICTED;                                   0034 00111000
*                                                                  0034 00112000
*           /*********************************************************/ 00113000
*           /*                                                       */ 00114000
*           /* ESTABLISH ADDRESSABILITY TO CAT ENTRY                 */ 00115000
*           /*                                                       */ 00116000
*           /*********************************************************/ 00117000
*                                                                  0035 00118000
*           CATPTR=ADDR(PCCACAT)+((ECCDBPTR-ECCPCDBQ)/LENGTH(ECCDB))*   00119000
*               LENGTH(CAT);                                       0035 00120000
**       LA    CATPTR,PCCACAT(,PCCAPTR)                        @ZP60030 00121000
         L     CATPTR,CVTPTR                                   @ZP60030 00121010
         L     CATPTR,1052(,CATPTR) <=CVTCST                   @ZP60030 00121020
         L     CATPTR,0(,CATPTR)                               @ZP60030 00121030
         LR    @10,ECCDBPTR                                        0035 00122000
         S     @10,ECCPCDBQ(,ECCPEPTR)                             0035 00123000
         SRDA  @10,32                                              0035 00124000
         D     @10,@CF01349                                        0035 00125000
         SLA   @11,4                   WAS 3                   @ZP60030 00126000
         AR    CATPTR,@11                                          0035 00127000
*           ECWRKRG1=CATSIOCT;      /* GET CURRENT SIO READING       */ 00128000
*                                                                  0036 00129000
         SR    ECWRKRG1,ECWRKRG1                                   0036 00130000
         ICM   ECWRKRG1,3,CATSIOCT(CATPTR)                         0036 00131000
*           /*********************************************************/ 00132000
*           /*                                                       */ 00133000
*           /* GET LAST SIO READING                                  */ 00134000
*           /*                                                       */ 00135000
*           /*********************************************************/ 00136000
*                                                                  0037 00137000
*           ECWRKRG2=ECCDLSIO;                                     0037 00138000
*                                                                  0037 00139000
         SR    ECWRKRG2,ECWRKRG2                                   0037 00140000
         ICM   ECWRKRG2,3,ECCDLSIO(ECCDBPTR)                       0037 00141000
*           /*********************************************************/ 00142000
*           /*                                                       */ 00143000
*           /* SAVE NEW READING                                      */ 00144000
*           /*                                                       */ 00145000
*           /*********************************************************/ 00146000
*                                                                  0038 00147000
*           ECCDLSIO=ECWRKRG1;                                     0038 00148000
*                                                                  0038 00149000
         STH   ECWRKRG1,ECCDLSIO(,ECCDBPTR)                        0038 00150000
*           /*********************************************************/ 00151000
*           /*                                                       */ 00152000
*           /* SUBTRACT OLD SIO COUNT FROM NEW OBSERVATION           */ 00153000
*           /*                                                       */ 00154000
*           /*********************************************************/ 00155000
*                                                                  0039 00156000
*           ECWRKRG1=ECWRKRG1-ECWRKRG2;                            0039 00157000
*                                                                  0039 00158000
         SR    ECWRKRG1,ECWRKRG2                                   0039 00159000
*           /*********************************************************/ 00160000
*           /*                                                       */ 00161000
*           /* COMPENSATE FOR POSSIBLE WRAPAROUND                    */ 00162000
*           /*                                                       */ 00163000
*           /*********************************************************/ 00164000
*                                                                  0040 00165000
*           ECWRKRG1=ECWRKRG1&ECKILLHI;                            0040 00166000
*                                                                  0040 00167000
         N     ECWRKRG1,@CF01329                                   0040 00168000
*           /*********************************************************/ 00169000
*           /*                                                       */ 00170000
*           /* ADD ACCUMULATOR TO SIOS SINCE LAST SAMPLE             */ 00171000
*           /*                                                       */ 00172000
*           /*********************************************************/ 00173000
*                                                                  0041 00174000
*           ECWRKRG1=ECWRKRG1+ECCDSIOS;                            0041 00175000
*                                                                  0041 00176000
         AL    ECWRKRG1,ECCDSIOS(,ECCDBPTR)                        0041 00177000
*           /*********************************************************/ 00178000
*           /*                                                       */ 00179000
*           /* STORE NEW ACCUMULATED SIOS                            */ 00180000
*           /*                                                       */ 00181000
*           /*********************************************************/ 00182000
*                                                                  0042 00183000
*           ECCDSIOS=ECWRKRG1;                                     0042 00184000
*                                                                  0042 00185000
         ST    ECWRKRG1,ECCDSIOS(,ECCDBPTR)                        0042 00186000
*           /*********************************************************/ 00187000
*           /*                                                       */ 00188000
*           /* MACDATE Y-2 73018                                     */ 00189000
*           /*                                                       */ 00190000
*           /*********************************************************/ 00191000
*                                                                  0043 00192000
*           RESPECIFY                                              0043 00193000
*            (ECWRKRG1,                                            0043 00194000
*             ECWRKRG2,                                            0043 00195000
*             CATPTR)UNRESTRICTED;                                 0043 00196000
*           IF ECCDALIV='0'B|ECCDCTYP=MULTPLEX THEN                0044 00197000
         TM    ECCDALIV(ECCDBPTR),B'00000001'                      0044 00198000
         BZ    @RT00044                                            0044 00199000
         TM    ECCDCTYP(ECCDBPTR),B'00010000'                      0044 00200000
         BNO   @RF00044                                            0044 00201000
         TM    ECCDCTYP(ECCDBPTR),B'11100000'                      0044 00202000
         BZ    @RT00044                                            0044 00203000
*             GO TO ECLAB2;         /* DONT COLLECT IF OFFLINE OR  0045 00204000
*                                      BYTE MULTIPLEXOR              */ 00205000
*                                                                  0045 00206000
*           /*********************************************************/ 00207000
*           /*                                                       */ 00208000
*           /* MACDATE Y-2 73018                                     */ 00209000
*           /*                                                       */ 00210000
*           /*********************************************************/ 00211000
*                                                                  0046 00212000
*           RESPECIFY                                              0046 00213000
*            (ECWRKRG1)RESTRICTED;                                 0046 00214000
*                                                                  0046 00215000
@RF00044 DS    0H                                                  0047 00216000
*           /*********************************************************/ 00217000
*           /*                                                       */ 00218000
*           /* CHANNEL ADDRESS TO WORK REGISTER                      */ 00219000
*           /*                                                       */ 00220000
*           /*********************************************************/ 00221000
*                                                                  0047 00222000
*           ECWRKRG1=(ECCDBPTR-ECCPCDBQ)/LENGTH(ECCDB);            0047 00223000
         LR    ECWRKRG1,ECCDBPTR                                   0047 00224000
         S     ECWRKRG1,ECCPCDBQ(,ECCPEPTR)                        0047 00225000
         SRDA  ECWRKRG1,32                                         0047 00226000
         D     ECWRKRG1,@CF01349                                   0047 00227000
         LR    ECWRKRG1,@07                                        0047 00228000
*           ECWRKRG1=ECWRKRG1*256;  /* SHIFT LEFT 8 TO PUT ADDRESS 0048 00229000
*                                      INTO BIT POSITIONS 16 - 23    */ 00230000
         SLL   ECWRKRG1,8                                          0048 00231000
*           TCH(ECWRKRG1);          /* ISSUE TCH INSTRUCTION         */ 00232000
         TCH   0(ECWRKRG1)                                         0049 00233000
*           BC(13,ECLAB2);          /* BRANCH IF NOT BUSY            */ 00234000
*                                                                  0050 00235000
         BC    13,ECLAB2                                           0050 00236000
*           /*********************************************************/ 00237000
*           /*                                                       */ 00238000
*           /* MACDATE Y-2 73018                                     */ 00239000
*           /*                                                       */ 00240000
*           /*********************************************************/ 00241000
*                                                                  0051 00242000
*           RESPECIFY                                              0051 00243000
*            (ECWRKRG1)UNRESTRICTED;                               0051 00244000
*                                                                  0051 00245000
*           /*********************************************************/ 00246000
*           /*                                                       */ 00247000
*           /* INCREMENT BUSY SAMPLES FOR THIS CHANNEL               */ 00248000
*           /*                                                       */ 00249000
*           /*********************************************************/ 00250000
*                                                                  0052 00251000
*           ECCDBUSY=ECCDBUSY+1;                                   0052 00252000
         LA    @12,1                                               0052 00253000
         L     @08,ECCDBUSY(,ECCDBPTR)                             0052 00254000
         ALR   @08,@12                                             0052 00255000
         ST    @08,ECCDBUSY(,ECCDBPTR)                             0052 00256000
*           IF PSATOLD=CVTWTCB THEN                                0053 00257000
*                                                                  0053 00258000
         L     @08,CVTPTR                                          0053 00259000
         CLC   PSATOLD(4),CVTWTCB(@08)                             0053 00260000
         BNE   @RF00053                                            0053 00261000
*             /*******************************************************/ 00262000
*             /*                                                     */ 00263000
*             /* THE CPU WAS WAITING WHEN IT TOOK THE INTERRUPT.     */ 00264000
*             /* INCREMENT OVERLAP SAMPLES                           */ 00265000
*             /*                                                     */ 00266000
*             /*******************************************************/ 00267000
*                                                                  0054 00268000
*             ECCDOLAP=ECCDOLAP+1;                                 0054 00269000
         AL    @12,ECCDOLAP(,ECCDBPTR)                             0054 00270000
         ST    @12,ECCDOLAP(,ECCDBPTR)                             0054 00271000
*ECLAB2:                            /* END OF ALL DATA COLLECTION FOR   00272000
*                                      A CHANNEL ON CURRENT CPU      */ 00273000
*         END;                                                     0055 00274000
@RF00053 DS    0H                                                  0055 00275000
ECLAB2   AH    ECCDBPTR,@CH01349                                   0055 00276000
@DE00031 LH    @12,ECCPCNUM(,ECCPEPTR)                             0055 00277000
         BCTR  @12,0                                               0055 00278000
         MH    @12,@CH01349                                        0055 00279000
         A     @12,ECCPCDBQ(,ECCPEPTR)                             0055 00280000
         CR    ECCDBPTR,@12                                        0055 00281000
         BNH   @DL00031                                            0055 00282000
*       END;                                                       0056 00283000
*     ELSE                          /* THIS CPU IS NOT THE CURRENT 0057 00284000
*                                      CPU                           */ 00285000
*       DO;                                                        0057 00286000
*                                                                  0057 00287000
         B     @RC00029                                            0057 00288000
@RF00029 DS    0H                                                  0058 00289000
*         /***********************************************************/ 00290000
*         /*                                                         */ 00291000
*         /* SIGNAL THAT PROCESSOR WITH REMOTE PENDING INTERRUPT     */ 00292000
*         /* MACDATE Y-2 73018                                       */ 00293000
*         /*                                                         */ 00294000
*         /***********************************************************/ 00295000
*                                                                  0058 00296000
*         RESPECIFY                                                0058 00297000
*          (ECWRKRG1,                                              0058 00298000
*           GPR00P,                                                0058 00299000
*           GPR14P,                                                0058 00300000
*           GPR15P)RESTRICTED;                                     0058 00301000
*         ECWRKRG1=GPR01P;          /* SAVE CEDT ADDRESS ACROSS    0059 00302000
*                                      INTERFACE                     */ 00303000
*                                                                  0059 00304000
         LR    ECWRKRG1,GPR01P                                     0059 00305000
*         /***********************************************************/ 00306000
*         /*                                                         */ 00307000
*         /* PCCA ADDRESS TO REGISTER                                */ 00308000
*         /*                                                         */ 00309000
*         /***********************************************************/ 00310000
*                                                                  0060 00311000
*         GPR01P=PCCAPTR;                                          0060 00312000
         LR    GPR01P,PCCAPTR                                      0060 00313000
*         GENERATE(RPSGNL MF1TCH,CPU=(1));                         0061 00314000
         RPSGNL MF1TCH,CPU=(1)                                          00315000
*         GPR01P=ECWRKRG1;          /* RESTORE REG 1                 */ 00316000
*                                                                  0062 00317000
         LR    GPR01P,ECWRKRG1                                     0062 00318000
*         /***********************************************************/ 00319000
*         /*                                                         */ 00320000
*         /* MACDATE Y-2 73018                                       */ 00321000
*         /*                                                         */ 00322000
*         /***********************************************************/ 00323000
*                                                                  0063 00324000
*         RESPECIFY                                                0063 00325000
*          (ECWRKRG1,                                              0063 00326000
*           GPR00P,                                                0063 00327000
*           GPR14P,                                                0063 00328000
*           GPR15P)UNRESTRICTED;                                   0063 00329000
*                                                                  0063 00330000
*         /***********************************************************/ 00331000
*         /*                                                         */ 00332000
*         /* LOOP THROUGH CDBS                                       */ 00333000
*         /*                                                         */ 00334000
*         /***********************************************************/ 00335000
*                                                                  0064 00336000
*         DO ECCDBPTR=ECCPCDBQ BY LENGTH(ECCDB)TO ECCPCDBQ+(ECCPCNUM-1) 00337000
*               *LENGTH(ECCDB);                                    0064 00338000
         L     ECCDBPTR,ECCPCDBQ(,ECCPEPTR)                        0064 00339000
         B     @DE00064                                            0064 00340000
@DL00064 DS    0H                                                  0065 00341000
*           IF ECCDVALD='0'B THEN                                  0065 00342000
         TM    ECCDVALD(ECCDBPTR),B'00000100'                      0065 00343000
         BZ    @RT00065                                            0065 00344000
*             GO TO ECLAB3;         /* SKIP DATA COLLECTION IF     0066 00345000
*                                      INVALID CHANNEL ENTRY         */ 00346000
*                                                                  0066 00347000
*           /*********************************************************/ 00348000
*           /*                                                       */ 00349000
*           /* MACDATE Y-2 73018                                     */ 00350000
*           /*                                                       */ 00351000
*           /*********************************************************/ 00352000
*                                                                  0067 00353000
*           RESPECIFY                                              0067 00354000
*            (ECWRKRG1,                                            0067 00355000
*             ECWRKRG2,                                            0067 00356000
*             CATPTR)RESTRICTED;                                   0067 00357000
*                                                                  0067 00358000
*           /*********************************************************/ 00359000
*           /*                                                       */ 00360000
*           /* ESTABLISH ADDRESSABILITY TO CAT ENTRY                 */ 00361000
*           /*                                                       */ 00362000
*           /*********************************************************/ 00363000
*                                                                  0068 00364000
*           CATPTR=ADDR(PCCACAT)+((ECCDBPTR-ECCPCDBQ)/LENGTH(ECCDB))*   00365000
*               LENGTH(CAT);                                       0068 00366000
**       LA    CATPTR,PCCACAT(,PCCAPTR)                        @ZP60030 00367000
         L     CATPTR,CVTPTR                                   @ZP60030 00367010
         L     CATPTR,1052(,CATPTR) <=CVTCST                   @ZP60030 00367020
         L     CATPTR,0(,CATPTR)                               @ZP60030 00367030
         LR    @10,ECCDBPTR                                        0068 00368000
         S     @10,ECCPCDBQ(,ECCPEPTR)                             0068 00369000
         SRDA  @10,32                                              0068 00370000
         D     @10,@CF01349                                        0068 00371000
         SLA   @11,4                   WAS 3                   @ZP60068 00372000
         AR    CATPTR,@11                                          0068 00373000
*           ECWRKRG1=CATSIOCT;      /* GET CURRENT SIO READING       */ 00374000
*                                                                  0069 00375000
         SR    ECWRKRG1,ECWRKRG1                                   0069 00376000
         ICM   ECWRKRG1,3,CATSIOCT(CATPTR)                         0069 00377000
*           /*********************************************************/ 00378000
*           /*                                                       */ 00379000
*           /* GET LAST SIO READING                                  */ 00380000
*           /*                                                       */ 00381000
*           /*********************************************************/ 00382000
*                                                                  0070 00383000
*           ECWRKRG2=ECCDLSIO;                                     0070 00384000
*                                                                  0070 00385000
         SR    ECWRKRG2,ECWRKRG2                                   0070 00386000
         ICM   ECWRKRG2,3,ECCDLSIO(ECCDBPTR)                       0070 00387000
*           /*********************************************************/ 00388000
*           /*                                                       */ 00389000
*           /* SAVE NEW READING                                      */ 00390000
*           /*                                                       */ 00391000
*           /*********************************************************/ 00392000
*                                                                  0071 00393000
*           ECCDLSIO=ECWRKRG1;                                     0071 00394000
*                                                                  0071 00395000
         STH   ECWRKRG1,ECCDLSIO(,ECCDBPTR)                        0071 00396000
*           /*********************************************************/ 00397000
*           /*                                                       */ 00398000
*           /* SUBTRACT OLD SIO COUNT FROM NEW OBSERVATION           */ 00399000
*           /*                                                       */ 00400000
*           /*********************************************************/ 00401000
*                                                                  0072 00402000
*           ECWRKRG1=ECWRKRG1-ECWRKRG2;                            0072 00403000
*                                                                  0072 00404000
         SR    ECWRKRG1,ECWRKRG2                                   0072 00405000
*           /*********************************************************/ 00406000
*           /*                                                       */ 00407000
*           /* COMPENSATE FOR POSSIBLE WRAPAROUND                    */ 00408000
*           /*                                                       */ 00409000
*           /*********************************************************/ 00410000
*                                                                  0073 00411000
*           ECWRKRG1=ECWRKRG1&ECKILLHI;                            0073 00412000
*                                                                  0073 00413000
         N     ECWRKRG1,@CF01329                                   0073 00414000
*           /*********************************************************/ 00415000
*           /*                                                       */ 00416000
*           /* ADD ACCUMULATOR TO SIOS SINCE LAST SAMPLE             */ 00417000
*           /*                                                       */ 00418000
*           /*********************************************************/ 00419000
*                                                                  0074 00420000
*           ECWRKRG1=ECWRKRG1+ECCDSIOS;                            0074 00421000
*                                                                  0074 00422000
         AL    ECWRKRG1,ECCDSIOS(,ECCDBPTR)                        0074 00423000
*           /*********************************************************/ 00424000
*           /*                                                       */ 00425000
*           /* STORE NEW ACCUMULATED SIOS                            */ 00426000
*           /*                                                       */ 00427000
*           /*********************************************************/ 00428000
*                                                                  0075 00429000
*           ECCDSIOS=ECWRKRG1;                                     0075 00430000
*                                                                  0075 00431000
         ST    ECWRKRG1,ECCDSIOS(,ECCDBPTR)                        0075 00432000
*           /*********************************************************/ 00433000
*           /*                                                       */ 00434000
*           /* MACDATE Y-2 73018                                     */ 00435000
*           /*                                                       */ 00436000
*           /*********************************************************/ 00437000
*                                                                  0076 00438000
*           RESPECIFY                                              0076 00439000
*            (ECWRKRG1,                                            0076 00440000
*             ECWRKRG2,                                            0076 00441000
*             CATPTR)UNRESTRICTED;                                 0076 00442000
*ECLAB3:                            /* END OF SIO DATA COLLECTION FOR   00443000
*                                      A CHANNEL ON NOT - CURRENT CPU*/ 00444000
*         END;                                                     0077 00445000
ECLAB3   AH    ECCDBPTR,@CH01349                                   0077 00446000
@DE00064 LH    @12,ECCPCNUM(,ECCPEPTR)                             0077 00447000
         BCTR  @12,0                                               0077 00448000
         MH    @12,@CH01349                                        0077 00449000
         A     @12,ECCPCDBQ(,ECCPEPTR)                             0077 00450000
         CR    ECCDBPTR,@12                                        0077 00451000
         BNH   @DL00064                                            0077 00452000
*       END;                                                       0078 00453000
*                                                                  0078 00454000
*     /***************************************************************/ 00455000
*     /*                                                             */ 00456000
*     /* MACDATE Y-2 73018                                           */ 00457000
*     /*                                                             */ 00458000
*     /***************************************************************/ 00459000
*                                                                  0079 00460000
*     RESPECIFY                                                    0079 00461000
*      (PCCAPTR)UNRESTRICTED;                                      0079 00462000
@RC00029 DS    0H                                                  0080 00463000
*ECLAB4:                            /* END OF ALL DATA COLLECTION FOR   00464000
*                                      ANY CPU                       */ 00465000
*   END;                                                           0080 00466000
*                                                                  0080 00467000
ECLAB4   AH    ECCPEPTR,@CH00040                                   0080 00468000
@DE00024 SR    @12,@12                                             0080 00469000
         IC    @12,ECCECPUS(,ECCEDPTR)                             0080 00470000
         BCTR  @12,0                                               0080 00471000
         SLA   @12,3                                               0080 00472000
         A     @12,ECCECPEQ(,ECCEDPTR)                             0080 00473000
         CR    ECCPEPTR,@12                                        0080 00474000
         BNH   @DL00024                                            0080 00475000
*   /*****************************************************************/ 00476000
*   /*                                                               */ 00477000
*   /* MACDATE Y-2 73018                                             */ 00478000
*   /*                                                               */ 00479000
*   /*****************************************************************/ 00480000
*                                                                  0081 00481000
*   RESPECIFY                                                      0081 00482000
*    (ECCPEPTR,                                                    0081 00483000
*     ECCDBPTR)UNRESTRICTED;                                       0081 00484000
*                                                                  0081 00485000
*   /*****************************************************************/ 00486000
*   /*                                                               */ 00487000
*   /* CHECK TO SEE IF IT IS TIME FOR A CONFIGURATION CHECK. IF SO,  */ 00488000
*   /* CHECK EACH CHANNELS CONFIGURATION. IF ANOTHER CPU HAS COME    */ 00489000
*   /* ONLINE, INITIALIZE ITS CHANNEL DATA BLOCK. LOOP THRU CPES     */ 00490000
*   /*                                                               */ 00491000
*   /*****************************************************************/ 00492000
*                                                                  0082 00493000
*   IF ECCESAMP//ECCECCHK^=0 THEN   /* IF NOT TIME FOR CONFIGURATION    00494000
*                                      CHECK                         */ 00495000
         L     @03,ECCESAMP(,ECCEDPTR)                             0082 00496000
         SR    @02,@02                                             0082 00497000
         D     @02,ECCECCHK(,ECCEDPTR)                             0082 00498000
         LTR   @02,@02                                             0082 00499000
         BNZ   @RT00082                                            0082 00500000
*     RETURN;                       /* RETURN TO IRBMFEVT, MF/1 EVENT   00501000
*                                      ROUTER PROGRAM                */ 00502000
*                                                                  0083 00503000
*   /*****************************************************************/ 00504000
*   /*                                                               */ 00505000
*   /* MACDATE Y-2 73018                                             */ 00506000
*   /*                                                               */ 00507000
*   /*****************************************************************/ 00508000
*                                                                  0084 00509000
*   RESPECIFY                                                      0084 00510000
*    (ECCPEPTR,                                                    0084 00511000
*     ECCPUMSK)RESTRICTED;                                         0084 00512000
*   ECCPUMSK=ECCPUZER;              /* MASK FOR CPU 0                */ 00513000
         L     ECCPUMSK,@CF01327                                   0085 00514000
*   DO ECCPEPTR=ECCECPEQ BY LENGTH(ECCPE)TO ECCECPEQ+(ECCECPUS-1)* 0086 00515000
*         LENGTH(ECCPE);                                           0086 00516000
*                                                                  0086 00517000
         L     ECCPEPTR,ECCECPEQ(,ECCEDPTR)                        0086 00518000
         B     @DE00086                                            0086 00519000
@DL00086 DS    0H                                                  0087 00520000
*     /***************************************************************/ 00521000
*     /*                                                             */ 00522000
*     /* PROVIDE ADDRESSABILITY TO PCCA MACDATE Y-2 73018            */ 00523000
*     /*                                                             */ 00524000
*     /***************************************************************/ 00525000
*                                                                  0087 00526000
*     RESPECIFY                                                    0087 00527000
*      (PCCAPTR)RESTRICTED;                                        0087 00528000
*     PCCAPTR=PCCAT00P(((ECCPEPTR-ECCECPEQ)/LENGTH(ECCPE))+1);     0088 00529000
*                                                                  0088 00530000
         LR    PCCAPTR,ECCPEPTR                                    0088 00531000
         S     PCCAPTR,ECCECPEQ(,ECCEDPTR)                         0088 00532000
         SRDA  PCCAPTR,32                                          0088 00533000
         D     PCCAPTR,@CF00040                                    0088 00534000
         SLA   @05,2                                               0088 00535000
         L     @12,CVTPTR                                          0088 00536000
         L     @03,CVTPCCAT(,@12)                                  0088 00537000
         L     PCCAPTR,PCCAT00P(@05,@03)                           0088 00538000
*     /***************************************************************/ 00539000
*     /*                                                             */ 00540000
*     /* IF THIS CPU IS ONLINE AND THE MF/1 CONTROL BLOCK IS NOT     */ 00541000
*     /* INITIALIZED                                                 */ 00542000
*     /*                                                             */ 00543000
*     /***************************************************************/ 00544000
*                                                                  0089 00545000
*     IF(CSDCPUAL&ECCPUMSK)^=0&ECCPVALD='0'B THEN                  0089 00546000
         L     @12,CVTCSD(,@12)                                    0089 00547000
         LR    @05,ECCPUMSK                                        0089 00548000
         SR    @03,@03                                             0089 00549000
         ICM   @03,3,CSDCPUAL(@12)                                 0089 00550000
         NR    @05,@03                                             0089 00551000
         LTR   @05,@05                                             0089 00552000
         BZ    @RF00089                                            0089 00553000
         TM    ECCPVALD(ECCPEPTR),B'00000001'                      0089 00554000
         BNZ   @RF00089                                            0089 00555000
*       ECCPVALD='1'B;              /* MARK CPU VALID                */ 00556000
         OI    ECCPVALD(ECCPEPTR),B'00000001'                      0090 00557000
*     IF ECCPVALD='0'B THEN                                        0091 00558000
@RF00089 TM    ECCPVALD(ECCPEPTR),B'00000001'                      0091 00559000
         BZ    @RT00091                                            0091 00560000
*       GO TO ECLAB8;               /* IF CPU ENTRY IS INVALID       */ 00561000
*                                                                  0092 00562000
*     /***************************************************************/ 00563000
*     /*                                                             */ 00564000
*     /* LOOP THROUGH CDB ENTRIES FOR THIS CPE MACDATE Y-2 73018     */ 00565000
*     /*                                                             */ 00566000
*     /***************************************************************/ 00567000
*                                                                  0093 00568000
*     RESPECIFY                                                    0093 00569000
*      (ECCDBPTR)RESTRICTED;                                       0093 00570000
*     DO ECCDBPTR=ECCPCDBQ BY LENGTH(ECCDB)TO ECCPCDBQ+(ECCPCNUM-1)*    00571000
*           LENGTH(ECCDB);                                         0094 00572000
*                                                                  0094 00573000
         L     ECCDBPTR,ECCPCDBQ(,ECCPEPTR)                        0094 00574000
         B     @DE00094                                            0094 00575000
@DL00094 DS    0H                                                  0095 00576000
*       /*************************************************************/ 00577000
*       /*                                                           */ 00578000
*       /* PROVIDE ADDRESSABILITY TO CAT ENTRY MACDATE Y-2 73018     */ 00579000
*       /*                                                           */ 00580000
*       /*************************************************************/ 00581000
*                                                                  0095 00582000
*       RESPECIFY                                                  0095 00583000
*        (CATPTR)RESTRICTED;                                       0095 00584000
*       CATPTR=ECCDBPTR-ECCPCDBQ;                                  0096 00585000
         LR    CATPTR,ECCDBPTR                                     0096 00586000
         S     CATPTR,ECCPCDBQ(,ECCPEPTR)                          0096 00587000
*       CATPTR=CATPTR/LENGTH(ECCDB);                               0097 00588000
         LR    @07,CATPTR                                          0097 00589000
         SR    @06,@06                                             0097 00590000
         D     @06,@CF01349                                        0097 00591000
         LR    CATPTR,@07                                          0097 00592000
*       CATPTR=CATPTR*LENGTH(CAT);                                 0098 00593000
         SLL   CATPTR,4                WAS 3                   @ZP60030 00594000
*       CATPTR=CATPTR+ADDR(PCCACAT);                               0099 00595000
*                                                                  0099 00596000
**       LA    @12,PCCACAT(,PCCAPTR)                           @ZP60030 00597000
         L     @12,CVTPTR                                      @ZP60030 00597010
         L     @12,1052(,@12) <=CVTCST                         @ZP60030 00597020
         L     @12,0(,@12)                                     @ZP60030 00597030
         AR    CATPTR,@12                                          0099 00598000
*       /*************************************************************/ 00599000
*       /*                                                           */ 00600000
*       /* IF ONLINE FLAGS IN THE CAT AND CDB DO NOT MATCH THEN      */ 00601000
*       /*                                                           */ 00602000
*       /*************************************************************/ 00603000
*                                                                  0100 00604000
*       IF CATNOP='1'B&ECCDALIV='1'B|CATNOP='0'B&ECCDALIV='0'B THEN     00605000
         TM    CATNOP(CATPTR),B'01000000'                          0100 00606000
         BNO   @GL00003                                            0100 00607000
         TM    ECCDALIV(ECCDBPTR),B'00000001'                      0100 00608000
         BO    @RT00100                                            0100 00609000
@GL00003 TM    CATNOP(CATPTR),B'01000000'                          0100 00610000
         BNZ   @RF00100                                            0100 00611000
         TM    ECCDALIV(ECCDBPTR),B'00000001'                      0100 00612000
         BNZ   @RF00100                                            0100 00613000
@RT00100 DS    0H                                                  0101 00614000
*         DO;                                                      0101 00615000
*ECLAB6:                            /* START RECOGNITION OF CPU    0102 00616000
*                                      CONFIGURATION CHANGE          */ 00617000
*                                                                  0102 00618000
*           /*********************************************************/ 00619000
*           /*                                                       */ 00620000
*           /* MACDATE Y-2 73018                                     */ 00621000
*           /*                                                       */ 00622000
*           /*********************************************************/ 00623000
*                                                                  0102 00624000
*           RESPECIFY                                              0102 00625000
*            (ECWRKRG1,                                            0102 00626000
*             ECWRKRG2)RESTRICTED;                                 0102 00627000
ECLAB6   DS    0H                                                  0103 00628000
*           ECWRKRG1=ECCMPSWP;      /* FLAG BYTE TO REGISTER         */ 00629000
         L     ECWRKRG1,ECCMPSWP(,ECCDBPTR)                        0103 00630000
*           ECWRKRG2=ECWRKRG1;      /* COPY TO WORK REGISTER         */ 00631000
*                                                                  0104 00632000
         LR    ECWRKRG2,ECWRKRG1                                   0104 00633000
*           /*********************************************************/ 00634000
*           /*                                                       */ 00635000
*           /* INDICATE CONFIGURATION CHANGED                        */ 00636000
*           /*                                                       */ 00637000
*           /*********************************************************/ 00638000
*                                                                  0105 00639000
*           ECWRKRG2=ECWRKRG2|ECCDCCON;                            0105 00640000
*                                                                  0105 00641000
         O     ECWRKRG2,@CF00079                                   0105 00642000
*           /*********************************************************/ 00643000
*           /*                                                       */ 00644000
*           /* SET CURRENT STATUS INTO CDB                           */ 00645000
*           /*                                                       */ 00646000
*           /*********************************************************/ 00647000
*                                                                  0106 00648000
*           IF CATNOP='1'B THEN                                    0106 00649000
*                                                                  0106 00650000
         TM    CATNOP(CATPTR),B'01000000'                          0106 00651000
         BNO   @RF00106                                            0106 00652000
*             /*******************************************************/ 00653000
*             /*                                                     */ 00654000
*             /* OFFLINE                                             */ 00655000
*             /*                                                     */ 00656000
*             /*******************************************************/ 00657000
*                                                                  0107 00658000
*             ECWRKRG2=ECWRKRG2&ECCDALOF;                          0107 00659000
         N     ECWRKRG2,@CF00085                                   0107 00660000
*           ELSE                                                   0108 00661000
*             DO;                                                  0108 00662000
*                                                                  0108 00663000
         B     @RC00106                                            0108 00664000
@RF00106 DS    0H                                                  0109 00665000
*               /*****************************************************/ 00666000
*               /*                                                   */ 00667000
*               /* ONLINE                                            */ 00668000
*               /*                                                   */ 00669000
*               /*****************************************************/ 00670000
*                                                                  0109 00671000
*               ECWRKRG2=ECWRKRG2|ECCDALON;                        0109 00672000
*                                                                  0109 00673000
         O     ECWRKRG2,@CF00083                                   0109 00674000
*               /*****************************************************/ 00675000
*               /*                                                   */ 00676000
*               /* NEW SIO COUNT                                     */ 00677000
*               /*                                                   */ 00678000
*               /*****************************************************/ 00679000
*                                                                  0110 00680000
*               ECCDLSIO=CATSIOCT;                                 0110 00681000
*                                                                  0110 00682000
         MVC   ECCDLSIO(2,ECCDBPTR),CATSIOCT(CATPTR)               0110 00683000
*               /*****************************************************/ 00684000
*               /*                                                   */ 00685000
*               /* SET CHANNEL TYPE                                  */ 00686000
*               /*                                                   */ 00687000
*               /*****************************************************/ 00688000
*                                                                  0111 00689000
*               ECCDCHID=CATCHID(1:2);                             0111 00690000
*                                                                  0111 00691000
         MVC   ECCDCHID(2,ECCDBPTR),CATCHID(CATPTR)                0111 00692000
*               /*****************************************************/ 00693000
*               /*                                                   */ 00694000
*               /* SET CHANNEL TYPE INVALID FLAG, IF NECESSARY       */ 00695000
*               /*                                                   */ 00696000
*               /*****************************************************/ 00697000
*                                                                  0112 00698000
*               IF CATNID='1'B THEN                                0112 00699000
         TM    CATNID(CATPTR),B'00001000'                          0112 00700000
         BNO   @RF00112                                            0112 00701000
*                 ECCDIVID='1'B;    /* THIS WILL CAUSE RECURSION ON     00702000
*                                      THE COMPARE AND SWAP          */ 00703000
*                                                                  0113 00704000
         OI    ECCDIVID(ECCDBPTR),B'00001000'                      0113 00705000
*               /*****************************************************/ 00706000
*               /*                                                   */ 00707000
*               /* MARK VALID                                        */ 00708000
*               /*                                                   */ 00709000
*               /*****************************************************/ 00710000
*                                                                  0114 00711000
*               ECWRKRG2=ECWRKRG2|ECCDVAON;                        0114 00712000
@RF00112 O     ECWRKRG2,@CF00075                                   0114 00713000
*             END;                                                 0115 00714000
*           CS(ECWRKRG1,ECWRKRG2,ECCMPSWP);                        0116 00715000
@RC00106 CS    ECWRKRG1,ECWRKRG2,ECCMPSWP(ECCDBPTR)                0116 00716000
*           BC(7,ECLAB6);           /* BRANCH IF UNSUCCESSFUL        */ 00717000
*                                                                  0117 00718000
         BC    7,ECLAB6                                            0117 00719000
*           /*********************************************************/ 00720000
*           /*                                                       */ 00721000
*           /* MACDATE Y-2 73018                                     */ 00722000
*           /*                                                       */ 00723000
*           /*********************************************************/ 00724000
*                                                                  0118 00725000
*           RESPECIFY                                              0118 00726000
*            (ECWRKRG1,                                            0118 00727000
*             ECWRKRG2)UNRESTRICTED;                               0118 00728000
*         END;                                                     0119 00729000
*                                                                  0119 00730000
*       /*************************************************************/ 00731000
*       /*                                                           */ 00732000
*       /* MACDATE Y-2 73018                                         */ 00733000
*       /*                                                           */ 00734000
*       /*************************************************************/ 00735000
*                                                                  0120 00736000
*       RESPECIFY                                                  0120 00737000
*        (CATPTR)UNRESTRICTED;                                     0120 00738000
@RF00100 DS    0H                                                  0121 00739000
*     END;                                                         0121 00740000
*                                                                  0121 00741000
         AH    ECCDBPTR,@CH01349                                   0121 00742000
@DE00094 LH    @12,ECCPCNUM(,ECCPEPTR)                             0121 00743000
         BCTR  @12,0                                               0121 00744000
         MH    @12,@CH01349                                        0121 00745000
         A     @12,ECCPCDBQ(,ECCPEPTR)                             0121 00746000
         CR    ECCDBPTR,@12                                        0121 00747000
         BNH   @DL00094                                            0121 00748000
*     /***************************************************************/ 00749000
*     /*                                                             */ 00750000
*     /* MACDATE Y-2 73018                                           */ 00751000
*     /*                                                             */ 00752000
*     /***************************************************************/ 00753000
*                                                                  0122 00754000
*     RESPECIFY                                                    0122 00755000
*      (ECCDBPTR,                                                  0122 00756000
*       PCCAPTR)UNRESTRICTED;                                      0122 00757000
*ECLAB8:                                                           0123 00758000
*     ECCPUMSK=ECCPUMSK/2;          /* SHIFT MASK RIGHT FOR NEXT CPU */ 00759000
ECLAB8   SRL   ECCPUMSK,1                                          0123 00760000
*   END;                                                           0124 00761000
*                                                                  0124 00762000
         AH    ECCPEPTR,@CH00040                                   0124 00763000
@DE00086 SR    @12,@12                                             0124 00764000
         IC    @12,ECCECPUS(,ECCEDPTR)                             0124 00765000
         BCTR  @12,0                                               0124 00766000
         SLA   @12,3                                               0124 00767000
         A     @12,ECCECPEQ(,ECCEDPTR)                             0124 00768000
         CR    ECCPEPTR,@12                                        0124 00769000
         BNH   @DL00086                                            0124 00770000
*   /*****************************************************************/ 00771000
*   /*                                                               */ 00772000
*   /* MACDATE Y-2 73018                                             */ 00773000
*   /*                                                               */ 00774000
*   /*****************************************************************/ 00775000
*                                                                  0125 00776000
*   RESPECIFY                                                      0125 00777000
*    (ECCPEPTR,                                                    0125 00778000
*     ECCPUMSK)UNRESTRICTED;                                       0125 00779000
*   RETURN;                         /* RETURN TO MFROUTER, IRBMFEVT  */ 00780000
@EL00001 DS    0H                                                  0126 00781000
@EF00001 DS    0H                                                  0126 00782000
@ER00001 LM    @14,@12,12(@13)                                     0126 00783000
         BR    @14                                                 0126 00784000
*   END                                                            0127 00785000
*                                                                  0127 00786000
*/* THE FOLLOWING INCLUDE STATEMENTS WERE FOUND IN THIS PROGRAM.     */ 00787000
*/*%INCLUDE SYSLIB  (CVT     )                                       */ 00788000
*/*%INCLUDE SYSLIB  (IHAPCCAT)                                       */ 00789000
*/*%INCLUDE SYSLIB  (IHAPCCA )                                       */ 00790000
*/*%INCLUDE SYSLIB  (IECDCAT )                                       */ 00791000
*/*%INCLUDE SYSLIB  (IHAPSA  )                                       */ 00792000
*/*%INCLUDE SYSLIB  (IHACSD  )                                       */ 00793000
*                                                                  0127 00794000
*       ;                                                          0127 00795000
         B     @EL00001                                            0127 00796000
@DATA    DS    0H                                                       00797000
@DATD    DSECT                                                          00798000
         DS    0F                                                       00799000
IRBMFECH CSECT                                                          00800000
         DS    0F                                                       00801000
@CF00040 DC    F'8'                                                     00802000
@CH00040 EQU   @CF00040+2                                               00803000
@CF01349 DC    F'20'                                                    00804000
@CH01349 EQU   @CF01349+2                                               00805000
@CF01327 DC    XL4'8000'                                                00806000
@CF00075 DC    XL4'04000000'                                            00807000
@CF00079 DC    XL4'02000000'                                            00808000
@CF00083 DC    XL4'01000000'                                            00809000
@CF00085 DC    XL4'FEFFFFFF'                                            00810000
@CF01329 DC    XL4'0000FFFF'                                            00811000
@DATD    DSECT                                                          00812000
         DS    0D                                                       00813000
@ENDDATD EQU   *                                                        00814000
IRBMFECH CSECT                                                          00815000
         NOPR  ((@ENDDATD-@DATD)*16)                                    00816000
         DS    0F                                                       00817000
@SIZDATD DC    AL1(0)                                                   00818000
         DC    AL3(@ENDDATD-@DATD)                                      00819000
         DS    0D                                                       00820000
ECPATCH  DS    CL100                                                    00821000
@00      EQU   00                      EQUATES FOR REGISTERS 0-15       00822000
@01      EQU   01                                                       00823000
@02      EQU   02                                                       00824000
@03      EQU   03                                                       00825000
@04      EQU   04                                                       00826000
@05      EQU   05                                                       00827000
@06      EQU   06                                                       00828000
@07      EQU   07                                                       00829000
@08      EQU   08                                                       00830000
@09      EQU   09                                                       00831000
@10      EQU   10                                                       00832000
@11      EQU   11                                                       00833000
@12      EQU   12                                                       00834000
@13      EQU   13                                                       00835000
@14      EQU   14                                                       00836000
@15      EQU   15                                                       00837000
GPR09P   EQU   @09                                                      00838000
ECCEDPTR EQU   @01                                                      00839000
ECCPEPTR EQU   @02                                                      00840000
ECCDBPTR EQU   @03                                                      00841000
GPR00P   EQU   @00                                                      00842000
GPR01P   EQU   @01                                                      00843000
GPR14P   EQU   @14                                                      00844000
GPR15P   EQU   @15                                                      00845000
PCCAPTR  EQU   @04                                                      00846000
CATPTR   EQU   @05                                                      00847000
ECCPUMSK EQU   @08                                                      00848000
ECWRKRG1 EQU   @06                                                      00849000
ECWRKRG2 EQU   @07                                                      00850000
GPR00F   EQU   @00                                                      00851000
GPR01F   EQU   @01                                                      00852000
GPR14F   EQU   @14                                                      00853000
GPR15F   EQU   @15                                                      00854000
CVTPTR   EQU   16                                                       00855000
PSA      EQU   0                                                        00856000
FLCRNPSW EQU   PSA                                                      00857000
FLCROPSW EQU   PSA+8                                                    00858000
FLCICCW2 EQU   PSA+16                                                   00859000
PSAEEPSW EQU   PSA+132                                                  00860000
PSAESPSW EQU   PSA+136                                                  00861000
FLCSVILC EQU   PSAESPSW+1                                               00862000
PSAEPPSW EQU   PSA+140                                                  00863000
FLCPIILC EQU   PSAEPPSW+1                                               00864000
FLCPICOD EQU   PSAEPPSW+2                                               00865000
PSAPICOD EQU   FLCPICOD+1                                               00866000
FLCTEA   EQU   PSAEPPSW+4                                               00867000
FLCPER   EQU   PSA+152                                                  00868000
FLCMCLA  EQU   PSA+168                                                  00869000
FLCIOEL  EQU   FLCMCLA+4                                                00870000
FLCIOA   EQU   FLCMCLA+16                                               00871000
FLCFSA   EQU   FLCMCLA+80                                               00872000
PSATOLD  EQU   PSA+540                                                  00873000
PSASUPER EQU   PSA+552                                                  00874000
PSASUP1  EQU   PSASUPER                                                 00875000
PSASUP2  EQU   PSASUPER+1                                               00876000
PSASUP3  EQU   PSASUPER+2                                               00877000
PSASUP4  EQU   PSASUPER+3                                               00878000
PSACLHT  EQU   PSA+640                                                  00879000
PSALKSA  EQU   PSA+696                                                  00880000
PSAHLHI  EQU   PSA+760                                                  00881000
PSACPUSA EQU   PSA+794                                                  00882000
PSADSSFL EQU   PSA+836                                                  00883000
PSADSSF1 EQU   PSADSSFL                                                 00884000
PSADSSF3 EQU   PSADSSFL+2                                               00885000
PSADSSF4 EQU   PSADSSFL+3                                               00886000
PSARSVT  EQU   PSA+896                                                  00887000
ECCEDT   EQU   0                                                        00888000
ECCECPEQ EQU   ECCEDT                                                   00889000
ECCECPUS EQU   ECCEDT+7                                                 00890000
ECCESAMP EQU   ECCEDT+8                                                 00891000
ECCECCHK EQU   ECCEDT+12                                                00892000
ECCPE    EQU   0                                                        00893000
ECCPFLGS EQU   ECCPE                                                    00894000
ECCPVALD EQU   ECCPFLGS                                                 00895000
ECCPCNUM EQU   ECCPE+2                                                  00896000
ECCPCDBQ EQU   ECCPE+4                                                  00897000
ECCDB    EQU   0                                                        00898000
ECCDFLGS EQU   ECCDB                                                    00899000
ECCDIVID EQU   ECCDFLGS                                                 00900000
ECCDVALD EQU   ECCDFLGS                                                 00901000
ECCDALIV EQU   ECCDFLGS                                                 00902000
ECCDCHID EQU   ECCDB+4                                                  00903000
ECCDCTYP EQU   ECCDCHID                                                 00904000
ECCDLSIO EQU   ECCDB+6                                                  00905000
ECCDSIOS EQU   ECCDB+8                                                  00906000
ECCDBUSY EQU   ECCDB+12                                                 00907000
ECCDOLAP EQU   ECCDB+16                                                 00908000
CVTMAP   EQU   0                                                        00909000
CVTDAR   EQU   CVTMAP+72                                                00910000
CVTFLGS1 EQU   CVTDAR                                                   00911000
CVTDCB   EQU   CVTMAP+116                                               00912000
CVTIERLC EQU   CVTMAP+144                                               00913000
CVTOPTA  EQU   CVTMAP+182                                               00914000
CVTOPTB  EQU   CVTMAP+183                                               00915000
CVTGTF   EQU   CVTMAP+236                                               00916000
CVTGTFST EQU   CVTGTF                                                   00917000
CVTGTFS  EQU   CVTGTFST                                                 00918000
CVTSTATE EQU   CVTGTFST                                                 00919000
CVTTMODE EQU   CVTGTFST                                                 00920000
CVTFORM  EQU   CVTGTFST                                                 00921000
CVTUSR   EQU   CVTGTFST                                                 00922000
CVTAQAVT EQU   CVTMAP+240                                               00923000
CVTTCMFG EQU   CVTAQAVT                                                 00924000
CVTVOLM2 EQU   CVTMAP+244                                               00925000
CVTTATA  EQU   CVTVOLM2                                                 00926000
CVTTSKS  EQU   CVTTATA                                                  00927000
CVTVOLF2 EQU   CVTTSKS                                                  00928000
CVTTAT   EQU   CVTTATA+1                                                00929000
CVTATER  EQU   CVTMAP+248                                               00930000
CVTEXT1  EQU   CVTMAP+252                                               00931000
CVTPURG  EQU   CVTMAP+260                                               00932000
CVTQMSG  EQU   CVTMAP+268                                               00933000
CVTDMSR  EQU   CVTMAP+272                                               00934000
CVTERPV  EQU   CVTMAP+316                                               00935000
CVTINTLA EQU   CVTMAP+320                                               00936000
CVTAPF   EQU   CVTMAP+324                                               00937000
CVTEXT2  EQU   CVTMAP+328                                               00938000
CVTHJES  EQU   CVTMAP+332                                               00939000
CVTPGSIA EQU   CVTMAP+348                                               00940000
CVTA1F1  EQU   CVTMAP+356                                               00941000
CVTSYSK  EQU   CVTMAP+357                                               00942000
CVTVOLM1 EQU   CVTMAP+380                                               00943000
CVTVOLF1 EQU   CVTVOLM1                                                 00944000
CVTATMCT EQU   CVTMAP+388                                               00945000
CVTXTNT1 EQU   0                                                        00946000
CVTXTNT2 EQU   0                                                        00947000
CVTDSSV  EQU   CVTXTNT2                                                 00948000
CVTRSV90 EQU   CVTXTNT2+5                                               00949000
CVTQID   EQU   CVTXTNT2+24                                              00950000
CVTRV400 EQU   CVTXTNT2+52                                              00951000
CVTRV409 EQU   CVTXTNT2+53                                              00952000
CVTATCVT EQU   CVTXTNT2+64                                              00953000
CVTRV429 EQU   CVTXTNT2+84                                              00954000
CVTRV438 EQU   CVTXTNT2+85                                              00955000
CVTRV457 EQU   CVTXTNT2+112                                             00956000
CVTRV466 EQU   CVTXTNT2+113                                             00957000
CVTFIX   EQU   0                                                        00958000
CVTRELNO EQU   CVTFIX+252                                               00959000
PCCAVT   EQU   0                                                        00960000
PCCAT00P EQU   PCCAVT                                                   00961000
PCCA     EQU   0                                                        00962000
PCCATMST EQU   PCCA+128                                                 00963000
PCCATMFL EQU   PCCATMST                                                 00964000
PCCATODE EQU   PCCATMST+1                                               00965000
PCCACCE  EQU   PCCATMST+2                                               00966000
PCCAINTE EQU   PCCATMST+3                                               00967000
PCCAEMSB EQU   PCCA+136                                                 00968000
PCCAEMSI EQU   PCCAEMSB                                                 00969000
PCCARISP EQU   PCCAEMSI                                                 00970000
PCCAEMS2 EQU   PCCAEMSI+1                                               00971000
PCCAEMS3 EQU   PCCAEMSI+2                                               00972000
PCCARMSB EQU   PCCAEMSI+3                                               00973000
PCCAWERP EQU   PCCA+280                                                 00974000
PCCACHPF EQU   PCCAWERP+4                                               00975000
PCCACHBL EQU   PCCAWERP+5                                               00976000
PCCACHVA EQU   PCCAWERP+6                                               00977000
PCCACHTS EQU   PCCAWERP+7                                               00978000
PCCACHS1 EQU   PCCA+288                                                 00979000
PCCACHS2 EQU   PCCA+289                                                 00980000
PCCACHRB EQU   PCCA+290                                                 00981000
PCCACHF1 EQU   PCCA+308                                                 00982000
PCCACHF2 EQU   PCCA+309                                                 00983000
PCCACHF3 EQU   PCCA+310                                                 00984000
PCCACHF4 EQU   PCCA+311                                                 00985000
PCCACAT  EQU   PCCA+384                                                 00986000
CAT      EQU   0                                                        00987000
CATENTRY EQU   CAT                                                      00988000
CATFLG   EQU   CATENTRY                                                 00989000
CATNOP   EQU   CATFLG                                                   00990000
CATNID   EQU   CATFLG                                                   00991000
CATFLA   EQU   CAT+1                                                    00992000
CATSIOCT EQU   CAT+2                                                    00993000
CATCHID  EQU   CAT+4                                                    00994000
CSD      EQU   0                                                        00995000
CSDCPUAL EQU   CSD+8                                                    00996000
CSDSCWRD EQU   CSD+12                                                   00997000
CSDSCFL1 EQU   CSDSCWRD                                                 00998000
CSDSCFL2 EQU   CSDSCWRD+1                                               00999000
CSDSCFL3 EQU   CSDSCWRD+2                                               01000000
CSDSCFL4 EQU   CSDSCWRD+3                                               01001000
CSDFLAGS EQU   CSD+23                                                   01002000
IKEBC    EQU   0                                                        01003000
IKEBF15  EQU   0                                                        01004000
IKEBF31  EQU   0                                                        01005000
IKEBP15  EQU   0                                                        01006000
IKEBP31  EQU   0                                                        01007000
IKEBP8   EQU   0                                                        01008000
CVTS01   EQU   CVTPGSIA                                                 01009000
CVTLPDIA EQU   CVTS01+12                                                01010000
CVTDIRST EQU   CVTLPDIA                                                 01011000
CVTSLIDA EQU   CVTS01+24                                                01012000
CVTCTLFG EQU   CVTS01+50                                                01013000
CVTCSD   EQU   CVTS01+312                                               01014000
CVTPCCAT EQU   CVTS01+416                                               01015000
CVTRV210 EQU   CVTS01+424                                               01016000
CVTRV219 EQU   CVTS01+425                                               01017000
CVTRV228 EQU   CVTS01+426                                               01018000
CVTRV237 EQU   CVTS01+427                                               01019000
CVTMFRTR EQU   CVTS01+452                                               01020000
CVTRV262 EQU   CVTS01+468                                               01021000
CVTRV271 EQU   CVTS01+469                                               01022000
CVTRV280 EQU   CVTS01+470                                               01023000
CVTRV289 EQU   CVTS01+471                                               01024000
CVTWTCB  EQU   CVTS01+540                                               01025000
CVTGSDA  EQU   CVTS01+600                                               01026000
PSARSVTE EQU   PSARSVT                                                  01027000
ECCMPSWP EQU   ECCDFLGS                                                 01028000
FLC      EQU   PSA                                                      01029000
*                                      START UNREFERENCED COMPONENTS    01030000
PSARSAV  EQU   PSARSVTE+60                                              01031000
PSARSTK  EQU   PSARSVTE+56                                              01032000
PSAESAV3 EQU   PSARSVTE+52                                              01033000
PSAESTK3 EQU   PSARSVTE+48                                              01034000
PSAESAV2 EQU   PSARSVTE+44                                              01035000
PSAESTK2 EQU   PSARSVTE+40                                              01036000
PSAESAV1 EQU   PSARSVTE+36                                              01037000
PSAESTK1 EQU   PSARSVTE+32                                              01038000
PSAPSAV  EQU   PSARSVTE+28                                              01039000
PSAPSTK  EQU   PSARSVTE+24                                              01040000
PSAMSAV  EQU   PSARSVTE+20                                              01041000
PSAMSTK  EQU   PSARSVTE+16                                              01042000
PSASSAV  EQU   PSARSVTE+12                                              01043000
PSASSTK  EQU   PSARSVTE+8                                               01044000
PSANSTK  EQU   PSARSVTE+4                                               01045000
PSACSTK  EQU   PSARSVTE                                                 01046000
CVTTPIO  EQU   CVTS01+608                                               01047000
CVTADV   EQU   CVTS01+604                                               01048000
CVTGSDAB EQU   CVTGSDA                                                  01049000
CVTQV3   EQU   CVTS01+596                                               01050000
CVTQV2   EQU   CVTS01+592                                               01051000
CVTQV1   EQU   CVTS01+588                                               01052000
CVTRPT   EQU   CVTS01+584                                               01053000
CVTSSRB  EQU   CVTS01+580                                               01054000
CVTCSDRL EQU   CVTS01+576                                               01055000
CVTEXP1  EQU   CVTS01+572                                               01056000
CVTRMPMT EQU   CVTS01+568                                               01057000
CVTRMPTT EQU   CVTS01+564                                               01058000
CVTVPSA  EQU   CVTS01+560                                               01059000
CVTVSTOP EQU   CVTS01+556                                               01060000
CVTGTFR8 EQU   CVTS01+552                                               01061000
CVTQUIT  EQU   CVTS01+548                                               01062000
CVTVACR  EQU   CVTS01+544                                               01063000
CVTSTPRS EQU   CVTS01+536                                               01064000
CVT0PT02 EQU   CVTS01+532                                               01065000
CVTDARCM EQU   CVTS01+528                                               01066000
CVTIRECM EQU   CVTS01+524                                               01067000
CVTJRECM EQU   CVTS01+520                                               01068000
CVTVEMS0 EQU   CVTS01+516                                               01069000
CVTSPFRR EQU   CVTS01+512                                               01070000
CVTRLSTG EQU   CVTS01+508                                               01071000
CVT0TC0A EQU   CVTS01+504                                               01072000
CVTGMBR  EQU   CVTS01+500                                               01073000
CVTLFRM  EQU   CVTS01+496                                               01074000
CVTRMBR  EQU   CVTS01+492                                               01075000
CVTVIOP  EQU   CVTS01+488                                               01076000
CVTRV307 EQU   CVTS01+486                                               01077000
CVTRV306 EQU   CVTS01+484                                               01078000
CVTRV305 EQU   CVTS01+482                                               01079000
CVTRV304 EQU   CVTS01+480                                               01080000
CVTRV303 EQU   CVTS01+478                                               01081000
CVTRV302 EQU   CVTS01+476                                               01082000
CVTRV301 EQU   CVTS01+475                                               01083000
CVTRV300 EQU   CVTS01+474                                               01084000
CVTRV299 EQU   CVTS01+473                                               01085000
CVTRV298 EQU   CVTS01+472                                               01086000
CVTRV297 EQU   CVTRV289                                                 01087000
CVTRV296 EQU   CVTRV289                                                 01088000
CVTRV295 EQU   CVTRV289                                                 01089000
CVTRV294 EQU   CVTRV289                                                 01090000
CVTRV293 EQU   CVTRV289                                                 01091000
CVTRV292 EQU   CVTRV289                                                 01092000
CVTRV291 EQU   CVTRV289                                                 01093000
CVTRV290 EQU   CVTRV289                                                 01094000
CVTRV288 EQU   CVTRV280                                                 01095000
CVTRV287 EQU   CVTRV280                                                 01096000
CVTRV286 EQU   CVTRV280                                                 01097000
CVTRV285 EQU   CVTRV280                                                 01098000
CVTRV284 EQU   CVTRV280                                                 01099000
CVTRV283 EQU   CVTRV280                                                 01100000
CVTRV282 EQU   CVTRV280                                                 01101000
CVTRV281 EQU   CVTRV280                                                 01102000
CVTRV279 EQU   CVTRV271                                                 01103000
CVTRV278 EQU   CVTRV271                                                 01104000
CVTRV277 EQU   CVTRV271                                                 01105000
CVTRV276 EQU   CVTRV271                                                 01106000
CVTRV275 EQU   CVTRV271                                                 01107000
CVTRV274 EQU   CVTRV271                                                 01108000
CVTRV273 EQU   CVTRV271                                                 01109000
CVTRV272 EQU   CVTRV271                                                 01110000
CVTRV270 EQU   CVTRV262                                                 01111000
CVTRV269 EQU   CVTRV262                                                 01112000
CVTRV268 EQU   CVTRV262                                                 01113000
CVTRV267 EQU   CVTRV262                                                 01114000
CVTRV266 EQU   CVTRV262                                                 01115000
CVTRV265 EQU   CVTRV262                                                 01116000
CVTRV264 EQU   CVTRV262                                                 01117000
CVTRV263 EQU   CVTRV262                                                 01118000
CVTVFP   EQU   CVTS01+464                                               01119000
CVTVSI   EQU   CVTS01+460                                               01120000
CVTVPSIB EQU   CVTS01+456                                               01121000
CVTMFACT EQU   CVTMFRTR                                                 01122000
CVTMFCTL EQU   CVTS01+448                                               01123000
CVTPVBP  EQU   CVTS01+444                                               01124000
CVTPWI   EQU   CVTS01+440                                               01125000
CVTRV254 EQU   CVTS01+438                                               01126000
CVTRV253 EQU   CVTS01+436                                               01127000
CVTRV252 EQU   CVTS01+434                                               01128000
CVTRV251 EQU   CVTS01+433                                               01129000
CVTRV250 EQU   CVTS01+432                                               01130000
CVTRV249 EQU   CVTS01+431                                               01131000
CVTRV248 EQU   CVTS01+430                                               01132000
CVTRV247 EQU   CVTS01+429                                               01133000
CVTRV246 EQU   CVTS01+428                                               01134000
CVTRV245 EQU   CVTRV237                                                 01135000
CVTRV244 EQU   CVTRV237                                                 01136000
CVTRV243 EQU   CVTRV237                                                 01137000
CVTRV242 EQU   CVTRV237                                                 01138000
CVTRV241 EQU   CVTRV237                                                 01139000
CVTRV240 EQU   CVTRV237                                                 01140000
CVTRV239 EQU   CVTRV237                                                 01141000
CVTRV238 EQU   CVTRV237                                                 01142000
CVTRV236 EQU   CVTRV228                                                 01143000
CVTRV235 EQU   CVTRV228                                                 01144000
CVTRV234 EQU   CVTRV228                                                 01145000
CVTRV233 EQU   CVTRV228                                                 01146000
CVTRV232 EQU   CVTRV228                                                 01147000
CVTRV231 EQU   CVTRV228                                                 01148000
CVTRV230 EQU   CVTRV228                                                 01149000
CVTRV229 EQU   CVTRV228                                                 01150000
CVTRV227 EQU   CVTRV219                                                 01151000
CVTRV226 EQU   CVTRV219                                                 01152000
CVTRV225 EQU   CVTRV219                                                 01153000
CVTRV224 EQU   CVTRV219                                                 01154000
CVTRV223 EQU   CVTRV219                                                 01155000
CVTRV222 EQU   CVTRV219                                                 01156000
CVTRV221 EQU   CVTRV219                                                 01157000
CVTRV220 EQU   CVTRV219                                                 01158000
CVTRV218 EQU   CVTRV210                                                 01159000
CVTRV217 EQU   CVTRV210                                                 01160000
CVTRV216 EQU   CVTRV210                                                 01161000
CVTRV215 EQU   CVTRV210                                                 01162000
CVTRV214 EQU   CVTRV210                                                 01163000
CVTRV213 EQU   CVTRV210                                                 01164000
CVTRV212 EQU   CVTRV210                                                 01165000
CVTRV211 EQU   CVTRV210                                                 01166000
CVTLCCAT EQU   CVTS01+420                                               01167000
CVTIPCRP EQU   CVTS01+412                                               01168000
CVTIPCRI EQU   CVTS01+408                                               01169000
CVTIPCDS EQU   CVTS01+404                                               01170000
CVTAIDVT EQU   CVTS01+400                                               01171000
CVTSSAP  EQU   CVTS01+396                                               01172000
CVTEHCIR EQU   CVTS01+392                                               01173000
CVTEHDEF EQU   CVTS01+388                                               01174000
CVTDAIR  EQU   CVTS01+384                                               01175000
CVTPERFM EQU   CVTS01+380                                               01176000
CVT044R2 EQU   CVTS01+376                                               01177000
CVTFETCH EQU   CVTS01+372                                               01178000
CVTRSTWD EQU   CVTS01+368                                               01179000
CVTSPOST EQU   CVTS01+364                                               01180000
CVTIOBP  EQU   CVTS01+360                                               01181000
CVTASMVT EQU   CVTS01+356                                               01182000
CVTRECRQ EQU   CVTS01+352                                               01183000
CVTWSAC  EQU   CVTS01+348                                               01184000
CVTRV149 EQU   CVTS01+344                                               01185000
CVTWSAL  EQU   CVTS01+340                                               01186000
CVTSPSA  EQU   CVTS01+336                                               01187000
CVTGLMN  EQU   CVTS01+332                                               01188000
CVTVEAC0 EQU   CVTS01+328                                               01189000
CVT062R1 EQU   CVTS01+324                                               01190000
CVTRPOST EQU   CVTS01+320                                               01191000
CVTDQIQE EQU   CVTS01+316                                               01192000
CVTLKRMA EQU   CVTS01+308                                               01193000
CVTRSPIE EQU   CVTS01+304                                               01194000
CVTRENQ  EQU   CVTS01+300                                               01195000
CVTLQCB  EQU   CVTS01+296                                               01196000
CVTFQCB  EQU   CVTS01+292                                               01197000
CVTQCS01 EQU   CVTS01+288                                               01198000
CVTAPFT  EQU   CVTS01+284                                               01199000
CVTPARRL EQU   CVTS01+280                                               01200000
CVTVWAIT EQU   CVTS01+276                                               01201000
CVTGSPL  EQU   CVTS01+272                                               01202000
CVTLSMQ  EQU   CVTS01+268                                               01203000
CVTGSMQ  EQU   CVTS01+264                                               01204000
CVTEXPRO EQU   CVTS01+260                                               01205000
CVTOPCTP EQU   CVTS01+256                                               01206000
CVTSIC   EQU   CVTS01+252                                               01207000
CVTTPIOS EQU   CVTS01+248                                               01208000
CVTRTMS  EQU   CVTS01+244                                               01209000
CVTSDBF  EQU   CVTS01+240                                               01210000
CVTSCBP  EQU   CVTS01+236                                               01211000
CVTSDMP  EQU   CVTS01+232                                               01212000
CVTSV60  EQU   CVTS01+228                                               01213000
CVTRTMCT EQU   CVTS01+224                                               01214000
CVTASCBL EQU   CVTS01+220                                               01215000
CVTASCBH EQU   CVTS01+216                                               01216000
CVTGDA   EQU   CVTS01+212                                               01217000
CVTASVT  EQU   CVTS01+208                                               01218000
CVTVVMDI EQU   CVTS01+204                                               01219000
CVTAQTOP EQU   CVTS01+200                                               01220000
CVTIOSCS EQU   CVTS01+196                                               01221000
CVTSDRM  EQU   CVTS01+192                                               01222000
CVTOPTE  EQU   CVTS01+188                                               01223000
CVTSTXU  EQU   CVTS01+184                                               01224000
CVTQUIS  EQU   CVTS01+180                                               01225000
CVTPARS  EQU   CVTS01+176                                               01226000
CVTS1EE  EQU   CVTS01+172                                               01227000
CVTFRAS  EQU   CVTS01+168                                               01228000
CVTQSAS  EQU   CVTS01+164                                               01229000
CVTCRAS  EQU   CVTS01+160                                               01230000
CVTCRMN  EQU   CVTS01+156                                               01231000
CVTDELCP EQU   CVTS01+152                                               01232000
CVTFRECL EQU   CVTS01+148                                               01233000
CVTGETCL EQU   CVTS01+144                                               01234000
CVTBLDCP EQU   CVTS01+140                                               01235000
CVTAUTHL EQU   CVTS01+136                                               01236000
CVTSCAN  EQU   CVTS01+132                                               01237000
CVTRV144 EQU   CVTS01+130                                               01238000
CVTMAXMP EQU   CVTS01+128                                               01239000
CVTSTCK  EQU   CVTS01+124                                               01240000
CVTRV139 EQU   CVTS01+123                                               01241000
CVTDSSAC EQU   CVTS01+122                                               01242000
CVTRV513 EQU   CVTS01+121                                               01243000
CVTIOSPL EQU   CVTS01+120                                               01244000
CVTPTGT  EQU   CVTS01+116                                               01245000
CVTCSPIE EQU   CVTS01+112                                               01246000
CVTSMFEX EQU   CVTS01+108                                               01247000
CVTOLT0A EQU   CVTS01+104                                               01248000
CVTSRBRT EQU   CVTS01+100                                               01249000
CVTPUTL  EQU   CVTS01+96                                                01250000
CVTRV519 EQU   CVTS01+92                                                01251000
CVTRV327 EQU   CVTS01+88                                                01252000
CVTRV326 EQU   CVTS01+84                                                01253000
CVTRV325 EQU   CVTS01+80                                                01254000
CVTRV324 EQU   CVTS01+76                                                01255000
CVT0VL01 EQU   CVTS01+72                                                01256000
CVTSHRVM EQU   CVTS01+68                                                01257000
CVTRV332 EQU   CVTS01+64                                                01258000
CVTTAS   EQU   CVTS01+60                                                01259000
CVTRSCN  EQU   CVTS01+56                                                01260000
CVTTRAC2 EQU   CVTS01+54                                                01261000
CVTTRACE EQU   CVTS01+52                                                01262000
CVTAPG   EQU   CVTS01+51                                                01263000
CVTSDTRC EQU   CVTCTLFG                                                 01264000
CVTGTRCE EQU   CVTCTLFG                                                 01265000
CVTNOMP  EQU   CVTCTLFG                                                 01266000
CVTRSV79 EQU   CVTCTLFG                                                 01267000
CVTDSTAT EQU   CVTCTLFG                                                 01268000
CVTRSV78 EQU   CVTCTLFG                                                 01269000
CVTRV333 EQU   CVTCTLFG                                                 01270000
CVTRV323 EQU   CVTCTLFG                                                 01271000
CVTSPVLK EQU   CVTS01+49                                                01272000
CVTRSV77 EQU   CVTS01+48                                                01273000
CVTRV331 EQU   CVTS01+44                                                01274000
CVTRV330 EQU   CVTS01+40                                                01275000
CVTRV329 EQU   CVTS01+36                                                01276000
CVTRV328 EQU   CVTS01+32                                                01277000
CVTRV322 EQU   CVTS01+28                                                01278000
CVTSLID  EQU   CVTSLIDA+1                                               01279000
CVTSYLK  EQU   CVTSLIDA                                                 01280000
CVTRV321 EQU   CVTS01+20                                                01281000
CVTRV320 EQU   CVTS01+16                                                01282000
CVTLPDIR EQU   CVTLPDIA+1                                               01283000
CVTRSV69 EQU   CVTDIRST                                                 01284000
CVTRSV68 EQU   CVTDIRST                                                 01285000
CVTRSV67 EQU   CVTDIRST                                                 01286000
CVTRSV66 EQU   CVTDIRST                                                 01287000
CVTRSV65 EQU   CVTDIRST                                                 01288000
CVTRSV64 EQU   CVTDIRST                                                 01289000
CVTRSV63 EQU   CVTDIRST                                                 01290000
CVTDICOM EQU   CVTDIRST                                                 01291000
CVTPVTP  EQU   CVTS01+8                                                 01292000
CVTLPDSR EQU   CVTS01+4                                                 01293000
CVTGETL  EQU   CVTS01                                                   01294000
@NM00028 EQU   CSD+160                                                  01295000
CSDMASK  EQU   CSD+128                                                  01296000
CSDUCNT  EQU   CSD+124                                                  01297000
CSDTCNT  EQU   CSD+120                                                  01298000
CSDGDTOD EQU   CSD+116                                                  01299000
CSDGDINT EQU   CSD+112                                                  01300000
CSDGDCC  EQU   CSD+108                                                  01301000
CSDDDRCT EQU   CSD+106                                                  01302000
CSDRV044 EQU   CSD+104                                                  01303000
CSDMAFF  EQU   CSD+24                                                   01304000
CSDRV038 EQU   CSDFLAGS                                                 01305000
CSDRV037 EQU   CSDFLAGS                                                 01306000
CSDRV036 EQU   CSDFLAGS                                                 01307000
CSDRV035 EQU   CSDFLAGS                                                 01308000
CSDRV034 EQU   CSDFLAGS                                                 01309000
CSDRV033 EQU   CSDFLAGS                                                 01310000
CSDRV032 EQU   CSDFLAGS                                                 01311000
CSDMP    EQU   CSDFLAGS                                                 01312000
CSDACR   EQU   CSD+22                                                   01313000
CSDMF1CP EQU   CSD+20                                                   01314000
CSDRV043 EQU   CSD+16                                                   01315000
CSDRV030 EQU   CSDSCFL4                                                 01316000
CSDRV029 EQU   CSDSCFL4                                                 01317000
CSDRV028 EQU   CSDSCFL4                                                 01318000
CSDRV027 EQU   CSDSCFL4                                                 01319000
CSDRV026 EQU   CSDSCFL4                                                 01320000
CSDRV025 EQU   CSDSCFL4                                                 01321000
CSDRV024 EQU   CSDSCFL4                                                 01322000
CSDRV023 EQU   CSDSCFL4                                                 01323000
CSDRV022 EQU   CSDSCFL3                                                 01324000
CSDRV021 EQU   CSDSCFL3                                                 01325000
CSDRV020 EQU   CSDSCFL3                                                 01326000
CSDRV019 EQU   CSDSCFL3                                                 01327000
CSDRV018 EQU   CSDSCFL3                                                 01328000
CSDRV017 EQU   CSDSCFL3                                                 01329000
CSDRV016 EQU   CSDSCFL3                                                 01330000
CSDRV015 EQU   CSDSCFL3                                                 01331000
CSDRV014 EQU   CSDSCFL2                                                 01332000
CSDRV013 EQU   CSDSCFL2                                                 01333000
CSDRV012 EQU   CSDSCFL2                                                 01334000
CSDRV011 EQU   CSDSCFL2                                                 01335000
CSDRV010 EQU   CSDSCFL2                                                 01336000
CSDRV009 EQU   CSDSCFL2                                                 01337000
CSDRV008 EQU   CSDSCFL2                                                 01338000
CSDRV007 EQU   CSDSCFL2                                                 01339000
CSDRV006 EQU   CSDSCFL1                                                 01340000
CSDRV005 EQU   CSDSCFL1                                                 01341000
CSDRV004 EQU   CSDSCFL1                                                 01342000
CSDRV003 EQU   CSDSCFL1                                                 01343000
CSDRV002 EQU   CSDSCFL1                                                 01344000
CSDRV001 EQU   CSDSCFL1                                                 01345000
CSDSYSND EQU   CSDSCFL1                                                 01346000
CSDRV042 EQU   CSDSCFL1                                                 01347000
CSDCPUOL EQU   CSD+10                                                   01348000
CSDSAFF  EQU   CSDCPUAL                                                 01349000
CSDCHAD  EQU   CSD+6                                                    01350000
CSDCPUJS EQU   CSD+4                                                    01351000
CSDCSD   EQU   CSD                                                      01352000
CATEND   EQU   CAT+8                                                    01353000
CASFLARS EQU   CATFLA                                                   01354000
CATBSY   EQU   CATFLA                                                   01355000
CATFLG7  EQU   CATFLG                                                   01356000
CATFLG6  EQU   CATFLG                                                   01357000
CATFLG5  EQU   CATFLG                                                   01358000
CATNCPU  EQU   CATFLG                                                   01359000
CATNGEN  EQU   CATFLG                                                   01360000
CATRES1  EQU   CATFLG                                                   01361000
@NM00003 EQU   PCCA+512                                                 01362000
PCCARV36 EQU   PCCA+380                                                 01363000
PCCARV35 EQU   PCCA+378                                                 01364000
PCCARV01 EQU   PCCA+377                                                 01365000
PCCACPUM EQU   PCCA+376                                                 01366000
PCCARV63 EQU   PCCA+372                                                 01367000
PCCARV62 EQU   PCCA+368                                                 01368000
PCCARV61 EQU   PCCA+364                                                 01369000
PCCARV60 EQU   PCCA+360                                                 01370000
PCCARV59 EQU   PCCA+356                                                 01371000
PCCARV58 EQU   PCCA+352                                                 01372000
PCCARV57 EQU   PCCA+348                                                 01373000
PCCARV56 EQU   PCCA+344                                                 01374000
PCCARV55 EQU   PCCA+340                                                 01375000
PCCARV54 EQU   PCCA+336                                                 01376000
PCCALOGA EQU   PCCA+332                                                 01377000
PCCACHID EQU   PCCA+324                                                 01378000
PCCACHSV EQU   PCCA+312                                                 01379000
PCCARV79 EQU   PCCACHF4                                                 01380000
PCCARV78 EQU   PCCACHF4                                                 01381000
PCCARV77 EQU   PCCACHF4                                                 01382000
PCCARV76 EQU   PCCACHF4                                                 01383000
PCCARV75 EQU   PCCACHF4                                                 01384000
PCCARV74 EQU   PCCACHF4                                                 01385000
PCCARV73 EQU   PCCACHF4                                                 01386000
PCCARV72 EQU   PCCACHF4                                                 01387000
PCCARV71 EQU   PCCACHF3                                                 01388000
PCCARV70 EQU   PCCACHF3                                                 01389000
PCCARV69 EQU   PCCACHF3                                                 01390000
PCCARV68 EQU   PCCACHF3                                                 01391000
PCCARV67 EQU   PCCACHF3                                                 01392000
PCCARV66 EQU   PCCACHF3                                                 01393000
PCCARV65 EQU   PCCACHF3                                                 01394000
PCCARV64 EQU   PCCACHF3                                                 01395000
PCCACF28 EQU   PCCACHF2                                                 01396000
PCCACF27 EQU   PCCACHF2                                                 01397000
PCCACF26 EQU   PCCACHF2                                                 01398000
PCCACF25 EQU   PCCACHF2                                                 01399000
PCCACF24 EQU   PCCACHF2                                                 01400000
PCCACF23 EQU   PCCACHF2                                                 01401000
PCCACF22 EQU   PCCACHF2                                                 01402000
PCCACF21 EQU   PCCACHF2                                                 01403000
PCCACF18 EQU   PCCACHF1                                                 01404000
PCCACF17 EQU   PCCACHF1                                                 01405000
PCCACF16 EQU   PCCACHF1                                                 01406000
PCCACF15 EQU   PCCACHF1                                                 01407000
PCCACF14 EQU   PCCACHF1                                                 01408000
PCCACF13 EQU   PCCACHF1                                                 01409000
PCCACF12 EQU   PCCACHF1                                                 01410000
PCCACF11 EQU   PCCACHF1                                                 01411000
PCCARV05 EQU   PCCA+306                                                 01412000
PCCACHPB EQU   PCCA+305                                                 01413000
PCCALGP2 EQU   PCCA+304                                                 01414000
PCCALGP1 EQU   PCCA+303                                                 01415000
PCCALOGL EQU   PCCA+302                                                 01416000
PCCARV80 EQU   PCCA+300                                                 01417000
PCCACHW2 EQU   PCCA+296                                                 01418000
PCCACHW1 EQU   PCCA+292                                                 01419000
PCCAIOSI EQU   PCCA+291                                                 01420000
PCCACNRB EQU   PCCACHRB                                                 01421000
PCCACCVB EQU   PCCACHRB                                                 01422000
PCCACSNB EQU   PCCACHRB                                                 01423000
PCCARV52 EQU   PCCACHRB                                                 01424000
PCCACHIB EQU   PCCACHRB                                                 01425000
PCCACTIB EQU   PCCACHRB                                                 01426000
PCCACINB EQU   PCCACHRB                                                 01427000
PCCACSIB EQU   PCCACHRB                                                 01428000
PCCARV51 EQU   PCCACHS2                                                 01429000
PCCARV50 EQU   PCCACHS2                                                 01430000
PCCARV49 EQU   PCCACHS2                                                 01431000
PCCACURC EQU   PCCACHS2                                                 01432000
PCCACNLG EQU   PCCACHS2                                                 01433000
PCCACMOD EQU   PCCACHS2                                                 01434000
PCCACALT EQU   PCCACHS2                                                 01435000
PCCACIOR EQU   PCCACHS2                                                 01436000
PCCARV47 EQU   PCCACHS1                                                 01437000
PCCACUCB EQU   PCCACHS1                                                 01438000
PCCACIBC EQU   PCCACHS1                                                 01439000
PCCACAND EQU   PCCACHS1                                                 01440000
PCCACNLS EQU   PCCACHS1                                                 01441000
PCCACFRR EQU   PCCACHS1                                                 01442000
PCCACNRE EQU   PCCACHS1                                                 01443000
PCCACCMP EQU   PCCACHS1                                                 01444000
PCCACSEQ EQU   PCCACHTS                                                 01445000
PCCACDIN EQU   PCCACHTS                                                 01446000
PCCARV44 EQU   PCCACHTS                                                 01447000
PCCARV43 EQU   PCCACHTS                                                 01448000
PCCACTEC EQU   PCCACHTS                                                 01449000
PCCACDAV EQU   PCCACHVA                                                 01450000
PCCACCHV EQU   PCCACHVA                                                 01451000
PCCACCMD EQU   PCCACHVA                                                 01452000
PCCACUNS EQU   PCCACHVA                                                 01453000
PCCACSQV EQU   PCCACHVA                                                 01454000
PCCARV42 EQU   PCCACHVA                                                 01455000
PCCARV41 EQU   PCCACHVA                                                 01456000
PCCACITF EQU   PCCACHVA                                                 01457000
PCCARV40 EQU   PCCACHBL                                                 01458000
PCCARV39 EQU   PCCACHBL                                                 01459000
PCCARV38 EQU   PCCACHBL                                                 01460000
PCCACCUE EQU   PCCACHBL                                                 01461000
PCCACSTG EQU   PCCACHBL                                                 01462000
PCCACSCU EQU   PCCACHBL                                                 01463000
PCCACCHA EQU   PCCACHBL                                                 01464000
PCCACCPU EQU   PCCACHBL                                                 01465000
PCCACNOR EQU   PCCACHPF                                                 01466000
PCCACCNT EQU   PCCACHPF                                                 01467000
PCCACSNS EQU   PCCACHPF                                                 01468000
PCCARV37 EQU   PCCACHPF                                                 01469000
PCCACHIO EQU   PCCACHPF                                                 01470000
PCCACTIO EQU   PCCACHPF                                                 01471000
PCCACINT EQU   PCCACHPF                                                 01472000
PCCACSIO EQU   PCCACHPF                                                 01473000
PCCACHUB EQU   PCCAWERP                                                 01474000
PCCACHEL EQU   PCCA+168                                                 01475000
PCCALRBR EQU   PCCA+164                                                 01476000
PCCALRBV EQU   PCCA+160                                                 01477000
PCCAPWAR EQU   PCCA+156                                                 01478000
PCCAPWAV EQU   PCCA+152                                                 01479000
PCCAEMSA EQU   PCCAEMSB+12                                              01480000
PCCAEMSE EQU   PCCAEMSB+8                                               01481000
PCCAEMSP EQU   PCCAEMSB+4                                               01482000
PCCARMS  EQU   PCCARMSB                                                 01483000
PCCARV34 EQU   PCCARMSB                                                 01484000
PCCARV33 EQU   PCCARMSB                                                 01485000
PCCARV32 EQU   PCCARMSB                                                 01486000
PCCARV31 EQU   PCCARMSB                                                 01487000
PCCARV30 EQU   PCCARMSB                                                 01488000
PCCARV29 EQU   PCCARMSB                                                 01489000
PCCARV28 EQU   PCCARMSB                                                 01490000
PCCARV27 EQU   PCCAEMS3                                                 01491000
PCCARV26 EQU   PCCAEMS3                                                 01492000
PCCARV25 EQU   PCCAEMS3                                                 01493000
PCCARV24 EQU   PCCAEMS3                                                 01494000
PCCARV23 EQU   PCCAEMS3                                                 01495000
PCCARV22 EQU   PCCAEMS3                                                 01496000
PCCARV21 EQU   PCCAEMS3                                                 01497000
PCCARV20 EQU   PCCAEMS3                                                 01498000
PCCARV19 EQU   PCCAEMS2                                                 01499000
PCCARV18 EQU   PCCAEMS2                                                 01500000
PCCARV17 EQU   PCCAEMS2                                                 01501000
PCCARV16 EQU   PCCAEMS2                                                 01502000
PCCARV15 EQU   PCCAEMS2                                                 01503000
PCCARV14 EQU   PCCAEMS2                                                 01504000
PCCARV13 EQU   PCCAEMS2                                                 01505000
PCCARV12 EQU   PCCAEMS2                                                 01506000
PCCARV11 EQU   PCCARISP                                                 01507000
PCCARV10 EQU   PCCARISP                                                 01508000
PCCARV09 EQU   PCCARISP                                                 01509000
PCCARV08 EQU   PCCARISP                                                 01510000
PCCARV07 EQU   PCCARISP                                                 01511000
PCCARV06 EQU   PCCARISP                                                 01512000
PCCASERL EQU   PCCARISP                                                 01513000
PCCAPARL EQU   PCCARISP                                                 01514000
PCCARPB  EQU   PCCA+132                                                 01515000
PCCACTIN EQU   PCCAINTE                                                 01516000
PCCANFIN EQU   PCCAINTE                                                 01517000
PCCANUIN EQU   PCCAINTE                                                 01518000
PCCACTCC EQU   PCCACCE                                                  01519000
PCCANFCC EQU   PCCACCE                                                  01520000
PCCANUCC EQU   PCCACCE                                                  01521000
PCCACTTD EQU   PCCATODE                                                 01522000
PCCANFTD EQU   PCCATODE                                                 01523000
PCCANUTD EQU   PCCATODE                                                 01524000
PCCARV04 EQU   PCCATMFL                                                 01525000
PCCARV03 EQU   PCCATMFL                                                 01526000
PCCARV02 EQU   PCCATMFL                                                 01527000
PCCAMINT EQU   PCCATMFL                                                 01528000
PCCAMCC  EQU   PCCATMFL                                                 01529000
PCCAVKIL EQU   PCCATMFL                                                 01530000
PCCASYNC EQU   PCCATMFL                                                 01531000
PCCAINIT EQU   PCCATMFL                                                 01532000
PCCARV9E EQU   PCCA+124                                                 01533000
PCCARV9D EQU   PCCA+120                                                 01534000
PCCARV9C EQU   PCCA+116                                                 01535000
PCCARV9B EQU   PCCA+112                                                 01536000
PCCARV9A EQU   PCCA+108                                                 01537000
PCCARV99 EQU   PCCA+104                                                 01538000
PCCARV98 EQU   PCCA+100                                                 01539000
PCCARV97 EQU   PCCA+96                                                  01540000
PCCARV96 EQU   PCCA+92                                                  01541000
PCCARV95 EQU   PCCA+88                                                  01542000
PCCARV94 EQU   PCCA+84                                                  01543000
PCCARV93 EQU   PCCA+80                                                  01544000
PCCARV92 EQU   PCCA+76                                                  01545000
PCCARV91 EQU   PCCA+72                                                  01546000
PCCARV90 EQU   PCCA+68                                                  01547000
PCCARV89 EQU   PCCA+64                                                  01548000
PCCARV88 EQU   PCCA+60                                                  01549000
PCCARV87 EQU   PCCA+56                                                  01550000
PCCARV86 EQU   PCCA+52                                                  01551000
PCCARV85 EQU   PCCA+48                                                  01552000
PCCARV84 EQU   PCCA+44                                                  01553000
PCCARV83 EQU   PCCA+40                                                  01554000
PCCARV82 EQU   PCCA+36                                                  01555000
PCCARV81 EQU   PCCA+32                                                  01556000
PCCAPSAR EQU   PCCA+28                                                  01557000
PCCAPSAV EQU   PCCA+24                                                  01558000
PCCATQEP EQU   PCCA+20                                                  01559000
PCCACAFM EQU   PCCA+18                                                  01560000
PCCACPUA EQU   PCCA+16                                                  01561000
PCCACPID EQU   PCCA+4                                                   01562000
PCCAPCCA EQU   PCCA                                                     01563000
CVTLEVL  EQU   CVTRELNO+2                                               01564000
CVTNUMB  EQU   CVTRELNO                                                 01565000
CVTMDL   EQU   CVTFIX+250                                               01566000
@NM00002 EQU   CVTFIX+248                                               01567000
@NM00001 EQU   CVTFIX                                                   01568000
CVTRV482 EQU   CVTXTNT2+128                                             01569000
CVTRV481 EQU   CVTXTNT2+124                                             01570000
CVTRV480 EQU   CVTXTNT2+120                                             01571000
CVTRV479 EQU   CVTXTNT2+118                                             01572000
CVTRV478 EQU   CVTXTNT2+117                                             01573000
CVTRV477 EQU   CVTXTNT2+116                                             01574000
CVTRV476 EQU   CVTXTNT2+115                                             01575000
CVTRV475 EQU   CVTXTNT2+114                                             01576000
CVTRV474 EQU   CVTRV466                                                 01577000
CVTRV473 EQU   CVTRV466                                                 01578000
CVTRV472 EQU   CVTRV466                                                 01579000
CVTRV471 EQU   CVTRV466                                                 01580000
CVTRV470 EQU   CVTRV466                                                 01581000
CVTRV469 EQU   CVTRV466                                                 01582000
CVTRV468 EQU   CVTRV466                                                 01583000
CVTRV467 EQU   CVTRV466                                                 01584000
CVTRV465 EQU   CVTRV457                                                 01585000
CVTRV464 EQU   CVTRV457                                                 01586000
CVTRV463 EQU   CVTRV457                                                 01587000
CVTRV462 EQU   CVTRV457                                                 01588000
CVTRV461 EQU   CVTRV457                                                 01589000
CVTRV460 EQU   CVTRV457                                                 01590000
CVTRV459 EQU   CVTRV457                                                 01591000
CVTRV458 EQU   CVTRV457                                                 01592000
CVTRV456 EQU   CVTXTNT2+108                                             01593000
CVTRV455 EQU   CVTXTNT2+104                                             01594000
CVTRV454 EQU   CVTXTNT2+100                                             01595000
CVTRV453 EQU   CVTXTNT2+96                                              01596000
CVTRV452 EQU   CVTXTNT2+94                                              01597000
CVTRV451 EQU   CVTXTNT2+92                                              01598000
CVTRV450 EQU   CVTXTNT2+90                                              01599000
CVTRV449 EQU   CVTXTNT2+88                                              01600000
CVTRV448 EQU   CVTXTNT2+87                                              01601000
CVTRV447 EQU   CVTXTNT2+86                                              01602000
CVTRV446 EQU   CVTRV438                                                 01603000
CVTRV445 EQU   CVTRV438                                                 01604000
CVTRV444 EQU   CVTRV438                                                 01605000
CVTRV443 EQU   CVTRV438                                                 01606000
CVTRV442 EQU   CVTRV438                                                 01607000
CVTRV441 EQU   CVTRV438                                                 01608000
CVTRV440 EQU   CVTRV438                                                 01609000
CVTRV439 EQU   CVTRV438                                                 01610000
CVTRV437 EQU   CVTRV429                                                 01611000
CVTRV436 EQU   CVTRV429                                                 01612000
CVTRV435 EQU   CVTRV429                                                 01613000
CVTRV434 EQU   CVTRV429                                                 01614000
CVTRV433 EQU   CVTRV429                                                 01615000
CVTRV432 EQU   CVTRV429                                                 01616000
CVTRV431 EQU   CVTRV429                                                 01617000
CVTRV430 EQU   CVTRV429                                                 01618000
CVTRV428 EQU   CVTXTNT2+80                                              01619000
CVTRV427 EQU   CVTXTNT2+76                                              01620000
CVTRV426 EQU   CVTXTNT2+72                                              01621000
CVTRV425 EQU   CVTXTNT2+68                                              01622000
CVTATACT EQU   CVTATCVT                                                 01623000
CVTRV423 EQU   CVTXTNT2+62                                              01624000
CVTRV422 EQU   CVTXTNT2+60                                              01625000
CVTRV421 EQU   CVTXTNT2+58                                              01626000
CVTRV420 EQU   CVTXTNT2+56                                              01627000
CVTRV419 EQU   CVTXTNT2+55                                              01628000
CVTRV418 EQU   CVTXTNT2+54                                              01629000
CVTRV417 EQU   CVTRV409                                                 01630000
CVTRV416 EQU   CVTRV409                                                 01631000
CVTRV415 EQU   CVTRV409                                                 01632000
CVTRV414 EQU   CVTRV409                                                 01633000
CVTRV413 EQU   CVTRV409                                                 01634000
CVTRV412 EQU   CVTRV409                                                 01635000
CVTRV411 EQU   CVTRV409                                                 01636000
CVTRV410 EQU   CVTRV409                                                 01637000
CVTRV408 EQU   CVTRV400                                                 01638000
CVTRV407 EQU   CVTRV400                                                 01639000
CVTRV406 EQU   CVTRV400                                                 01640000
CVTRV405 EQU   CVTRV400                                                 01641000
CVTRV404 EQU   CVTRV400                                                 01642000
CVTRV403 EQU   CVTRV400                                                 01643000
CVTRV402 EQU   CVTRV400                                                 01644000
CVTRV401 EQU   CVTRV400                                                 01645000
CVTRSVA1 EQU   CVTXTNT2+48                                              01646000
CVTRSVA0 EQU   CVTXTNT2+44                                              01647000
CVTRSV99 EQU   CVTXTNT2+40                                              01648000
CVTRSV98 EQU   CVTXTNT2+36                                              01649000
CVTRSV97 EQU   CVTXTNT2+34                                              01650000
CVTRSV96 EQU   CVTXTNT2+32                                              01651000
CVTOLTEP EQU   CVTXTNT2+28                                              01652000
CVTQIDA  EQU   CVTQID+1                                                 01653000
CVTRSV95 EQU   CVTQID                                                   01654000
CVTRSV94 EQU   CVTXTNT2+20                                              01655000
CVTRSV93 EQU   CVTXTNT2+16                                              01656000
CVTRSV92 EQU   CVTXTNT2+12                                              01657000
CVTDEBVR EQU   CVTXTNT2+8                                               01658000
CVTRSV91 EQU   CVTXTNT2+6                                               01659000
CVTRSV9H EQU   CVTRSV90                                                 01660000
CVTRSV9G EQU   CVTRSV90                                                 01661000
CVTRSV9F EQU   CVTRSV90                                                 01662000
CVTRSV9E EQU   CVTRSV90                                                 01663000
CVTRSV9D EQU   CVTRSV90                                                 01664000
CVTRSV9C EQU   CVTRSV90                                                 01665000
CVTRSV9B EQU   CVTRSV90                                                 01666000
CVTRSV9A EQU   CVTRSV90                                                 01667000
CVTNUCLS EQU   CVTXTNT2+4                                               01668000
CVTDSSVA EQU   CVTDSSV+1                                                01669000
CVTRSV89 EQU   CVTDSSV                                                  01670000
CVTRSV88 EQU   CVTXTNT1+8                                               01671000
CVTRSV87 EQU   CVTXTNT1+4                                               01672000
CVTFACHN EQU   CVTXTNT1                                                 01673000
CVTRV488 EQU   CVTMAP+412                                               01674000
CVTRV487 EQU   CVTMAP+408                                               01675000
CVTRV486 EQU   CVTMAP+404                                               01676000
CVTRV485 EQU   CVTMAP+400                                               01677000
CVTRV484 EQU   CVTMAP+396                                               01678000
CVTAUTH  EQU   CVTMAP+392                                               01679000
CVTATMCA EQU   CVTATMCT+1                                               01680000
CVTATMST EQU   CVTATMCT                                                 01681000
CVTRSV61 EQU   CVTMAP+384                                               01682000
CVTVOLT1 EQU   CVTVOLM1+1                                               01683000
CVTVOLI1 EQU   CVTVOLF1                                                 01684000
CVTSTOA  EQU   CVTMAP+376                                               01685000
CVTRSV58 EQU   CVTMAP+374                                               01686000
CVTRSV57 EQU   CVTMAP+372                                               01687000
CVTDDCE  EQU   CVTMAP+368                                               01688000
CVTPNWFR EQU   CVTMAP+364                                               01689000
CVTSMF   EQU   CVTMAP+360                                               01690000
CVTSULK  EQU   CVTMAP+358                                               01691000
CVTSLKO  EQU   CVTSYSK                                                  01692000
CVTSLKP  EQU   CVTSYSK                                                  01693000
CVTSLKQ  EQU   CVTSYSK                                                  01694000
CVTSLKR  EQU   CVTSYSK                                                  01695000
CVTRSV56 EQU   CVTSYSK                                                  01696000
CVTRSV55 EQU   CVTSYSK                                                  01697000
CVTRSV54 EQU   CVTSYSK                                                  01698000
CVTRSV53 EQU   CVTSYSK                                                  01699000
CVTRSV52 EQU   CVTA1F1                                                  01700000
CVTRSV51 EQU   CVTA1F1                                                  01701000
CVTRSV50 EQU   CVTA1F1                                                  01702000
CVTRSV49 EQU   CVTA1F1                                                  01703000
CVTRSV48 EQU   CVTA1F1                                                  01704000
CVTRSV47 EQU   CVTA1F1                                                  01705000
CVTSRSW  EQU   CVTA1F1                                                  01706000
CVTPFSW  EQU   CVTA1F1                                                  01707000
CVTPCVT  EQU   CVTMAP+352                                               01708000
CVTRSV46 EQU   CVTMAP+344                                               01709000
CVTRSV45 EQU   CVTMAP+340                                               01710000
CVTRSV44 EQU   CVTMAP+338                                               01711000
CVTRSV43 EQU   CVTMAP+336                                               01712000
CVTHJESA EQU   CVTHJES+1                                                01713000
CVTRSV42 EQU   CVTHJES                                                  01714000
CVTEXT2A EQU   CVTEXT2+1                                                01715000
CVTRSV41 EQU   CVTEXT2                                                  01716000
CVTAPFA  EQU   CVTAPF+1                                                 01717000
CVTRSV40 EQU   CVTAPF                                                   01718000
CVTRV518 EQU   CVTINTLA                                                 01719000
CVTRV517 EQU   CVTERPV                                                  01720000
CVTEORM  EQU   CVTMAP+312                                               01721000
CVTMCHPR EQU   CVTMAP+308                                               01722000
CVTTZ    EQU   CVTMAP+304                                               01723000
CVTJEPS  EQU   CVTMAP+300                                               01724000
CVTJESCT EQU   CVTMAP+296                                               01725000
CVTMODE  EQU   CVTMAP+292                                               01726000
CVTPTRV  EQU   CVTMAP+288                                               01727000
CVTREAL  EQU   CVTMAP+284                                               01728000
CVTRSV39 EQU   CVTMAP+280                                               01729000
CVTRSV38 EQU   CVTMAP+276                                               01730000
CVTDMSRA EQU   CVTDMSR+1                                                01731000
CVTRSV37 EQU   CVTDMSR                                                  01732000
CVTQMSGA EQU   CVTQMSG+1                                                01733000
CVTRSV36 EQU   CVTQMSG                                                  01734000
CVTAMFF  EQU   CVTMAP+264                                               01735000
CVTPURGA EQU   CVTPURG+1                                                01736000
CVTRSV35 EQU   CVTPURG                                                  01737000
CVTCBSP  EQU   CVTMAP+256                                               01738000
CVTATERA EQU   CVTATER+1                                                01739000
CVTSYST  EQU   CVTATER                                                  01740000
CVTVOLT2 EQU   CVTTAT                                                   01741000
CVTVOLI2 EQU   CVTVOLF2                                                 01742000
CVTAQAVB EQU   CVTAQAVT+1                                               01743000
CVTRSV34 EQU   CVTTCMFG                                                 01744000
CVTRSV33 EQU   CVTTCMFG                                                 01745000
CVTRSV32 EQU   CVTTCMFG                                                 01746000
CVTRSV31 EQU   CVTTCMFG                                                 01747000
CVTRSV30 EQU   CVTTCMFG                                                 01748000
CVTRSV29 EQU   CVTTCMFG                                                 01749000
CVTRSV28 EQU   CVTTCMFG                                                 01750000
CVTTCRDY EQU   CVTTCMFG                                                 01751000
CVTGTFA  EQU   CVTGTF+1                                                 01752000
CVTRSV27 EQU   CVTGTFST                                                 01753000
CVTRNIO  EQU   CVTGTFST                                                 01754000
CVTRV319 EQU   CVTUSR                                                   01755000
CVTRV318 EQU   CVTFORM                                                  01756000
CVTRV317 EQU   CVTTMODE                                                 01757000
CVTRV316 EQU   CVTSTATE                                                 01758000
CVTRV315 EQU   CVTGTFS                                                  01759000
CVTGTFAV EQU   CVTGTFS                                                  01760000
CVT0SCR1 EQU   CVTMAP+232                                               01761000
CVTRV515 EQU   CVTMAP+228                                               01762000
CVTRMS   EQU   CVTMAP+224                                               01763000
CVTPATCH EQU   CVTMAP+220                                               01764000
CVTTSCE  EQU   CVTMAP+216                                               01765000
CVTLNKSC EQU   CVTMAP+214                                               01766000
CVTQABST EQU   CVTMAP+212                                               01767000
CVTMDLDS EQU   CVTMAP+208                                               01768000
CVTUSER  EQU   CVTMAP+204                                               01769000
CVTABEND EQU   CVTMAP+200                                               01770000
CVTSMCA  EQU   CVTMAP+196                                               01771000
CVTRSV18 EQU   CVTMAP+192                                               01772000
CVTQLPAQ EQU   CVTMAP+188                                               01773000
CVTQCDSR EQU   CVTMAP+184                                               01774000
CVTRSV17 EQU   CVTOPTB                                                  01775000
CVTRSV16 EQU   CVTOPTB                                                  01776000
CVTFP    EQU   CVTOPTB                                                  01777000
CVTAPTHR EQU   CVTOPTB                                                  01778000
CVTNLOG  EQU   CVTOPTB                                                  01779000
CVTTOD   EQU   CVTOPTB                                                  01780000
CVTCTIMS EQU   CVTOPTB                                                  01781000
CVTPROT  EQU   CVTOPTB                                                  01782000
CVTXPFP  EQU   CVTOPTA                                                  01783000
CVTASCII EQU   CVTOPTA                                                  01784000
CVTRSV13 EQU   CVTOPTA                                                  01785000
CVTRSV12 EQU   CVTOPTA                                                  01786000
CVTNIP   EQU   CVTOPTA                                                  01787000
CVTDDR   EQU   CVTOPTA                                                  01788000
CVTAPR   EQU   CVTOPTA                                                  01789000
CVTCCH   EQU   CVTOPTA                                                  01790000
CVTSNCTR EQU   CVTMAP+180                                               01791000
CVTQMWR  EQU   CVTMAP+176                                               01792000
CVTQOCR  EQU   CVTMAP+172                                               01793000
CVT1EF00 EQU   CVTMAP+168                                               01794000
CVTMZ00  EQU   CVTMAP+164                                               01795000
CVTHEAD  EQU   CVTMAP+160                                               01796000
CVTRSV11 EQU   CVTMAP+156                                               01797000
CVT0PT01 EQU   CVTMAP+152                                               01798000
CVTMSER  EQU   CVTMAP+148                                               01799000
CVTRV516 EQU   CVTIERLC                                                 01800000
CVTILCH  EQU   CVTMAP+140                                               01801000
CVT0DS   EQU   CVTMAP+136                                               01802000
CVTFBOSV EQU   CVTMAP+132                                               01803000
CVTNUCB  EQU   CVTMAP+128                                               01804000
CVTIXAVL EQU   CVTMAP+124                                               01805000
CVTIOQET EQU   CVTMAP+120                                               01806000
CVTDCBA  EQU   CVTMAP+117                                               01807000
CVTMVS2  EQU   CVTDCB                                                   01808000
CVT6DAT  EQU   CVTDCB                                                   01809000
CVT4MPS  EQU   CVTDCB                                                   01810000
CVTRSV09 EQU   CVTDCB                                                   01811000
CVT4MS1  EQU   CVTDCB                                                   01812000
CVT2SPS  EQU   CVTDCB                                                   01813000
CVT1SSS  EQU   CVTDCB                                                   01814000
CVTRSV08 EQU   CVTDCB                                                   01815000
CVTSTB   EQU   CVTMAP+112                                               01816000
CVTQTD00 EQU   CVTMAP+108                                               01817000
CVTQTE00 EQU   CVTMAP+104                                               01818000
CVTCUCB  EQU   CVTMAP+100                                               01819000
CVTSJQ   EQU   CVTMAP+96                                                01820000
CVTPBLDL EQU   CVTMAP+92                                                01821000
CVTTPC   EQU   CVTMAP+88                                                01822000
CVTSVDCB EQU   CVTMAP+84                                                01823000
CVTBRET  EQU   CVTMAP+82                                                01824000
CVTEXIT  EQU   CVTMAP+80                                                01825000
CVT0FN00 EQU   CVTMAP+76                                                01826000
CVTDARA  EQU   CVTDAR+1                                                 01827000
CVTRSV07 EQU   CVTFLGS1                                                 01828000
CVTRSV06 EQU   CVTFLGS1                                                 01829000
CVTRSV05 EQU   CVTFLGS1                                                 01830000
CVTRSV04 EQU   CVTFLGS1                                                 01831000
CVTRSV03 EQU   CVTFLGS1                                                 01832000
CVTRSV02 EQU   CVTFLGS1                                                 01833000
CVTRSV01 EQU   CVTFLGS1                                                 01834000
CVTDMPLK EQU   CVTFLGS1                                                 01835000
CVTXITP  EQU   CVTMAP+68                                                01836000
CVTZDTAB EQU   CVTMAP+64                                                01837000
CVTMSLT  EQU   CVTMAP+60                                                01838000
CVTDATE  EQU   CVTMAP+56                                                01839000
CVTBTERM EQU   CVTMAP+52                                                01840000
CVTSYSAD EQU   CVTMAP+48                                                01841000
CVTXTLER EQU   CVTMAP+44                                                01842000
CVTILK2  EQU   CVTMAP+40                                                01843000
CVTILK1  EQU   CVTMAP+36                                                01844000
CVTPRLTV EQU   CVTMAP+32                                                01845000
CVTPCNVT EQU   CVTMAP+28                                                01846000
CVT0VL00 EQU   CVTMAP+24                                                01847000
CVTXAPG  EQU   CVTMAP+20                                                01848000
CVTBUF   EQU   CVTMAP+16                                                01849000
CVTJOB   EQU   CVTMAP+12                                                01850000
CVTLINK  EQU   CVTMAP+8                                                 01851000
CVT0EF00 EQU   CVTMAP+4                                                 01852000
CVTTCBP  EQU   CVTMAP                                                   01853000
CVT      EQU   CVTMAP                                                   01854000
ECCDCMOD EQU   ECCDCHID                                                 01855000
ECCDRS02 EQU   ECCDB+1                                                  01856000
ECCDCCHG EQU   ECCDFLGS                                                 01857000
ECCDRS01 EQU   ECCDFLGS                                                 01858000
ECCPRS02 EQU   ECCPE+1                                                  01859000
ECCPRS01 EQU   ECCPFLGS                                                 01860000
ECCERS01 EQU   ECCEDT+4                                                 01861000
@NM00027 EQU   PSA+3412                                                 01862000
PSASTAK  EQU   PSA+3072                                                 01863000
@NM00026 EQU   PSA+1032                                                 01864000
PSAUSEND EQU   PSA+1032                                                 01865000
PSAPCPSW EQU   PSA+1024                                                 01866000
PSARV060 EQU   PSA+1020                                                 01867000
PSARV059 EQU   PSA+1018                                                 01868000
PSASVC13 EQU   PSA+1016                                                 01869000
PSALSFCC EQU   PSA+1012                                                 01870000
PSASFACC EQU   PSA+1008                                                 01871000
PSASTOP  EQU   PSA+992                                                  01872000
PSASTART EQU   PSA+976                                                  01873000
PSARSPSW EQU   PSA+968                                                  01874000
PSASRPSW EQU   PSA+960                                                  01875000
PSARV045 EQU   PSA+892                                                  01876000
PSARV044 EQU   PSA+888                                                  01877000
PSARV043 EQU   PSA+884                                                  01878000
PSARV042 EQU   PSA+880                                                  01879000
PSARV041 EQU   PSA+876                                                  01880000
PSARV040 EQU   PSA+872                                                  01881000
PSARV025 EQU   PSA+868                                                  01882000
PSADSSED EQU   PSA+868                                                  01883000
PSADSSPR EQU   PSA+864                                                  01884000
PSADSSFW EQU   PSA+860                                                  01885000
PSADSS14 EQU   PSA+856                                                  01886000
PSADSSPP EQU   PSA+848                                                  01887000
PSADSSRP EQU   PSA+840                                                  01888000
PSADSS05 EQU   PSADSSF4                                                 01889000
PSADSS10 EQU   PSADSSF4                                                 01890000
PSADSSVE EQU   PSADSSF4                                                 01891000
PSADSSDE EQU   PSADSSF4                                                 01892000
PSADSSC0 EQU   PSADSSF4                                                 01893000
PSADSSIE EQU   PSADSSF4                                                 01894000
PSADSS12 EQU   PSADSSF4                                                 01895000
PSADSSRC EQU   PSADSSF4                                                 01896000
PSARV057 EQU   PSADSSF3                                                 01897000
PSARV056 EQU   PSADSSF3                                                 01898000
PSARV055 EQU   PSADSSF3                                                 01899000
PSARV054 EQU   PSADSSF3                                                 01900000
PSADSSRW EQU   PSADSSF3                                                 01901000
PSADSSNM EQU   PSADSSF3                                                 01902000
PSADSSES EQU   PSADSSF3                                                 01903000
PSADSSGP EQU   PSADSSF3                                                 01904000
PSADSSF2 EQU   PSADSSFL+1                                               01905000
PSADSSPI EQU   PSADSSF1                                                 01906000
PSADSSOI EQU   PSADSSF1                                                 01907000
PSADSSSP EQU   PSADSSF1                                                 01908000
PSADSSTP EQU   PSADSSF1                                                 01909000
PSADSSDW EQU   PSADSSF1                                                 01910000
PSADSSDD EQU   PSADSSF1                                                 01911000
PSADSSDM EQU   PSADSSF1                                                 01912000
PSADSSMV EQU   PSADSSF1                                                 01913000
PSADSSTS EQU   PSA+816                                                  01914000
PSADSSWK EQU   PSA+812                                                  01915000
PSADSSR3 EQU   PSA+808                                                  01916000
PSADSSR2 EQU   PSA+804                                                  01917000
PSADSSRS EQU   PSA+800                                                  01918000
PSASTOR  EQU   PSA+796                                                  01919000
PSAVSTAP EQU   PSA+792                                                  01920000
PSAWKVAP EQU   PSA+788                                                  01921000
PSAWKRAP EQU   PSA+784                                                  01922000
PSAMCHIC EQU   PSA+783                                                  01923000
PSARV061 EQU   PSA+782                                                  01924000
PSASYMSK EQU   PSA+781                                                  01925000
PSAMCHFL EQU   PSA+780                                                  01926000
PSACR0   EQU   PSA+776                                                  01927000
PSAPSWSV EQU   PSA+768                                                  01928000
PSALITA  EQU   PSA+764                                                  01929000
PSACLHS  EQU   PSAHLHI                                                  01930000
PSALKR15 EQU   PSALKSA+60                                               01931000
PSALKR14 EQU   PSALKSA+56                                               01932000
PSALKR13 EQU   PSALKSA+52                                               01933000
PSALKR12 EQU   PSALKSA+48                                               01934000
PSALKR11 EQU   PSALKSA+44                                               01935000
PSALKR10 EQU   PSALKSA+40                                               01936000
PSALKR9  EQU   PSALKSA+36                                               01937000
PSALKR8  EQU   PSALKSA+32                                               01938000
PSALKR7  EQU   PSALKSA+28                                               01939000
PSALKR6  EQU   PSALKSA+24                                               01940000
PSALKR5  EQU   PSALKSA+20                                               01941000
PSALKR4  EQU   PSALKSA+16                                               01942000
PSALKR3  EQU   PSALKSA+12                                               01943000
PSALKR2  EQU   PSALKSA+8                                                01944000
PSALKR1  EQU   PSALKSA+4                                                01945000
PSALKR0  EQU   PSALKSA                                                  01946000
PSARV023 EQU   PSACLHT+52                                               01947000
PSALOCAL EQU   PSACLHT+48                                               01948000
PSACMSL  EQU   PSACLHT+44                                               01949000
PSAOPTL  EQU   PSACLHT+40                                               01950000
PSATPACL EQU   PSACLHT+36                                               01951000
PSATPDNL EQU   PSACLHT+32                                               01952000
PSATPNCL EQU   PSACLHT+28                                               01953000
PSAIOSLL EQU   PSACLHT+24                                               01954000
PSAIOSUL EQU   PSACLHT+20                                               01955000
PSAIOSCL EQU   PSACLHT+16                                               01956000
PSAIOSSL EQU   PSACLHT+12                                               01957000
PSASALCL EQU   PSACLHT+8                                                01958000
PSAASML  EQU   PSACLHT+4                                                01959000
PSADISPL EQU   PSACLHT                                                  01960000
PSASRSA  EQU   PSA+636                                                  01961000
PSARV050 EQU   PSA+634                                                  01962000
PSADSSGO EQU   PSA+633                                                  01963000
PSARECUR EQU   PSA+632                                                  01964000
PSAHLHIS EQU   PSA+628                                                  01965000
PSAIPCSA EQU   PSA+624                                                  01966000
@NM00025 EQU   PSA+621                                                  01967000
PSAIPCDM EQU   PSA+620                                                  01968000
PSAIPCD  EQU   PSA+616                                                  01969000
@NM00024 EQU   PSA+613                                                  01970000
PSAIPCRM EQU   PSA+612                                                  01971000
PSAIPCR  EQU   PSA+608                                                  01972000
PSAMCHEX EQU   PSA+600                                                  01973000
PSAMPSW  EQU   PSA+592                                                  01974000
PSAEXPS2 EQU   PSA+584                                                  01975000
PSAEXPS1 EQU   PSA+576                                                  01976000
PSAPIREG EQU   PSA+572                                                  01977000
PSARSREG EQU   PSA+568                                                  01978000
PSAGPREG EQU   PSA+556                                                  01979000
PSARV022 EQU   PSASUP4                                                  01980000
PSARV021 EQU   PSASUP4                                                  01981000
PSARV020 EQU   PSASUP4                                                  01982000
PSARV019 EQU   PSASUP4                                                  01983000
PSARV018 EQU   PSASUP4                                                  01984000
PSARV017 EQU   PSASUP4                                                  01985000
PSARV016 EQU   PSASUP4                                                  01986000
PSARV015 EQU   PSASUP4                                                  01987000
PSARV014 EQU   PSASUP3                                                  01988000
PSARV013 EQU   PSASUP3                                                  01989000
PSARV012 EQU   PSASUP3                                                  01990000
PSARV011 EQU   PSASUP3                                                  01991000
PSARV010 EQU   PSASUP3                                                  01992000
PSARV009 EQU   PSASUP3                                                  01993000
PSARV008 EQU   PSASUP3                                                  01994000
PSAIOSUP EQU   PSASUP3                                                  01995000
PSALCR   EQU   PSASUP2                                                  01996000
PSARTM   EQU   PSASUP2                                                  01997000
PSAACR   EQU   PSASUP2                                                  01998000
PSAIPCE2 EQU   PSASUP2                                                  01999000
PSAIPCES EQU   PSASUP2                                                  02000000
PSAIPCEC EQU   PSASUP2                                                  02001000
PSAGTF   EQU   PSASUP2                                                  02002000
PSAIPCRI EQU   PSASUP2                                                  02003000
PSAIPCRP EQU   PSASUP1                                                  02004000
PSAIPCDR EQU   PSASUP1                                                  02005000
PSADISP  EQU   PSASUP1                                                  02006000
PSALOCK  EQU   PSASUP1                                                  02007000
PSAPI    EQU   PSASUP1                                                  02008000
PSAEXT   EQU   PSASUP1                                                  02009000
PSASVC   EQU   PSASUP1                                                  02010000
PSAIO    EQU   PSASUP1                                                  02011000
PSAAOLD  EQU   PSA+548                                                  02012000
PSAANEW  EQU   PSA+544                                                  02013000
PSATNEW  EQU   PSA+536                                                  02014000
PSALCCAR EQU   PSA+532                                                  02015000
PSALCCAV EQU   PSA+528                                                  02016000
PSAPCCAR EQU   PSA+524                                                  02017000
PSAPCCAV EQU   PSA+520                                                  02018000
PSACPULA EQU   PSA+518                                                  02019000
PSACPUPA EQU   PSA+516                                                  02020000
PSAPSA   EQU   PSA+512                                                  02021000
FLCHDEND EQU   PSA+512                                                  02022000
FLCCRSAV EQU   FLCMCLA+280                                              02023000
FLCGRSAV EQU   FLCMCLA+216                                              02024000
FLCFPSAV EQU   FLCMCLA+184                                              02025000
FLCFLA   EQU   FLCMCLA+88                                               02026000
FLCRGNCD EQU   FLCMCLA+84                                               02027000
FLCFSAA  EQU   FLCFSA+1                                                 02028000
@NM00023 EQU   FLCFSA                                                   02029000
@NM00022 EQU   FLCMCLA+72                                               02030000
FLCMCIC  EQU   FLCMCLA+64                                               02031000
@NM00021 EQU   FLCMCLA+20                                               02032000
FLCIOAA  EQU   FLCIOA+1                                                 02033000
@NM00020 EQU   FLCIOA                                                   02034000
@NM00019 EQU   FLCMCLA+15                                               02035000
@NM00018 EQU   FLCMCLA+14                                               02036000
@NM00017 EQU   FLCMCLA+12                                               02037000
FLCLCL   EQU   FLCMCLA+8                                                02038000
FLCIOELA EQU   FLCIOEL+1                                                02039000
@NM00016 EQU   FLCIOEL                                                  02040000
FLCCHNID EQU   FLCMCLA                                                  02041000
@NM00015 EQU   PSA+160                                                  02042000
FLCMTRCD EQU   PSA+157                                                  02043000
@NM00014 EQU   PSA+156                                                  02044000
FLCPERA  EQU   FLCPER+1                                                 02045000
@NM00013 EQU   FLCPER                                                   02046000
@NM00012 EQU   PSA+151                                                  02047000
FLCPERCD EQU   PSA+150                                                  02048000
FLCMCNUM EQU   PSA+149                                                  02049000
@NM00011 EQU   PSA+148                                                  02050000
FLCTEAA  EQU   FLCTEA+1                                                 02051000
@NM00010 EQU   FLCTEA                                                   02052000
PSAPIPC  EQU   PSAPICOD                                                 02053000
PSAPIMC  EQU   PSAPICOD                                                 02054000
PSAPIPER EQU   PSAPICOD                                                 02055000
PSARV049 EQU   FLCPICOD                                                 02056000
FLCPILCB EQU   FLCPIILC                                                 02057000
@NM00009 EQU   FLCPIILC                                                 02058000
@NM00008 EQU   PSAEPPSW                                                 02059000
FLCSVCN  EQU   PSAESPSW+2                                               02060000
FLCSILCB EQU   FLCSVILC                                                 02061000
@NM00007 EQU   FLCSVILC                                                 02062000
@NM00006 EQU   PSAESPSW                                                 02063000
FLCEICOD EQU   PSAEEPSW+2                                               02064000
PSASPAD  EQU   PSAEEPSW                                                 02065000
@NM00005 EQU   PSA+128                                                  02066000
FLCINPSW EQU   PSA+120                                                  02067000
FLCMNPSW EQU   PSA+112                                                  02068000
FLCPNPSW EQU   PSA+104                                                  02069000
FLCSNPSW EQU   PSA+96                                                   02070000
FLCENPSW EQU   PSA+88                                                   02071000
FLCTRACE EQU   PSA+84                                                   02072000
FLCTIMER EQU   PSA+80                                                   02073000
FLCCVT2  EQU   PSA+76                                                   02074000
FLCCAW   EQU   PSA+72                                                   02075000
FLCCSW   EQU   PSA+64                                                   02076000
FLCIOPSW EQU   PSA+56                                                   02077000
FLCMOPSW EQU   PSA+48                                                   02078000
FLCPOPSW EQU   PSA+40                                                   02079000
FLCSOPSW EQU   PSA+32                                                   02080000
FLCEOPSW EQU   PSA+24                                                   02081000
@NM00004 EQU   FLCICCW2+4                                               02082000
FLCCVT   EQU   FLCICCW2                                                 02083000
FLCICCW1 EQU   FLCROPSW                                                 02084000
FLCIPPSW EQU   FLCRNPSW                                                 02085000
*                                      END UNREFERENCED COMPONENTS      02086000
@RT00025 EQU   ECLAB4                                                   02087000
@RT00032 EQU   ECLAB2                                                   02088000
@RT00044 EQU   ECLAB2                                                   02089000
@RT00065 EQU   ECLAB3                                                   02090000
@RT00082 EQU   @EL00001                                                 02091000
@RT00091 EQU   ECLAB8                                                   02092000
@ENDDATA EQU   *                                                        02093000
         END   IRBMFECH,(C'PL/S-II',0502,74086)                         02094000
/*
//*
//STEP03  EXEC PGM=IEBGENER
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  *
  IDENTIFY IRBMFECH('ZP60030')
++MOD(IRBMFIHA) DISTLIB(AOSC5).
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
         TITLE '*/* IRBMFIHA - CHANNEL INITIALIZATION MODULE           *00001000
                       '                                                00002000
IRBMFIHA CSECT ,                                                   0001 00003000
@MAINENT DS    0H                                                  0001 00004000
         USING *,@15                                               0001 00005000
         B     @PROLOG                                             0001 00006000
         DC    AL1(16)                                             0001 00007000
         DC    C'IRBMFIHA  74.086'                                 0001 00008000
         DROP  @15                                                      00009000
@PROLOG  STM   @14,@12,12(@13)                                     0001 00010000
         BALR  @10,0                                               0001 00011000
@PSTART  DS    0H                                                  0001 00012000
         USING @PSTART,@10                                         0001 00013000
         L     @00,@SIZDATD                                        0001 00014000
         GETMAIN R,LV=(0)                                               00015000
         LR    @09,@01                                             0001 00016000
         USING @DATD,@09                                           0001 00017000
         ST    @13,@SA00001+4                                      0001 00018000
         LM    @00,@01,20(@13)                                     0001 00019000
         ST    @09,8(,@13)                                         0001 00020000
         LR    @13,@09                                             0001 00021000
         MVC   @PC00001(12),0(@01)                                 0001 00022000
*                                                                  0032 00023000
*/* BEGIN MAINLINE PROCESSING                                        */ 00024000
*                                                                  0032 00025000
*   IF STSMSTA='0'B THEN            /* IF THE CHANNEL OPTION FLAG IS    00026000
*                                      NOT SET                       */ 00027000
         L     @12,@PC00001                                        0032 00028000
         L     @12,IHSMAPTR(,@12)                                  0032 00029000
         TM    STSMSTA(@12),B'00000001'                            0032 00030000
         BNZ   @RF00032                                            0032 00031000
*     DO;                                                          0033 00032000
*       STSMOPT=0;                  /* CLEAR OPTION WORD             */ 00033000
         SR    @11,@11                                             0034 00034000
         ST    @11,STSMOPT(,@12)                                   0034 00035000
*       RETURN CODE(0);             /* RETURN TO INITIALIZATION    0035 00036000
*                                      MAINLINE-MFIMAINL IN IGX00013.*/ 00037000
         L     @13,4(,@13)                                         0035 00038000
         L     @00,@SIZDATD                                        0035 00039000
         LR    @01,@09                                             0035 00040000
         FREEMAIN R,LV=(0),A=(1)                                        00041000
         SR    @15,@15                                             0035 00042000
         L     @14,12(,@13)                                        0035 00043000
         LM    @00,@12,20(@13)                                     0035 00044000
         BR    @14                                                 0035 00045000
*     END;                                                         0036 00046000
*                                                                  0036 00047000
*   /*****************************************************************/ 00048000
*   /*                                                               */ 00049000
*   /* ALLOCATE STORAGE FOR THE STORAGE RESOURCE TABLE. CONNECT IT TO*/ 00050000
*   /* THE RESOURCE VECTOR TABLE MACDATE Y-2 73018                   */ 00051000
*   /*                                                               */ 00052000
*   /*****************************************************************/ 00053000
*                                                                  0037 00054000
*   RESPECIFY                                                      0037 00055000
*    (GPR00P,                                                      0037 00056000
*     GPR01P,                                                      0037 00057000
*     GPR14P,                                                      0037 00058000
*     GPR15P)RESTRICTED;                                           0037 00059000
@RF00032 DS    0H                                                  0038 00060000
*   GPR00P=IHSGTGMC;                /* GETMAIN CONTROL WORD TO REG   */ 00061000
         L     GPR00P,IHSGTGMC                                     0038 00062000
*   GENERATE;                                                      0039 00063000
IHGETMN1 GETMAIN R,LV=(0),RELATED=(MFIHSGGM,IRBMFTRM(TR0250))           00064000
*   GPR01P->IHGMCWRD=IHSGTGMC;      /* SUBPOOL AND LENGTH TO FIRST 0040 00065000
*                                      WORD OF GOTTEN AREA           */ 00066000
         MVC   IHGMCWRD(4,GPR01P),IHSGTGMC                         0040 00067000
*   STRVSSGT=GPR01P+LENGTH(IHGMCWRD);/* ADDRESS TO RVT               */ 00068000
*                                                                  0041 00069000
         L     @12,@PC00001                                        0041 00070000
         L     @12,IHSMAPTR(,@12)                                  0041 00071000
         L     @12,STSMRVT(,@12)                                   0041 00072000
         LA    @11,4                                               0041 00073000
         AR    @11,GPR01P                                          0041 00074000
         ST    @11,STRVSSGT(,@12)                                  0041 00075000
*   /*****************************************************************/ 00076000
*   /*                                                               */ 00077000
*   /* MACDATE Y-2 73018                                             */ 00078000
*   /*                                                               */ 00079000
*   /*****************************************************************/ 00080000
*                                                                  0042 00081000
*   RESPECIFY                                                      0042 00082000
*    (GPR00P,                                                      0042 00083000
*     GPR01P,                                                      0042 00084000
*     GPR14P,                                                      0042 00085000
*     GPR15P)UNRESTRICTED;                                         0042 00086000
*   STRVNSGT=1;                     /* FIRST ENTRY IS NEXT AVAILABLE */ 00087000
*                                                                  0043 00088000
         MVC   STRVNSGT(4,@12),@CF00048                            0043 00089000
*   /*****************************************************************/ 00090000
*   /*                                                               */ 00091000
*   /* ALLOCATE STORAGE FOR THE PROGRAM RESOURCE TABLE. CONNECT IT TO*/ 00092000
*   /* THE RESOURCE VECTOR TABLE MACDATE Y-2 73018                   */ 00093000
*   /*                                                               */ 00094000
*   /*****************************************************************/ 00095000
*                                                                  0044 00096000
*   RESPECIFY                                                      0044 00097000
*    (GPR00P,                                                      0044 00098000
*     GPR01P,                                                      0044 00099000
*     GPR14P,                                                      0044 00100000
*     GPR15P)RESTRICTED;                                           0044 00101000
*   GPR00P=IHPRTGMC;                /* GETMAIN CONTROL WORD TO REG   */ 00102000
         L     GPR00P,IHPRTGMC                                     0045 00103000
*   GENERATE;                                                      0046 00104000
IHGETMN2 GETMAIN R,LV=(0),RELATED=(MFIHPRGM,IRBMFTRM(TR0150))           00105000
*   GPR01P->IHGMCWRD=IHPRTGMC;      /* SUBPOOL AND LENGTH TO FIRST 0047 00106000
*                                      WORD OF GOTTEN STORAGE        */ 00107000
         MVC   IHGMCWRD(4,GPR01P),IHPRTGMC                         0047 00108000
*   STRVSPRT=GPR01P+LENGTH(IHGMCWRD);/* ADDRESS TO RVT               */ 00109000
*                                                                  0048 00110000
         L     @12,@PC00001                                        0048 00111000
         L     @12,IHSMAPTR(,@12)                                  0048 00112000
         L     @12,STSMRVT(,@12)                                   0048 00113000
         LA    @11,4                                               0048 00114000
         AR    @11,GPR01P                                          0048 00115000
         ST    @11,STRVSPRT(,@12)                                  0048 00116000
*   /*****************************************************************/ 00117000
*   /*                                                               */ 00118000
*   /* MACDATE Y-2 73018                                             */ 00119000
*   /*                                                               */ 00120000
*   /*****************************************************************/ 00121000
*                                                                  0049 00122000
*   RESPECIFY                                                      0049 00123000
*    (GPR00P,                                                      0049 00124000
*     GPR01P,                                                      0049 00125000
*     GPR14P,                                                      0049 00126000
*     GPR15P)UNRESTRICTED;                                         0049 00127000
*   STRVNPRT=1;                     /* FIRST ENTRY IS NEXT AVAILABLE */ 00128000
*                                                                  0050 00129000
         MVC   STRVNPRT(4,@12),@CF00048                            0050 00130000
*   /*****************************************************************/ 00131000
*   /*                                                               */ 00132000
*   /* THE CHANNEL OPTION WAS SELECTED. LOAD MODULE IRBMFEVT, FIX IT,*/ 00133000
*   /* AND STORE ITS NAME INTO THE RESOURCE LIST. ITS ADDRESS IS     */ 00134000
*   /* PLACED INTO THE CVT MACDATE Y-2 73018                         */ 00135000
*   /*                                                               */ 00136000
*   /*****************************************************************/ 00137000
*                                                                  0051 00138000
*   RESPECIFY                                                      0051 00139000
*    (GPR00P,                                                      0051 00140000
*     GPR01P,                                                      0051 00141000
*     GPR14P,                                                      0051 00142000
*     GPR15P)RESTRICTED;                                           0051 00143000
*   GENERATE;                                                      0052 00144000
IHLOADM1 LOAD  EP=IRBMFEVT,RELATED=(MFIHLEVT,IRBMFTRM(TR0120))          00145000
         LA    @01,0(,@01)          CLEAR HIGH ORDER BYTE               00146000
         SLL   @01,3                GET LENGTH IN BYTES                 00147000
*   CVTMFRTR=GPR00P;                /* MFROUTER SERVICE MODULE     0053 00148000
*                                      ADDRESS TO CVT                */ 00149000
         L     @12,CVTPTR                                          0053 00150000
         ST    GPR00P,CVTMFRTR(,@12)                               0053 00151000
*   STPRNAME(STRVNPRT)='IRBMFEVT';  /* MODULE NAME TO PRT            */ 00152000
         L     @12,@PC00001                                        0054 00153000
         L     @12,IHSMAPTR(,@12)                                  0054 00154000
         L     @12,STSMRVT(,@12)                                   0054 00155000
         L     @11,STRVNPRT(,@12)                                  0054 00156000
         LR    @08,@11                                             0054 00157000
         SLA   @08,4                                               0054 00158000
         L     @07,STRVSPRT(,@12)                                  0054 00159000
         ST    @08,@TF00001                                        0054 00160000
         ALR   @08,@07                                             0054 00161000
         AL    @08,@CF01569                                        0054 00162000
         MVC   STPRNAME(8,@08),@CC01549                            0054 00163000
*   STPRADDR(STRVNPRT)=GPR00P;      /* MODULE ADDRESS TO PRT         */ 00164000
         L     @08,@TF00001                                        0055 00165000
         AL    @08,@CF01570                                        0055 00166000
         ST    GPR00P,STPRADDR-8(@08,@07)                          0055 00167000
*   STPRLGTH(STRVNPRT)=-(GPR01P);   /* SAVE NEGATIVE LENGTH TO FLAG     00168000
*                                      NEED FOR BRANCH TO PGFREE     */ 00169000
         LCR   @08,GPR01P                                          0056 00170000
         L     @06,@TF00001                                        0056 00171000
         AL    @07,@CF01571                                        0056 00172000
         ST    @08,STPRLGTH-12(@06,@07)                            0056 00173000
*   STRVNPRT=STRVNPRT+1;            /* POINT TO NEXT SLOT IN PRT     */ 00174000
*                                                                  0057 00175000
         AH    @11,@CH00048                                        0057 00176000
         ST    @11,STRVNPRT(,@12)                                  0057 00177000
*   /*****************************************************************/ 00178000
*   /*                                                               */ 00179000
*   /* GET LOCAL LOCK BEFORE PGFIX BRANCH ENTRY                      */ 00180000
*   /*                                                               */ 00181000
*   /*****************************************************************/ 00182000
*                                                                  0058 00183000
*   RFY                                                            0058 00184000
*    (GPR11P,                                                      0058 00185000
*     GPR12P,                                                      0058 00186000
*     GPR13P)RSTD;                                                 0058 00187000
*   GEN REFS(PSALITA);                                             0059 00188000
         LR    GPR15P,GPR13P        SAVE SAVE AREA ADDRESS              00189000
IHSTLCK1 SETLOCK OBTAIN,TYPE=LOCAL,MODE=UNCOND,                        X00190000
               RELATED=(LOCAL,IRBMFIHA(IHSTLCK2))                       00191000
         LR    GPR13P,GPR15P        RESTORE SAVE AREA ADDRESS           00192000
*   RFY                                                            0060 00193000
*    (GPR11P,                                                      0060 00194000
*     GPR12P,                                                      0060 00195000
*     GPR13P)UNRSTD;                                               0060 00196000
*                                                                  0060 00197000
*   /*****************************************************************/ 00198000
*   /*                                                               */ 00199000
*   /* SET NON SWPA BIT IN ASCB TO FORCE FIX IN PREF AREA    @YM8211P*/ 00200000
*   /*                                                               */ 00201000
*   /*****************************************************************/ 00202000
*                                                                  0061 00203000
*   ASCBNSWP=IHNSWPON;              /*                       @YM8211P*/ 00204000
*                                                                  0061 00205000
         L     @12,ASCBPTR                                         0061 00206000
         OI    ASCBNSWP(@12),B'00000001'                           0061 00207000
*   /*****************************************************************/ 00208000
*   /*                                                               */ 00209000
*   /* FIX MODULE IRBMFEVT PAGE FIX WITH BRANCH ENTRY, TCB=0 TO KEEP */ 00210000
*   /* FIXED WHILE MF1 SWAPPED OUT                                   */ 00211000
*   /*                                                               */ 00212000
*   /*****************************************************************/ 00213000
*                                                                  0062 00214000
*   RFY                                                            0062 00215000
*    (GPR02P,                                                      0062 00216000
*     GPR04P)RSTD;                                                 0062 00217000
*   GPR02P=GPR00P+GPR01P;           /* END ADDRESS                   */ 00218000
         LR    GPR02P,GPR00P                                       0063 00219000
         AR    GPR02P,GPR01P                                       0063 00220000
*   GPR01P=GPR00P|IHCPGFIX;         /* START ADDRESS                 */ 00221000
         LR    GPR01P,GPR00P                                       0064 00222000
         O     GPR01P,@CF01540                                     0064 00223000
*   GPR00P=ADDR(IHPAGECB);          /* ECB ADDRESS                   */ 00224000
         LA    GPR00P,IHPAGECB                                     0065 00225000
*   GPR04P=0;                       /* TCB ADDRESS - ZERO TO KEEP  0066 00226000
*                                      FIXED WHILE MEMORY SWAPPED    */ 00227000
         SR    @12,@12                                             0066 00228000
         LR    GPR04P,@12                                          0066 00229000
*   IHPAGECB=0;                                                    0067 00230000
         ST    @12,IHPAGECB                                        0067 00231000
*   CALL IEAVPSIB;                  /* CALL PAGE FIX ROUTINE         */ 00232000
         L     @12,CVTPTR                                          0068 00233000
         L     @15,CVTVPSIB(,@12)                                  0068 00234000
         BALR  @14,@15                                             0068 00235000
*   RFY                                                            0069 00236000
*    (GPR02P,                                                      0069 00237000
*     GPR04P)UNRSTD;                                               0069 00238000
*                                                                  0069 00239000
*   /*****************************************************************/ 00240000
*   /*                                                               */ 00241000
*   /* RESET ASCB NON SWAP BIT                               @YM8211P*/ 00242000
*   /*                                                               */ 00243000
*   /*****************************************************************/ 00244000
*                                                                  0070 00245000
*   ASCBNSWP=IHNSWPOF;              /*                       @YM8211P*/ 00246000
*                                                                  0070 00247000
         L     @12,ASCBPTR                                         0070 00248000
         NI    ASCBNSWP(@12),B'11111110'                           0070 00249000
*   /*****************************************************************/ 00250000
*   /*                                                               */ 00251000
*   /* RELEASE LOCAL LOCK                                            */ 00252000
*   /*                                                               */ 00253000
*   /*****************************************************************/ 00254000
*                                                                  0071 00255000
*   RFY                                                            0071 00256000
*    (GPR11P,                                                      0071 00257000
*     GPR12P,                                                      0071 00258000
*     GPR13P)RSTD;                                                 0071 00259000
*   GEN REFS(PSALITA);                                             0072 00260000
         LR    GPR00P,GPR13P         SAVE SAVE AREA ADDRESS             00261000
IHSTLCK2 SETLOCK RELEASE,TYPE=LOCAL,                                   X00262000
               RELATED=(LOCAL,IRBMFIHA(IHSTLCK1))                       00263000
         LR    GPR13P,GPR00P                                            00264000
*   RFY                                                            0073 00265000
*    (GPR11P,                                                      0073 00266000
*     GPR12P,                                                      0073 00267000
*     GPR13P)UNRSTD;                                               0073 00268000
*   GEN SETS(IHPAGECB)(WAIT ECB=IHPAGECB);                         0074 00269000
*                                                                  0074 00270000
         WAIT ECB=IHPAGECB                                              00271000
*   /*****************************************************************/ 00272000
*   /*                                                               */ 00273000
*   /* LOAD EVENT MG MODULE IRBMFECH, FIX, AND STORE NAME INTO THE   */ 00274000
*   /* RESOURCE LIST AND INTO THE MMV                                */ 00275000
*   /*                                                               */ 00276000
*   /*****************************************************************/ 00277000
*                                                                  0075 00278000
*   GENERATE;                                                      0075 00279000
IHLOADM2 LOAD  EP=IRBMFECH,RELATED=(MFIHLECH,IRBMFTRM(TR0120))          00280000
         LA    @01,0(,@01)          CLEAR HIGH ORDER BYTE               00281000
         SLL   @01,3                GET LENGTH IN BYTES                 00282000
*   STPRNAME(STRVNPRT)='IRBMFECH';  /* MODULE NAME TO PRT            */ 00283000
         L     @12,@PC00001                                        0076 00284000
         L     @12,IHSMAPTR(,@12)                                  0076 00285000
         L     @12,STSMRVT(,@12)                                   0076 00286000
         L     @11,STRVNPRT(,@12)                                  0076 00287000
         LR    @08,@11                                             0076 00288000
         SLA   @08,4                                               0076 00289000
         L     @07,STRVSPRT(,@12)                                  0076 00290000
         ST    @08,@TF00001                                        0076 00291000
         ALR   @08,@07                                             0076 00292000
         AL    @08,@CF01569                                        0076 00293000
         MVC   STPRNAME(8,@08),@CC01550                            0076 00294000
*   STPRADDR(STRVNPRT)=GPR00P;      /* MODULE ADDRESS TO PRT         */ 00295000
         L     @08,@TF00001                                        0077 00296000
         AL    @08,@CF01570                                        0077 00297000
         ST    GPR00P,STPRADDR-8(@08,@07)                          0077 00298000
*   STPRLGTH(STRVNPRT)=-(GPR01P);   /* SAVE NEGATIVE LENGTH TO FLAG     00299000
*                                      NEED FOR BRANCH TO PAGE FREE  */ 00300000
         LCR   @08,GPR01P                                          0078 00301000
         L     @06,@TF00001                                        0078 00302000
         AL    @07,@CF01571                                        0078 00303000
         ST    @08,STPRLGTH-12(@06,@07)                            0078 00304000
*   STRVNPRT=STRVNPRT+1;            /* POINT TO NEXT PRT SLOT        */ 00305000
*                                                                  0079 00306000
         AH    @11,@CH00048                                        0079 00307000
         ST    @11,STRVNPRT(,@12)                                  0079 00308000
*   /*****************************************************************/ 00309000
*   /*                                                               */ 00310000
*   /* GET LOCAL LOCK BEFORE PGFIX BRANCH ENTRY                      */ 00311000
*   /*                                                               */ 00312000
*   /*****************************************************************/ 00313000
*                                                                  0080 00314000
*   RFY                                                            0080 00315000
*    (GPR11P,                                                      0080 00316000
*     GPR12P,                                                      0080 00317000
*     GPR13P)RSTD;                                                 0080 00318000
*   GEN REFS(PSALITA);                                             0081 00319000
         LR    GPR15P,GPR13P        SAVE SAVE AREA ADDRESS              00320000
IHSTLCK3 SETLOCK OBTAIN,TYPE=LOCAL,MODE=UNCOND,                        X00321000
               RELATED=(LOCAL,IRBMFIHA(IHSTLCK4))                       00322000
         LR    GPR13P,GPR15P        RESTORE SAVE AREA ADDRESS           00323000
*   RFY                                                            0082 00324000
*    (GPR11P,                                                      0082 00325000
*     GPR12P,                                                      0082 00326000
*     GPR13P)UNRSTD;                                               0082 00327000
*                                                                  0082 00328000
*   /*****************************************************************/ 00329000
*   /*                                                               */ 00330000
*   /* SET NON SWPA BIT IN ASCB TO FORCE FIX IN PREF AREA    @YM8211P*/ 00331000
*   /*                                                               */ 00332000
*   /*****************************************************************/ 00333000
*                                                                  0083 00334000
*   ASCBNSWP=IHNSWPON;              /*                       @YM8211P*/ 00335000
*                                                                  0083 00336000
         L     @12,ASCBPTR                                         0083 00337000
         OI    ASCBNSWP(@12),B'00000001'                           0083 00338000
*   /*****************************************************************/ 00339000
*   /*                                                               */ 00340000
*   /* FIX MODULE IRBMFECH PAGE FIX WITH BRANCH ENTRY, TCB=0 TO KEEP */ 00341000
*   /* FIXED WHILE MF1 SWAPPED OUT                                   */ 00342000
*   /*                                                               */ 00343000
*   /*****************************************************************/ 00344000
*                                                                  0084 00345000
*   RFY                                                            0084 00346000
*    (GPR02P,                                                      0084 00347000
*     GPR04P)RSTD;                                                 0084 00348000
*   GPR02P=GPR00P+GPR01P;           /* END ADDRESS                   */ 00349000
         LR    GPR02P,GPR00P                                       0085 00350000
         AR    GPR02P,GPR01P                                       0085 00351000
*   GPR01P=GPR00P|IHCPGFIX;         /* START ADDRESS                 */ 00352000
         LR    GPR01P,GPR00P                                       0086 00353000
         O     GPR01P,@CF01540                                     0086 00354000
*   GPR00P=ADDR(IHPAGECB);          /* ECB ADDRESS                   */ 00355000
         LA    GPR00P,IHPAGECB                                     0087 00356000
*   GPR04P=0;                       /* TCB ADDRESS ZERO TO KEEP FIXED   00357000
*                                      WHILE SWAPPED OUT             */ 00358000
         SR    @12,@12                                             0088 00359000
         LR    GPR04P,@12                                          0088 00360000
*   IHPAGECB=0;                                                    0089 00361000
         ST    @12,IHPAGECB                                        0089 00362000
*   CALL IEAVPSIB;                  /* CALL PAGE FIX ROUTINE         */ 00363000
         L     @12,CVTPTR                                          0090 00364000
         L     @15,CVTVPSIB(,@12)                                  0090 00365000
         BALR  @14,@15                                             0090 00366000
*   RFY                                                            0091 00367000
*    (GPR02P,                                                      0091 00368000
*     GPR04P)UNRSTD;                                               0091 00369000
*                                                                  0091 00370000
*   /*****************************************************************/ 00371000
*   /*                                                               */ 00372000
*   /* RESET ASCB NON SWAP BIT                               @YM8211P*/ 00373000
*   /*                                                               */ 00374000
*   /*****************************************************************/ 00375000
*                                                                  0092 00376000
*   ASCBNSWP=IHNSWPOF;              /*                       @YM8211P*/ 00377000
*                                                                  0092 00378000
         L     @12,ASCBPTR                                         0092 00379000
         NI    ASCBNSWP(@12),B'11111110'                           0092 00380000
*   /*****************************************************************/ 00381000
*   /*                                                               */ 00382000
*   /* RELEASE LOCAL LOCK                                            */ 00383000
*   /*                                                               */ 00384000
*   /*****************************************************************/ 00385000
*                                                                  0093 00386000
*   RFY                                                            0093 00387000
*    (GPR11P,                                                      0093 00388000
*     GPR12P,                                                      0093 00389000
*     GPR13P)RSTD;                                                 0093 00390000
*   GEN REFS(PSALITA);                                             0094 00391000
         LR    GPR00P,GPR13P        SAVE SAVE AREA ADDRESS              00392000
IHSTLCK4 SETLOCK RELEASE,TYPE=LOCAL,                                   X00393000
               RELATED=(LOCAL,IRBMFIHA(IHSTLCK3))                       00394000
         LR    GPR13P,GPR00P        RESTORE SAVE AREA ADDRESS           00395000
*   RFY                                                            0095 00396000
*    (GPR11P,                                                      0095 00397000
*     GPR12P,                                                      0095 00398000
*     GPR13P)UNRSTD;                                               0095 00399000
*   GEN SETS(IHPAGECB)(WAIT ECB=IHPAGECB);                         0096 00400000
*                                                                  0096 00401000
         WAIT ECB=IHPAGECB                                              00402000
*   /*****************************************************************/ 00403000
*   /*                                                               */ 00404000
*   /* STORE ADDRESS OF MODULE INTO MFROUTER VECTOR TABLES FOR TIMER */ 00405000
*   /* EVENT. FIRST ESTABLISH ADDRESSABILITY TO MMV TIMER MG ROUTINE */ 00406000
*   /* LIST                                                          */ 00407000
*   /*                                                               */ 00408000
*   /*****************************************************************/ 00409000
*                                                                  0097 00410000
*   RESPECIFY                                                      0097 00411000
*     STMMMGRL BASED(STMMEVNT(STMMTIME));                          0097 00412000
*                                                                  0097 00413000
*   /*****************************************************************/ 00414000
*   /*                                                               */ 00415000
*   /* MOVE IN ADDRESS OF MODULE                                     */ 00416000
*   /*                                                               */ 00417000
*   /*****************************************************************/ 00418000
*                                                                  0098 00419000
*   STMMMGAD(STMMNXMG(STMMTIME))=STPRADDR(STRVNPRT-1);             0098 00420000
*                                                                  0098 00421000
         L     @12,@PC00001+4                                      0098 00422000
         L     @12,IHMMVPTR(,@12)                                  0098 00423000
         L     @11,STMMNXMG(,@12)                                  0098 00424000
         SLA   @11,3                                               0098 00425000
         L     @12,STMMEVNT(,@12)                                  0098 00426000
         L     @08,@PC00001                                        0098 00427000
         L     @08,IHSMAPTR(,@08)                                  0098 00428000
         L     @08,STSMRVT(,@08)                                   0098 00429000
         L     @07,STRVNPRT(,@08)                                  0098 00430000
         SLA   @07,4                                               0098 00431000
         L     @08,STRVSPRT(,@08)                                  0098 00432000
         AL    @08,@CF01574                                        0098 00433000
         L     @08,STPRADDR-8(@07,@08)                             0098 00434000
         AL    @12,@CF01570                                        0098 00435000
         ST    @08,STMMMGAD(@11,@12)                               0098 00436000
*   /*****************************************************************/ 00437000
*   /*                                                               */ 00438000
*   /* LOAD EVENT MG MODULE IRBMFTCH, FIX AND STORE NAME INTO        */ 00439000
*   /* RESOURCE LIST AND INTO THE MMV                                */ 00440000
*   /*                                                               */ 00441000
*   /*****************************************************************/ 00442000
*                                                                  0099 00443000
*   GENERATE;                                                      0099 00444000
IHLOADM3 LOAD  EP=IRBMFTCH,RELATED=(MFIHLTCH,IRBMFTRM(TR0120))          00445000
         LA    @01,0(,@01)          CLEAR HIGH ORDER BYTE               00446000
         SLL   @01,3                GET LENGTH IN BYTES                 00447000
*   STPRNAME(STRVNPRT)='IRBMFTCH';  /* NAME TO PRT                   */ 00448000
         L     @12,@PC00001                                        0100 00449000
         L     @12,IHSMAPTR(,@12)                                  0100 00450000
         L     @12,STSMRVT(,@12)                                   0100 00451000
         L     @11,STRVNPRT(,@12)                                  0100 00452000
         LR    @08,@11                                             0100 00453000
         SLA   @08,4                                               0100 00454000
         L     @07,STRVSPRT(,@12)                                  0100 00455000
         ST    @08,@TF00001                                        0100 00456000
         ALR   @08,@07                                             0100 00457000
         AL    @08,@CF01569                                        0100 00458000
         MVC   STPRNAME(8,@08),@CC01553                            0100 00459000
*   STPRADDR(STRVNPRT)=GPR00P;      /* ADDRESS TO PRT                */ 00460000
         L     @08,@TF00001                                        0101 00461000
         AL    @08,@CF01570                                        0101 00462000
         ST    GPR00P,STPRADDR-8(@08,@07)                          0101 00463000
*   STPRLGTH(STRVNPRT)=-(GPR01P);   /* SAVE NEGATIVE LENGTH TO FLAG     00464000
*                                      NEED FOR BRANCH TO PAGE FREE  */ 00465000
         LCR   @08,GPR01P                                          0102 00466000
         L     @06,@TF00001                                        0102 00467000
         AL    @07,@CF01571                                        0102 00468000
         ST    @08,STPRLGTH-12(@06,@07)                            0102 00469000
*   STRVNPRT=STRVNPRT+1;            /* POINT TO NEXT PRT SLOT        */ 00470000
*                                                                  0103 00471000
         AH    @11,@CH00048                                        0103 00472000
         ST    @11,STRVNPRT(,@12)                                  0103 00473000
*   /*****************************************************************/ 00474000
*   /*                                                               */ 00475000
*   /* GET LOCAL LOCK BEFORE PGFIX BRANCH ENTRY                      */ 00476000
*   /*                                                               */ 00477000
*   /*****************************************************************/ 00478000
*                                                                  0104 00479000
*   RFY                                                            0104 00480000
*    (GPR11P,                                                      0104 00481000
*     GPR12P,                                                      0104 00482000
*     GPR13P)RSTD;                                                 0104 00483000
*   GEN REFS(PSALITA);                                             0105 00484000
         LR    GPR15P,GPR13P        SAVE SAVE AREA ADDRESS              00485000
IHSTLCK5 SETLOCK OBTAIN,TYPE=LOCAL,MODE=UNCOND,                        X00486000
               RELATED=(LOCAL,IRBMFIHA(IHSTLCK6))                       00487000
         LR    GPR13P,GPR15P        RESTORE SAVE AREA ADDRESS           00488000
*   RFY                                                            0106 00489000
*    (GPR11P,                                                      0106 00490000
*     GPR12P,                                                      0106 00491000
*     GPR13P)UNRSTD;                                               0106 00492000
*                                                                  0106 00493000
*   /*****************************************************************/ 00494000
*   /*                                                               */ 00495000
*   /* SET NON SWPA BIT IN ASCB TO FORCE FIX IN PREF AREA    @YM8211P*/ 00496000
*   /*                                                               */ 00497000
*   /*****************************************************************/ 00498000
*                                                                  0107 00499000
*   ASCBNSWP=IHNSWPON;              /*                       @YM8211P*/ 00500000
*                                                                  0107 00501000
         L     @12,ASCBPTR                                         0107 00502000
         OI    ASCBNSWP(@12),B'00000001'                           0107 00503000
*   /*****************************************************************/ 00504000
*   /*                                                               */ 00505000
*   /* FIX MODULE IRBMFTCH PAGE FIX WITH BRANCH ENTRY, TCB=0 TO KEEP */ 00506000
*   /* FIXED WHILE MF1 SWAPPED OUT                                   */ 00507000
*   /*                                                               */ 00508000
*   /*****************************************************************/ 00509000
*                                                                  0108 00510000
*   RFY                                                            0108 00511000
*    (GPR02P,                                                      0108 00512000
*     GPR04P)RSTD;                                                 0108 00513000
*   GPR02P=GPR00P+GPR01P;           /* END ADDRESS                   */ 00514000
         LR    GPR02P,GPR00P                                       0109 00515000
         AR    GPR02P,GPR01P                                       0109 00516000
*   GPR01P=GPR00P|IHCPGFIX;         /* STRT ADDRESS                  */ 00517000
         LR    GPR01P,GPR00P                                       0110 00518000
         O     GPR01P,@CF01540                                     0110 00519000
*   GPR00P=ADDR(IHPAGECB);          /* ECB ADDRESS                   */ 00520000
         LA    GPR00P,IHPAGECB                                     0111 00521000
*   GPR04P=0;                       /* TCB ADDR ZERO TO KEEP FIXED   */ 00522000
         SR    @12,@12                                             0112 00523000
         LR    GPR04P,@12                                          0112 00524000
*   IHPAGECB=0;                                                    0113 00525000
         ST    @12,IHPAGECB                                        0113 00526000
*   CALL IEAVPSIB;                  /* CALL PAGE FIX ROUTINE         */ 00527000
         L     @12,CVTPTR                                          0114 00528000
         L     @15,CVTVPSIB(,@12)                                  0114 00529000
         BALR  @14,@15                                             0114 00530000
*   RFY                                                            0115 00531000
*    (GPR02P,                                                      0115 00532000
*     GPR04P)UNRSTD;                                               0115 00533000
*                                                                  0115 00534000
*   /*****************************************************************/ 00535000
*   /*                                                               */ 00536000
*   /* RESET ASCB NON SWAP BIT                               @YM8211P*/ 00537000
*   /*                                                               */ 00538000
*   /*****************************************************************/ 00539000
*                                                                  0116 00540000
*   ASCBNSWP=IHNSWPOF;              /*                       @YM8211P*/ 00541000
*                                                                  0116 00542000
         L     @12,ASCBPTR                                         0116 00543000
         NI    ASCBNSWP(@12),B'11111110'                           0116 00544000
*   /*****************************************************************/ 00545000
*   /*                                                               */ 00546000
*   /* RELEASE LOCAL LOCK                                            */ 00547000
*   /*                                                               */ 00548000
*   /*****************************************************************/ 00549000
*                                                                  0117 00550000
*   RFY                                                            0117 00551000
*    (GPR11P,                                                      0117 00552000
*     GPR12P,                                                      0117 00553000
*     GPR13P)RSTD;                                                 0117 00554000
*   GEN REFS(PSALITA);                                             0118 00555000
         LR    GPR00P,GPR13P        SAVE SAVE AREA ADDRESS              00556000
IHSTLCK6 SETLOCK   RELEASE,TYPE=LOCAL,                                 X00557000
               RELATED=(LOCAL,IRBMFIHA(IHSTLCK5))                       00558000
         LR    GPR13P,GPR00P        RESTORE SAVE AREA ADDRESS           00559000
*   RFY                                                            0119 00560000
*    (GPR11P,                                                      0119 00561000
*     GPR12P,                                                      0119 00562000
*     GPR13P)UNRSTD;                                               0119 00563000
*   GEN SETS(IHPAGECB)(WAIT ECB=IHPAGECB);                         0120 00564000
*                                                                  0120 00565000
         WAIT ECB=IHPAGECB                                              00566000
*   /*****************************************************************/ 00567000
*   /*                                                               */ 00568000
*   /* STORE ADDRESS OF THE MODULE INTO MFROUTER VECTOR TABLE FOR THE*/ 00569000
*   /* TEST CHANNEL SIGP EVENT                                       */ 00570000
*   /*                                                               */ 00571000
*   /*****************************************************************/ 00572000
*                                                                  0121 00573000
*   RESPECIFY                                                      0121 00574000
*     STMMMGRL BASED(STMMEVNT(STMMTCH));                           0121 00575000
*                                                                  0121 00576000
*   /*****************************************************************/ 00577000
*   /*                                                               */ 00578000
*   /* MOVE IN ADDRESS                                               */ 00579000
*   /*                                                               */ 00580000
*   /*****************************************************************/ 00581000
*                                                                  0122 00582000
*   STMMMGAD(STMMNXMG(STMMTCH))=STPRADDR(STRVNPRT-1);              0122 00583000
*                                                                  0122 00584000
         L     @12,@PC00001+4                                      0122 00585000
         L     @12,IHMMVPTR(,@12)                                  0122 00586000
         L     @11,STMMNXMG+8(,@12)                                0122 00587000
         SLA   @11,3                                               0122 00588000
         L     @12,STMMEVNT+8(,@12)                                0122 00589000
         L     @08,@PC00001                                        0122 00590000
         L     @08,IHSMAPTR(,@08)                                  0122 00591000
         L     @08,STSMRVT(,@08)                                   0122 00592000
         L     @07,STRVNPRT(,@08)                                  0122 00593000
         SLA   @07,4                                               0122 00594000
         L     @08,STRVSPRT(,@08)                                  0122 00595000
         AL    @08,@CF01574                                        0122 00596000
         L     @08,STPRADDR-8(@07,@08)                             0122 00597000
         AL    @12,@CF01570                                        0122 00598000
         ST    @08,STMMMGAD(@11,@12)                               0122 00599000
*   /*****************************************************************/ 00600000
*   /*                                                               */ 00601000
*   /* LOAD THE INTERVAL-DRIVEN MG ROUTINE (IRBMFDHP)                */ 00602000
*   /*                                                               */ 00603000
*   /*****************************************************************/ 00604000
*                                                                  0123 00605000
*   GENERATE;                                                      0123 00606000
*                                                                  0123 00607000
IHLOADM4 LOAD  EP=IRBMFDHP,RELATED=(MFIHLDHP,IRBMFTRM(TR0120))          00608000
         LA    @01,0(,@01)          CLEAR HIGH ORDER BYTE               00609000
         SLL   @01,3                GET LENGTH IN BYTES                 00610000
*   /*****************************************************************/ 00611000
*   /*                                                               */ 00612000
*   /* STORE NAME, ADDRESS, AND LENGTH INTO PROGRAM RESOURCE TABLE   */ 00613000
*   /*                                                               */ 00614000
*   /*****************************************************************/ 00615000
*                                                                  0124 00616000
*   STPRNAME(STRVNPRT)='IRBMFDHP';  /* NAME TO PRT                   */ 00617000
         L     @12,@PC00001                                        0124 00618000
         L     @12,IHSMAPTR(,@12)                                  0124 00619000
         L     @11,STSMRVT(,@12)                                   0124 00620000
         L     @08,STRVNPRT(,@11)                                  0124 00621000
         LR    @07,@08                                             0124 00622000
         SLA   @07,4                                               0124 00623000
         L     @06,STRVSPRT(,@11)                                  0124 00624000
         ST    @07,@TF00001                                        0124 00625000
         ALR   @07,@06                                             0124 00626000
         AL    @07,@CF01569                                        0124 00627000
         MVC   STPRNAME(8,@07),@CC01556                            0124 00628000
*   STPRADDR(STRVNPRT)=GPR00P;      /* ADDRESS TO PRT                */ 00629000
         L     @07,@TF00001                                        0125 00630000
         AL    @07,@CF01570                                        0125 00631000
         ST    GPR00P,STPRADDR-8(@07,@06)                          0125 00632000
*   STPRLGTH(STRVNPRT)=GPR01P;      /* LENGTH TO PRT                 */ 00633000
         L     @07,@TF00001                                        0126 00634000
         AL    @06,@CF01571                                        0126 00635000
         ST    GPR01P,STPRLGTH-12(@07,@06)                         0126 00636000
*   STRVNPRT=STRVNPRT+1;            /* POINT TO NEXT PRT SLOT        */ 00637000
         LA    @07,1                                               0127 00638000
         AR    @08,@07                                             0127 00639000
         ST    @08,STRVNPRT(,@11)                                  0127 00640000
*   STSMINTP=GPR00P;                /* ADDRESS TO SMA                */ 00641000
*                                                                  0128 00642000
         ST    GPR00P,STSMINTP(,@12)                               0128 00643000
*   /*****************************************************************/ 00644000
*   /*                                                               */ 00645000
*   /* MACDATE Y-2 73018                                             */ 00646000
*   /*                                                               */ 00647000
*   /*****************************************************************/ 00648000
*                                                                  0129 00649000
*   RESPECIFY                                                      0129 00650000
*    (GPR00P,                                                      0129 00651000
*     GPR01P,                                                      0129 00652000
*     GPR14P,                                                      0129 00653000
*     GPR15P)UNRESTRICTED;                                         0129 00654000
*                                                                  0129 00655000
*   /*****************************************************************/ 00656000
*   /*                                                               */ 00657000
*   /* ALLOCATE GLOBAL KEY ZERO FIXED STORAGE FOR THE CHANNEL EVENT  */ 00658000
*   /* DATA TABLES                                                   */ 00659000
*   /*                                                               */ 00660000
*   /*****************************************************************/ 00661000
*                                                                  0130 00662000
*   IHEVTSBP=IHSQASBP;              /* SUBPOOL                       */ 00663000
*                                                                  0130 00664000
         MVI   IHEVTSBP,X'F5'                                      0130 00665000
*   /*****************************************************************/ 00666000
*   /*                                                               */ 00667000
*   /* LENGTH                                                        */ 00668000
*   /*                                                               */ 00669000
*   /*****************************************************************/ 00670000
*                                                                  0131 00671000
*   IHEVTLEN=LENGTH(ECCEDT)+(CVTMAXMP+1)*(LENGTH(ECCPE)+(CSDCHAD+1)*    00672000
*       LENGTH(ECCDB));                                            0131 00673000
*                                                                  0131 00674000
         L     @12,CVTPTR                                          0131 00675000
         LH    @01,CVTMAXMP(,@12)                                  0131 00676000
         AR    @01,@07                                             0131 00677000
         L     @12,CVTCSD(,@12)                                    0131 00678000
         AH    @07,CSDCHAD(,@12)                                   0131 00679000
         MH    @07,@CH01564                                        0131 00680000
         AH    @07,@CH00044                                        0131 00681000
         MR    @00,@07                                             0131 00682000
         AH    @01,@CH00187                                        0131 00683000
         STCM  @01,7,IHEVTLEN                                      0131 00684000
*   /*****************************************************************/ 00685000
*   /*                                                               */ 00686000
*   /* MACDATE Y-2 73018                                             */ 00687000
*   /*                                                               */ 00688000
*   /*****************************************************************/ 00689000
*                                                                  0132 00690000
*   RESPECIFY                                                      0132 00691000
*    (GPR00P,                                                      0132 00692000
*     GPR01P,                                                      0132 00693000
*     GPR14P,                                                      0132 00694000
*     GPR15P)RESTRICTED;                                           0132 00695000
*   GPR00P=IHEVTGMC;                /* GETMAIN CONTROL WORD TO REG   */ 00696000
         L     GPR00P,IHEVTGMC                                     0133 00697000
*   GENERATE;                                                      0134 00698000
IHGETMN3 GETMAIN R,LV=(0),RELATED=(MFIHEVGM,IRBMFTRM(TR0220))           00699000
*   STSMEDAD=GPR01P;                /* ADDRESS TO SMA                */ 00700000
*                                                                  0135 00701000
         L     @12,@PC00001                                        0135 00702000
         L     @12,IHSMAPTR(,@12)                                  0135 00703000
         ST    GPR01P,STSMEDAD(,@12)                               0135 00704000
*   /*****************************************************************/ 00705000
*   /*                                                               */ 00706000
*   /* MACDATE Y-2 73018                                             */ 00707000
*   /*                                                               */ 00708000
*   /*****************************************************************/ 00709000
*                                                                  0136 00710000
*   RESPECIFY                                                      0136 00711000
*    (GPR00P,                                                      0136 00712000
*     GPR01P,                                                      0136 00713000
*     GPR14P,                                                      0136 00714000
*     GPR15P)UNRESTRICTED;                                         0136 00715000
*                                                                  0136 00716000
*   /*****************************************************************/ 00717000
*   /*                                                               */ 00718000
*   /* STORE ADDRESS INTO STORAGE RESOURCE TABLE                     */ 00719000
*   /*                                                               */ 00720000
*   /*****************************************************************/ 00721000
*                                                                  0137 00722000
*   STSGFREE(STRVNSGT)=IHEVTGMC;    /* SUBPOOL AND LENGTH            */ 00723000
         L     @12,STSMRVT(,@12)                                   0137 00724000
         L     @11,STRVNSGT(,@12)                                  0137 00725000
         LR    @08,@11                                             0137 00726000
         SLA   @08,3                                               0137 00727000
         L     @15,STRVSSGT(,@12)                                  0137 00728000
         ST    @08,@TF00001                                        0137 00729000
         ALR   @08,@15                                             0137 00730000
         AL    @08,@CF01570                                        0137 00731000
         MVC   STSGFREE(4,@08),IHEVTGMC                            0137 00732000
*   STSGADD(STRVNSGT)=STSMEDAD;     /* ADDRESS                       */ 00733000
         L     @08,@PC00001                                        0138 00734000
         L     @08,IHSMAPTR(,@08)                                  0138 00735000
         L     @08,STSMEDAD(,@08)                                  0138 00736000
         L     @14,@TF00001                                        0138 00737000
         AL    @15,@CF01571                                        0138 00738000
         ST    @08,STSGADD-4(@14,@15)                              0138 00739000
*   STRVNSGT=STRVNSGT+1;            /* POINT TO NEXT SGT SLOT        */ 00740000
*                                                                  0139 00741000
         LA    @15,1                                               0139 00742000
         AR    @11,@15                                             0139 00743000
         ST    @11,STRVNSGT(,@12)                                  0139 00744000
*   /*****************************************************************/ 00745000
*   /*                                                               */ 00746000
*   /* STORE ADDRESS OF CHANNEL EVENT DATA TABLE INTO THE MFROUTER   */ 00747000
*   /* TABLE TIMER AND TEST CHANNEL LISTS ESTABLISH ADDRESSABILITY TO*/ 00748000
*   /* MMV TIMER MG ROUTINE LIST                                     */ 00749000
*   /*                                                               */ 00750000
*   /*****************************************************************/ 00751000
*                                                                  0140 00752000
*   RESPECIFY                                                      0140 00753000
*     STMMMGRL BASED(STMMEVNT(STMMTIME));                          0140 00754000
*                                                                  0140 00755000
*   /*****************************************************************/ 00756000
*   /*                                                               */ 00757000
*   /* MOVE IN ADDRESS                                               */ 00758000
*   /*                                                               */ 00759000
*   /*****************************************************************/ 00760000
*                                                                  0141 00761000
*   STMMMGDA(STMMNXMG(STMMTIME))=STSMEDAD;                         0141 00762000
*                                                                  0141 00763000
         L     @12,@PC00001+4                                      0141 00764000
         L     @12,IHMMVPTR(,@12)                                  0141 00765000
         L     @11,STMMNXMG(,@12)                                  0141 00766000
         LR    @14,@11                                             0141 00767000
         SLA   @14,3                                               0141 00768000
         L     @01,STMMEVNT(,@12)                                  0141 00769000
         AL    @01,@CF01571                                        0141 00770000
         ST    @08,STMMMGDA-4(@14,@01)                             0141 00771000
*   /*****************************************************************/ 00772000
*   /*                                                               */ 00773000
*   /* INDEX TO NEXT SAMPLING MG ROUTINE SLOT                        */ 00774000
*   /*                                                               */ 00775000
*   /*****************************************************************/ 00776000
*                                                                  0142 00777000
*   STMMNXMG(STMMTIME)=STMMNXMG(STMMTIME)+1;                       0142 00778000
*                                                                  0142 00779000
         AR    @11,@15                                             0142 00780000
         ST    @11,STMMNXMG(,@12)                                  0142 00781000
*   /*****************************************************************/ 00782000
*   /*                                                               */ 00783000
*   /* ESTABLISH ADDRESSABILITY TO THE TEST CHANNEL MG ROUTINE LIST  */ 00784000
*   /*                                                               */ 00785000
*   /*****************************************************************/ 00786000
*                                                                  0143 00787000
*   RESPECIFY                                                      0143 00788000
*     STMMMGRL BASED(STMMEVNT(STMMTCH));                           0143 00789000
*                                                                  0143 00790000
*   /*****************************************************************/ 00791000
*   /*                                                               */ 00792000
*   /* MOVE IN DATA AREA ADDRESS                                     */ 00793000
*   /*                                                               */ 00794000
*   /*****************************************************************/ 00795000
*                                                                  0144 00796000
*   STMMMGDA(STMMNXMG(STMMTCH))=STSMEDAD;                          0144 00797000
*                                                                  0144 00798000
         L     @11,STMMNXMG+8(,@12)                                0144 00799000
         LR    @14,@11                                             0144 00800000
         SLA   @14,3                                               0144 00801000
         L     @01,STMMEVNT+8(,@12)                                0144 00802000
         AL    @01,@CF01571                                        0144 00803000
         ST    @08,STMMMGDA-4(@14,@01)                             0144 00804000
*   /*****************************************************************/ 00805000
*   /*                                                               */ 00806000
*   /* POINT TO NEXT MG ROUTINE SLOT ON TEST CHANNEL LIST            */ 00807000
*   /*                                                               */ 00808000
*   /*****************************************************************/ 00809000
*                                                                  0145 00810000
*   STMMNXMG(STMMTCH)=STMMNXMG(STMMTCH)+1;                         0145 00811000
*                                                                  0145 00812000
         AR    @11,@15                                             0145 00813000
         ST    @11,STMMNXMG+8(,@12)                                0145 00814000
*   /*****************************************************************/ 00815000
*   /*                                                               */ 00816000
*   /* INITIALIZE CHANNEL EVENT DATA STRUCTURE. FIRST ZERO CHANNEL   */ 00817000
*   /* EVENT DATA TABLE                                              */ 00818000
*   /*                                                               */ 00819000
*   /*****************************************************************/ 00820000
*                                                                  0146 00821000
*   ECCEDT=ECCEDT&&ECCEDT;                                         0146 00822000
*                                                                  0146 00823000
         XC    ECCEDT(16,@08),ECCEDT(@08)                          0146 00824000
*   /*****************************************************************/ 00825000
*   /*                                                               */ 00826000
*   /* INITIALIZE CHANNEL EVENT DATA TABLE. FIRST SET POINTER TO CPE */ 00827000
*   /* TABLE                                                         */ 00828000
*   /*                                                               */ 00829000
*   /*****************************************************************/ 00830000
*                                                                  0147 00831000
*   ECCECPEQ=ADDR(ECCEDT)+LENGTH(ECCEDT);                          0147 00832000
         LA    @12,16                                              0147 00833000
         AR    @12,@08                                             0147 00834000
         ST    @12,ECCECPEQ(,@08)                                  0147 00835000
*   ECCECPUS=CVTMAXMP+1;            /* MAXIMUM NUMBER OF CPUS        */ 00836000
         L     @11,CVTPTR                                          0148 00837000
         LH    @11,CVTMAXMP(,@11)                                  0148 00838000
         AR    @11,@15                                             0148 00839000
         STC   @11,ECCECPUS(,@08)                                  0148 00840000
*   ECCECCHK=IHSAMPCC;              /* NUMBER OF SAMPLES BETWEEN   0149 00841000
*                                      CONFIGURATION CHECKS          */ 00842000
*                                                                  0149 00843000
         L     @14,@PC00001+8                                      0149 00844000
         MVC   ECCECCHK(4,@08),IHSAMPCC(@14)                       0149 00845000
*   /*****************************************************************/ 00846000
*   /*                                                               */ 00847000
*   /* INITIALIZE CPE AND CDB TABLE STRUCTURES. FIRST POINT TO FIRST */ 00848000
*   /* DDB SLOT                                                      */ 00849000
*   /*                                                               */ 00850000
*   /*****************************************************************/ 00851000
*                                                                  0150 00852000
*   IHCDBPTR=ECCECPEQ+ECCECPUS*LENGTH(ECCPE);                      0150 00853000
         SLA   @11,3                                               0150 00854000
         AR    @12,@11                                             0150 00855000
         ST    @12,IHCDBPTR                                        0150 00856000
*   DO IHCPEIDX=1 TO ECCECPUS;      /* LOOP THROUGH CPU ENTRIES      */ 00857000
*                                                                  0151 00858000
         LR    IHCPEIDX,@15                                        0151 00859000
         B     @DE00151                                            0151 00860000
@DL00151 DS    0H                                                  0152 00861000
*     /***************************************************************/ 00862000
*     /*                                                             */ 00863000
*     /* ZERO CPE ENTRY                                              */ 00864000
*     /*                                                             */ 00865000
*     /***************************************************************/ 00866000
*                                                                  0152 00867000
*     ECCPE(IHCPEIDX)=ECCPE(IHCPEIDX)&&ECCPE(IHCPEIDX);            0152 00868000
         LR    @12,IHCPEIDX                                        0152 00869000
         SLA   @12,3                                               0152 00870000
         L     @11,@PC00001                                        0152 00871000
         L     @11,IHSMAPTR(,@11)                                  0152 00872000
         L     @11,STSMEDAD(,@11)                                  0152 00873000
         L     @11,ECCECPEQ(,@11)                                  0152 00874000
         ST    @12,@TF00001                                        0152 00875000
         ALR   @12,@11                                             0152 00876000
         AL    @12,@CF01570                                        0152 00877000
         XC    ECCPE(8,@12),ECCPE(@12)                             0152 00878000
*     ECCPCDBQ(IHCPEIDX)=IHCDBPTR;  /* POINT TO NEXT CDB TABLE SLOT  */ 00879000
*                                                                  0153 00880000
         L     @12,IHCDBPTR                                        0153 00881000
         L     @08,@TF00001                                        0153 00882000
         AL    @08,@CF01571                                        0153 00883000
         ST    @12,ECCPCDBQ-4(@08,@11)                             0153 00884000
*     /***************************************************************/ 00885000
*     /*                                                             */ 00886000
*     /* NUMBER OF CDB ENTRIES                                       */ 00887000
*     /*                                                             */ 00888000
*     /***************************************************************/ 00889000
*                                                                  0154 00890000
*     ECCPCNUM(IHCPEIDX)=CSDCHAD+1;                                0154 00891000
*                                                                  0154 00892000
         LA    @08,1                                               0154 00893000
         L     @02,CVTPTR                                          0154 00894000
         L     @02,CVTCSD(,@02)                                    0154 00895000
         LH    @02,CSDCHAD(,@02)                                   0154 00896000
         AR    @02,@08                                             0154 00897000
         L     @01,@TF00001                                        0154 00898000
         AL    @11,@CF01575                                        0154 00899000
         STH   @02,ECCPCNUM-2(@01,@11)                             0154 00900000
*     /***************************************************************/ 00901000
*     /*                                                             */ 00902000
*     /* POINT TO NEXT CDB TABLE SLOT                                */ 00903000
*     /*                                                             */ 00904000
*     /***************************************************************/ 00905000
*                                                                  0155 00906000
*     IHCDBPTR=IHCDBPTR+ECCPCNUM(IHCPEIDX)*LENGTH(ECCDB);          0155 00907000
*                                                                  0155 00908000
         MH    @02,@CH01564                                        0155 00909000
         AR    @12,@02                                             0155 00910000
         ST    @12,IHCDBPTR                                        0155 00911000
*     /***************************************************************/ 00912000
*     /*                                                             */ 00913000
*     /* ZERO ALL CDB ENTRIES                                        */ 00914000
*     /*                                                             */ 00915000
*     /***************************************************************/ 00916000
*                                                                  0156 00917000
*     DO IHCDBIDX=1 TO ECCPCNUM(IHCPEIDX);                         0156 00918000
         B     @DE00156                                            0156 00919000
@DL00156 DS    0H                                                  0157 00920000
*       ECCDB(IHCDBIDX)=ECCDB(IHCDBIDX)&&ECCDB(IHCDBIDX);          0157 00921000
         LR    @12,@08                                             0157 00922000
         MH    @12,@CH01564                                        0157 00923000
         LR    @11,IHCPEIDX                                        0157 00924000
         SLA   @11,3                                               0157 00925000
         L     @02,@PC00001                                        0157 00926000
         L     @02,IHSMAPTR(,@02)                                  0157 00927000
         L     @02,STSMEDAD(,@02)                                  0157 00928000
         L     @02,ECCECPEQ(,@02)                                  0157 00929000
         AL    @02,@CF01571                                        0157 00930000
         L     @11,ECCPCDBQ-4(@11,@02)                             0157 00931000
         ALR   @11,@12                                             0157 00932000
         AL    @11,@CF01576                                        0157 00933000
         XC    ECCDB(20,@11),ECCDB(@11)                            0157 00934000
*     END;                                                         0158 00935000
         AH    @08,@CH00048                                        0158 00936000
@DE00156 ST    @08,IHCDBIDX                                        0158 00937000
         LR    @12,IHCPEIDX                                        0158 00938000
         SLA   @12,3                                               0158 00939000
         L     @11,@PC00001                                        0158 00940000
         L     @11,IHSMAPTR(,@11)                                  0158 00941000
         L     @11,STSMEDAD(,@11)                                  0158 00942000
         L     @11,ECCECPEQ(,@11)                                  0158 00943000
         AL    @11,@CF01575                                        0158 00944000
         CH    @08,ECCPCNUM-2(@12,@11)                             0158 00945000
         BNH   @DL00156                                            0158 00946000
*   END;                                                           0159 00947000
*                                                                  0159 00948000
         AH    IHCPEIDX,@CH00048                                   0159 00949000
@DE00151 L     @12,@PC00001                                        0159 00950000
         L     @12,IHSMAPTR(,@12)                                  0159 00951000
         L     @12,STSMEDAD(,@12)                                  0159 00952000
         SR    @11,@11                                             0159 00953000
         IC    @11,ECCECPUS(,@12)                                  0159 00954000
         CR    IHCPEIDX,@11                                        0159 00955000
         BNH   @DL00151                                            0159 00956000
*   /*****************************************************************/ 00957000
*   /*                                                               */ 00958000
*   /* INITIALIZE CPE ENTRIES AND CDBS FOR VALID ONLINE CPUS         */ 00959000
*   /*                                                               */ 00960000
*   /*****************************************************************/ 00961000
*                                                                  0160 00962000
*   IHCPUMSK=IHCPUZER;              /* MASK FOR CPU ZERO             */ 00963000
         MVC   IHCPUMSK(2),@CH01548                                0160 00964000
*   DO IHCPEIDX=1 TO ECCECPUS;      /* LOOP THROUGH CPU ADDRESSES    */ 00965000
         LA    IHCPEIDX,1                                          0161 00966000
         B     @DE00161                                            0161 00967000
@DL00161 DS    0H                                                  0162 00968000
*     IF(CSDCPUAL&IHCPUMSK)^=0 THEN                                0162 00969000
         L     @12,CVTPTR                                          0162 00970000
         L     @11,CVTCSD(,@12)                                    0162 00971000
         SR    @08,@08                                             0162 00972000
         ICM   @08,3,IHCPUMSK                                      0162 00973000
         SR    @02,@02                                             0162 00974000
         ICM   @02,3,CSDCPUAL(@11)                                 0162 00975000
         NR    @08,@02                                             0162 00976000
         LTR   @08,@08                                             0162 00977000
         BZ    @RF00162                                            0162 00978000
*       DO;                         /* CPU WITH THIS ADDRESS IS    0163 00979000
*                                      ONLINE                        */ 00980000
*                                                                  0163 00981000
*         /***********************************************************/ 00982000
*         /*                                                         */ 00983000
*         /* INITIALIZE CPE AND CDBS FOR THIS CPU. PROVIDE           */ 00984000
*         /* ADDRESSABILITY TO PCCA                                  */ 00985000
*         /*                                                         */ 00986000
*         /***********************************************************/ 00987000
*                                                                  0164 00988000
*         PCCAPTR=PCCAT00P(IHCPEIDX);                              0164 00989000
*                                                                  0164 00990000
         LR    @11,IHCPEIDX                                        0164 00991000
         SLA   @11,2                                               0164 00992000
         L     @12,CVTPCCAT(,@12)                                  0164 00993000
         AL    @12,@CF01571                                        0164 00994000
         L     @12,PCCAT00P(@11,@12)                               0164 00995000
         ST    @12,PCCAPTR                                         0164 00996000
*         /***********************************************************/ 00997000
*         /*                                                         */ 00998000
*         /* INITIALIZE CDBS                                         */ 00999000
*         /*                                                         */ 01000000
*         /***********************************************************/ 01001000
*                                                                  0165 01002000
*         DO IHCDBIDX=1 TO ECCPCNUM(IHCPEIDX);                     0165 01003000
*                                                                  0165 01004000
         LA    @12,1                                               0165 01005000
         B     @DE00165                                            0165 01006000
@DL00165 DS    0H                                                  0166 01007000
*           /*********************************************************/ 01008000
*           /*                                                       */ 01009000
*           /* PROVIDE ADDRESSABILITY FOR CAT ENTRY                  */ 01010000
*           /*                                                       */ 01011000
*           /*********************************************************/ 01012000
*                                                                  0166 01013000
*           CATPTR=ADDR(PCCACAT)+(IHCDBIDX-1)*LENGTH(CAT);         0166 01014000
         L     @11,CVTPTR                                     3@ZP60030 01015000
         L     @11,1052(,@11) <=CVTCST   L     @11,PCCAPTR         0166 01016000
         L     @11,0(,@11)               LA    @11,PCCACAT(,@11)   0166 01017000
         LR    @08,@12                                             0166 01018000
         BCTR  @08,0                                               0166 01019000
         SLA   @08,4                     SLA   @08,3           @ZP60030 01020000
         AR    @11,@08                                             0166 01021000
         ST    @11,CATPTR                                          0166 01022000
*           /*********************************************************/ 01023000
*           /*                                                       */ 01024000
*           /* BYPASS IF CHANNEL IS NOT ONLINE                       */ 01025000
*           /*                                                       */ 01026000
*           /*********************************************************/ 01027000
*                                                                  0167 01028000
*           IF CATNOP='1'B THEN                                    0167 01029000
         TM    CATNOP(@11),B'01000000'                             0167 01030000
         BO    @RT00167                                            0167 01031000
*             GO TO IHLAB1;         /* GO TO NEXT CPU ADDRESS        */ 01032000
*                                                                  0168 01033000
*           /*********************************************************/ 01034000
*           /*                                                       */ 01035000
*           /* SET CHANNEL TYPE AND MODEL NUMBER                     */ 01036000
*           /*                                                       */ 01037000
*           /*********************************************************/ 01038000
*                                                                  0169 01039000
*           ECCDCHID(IHCDBIDX)=CATCHID(1:2);                       0169 01040000
*                                                                  0169 01041000
         MH    @12,@CH01564                                        0169 01042000
         LR    @08,IHCPEIDX                                        0169 01043000
         SLA   @08,3                                               0169 01044000
         L     @02,@PC00001                                        0169 01045000
         L     @02,IHSMAPTR(,@02)                                  0169 01046000
         L     @02,STSMEDAD(,@02)                                  0169 01047000
         L     @02,ECCECPEQ(,@02)                                  0169 01048000
         AL    @02,@CF01571                                        0169 01049000
         L     @08,ECCPCDBQ-4(@08,@02)                             0169 01050000
         ST    @12,@TF00001                                        0169 01051000
         ALR   @12,@08                                             0169 01052000
         AL    @12,@CF01569                                        0169 01053000
         MVC   ECCDCHID-4(2,@12),CATCHID(@11)                      0169 01054000
*           /*********************************************************/ 01055000
*           /*                                                       */ 01056000
*           /* SET CHANNEL TYPE FIELD VALIDITY FLAG                  */ 01057000
*           /*                                                       */ 01058000
*           /*********************************************************/ 01059000
*                                                                  0170 01060000
*           IF CATNID='1'B THEN                                    0170 01061000
*                                                                  0170 01062000
         TM    CATNID(@11),B'00001000'                             0170 01063000
         BNO   @RF00170                                            0170 01064000
*             /*******************************************************/ 01065000
*             /*                                                     */ 01066000
*             /* INVALID CHANNEL TYPE FIELD                          */ 01067000
*             /*                                                     */ 01068000
*             /*******************************************************/ 01069000
*                                                                  0171 01070000
*             ECCDIVID(IHCDBIDX)='1'B;                             0171 01071000
*                                                                  0171 01072000
         AL    @08,@TF00001                                        0171 01073000
         AL    @08,@CF01576                                        0171 01074000
         OI    ECCDIVID(@08),B'00001000'                           0171 01075000
*           /*********************************************************/ 01076000
*           /*                                                       */ 01077000
*           /* SET CURRENT SIO COUNT                                 */ 01078000
*           /*                                                       */ 01079000
*           /*********************************************************/ 01080000
*                                                                  0172 01081000
*           ECCDLSIO(IHCDBIDX)=CATSIOCT;                           0172 01082000
*                                                                  0172 01083000
@RF00170 L     @12,IHCDBIDX                                        0172 01084000
         MH    @12,@CH01564                                        0172 01085000
         LR    @11,IHCPEIDX                                        0172 01086000
         SLA   @11,3                                               0172 01087000
         L     @08,@PC00001                                        0172 01088000
         L     @08,IHSMAPTR(,@08)                                  0172 01089000
         L     @08,STSMEDAD(,@08)                                  0172 01090000
         L     @08,ECCECPEQ(,@08)                                  0172 01091000
         AL    @08,@CF01571                                        0172 01092000
         L     @11,ECCPCDBQ-4(@11,@08)                             0172 01093000
         ST    @12,@TF00001                                        0172 01094000
         ALR   @12,@11                                             0172 01095000
         AL    @12,@CF01578                                        0172 01096000
         L     @08,CATPTR                                          0172 01097000
         MVC   ECCDLSIO-6(2,@12),CATSIOCT(@08)                     0172 01098000
*           /*********************************************************/ 01099000
*           /*                                                       */ 01100000
*           /* VALID CHANNEL ENTRY NOW                               */ 01101000
*           /*                                                       */ 01102000
*           /*********************************************************/ 01103000
*                                                                  0173 01104000
*           ECCDVALD(IHCDBIDX)='1'B;                               0173 01105000
*                                                                  0173 01106000
*           /*********************************************************/ 01107000
*           /*                                                       */ 01108000
*           /* ONLINE                                                */ 01109000
*           /*                                                       */ 01110000
*           /*********************************************************/ 01111000
*                                                                  0174 01112000
*           ECCDALIV(IHCDBIDX)='1'B;                               0174 01113000
         AL    @11,@TF00001                                        0174 01114000
         AL    @11,@CF01576                                        0174 01115000
         OI    ECCDVALD(@11),B'00000101'                           0174 01116000
*IHLAB1:                            /* END OF CHANNEL INITIALIZATION    01117000
*                                      PER CPU ADDRESS               */ 01118000
*         END;                                                     0175 01119000
IHLAB1   LA    @12,1                                               0175 01120000
         A     @12,IHCDBIDX                                        0175 01121000
@DE00165 ST    @12,IHCDBIDX                                        0175 01122000
         LR    @11,IHCPEIDX                                        0175 01123000
         SLA   @11,3                                               0175 01124000
         L     @08,@PC00001                                        0175 01125000
         L     @08,IHSMAPTR(,@08)                                  0175 01126000
         L     @08,STSMEDAD(,@08)                                  0175 01127000
         L     @08,ECCECPEQ(,@08)                                  0175 01128000
         AL    @08,@CF01575                                        0175 01129000
         CH    @12,ECCPCNUM-2(@11,@08)                             0175 01130000
         BNH   @DL00165                                            0175 01131000
*         ECCPVALD(IHCPEIDX)='1'B;  /* MARK CPU ENTRY VALID NOW      */ 01132000
         LR    @12,IHCPEIDX                                        0176 01133000
         SLA   @12,3                                               0176 01134000
         L     @11,@PC00001                                        0176 01135000
         L     @11,IHSMAPTR(,@11)                                  0176 01136000
         L     @11,STSMEDAD(,@11)                                  0176 01137000
         L     @11,ECCECPEQ(,@11)                                  0176 01138000
         ALR   @11,@12                                             0176 01139000
         AL    @11,@CF01570                                        0176 01140000
         OI    ECCPVALD(@11),B'00000001'                           0176 01141000
*       END;                                                       0177 01142000
*     IHCPUMSK=IHCPUMSK/2;          /* SHIFT CPU MASK BIT TO RIGHT 0178 01143000
*                                      ONE POSITION FOR NEXT CPU     */ 01144000
@RF00162 SR    @12,@12                                             0178 01145000
         ICM   @12,3,IHCPUMSK                                      0178 01146000
         SRL   @12,1                                               0178 01147000
         STH   @12,IHCPUMSK                                        0178 01148000
*   END;                                                           0179 01149000
*                                                                  0179 01150000
         AH    IHCPEIDX,@CH00048                                   0179 01151000
@DE00161 L     @12,@PC00001                                        0179 01152000
         L     @12,IHSMAPTR(,@12)                                  0179 01153000
         L     @12,STSMEDAD(,@12)                                  0179 01154000
         SR    @11,@11                                             0179 01155000
         IC    @11,ECCECPUS(,@12)                                  0179 01156000
         CR    IHCPEIDX,@11                                        0179 01157000
         BNH   @DL00161                                            0179 01158000
*   /*****************************************************************/ 01159000
*   /*                                                               */ 01160000
*   /* CALCULATE LENGTH AND SUBPOOL OF INTERVAL DATA AREA AND STORE  */ 01161000
*   /* INTO THE SMA LENGTH                                           */ 01162000
*   /*                                                               */ 01163000
*   /*****************************************************************/ 01164000
*                                                                  0180 01165000
*   STSMILEN=LENGTH(STSMIGMC)+LENGTH(SMFRCD73)+LENGTH(SMF73A)+(CVTMAXMP 01166000
*       +1)*(CSDCHAD+1)*LENGTH(SMF73B);                            0180 01167000
         L     @12,@PC00001                                        0180 01168000
         L     @12,IHSMAPTR(,@12)                                  0180 01169000
         LA    @11,1                                               0180 01170000
         L     @08,CVTPTR                                          0180 01171000
         LH    @01,CVTMAXMP(,@08)                                  0180 01172000
         AR    @01,@11                                             0180 01173000
         L     @08,CVTCSD(,@08)                                    0180 01174000
         AH    @11,CSDCHAD(,@08)                                   0180 01175000
         MR    @00,@11                                             0180 01176000
         SLA   @01,4                                               0180 01177000
         AH    @01,@CH01567                                        0180 01178000
         STCM  @01,7,STSMILEN(@12)                                 0180 01179000
*   RETURN CODE('2C'X);             /* INDICATE THAT CALLER MUST   0181 01180000
*                                      ENABLE THE MFROUTER,        0181 01181000
*                                      INITIALIZE IOS AND ENQ THE  0181 01182000
*                                      SYSTEM TQE                    */ 01183000
         LA    @12,44                                              0181 01184000
         L     @13,4(,@13)                                         0181 01185000
         L     @00,@SIZDATD                                        0181 01186000
         LR    @01,@09                                             0181 01187000
         FREEMAIN R,LV=(0),A=(1)                                        01188000
         LR    @15,@12                                             0181 01189000
         L     @14,12(,@13)                                        0181 01190000
         LM    @00,@12,20(@13)                                     0181 01191000
         BR    @14                                                 0181 01192000
*   END                                                            0182 01193000
*                                                                  0182 01194000
*/* THE FOLLOWING INCLUDE STATEMENTS WERE FOUND IN THIS PROGRAM.     */ 01195000
*/*%INCLUDE SYSLIB  (IFASMFR )                                       */ 01196000
*/*%INCLUDE SYSLIB  (CVT     )                                       */ 01197000
*/*%INCLUDE SYSLIB  (IHAASCB )                                       */ 01198000
*/*%INCLUDE SYSLIB  (IHACSD  )                                       */ 01199000
*/*%INCLUDE SYSLIB  (IHAPCCAT)                                       */ 01200000
*/*%INCLUDE SYSLIB  (IHAPCCA )                                       */ 01201000
*/*%INCLUDE SYSLIB  (IECDCAT )                                       */ 01202000
*/*%INCLUDE SYSLIB  (IHAPSA  )                                       */ 01203000
*                                                                  0182 01204000
*       ;                                                          0182 01205000
@DATA    DS    0H                                                       01206000
@CH00044 DC    H'8'                                                     01207000
@CH00187 DC    H'16'                                                    01208000
@CH01564 DC    H'20'                                                    01209000
@CH01567 DC    H'60'                                                    01210000
@CH01548 DC    XL2'8000'                                                01211000
@DATD    DSECT                                                          01212000
         DS    0F                                                       01213000
@SA00001 DS    18F                                                      01214000
@PC00001 DS    3F                                                       01215000
@TF00001 DS    F                                                        01216000
IRBMFIHA CSECT                                                          01217000
         DS    0F                                                       01218000
@CF01574 DC    F'-24'                                                   01219000
@CF01576 DC    F'-20'                                                   01220000
@CF01569 DC    F'-16'                                                   01221000
@CF01578 DC    F'-14'                                                   01222000
@CF01570 DC    F'-8'                                                    01223000
@CF01575 DC    F'-6'                                                    01224000
@CF01571 DC    F'-4'                                                    01225000
@CF00048 DC    F'1'                                                     01226000
@CH00048 EQU   @CF00048+2                                               01227000
@CF01540 DC    XL4'42000000'                                            01228000
@DATD    DSECT                                                          01229000
         DS    0D                                                       01230000
PCCAPTR  DS    A                                                        01231000
CATPTR   DS    A                                                        01232000
IHCDBIDX DS    F                                                        01233000
IHCDBPTR DS    A                                                        01234000
IHPAGECB DS    F                                                        01235000
IHCPUMSK DS    H                                                        01236000
         DS    CL2                                                      01237000
IHEVTGMC DS    CL4                                                      01238000
         ORG   IHEVTGMC                                                 01239000
IHEVTSBP DS    FL1                                                      01240000
IHEVTLEN DS    FL3                                                      01241000
         ORG   IHEVTGMC+4                                               01242000
         ORG   *+1-(*-@DATD)/(*-@DATD) INSURE DSECT DATA                01243000
@ENDDATD EQU   *                                                        01244000
IRBMFIHA CSECT                                                          01245000
         DS    0F                                                       01246000
@SIZDATD DC    AL1(0)                                                   01247000
         DC    AL3(@ENDDATD-@DATD)                                      01248000
         DS    0D                                                       01249000
@CC01549 DC    C'IRBMFEVT'                                              01250000
@CC01550 DC    C'IRBMFECH'                                              01251000
@CC01553 DC    C'IRBMFTCH'                                              01252000
@CC01556 DC    C'IRBMFDHP'                                              01253000
IHSGTGMC DS    CL4                                                      01254000
         ORG   IHSGTGMC                                                 01255000
IHSGTSBP DC    AL1(0)                                                   01256000
IHSGTLEN DC    AL3(4+8+8)                                               01257000
         ORG   IHSGTGMC+4                                               01258000
IHPRTGMC DS    CL4                                                      01259000
         ORG   IHPRTGMC                                                 01260000
IHPRTSBP DC    AL1(0)                                                   01261000
IHPRTLEN DC    AL3(4+16+16+16+16)                                       01262000
         ORG   IHPRTGMC+4                                               01263000
IHPATCH  DS    CL100                                                    01264000
@00      EQU   00                      EQUATES FOR REGISTERS 0-15       01265000
@01      EQU   01                                                       01266000
@02      EQU   02                                                       01267000
@03      EQU   03                                                       01268000
@04      EQU   04                                                       01269000
@05      EQU   05                                                       01270000
@06      EQU   06                                                       01271000
@07      EQU   07                                                       01272000
@08      EQU   08                                                       01273000
@09      EQU   09                                                       01274000
@10      EQU   10                                                       01275000
@11      EQU   11                                                       01276000
@12      EQU   12                                                       01277000
@13      EQU   13                                                       01278000
@14      EQU   14                                                       01279000
@15      EQU   15                                                       01280000
IHCPEIDX EQU   @03                                                      01281000
GPR00P   EQU   @00                                                      01282000
GPR01P   EQU   @01                                                      01283000
GPR14P   EQU   @14                                                      01284000
GPR15P   EQU   @15                                                      01285000
GPR02P   EQU   @02                                                      01286000
GPR04P   EQU   @04                                                      01287000
GPR11P   EQU   @11                                                      01288000
GPR12P   EQU   @12                                                      01289000
GPR13P   EQU   @13                                                      01290000
GPR00F   EQU   @00                                                      01291000
GPR01F   EQU   @01                                                      01292000
GPR14F   EQU   @14                                                      01293000
GPR15F   EQU   @15                                                      01294000
CVTPTR   EQU   16                                                       01295000
PSA      EQU   0                                                        01296000
FLCRNPSW EQU   PSA                                                      01297000
FLCROPSW EQU   PSA+8                                                    01298000
FLCICCW2 EQU   PSA+16                                                   01299000
PSAEEPSW EQU   PSA+132                                                  01300000
PSAESPSW EQU   PSA+136                                                  01301000
FLCSVILC EQU   PSAESPSW+1                                               01302000
PSAEPPSW EQU   PSA+140                                                  01303000
FLCPIILC EQU   PSAEPPSW+1                                               01304000
FLCPICOD EQU   PSAEPPSW+2                                               01305000
PSAPICOD EQU   FLCPICOD+1                                               01306000
FLCTEA   EQU   PSAEPPSW+4                                               01307000
FLCPER   EQU   PSA+152                                                  01308000
FLCMCLA  EQU   PSA+168                                                  01309000
FLCIOEL  EQU   FLCMCLA+4                                                01310000
FLCIOA   EQU   FLCMCLA+16                                               01311000
FLCFSA   EQU   FLCMCLA+80                                               01312000
PSAAOLD  EQU   PSA+548                                                  01313000
PSASUPER EQU   PSA+552                                                  01314000
PSASUP1  EQU   PSASUPER                                                 01315000
PSASUP2  EQU   PSASUPER+1                                               01316000
PSASUP3  EQU   PSASUPER+2                                               01317000
PSASUP4  EQU   PSASUPER+3                                               01318000
PSACLHT  EQU   PSA+640                                                  01319000
PSALKSA  EQU   PSA+696                                                  01320000
PSAHLHI  EQU   PSA+760                                                  01321000
PSALITA  EQU   PSA+764                                                  01322000
PSADSSFL EQU   PSA+836                                                  01323000
PSADSSF1 EQU   PSADSSFL                                                 01324000
PSADSSF3 EQU   PSADSSFL+2                                               01325000
PSADSSF4 EQU   PSADSSFL+3                                               01326000
PSARSVT  EQU   PSA+896                                                  01327000
STSMA    EQU   0                                                        01328000
STSMOPT  EQU   STSMA                                                    01329000
STSMOFLG EQU   STSMOPT+3                                                01330000
STSMSTA  EQU   STSMOFLG                                                 01331000
STSMINTP EQU   STSMA+20                                                 01332000
STSMEDAD EQU   STSMA+32                                                 01333000
STSMIGMC EQU   STSMA+36                                                 01334000
STSMILEN EQU   STSMIGMC+1                                               01335000
STSMRVT  EQU   STSMA+44                                                 01336000
STMMEVTL EQU   0                                                        01337000
STMMEVNT EQU   STMMEVTL                                                 01338000
STMMNXMG EQU   STMMEVTL+4                                               01339000
STMMMGRL EQU   0                                                        01340000
STMMMGAD EQU   STMMMGRL                                                 01341000
STMMMGDA EQU   STMMMGRL+4                                               01342000
STRVT    EQU   0                                                        01343000
STRVSPRT EQU   STRVT                                                    01344000
STRVNPRT EQU   STRVT+4                                                  01345000
STRVSSGT EQU   STRVT+8                                                  01346000
STRVNSGT EQU   STRVT+12                                                 01347000
STPRT    EQU   0                                                        01348000
STPRNAME EQU   STPRT                                                    01349000
STPRADDR EQU   STPRT+8                                                  01350000
STPRLGTH EQU   STPRT+12                                                 01351000
STSGT    EQU   0                                                        01352000
STSGFREE EQU   STSGT                                                    01353000
STSGADD  EQU   STSGT+4                                                  01354000
SMFRCD73 EQU   0                                                        01355000
SMF73SIZ EQU   SMFRCD73+18                                              01356000
SMF73PTR EQU   0                                                        01357000
SMF73A   EQU   0                                                        01358000
SMF73SHC EQU   SMF73A                                                   01359000
SMF73B   EQU   0                                                        01360000
SMF73FG2 EQU   SMF73B+3                                                 01361000
ECCEDT   EQU   0                                                        01362000
ECCECPEQ EQU   ECCEDT                                                   01363000
ECCECPUS EQU   ECCEDT+7                                                 01364000
ECCECCHK EQU   ECCEDT+12                                                01365000
ECCPE    EQU   0                                                        01366000
ECCPFLGS EQU   ECCPE                                                    01367000
ECCPVALD EQU   ECCPFLGS                                                 01368000
ECCPCNUM EQU   ECCPE+2                                                  01369000
ECCPCDBQ EQU   ECCPE+4                                                  01370000
ECCDB    EQU   0                                                        01371000
ECCDFLGS EQU   ECCDB                                                    01372000
ECCDIVID EQU   ECCDFLGS                                                 01373000
ECCDVALD EQU   ECCDFLGS                                                 01374000
ECCDALIV EQU   ECCDFLGS                                                 01375000
ECCDCHID EQU   ECCDB+4                                                  01376000
ECCDLSIO EQU   ECCDB+6                                                  01377000
CVTMAP   EQU   0                                                        01378000
CVTDAR   EQU   CVTMAP+72                                                01379000
CVTFLGS1 EQU   CVTDAR                                                   01380000
CVTDCB   EQU   CVTMAP+116                                               01381000
CVTIERLC EQU   CVTMAP+144                                               01382000
CVTOPTA  EQU   CVTMAP+182                                               01383000
CVTOPTB  EQU   CVTMAP+183                                               01384000
CVTGTF   EQU   CVTMAP+236                                               01385000
CVTGTFST EQU   CVTGTF                                                   01386000
CVTGTFS  EQU   CVTGTFST                                                 01387000
CVTSTATE EQU   CVTGTFST                                                 01388000
CVTTMODE EQU   CVTGTFST                                                 01389000
CVTFORM  EQU   CVTGTFST                                                 01390000
CVTUSR   EQU   CVTGTFST                                                 01391000
CVTAQAVT EQU   CVTMAP+240                                               01392000
CVTTCMFG EQU   CVTAQAVT                                                 01393000
CVTVOLM2 EQU   CVTMAP+244                                               01394000
CVTTATA  EQU   CVTVOLM2                                                 01395000
CVTTSKS  EQU   CVTTATA                                                  01396000
CVTVOLF2 EQU   CVTTSKS                                                  01397000
CVTTAT   EQU   CVTTATA+1                                                01398000
CVTATER  EQU   CVTMAP+248                                               01399000
CVTEXT1  EQU   CVTMAP+252                                               01400000
CVTPURG  EQU   CVTMAP+260                                               01401000
CVTQMSG  EQU   CVTMAP+268                                               01402000
CVTDMSR  EQU   CVTMAP+272                                               01403000
CVTERPV  EQU   CVTMAP+316                                               01404000
CVTINTLA EQU   CVTMAP+320                                               01405000
CVTAPF   EQU   CVTMAP+324                                               01406000
CVTEXT2  EQU   CVTMAP+328                                               01407000
CVTHJES  EQU   CVTMAP+332                                               01408000
CVTPGSIA EQU   CVTMAP+348                                               01409000
CVTA1F1  EQU   CVTMAP+356                                               01410000
CVTSYSK  EQU   CVTMAP+357                                               01411000
CVTVOLM1 EQU   CVTMAP+380                                               01412000
CVTVOLF1 EQU   CVTVOLM1                                                 01413000
CVTATMCT EQU   CVTMAP+388                                               01414000
CVTXTNT1 EQU   0                                                        01415000
CVTXTNT2 EQU   0                                                        01416000
CVTDSSV  EQU   CVTXTNT2                                                 01417000
CVTRSV90 EQU   CVTXTNT2+5                                               01418000
CVTQID   EQU   CVTXTNT2+24                                              01419000
CVTRV400 EQU   CVTXTNT2+52                                              01420000
CVTRV409 EQU   CVTXTNT2+53                                              01421000
CVTATCVT EQU   CVTXTNT2+64                                              01422000
CVTRV429 EQU   CVTXTNT2+84                                              01423000
CVTRV438 EQU   CVTXTNT2+85                                              01424000
CVTRV457 EQU   CVTXTNT2+112                                             01425000
CVTRV466 EQU   CVTXTNT2+113                                             01426000
CVTFIX   EQU   0                                                        01427000
CVTRELNO EQU   CVTFIX+252                                               01428000
ASCB     EQU   0                                                        01429000
ASCBFW1  EQU   ASCB+100                                                 01430000
ASCBRCTF EQU   ASCBFW1+2                                                01431000
ASCBFLG1 EQU   ASCBFW1+3                                                01432000
ASCBNSWP EQU   ASCBFLG1                                                 01433000
ASCBDSP1 EQU   ASCB+114                                                 01434000
ASCBFLG2 EQU   ASCB+115                                                 01435000
CSD      EQU   0                                                        01436000
CSDCHAD  EQU   CSD+6                                                    01437000
CSDCPUAL EQU   CSD+8                                                    01438000
CSDSCWRD EQU   CSD+12                                                   01439000
CSDSCFL1 EQU   CSDSCWRD                                                 01440000
CSDSCFL2 EQU   CSDSCWRD+1                                               01441000
CSDSCFL3 EQU   CSDSCWRD+2                                               01442000
CSDSCFL4 EQU   CSDSCWRD+3                                               01443000
CSDFLAGS EQU   CSD+23                                                   01444000
PCCAVT   EQU   0                                                        01445000
PCCAT00P EQU   PCCAVT                                                   01446000
PCCA     EQU   0                                                        01447000
PCCATMST EQU   PCCA+128                                                 01448000
PCCATMFL EQU   PCCATMST                                                 01449000
PCCATODE EQU   PCCATMST+1                                               01450000
PCCACCE  EQU   PCCATMST+2                                               01451000
PCCAINTE EQU   PCCATMST+3                                               01452000
PCCAEMSB EQU   PCCA+136                                                 01453000
PCCAEMSI EQU   PCCAEMSB                                                 01454000
PCCARISP EQU   PCCAEMSI                                                 01455000
PCCAEMS2 EQU   PCCAEMSI+1                                               01456000
PCCAEMS3 EQU   PCCAEMSI+2                                               01457000
PCCARMSB EQU   PCCAEMSI+3                                               01458000
PCCAWERP EQU   PCCA+280                                                 01459000
PCCACHPF EQU   PCCAWERP+4                                               01460000
PCCACHBL EQU   PCCAWERP+5                                               01461000
PCCACHVA EQU   PCCAWERP+6                                               01462000
PCCACHTS EQU   PCCAWERP+7                                               01463000
PCCACHS1 EQU   PCCA+288                                                 01464000
PCCACHS2 EQU   PCCA+289                                                 01465000
PCCACHRB EQU   PCCA+290                                                 01466000
PCCACHF1 EQU   PCCA+308                                                 01467000
PCCACHF2 EQU   PCCA+309                                                 01468000
PCCACHF3 EQU   PCCA+310                                                 01469000
PCCACHF4 EQU   PCCA+311                                                 01470000
PCCACAT  EQU   PCCA+384                                                 01471000
CAT      EQU   0                                                        01472000
CATENTRY EQU   CAT                                                      01473000
CATFLG   EQU   CATENTRY                                                 01474000
CATNOP   EQU   CATFLG                                                   01475000
CATNID   EQU   CATFLG                                                   01476000
CATFLA   EQU   CAT+1                                                    01477000
CATSIOCT EQU   CAT+2                                                    01478000
CATCHID  EQU   CAT+4                                                    01479000
IHGMCWRD EQU   0                                                        01480000
IEAVPSIB EQU   0                                                        01481000
IKEBC    EQU   0                                                        01482000
IKEBF15  EQU   0                                                        01483000
IKEBF31  EQU   0                                                        01484000
IKEBP15  EQU   0                                                        01485000
IKEBP31  EQU   0                                                        01486000
IKEBP8   EQU   0                                                        01487000
IHSMAPTR EQU   0                                                        01488000
IHMMVPTR EQU   0                                                        01489000
IHSAMPCC EQU   0                                                        01490000
STSMB    EQU   STSMOPT                                                  01491000
@NM00004 EQU   STSMB+3                                                  01492000
STSMDEVF EQU   @NM00004                                                 01493000
STSMC    EQU   STSMOPT                                                  01494000
@NM00006 EQU   STSMC+3                                                  01495000
CVTS01   EQU   CVTPGSIA                                                 01496000
CVTLPDIA EQU   CVTS01+12                                                01497000
CVTDIRST EQU   CVTLPDIA                                                 01498000
CVTSLIDA EQU   CVTS01+24                                                01499000
CVTCTLFG EQU   CVTS01+50                                                01500000
CVTMAXMP EQU   CVTS01+128                                               01501000
CVTCSD   EQU   CVTS01+312                                               01502000
CVTPCCAT EQU   CVTS01+416                                               01503000
CVTRV210 EQU   CVTS01+424                                               01504000
CVTRV219 EQU   CVTS01+425                                               01505000
CVTRV228 EQU   CVTS01+426                                               01506000
CVTRV237 EQU   CVTS01+427                                               01507000
CVTMFRTR EQU   CVTS01+452                                               01508000
CVTVPSIB EQU   CVTS01+456                                               01509000
CVTRV262 EQU   CVTS01+468                                               01510000
CVTRV271 EQU   CVTS01+469                                               01511000
CVTRV280 EQU   CVTS01+470                                               01512000
CVTRV289 EQU   CVTS01+471                                               01513000
CVTGSDA  EQU   CVTS01+600                                               01514000
ASCBPTR  EQU   PSAAOLD                                                  01515000
PSARSVTE EQU   PSARSVT                                                  01516000
FLC      EQU   PSA                                                      01517000
         AGO   .@UNREFD                START UNREFERENCED COMPONENTS    01518000
PSARSAV  EQU   PSARSVTE+60                                              01519000
PSARSTK  EQU   PSARSVTE+56                                              01520000
PSAESAV3 EQU   PSARSVTE+52                                              01521000
PSAESTK3 EQU   PSARSVTE+48                                              01522000
PSAESAV2 EQU   PSARSVTE+44                                              01523000
PSAESTK2 EQU   PSARSVTE+40                                              01524000
PSAESAV1 EQU   PSARSVTE+36                                              01525000
PSAESTK1 EQU   PSARSVTE+32                                              01526000
PSAPSAV  EQU   PSARSVTE+28                                              01527000
PSAPSTK  EQU   PSARSVTE+24                                              01528000
PSAMSAV  EQU   PSARSVTE+20                                              01529000
PSAMSTK  EQU   PSARSVTE+16                                              01530000
PSASSAV  EQU   PSARSVTE+12                                              01531000
PSASSTK  EQU   PSARSVTE+8                                               01532000
PSANSTK  EQU   PSARSVTE+4                                               01533000
PSACSTK  EQU   PSARSVTE                                                 01534000
CVTTPIO  EQU   CVTS01+608                                               01535000
CVTADV   EQU   CVTS01+604                                               01536000
CVTGSDAB EQU   CVTGSDA                                                  01537000
CVTQV3   EQU   CVTS01+596                                               01538000
CVTQV2   EQU   CVTS01+592                                               01539000
CVTQV1   EQU   CVTS01+588                                               01540000
CVTRPT   EQU   CVTS01+584                                               01541000
CVTSSRB  EQU   CVTS01+580                                               01542000
CVTCSDRL EQU   CVTS01+576                                               01543000
CVTEXP1  EQU   CVTS01+572                                               01544000
CVTRMPMT EQU   CVTS01+568                                               01545000
CVTRMPTT EQU   CVTS01+564                                               01546000
CVTVPSA  EQU   CVTS01+560                                               01547000
CVTVSTOP EQU   CVTS01+556                                               01548000
CVTGTFR8 EQU   CVTS01+552                                               01549000
CVTQUIT  EQU   CVTS01+548                                               01550000
CVTVACR  EQU   CVTS01+544                                               01551000
CVTWTCB  EQU   CVTS01+540                                               01552000
CVTSTPRS EQU   CVTS01+536                                               01553000
CVT0PT02 EQU   CVTS01+532                                               01554000
CVTDARCM EQU   CVTS01+528                                               01555000
CVTIRECM EQU   CVTS01+524                                               01556000
CVTJRECM EQU   CVTS01+520                                               01557000
CVTVEMS0 EQU   CVTS01+516                                               01558000
CVTSPFRR EQU   CVTS01+512                                               01559000
CVTRLSTG EQU   CVTS01+508                                               01560000
CVT0TC0A EQU   CVTS01+504                                               01561000
CVTGMBR  EQU   CVTS01+500                                               01562000
CVTLFRM  EQU   CVTS01+496                                               01563000
CVTRMBR  EQU   CVTS01+492                                               01564000
CVTVIOP  EQU   CVTS01+488                                               01565000
CVTRV307 EQU   CVTS01+486                                               01566000
CVTRV306 EQU   CVTS01+484                                               01567000
CVTRV305 EQU   CVTS01+482                                               01568000
CVTRV304 EQU   CVTS01+480                                               01569000
CVTRV303 EQU   CVTS01+478                                               01570000
CVTRV302 EQU   CVTS01+476                                               01571000
CVTRV301 EQU   CVTS01+475                                               01572000
CVTRV300 EQU   CVTS01+474                                               01573000
CVTRV299 EQU   CVTS01+473                                               01574000
CVTRV298 EQU   CVTS01+472                                               01575000
CVTRV297 EQU   CVTRV289                                                 01576000
CVTRV296 EQU   CVTRV289                                                 01577000
CVTRV295 EQU   CVTRV289                                                 01578000
CVTRV294 EQU   CVTRV289                                                 01579000
CVTRV293 EQU   CVTRV289                                                 01580000
CVTRV292 EQU   CVTRV289                                                 01581000
CVTRV291 EQU   CVTRV289                                                 01582000
CVTRV290 EQU   CVTRV289                                                 01583000
CVTRV288 EQU   CVTRV280                                                 01584000
CVTRV287 EQU   CVTRV280                                                 01585000
CVTRV286 EQU   CVTRV280                                                 01586000
CVTRV285 EQU   CVTRV280                                                 01587000
CVTRV284 EQU   CVTRV280                                                 01588000
CVTRV283 EQU   CVTRV280                                                 01589000
CVTRV282 EQU   CVTRV280                                                 01590000
CVTRV281 EQU   CVTRV280                                                 01591000
CVTRV279 EQU   CVTRV271                                                 01592000
CVTRV278 EQU   CVTRV271                                                 01593000
CVTRV277 EQU   CVTRV271                                                 01594000
CVTRV276 EQU   CVTRV271                                                 01595000
CVTRV275 EQU   CVTRV271                                                 01596000
CVTRV274 EQU   CVTRV271                                                 01597000
CVTRV273 EQU   CVTRV271                                                 01598000
CVTRV272 EQU   CVTRV271                                                 01599000
CVTRV270 EQU   CVTRV262                                                 01600000
CVTRV269 EQU   CVTRV262                                                 01601000
CVTRV268 EQU   CVTRV262                                                 01602000
CVTRV267 EQU   CVTRV262                                                 01603000
CVTRV266 EQU   CVTRV262                                                 01604000
CVTRV265 EQU   CVTRV262                                                 01605000
CVTRV264 EQU   CVTRV262                                                 01606000
CVTRV263 EQU   CVTRV262                                                 01607000
CVTVFP   EQU   CVTS01+464                                               01608000
CVTVSI   EQU   CVTS01+460                                               01609000
CVTMFACT EQU   CVTMFRTR                                                 01610000
CVTMFCTL EQU   CVTS01+448                                               01611000
CVTPVBP  EQU   CVTS01+444                                               01612000
CVTPWI   EQU   CVTS01+440                                               01613000
CVTRV254 EQU   CVTS01+438                                               01614000
CVTRV253 EQU   CVTS01+436                                               01615000
CVTRV252 EQU   CVTS01+434                                               01616000
CVTRV251 EQU   CVTS01+433                                               01617000
CVTRV250 EQU   CVTS01+432                                               01618000
CVTRV249 EQU   CVTS01+431                                               01619000
CVTRV248 EQU   CVTS01+430                                               01620000
CVTRV247 EQU   CVTS01+429                                               01621000
CVTRV246 EQU   CVTS01+428                                               01622000
CVTRV245 EQU   CVTRV237                                                 01623000
CVTRV244 EQU   CVTRV237                                                 01624000
CVTRV243 EQU   CVTRV237                                                 01625000
CVTRV242 EQU   CVTRV237                                                 01626000
CVTRV241 EQU   CVTRV237                                                 01627000
CVTRV240 EQU   CVTRV237                                                 01628000
CVTRV239 EQU   CVTRV237                                                 01629000
CVTRV238 EQU   CVTRV237                                                 01630000
CVTRV236 EQU   CVTRV228                                                 01631000
CVTRV235 EQU   CVTRV228                                                 01632000
CVTRV234 EQU   CVTRV228                                                 01633000
CVTRV233 EQU   CVTRV228                                                 01634000
CVTRV232 EQU   CVTRV228                                                 01635000
CVTRV231 EQU   CVTRV228                                                 01636000
CVTRV230 EQU   CVTRV228                                                 01637000
CVTRV229 EQU   CVTRV228                                                 01638000
CVTRV227 EQU   CVTRV219                                                 01639000
CVTRV226 EQU   CVTRV219                                                 01640000
CVTRV225 EQU   CVTRV219                                                 01641000
CVTRV224 EQU   CVTRV219                                                 01642000
CVTRV223 EQU   CVTRV219                                                 01643000
CVTRV222 EQU   CVTRV219                                                 01644000
CVTRV221 EQU   CVTRV219                                                 01645000
CVTRV220 EQU   CVTRV219                                                 01646000
CVTRV218 EQU   CVTRV210                                                 01647000
CVTRV217 EQU   CVTRV210                                                 01648000
CVTRV216 EQU   CVTRV210                                                 01649000
CVTRV215 EQU   CVTRV210                                                 01650000
CVTRV214 EQU   CVTRV210                                                 01651000
CVTRV213 EQU   CVTRV210                                                 01652000
CVTRV212 EQU   CVTRV210                                                 01653000
CVTRV211 EQU   CVTRV210                                                 01654000
CVTLCCAT EQU   CVTS01+420                                               01655000
CVTIPCRP EQU   CVTS01+412                                               01656000
CVTIPCRI EQU   CVTS01+408                                               01657000
CVTIPCDS EQU   CVTS01+404                                               01658000
CVTAIDVT EQU   CVTS01+400                                               01659000
CVTSSAP  EQU   CVTS01+396                                               01660000
CVTEHCIR EQU   CVTS01+392                                               01661000
CVTEHDEF EQU   CVTS01+388                                               01662000
CVTDAIR  EQU   CVTS01+384                                               01663000
CVTPERFM EQU   CVTS01+380                                               01664000
CVT044R2 EQU   CVTS01+376                                               01665000
CVTFETCH EQU   CVTS01+372                                               01666000
CVTRSTWD EQU   CVTS01+368                                               01667000
CVTSPOST EQU   CVTS01+364                                               01668000
CVTIOBP  EQU   CVTS01+360                                               01669000
CVTASMVT EQU   CVTS01+356                                               01670000
CVTRECRQ EQU   CVTS01+352                                               01671000
CVTWSAC  EQU   CVTS01+348                                               01672000
CVTRV149 EQU   CVTS01+344                                               01673000
CVTWSAL  EQU   CVTS01+340                                               01674000
CVTSPSA  EQU   CVTS01+336                                               01675000
CVTGLMN  EQU   CVTS01+332                                               01676000
CVTVEAC0 EQU   CVTS01+328                                               01677000
CVT062R1 EQU   CVTS01+324                                               01678000
CVTRPOST EQU   CVTS01+320                                               01679000
CVTDQIQE EQU   CVTS01+316                                               01680000
CVTLKRMA EQU   CVTS01+308                                               01681000
CVTRSPIE EQU   CVTS01+304                                               01682000
CVTRENQ  EQU   CVTS01+300                                               01683000
CVTLQCB  EQU   CVTS01+296                                               01684000
CVTFQCB  EQU   CVTS01+292                                               01685000
CVTQCS01 EQU   CVTS01+288                                               01686000
CVTAPFT  EQU   CVTS01+284                                               01687000
CVTPARRL EQU   CVTS01+280                                               01688000
CVTVWAIT EQU   CVTS01+276                                               01689000
CVTGSPL  EQU   CVTS01+272                                               01690000
CVTLSMQ  EQU   CVTS01+268                                               01691000
CVTGSMQ  EQU   CVTS01+264                                               01692000
CVTEXPRO EQU   CVTS01+260                                               01693000
CVTOPCTP EQU   CVTS01+256                                               01694000
CVTSIC   EQU   CVTS01+252                                               01695000
CVTTPIOS EQU   CVTS01+248                                               01696000
CVTRTMS  EQU   CVTS01+244                                               01697000
CVTSDBF  EQU   CVTS01+240                                               01698000
CVTSCBP  EQU   CVTS01+236                                               01699000
CVTSDMP  EQU   CVTS01+232                                               01700000
CVTSV60  EQU   CVTS01+228                                               01701000
CVTRTMCT EQU   CVTS01+224                                               01702000
CVTASCBL EQU   CVTS01+220                                               01703000
CVTASCBH EQU   CVTS01+216                                               01704000
CVTGDA   EQU   CVTS01+212                                               01705000
CVTASVT  EQU   CVTS01+208                                               01706000
CVTVVMDI EQU   CVTS01+204                                               01707000
CVTAQTOP EQU   CVTS01+200                                               01708000
CVTIOSCS EQU   CVTS01+196                                               01709000
CVTSDRM  EQU   CVTS01+192                                               01710000
CVTOPTE  EQU   CVTS01+188                                               01711000
CVTSTXU  EQU   CVTS01+184                                               01712000
CVTQUIS  EQU   CVTS01+180                                               01713000
CVTPARS  EQU   CVTS01+176                                               01714000
CVTS1EE  EQU   CVTS01+172                                               01715000
CVTFRAS  EQU   CVTS01+168                                               01716000
CVTQSAS  EQU   CVTS01+164                                               01717000
CVTCRAS  EQU   CVTS01+160                                               01718000
CVTCRMN  EQU   CVTS01+156                                               01719000
CVTDELCP EQU   CVTS01+152                                               01720000
CVTFRECL EQU   CVTS01+148                                               01721000
CVTGETCL EQU   CVTS01+144                                               01722000
CVTBLDCP EQU   CVTS01+140                                               01723000
CVTAUTHL EQU   CVTS01+136                                               01724000
CVTSCAN  EQU   CVTS01+132                                               01725000
CVTRV144 EQU   CVTS01+130                                               01726000
CVTSTCK  EQU   CVTS01+124                                               01727000
CVTRV139 EQU   CVTS01+123                                               01728000
CVTDSSAC EQU   CVTS01+122                                               01729000
CVTRV513 EQU   CVTS01+121                                               01730000
CVTIOSPL EQU   CVTS01+120                                               01731000
CVTPTGT  EQU   CVTS01+116                                               01732000
CVTCSPIE EQU   CVTS01+112                                               01733000
CVTSMFEX EQU   CVTS01+108                                               01734000
CVTOLT0A EQU   CVTS01+104                                               01735000
CVTSRBRT EQU   CVTS01+100                                               01736000
CVTPUTL  EQU   CVTS01+96                                                01737000
CVTRV519 EQU   CVTS01+92                                                01738000
CVTRV327 EQU   CVTS01+88                                                01739000
CVTRV326 EQU   CVTS01+84                                                01740000
CVTRV325 EQU   CVTS01+80                                                01741000
CVTRV324 EQU   CVTS01+76                                                01742000
CVT0VL01 EQU   CVTS01+72                                                01743000
CVTSHRVM EQU   CVTS01+68                                                01744000
CVTRV332 EQU   CVTS01+64                                                01745000
CVTTAS   EQU   CVTS01+60                                                01746000
CVTRSCN  EQU   CVTS01+56                                                01747000
CVTTRAC2 EQU   CVTS01+54                                                01748000
CVTTRACE EQU   CVTS01+52                                                01749000
CVTAPG   EQU   CVTS01+51                                                01750000
CVTSDTRC EQU   CVTCTLFG                                                 01751000
CVTGTRCE EQU   CVTCTLFG                                                 01752000
CVTNOMP  EQU   CVTCTLFG                                                 01753000
CVTRSV79 EQU   CVTCTLFG                                                 01754000
CVTDSTAT EQU   CVTCTLFG                                                 01755000
CVTRSV78 EQU   CVTCTLFG                                                 01756000
CVTRV333 EQU   CVTCTLFG                                                 01757000
CVTRV323 EQU   CVTCTLFG                                                 01758000
CVTSPVLK EQU   CVTS01+49                                                01759000
CVTRSV77 EQU   CVTS01+48                                                01760000
CVTRV331 EQU   CVTS01+44                                                01761000
CVTRV330 EQU   CVTS01+40                                                01762000
CVTRV329 EQU   CVTS01+36                                                01763000
CVTRV328 EQU   CVTS01+32                                                01764000
CVTRV322 EQU   CVTS01+28                                                01765000
CVTSLID  EQU   CVTSLIDA+1                                               01766000
CVTSYLK  EQU   CVTSLIDA                                                 01767000
CVTRV321 EQU   CVTS01+20                                                01768000
CVTRV320 EQU   CVTS01+16                                                01769000
CVTLPDIR EQU   CVTLPDIA+1                                               01770000
CVTRSV69 EQU   CVTDIRST                                                 01771000
CVTRSV68 EQU   CVTDIRST                                                 01772000
CVTRSV67 EQU   CVTDIRST                                                 01773000
CVTRSV66 EQU   CVTDIRST                                                 01774000
CVTRSV65 EQU   CVTDIRST                                                 01775000
CVTRSV64 EQU   CVTDIRST                                                 01776000
CVTRSV63 EQU   CVTDIRST                                                 01777000
CVTDICOM EQU   CVTDIRST                                                 01778000
CVTPVTP  EQU   CVTS01+8                                                 01779000
CVTLPDSR EQU   CVTS01+4                                                 01780000
CVTGETL  EQU   CVTS01                                                   01781000
STSMWKLD EQU   @NM00006                                                 01782000
@NM00007 EQU   @NM00006                                                 01783000
@NM00005 EQU   STSMC                                                    01784000
STSMCRDR EQU   STSMDEVF                                                 01785000
STSMUNIT EQU   STSMDEVF                                                 01786000
STSMGRAP EQU   STSMDEVF                                                 01787000
STSMDA   EQU   STSMDEVF                                                 01788000
STSMTP   EQU   STSMDEVF                                                 01789000
STSMTAPE EQU   STSMDEVF                                                 01790000
@NM00003 EQU   STSMB                                                    01791000
CATEND   EQU   CAT+8                                                    01792000
CASFLARS EQU   CATFLA                                                   01793000
CATBSY   EQU   CATFLA                                                   01794000
CATFLG7  EQU   CATFLG                                                   01795000
CATFLG6  EQU   CATFLG                                                   01796000
CATFLG5  EQU   CATFLG                                                   01797000
CATNCPU  EQU   CATFLG                                                   01798000
CATNGEN  EQU   CATFLG                                                   01799000
CATRES1  EQU   CATFLG                                                   01800000
@NM00012 EQU   PCCA+512                                                 01801000
PCCARV36 EQU   PCCA+380                                                 01802000
PCCARV35 EQU   PCCA+378                                                 01803000
PCCARV01 EQU   PCCA+377                                                 01804000
PCCACPUM EQU   PCCA+376                                                 01805000
PCCARV63 EQU   PCCA+372                                                 01806000
PCCARV62 EQU   PCCA+368                                                 01807000
PCCARV61 EQU   PCCA+364                                                 01808000
PCCARV60 EQU   PCCA+360                                                 01809000
PCCARV59 EQU   PCCA+356                                                 01810000
PCCARV58 EQU   PCCA+352                                                 01811000
PCCARV57 EQU   PCCA+348                                                 01812000
PCCARV56 EQU   PCCA+344                                                 01813000
PCCARV55 EQU   PCCA+340                                                 01814000
PCCARV54 EQU   PCCA+336                                                 01815000
PCCALOGA EQU   PCCA+332                                                 01816000
PCCACHID EQU   PCCA+324                                                 01817000
PCCACHSV EQU   PCCA+312                                                 01818000
PCCARV79 EQU   PCCACHF4                                                 01819000
PCCARV78 EQU   PCCACHF4                                                 01820000
PCCARV77 EQU   PCCACHF4                                                 01821000
PCCARV76 EQU   PCCACHF4                                                 01822000
PCCARV75 EQU   PCCACHF4                                                 01823000
PCCARV74 EQU   PCCACHF4                                                 01824000
PCCARV73 EQU   PCCACHF4                                                 01825000
PCCARV72 EQU   PCCACHF4                                                 01826000
PCCARV71 EQU   PCCACHF3                                                 01827000
PCCARV70 EQU   PCCACHF3                                                 01828000
PCCARV69 EQU   PCCACHF3                                                 01829000
PCCARV68 EQU   PCCACHF3                                                 01830000
PCCARV67 EQU   PCCACHF3                                                 01831000
PCCARV66 EQU   PCCACHF3                                                 01832000
PCCARV65 EQU   PCCACHF3                                                 01833000
PCCARV64 EQU   PCCACHF3                                                 01834000
PCCACF28 EQU   PCCACHF2                                                 01835000
PCCACF27 EQU   PCCACHF2                                                 01836000
PCCACF26 EQU   PCCACHF2                                                 01837000
PCCACF25 EQU   PCCACHF2                                                 01838000
PCCACF24 EQU   PCCACHF2                                                 01839000
PCCACF23 EQU   PCCACHF2                                                 01840000
PCCACF22 EQU   PCCACHF2                                                 01841000
PCCACF21 EQU   PCCACHF2                                                 01842000
PCCACF18 EQU   PCCACHF1                                                 01843000
PCCACF17 EQU   PCCACHF1                                                 01844000
PCCACF16 EQU   PCCACHF1                                                 01845000
PCCACF15 EQU   PCCACHF1                                                 01846000
PCCACF14 EQU   PCCACHF1                                                 01847000
PCCACF13 EQU   PCCACHF1                                                 01848000
PCCACF12 EQU   PCCACHF1                                                 01849000
PCCACF11 EQU   PCCACHF1                                                 01850000
PCCARV05 EQU   PCCA+306                                                 01851000
PCCACHPB EQU   PCCA+305                                                 01852000
PCCALGP2 EQU   PCCA+304                                                 01853000
PCCALGP1 EQU   PCCA+303                                                 01854000
PCCALOGL EQU   PCCA+302                                                 01855000
PCCARV80 EQU   PCCA+300                                                 01856000
PCCACHW2 EQU   PCCA+296                                                 01857000
PCCACHW1 EQU   PCCA+292                                                 01858000
PCCAIOSI EQU   PCCA+291                                                 01859000
PCCACNRB EQU   PCCACHRB                                                 01860000
PCCACCVB EQU   PCCACHRB                                                 01861000
PCCACSNB EQU   PCCACHRB                                                 01862000
PCCARV52 EQU   PCCACHRB                                                 01863000
PCCACHIB EQU   PCCACHRB                                                 01864000
PCCACTIB EQU   PCCACHRB                                                 01865000
PCCACINB EQU   PCCACHRB                                                 01866000
PCCACSIB EQU   PCCACHRB                                                 01867000
PCCARV51 EQU   PCCACHS2                                                 01868000
PCCARV50 EQU   PCCACHS2                                                 01869000
PCCARV49 EQU   PCCACHS2                                                 01870000
PCCACURC EQU   PCCACHS2                                                 01871000
PCCACNLG EQU   PCCACHS2                                                 01872000
PCCACMOD EQU   PCCACHS2                                                 01873000
PCCACALT EQU   PCCACHS2                                                 01874000
PCCACIOR EQU   PCCACHS2                                                 01875000
PCCARV47 EQU   PCCACHS1                                                 01876000
PCCACUCB EQU   PCCACHS1                                                 01877000
PCCACIBC EQU   PCCACHS1                                                 01878000
PCCACAND EQU   PCCACHS1                                                 01879000
PCCACNLS EQU   PCCACHS1                                                 01880000
PCCACFRR EQU   PCCACHS1                                                 01881000
PCCACNRE EQU   PCCACHS1                                                 01882000
PCCACCMP EQU   PCCACHS1                                                 01883000
PCCACSEQ EQU   PCCACHTS                                                 01884000
PCCACDIN EQU   PCCACHTS                                                 01885000
PCCARV44 EQU   PCCACHTS                                                 01886000
PCCARV43 EQU   PCCACHTS                                                 01887000
PCCACTEC EQU   PCCACHTS                                                 01888000
PCCACDAV EQU   PCCACHVA                                                 01889000
PCCACCHV EQU   PCCACHVA                                                 01890000
PCCACCMD EQU   PCCACHVA                                                 01891000
PCCACUNS EQU   PCCACHVA                                                 01892000
PCCACSQV EQU   PCCACHVA                                                 01893000
PCCARV42 EQU   PCCACHVA                                                 01894000
PCCARV41 EQU   PCCACHVA                                                 01895000
PCCACITF EQU   PCCACHVA                                                 01896000
PCCARV40 EQU   PCCACHBL                                                 01897000
PCCARV39 EQU   PCCACHBL                                                 01898000
PCCARV38 EQU   PCCACHBL                                                 01899000
PCCACCUE EQU   PCCACHBL                                                 01900000
PCCACSTG EQU   PCCACHBL                                                 01901000
PCCACSCU EQU   PCCACHBL                                                 01902000
PCCACCHA EQU   PCCACHBL                                                 01903000
PCCACCPU EQU   PCCACHBL                                                 01904000
PCCACNOR EQU   PCCACHPF                                                 01905000
PCCACCNT EQU   PCCACHPF                                                 01906000
PCCACSNS EQU   PCCACHPF                                                 01907000
PCCARV37 EQU   PCCACHPF                                                 01908000
PCCACHIO EQU   PCCACHPF                                                 01909000
PCCACTIO EQU   PCCACHPF                                                 01910000
PCCACINT EQU   PCCACHPF                                                 01911000
PCCACSIO EQU   PCCACHPF                                                 01912000
PCCACHUB EQU   PCCAWERP                                                 01913000
PCCACHEL EQU   PCCA+168                                                 01914000
PCCALRBR EQU   PCCA+164                                                 01915000
PCCALRBV EQU   PCCA+160                                                 01916000
PCCAPWAR EQU   PCCA+156                                                 01917000
PCCAPWAV EQU   PCCA+152                                                 01918000
PCCAEMSA EQU   PCCAEMSB+12                                              01919000
PCCAEMSE EQU   PCCAEMSB+8                                               01920000
PCCAEMSP EQU   PCCAEMSB+4                                               01921000
PCCARMS  EQU   PCCARMSB                                                 01922000
PCCARV34 EQU   PCCARMSB                                                 01923000
PCCARV33 EQU   PCCARMSB                                                 01924000
PCCARV32 EQU   PCCARMSB                                                 01925000
PCCARV31 EQU   PCCARMSB                                                 01926000
PCCARV30 EQU   PCCARMSB                                                 01927000
PCCARV29 EQU   PCCARMSB                                                 01928000
PCCARV28 EQU   PCCARMSB                                                 01929000
PCCARV27 EQU   PCCAEMS3                                                 01930000
PCCARV26 EQU   PCCAEMS3                                                 01931000
PCCARV25 EQU   PCCAEMS3                                                 01932000
PCCARV24 EQU   PCCAEMS3                                                 01933000
PCCARV23 EQU   PCCAEMS3                                                 01934000
PCCARV22 EQU   PCCAEMS3                                                 01935000
PCCARV21 EQU   PCCAEMS3                                                 01936000
PCCARV20 EQU   PCCAEMS3                                                 01937000
PCCARV19 EQU   PCCAEMS2                                                 01938000
PCCARV18 EQU   PCCAEMS2                                                 01939000
PCCARV17 EQU   PCCAEMS2                                                 01940000
PCCARV16 EQU   PCCAEMS2                                                 01941000
PCCARV15 EQU   PCCAEMS2                                                 01942000
PCCARV14 EQU   PCCAEMS2                                                 01943000
PCCARV13 EQU   PCCAEMS2                                                 01944000
PCCARV12 EQU   PCCAEMS2                                                 01945000
PCCARV11 EQU   PCCARISP                                                 01946000
PCCARV10 EQU   PCCARISP                                                 01947000
PCCARV09 EQU   PCCARISP                                                 01948000
PCCARV08 EQU   PCCARISP                                                 01949000
PCCARV07 EQU   PCCARISP                                                 01950000
PCCARV06 EQU   PCCARISP                                                 01951000
PCCASERL EQU   PCCARISP                                                 01952000
PCCAPARL EQU   PCCARISP                                                 01953000
PCCARPB  EQU   PCCA+132                                                 01954000
PCCACTIN EQU   PCCAINTE                                                 01955000
PCCANFIN EQU   PCCAINTE                                                 01956000
PCCANUIN EQU   PCCAINTE                                                 01957000
PCCACTCC EQU   PCCACCE                                                  01958000
PCCANFCC EQU   PCCACCE                                                  01959000
PCCANUCC EQU   PCCACCE                                                  01960000
PCCACTTD EQU   PCCATODE                                                 01961000
PCCANFTD EQU   PCCATODE                                                 01962000
PCCANUTD EQU   PCCATODE                                                 01963000
PCCARV04 EQU   PCCATMFL                                                 01964000
PCCARV03 EQU   PCCATMFL                                                 01965000
PCCARV02 EQU   PCCATMFL                                                 01966000
PCCAMINT EQU   PCCATMFL                                                 01967000
PCCAMCC  EQU   PCCATMFL                                                 01968000
PCCAVKIL EQU   PCCATMFL                                                 01969000
PCCASYNC EQU   PCCATMFL                                                 01970000
PCCAINIT EQU   PCCATMFL                                                 01971000
PCCARV9E EQU   PCCA+124                                                 01972000
PCCARV9D EQU   PCCA+120                                                 01973000
PCCARV9C EQU   PCCA+116                                                 01974000
PCCARV9B EQU   PCCA+112                                                 01975000
PCCARV9A EQU   PCCA+108                                                 01976000
PCCARV99 EQU   PCCA+104                                                 01977000
PCCARV98 EQU   PCCA+100                                                 01978000
PCCARV97 EQU   PCCA+96                                                  01979000
PCCARV96 EQU   PCCA+92                                                  01980000
PCCARV95 EQU   PCCA+88                                                  01981000
PCCARV94 EQU   PCCA+84                                                  01982000
PCCARV93 EQU   PCCA+80                                                  01983000
PCCARV92 EQU   PCCA+76                                                  01984000
PCCARV91 EQU   PCCA+72                                                  01985000
PCCARV90 EQU   PCCA+68                                                  01986000
PCCARV89 EQU   PCCA+64                                                  01987000
PCCARV88 EQU   PCCA+60                                                  01988000
PCCARV87 EQU   PCCA+56                                                  01989000
PCCARV86 EQU   PCCA+52                                                  01990000
PCCARV85 EQU   PCCA+48                                                  01991000
PCCARV84 EQU   PCCA+44                                                  01992000
PCCARV83 EQU   PCCA+40                                                  01993000
PCCARV82 EQU   PCCA+36                                                  01994000
PCCARV81 EQU   PCCA+32                                                  01995000
PCCAPSAR EQU   PCCA+28                                                  01996000
PCCAPSAV EQU   PCCA+24                                                  01997000
PCCATQEP EQU   PCCA+20                                                  01998000
PCCACAFM EQU   PCCA+18                                                  01999000
PCCACPUA EQU   PCCA+16                                                  02000000
PCCACPID EQU   PCCA+4                                                   02001000
PCCAPCCA EQU   PCCA                                                     02002000
@NM00011 EQU   CSD+160                                                  02003000
CSDMASK  EQU   CSD+128                                                  02004000
CSDUCNT  EQU   CSD+124                                                  02005000
CSDTCNT  EQU   CSD+120                                                  02006000
CSDGDTOD EQU   CSD+116                                                  02007000
CSDGDINT EQU   CSD+112                                                  02008000
CSDGDCC  EQU   CSD+108                                                  02009000
CSDDDRCT EQU   CSD+106                                                  02010000
CSDRV044 EQU   CSD+104                                                  02011000
CSDMAFF  EQU   CSD+24                                                   02012000
CSDRV038 EQU   CSDFLAGS                                                 02013000
CSDRV037 EQU   CSDFLAGS                                                 02014000
CSDRV036 EQU   CSDFLAGS                                                 02015000
CSDRV035 EQU   CSDFLAGS                                                 02016000
CSDRV034 EQU   CSDFLAGS                                                 02017000
CSDRV033 EQU   CSDFLAGS                                                 02018000
CSDRV032 EQU   CSDFLAGS                                                 02019000
CSDMP    EQU   CSDFLAGS                                                 02020000
CSDACR   EQU   CSD+22                                                   02021000
CSDMF1CP EQU   CSD+20                                                   02022000
CSDRV043 EQU   CSD+16                                                   02023000
CSDRV030 EQU   CSDSCFL4                                                 02024000
CSDRV029 EQU   CSDSCFL4                                                 02025000
CSDRV028 EQU   CSDSCFL4                                                 02026000
CSDRV027 EQU   CSDSCFL4                                                 02027000
CSDRV026 EQU   CSDSCFL4                                                 02028000
CSDRV025 EQU   CSDSCFL4                                                 02029000
CSDRV024 EQU   CSDSCFL4                                                 02030000
CSDRV023 EQU   CSDSCFL4                                                 02031000
CSDRV022 EQU   CSDSCFL3                                                 02032000
CSDRV021 EQU   CSDSCFL3                                                 02033000
CSDRV020 EQU   CSDSCFL3                                                 02034000
CSDRV019 EQU   CSDSCFL3                                                 02035000
CSDRV018 EQU   CSDSCFL3                                                 02036000
CSDRV017 EQU   CSDSCFL3                                                 02037000
CSDRV016 EQU   CSDSCFL3                                                 02038000
CSDRV015 EQU   CSDSCFL3                                                 02039000
CSDRV014 EQU   CSDSCFL2                                                 02040000
CSDRV013 EQU   CSDSCFL2                                                 02041000
CSDRV012 EQU   CSDSCFL2                                                 02042000
CSDRV011 EQU   CSDSCFL2                                                 02043000
CSDRV010 EQU   CSDSCFL2                                                 02044000
CSDRV009 EQU   CSDSCFL2                                                 02045000
CSDRV008 EQU   CSDSCFL2                                                 02046000
CSDRV007 EQU   CSDSCFL2                                                 02047000
CSDRV006 EQU   CSDSCFL1                                                 02048000
CSDRV005 EQU   CSDSCFL1                                                 02049000
CSDRV004 EQU   CSDSCFL1                                                 02050000
CSDRV003 EQU   CSDSCFL1                                                 02051000
CSDRV002 EQU   CSDSCFL1                                                 02052000
CSDRV001 EQU   CSDSCFL1                                                 02053000
CSDSYSND EQU   CSDSCFL1                                                 02054000
CSDRV042 EQU   CSDSCFL1                                                 02055000
CSDCPUOL EQU   CSD+10                                                   02056000
CSDSAFF  EQU   CSDCPUAL                                                 02057000
CSDCPUJS EQU   CSD+4                                                    02058000
CSDCSD   EQU   CSD                                                      02059000
ASCBEND  EQU   ASCB+208                                                 02060000
ASCBSRBT EQU   ASCB+200                                                 02061000
ASCBSWTL EQU   ASCB+196                                                 02062000
ASCBRS14 EQU   ASCB+195                                                 02063000
ASCBSMCT EQU   ASCB+194                                                 02064000
ASCBRS12 EQU   ASCB+192                                                 02065000
ASCBPCTT EQU   ASCB+188                                                 02066000
ASCBVGTT EQU   ASCB+184                                                 02067000
ASCBLGCB EQU   ASCB+180                                                 02068000
ASCBJBNS EQU   ASCB+176                                                 02069000
ASCBJBNI EQU   ASCB+172                                                 02070000
ASCBMCC  EQU   ASCB+168                                                 02071000
ASCBRTWA EQU   ASCB+164                                                 02072000
ASCBIQEA EQU   ASCB+160                                                 02073000
ASCBXMPQ EQU   ASCB+156                                                 02074000
ASCBRS01 EQU   ASCB+154                                                 02075000
ASCBFMCT EQU   ASCB+152                                                 02076000
ASCBOUXB EQU   ASCB+148                                                 02077000
ASCBOUCB EQU   ASCB+144                                                 02078000
ASCBMECB EQU   ASCB+140                                                 02079000
ASCBQECB EQU   ASCB+136                                                 02080000
ASCBCMSB EQU   ASCB+132                                                 02081000
ASCBCMSF EQU   ASCB+128                                                 02082000
ASCBTCBS EQU   ASCB+124                                                 02083000
ASCBNVSC EQU   ASCB+122                                                 02084000
ASCBVSC  EQU   ASCB+120                                                 02085000
ASCBSRBS EQU   ASCB+118                                                 02086000
ASCBSSRB EQU   ASCB+116                                                 02087000
ASCBRV06 EQU   ASCBFLG2                                                 02088000
ASCBRV05 EQU   ASCBFLG2                                                 02089000
ASCBRV04 EQU   ASCBFLG2                                                 02090000
ASCBSNQS EQU   ASCBFLG2                                                 02091000
ASCBS2S  EQU   ASCBFLG2                                                 02092000
ASCBCEXT EQU   ASCBFLG2                                                 02093000
ASCBPXMT EQU   ASCBFLG2                                                 02094000
ASCBXMPT EQU   ASCBFLG2                                                 02095000
ASCBRF07 EQU   ASCBDSP1                                                 02096000
ASCBRF06 EQU   ASCBDSP1                                                 02097000
ASCBRF05 EQU   ASCBDSP1                                                 02098000
ASCBRF04 EQU   ASCBDSP1                                                 02099000
ASCBRF03 EQU   ASCBDSP1                                                 02100000
ASCBRF02 EQU   ASCBDSP1                                                 02101000
ASCBFAIL EQU   ASCBDSP1                                                 02102000
ASCBNOQ  EQU   ASCBDSP1                                                 02103000
ASCBSWCT EQU   ASCB+112                                                 02104000
ASCBASXB EQU   ASCB+108                                                 02105000
ASCBTMCH EQU   ASCB+104                                                 02106000
ASCBTYP1 EQU   ASCBFLG1                                                 02107000
ASCBSTND EQU   ASCBFLG1                                                 02108000
ASCBABNT EQU   ASCBFLG1                                                 02109000
ASCBTERM EQU   ASCBFLG1                                                 02110000
ASCBS3S  EQU   ASCBFLG1                                                 02111000
ASCBCMSH EQU   ASCBFLG1                                                 02112000
ASCBTOFF EQU   ASCBFLG1                                                 02113000
ASCBRF01 EQU   ASCBRCTF                                                 02114000
ASCBTMLW EQU   ASCBRCTF                                                 02115000
ASCBOUT  EQU   ASCBRCTF                                                 02116000
ASCBWAIT EQU   ASCBRCTF                                                 02117000
ASCBRV08 EQU   ASCBRCTF                                                 02118000
ASCBFQU  EQU   ASCBRCTF                                                 02119000
ASCBFRS  EQU   ASCBRCTF                                                 02120000
ASCBTMNO EQU   ASCBRCTF                                                 02121000
ASCBAFFN EQU   ASCBFW1                                                  02122000
ASCBDUMP EQU   ASCB+96                                                  02123000
ASCBQSVC EQU   ASCB+92                                                  02124000
ASCBUBET EQU   ASCB+88                                                  02125000
ASCBECB  EQU   ASCB+84                                                  02126000
ASCBJSTL EQU   ASCB+80                                                  02127000
ASCBEWST EQU   ASCB+72                                                  02128000
ASCBEJST EQU   ASCB+64                                                  02129000
ASCBTSB  EQU   ASCB+60                                                  02130000
ASCBCSCB EQU   ASCB+56                                                  02131000
ASCBRSM  EQU   ASCB+52                                                  02132000
ASCBLDA  EQU   ASCB+48                                                  02133000
ASCBSTOR EQU   ASCB+44                                                  02134000
ASCBDP   EQU   ASCB+43                                                  02135000
ASCBRV07 EQU   ASCB+42                                                  02136000
ASCBIOSM EQU   ASCB+40                                                  02137000
ASCBSEQN EQU   ASCB+38                                                  02138000
ASCBASID EQU   ASCB+36                                                  02139000
ASCBCPUS EQU   ASCB+32                                                  02140000
ASCBSPL  EQU   ASCB+28                                                  02141000
ASCBLSLQ EQU   ASCB+24                                                  02142000
ASCBFSLQ EQU   ASCB+20                                                  02143000
ASCBIOSP EQU   ASCB+16                                                  02144000
ASCBLOCK EQU   ASCB+12                                                  02145000
ASCBBWDP EQU   ASCB+8                                                   02146000
ASCBFWDP EQU   ASCB+4                                                   02147000
ASCBASCB EQU   ASCB                                                     02148000
ASCBEGIN EQU   ASCB                                                     02149000
CVTLEVL  EQU   CVTRELNO+2                                               02150000
CVTNUMB  EQU   CVTRELNO                                                 02151000
CVTMDL   EQU   CVTFIX+250                                               02152000
@NM00010 EQU   CVTFIX+248                                               02153000
@NM00009 EQU   CVTFIX                                                   02154000
CVTRV482 EQU   CVTXTNT2+128                                             02155000
CVTRV481 EQU   CVTXTNT2+124                                             02156000
CVTRV480 EQU   CVTXTNT2+120                                             02157000
CVTRV479 EQU   CVTXTNT2+118                                             02158000
CVTRV478 EQU   CVTXTNT2+117                                             02159000
CVTRV477 EQU   CVTXTNT2+116                                             02160000
CVTRV476 EQU   CVTXTNT2+115                                             02161000
CVTRV475 EQU   CVTXTNT2+114                                             02162000
CVTRV474 EQU   CVTRV466                                                 02163000
CVTRV473 EQU   CVTRV466                                                 02164000
CVTRV472 EQU   CVTRV466                                                 02165000
CVTRV471 EQU   CVTRV466                                                 02166000
CVTRV470 EQU   CVTRV466                                                 02167000
CVTRV469 EQU   CVTRV466                                                 02168000
CVTRV468 EQU   CVTRV466                                                 02169000
CVTRV467 EQU   CVTRV466                                                 02170000
CVTRV465 EQU   CVTRV457                                                 02171000
CVTRV464 EQU   CVTRV457                                                 02172000
CVTRV463 EQU   CVTRV457                                                 02173000
CVTRV462 EQU   CVTRV457                                                 02174000
CVTRV461 EQU   CVTRV457                                                 02175000
CVTRV460 EQU   CVTRV457                                                 02176000
CVTRV459 EQU   CVTRV457                                                 02177000
CVTRV458 EQU   CVTRV457                                                 02178000
CVTRV456 EQU   CVTXTNT2+108                                             02179000
CVTRV455 EQU   CVTXTNT2+104                                             02180000
CVTRV454 EQU   CVTXTNT2+100                                             02181000
CVTRV453 EQU   CVTXTNT2+96                                              02182000
CVTRV452 EQU   CVTXTNT2+94                                              02183000
CVTRV451 EQU   CVTXTNT2+92                                              02184000
CVTRV450 EQU   CVTXTNT2+90                                              02185000
CVTRV449 EQU   CVTXTNT2+88                                              02186000
CVTRV448 EQU   CVTXTNT2+87                                              02187000
CVTRV447 EQU   CVTXTNT2+86                                              02188000
CVTRV446 EQU   CVTRV438                                                 02189000
CVTRV445 EQU   CVTRV438                                                 02190000
CVTRV444 EQU   CVTRV438                                                 02191000
CVTRV443 EQU   CVTRV438                                                 02192000
CVTRV442 EQU   CVTRV438                                                 02193000
CVTRV441 EQU   CVTRV438                                                 02194000
CVTRV440 EQU   CVTRV438                                                 02195000
CVTRV439 EQU   CVTRV438                                                 02196000
CVTRV437 EQU   CVTRV429                                                 02197000
CVTRV436 EQU   CVTRV429                                                 02198000
CVTRV435 EQU   CVTRV429                                                 02199000
CVTRV434 EQU   CVTRV429                                                 02200000
CVTRV433 EQU   CVTRV429                                                 02201000
CVTRV432 EQU   CVTRV429                                                 02202000
CVTRV431 EQU   CVTRV429                                                 02203000
CVTRV430 EQU   CVTRV429                                                 02204000
CVTRV428 EQU   CVTXTNT2+80                                              02205000
CVTRV427 EQU   CVTXTNT2+76                                              02206000
CVTRV426 EQU   CVTXTNT2+72                                              02207000
CVTRV425 EQU   CVTXTNT2+68                                              02208000
CVTATACT EQU   CVTATCVT                                                 02209000
CVTRV423 EQU   CVTXTNT2+62                                              02210000
CVTRV422 EQU   CVTXTNT2+60                                              02211000
CVTRV421 EQU   CVTXTNT2+58                                              02212000
CVTRV420 EQU   CVTXTNT2+56                                              02213000
CVTRV419 EQU   CVTXTNT2+55                                              02214000
CVTRV418 EQU   CVTXTNT2+54                                              02215000
CVTRV417 EQU   CVTRV409                                                 02216000
CVTRV416 EQU   CVTRV409                                                 02217000
CVTRV415 EQU   CVTRV409                                                 02218000
CVTRV414 EQU   CVTRV409                                                 02219000
CVTRV413 EQU   CVTRV409                                                 02220000
CVTRV412 EQU   CVTRV409                                                 02221000
CVTRV411 EQU   CVTRV409                                                 02222000
CVTRV410 EQU   CVTRV409                                                 02223000
CVTRV408 EQU   CVTRV400                                                 02224000
CVTRV407 EQU   CVTRV400                                                 02225000
CVTRV406 EQU   CVTRV400                                                 02226000
CVTRV405 EQU   CVTRV400                                                 02227000
CVTRV404 EQU   CVTRV400                                                 02228000
CVTRV403 EQU   CVTRV400                                                 02229000
CVTRV402 EQU   CVTRV400                                                 02230000
CVTRV401 EQU   CVTRV400                                                 02231000
CVTRSVA1 EQU   CVTXTNT2+48                                              02232000
CVTRSVA0 EQU   CVTXTNT2+44                                              02233000
CVTRSV99 EQU   CVTXTNT2+40                                              02234000
CVTRSV98 EQU   CVTXTNT2+36                                              02235000
CVTRSV97 EQU   CVTXTNT2+34                                              02236000
CVTRSV96 EQU   CVTXTNT2+32                                              02237000
CVTOLTEP EQU   CVTXTNT2+28                                              02238000
CVTQIDA  EQU   CVTQID+1                                                 02239000
CVTRSV95 EQU   CVTQID                                                   02240000
CVTRSV94 EQU   CVTXTNT2+20                                              02241000
CVTRSV93 EQU   CVTXTNT2+16                                              02242000
CVTRSV92 EQU   CVTXTNT2+12                                              02243000
CVTDEBVR EQU   CVTXTNT2+8                                               02244000
CVTRSV91 EQU   CVTXTNT2+6                                               02245000
CVTRSV9H EQU   CVTRSV90                                                 02246000
CVTRSV9G EQU   CVTRSV90                                                 02247000
CVTRSV9F EQU   CVTRSV90                                                 02248000
CVTRSV9E EQU   CVTRSV90                                                 02249000
CVTRSV9D EQU   CVTRSV90                                                 02250000
CVTRSV9C EQU   CVTRSV90                                                 02251000
CVTRSV9B EQU   CVTRSV90                                                 02252000
CVTRSV9A EQU   CVTRSV90                                                 02253000
CVTNUCLS EQU   CVTXTNT2+4                                               02254000
CVTDSSVA EQU   CVTDSSV+1                                                02255000
CVTRSV89 EQU   CVTDSSV                                                  02256000
CVTRSV88 EQU   CVTXTNT1+8                                               02257000
CVTRSV87 EQU   CVTXTNT1+4                                               02258000
CVTFACHN EQU   CVTXTNT1                                                 02259000
CVTRV488 EQU   CVTMAP+412                                               02260000
CVTRV487 EQU   CVTMAP+408                                               02261000
CVTRV486 EQU   CVTMAP+404                                               02262000
CVTRV485 EQU   CVTMAP+400                                               02263000
CVTRV484 EQU   CVTMAP+396                                               02264000
CVTAUTH  EQU   CVTMAP+392                                               02265000
CVTATMCA EQU   CVTATMCT+1                                               02266000
CVTATMST EQU   CVTATMCT                                                 02267000
CVTRSV61 EQU   CVTMAP+384                                               02268000
CVTVOLT1 EQU   CVTVOLM1+1                                               02269000
CVTVOLI1 EQU   CVTVOLF1                                                 02270000
CVTSTOA  EQU   CVTMAP+376                                               02271000
CVTRSV58 EQU   CVTMAP+374                                               02272000
CVTRSV57 EQU   CVTMAP+372                                               02273000
CVTDDCE  EQU   CVTMAP+368                                               02274000
CVTPNWFR EQU   CVTMAP+364                                               02275000
CVTSMF   EQU   CVTMAP+360                                               02276000
CVTSULK  EQU   CVTMAP+358                                               02277000
CVTSLKO  EQU   CVTSYSK                                                  02278000
CVTSLKP  EQU   CVTSYSK                                                  02279000
CVTSLKQ  EQU   CVTSYSK                                                  02280000
CVTSLKR  EQU   CVTSYSK                                                  02281000
CVTRSV56 EQU   CVTSYSK                                                  02282000
CVTRSV55 EQU   CVTSYSK                                                  02283000
CVTRSV54 EQU   CVTSYSK                                                  02284000
CVTRSV53 EQU   CVTSYSK                                                  02285000
CVTRSV52 EQU   CVTA1F1                                                  02286000
CVTRSV51 EQU   CVTA1F1                                                  02287000
CVTRSV50 EQU   CVTA1F1                                                  02288000
CVTRSV49 EQU   CVTA1F1                                                  02289000
CVTRSV48 EQU   CVTA1F1                                                  02290000
CVTRSV47 EQU   CVTA1F1                                                  02291000
CVTSRSW  EQU   CVTA1F1                                                  02292000
CVTPFSW  EQU   CVTA1F1                                                  02293000
CVTPCVT  EQU   CVTMAP+352                                               02294000
CVTRSV46 EQU   CVTMAP+344                                               02295000
CVTRSV45 EQU   CVTMAP+340                                               02296000
CVTRSV44 EQU   CVTMAP+338                                               02297000
CVTRSV43 EQU   CVTMAP+336                                               02298000
CVTHJESA EQU   CVTHJES+1                                                02299000
CVTRSV42 EQU   CVTHJES                                                  02300000
CVTEXT2A EQU   CVTEXT2+1                                                02301000
CVTRSV41 EQU   CVTEXT2                                                  02302000
CVTAPFA  EQU   CVTAPF+1                                                 02303000
CVTRSV40 EQU   CVTAPF                                                   02304000
CVTRV518 EQU   CVTINTLA                                                 02305000
CVTRV517 EQU   CVTERPV                                                  02306000
CVTEORM  EQU   CVTMAP+312                                               02307000
CVTMCHPR EQU   CVTMAP+308                                               02308000
CVTTZ    EQU   CVTMAP+304                                               02309000
CVTJEPS  EQU   CVTMAP+300                                               02310000
CVTJESCT EQU   CVTMAP+296                                               02311000
CVTMODE  EQU   CVTMAP+292                                               02312000
CVTPTRV  EQU   CVTMAP+288                                               02313000
CVTREAL  EQU   CVTMAP+284                                               02314000
CVTRSV39 EQU   CVTMAP+280                                               02315000
CVTRSV38 EQU   CVTMAP+276                                               02316000
CVTDMSRA EQU   CVTDMSR+1                                                02317000
CVTRSV37 EQU   CVTDMSR                                                  02318000
CVTQMSGA EQU   CVTQMSG+1                                                02319000
CVTRSV36 EQU   CVTQMSG                                                  02320000
CVTAMFF  EQU   CVTMAP+264                                               02321000
CVTPURGA EQU   CVTPURG+1                                                02322000
CVTRSV35 EQU   CVTPURG                                                  02323000
CVTCBSP  EQU   CVTMAP+256                                               02324000
CVTATERA EQU   CVTATER+1                                                02325000
CVTSYST  EQU   CVTATER                                                  02326000
CVTVOLT2 EQU   CVTTAT                                                   02327000
CVTVOLI2 EQU   CVTVOLF2                                                 02328000
CVTAQAVB EQU   CVTAQAVT+1                                               02329000
CVTRSV34 EQU   CVTTCMFG                                                 02330000
CVTRSV33 EQU   CVTTCMFG                                                 02331000
CVTRSV32 EQU   CVTTCMFG                                                 02332000
CVTRSV31 EQU   CVTTCMFG                                                 02333000
CVTRSV30 EQU   CVTTCMFG                                                 02334000
CVTRSV29 EQU   CVTTCMFG                                                 02335000
CVTRSV28 EQU   CVTTCMFG                                                 02336000
CVTTCRDY EQU   CVTTCMFG                                                 02337000
CVTGTFA  EQU   CVTGTF+1                                                 02338000
CVTRSV27 EQU   CVTGTFST                                                 02339000
CVTRNIO  EQU   CVTGTFST                                                 02340000
CVTRV319 EQU   CVTUSR                                                   02341000
CVTRV318 EQU   CVTFORM                                                  02342000
CVTRV317 EQU   CVTTMODE                                                 02343000
CVTRV316 EQU   CVTSTATE                                                 02344000
CVTRV315 EQU   CVTGTFS                                                  02345000
CVTGTFAV EQU   CVTGTFS                                                  02346000
CVT0SCR1 EQU   CVTMAP+232                                               02347000
CVTRV515 EQU   CVTMAP+228                                               02348000
CVTRMS   EQU   CVTMAP+224                                               02349000
CVTPATCH EQU   CVTMAP+220                                               02350000
CVTTSCE  EQU   CVTMAP+216                                               02351000
CVTLNKSC EQU   CVTMAP+214                                               02352000
CVTQABST EQU   CVTMAP+212                                               02353000
CVTMDLDS EQU   CVTMAP+208                                               02354000
CVTUSER  EQU   CVTMAP+204                                               02355000
CVTABEND EQU   CVTMAP+200                                               02356000
CVTSMCA  EQU   CVTMAP+196                                               02357000
CVTRSV18 EQU   CVTMAP+192                                               02358000
CVTQLPAQ EQU   CVTMAP+188                                               02359000
CVTQCDSR EQU   CVTMAP+184                                               02360000
CVTRSV17 EQU   CVTOPTB                                                  02361000
CVTRSV16 EQU   CVTOPTB                                                  02362000
CVTFP    EQU   CVTOPTB                                                  02363000
CVTAPTHR EQU   CVTOPTB                                                  02364000
CVTNLOG  EQU   CVTOPTB                                                  02365000
CVTTOD   EQU   CVTOPTB                                                  02366000
CVTCTIMS EQU   CVTOPTB                                                  02367000
CVTPROT  EQU   CVTOPTB                                                  02368000
CVTXPFP  EQU   CVTOPTA                                                  02369000
CVTASCII EQU   CVTOPTA                                                  02370000
CVTRSV13 EQU   CVTOPTA                                                  02371000
CVTRSV12 EQU   CVTOPTA                                                  02372000
CVTNIP   EQU   CVTOPTA                                                  02373000
CVTDDR   EQU   CVTOPTA                                                  02374000
CVTAPR   EQU   CVTOPTA                                                  02375000
CVTCCH   EQU   CVTOPTA                                                  02376000
CVTSNCTR EQU   CVTMAP+180                                               02377000
CVTQMWR  EQU   CVTMAP+176                                               02378000
CVTQOCR  EQU   CVTMAP+172                                               02379000
CVT1EF00 EQU   CVTMAP+168                                               02380000
CVTMZ00  EQU   CVTMAP+164                                               02381000
CVTHEAD  EQU   CVTMAP+160                                               02382000
CVTRSV11 EQU   CVTMAP+156                                               02383000
CVT0PT01 EQU   CVTMAP+152                                               02384000
CVTMSER  EQU   CVTMAP+148                                               02385000
CVTRV516 EQU   CVTIERLC                                                 02386000
CVTILCH  EQU   CVTMAP+140                                               02387000
CVT0DS   EQU   CVTMAP+136                                               02388000
CVTFBOSV EQU   CVTMAP+132                                               02389000
CVTNUCB  EQU   CVTMAP+128                                               02390000
CVTIXAVL EQU   CVTMAP+124                                               02391000
CVTIOQET EQU   CVTMAP+120                                               02392000
CVTDCBA  EQU   CVTMAP+117                                               02393000
CVTMVS2  EQU   CVTDCB                                                   02394000
CVT6DAT  EQU   CVTDCB                                                   02395000
CVT4MPS  EQU   CVTDCB                                                   02396000
CVTRSV09 EQU   CVTDCB                                                   02397000
CVT4MS1  EQU   CVTDCB                                                   02398000
CVT2SPS  EQU   CVTDCB                                                   02399000
CVT1SSS  EQU   CVTDCB                                                   02400000
CVTRSV08 EQU   CVTDCB                                                   02401000
CVTSTB   EQU   CVTMAP+112                                               02402000
CVTQTD00 EQU   CVTMAP+108                                               02403000
CVTQTE00 EQU   CVTMAP+104                                               02404000
CVTCUCB  EQU   CVTMAP+100                                               02405000
CVTSJQ   EQU   CVTMAP+96                                                02406000
CVTPBLDL EQU   CVTMAP+92                                                02407000
CVTTPC   EQU   CVTMAP+88                                                02408000
CVTSVDCB EQU   CVTMAP+84                                                02409000
CVTBRET  EQU   CVTMAP+82                                                02410000
CVTEXIT  EQU   CVTMAP+80                                                02411000
CVT0FN00 EQU   CVTMAP+76                                                02412000
CVTDARA  EQU   CVTDAR+1                                                 02413000
CVTRSV07 EQU   CVTFLGS1                                                 02414000
CVTRSV06 EQU   CVTFLGS1                                                 02415000
CVTRSV05 EQU   CVTFLGS1                                                 02416000
CVTRSV04 EQU   CVTFLGS1                                                 02417000
CVTRSV03 EQU   CVTFLGS1                                                 02418000
CVTRSV02 EQU   CVTFLGS1                                                 02419000
CVTRSV01 EQU   CVTFLGS1                                                 02420000
CVTDMPLK EQU   CVTFLGS1                                                 02421000
CVTXITP  EQU   CVTMAP+68                                                02422000
CVTZDTAB EQU   CVTMAP+64                                                02423000
CVTMSLT  EQU   CVTMAP+60                                                02424000
CVTDATE  EQU   CVTMAP+56                                                02425000
CVTBTERM EQU   CVTMAP+52                                                02426000
CVTSYSAD EQU   CVTMAP+48                                                02427000
CVTXTLER EQU   CVTMAP+44                                                02428000
CVTILK2  EQU   CVTMAP+40                                                02429000
CVTILK1  EQU   CVTMAP+36                                                02430000
CVTPRLTV EQU   CVTMAP+32                                                02431000
CVTPCNVT EQU   CVTMAP+28                                                02432000
CVT0VL00 EQU   CVTMAP+24                                                02433000
CVTXAPG  EQU   CVTMAP+20                                                02434000
CVTBUF   EQU   CVTMAP+16                                                02435000
CVTJOB   EQU   CVTMAP+12                                                02436000
CVTLINK  EQU   CVTMAP+8                                                 02437000
CVT0EF00 EQU   CVTMAP+4                                                 02438000
CVTTCBP  EQU   CVTMAP                                                   02439000
CVT      EQU   CVTMAP                                                   02440000
ECCDOLAP EQU   ECCDB+16                                                 02441000
ECCDBUSY EQU   ECCDB+12                                                 02442000
ECCDSIOS EQU   ECCDB+8                                                  02443000
ECCDCMOD EQU   ECCDCHID                                                 02444000
ECCDCTYP EQU   ECCDCHID                                                 02445000
ECCDRS02 EQU   ECCDB+1                                                  02446000
ECCDCCHG EQU   ECCDFLGS                                                 02447000
ECCDRS01 EQU   ECCDFLGS                                                 02448000
ECCPRS02 EQU   ECCPE+1                                                  02449000
ECCPRS01 EQU   ECCPFLGS                                                 02450000
ECCESAMP EQU   ECCEDT+8                                                 02451000
ECCERS01 EQU   ECCEDT+4                                                 02452000
SMF73OLP EQU   SMF73B+12                                                02453000
SMF73BSY EQU   SMF73B+8                                                 02454000
SMF73CNT EQU   SMF73B+4                                                 02455000
SMF73STA EQU   SMF73FG2                                                 02456000
SMF73VAC EQU   SMF73FG2                                                 02457000
SMF73IID EQU   SMF73FG2                                                 02458000
@NM00008 EQU   SMF73FG2                                                 02459000
SMF73TYP EQU   SMF73FG2                                                 02460000
SMF73HID EQU   SMF73B+2                                                 02461000
SMF73CID EQU   SMF73B                                                   02462000
SMF73RV2 EQU   SMF73A+6                                                 02463000
SMF73SHD EQU   SMF73A+4                                                 02464000
SMF73CHA EQU   SMF73A+2                                                 02465000
SMF73RLS EQU   SMFRCD73+44                                              02466000
SMF73RV1 EQU   SMFRCD73+42                                              02467000
SMF73MFV EQU   SMFRCD73+40                                              02468000
SMF73SAM EQU   SMFRCD73+36                                              02469000
SMF73SUB EQU   SMFRCD73+34                                              02470000
SMF73CYC EQU   SMFRCD73+32                                              02471000
SMF73INT EQU   SMFRCD73+28                                              02472000
SMF73DAT EQU   SMFRCD73+24                                              02473000
SMF73IST EQU   SMFRCD73+20                                              02474000
SMF73SID EQU   SMFRCD73+14                                              02475000
SMF73DTE EQU   SMFRCD73+10                                              02476000
SMF73TME EQU   SMFRCD73+6                                               02477000
SMF73RTY EQU   SMFRCD73+5                                               02478000
SMF73FLG EQU   SMFRCD73+4                                               02479000
SMF73SEG EQU   SMFRCD73+2                                               02480000
SMF73LEN EQU   SMFRCD73                                                 02481000
STSGLEN  EQU   STSGFREE+1                                               02482000
STSGSP   EQU   STSGFREE                                                 02483000
STSMRSV1 EQU   STSMA+53                                                 02484000
STSMSSP  EQU   STSMA+52                                                 02485000
STSMLCOM EQU   STSMA+48                                                 02486000
STSMIADD EQU   STSMA+40                                                 02487000
STSMISP  EQU   STSMIGMC                                                 02488000
STSMENTR EQU   STSMA+28                                                 02489000
STSMSAVE EQU   STSMA+24                                                 02490000
STSMTERM EQU   STSMA+12                                                 02491000
STSMINIT EQU   STSMA+4                                                  02492000
@NM00002 EQU   STSMOFLG                                                 02493000
@NM00001 EQU   STSMOPT                                                  02494000
@NM00036 EQU   PSA+3412                                                 02495000
PSASTAK  EQU   PSA+3072                                                 02496000
@NM00035 EQU   PSA+1032                                                 02497000
PSAUSEND EQU   PSA+1032                                                 02498000
PSAPCPSW EQU   PSA+1024                                                 02499000
PSARV060 EQU   PSA+1020                                                 02500000
PSARV059 EQU   PSA+1018                                                 02501000
PSASVC13 EQU   PSA+1016                                                 02502000
PSALSFCC EQU   PSA+1012                                                 02503000
PSASFACC EQU   PSA+1008                                                 02504000
PSASTOP  EQU   PSA+992                                                  02505000
PSASTART EQU   PSA+976                                                  02506000
PSARSPSW EQU   PSA+968                                                  02507000
PSASRPSW EQU   PSA+960                                                  02508000
PSARV045 EQU   PSA+892                                                  02509000
PSARV044 EQU   PSA+888                                                  02510000
PSARV043 EQU   PSA+884                                                  02511000
PSARV042 EQU   PSA+880                                                  02512000
PSARV041 EQU   PSA+876                                                  02513000
PSARV040 EQU   PSA+872                                                  02514000
PSARV025 EQU   PSA+868                                                  02515000
PSADSSED EQU   PSA+868                                                  02516000
PSADSSPR EQU   PSA+864                                                  02517000
PSADSSFW EQU   PSA+860                                                  02518000
PSADSS14 EQU   PSA+856                                                  02519000
PSADSSPP EQU   PSA+848                                                  02520000
PSADSSRP EQU   PSA+840                                                  02521000
PSADSS05 EQU   PSADSSF4                                                 02522000
PSADSS10 EQU   PSADSSF4                                                 02523000
PSADSSVE EQU   PSADSSF4                                                 02524000
PSADSSDE EQU   PSADSSF4                                                 02525000
PSADSSC0 EQU   PSADSSF4                                                 02526000
PSADSSIE EQU   PSADSSF4                                                 02527000
PSADSS12 EQU   PSADSSF4                                                 02528000
PSADSSRC EQU   PSADSSF4                                                 02529000
PSARV057 EQU   PSADSSF3                                                 02530000
PSARV056 EQU   PSADSSF3                                                 02531000
PSARV055 EQU   PSADSSF3                                                 02532000
PSARV054 EQU   PSADSSF3                                                 02533000
PSADSSRW EQU   PSADSSF3                                                 02534000
PSADSSNM EQU   PSADSSF3                                                 02535000
PSADSSES EQU   PSADSSF3                                                 02536000
PSADSSGP EQU   PSADSSF3                                                 02537000
PSADSSF2 EQU   PSADSSFL+1                                               02538000
PSADSSPI EQU   PSADSSF1                                                 02539000
PSADSSOI EQU   PSADSSF1                                                 02540000
PSADSSSP EQU   PSADSSF1                                                 02541000
PSADSSTP EQU   PSADSSF1                                                 02542000
PSADSSDW EQU   PSADSSF1                                                 02543000
PSADSSDD EQU   PSADSSF1                                                 02544000
PSADSSDM EQU   PSADSSF1                                                 02545000
PSADSSMV EQU   PSADSSF1                                                 02546000
PSADSSTS EQU   PSA+816                                                  02547000
PSADSSWK EQU   PSA+812                                                  02548000
PSADSSR3 EQU   PSA+808                                                  02549000
PSADSSR2 EQU   PSA+804                                                  02550000
PSADSSRS EQU   PSA+800                                                  02551000
PSASTOR  EQU   PSA+796                                                  02552000
PSACPUSA EQU   PSA+794                                                  02553000
PSAVSTAP EQU   PSA+792                                                  02554000
PSAWKVAP EQU   PSA+788                                                  02555000
PSAWKRAP EQU   PSA+784                                                  02556000
PSAMCHIC EQU   PSA+783                                                  02557000
PSARV061 EQU   PSA+782                                                  02558000
PSASYMSK EQU   PSA+781                                                  02559000
PSAMCHFL EQU   PSA+780                                                  02560000
PSACR0   EQU   PSA+776                                                  02561000
PSAPSWSV EQU   PSA+768                                                  02562000
PSACLHS  EQU   PSAHLHI                                                  02563000
PSALKR15 EQU   PSALKSA+60                                               02564000
PSALKR14 EQU   PSALKSA+56                                               02565000
PSALKR13 EQU   PSALKSA+52                                               02566000
PSALKR12 EQU   PSALKSA+48                                               02567000
PSALKR11 EQU   PSALKSA+44                                               02568000
PSALKR10 EQU   PSALKSA+40                                               02569000
PSALKR9  EQU   PSALKSA+36                                               02570000
PSALKR8  EQU   PSALKSA+32                                               02571000
PSALKR7  EQU   PSALKSA+28                                               02572000
PSALKR6  EQU   PSALKSA+24                                               02573000
PSALKR5  EQU   PSALKSA+20                                               02574000
PSALKR4  EQU   PSALKSA+16                                               02575000
PSALKR3  EQU   PSALKSA+12                                               02576000
PSALKR2  EQU   PSALKSA+8                                                02577000
PSALKR1  EQU   PSALKSA+4                                                02578000
PSALKR0  EQU   PSALKSA                                                  02579000
PSARV023 EQU   PSACLHT+52                                               02580000
PSALOCAL EQU   PSACLHT+48                                               02581000
PSACMSL  EQU   PSACLHT+44                                               02582000
PSAOPTL  EQU   PSACLHT+40                                               02583000
PSATPACL EQU   PSACLHT+36                                               02584000
PSATPDNL EQU   PSACLHT+32                                               02585000
PSATPNCL EQU   PSACLHT+28                                               02586000
PSAIOSLL EQU   PSACLHT+24                                               02587000
PSAIOSUL EQU   PSACLHT+20                                               02588000
PSAIOSCL EQU   PSACLHT+16                                               02589000
PSAIOSSL EQU   PSACLHT+12                                               02590000
PSASALCL EQU   PSACLHT+8                                                02591000
PSAASML  EQU   PSACLHT+4                                                02592000
PSADISPL EQU   PSACLHT                                                  02593000
PSASRSA  EQU   PSA+636                                                  02594000
PSARV050 EQU   PSA+634                                                  02595000
PSADSSGO EQU   PSA+633                                                  02596000
PSARECUR EQU   PSA+632                                                  02597000
PSAHLHIS EQU   PSA+628                                                  02598000
PSAIPCSA EQU   PSA+624                                                  02599000
@NM00034 EQU   PSA+621                                                  02600000
PSAIPCDM EQU   PSA+620                                                  02601000
PSAIPCD  EQU   PSA+616                                                  02602000
@NM00033 EQU   PSA+613                                                  02603000
PSAIPCRM EQU   PSA+612                                                  02604000
PSAIPCR  EQU   PSA+608                                                  02605000
PSAMCHEX EQU   PSA+600                                                  02606000
PSAMPSW  EQU   PSA+592                                                  02607000
PSAEXPS2 EQU   PSA+584                                                  02608000
PSAEXPS1 EQU   PSA+576                                                  02609000
PSAPIREG EQU   PSA+572                                                  02610000
PSARSREG EQU   PSA+568                                                  02611000
PSAGPREG EQU   PSA+556                                                  02612000
PSARV022 EQU   PSASUP4                                                  02613000
PSARV021 EQU   PSASUP4                                                  02614000
PSARV020 EQU   PSASUP4                                                  02615000
PSARV019 EQU   PSASUP4                                                  02616000
PSARV018 EQU   PSASUP4                                                  02617000
PSARV017 EQU   PSASUP4                                                  02618000
PSARV016 EQU   PSASUP4                                                  02619000
PSARV015 EQU   PSASUP4                                                  02620000
PSARV014 EQU   PSASUP3                                                  02621000
PSARV013 EQU   PSASUP3                                                  02622000
PSARV012 EQU   PSASUP3                                                  02623000
PSARV011 EQU   PSASUP3                                                  02624000
PSARV010 EQU   PSASUP3                                                  02625000
PSARV009 EQU   PSASUP3                                                  02626000
PSARV008 EQU   PSASUP3                                                  02627000
PSAIOSUP EQU   PSASUP3                                                  02628000
PSALCR   EQU   PSASUP2                                                  02629000
PSARTM   EQU   PSASUP2                                                  02630000
PSAACR   EQU   PSASUP2                                                  02631000
PSAIPCE2 EQU   PSASUP2                                                  02632000
PSAIPCES EQU   PSASUP2                                                  02633000
PSAIPCEC EQU   PSASUP2                                                  02634000
PSAGTF   EQU   PSASUP2                                                  02635000
PSAIPCRI EQU   PSASUP2                                                  02636000
PSAIPCRP EQU   PSASUP1                                                  02637000
PSAIPCDR EQU   PSASUP1                                                  02638000
PSADISP  EQU   PSASUP1                                                  02639000
PSALOCK  EQU   PSASUP1                                                  02640000
PSAPI    EQU   PSASUP1                                                  02641000
PSAEXT   EQU   PSASUP1                                                  02642000
PSASVC   EQU   PSASUP1                                                  02643000
PSAIO    EQU   PSASUP1                                                  02644000
PSAANEW  EQU   PSA+544                                                  02645000
PSATOLD  EQU   PSA+540                                                  02646000
PSATNEW  EQU   PSA+536                                                  02647000
PSALCCAR EQU   PSA+532                                                  02648000
PSALCCAV EQU   PSA+528                                                  02649000
PSAPCCAR EQU   PSA+524                                                  02650000
PSAPCCAV EQU   PSA+520                                                  02651000
PSACPULA EQU   PSA+518                                                  02652000
PSACPUPA EQU   PSA+516                                                  02653000
PSAPSA   EQU   PSA+512                                                  02654000
FLCHDEND EQU   PSA+512                                                  02655000
FLCCRSAV EQU   FLCMCLA+280                                              02656000
FLCGRSAV EQU   FLCMCLA+216                                              02657000
FLCFPSAV EQU   FLCMCLA+184                                              02658000
FLCFLA   EQU   FLCMCLA+88                                               02659000
FLCRGNCD EQU   FLCMCLA+84                                               02660000
FLCFSAA  EQU   FLCFSA+1                                                 02661000
@NM00032 EQU   FLCFSA                                                   02662000
@NM00031 EQU   FLCMCLA+72                                               02663000
FLCMCIC  EQU   FLCMCLA+64                                               02664000
@NM00030 EQU   FLCMCLA+20                                               02665000
FLCIOAA  EQU   FLCIOA+1                                                 02666000
@NM00029 EQU   FLCIOA                                                   02667000
@NM00028 EQU   FLCMCLA+15                                               02668000
@NM00027 EQU   FLCMCLA+14                                               02669000
@NM00026 EQU   FLCMCLA+12                                               02670000
FLCLCL   EQU   FLCMCLA+8                                                02671000
FLCIOELA EQU   FLCIOEL+1                                                02672000
@NM00025 EQU   FLCIOEL                                                  02673000
FLCCHNID EQU   FLCMCLA                                                  02674000
@NM00024 EQU   PSA+160                                                  02675000
FLCMTRCD EQU   PSA+157                                                  02676000
@NM00023 EQU   PSA+156                                                  02677000
FLCPERA  EQU   FLCPER+1                                                 02678000
@NM00022 EQU   FLCPER                                                   02679000
@NM00021 EQU   PSA+151                                                  02680000
FLCPERCD EQU   PSA+150                                                  02681000
FLCMCNUM EQU   PSA+149                                                  02682000
@NM00020 EQU   PSA+148                                                  02683000
FLCTEAA  EQU   FLCTEA+1                                                 02684000
@NM00019 EQU   FLCTEA                                                   02685000
PSAPIPC  EQU   PSAPICOD                                                 02686000
PSAPIMC  EQU   PSAPICOD                                                 02687000
PSAPIPER EQU   PSAPICOD                                                 02688000
PSARV049 EQU   FLCPICOD                                                 02689000
FLCPILCB EQU   FLCPIILC                                                 02690000
@NM00018 EQU   FLCPIILC                                                 02691000
@NM00017 EQU   PSAEPPSW                                                 02692000
FLCSVCN  EQU   PSAESPSW+2                                               02693000
FLCSILCB EQU   FLCSVILC                                                 02694000
@NM00016 EQU   FLCSVILC                                                 02695000
@NM00015 EQU   PSAESPSW                                                 02696000
FLCEICOD EQU   PSAEEPSW+2                                               02697000
PSASPAD  EQU   PSAEEPSW                                                 02698000
@NM00014 EQU   PSA+128                                                  02699000
FLCINPSW EQU   PSA+120                                                  02700000
FLCMNPSW EQU   PSA+112                                                  02701000
FLCPNPSW EQU   PSA+104                                                  02702000
FLCSNPSW EQU   PSA+96                                                   02703000
FLCENPSW EQU   PSA+88                                                   02704000
FLCTRACE EQU   PSA+84                                                   02705000
FLCTIMER EQU   PSA+80                                                   02706000
FLCCVT2  EQU   PSA+76                                                   02707000
FLCCAW   EQU   PSA+72                                                   02708000
FLCCSW   EQU   PSA+64                                                   02709000
FLCIOPSW EQU   PSA+56                                                   02710000
FLCMOPSW EQU   PSA+48                                                   02711000
FLCPOPSW EQU   PSA+40                                                   02712000
FLCSOPSW EQU   PSA+32                                                   02713000
FLCEOPSW EQU   PSA+24                                                   02714000
@NM00013 EQU   FLCICCW2+4                                               02715000
FLCCVT   EQU   FLCICCW2                                                 02716000
FLCICCW1 EQU   FLCROPSW                                                 02717000
FLCIPPSW EQU   FLCRNPSW                                                 02718000
.@UNREFD ANOP                          END UNREFERENCED COMPONENTS      02719000
@RT00167 EQU   IHLAB1                                                   02720000
@ENDDATA EQU   *                                                        02721000
         END   IRBMFIHA,(C'PL/S-II',0502,74086)                         02722000
/*
//*
//STEP05  EXEC PGM=IEBGENER
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  *
  IDENTIFY IRBMFIHA('ZP60030')
/*
//SYSUT2   DD  DSN=&&SMPMCS,DISP=(MOD,PASS)
//SYSIN    DD  DUMMY
//*
//STEP06  EXEC SMPREC,WORK='SYSALLDA'
//SMPPTFIN DD  DSN=&&SMPMCS,DISP=(OLD,DELETE)
//SMPCNTL  DD  *
  RECEIVE
          SELECT(ZP60030)
          .
/*
//*
//STEP07CK EXEC SMPAPP,WORK='SYSALLDA'
//SMPCNTL  DD  *
  APPLY
        SELECT(ZP60030)
        CHECK
        .
/*
//*
//STEP07  EXEC SMPAPP,COND=(0,NE),WORK='SYSALLDA'
//SMPCNTL  DD  *
  APPLY
        SELECT(ZP60030)
        DIS(WRITE)
        .
/*
//
//ZP60031  JOB (SYSGEN),'J05 M42: ZP60031',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             REGION=4096K
/*JOBPARM LINES=100
//JOBCAT   DD  DSN=SYS1.VSAM.MASTER.CATALOG,DISP=SHR
//*
//*  ALLOW TYPE 6 AND TYPE 26 SMF RECORDS FOR STARTED TASKS.
//*
//RECEIVE EXEC SMPREC
//SMPPTFIN DD  *
++USERMOD(ZP60031)            /* ALLOW STC SMF TYPE 6 AND 26 */  .
++VER(Z038) FMID(EJE1103)
  PRE(UZ31176,UZ33158,UZ35334,UZ37263,UZ52543,UZ54837,UZ57911,
      UZ60375,UZ63374,UZ65742,UZ68537,UZ71437,UZ76165,TJES801)
 /*
   PROBLEM DESCRIPTION:
     NO SMF TYPE 6 AND TYPE 26 RECORDS ARE WRITTEN FOR STARTED TASKS.
       SMF CANNOT BE USED TO MONITOR ALL PRINTER AND PUNCH ACTIVITY
       BECAUSE NO TYPE 6 SMF RECORDS CAN BE CREATED TO TRACK STARTED
       TASK OUTPUT.  JES2 PHASE ANALYSIS FROM SMF TYPE 26 RECORD DATA
       CANNOT INCLUDE STARTED TASKS BECAUSE NO CORRESPONDING TYPE 26
       RECORDS CAN BE CREATED.

       THIS USERMOD ALTERS JES2 SO THAT SETTINGS FROM JES2 PARAMETERS
       WHICH CAN REQUEST TYPE 6 AND TYPE 26 SMF RECORDS ARE NOT
       OVERLAID BY HARD-CODED LOGIC.  SPECIFICALLY, NOTYPE6 AND
       NOTYPE26 ARE NO LONGER FORCED ON THE &STC JOB CLASS PARAMETER.
       ALSO, THE NOUSO AND NOUJP SETTINGS ARE NO LONGER FORCED,
       THEREBY ALLOWING AN INSTALLATION TO USE THE IEFUSO AND IEFUJP
       SMF EXITS FOR STARTED TASKS.

       NOTE THAT JOB CLASS SETTINGS SUCH AS THESE ARE ASSIGNED AT
       JCL CONVERSION TIME AND NOT DURING LATER PHASES SUCH AS
       OUTPUT, PRINT AND PURGE.


   SPECIAL CONDITIONS:
     ACTION:
       JES2 MUST BE WARM STARTED FOR THIS CHANGE TO BECOME ACTIVE.

   COMMENTS:
     PRYCROFT SIX P/L PUBLIC DOMAIN USERMOD FOR MVS 3.8 NO. 31.

     THE FOLLOWING MODULES AND/OR MACROS ARE AFFECTED BY THIS USERMOD:
     MODULES:
       HASPINIT
 */.
++ SRCUPD   (HASPINIT)  DISTLIB(HASPSRC ).
./ CHANGE NAME=HASPINIT
         OI    CATSMFLG,0  DO NOT FORBID SMF EXITS AND RECORDS  ZP60031 M4793500
/*
//SMPCNTL  DD  *
  RECEIVE
          SELECT(ZP60031)
          .
/*
//*
//APPLYCK EXEC SMPAPP,WORK='SYSALLDA'
//SMPCNTL  DD  *
  APPLY
        SELECT(ZP60031)
        CHECK
        .
/*
//*
//APPLY   EXEC SMPAPP,COND=(0,NE),WORK='SYSALLDA'
//SMPCNTL  DD  *
  APPLY
        SELECT(ZP60031)
        DIS(WRITE)
        .
/*
//
//ZP60032  JOB (SYSGEN),'J06 M43: ZP60032',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             REGION=4096K
/*JOBPARM LINES=100
//JOBCAT   DD  DSN=SYS1.VSAM.MASTER.CATALOG,DISP=SHR
//*
//*  ALLOW THE GTTERM MACRO TO REPORT THE TSO TERMINAL NAME.
//*
//RECEIVE EXEC SMPREC,WORK='SYSALLDA'
//SMPPTFIN DD  *
++USERMOD(ZP60032)         /* ADD TERMID TO THE GTTERM MACRO */  .
++VER(Z038) FMID(ETI1106) PRE(UZ44753)
 /*
   PROBLEM DESCRIPTION:
     THE GTTERM MACRO CANNOT EXPLOIT ALL FUNCTIONS OF THE INTERFACE.
       THE ZP60009 USERMOD ENHANCED TSO/VTAM TO ALLOW THE SVC 94
       GTTERM INTERFACE TO RETURN THE 8-BYTE VTAM LU NAME OF THE
       TSO TERMINAL, BUT THE GTTERM MACRO OWNED BY TIOC DOES NOT
       SUPPORT THE TERMID OPERAND.

       THIS USERMOD ADDS THE TERMID OPERAND TO THE GTTERM MACRO
       IN A WAY WHICH IS COMPATIBLE WITH TSO/E.


   SPECIAL CONDITIONS:
     NONE.

   COMMENTS:
     PRYCROFT SIX P/L PUBLIC DOMAIN USERMOD FOR MVS 3.8 NO. 32.

     THE FOLLOWING MODULES AND/OR MACROS ARE AFFECTED BY THIS USERMOD:
     MACROS:
       GTTERM
 */.
++MAC(GTTERM) DISTLIB(ATSOMAC).
         MACRO                                                          00050000
&NAME    GTTERM &PRMSZE=,&ALTSZE=,&ATTRIB=,&TERMID=,&MF=I      @G76XRYU 00070990
.*                                                             @OW03892 00071980
.* A000000-999999                                              @G76XR00 00075000
.* A034000                                                     @OZ90350 00077000
.* NOCHANGE SHIPPED WITH JCLIN                                 @OY20024 00078000
.* NOCHANGE SHIPPED WITH JCLIN                                 @OY26821 00079000
.* ADDED TERMID PARAMETER                                      @OW03892 00080990
.*                                                                      00081980
         LCLC  &NDX                                            @G76XRYU 00085000
&NDX     SETC  '&SYSNDX'                                       @G76XRYU 00090000
         AIF   ('&MF' EQ 'I' AND '&PRMSZE' EQ '' AND '&ATTRIB' EQ '' AN-00799990
               D '&TERMID' EQ '').ERROR1              @G76XRYU @OW03892 01499980
         AIF   ('&MF' EQ 'L').LFORM                                     02450000
         AIF   ('&MF(1)' EQ 'E').EFORM                                  02500000
         AIF   ('&MF' NE 'I').ERROR2                                    02550000
.*********************I -FORM OF MACRO********************************* 02600000
&NAME    CNOP  0,4                                                      02650000
         BAL   1,*+20                   BRANCH AROUND PARMS    @G76XRYU 02672990
.*                                                             @OW03892 02675980
GTRM&NDX DC    A(0)                     ADDRESS OF PRIMARY     @G76XRYU 02680000
         DC    A(0)                     ADDRESS OF ALTERNATE            02690000
         DC    A(0)                     ADDRESS OF ATTRIBUTE   @G76XRYU 02700000
         DC    A(0)                     ADDRESS OF TERMINAL ID @OW03892 03000000
         AGO   .STADDR                                                  03350000
.EFORM   ANOP                                                           03400000
&NAME    CNOP  0,4                                             @OZ90350 03429990
         IHBOPLST ,,&NAME,MF=&MF                                        03450000
.STADDR  ANOP                                                           03500000
.*********COMMON CODE FOR BOTH I AND E FORM OF MACRO******************  03550000
         AIF   ('&PRMSZE' EQ '').LABEL2                                 03600000
         AIF   ('&PRMSZE'(1,1) NE '(').LOADPRM                          03700000
         ST    &PRMSZE(1),0(,1)         STORE PRIMARY ADDRESS           03750000
         AGO   .LABEL2                                                  03800000
.LOADPRM ANOP                                                           03850000
         AIF   ('&PRMSZE'(K'&PRMSZE,1) EQ ')' OR '&MF' NE 'I').LPARM    03857000
         ORG   GTRM&NDX                 PUT ADDR OF PRIMARY    @G76XRYU 03864000
         DC    A(&PRMSZE)               IN PARM LIST           @G76XRYU 03871000
         ORG                                                            03878000
         AGO   .LABEL2                                         @G76XRYU 03885000
.LPARM   ANOP                      ..LA ADDR OR EXECUTE FORM   @G76XRYU 03892000
         LA    0,&PRMSZE                LOAD ADDRESS OF PRIMARY         03900000
         ST    0,0(,1)                  STORE ADDRESS OF PRIMARY        03950000
.LABEL2  ANOP                                                           04000000
         AIF   ('&ALTSZE' EQ '').IEATRCK                       @G76XRYU 04020000
         AIF   ('&ALTSZE'(1,1) NE '(').LOADALT                          04150000
         ST    &ALTSZE(1),4(,1)         STORE ADDRESS OF ALTERNATE      04200000
         AGO   .IEATRCK                                        @G76XRYU 04220000
.LOADALT ANOP                                                           04300000
         AIF   ('&ALTSZE'(K'&ALTSZE,1) EQ ')' OR '&MF' NE 'I').LAALT    04300100
         ORG   GTRM&NDX+4               PUT ALTERNATE SIZE     @G76XRYU 04306100
         DC    A(&ALTSZE)               IN PARM LIST           @G76XRYU 04312100
         ORG                                                            04318100
         AGO   .IEATRCK                                        @G76XRYU 04324100
.LAALT   ANOP                     ...LA ADDR OR EXECUTE FORM   @G76XRYU 04330100
         LA    0,&ALTSZE                LOAD ADDR OF ALTERNATE @G76XRYU 04336100
         ST    0,4(,1)                  STORE ADD OF ALTERNATE @G76XRYU 04350000
.*  PROCESS ATTRIBUTE PARM FOR I AND E FORMS WHEN NOT NULL     @G76XRYU 04450000
.IEATRCK ANOP                                                  @G76XRYU 04451000
         AIF   ('&ATTRIB' EQ '').LABEL3               @G76XRYU @OW03892 04452490
         AIF   ('&ATTRIB'(1,1) EQ '(').REGATR                  @G76XRYU 04453000
         AIF   ('&ATTRIB'(K'&ATTRIB,1) EQ ')' OR '&MF' NE 'I').LAATRIB  04454000
         ORG   GTRM&NDX+8               PUT ATTRIB BYTE ADDR   @G76XRYU 04455000
         DC    A(&ATTRIB)               IN PARM LIST           @G76XRYU 04456000
         ORG                                                            04457000
         AGO   .LABEL3                                @G76XRYU @OW03892 04458490
.LAATRIB ANOP                       .. LA ADDR OR EXECUTE FORM @G76XRYU 04459000
         LA    0,&ATTRIB                GET ADR OF ATTRIB BYTE @G76XRYU 04460000
         ST    0,8(1)                   PUT IN 3RD PARM WORD   @G76XRYU 04461000
         AGO   .LABEL3                                @G76XRYU @OW03892 04462490
.REGATR  ANOP                                                  @G76XRYU 04463000
         ST    &ATTRIB(1),8(1)          REG => 3RD PARM WORD   @G76XRYU 04464000
.*  PROCESS TERMINAL ID PARM FOR I AND E FORMS WHEN NOT NULL   @OW03892 04464040
.LABEL3  ANOP                                                  @OW03892 04464080
         AIF   ('&TERMID' EQ '').SVCENTY                       @OW03892 04464120
         AIF   ('&TERMID'(1,1) NE '(').LOTRMID                 @OW03892 04464160
         ST    &TERMID(1),12(,1)        STORE PRIMARY ADDRESS  @OW03892 04464200
         OI    12(1),128                END OF LIST INDICATOR  @OW03892 04464240
         AGO   .SVCENT2                                        @OW03892 04464280
.LOTRMID ANOP                                                  @OW03892 04464320
         AIF   ('&TERMID'(K'&TERMID,1) EQ ')' OR '&MF' NE 'I').LTERM    04464360
.*                                                             @OW03892 04464400
         ORG   GTRM&NDX+12              PUT ADDR OF TERMID IN  @OW03892 04464440
         DC    XL1'80',AL3(&TERMID)     PARM LIST WITH END OF  @OW03892 04464480
.*                                      LIST INDICATOR         @OW03892 04464520
         ORG                                                            04464560
         AGO   .SVCENT2                                        @OW03892 04464600
.LTERM   ANOP                      ..LA ADDR OR EXECUTE FORM   @OW03892 04464640
         LA    0,&TERMID                LOAD ADDRESS OF TERMINAL ID     04464680
.*                                                             @OW03892 04464720
         ST    0,12(,1)                 STORE ADDRESS OF TERMINAL ID    04464760
.*                                                             @OW03892 04464800
         OI    12(1),128                END OF LIST INDICATOR  @OW03892 04464840
         AGO   .SVCENT2                                        @OW03892 04464880
.SVCENTY ANOP                                                           04465000
         OI    8(1),128                 END OF LIST INDICATOR  @G76XRYU 04470000
.SVCENT2 ANOP                                                           04510000
         LA    0,17                     ENTRY CODE                      04550000
         SLL   0,24                     SHIFT TO HIGH ORDER BYTE        04600000
         SVC   94                       ISSUE SVC                       04650000
         MEXIT                                                          04700000
.***************  L  - FORM  ***************************                04750000
.LFORM   ANOP                                                           04800000
&NAME    DS    0F                                                       04850000
         AIF   ('&PRMSZE' EQ '').NOPRMAD                                04900000
         AIF   ('&PRMSZE'(1,1) EQ '(').NOPRMAD                          04950000
         DC    A(&PRMSZE)               ADDRESS OF PRIMARY PARM ADDR    05000000
         AGO   .CHKALT                                                  05050000
.NOPRMAD ANOP                                                           05100000
         DC    A(0)                     ADDRESS OF PRIMARY PARM ADDR    05150000
.CHKALT  AIF   ('&ALTSZE' EQ '').NOALTAD                                05200000
         AIF   ('&ALTSZE'(1,1) EQ '(').NOALTAD                          05250000
         DC    A(&ALTSZE)               ADDRESS OF ALTERNATE ADDR       05300000
         AGO   .LATTCK                                         @G76XRYU 05320000
.NOALTAD ANOP                                                           05400000
         DC    A(0)                     ADDR OF ALTERNATE      @G76XRYU 05420000
.*  PROCESS ATTRIBUTE PARM FOR LIST FORM                       @G76XRYU 05422000
.LATTCK  ANOP                                                  @G76XRYU 05424000
         AIF   ('&ATTRIB' NE '').CKATTR                        @G76XRYU 05426000
         DC    A(0)                     L-FORM--ATTRIB BYTE    @G76XRYU 05428000
         AGO   .CKTERM                                @G76XRYU @OW03892 05430990
.CKATTR  ANOP                                                  @G76XRYU 05432000
         AIF   ('&ATTRIB'(1,1) NE '(').ATTROK                  @G76XRYU 05434000
         MNOTE 12,'IHB300 INCOMPATIBLE OPERANDS: MF=L AND ATTRIB=&ATTRI*05436000
               B'                                              @G76XRYU 05438000
         AGO   .CKTERM                                @G76XRYU @OW03892 05440990
.ATTROK  ANOP                                                  @G76XRYU 05442000
         DC    A(&ATTRIB)               L-FORM--A(ATTR BYTE)   @G76XRYU 05444000
         AGO   .CKTERM                                         @OW03892 05558990
.CKTERM  AIF   ('&TERMID' EQ '').NOTRMAD                       @OW03892 05567980
         AIF   ('&TERMID'(1,1) EQ '(').NOTRMAD                 @OW03892 05576970
         DC    A(&TERMID)               ADDRESS OF TERMINAL ID ADDR     05585960
.*                                                             @OW03892 05594950
         MEXIT                                                 @OW03892 05603940
.NOTRMAD ANOP                                                  @OW03892 05612930
         DC    A(0)                     ADDRESS OF TERMINAL ID ADDR     05621920
.*                                                             @OW03892 05630910
         MEXIT                                                 @OW03892 05639900
.ERROR1  ANOP                                                           05650000
         IHBERMAC 1006,PRMSZE                                           05700000
         MEXIT                                                          05750000
.ERROR2  IHBERMAC 54,,&MF                                               05800000
         MEXIT                                                          05850000
         MEND                                                           05900000
/*
//SMPCNTL  DD  *
  RECEIVE
          SELECT(ZP60032)
          .
/*
//*
//APPLYCK EXEC SMPAPP,WORK='SYSALLDA'
//SMPCNTL  DD  *
  APPLY
        SELECT(ZP60032)
        CHECK
        .
/*
//*
//APPLY   EXEC SMPAPP,COND=(0,NE),WORK='SYSALLDA'
//SMPCNTL  DD  *
  APPLY
        SELECT(ZP60032)
        DIS(WRITE)
        .
/*
//
//ZP60033  JOB (SYSGEN),'J07 M44: ZP60033',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             REGION=4096K
/*JOBPARM LINES=100
//JOBCAT   DD  DSN=SYS1.VSAM.MASTER.CATALOG,DISP=SHR
//*
//*  ADD SUPPORT FOR THE LOC PARAMETER TO THE GETMAIN MACRO.
//*
//RECEIVE EXEC SMPREC,WORK='SYSALLDA'
//SMPPTFIN DD  *
++USERMOD(ZP60033)         /* ADD LOC= TO THE GETMAIN MACRO */  .
++VER(Z038) FMID(EBB1102)
 /*
   PROBLEM DESCRIPTION:
     THE GETMAIN MACRO DOES NOT TOLERATE A LOC VALUE SPECIFICATION.
       MANY PROGRAMS SPECIFY VIRTUAL STORAGE LOCATIONS OF 24-BIT
       OR 31-BIT WHEN REQUESTING MORE STORAGE FROM THE SYSTEM.
       FURTHER, SOME PROGRAMS SPECIFY THAT WHEN PAGE-FIXED, THE
       NEW STORAGE MAY BE BACKED IN 24-BIT, 31-BIT OR 64-BIT
       ADDRESSABLE REAL STORAGE.  PROGRAMS WITH SUCH LOC VALUES
       CODED WILL NOT BE ASSEMBLED CORRECTLY ON MVS 3.8.

       THIS USERMOD ADDS SUPPORT FOR THE LOC VALUE TO THE GETMAIN
       MACRO.  THE LOC PARAMETER MAY BE SPECIFIED WITH THE RU AND
       RC FORMS OF GETMAIN.

       THE FIRST VALUE OF LOC MAY BE ONE OF THE FOLLOWING LITERALS:
            'BELOW', '24', 'ANY' OR '31'.

       USE 'BELOW' OR '24' TO REQUEST 24-BIT ADDRESSABLE STORAGE.
       USE 'ANY' OR '31' TO REQUEST 31-BIT ADDRESSABLE STORAGE.

       IF SPECIFIED, THE SECOND VALUE OF LOC MAY BE ONE OF:
            'ANY', '31' OR '64'.

       ANY ONE OF THESE THREE VALUES REQUESTS AN OVERRIDE TO THE
       SYSTEM DEFAULT OF BACKING THE STORAGE BELOW THE 16MB LINE
       IN REAL STORAGE IF THE STORAGE IS EVER PAGE-FIXED.

       THIS VERSION OF GETMAIN WILL GENERATE FLAG SETTINGS COMPATIBLE
       WITH MVS/XA.  A SECOND LOC VALUE OF '64' IS TOLERATED BUT IS
       TREATED AS IF '31' HAD BEEN SPECIFIED.

   SPECIAL CONDITIONS:
     NONE.

   COMMENTS:
     PRYCROFT SIX P/L PUBLIC DOMAIN USERMOD FOR MVS 3.8 NO. 33.

     REWORK HISTORY:
       2017-01-26: FIRST SMP VERSION OF THE AUGUST 2016 REVISION
                   OF THE MVS/380 VERSION.

     THE FOLLOWING MODULES AND/OR MACROS ARE AFFECTED BY THIS USERMOD:
     MACROS:
       GETMAIN
 */.
++MAC(GETMAIN) DISTLIB(AMACLIB).
* %/*
         MACRO
&NAME    GETMAIN &MODE,&LV=,&LA=,&A=,&SP=,&MF=I,&HIARCHY=,&BNDRY=,     X
               &KEY=,&BRANCH=,&RELATED=,&LOC=              *MVS380*
.*     OS/VS2 RELEASE 4 VERSION -- 10/21/75
         LCLA  &SVCN,&PRG
         LCLB  &B,&C,&D,&GLBR,&BR,&SPREG,&UNCND,&BND,&L,&V,&CND
         LCLB  &RL,&VA,&VB                                 *MVS380*
         LCLB  &KEYRG,&LVREG
         LCLC  &GNAME
.*0000000400,012200,013000-013200,016800-017600,026600-026800      LC0A
.*    047000                                                       LC0A
&GNAME   SETC  'IHB'.'&SYSNDX'
&PRG     SETA  15                 DEFAULT REGISTER FOR RC AND RU
         AIF   ('&MODE' EQ '').NOMODE
         AIF   ('&MODE'(1,1) NE 'V' AND '&MODE'(1,1) NE 'E' AND        *
               '&MODE'(1,1) NE 'L' AND '&MODE'(1,1) NE 'R' AND         *
               '&MODE'(1,1) NE 'P').ERROR7                     @Z30EN9G
         AIF   (K'&MODE EQ 1).MODE1    SINGLE CHARACTER MODE?
         AIF   ('&MODE'(2,1) NE 'U' AND '&MODE'(2,1) NE 'C').ERROR7
.MODE1   ANOP
&L       SETB  ('&MODE'(1,1) EQ 'V' OR '&MODE'(1,1) EQ 'L')
&V       SETB  ('&MODE'(1,1) EQ 'V')
         AIF   ('&MODE'(1,1) EQ 'R' OR '&MODE'(1,1) EQ 'P').NOMODE
         AIF   (K'&MODE EQ 1).ERROR7
&CND     SETB  ('&MODE'(2,1) EQ 'C')
.NOMODE  AIF   ('&BNDRY' EQ '').NOBNDRY
&BND     SETB  ('&BNDRY' EQ 'PAGE')
.NOBNDRY AIF   ('&LOC' EQ '').NOLOC                        *MVS380*
.*                                                         *MVS380*
.*   LOC ADDED FOR THE MVS/380 PROJECT FOR COMPATIBILITY   *MVS380*
.*   WITH THE X/A AND LATER VERSIONS OF THE SYSTEM.        *MVS380*
.*   BITS ARE (CURRENTLY) IGNORED BY THE SVC 120           *MVS380*
.*   INTERCEPT CODE.                                       *MVS380*
.*   USE RC/RU, AND LV>=16mb FOR ABOVE THE LINE MEMORY     *MVS380*
.*                                                         *MVS380*
.*   REVISION 2016-AUG-25                                  *MVS380* REV
.*   1. Remove LOC=EXPLICIT due to no INADDR parameter.    *MVS380* REV
.*   2. Support '24', '31' and '64' LOC specifications.    *MVS380* REV
.*                                                         *MVS380* REV
.*   R15 low byte "MODE" settings match those of z/OS.     *MVS380* REV
.*   LOC=(,64) and LOC=(,31) are flagged identically.      *MVS380* REV
.*   z/OS sets x'10' in the R15 high byte for LOC=(,64).   *MVS380* REV
.*                                                         *MVS380* REV
         AIF   (N'&LOC LE 2).LOCNUGH   TOO MANY OPERANDS?  *MVS380*
         IHBERMAC 1012,LOC   TOO MANY OPERANDS             *MVS380*
.LOCNUGH AIF   (N'&LOC EQ 1).LOCONE    JUST ONE ?          *MVS380*
         AIF   ('&LOC(2)' EQ '').LOCONE  COMMA ONLY?       *MVS380*
&RL      SETB  ('&LOC(2)' EQ 'ANY' OR '&LOC(2)' EQ '31' OR             X
               '&LOC(2)' EQ '64')                          *MVS380* REV
         AIF   (&RL).LOCONE                                *MVS380* REV
.ERR22   IHBERMAC 1007,LOC(2)     INVALID                  *MVS380*
         MEXIT ,                                           *MVS380*
.LOCONE  AIF   ('&LOC(1)' EQ '').NOLOC                     *MVS380*
         AIF   ('&LOC(1)' EQ 'RES').NOLOC                  *MVS380* REV
&VA      SETB  ('&LOC(1)' EQ 'ANY' OR '&LOC(1)' EQ '31' )  *MVS380* REV
&VB      SETB  ('&LOC(1)' EQ 'BELOW' OR '&LOC(1)' EQ '24' OR           X
               &VA)           Set both bits for LOC=31     *MVS380* REV
&RL      SETB  (&VA OR &RL)   Handle absent &LOC(2)        *MVS380* REV
         AIF   (&VB).NOLOC                                 *MVS380* REV
         IHBERMAC 1007,LOC(1)     INVALID                  *MVS380*
         MEXIT ,                                           *MVS380*
.NOLOC   AIF   ('&KEY' EQ '').SKIP                         *MVS380*
         AIF   ('&MODE' NE 'RC' AND '&MODE' NE 'RU').ERRORA
         AIF   ('&BRANCH' EQ '').ERRORE
         AIF   ('&BRANCH' NE 'YES' AND '&BRANCH'(1,1) NE '(').ERRORA
         AIF   ('&BRANCH(1)' NE 'YES').ERRORE
         AIF   ('&KEY'(1,1) EQ '(').SKIP
         AIF   (T'&KEY NE 'N').SKIP   CAN'T CHECK EQUATED VALUE
         AIF   (&KEY GT 15).ERRORB
.SKIP    AIF   ('&MF' EQ 'L' AND '&BRANCH' NE '').ERRORC
         AIF   ('&BRANCH' EQ '').BRCNT
         AIF   (N'&BRANCH LT 2).BRSNG
       AIF   ('&BRANCH(1)' NE 'YES' OR '&BRANCH(2)' NE 'GLOBAL').ERRORD
         AIF   ('&BRANCH(2)' EQ 'GLOBAL' AND '&MODE' NE 'RC' AND       *
               '&MODE' NE 'RU').ERRORF
&GLBR    SETB  1
&BR      SETB  1
         AGO   .BRCNT
.BRSNG   AIF   ('&BRANCH' NE 'YES').ERRORD
&BR      SETB  1
.BRCNT   ANOP
&SVCN    SETA  4
         AIF   ('&MODE' EQ '' AND '&MF' EQ 'I').ERROR1
         AIF   ('&LV' NE '' AND '&LA' NE '').ERROR5
         AIF   ('&MODE' EQ '').CONT1
         AIF   ('&MODE'(1,1) EQ 'E' AND '&LA' NE '').ERROR6
         AIF   ('&MODE'(1,1) EQ 'R' AND '&LA' NE '').ERROR6
         AIF   ('&BNDRY' NE '' AND '&BNDRY' NE 'DBLWD' AND '&BNDRY'    X
               NE 'PAGE').ERROR10
         AIF   ('&BNDRY' EQ 'PAGE' AND '&MODE' EQ 'R').ERR10A
         AIF   ('&MODE'(1,1) EQ 'L' AND '&LV' NE '').ERROR4
         AIF   ('&MODE'(1,1) EQ 'V' AND '&LV' NE '').ERROR4
         AIF   ('&HIARCHY' EQ '' OR '&HIARCHY' EQ '0' OR '&HIARCHY' EQ X
               '1').CONT1
         IHBERMAC 195
         MEXIT
.CONT1   AIF   ('&MF' EQ 'L').LROUT
         AIF   ('&MF' EQ 'I').IROUT
         AIF   (N'&MF LE 1).ERROR2
         AIF   ('&MF(1)' NE 'E').ERROR2
&NAME    IHBINNRA &MF(2)
         AIF   ('&LV' EQ '').CONTB
         AIF   ('&LV'(1,1) EQ '(').ISAREG
         AIF   (T'&LV NE 'N').CONTBB
         AIF   (&LV LE 4095).CONTAA
.CONTBB  CNOP  0,4
         B     *+8                               BRANCH AROUND LENGTH
         DC    A(&LV)                            LENGTH
         MVC   0(4,1),*-4                        MOVE LENGTH INTO LIST
         AGO   .CONTB
.CONTAA  LA    14,&LV.(0,0)                      PICK UP LENGTH
         ST    14,0(0,1)                         STORE INTO LIST
         AGO   .CONTB
.ISAREG  ST    &LV(1),0(0,1)                     STORE LENGTH INTO LIST
.CONTB   AIF   ('&LA' EQ '').CONTD
         AIF   ('&LA'(1,1) EQ '(').ISAREGA
         LA    14,&LA                            PICK UP LIST ADDRESS
         ST    14,0(0,1)                         STORE INTO PARAM LIST
         AGO   .CONTD
.ISAREGA ST    &LA(1),0(0,1)                     STORE LA IN PARAM LIST
.CONTD   AIF   ('&MODE' EQ '' AND '&BNDRY' EQ '').CONTE
         MVI   8(1),B'&L&V&CND&BND.0000'         SET MODE / BNDRY FLGS
.CONTE   AIF   ('&A' EQ '').CONTI
         AIF   ('&A'(1,1) EQ '(').ISAREGB
         LA    14,&A                             LOAD AREA LIST ADDRESS
         ST    14,4(0,1)                         STORE INTO PARAM LIST
         AGO   .CONTI
.ISAREGB ST    &A(1),4(1,0)                      STORE INTO PARAM LIST
.CONTI   AIF   ('&SP' EQ '').FINI
         AIF   ('&SP'(1,1) EQ '(').ISAREGC
         MVI   9(1),&SP                          MOVE IN SUBPOOL VALUE
         AGO   .FINI
.ISAREGC STC   &SP(1),9(1,0)                     STORE SUBPOOL VALUE
         AGO   .FINI
.LROUT   AIF   ('&LV' EQ  '').CONTJ
         AIF   ('&LV'(1,1) EQ '(').ERROR3
&NAME    DC    A(&LV)                            LENGTH
         AGO   .CONTLL
.CONTJ   AIF   ('&LA' EQ '').CONTK
         AIF   ('&LA'(1,1) EQ '(').ERROR3
&NAME    DC    A(&LA)                            ADDR. OF LENGTH LIST
         AGO   .CONTLL
.CONTK   ANOP
&NAME    DC   A(0)                               LA OR LU
.CONTLL  AIF   ('&A' EQ '').CONTM
         AIF   ('&A'(1,1) EQ '(').ERROR3
         DC    A(&A)                             ADDR. OF ADDR. LIST
         AGO   .CONTN
.CONTM   DC    A(0)                              ADDR. OF ADDR. LIST
.CONTN   DC    BL1'&L&V&CND&BND.0000'            MODE AND OPTION FLAGS
         AIF   ('&SP' EQ '').CONTU
         AIF   ('&SP'(1,1) EQ '(').ERROR3
         DC    AL1(&SP)                          SUBPOOL VALUE
         AGO   .FINISH
.CONTU   DC    AL1(0)                            SUBPOOL VALUE
.FINISH  MEXIT
.IROUT   AIF   ('&MODE'(1,1) EQ 'R').RROUT
         AIF   ('&MODE'(1,1) EQ 'P').PROUT                     @Z30EN9G
         AIF   ('&LV' EQ '' AND '&LA' EQ '').ERROR8
         CNOP  0,4
&NAME    BAL   1,*+14                            BRANCH AROUND LIST
         AIF   ('&LV' EQ '').CNTA
         AIF   ('&LV'(1,1) EQ '(').CNTB
         DC    A(&LV)                            LENGTH
         AGO   .CNTC
.CNTB    DC    A(0)                              LENGTH
&B       SETB  1
         AGO   .CNTC
.CNTA    AIF   ('&LA'(1,1) EQ '(').CNTD
         DC    A(&LA)                            ADDR. OF LENGTH LIST
         AGO   .CNTC
.CNTD    DC    A(0)                              ADDR. OF LENGTH LIST
&C       SETB  1
.CNTC    AIF   ('&A' EQ '').ERROR8
         AIF   ('&A'(1,1) EQ '(').CNTE
         DC    A(&A)                  ADDR. OF ADDR. LIST
         AGO   .CNTF
.CNTE    DC    A(0)                              ADDR. OF ADDR. LIST
&D       SETB  1
.CNTF    DC    BL1'&L&V&CND&BND.0000'            MODE AND OPTION FLAGS
         AIF   ('&SP' EQ '').CNTL
         AIF   ('&SP'(1,1) EQ '(').ISAREGQ
         DC    AL1(&SP)                          SUBPOOL VALUE
         AGO   .CNTM
.ISAREGQ DC    AL1(0)                            SUBPOOL VALUE
         STC   &SP(1),9(0,1)                     STORE SP INTO LIST
         AGO   .CNTM
.CNTL    DC    AL1(0)                            SUBPOOL VALUE
.CNTM    AIF   (NOT &B).CNTN
         ST    &LV(1),0(0,1)                     STORE LENGTH INTO LIST
         AGO   .CNTO
.CNTN    AIF   (NOT &C).CNTO
         ST    &LA(1),0(0,1)                     STORE LA INTO LIST
.CNTO    AIF   (NOT &D).FINI
         ST    &A(1),4(0,1)                      STORE INTO PARAM LIST
         AGO   .FINI
.PROUT   ANOP                                                  @Z30EN9G
         AIF   ('&SP' EQ '').ERROR8                            @Z30EN9G
         AIF   ('&BRANCH' EQ '').ERROR8                        @Z30EN9G
         AGO   .PROUT1                                         @Z30EN9G
.*       R-FORM GETMAIN (REGMAIN) OR RC OR RU FORMS
.RROUT   AIF   ('&A' NE '').ERROR9
         AIF   ('&LV' EQ '').ERROR8
         AIF   (K'&MODE EQ 2).NREGM
&SVCN    SETA  10
         AIF   ('&LV'(1,1) EQ '(').ISARGA
         AIF   ('&SP' EQ '').CTUA
.PROUT1  ANOP                                                  @Z30EN9G
         AIF   ('&SP'(1,1) EQ '(').ISARGB
         CNOP  0,4
         AIF   ('&MODE' EQ 'P').PMODE                          @Z30EN9G
&NAME    BAL   1,*+8                             BRANCH AROUND SP+LV
         DC    AL1(&SP)                          SUBPOOL VALUE
         DC    AL3(&LV)                          LENGTH
         L     0,0(0,1)                          LOAD SP AND LV
         AGO   .FINI
.PMODE   ANOP                                                  @Z30EN9G
&NAME    LA    0,&SP.(0,0)      PICK UP SUBPOOL                @Z30EN9G
         SLL   0,24(0)          SHIFT TO HIGH-ORDER BYTE       @Z30EN9G
         BAL   1,*+4            INDICATE GETMAIN               @Z30EN9G
         AGO   .FINI                                           @Z30EN9G
.CTUA    AIF   (T'&LV NE 'N').CTUAA
         AIF   (&LV LE 4095).CONTCC
.CTUAA   CNOP  0,4
&NAME    BAL   1,*+8                             BRANCH AROUND LENGTH
         DC    A(&LV)                            LENGTH
         L     0,0(0,1)                          LOAD LENGTH
         AGO   .FINI
.CONTCC  ANOP
&NAME    LA    0,&LV.(0,0)                       LOAD LENGTH
         AGO   .NOP2
.ISARGB  AIF   ('&MODE' EQ 'P').PMODE2                         @Z30EN9G
         AIF   (T'&LV NE 'N').CONTFF                           @Z30EN9G
         AIF   (&LV LE 4095).CONTEE
.CONTFF  CNOP  0,4
&NAME    BAL   1,*+8                             BRANCH AROUND LENGTH
         DC    A(&LV)                            LENGTH
         LR    0,&SP(1)                          PICK UP SUBPOOL
         SLL   0,24(0)                           SHIFT TO HI-ORDER BYTE
         O     0,0(0,1)                          PACK SP AND LV
         AGO   .FINI
.PMODE2  ANOP                                                  @Z30EN9G
         AIF   ('&SP(1)' EQ '0').PMODE3                        @Z30EN9G
&NAME    LR    0,&SP(1)         PICK UP SUBPOOL                @Z30EN9G
         CNOP  0,4                                             @Z30EN9G
         SLL   0,24(0)          SHIFT TO HIGH-ORDER BYTE       @Z30EN9G
         BAL   1,*+4            INDICATE GETMAIN               @Z30EN9G
         AGO   .FINI                                           @Z30EN9G
.PMODE3  ANOP                                                  @Z30EN9G
         CNOP  0,4                                             @Z30EN9G
&NAME    SLL   0,24(0)          SHIFT SUBPOOL TO HIGH-BYTE     @Z30EN9G
         BAL   1,*+4            INDICATE GETMAIN               @Z30EN9G
         AGO   .FINI                                           @Z30EN9G
.CONTEE  ANOP
&NAME    LR    0,&SP(1)                          PICK UP SUBPOOL
         SLL   0,24(0)                           SHIFT TO HI-ORDER BYTE
         LA    1,&LV.(0,0)                       LOAD LENGTH
         OR    0,1                               PACK SP AND LV
         AGO   .NOP2
.ISARGA  AIF   ('&LV(1)' EQ '0').ZEROUT
         AIF   ('&SP' EQ '').CTUB
         AIF   ('&SP'(1,1) EQ '(').ISARGC
&NAME    LA    0,&SP.(0,0)                       PICK UP SUBPOOL
.NOP1    SLL   0,24(0)                           SHIFT TO HI-ORDER BYTE
         OR    0,&LV(1)                          PACK SP AND LV
.NOP2    BAL   1,*+4                             INDICATE GETMAIN
         AGO   .FINI
.CTUB    ANOP
&NAME    LR    0,&LV(1)                          LOAD LENGTH
         AGO   .NOP2
.ISARGC  ANOP
&NAME    LR    0,&SP(1)                          PICK UP SUBPOOL
         AGO   .NOP1
.ZEROUT  AIF   ('&SP' NE '').ERROR0
&NAME    BAL   1,*+4                             INDICATE GETMAIN
         AGO   .FINI
.NREGM   ANOP    RC AND RU FORMS OF GETMAIN
&SVCN    SETA  120
&UNCND   SETB  ('&MODE' EQ 'RU')  CONDITIONAL REQUEST FLAG
         AIF   (NOT &BR).COND     CHECK FOR BRANCH ENTRY TO GETMAIN
&PRG     SETA  3                  SET PARAMETER REG FOR BRANCH ENTRY
.COND    AIF   ('&SP' EQ '').CREG1
&SPREG   SETB  ('&SP'(1,1) EQ '(')
.CREG1   AIF   ('&KEY' EQ '').CREG01
&KEYRG   SETB  ('&KEY'(1,1) EQ '(')
.CREG01  ANOP
&LVREG   SETB  ('&LV'(1,1) EQ '(')
&B       SETB  (&SPREG AND &KEYRG)
         CNOP  0,4
&NAME    B     *+12-4*&LVREG-2*&B                BRANCH AROUND DATA
         AIF   (&LVREG).CREG11
         DC    A(&LV)                            LENGTH
.CREG11  ANOP
&GNAME.F DC    AL1(0)                            RESERVED
         AIF   ('&KEY' EQ '').CREG1A             KEY OMITTED ?
         AIF   (&KEYRG).CREG1B                   KEY IN REGISTER?
         DC    AL1(&KEY*16)                      STORAGE KEY
         AGO   .CREG1B
.CREG1A  DC    AL1(0)                            RESERVED
.CREG1B  AIF   ('&SP' EQ '').SPNULL1             SUBPOOL OMITTED?
         AIF   (&SPREG).CREG1C                   SUBPOOL IN REGISTER?
         DC    AL1(&SP)                          SUBPOOL
         AGO   .CREG1C
.CREG1E  AIF   ('&LV(1)' EQ '0').CREG1D                        @ZA07133
         LR    0,&LV(1)                          LOAD LENGTH   @ZA07133
         AGO   .CREG1D                                         @ZA07133
.SPNULL1 DC    AL1(0)                            SUBPOOL
.CREG1C  DC    BL1'0&RL&VA&VB.0&BND&UNCND.0'     MODE BYTE *MVS380*
         AIF   (&LVREG).CREG1E                                 @ZA07133
         L     0,*-8+2*&B                        LOAD LENGTH
.CREG1D  AIF   (&KEYRG OR &SPREG).KORSREG
.*       NEITHER KEY OR SP IS A REGISTER.
         L     &PRG.,&GNAME.F                    LOAD GETMAIN PARMS
         AGO   .LVCHK
.*       EITHER KEY OR SP IS A REGISTER.
.KORSREG AIF   (&KEYRG AND &SPREG).BOTHREG
.*       ONLY ONE OF THEM IS A REGISTER
         AIF   (NOT &SPREG).KEYREG
.*       ONLY SP IS A REGISTER
         AIF   ('&SP(1)' EQ '&PRG').SPINPRG
.*       SP IS NOT IN THE PREFERRED PARM REG.
         LR    &PRG.,&SP(1)                      OBTAIN SUBPOOL ID
.SPINPRG SLL   &PRG.,8(0)                 MOVE SUBPOOL TO BYTE 2 YM1995
         ICM   &PRG.,13,&GNAME.F                 ADD REMAINING PARMS
         AGO   .LVCHK
.* ONLY KEY IS A REGISTER
.KEYREG  AIF   ('&KEY(1)' EQ '&PRG').KYINPRG
.*       KEY IS NOT IN THE PREFERRED REGISTER
         LR    &PRG.,&KEY(1)                     GET STORAGE KEY
.KYINPRG SLL   &PRG.,16(0)                   SHIFT KEY TO BYTE 1 YM1995
         ICM   &PRG.,11,&GNAME.F                 ADD REMAINING PARMS
         AGO      .LVCHK
.*       BOTH KEY AND SP ARE IN REGISTERS
.BOTHREG AIF   ('&KEY(1)' NE '&SP(1)').NOTSAME
.*       BOTH KEY AND SP ARE IN THE SAME REGISTER.
         AIF   ('&KEY(1)' EQ '&PRG').BOTHPRG
.*       THE COMMON REGISTER IS NOT THE PREFERRED PARM REGISTER.
         LR    &PRG.,&KEY(1)                     GET STORAGE KEY
.BOTHPRG ICM   &PRG.,9,&GNAME.F                  ADD REMAINING PARMS
         AGO   .LVCHK
.* BOTH ARE IN REGISTERS, BUT THEY ARE DIFFERENT REGISTERS.
.NOTSAME AIF   ('&KEY(1)' EQ '&PRG' OR '&SP(1)' EQ '&PRG').PRGIS1
.*       NEITHER REGISTER IS THE PREFERRED PARM REGISTER.
         LR    &PRG.,&KEY(1)                     GET STORAGE KEY
.ADDSP   SLL   &PRG.,8(0)                  SHIFT KEY OVER FOR SP YM1995
         OR    &PRG.,&SP(1)                      ADD SUBPOOL NUMBER
         SLL   &PRG.,8(0)                 MOVE PAIR TO BYTES 1-2 YM1995
         AGO   .BOTHPRG
.PRGIS1  AIF   ('&KEY(1)' EQ '&PRG').ADDSP
.*       SP IN IN THE PREFERRED PARM REGISTER.
         SLL   &PRG.,8(0)                 MOVE SUBPOOL TO BYTE 2 YM1995
         SLL   &KEY(1),16(0)                 SHIFT KEY TO BYTE 1 YM1995
         OR    &PRG.,&KEY(1)                     COMBINE KEY & SP
         AGO   .BOTHPRG
.LVCHK   ANOP                                                  @ZA07133
.FINI    AIF   ('&MODE' NE 'RC' AND '&MODE' NE 'RU').FINI1
         SR    1,1                               ZERO RESERVED REG 1
.FINI1   AIF   (&BR).SETBE                    TEST FOR BRANCH=YES
         SVC   &SVCN                             ISSUE GETMAIN SVC
         MEXIT
.SETBE   L     15,CVTPTR(0,0)                    LOAD THE CVT ADDRESS
         AIF   ('&MODE' EQ 'P').CBBE                           @Z30EN9G
         AIF   (&SVCN EQ 120).CRBE
         AIF   (&SVCN EQ 10).RMBE
         L     15,CVTGMBR-CVTMAP(0,15)           GETMAIN ENTRY ADDRESS
         AGO   .SBE
.CBBE    ANOP                                                  @Z30EN9G
         L     15,CVTCBBR-CVTMAP(0,15)  GETMAIN ENTRY ADDRESS  @Z30EN9G
         AGO   .SBE                                            @Z30EN9G
.RMBE    L     15,CVTRMBR-CVTMAP(0,15)           GETMAIN ENTRY ADDRESS
         AGO   .SBE
.CRBE    AIF   (&GLBR).GLBE   GLOBAL BRANCH ENTRY?
         L     15,CVTCRMN-CVTMAP(0,15)           GETMAIN ENTRY ADDRESS
         AGO   .SBE
.GLBE    L     4,CVTSPSA-CVTMAP(0,15)            SAVE AREA VECTOR
         L     4,WSAGGMFM-WSAG(0,4)              GLOBAL SAVE AREA ADDR
         L     15,CVTGLMN-CVTMAP(0,15)           GLBRANCH ENTRY ADDR
.SBE     BALR  14,15                             BRANCH TO GETMAIN
         MEXIT
.ERROR0  IHBERMAC 92
         MEXIT
.ERROR1  IHBERMAC 17
         MEXIT
.ERROR2  IHBERMAC 1001,MF,&MF
         MEXIT
.ERROR3  IHBERMAC 69
         MEXIT
.ERROR4  IHBERMAC 89
         MEXIT
.ERROR5  IHBERMAC 91
         MEXIT
.ERROR6  IHBERMAC 90
         MEXIT
.ERROR7  IHBERMAC 1001,MODE,&MODE
         MEXIT
.ERROR8  IHBERMAC 01
         MEXIT
.ERROR9  IHBERMAC 93
         MEXIT
.ERROR10 IHBERMAC 1014,BNDRY
         MEXIT
.ERR10A  IHBERMAC 1020,&BNDRY,&MODE
         MEXIT
.ERRORA  IHBERMAC 1020,KEY,&MODE
         MEXIT
.ERRORB  IHBERMAC 1001,KEY,&KEY
         MEXIT
.ERRORC  IHBERMAC 1020,BRANCH,''MF=L''
         MEXIT
.ERRORD  IHBERMAC 1001,BRANCH,&BRANCH
         MEXIT
.ERRORE  IHBERMAC 1020,KEY,''BRANCH=''&BRANCH
         MEXIT
.ERRORF  IHBERMAC 1020,&BRANCH(2),&MODE
         MEND
* */
* GETMAIN: MACRO KEYS(LV,LA,A,SP,BNDRY,BRANCH,MF,RTCD,KEY,RELATED);
*          ANS('?'³³MACLABEL³³'GETMAINP '³³MACLIST³³MACKEYS³³';');
*%     END GETMAIN;
/*
//SMPCNTL  DD  *
  RECEIVE
          SELECT(ZP60033)
          .
/*
//*
//APPLYCK EXEC SMPAPP,WORK='SYSALLDA'
//SMPCNTL  DD  *
  APPLY
        SELECT(ZP60033)
        CHECK
        .
/*
//*
//APPLY   EXEC SMPAPP,COND=(0,NE),WORK='SYSALLDA'
//SMPCNTL  DD  *
  APPLY
        SELECT(ZP60033)
        DIS(WRITE)
        .
/*
//
//ZP60034  JOB (SYSGEN),'J08 M45: ZP60034',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             REGION=4096K
/*JOBPARM LINES=100
//JOBCAT   DD  DSN=SYS1.VSAM.MASTER.CATALOG,DISP=SHR
//*
//*  RESOLVE &SYSUID, AND SUPPLY USER= AND PASSWORD= ON JOB STATEMENT.
//*
//STEP1   EXEC PGM=IEBGENER
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  *
++USERMOD(ZP60034)                  /* RESOLVE &SYSUID IN JCL */  .
++VER(Z038) FMID(EBB1102) SUP(ZJW0001)
 /*
   PROBLEM DESCRIPTION:
     THE &SYSUID SYSTEM SYMBOL IS NOT RESOLVED IN SUBMITTED JCL.
       THE &SYSUID SYSTEM SYMBOL CAN BE VERY USEFUL IN REDUCING
       THE CUSTOMIZATION THAT SHIPPED SAMPLE JCL REQUIRES BEFORE
       BEING SUBMITTED, BUT THIS IS NOT SUPPORTED BY MVS 3.8.
     JOBS SUBMITTED BY TSO USERS DO NOT INHERIT THE USER ID.
       USER= AND PASSWORD= MUST MANUALLY BE SUPPLIED BY A TSO USER
       SUBMITTING BATCH JOBS FOR THE JOBS TO RUN WITH THE USER'S
       SECURITY PROFILE, WHICH INCREASES THE RISK THAT THE SECRECY
       OF THE USER'S PASSWORD CAN BECOME COMPROMISED.

       THIS USERMOD SHIPS A VERSION OF THE IKJEFF10 EXIT FOR THE
       TSO SUBMIT COMMAND WHICH APPENDS THE USER AND PASSWORD
       PARAMETERS TO THE JOB JCL STATEMENT IF NOT ALREADY ADDED
       IN AN ENVIRONMENT WHERE A SECURITY PRODUCT IS ACTIVE.
       THIS EXIT HAS BEEN ENHANCED TO RESOLVE THE &SYSUID SYSTEM
       SYMBOL (WITH TRAILING PERIOD IF PRESENT) WHEN FOUND IN THE
       OPERANDS (MEANING NOT IN LABELS OR VERBS) OF THE FOLLOWING
       TYPES OF JCL STATEMENT:

         - JOB
         - DD
         - COMMENT
         - COMMAND

       THE FOLLOWING TYPES OF JCL STATEMENT ARE IGNORED BY THIS EXIT:
         - EXEC
         - JES2 JECL
         - NULL

       NOTE THAT &&SYSUID WILL BE LEFT UNALTERED.

       &SYSUID RESOLUTION DOES NOT REQUIRE AN ACTIVE SECURITY PRODUCT.

   SPECIAL CONDITIONS:
     NONE.

   COMMENTS:
     PRYCROFT SIX P/L PUBLIC DOMAIN USERMOD FOR MVS 3.8 NUMBER 34.

     THE FOLLOWING MODULES AND/OR MACROS ARE AFFECTED BY THIS USERMOD:
     MODULES:
       IKJEFF10
 */.
++MOD(IKJEFF10) DISTLIB(ACMDLIB).
/*
//SYSUT2   DD  DSN=&&SMPMCS,DISP=(NEW,PASS),UNIT=SYSALLDA,
//             SPACE=(CYL,3),
//             DCB=(DSORG=PS,RECFM=FB,LRECL=80,BLKSIZE=4080)
//SYSIN    DD  DUMMY
//*
//STEP2   EXEC PGM=IFOX00,PARM='OBJECT,NODECK,NOTERM,XREF(SHORT),RENT'
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  UNIT=SYSALLDA,SPACE=(CYL,10)
//SYSUT2   DD  UNIT=SYSALLDA,SPACE=(CYL,10)
//SYSUT3   DD  UNIT=SYSALLDA,SPACE=(CYL,10)
//SYSLIB   DD  DSN=SYS1.MACLIB,DISP=SHR
//         DD  DSN=SYS1.SMPMTS,DISP=SHR
//         DD  DSN=SYS1.AMODGEN,DISP=SHR
//SYSGO    DD  DSN=&&SMPMCS,DISP=(MOD,PASS)
//SYSIN    DD  *
IKJEFF10 TITLE ' TSO SUBMIT EXIT FOR &&SYSUID, USER AND PASSWORD '
*
*  THIS VERSION OF THE IKJEFF10 EXIT FOR THE TSO SUBMIT COMMAND HAS
*  BEEN MODIFIED TO ALLOW THE JCL SYMBOL &SYSUID TO BE RESOLVED TO
*  THE ACTUAL USER ID OF THE SUBMITTER AT SUBMIT TIME.
*
*  THIS SUPPORT IS EXPECTED TO REDUCE - AND IN SOME CASES ELIMINATE -
*  THE MANUAL CUSTOMIZATION REQUIRED BY SAMPLE JCL BEFORE SUBMISSION.
*
*  WHILE THE MAIN INTEREST IS RESOLVING &SYSUID IN THE JOB NOTIFY
*  PARAMETER AND THE DD DSNAME PARAMETER, IT WILL BE RESOLVED ANYWHERE
*  IN JOB STATEMENT OPERANDS (SUCH AS THE ACCOUNT OR PROGRAMMER NAME),
*  IN DD STATEMENT OPERANDS (SUCH AS DCB), COMMANDS (GOOD PRACTICE
*  MEANS THESE ARE USUALLY DISABLED, BUT IF ALLOWED, THEN WHY NOT?),
*  AND JCL COMMENTS.  EXEC STATEMENTS ARE NOT PROCESSED BY THIS EXIT.
*
*  JCL STATEMENT LABELS AND VERBS ARE NOT SCANNED BY THIS EXIT.
*
*  USE A DOUBLE AMPERSAND TO PREVENT SUBSTITUTION.  THE DOUBLE
*  AMPERSAND WILL REMAIN UNCHANGED AND WILL NOT BE CONVERTED TO A
*  SINGLE AMPERSAND.
*
*  &SYSUID. (WITH A TRAILING PERIOD) WILL BE CHANGED TO THE SUBMITTER'S
*  USER ID - AS WILL &SYSUID (WITHOUT A TRAILING PERIOD) IF FOLLOWED
*  IMMEDIATELY BY A BLANK, COMMA, AMPERSAND, OR EITHER PARENTHESIS.
*
*  THE &SYSUID RESOLUTION WILL BE PERFORMED WHEN THE USER ID CAN BE
*  ASCERTAINED FROM THE RACF USER ID OR THE UADS USER ID EVEN IF NO
*  SECURITY PACKAGE SUCH AS RACF IS INSTALLED ON THE SYSTEM.
*
*  THIS EXIT'S ORIGINAL PROCESSING OF APPENDING USER AND PASSWORD
*  PARAMETERS ON TO THE JOB STATEMENT WILL ONLY OCCUR IF A SECURITY
*  PRODUCT IS PRESENT.
*
*  GREG PRICE, MARCH 2017
*
*
*  THE PROGRAM'S ORIGINAL COMMENTS FROM DECADES AGO NOW FOLLOW:
         EJECT
*
*  THIS EXIT INSERTS A CONTINUATION OF EACH JOB CARD SUBMITTED BY
*  A RACF DEFINED USER. THE CONTINUATION CARD CONTAINS THE USER ID
*  AND LOGON PASSWORD OF THE PERSON SUBMITTING THE JOB. IF THERE
*  IS NO ROOM TO INSERT A COMMA AND A BLANK THE JOB IS SUBMITTED
*  WITHOUT ADDING A CONTINNUATION CARD AND A MESSAGE IS SENT TO THE
*  USER INFORMING THEM OF THIS. IF THE USER IS NOT RACF DEFINED OR
*  EITHER 'USER' OR 'PASSWORD' KEY WORDS ARE FOUND THEN THE JOB IS
*  PASSED ON ASIS AND NO MESSAGE IS SENT. YOU CANNOT GET SOMEONE
*  ELSE'S PASSWORD BY USING THIS EXIT AS WRITTEN.
*
*  THIS EXIT IKJEFF10 REPLACES THE IBM VERSION OF IKJEFF10 WHICH IS
*  EFECTIVELY A BR14.  THIS EXIT WORKS WITH OR WITHOUT THE TSO/E OR
*  THE EARLIER TSO COMMAND PACKAGE AS THE DUMMY EXIT IS IN THE BASE
*  TSO CODE.  THIS EXIT WAS DEVELOPED AT THE GEORGIA DEPARTMENT OF
*  LABOR AND HAS BEEN IN USE FOR OVER ONE YEAR WITH NO KNOWN PROBLEMS.
*  WE WILL ATTEMPT TO FIX ERRORS AS LONG AS WE CONTINUE TO USE THIS
*  EXIT, BUT DO NOT PROMISE THAT WE WILL FIX BUGS OR PROVIDE ANY
*  SUPPORT IN THE FUTURE.
*
*  SEND COMMENTS AND ERROR REPORTS TO:
*        SYSTEMS SUPPORT UNIT
*        GEORGIA DEPARTMENT OF LABOR
*        ROOM 370 STATE LABOR BUILDING
*        ATLANTA, GA  30334
*
R0       EQU 0  OS LINKAGE
R1       EQU 1  OS LINKAGE - POINTER TO POINTER TO PARM LIST (IEEXITL)
R2       EQU 2  WORK REGISTER FOR GETMAIN, CVT, USERJCL, AND GENCD
R3       EQU 3  BASE REGISTER FOR ASCB, TCB, JSCB, AND PSCB
R4       EQU 4  BASE REGISTER FOR ASXB
R5       EQU 5  BASE REGISTER FOR ACEE
R6       EQU 6  BASE REGISTER FOR TSB
R7       EQU 7  BASE REGISTER FOR IEEXITL
R8       EQU 8  BASE REGISTER FOR IESUBCTD
R9       EQU 9  WORK REGISTER FOR CHANGING USERJCL (BASE FROM IECARDP)
R10      EQU 10 WORK REGISTER FOR USERJCL, MSGTEXT1, AND GENCD
R11      EQU 11 BASE REGISTER FOR GETMAIN AREA (STORED IN IEEXITWD)
R12      EQU 12 BASE REGISTER
R13      EQU 13 SAVE AREA
R14      EQU 14 OS LINKAGE
R15      EQU 15 OS LINKAGE - RETURN CODE FOR CALLING PROGRAM IKJEFF09
*
         USING PSA,0
         USING IKJEFF10,R12
IKJEFF10 CSECT
         SAVE  (14,12),,IKJEFF10-SUBMIT-USER-EXIT-&SYSDATE-&SYSTIME
         LR    R12,R15
*
*                                    REGISTERS NOT CHAINED UNTIL AFTER
*                                      GETMAIN
*
         L     R7,0(0,R1)            GET ADDRESS OF PARM LIST (IEEXITL)
         USING IEEXITL,R7
         L     R8,IESUBCTP           GET ADDRESS OF SUBMIT JCL INFO
         USING IESUBCTD,R8
*
         ICM   R11,15,IEEXITWD       FIRST INVOCATION OF THIS EXIT?
         BNZ   CONTINUA              NO, GO AROUND GETMAIN
*
         LA    R2,SIZDATD
         GETMAIN  R,LV=(2),SP=230
         LR    R11,R1                GET ADDRESS OF GETMAIN DSECT
         USING DATD,R11
         ST    R11,IEEXITWD          POINT TO GETMAIN AREA
         XC    DATD(SIZDATD),DATD    CLEAR GETMAIN AREA
*
         L     R3,PSATOLD            GET ADDRESS OF CURRENT TCB
         L     R3,TCBJSCB-TCB(,R3)   GET ADDRESS OF CURRENT JSCB
         USING IEZJSCB,R3
         L     R3,JSCBPSCB           GET ADDRESS OF PSCB
         DROP  R3                    (IEZJSCB)
         LTR   R3,R3                 ANY PSCB FOUND?
         BZ    DONEPSCB              NO
         USING PSCB,R3
         MVC   SAVEUSRI,PSCBUSER
         MVI   SAVEUSRI+7,BLANK
         MVC   SAVEUSRL,PSCBUSRL
         DROP  R3                    (PSCB)
         MVI   IETAKEEX,IETJOB+IETDD+IETCMD+IETCOMNT
DONEPSCB EQU   *
*
         L     R3,PSAAOLD            GET ADDRESS OF CURRENT ASCB
         USING ASCB,R3
         L     R4,ASCBASXB           GET ADDRESS OF CURRENT ASXB
         USING ASXB,R4
         L     R5,ASXBSENV           GET ADDRESS OF CURRENT ACEE
         LTR   R5,R5                 IS THERE AN ACEE ADDRESS?
         BZ    TAKEEXOF              NO, CAN'T INSERT CARD
         USING ACEE,R5
         TM    ACEEFLG1,ACEERACF     IS USER RACF DEFINED?
         BZ    TAKEEXOF              NO, NO MORE JOB CARDS TO COME
*                                      TO THIS EXIT FOR THIS SUBMIT
         L     R6,ASCBTSB            GET ADDRESS OF TSB
         LTR   R6,R6                 IS THERE A TSB ADDRESS?
         BZ    TAKEEXOF              NO, CAN'T INSERT CARD
*
         USING TSB,R6
         MODESET KEY=ZERO
         MVC   SAVEPSWD,TSBPSWD      SAVE PASSWORD FROM TSB
         MODESET KEY=NZERO
         CLI   SAVEPSWD,BLANK        IS THERE A LOGON PASSWORD?
         BNH   TAKEEXOF              NO, CAN'T INSERT CARD
*
         MVC   SAVEUSER,ACEEUSER     SAVE PASSWORD FROM TSB
         MVI   IETAKEEX,IETJOB+IETDD+IETCMD+IETCOMNT
         B     CONTINUA
         DROP  R3,R4,R5,R6           (ASCB,ASXB,ACEE,TSB)
*
TAKEEXOF EQU   *
         OI    SW,NOACEE             NO USABLE SECURITY FOR THIS ID
         MVI   IETAKEEX,ZEROHEX      TURN OFF TAKE EXIT SWITCH
         CLI   SAVEUSRL,ZEROHEX      GOT A USER ID?
         BE    SETRC0                NO, CAN'T DO MUCH HERE
         CLI   SAVEUSRL,SEVENX       SENSIBLE LENGTH?
         BH    SETRC0                NO, CAN'T DO MUCH HERE
         MVI   IETAKEEX,IETJOB+IETDD+IETCMD+IETCOMNT
*        B     CONTINUA              YES, CAN PROCESS &SYSUID
*
CONTINUA EQU   *
         LA    R1,SAVE
         ST    R13,4(,R1)            BACK CHAIN SAVE AREAS
         ST    R1,8(,R13)            FORWARD CHAIN SAVE AREAS
         LR    R13,R1                SET R13 TO NEW SAVE AREA
         L     R1,4(R13)             SET R1 TO SAVE AREA AT ENTRY
         L     R1,24(,R1)            RESTORE R1 FROM SAVE AREA
*
         TM    IETAKEEX,IETJOB       TAKE EXIT FOR JOB CARD?
         BZ    SETRC0                SHOULD NOT BE IN THIS EXIT
*
         CLC   IEMSGP,ZEROS          RETURN FROM SENDING MESSAGE?
         BE    REENT                 NO,
*
         MVC   IEMSGP,ZEROS          YES, CLEAR POINTER TO MESSAGE
         B     SETRC0                THIS CARD ALREADY PROCESSED
*
REENT    EQU   *
         L     R9,IECARDP            GET ADDRESS OF CURRENT CARD
         LTR   R9,R9                 IS EXIT BEING RE-ENTERED?
         BZ    GENCD                 YES, INSERT USER AND PASSWORD CARD
*
         CLI   IEOPRAND,ZEROHEX      OPERAND COLUMN?
         BZ    NOOPRAND              NO OPERANDS ON THIS CARD
*
UIDSRCH  EQU   *
         SR    R2,R2
         IC    R2,IEOPRAND
         AR    R9,R2                 REGISTER TO POINT TO OPERAND
         BCTR  R9,0
*
         LA    R10,COMMALMT-SEVENX+2 GET MAX COLUMN COUNT TO EXAMINE
         SR    R10,R2                LESS FIRST PART OF CARD
         BNP   UIDDONE               NOT ENOUGH ROOM LEFT FOR &SYSUID
*
UIDLOOP  EQU   *
         CLC   =C'&&SYSUID',0(R9)    FOUND THE SYMBOL?
         BE    UIDMTCH               YES
UIDNEXT  EQU   *
         LA    R9,1(,R9)             NO, POINT TO NEXT CHARACTER
         BCT   R10,UIDLOOP           TRY AGAIN
         B     UIDDONE               &SYSUID NOT FOUND
*
UIDMTCH  EQU   *
         LA    R4,SEVENX(,R9)        POINT PAST MATCHED STRING
         CLI   0(R4),COMMA           FOLLOWED BY COMMA?
         BE    UIDSUB                YES, PERFORM SUBSTITUTION
         CLI   0(R4),AMPER           FOLLOWED BY AMPERSAND?
         BE    UIDSUB                YES, PERFORM SUBSTITUTION
         CLI   0(R4),BLANK           FOLLOWED BY A BLANK?
         BE    UIDSUB                YES, PERFORM SUBSTITUTION\
         CLI   0(R4),LPAREN          FOLLOWED BY AN OPEN BRACKET?
         BE    UIDSUB                YES, PERFORM SUBSTITUTION
         CLI   0(R4),RPAREN          FOLLOWED BY A CLOSE BRACKET?
         BE    UIDSUB                YES, PERFORM SUBSTITUTION
         CLI   0(R4),PERIOD          TRAILING PERIOD?
         BNE   UIDNEXT               NO, SO NOT A MATCH TO ACTION
         LA    R4,SEVENX+1(,R9)      YES, CONSUME IT AS WELL
UIDSUB   EQU   *
         BCTR  R9,0                  POINT BEHIND AMPERSAND
         CLI   0(R9),AMPER           REALLY DOUBLE AMPERSAND?
         LA    R9,1(,R9)             RESTORE POINTER
         BE    UIDNEXT               YES, SO NOT A MATCH TO ACTION
         MVC   0(SEVENX,R9),SAVEUSRI LOAD IN THE USERID
         SR    R0,R0
         IC    R0,SAVEUSRL           GET ITS LENGTH
         ALR   R9,R0                 POINT PAST LOADED USERID
         L     R5,IECARDP            GET ADDRESS OF CURRENT CARD
         LA    R5,COMMALMT(,R5)      GET ADDRESS OF SHUFFLE LIMIT
         MVC   1(8,R5),UIDSTAMP      STAMP THIS RECORD AS ALTERED
UIDSHFL  EQU   *
         CR    R9,R4                 TEXT AMENDMENT COMPLETE?
         BNL   REENT                 YES, NOW RESCAN FOR REPEATS
         MVI   0(R9),BLANK           NO, ADD A BLANK
         CR    R4,R5                 SHUFFLE SOURCE ALL GONE?
         BH    UIDSHFLD              YES, JUST KEEP INSERTING BLANKS
         MVC   0(1,R9),0(R4)         NO, SHUFFLE A BYTE UP
         CLI   0(R4),BLANK           IS SOURCE BYTE A BLANK?
         BE    UIDSHFLD              YES, DO NOT ADVANCE SOURCE
         LA    R4,1(,R4)             NO, INCREMENT SHUFFLE SOURCE
UIDSHFLD EQU   *
         LA    R9,1(,R9)             INCREMENT SHUFFLE TARGET
         B     UIDSHFL               CONTINUE THE SHUFFLE
*
UIDDONE  EQU   *
         TM    IESTMTYP,IESJOB       IS STATEMENT JOB CARD?
         BZ    SETRC0                NO, DONT INSERT
*
         L     R9,IECARDP            GET ADDRESS OF CURRENT CARD
         SR    R2,R2
         IC    R2,IEOPRAND
         AR    R9,R2                 REGISTER TO POINT TO OPERAND
         BCTR  R9,0
*
         LA    R10,COMMALMT          IN LOOP, LOOK AT 71 COLUMNS
         SR    R10,R2                LESS FIRST PART OF CARD
*
         SR    R2,R2                 CLEAR FOR QUOTE SEARCH
*
COMPQUOT EQU   *
         CLC   0(1,R9),QUOTE         IS IT A QUOTE MARK?
         BNE   CKQUOT                NO,
*
         LTR   R2,R2                 IS IT THE BEGINNING QUOTE?
         BZ    BEGQUOT               YES
*
         SR    R2,R2                 NO, END OF QUOTE, RESET SWITCH
         B     NEXTCOL
*
BEGQUOT  EQU   *
         LA    R2,1                  SET SWITCH ON FOR QUOTE
         B     NEXTCOL               AND GO TO NEXT COLUMN
*
CKQUOT   EQU   *
         LTR   R2,R2                 ARE WE IN A QUOTATION?
         BP    NEXTCOL               YES, DONT LOOK FOR PASSWORD=
*
COMPPSWD EQU   *
         CLC   0(9,R9),PSWDCON       IS IT PASSWORD=?
         BE    SETSW                 YES, DONT INSERT CARD
*
COMPUSER EQU   *
         CLC   0(5,R9),USRCON        IS IT USER=?
         BE    SETSW                 YES, DONT INSERT CARD
*
COMPBLK  EQU   *
         CLC   0(1,R9),BLANKS        END OF OPERANDS?
         BNE   NEXTCOL               NO,
*
         B     CONTOPER              YES
*
NEXTCOL  EQU   *
         LA    R9,1(R9)              TRY NEXT COLUMN
         BCT   R10,COMPQUOT          IF NOT AT END OF CARD, LOOP
*
         B     CONTOPER              YES, IS OPERAND CONTINUED?
*
SETSW    EQU   *
         OI    SW,UPHERE             TURN SWITCH ON, DONT NEED CARD
*
CONTOPER EQU   *
         TM    IESTMTYP,IESOPCON     IS OPERAND TO BE CONTINUED?
         BO    SETRC0                YES
*
         TM    SW,UPHERE+NOACEE      NEED TO INSERT CARD?
         BNZ   NOINSERT              NO, GO GET NEXT STATEMENT, IF ANY
*
         CLC   0(2,R9),BLANKS        ROOM FOR COMMA AND BLANK?
         BE    MVCOMMA               YES, CAN INSERT CARD
*
         CLC   0(1,R9),BLANKS        ROOM FOR JUST COMMA?
         BE    LASTCOL               MAYBE OK
*
         B     WARNING               CAN'T INSERT CARD
*
LASTCOL  EQU   *
         C     R10,ZEROS             AT COLUMN 71?
         BE    MVCOMMA               YES, CAN PUT COMMA WITHOUT BLANK
*                                       FOLLOWING
WARNING  EQU   *
         MVC   MSG,MSG1              MOVE MSG TO MSG AREA
         LA    R10,MSG               POINT REGISTER TO MESSAGE AREA
         ST    R10,IEMSGP            GIVE ADDRESS TO CALLER
         LA    R15,IEMSG             RC=8, TELL CALLER TO SEND MESSAGE
         B     RETURN
*
MVCOMMA  EQU   *
         MVI   0(R9),COMMA
*
         MVC   SAVETYP,IESTMTYP      SAVE SWITCHES
         OI    IESTMTYP,IESOPCON     NOW THERE IS A COMMA,
         OI    IESTMTYP,IESSCON        SO SET THE FLAGS
         LA    R15,IERETURN          RC=4, TELL CALLER TO RETURN FOR
         B     RETURN                    INSERTED CARDS
*
NOOPRAND EQU   *
         CLI   SW,ONE                USER OR PASSWORD ALREADY FOUND
         BNE   SETRC0                OR NOT FOUND AND GENERATED
*
NOINSERT EQU   *
         NI    SW,255-UPHERE         TURN SWITCH OFF FOR NEXT JOB CARD
*
SETRC0   EQU   *
         LA    R15,IECONTIN          RC=0, TELL CALLER TO COMPLETE
*
RETURN   EQU   *
         L     R13,4(R13)            RESTORE SAVE AREA
         RETURN (14,12),RC=(15)
*
GENCD    EQU   *
         LA    R10,CD               POINT WORK REGISTER TO GETMAIN AREA
         ST    R10,IECARDP          POINT TO INSERTED CARD FOR CALLER
         MVI   CD,BLANK
         MVC   CD+1(ENDCD-CD-1),CD  CLEAR CARD AREA
         MVC   0(19,R10),CDUSER     MOVE USER CONSTANT
         LA    R10,19(R10)
         MVC   0(8,R10),SAVEUSRI    MOVE USER TO CARD
         SR    R2,R2                CLEAR REG FOR LENGTH
         IC    R2,SAVEUSRL
         AR    R10,R2               BUMP PAST USER
         MVC   0(10,R10),CDPSWD     MOVE PASSWORD CONSTANT TO CARD
         LA    R10,10(R10)
         MVC   0(8,R10),SAVEPSWD    MOVE PASSWORD TO CARD
         MVC   CD+50(21),CDMSG      MOVE COMMENT CONSTANT
*
         MVI   IEOPRAND,FIFTEEN     MOVE OPERAND COLUMN NUMBER
*
         MVC   IESTMTYP,SAVETYP     RESTORE SWITCH
         TM    IESTMTYP,IESSCON     IS THIS LAST CARD?
         BZ    SETCONTN             YES
*
         LR    R10,R11              SET UP WORK REGISTER
         LA    R10,71(R10)          TO COLUMN 72
         MVI   0(R10),NONBLANK      AND MOVE X TO COLUMN 72
*
SETCONTN EQU   *
         OI    IESTMTYP,IESCONTN    SET CONTINUATION FLAG
         B     SETRC0               AND LEAVE
         TITLE ' CONSTANTS AND LITERALS '
ZEROS    DC    D'0'
*
*
USRCON   DC    CL5'USER='
PSWDCON  DC    CL9'PASSWORD='
BLANKS   DC    CL9'         '
QUOTE    DC    XL1'7D'
*
CDUSER   DC    CL19'//            USER='
CDPSWD   DC    CL10',PASSWORD='
CDMSG    DC    CL21'GENERATED BY IKJEFF10'
UIDSTAMP EQU   *-8,8
*
MSG1     EQU   *
MSGL1    DC    AL2(L'MSGTEXT1+2)
MSGTEXT1 DC    C'NO SPACE FOR COMMA - CAN NOT INSERT USER/PASSWORD'
*
MSGAREAL EQU   L'MSGTEXT1+2         LENGTH OF AREA IN GETMAIN AREA
*
COMMALMT EQU   71
BLANK    EQU   C' '
PERIOD   EQU   C'.'
AMPER    EQU   X'50'
LPAREN   EQU   C'('
RPAREN   EQU   C')'
COMMA    EQU   C','
ONE      EQU   C'1'
NONBLANK EQU   C'+'
ZEROHEX  EQU   X'00'
SEVENX   EQU   X'07'
FIFTEEN  EQU   X'0F'
*
         LTORG
*
         DC    0D'0'                END OF CSECT
         TITLE ' WORKING STORAGE '
DATD     DSECT
*
CD       DS    CL80                 CARD TO BE INSERTED IN JCL
ENDCD    EQU   *
*
SAVE     DS    CL72                 REGISTER SAVE AREA
*
MSG      DS    CL(MSGAREAL)         WARNING MESSAGE TO TSO USER
*
SW       DS    XL1                  FLAG BYTE
UPHERE   EQU   X'80'                FOUND USER OR PASSWORD ON JOB CARD
NOACEE   EQU   X'40'                THERE IS NO ACEE SO NO SECURITY
*
SAVEUSER DS    0CL9
SAVEUSRL DS    AL1
SAVEUSRI DS    CL8
*
SAVEPSWD DS    CL8
*
SAVETYP  DS    CL1
*
ENDDATD  DS    0D
SIZDATD  EQU   ENDDATD-DATD
         TITLE ' PARAMETER LIST '
         IKJEFFIE IETYPE=SUBMIT
         TITLE ' CONTROL BLOCKS '
         PRINT NOGEN
         IHAPSA
         SPACE
         IHAASCB
         SPACE
         IHAASXB
         SPACE
         IHAACEE
         SPACE
         IKJTSB   LIST=NO
         SPACE
         IKJTCB   LIST=NO
         SPACE
         IEZJSCB
         SPACE
         IKJPSCB
         SPACE
         END   IKJEFF10
/*
//*
//STEP3   EXEC PGM=IEBGENER
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  *
  IDENTIFY IKJEFF10('ZP60034')
/*
//SYSUT2   DD  DSN=&&SMPMCS,DISP=(MOD,PASS)
//SYSIN    DD  DUMMY
//*
//STEP4   EXEC SMPREC,WORK='SYSALLDA'
//SMPPTFIN DD  DSN=&&SMPMCS,DISP=(OLD,DELETE)
//SMPCNTL  DD  *
  RECEIVE
          SELECT(ZP60034)
          .
/*
//*
//STEP5   EXEC SMPAPP,WORK='SYSALLDA'
//SMPCNTL  DD  *
  APPLY
        SELECT(ZP60034)
        CHECK
        .
/*
//*
//STEP5   EXEC SMPAPP,COND=(0,NE),WORK='SYSALLDA'
//SMPCNTL  DD  *
  APPLY
        SELECT(ZP60034)
        DIS(WRITE)
        .
/*
//
//ZP60035  JOB (SYSGEN),'J09 M46: ZP60035',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             REGION=4096K
/*JOBPARM LINES=100
//JOBCAT   DD  DSN=SYS1.VSAM.MASTER.CATALOG,DISP=SHR
//*
//*  SUPPORT ANY MVS-SUPPORTED DASD FOR LOGREC - SYSMOD 1 OF 3
//*
//STEP1   EXEC PGM=IEBGENER
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  *
++USERMOD(ZP60035)          /* FIX LOGREC DEVICE TYPE SUPPORT */  .
++VER(Z038) FMID(EBB1102) PRE(UZ42622)
 /*
   PROBLEM DESCRIPTION:
     MVS DOES NOT SUPPORT SYS1.LOGREC ON ALL DASD DEVICE TYPES.
       THE DASD DEVICE TYPES THAT THE LOGREC WRITER (SVC 76)
       SUPPORTS IS HARD-CODED WITHIN THE MODULE.  THIS MEANS THAT
       IN ORDER MAINTAIN A FUNCTIONING LOGREC FACILTY THE SYSTEM
       RESIDENCE VOLUME IS LIMITED TO THE ORIGINAL SET OF DASD
       DEVICE TYPES AND CANNOT BE MIGRATED TO EXPLOIT LARGER VOLUME
       GEOMETRIES AS SUPPORT FOR NEWER DEVICES IS ADDED TO MVS.

       THIS USERMOD SHIPS A VERSION OF IFBSVC76 WHICH USES THE MVS
       TRKCALC SERVICE TO OBTAIN DASD GEOMETRY DEPENDENT DETAILS
       INSTEAD OF PRE-CODED VALUES SO THAT NOW IT SUPPORTS ANY
       DASD DEVICE TYPE SUPPORTED BY THE SYSTEM.

   SPECIAL CONDITIONS:
     ACTION:
       A "CLPA" MUST BE PERFORMED AT IPL TIME FOR THIS SYSMOD TO
       BECOME ACTIVE.

   COMMENTS:
     PRYCROFT SIX P/L PUBLIC DOMAIN USERMOD FOR MVS 3.8 NUMBER 35.

     THIS IS SYSMOD NUMBER 1 OF 3 IN A PACKAGE TO GENERALIZE LOGREC
     DASD SUPPORT WRITTEN BY TOM ARMSTRONG.  ALL 3 SYSMODS SHOULD BE
     ACTIVATED IN THE SAME IPL.  THE SYSMOD DETAILS ARE:
      +---------+----------+---------+------+-----------------------+
      | USERMOD | MODULE   | FMID    | COMP | MODULE FUNCTION       |
      +---------+----------+---------+------+-----------------------+
      | ZP60035 | IFBSVC76 | EBB1102 | BCP  | LOGREC WRITER         |
      | ZP60036 | IFCDIP00 | FBB1221 | SU64 | LOGREC INITIALIZATION |
      | ZP60037 | IFCIOHND | EER1400 | EREP | EREP I/O SERVICES     |
      +---------+----------+---------+------+-----------------------+

     THE FOLLOWING MODULES AND/OR MACROS ARE AFFECTED BY THIS USERMOD:
     MODULES:
       IFBSVC76
 */.
++MOD(IFBSVC76) DISTLIB(ALPALIB).
/*
//SYSUT2   DD  DSN=&&SMPMCS,DISP=(NEW,PASS),UNIT=SYSALLDA,
//             SPACE=(CYL,3),
//             DCB=(DSORG=PS,RECFM=FB,LRECL=80,BLKSIZE=4080)
//SYSIN    DD  DUMMY
//*
//STEP2   EXEC PGM=IFOX00,PARM='OBJECT,NODECK,NOTERM,XREF(SHORT),RENT'
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  UNIT=SYSALLDA,SPACE=(CYL,10)
//SYSUT2   DD  UNIT=SYSALLDA,SPACE=(CYL,10)
//SYSUT3   DD  UNIT=SYSALLDA,SPACE=(CYL,10)
//SYSLIB   DD  DSN=SYS1.MACLIB,DISP=SHR
//         DD  DSN=SYS1.SMPMTS,DISP=SHR
//         DD  DSN=SYS1.AMODGEN,DISP=SHR
//SYSGO    DD  DSN=&&SMPMCS,DISP=(MOD,PASS)
//SYSIN    DD  *
IFB      TITLE 'IFBSVC76 - IGC0007F - SVC 76 LOGREC RECORDER'           00000100
*                                                                       00000200
*        MODULE NAME - IGC0007F                                         00000300
*                                                                       00000400
*        DESCRIPTIVE NAME - IFBSTAT - SYS1.LOGREC RECORDER              00000500
*                                                                       00000600
*        STATUS -                                                       00000700
*        SOURCE CODE WAS UPDATED BY DISASSEMBLY TO REFLECT THE          00000800
*        OWNING PTF OF UZ42622. THE LOAD MODULE ASSEMBLED FROM          00000900
*        THE UPDATED SOURCE MATCHED THE MVS 3.8J DISTRIBUTION MODULE    00001000
*        PRIOR TO THE ENHANCEMENTS LISTED BELOW                         00001100
*                                                                       00001200
*        ENHANCEMENTS -                                                 00001300
*        1. IFBSVC76 WILL SUPPORT SYS1.LOGREC ON ANY DASD               00001400
*           UNIT TYPE SUPPORTED BY MVS 3.8                              00001500
*        2. THE MVS TRKCALC SERVICE IS USED TO DETERMINE IF A RECORD    00001600
*           TO BE WRITTEN WILL FIT ON A TRACK REPLACING A SIMPLE        00001700
*           NON STANDARD CALCULATION                                    00001800
*        3. NUMEROUS CHANGES TO IMPROVE SOURCE READABILITY              00001900
*        4. USE OF STANDARD SYSTEM MAPPING MACROS                       00002000
*        5. NOTE THAT THE BUFFERED LOG FOR DASD DEVICES 3375,           00002100
*           3380 AND 3390 ARE NOT RETRIEVED FROM THE DEVICE             00002200
*           AS HERCULES DOES NOT MAINTAIN THE DEVICE LOG BUFFERS        00002300
*                                                                       00002400
*        FUNCTION -                                                     00002500
*        IFBSTAT, SVC 76 IS THE SYS1.LOGREC RECORDER. ITS               00002600
*        FUNCTION PROVIDES A METHOD OF RECORDING TO THE                 00002700
*        SYS1.LOGREC DATASET. ADDITIONALLY, IF DEMF IS INSTALLED        00002800
*        AND RUNNING IN THE SYSTEM, THEN THE RECORD WILL BE POSTED      00002900
*        TO THE DEMF BUFFER FOR THE DEMF WRITER TO UPDATE THE DEMF      00003000
*        DATASET                                                        00003100
*                                                                       00003200
*        FUNCTION 1 - RECORD PREFORMATTED RECORD                        00003300
*                                                                       00003400
*        FUNCTION 2 - THIS PROVIDES A METHOD WHEREBY THE                00003500
*        INTERNAL STATISTICAL DATA, MAINTAINED BY THE IOS ERROR         00003600
*        ROUTINES, CAN BE RECORDED ON THE SYS1.LOGREC DATASET           00003700
*                 A - ALL STATISTICAL DATA WILL BE RECORDED             00003800
*                     ON THE SYS1.LOGREC DATA SET, IF THE INPUT         00003900
*                     PARAMETER IS 0 (HALT - END OF DAY), OR            00004000
*                     4(IFCEREPO)                                       00004100
*                 B - EXTRACTING CVTILK2 FROM THE CVT YIELDS            00004200
*                     A LIST OF UCB POINTERS. EACH UCB IS               00004300
*                     ACCESSED, AND ITS UCBSTI FIELD MULTIPLIED         00004400
*                     BY TEN AND ADDED TO THE CONTENTS OF THE           00004500
*                     CVTSTB FIELD IN THE CVT. THE EIGHT BYTES AT       00004600
*                     THE CALCULATED FIELD ARE CHECKED FOR ZERO.        00004700
*                     A ZERO FIELD CAUSES NO FURTHER ACTION TO          00004800
*                     BE TAKEN FOR THIS STATISTICAL FIELD. A NON        00004900
*                     ZERO FIELD IS SAVED IN GOTTEN CORE AND THE        00005000
*                     STATISTICAL COUNTER IS CLEARED                    00005100
*                 C - A SHORT OBR RECORD IS FORMATTED TO                00005200
*                     INCLUDE THE STATISTICAL DATA AREA                 00005300
*                 D - THE SHORT OBR RECORD IS THEN WRITTEN              00005400
*                     TO THE SYS1.LOGREC DATASET. THIS PROCESS          00005500
*                     IS REPEATED EACH TIME A STATISTICAL FIELD         00005600
*                     IS FOUND TO BE NON ZERO                           00005700
*                 E - FUNCTION 3 WILL BE GIVEN CONTROL                  00005800
*                     INLINE IF IT IS TO BE PERFORMED. IF               00005900
*                     FUNCTION 3 IS NOT TO BE PERFORMED THEN EXIT       00006000
*                     IS MADE VIA R14.                                  00006100
*                                                                       00006200
*        FUNCTION 3 - THIS FUNCTION CAN BE EITHER EXCLUSIVE             00006300
*        OF FUNCTION 2 IF THE INPUT PARAMETER IS 8 (IPL - VIA MASTER    00006400
*        SCHEDULER), OR IN ADDITION TO FUNCTION 2 IF THE INPUT          00006500
*        PARAMETER IS 0 (HALT - END OF DAY)                             00006600
*        TO CONTINUE THE IPL OR EOD RECORD PROCESSING WHEN IT           00006700
*        WAS DETERMINED THAT SUCH A RECORD WAS TO BE PLACED ON          00006800
*        SYS1.LOGREC                                                    00006900
*                 A - VIA A WTOR, INFORMATION IS GATHERED FROM          00007000
*                     THE OPERATOR TO ASSIST IN FORMULATING AN          00007100
*                     IPL RECORD. IF AN EOD IS THE TYPE REQUIRED        00007200
*                     CONSTRUCTION OF THE RECORD IS PERFORMED           00007300
*                     WITHOUT THE AID OF THE OPERATOR. THE              00007400
*                     OPERATOR'S REPLY TO THE SYSTEM IS CHECKED         00007500
*                     AND IF FOUND TO BE INVALID A MESSAGE IS           00007600
*                     ISSUED TO THAT EFFECT, AND THE INITIAL WTOR       00007700
*                     IS REISSUED.                                      00007800
*                 B - AFTER CONSTRUCTION OF THE RECORD (EOD OR          00007900
*                     IPL) IS COMPLETED, CONTROL IS PASSED TO           00008000
*                     FUNCTION 1 FOR RECORDING OF THE RECORD            00008100
*                                                                       00008200
*        REGISTER CONVENTIONS -                                         00008300
*        R0, R1 INPUT PARAMETER REGISTERS                               00008400
*        R2  ->  CVT                                                    00008500
*        R3      BASE REG                                               00008600
*        R4-R13  WORK REGS                                              00008700
*        R14     RETURN ADDR                                            00008800
*        R15     RETURN CODE                                            00008900
*                                                                       00009000
*        STANDARD SVC REGISTER CONVENTIONS -                            00009100
*        R0, R1 INPUT PARAMETER REGISTERS                               00009200
*        R2  UNDEFINED                                                  00009300
*        R3  -> CVT                                                     00009400
*        R4  -> TCB                                                     00009500
*        R5  -> SVRB                                                    00009600
*        R6  -> ENTRY POINT                                             00009700
*        R7  -> ASCB                                                    00009800
*        R8,R12 UNDEFINED                                               00009900
*        R13    CALLERS VALUE                                           00010000
*        R14    RETURN ADDR                                             00010100
*        R15    CALLERS VALUE                                           00010200
*                                                                       00010300
*        MODULE - TYPE -                                                00010400
*        PROCEDURE, TYPE 3 SVC, APF AUTHORIZATION SPECIFIED             00010500
*                                                                       00010600
*        ATTRIBUTES -                                                   00010700
*        PROTECT KEY ZERO, REENTRANT                                    00010800
*                                                                       00010900
*        ENTRY - POINT - IGC0007F                                       00011000
*                                                                       00011100
*        PURPOSE -                                                      00011200
*        RECORD TO SYS1.LOGREC DATASET                                  00011300
*        WRITE MESSAGES TO OPERATOR INVOKED BY ISSUING SVC 76           00011400
*                                                                       00011500
*        INPUT -                                                        00011600
*        PREFORMATTED SYS1.LOGREC RECORDING                             00011700
*        R0  - TWOS COMPLEMENT OF NUMBER OF BYTES TO BE RECORDED        00011800
*        R1 -> DATA TO BE RECORDED                                      00011900
*                                                                       00012000
*        INPUT -                                                        00012100
*        SYS1.LOGREC RECORDINGS FORMATTED BY SVC76                      00012200
*        R0 = 0 - ENTRY IS VIA HALT END OF DAY                          00012300
*        R0 = 4 - ENTRY IS VIA IFCEREPO UTILITY                         00012400
*        R0 = 8 - ENTRY IS VIA MASTER SCHEDULER(IPL)                    00012500
*        R0 = C - ENTRY IS FOR OUTAGE                                   00012600
*                                                                       00012700
*        OUTPUT -                                                       00012800
*        WRITE TO SYS1.LOGREC DATASET                                   00012900
*                                                                       00013000
*        OUTPUT -                                                       00013100
*        MESSAGES TO OPERATOR HH-HOUR, MM-MINUTE, SS-SECOND             00013200
*        IFB030I SYS1.LOGREC I/O ACCESS ERROR,HH.MM.SS                  00013300
*        IFB040I SYS1.LOGREC AREA IS FULL,HH.MM.SS                      00013400
*        IFB050I SYS1.LOGREC FORMAT ERROR,HH.MM.SS                      00013500
*        IFB060E SYS1.LOGREC NEAR FULL                                  00013600
*                                                                       00013700
*        EXIT - NORMAL -                                                00013800
*        (1) INSTRUCTION FOLLOWING THE SVC CALL                         00013900
*        R15 = ZERO                                                     00014000
*                                                                       00014100
*        EXIT - ERROR -                                                 00014200
*        (1) INSTRUCTION FOLLOWING THE SVC CALL                         00014300
*        R15 =  8 - STORAGE NOT AVAILABLE                               00014400
*                   (NO RECORDING ATTEMPTED)                            00014500
*        R15 = 12 - NO SPACE AVAILABLE (NO RECORDING ATTEMPTED)         00014600
*                 - RECORD OVERRIDE SWITCH SET                          00014700
*        R16 = 16 - PERMANENT I/O ERROR                                 00014800
*                 - FORMAT ERROR (HEADER RECORD)                        00014900
*                                                                       00015000
*        DATA AREAS -                                                   00015100
*        INPUT DATA AREA, CVT, LOGDCB, WORK AREA                        00015200
*        WORK AREA AQUIRED BY LOGREC RECORDER                           00015300
*                                                                       00015400
*        EXTERNAL REFERENCES -                                          00015500
*        CVTILK2 - ADDRESS OF THE TWO BYTE UCB POINTERS                 00015600
*        CVTSTB  - BASE ADDRESS VALUE OF STATISTICAL DATA               00015700
*        STATAB  - ONE BYTE STATISTICAL DATA AREA INDEX IN UCB          00015800
*        CVTPTR  - ADDRESS OF CVT                                       00015900
*                                                                       00016000
*        TABLES/WORK AREAS -                                            00016100
*        WORKAREA OBTAINED FROM SP-245                                  00016200
*        CVT, UCB'S, SER DCB, STATAB                                    00016300
*                                                                       00016400
*        LOGREC HEADER RECORD -                                         00016500
*                                                                       00016600
*       *************************************************************** 00016700
*       *              *                                *     HIGH    * 00016800
*  +0   *  F F F F     *          LOW EXTENT            *   EXTENT    * 00016900
*       *              *            (CCHH)              *      (CC)   * 00017000
*       *************************************************************** 00017100
*       * HIGH EXTENT  *       *         RECORD ENTRY AREA ADDRESS    * 00017200
*  +8   *   (CONT)     * SPARE *                (BBCCHHR)             * 00017300
*       *    (HH)      *       *                                      * 00017400
*       *************************************************************** 00017500
*       *  REC. ENTRY  *  REMAINING     *  TOTAL BYTES  *  ADDRESS OF * 00017600
*  +16  * AREA ADDR    *   BYTES ON     *      ON       *  LAST RECORD* 00017700
*       *    (CONT)    *     TRACK      *    TRACK      *    WRITTEN  * 00017800
*       *************************************************************** 00017900
*       *           ADDRESS OF LAST RECORD       * TRACKS      * EARLY* 00018000
*  +24  *                WRITTEN (CONT)          *    PER      * WARN * 00018100
*       *                                        * CYLINDER    * 90%  * 00018200
*       *************************************************************** 00018300
*       * EARLY * DEV- *     EARLY WARNING              * EARLY* CHECK* 00018400
*  +32  * WARN. *  ICE *               MESSAGE TRACK    * WARN * BYTE * 00018500
*       * L TRK * CODE *                                * SW   * (FF) * 00018600
*       *************************************************************** 00018700
*                                                                       00018800
*        OBR RECORD - SHORT FORM                                        00018900
*                                                                       00019000
*       *********************************************************       00019100
*       * REC  * OP   *                           * REC  *      *       00019200
*  +0   * TYPE * SYS  *         SWITCHES          * CNT  *SPARE *       00019300
*       *      *      *                           *      *      *       00019400
*       *********************************************************       00019500
*       *                           *                           *       00019600
*  +8   *           DATE            *         TIME              *       00019700
*       *                           *                           *       00019800
*       *********************************************************       00019900
*       *      *      CPU           *   MODEL     *    MAX      *       00020000
*  +16  * RSVD *      SERIAL        *   NUMBER    *    MCEL     *       00020100
*       *      *      NUMBER        *             *    LENGTH   *       00020200
*       *********************************************************       00020300
*       *                           *       *                   *       00020400
*  +24  *        DEVICE TYPE        *  SOR  *        CUA        *       00020500
*       *                           * COUNT *                   *       00020600
*       *********************************************************       00020700
*       *                                                       *       00020800
*  +32  *               STATISTICAL  DATA  AREA   (10 BYTES)    *       00020900
*       *                                                       *       00021000
*       *********************************************************       00021100
*       *               *                                       *       00021200
*  +40  *               *       (20 BYTES)                      *       00021300
*       *               *                                       *       00021400
*       *********************************************************       00021500
*       *                           *                                   00021600
*  +48  *                           *                                   00021700
*       *                           *                                   00021800
*       *****************************                                   00021900
*                                                                       00022000
*        IPL RECORD FORMAT                                              00022100
*                                                                       00022200
*       *********************************************************       00022300
*       * REC  * OP   *                           * REC  *      *       00022400
*  +0   * TYPE * SYS  *         SWITCHES          * CNT  *SPARE *       00022500
*       *      *      *                           *      *      *       00022600
*       *********************************************************       00022700
*       *                           *                           *       00022800
*  +8   *           DATE            *         TIME              *       00022900
*       *                           *                           *       00023000
*       *********************************************************       00023100
*       *      *      CPU           *   MODEL     *    MAX      *       00023200
*  +16  * RSVD *      SERIAL        *   NUMBER    *    MCEL     *       00023300
*       *      *      NUMBER        *             *    LENGTH   *       00023400
*       *********************************************************       00023500
*       *SUB-   *  CHANNEL UNIT      *   REASON     *  CHANNEL  *       00023600
*  +24  *SYSTEM *   ADDRESS OF       *    FOR       *    MAP    *       00023700
*       *  ID   *  FAILING DEVICE    *    IPL       *           *       00023800
*       *********************************************************       00023900
*       *                                                       *       00024000
*  +32  *            CHANNEL    TYPE    ASSIGNMENT              *       00024100
*       *                                                       *       00024200
*       *********************************************************       00024300
*       *                            *                          *       00024400
*  +40  *        HIGHEST  ADDRESS    *      EXPANSION  BYTES    *       00024500
*       *             AT  IPL        *                          *       00024600
*       *********************************************************       00024700
*       *                                                       *       00024800
*  +48  *                       TIME STAMP                      *       00024900
*       *                                                       *       00025000
*       *********************************************************       00025100
*                                                                       00025200
*        EOD RECORD FORMAT                                              00025300
*                                                                       00025400
*       *********************************************************       00025500
*       * REC  * OP   *                           * REC  *      *       00025600
*  +0   * TYPE * SYS  *         SWITCHES          * CNT  *SPARE *       00025700
*       *      *      *                           *      *      *       00025800
*       *********************************************************       00025900
*       *                           *                           *       00026000
*  +8   *           DATE            *         TIME              *       00026100
*       *                           *                           *       00026200
*       *********************************************************       00026300
*       *      *      CPU           *   MODEL     *    MAX      *       00026400
*  +16  * RSVD *      SERIAL        *   NUMBER    *    MCEL     *       00026500
*       *      *      NUMBER        *             *    LENGTH   *       00026600
*       *********************************************************       00026700
*                                                                       00026800
IGC0007F CSECT                                                          00026900
*                                                                       00027000
*        DISPLACEMENT AND LENGTH EQUATES                                00027100
*                                                                       00027200
CHASTBLN EQU   8                       LENGTH OF CHANNEL ASSIGNMENT TAB 00027300
EODSIZE  EQU   24                      LENGTH OF AN EOD RECORD          00027400
IPLSIZE  EQU   56                                                       00027500
SDRDATA  EQU   8                       LENGTH OF COUNTERS IN A 10 BYTE  00027600
*                                      STAT TAB                         00027700
SDRLEN   EQU   10                      LENGTH OF NORMAL STAT FIELD      00027800
NEWSDRDT EQU   18                      LENGTH OF STAT COUNTERS IN A     00027900
*                                      20 BYTE STAT FLD                 00028000
NEWSDRLN EQU   20                      LENGTH OF A 20 BYTE STAT FIELD   00028100
RECLNW10 EQU   42                      LENGTH OF RECORD WITH A 10 BYTE  00028200
*                                      STAT FIELD                       00028300
RECLNW20 EQU   52                      LENGTH OF RECORD WITH A 20 BYTE  00028400
*                                      STAT FIELD                       00028500
*        EQUATES FOR BIT AND MASK POSITIONS                             00028600
*                                                                       00028700
VS2IND   EQU   X'80'                   VS2 RELEASE INDICATOR            00028800
AOSFLG   EQU   X'40'                   INDICATE AOS IN REL NO           00028900
TIMEBIT  EQU   X'08'                   INDICATE TIME MACRO WAS USED     00029000
*                                                                       00029100
OUTFLAG  EQU   X'80'                                                    00029200
OUTTYPE  EQU   X'83'                   OUTAGE RECORD TYPE               00029300
IPLTYPE  EQU   X'50'                   IPL RECORD TYPE                  00029400
EODREC   EQU   X'80'                   EOD RECORD TYPE                  00029500
MCHREC   EQU   X'13'                   MCH RECORD TYPE                  00029600
SDR20BYT EQU   X'01'                   BIT IN STAT TO SIGNIFY THAT THIS 00029700
*                                      IS A 20 BYTE STAT TAB            00029800
UCBNEWST EQU   X'10'                   BIT IN UCB FLAG BYTE TO SIGNIFY  00029900
*                                      A NEW TYPE STATISTICS TABLE      00030000
EODSHORT EQU   X'A0'                   BITS IN UNIT CHECK RECORD TO     00030100
*                                      SIGNIFY A SHORT REC & SDR DUMP   00030200
NOCLR    EQU   X'80'                   BIT IN STAT TO SIGNIFY NOT TO    00030300
*                                      CLEAR THE OPTION BYTE(BYTE TWO)  00030400
OBRREC   EQU   X'30'                   UNIT CHECK - OBR RECORD TYPE     00030500
*                                                                       00030600
*        EQUATES FOR CONDITION CODE SETTINGS                            00030700
*                                                                       00030800
LOGFULL  EQU   0                       CODE TO MSG MOD FOR LOGREC FULL  00030900
LOGFORER EQU   4                       CODE TO MSG MOD FOR HDR REC INCR 00031000
LOGIOERR EQU   8                       CODE TO MSG MOD FOR LOGREC IO ER 00031100
LOGRECFL EQU   12                      LOG FULL CONDITION CODE          00031200
IOERROR  EQU   16                      I/O ERROR CONDITION CODE         00031300
*                                                                       00031400
*        SVC 76 FUNCTION REQUEST VALUES                                 00031500
*                                                                       00031600
EODREQ   EQU   0                       HALT END OF DAY REQUEST          00031700
SDRREQ   EQU   4                       IFCEREPO UTILITY REQUEST         00031800
IPLREQ   EQU   8                       MASTER SCHEDULER(IPL) REQUEST    00031900
OUTREQ   EQU   12                      OUTAGE REQUEST                   00032000
*                                                                       00032100
DEVNUM   EQU   8                       NUMBER OF BUFFERED LOG DEVS      00032200
ASPNPARM EQU   51                      PARM PASSED TO SVC91 FOR AN      00032300
*                                      ASPEN TAPE DEVICE                00032400
EODPARM  EQU   32                      EOD SERVICE                      00032500
IPLPARM  EQU   56                      IPL SERVICE                      00032600
REASONUM EQU   13                      NUM OF POSSIBLE REASON CODES     00032700
IDNUM    EQU   14                      NUM OF POSSIBLE SUB SYS ID'S     00032800
*                                                                       00032900
*********************************************************************** 00033000
*                                                                       00033100
*        LOGREC RECORDER SECTION                                        00033200
*                                                                       00033300
*********************************************************************** 00033400
*                                                                       00033500
         USING IGC0007F,R6                                              00033600
         B     START                                                    00033700
*                                                                       00033800
         DC    AL1(L'EYECATCH)                                          00033900
EYECATCH DC    C'IGC0007F ZP60035 &SYSDATE &SYSTIME'                    00034000
*                                                                       00034100
START    LR    R3,R6                   SET UP BASE REGISTER             00034200
*                                                                       00034300
         DROP  R6                                                       00034400
         USING IGC0007F,R3                                              00034500
         USING LOGWA,R9                WORKAREA DSECT                   00034600
         USING CVTMAP,R2                                                00034700
*                                                                       00034800
         LTR   R0,R0                   SVC 76 FUNCTION REQUESTED ?      00034900
         BNM   WRTREC                  YES, BRANCH                      00035000
*                                      NO, CONTINUE TO WRITE RECORD     00035100
*                                                                       00035200
*        NEGATIVE VALUE IN R0                                           00035300
*        R1 -> RECORD TO BE WRITTEN                                     00035400
*                                                                       00035500
         LPR   R10,R0                  MAKE L'RECORD POSITIVE           00035600
         LA    R8,24                   L'MINIMUM RECORD                 00035700
         CR    R10,R8                  L'RECORD LESS THAN 24 ?          00035800
         BLR   R14                     LESS THAN 24, RETURN TO CALLER   00035900
         B     DEMFPROC                BRANCH TO DEMF CODE              00036000
*                                                                       00036100
*        SVC 76 HAS RECEIVED A FUNCTION REQUEST TO GENERATE A RECORD    00036200
*                                                                       00036300
*        DEMF PROCESSING RETURNS TO WRTREC FOR STANDARD SVC 76          00036400
*        PROCESSING                                                     00036500
*                                                                       00036600
WRTREC   L     R2,CVTPTR               LOAD CVT POINTER                 00036700
*                                      SAVE PARAMETER REGS              00036800
         LR    R11,R1                  R11 -> RECORD                    00036900
         LR    R10,R0                  R10 =  - VE L'RECORD OR          00037000
*                                             FUNCTION REQUEST NUMBER   00037100
*                                                                       00037200
         GETMAIN RC,LV=LOGWAL,SP=245   REQUEST WORKAREA                 00037300
*                                                                       00037400
         LTR   R15,R15                 SUCCESSFUL GETMAIN ?             00037500
         BZ    STGAVAIL                YES, CONTINUE RECORDING          00037600
         LA    R15,8                   NO, SET RETURN CODE OF 8         00037700
         BR    R14                     RETURN TO CALLER                 00037800
*                                                                       00037900
STGAVAIL LR    R9,R1                   R9 -> LOGWA DSECT                00038000
         LR    R0,R1                   CLEAR GOTTEN STORAGE             00038100
         LA    R1,LOGWAL               L'WORKAREA                       00038200
         SR    R15,R15                 L'=0, PAD=X'00'                  00038300
         MVCL  R0,R14                                                   00038400
         ST    R14,RETURNAD            SAVE RETURN ADDR                 00038500
         MVC   ESTAEWRK(16),ESTALIST   MOVE ESTAE LIST TO ESTAE WORK    00038600
         ST    R9,ESTAPARM             SAVE ADDR OF LOGWA FOR ESTAE     00038700
*                                                                       00038800
         ESTAE PGMCKENT,PARAM=ESTAPARM,MF=(E,ESTAEWRK)                  00038900
*                                                                       00039000
*                                      RESTORE PARAMETER REGS           00039100
         LR    R1,R11                  R1 -> RECORD                     00039200
*                                      R0 = L'RECORD OR FUNCTION REQ    00039300
         LTR   R0,R10                  R0 NEGATIVE ?                    00039400
         BNM   FUNCTION                NO, FUNCTION CALL REQUESTED      00039500
*                                                                       00039600
*        SAVE R14 IN CASE ENTRY IS FOR A SHORT RECORD FROM THIS MODULE  00039700
*                                                                       00039800
*        INPUTS FOR SHORT OBR ARE R0 AND R1 AT THIS POINT               00039900
*                                                                       00040000
WRTSTAT  ST    R14,SHORTRTN            SAVE SHORT RETURN ADDR           00040100
         LPR   R10,R0                  SET L'RECORD POSITIVE IN R10     00040200
*                                                                       00040300
*        GET POINTER IN R11 IN CASE OF SHORT OBR                        00040400
*                                                                       00040500
         LR    R11,R1                  R11 -> RECORD                    00040600
         TM    2(R11),TIMEBIT          TEST FOR TIME MACRO              00040700
         BZ    ISSUENQ                 NO, BRANCH AROUND                00040800
         OI    11(R11),X'0F'           YES, PUT X'0F' IN DATE           00040900
*                                                                       00041000
*********************************************************************** 00041100
*                                                                       00041200
*        ENQ TO SERIALIZE ACCESS TO LOGREC DATASET                      00041300
*                                                                       00041400
*********************************************************************** 00041500
*                                                                       00041600
ISSUENQ  MVC   ENQDEQWA(28),ENQDEQL    MOVE ENQ TO GETMAINED AREA       00041700
         LA    R8,QNM                  SET MAJ/MIN QNAME ADDRS          00041800
         ST    R8,QPTR                                                  00041900
         LA    R8,RNM                                                   00042000
         ST    R8,RPTR                                                  00042100
*                                                                       00042200
         ENQ   MF=(E,ENQDEQWA)                                          00042300
*                                                                       00042400
         OI    INTSW,ENQIND            ENQUEUE INDICATED                00042500
*                                                                       00042600
*********************************************************************** 00042700
*                                                                       00042800
*        INIT CCW COMMANDS IN LOGWA WORKAREA TO READ HDRREC             00042900
*                                                                       00043000
*********************************************************************** 00043100
*                                                                       00043200
         MVC   CCWAREA(HDRCCWL),HDRCCW  MOVE CHANNEL PROGRAM SKELETON   00043300
         LA    R8,SEARCHID+2           RESOLVE SEARCH ID EQ ADDR        00043400
         STCM  R8,B'0111',CCW0+1       STORE SEARCH ARG ADDR IN CCW     00043500
         LA    R8,CCW0                 R8 -> 1ST CCW IN CHAIN           00043600
         STCM  R8,B'0111',CCW8+1       STORE ADDR OF 1ST CCW FOR TIC    00043700
         LA    R8,HDRREC               R8 -> HDRREC                     00043800
         STCM  R8,B'0111',CCW16+1      STORE HDRREC I/O AREA ADDR       00043900
*                                                                       00044000
*********************************************************************** 00044100
*                                                                       00044200
*        READ THE LOGREC HEADER RECORD                                  00044300
*                                                                       00044400
*********************************************************************** 00044500
*                                                                       00044600
*        INITIALIZE IOB                                                 00044700
*                                                                       00044800
         ICM   R7,B'0111',CVTDCBA      R7 -> LOGREC DCB                 00044900
         STCM  R7,B'0111',IOBDCBPB                                      00045000
         L     R7,DCBDEBAD-IHADCB(,R7)  R7 -> LOGREC DEB                00045100
*                                      MOVE BBCCHH FROM DEB             00045200
         MVC   SEARCHID(6),DEBBINUM-DEBDASD+DEBBASND-DEBBASIC(R7)       00045300
         MVI   SEARCHID+6,1            SET RECORD FIELD FOR FIRST REC   00045400
         LA    R6,TPRECB               R6 -> ECB ADDR                   00045500
         ST    R6,IOBECBPT             STORE IN IOB                     00045600
         LA    R6,CCW0                 R6 -> ADDR OF CCW'S              00045700
         ST    R6,IOBSTART             STORE CHANNEL PGM ADDR INTO IOB  00045800
         MVI   IOBFLAG1,IOBCMDCH+IOBUNREL  SET FLAG1 FIELD IN IOB       00045900
         MVC   IOBSEEK+1(6),SEARCHID   PLACE SEEK ADDR IN IOB           00046000
         L     R5,DEBUCBAD-DEBDASD+DEBBASND-DEBBASIC(,R7)               00046100
         USING UCB,R5                                                   00046200
         TM    UCBFLB,UCBIORST         PATH AVAILABLE ?                 00046300
         BNO   READHDR                                                  00046400
         TM    UCBJBNR,UCBBOX          UCB FORCED OFFLINE ?             00046500
         BNO   READHDR                 NO, GO READ HEADER               00046600
         OI    FLAGS,LOGRECNA          MARK LOGREC NOT AVAILABLE        00046700
         B     RTRN                                                     00046800
*                                                                       00046900
READHDR  C     R10,OUTIND              OUTAGE REQUEST ?                 00047000
         BE    OUTROUT                 YES, BRANCH                      00047100
         BAL   R5,EXCPRTN              CALL EXCPRTN TO READ HDRREC      00047200
*                                                                       00047300
*        VALIDATE HDR RECORD AND DEB EXTENTS                            00047400
*                                                                       00047500
         CLI   HDRCHK,X'FF'            COMPARE CHECK BYTE IN HDR TO     00047600
*                                      VALIDATE HDR RECORD              00047700
         BNE   ERROR2                  NOT A GOOD HEADER RECORD, BRANCH 00047800
         CLC   HDRSTRCC,DEBSTRCC-DEBDASD+DEBBASND-DEBBASIC(R7)          00047900
         BNE   ERROR2                  ERROR, EXTENT DOES NOT MATCH     00048000
         CLC   HDRENDCC,DEBENDCC-DEBDASD+DEBBASND-DEBBASIC(R7)          00048100
         BNE   ERROR2                  ERROR, EXTENT DOES NOT MATCH     00048200
*                                                                       00048300
*        SETUP TO WRITE RECORD TO LOGREC                                00048400
*                                                                       00048500
         STH   R10,CCW48+6             STORE COUNT IN WRITE CKD CCW     00048600
         STH   R10,CTFDL               SAVE RECORD SIZE IN CKD          00048700
         MVI   CTFK,0                  ZERO CKD KEY FIELD               00048800
         STCM  R11,B'0111',CCW48+1     RECORD ADDR TO CHAINED CCW       00048900
         CLM   R10,B'0011',HDRTRKLN    RECORD TOO LARGE FOR DEVICE ?    00049000
         BNH   TRYTRK                  NO, BRANCH TO CONTINUE           00049100
         LA    R8,LOGRECFL             SET UP ERROR CODE                00049200
         B     RTRN                    RETURN WITH ERROR                00049300
*                                                                       00049400
*        DETERMINE IF ENOUGH BYTES REMAINING ON TRACK FOR RECORD        00049500
*                                                                       00049600
TRYTRK   L     R5,CVTDCB                R5 -> LOGREC DCB                00049700
         L     R5,DCBDEBAD-IHADCB(,R5)  R5 -> LOGREC DEB                00049800
*                                       R6 -> LOGREC UCB FROM DEB       00049900
         L     R6,DEBUCBAD-DEBDASD+DEBBASND-DEBBASIC(,R5)               00050000
         STM   R1,R14,TRKRSAVE         SAVE REGS ACROSS TRKCALC         00050100
         LA    R7,TRKRSAVE             ..                               00050200
         IC    R5,HDRLASTR+6           GET LAST RECORD NO               00050300
         LA    R5,1(,R5)               INCR FOR NEXT RECORD NUMBER      00050400
         SLL   R5,24                   MOVE RECORD NO TO HIORDER BYTE   00050500
         OR    R10,R5                  SET R IN RKDD TRKBAL REG         00050600
         SR    R4,R4                                                    00050700
         ICM   R4,B'0011',HDRTBAL      R4 = TRACK BALANCE FROM HDRREC   00050800
*                                                                       00050900
         TRKCALC FUNCTN=TRKBAL,UCB=(6),BALANCE=(4),RKDD=(10),          X00051000
               REGSAVE=NO,MF=(E,TRKWA)                                  00051100
*                                                                       00051200
         LM    R1,R14,0(R7)            RESTORE REGS                     00051300
         LTR   R15,R15                 TRKCALC OK ?                     00051400
         BNZ   NEXTTRK                 NO, BRANCH                       00051500
         STCM  R0,B'0011',HDRTBAL      YES, UPDATE TRACK BALANCE IN HDR 00051600
         MVC   IOBR,HDRLASTR+6         SET REC ID IN IOB SEEK FIELD     00051700
         B     WRTHDR                  REC WILL FIT ON TRACK, BRANCH    00051800
*                                                                       00051900
*        CHECK IF THERE IS ANOTHER TRACK AVAIL ON THIS CYLINDER         00052000
*                                                                       00052100
NEXTTRK  ICM   R6,B'0011',HDRLASTR+4   R6 = LAST USED HEAD POSITION     00052200
         LA    R6,1(,R6)               INCR HEAD NUMBER                 00052300
         CLM   R6,B'0011',HDRTRK       EXCEED TRKS PER CYL ?            00052400
         BH    TRYCYL                  YES, BRANCH                      00052500
         ICM   R6,B'1100',HDRLASTR+2   NO, GET LAST CC USED             00052600
         CLM   R6,B'1111',HDRENDCC     OUT OF EXTENT ?                  00052700
         BH    ERROR1                  YES, ERROR                       00052800
*                                                                       00052900
*        START WRITING ON NEW TRACK                                     00053000
*                                                                       00053100
SETUPTRK STH   R6,HDRLASTR+4           UPDATE HEAD NUM IN HEADER REC    00053200
         MVI   HDRLASTR+6,0            SET LAST RECORD ID TO ZERO       00053300
         MVC   HDRTBAL,HDRTRKLN        RESET TRKBAL TO MAX FOR DEVICE   00053400
         B     TRYTRK                  GO WRITE ON NEW TRACK            00053500
*                                                                       00053600
*        START WRITING ON A NEW CYLINDER                                00053700
*                                                                       00053800
TRYCYL   SR    R6,R6                   ZERO HEAD NUM                    00053900
         SR    R7,R7                                                    00054000
         ICM   R7,B'0011',HDRLASTR+2   R7 = LAST USED CYL POSITION      00054100
         LA    R7,1(,R7)               INCR CYL COUNT                   00054200
         CLM   R7,B'0011',HDRENDCC     OUT OF EXTENT ?                  00054300
         BH    ERROR1                  YES, ERROR                       00054400
         STCM  R7,B'0011',HDRLASTR+2   STORE UPDATED CYL NUMBER         00054500
         B     SETUPTRK                GO WRITE ON NEW CYL, TRACK       00054600
*                                                                       00054700
*        COMPLETE UPDATE OF HEADER RECORD                               00054800
*                                                                       00054900
WRTHDR   IC    R5,HDRLASTR+6           GET ID OF LAST ID WRITTEN        00055000
         LA    R5,1(,R5)               INCR RECORD ID                   00055100
         STC   R5,HDRLASTR+6           UPDATE LAST ID WRITTEN IN HDR    00055200
*                                                                       00055300
*        INIT WRITE RECORD CCW STRINGS IN LOGWA WORKAREA                00055400
*                                                                       00055500
         LA    R1,IOBCC                R1 -> CCHHR                      00055600
         STCM  R1,B'0111',CCW24+1      SET ADDR OF SEARCH ID EQ         00055700
         LA    R1,CCW24                                                 00055800
         STCM  R1,B'0111',CCW32+1      SET TIC ADDR IN CCW              00055900
         MVC   CTFCCHH(5),HDRLASTR+2   SET UP COUNT FIELD FOR RECORD    00056000
         LA    R1,CTFIELD              R1 -> CCHHR,KEY,DATA LENGTH AREA 00056100
         STCM  R1,B'0111',CCW40+1      STORE ADDR                       00056200
*                                                                       00056300
*        WRITE THE COMPLETED LOGREC RECORD                              00056400
*                                                                       00056500
         LA    R5,CCW24                R5 -> FIRST CCW IN WRITE CHAIN   00056600
         ST    R5,IOBSTART             STORE CHAN PGM ADDR IN IOB       00056700
         MVC   IOBBB(6),HDRLASTR       SET SEEK ADDR                    00056800
         OI    IOBFLAG1,IOBDATCH       INDICATE DATA CHAINING           00056900
         BAL   R5,EXCPRTN              CALL EXCP RTN TO WRITE RECORD    00057000
*                                                                       00057100
*        SETUP AND WRITE AN END OF FILE RECORD IF REQUIRED              00057200
*        BYPASS IF RECORD WAS WRITTEN ON THE LAST TRACK OF THE          00057300
*        LOGREC DATASET AS ALL DATA WILL HAVE BEEN ERASED BY            00057400
*        WRITE CKD CCW COMMAND USED TO WRITE THE LOGREC RECORD          00057500
*                                                                       00057600
         CLC   IOBCC(4),HDRENDCC       RECORD WRITTEN ON LAST TRACK ?   00057700
         BE    EWMSG                   YES, ON THE LAST TRK, NO EOF REQ 00057800
*                                                                       00057900
*        DETERMINE IF EOF RECORD IS NEEDED                              00058000
*                                                                       00058100
         CLC   IOBBB(7),HDRREA         FIRST RECORD WRITTEN TO LOGREC ? 00058200
         BNE   EWMSG                   NO, BYPASS WRITE OF EOF RECORD   00058300
*                                                                       00058400
*        WRITE EOF RECORD ON LAST TRACK                                 00058500
*                                                                       00058600
         MVC   IOBCC(4),HDRENDCC       MOVE THE LAST CCHH OF THE LOGREC 00058700
*                                      DATASET TO IOBSEEK               00058800
         MVI   IOBR,0                  SEARCH ON RECORD 0               00058900
         MVC   CTFCCHH(4),IOBCC        MOVE CCHHR TO EOF RECORD CKD     00059000
         MVI   CTFR,1                  WRITE RECORD 1                   00059100
         XC    CTFK(3),CTFK            CLEAR RECORD L'KEY L'DATA FIELD  00059200
         MVI   CCW40+4,CCWSLI          RESTORE SILI, TURN OFF DATA CH   00059300
         BAL   R5,EXCPRTN              CALL EXCP ROUTINE TO WRITE EOF   00059400
         NI    IOBFLAG1,255-IOBDATCH   NO MORE DATA CHAINING            00059500
*                                                                       00059600
*        SETUP TO WRITE HEADER RECORD BACK TO LOGREC                    00059700
*                                                                       00059800
EWMSG    CLI   0(R11),MCHREC           MCH RECORD ?                     00059900
         BNE   EWMSGA                  NO, BRANCH AROUND                00060000
         TM    5(R11),X'FF'            YES, TEST FOR CLOBBER BIT        00060100
         LA    R15,LOGRECFL            SET RETURN CODE                  00060200
         BNZ   RTRN                    YES, RETURN                      00060300
EWMSGA   TM    HDREWSW,HDREWMI         EARLY WARNING MSG BEEN ISSUED ?  00060400
         BO    UPDATHDR                YES, BYPASS CHECK FOR EARLY WARN 00060500
         CLC   HDRLASTR+2(4),HDREWMT   90% OF ALL TRKS HAS BEEN USED ?  00060600
         BL    UPDATHDR                NO, BRANCH ALL OK                00060700
         BE    WMSG                    90% TRACKS USED GO WRT MSG       00060800
         CLC   HDRLASTR+2(4),HDRENDCC  ON THE LAST TRACK ?              00060900
         BNE   UPDATHDR                NO, BRANCH                       00061000
*                                                                       00061100
*        CHECK TRACK CAPACITY OF LAST TRACK                             00061200
*                                                                       00061300
         CLC   HDRTBAL,HDREWTC         90% OF LAST TRACK USED ?         00061400
         BH    UPDATHDR                NO, BYPASS                       00061500
*                                                                       00061600
*        SETUP TO WRITE MSG IFB060E WHEN -                              00061700
*        1. 90% OF ALL TRACKS IN LOGREC DATASET USED -OR-               00061800
*        2. 90% OF LAST TRACK USED                                      00061900
*                                                                       00062000
WMSG     OI    HDREWSW,HDREWMI         YES, SET HDREWMI IN HDR RECORD   00062100
         OI    FLAGS,WRTEWMSG          SET EWM NEEDED SWITCH IN FLAGS   00062200
*                                                                       00062300
*        REWRITE HEADER RECORD WITH UPDATED FIELDS                      00062400
*        -OR-                                                           00062500
*        WRITE OUTAGE RECORD (RECORD FOLLOWING HDRREC)                  00062600
*                                                                       00062700
UPDATHDR MVI   CCW16,WRITEDAT          CCW CMD FOR UPDATE OF HDRREC     00062800
         LA    R5,CCW0                 STORE CCW ADDR IN IOB            00062900
         ST    R5,IOBSTART                                              00063000
         MVC   IOBSEEK+1(7),SEARCHID   PLACE SEEK ADDR IN IOB           00063100
*                                      CALL EXCP RTN TO REWRITE HDR     00063200
         BAL   R5,EXCPRTN              OR WRITE OUTAGE RECORD           00063300
*                                                                       00063400
RTRN     DEQ   MF=(E,ENQDEQWA)                                          00063500
*                                                                       00063600
         NI    INTSW,255-ENQIND        TURN OFF ENQ IND                 00063700
*                                                                       00063800
*        TESTS AND BRANCHES FOR EXITS REQUIRING A MESSAGE TO BE         00063900
*        WRITTEN TO THE OPERATOR CONSOLE                                00064000
*                                                                       00064100
         TM    FLAGS,LOGRECNA          LOGREC DATASET NOT AVAILABLE ?   00064200
         BO    MSGEW                   YES, SET CODE FOR ERROR MSG      00064300
         TM    FLAGS,EOEXTFLG          LOGREC FULL ?                    00064400
         BO    ERROR1A                                                  00064500
         TM    FLAGS,BADHDR            BAD HDR REC ERROR ?              00064600
         BO    ERROR2A                 SET CODE FOR ERROR MSG           00064700
         TM    FLAGS,IOERR             IO ERROR ?                       00064800
         BO    ERROR4A                 SET CODE FOR ERROR MSG           00064900
         TM    FLAGS,WRTEWMSG          EWM SWITCH ON ?                  00065000
         BO    ERROR5                  SET CODE FOR WARNING MSG         00065100
         TM    INTSW,SHTSW             SHORT RECORD ?                   00065200
         BZ    EXIT                    NO, BRANCH                       00065300
         NI    INTSW,255-SHTSW         YES, CLEAR SWITCH                00065400
         L     R14,SHORTRTN            GET SHORT RETURN ADDR            00065500
         BR    R14                     GO CHECK FOR MORE                00065600
*                                                                       00065700
EXIT     SR    R8,R8                   CLEAR CODE REG                   00065800
*                                                                       00065900
RTRNM    ESTAE 0                       CANCEL ESTAE                     00066000
*                                                                       00066100
         L     R14,RETURNAD            GET RETURN POINTER               00066200
*                                                                       00066300
         FREEMAIN  R,LV=LOGWAL,SP=245,A=(9)   FREE WORKAREA             00066400
*                                                                       00066500
         LR    R15,R8                  GET RETURN CODE                  00066600
         BR    R14                     RETURN TO CALLER                 00066700
*                                                                       00066800
*********************************************************************** 00066900
*                                                                       00067000
*        ISSUE EXCP AND CHECK COMPLETION CODE IN ECB                    00067100
*                                                                       00067200
*********************************************************************** 00067300
*                                                                       00067400
EXCPRTN  XC    TPRECB(4),TPRECB        CLEAR ECB                        00067500
*                                                                       00067600
         EXCP  TPRIOB                                                   00067700
*                                                                       00067800
         WAIT  1,ECB=TPRECB                                             00067900
*                                                                       00068000
         CLI   TPRECB,ECBNORM          COMPARE COMPLETION CODE IN ECB   00068100
         BER   R5                      RETURN  IF COMPLETED OK          00068200
         CLI   TPRECB,ECBDAEA          COMPARE COMPLETION CODE IN ECB   00068300
         BE    ERROR1                  OUT OF EXTENT, BRANCH            00068400
         CLI   TPRECB,ECBINCPT         COMPARE COMPLETION CODE IN ECB   00068500
         BE    EXCPRTN                 BRANCH TO RETRY EXCP AGAIN       00068600
         B     ERROR4                  I/O FAILURE, BRANCH              00068700
*                                                                       00068800
*********************************************************************** 00068900
*                                                                       00069000
*        SET CONDITION CODES FOR THE MESSAGE MODULE                     00069100
*                                                                       00069200
*********************************************************************** 00069300
*                                                                       00069400
ERROR1A  LA    R12,0                   SET R12 TO MSG CODE              00069500
         B     EXITMSG                 EXIT TO MESSAGE MODULE           00069600
*                                                                       00069700
ERROR2A  LA    R12,4                   SET R12 TO MSG CODE              00069800
         B     EXITMSG                 EXIT TO MESSAGE MODULE           00069900
*                                                                       00070000
ERROR4A  LA    R12,8                   SET R12 TO MSG CODE              00070100
         B     EXITMSG                 EXIT TO MESSAGE MODULE           00070200
*                                                                       00070300
MSGEW    LA    R12,16                  SET R12 TO MSG CODE              00070400
*                                      FOR EARLY WARNING MSG            00070500
         B     EXITMSG                                                  00070600
*                                                                       00070700
ERROR5   LA    R12,12                                                   00070800
EXITMSG  L     R13,RETURNAD            GET RETURN ADDR IN R13           00070900
         B     MESS                    BRANCH TO MSG PROCESSING         00071000
*                                                                       00071100
*        SET UP FOR MESSAGE                                             00071200
*                                                                       00071300
ERROR1   OI    FLAGS,EOEXTFLG          SET FLAG FOR END OF EXTENT ERROR 00071400
         B     RTRN                    BR TO DEQ                        00071500
*                                                                       00071600
ERROR2   OI    FLAGS,BADHDR            SET FLAG FOR INCORRECT HDR REC   00071700
         B     RTRN                    BR TO DEQ                        00071800
*                                                                       00071900
ERROR4   OI    FLAGS,IOERR             SET FLAG FOR AN IO ERROR         00072000
         B     RTRN                    BR TO DEQ                        00072100
*                                                                       00072200
*        SETUP TO UPDATE OUTAGE RECORD USING THE HDRREC UPDATE RTN      00072300
*                                                                       00072400
OUTROUT  MVI   SEARCHID+6,2            SET TO WRITE 2ND REC             00072500
         MVC   HDRREC(40),0(R11)       MOVE OUTAGE RECORD TO HDRREC     00072600
         B     UPDATHDR                GO UPDATE OUTAGE RECORD          00072700
*                                                                       00072800
*********************************************************************** 00072900
*                                                                       00073000
*        FUNCTION PROCESSING                                            00073100
*                                                                       00073200
*********************************************************************** 00073300
*                                                                       00073400
FUNCTION LTR   R7,R0                   R7 = FUNCTION NUMBER REQUEST     00073500
*                                                                       00073600
*        SAVE INPUT PARAMETER AND TEST SAME                             00073700
*                                                                       00073800
         STH   R7,FUNCTREQ             SAVE FUNCTION REQUEST            00073900
*                                      HALT END OF DAY REQUEST ?        00074000
         BZ    STATUPA                 YES, BRANCH                      00074100
         CLI   FUNCTREQ+1,SDRREQ       IFCEREPO UTILITY REQUEST ?       00074200
         BE    STATUPA                 YES, BRANCH                      00074300
         CLI   FUNCTREQ+1,IPLREQ       ENTRY FOR RDE SERVICE ?          00074400
         BE    WTIPLEOD                YES, BRANCH                      00074500
         CLI   FUNCTREQ+1,OUTREQ       ENTRY FOR OUTAGE ?               00074600
         BE    OUTPROC                 YES, FOR OUTAGE PROCESSING       00074700
         B     EXIT                    EXIT, INPUT PARM IS IN ERROR     00074800
*                                                                       00074900
*        LOAD FIRST UCB ADDR FROM THE UCB ADDR LIST PORTION OF          00075000
*        THE UCB LOOKUP TABLE                                           00075100
*                                                                       00075200
STATUPA  L     R4,CVTILK2              R4 -> UCB HALFWORD ADDR LIST     00075300
*                                                                       00075400
*        TEST ALL MAIN STORAGE STAT TABLES FOR ZERO                     00075500
*                                                                       00075600
         SR    R5,R5                   CLEAR REGISTER                   00075700
STATLKUP ICM   R5,B'0011',0(R4)        R5 -> UCB                        00075800
         LA    R4,2(,R4)               UPDATE UCB ADDR POINTER          00075900
         BZ    STATLKUP                UCB ADDR IS ZERO, BRANCH         00076000
         CLM   R5,B'0011',KXFFFF       END OF UCB ADDR LIST ?           00076100
         BE    WTIPLEOD                YES, BRANCH                      00076200
         SR    R6,R6                   CLEAR REGISTER                   00076300
         L     R8,20(,R5)              GET UCB EXTENSION PTR            00076400
         IC    R6,1(,R8)               GET STATAB INDEX                 00076500
         CLC   TYASPN(2),UCBDVCLS-UCB(R5)  ASPEN (3410/3420) ?          00076600
         BNE   TESTOAK                 NO, CHECK FOR MSS                00076700
         LA    R1,ASPNPARM             SET PARM FOR SVC91               00076800
         B     ISNEWDEV                BRANCH TO READ BUF LOG           00076900
*                                                                       00077000
TESTOAK  CLC   SS1(2),UCBDVCLS-UCB(R5)  OAK (3850) ?                    00077100
         BNE   MERLZ                   NO, CHECK FOR MERLIN-ZEUS        00077200
         SR    R1,R1                   CLEAR R1                         00077300
         ICM   R1,B'0011',KXFFFF       LOAD CODE FOR SS1                00077400
         B     ISNEWDEV                GO FOR BUFFERED LOG              00077500
*                                                                       00077600
MERLZ    LA    R10,DEVNUM              NUM OF BUF LOG DEVICES           00077700
*                                      R8 -> TABLE OF DEVICES WITH      00077800
         LA    R8,NEWDEVTB             BUFFERED LOG                     00077900
CHECKNEW CLC   0(2,R8),UCBDVCLS-UCB(R5)   BUFFERED LOG DEVICE ?         00078000
         BE    ISNEWDEV                YES, BRANCH TO READ BUF LOG      00078100
         LA    R8,2(,R8)               NO, INCR TO NEXT DEV TYPE CODE   00078200
         BCT   R10,CHECKNEW            LOOP                             00078300
*                                                                       00078400
ALSOSDR  LTR   R6,R6                   ZERO STATAB ?                    00078500
         BZ    STATLKUP                YES, GET NEXT                    00078600
         L     R7,CVTSTB               R7 -> STAT TABS                  00078700
LOOP1    CLM   R5,B'0011',0(R7)        DEVICE IN THIS SECTION OF        00078800
*                                      STAT TAB'S ?                     00078900
         BL    LOOP3                   OK, BRANCH                       00079000
         LA    R7,2(,R7)               LOAD ADDR OF THE FIRST UCB IN    00079100
*                                      THE NEXT STAT TAB SECTION        00079200
         LA    R6,256(,R6)             BUMP STAT TAB OFFSET PAST THIS   00079300
*                                      SECTION                          00079400
         B     LOOP1                   LOOP TO CHECK THIS SECTION       00079500
*                                                                       00079600
ISNEWDEV TM    UCBSTAT-UCB(R5),UCBONLI  DEVICE ONLINE ?                 00079700
         BZ    STATLKUP                NO, GO LOOK AT NEXT UCB          00079800
         LNR   R0,R5                   PREPARE TO PASS ADDR OF          00079900
*                                      UCB TO SVC91                     00080000
         SVC   91                      ISSUE VOLSTAT SVC                00080100
*                                                                       00080200
         SR    R1,R1                   CLEAR SVC91 PARM                 00080300
         CLC   UCBDVCLS-UCB(2,R5),UR3800                                00080400
         BE    ALSOSDR                                                  00080500
         B     STATLKUP                BRANCH TO CONTINUE STAT LOOKUP   00080600
*                                                                       00080700
LOOP3    MH    R6,MULTICON             MULTIPLY BY TEN                  00080800
         AL    R6,CVTSTB               ADD ADDR OF STAT TABS TO OFFSET  00080900
         TM    UCBFL5-UCB(R5),UCBNEWST  NEW TYPE STAT ?                 00081000
         BO    NEWSTAT                 YES, BRANCH TO NEWSTAT           00081100
         NC    0(SDRDATA,R6),0(R6)     CHECK FOR ZERO STAT FLD          00081200
         BNZ   STATMOVE                STAT FIELD NOT ZERO, BRANCH      00081300
         B     STATLKUP                GO CHECK FOR MORE                00081400
*                                                                       00081500
NEWSTAT  IC    R8,0(,R6)               GET FLAGS BYTE                   00081600
         N     R8,MASK                 SAVE LOW ORDER COUNT             00081700
         MH    R8,MULTICON             MULTIPLY BY 10                   00081800
         AH    R8,MULTICON             ADD 10                           00081900
         LR    R12,R8                  SAVE COUNT                       00082000
         BCTR  R8,0                    REDUCE COUNT BY 1                00082100
         TM    0(R6),NOCLR             TEST CLEAR OPTION BYTE           00082200
         BO    NEWSTAT1                ONE, DO NOT TEST BYTE 1          00082300
         NC    1(1,R6),1(R6)           CHECK FOR STAT CNT               00082400
         BNZ   NEWSTAT2                YES, GO COMPLETE RECORD          00082500
NEWSTAT1 BCTR  R8,0                    DECREMENT COUNT                  00082600
         BCTR  R8,0                    DECREMENT COUNT                  00082700
         EX    R8,TSTSTAT              TEST FOR ANY COUNT               00082800
         BZ    STATLKUP                ZERO, CHECK NEXT STAT            00082900
         LA    R8,2(,R8)               BUMP COUNT TWO                   00083000
NEWSTAT2 EX    R8,MOVESTAT             MOVE STATS TO RECORD             00083100
         BCTR  R8,0                    DECREMENT COUNT                  00083200
         BCTR  R8,0                    DECREMENT COUNT                  00083300
         EX    R8,CLRSTAT              CLEAR STAT ENTRY                 00083400
         STC   R12,SDRCNT              STORE CNT IN RECORD              00083500
         B     EODOBR                  GO COMPLETE                      00083600
*                                                                       00083700
CLRSTAT  XC    2(0,R6),2(R6)           CLEAR STATS                      00083800
MOVESTAT MVC   SDR(0),0(R6)            MOVE STATS TO RECORD             00083900
TSTSTAT  NC    2(0,R6),2(R6)           TEST STATS                       00084000
*                                                                       00084100
STATMOVE MVC   SDR(SDRLEN),0(R6)       MOVE STAT COUNTERS TO RECORD     00084200
         XC    0(10,R6),0(R6)          CLEAR STAT COUNTERS              00084300
         MVI   SDRCNT,SDRLEN           SET SDR COUNT IN RECORD TO 10    00084400
EODOBR   TM    0(R6),NOCLR             TEST CLEAR OPTION BIT            00084500
         BO    NCLROPT                 NO, BYPASS CLEAR OPT BYTE        00084600
         MVI   1(R6),0                 CLEAR OPTION BYTE                00084700
*                                                                       00084800
*        CONSTRUCT OBR/EOD RECORD                                       00084900
*                                                                       00085000
NCLROPT  MVI   CLASRC,OBRREC           SET RECORD TYPE - CLASS & SOURCE 00085100
         OI    SW2,EODSHORT            SET SWITCH FIELD IN RECORD       00085200
         L     R12,CVTPTR              R12 -> CVT                       00085300
         SH    R12,KH4                 R12 -> CVTRELNO                  00085400
         PACK  WORKAREA(L'WORKAREA),0(2,R12) PACK RELEASE NUM           00085500
         CVB   R12,WORKAREA            CONVERT REL NO TO BINARY         00085600
         STC   R12,OPSYS               STORE SYS REL NUM IN RECORD      00085700
         OI    OPSYS,VS2IND            INDICATE VS2                     00085800
*                                                                       00085900
         TIME  DEC                     GET TIME                         00086000
*                                                                       00086100
         ST    R0,TIME                 STORE CURRENT TIME IN RECORD     00086200
         ST    R1,DATE                 SET DATE IN RECORD               00086300
         OI    SW1,TIMEBIT             SET TIME MACRO USED              00086400
         STIDP CPUSER                  STORE CPU ID IN RECORD           00086500
         MVC   DEVTYP(4),UCBTYP-UCB(R5)  MOVE DEV TYP FLD TO RECORD     00086600
*                                                                       00086700
*        EXTRACT THE EBCDIC CUA FROM THE UCB, CONVERTS IT TO            00086800
*        BINARY, STORE IT IN THE RECORD                                 00086900
*                                                                       00087000
         LA    R11,3                   LOAD NUM OF DIGITS IN A CUA      00087100
         L     R6,UCBNAME-UCB-1(R5)    LOAD PRI CUA                     00087200
LABL1    STC   R6,WORKAREA             STORE LOW ORDER BYTE             00087300
         TM    WORKAREA,X'F0'          DIGIT NUMERIC ?                  00087400
         BO    LABL2                   YES, USE LOW 4 BITS              00087500
         LA    R6,9(,R6)               NO, ADD 9 TO LOW ORDER           00087600
LABL2    SRDL  R6,4                    SHIFT BIN DIG TO R7              00087700
         SRL   R6,4                    SHIFT OUT SIGN                   00087800
         BCT   R11,LABL1               BRANCH IF THERE ARE MORE DIGITS  00087900
         SRL   R7,20                   SHIFT CUA TO LOW ORDER           00088000
         STH   R7,CUA                  STORE CUA IN RECORD              00088100
         OI    INTSW,SHTSW             SET SHORT SWITCH                 00088200
         SR    R1,R1                   CLEAR PARM REG                   00088300
         IC    R1,SDRCNT               GET STAT LENGTH                  00088400
         LA    R0,32(,R1)              GET RECORD COUNT                 00088500
         LA    R1,RECORD               R1 -> RECORD                     00088600
         LNR   R0,R0                   LOAD NEG LENGTH OF RECORD        00088700
         STM   R4,R15,SAVEREGS         SAVE REGISTERS                   00088800
         BAL   R14,WRTSTAT             GO WRITE RECORD                  00088900
         LM    R4,R15,SAVEREGS         RESTORE REGISTERS                00089000
         B     STATLKUP                CHECK NEXT STAT                  00089100
*                                                                       00089200
WTIPLEOD CLI   FUNCTREQ+1,SDRREQ       INPUT PARM = 4 ?                 00089300
         BE    EXIT                    YES, EXIT INPUT WAS FROM EREP    00089400
         BH    IPLRCD                  BR INPUT PARM IS 8 (IPL)         00089500
         MVI   FUNCTREQ+1,EODPARM      SET PARM FOR EOD SERVICE         00089600
         B     MAKEREC                 GO RECORD                        00089700
*                                                                       00089800
*        OUTAGE RECORD PROCESSING                                       00089900
*                                                                       00090000
OUTPROC  OI    FUNCTREQ,OUTFLAG        SET OUTAGE FLAG                  00090100
         MVI   FUNCTREQ+1,EODPARM      OVERIDE PARM FOR OUTREQ TO       00090200
*                                      EOD SERVICE                      00090300
         XC    RECORD(40),RECORD       CLEAR REC AREA FOR OUTAGE        00090400
         B     MAKEREC                 GO RECORD                        00090500
*                                                                       00090600
IPLRCD   MVI   FUNCTREQ+1,IPLPARM      SET PARM FOR IPL SERVICE         00090700
*                                                                       00090800
*        TEST FOR IPL OR EOD                                            00090900
*                                                                       00091000
         CLI   FUNCTREQ+1,EODPARM      ENTRY FOR EOD ?                  00091100
         BE    MAKEREC                 YES, BRANCH                      00091200
         BAL   R6,RDOUTREC             READ OUTAGE REC & SAVE TIME      00091300
         XC    RECORD(IPLSIZE),RECORD  CLEAR RECORD AREA                00091400
*                                                                       00091500
*        TEST IF SYSTEM WAS SYSGENED FOR RDE                            00091600
*                                                                       00091700
*        ISSUE BLDL FOR IFCRDE03 MODULE AS TEST FOR SYSTEM WITH RDE     00091800
*                                                                       00091900
         MVC   WRTCCW(L'IFCRDE03),IFCRDE03   MOVE BLDL LIST             00092000
         LA    R0,WRTCCW               R0 -> BLDL LIST                  00092100
         L     R1,CVTLINK              R1 -> LINKLIB DCB                00092200
*                                                                       00092300
         BLDL  (1),(0)                 ISSUE BLDL TO SEE IF 'RDE'       00092400
*                                      GENERATED IN THE SYSTEM          00092500
         LTR   R15,R15                 IFCRDE03 MODULE PRESENT ?        00092600
         BZ    IPLREASN                YES, GO PUT OUT MESSAGE          00092700
*                                      TO OBTAIN IPL REASON CODE        00092800
         B     CONSTIPL                GO CONSTRUCT IPL RECORD          00092900
*                                                                       00093000
*        PERFORM INFORMATION GATHERING ROUTINE FOR RECORD               00093100
*                                                                       00093200
*        FORMAT WTOR FIELDS                                             00093300
*                                                                       00093400
IPLREASN MVC   WTORMESS(IFB010L),IFB010    MOVE MESSAGE INTO WORKAREA   00093500
         LA    R4,WTOREPLY             R4 -> REPLY AREA                 00093600
         ST    R4,WTORADDR             STORE ADDR IN WTOR STRUCTURE     00093700
         MVI   WTORLGTH,5              STORE L'REPLY                    00093800
         LA    R4,WTOECB               R4 -> ECB                        00093900
         ST    R4,WTORECB              STORE ADDR IN WTOR STRUCTURE     00094000
         XC    WTOECB,WTOECB           CLEAR ECB                        00094100
*                                                                       00094200
         WTOR  MF=(E,WTORAREA)         WRITE OPERATOR WITH REPLY        00094300
*                                                                       00094400
         WAIT  1,ECB=WTOECB            WAIT FOR WTOR COMPLETION         00094500
*                                                                       00094600
         LA    R11,REASONUM            SET UP FOR LOOP CONTROL          00094700
         LA    R5,REPLYNM              R5 -> REASON CODE TABLE          00094800
         OC    WTOREPLY(L'KBLANKS),KBLANKS   CONVERT TO UPPER CASE      00094900
RECHECK1 CLC   WTOREPLY(2),0(R5)       VALID REPLY ?                    00095000
         BE    SECONDHF                YES, CHECK 2ND HALF OF REPLY     00095100
         LA    R5,2(,R5)               BUMP POINTER TO NEXT VALID REPLY 00095200
         BCT   R11,RECHECK1            BRANCH IF THERE ARE MORE         00095300
         CLI   WTOREPLY,C'U'           OPERATOR REPLY 'U' ?             00095400
         BNE   INFORM                  NO, INFORM OPERATOR              00095500
         B     CONSTIPL                GO BUILD DEFAULT RECORD          00095600
*                                                                       00095700
*        INFORM OPERATOR AND REISSUE WTOR ROUTINE                       00095800
*                                                                       00095900
INFORM   WTO   MF=(E,IFB020)           WRITE TO OPERATOR                00096000
*                                                                       00096100
         B     IPLREASN                BRANCH TO REISSUE WTOR           00096200
*                                                                       00096300
SECONDHF LA    R11,IDNUM               SET UP FOR LOOP CONTROL          00096400
         LA    R5,REPLY00              LOAD ADDR OF VALID SUB ID TABLE  00096500
RECHECK2 CLC   SUBIDRPL(2),0(R5)       VALID REPLY ?                    00096600
         BE    REPLYOK                 BRANCH TWO VALID REPLIES         00096700
         LA    R5,3(,R5)               BUMP POINTER TO NEXT VALID SUBID 00096800
         BCT   R11,RECHECK2            BRANCH IF THERE ARE MORE         00096900
         B     INFORM                  BRANCH NO MATCH ON 2ND HALF      00097000
*                                                                       00097100
*        CONSTRUCT IPL OR EOD RECORD FOR SYS1.LOGREC                    00097200
*                                                                       00097300
REPLYOK  MVC   REASON(2),WTOREPLY      MOVE IPL REASON TO RECORD        00097400
         MVC   SUBSYSID(1),2(R5)       MOVE SUB ID TO RECORD            00097500
         B     CONSTHDR                BRANCH TO CONSTRUCT HEADER       00097600
*                                                                       00097700
CONSTIPL MVC   REASON(2),DEFAULT       MOVE DEFAULT CODE TO RECORD      00097800
         B     CONSTHDR                BRANCH TO CONSTRUCT HEADER       00097900
*                                                                       00098000
*        READ OUTAGE RECORD                                             00098100
*                                                                       00098200
RDOUTREC MVC   CCWAREA(HDRCCWL),HDRCCW  MOVE CHAN PROGRAM               00098300
         LA    R8,SEARCHID+2           SET SEEK ADDR                    00098400
         STCM  R8,B'0111',CCW0+1       STORE IN 1ST CCW                 00098500
         LA    R8,CCW0                 ADDR OF 1ST                      00098600
         STCM  R8,B'0111',CCW8+1       CCW IN CHAIN                     00098700
         LA    R8,HDRREC               HDRREC ADDR                      00098800
         STCM  R8,B'0111',CCW16+1      TO CCW                           00098900
         L     R8,CVTDCB               R8 -> LOGREC DCB                 00099000
         ST    R8,IOBDCBPT                                              00099100
         L     R8,DCBDEBAD-IHADCB(R8)  R8 -> DEB                        00099200
         MVC   SEARCHID(6),DEBBINUM-DEBDASD+DEBBASND-DEBBASIC(R8)       00099300
         MVI   SEARCHID+6,2            SET SRCH ID TO 2                 00099400
         LA    R8,TPRECB               R8 -> ECB ADDR                   00099500
         ST    R8,IOBECBPT             STORE IN IOB                     00099600
         LA    R8,CCW0                 R8 -> CCW'S                      00099700
         ST    R8,IOBSTART             STORE IN IOB                     00099800
         MVI   IOBFLAG1,IOBCMDCH+IOBUNREL  SET FLAG1 FIELD IN IOB       00099900
         MVC   IOBSEEK+1(6),SEARCHID   SET SEEK ADDR                    00100000
REDRIVE  XC    TPRECB(4),TPRECB        CLEAR ECB                        00100100
*                                                                       00100200
         EXCP  TPRIOB                                                   00100300
*                                                                       00100400
         WAIT  1,ECB=TPRECB                                             00100500
*                                                                       00100600
         CLI   TPRECB,ECBNORM          READ OK ?                        00100700
         BE    MOVETIME                YES                              00100800
         CLI   TPRECB,ECBINCPT         INTERCEPT ?                      00100900
         BE    REDRIVE                 YES, TRY AGAIN                   00101000
         BR    R6                      RETURN                           00101100
*                                                                       00101200
MOVETIME MVC   OUTSAVE(8),TMESTMP      SAVE OUTAGE TIMESTAMP INFO       00101300
         BR    R6                      RETURN                           00101400
*                                                                       00101500
*        CONSTRUCT RECORD                                               00101600
*                                                                       00101700
MAKEREC  XC    RECORD(EODSIZE),RECORD  CLEAR RECORD AREA                00101800
*                                                                       00101900
CONSTHDR STIDP CPUSER                  STORE CPU ID & MODEL IN RECORD   00102000
         LR    R4,R2                   R4 -> CVT                        00102100
         SH    R4,KH4                  R4 -> CVTRELNO                   00102200
         MVC   INCREL(1),3(R4)         MOVE LAST DIGIT OF CVTRELNO      00102300
*                                      NUMBER TO RECORD                 00102400
         PACK  CONVERT,0(2,R4)         PACK CVTNUMB                     00102500
         CVB   R4,CONVERT              CONVERT REL NUM TO BINARY IN R4  00102600
         XC    CONVERT(8),CONVERT      CLEAR WORK AREA                  00102700
         STC   R4,OPSYS                STORE REL NUM IN RECORD          00102800
         OI    OPSYS,VS2IND            INDICATE VS2                     00102900
*                                                                       00103000
         TIME  DEC                     ISSUE TIME MACRO                 00103100
*                                                                       00103200
         ST    R0,TIME                 STORE TIME OF DAY IN RECORD      00103300
         ST    R1,DATE                 STORE DATE IN RECORD             00103400
         OI    SW1,TIMEBIT             SET TIME MACRO USED SWITCH       00103500
         CLI   FUNCTREQ+1,EODPARM      EOD RECORD ?                     00103600
         BE    PREPARE                 YES, EOD IS HEADER ONLY REC      00103700
         MVC   HIGHADDR(4),CVTMZ00     MOVE HIGHEST VIRT STORAGE ADDR   00103800
         L     R4,CVTDCB               GET ADDR OF LOGREC DCB           00103900
         SH    R4,KH8                  SUB TO PTR TO CHAN ASSN TAB ADDR 00104000
         L     R4,0(,R4)               LOAD ADDR OF CHAN ASSIGNMENT TAB 00104100
         MVC   CHANASSN(CHASTBLN),0(R4)  MOVE CHANNEL ASSIGNMNT         00104200
*                                      TABLE TO RECORD                  00104300
         MVC   OUTAGE(8),OUTSAVE       FETCH OUTAGE                     00104400
         OI    SW2,OUTFLAG             TURN ON OUTAGE INDICATOR         00104500
         OI    CLASRC,IPLTYPE          SET RECORD TYPE - CLASS & SOURCE 00104600
         LA    R0,IPLSIZE              LOAD RECORD LENGTH               00104700
         B     IPL                     BRANCH TO WRITE RECORD           00104800
*                                                                       00104900
PREPARE  OI    CLASRC,EODREC           SET RECORD TYPE - CLASS & SOURCE 00105000
         LA    R0,EODSIZE              LOAD RECORD LENGTH               00105100
IPL      LA    R1,RECORD               R1 -> RECORD                     00105200
         TM    FUNCTREQ,OUTFLAG        REQ TO WRITE OUTAGE ?            00105300
         BO    OUTAGEWT                YES, BRANCH                      00105400
LOADNEG  LNR   R0,R0                   SET LENGTH NEGATIVE              00105500
         B     WRTSTAT                 GO WRITE RECORD                  00105600
*                                                                       00105700
OUTAGEWT L     R0,OUTIND               INDICATE OUTAGE                  00105800
         LNR   R0,R0                   SET LENGTH NEGATIVE              00105900
         MVI   CLASRC,OUTTYPE          INDICATE OUTAGE REQUEST          00106000
         B     WRTSTAT                 GO WRITE RECORD                  00106100
*                                                                       00106200
*********************************************************************** 00106300
*                                                                       00106400
*        MESSAGE SECTION                                                00106500
*                                                                       00106600
*********************************************************************** 00106700
*                                                                       00106800
MESS     LR    R14,R13                 LOAD RETURN ADDR                 00106900
         TM    CVTOPTA,CVTNIP          NIP IN PROCESS ?                 00107000
         BO    EXIT                    YES, NO WTO'S - RETURN           00107100
         LA    R5,INDEX(R12)           SETUP REG FOR BRANCH TABLE ENTRY 00107200
         BR    R5                      BRANCH INTO BRANCH TABLE         00107300
*                                                                       00107400
INDEX    B     ERROR1M              *  +00  ERROR                       00107500
         B     ERROR2M              |  +04  MSG                         00107600
         B     ERROR4M              |  +08  BRANCH                      00107700
         B     MSGEWM               |  +12  TABLE                       00107800
         B     ERROR5M              V  +16  LOGREC NOT AVAILABLE        00107900
*                                                                       00108000
GETIME   TIME  DEC                                                      00108100
*                                                                       00108200
         ST    R0,SAVETIME             SAVE TIME                        00108300
         UNPK  UNPKHH(7),SAVETIME(4)   UNPACK THE TIME                  00108400
         MVC   FORHH(2),UNPKHH         MOVE HOURS - HH                  00108500
         MVI   FRSTDOT,C'.'            INSERT DELIMITER AFTER HH        00108600
         MVC   FORMM(2),UNPKMM         MOVE MINUTES - MM                00108700
         MVI   SECDOT,C'.'             INSERT DELIMETER AFTER MM        00108800
*                                      SECONDS ARE ALREADY IN POSITION  00108900
         MVI   UNPKTEN,C' '            BLANK OUT TENTHS OF A SECOND     00109000
         BR    R6                      RETURN TO CALLING ROUTINE        00109100
*                                                                       00109200
ERROR1M  ICM   R6,B'1111',CVTSV76M     R6 = SVC 76 MSG COUNT FIELD      00109300
         LR    R7,R6                                                    00109400
         BNZ   ERROR1MA                MSG COUNT NOT ZERO, BRANCH       00109500
         L     R7,DATA0DA4                                              00109600
ERROR1MA AL    R7,DATA0DAC                                              00109700
         CLM   R7,B'0011',KH100                                         00109800
         BL    ERROR1MB                                                 00109900
         N     R7,DATA0DB0                                              00110000
         CS    R6,R7,CVTSV76M                                           00110100
         BNE   ERROR1M                                                  00110200
         MVC   MSGAREA(IFB040L),IFB040   MOVE IN LOG FULL MSG IFB040I   00110300
         BAL   R6,GETIME               BRANCH TO GET TIME               00110400
         MVC   FULLTIME(8),FORHH       MOVE TIME TO MSG                 00110500
*                                                                       00110600
         WTO   MF=(E,MSGAREA)          ISSUE WRITE TO OPERATOR          00110700
*                                                                       00110800
         B     ERROR1MC                BRANCH TO FREEMAIN AND RETURN    00110900
*                                                                       00111000
ERROR1MB CS    R6,R7,CVTSV76M                                           00111100
         BNE   ERROR1M                                                  00111200
ERROR1MC LA    R8,LOGRECFL             SAVE LOG FULL COND CODE          00111300
         B     RTRNM                                                    00111400
*                                                                       00111500
ERROR2M  MVC   MSGAREA(IFB050L),IFB050   MOVE FORMAT ERR MSG            00111600
         BAL   R6,GETIME               CALL TO GET TIME                 00111700
         MVC   FORMTIME(8),FORHH       MOVE TIME TO MSG                 00111800
         B     ERREXIT1                BRANCH TO WTO                    00111900
*                                                                       00112000
ERROR4M  MVC   MSGAREA(IFB030L),IFB030   MOVE IN I/O ERR MSG            00112100
         LH    R5,IOBSENS0             LOAD SENSE BYTES                 00112200
         SLL   R5,4                    SHIFT LEFT 4 BITS                00112300
         STC   R5,SENSBYT3             STORE CHAR                       00112400
         SRL   R5,8                    SHIFT OUT END CHAR               00112500
         STC   R5,SENSBYT2             STORE CHAR                       00112600
         SRL   R5,8                    SHIFT OUT END CHAR               00112700
         STC   R5,SENSBYT1             STORE CHAR                       00112800
         OI    SENSBYT3,X'0F'          PUT IN 'F'                       00112900
         UNPK  SENSBYT1(4),SENSBYT1(3)  CHG SENSE TO NOS                00113000
         TR    SENSBYT1(4),CONVT-240   TRANSLATE TO HEX CHARS           00113100
         LH    R5,IOBUSTAT             LOAD STATUS BYTES                00113200
         SLL   R5,4                    SHIFT LEFT 4 BITS                00113300
         STC   R5,STATBYT3             STORE CHAR                       00113400
         SRL   R5,8                    SHIFT OUT END CHAR               00113500
         STC   R5,STATBYT2             STORE                            00113600
         SRL   R5,8                    SHIFT OUT END CHAR               00113700
         STC   R5,STATBYT1             STORE                            00113800
         OI    STATBYT3,X'0F'          PUT IN 'F'                       00113900
         UNPK  STATBYT1(4),STATBYT1(3)  CHG STATUS TO NOS               00114000
         TR    STATBYT1(4),CONVT-240   TRANSLATE TO HEX CHARS           00114100
         BAL   R6,GETIME               CALL TO GET TIME                 00114200
         MVC   IOERTIME(8),FORHH       MOVE TIME TO MSG                 00114300
         B     ERREXIT1                                                 00114400
*                                                                       00114500
ERROR5M  ICM   R6,B'1111',CVTSV76M                                      00114600
         LR    R7,R6                                                    00114700
         BNZ   ERROR5MA                                                 00114800
         L     R7,DATA0DA4                                              00114900
ERROR5MA AL    R7,DATA0DB4                                              00115000
         CLM   R7,B'1100',KH100                                         00115100
         BL    ERROR5MB                                                 00115200
         N     R7,DATA0DB8                                              00115300
         CS    R6,R7,CVTSV76M                                           00115400
         BNE   ERROR5M                                                  00115500
*                                                                       00115600
         WTO   MF=(E,IFB070)                                            00115700
*                                                                       00115800
         B     ERROR5MC                                                 00115900
*                                                                       00116000
ERROR5MB CS    R6,R7,CVTSV76M                                           00116100
         BNE   ERROR5M                                                  00116200
ERROR5MC B     RTRNM                                                    00116300
*                                                                       00116400
ERREXIT1 WTO   MF=(E,MSGAREA)          ISSUE WRITE TO OPERATOR          00116500
*                                                                       00116600
         LA    R8,IOERROR              SAVE I/O ERR COND CODE           00116700
         B     RTRNM                   BRANCH TO FREEMAIN AND RETURN    00116800
*                                                                       00116900
*        WRITE MSG IFB060E LOGREC NEAR FULL (90%)                       00117000
*                                                                       00117100
MSGEWM   SR    R6,R6                                                    00117200
         L     R1,DATA0DA8                                              00117300
         CS    R6,R1,CVTSV76M                                           00117400
*                                                                       00117500
         WTO   MF=(E,IFB060)           ISSUE WRITE TO OPERATOR          00117600
*                                                                       00117700
*        CLEAR WRTEWMSG FLAG TO PREVENT DUP MSG IN CASE OF SHORT        00117800
*        RECORD                                                         00117900
*                                                                       00118000
         NI    FLAGS,255-WRTEWMSG      CLEAR WRTEWMSG FLAG              00118100
         TM    INTSW,SHTSW             SHORT RECORD ?                   00118200
         BZ    EXIT                    NO, RETURN TO CALLER             00118300
         NI    INTSW,255-SHTSW         CLEAR SHORT RECORD INDICATOR     00118400
         L     R14,SHORTRTN            GET SHORT RECORD RETURN ADDR     00118500
         BR    R14                     GO CHECK FOR MORE                00118600
*                                                                       00118700
*********************************************************************** 00118800
*                                                                       00118900
*        DEMF RECORDER SECTION                                          00119000
*                                                                       00119100
*********************************************************************** 00119200
*                                                                       00119300
*        DETERMINE IF DEMF IS ACTIVE THEN POST RECORD TO DEMF BUFFER    00119400
*        ELSE RETURN TO STANDARD LOGREC RECORDER FUNCTION               00119500
*                                                                       00119600
DEMFPROC L     R2,CVTPTR                                                00119700
         L     R4,CVTDCB               R4 -> LOGREC DCB                 00119800
         LA    R6,IFBDCB-IFBDCB00                                       00119900
         SR    R4,R6                   R4 -> IFBDCB00                   00120000
         USING IFBDCB00,R4                                              00120100
         ICM   R6,B'1111',IFBBUFP      R6 -> DEMF BUFFER                00120200
         BZ    WRTREC                  NO BUFFER, BYPASS DEMF PROC      00120300
         TM    IFBFLGS1,X'80'          DEMF ACTIVE ?                    00120400
         BNO   WRTREC                  NO, BYPASS DEMF PROCESSING       00120500
         LR    R7,R0                   MAKE A COPY OF SVC INPUT R0      00120600
         LR    R8,R1                   MAKE A COPY OF SVC INPUT R1      00120700
*                                                                       00120800
         GETMAIN RC,LV=LOGWAL,SP=245     REQUEST WORKAREA               00120900
*                                                                       00121000
         LTR   R15,R15                 SUCCESSFUL ?                     00121100
         BZ    DEMFSTGA                YES, BRANCH                      00121200
         LA    R15,8                   NO, RETURN TO SVC ISSUER         00121300
         BR    R14                                                      00121400
*                                                                       00121500
DEMFSTGA LR    R9,R1                   R9 -> LOGWA DSECT                00121600
         LR    R0,R1                   CLEAR GOTTEN STORAGE             00121700
         LA    R1,LOGWAL               L'WORKAREA                       00121800
         SR    R15,R15                 L'=0, PAD=X'00'                  00121900
         MVCL  R0,R14                                                   00122000
         MVC   ESTAEWRK(16),ESTALIST   MOVE IN ESTAE MF=L               00122100
         ST    R9,ESTAPARM             INITIALIZE ESTAE PARAMETERS      00122200
*                                                                       00122300
         ESTAE PGMCKENT,PARAM=ESTAPARM,MF=(E,ESTAEWRK)                  00122400
*                                                                       00122500
         MVC   ENQDEQWA(4),ENQDEQP     MOVE IN ENQ/DEQ PARAMETERS       00122600
         TM    IFBFLGS2,X'40'                                           00122700
         BZ    IFB00A40                                                 00122800
         MVC   CCWAREA(16),SYSZLOGR    MOVE IN LOGREC MAJ/MIN NAMES     00122900
         B     IFB00A46                                                 00123000
*                                                                       00123100
*        MOVE DEMF MAJ/MIN NAMES TO ENQ PARAMETER LIST                  00123200
*                                                                       00123300
IFB00A40 MVC   CCWAREA(16),BNGLOGER    MOVE ENQ MAJ/MIN NAMES           00123400
*                                      TO WORKAREA                      00123500
IFB00A46 LA    R10,CCWAREA             R10 > MAJ NAME                   00123600
         ST    R10,QPTR                STORE IN ENQ/DEQ LIST            00123700
         LA    R10,WRTCCW                                               00123800
         ST    R10,RPTR                STORE IN ENQ/DEQ LIST            00123900
         MVC   ESTAPARM+4(1),0(R8)                                      00124000
         OI    ESTAPARM+4,X'0F'                                         00124100
         CLI   ESTAPARM+4,X'3F'                                         00124200
         BNE   IFB00AA2                                                 00124300
         TM    2(R8),X'20'             TEST INPUT RECORD                00124400
         BO    IFB00CC6                                                 00124500
         CLI   54(R8),X'42'            TEST INPUT RECORD                00124600
         BE    IFB00CC6                                                 00124700
*                                                                       00124800
         ENQ   MF=(E,ENQDEQWA)                                          00124900
*                                                                       00125000
         LA    R1,ENQDEQWA                                              00125100
         NI    2(R1),X'F8'                                              00125200
         LTR   R15,R15                 ENQ SUCCESSFUL ?                 00125300
         BNZ   IFB00CBC                NO, EXIT                         00125400
         OI    INTSW,ENQIND            SET ENQ DONE                     00125500
         TM    IFBFLGS1,X'80'          DEMF ACTIVE ?                    00125600
         BZ    IFB00CB0                NO, BRANCH                       00125700
         BAL   R10,IFB00B1C                                             00125800
         MVC   CCWAREA2+1(3),57(R8)                                     00125900
         B     IFB00C06                                                 00126000
*                                                                       00126100
IFB00AA2 CLI   ESTAPARM+4,X'9F'                                         00126200
         BNE   IFB00AE8                                                 00126300
         CLI   4(R8),X'19'                                              00126400
         BL    IFB00AB6                                                 00126500
         B     IFB00CC6                                                 00126600
*                                                                       00126700
IFB00AB6 CLI   4(R8),X'01'                                              00126800
         BL    IFB00CC6                                                 00126900
*                                                                       00127000
         ENQ   MF=(E,ENQDEQWA)                                          00127100
*                                                                       00127200
         LA    R1,ENQDEQWA                                              00127300
         NI    2(R1),X'F8'                                              00127400
         LTR   R15,R15                 ENQ SUCCESSFUL ?                 00127500
         BNZ   IFB00CBC                NO, EXIT                         00127600
         OI    INTSW,ENQIND            SET ENQ DONE                     00127700
         TM    IFBFLGS1,X'80'          DEMF ACTIVE ?                    00127800
         BZ    IFB00CB0                NO, BRANCH                       00127900
         BAL   R10,IFB00B1C                                             00128000
         MVC   CTFIELD(2),24(R8)                                        00128100
         B     IFB00C06                                                 00128200
*                                                                       00128300
IFB00AE8 CLI   ESTAPARM+4,X'2F'                                         00128400
         BE    IFB00AF8                                                 00128500
         CLI   ESTAPARM+4,X'1F'                                         00128600
         BNE   IFB00CC6                                                 00128700
*                                                                       00128800
IFB00AF8 ENQ   MF=(E,(1))                                               00128900
*                                                                       00129000
         LA    R1,ENQDEQWA                                              00129100
         NI    2(R1),X'F8'                                              00129200
         LTR   R15,R15                                                  00129300
         BNZ   IFB00CBC                                                 00129400
         OI    INTSW,ENQIND            SET ENQ DONE                     00129500
         TM    IFBFLGS1,X'80'          DEMF ACTIVE ?                    00129600
         BZ    IFB00CB0                NO, BRANCH                       00129700
         BAL   R10,IFB00B1C                                             00129800
         B     IFB00C06                                                 00129900
*                                                                       00130000
*        MOVE LOGREC RECORD TO DEMF BUFFER                              00130100
*                                                                       00130200
IFB00B1C TM    IFBFLGS2,X'80'                                           00130300
         BZ    IFB00B82                                                 00130400
         L     R11,32(,R6)                                              00130500
         LA    R11,1(R11)                                               00130600
         C     R11,28(,R6)                                              00130700
         BNH   IFB00B38                                                 00130800
         LA    R11,1                                                    00130900
IFB00B38 LR    R1,R11                                                   00131000
         BCTR  R1,R0                                                    00131100
         MH    R1,18(,R6)                                               00131200
         LA    R9,60(,R6)                                               00131300
         AR    R9,R1                                                    00131400
         LH    R2,16(,R6)                                               00131500
         EX    R2,IFB00BF8                                              00131600
         LPR   R2,R7                                                    00131700
         LH    R12,18(,R6)                                              00131800
         SH    R12,16(,R6)                                              00131900
         CR    R2,R12                                                   00132000
         BNH   IFB00B60                                                 00132100
         LR    R2,R12                                                   00132200
IFB00B60 AH    R2,16(,R6)                                               00132300
         STC   R2,0(R9)                                                 00132400
         SH    R2,16(,R6)                                               00132500
         BCTR  R2,0                                                     00132600
         AH    R9,16(,R6)                                               00132700
         EX    R2,IFB00BF2                                              00132800
         ST    R11,32(,R6)                                              00132900
         SH    R9,16(,R6)                                               00133000
         BR    R10                                                      00133100
*                                                                       00133200
IFB00B82 LA    R9,4020(,R6)                                             00133300
IFB00B86 TM    CCWAREA2,X'80'                                           00133400
         BZ    IFB00B96                                                 00133500
         LA    R9,1(,R9)                                                00133600
         B     IFB00B86                                                 00133700
*                                                                       00133800
IFB00B96 CLI   CCWAREA2,X'7F'                                           00133900
         BNE   IFB00BA6                                                 00134000
         OI    4061(R6),X'20'                                           00134100
         B     IFB00C72                                                 00134200
*                                                                       00134300
IFB00BA6 SR    R11,R11                                                  00134400
         IC    R11,0(R9)                                                00134500
         LR    R1,R9                                                    00134600
         MH    R11,4016(,R6)                                            00134700
         L     R9,356(,R4)                                              00134800
         AR    R9,R11                                                   00134900
         LH    R2,4018(,R6)                                             00135000
         EX    R2,IFB00BF8                                              00135100
         LPR   R2,R7                                                    00135200
         LH    R12,4016(,R6)                                            00135300
         SH    R12,4018(,R6)                                            00135400
         CR    R2,R12                                                   00135500
         BH    IFB00BFE                                                 00135600
         AH    R2,4018(,R6)                                             00135700
         STC   R2,0(R9)                                                 00135800
         SH    R2,4018(,R6)                                             00135900
         BCTR  R2,0                                                     00136000
         AH    R9,4018(,R6)                                             00136100
         EX    R2,IFB00BF2                                              00136200
         OI    0(R1),X'80'                                              00136300
         SH    R9,4018(,R6)                                             00136400
         BR    R10                                                      00136500
*                                                                       00136600
IFB00BF2 MVC   CCWAREA2(1),0(R8)                                        00136700
IFB00BF8 XC    CCWAREA2(1),CCWAREA2                                     00136800
*                                                                       00136900
IFB00BFE OI    4061(R6),X'40'                                           00137000
         B     IFB00C72                                                 00137100
*                                                                       00137200
IFB00C06 TM    IFBFLGS2,X'80'                                           00137300
         BZ    IFB00C42                                                 00137400
         L     R1,0(,R6)                                                00137500
         L     R2,360(,R4)                                              00137600
         LA    R9,IFB00CF0                                              00137700
         LA    R10,4(,R6)                                               00137800
         SR    R0,R0                                                    00137900
         ST    R1,0(,R10)                                               00138000
         ST    R2,4(,R10)                                               00138100
         ST    R9,8(,R10)                                               00138200
         LR    R1,R10                                                   00138300
*                                                                       00138400
         POST  ,(0),MF=(E,(1))                                          00138500
*                                                                       00138600
         B     IFB00CB0                                                 00138700
*                                                                       00138800
IFB00C42 LA    R1,4000(,R6)                                             00138900
         L     R2,360(,R4)                                              00139000
         LA    R9,IFB00CF0                                              00139100
         LA    R10,4004(,R6)                                            00139200
         SR    R0,R0                                                    00139300
         ST    R1,0(,R10)                                               00139400
         ST    R2,4(,R10)                                               00139500
         ST    R9,8(,R10)                                               00139600
         LR    R1,R10                                                   00139700
*                                                                       00139800
         POST  ,(0),MF=(E,(1))                                          00139900
*                                                                       00140000
IFB00C72 TM    4061(R6),X'80'                                           00140100
         BZ    IFB00CB0                                                 00140200
         LA    R9,96(,R5)                                               00140300
         L     R9,12(,R9)                                               00140400
         LA    R1,ENQDEQWA                                              00140500
         OI    2(R1),X'01'                                              00140600
*                                                                       00140700
         DEQ   MF=(E,(1))                                               00140800
*                                                                       00140900
         NI    INTSW,255-ENQIND                                         00141000
*                                                                       00141100
         FREEMAIN R,LV=LOGWAL,SP=245,A=(9)  FREE WORK AREA              00141200
*                                                                       00141300
         ESTAE 0                       CANCEL ESTAE                     00141400
*                                                                       00141500
         SR    R15,R15                                                  00141600
         BR    R14                                                      00141700
*                                                                       00141800
IFB00CB0 LA    R9,96(,R5)                                               00141900
         L     R9,12(,R9)                                               00142000
         LA    R1,ENQDEQWA                                              00142100
IFB00CBC OI    2(R1),X'01'                                              00142200
*                                                                       00142300
         DEQ   MF=(E,(1))                                               00142400
*                                                                       00142500
         NI    INTSW,255-ENQIND                                         00142600
IFB00CC6 LA    R9,96(,R5)                                               00142700
         L     R1,12(,R9)                                               00142800
*                                                                       00142900
         FREEMAIN R,LV=LOGWAL,SP=245,A=(1)                              00143000
*                                                                       00143100
         ESTAE 0                       CANCEL ESTAE                     00143200
*                                                                       00143300
         LR    R0,R7                   RESTORE PARAMETER REGS           00143400
         LR    R1,R8                                                    00143500
         B     WRTREC                                                   00143600
*                                                                       00143700
IFB00CF0 BR    R14                                                      00143800
*                                                                       00143900
ENQDEQP  ENQ   (0,0,E,6,SYSTEMS),RET=TEST,MF=L                          00144000
*                                                                       00144100
BNGLOGER DC    C'BNGLOGER'                                              00144200
         DC    C'BUFFER'                                                00144300
         DC    X'0000'                                                  00144400
SYSZLOGR DC    C'SYSZLOGR'                                              00144500
         DC    C'BUFFER'                                                00144600
         DC    X'0000'                                                  00144700
*                                                                       00144800
*        CONSTANTS AND CCW'S                                            00144900
*                                                                       00145000
HDRCCW   CCW   SEARIDEQ,0,CCWCC,5    * SEARCH ON ID EQUAL               00145100
         CCW   CCWTIC,0,CCWCC,1      | TRANSFER IN CHANNEL              00145200
         CCW   READDATA,0,0,40       | READ HDR RECORD                  00145300
*                                    |                                  00145400
WTCCW    CCW   SEARIDEQ,0,CCWCC,5    | SEARCH ON ID EQUAL               00145500
         CCW   CCWTIC,0,CCWCC,1      | TRANSFER IN CHANNEL              00145600
         CCW   WRITECKD,0,CCWCD,8    | WRITE CKD 8 BYTES DATA CHAINED   00145700
         CCW   0,0,CCWSLI,0          | TO RECORD BEING WRITTEN          00145800
HDRCCWL  EQU   *-HDRCCW              V L'CCWS TO MOVE TO LOGWA          00145900
*                                                                       00146000
ENQDEQL  ENQ   (MAJOR,MINOR,E,8,SYSTEM),RET=NONE,MF=L      *            00146100
MAJOR    DC    CL8'SYSZLOGR'                               |            00146200
MINOR    DC    CL8'RECORDER'                               V            00146300
*                                                                       00146400
MULTICON DC    H'10'                   CONSTANT WHICH IS MULTIPLIED     00146500
*                                      TO OFFSET TO BECOME INDEX TO     00146600
*                                      STAT TABS                        00146700
KXFFFF   DC    X'FFFF'                                                  00146800
MASK     DC    F'15'                                                    00146900
OUTIND   DC    F'1'                    OUTAGE RECORD                    00147000
DATA0DA4 DC    X'00620062'                                              00147100
DATA0DA8 DC    X'00630063'                                              00147200
DATA0DAC DC    X'00000001'                                              00147300
DATA0DB0 DC    X'FFFF0000'                                              00147400
DATA0DB4 DC    X'00010000'                                              00147500
DATA0DB8 DC    X'0000FFFF'                                              00147600
KH100    DC    H'100'                                                   00147700
KH4      DC    H'4'                    CONSTANT OF FOUR                 00147800
KBLANKS  DC    CL5' '                                                   00147900
*                                                                       00148000
*        DEVICE TYPE CODES FOR DEVICES WITH BUFFERED LOGS               00148100
*                                                                       00148200
NEWDEVTB DC    X'2006'                 ZEUS MOD 1 2305-1 DEVICE         00148300
         DC    X'2007'                 ZEUS MOD 2 2305-2 DEVICE         00148400
         DC    X'2009'                 MERLIN     3330   DEVICE         00148500
         DC    X'200A'                 WINCHESTER 3340   DEVICE         00148600
         DC    X'200B'                 3350 DEVICE                      00148700
         DC    X'200D'                 ICEBERG   3330-11 DEVICE         00148800
         DC    X'0819'                 MOHAWK-3895                      00148900
UR3800   DC    X'080E'                                                  00149000
SS1      DC    X'0842'                 MSS                              00149100
TYASPN   DC    X'8003'                 ASPEN-3420/3410 DEVICE           00149200
*                                                                       00149300
*        BLDL PARAMETER LIST FOR IFCRDE03                               00149400
*                                                                       00149500
         DC    0H'0'                                                    00149600
IFCRDE03 DC    XL18'00'                                                 00149700
         ORG   IFCRDE03                                                 00149800
         DC    H'1'                                                     00149900
         DC    H'14'                                                    00150000
         DC    CL8'IFCRDE03'           RDE MODULE                       00150100
         DC    XL6'00'                                                  00150200
         ORG                                                            00150300
*                                                                       00150400
*        MESSAGES TO OPERATOR                                           00150500
*                                                                       00150600
IFB020   WTO   'IFB020I INVALID REPLY TO IFB010',                      X00150700
               DESC=4,ROUTCDE=2,MF=L                                    00150800
*                                                                       00150900
IFB010   WTO   'IFB010D ENTER ''IPL REASON,SUBSYSTEM ID'' OR ''U''',   X00151000
               DESC=2,ROUTCDE=1,MF=L                                    00151100
IFB010L  EQU   *-IFB010                                                 00151200
*                                                                       00151300
*        VALID WTOR REASON REPLIES                                      00151400
*                                                                       00151500
REPLYNM  DC    C'NM'                   NORMAL                           00151600
REPLYUN  DC    C'UN'                   UNKNOWN                          00151700
REPLYIE  DC    C'IE'                   IBM HDW OR TYPE1 CODE            00151800
REPLYIM  DC    C'IM'                   SAME AS IE ACTION TAKEN          00151900
REPLYOP  DC    C'OP'                   OPERATIONAL PROBLEM              00152000
REPLYEN  DC    C'EN'                   ENVIRONMENTAL                    00152100
REPLYCE  DC    C'CE'                   CE/SE HAS SYSTEM CONTROL         00152200
REPLYUP  DC    C'UP'                   USER PROGRAM                     00152300
REPLYPP  DC    C'ME'                   MEDIA                            00152400
         DC    X'0000000000000000'     RESERVED FOR MORE REASONS        00152500
*                                                                       00152600
*        VALID WTOR SUBSYSTEM ID REPLIES                                00152700
*                                                                       00152800
REPLY00  DC    C'00'                   NOT KNOWN                        00152900
         DC    X'00'                                                    00153000
REPLY10  DC    C'10'                   PROCESSOR SUBSYSTEM              00153100
         DC    X'10'                                                    00153200
REPLY20  DC    C'20'                   DASD                             00153300
         DC    X'20'                                                    00153400
REPLY30  DC    C'30'                   DISK                             00153500
         DC    X'30'                                                    00153600
REPLY40  DC    C'40'                   TAPE                             00153700
         DC    X'40'                                                    00153800
REPLY50  DC    C'50'                   CARD MACHINE/PRINTER             00153900
         DC    X'50'                                                    00154000
REPLY51  DC    C'51'                   AUX CARD MACH/PRINTER            00154100
         DC    X'51'                                                    00154200
REPLY52  DC    C'52'                   PRINTER                          00154300
         DC    X'52'                                                    00154400
REPLY60  DC    C'60'                   DRUM                             00154500
         DC    X'60'                                                    00154600
REPLY70  DC    C'70'                   TELEPROCESSING                   00154700
         DC    X'70'                                                    00154800
REPLY80  DC    C'80'                   GRAPHICS                         00154900
         DC    X'80'                                                    00155000
REPLY90  DC    C'90'                   IBM PROGRAM                      00155100
         DC    X'90'                                                    00155200
REPLY91  DC    C'91'                   IBM PROGRAMING PRODUCT           00155300
         DC    X'91'                                                    00155400
REPLY92  DC    C'92'                                                    00155500
         DC    X'92'                                                    00155600
         DC    X'000000000000000000'                                    00155700
*                                                                       00155800
DEFAULT  DC    C'DF'                   DEFAULT REASON CODE FOR IPL      00155900
*                                                                       00156000
KH8      DC    H'8'                    CONSTANT OF 8                    00156100
*                                                                       00156200
*        MESSAGES TO OPERATOR                                           00156300
*                                                                       00156400
IFB030   WTO   'IFB030I SYS1.LOGREC I/O ACCESS ERROR,SENS,STAT,HH.MM.SSX00156500
               ',DESC=4,ROUTCDE=1,MF=L                                  00156600
IFB030L  EQU   *-IFB030                                                 00156700
*                                                                       00156800
IFB040   WTO   'IFB040I SYS1.LOGREC AREA IS FULL,HH.MM.SS',            X00156900
               DESC=11,ROUTCDE=1,MF=L                                   00157000
IFB040L  EQU   *-IFB040                                                 00157100
*                                                                       00157200
IFB050   WTO   'IFB050I SYS1.LOGREC FORMAT ERROR,HH.MM.SS',            X00157300
               DESC=4,ROUTCDE=1,MF=L                                    00157400
IFB050L  EQU   *-IFB050                                                 00157500
*                                                                       00157600
IFB060   WTO   'IFB060E SYS1.LOGREC NEAR FULL',                        X00157700
               DESC=11,ROUTCDE=1,MF=L                                   00157800
*                                                                       00157900
IFB070   WTO   'IFB070I SYSRES CANNOT BE ACCESSED. RECORD IS LOST ',   X00158000
               DESC=4,ROUTCDE=2,MF=L                                    00158100
*                                                                       00158200
CONVT    DC    C'0123456789ABCDEF'     CONVERSION TABLE                 00158300
*                                                                       00158400
ESTALIST ESTAE MF=L                                                     00158500
*                                                                       00158600
*        KEEP THIS THE LAST CODE IN MODULE                              00158700
*                                                                       00158800
*        STAE EXIT ROUTINE TO DEQUEUE AND FREE MAIN                     00158900
*                                                                       00159000
         DROP  R3                                                       00159100
PGMCKENT LR    R5,R15                  SET BASE                         00159200
         USING PGMCKENT,R5                                              00159300
         LA    R9,12                                                    00159400
         CR    R0,R9                   SDWA PROVIDED ?                  00159500
         BE    GETREGS                 NO, USE R2                       00159600
         L     R2,0(R1)                GET POINTER IN R2                00159700
GETREGS  L     R9,0(R2)                GET REGISTER                     00159800
         TM    INTSW,ENQIND            ENQUEUE DONE ?                   00159900
         BZ    NOENQ                   NO, DO NOT DEQUEUE               00160000
*                                                                       00160100
         DEQ   MF=(E,ENQDEQWA)         ISSUE DEQUEUE                    00160200
*                                                                       00160300
NOENQ    FREEMAIN  R,LV=LOGWAL,SP=245,A=(9)                             00160400
*                                                                       00160500
         BR    R14                     RETURN TO RTM                    00160600
*                                                                       00160700
*        SVC 76 GETMAINED WORKAREA                                      00160800
*                                                                       00160900
LOGWA    DSECT                                                          00161000
WCFIELD  DS    CL2                     UNUSED                           00161100
CTFIELD  DS    0CL10                   RECORD ENTRY TP ERROR            00161200
CTFCCHH  DS    XL4                     COUNT FIELD ID-CCHH              00161300
CTFR     DS    XL1                     COUNT FIELD ID-R                 00161400
CTFK     DS    XL1                     COUNT FIELD KEY LENGTH (0)       00161500
CTFDL    DS    AL2                     COUNT FIELD DATA LENGTH          00161600
         DS    XL2                                                      00161700
         DS    0H                                                       00161800
         DS    CL10                                                     00161900
         DS    0D                                                       00162000
CCWAREA  DS    0CL56                   CHANNEL PROGRAM                  00162100
CCW0     CCW   SEARIDEQ,0,CCWCC,5      FOR READING/WRITING HDRREC       00162200
CCW8     CCW   CCWTIC,0,CCWCC,1                                         00162300
CCW16    CCW   READDATA,0,0,40                                          00162400
*                                                                       00162500
CCW24    CCW   SEARIDEQ,0,CCWCC,5      FOR WRITING LOGREC RECORD        00162600
CCW32    CCW   CCWTIC,0,CCWCC,1                                         00162700
CCW40    CCW   WRITECKD,0,CCWCD,8      DATA CHAINING                    00162800
CCW48    CCW   0,0,CCWSLI,0                                             00162900
*                                                                       00163000
MSGAREA  EQU   CCWAREA                                                  00163100
EWMTIME  EQU   MSGAREA+34              TIME PORTION OF EARLY WARNIG MSG 00163200
FULLTIME EQU   MSGAREA+37              TIME PORTION OF LOG FULL MSG     00163300
FORMTIME EQU   MSGAREA+37              TIME PORTION OF FORMAT ERR MSG   00163400
SENSBYT1 EQU   MSGAREA+41              SNS CHAR 1                       00163500
SENSBYT2 EQU   MSGAREA+42              SNS CHAR 2                       00163600
SENSBYT3 EQU   MSGAREA+43              SNS CHAR 3                       00163700
SENSBYT4 EQU   MSGAREA+44              SNS CHAR 4                       00163800
STATBYT1 EQU   MSGAREA+46              STATUS CHAR 1                    00163900
STATBYT2 EQU   MSGAREA+47              STATUS CHAR 2                    00164000
STATBYT3 EQU   MSGAREA+48              STATUS CHAR 3                    00164100
STATBYT4 EQU   MSGAREA+49              STATUS CHAR 4                    00164200
IOERTIME EQU   MSGAREA+51              TIME PORTION OF I/O ERR          00164300
*                                                                       00164400
         DS    CL5                     SPARE                            00164500
*                                                                       00164600
TPRIOB   DS    5D                      IOB FOR DASD DEVICE              00164700
         ORG   TPRIOB-16               ALLOW FOR IOB PREFIX             00164800
*                                                                       00164900
         PRINT NOGEN                                                    00165000
*                                                                       00165100
         IEZIOB DSECT=NO               MAP IOB                          00165200
*                                                                       00165300
         PRINT GEN                                                      00165400
*                                                                       00165500
TIMEAREA EQU   TPRIOB                  WORK AREA FOR CONVERTING TIME    00165600
UNPKHH   EQU   TIMEAREA+3              UNPACKED HOURS - HH              00165700
UNPKMM   EQU   TIMEAREA+5              UNPACKED MINUTES - MM            00165800
UNPKSS   EQU   TIMEAREA+7              UNPACKED SECONDS - SS            00165900
UNPKTEN  EQU   TIMEAREA+9              UNPACKED TENTHS OF A SECOND - T  00166000
UNPKHUN  EQU   TIMEAREA+10             UNPACKED HUNDREDS OF A SECOND -H 00166100
FORHH    EQU   TIMEAREA+1              FORMATTED HOURS - HH             00166200
FRSTDOT  EQU   TIMEAREA+3              FIRST DELIMETER - AFTER HH       00166300
FORMM    EQU   TIMEAREA+4              FORMATTED MINUTES - MM           00166400
SECDOT   EQU   TIMEAREA+6              SECOND DELIMETER - AFTER MM      00166500
FORSS    EQU   TIMEAREA+7              FORMATTED SECONDS - SS           00166600
*                                                                       00166700
SAVETIME EQU   TPRIOB+32               SAVE TIME AFTER TIME MACRO INST  00166800
FUNCTREQ DS    H                       RECORD TYPE                      00166900
         DS    H                       SPARE                            00167000
SEARCHID DS    CL7                     BBCCHHR USED FOR RD/WR           00167100
*                                                                       00167200
FLAGS    DS    X                       INDICATORS                       00167300
WRTEWMSG EQU   X'01'                   WRITE EARLY WARNING MSG          00167400
IOERR    EQU   X'02'                   IO ACCESS ERROR                  00167500
BADHDR   EQU   X'04'                   HDRREC IS NOT VALID              00167600
EOEXTFLG EQU   X'08'                   LOGREC IS FULL                   00167700
LOGRECNA EQU   X'10'                   LOGREC NOT AVAILABLE             00167800
*                                                                       00167900
*        LOGREC HEADER RECORD                                           00168000
*        INITIALIZED BY IFCDIP00                                        00168100
*        UPDATED AND REWRITTEN BY IFBSVC76                              00168200
*                                                                       00168300
HDRREC   DC    X'FFFF'             +00 RECORD ID                        00168400
HDRSTRCC DC    XL4'00000000'       +02 START OF LOGREC EXTENT CCHH      00168500
HDRENDCC DC    XL4'00000000'       +06 END OF LOGREC EXTENT CCHH        00168600
HDRMSGCT DC    XL1'00'             +10 COUNT OF LOGREC FULL MSG         00168700
HDRREA   DC    XL7'00000000000000' +11 RECORD ENTRY AREA (BBCCHHR)      00168800
*                                      START OF LOGREC RECORDS          00168900
HDRTBAL  DC    AL2(0)              +18 TRACK BALANCE (2 BYTES UNSIGNED) 00169000
HDRTRKLN DC    AL2(0)              +20 NO OF BYTES PER TRACK     ..     00169100
HDRLASTR DC    XL7'00000000000000' +22 LAST RECORD WRITTEN (BBCCHHR)    00169200
HDRTRK   DC    AL2(0)              +29 TRKS PER CYL(HIGHEST TRK ON CYL) 00169300
HDREWTC  DC    AL2(0)              +31 EARLY WARNING TRACK CAPACITY     00169400
*                                      SET AT 90% OF LAST TRACK         00169500
HDRDEVT  DC    X'00'               +33 UNIT DEVICE TYPE (LOW NIBBLE)    00169600
HDREWMT  DC    XL4'00000000'       +34 EARLY WARNING TRK (CCHH)         00169700
HDREWSW  DC    X'00'                   EARLY WARNING SWITCH             00169800
HDREWMI  EQU   X'80'               +38 SET WHEN EARLY WARNING MSG       00169900
*                                      IFB060E HAS BEEN ISSUED TO OPER  00170000
HDRFRSW  EQU   X'20'                   FRAMES PRESENT IN LOGREC         00170100
HDRCHK   DC    X'FF'               +39 CHECK BYTE                       00170200
HDRRECL  EQU   *-HDRREC                L'HDRREC                         00170300
*                                                                       00170400
TMESTMP  EQU   HDRREC+8                OUTAGE TIME STAMP                00170500
*                                                                       00170600
TPRECB   DS    F                       ECB FOR I/O                      00170700
         DS    0D                                                       00170800
SAVEREGS DS    15F                     REGISTER SAVE AREA               00170900
*                                                                       00171000
RETURNAD DS    F                       SAVE R14 AREA                    00171100
SHORTRTN DS    F                       SHORT RE RETURN ADDR             00171200
INTSW    DS    CL1                     INTERNAL SWITCH                  00171300
SHTSW    EQU   X'01'                   SHORT RE INDICATOR               00171400
ENQIND   EQU   X'02'                   ENQUEUE IND FOR STAE             00171500
*                                                                       00171600
*        ORG BACK TO BEGINNING OF LOGWA                                 00171700
*                                                                       00171800
         ORG   LOGWA                                                    00171900
*                                                                       00172000
CCWAREA2 DS    3D                      TO HOLD RECORD CCW'S             00172100
CONVERT  EQU   CCWAREA2                AREA TO ISSUE CVB FROM           00172200
WTOAREA  EQU   CCWAREA2                WTO MESSAGE AREA                 00172300
WTOREPLY EQU   CCWAREA2                TO HOLD REPLY OF WTOR            00172400
SUBIDRPL EQU   WTOREPLY+3              SUB-SYSTEM ID PORTION OF REPLY   00172500
WTORAREA EQU   CCWAREA2+8              WTOR MESSAGE AREA                00172600
WTORLGTH EQU   WTORAREA                LENGTH OF WTOR REPLY             00172700
WTORADDR EQU   WTORAREA                ADDR OF WTOR REPLY BUFFER        00172800
WTORECB  EQU   WTORAREA+4              ADDR OF WTOR ECB                 00172900
WTORMESS EQU   WTORAREA+8              BASIC STRUCTURE OF WTOR          00173000
         DS    D                       ALIGNMENT                        00173100
WRTCCW   DS    4D                      WRITE CHANNEL COMMAND WORDS      00173200
         DS    D                       ALIGNMENT                        00173300
*                                                                       00173400
         ORG   WCFIELD+296             RESTORE LOCTR                    00173500
*                                                                       00173600
RECORD   DS    8D                                                       00173700
CLASRC   EQU   RECORD                                                   00173800
OPSYS    EQU   RECORD+1                                                 00173900
SW1      EQU   RECORD+2                                                 00174000
SW2      EQU   RECORD+3                                                 00174100
INCREL   EQU   RECORD+5                                                 00174200
DATE     EQU   RECORD+8                                                 00174300
TIME     EQU   RECORD+12                                                00174400
CPUSER   EQU   RECORD+16                                                00174500
CPUSER1  EQU   RECORD+17                                                00174600
CPUMODEL EQU   RECORD+20                                                00174700
CPUID    EQU   RECORD+20                                                00174800
SUBSYSID EQU   RECORD+24                                                00174900
DEVTYP   EQU   RECORD+24                                                00175000
REASON   EQU   RECORD+28                                                00175100
SDRCNT   EQU   RECORD+28                                                00175200
CHANMAP  EQU   RECORD+30                                                00175300
CUA      EQU   RECORD+30                                                00175400
UCUA     EQU   RECORD+31                                                00175500
CHANASSN EQU   RECORD+32                                                00175600
SDR      EQU   RECORD+32                                                00175700
HIGHADDR EQU   RECORD+40                                                00175800
SPARE2   EQU   RECORD+44                                                00175900
OUTAGE   EQU   RECORD+48                                                00176000
OUTSAVE  EQU   RECORD+56               OUTAGE SAVE AREA                 00176100
*                                                                       00176200
ESTAEWRK DS    CL16                    STAE LIST SAVE AREA              00176300
ESTAPARM DS    2F                      REG SAVEAREA FOR STAE            00176400
*                                                                       00176500
TRKWA    TRKCALC MF=L                                                   00176600
*                                                                       00176700
TRKRSAVE DS    14F                     SAVE AREA FOR R1-R14 ACROSS      00176800
*                                      TRKCALC CALL                     00176900
LOGWAL   EQU   *-LOGWA                 L' SVC 76 WORKAREA               00177000
*        REUSE OF LOGWA FIELDS                                          00177100
WTOECB   EQU   RECORD                                                   00177200
*                                                                       00177300
         ORG   LOGWA+120                                                00177400
EXSTAT   DS    6F                      ADDITIONAL STAT AREA             00177500
         ORG   SHORTRTN+4                                               00177600
         DS    0D                                                       00177700
WORKAREA DS    D                       WORK AREA                        00177800
*                                                                       00177900
*        ENQ LIST AREA                                                  00178000
*                                                                       00178100
ENQDEQWA DS    F                       ENQ LIST AREA                    00178200
QPTR     DS    F                       QNAME POINTER                    00178300
RPTR     DS    F                       RNAME POINTER                    00178400
QNM      DS    CL8                     QNAME AREA                       00178500
RNM      DS    CL8                     RNAME AREA                       00178600
*                                                                       00178700
*        END OF LOGWA                                                   00178800
*                                                                       00178900
*                                                                       00179000
*        IOS CCW AND CSW EQUATES                                        00179100
*                                                                       00179200
*        CCW COMMAND CODE EQUATES FOR DISK I/O                          00179300
*                                                                       00179400
CCWNOOP  EQU   X'03'              NO OPERATION                          00179500
CCWTIC   EQU   X'08'              TRANSFER IN CHANNEL                   00179600
CCWSENSE EQU   X'04'              SENSE                                 00179700
*                                                                       00179800
SEEK     EQU   X'07'              SEEK                                  00179900
SEEKC    EQU   X'0B'              SEEK CYL                              00180000
SEEKH    EQU   X'1B'              SEEK HEAD                             00180100
SEARKEQ  EQU   X'29'              SEARCH KEY EQUAL                      00180200
SEARKHE  EQU   X'69'              SEARCH KEY HIGH OR EQUAL              00180300
READHA   EQU   X'1A'              READ HOME ADDRESS                     00180400
READR0   EQU   X'16'              READ RECORD 0                         00180500
READCNT  EQU   X'12'              READ COUNT                            00180600
READDATA EQU   X'06'              READ DATA                             00180700
READKD   EQU   X'0E'              READ KEY AND DATA                     00180800
READCKD  EQU   X'1E'              READ COUNT, KEY AND DATA              00180900
READMCKD EQU   X'5E'              READ MULTIPLE COUNT, KEY, AND DATA    00181000
SETSECT  EQU   X'23'              SET SECTOR                            00181100
SETFM    EQU   X'1F'              SET FILE MASK                         00181200
READSECT EQU   X'22'              READ SECTOR                           00181300
SEARIDEQ EQU   X'31'              SEARCH IDENTIFIER EQUAL               00181400
*                                                                       00181500
WRITEDAT EQU   X'05'              WRITE DATA                            00181600
WRITEKD  EQU   X'0D'              WRITE KEY AND DATA                    00181700
WRITECKD EQU   X'1D'              WRITE COUNT,KEY AND DATA              00181800
WRITSCKD EQU   X'01'              WRITE SPECIAL COUNT,KEY AND DATA      00181900
*                                 FOR OVERFLOW RECORDS                  00182000
WRITERSE EQU   X'11'              ERASE                                 00182100
*                                                                       00182200
READEVCH EQU   X'64'              READ DEVICE CHARACTERISTICS           00182300
*                                                                       00182400
CCWMT    EQU   X'80'              MULTI-TRACK USE WITH APPROPRIATE      00182500
*                                 OPCODES - CCW SEARKHE+CCWMT,B,C,D     00182600
*                                                                       00182700
*        CCW FLAG EQUATES                                               00182800
*                                                                       00182900
CCWCD    EQU   X'80'              CHAIN DATA                            00183000
CCWCC    EQU   X'40'              CHAIN COMMAND                         00183100
CCWSLI   EQU   X'20'              SUPPRESS INCORRECT LENGTH INDICATION  00183200
CCWSKIP  EQU   X'10'              SKIP TRANSFER OF DATA                 00183300
CCWPCI   EQU   X'08'              PROGRAM CONTROLLED INTERRUPT          00183400
CCWIDA   EQU   X'04'              INDIRECT DATA ADDR (IDAW)             00183500
*                                                                       00183600
*        CSW FLAG EQUATES                                               00183700
*                                                                       00183800
*        UNIT STATUS BYTE                                               00183900
*                                                                       00184000
CSWCUE   EQU   X'20'              CONTROL UNIT END                      00184100
CSWCUB   EQU   X'10'              CONTROL UNIT BUSY                     00184200
CSWCE    EQU   X'08'              CHANNEL END                           00184300
CSWDE    EQU   X'04'              DEVICE END                            00184400
CSWUC    EQU   X'02'              UNIT CHECK                            00184500
CSWUE    EQU   X'01'              UNIT EXCEPTION                        00184600
*                                                                       00184700
*        CHANNEL STATUS BYTE                                            00184800
*                                                                       00184900
CSWPCI   EQU   X'80'              PROGRAM CONTROLLED INTERRUPT          00185000
CSWIL    EQU   X'40'              INCORRECT LENGTH                      00185100
CSWPC    EQU   X'20'              PROGRAM CHECK                         00185200
CSWPROTC EQU   X'10'              PROTECTION CHECK                      00185300
CSWCDC   EQU   X'08'              CHANNEL DATA CHECK                    00185400
CSWCCC   EQU   X'04'              CHANNEL CONTROL CHECK                 00185500
CSWICC   EQU   X'02'              INTERFACE CONTROL CHECK               00185600
CSWCHK   EQU   X'01'              CHAINING CHECK                        00185700
*                                                                       00185800
IFBDCB00 DSECT                                                          00185900
*                                                                       00186000
*        FUNCTION/OPERATION -                                           00186100
*        RESIDENT DCB AND DEB CONTROL BLOCKS FOR USE BY THE SER         00186200
*        MODULES                                                        00186300
*                                                                       00186400
**********************************************************************  00186500
*                                                                       00186600
*        THE FOLLOWING DEB AND DCB RESIDE PERMANENTLY IN THE NUCLEUS    00186700
*        AND WILL BE COMPLETED BY NIP. THEY ARE FOR USE ONLY BY         00186800
*        THE SER ROUTINES                                               00186900
*                                                                       00187000
**********************************************************************  00187100
*                                                                       00187200
         DC    AL4(0)                                                   00187300
         DC    AL4(0)                                                   00187400
         DC    AL4(0)                   POINTER TO CHAN TYPE TABLE      00187500
IFBDEB   DC    1F'0'                    START OF DEB                    00187600
IFBDCB   DC    AL4(0)                                                   00187700
         DC    4F'0'                    OVERLAYED DCB                   00187800
         DC    X'0F'                    DEB ID FIELD                    00187900
         DC    AL3(IFBDCB)              ADDR OF DCB                     00188000
         DC    5F'0'                                                    00188100
         DC    AL4(IFBDEB)              ADDR OF SER DEB                 00188200
         DC    X'01'                    FLAG                            00188300
         DC    XL3'0'                                                   00188400
         DC    2F'0'                                                    00188500
*                                                                       00188600
*********************************************************************** 00188700
*                                                                       00188800
*        THE FOLLOWING ADDED TO SUPPORT DISPLAY EXCEPTION MONITORING    00188900
*        FACILITY (DEMF)                                                00189000
*                                                                       00189100
*********************************************************************** 00189200
*                                                                       00189300
         ORG   IFBDCB00+356        DEMF ORIGIN +89 FULL WORDS           00189400
IFBBUFP  DC    1F'0'               DEMF BUFFER POINTER                  00189500
IFBASCBP DC    1F'0'               DEMF BNGLOGR ASCB POINTER (MVS)      00189600
         DC    4F'0'               DEMF RESERVED                        00189700
IFBFLGS1 DC    X'00'               DEMF FLAGS1                          00189800
IFBFLGS2 DC    X'00'               DEMF FLAGS2                          00189900
IFBFLGS3 DC    X'00'               DEMF FLAGS3                          00190000
IFBFLGS4 DC    X'00'               DEMF FLAGS4                          00190100
*                                                                       00190200
         PRINT NOGEN                                                    00190300
*                                                                       00190400
*        IEZDEB                                                         00190500
*                                                                       00190600
         IEZDEB                                                         00190700
*                                                                       00190800
*        IHADVCT                   DEVICE CHARACTERISTICS TABLE         00190900
*                                                                       00191000
         IHADVCT                                                        00191100
*                                                                       00191200
*        UCB                                                            00191300
*                                                                       00191400
UCB      DSECT                                                          00191500
*                                                                       00191600
         IEFUCBOB                                                       00191700
*                                                                       00191800
*        CVT                                                            00191900
*                                                                       00192000
         CVT   DSECT=YES                                                00192100
*                                                                       00192200
*        DCB                                                            00192300
*                                                                       00192400
         DCBD  DSORG=PS,DEVD=(DA)                                       00192500
*                                                                       00192600
*        ECB                                                            00192700
*                                                                       00192800
         IHAECB                                                         00192900
*                                                                       00193000
*        REGISTER EQUATES                                               00193100
*                                                                       00193200
R0       EQU   0                                                        00193300
R1       EQU   1                                                        00193400
R2       EQU   2                                                        00193500
R3       EQU   3                                                        00193600
R4       EQU   4                                                        00193700
R5       EQU   5                                                        00193800
R6       EQU   6                                                        00193900
R7       EQU   7                                                        00194000
R8       EQU   8                                                        00194100
R9       EQU   9                                                        00194200
R10      EQU   10                                                       00194300
R11      EQU   11                                                       00194400
R12      EQU   12                                                       00194500
R13      EQU   13                                                       00194600
R14      EQU   14                                                       00194700
R15      EQU   15                                                       00194800
*                                                                       00194900
         END                                                            00195000
/*
//*
//STEP3   EXEC PGM=IEBGENER
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  *
  IDENTIFY IGC0007F('ZP60035')
/*
//SYSUT2   DD  DSN=&&SMPMCS,DISP=(MOD,PASS)
//SYSIN    DD  DUMMY
//*
//STEP4   EXEC SMPREC,WORK='SYSALLDA'
//SMPPTFIN DD  DSN=&&SMPMCS,DISP=(OLD,DELETE)
//SMPCNTL  DD  *
  RECEIVE
          SELECT(ZP60035)
          .
/*
//*
//STEP5CK EXEC SMPAPP,WORK='SYSALLDA'
//SMPCNTL  DD  *
  APPLY
        SELECT(ZP60035)
        CHECK
        .
/*
//*
//STEP5   EXEC SMPAPP,COND=(0,NE),WORK='SYSALLDA'
//SMPCNTL  DD  *
  APPLY
        SELECT(ZP60035)
        DIS(WRITE)
        COMPRESS(ALL)
        .
/*
//
