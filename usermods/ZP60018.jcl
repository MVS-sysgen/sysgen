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
